#define MAX(a, b) ((a) > (b) ? (a) : (b))

typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf_[0];
} BufHdr;

#define buf__hdr(buf) ((struct BufHdr *) ((char *) buf - offsetof(struct BufHdr, buf_)))
#define buf__fit(buf)                                                                              \
    ((!(buf) || buf__hdr(buf)->len == buf__hdr(buf)->cap) ? (buf = buf_grow(buf, sizeof(*buf))) : 0)

// macros to be used by clients
#define buf_push(buf, ...) (buf__fit(buf), ((buf)[buf__hdr(buf)->len++] = (__VA_ARGS__)))
#define buf_len(buf) ((buf) ? (buf__hdr(buf)->len) : 0)
#define buf_end(buf) ((buf) + buf_len(buf))
#define buf_cap(buf) ((buf) ? buf__hdr(buf)->cap : 0)
#define buf_free(buf) ((buf) ? (free(buf__hdr(buf)), (buf) = NULL) : 0)
#define buf_sizeof(buf) ((buf) ? (buf_len(buf) * sizeof(*buf)) : 0)
#define buf_printf(buf, ...) buf = buf__printf((buf), __VA_ARGS__)

void *xmalloc(size_t num_bytes) {
    void *ptr = malloc(num_bytes);

    if (!ptr) {
        perror("malloc failed\n");
        exit(1);
    }

    return ptr;
}

void *xrealloc(void *ptr, size_t num_bytes) {
    ptr = realloc(ptr, num_bytes);

    if (!ptr) {
        assert(0);
        perror("realloc failed\n");
        exit(1);
    }

    return ptr;
}

void *xcalloc(size_t num_bytes) {
    void *ptr = calloc(1, num_bytes);

    if (!ptr) {
        perror("calloc failed\n");
        exit(1);
    }

    return ptr;
}

const char *strf(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t n = 1 + vsnprintf(NULL, 0, fmt, args);
    char *str = xmalloc(n);
    va_end(args);
    va_start(args, fmt);
    vsnprintf(str, n, fmt, args);
    va_end(args);
    return str;
}

void *buf_grow(void *_buf, int elem_size) {
    struct BufHdr *buf;

    if (!_buf) {
        buf = xmalloc(sizeof(struct BufHdr) + elem_size);
        buf->len = 0;
        buf->cap = 1;
    } else {
        buf = buf__hdr(_buf);
        buf = xrealloc(buf, sizeof(struct BufHdr) + (buf->cap * elem_size) * 2);
        buf->cap = buf->cap * 2;
    }

    return (char *) buf + offsetof(struct BufHdr, buf_);
}

void *print_buf_grow(void *_buf, int new_size) {
    struct BufHdr *buf;

    if (!_buf) {
        buf = xmalloc(sizeof(struct BufHdr) + new_size);
        buf->len = 0;
        buf->cap = new_size;
    } else {
        buf = buf__hdr(_buf);
        buf = xrealloc(buf, sizeof(struct BufHdr) + new_size);
        buf->cap = new_size;
    }

    return (char *) buf + offsetof(struct BufHdr, buf_);
}

char *buf__printf(char *buf, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t n = 1 + vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    if (buf_cap(buf) < buf_len(buf) + n) {
        buf = print_buf_grow(buf, buf_len(buf) + n);
    }

    va_start(args, fmt);
    vsnprintf(&(buf)[buf__hdr(buf)->len], n, fmt, args);
    buf__hdr(buf)->len += (n - 1);
    va_end(args);
    return buf;
}

#define ARENA_BLOCK_SIZE (1 << 21)
#define ARENA_ALIGNMENT 8

#define POW_OF_2(n) (((n) & (n) -1) == 0)
#define ALIGN_DOWN(n, a) ((n) & ~((a) -1))
#define ALIGN_UP(n, a) ALIGN_DOWN(((n) + (a) -1), (a))
#define ALIGN_UP_PTR(n, a) ((void *) ALIGN_UP((uint64_t)(n), (a)))
#define ALIGN_DOWN_PTR(n, a) ((void *) ALIGN_DOWN((uint64_t)(n), (a)))

typedef struct Arena {
    char **blocks;
    char *ptr;
    char *end;
} Arena;

void arena_grow(Arena *arena) {
    void *new_block = xmalloc(ARENA_BLOCK_SIZE);

    assert(new_block == ALIGN_DOWN_PTR(new_block, ARENA_ALIGNMENT));
    buf_push(arena->blocks, new_block);
    arena->ptr = new_block;
    arena->end = arena->ptr + ARENA_BLOCK_SIZE;
}

void arena_free(Arena *arena) {
    for (size_t i = 0; i < buf_len(arena->blocks); i++) {
        free(arena->blocks[i]);
    }

    free(arena->blocks);
}

void *arena_alloc(Arena *arena, size_t size) {

    assert(POW_OF_2(ARENA_ALIGNMENT));
    size = ALIGN_UP(size, ARENA_ALIGNMENT);

    if (size > (size_t)(arena->end - arena->ptr)) {
        arena_grow(arena);
        assert(size <= (size_t)(arena->end - arena->ptr));
    }

    void *ptr = arena->ptr;
    arena->ptr += size;

    return ptr;
}

const char *read_file(const char *path) {
    FILE *fp = fopen(path, "r");
    assert(fp);
    fseek(fp, 0L, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0L, SEEK_SET);
    char *str = xmalloc(size + 1);
    size_t read = 0;

    while (read != size) {
        read += fread(&str[read], 1, size - read, fp);
    }

    str[size] = '\0';
    return str;
}

void write_file(const char *path, const char *data, size_t length) {
    FILE *fp = fopen(path, "w+");
    assert(fp);
    size_t written = 0;

    while (written != length) {
        written += fwrite(&data[written], 1, length - written, fp);
    }
}

const char *replace_ext(const char *path, const char *ext) {
    size_t len = 0;

    while (path[len] != '\0' && path[len] != '.') {
        len++;
    }

    if (path[len] == '\0') {
        return NULL;
    }

    len++;
    char *newstr = xmalloc(len + strlen(ext) + 1);
    strncpy(newstr, path, len);
    strncpy(&newstr[len], ext, strlen(ext) + 1);

    return newstr;
}

typedef struct Map {
    const void **key;
    const void **val;
    size_t len;
    size_t cap;
} Map;

void map_put(Map *map, const void *key, const void *val);

uint64_t hash_uint64(uint64_t key) {
    key *= 0xff51afd7ed558ccd;
    key ^= key >> 32;
    return key;
}

uint64_t hash_ptr(const void *key) {
    return hash_uint64((uint64_t)key);
}

void map_grow(Map *map) {
    size_t new_size = MAX(16, 2 * map->cap);

    Map new_map = {
        .key = xcalloc(new_size * sizeof(void *)),
        .val = xcalloc(new_size * sizeof(void *)),
        .len = 0,
        .cap = new_size,
    };

    for (size_t h = 0; h < map->cap; h++) {
        if (map->key[h]) {
            map_put(&new_map, map->key[h], map->val[h]);
        }
    }

    free(map->key);
    free(map->val);
    
    *map = new_map;
}

const void *map_get(Map *map, const void *key) {
    assert(key);
    if (!map->cap) {
        return NULL;
    }
    
    uint64_t h = hash_ptr(key) % map->cap;
    while (map->key[h] && map->key[h] != key) {
        h = (h + 1) % map->cap;
    }

    if (map->key[h] == key) {
        return map->val[h];
    }

    return NULL;
}

void map_put(Map *map, const void *key, const void *val) {
    assert(key);
    assert(val);

    if (map->len * 2 >= map->cap) {
        map_grow(map);
    }

    uint64_t h = hash_ptr(key) & (map->cap-1);

    while (map->key[h] && map->key[h] != key) {
        // when capacity is power of 2 the following trick can be used.
        h = (h+1) & (map->cap-1); 
    }

    if (map->key[h] != key) {
        map->len++;
    }

    map->key[h] = key;
    map->val[h] = val;
}

struct internStr {
    size_t len;
    char *str;
};

struct internStr *interns = NULL;
Arena str_arena;

Map str_intern_map;

uint64_t hash_str(const char *start, const char *end) {
    uint64_t h = 0xcbf29ce484222325;

    for (const char *it = start; it != end; it++) {
        h ^= *it;
        h *= 0x100000001b3;
        h ^= h >> 32;
    }

    return h;
}

const char *str_intern_range(const char *start, const char *end) {
    size_t len = end - start;

    uint64_t hash = hash_str(start, end);
    hash = (!hash) ? 1 : hash;
    char *strp = NULL;
    
    if (strp = (char *)map_get(&str_intern_map, (void *)hash)) {
        if (!strncmp(strp, start, len)) {
            return strp;
        }
    }

    strp = arena_alloc(&str_arena, len + 1);
    strncpy(strp, start, len);
    strp[len] = '\0';
    map_put(&str_intern_map, (void*)hash, strp);
    buf_push(interns, ((struct internStr){len, strp}));

    return strp;
}

const char *str_intern(const char *str) {
    return str_intern_range(str, str + strlen(str));
}

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

void map_test() {
    Map map = {};
    for (uint64_t i = 1; i < 1024; i++) {
        map_put(&map, (void *)i, (void *)i);
    }

    for (uint64_t i = 1; i < 1024; i++) {
        void *val = (void *)map_get(&map, (void *)i);
        assert((uint64_t)val == (uint64_t)i);
    }
}

void buf_test() {
    int *buf = NULL;

    enum { N = 1024 };

    assert(buf_len(buf) == 0);
    for (int i = 0; i < N; i++) {
        buf_push(buf, i);
    }

    assert(buf_len(buf) == N);

    for (int i = 0; i < N; i++) {
        assert(buf[i] == i);
    }

    buf_free(buf);
    assert(buf == NULL);
    assert(buf_len(buf) == 0);

    {
        char *buf_print = NULL;

        buf_print = buf__printf(buf_print, "%s", "hello");
        buf_print = buf__printf(buf_print, " %s", "world");

        if (strcmp(buf_print, "hello world")) {
            assert(0);
        }
    }

    printf("buf test passed\n");
}

void str_intern_test() {
    char a[] = "hello";
    char b[] = "hello";
    char c[] = "hell";

    assert(a != b);
    assert(str_intern(&a[0]) == str_intern(&b[0]));
    assert(str_intern_range(&a[0], &a[strlen(a)]) != str_intern_range(&c[0], &b[strlen(c)]));
    printf("string intern test passed\n");
}

void arena_test() {
    Arena arena = {0};
    char *ptr1, *ptr2;
    ptr1 = arena_alloc(&arena, 5);

    assert(ptr1 == ALIGN_DOWN_PTR(ptr1, ARENA_ALIGNMENT));
    assert(arena.end == ptr1 + ARENA_BLOCK_SIZE);
    assert(arena.ptr == ptr1 + ARENA_ALIGNMENT);

    ptr2 = arena_alloc(&arena, 5);
    assert(ptr2 == ALIGN_DOWN_PTR(ptr2, ARENA_ALIGNMENT));
    assert(arena.end == ptr1 + ARENA_BLOCK_SIZE);
    assert(arena.ptr == ptr2 + ARENA_ALIGNMENT);
}

void common_test() {
    arena_test();
    buf_test();
    str_intern_test();
    map_test();
    read_file("test.ion");
}
