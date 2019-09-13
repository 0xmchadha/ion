typedef struct BufHdr {
    size_t len;
    size_t cap;
    char buf_[0];
} BufHdr;

#define buf__hdr(buf) ((struct BufHdr *) ((char *) buf - offsetof(struct BufHdr, buf_)))
#define buf__fit(buf)                                                                              \
    ((!(buf) || buf__hdr(buf)->len == buf__hdr(buf)->cap) ? (buf = buf_grow(buf, sizeof(*buf)))    \
                                                          : 0)

// macros to be used by clients
#define buf_push(buf, ...) (buf__fit(buf), ((buf)[buf__hdr(buf)->len++] = (__VA_ARGS__)))
#define buf_len(buf) ((buf) ? (buf__hdr(buf)->len) : 0)
#define buf_end(buf) ((buf) + buf_len(buf))		
#define buf_cap(buf) ((buf) ? buf__hdr(buf)->cap : 0)
#define buf_free(buf) ((buf) ? (free(buf__hdr(buf)), (buf) = NULL) : 0)
#define buf_sizeof(buf) ((buf) ? (buf_len(buf) * sizeof(*buf)) : 0)

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

void *buf_grow(void *_buf, int elem_size) {
    struct BufHdr *buf;
    if (!_buf) {
        buf = xmalloc(sizeof(struct BufHdr) + elem_size);
        buf->len = 0;
        buf->cap = 1;
    } else {
        buf = buf__hdr(_buf);
        buf = xrealloc(buf, sizeof(struct BufHdr) + buf->cap * 2 * elem_size);
        buf->cap = buf->cap * 2;
    }

    return (char *) buf + offsetof(struct BufHdr, buf_);
}

#define ARENA_BLOCK_SIZE 1024
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

    printf("buf test passed\n");
}

struct internStr {
    size_t len;
    char *str;
};

struct internStr *interns = NULL;
Arena str_arena;

const char *str_intern_range(const char *start, const char *end) {
    size_t len = end - start;
    size_t i;

    for (i = 0; i < buf_len(interns); i++) {
        if (interns[i].len == len && !strncmp(interns[i].str, start, len)) {
            return interns[i].str;
        }
    }

    char *strp = arena_alloc(&str_arena, len + 1);
    strncpy(strp, start, len);
    strp[len] = '\0';

    buf_push(interns, ((struct internStr){len, strp}));

    return interns[i].str;
}

const char *str_intern(const char *str) { return str_intern_range(str, str + strlen(str)); }

void str_intern_test() {
    char a[] = "hello";
    char b[] = "hello";
    char c[] = "hell";

    assert(a != b);
    assert(str_intern(&a[0]) == str_intern(&b[0]));
    assert(str_intern_range(&a[0], &a[strlen(a)]) !=
           str_intern_range(&c[0], &b[strlen(c)]));
    printf("string intern test passed\n");
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

#define syntax_error(fmt, ...) syntax__error(fmt, __LINE__, __FILE__)

void syntax__error(const char *fmt, int line, char *file,...) {
    va_list args;
    va_start(args, file);
    printf("Syntax Error: %d %s ", line, file);
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
}

void common_test() {
    buf_test();
    str_intern_test();
}
