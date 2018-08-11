#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <ctype.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <limits.h>
#include <math.h>

struct bufHdr {
        size_t len;
        size_t cap;
        char buf_[0];
};

#define buf__hdr(buf) ((struct bufHdr *) ((char *)buf - offsetof(struct bufHdr, buf_)))
#define buf__fit(buf) ((!(buf) || buf__hdr(buf)->len == buf__hdr(buf)->cap) ? \
                       (buf = buf_grow(buf, sizeof(*buf))) : (void)0)


// macros to be used by clients
#define buf_push(buf, val) (buf__fit(buf), ((buf)[buf__hdr(buf)->len++] = (val)))
#define buf_len(buf) ((buf) ? (buf__hdr(buf)->len) : 0)
#define buf_cap(buf) ((buf) ? buf__hdr(buf)->cap : 0)
#define buf_free(buf) (buf) ? (free(buf__hdr(buf)), (buf) = NULL) : (void)0
#define buf_sizeof(buf) (buf) ? (buf_len(buf)*sizeof(*buf)) : 0

        
void *xmalloc(size_t num_bytes)
{
        void *ptr = malloc(num_bytes);

        if (!ptr) {
                perror("malloc failed\n");
                exit(1);
        }

        return ptr;
}

void *xrealloc(void *ptr, size_t num_bytes)
{
        ptr = realloc(ptr, num_bytes);

        if (!ptr) {
                perror("realloc failed\n");
                exit(1);
        }

        return ptr;
}

void *buf_grow(void *_buf, int elem_size)
{
        struct bufHdr *buf;
        if (!_buf) {
                buf = xmalloc(sizeof(struct bufHdr) + elem_size);
                buf->len = 0;
                buf->cap = 1;
        } else {
                buf = buf__hdr(_buf);
                buf = xrealloc(buf, sizeof(struct bufHdr) + buf->cap * 2 * elem_size);
                buf->cap = buf->cap * 2;
        }

        return (char *) buf + offsetof(struct bufHdr, buf_);
}

void buf_test()
{
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

const char *str_intern_range(const char *start, const char *end)
{
        size_t len = end - start + 1;
        size_t i;

        for (i = 0; i < buf_len(interns); i++) {
                if (interns[i].len == len && !strncmp(interns[i].str, start, len)) {
                        return interns[i].str;
                }
        }

        char *strp = xmalloc(len);
        strncpy(strp, start, len);
        strp[len] = '\0';

        buf_push(interns, ((struct internStr){len, strp}));

        return interns[i].str;
}

const char *str_intern(const char *str)
{
        return str_intern_range(str, str + strlen(str) - 1);
}

void str_intern_test()
{
        char a[] = "hello";
        char b[] = "hello";
        char c[] = "hell";

        assert(a != b);
        assert(str_intern(&a[0]) == str_intern(&b[0]));
        assert(str_intern_range(&a[0], &a[strlen(a)-1]) != str_intern_range(&c[0], &b[strlen(c)-1]));
        assert(str_intern(&a[0]) == str_intern_range(&a[0], &a[4]));
        printf("string intern test passed\n");
}

void common_test()
{
        buf_test();
        str_intern_test();
}
