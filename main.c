#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stddef.h>
#include <assert.h>
#include <ctype.h>
#include <stdint.h>
// stretchy buffers

//buf_push
//buf_len
//buf_cap
//buf_free

struct bufHdr {
        int len;
        int cap;
        char buf_[0];
};

#define buf__hdr(buf) ((struct bufHdr *) ((char *)buf - offsetof(struct bufHdr, buf_)))
#define buf__fit(buf) ((!(buf) || buf__hdr(buf)->len == buf__hdr(buf)->cap) ? \
                       (buf = buf_grow(buf, sizeof(*buf))) : (void)0)


// macros to be used by clients
#define buf_push(buf, val) (buf__fit(buf), ((buf)[buf__hdr(buf)->len++] = (val)))
#define buf_len(buf) (buf ? (buf__hdr(buf)->len) : 0)
#define buf_cap(buf) (buf ? buf__hdr(buf)->cap : 0)
#define buf_free(buf) (buf) ? (free(buf__hdr(buf)), (buf) = NULL) : (void)0

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
}


typedef enum {
        TOKEN_INT = 128,
        TOKEN_OPERATOR,
        TOKEN_IDENT,
        TOKEN_KEYWORD
} tokenKind;

typedef struct {
        tokenKind kind;

        union {
                uint64_t val;
                struct {
                        char *buf, *end;
                };
        };
        
}token_t;

token_t *tokenizer(char *stream)
{
        char *start = NULL, *end = NULL;
        token_t *tokens = NULL;

        while (*stream) {
                start = stream;
                end = NULL;
                
                if (isalpha(*stream)) {
                        while(*stream && isalnum(*stream++));
                        end = stream-1;

                        buf_push(tokens, ((token_t){.kind = TOKEN_IDENT, .buf = start, .end = end}));
                        continue;
                }
 
                if (isdigit(*stream)) {
                        uint64_t val = 0;
                        while(*stream && isdigit(*stream)) {
                                val = val * 10 + *stream - '0';
                                stream++;
                        }

                        buf_push(tokens, ((token_t){.kind = TOKEN_INT, .val = val}));
                        continue;
                }

                buf_push(tokens, ((token_t){.kind = *stream}));
                stream++;
        }

        return tokens;
}

void lex_test()
{
        char *prog = "+ 123,HELLO(), abc32343 84384384";
        token_t *token_arr = NULL;

        token_arr = tokenizer(prog);

        printf("tokens len = %d\n", buf_len(token_arr));

        for (int i = 0; i < buf_len(token_arr); i++) {
                if (token_arr[i].kind == TOKEN_INT) {
                        printf("%d\n", token_arr[i].val);
                }
        }
}

int main()
{
        buf_test();
        lex_test();
}

