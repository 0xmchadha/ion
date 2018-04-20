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
                        const char *start, *end;
                };
        };
        
} token_t;

token_t token;
char *stream;

/*
char *strings;

void *str_intern(const char *string)
{
        for (int i = 0; i < buf_len(strings); i++) {
                if (strcmp(string, strings[i]
        }
}
*/
void next_token()
{
        switch (*stream) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        {
                uint64_t val = 0;
                while (*stream && isdigit(*stream)) {
                        val = val * 10 + *stream - '0';
                        stream++;
                }
                token.kind = TOKEN_INT;
                token.val = val;
                break;
        }
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
        case '_':
        {
                const char *start = stream++;
                const char *end = NULL;
                
                while (isalnum(*stream) || *stream == '_') {
                        stream++;
                }
                
                end = stream - 1;
                
                token.kind = TOKEN_IDENT;
                token.start = start;
                token.end = end;
                break;
        }
        default:
                token.kind = *stream++;
                
        }
}

void print_token()
{
        switch (token.kind) {
        case TOKEN_INT:
                printf("%d\n", token.val);
                break;
                
        case TOKEN_IDENT:
                printf("%.*s\n", (token.end - token.start + 1), token.start);
                break;

        default:
                printf("token kind = %c\n", token.kind);
        } 
}


void lex_test()
{
        char *prog = "+ 123,HELLO(), abc32343 84384384";
        token_t *token_arr = NULL;
        
        stream = prog;
        next_token();
        
        while (token.kind) {
                print_token();
                next_token();
                buf_push(token_arr, token);
        }
}

int main()
{
        buf_test();
        lex_test();
}

