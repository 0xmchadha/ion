#include <stdio.h>
#include <stdlib.h>

/*************************************************************/
/* Convert the input program string into an array of tokens. */
/* Tokens can be of different kinds.                         */
/* In ion, we use the following token types.                 */
/*                                                           */
/*         Ident:                                            */
/*         Operator                                          */
/*         Keywords                                          */
/*         Int                                               */
/*************************************************************/

typedef enum {
        TOKEN_INT,
        TOKEN_OPERATOR,
        TOKEN_IDENT,
        TOKEN_KEYWORD
} tokenKind;

typedef struct {
        tokenKind kind;
        
} token_t;

struct token *tokenizer(char *stream)
{
        char *start = NULL, *end = NULL;
        struct token *tokens = NULL;

        while (*stream) {
                start = stream;
                end = NULL;
                
                if (isalpha(*stream)) {
                        while(*stream && isalphanum(*stream++));
                        end = stream-1;

                        buf_push(tokens, (token_t){TOKEN_IDENT});
                        continue;
                }

                if (isnum(*stream)) {
                        while(*stream && isnum(*stream++));
                        end = stream-1;

                        buf_push(tokens, (token_t){TOKEN_INT});
                        continue;
                }
              
                stream++;
        }

        return tokens;
}

void lex_test()
{
        char *prog = "+ 123,HELLO(), abc";
        struct token *token_arr = NULL;

        lex = tokenizer(prog);
}
