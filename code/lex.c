const char *keyword_if;
const char *keyword_else;
const char *keyword_return;
const char *keyword_break;
const char *keyword_continue;
const char *keyword_for;
const char *keyword_while;
const char *keyword_do;
const char *keyword_func;
const char *keyword_var;
const char *keyword_const;
const char *keyword_enum;
const char *keyword_struct;
const char *keyword_union;
const char *keyword_switch;
const char *keyword_case;
const char *keyword_default;
const char *keyword_typedef;
const char *keyword_sizeof;

typedef enum TokenKind {
        TOKEN_EOF = 0,
        TOKEN_RBRACE = '{',
        TOKEN_LBRACE = '}',
        TOKEN_LBRACKET = '[',
        TOKEN_RBRACKET = ']',
        TOKEN_LPAREN = '(',
        TOKEN_RPAREN = ')',
        TOKEN_COLON = ':',
        // Reserve the first 128 values for one-char tokens
        TOKEN_LAST_CHAR = 127,
        TOKEN_INT,
        TOKEN_FLOAT,
        TOKEN_NAME,
        TOKEN_KEYWORD,
        TOKEN_STR,
        TOKEN_LSHIFT,
        TOKEN_RSHIFT,
        TOKEN_EQ,
        TOKEN_NOTEQ,
        TOKEN_LTEQ,
        TOKEN_GTEQ,
        TOKEN_AND,
        TOKEN_OR,
        TOKEN_INC,
        TOKEN_DEC,
        TOKEN_COLON_ASSIGN,
        TOKEN_ADD_ASSIGN,
        TOKEN_SUB_ASSIGN,
        TOKEN_OR_ASSIGN,
        TOKEN_AND_ASSIGN,
        TOKEN_XOR_ASSIGN,
        TOKEN_LSHIFT_ASSIGN,
        TOKEN_RSHIFT_ASSIGN,
        TOKEN_MUL_ASSIGN,
        TOKEN_DIV_ASSIGN,
        TOKEN_MOD_ASSIGN,
} tokenKind;

typedef enum TokenMod {
        TOKENMOD_NONE,
        TOKENMOD_CHAR,
        TOKENMOD_HEX,
        TOKENMOD_OCT,
        TOKENMOD_BIN
} tokenMod;

typedef struct {
        tokenKind kind;
        tokenMod mod;
        const char *start;
        const char *end;
        union {
                uint64_t int_val;
                double float_val;
                const char *name;
                const char *str_val;
        };
} token_t;

token_t token;
char *stream;

void fatal(const char *fmt, ...) {
        va_list args;
        va_start(args, fmt);
        printf("FATAL: ");
        vprintf(fmt, args);
        printf("\n");
        va_end(args);
}

void syntax_error(const char *fmt, ...) {
        va_list args;

        va_start(args, fmt);
        printf("Syntax Error: ");
        vprintf(fmt, args);
        printf("\n");
        va_end(args);
}

static int convert_hex(char c)
{
#define hex_to_num(a, b) case a : return b
        switch (c) {
                hex_to_num('0', 0);
                hex_to_num('1', 1);
                hex_to_num('2', 2);
                hex_to_num('3', 3);
                hex_to_num('4', 4);
                hex_to_num('5', 5);
                hex_to_num('6', 6);
                hex_to_num('7', 7);
                hex_to_num('8', 8);
                hex_to_num('9', 9);
                hex_to_num('a', 10);
                hex_to_num('b', 11);
                hex_to_num('c', 12);
                hex_to_num('d', 13);
                hex_to_num('e', 14);
                hex_to_num('f', 15);
                hex_to_num('A', 10);
                hex_to_num('B', 11);
                hex_to_num('C', 12);
                hex_to_num('D', 13);
                hex_to_num('E', 14);
                hex_to_num('F', 15);
        default: 
                return -1;
        }
}

char escape_to_char[256] = {
        ['r'] = '\r',
        ['n'] = '\n',
        ['t'] = '\t',
        ['v'] = '\v',
        ['b'] = '\b',
        ['a'] = '\a',
        ['0'] = '\0',
};

void scan_char()
{
        char val;
        assert(*stream == '\'');
        stream++;

        if (*stream == '\'') {
                syntax_error("char literal can not be empty");
        }

        if (*stream == '\n') {
                syntax_error("can not have new line in a char literal");
        }

        if (*stream == '\\') {
                stream++;
                val = escape_to_char[(int)*stream];
                if (val == 0 && *stream != '0') {
                        syntax_error("Invalid char literal escape '\\%c'", *stream);
                }
        } else {
                val = *stream;
        }

        stream++;
        
        if (*stream != '\'') {
                syntax_error("Expected literal ' but instead got '%c'", *stream);
        } else {
                stream++;
        }

        token.kind = TOKEN_INT;
        token.mod= TOKENMOD_CHAR;
        token.int_val = val;
}

void scan_float()
{
        double val;
        char *digit_stream = stream;
        bool parse_done = false;
top:
        switch (*stream) {
        case '0' : case '1' : case '2' : case '3' : case '4' : case '5' : case '6' : case '7' : case '8' : case '9' :
        {
                if (isdigit(*stream)) stream++;
                break;
        }
        case 'e' : case 'E' : case '.' : case '+' : case '-':
        {
                stream++;
                break;
        }

        default:
                parse_done = true;
        }

        if (!parse_done) goto top;

        val = strtod(digit_stream, NULL);
        if (val == HUGE_VAL || val == -HUGE_VAL) {
                syntax_error("float literal overflow");
        }

        token.kind = TOKEN_FLOAT;
        token.float_val = val;
}

void scan_int()
{
        int base, digit;
        uint64_t val = 0;

        switch (*stream) {
        case '0':
                stream++;
                
                if (*stream == 'x') {
                        base = 16;
                        stream++;
                        token.mod = TOKENMOD_HEX;
                } else if (tolower(*stream) == 'b') {
                        stream++;
                        base = 2;
                        token.mod = TOKENMOD_BIN;
                } else if (isdigit(*stream)) {
                        base = 8;
                        token.mod = TOKENMOD_OCT;
                }
                break;
        default:
                base = 10;
        }

        while (*stream && (digit = convert_hex(*stream)) != -1) {
                //               int digit = *stream - '0';
                if (val > (uint64_t)(UINT64_MAX - digit) / base) {
                        syntax_error("Interger literal overflow");

                        while (isdigit(*stream)) {
                                stream++;
                        }
                        val = 0;
                }
                val = val * base + digit;
                stream++;
        }

        token.kind = TOKEN_INT;
        token.int_val = val;
}

void scan_str()
{
        assert(*stream == '"');
        stream++;

        char *str = NULL;
        char val;

        while (*stream && *stream != '"') {
                val = *stream;

                if (*stream == '\n') {
                        syntax_error("String literals can not contain new line characters");
                }

                if (*stream == '\\') {
                        stream++;
                        if (*stream == '"') {
                                val = *stream;
                        } else {
                                val = escape_to_char[(int)*stream];
                                if (val == 0 && *stream != '0') {
                                        syntax_error("invalid string literal escape '\\%c'", *stream);
                                }
                        }
                }

                buf_push(str, val);
                stream++;
        }

        stream++;
        buf_push(str, '\0');
        token.kind = TOKEN_STR;
        token.str_val = str;
}

bool token_keyword()
{
}

void next_token() 
{
top:
        token.start = stream;
        token.mod = TOKENMOD_NONE;

        switch (*stream) {
        case ' ' : case '\n' : case '\r' : case '\t' : case '\v':
                while (isspace(*stream)) {
                        stream++;
                }
                goto top;
                break;
        case '\'':
                scan_char();
                break;
        case '"':
                scan_str();
                break;
        case '.':
                scan_float();
                break;
        case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
        {
                char *digit_stream = stream;

                while(isdigit(*digit_stream)) {digit_stream++;}
                if (*digit_stream == '.' ||
                    tolower(*digit_stream) == 'e') {
                        scan_float();
                } else {
                        scan_int();
                }
                break;
        }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B': case 'C': case 'D':
        case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
        case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
        case 'Y': case 'Z':
        case '_':
        {
                const char *start = stream++;
                const char *end = NULL;

                while (isalnum(*stream) || *stream == '_') {
                        stream++;
                }
                end = stream - 1;
                token.start = start;
                token.end = end;
                token.name = str_intern_range(start, end);
                
                if (token_keyword()) {
                        token.kind = TOKEN_KEYWORD;
                } else {
                        token.kind = TOKEN_NAME;
                }
                break;
        }
#define CASE1(c, a1, t1, a2, t2)                \
        case c:                                 \
                token.kind = *stream++;         \
                if (*stream == a1)  {           \
                        token.kind = t1;        \
                        stream++;               \
                } else {                        \
                        token.kind = t2;        \
                        stream++;               \
                }                               \
                break

                CASE1('+', '+', TOKEN_INC, '=', TOKEN_ADD_ASSIGN);
                CASE1('-', '-', TOKEN_DEC, '=', TOKEN_SUB_ASSIGN);
                CASE1('|', '|', TOKEN_OR, '=', TOKEN_OR_ASSIGN);
                CASE1('&', '&', TOKEN_AND, '=', TOKEN_ADD_ASSIGN);
                
        case '^':
                token.kind = *stream++;

                if (*stream == '=') {
                        token.kind = TOKEN_XOR_ASSIGN;
                        stream++;
                }
                break;
        case ':':
                token.kind = *stream++;

                if (*stream == '=') {
                        token.kind = TOKEN_COLON_ASSIGN;
                        stream++;
                }
                break;

        case '*':
                token.kind = *stream++;

                if (*stream == '=') {
                        token.kind = TOKEN_MUL_ASSIGN;
                        stream++;
                }

                break;
        case '/':
                token.kind = *stream++;

                if (*stream == '=') {
                        token.kind = TOKEN_DIV_ASSIGN;
                        stream++;
                }
                break;

        case '%':
                token.kind = *stream++;

                if (*stream == '=') {
                        token.kind = TOKEN_MOD_ASSIGN;
                        stream++;
                }
                break;
        case '<':
                token.kind = *stream++;

                if (*stream == '<') {
                        token.kind = TOKEN_LSHIFT;
                        stream++;

                        if (*stream == '=') {
                                token.kind = TOKEN_RSHIFT_ASSIGN;
                                stream++;
                        }
                } else if (*stream == '=') {
                        token.kind = TOKEN_LTEQ;
                        stream++;
                }
                break;
        case '>':
                token.kind = *stream++;

                if (*stream == '>') {
                        token.kind = TOKEN_RSHIFT;
                        stream++;

                        if (*stream == '=') {
                                token.kind = TOKEN_RSHIFT_ASSIGN;
                                stream++;
                        }
                } else if (*stream == '=') {
                        token.kind = TOKEN_GTEQ;
                        stream++;
                } 
                break;
        case '=':
                token.kind = TOKEN_ASSIGN;

                if (*stream == '=') {
                        
                }

                break;
                
        default:
                token.kind = *stream++;
        }

        token.end = stream - 1;
}

bool is_token(tokenKind kind)
{
        if (token.kind == kind) {
                return true;
        }

        return false;
}

bool match_token(tokenKind kind)
{
        if (token.kind == kind) {
                next_token();
                return true;
        }

        return false;
}

void expect_token(tokenKind kind)
{
        if (token.kind == kind) {
                next_token();
                return;
        }

        syntax_error();
}

bool match_keyword(const char *keyword) {
        if (token.kind == TOKEN_KEYWORD && token.name == keyword) {
                next_token();
                return true;
        }

        return false;
}

void print_token()
{
        switch (token.kind) {
        case TOKEN_INT:
                printf("%lu\n", token.int_val);
                break;
        case TOKEN_FLOAT:
                printf("%f\n", token.float_val);
                break;
        case TOKEN_NAME:
                printf("%.*s\n", (int)(token.end - token.start + 1), token.start);
                break;

        default:
                printf("token kind = %c\n", token.kind);
        }
}

static void init_stream(const char *str)
{
        stream = (char *) str;
        next_token();
}

#define assert_token_int(val) assert(token.int_val == val && match_token(TOKEN_INT))
#define assert_token_float(val) assert(token.float_val == val && match_token(TOKEN_FLOAT))
#define assert_token_char(val) assert(token.int_val == val && token.mod == TOKENMOD_CHAR && match_token(TOKEN_INT))
#define assert_token_str(val) assert(strcmp(token.str_val, val) == 0 && match_token(TOKEN_STR))
#define assert_token_ident(val) assert(token.name == (val) && match_token(TOKEN_NAME))
#define assert_token_eof() assert(token.kind == '\0')

static void lex_init()
{
        keyword_if = str_intern("if");
        keyword_else = str_intern("else");
        keyword_return = str_intern("return");
        keyword_break = str_intern("break");
        keyword_continue = str_intern("continue");
        keyword_for = str_intern("for");
        keyword_while = str_intern("while");
        keyword_do = str_intern("do");
        keyword_func = str_intern("func");
        keyword_var = str_intern("var");
        keyword_const = str_intern("const");
        keyword_enum = str_intern("enum");
        keyword_struct = str_intern("struct");
        keyword_union = str_intern("union");
        keyword_switch = str_intern("switch");
        keyword_case = str_intern("case");
        keyword_default = str_intern("default");
        keyword_typedef = str_intern("typedef");
        sizeof_keyword = str_intern("sizeof");
}
        
static void lex_test()
{
//        init_stream("+ 123,HELLO(), abc32343 84384384 0111 0xffffffffffffffff 0xa 1.4 1.4e10 1e10 4 0x5 1e10");
        // identifier test
        init_stream("hello123");
        assert_token_ident(str_intern("hello123"));
        assert_token_eof();

        // integer literal test
        init_stream("123 0 23");
        assert_token_int(123);

        // floating point test
        init_stream("0xff 1.2");
        assert(token.mod == TOKENMOD_HEX);
        assert_token_int(255);
        assert_token_float(1.2);
        assert_token_eof();

        // char literal test
        init_stream("'\\n' 'a' " );
        assert_token_int('\n');
        assert_token_int('a');
        assert_token_eof();

        // string literal tests
        init_stream("\"foo\" \"a\\n\"");

        assert_token_str("foo");
        assert_token_str("a\n");
        assert_token_eof();

        init_stream("0");
        assert_token_int(0);

        printf("lex test passed\n");
}

