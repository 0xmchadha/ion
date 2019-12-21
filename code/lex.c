// keywords
const char *keyword_typedef;
const char *keyword_enum;
const char *keyword_struct;
const char *keyword_union;
const char *keyword_const;
const char *keyword_var;
const char *keyword_func;
const char *keyword_sizeof;
const char *keyword_break;
const char *keyword_continue;
const char *keyword_return;
const char *keyword_if;
const char *keyword_else;
const char *keyword_while;
const char *keyword_for;
const char *keyword_do;
const char *keyword_switch;
const char *keyword_case;
const char *keyword_cast;
const char *keyword_foreign;
const char *keyword_default;

const char *first_keyword;
const char *last_keyword;
const char **keywords;

#define KEYWORD(x)                                                                                 \
    keyword_##x = str_intern(#x);                                                                  \
    buf_push(keywords, keyword_##x)

// clang-format off
void init_keywords() {
    const char *arena_end;

    KEYWORD(typedef);
    arena_end = str_arena.end;

    KEYWORD(enum);
    KEYWORD(struct);
    KEYWORD(union);
    KEYWORD(const);
    KEYWORD(var);
    KEYWORD(func);
    KEYWORD(cast);
    KEYWORD(sizeof);
    KEYWORD(break);
    KEYWORD(continue);
    KEYWORD(return );
    KEYWORD(if);
    KEYWORD(else);
    KEYWORD(while);
	KEYWORD(for);
	KEYWORD(do);
	KEYWORD(switch);
	KEYWORD(case);
	KEYWORD(default);
        KEYWORD(foreign);
	assert(str_arena.end == arena_end);

	first_keyword = keyword_typedef;
	last_keyword = keyword_foreign;
}
// clang-format on

#undef KEYWORD

bool is_token_keyword(const char *name) {
    return name >= first_keyword && name <= last_keyword;
}

typedef enum TokenKind {
    TOKEN_EOF = 0,
    TOKEN_FOREIGN,
    TOKEN_QUESTION,
    TOKEN_COLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACKETS,
    TOKEN_RBRACKETS,
    TOKEN_LBRACES,
    TOKEN_RBRACES,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_ELLIPSIS,
    TOKEN_SEMICOLON,
    TOKEN_KEYWORD,
    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_STR,
    TOKEN_NAME,
    TOKEN_NEG,
    TOKEN_NOT,
    TOKEN_MUL,
    // token mul precedence
    TOKEN_FIRST_MUL = TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_AND,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_LAST_MUL = TOKEN_RSHIFT,
    // token add precedence
    TOKEN_ADD,
    TOKEN_FIRST_ADD = TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_OR,
    TOKEN_XOR,
    TOKEN_LAST_ADD = TOKEN_XOR,

    // token eq precedence
    TOKEN_EQ,
    TOKEN_FIRST_EQ = TOKEN_EQ,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_NOTEQ,
    TOKEN_LAST_EQ = TOKEN_NOTEQ,
    TOKEN_AND_AND,
    TOKEN_OR_OR,
    TOKEN_ASSIGN,
    TOKEN_FIRST_ASSIGN = TOKEN_ASSIGN,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
    TOKEN_LAST_ASSIGN = TOKEN_MOD_ASSIGN,
    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_COLON_ASSIGN,
} TokenKind;

typedef enum TokenMod {
    TOKENMOD_NONE,
    TOKENMOD_CHAR,
    TOKENMOD_HEX,
    TOKENMOD_OCT,
    TOKENMOD_BIN,
} TokenMod;

const char *token_to_str[] = {
    [TOKEN_EOF] = "EOF",
    [TOKEN_KEYWORD] = "KEYWORD",
    [TOKEN_NAME] = "name",
    [TOKEN_STR] = "str",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_QUESTION] = "?",
    [TOKEN_COLON] = ":",
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACKETS] = "[",
    [TOKEN_RBRACKETS] = "]",
    [TOKEN_LBRACES] = "{",
    [TOKEN_RBRACES] = "}",
    [TOKEN_COMMA] = ",",
    [TOKEN_DOT] = ".",
    [TOKEN_ELLIPSIS] "...",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_NEG] = "~",
    [TOKEN_NOT] = "!",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",
    [TOKEN_AND] = "&",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_OR] = "|",
    [TOKEN_XOR] = "^",
    [TOKEN_EQ] = "==",
    [TOKEN_LT] = "<",
    [TOKEN_GT] = ">",
    [TOKEN_LTEQ] = "<=",
    [TOKEN_GTEQ] = ">=",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_AND_AND] = "&&",
    [TOKEN_OR_OR] = "||",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_XOR_ASSIGN] = "^=",
    [TOKEN_OR_ASSIGN] = "|=",
    [TOKEN_AND_ASSIGN] = "&=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_MUL_ASSIGN] = "*=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
    [TOKEN_INC] = "++",
    [TOKEN_DEC] = "--",
    [TOKEN_COLON_ASSIGN] = ":=",
};

typedef struct {
    TokenKind kind;
    TokenMod mod;
    SrcPos pos;
    const char *start;
    const char *end;
    union {
        int int_val;
        double float_val;
        const char *name;
        const char *str_val;
    };
} token_t;

token_t token;
const char *stream;
size_t line_num;
const char *file_name;

uint8_t char_digit[] = {
    ['0'] = 0,  ['1'] = 1,  ['2'] = 2,  ['3'] = 3,  ['4'] = 4,  ['5'] = 5,  ['6'] = 6,  ['7'] = 7,
    ['8'] = 8,  ['9'] = 9,  ['a'] = 10, ['A'] = 10, ['b'] = 11, ['B'] = 11, ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13, ['e'] = 14, ['E'] = 14, ['f'] = 15, ['F'] = 15,
};

static inline int char_to_digit(char c) {
    return (c == '0' || char_digit[(uint8_t) c] != 0) ? char_digit[(uint8_t) c] : -1;
}

void scan_int() {
    int base, digit;
    int val = 0;

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

    for (;;) {

        digit = char_to_digit(*stream);

        if (digit == -1) {
            break;
        }

        if (digit >= base) {
            syntax_error(token.pos, "digit %c is out of range for the base %d", *stream, base);
            break;
        }

        if (val > (int) (INT_MAX - digit) / base) {
            syntax_error(token.pos, "Interger literal overflow");

            while (isdigit(*stream)) {
                stream++;
            }
            val = 0;
            break;
        }

        val = val * base + digit;
        stream++;
    }

    token.kind = TOKEN_INT;
    token.int_val = val;
}

void scan_float() {
    const char *digit_stream = stream;

    switch (tolower(*stream)) {
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
        while (isdigit(*stream)) {
            stream++;
        }
    case '.':
        if (*stream == '.') {
            stream++;

            while (isdigit(*stream)) {
                stream++;
            }
            if (tolower(*stream) != 'e') {
                break;
            }
        }
    case 'e':
        if (*stream == 'e') {
            if (*stream == '+' || *stream == '-') {
                stream++;
            }
            if (!isdigit(*stream)) {
                syntax_error(token.pos, "expected a digit but found '%c'", *stream);
            }
            while (isdigit(*stream)) {
                stream++;
            }
        }
    }

    double val = strtod(digit_stream, NULL);
    if (val == HUGE_VAL) {
        syntax_error(token.pos, "Float literal overflow");
    }
    token.kind = TOKEN_FLOAT;
    token.float_val = val;
}

char escape_to_char[] = {
    ['r'] = '\r', ['n'] = '\n', ['t'] = '\t', ['v'] = '\v',
    ['b'] = '\b', ['a'] = '\a', ['0'] = '\0',
};

char char_to_escape[] = {
    ['\r'] = 'r', ['\n'] = 'n', ['\t'] = 't', ['\v'] = 'v',
    ['\b'] = 'b', ['\a'] = 'a', ['\0'] = '0', ['"'] = '"',
};

void scan_char() {
    char val;
    assert(*stream == '\'');
    stream++;

    if (*stream == '\'') {
        syntax_error(token.pos, "char literal can not be empty");
    }

    if (*stream == '\n') {
        syntax_error(token.pos, "can not have new line in a char literal");
    }

    if (*stream == '\\') {
        stream++;
        val = escape_to_char[(int) *stream];
        if (val == 0 && *stream != '0') {
            syntax_error(token.pos, "Invalid char literal escape '\\%c'", *stream);
        }
    } else {
        val = *stream;
    }

    stream++;

    if (*stream != '\'') {
        syntax_error(token.pos, "Expected literal ' but instead got '%c'", *stream);
    } else {
        stream++;
    }

    token.kind = TOKEN_INT;
    token.mod = TOKENMOD_CHAR;
    token.int_val = val;
}

void scan_str() {
    assert(*stream == '"');
    stream++;

    char *str = NULL;
    char val;

    while (*stream && *stream != '"') {
        val = *stream;

        if (*stream == '\n') {
            syntax_error(token.pos, "String literals can not contain new line characters");
        }

        if (*stream == '\\') {
            stream++;
            if (*stream == '"') {
                val = *stream;
            } else {
                val = escape_to_char[(int) *stream];
                if (val == 0 && *stream != '0') {
                    syntax_error(token.pos, "invalid string literal escape '\\%c'", *stream);
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

void next_token() {
    bool is_atrate = false;

repeat:

    token.start = stream;
    token.mod = TOKENMOD_NONE;
    token.pos = (SrcPos){file_name, line_num};

    switch (*stream) {
    case ' ':
    case '\n':
    case '\r':
    case '\t':
    case '\v':
        while (isspace(*stream)) {
            if (*stream == '\n') {
                line_num++;
            }
            stream++;
        }
        goto repeat;
    case '\'':
        scan_char();
        break;
    case '"':
        scan_str();
        break;
    case '.':
        if (isdigit(stream[1])) {
            scan_float();
        } else {
            if (stream[1] == '.' && stream[2] == '.') {
                token.kind = TOKEN_ELLIPSIS;
                stream += 3;
            } else {
                token.kind = TOKEN_DOT;
                stream++;
            }
        }
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
        const char *digit_stream = stream;

        while (isdigit(*digit_stream)) {
            digit_stream++;
        }
        if (*digit_stream == '.' || tolower(*digit_stream) == 'e') {
            scan_float();
        } else {
            scan_int();
        }
        break;
    }
    case '@':
        is_atrate = true;
        stream++;
        /* stream++; */
        /* /\* const char *str_foreign; *\/ */
        /* /\* if (str_intern_range(&stream[1], &stream[1 + strlen("foreign")]) == keyword_foreign) { *\/ */
        /* /\*     token.name = keyword_foreign; *\/ */
        /* /\*     token.kind = TOKEN_KEYWORD; *\/ */
        /* /\*     stream += strlen("foreign") + 1; *\/ */
        /* /\* } else { *\/ */
        /* /\*     syntax_error(token.pos, "only foreign keyword allowed after @ symbol"); *\/ */
        /* /\* } *\/ */
        /* break; */

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
        while (isalnum(*stream) || *stream == '_') {
            stream++;
        }

        if (is_atrate) {
            token.start++;
            if (str_intern_range(token.start, stream) != keyword_foreign) {
                syntax_error(token.pos, "Only foreign keyword is allowed after @");
            }
            is_atrate = false;
        }

        token.name = str_intern_range(token.start, stream);
        token.kind = is_token_keyword(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
        
        break;

#define CASE1(a1, b)                                                                               \
    case a1:                                                                                       \
        token.kind = b;                                                                            \
        stream++;                                                                                  \
        break

        CASE1('\0', TOKEN_EOF);
        CASE1('(', TOKEN_LPAREN);
        CASE1(')', TOKEN_RPAREN);
        CASE1('[', TOKEN_LBRACKETS);
        CASE1(']', TOKEN_RBRACKETS);
        CASE1('{', TOKEN_LBRACES);
        CASE1('}', TOKEN_RBRACES);
        CASE1(',', TOKEN_COMMA);
        CASE1(';', TOKEN_SEMICOLON);
        CASE1('?', TOKEN_QUESTION);
        CASE1('~', TOKEN_NEG);

#undef CASE1

#define CASE2(c1, tk1, c2, tk2, c3, tk3)                                                           \
    case c1:                                                                                       \
        token.kind = tk1;                                                                          \
        stream++;                                                                                  \
        if (*stream == c2) {                                                                       \
            token.kind = tk2;                                                                      \
            stream++;                                                                              \
            break;                                                                                 \
        }                                                                                          \
        if (*stream == c3) {                                                                       \
            token.kind = tk3;                                                                      \
            stream++;                                                                              \
            break;                                                                                 \
        }                                                                                          \
        break

        CASE2('+', TOKEN_ADD, '+', TOKEN_INC, '=', TOKEN_ADD_ASSIGN);
        CASE2('-', TOKEN_SUB, '-', TOKEN_DEC, '=', TOKEN_SUB_ASSIGN);
        CASE2('|', TOKEN_OR, '|', TOKEN_OR_OR, '=', TOKEN_OR_ASSIGN);
        CASE2('&', TOKEN_AND, '&', TOKEN_AND_AND, '=', TOKEN_AND_ASSIGN);
#undef CASE2

#define CASE3(c1, tk1, c2, tk2)                                                                    \
    case c1:                                                                                       \
        token.kind = tk1;                                                                          \
        stream++;                                                                                  \
        if (*stream == c2) {                                                                       \
            token.kind = tk2;                                                                      \
            stream++;                                                                              \
        }                                                                                          \
        break

        CASE3('^', TOKEN_XOR, '=', TOKEN_XOR_ASSIGN);
        CASE3(':', TOKEN_COLON, '=', TOKEN_COLON_ASSIGN);
        CASE3('*', TOKEN_MUL, '=', TOKEN_MUL_ASSIGN);
        CASE3('/', TOKEN_DIV, '=', TOKEN_DIV_ASSIGN);
        CASE3('%', TOKEN_MOD, '=', TOKEN_MOD_ASSIGN);
        CASE3('=', TOKEN_ASSIGN, '=', TOKEN_EQ);
        CASE3('!', TOKEN_NOT, '=', TOKEN_NOTEQ);

#undef CASE3

    case '<':
        token.kind = TOKEN_LT;
        stream++;
        if (*stream == '=') {
            token.kind = TOKEN_LTEQ;
            stream++;
            break;
        }

        if (*stream == '<') {
            token.kind = TOKEN_LSHIFT;
            stream++;
            if (*stream == '=') {
                token.kind = TOKEN_LSHIFT_ASSIGN;
                stream++;
            }
        }
        break;

    case '>':
        token.kind = TOKEN_GT;
        stream++;
        if (*stream == '=') {
            token.kind = TOKEN_GTEQ;
            stream++;
            break;
        }

        if (*stream == '>') {
            token.kind = TOKEN_RSHIFT;
            stream++;
            if (*stream == '=') {
                token.kind = TOKEN_RSHIFT_ASSIGN;
                stream++;
            }
        }
        break;

    default:
        syntax_error(token.pos, "Invalid '%c' token, skipping", *stream);
        stream++;
        goto repeat;
    }

    token.end = stream;
}

bool is_keyword(const char *name) {
    return token.kind == TOKEN_KEYWORD && token.name == name;
}

bool is_token(TokenKind kind) {
    return token.kind == kind;
}

bool is_token_eof() {
    return is_token(TOKEN_EOF);
}

bool match_token(TokenKind kind) {
    if (token.kind == kind) {
        next_token();
        return true;
    }

    return false;
}

bool match_keyword(const char *keyword) {
    if (token.kind == TOKEN_KEYWORD && token.name == keyword) {
        next_token();
        return true;
    }

    return false;
}

static void init_stream(const char *path, const char *str) {
    path = (!path) ? "<anonymous>" : path;
    line_num = 1;
    file_name = path;
    stream = str;
    next_token();
}

const char *token_kind_name(TokenKind kind) {
    if (kind < sizeof(token_to_str) / sizeof(*token_to_str)) {
        return token_to_str[kind];
    }

    return "<unknown>";
}

const char *token_info() {
    if (token.kind == TOKEN_NAME || token.kind == TOKEN_KEYWORD) {
        return token.name;
    }

    return token_kind_name(token.kind);
}

bool expect_token(TokenKind kind) {
    if (is_token(kind)) {
        next_token();
        return true;
    } else {
        fatal("expected token '%s' got '%s'", token_kind_name(kind), token_info());
        return false;
    }
}

#define assert_token_int(val) assert(token.int_val == val && match_token(TOKEN_INT))
#define assert_token_float(val) assert(token.float_val == val && match_token(TOKEN_FLOAT))
#define assert_token_char(val)                                                                     \
    assert(token.int_val == val && token.mod == TOKENMOD_CHAR && match_token(TOKEN_INT))
#define assert_token_str(val) assert(strcmp(token.str_val, val) == 0 && match_token(TOKEN_STR))
#define assert_token_ident(val) assert(token.name == (val) && match_token(TOKEN_NAME))
#define assert_token_eof() assert(token.kind == TOKEN_EOF)

static void lex_test() {
    // identifier test
    init_keywords();
    init_stream(NULL, "hello123");
    assert_token_ident(str_intern("hello123"));
    assert_token_eof();

    // integer literal test
    init_stream(NULL, "123 0 23");
    assert_token_int(123);
    assert_token_int(0);
    assert_token_int(23);
    assert_token_eof();

    init_stream(NULL, "0");
    assert_token_int(0);

    /* floating point test */
    init_stream(NULL, "0xff 1.2 ! != ...");
    assert(token.mod == TOKENMOD_HEX);
    assert_token_int(255);
    assert_token_float(1.2);
    assert(match_token(TOKEN_NOT));
    assert(match_token(TOKEN_NOTEQ));
    assert(match_token(TOKEN_ELLIPSIS));
    assert_token_eof();

    // char literal test
    init_stream(NULL, "'\\n' 'a' ");
    assert_token_int('\n');
    assert_token_int('a');
    assert_token_eof();

    // string literal tests
    init_stream(NULL, "\"foo\" \"a\\n\"");
    assert_token_str("foo");
    assert_token_str("a\n");
    assert_token_eof();

    // expression test //
    init_stream(NULL, "a+b=c+d;");
    assert_token_ident(str_intern("a"));
    assert(match_token(TOKEN_ADD));
    assert_token_ident(str_intern("b"));
    assert(match_token(TOKEN_ASSIGN));

    assert_token_ident(str_intern("c"));
    assert(match_token(TOKEN_ADD));
    assert_token_ident(str_intern("d"));
    assert(match_token(TOKEN_SEMICOLON));
    assert_token_eof();

    // operator test //

    init_stream(NULL, ": := ++ += < << <= <<=");

    assert(match_token(TOKEN_COLON));
    assert(match_token(TOKEN_COLON_ASSIGN));
    assert(match_token(TOKEN_INC));
    assert(match_token(TOKEN_ADD_ASSIGN));
    assert(match_token(TOKEN_LT));
    assert(match_token(TOKEN_LSHIFT));
    assert(match_token(TOKEN_LTEQ));
    assert(match_token(TOKEN_LSHIFT_ASSIGN));

    printf("lex test passed\n");
}
