

Type *parse_type_func()
{
        Typespec **args = NULL;
        Typespec *ret_type = NULL;
        expect_token(TOKEN_LPAREN);

        for (;;) {
                buf_push(args, parse_type());
                if (match_token(TOKEN_RPAREN)) {
                        break;
                }
                expect_token(TOKEN_COMMA);
        }
        if (match_token(TOKEN_COLON)) {
                ret_type = parse_type();
        }
        return type_func(args, buf_len(args), ret_type);
}

Type *parse_type_name()
{
        const char *name = parse_name();
        return type_name(name);
}

Type *parse_type_pointer(Type *type)
{
        if (match_token(TOKEN_MUL)) {
                return type_pointer(type);
        }

        return NULL;
}

Type *parse_type_array(Type *type)
{
        if (match_token(TOKEN_LBRACKET)) {

                
                expect_token(TOKEN_RBRACKET);
        }

        return NULL;
}

Type *parse_type()
{
        Type *type = NULL;
        const char *name = NULL;

        if (is_token(TOKEN_NAME)) {
                type = parse_type_name();
        } else if (match_keyword(KEYWORD_FUNC)) {
                type = parse_type_func();
        } else if (match_token(TOKEN_LPAREN)) {
                type = parse_type();
                expect_token(TOKEN_RPAREN);
        }

        if (!type) {
                syntax_error("Expected to find a type");
        }

        for (;;) {
                Type *pointer_type, *array_type;

                pointer_type = parse_type_pointer(type);
                if (pointer_type) {
                        type = pointer_type;
                }
                array_type = parse_type_array(type);
                if (array_type) {
                        type = array_type;
                }

                if (!pointer_type && !array_type) {
                        break;
                }
        }

        return type;
}

const char *parse_name()
{
        const char *name = token.name;
        expect_token(TOKEN_NAME);
        return name;
}

EnumItem parse_decl_enum_item()
{
        const char *name = parse_name();
        Expr *init = NULL;

        if (is_token(TOKEN_EQUAL)) {
                init = parse_expr();
        }

        return (EnumItem){name, init};
}

Decl *parse_decl_enum()
{
        const char *enum_name = parse_name();
        expect_token(TOKEN_LBRACE);
        EnumItem *items = NULL;

        for (;;) {
                buf_push(items, parse_decl_enum_item());
                if (match_token(TOKEN_RBRACE)) {
                        break;
                }

                expect_token(TOKEN_COMMA);
        }

        return decl_enum(enum_name, items, buf_len(items));
}

AggregateItem parse_aggregate_items()
{
        const char **names = NULL;
        Type *type = NULL;

        for (;;) {
                buf_push(names, parse_name());
                if (match_token(TOKEN_COLON)) {
                        type = parse_type();
                        expect_token(TOKEN_SEMICOLON);
                        break;
                }

                expect_token(TOKEN_COMMA);
        }

        (AggregateItem){names, buf_len(names), type};
}

Decl *parse_decl_aggregate(DeclKind decl_kind)
{
        const char *decl_name = parse_name();
        expect_token(TOKEN_LBRACE);
        AggregateItem *items = NULL;

        for (;;) {
                buf_push(items, parse_aggregate_items());
                if (match_token(TOKEN_RBRACE)) {
                        break;
                }
        }
        return decl_aggregate(decl_kind, decl_name, items, buf_len(items));
}

Decl *parse_decl_var()
{
        const char *name = parse_name();
        Type *type = NULL;
        Expr *expr = NULL;

        if (match_token(TOKEN_COLON)) {
                type = parse_type();

                if (match_token(TOKEN_EQUAL)) {
                        expr = parse_expr();
                }
        } else {
                expect_token(TOKEN_EQUAL);
                expr = parse_expr();
        }

        return decl_var(name, type, expr);
}

Decl *parse_decl_const()
{
        const char *name = parse_name();
        expect_token(TOKEN_EQUAL);
        return decl_const(name, parse_expr());
}

FuncParam parse_func_param()
{
        const char *name = parse_name();
        Type *type = parse_type();

        return (FuncParam){name, type};
}

Decl *parse_decl_func()
{
        const char *func_name = parse_name();
        FuncParam *params = NULL;
        Type *ret_type = NULL;
        expect_token(TOKEN_LPAREN);
        for (;;) {
                buf_push(params, parse_func_param());
                if (match_token(TOKEN_RPAREN)) {
                        break;
                }
                expect_token('TOKEN_COMMA');
        }
        expect_token(TOKEN_COLON);
        ret_type = parse_type();

        return decl_func(func_name, params, buf_len(params), ret_type, parse_stmt_block());
}

Decl *parse_decl_typedef()
{
        const char *name = parse_name();
        expect_token(TOKEN_EQUAL);
        return decl_typedef(name, parse_type());
}

Decl *parse_decl()
{
        if (!is_token(TOKEN_KEYWORD)) {
                return NULL
        } else if (match_keyword(KEYWORD_ENUM)) {
                return parse_enum();
        } else if (match_keyword(KEYWORD_STRUCT)) {
                return parse_decl_aggregate(DECL_STRUCT);
        } else if (match_keyword(KEYWORD_UNION)) {
                return parse_decl_aggregate(DECL_UNION)
        } else if (match_keyword(KEYWORD_VAR)) {
                return parse_decl_var();
        } else if (match_keyword(KEYWORD_CONST)) {
                return parse_decl_const();
        } else if (match_keyword(KEYWORD_FUNC)) {
                return parse_decl_func();
        } else if (match_keyword(KEYWORD_TYPEDEF)) {
                return parse_decl_typedef();
        }
        return NULL;
}

