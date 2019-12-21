Expr *parse_expr();
Typespec *parse_type();
Stmt *parse_stmt();
StmtBlock parse_stmtblock();

const char *parse_name() {
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

EnumItem parse_decl_enum_item() {
    const char *name = parse_name();
    Expr *expr = NULL;

    if (match_token(TOKEN_ASSIGN)) {
        expr = parse_expr();
    }
    return (EnumItem){name, expr};
}

Decl *parse_decl_enum() {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    EnumItem *items = NULL;
    expect_token(TOKEN_LBRACES);

    if (!is_token(TOKEN_RBRACES)) {
        buf_push(items, parse_decl_enum_item());
    }

    while (match_token(TOKEN_COMMA)) {
        buf_push(items, parse_decl_enum_item());
    }

    expect_token(TOKEN_RBRACES);
    return decl_enum(name, pos, items, buf_len(items));
}

AggregateItem parse_decl_aggregate_item() {
    const char **names = NULL;
    const char *name = parse_name();

    buf_push(names, name);
    while (match_token(TOKEN_COMMA)) {
        buf_push(names, parse_name());
    }

    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    expect_token(TOKEN_SEMICOLON);

    return (AggregateItem){names, buf_len(names), type};
}

Decl *parse_decl_aggregate(AggregateKind kind) {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    AggregateItem *items = NULL;
    expect_token(TOKEN_LBRACES);

    if (!is_token(TOKEN_RBRACES)) {
        buf_push(items, parse_decl_aggregate_item());
    }

    while (!is_token(TOKEN_RBRACES)) {
        buf_push(items, parse_decl_aggregate_item());
    }

    expect_token(TOKEN_RBRACES);
    return decl_aggregate(kind, pos, name, items, buf_len(items));
}

Decl *parse_decl_var() {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    Typespec *type = NULL;
    Expr *expr = NULL;

    if (match_token(TOKEN_COLON)) {
        type = parse_type();
        if (match_token(TOKEN_ASSIGN)) {
            expr = parse_expr();
        }
    } else if (match_token(TOKEN_ASSIGN)) {
        expr = parse_expr();
    } else {
        syntax_error(pos, "var declaration expects a :type = expr or :type");
        return NULL;
    }

    return decl_var(name, pos, type, expr);
}

Decl *parse_decl_const() {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    return decl_const(name, pos, parse_expr());
}

FuncArg parse_decl_func_arg() {
    const char *name = parse_name();
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();

    return (FuncArg){name, type};
}

Decl *parse_decl_func(bool is_foreign) {
    SrcPos pos = token.pos;
    FuncArg *args = NULL;
    Typespec *ret_type = NULL;
    bool is_variadic = false;
    const char *name = parse_name();

    expect_token(TOKEN_LPAREN);
    if (is_token(TOKEN_NAME)) {
        buf_push(args, parse_decl_func_arg());

        while (match_token(TOKEN_COMMA)) {
            if (is_token(TOKEN_ELLIPSIS)) {
                is_variadic = true;
                next_token();
                break;
            }
            buf_push(args, parse_decl_func_arg());
        }
    }
    expect_token(TOKEN_RPAREN);
    if (match_token(TOKEN_COLON)) {
        ret_type = parse_type();
    }

    StmtBlock block = parse_stmtblock();
    return decl_func(name, pos, args, buf_len(args), ret_type, block, is_foreign, is_variadic);
}

Decl *parse_decl_typedef() {
    SrcPos pos = token.pos;
    const char *name = parse_name();
    expect_token(TOKEN_ASSIGN);
    Typespec *type = parse_type();
    return decl_typedef(name, pos, type);
}

Decl *parse_decl_opt() {
    bool foreign_decl = false;
    if (match_keyword(keyword_enum)) {
        return parse_decl_enum();
    } else if (match_keyword(keyword_struct)) {
        return parse_decl_aggregate(AGGREGATE_STRUCT);
    } else if (match_keyword(keyword_union)) {
        return parse_decl_aggregate(AGGREGATE_UNION);
    } else if (match_keyword(keyword_var)) {
        return parse_decl_var();
    } else if (match_keyword(keyword_const)) {
        return parse_decl_const();
    } else if (match_keyword(keyword_func)) {
        return parse_decl_func(false);
    } else if (match_keyword(keyword_typedef)) {
        return parse_decl_typedef();
    } else if (match_keyword(keyword_foreign)) {
        if (match_keyword(keyword_func)) {
            return parse_decl_func(true);
        }
    }

    return NULL;
}

Decl *parse_decl() {
    Decl *decl = parse_decl_opt();

    if (!decl && !is_token_eof()) {
        SrcPos pos = token.pos;
        syntax_error(pos, "Expected declaration keyword, got %s", token_info());
    }

    return decl;
}

CompoundVal *parse_compound_val() {
    if (match_token(TOKEN_LBRACKETS)) {
        Expr *index = parse_expr();
        expect_token(TOKEN_RBRACKETS);
        expect_token(TOKEN_ASSIGN);
        Expr *val = parse_expr();
        return compound_index(index, val);
    }

    Expr *expr = parse_expr();
    if (match_token(TOKEN_ASSIGN)) {
        if (expr->kind != EXPR_NAME) {
            syntax_error(expr->pos,
                         "Inside Compound literals value can be assigned to named fields only");
        }
        Expr *val = parse_expr();
        return compound_name(expr->name, val);
    }

    return compound_simple(expr);
}

/* struct Vector { */
/*      a:int; */
/*      b:int; */
/* }
/* var a = Vector{b=10}
/*
/* var b = int[256] = {1,2,3,['a']=50, 100}
*/
Expr *parse_expr_compound(Typespec *type) {
    SrcPos pos = token.pos;
    CompoundVal **args = NULL;
    expect_token(TOKEN_LBRACES);
    if (!is_token(TOKEN_RBRACES)) {
        buf_push(args, parse_compound_val());
    }

    while (match_token(TOKEN_COMMA)) {
        buf_push(args, parse_compound_val());
    }
    expect_token(TOKEN_RBRACES);
    return expr_compound(pos, type, args, buf_len(args));
}

Expr *parse_simple_expr() {
    SrcPos pos = token.pos;
    if (is_token(TOKEN_INT)) {
        uint64_t val = token.int_val;
        next_token();
        return expr_int(pos, val);
    } else if (is_token(TOKEN_FLOAT)) {
        double val = token.float_val;
        next_token();
        return expr_float(pos, val);
    } else if (is_token(TOKEN_STR)) {
        const char *str = token.str_val;
        next_token();
        return expr_str(pos, str);
    } else if (match_keyword(keyword_sizeof)) {
        expect_token(TOKEN_LPAREN);
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return expr_sizeof_type(pos, type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return expr_sizeof_expr(pos, expr);
        }
    } else if (match_keyword(keyword_cast)) {
        expect_token(TOKEN_LPAREN);
        Typespec *type = parse_type();
        expect_token(TOKEN_COMMA);
        Expr *expr = parse_expr();
        expect_token(TOKEN_RPAREN);
        return expr_cast(pos, type, expr);
    } else if (is_token(TOKEN_NAME)) {
        const char *name = parse_name();
        if (is_token(TOKEN_LBRACES)) {
            return parse_expr_compound(typespec_name(pos, name));
        } else {
            return expr_name(pos, name);
        }
    } else if (match_token(TOKEN_LPAREN)) {
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            return parse_expr_compound(type);
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return expr;
        }
    } else if (is_token(TOKEN_LBRACES)) {
        return parse_expr_compound(NULL);
    } else {
        syntax_error(pos, "Unexpected token %s in expression", token_info());
        return NULL;
    }
}

Expr *parse_expr_call(Expr *expr) {
    SrcPos pos = token.pos;
    Expr **args = NULL;

    expect_token(TOKEN_LPAREN);

    if (!is_token(TOKEN_RPAREN)) {
        buf_push(args, parse_expr());
    }

    while (match_token(TOKEN_COMMA)) {
        buf_push(args, parse_expr());
    }

    expect_token(TOKEN_RPAREN);
    return expr_call(pos, expr, args, buf_len(args));
}

Expr *parse_expr_field(Expr *expr) {
    SrcPos pos = token.pos;
    expect_token(TOKEN_DOT);
    const char *field = parse_name();
    return expr_field(pos, expr, field);
}

Expr *parse_expr_index(Expr *expr) {
    SrcPos pos = token.pos;
    expect_token(TOKEN_LBRACKETS);
    Expr *index = parse_expr();
    expect_token(TOKEN_RBRACKETS);
    return expr_index(pos, expr, index);
}

Expr *parse_expr_misc() {
    SrcPos pos = token.pos;
    Expr *expr = parse_simple_expr();

    for (;;) {
        if (is_token(TOKEN_DOT)) {
            expr = parse_expr_field(expr);
        } else if (is_token(TOKEN_LPAREN)) {
            expr = parse_expr_call(expr);
        } else if (is_token(TOKEN_LBRACKETS)) {
            expr = parse_expr_index(expr);
        } else {
            break;
        }
    }

    return expr;
}

bool is_unary_op() {
    // +,-,*,&
    return is_token(TOKEN_ADD) || is_token(TOKEN_SUB) || is_token(TOKEN_MUL) ||
           is_token(TOKEN_AND) || is_token(TOKEN_NEG) || is_token(TOKEN_NOT);
}

Expr *parse_expr_unary() {
    SrcPos pos = token.pos;
    if (is_unary_op()) {
        TokenKind op = token.kind;
        next_token();
        return expr_unary(pos, op, parse_expr_unary());
    }

    return parse_expr_misc();
}

bool is_mul_op() {
    return token.kind >= TOKEN_FIRST_MUL && token.kind <= TOKEN_LAST_MUL;
}

Expr *parse_expr_mul() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr_unary();

    while (is_mul_op()) {
        TokenKind op = token.kind;
        next_token();
        Expr *right_expr = parse_expr_misc();
        expr = expr_binary(pos, expr, op, right_expr);
    }

    return expr;
}

bool is_add_op() {
    return (token.kind >= TOKEN_FIRST_ADD && token.kind <= TOKEN_LAST_ADD);
}

Expr *parse_expr_add() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr_mul();

    while (is_add_op()) {
        TokenKind op = token.kind;
        next_token();
        Expr *right_expr = parse_expr_mul();
        expr = expr_binary(pos, expr, op, right_expr);
    }

    return expr;
}

bool is_cmp_op() {
    return (token.kind >= TOKEN_FIRST_EQ && token.kind <= TOKEN_LAST_EQ);
}

Expr *parse_expr_cmp() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr_add();

    while (is_cmp_op()) {
        TokenKind op = token.kind;
        next_token();
        Expr *right_expr = parse_expr_add();
        expr = expr_binary(pos, expr, op, right_expr);
    }

    return expr;
}

Expr *parse_expr_and() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr_cmp();

    while (match_token(TOKEN_AND_AND)) {
        Expr *right_expr = parse_expr_cmp();
        expr = expr_binary(pos, expr, TOKEN_AND_AND, right_expr);
    }

    return expr;
}

Expr *parse_expr_or() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr_and();

    while (match_token(TOKEN_OR_OR)) {
        Expr *right_expr = parse_expr_and();
        expr = expr_binary(pos, expr, TOKEN_OR_OR, right_expr);
    }

    return expr;
}

Expr *parse_expr_ternary() {
    SrcPos pos = token.pos;
    Expr *cond_expr = parse_expr_or();

    if (match_token(TOKEN_QUESTION)) {
        Expr *then_expr = parse_expr();
        expect_token(TOKEN_COLON);
        Expr *else_expr = parse_expr();
        return expr_ternary(pos, cond_expr, then_expr, else_expr);
    } else {
        return cond_expr;
    }
}

Expr *parse_expr() {
    return parse_expr_ternary();
}

Typespec *parse_type_func() {
    SrcPos pos = token.pos;
    Typespec **args = NULL, *ret = NULL;
    expect_token(TOKEN_LPAREN);

    if (!is_token(TOKEN_RPAREN)) {
        buf_push(args, parse_type());
    }

    while (match_token(TOKEN_COMMA)) {
        buf_push(args, parse_type());
    }

    expect_token(TOKEN_RPAREN);
    if (match_token(TOKEN_COLON)) {
        ret = parse_type();
    }
    return typespec_func(pos, args, buf_len(args), ret);
}

Typespec *parse_type_base() {
    SrcPos pos = token.pos;
    if (is_token(TOKEN_NAME)) {
        const char *name = parse_name();
        return typespec_name(pos, name);
    } else if (match_keyword(keyword_func)) {
        return parse_type_func();
    } else if (match_token(TOKEN_LPAREN)) {
        Typespec *type = parse_type();
        expect_token(TOKEN_RPAREN);
        return type;
    } else {
        syntax_error(pos, "Unexpected token %s in type", token_info());
        return NULL;
    }
}

Typespec *parse_type() {
    SrcPos pos = token.pos;
    Typespec *type = parse_type_base();

    for (;;) {
        if (match_token(TOKEN_MUL)) {
            type = typespec_ptr(pos, type);
        } else if (match_token(TOKEN_LBRACKETS)) {
            Expr *expr = NULL;
            if (!is_token(TOKEN_RBRACKETS)) {
                expr = parse_expr();
            }
            expect_token(TOKEN_RBRACKETS);
            type = typespec_array(pos, type, expr);
        } else {
            break;
        }
    }

    return type;
}

bool is_assign_op() {
    return token.kind >= TOKEN_FIRST_ASSIGN && token.kind <= TOKEN_LAST_ASSIGN;
}

Stmt *parse_simple_stmt() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr();

    if (match_token(TOKEN_COLON_ASSIGN)) {
        if (expr->kind == EXPR_NAME) {
            return stmt_init(pos, expr->name, parse_expr());
        } else {
            syntax_error(expr->pos, "Expected a name before the := operator");
        }
    } else if (is_assign_op()) {
        TokenKind op = token.kind;
        next_token();
        return stmt_assign(pos, expr, op, parse_expr());
    } else if (is_token(TOKEN_INC) || is_token(TOKEN_DEC)) {
        TokenKind op = token.kind;
        next_token();
        return stmt_assign(pos, expr, op, NULL);
    }

    return stmt_expr(pos, expr);
}

StmtBlock parse_stmtblock() {
    expect_token(TOKEN_LBRACES);

    Stmt **stmts = NULL;
    Stmt *stmt = NULL;

    while (!is_token(TOKEN_RBRACES)) {
        buf_push(stmts, parse_stmt());
    }

    expect_token(TOKEN_RBRACES);
    return (StmtBlock){stmts, buf_len(stmts)};
}

Expr *parse_paren_expr() {
    expect_token(TOKEN_LPAREN);
    Expr *expr = parse_expr();
    expect_token(TOKEN_RPAREN);
    return expr;
}

ElseIf parse_else_if() {
    Expr *expr = parse_paren_expr();
    return (ElseIf){expr, parse_stmtblock()};
}

void print_expr(Expr *);

Stmt *parse_stmt_if() {
    SrcPos pos = token.pos;
    Expr *expr = parse_paren_expr();
    StmtBlock if_block = parse_stmtblock(), else_block = {0};
    ElseIf *else_ifs = NULL;

    for (;;) {
        if (match_keyword(keyword_else)) {
            if (match_keyword(keyword_if)) {
                buf_push(else_ifs, parse_else_if());
            } else {
                else_block = parse_stmtblock();
                break;
            }
        } else {
            break;
        }
    }

    return stmt_if(pos, expr, if_block, else_ifs, buf_len(else_ifs), else_block);
}

Stmt *parse_stmt_for() {
    SrcPos pos = token.pos;
    expect_token(TOKEN_LPAREN);
    Stmt *init = NULL;

    if (!match_token(TOKEN_SEMICOLON)) {
        init = parse_simple_stmt();
        expect_token(TOKEN_SEMICOLON);
    }

    Expr *expr = NULL;

    if (!match_token(TOKEN_SEMICOLON)) {
        expr = parse_expr();
        expect_token(TOKEN_SEMICOLON);
    }

    Stmt *next = NULL;
    if (!is_token(TOKEN_RPAREN)) {
        next = parse_simple_stmt();
        if (next->kind == STMT_INIT) {
            syntax_error(next->pos,
                         "init statements are not allowed in for-statement's next clause");
        }
    }

    expect_token(TOKEN_RPAREN);
    StmtBlock block = parse_stmtblock();
    return stmt_for(pos, init, expr, next, block);
}

Stmt *parse_stmt_do_while() {
    SrcPos pos = token.pos;
    StmtBlock block = parse_stmtblock();

    if (!match_keyword(keyword_while)) {
        syntax_error(pos, "expected while after do block");
    }

    Stmt *stmt = stmt_do_while(pos, block, parse_paren_expr());
    expect_token(TOKEN_SEMICOLON);
    return stmt;
}

Stmt *parse_stmt_while() {
    SrcPos pos = token.pos;
    Expr *expr = parse_paren_expr();
    StmtBlock block = parse_stmtblock();
    return stmt_while(pos, expr, block);
}

SwitchCase parse_switch_case() {
    bool is_default = false;
    Expr **exprs = NULL;

    while (1) {
        if (match_keyword(keyword_case)) {
            buf_push(exprs, parse_expr());
            expect_token(TOKEN_COLON);
            continue;
        }

        if (match_keyword(keyword_default)) {
            if (is_default) {
                SrcPos pos = token.pos;
                syntax_error(pos, "Duplicate default labels in the switch clause");
            }

            is_default = true;
            expect_token(TOKEN_COLON);
            continue;
        }

        break;
    }

    Stmt **stmts = NULL;

    while (!is_keyword(keyword_default) && !is_keyword(keyword_case) && !is_token(TOKEN_RBRACES)) {
        buf_push(stmts, parse_stmt());
    }

    return (SwitchCase){exprs, buf_len(exprs), is_default, (StmtBlock){stmts, buf_len(stmts)}};
}

Stmt *parse_stmt_switch() {
    SrcPos pos = token.pos;
    Expr *expr = parse_expr();
    SwitchCase *cases = NULL;
    bool is_default = false;

    expect_token(TOKEN_LBRACES);

    while (is_keyword(keyword_case) || is_keyword(keyword_default)) {
        if (is_keyword(keyword_default)) {
            if (is_default) {
                SrcPos pos = token.pos;
                syntax_error(pos, "Duplicate default labels in the switch clause");
            } else {
                is_default = true;
            }
        }

        buf_push(cases, parse_switch_case());
    }

    expect_token(TOKEN_RBRACES);
    return stmt_switch(pos, expr, cases, buf_len(cases));
}

Stmt *parse_stmt() {
    SrcPos pos = token.pos;
    Decl *decl = parse_decl_opt();

    // expressions are parses as simple stmts
    if (decl) {
        return stmt_decl(pos, decl);
    } else {
        if (match_keyword(keyword_if)) {
            return parse_stmt_if();
        } else if (match_keyword(keyword_for)) {
            return parse_stmt_for();
        } else if (match_keyword(keyword_do)) {
            return parse_stmt_do_while();
        } else if (match_keyword(keyword_while)) {
            return parse_stmt_while();
        } else if (match_keyword(keyword_switch)) {
            return parse_stmt_switch();
        } else if (match_keyword(keyword_return)) {
            Expr *expr = NULL;
            if (!is_token(TOKEN_SEMICOLON)) {
                expr = parse_expr();
            }
            expect_token(TOKEN_SEMICOLON);
            return stmt_return(pos, expr);
        } else if (match_keyword(keyword_break)) {
            expect_token(TOKEN_SEMICOLON);
            return stmt_break(pos);
        } else if (match_keyword(keyword_continue)) {
            expect_token(TOKEN_SEMICOLON);
            return stmt_continue(pos);
        } else if (is_token(TOKEN_LBRACES)) {
            return stmt_block(pos, parse_stmtblock());
        } else {
            Stmt *stmt = parse_simple_stmt();
            expect_token(TOKEN_SEMICOLON);
            return stmt;
        }
    }
}

DeclSet *parse_file() {
    Decl **decls = NULL;
    Decl *d;

    while (d = parse_decl()) {
        buf_push(decls, d);
    }

    return decl_set(decls, buf_len(decls));
}
