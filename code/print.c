void print_stmt(Stmt *);
void print_decl(Decl *);
void print_expr(Expr *);

int indent;

void print_newline() {
    printf("\n%.*s", 2 * indent, "                                                 ");
}

void print_typespec(Typespec *type) {
    switch (type->kind) {
    case TYPESPEC_NONE:
        break;
    case TYPESPEC_NAME:
        printf("%s", type->name);
        break;
    case TYPESPEC_FUNC:
        printf("(func (");
        for (Typespec **args = type->func.args; args != type->func.args + type->func.num_args;
             args++) {
            printf(" ");
            print_typespec(*args);
        }
        printf(" ) ");
        if (type->func.ret) {
            print_typespec(type->func.ret);
        }
        printf(")");
        break;
    case TYPESPEC_ARRAY:
        printf("(array ");
        print_typespec(type->array.type);
        printf(" ");
        if (type->array.expr) {
            print_expr(type->array.expr);
        }
        printf(")");
        break;
    case TYPESPEC_PTR:
        printf("(ptr ");
        print_typespec(type->ptr.type);
        printf(")");
        break;
    }
}

void print_expr(Expr *expr) {
    switch (expr->kind) {
    case EXPR_NONE:
        assert(0);
        break;
    case EXPR_INT:
        printf("%lu", expr->int_val);
        break;
    case EXPR_FLOAT:
        printf("%lf", expr->float_val);
        break;
    case EXPR_STR:
        printf("\"%s\"", expr->str_val);
        break;
    case EXPR_NAME:
        printf("%s", expr->name);
        break;
    case EXPR_FIELD:
        printf("(field ");
        print_expr(expr->field_expr.expr);
        printf(" %s)", expr->field_expr.name);
        break;
    case EXPR_INDEX:
        printf("(index ");
        print_expr(expr->index_expr.expr);
        printf(" ");
        print_expr(expr->index_expr.index);
        printf(")");
        break;
    case EXPR_CALL:
        printf("(");
        print_expr(expr->call_expr.expr);
        for (Expr **args = expr->call_expr.args;
             args != expr->call_expr.args + expr->call_expr.num_args; args++) {
            printf(" ");
            print_expr(*args);
        }
        printf(")");
        break;
    case EXPR_UNARY:
        printf("(");
        printf("%s", token_kind_name(expr->unary_expr.op));
        print_expr(expr->unary_expr.expr);
        printf(")");
        break;
    case EXPR_BINARY:
        printf("(");
        print_expr(expr->binary_expr.left);
        printf(" %s ", token_kind_name(expr->binary_expr.op));
        print_expr(expr->binary_expr.right);
        printf(")");
        break;
    case EXPR_TERNARY:
        printf("(? ");
        print_expr(expr->ternary_expr.eval);
        printf(" ");
        print_expr(expr->ternary_expr.then_expr);
        printf(" ");
        print_expr(expr->ternary_expr.else_expr);
        printf(")");
        break;
    case EXPR_CAST:
        printf("(cast ");
        print_typespec(expr->cast_expr.type);
        printf(" ");
        print_expr(expr->cast_expr.expr);
        printf(")");
        break;
    case EXPR_COMPOUND:
        printf("(compound ");
        if (expr->compound_expr.type) {
            print_typespec(expr->compound_expr.type);
        } else {
            printf("nil");
        }
        for (CompoundVal **args = expr->compound_expr.args;
             args != expr->compound_expr.args + expr->compound_expr.num_args; args++) {
            printf(" ");
            switch ((*args)->kind) {
            case SIMPLE_EXPR:
                print_expr((*args)->expr);
                break;
            case INDEX_EXPR:
                printf("(index ");
                print_expr((*args)->index.index);
                printf(" ");
                print_expr((*args)->index.val);
                printf(")");
                break;
            case NAME_EXPR:
                printf("(name ");
                printf("%s ", (*args)->name.name);
                print_expr((*args)->name.val);
                printf(")");
                break;
            default:
                assert(0);
            }
        }
        printf(")");
        break;
    case EXPR_SIZEOF_TYPE:
        printf("(sizeof_type ");
        print_typespec(expr->sizeof_type);
        printf(")");
        break;
    case EXPR_SIZEOF_EXPR:
        printf("(sizeof_expr ");
        print_expr(expr->sizeof_expr);
        break;
    }
}

void print_aggregate_item(AggregateItem *item) {
    printf("(");
    print_typespec(item->type);
    for (const char **names = item->names; names != item->names + item->num_items; names++) {
        printf(" %s", *names);
    }
    printf(")");
}

void print_stmtblock(StmtBlock block) {
    printf("(block");
    indent++;
    for (Stmt **stmts = block.stmts; stmts != block.stmts + block.num_stmts; stmts++) {
        print_newline();
        print_stmt(*stmts);
    }
    indent--;
    printf(")");
}

void print_stmt(Stmt *stmt) {
    switch (stmt->kind) {
    case STMT_NONE:
        assert(0);
        break;
    case STMT_DECL:
        print_decl(stmt->decl);
        break;
    case STMT_RETURN:
        printf("(return ");
        if (stmt->stmt_return.expr) {
            print_expr(stmt->stmt_return.expr);
        } else {
            printf("nil");
        }
        printf(")");
        break;
    case STMT_BREAK:
        printf("(break)");
        break;
    case STMT_CONTINUE:
        printf("(continue)");
        break;
    case STMT_BLOCK:
        print_stmtblock(stmt->block);
        break;
    case STMT_IF:
        printf("(if ");
        print_expr(stmt->stmt_if.expr);
        indent++;
        print_newline();
        print_stmtblock(stmt->stmt_if.if_block);
        indent--;
        printf(")");
        print_newline();
        for (ElseIf *else_if = stmt->stmt_if.else_ifs;
             else_if != stmt->stmt_if.else_ifs + stmt->stmt_if.num_elseifs; else_if++) {
            printf("(else if ");
            print_expr(else_if->expr);
            indent++;
            print_newline();
            print_stmtblock(else_if->block);
            indent--;
            printf(")");
        }
        print_newline();
        printf("(else ");
        indent++;
        print_newline();
        print_stmtblock(stmt->stmt_if.else_block);
        printf(")");
        indent--;
        break;
    case STMT_WHILE:
        printf("(while ");
        print_expr(stmt->stmt_while.expr);
        indent++;
        print_newline();
        print_stmtblock(stmt->stmt_while.block);
        indent--;
        printf(")");
        break;
    case STMT_DO_WHILE:
        printf("(do ");
        indent++;
        print_newline();
        print_stmtblock(stmt->stmt_while.block);
        indent--;
        print_newline();
        printf(" while ");
        print_expr(stmt->stmt_while.expr);
        printf(")");
        break;
    case STMT_FOR:
        printf("(for ");
        print_stmt(stmt->stmt_for.init);
        print_expr(stmt->stmt_for.cond);
        print_stmt(stmt->stmt_for.next);
        indent++;
        print_newline();
        print_stmtblock(stmt->stmt_for.block);
        indent--;
        printf(")");
        break;
    case STMT_ASSIGN:
        printf("(");
        print_expr(stmt->stmt_assign.left_expr);
        printf("%s", token_kind_name(stmt->stmt_assign.op));
        if (stmt->stmt_assign.right_expr) {
            print_expr(stmt->stmt_assign.right_expr);
        }
        printf(")");
        break;
    case STMT_INIT:
        printf("(:= %s ", stmt->stmt_init.name);
        print_expr(stmt->stmt_init.expr);
        printf(")");
        break;
    case STMT_SWITCH:
        printf("(switch ");
        print_expr(stmt->stmt_switch.expr);
        indent++;
        for (SwitchCase *cases = stmt->stmt_switch.cases;
             cases != stmt->stmt_switch.cases + stmt->stmt_switch.num_cases; cases++) {
            print_newline();
            if (cases->is_default) {
                printf("(default");
            } else {
                for (Expr **expr = stmt->stmt_switch.cases->expr;
                     expr != stmt->stmt_switch.cases->expr + stmt->stmt_switch.cases->num_exprs;
                     expr++) {
                    printf("(case ");
                    print_expr(*expr);
                    print_newline();
                }
            }
            indent++;
            print_newline();
            print_stmtblock(cases->block);
            indent--;
            printf(")");
        }
        indent--;

        break;
    case STMT_EXPR:
        print_expr(stmt->expr);
        break;
    }
}

void print_decl(Decl *decl) {
    switch (decl->kind) {
    case DECL_NONE:
        assert(0);
        break;
    case DECL_ENUM:
        printf("(enum %s", decl->name);
        indent++;
        for (EnumItem *item = decl->enum_decl.items;
             item != decl->enum_decl.items + decl->enum_decl.num_enum_items; item++) {
            print_newline();
            printf("(%s ", item->name);
            if (item->expr != NULL) {
                print_expr(item->expr);
            } else {
                printf("nil");
            }
            printf(")");
        }
        indent--;
        printf(")");
        break;
    case DECL_AGGREGATE:
        assert(decl->aggregate_decl.kind != AGGREGATE_NONE);
        if (decl->aggregate_decl.kind == AGGREGATE_STRUCT) {
            printf("(struct %s", decl->name);
        } else {
            printf("(union %s", decl->name);
        }
        indent++;
        for (AggregateItem *item = decl->aggregate_decl.items;
             item != decl->aggregate_decl.items + decl->aggregate_decl.num_items; item++) {
            print_newline();
            print_aggregate_item(item);
        }
        indent--;
        printf(")");
        break;
    case DECL_VAR:
        printf("(var %s ", decl->name);
        if (decl->var_decl.type != NULL) {
            print_typespec(decl->var_decl.type);
        } else {
            print_expr(decl->var_decl.expr);
        }
        printf(")");
        break;
    case DECL_CONST:
        printf("(const %s ", decl->name);
        print_expr(decl->const_decl.expr);
        printf(")");
        break;
    case DECL_FUNC:
        printf("(func %s ", decl->name);
        printf("(");
        for (FuncArg *args = decl->func_decl.args;
             args != decl->func_decl.args + decl->func_decl.num_func_args; args++) {
            printf(" %s ", args->name);
            print_typespec(args->type);
        }
        printf(") ");

        if (decl->func_decl.type) {
            print_typespec(decl->func_decl.type);
        } else {
            printf("nil");
        }
        indent++;
        print_newline();
        print_stmtblock(decl->func_decl.block);
        indent--;
        break;
    case DECL_TYPEDEF:
        printf("(typedef %s ", decl->name);
        print_typespec(decl->typedef_decl.type);
        printf(")");
        break;
    }
}

void print_decl_test() {
    const char *decl[] = {
        "enum Color { RED, BLUE=1, GREEN }",
        "var foo :int*",
        "var foo = \"hello world\"",
        "var foo  = 1+2",
        "const foo = 10",
        "var bar1 = ~5",
        "var bar2 = !5",
        "struct Vector { x,y : float; z,a,c :int;}",
        "union Vector { x,y : float; f:float;}",
        "func bar(i :int) {printf(\"\");}",
        "typedef Decl = decl*",
        "const n = sizeof(:int*[16])",
        "const n = sizeof(1+2)",
        "var x = b == 1 ? 1+2 : 3-4",
        "func fact(n: int): int { trace(\"fact\"); if (n==0){return 1;} else {return n * "
        "fact(n-1);}}",
        "func fact(n :int) :int { p := 1; for (i := 1 ; i <= n; i++) { p *= i; } return p; }",
        "var foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r:0",
        "func f(x: int): bool { switch(x) { case 0: case 1: return true; case 2: default: return "
        "false;} }",
        "const pi = 3.14",
        "var v = Vector{1.0, -1.0}",
        "var v :Vector = {1.0, -1.0}",
        "var n = v.a.b",
        "typedef Vectors = Vector[1+2]",
        "func f() { do { print(42); } while (1); }",
        "typedef T = (func(int):int)[16]",
        "func f() { enum E {A,B,C} return; }",
        "func f() { if (1) { return 1;} else if (2) {return 2;} else {return 3;}}",
        "typedef cmplx = int***[16]",
        "var vs = int[2] {[0] = 1}",
        "typedef Ftest1 = (func() :(func():int)[10])*",
    };

    for (size_t i = 0; i < sizeof(decl) / sizeof(*decl); i++) {
        init_stream(decl[i]);
        Decl *d = parse_decl_opt();
        print_decl(d);
        printf("\n");
    }
}

void print_stmt_test() {
}

void print_expr_test() {
}

/*
    "var foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r:0",

(var foo (? a (((((a&b)+(c<<d))+(e*f))==(((((+u)-v)-w)+((*g)/(h x y)))+((-i)%(index k
x))))&&(m<=((n*(p+q))/r))) 0))

if (i == 0) {
} else if () {
} else if () {
} else {
}

(if ()
  (
      print();
  )
  (else if ()
     (
     ))
  (else if ()
     (
     ))
  (else
     (
     )))
 */
