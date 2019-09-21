typedef struct Type Type;

void order_decl(Decl *decl);
Type *create_type(Typespec *type);
void complete_type(Type *type);
typedef enum {
    SYM_UNRESOLVED,
    SYM_RESOLVING,
    SYM_RESOLVED,
} SymState;

typedef enum SymKind {
    SYM_NONE,
    SYM_VAR,
    SYM_CONST,
    SYM_FUNC,
    SYM_TYPE,
} SymKind;

typedef struct Sym {
    const char *name;
    SymKind kind;
    Decl *decl;
    SymState state;
    Type *type;
    union {
        int64_t val;
    };
} Sym;

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_INT,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_FUNC,
} TypeKind;

Type *type_alloc(TypeKind);

typedef enum TypeState {
    TYPE_UNRESOLVED,
    TYPE_RESOLVING,
    TYPE_RESOLVED,
} TypeState;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

typedef struct Type {
    TypeKind kind;
    TypeState state;
    union {
        Type *base_type;
        const char *name;
        struct {
            Type *elem;
        } ptr;
        struct {
            Type *elem;
            size_t size;
        } array;
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        struct {
            Type **args;
            size_t num_args;
            Type *ret_type;
        } func;
    };
} Type;

Type *type_int = &(Type){TYPE_INT};

Sym *syms;
Decl **ordered_decls;

typedef struct ResolvedExpr {
    Type *type;
    bool is_lvalue;
    bool is_const;
    union {
        int64_t val;
    };
} ResolvedExpr;

ResolvedExpr resolve_expr(Expr *expr);
ResolvedExpr resolve_expected_expr(Expr *expr, Type *expected_type);
Type *type_func(Type **args, size_t num_args, Type *ret_type);

void create_global_decl() {
    const char *name_int = str_intern("int");
    buf_push(syms,
             (Sym){.name = name_int, .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_int});
}

Sym *sym_get(const char *name) {
    for (Sym *sym = syms; sym != buf_end(syms); sym++) {
        if (sym->name == name) {
            return sym;
        }
    }

    return NULL;
}

void install_decls(Decl *decl) {
    if (sym_get(decl->name) != NULL) {
        fatal("symbol %s already exists", decl->name);
    }

    Type *type_aggregate;

    switch (decl->kind) {
    case DECL_CONST:
        buf_push(
            syms,
            (Sym){.name = decl->name, .kind = SYM_CONST, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    case DECL_VAR:
        buf_push(syms,
                 (Sym){.name = decl->name, .kind = SYM_VAR, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    case DECL_AGGREGATE:
        type_aggregate = type_alloc(TYPE_STRUCT);
        type_aggregate->state = TYPE_UNRESOLVED;
        type_aggregate->name = decl->name;

        buf_push(syms, (Sym){.name = decl->name,
                             .kind = SYM_TYPE,
                             .decl = decl,
                             .state = SYM_UNRESOLVED,
                             .type = type_aggregate});
        break;
    case DECL_FUNC:
        buf_push(
            syms,
            (Sym){.name = decl->name, .kind = SYM_FUNC, .decl = decl, .state = SYM_UNRESOLVED});
        break;

    case DECL_TYPEDEF:
        buf_push(
            syms,
            (Sym){.name = decl->name, .kind = SYM_TYPE, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    }
}


typedef struct CachedPtrTypes {
    Type *elem;
    Type *ptr;
} CachedPtrTypes;

CachedPtrTypes *cached_ptr_types;

Type *type_alloc(TypeKind kind) {
    Type *type = xcalloc(sizeof(Type));
    type->kind = kind;
    return type;
}

Type *type_ptr(Type *elem) {
    for (int i = 0; i < buf_len(cached_ptr_types); i++) {
        if (cached_ptr_types[i].elem == elem) {
            return cached_ptr_types[i].ptr;
        }
    }

    Type *ptr = type_alloc(TYPE_PTR);
    ptr->ptr.elem = elem;
    buf_push(cached_ptr_types, (CachedPtrTypes){elem, ptr});
    return ptr;
}

Type *type_array(Type *elem, size_t size) {
    Type *type_array = type_alloc(TYPE_ARRAY);
    type_array->array.elem = elem;
    type_array->array.size = size;
    return type_array;
}

typedef struct CachedFuncType {
    struct {
        Type **args;
        size_t num_args;
        Type *ret_type;
    };
    Type *type;
} CachedFuncType;

CachedFuncType *cached_func_types;

Type *type_func(Type **args, size_t num_args, Type *ret_type) {
    for (int i = 0; i < buf_len(cached_func_types); i++) {
        if (num_args != cached_func_types[i].num_args) {
            continue;
        }

        for (int j = 0; j < cached_func_types[i].num_args; j++) {
            if (args[j] != cached_func_types[i].args[j]) {
                goto skip;
            }
        }

        if (cached_func_types[i].ret_type == ret_type) {
            return cached_func_types[i].type;
        }

    skip:;
    }

    Type *type = type_alloc(TYPE_FUNC);
    type->func.args = args;
    type->func.num_args = num_args;
    type->func.ret_type = ret_type;

    buf_push(cached_func_types, (CachedFuncType){args, num_args, ret_type, type});

    return type;
}

Type *create_type(Typespec *type) {

    if (type == NULL) {
        return NULL;
    }

    switch (type->kind) {

    case TYPESPEC_NAME: {
        Sym *sym = sym_get(type->name);
        if (sym->kind != SYM_TYPE) {
            fatal("Symbol %s is not a type", type->name);
        }
        return sym->type;
    }

    case TYPESPEC_PTR:
        return type_ptr(create_type(type->ptr.type));

    case TYPESPEC_ARRAY: {
        ResolvedExpr array_size = resolve_expr(type->array.expr);
        if (!array_size.is_const) {
            fatal("Array size is expected to be a const");
        }

        Type *elem = create_type(type->array.type);
        return type_array(elem, array_size.val);
    }

    case TYPESPEC_FUNC: {
        Type **func_args = NULL;
        for (int i = 0; i < type->func.num_args; i++) {
            buf_push(func_args, create_type(type->func.args[i]));
        }
        Type *ret_type = create_type(type->func.ret);
        return type_func(func_args, buf_len(func_args), ret_type);
    }
    }

    return NULL;
}

void complete_type(Type *type) {
    TypeField *fields = NULL;
    size_t num_fields = 0;

    if (type->kind == TYPE_STRUCT) {
        if (type->state == TYPE_RESOLVED) {
            return;
        }

        if (type->state == TYPE_RESOLVING) {
            fatal("Illegal value cycle while resolving %s\n", type->name);
            return;
        }

        type->state = TYPE_RESOLVING;
        Sym *sym = sym_get(type->name);
        assert(sym);
        AggregateDecl *aggregate_decl = &sym->decl->aggregate_decl;
        for (int i = 0; i < aggregate_decl->num_items; i++) {
            for (int j = 0; j < aggregate_decl->items[i].num_items; j++) {
                Type *type = create_type(aggregate_decl->items[i].type);
                complete_type(type);
                buf_push(fields,
                         (TypeField){.name = aggregate_decl->items[i].names[j], .type = type});
                num_fields++;
            }
        }
        type->aggregate.fields = fields;
        type->aggregate.num_fields = num_fields;
        type->state = TYPE_RESOLVED;
    }
}

ResolvedExpr ptr_decay(ResolvedExpr expr) {
    if (expr.type->kind == TYPE_ARRAY) {
        ResolvedExpr expr_ptr = expr;
        expr_ptr.type = type_ptr(expr.type->array.elem);
        return expr_ptr;
    }

    return expr;
}

ResolvedExpr const_int_expr(int64_t val) {
    return (ResolvedExpr){
        .type = type_int,
        .is_const = true,
        .val = val,
    };
}

ResolvedExpr rvalue_expr(Type *type) {
    return (ResolvedExpr){.type = type};
}

ResolvedExpr lvalue_expr(Type *type) {
    return (ResolvedExpr){.type = type, .is_lvalue = true};
}

int64_t eval_binary_int_expr(int64_t left, int64_t right, TokenKind op) {
    switch (op) {
    case TOKEN_MUL:
        return left * right;
    case TOKEN_DIV:
        return (int64_t) left / right;
    case TOKEN_MOD:
        return left % right;
    case TOKEN_AND:
        return left & right;
    case TOKEN_LSHIFT:
        return left << right;
    case TOKEN_RSHIFT:
        return left >> right;
    case TOKEN_ADD:
        return left + right;
    case TOKEN_SUB:
        return left - right;
    case TOKEN_OR:
        return left | right;
    case TOKEN_XOR:
        return left ^ right;
    case TOKEN_EQ:
        return left == right;
    case TOKEN_LT:
        return left < right;
    case TOKEN_GT:
        return left > right;
    case TOKEN_LTEQ:
        return left <= right;
    case TOKEN_GTEQ:
        return left >= right;
    case TOKEN_NOTEQ:
        return left != right;
    case TOKEN_AND_AND:
        return left && right;
    case TOKEN_OR_OR:
        return left || right;
    default:
        assert(0);
    }
}

ResolvedExpr resolve_binary_expr(Expr *expr) {
    ResolvedExpr expr_left = resolve_expr(expr->binary_expr.left);
    ResolvedExpr expr_right = resolve_expr(expr->binary_expr.right);

    if (expr_left.type != type_int || expr_right.type != type_int) {
        fatal("expected int expression");
    }

    if (expr_left.is_const == true && expr_right.is_const == true) {
        return const_int_expr(
            eval_binary_int_expr(expr_left.val, expr_right.val, expr->binary_expr.op));
    }

    return rvalue_expr(type_int);
}

ResolvedExpr resolve_name_expr(Expr *expr) {
    Sym *sym = sym_get(expr->name);
    if (!sym) {
        fatal("Expected sym %s to exist", expr->name);
    }
    order_decl(sym->decl);
    if (sym->kind == SYM_VAR) {
        return lvalue_expr(sym->type);
    }
    if (sym->kind == SYM_CONST) {
        return const_int_expr(sym->val);
    } else {
        fatal("%s can only be var or const", expr->name);
    }
    return (ResolvedExpr){};
}

ResolvedExpr resolve_unary_expr(Expr *expr) {
    // *,&,+,-,!,~
    ResolvedExpr operand = resolve_expr(expr->unary_expr.expr);
    switch (expr->unary_expr.op) {
    case TOKEN_MUL:
        operand = ptr_decay(operand);
        if (operand.type->kind != TYPE_PTR) {
            fatal("Can deref only a pointer type");
        }
        return lvalue_expr(operand.type->ptr.elem);
    case TOKEN_AND:
        return rvalue_expr(type_ptr(operand.type));
    }
}

ResolvedExpr resolve_field_expr(Expr *expr) {
}

ResolvedExpr resolve_index_expr(Expr *expr) {
}

ResolvedExpr resolve_call_expr(Expr *expr) {
}

ResolvedExpr resolve_expected_expr(Expr *expr, Type *expected_type) {
    if (!expr) {
        return (ResolvedExpr){};
    }

    switch (expr->kind) {
    case EXPR_INT:
        return const_int_expr(expr->int_val);
    case EXPR_UNARY:
        return resolve_unary_expr(expr);
    case EXPR_BINARY:
        return resolve_binary_expr(expr);
    case EXPR_NAME:
        return resolve_name_expr(expr);
    case EXPR_FIELD:
        return resolve_field_expr(expr);
    case EXPR_INDEX:
        return resolve_index_expr(expr);
    case EXPR_CALL:
        return resolve_call_expr(expr);
    default:
        assert(0);
        return (ResolvedExpr){};
    }
}

ResolvedExpr resolve_expr(Expr *expr) {
    return resolve_expected_expr(expr, NULL);
}

Type *order_decl_var(Decl *decl) {
    Type *type = create_type(decl->var_decl.type);
    resolve_expected_expr(decl->var_decl.expr, type);
    complete_type(type);
    return type;
}

Type *order_decl_const(Decl *decl, int64_t *val) {
    ResolvedExpr resolved_expr = resolve_expr(decl->const_decl.expr);
    if (!resolved_expr.is_const) {
        fatal("%s declared as constant, but the value assigned is not a constant", decl->name);
    }
    *val = resolved_expr.val;
    return resolved_expr.type;
}

void order_decl(Decl *decl) {
    Sym *sym;

    if ((sym = sym_get(decl->name)) == NULL) {
        fatal("symbol %s does not exist", decl->name);
        return;
    }

    if (sym->state == SYM_RESOLVING) {
        fatal("illegal value cycle in types");
    }

    if (sym->state == SYM_RESOLVED) {
        return;
    }

    sym->state = SYM_RESOLVING;

    // resolve the dependencies of this decl
    ResolvedExpr resolved_expr = {0};
    switch (decl->kind) {
    case DECL_CONST:
        sym->type = order_decl_const(decl, &sym->val);
        break;
    case DECL_VAR:
        sym->type = order_decl_var(decl);
        break;
    case DECL_AGGREGATE:
        break;
    case DECL_FUNC: {
        Type **func_args = NULL;
        for (int i = 0; i < decl->func_decl.num_func_args; i++) {
            buf_push(func_args, create_type(decl->func_decl.args[i].type));
        }
        Type *ret_type = create_type(decl->func_decl.type);
        sym->type = type_func(func_args, buf_len(func_args), ret_type);

        for (int i = 0; i < decl->func_decl.block.num_stmts; i++) {
            // TODO: Order statements code comes here
        }

        break;
    }
    case DECL_TYPEDEF:
        sym->type = create_type(decl->typedef_decl.type);
    }

    buf_push(ordered_decls, sym->decl);
    sym->state = SYM_RESOLVED;
}

void resolve_test() {
    const char *decl[] = {
        "var i:int",
        "var j:int",
        "var k:int*",
        "var m:int*",
        "var q:S*",
        "struct S {t : T*;}",
        "struct T {i:int; s : S*;}",
        "func hello(i :int, j:int):int {}",
        "typedef D = func(int,int):int",
        "typedef E = S*",
        "var p:int[o]",
        "const o = 1+2",
    };

    create_global_decl();
    for (int i = 0; i < sizeof(decl) / sizeof(*decl); i++) {
        init_stream(decl[i]);
        Decl *d = parse_decl_opt();
        install_decls(d);
    }

    for (Sym *sym = syms; sym != buf_end(syms); sym++) {
        if (sym->decl) {
            order_decl(sym->decl);
        }
    }

    for (Sym *sym = syms; sym != buf_end(syms); sym++) {
        if (sym->type) {
            complete_type(sym->type);
        }
    }

    for (Decl **decl = ordered_decls; decl != buf_end(ordered_decls); decl++) {
        print_decl(*decl);
        printf("\n");
    }
}
