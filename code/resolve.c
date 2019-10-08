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
    TYPE_VOID,
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
    size_t size;
    size_t alignment;
    union {
        const char *name;
        struct {
            Type *elem;
        } ptr;
        struct {
            Type *elem;
            size_t size;
        } array;
        struct {
            const char *name;
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

Type *type_int = &(Type){.kind = TYPE_INT, .size = 4, .alignment = 4};
Type *type_void = &(Type){.kind = TYPE_VOID, .size = 0, .alignment = 0};

const size_t PTR_SIZE = 8;
const size_t PTR_ALIGNMENT = 8;

Decl **ordered_decls;
Sym *syms;

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
    const char *name_void = str_intern("void");
    buf_push(syms,
             (Sym){.name = name_int, .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_int});
    buf_push(syms,
             (Sym){.name = name_void, .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_void});
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
        type_aggregate->aggregate.name = decl->name;
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
    ptr->size = PTR_SIZE;
    ptr->alignment = PTR_ALIGNMENT;
    buf_push(cached_ptr_types, (CachedPtrTypes){elem, ptr});
    return ptr;
}

Type *type_array(Type *elem, size_t size) {
    Type *type_array = type_alloc(TYPE_ARRAY);
    type_array->array.elem = elem;
    type_array->array.size = size;
    type_array->size = size * elem->size;
    type_array->alignment = elem->alignment;
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
        if (!sym) {
            fatal("Type %s is not defined", type->name);
        }
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

#define MAX(a,b) (((a)>(b)) ? (a) : (b))

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
        size_t size = 0;
        size_t alignment = 0;
        for (int i = 0; i < aggregate_decl->num_items; i++) {
            for (int j = 0; j < aggregate_decl->items[i].num_items; j++) {
                Type *type = create_type(aggregate_decl->items[i].type);
                complete_type(type);
                alignment = MAX(alignment, type->alignment);
                size = ALIGN_UP(size, type->alignment) + type->size;
                buf_push(fields,
                         (TypeField){.name = aggregate_decl->items[i].names[j], .type = type});
                num_fields++;
            }
        }

        type->size = size;
        type->alignment = alignment;
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

int64_t eval_unary_int_expr(TokenKind op, int64_t val) {
    switch (op) {
    case TOKEN_ADD:
        return val;
    case TOKEN_SUB:
        return -val;
    case TOKEN_NEG:
        return ~val;
    case TOKEN_NOT:
        return ~val;
    default:
        fatal("Operator not supported for unary expressions");
    }
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
    } else if (sym->kind == SYM_CONST) {
        return const_int_expr(sym->val);
    } else if (sym->kind == SYM_FUNC) {
        return rvalue_expr(sym->type);
    } else {
        fatal("%s can only be var var, const or func", expr->name);
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
    default: {
        if (operand.type->kind != TYPE_INT) {
            fatal("unary operator +,-,!,~ are applied on integer types only");
        }

        if (operand.is_const) {
            return const_int_expr(eval_unary_int_expr(expr->unary_expr.op, operand.val));
        }

        return rvalue_expr(type_int);
    }
    }
}

ResolvedExpr resolve_field_expr(Expr *expr) {
    ResolvedExpr base = resolve_expr(expr->field_expr.expr);

    if (base.type->kind != TYPE_STRUCT) {
        fatal("Error: only struct types have access fields");
    }

    for (int i = 0; i < base.type->aggregate.num_fields; i++) {
        if (base.type->aggregate.fields[i].name == expr->field_expr.name) {
            return lvalue_expr(base.type->aggregate.fields[i].type);
        }
    }

    fatal("%s field does not exist with type %s", expr->field_expr.name, base.type->aggregate.name);
}

ResolvedExpr resolve_index_expr(Expr *expr) {
    ResolvedExpr index = resolve_expr(expr->index_expr.index);
    if (index.type->kind != TYPE_INT) {
        fatal("array indices can be of type int only");
    }

    ResolvedExpr operand = resolve_expr(expr->index_expr.expr);
    if (operand.type->kind != TYPE_PTR && operand.type->kind != TYPE_ARRAY) {
        fatal("Only pointer and array types can be indexed");
    }

    ResolvedExpr ptr_expr = ptr_decay(operand);
    return lvalue_expr(ptr_expr.type->ptr.elem);
}

ResolvedExpr resolve_call_expr(Expr *expr) {
    ResolvedExpr func = resolve_expr(expr->call_expr.expr);
    if (func.type->kind != TYPE_FUNC) {
        fatal("Expected type to be func");
    }

    if (expr->call_expr.num_args != func.type->func.num_args) {
        fatal("func required %d arguments, but being passed %d", func.type->func.num_args,
              expr->call_expr.num_args);
    }

    for (int i = 0; i < expr->call_expr.num_args; i++) {
        ResolvedExpr expr_args =
            resolve_expected_expr(expr->call_expr.args[i], func.type->func.args[i]);
        if (expr_args.type != func.type->func.args[i]) {
            fatal("func arg types %d do not match", i);
        }
    }

    return rvalue_expr(func.type->func.ret_type);
}

ResolvedExpr resolve_cast_expr(Expr *expr) {
    ResolvedExpr original = resolve_expr(expr->cast_expr.expr);
    Type *casted_type = create_type(expr->cast_expr.type);

    if (original.type->kind != TYPE_INT && original.type->kind != TYPE_PTR &&
        original.type->kind != TYPE_ARRAY) {
        fatal("only int and pointer types can be casted");
    }

    if (casted_type->kind != TYPE_INT && casted_type->kind != TYPE_PTR) {
        fatal("can only cast to ints or pointer types");
    }

    return rvalue_expr(casted_type);
}

size_t get_index_field(Type *type, const char *name) {
    if (type->kind != TYPE_STRUCT) {
        fatal("only struct types can have index to fields");
    }

    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        if (name == type->aggregate.fields[i].name) {
            return i;
        }
    }

    fatal("Named field does not exist in the type");
}

ResolvedExpr resolve_compound_expr(Expr *expr, Type *expected_type) {
    Type *compound_expr_type = create_type(expr->compound_expr.type);
    expected_type = (expected_type) ? expected_type : compound_expr_type;
    if (!expected_type) {
        fatal("compound expression requires an expected type");
    }

    complete_type(expected_type);

    if (expected_type->kind != TYPE_STRUCT && expected_type->kind != TYPE_ARRAY) {
        fatal("compound expression requires struct or array types");
    }

    if (expected_type->kind == TYPE_STRUCT) {
        if (expr->compound_expr.type != NULL && expected_type != NULL) {
            if (create_type(expr->compound_expr.type) != expected_type) {
                fatal("Expected type of compound literal should match the declared type of "
                      "compound literal");
                return rvalue_expr(type_void);
            }
        }

        if (expected_type->aggregate.num_fields < expr->compound_expr.num_args) {
            fatal("Actual type has fewer memebers than being passed in the compound literal");
            return rvalue_expr(type_void);
        }

        for (size_t i = 0, j = 0; i < expr->compound_expr.num_args; i++, j++) {
            if (j == expected_type->aggregate.num_fields) {
                fatal("field initializer out of range");
            }

            CompoundVal *val = expr->compound_expr.args[i];
            Expr *expr_literal = NULL;

            switch (val->kind) {
            case SIMPLE_EXPR:
                expr_literal = val->expr;
                break;
            case NAME_EXPR:
                j = get_index_field(expected_type, val->name.name);
                expr_literal = val->name.val;
                break;
            case INDEX_EXPR:
                fatal("Can not have index expression inside struct compound literal");
                break;
            default:
                assert(0);
            }

            ResolvedExpr resolved_arg =
                resolve_expected_expr(expr_literal, expected_type->aggregate.fields[j].type);
            if (resolved_arg.type != expected_type->aggregate.fields[j].type) {
                fatal("type of expression in compound literal doesn't match the actual type of the "
                      "field");
                return rvalue_expr(type_void);
            }
        }
    } else {
        if (expr->compound_expr.type != NULL && expected_type != NULL) {

            if ((compound_expr_type->array.elem != expected_type->array.elem) ||
                (compound_expr_type->array.size != expected_type->array.size)) {
                fatal("array compound literals types do not match");
            }
        }

        if (expected_type->array.size < expr->compound_expr.num_args) {
            fatal("compound literal has more array members than the defined type");
        }

        for (size_t i = 0, array_index = 0; i < expr->compound_expr.num_args; i++, array_index++) {
            if (array_index >= expected_type->array.size) {
                fatal("compound literal initializing array out of bounds");
            }

            CompoundVal *val = expr->compound_expr.args[i];
            Expr *expr_literal = NULL;

            switch (val->kind) {
            case SIMPLE_EXPR:
                expr_literal = val->expr;
                break;
            case INDEX_EXPR: {
                ResolvedExpr expr_index = resolve_expr(val->index.index);
                if (!expr_index.is_const) {
                    fatal("Field initializer index in array compound expression has to be a "
                          "constant");
                }

                array_index = (size_t) expr_index.val;
                if (array_index >= expected_type->array.size) {
                    fatal("compound literal initializing array out of bounds");
                }

                expr_literal = val->index.val;
                break;
            }
            case NAME_EXPR:
                fatal("Can not initialize named fields inside array compound expression");
                break;
            default:
                assert(0);
            }

            ResolvedExpr resolved_arg =
                resolve_expected_expr(expr_literal, expected_type->array.elem);
            if (resolved_arg.type != expected_type->array.elem) {
                fatal("type of expression in compound literal doesn't match the actual type of "
                      "the "
                      "field");
                return rvalue_expr(type_void);
            }
        }
    }

    return rvalue_expr(expected_type);
}

ResolvedExpr resolve_ternary_expr(Expr *expr, Type *expected_type) {
    ResolvedExpr cond = resolve_expr(expr->ternary_expr.eval);

    if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_PTR) {
        fatal("ternary condition expression should be of type int or ptr");
    }

    ResolvedExpr then_expr = resolve_expected_expr(expr->ternary_expr.then_expr, expected_type);
    ResolvedExpr else_expr = resolve_expected_expr(expr->ternary_expr.else_expr, expected_type);

    if (then_expr.type != else_expr.type) {
        fatal("Ternary then else expressions types should match");
    }

    if (cond.is_const) {
        return (cond.val) ? then_expr : else_expr;
    } else {
        return rvalue_expr(then_expr.type);
    }
}

ResolvedExpr resolve_sizeof_type(Expr *expr) {
    Type *type = create_type(expr->sizeof_type);
    complete_type(type);
    return const_int_expr(type->size);
}

ResolvedExpr resolve_sizeof_expr(Expr *expr) {
    ResolvedExpr type_expr = resolve_expr(expr->sizeof_expr);
    complete_type(type_expr.type);
    return const_int_expr(type_expr.type->size);
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
    case EXPR_TERNARY:
        return resolve_ternary_expr(expr, expected_type);
    case EXPR_NAME:
        return resolve_name_expr(expr);
    case EXPR_FIELD:
        return resolve_field_expr(expr);
    case EXPR_INDEX:
        return resolve_index_expr(expr);
    case EXPR_CALL:
        return resolve_call_expr(expr);
    case EXPR_CAST:
        return resolve_cast_expr(expr);
    case EXPR_COMPOUND:
        return resolve_compound_expr(expr, expected_type);
    case EXPR_SIZEOF_TYPE:
        return resolve_sizeof_type(expr);
    case EXPR_SIZEOF_EXPR:
        return resolve_sizeof_expr(expr);
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

    if (decl->var_decl.expr) {
        ResolvedExpr expr = resolve_expected_expr(decl->var_decl.expr, type);
        if (type != NULL && type != expr.type) {
            fatal("expression type and var type do not match for %s", decl->name);
        }

        type = expr.type;
    }

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
        "func hello(i :int*, j:int*):int {}",
        "typedef D = func(int,int):int",
        "typedef E = S*",
        "var p:int[o]",
        "const o = 1+2",
        "const oo = !1",
        "const pp = ~2",
        // test struct fields.. negative tests are also evaluated.
        "struct TestA {a: TestB*;}",
        "struct TestB {t: TestA;}",
        "var test :TestA",
        "var test2:TestB* = test.a",
        // test index fields
        "struct TestC {a :int[10]; b :int**;}",
        "var testc :TestC",
        "var test3:int = testc.a[0]",
        "var test4 = testc.b[0]",
        // test call expr
        "func test_func(i:int *, j:int *):int{}",
        "var hh = test_func(&i,&j)",
        "func test_func2(i: S*):int{}",
        "var s:S*",
        "var aaa = test_func2(s)",
        // test cast expr
        "var c = cast(int *, 10)",
        "var d:int* = c",
        "var e:int = cast(int, d)",
        "var f:int[10]",
        "var g:int = cast(int, f)",
        // test teranary
        "var ta = g ? 2 : 3",
        // test sizeof expr
        "const size_int = sizeof(:int)",
        "const size_int1 = sizeof(ta)",
        // test compound literal
        "struct C1 {a :int; b :int;}",
        "var ca = C1{1,2}",
        "struct C2 {a:C1; b:int;}",
        "var cb :C2 = {{1,2},3}",
        "var cc :int[3] = {1,2,4}",
        "var cd = (:int[3]){1,2,3}",
        "var ce =(:int[2][2]){{1,2}, {1,2}}",
        "var cf:int[2][2] = {{2,3},{4,cd[0]}}",
        "var cg = C1{1,2}",
        "struct C3 {i :int; j:int*;}",
        "var ck = C3{1, &ca.a}",
        "struct C4 {i:int; j:int;}",
        "func CF1(v: C4, w: C4):C4 {return {1,2};}",
        "var co:C4 = CF1(C4{1,2}, {2,3})",
        "struct cVector{x,y:int;}",
        "var v: cVector = 0 ? {1,2} : {3,4}",
        "var vs:cVector[2][2] = {{{1,2},{3,4}},{{5,6},{7,8}}}",
        "struct nVector{x,y:int;}",
        "var tVector = nVector{x=10,y=10}",
        "var t2vector = (:int[2]){1,2}",
        "var t3vector = (:int[2]){[1] = 1},",
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
            printf("Type name = %s, size = %lu, alignment = %lu\n", sym->name, sym->type->size, sym->type->alignment);
        }
    }

    for (Decl **decl = ordered_decls; decl != buf_end(ordered_decls); decl++) {
        print_decl(*decl);
        printf("\n");
    }
}
