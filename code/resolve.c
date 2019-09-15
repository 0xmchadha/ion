typedef struct Type Type;

void order_expr(Expr *expr);
void order_decl(Decl *decl);
void order_typespec(Typespec *);
Type *create_type(Typespec *type);

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
    union {
        Type *type;
    };
} Sym;

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_INT,
    TYPE_PTR,
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

void create_global_decl() {
    const char *name_int = str_intern("int");
    buf_push(syms, (Sym){.name = name_int, .state = SYM_RESOLVED, .type = type_int});
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
        buf_push(syms,
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
    {
        switch (decl->kind) {
        case DECL_CONST:
            order_expr(decl->const_decl.expr);
            break;
        case DECL_VAR:
            order_typespec(decl->var_decl.type);
            order_expr(decl->var_decl.expr);
            break;
        case DECL_AGGREGATE:
            for (int i = 0; i < decl->aggregate_decl.num_items; i++) {
                order_typespec(decl->aggregate_decl.items[i].type);
            }
            break;
        case DECL_FUNC:
            for (int i = 0; i < decl->func_decl.num_func_args; i++) {
                order_typespec(decl->func_decl.args[i].type);
            }

            order_typespec(decl->func_decl.type);

            for (int i = 0; i < decl->func_decl.block.num_stmts; i++) {
                // TODO: Order statements code comes here
            }

            break;

        case DECL_TYPEDEF:
            order_typespec(decl->typedef_decl.type);
            sym->type = create_type(decl->typedef_decl.type);
        }
    }

    buf_push(ordered_decls, sym->decl);
    sym->state = SYM_RESOLVED;
}

void order_expr(Expr *expr) {
    return;
}

void order_typespec(Typespec *type) {
    Sym *sym = NULL;

    if (type == NULL) {
        return;
    }

    switch (type->kind) {
    case TYPESPEC_NAME:
        sym = sym_get(type->name);
        if (sym == NULL) {
            fatal("symbol %s does not exist", type->name);
            return;
        }
        break;
    case TYPESPEC_PTR:
        order_typespec(type->ptr.type);
        break;

    case TYPESPEC_FUNC:
        for (int i = 0; i < type->func.num_args; i++) {
            order_typespec(type->func.args[i]);
        }
        order_typespec(type->func.ret);
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

    switch (type->kind) {
    case TYPESPEC_PTR:
        return type_ptr(create_type(type->ptr.type));
    case TYPESPEC_NAME:
        return sym_get(type->name)->type;
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
