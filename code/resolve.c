typedef struct Type Type;

void order_expr(Expr *expr);
void order_decl(Decl *decl);
void order_typespec(Typespec *);

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
    };
} Type;

Type *type_int = &(Type){TYPE_INT};

Sym *syms;
Decl **ordered_decls;

// CachedPtrTypes;

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
        fatal("symbol already exists");
    }

    Type *type_aggregate;

    switch (decl->kind) {
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
        case DECL_VAR:
            order_typespec(decl->var_decl.type);
            order_expr(decl->var_decl.expr);
            break;
        case DECL_AGGREGATE:
            for (int i = 0; i < decl->aggregate_decl.num_items; i++) {
                order_typespec(decl->aggregate_decl.items[i].type);
            }
            break;
        case DECL_TYPEDEF:
            order_typespec(decl->typedef_decl.type);
            break;
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
    }
}

Type **cached_ptr_types;

Type *type_alloc(TypeKind kind) {
    Type *type = xcalloc(sizeof(Type));
    type->kind = kind;
    return type;
}

Type *type_ptr(Type *type) {
    for (int i = 0; i < buf_len(cached_ptr_types); i++) {
        if (cached_ptr_types[i]->ptr.elem == type) {
            return cached_ptr_types[i];
        }
    }

    Type *type_ptr = type_alloc(TYPE_PTR);
    type_ptr->ptr.elem = type;
    buf_push(cached_ptr_types, type_ptr);
}

Type *create_type(Typespec *type) {

    switch (type->kind) {
    case TYPESPEC_PTR:
        return type_ptr(create_type(type->ptr.type));
    case TYPESPEC_NAME:
        return sym_get(type->name)->type;
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
            }
        }
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
