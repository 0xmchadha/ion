
typedef enum TypeKind {
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_BOOL,
    TYPE_ENUM,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_FUNC,
};

typedef struct Type {
    TypeKind kind;
    union {
        struct {
            Type *base;
        } ptr;
        struct {
            Type *base;
            size_t size;
        } array;
        struct {
            TypeField *fields;
            size_t num_fields;
        } aggregate;
        struct {
            TypeField *params;
            Type *ret;
        } func;
    };
} Type;

typedef struct TypeField {
    const char *name;
    Type *type;
} TypeField;

Type *type_int();
Type *type_float();
Type *type_ptr(Type *base);
Type *type_array(Type *base, size_t size);
Type *type_struct(TypeField *fields, size_t num_fields);
Type *type_union(TypeField *fields, size_t num_fields);
Type *type_func(TypeField *params, size_t num_params, Type *ret_type);

typedef enum SymState {
    STATE_UNRESOLVED = 0,
    STATE_RESOLVING,
    STATE_RESOLVED,
} SymState;

typedef struct Sym {
    const char *name;
    SymState state;
    Decl *decl;
    Entity *ent;
} Sym;

Sym *sym_list;

Sym *sym_get(const char *name) {
    for (Sym *s = sym_list; s != buf_end(sym_list); s++) {
        if (s->name == name) {
            return s;
        }
    }

    return NULL;
}

void sym_put(Decl *decl) {
    assert(decl->name);
    assert(!sym_get(decl->name));
    buf_push(sym_list, (Sym){decl->name, STATE_UNRESOLVED, decl});
}

void resolve_expr(Expr *expr) {
    if (!expr) {
        return;
    }
}

void resolve_type(Typespec *type) {
    if (!type) {
        return;
    }
}

void resolve_decl(Decl *decl) {
    switch (decl->kind) {
    case DECL_VAR:
        resolve_type(decl->var_decl.type);
        resolve_expr(decl->var_decl.expr);
        break;
    case DECL_CONST:
        ConstEntity *const_ent = resolve_const_expr(decl->const_decl.expr);
        break;
    case DECL_STRUCT:
        for (int i = 0; i < decl->aggregate_decl.num_items; i++) {
            resolve_type(decl->aggregate_decl.items[i].type);
        }

    case DECL_UNION:
    case DECL_TYPEDEF:
    case DECL_FUNC:
    }
}

Sym *resolve_name(const char *name) {
    Sym *sym = sym_get(name);
    if (!sym) {
        fatal("unknown name");
        return NULL;
    }
    resolve_sym(sym);
    return sym;
}

void resolve_sym(Sym *s) {
    if (s->state == STATE_RESOLVED) {
        return;
    }

    if (sym->state == STATE_RESOLVING) {
        fatal("cyclic dependency");
        return;
    }

    resolve_decl(s->decl);
    s->state = STATE_RESOLVED;
}

Sym *resolve_syms() {
    for (Sym *s = sym_list; s != sym_end(sym_list); s++) {
        resolve_sym(s);
    }
}

/* Sym *resolve_type(Type *type) { */
/*     switch (type->kind) { */
/*     case TYPE_NAME: */
/*         sym_resolve(sym_get(type->name)); */
/*         break; */
/*     } */
/* } */

/* void sym_resolve(Sym *sym) { */
/*     switch (decl->kind) { */
/*     case DECL_VAR: */

/*         break; */
/*     case DECL_CONST: */
/*     case DECL_STRUCT: */
/*     case DECL_UNION: */
/*     case DECL_TYPEDEF: */
/*     case DECL_FUNC: */
/*     } */
/*     sym->state = STATE_RESOLVED; */
/* } */

void resolve_test() {
    const char *name = str_intern("foo");
    assert(sym_get("foo") == NULL);
    Decl *decl = decl_const(name, expr_int(0));
    sym_put(decl);
    assert(decl == sym_get(name)->decl);
}

/*
  var d :T

  typedef T = struct S

  struct s {
  }
 */
