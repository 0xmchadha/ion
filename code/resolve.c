typedef struct Type Type;
typedef struct Sym Sym;

void resolve_global_sym(Sym *sym);
Type *create_type(Typespec *type);
void complete_type(Type *type);
Type *ptr_decay(Type *type);

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
    SYM_ENUM_MEM,
    SYM_TYPEDEF,
} SymKind;

typedef union Val {
    char ch;
    signed char sch;
    unsigned char uch;
    short s;
    unsigned short us;
    int i;
    unsigned int ui;
    long l;
    unsigned long ul;
    long long ll;
    unsigned long long ull;
    float f;
    double d;
} Val;

typedef struct Sym {
    const char *name;
    SymKind kind;
    Decl *decl;
    SymState state;
    Type *type;
    // used for enum members
    const char *enum_decl;
    // used for constants, ie const int and enums
    Val val;
} Sym;

typedef enum TypeKind {
    TYPE_NONE,
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_SCHAR,
    TYPE_UCHAR,
    TYPE_SHORT,
    TYPE_USHORT,
    TYPE_INT,
    TYPE_UINT,
    TYPE_LONG,
    TYPE_ULONG,
    TYPE_LONGLONG,
    TYPE_ULONGLONG,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_PTR,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ENUM,
    TYPE_FUNC,
    TYPE_MAX,
} TypeKind;

const char *type_kind[] = {
    [TYPE_VOID] = "void",
    [TYPE_CHAR] = "char",
    [TYPE_SCHAR] = "signed char",
    [TYPE_UCHAR] = "unsigned char",
    [TYPE_SHORT] = "short",
    [TYPE_USHORT] = "unsigned short",
    [TYPE_INT] = "int",
    [TYPE_UINT] = "unsigned int",
    [TYPE_LONG] = "long",
    [TYPE_ULONG] = "unsigned long",
    [TYPE_LONGLONG] = "long long",
    [TYPE_ULONGLONG] = "unsigned long long",
    [TYPE_FLOAT] = "float",
    [TYPE_DOUBLE] = "double",
    [TYPE_PTR] = "pointer",
    [TYPE_ARRAY] = "array",
    [TYPE_STRUCT] = "struct",
    [TYPE_UNION] = "union",
    [TYPE_ENUM] = "enum",
    [TYPE_FUNC] = "func",
};

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
            bool is_variadic;
        } func;
    };
} Type;

Type *type_void = &(Type){.kind = TYPE_VOID, .size = 0, .alignment = 0};
Type *type_char = &(Type){.kind = TYPE_CHAR, .size = 1, .alignment = 1};
Type *type_schar = &(Type){.kind = TYPE_SCHAR, .size = 1, .alignment = 1};
Type *type_uchar = &(Type){.kind = TYPE_UCHAR, .size = 1, .alignment = 1};
Type *type_short = &(Type){.kind = TYPE_SHORT, .size = 2, .alignment = 2};
Type *type_ushort = &(Type){.kind = TYPE_USHORT, .size = 2, .alignment = 2};
Type *type_int = &(Type){.kind = TYPE_INT, .size = 4, .alignment = 4};
Type *type_uint = &(Type){.kind = TYPE_UINT, .size = 4, .alignment = 4};
Type *type_long = &(Type){.kind = TYPE_LONG, .size = 8, .alignment = 8};
Type *type_ulong = &(Type){.kind = TYPE_ULONG, .size = 8, .alignment = 8};
Type *type_longlong = &(Type){.kind = TYPE_LONGLONG, .size = 8, .alignment = 8};
Type *type_ulonglong = &(Type){.kind = TYPE_ULONGLONG, .size = 8, .alignment = 8};
Type *type_float = &(Type){.kind = TYPE_FLOAT, .size = 4, .alignment = 4};
Type *type_double = &(Type){.kind = TYPE_DOUBLE, .size = 8, .alignment = 8};

#define type_size_t type_ulonglong
#define TYPE_SIZE_T TYPE_ULONGLONG

const size_t PTR_SIZE = 8;
const size_t PTR_ALIGNMENT = 8;

bool is_integer_type(Type *type) {
    return (type->kind >= TYPE_CHAR && type->kind <= TYPE_ULONGLONG) ? true : false;
}

bool is_arithmetic_type(Type *type) {
    return (type->kind >= TYPE_CHAR && type->kind <= TYPE_DOUBLE) ? true : false;
}

bool is_scalar_type(Type *type) {
    return (type->kind == TYPE_PTR || is_arithmetic_type(type));
}

Decl **ordered_decls;
Map global_sym_map;
Sym **global_sym_list;

typedef struct ResolvedExpr {
    Type *type;
    bool is_lvalue;
    bool is_const;
    Val val;
} ResolvedExpr;

ResolvedExpr resolve_expr(Expr *expr);
ResolvedExpr resolve_expected_expr(Expr *expr, Type *expected_type);
Type *type_func(Type **args, size_t num_args, Type *ret_type, bool is_variadic);
Type *type_ptr(Type *elem);
Sym *sym_get(const char *name);
void resolve_stmt(Stmt *stmt, Type *expected_type, Sym *scope_start);

bool is_type_conversion_legal(Type *src, Type *dst) {
    if (is_arithmetic_type(src) && is_arithmetic_type(dst)) {
        return true;
    }

    if (src->kind == TYPE_PTR && dst->kind == TYPE_PTR) {
        if (src->ptr.elem->kind == TYPE_VOID || dst->ptr.elem->kind == TYPE_VOID) {
            return true;
        }
    }

    return false;
}

void convert_operand_type(ResolvedExpr *expr, Type *type) {
    if (!is_type_conversion_legal(expr->type, type)) {
        return;
    }

#define TYPE_CONVERT(VAL)                                                                          \
    {                                                                                              \
        switch (type->kind) {                                                                      \
        case TYPE_CHAR:                                                                            \
            expr->val.ch = (char) VAL;                                                             \
            break;                                                                                 \
        case TYPE_SCHAR:                                                                           \
            expr->val.sch = (signed char) VAL;                                                     \
            break;                                                                                 \
        case TYPE_UCHAR:                                                                           \
            expr->val.uch = (unsigned char) VAL;                                                   \
            break;                                                                                 \
        case TYPE_SHORT:                                                                           \
            expr->val.s = (short) VAL;                                                             \
            break;                                                                                 \
        case TYPE_USHORT:                                                                          \
            expr->val.us = (unsigned short) VAL;                                                   \
            break;                                                                                 \
        case TYPE_INT:                                                                             \
            expr->val.i = (int) VAL;                                                               \
            break;                                                                                 \
        case TYPE_UINT:                                                                            \
            expr->val.ui = (unsigned int) VAL;                                                     \
            break;                                                                                 \
        case TYPE_LONG:                                                                            \
            expr->val.l = (long) VAL;                                                              \
            break;                                                                                 \
        case TYPE_ULONG:                                                                           \
            expr->val.ul = (unsigned long) VAL;                                                    \
            break;                                                                                 \
        case TYPE_LONGLONG:                                                                        \
            expr->val.ll = (long long) VAL;                                                        \
            break;                                                                                 \
        case TYPE_ULONGLONG:                                                                       \
            expr->val.ull = (unsigned long long) VAL;                                              \
            break;                                                                                 \
        case TYPE_FLOAT:                                                                           \
            expr->val.f = (float) VAL;                                                             \
            break;                                                                                 \
        case TYPE_DOUBLE:                                                                          \
            expr->val.d = (double) VAL;                                                            \
            break;                                                                                 \
        default:                                                                                   \
            expr->is_const = false;                                                                \
        }                                                                                          \
    }

    switch (expr->type->kind) {
    case TYPE_CHAR:
        TYPE_CONVERT(expr->val.ch);
        break;
    case TYPE_SCHAR:
        TYPE_CONVERT(expr->val.sch);
        break;
    case TYPE_UCHAR:
        TYPE_CONVERT(expr->val.uch);
        break;
    case TYPE_SHORT:
        TYPE_CONVERT(expr->val.s);
        break;
    case TYPE_USHORT:
        TYPE_CONVERT(expr->val.us);
        break;
    case TYPE_INT:
        TYPE_CONVERT(expr->val.i);
        break;
    case TYPE_UINT:
        TYPE_CONVERT(expr->val.ui);
        break;
    case TYPE_LONG:
        TYPE_CONVERT(expr->val.l);
        break;
    case TYPE_ULONG:
        TYPE_CONVERT(expr->val.ul);
        break;
    case TYPE_ULONGLONG:
        TYPE_CONVERT(expr->val.ull);
        break;
    case TYPE_FLOAT:
        TYPE_CONVERT(expr->val.f);
        break;
    case TYPE_DOUBLE:
        TYPE_CONVERT(expr->val.d);
        break;
    }

    expr->type = type;

#undef TYPE_CONVERT
}

void promote_integer(ResolvedExpr *operand) {
    switch (operand->type->kind) {
    case TYPE_CHAR:
    case TYPE_UCHAR:
    case TYPE_SCHAR:
    case TYPE_SHORT:
    case TYPE_USHORT:
        convert_operand_type(operand, type_int);
    }
}

int ranks[TYPE_MAX] = {
    [TYPE_CHAR] = 0,   [TYPE_SCHAR] = 0,    [TYPE_UCHAR] = 0,     [TYPE_SHORT] = 1,
    [TYPE_USHORT] = 1, [TYPE_INT] = 2,      [TYPE_UINT] = 2,      [TYPE_LONG] = 3,
    [TYPE_ULONG] = 3,  [TYPE_LONGLONG] = 4, [TYPE_ULONGLONG] = 4,
};

bool is_signed_type(Type *type) {
    switch (type->kind) {
    case TYPE_CHAR:
    case TYPE_SCHAR:
    case TYPE_SHORT:
    case TYPE_INT:
    case TYPE_LONG:
    case TYPE_LONGLONG:
        return true;
    }

    return false;
}

Type *convert_to_unsigned(Type *type) {
    if (type == type_int) {
        return type_uint;
    }

    if (type == type_long) {
        return type_ulong;
    }

    if (type == type_longlong) {
        return type_ulonglong;
    }

    assert(0);
    return NULL;
}

void convert_binary_operands_internal(ResolvedExpr *e1, ResolvedExpr *e2) {

    // If either of the types is double, convert the other to double
    if (e1->type == type_double || e2->type == type_double) {
        convert_operand_type(e1, type_double);
        convert_operand_type(e2, type_double);
        return;
    }

    // If either of the types is float, convert the other to float
    if (e1->type == type_float || e2->type == type_float) {
        convert_operand_type(e1, type_float);
        convert_operand_type(e2, type_float);
        return;
    }

    assert(is_integer_type(e1->type) && is_integer_type(e2->type));
    // When dealing with integer types, first do integer promotion.
    promote_integer(e1);
    promote_integer(e2);

    // If the types are same, we are done.
    if (e1->type == e2->type) {
        return;
    }

    // If both types are either signed or unsigned, the lesser rank is converted to
    // type of higher rank.
    if (is_signed_type(e1->type) == is_signed_type(e2->type)) {
        (ranks[e1->type->kind] < ranks[e2->type->kind]) ? convert_operand_type(e1, e2->type)
                                                        : convert_operand_type(e2, e1->type);
        return;
    }

    // If unsigned type has rank greater than or equal to the rank of the signed then
    // convert signed to the unsigned type.
    {
        if (!is_signed_type(e1->type) && (ranks[e1->type->kind] >= ranks[e2->type->kind])) {
            convert_operand_type(e2, e1->type);
            return;
        }

        if (!is_signed_type(e2->type) && (ranks[e2->type->kind] >= ranks[e1->type->kind])) {
            convert_operand_type(e1, e2->type);
            return;
        }
    }

    // Rank of sign is greater than rank of unsigned and the unsigned can be represented in the
    // signed type.
    // eg. TYPE_LONG + TYPE_UINT
    {
        if (is_signed_type(e1->type) && e1->type->size > e2->type->size) {
            convert_operand_type(e2, e1->type);
            return;
        }

        if (is_signed_type(e2->type) && e2->type->size > e1->type->size) {
            convert_operand_type(e1, e2->type);
            return;
        }
    }

    // rank of sign is greater than the rank of unsigned but unsigned can not be represented
    // eg. TYPE_LONGLONG + TYPE_ULONG
    {
        Type *unsigned_type;

        if (is_signed_type(e1->type)) {
            unsigned_type = convert_to_unsigned(e1->type);
            convert_operand_type(e1, unsigned_type);
            convert_operand_type(e2, unsigned_type);
            return;
        }

        // if above fails this is tautology, but for symmetry this is a good choice to add.
        if (is_signed_type(e2->type)) {
            unsigned_type = convert_to_unsigned(e2->type);
            convert_operand_type(e1, unsigned_type);
            convert_operand_type(e2, unsigned_type);
            return;
        }
    }

    assert(0);
}

void convert_binary_operands(ResolvedExpr *e1, ResolvedExpr *e2) {
    convert_binary_operands_internal(e1, e2);
    assert(e1->type == e2->type);
}

void insert_global_syms(Sym sym) {
    Sym *sym_alloc = xmalloc(sizeof(Sym));
    *sym_alloc = sym;

    map_put(&global_sym_map, sym.name, sym_alloc);
    buf_push(global_sym_list, sym_alloc);
}

void export_printf_func() {
    Type **args = NULL;
    buf_push(args, type_ptr(type_char));
    insert_global_syms((Sym){.name = str_intern("printf"),
                             .state = SYM_RESOLVED,
                             .kind = SYM_FUNC,
                             .type = type_func(args, 1, type_int, true)});
}

void create_base_types() {

    insert_global_syms((Sym){
        .name = str_intern("void"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_void});

    insert_global_syms((Sym){
        .name = str_intern("char"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_char});

    insert_global_syms((Sym){
        .name = str_intern("schar"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_schar});

    insert_global_syms((Sym){
        .name = str_intern("uchar"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_uchar});

    insert_global_syms((Sym){
        .name = str_intern("short"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_short});

    insert_global_syms((Sym){.name = str_intern("ushort"),
                             .state = SYM_RESOLVED,
                             .kind = SYM_TYPE,
                             .type = type_ushort});

    insert_global_syms((Sym){
        .name = str_intern("int"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_int});

    insert_global_syms((Sym){
        .name = str_intern("uint"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_uint});

    insert_global_syms((Sym){
        .name = str_intern("long"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_long});

    insert_global_syms((Sym){
        .name = str_intern("ulong"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_ulong});

    insert_global_syms((Sym){.name = str_intern("llong"),
                             .state = SYM_RESOLVED,
                             .kind = SYM_TYPE,
                             .type = type_longlong});

    insert_global_syms((Sym){.name = str_intern("ullong"),
                             .state = SYM_RESOLVED,
                             .kind = SYM_TYPE,
                             .type = type_ulonglong});

    insert_global_syms((Sym){
        .name = str_intern("float"), .state = SYM_RESOLVED, .kind = SYM_TYPE, .type = type_float});

    insert_global_syms((Sym){.name = str_intern("double"),
                             .state = SYM_RESOLVED,
                             .kind = SYM_TYPE,
                             .type = type_double});
}

void install_global_decl(Decl *decl) {
    if (sym_get(decl->name) != NULL) {
        fatal_error(decl, "symbol %s already exists", decl->name);
    }

    Type *type_aggregate;

    switch (decl->kind) {
    case DECL_CONST:
        insert_global_syms(
            (Sym){.name = decl->name, .kind = SYM_CONST, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    case DECL_VAR:
        insert_global_syms(
            (Sym){.name = decl->name, .kind = SYM_VAR, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    case DECL_AGGREGATE:
        type_aggregate =
            type_alloc((decl->aggregate_decl.kind == AGGREGATE_STRUCT) ? TYPE_STRUCT : TYPE_UNION);
        type_aggregate->aggregate.name = decl->name;
        type_aggregate->state = TYPE_UNRESOLVED;
        type_aggregate->name = decl->name;

        insert_global_syms((Sym){.name = decl->name,
                                 .kind = SYM_TYPE,
                                 .decl = decl,
                                 .state = SYM_UNRESOLVED,
                                 .type = type_aggregate});
        break;
    case DECL_FUNC:
        insert_global_syms(
            (Sym){.name = decl->name, .kind = SYM_FUNC, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    case DECL_ENUM: {
        insert_global_syms((Sym){.name = decl->name,
                                 .kind = SYM_TYPE,
                                 .decl = decl,
                                 .type = type_int,
                                 .state = SYM_UNRESOLVED});

        for (int i = 0; i < decl->enum_decl.num_enum_items; i++) {
            insert_global_syms((Sym){.name = decl->enum_decl.items[i].name,
                                     .kind = SYM_ENUM_MEM,
                                     .decl = decl,
                                     .type = type_int,
                                     .state = SYM_UNRESOLVED,
                                     .enum_decl = decl->name});
        }
        break;
    }
    case DECL_TYPEDEF: {
        insert_global_syms(
            (Sym){.name = decl->name, .kind = SYM_TYPEDEF, .decl = decl, .state = SYM_UNRESOLVED});
        break;
    }
    }
}

typedef struct CachedPtrTypes {
    Type *elem;
    Type *ptr;
} CachedPtrTypes;

// CachedPtrTypes *cached_ptr_types;
Map cached_ptr_types;

Type *type_alloc(TypeKind kind) {
    Type *type = xcalloc(sizeof(Type));
    type->kind = kind;
    return type;
}

Type *type_ptr(Type *elem) {
    Type *ptr;

    if ((ptr = (Type *) map_get(&cached_ptr_types, elem))) {
        return ptr;
    }

    ptr = type_alloc(TYPE_PTR);
    ptr->ptr.elem = elem;
    ptr->size = PTR_SIZE;
    ptr->alignment = PTR_ALIGNMENT;

    map_put(&cached_ptr_types, elem, ptr);
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
        bool is_variadic;
    };
    Type *type;
} CachedFuncType;

CachedFuncType *cached_func_types;

Type *type_func(Type **args, size_t num_args, Type *ret_type, bool is_variadic) {
    for (int i = 0; i < buf_len(cached_func_types); i++) {
        if (num_args != cached_func_types[i].num_args) {
            continue;
        }

        if (cached_func_types[i].is_variadic != is_variadic) {
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
    type->func.args = AST_DUP(args);
    type->func.num_args = num_args;
    type->func.ret_type = ret_type;
    type->func.is_variadic = is_variadic;

    buf_push(cached_func_types, (CachedFuncType){args, num_args, ret_type, is_variadic, type});

    return type;
}

Type *create_type(Typespec *type) {

    if (type == NULL) {
        return type_void;
    }

    switch (type->kind) {
    case TYPESPEC_NAME: {
        Sym *sym = sym_get(type->name);
        if (!sym) {
            fatal_error(type, "Type %s is not defined", type->name);
        }

        if (sym->kind == SYM_TYPEDEF) {
            sym->type = create_type(sym->decl->typedef_decl.type);
        } else if (sym->kind != SYM_TYPE) {
            fatal_error(type, "Symbol %s is not a type", type->name);
        }

        return sym->type;
    }

    case TYPESPEC_PTR:
        return type_ptr(create_type(type->ptr.type));

    case TYPESPEC_ARRAY: {
        ResolvedExpr array_size = {};
        if (type->array.expr) {
            array_size = resolve_expr(type->array.expr);
            convert_operand_type(&array_size, type_size_t);
            if (!array_size.is_const || !is_integer_type(array_size.type)) {
                fatal_error(type, "Illegal: Array size should be a constant of integer type");
            }
        }

        Type *elem = create_type(type->array.type);
        return type_array(elem, array_size.val.ull);
    }

    case TYPESPEC_FUNC: {
        Type **func_args = NULL;
        for (int i = 0; i < type->func.num_args; i++) {
            // function parameter array automatically decays to a pointer
            buf_push(func_args, ptr_decay(create_type(type->func.args[i])));
        }
        Type *ret_type = create_type(type->func.ret);
        return type_func(func_args, buf_len(func_args), ret_type, false);
    }
    }

    return NULL;
}

void complete_struct_type(Type *type) {
    size_t size = 0;
    size_t alignment = 0;

    for (int i = 0; i < type->aggregate.num_fields; i++) {
        Type *t = type->aggregate.fields[i].type;
        alignment = MAX(alignment, t->alignment);
        size = ALIGN_UP(size, t->alignment) + t->size;
    }

    type->size = size;
    type->alignment = alignment;
}

void complete_union_type(Type *type) {
    size_t size = 0;
    size_t alignment = 0;

    for (int i = 0; i < type->aggregate.num_fields; i++) {
        Type *t = type->aggregate.fields[i].type;
        alignment = MAX(alignment, t->alignment);
        size = MAX(ALIGN_UP(t->size, t->alignment), size);
    }

    type->size = size;
    type->alignment = alignment;
}

void complete_type(Type *type) {
    TypeField *fields = NULL;
    size_t num_fields = 0;

    if (type->state == TYPE_RESOLVED) {
        return;
    }

    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
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
            buf_push(fields, (TypeField){.name = aggregate_decl->items[i].names[j], .type = type});
            num_fields++;
        }
    }

    type->aggregate.fields = fields;
    type->aggregate.num_fields = num_fields;
    type->state = TYPE_RESOLVED;

    if (type->kind == TYPE_STRUCT) {
        complete_struct_type(type);
    } else if (type->kind == TYPE_UNION) {
        complete_union_type(type);
    }

    // fast lookup of sym from decl
    sym->decl->sym = sym;
    sym->state = SYM_RESOLVED;

    buf_push(ordered_decls, sym->decl);
}

Type *ptr_decay(Type *type) {
    if (type->kind == TYPE_ARRAY) {
        return type_ptr(type->array.elem);
    }

    return type;
}

ResolvedExpr const_charptr_expr() {
    return (ResolvedExpr){
        .type = type_ptr(type_char),
        .is_const = true,
    };
}

ResolvedExpr const_expr(Type *type, Val val) {
    return (ResolvedExpr){
        .type = type,
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

long long eval_unary_longlong_expr(Expr *expr, TokenKind op, long long val) {
    switch (op) {
    case TOKEN_ADD:
        return val;
    case TOKEN_SUB:
        return -val;
    case TOKEN_NEG:
        return ~val;
    case TOKEN_NOT:
        return !val;
    default:
        fatal_error(expr, "Operator not supported for unary expressions");
    }
}

unsigned long long eval_unary_ulonglong_expr(Expr *expr, TokenKind op, unsigned long long val) {
    switch (op) {
    case TOKEN_ADD:
        return val;
    case TOKEN_SUB:
        return -val;
    case TOKEN_NEG:
        return ~val;
    case TOKEN_NOT:
        return !val;
    default:
        fatal_error(expr, "Operator not supported for unary expressions");
    }
}

long long eval_binary_longlong_expr(Expr *expr, long long left, long long right, TokenKind op) {
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
        fatal_error(expr, "Operator not supported for unary expressions");
    }
}

unsigned long long eval_binary_ulonglong_expr(Expr *expr, unsigned long long left,
                                              unsigned long long right, TokenKind op) {
    switch (op) {
    case TOKEN_MUL:
        return left * right;
    case TOKEN_DIV:
        return left / right;
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
        fatal_error(expr, "Operator not supported for unary expressions");
        assert(0);
    }
}

ResolvedExpr resolve_binary_expr(Expr *expr) {
    ResolvedExpr expr_left = resolve_expr(expr->binary_expr.left);
    ResolvedExpr expr_right = resolve_expr(expr->binary_expr.right);
    convert_binary_operands(&expr_left, &expr_right);
    Type *bin_expr_type = expr_left.type;

    ResolvedExpr eval_const_expr = {};
    bool const_eval = false;

    if (expr_left.is_const == true && expr_right.is_const == true) {
        const_eval = true;
        if (is_signed_type(bin_expr_type)) {
            convert_operand_type(&expr_left, type_longlong);
            convert_operand_type(&expr_right, type_longlong);
            eval_const_expr.type = type_longlong;
            eval_const_expr.val.ll = eval_binary_longlong_expr(
                expr, expr_left.val.ll, expr_right.val.ll, expr->binary_expr.op);
        } else {
            convert_operand_type(&expr_left, type_ulonglong);
            convert_operand_type(&expr_left, type_ulonglong);
            eval_const_expr.type = type_ulonglong;
            eval_const_expr.val.ull = eval_binary_ulonglong_expr(
                expr, expr_left.val.ull, expr_right.val.ull, expr->binary_expr.op);
        }
    }

    switch (expr->binary_expr.op) {
    case TOKEN_MUL:
    case TOKEN_DIV:
    case TOKEN_MOD:
    case TOKEN_AND:
    case TOKEN_LSHIFT:
    case TOKEN_RSHIFT:
    case TOKEN_ADD:
    case TOKEN_SUB:
    case TOKEN_OR:
    case TOKEN_XOR: {
        if (const_eval == true) {
            convert_operand_type(&eval_const_expr, bin_expr_type);
            return const_expr(bin_expr_type, eval_const_expr.val);
        } else {
            return rvalue_expr(bin_expr_type);
        }
    }

    case TOKEN_EQ:
    case TOKEN_LT:
    case TOKEN_GT:
    case TOKEN_LTEQ:
    case TOKEN_GTEQ:
    case TOKEN_NOTEQ:
    case TOKEN_AND_AND:
    case TOKEN_OR_OR: {
        if (const_eval == true) {
            convert_operand_type(&eval_const_expr, type_int);
            return const_expr(type_int, eval_const_expr.val);
        } else {
            return rvalue_expr(type_int);
        }
    }
    }
}

ResolvedExpr resolve_name_expr(Expr *expr) {
    Sym *sym = sym_get(expr->name);
    if (!sym) {
        fatal_error(expr, "Expected sym %s to exist", expr->name);
    }

    resolve_global_sym(sym);
    if (sym->kind == SYM_VAR) {
        return lvalue_expr(sym->type);
    } else if (sym->kind == SYM_CONST) {
        return const_expr(type_int, sym->val);
    } else if (sym->kind == SYM_ENUM_MEM) {
        return const_expr(type_int, sym->val);
    } else if (sym->kind == SYM_FUNC) {
        return rvalue_expr(sym->type);
    } else {
        fatal_error(expr, "%s can only be var, const or func", expr->name);
    }
    return (ResolvedExpr){};
}

ResolvedExpr resolve_unary_op(Expr *expr, ResolvedExpr *operand, Type *final_type) {
    if (operand->is_const) {
        if (is_signed_type(operand->type)) {
            convert_operand_type(operand, type_longlong);
            operand->val.ll = eval_unary_longlong_expr(expr, expr->unary_expr.op, operand->val.ll);
        } else {
            convert_operand_type(operand, type_ulonglong);
            operand->val.ull =
                eval_unary_ulonglong_expr(expr, expr->unary_expr.op, operand->val.ull);
        }

        convert_operand_type(operand, final_type);
        return const_expr(final_type, operand->val);
    } else {
        return rvalue_expr(final_type);
    }
}

ResolvedExpr resolve_unary_expr(Expr *expr) {
    // *,&,+,-,~,!
    ResolvedExpr operand = resolve_expr(expr->unary_expr.expr);
    TokenKind op = expr->unary_expr.op;

    if (op == TOKEN_MUL) {
        operand.type = ptr_decay(operand.type);
        if (operand.type->kind != TYPE_PTR) {
            fatal_error(expr, "Can deref only a pointer type");
        }
        return lvalue_expr(operand.type->ptr.elem);
    } else if (op == TOKEN_AND) {
        if (!operand.is_lvalue) {
            fatal_error(expr, "can not take address of an rvalue expression");
        }
        return rvalue_expr(type_ptr(operand.type));
    } else if (op == TOKEN_ADD || op == TOKEN_SUB) {
        if (!is_arithmetic_type(operand.type)) {
            fatal_error(expr, "unary operator +, - are applied on arithmetic types only");
        }
        promote_integer(&operand);
        return resolve_unary_op(expr, &operand, operand.type);
    } else if (op == TOKEN_NEG) {
        if (!is_integer_type(operand.type)) {
            fatal_error(expr, "unary operator ~ are applied on integer types only");
        }
        promote_integer(&operand);
        return resolve_unary_op(expr, &operand, operand.type);
    } else if (op == TOKEN_NOT) {
        if (!is_scalar_type(operand.type)) {
            fatal_error(expr, "unary operator ! requires scalar types");
        }
        return resolve_unary_op(expr, &operand, type_int);
    }

    assert(0);
}

ResolvedExpr resolve_field_expr(Expr *expr) {
    ResolvedExpr base = resolve_expr(expr->field_expr.expr);

    if (base.type->kind != TYPE_STRUCT && base.type->kind != TYPE_UNION) {
        fatal_error(expr, "Error: only struct types have access fields");
    }

    for (int i = 0; i < base.type->aggregate.num_fields; i++) {
        if (base.type->aggregate.fields[i].name == expr->field_expr.name) {
            return lvalue_expr(base.type->aggregate.fields[i].type);
        }
    }

    fatal_error(expr, "%s field does not exist with type %s", expr->field_expr.name,
                base.type->aggregate.name);
}

ResolvedExpr resolve_index_expr(Expr *expr) {
    ResolvedExpr index = resolve_expr(expr->index_expr.index);

    if (!is_integer_type(index.type)) {
        fatal_error(expr, "Illegal array indices can be of integer type only");
    }

    convert_operand_type(&index, type_size_t);

    if (index.type->kind != TYPE_SIZE_T) {
        fatal_error(expr, "array indices can be of type integer only");
    }

    ResolvedExpr operand = resolve_expr(expr->index_expr.expr);
    if (operand.type->kind != TYPE_PTR && operand.type->kind != TYPE_ARRAY) {
        fatal_error(expr, "Only pointer and array types can be indexed");
    }

    operand.type = ptr_decay(operand.type);
    return lvalue_expr(operand.type->ptr.elem);
}

ResolvedExpr resolve_call_expr(Expr *expr) {
    ResolvedExpr func = resolve_expr(expr->call_expr.expr);
    if (func.type->kind != TYPE_FUNC) {
        fatal_error(expr, "Expected type to be func");
    }

    if (!func.type->func.is_variadic && expr->call_expr.num_args != func.type->func.num_args) {
        fatal_error(expr, "func required %d arguments, but being passed %d",
                    func.type->func.num_args, expr->call_expr.num_args);
    } else if (func.type->func.is_variadic && expr->call_expr.num_args < func.type->func.num_args) {
        fatal_error(expr,
                    "less arguments being passed to a variadic function: required %d arguments, "
                    "but being passed %d",
                    func.type->func.num_args, expr->call_expr.num_args);
    }

    for (int i = 0; i < func.type->func.num_args; i++) {
        ResolvedExpr expr_args =
            resolve_expected_expr(expr->call_expr.args[i], func.type->func.args[i]);
        expr_args.type = ptr_decay(expr_args.type);

        if (expr_args.type != func.type->func.args[i]) {
            fatal_error(expr, "func arg types %d do not match. expr type %s but function type %s",
                        i, type_kind[expr_args.type->kind],
                        type_kind[func.type->func.args[i]->kind]);
        }
    }

    for (int i = func.type->func.num_args; i < expr->call_expr.num_args; i++) {
        resolve_expr(expr->call_expr.args[i]);
    }

    return rvalue_expr(func.type->func.ret_type);
}

ResolvedExpr resolve_cast_expr(Expr *expr) {
    ResolvedExpr original = resolve_expr(expr->cast_expr.expr);
    Type *casted_type = create_type(expr->cast_expr.type);

    if (!is_arithmetic_type(original.type) && original.type->kind != TYPE_PTR &&
        original.type->kind != TYPE_ARRAY) {
        fatal_error(expr, "only integer, float, doubles and pointer types can be casted");
    }

    if (!is_arithmetic_type(casted_type) && casted_type->kind != TYPE_PTR) {
        fatal_error(expr, "can only cast to scalar(arithmetic, pointer) types");
    }

    return rvalue_expr(casted_type);
}

size_t get_index_field(Expr *expr, Type *type, const char *name) {

    if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION) {
        fatal_error(expr, "only struct and union types can have index to fields");
    }

    for (size_t i = 0; i < type->aggregate.num_fields; i++) {
        if (name == type->aggregate.fields[i].name) {
            return i;
        }
    }

    fatal_error(expr, "Named field does not exist in the type");
}

/* This is the trickiest of them all. In ion compound expressions can have */
/* inferred types ie in a func returning struct foo{i,j:int;}. A return expression */
/* {1,2} is automatically inferred to have type struct foo */

ResolvedExpr resolve_compound_expr(Expr *expr, Type *expected_type) {
    Type *compound_expr_type = create_type(expr->compound_expr.type);

    // If both types are not void then they should match
    if (expected_type != compound_expr_type && expected_type != type_void &&
        compound_expr_type != type_void) {
        fatal_error(expr, "compound expression type and expected type do not match");
    }

    // Take the type that is not void
    expected_type = (expected_type != type_void) ? expected_type : compound_expr_type;

    if (expected_type == type_void) {
        fatal_error(expr, "compound expression type can not be void");
    }

    complete_type(expected_type);

    if (is_scalar_type(expected_type)) {
        if (expr->compound_expr.num_args != 1) {
            fatal_error(expr,
                        "scalar compound expressions can not have more than one compound literal");
        }

        CompoundVal *val = expr->compound_expr.args[0];
        if (val->kind != SIMPLE_EXPR) {
            fatal_error(expr, "scalar compound expression literal has to be a simple expression");
            return lvalue_expr(type_void);
        }

        Expr *expr_literal = val->expr;
        ResolvedExpr resolved_arg = resolve_expected_expr(expr_literal, expected_type);
        if (!is_scalar_type(resolved_arg.type)) {
            fatal_error(expr, "type of expression in compound literal: %s type expected %s",
                        type_kind[resolved_arg.type->kind], type_kind[expected_type->kind]);
            return lvalue_expr(type_void);
        }
    } else if (expected_type->kind == TYPE_STRUCT || expected_type->kind == TYPE_UNION) {
        if (expr->compound_expr.type != NULL && expected_type != NULL) {
            if (create_type(expr->compound_expr.type) != expected_type) {
                fatal_error(expr,
                            "Expected type of compound literal should match the declared type of "
                            "compound literal");
                return lvalue_expr(type_void);
            }
        }

        if (expected_type->aggregate.num_fields < expr->compound_expr.num_args) {
            fatal_error(expr,
                        "Actual type has fewer memebers than being passed in the compound literal");
            return lvalue_expr(type_void);
        }

        for (size_t i = 0, j = 0; i < expr->compound_expr.num_args; i++, j++) {
            if (j == expected_type->aggregate.num_fields) {
                fatal_error(expr, "field initializer out of range");
            }

            CompoundVal *val = expr->compound_expr.args[i];
            Expr *expr_literal = NULL;

            switch (val->kind) {
            case SIMPLE_EXPR:
                expr_literal = val->expr;
                break;
            case NAME_EXPR:
                j = get_index_field(expr, expected_type, val->name.name);
                expr_literal = val->name.val;
                break;
            case INDEX_EXPR:
                fatal_error(expr, "Can not have index expression inside struct compound literal");
                break;
            default:
                assert(0);
            }

            ResolvedExpr resolved_arg =
                resolve_expected_expr(expr_literal, expected_type->aggregate.fields[j].type);
            if (resolved_arg.type != expected_type->aggregate.fields[j].type) {
                fatal_error(expr, "type of expression in compound literal: %s type expected %s",
                            type_kind[resolved_arg.type->kind],
                            type_kind[expected_type->aggregate.fields[j].type->kind]);
                return lvalue_expr(type_void);
            }
        }
    } else if (expected_type->kind == TYPE_ARRAY) {
        if (expr->compound_expr.type != NULL && expected_type != NULL) {
            if ((compound_expr_type->array.elem != expected_type->array.elem) ||
                (compound_expr_type->array.size != expected_type->array.size)) {
                fatal_error(expr, "array compound literals types do not match");
            }
        }

        if (expected_type->array.size && expected_type->array.size < expr->compound_expr.num_args) {
            fatal_error(
                expr,
                "compound literal has more array members than the defined type: expected: %d "
                "actual :%d",
                expected_type->array.size, expr->compound_expr.num_args);
        }

        size_t max_index = 0;
         for (size_t i = 0, array_index = 0; i < expr->compound_expr.num_args; i++, array_index++) {
            if (expected_type->array.size && array_index >= expected_type->array.size) {
                fatal_error(expr, "compound literal initializing array out of bounds");
            }

            CompoundVal *val = expr->compound_expr.args[i];
            Expr *expr_literal = NULL;

            switch (val->kind) {
            case SIMPLE_EXPR:
                expr_literal = val->expr;
                break;
            case INDEX_EXPR: {
                ResolvedExpr expr_index = resolve_expr(val->index.index);
                if (!expr_index.is_const || !is_integer_type(expr_index.type)) {
                    fatal_error(expr,
                                "Field initializer index in array compound expression has to be a "
                                "constant");
                }
                convert_operand_type(&expr_index, type_size_t);

                array_index = (size_t) expr_index.val.ull;
                if (expected_type->array.size && array_index >= expected_type->array.size) {
                    fatal_error(expr, "compound literal initializing array out of bounds");
                }

                expr_literal = val->index.val;
                break;
            }
            case NAME_EXPR:
                fatal_error(expr,
                            "Can not initialize named fields inside array compound expression");
                break;
            default:
                assert(0);
            }

            ResolvedExpr resolved_arg =
                resolve_expected_expr(expr_literal, expected_type->array.elem);
            if (resolved_arg.type != expected_type->array.elem) {
                fatal_error(expr, "type of expression: %s expected type %s",
                            type_kind[resolved_arg.type->kind], type_kind[resolved_arg.type->kind]);
                return lvalue_expr(type_void);
            }
            max_index = MAX(max_index, array_index);
        }

        *expected_type = *type_array(expected_type->array.elem, max_index + 1);
    }

    return lvalue_expr(expected_type);
}

ResolvedExpr resolve_ternary_expr(Expr *expr, Type *expected_type) {
    ResolvedExpr cond = resolve_expr(expr->ternary_expr.eval);

    if (!is_arithmetic_type(cond.type) && cond.type->kind != TYPE_PTR) {
        fatal_error(expr, "ternary condition expression should be of type scalar");
    }

    ResolvedExpr then_expr = resolve_expected_expr(expr->ternary_expr.then_expr, expected_type);
    ResolvedExpr else_expr = resolve_expected_expr(expr->ternary_expr.else_expr, expected_type);

    if (then_expr.type != else_expr.type) {
        fatal_error(expr, "Ternary then else expressions types should match");
    }

    if (cond.is_const) {
        convert_operand_type(&cond, type_int);
        return (cond.val.i) ? then_expr : else_expr;
    } else {
        return rvalue_expr(then_expr.type);
    }
}

ResolvedExpr resolve_sizeof_type(Expr *expr) {
    Type *type = create_type(expr->sizeof_type);
    complete_type(type);
    return const_expr(type_size_t, (Val){.ull = type->size});
}

ResolvedExpr resolve_sizeof_expr(Expr *expr) {
    ResolvedExpr type_expr = resolve_expr(expr->sizeof_expr);
    complete_type(type_expr.type);
    return const_expr(type_size_t, (Val){.ull = type_expr.type->size});
}

ResolvedExpr resolve_expected_expr(Expr *expr, Type *expected_type) {
    if (!expr) {
        return (ResolvedExpr){.type = type_void};
    }

    switch (expr->kind) {
    case EXPR_INT:
        return const_expr(type_int, (Val){.i = expr->int_val});
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
    case EXPR_STR:
        return const_charptr_expr();
    default:
        assert(0);
        return (ResolvedExpr){};
    }
}

ResolvedExpr resolve_expr(Expr *expr) {
    return resolve_expected_expr(expr, type_void);
}

Type *order_decl_var(Decl *decl) {
    Type *type = create_type(decl->var_decl.type);

    if (decl->var_decl.expr) {
        ResolvedExpr expr = resolve_expected_expr(decl->var_decl.expr, type);
        if (type != type_void && type != expr.type) {
            fatal_error(decl, "expression type and var type do not match for %s", decl->name);
        }

        type = expr.type;
    }

    if (type == type_void) {
        fatal_error(decl, "%s var declared as void. This is illegal", decl->name);
    }

    complete_type(type);
    return type;
}

Type *order_decl_const(Decl *decl, int *val) {
    ResolvedExpr resolved_expr = resolve_expr(decl->const_decl.expr);
    if (!resolved_expr.is_const) {
        fatal_error(decl, "%s declared as constant, but the value assigned is not a constant",
                    decl->name);
    }
    *val = resolved_expr.val.i;
    return resolved_expr.type;
}

void order_enum_const(const char *name, Expr *expr, int *val) {
    ResolvedExpr resolved_expr = resolve_expr(expr);
    if (!resolved_expr.is_const) {
        fatal_error(expr, "%s declared as constant, but the value assigned is not a constant",
                    name);
    }
    *val = resolved_expr.val.i;
}

void resolve_global_sym(Sym *sym) {
    if (sym->state == SYM_RESOLVED) {
        return;
    }

    Decl *decl = sym->decl;

    if (sym->state == SYM_RESOLVING) {
        fatal_error(decl, "illegal value cycle while resolving %s in types", sym->name);
    }

    sym->state = SYM_RESOLVING;

    // resolve the dependencies of this decl
    ResolvedExpr resolved_expr = {0};
    switch (decl->kind) {
    case DECL_CONST:
        sym->type = order_decl_const(decl, &sym->val.i);
        break;
    case DECL_VAR:
        sym->type = order_decl_var(decl);
        break;
    case DECL_AGGREGATE:
        return;
    case DECL_FUNC: {
        Type **func_args = NULL;
        for (int i = 0; i < decl->func_decl.num_func_args; i++) {
            Type *type = create_type(decl->func_decl.args[i].type);
            complete_type(type);
            buf_push(func_args, type);
        }
        Type *ret_type = create_type(decl->func_decl.type);
        sym->type = type_func(func_args, buf_len(func_args), ret_type, decl->func_decl.is_variadic);
        break;
    }
    case DECL_TYPEDEF: {
        sym->type = create_type(decl->typedef_decl.type);
        break;
    }
    case DECL_ENUM: {
        if (sym->kind == SYM_TYPE) {
            int val = 0;
            for (int i = 0; i < decl->enum_decl.num_enum_items; i++) {
                Sym *sym = sym_get(decl->enum_decl.items[i].name);
                if (!decl->enum_decl.items[i].expr) {
                    sym->val.i = val;
                } else {
                    order_enum_const(sym->name, decl->enum_decl.items[i].expr, &sym->val.i);
                    val = sym->val.i;
                }
                val++;
                sym->state = SYM_RESOLVED;
            }
        } else {
            Sym *sym_enum = sym_get(sym->enum_decl);
            resolve_global_sym(sym_enum);
            return;
        }
    }
    }
    // fast lookup of sym from decl
    sym->decl->sym = sym;
    buf_push(ordered_decls, sym->decl);
    sym->state = SYM_RESOLVED;
}

enum {
    MAX_LOCAL_SYMS = 1024,
};

Sym local_symbol_table[MAX_LOCAL_SYMS];
Sym *local_syms = local_symbol_table;
Sym *local_sym_end = &local_symbol_table[MAX_LOCAL_SYMS - 1];

Sym *scope_enter() {
    return local_syms;
}

void scope_leave(Sym *sym) {
    local_syms = sym;
}

void sym_push_var(const char *name, Type *type, Sym *scope_start) {
    if (local_syms == local_sym_end) {
        fatal("Too many local syms");
    }

    for (Sym *it = local_syms; it > scope_start; it--) {
        if (it[-1].name == name) {
            fatal("Symbol %s redeclared", name);
        }
    }

    *local_syms++ = (Sym){.name = name, .kind = SYM_VAR, .state = SYM_RESOLVED, .type = type};
}

Sym *sym_get(const char *name) {
    for (Sym *it = local_syms; it > local_symbol_table; it--) {
        if (it[-1].name == name) {
            return &it[-1];
        }
    }

    return (Sym *) map_get(&global_sym_map, name);
}

void resolve_stmtblock(StmtBlock block, Type *expected_type, Sym *scope_start) {
    for (int i = 0; i < block.num_stmts; i++) {
        resolve_stmt(block.stmts[i], expected_type, scope_start);
    }
}

void resolve_cond_expr(Expr *expr) {
    ResolvedExpr cond_expr = resolve_expr(expr);
    if (!is_arithmetic_type(cond_expr.type)) {
        fatal_error(expr, "If expression should be of type int");
    }
}

void resolve_stmt(Stmt *stmt, Type *expected_type, Sym *scope_start) {
    switch (stmt->kind) {
    case STMT_NONE:
        assert(0);

    case STMT_DECL:
        break;

    case STMT_RETURN: {
        ResolvedExpr expr = resolve_expected_expr(stmt->stmt_return.expr, expected_type);
        if (expr.type != expected_type) {
            fatal_error(stmt, "Return type of func doesn't match the function signature");
        }
        break;
    }
    case STMT_BREAK:
    case STMT_CONTINUE:
        break;
    case STMT_BLOCK: {
        Sym *scope_start = scope_enter();
        resolve_stmtblock(stmt->block, expected_type, scope_start);
        scope_leave(scope_start);
        break;
    }
    case STMT_IF: {
        resolve_cond_expr(stmt->stmt_if.expr);
        Sym *scope_start = scope_enter();
        resolve_stmtblock(stmt->stmt_if.if_block, expected_type, scope_start);
        scope_leave(scope_start);
        for (int i = 0; i < stmt->stmt_if.num_elseifs; i++) {
            resolve_cond_expr(stmt->stmt_if.else_ifs[i].expr);
            scope_start = scope_enter();
            resolve_stmtblock(stmt->stmt_if.else_ifs[i].block, expected_type, scope_start);
            scope_leave(scope_start);
        }

        scope_start = scope_enter();
        resolve_stmtblock(stmt->stmt_if.else_block, expected_type, scope_start);
        scope_leave(scope_start);
        break;
    }
    case STMT_DO_WHILE:
    case STMT_WHILE: {
        resolve_cond_expr(stmt->stmt_while.expr);
        Sym *scope_start = scope_enter();
        resolve_stmtblock(stmt->stmt_while.block, expected_type, scope_start);
        scope_leave(scope_start);
        break;
    }

    case STMT_FOR: {
        // outer for scope
        Sym *scope_start_for = scope_enter();
        resolve_stmt(stmt->stmt_for.init, NULL, scope_start_for);
        resolve_cond_expr(stmt->stmt_for.cond);
        resolve_stmt(stmt->stmt_for.next, NULL, scope_start_for);
        // inner scope
        Sym *scope_start_inner = scope_enter();
        resolve_stmtblock(stmt->stmt_for.block, expected_type, scope_start_inner);
        // leave inner scope
        scope_leave(scope_start_inner);
        // leave outer for scope
        scope_leave(scope_start_for);
        break;
    }
    case STMT_SWITCH: {
        ResolvedExpr switch_expr = resolve_expr(stmt->stmt_switch.expr);
        Sym *scope_start = scope_enter();
        for (size_t i = 0; i < stmt->stmt_switch.num_cases; i++) {
            for (size_t j = 0; j < stmt->stmt_switch.cases->num_exprs; j++) {
                ResolvedExpr expr_case = resolve_expr(stmt->stmt_switch.cases->expr[j]);
                if (expr_case.type != switch_expr.type) {
                    fatal_error(stmt, "switch and case expression types should match");
                }
            }

            Sym *scope_inner = scope_enter();
            resolve_stmtblock(stmt->stmt_switch.cases->block, expected_type, scope_inner);
            scope_leave(scope_inner);
        }
        scope_leave(scope_start);
        break;
    }
    case STMT_ASSIGN: {
        ResolvedExpr left_expr = resolve_expr(stmt->stmt_assign.left_expr);
        ResolvedExpr right_expr = resolve_expr(stmt->stmt_assign.right_expr);

        if (left_expr.type->kind == TYPE_ARRAY) {
            fatal_error(stmt, "can not assign to lvalue of type array");
        }

        if (!left_expr.is_lvalue) {
            fatal_error(stmt, "Can not assign value to non lvalue expression");
        }

        if (right_expr.type != type_void) {
            convert_operand_type(&left_expr, right_expr.type);

            if (left_expr.type != right_expr.type) {
                fatal_error(
                    stmt,
                    "Left and right expression types do not match in the assignment statement");
            }
        } else {
            if (!is_scalar_type(left_expr.type)) {
                fatal_error(stmt, "can only use %s with a scalar type",
                            token_kind_name(stmt->stmt_assign.op));
            }
        }

        break;
    }

    case STMT_INIT: {
        ResolvedExpr rvalue_expr = resolve_expr(stmt->stmt_init.expr);
        sym_push_var(stmt->stmt_init.name, rvalue_expr.type, scope_start);
        stmt->stmt_init.type = rvalue_expr.type;
        break;
    }
    case STMT_EXPR:
        resolve_expr(stmt->expr);
        break;
    default:
        assert(0);
        break;
    }
}

void resolve_func_body(Sym *sym) {
    assert(sym->decl->kind == DECL_FUNC);

    Decl *decl = sym->decl;
    Sym *scope_start = scope_enter();

    for (size_t i = 0; i < decl->func_decl.num_func_args; i++) {
        sym_push_var(decl->func_decl.args[i].name, create_type(decl->func_decl.args[i].type),
                     scope_start);
    }

    resolve_stmtblock(decl->func_decl.block, create_type(decl->func_decl.type), scope_start);
    scope_leave(scope_start);
}

void install_global_decls(DeclSet *declset) {
    for (int i = 0; i < declset->num_decls; i++) {
        install_global_decl(declset->decls[i]);
    }
}

void test_type_conversion() {
    ResolvedExpr expr;
#define promote_integer(set_type, promoted_type)                                                   \
    {                                                                                              \
        expr.type = set_type;                                                                      \
        promote_integer(&expr);                                                                    \
        assert(expr.type == promoted_type);                                                        \
    }

    promote_integer(type_char, type_int);
    promote_integer(type_schar, type_int);
    promote_integer(type_uchar, type_int);
    promote_integer(type_short, type_int);
    promote_integer(type_ushort, type_int);
    promote_integer(type_int, type_int);
    promote_integer(type_uint, type_uint);
    promote_integer(type_long, type_long);
    promote_integer(type_ulong, type_ulong);
    promote_integer(type_longlong, type_longlong);
    promote_integer(type_ulonglong, type_ulonglong);
    promote_integer(type_float, type_float);
    promote_integer(type_double, type_double);

#undef promote_integer
    ResolvedExpr expr1, expr2;
#define convert_arithmetic_type(type_first, type_second, converted_type)                           \
    {                                                                                              \
        expr1.type = type_first;                                                                   \
        expr2.type = type_second;                                                                  \
        convert_binary_operands(&expr1, &expr2);                                                   \
        assert(expr1.type == converted_type);                                                      \
    }

    convert_arithmetic_type(type_char, type_char, type_int);
    convert_arithmetic_type(type_char, type_ushort, type_int);
    convert_arithmetic_type(type_int, type_long, type_long);
    convert_arithmetic_type(type_long, type_ulong, type_ulong);

#undef convert_arithmetic_type

    ResolvedExpr expr3;
#define convert_operand_type(set_val, set_type, type_convert, new_val)                             \
    {                                                                                              \
        expr3.type = set_type;                                                                     \
        expr3.val = set_val;                                                                       \
        convert_operand_type(&expr3, type_convert);                                                \
        assert(!memcmp(&expr3.val, &new_val, sizeof(Val)));                                        \
    }

    convert_operand_type((Val){.i = -1}, type_int, type_uint, (Val){.ui = UINT_MAX});
}

void resolve_test() {
    test_type_conversion();

    const char *decl[] = {
        // test enum
        "enum enumD{A=F,B,C}",
        "enum enumE{F=3,G}",
        "var testenumarr :int[C] = {1,2,3,4,5}",
        // Test functions and Statements
        //      check return statement matching the type
        "struct funcTest1 {i,j :int;}",
        "union testunion {i, j :int;}",
        "var testvarunion : testunion",
        "var testvari :int = testvarunion.i",
        "func foo1(a :int):funcTest1 {return funcTest1{1,2};}",
        "var Testa :int",
        "func foo2(a :int):int {if (a) {return a;} else {return Testa;}}",
        "func foo3():funcTest1 {return {1,2};}",
        // Test type inference
        "func foo4():int {i := funcTest1{1,2}; return 1;}",
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
        /* // test struct fields.. negative tests are also evaluated. */
        "struct TestA {a: TestB*;}",
        "struct TestB {t: TestA;}",
        "var test :TestA",
        "var test2:TestB* = test.a",
        /* // test index fields */
        "struct TestC {a :int[10]; b :int**;}",
        "var testc :TestC",
        "var test4 = testc.b[0]",
        "var test3:int = testc.a[0]",

        /* // test call expr */
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
        /* /\* // test teranary *\/ */
        "var ta = g ? 2 : 3",
        // test sizeof expr
        "const size_int = sizeof(:int)",
        "const size_int1 = sizeof(ta)",
        //        test compound literal
        "var ce = (:int[2][2]) {{1,2}, {1,2}}",
        "struct C1 {a :int; b :int;}",
        "var ca = C1{1,2}",
        "var ck = C3{1, &ca.a}",
        "struct C2 {a:C1; b:int;}",
        "var cb :C2 = {{1,2},3}",
        "var cc :int[3] = {1,2,4}",
        "var cd = (:int[3]){1,2,3}",

        "var cf:int[2][2] = {{2,3},{4,cd[0]}}",
        "var cg = C1{1,2}",
        "struct C3 {i :int; j:int*;}",

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
        "var testM = (:nVector[2]){{},{}}",
    };

    create_base_types();

    for (int i = 0; i < sizeof(decl) / sizeof(*decl); i++) {
        init_stream(NULL, decl[i]);
        Decl *d = parse_decl_opt();
        assert(d);
        install_global_decl(d);
    }

    for (Sym **sym = global_sym_list; sym != buf_end(global_sym_list); sym++) {
        if ((*sym)->decl) {
            resolve_global_sym(*sym);
        }
    }

    for (Sym **sym = global_sym_list; sym != buf_end(global_sym_list); sym++) {
        if ((*sym)->type) {
            complete_type((*sym)->type);
        }

        if ((*sym)->decl && (*sym)->decl->kind == DECL_FUNC) {
            resolve_func_body(*sym);
        }
    }

    for (Decl **decl = ordered_decls; decl != buf_end(ordered_decls); decl++) {
        print_decl(*decl);
        printf("\n");
    }
}
