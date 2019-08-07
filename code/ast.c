typedef enum DeclKind {
    DECL_NONE,
    DECL_ENUM,
    DECL_STRUCT,
    DECL_UNION,
    DECL_VAR,
    DECL_CONST,
    DECL_FUNC,
    DECL_TYPEDEF,
} DeclKind;

typedef enum AggregateKind {
    AGGREGATE_NONE,
    AGGREGATE_STRUCT,
    AGGREGATE_UNION,
} AggregateKind;

typedef struct EnumItem {
    const char *name;
    Expr *expr;
} EnumItem;

typedef struct EnumDecl {
    EnumItem *items;
    size_t num_enum_items;
} EnumDecl;

typedef struct AggregateItem {
    char **names;
    size_t num_items;
    Typespec *type;
} AggregateItem;

typedef struct AggregateDecl {
    size_t num_items;
    AggregateItem *items;
} AggregateDecl;

typedef struct FuncArg {
    const char *name;
    Typespec *type;
} FuncArg;

typedef struct FuncDecl {
    size_t num_func_args;
    FuncArg *args;
    Typespec *type;
    StmtBlock block;
} FuncDecl;

typedef struct TypedefDecl {
    Typespec *type;
} TypedefDecl;

typedef struct VarDecl {
    Typespec *type;
    Expr *expr;
} VarDecl;

typedef struct ConstDecl {
    Expr *expr;
} ConstDecl;

typedef struct Decl {
    DeclKind kind;
    const char *name;
    union {
        EnumDecl enum_decl;
        AggregateDecl aggregate_decl;
        FuncDecl func_decl;
        TypedefDecl typedef_decl;
        VarDecl var_decl;
        ConstDecl const_decl;
    };
} Decl;

Decl *decl_new(DeclKind kind, const char *name) {
    Decl *decl = xmalloc(sizeof(Decl));
    decl->name = name;
    decl->kind = kind;
    return decl;
}

Decl *decl_enum(const char *name, EnumItem *items, size_t num_enum_items) {
    Decl *decl = decl_new(DECL_ENUM, name);
    decl->enum_decl.items = items;
    decl->enum_decl.num_enum_items = num_enum_items;
    return decl;
}

Decl *decl_var(const char *name, Typespec *type, Expr *expr) {
    Decl *decl = decl_new(DECL_VAR, name);
    decl->var_decl.type = type;
    decl->var_decl.expr = expr;
    return decl;
}

Decl *decl_const(const char *name, Expr *expr) {
    Decl *decl = decl_new(DECL_CONST, name);
    decl->const_decl.expr = expr;
    return decl;
}

Decl *decl_aggregate(AggregateKind kind, const char *name, AggregateItem *items,
                     size_t num_items, ) {
    Decl *decl = decl_new(kind, name);
    decl->aggregate_decl.items = items;
    decl->aggregate_decl.num_items = num_items;
    return decl;
}

Decl *decl_func(const char *name, FuncArg *args, size_t num_func_args, Typespec *type,
                StmtBlock block) {
    Decl *decl = decl_new(DECL_FUNC, name);
    decl->func_decl.args = args;
    decl->func_decl.num_func_args = num_func_args;
    decl->func.decl.type = type;
    decl->func_decl.block = block;
}

Decl *decl_typedef(const char *name, Typespec *type) {
    Decl *decl = decl_new(DECL_TYPEDEF, name);
    decl->typedef_decl.type = type;
}

typedef enum ExprKind {
    EXPR_NONE,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_CAST,
    EXPR_COMPOUND,
    EXPR_SIZEOF_TYPE,
    EXPR_SIZEOF_EXPR,
    EXPR_FIELD,
    EXPR_INDEX,
    EXPR_CALL,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
} ExprKind;

typedef struct Expr {
    ExprKind kind;
    union {
        uint64_t int_val;
        double float_val;
        char *str_val;
        char *name;
        struct {
            Expr *expr;
            Expr *index
        } index_expr;

        struct {
            Expr *expr;
            char *name;
        } field_expr;

        struct {
            Expr *expr;
            Expr **args;
            size_t num_args;
        } call_expr;

        struct {
            Typespec *type;
            Expr **args;
            size_t num_args;
        } compound_expr;

        Typespec *sizeof_type;
        Expr *sizeof_expr;

        struct {
            Typespec *type;
            Expr *expr;
        } cast_expr;
        struct {
            tokenKind op;
            Expr *expr;
        } unary_expr;

        struct {
            Expr *left;
            tokenKind op;
            Expr *right;
        } binary_expr;

        struct {
            Expr *eval;
            Expr *then_expr;
            Expr *else_expr;
        } ternary_expr;
    };
} Expr;

Expr *expr_new(ExprKind kind) {
    Expr *expr = xmalloc(sizeof(struct Expr));
    expr->kind = kind;
    return expr;
}

Expr *expr_int(uint64_t int_val) {
    Expr *expr = new_expr(EXPR_INT);
    expr->int_val = int_val;
    return expr;
}

Expr *expr_float(double float_val) {
    Expr *expr = new_expr(EXPR_FLOAT);
    expr->float_val = float_val;
    return expr;
}

Expr *expr_str(char *str) {
    Expr *expr = new_expr(EXPR_STR);
    expr->str_val = str;
    return expr;
}

Expr *expr_name(char *name) {
    Expr *expr = new_expr(EXPR_NAME);
    expr->name_val = name;
    return expr;
}

Expr *expr_compound(Typespec *type, Expr **args, size_t num_args) {
    Expr *expr = new_expr(EXPR_COMPOUND);
    expr->compound_expr.type = type;
    expr->compound_expr.args = args;
    expr->compound_expr.num_args = num_args;
    return expr;
}

Expr *expr_sizeof_type(Typespec *type) {
    Expr *expr = new_expr(EXPR_SIZEOF_TYPE);
    expr->sizeof_type = type;
    return expr;
}

Expr *expr_sizeof_expr(Expr *Expr) {
    Expr *expr = new_expr(EXPR_SIZEOF_EXPR);
    expr->sizeof_expr = expr;
    return expr;
}

Expr *expr_unary(tokenKind op, Expr *expr) {
    Expr *expr = new_expr(EXPR_UNARY);
    expr->unaryExpr.op = op;
    expr->unaryExpr.expr = expr;
    return expr;
}

Expr *expr_call(Expr *expr, Expr **args, size_t num_args) {
    Expr *expr = new_expr(EXPR_CALL);
    expr->call_expr.expr = expr;
    expr->call_expr.args = args;
    expr->call_expr.num_args = num_args;
    return expr;
}

Expr *expr_field(Expr *expr, const char *field) {
    Expr *expr = new_expr(EXPR_FIELD);
    expr->fieldExpr.expr = expr;
    expr->fieldExpr.args = field;
    return expr;
}

Expr *expr_index(Expr *expr, Expr *index) {
    Expr *expr = new_expr(EXPR_INDEX);
    expr->indexExpr.expr = expr;
    expr->indexExpr.index = index;
    return expr;
}

Expr *expr_binary(Expr *left, tokenKind op, Expr *right) {
    Expr *expr = new_expr(EXPR_BINARY);
    expr->binary_expr.left = left;
    expr->binary_expr.right = right;
    expr->binary_expr.op =
}

Expr *expr_ternary(Expr *eval, Expr *then_expr, Expr *else_expr) {
    Expr *expr = new_expr(EXPR_TERNARY);
    expr->ternary_expr.eval = eval;
    expr->ternary_expr.then_expr = then_expr;
    expr->ternary_expr.else_expr = else_expr;
    return expr;
}

typedef enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_PTR,
} TypespecKind;

typedef struct FuncTypespec {
    Typespec **args;
    size_t num_args;
    Typespec *ret;
} type_func;

typedef struct ArrayTypespec {
    Expr *expr;
    Typespec *type;
} ArrayTypespec;

typedef struct PtrTypespec {
    Typepsec *type;
} PtrTypespec;

typedef struct Typespec {
    TypespecKind kind;
    union {
        const char *name;
        FuncTypespec func;
        PtrTypspec ptr;
        ArrayTypespec array;
    };
} Typespec;

Typespec *typespec_new(Typespec kind) {
    Typespec *type = xmalloc(sizeof(Typespec));
    type->kind = kind;
    return type;
}

Typespec *typespec_name(const char *name) {
    Typespec *type = type_new(TYPESPEC_NAME);
    type->name = name;
    return type;
}

Typespec *typespec_func(Typespec **args, size_t num_args, Typespec *ret) {
    Typespec *type = type_new(TYPESPEC_FUNC);
    type->func.args = args;
    type->func.num_args = num_args;
    type->func.ret = ret;
    return type;
}

Typespec *typespec_array(Typespec *type, Expr *expr) {
    Typespec *type = type_new(TYPESPEC_ARRAY);
    type->array.type = type;
    type->array.expr = expr;
    return type;
}

Typespec *typespec_ptr(Typespec *ptr_type) {
    Typespec *type = type_new(TYPESPEC_PTR);
    type->ptr.type = ptr_type;
    return type;
}

typedef enum StmtKind {
    STMT_NONE,
    STMT_DECL,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_DO_WHILE,
    STMT_FOR,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_INIT,
    STMT_EXPR,
} StmtKind;

typedef struct StmtReturn {
    Expr *expr;
} StmtReturn;

typedef struct ElseIf {
    Expr *expr;
    StmtBlock block;
} Elseif;

typedef struct StmtIf {
    Expr *expr;
    StmtBlock if_block;
    ElseIf *else_ifs;
    size_t num_elseifs;
    StmtBlock else_block;
} StmtIf;

typedef struct StmtWhile {
    Expr *expr;
    StmtBlock block;
} StmtWhile;

typedef struct StmtDoWhile {
    StmtBlock block;
    Expr *expr;
} StmtDoWhile;

typedef struct StmtFor {
    Stmt *init;
    Expr *cond;
    Stmt *next;
    StmtBlock block;
} StmtFor;

typedef struct SwitchCase {
    Expr *expr;
    bool is_default;
    Stmt **stmts;
    size_t num_stmts;
};

typedef struct StmtSwitch {
    Expr *expr;
    SwitchCase *cases;
    size_t num_cases;
} StmtSwitch;

typedef struct StmtAssign {
    Expr *left_expr;
    TokenKind op;
    Expr *right_expr;
} StmtAssign;

typedef struct StmtInit {
    const char *name;
    Expr *expr;
} StmtInit;

typedef struct StmtBlock {
    Stmt **stmts;
    size_t num_stmts;
} StmtBlock;

typedef struct Stmt {
    StmtKind kind;
    union {
        Decl *decl;
        StmtBlock block;
        StmtReturn stmt_return;
        StmtIf *stmt_if;
        StmtWhile *stmt_while;
        StmtDoWhile *stmt_do_while;
        StmtFor *stmt_for;
        StmtSwitch *stmt_switch;
        StmtAssign *stmt_assign;
        StmtInit *stmt_init;
        Expr *expr;
    };
} Stmt;

Stmt *stmt_new(StmtKind kind) {
    Stmt *stmt = xmalloc(sizeof(Stmt));
    stmt->kind = kind;
    return stmt;
}

Stmt *stmt_return(Expr *expr) {
    Stmt *stmt = stmt_new(STMT_RETURN);
    stmt->stmt_return.expr = expr;
	return stmt;
}

Stmt *stmt_break() {
    return stmt_new(STMT_BREAK);
}

Stmt *stmt_continue() {
    return stmt_new(STMT_CONTINUE);
}

Stmt *stmt_block(StmtBlock block) {
    Stmt *stmt = stmt_new(STMT_BLOCK);
    stmt->block = block;
    return stmt;
}

Stmt *stmt_if(Expr *expr, StmtBlock if_block, ElseIf *else_if, size_t num_elseif,
              StmtBlock else_block) {
    Stmt *stmt = stmt_new(STMT_IF);
    stmt->stmt_if.expr = expr;
    stmt->stmt_if.if_block = if_block;
    stmt->stmt_if.else_ifs = else_ifs;
    stmt->stmt_if.num_elseifs = num_elseifs;
    stmt->stmt_if.else_block = else_block;
    return stmt;
}

Stmt *stmt_while(Expr *expr, StmtBlock block) {
    Stmt *stmt = stmt_new(STMT_WHILE);
    stmt->stmt_while.expr = expr;
    stmt->stmt_while.block = block;
    return stmt;
}

Stmt *stmt_do_while(StmtBlock block, Expr *Expr) {
    Stmt *stmt = stmt_new(STMT_DO_WHILE);
    stmt->stmt_do_while->block = block;
    stmt->stmt_do_while->expr = expr;
    return stmt;
}

Stmt *stmt_for(Stmt *init, Expr *cond, Stmt *next, StmtBlock block) {
    Stmt *stmt = stmt_new(STMT_FOR);
    stmt->stmt_for.init = init;
    stmt->stmt_for.cond = cond;
    stmt->stmt_for.next = next;
    stmt->stmt_for.block = block;
    return stmt;
}

Stmt *stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases) {
    Stmt *stmt = stmt_new(STMT_SWITCH);
    stmt->stmt_switch.expr = expr;
    stmt->stmt_switch.cases = cases;
    stmt->stmt_switch.num_cases = num_cases;
    return stmt;
}

Stmt *stmt_init(const char *name, Expr *expr) {
    Stmt *stmt = stmt_new(STMT_INIT);
    stmt->stmt_init.name = name;
    stmt->stmt_init.expr = expr;
    return stmt;
}

Stmt *stmt_assign(Expr *left_expr, TokenKind op, Expr *right_expr) {
    Stmt *stmt = stmt_new(STMT_ASSIGN);
    stmt->stmt_assign.left_expr = left_expr;
    stmt->stmt_assign.right_expr = right_expr;
    stmt->stmt_assign.op = op;
    return stmt;
}

Stmt *stmt_expr(Expr *expr) {
    Stmt *stmt = stmt_new(STMT_EXPR);
    stmt->expr = expr;
    return stmt;
}

Stmt *stmt_decl(Decl *decl) {
    Stmt *stmt = stmt_new(STMT_DECL);
    stmt->decl = decl;
    return stmt;
}
