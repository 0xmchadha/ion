typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Stmt Stmt;
typedef struct Typespec Typespec;
typedef struct Sym Sym;

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

typedef struct StmtBlock {
    Stmt **stmts;
    size_t num_stmts;
} StmtBlock;

typedef struct StmtReturn {
    Expr *expr;
} StmtReturn;

typedef struct ElseIf {
    Expr *expr;
    StmtBlock block;
} ElseIf;

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

typedef struct StmtFor {
    Stmt *init;
    Expr *cond;
    Stmt *next;
    StmtBlock block;
} StmtFor;

typedef struct SwitchCase {
    Expr **expr;
    size_t num_exprs;
    bool is_default;
    StmtBlock block;
} SwitchCase;

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
    // This is a hack required while generating c code.
    void *type;
} StmtInit;

typedef struct Stmt {
    SrcPos pos;
    StmtKind kind;
    union {
        StmtBlock block;
        StmtReturn stmt_return;
        StmtIf stmt_if;
        StmtWhile stmt_while;
        StmtFor stmt_for;
        StmtSwitch stmt_switch;
        StmtAssign stmt_assign;
        StmtInit stmt_init;
        Decl *decl;
        Expr *expr;
    };
} Stmt;

typedef enum DeclKind {
    DECL_NONE,
    DECL_ENUM,
    DECL_AGGREGATE,
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
    const char **names;
    size_t num_items;
    Typespec *type;
} AggregateItem;

typedef struct AggregateDecl {
    AggregateKind kind;
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
    SrcPos pos;
    DeclKind kind;
    const char *name;
    Sym *sym;
    union {
        EnumDecl enum_decl;
        AggregateDecl aggregate_decl;
        FuncDecl func_decl;
        TypedefDecl typedef_decl;
        VarDecl var_decl;
        ConstDecl const_decl;
    };
} Decl;

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

typedef enum CompoundValKind {
    SIMPLE_EXPR,
    INDEX_EXPR,
    NAME_EXPR,
} CompoundValKind;

typedef struct CompoundVal {
    CompoundValKind kind;
    union {
        Expr *expr;
        struct {
            Expr *index;
            Expr *val;
        } index;
        struct {
            const char *name;
            Expr *val;
        } name;
    };
} CompoundVal;

typedef struct Expr {
    SrcPos pos;
    ExprKind kind;
    union {
        int int_val;
        double float_val;
        const char *str_val;
        const char *name;

        struct {
            Expr *expr;
            Expr *index;
        } index_expr;

        struct {
            Expr *expr;
            const char *name;
        } field_expr;

        struct {
            Expr *expr;
            Expr **args;
            size_t num_args;
        } call_expr;

        struct {
            Typespec *type;
            CompoundVal **args;
            size_t num_args;
        } compound_expr;

        Typespec *sizeof_type;
        Expr *sizeof_expr;

        struct {
            Typespec *type;
            Expr *expr;
        } cast_expr;
        struct {
            TokenKind op;
            Expr *expr;
        } unary_expr;

        struct {
            Expr *left;
            TokenKind op;
            Expr *right;
        } binary_expr;

        struct {
            Expr *eval;
            Expr *then_expr;
            Expr *else_expr;
        } ternary_expr;
    };
} Expr;

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
} FuncTypespec;

typedef struct ArrayTypespec {
    Expr *expr;
    Typespec *type;
} ArrayTypespec;

typedef struct PtrTypespec {
    Typespec *type;
} PtrTypespec;

typedef struct Typespec {
    SrcPos pos;
    TypespecKind kind;
    union {
        const char *name;
        FuncTypespec func;
        PtrTypespec ptr;
        ArrayTypespec array;
    };
} Typespec;

typedef struct DeclSet {
    Decl **decls;
    size_t num_decls;
} DeclSet;
