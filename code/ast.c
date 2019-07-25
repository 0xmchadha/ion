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
        size_t num_enum_items;
        EnumItem *items;
} EnumDecl;

typedef struct AggregateItem {
        size_t num_items;
        char **names;
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

Decl *new_decl(DeclKind kind, const char *name) {
        Decl *decl = xmalloc(sizeof(Decl));
        decl->name = name;
        decl->kind = kind;
        return decl;
}

Decl *new_decl_var(const char *name, Typespec *type, Expr *expr) {
        Decl *decl = new_decl(DECL_VAR, name);
        decl->var_decl.type = type;
        decl->var_decl.expr = expr;
        return decl;
}

Decl *new_decl_const(const char *name, Expr *expr) {
        Decl *decl = new_decl(DECL_CONST, name);
        decl->const_decl.expr = expr;
        return decl;
}

Decl *new_decl_aggregate(AggregateKind kind, const char *name, size_t num_items, AggregateItem *items) {
        Decl *decl = new_decl(kind, name);
        decl->aggregate_decl.num_items = num_items;
        decl->aggregate_decl.items = items;
        return decl;
}

Decl *new_decl_func(const char *name, size_t num_func_args, FuncArg *args, StmtBlock block) {
        Decl *decl = new_decl(DECL_FUNC, name);
        decl->func_decl.num_func_args = num_func_args;
        decl->func_decl.args = args;
        decl->func_decl.block = block;
}

Decl *new_decl_typedef(const char *name, Typespec *type) {
        Decl *decl = new_decl(DECL_TYPEDEF, name);
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

                struct {
                        Typespec *type;
                        Expr *expr;
                } cast_expr;
                struct {
                        tokenKind op;
                        Expr *expr;
                } unary_expr;
               
                struct  {
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

Expr *new_expr(ExprKind kind) {
        Expr *expr = xmalloc(sizeof(struct Expr));
        expr->kind = kind;
        return expr;
}

Expr *new_expr_int(uint64_t int_val) {
        Expr *expr = new_expr(EXPR_INT);
        expr->int_val = int_val;
        return expr;
}

Expr *new_expr_float(double float_val) {
        Expr *expr = new_expr(EXPR_FLOAT);
        expr->float_val = float_val;
        return expr;
}

Expr *new_expr_str(char *str) {
        Expr *expr = new_expr(EXPR_STR);
        expr->str_val = str;
        return expr;
}

Expr *new_expr_name(char *name) {
        Expr *expr = new_expr(EXPR_NAME);
        expr->name_val = name;
        return expr;
}

Expr *new_expr_unary(tokenKind op, Expr *expr) {
        Expr *expr = new_expr(EXPR_UNARY);
        expr->unaryExpr.op = op;
        expr->unaryExpr.expr = expr;
        return expr;
}

Expr *new_expr_call(Expr *expr, Expr **args) {
        Expr *expr = new_expr(EXPR_CALL);
        expr->callExpr.expr = left;
        expr->callExpr.args = args;
        return expr;
}

Expr *new_expr_field(Expr *expr, const char *field) {
        Expr *expr = new_expr(EXPR_FIELD);
        expr->fieldExpr.expr = expr;
        expr->fieldExpr.args = field;
        return expr;
}

Expr *new_expr_index(Expr *expr, Expr *index) {
        Expr *expr = new_expr(EXPR_INDEX);
        expr->indexExpr.expr = expr;
        expr->indexExpr.index = index;
        return expr;
}

Expr *new_expr_binary(Expr *left, tokenKind op, Expr *right) {
        Expr *expr = new_expr(EXPR_BINARY);
        expr->binary_expr.left = left;
        expr->binary_expr.right = right;
        expr->binary_expr.op = 
}

Expr *new_expr_ternary(Expr *eval, Expr *then_expr, Expr *else_expr) {
        Expr *expr = new_expr(EXPR_TERNARY);
        expr->ternary_expr.eval = eval;
        expr->ternary_expr.then_expr = then_expr;
        expr->ternary_expr.else_expr = else_expr;
        return expr;
}

typedef enum StmtKind {
        STMT_RETURN,
        STMT_BREAK,
        STMT_CONTINUE,
        STMT_IF,
        STMT_SWITCH,
        STMT_FOR,
        STMT_WHILE,
        STMT_DO_WHILE,
} StmtKind;

typedef struct ReturnStmt {
        Expr *expr;
} ReturnStmt;

typedef struct ElseIf {
        Expr *expr;
        StmtBlock block;
} ElseIf;

typedef struct IfStmt {
        Expr *if_expr;
        StmtBlock if_block;
        ElseIf *else_ifs;
        StmtBlock else_block;
} IfStmt;

typedef struct CaseStmt {
        Expr *expr;
        StmtBlock block;
} CaseStmt;

typedef struct SwitchStmt {
        Expr *expr;
        CastStmt *case_stmts;
        StmtBlock default_block;
};
     
typedef struct Stmt {
        StmtKind kind;
        union {
                ReturnStmt return_stmt;
                IfStmt if_stmt;
                SwitchStmt switch_stmt;
        };
} Stmt;















typedef struct Expr Expr;
typedef struct Stmt Stmt;
typedef struct Decl Decl;
typedef struct Typespec Typespec

typedef struct ReturnStmt {
        Expr *ret;
} ReturnStmt;

typedef struct StmtBlock {
        Stmt **stmts;
        size_t num_stmts;
} StmtBlock;

typedef struct ElseIf {
        Expr *expr;
        StmtBlock block;
} ElseIf;

typedef struct IfStmt {
        Expr *condition;
        StmtBlock if_block;
        ElseIf *elseifs;
        size_t num_elseifs;
        StmtBlock else_block;
} IfStmt;

typedef struct WhileStmt {
        Expr *expr;
        StmtBlock block;
};

typedef struct SwitchCase {
        bool is_default;
        Expr *eval;
        StmtBlock block;
};

typedef struct SwitchStmt {
        Expr *expr;
        SwitchCase *cases;
        size_t num_cases;
} SwitchStmt;

typedef struct ForStmt {
        Stmt *init
        Expr *expr;
        Stmt *next;
        StmtBlock block;
} ForStmt;

typedef enum StmtKind {
        STMT_NONE,
        STMT_DECL,
        STMT_EXPR,
        STMT_RETURN,
        STMT_BREAK,
        STMT_CONTINUE,
        STMT_ASSIGN,
        STMT_BLOCK,
        STMT_IF,
        STMT_WHILE,
        STMT_FOR,
        STMT_DO,
        STMT_SWITCH,
} StmtKind;

typedef struct AssignStmt {
        Expr *left;
        TokenKind kind;
        Expr *right;
} AssignStmt;
        
typedef struct Stmt {
        StmtKind kind;

        union {
                ReturnStmt ret;
                IfStmt if_stmt;
                ForStmt for_stmt;
                WhileStmt while_stmt;
                SwitchStmt switch_stmt;
                StmtBlock block;
                AssignStmt assign;
                Expr *expr;
                Decl *decl;
         };
};

typedef enum ExprKind {
        EXPR_NONE,
        EXPR_INT,
        EXPR_FLOAT,
        EXPR_STR,
        EXPR_NAME,
        EXPR_CAST,
        EXPR_CALL,
        EXPR_INDEX,
        EXPR_FIELD,
        EXPR_COMPOUND,
        EXPR_UNARY,
        EXPR_BINARY,
        EXPR_TERNARY
} ExprKind;

typedef struct CastExpr {
        Typespec *type;
        Expr *expr;
} CastExpr;

typedef struct CallExpr {
        Expr *expr;
        Expr **args;
        size_t num_args;
} CallExpr;

typedef struct IndexExpr {
        Expr *init;
        Expr *index;
} IndexExpr;

typedef struct FieldExpr {
        Expr *expr;
        const char *field;
} FieldExpr;

typedef struct CompoundExpr {
        Typespec *type;
        Expr **args;
        size_t num_args;
} CompoundExpr;

typedef struct UnaryExpr {
        Expr *expr;
        TokenKind kind;
};

typedef struct BinaryExpr {
        Expr *left;
        Expr *right;
        TokenKind kind;
};

typedef struct TernaryExpr {
        Expr *condition;
        Expr *then_expr;
        Expr *else_expr;
};

typedef struct Expr {
        ExprKind kind;
        union {
                uint64_t val_int;
                double    val_float;
                const char *str_val;
                const char *name;
                CastExpr cast;
                CallExpr call;
                CompoundExpr compound;
                IndexExpr index;
                FieldExpr expr;
                UnaryExpr unary;
                BinaryExpr binary;
                TernaryExpr ternary;
        };
};

typedef enum TypespecKind {
        TYPESPEC_NONE,
        TYPESPEC_NAME,
        TYPESPEC_FUNC,
        TYPESPEC_ARRAY,
        TYPESPEC_POINTER,
} TypespecKind;

typedef struct Typefunc {
        Typespec **args;
        size_t num_items;
        Typespec *ret;
} Typefunc;     

typedef struct Typearray {
        Expr *expr;
        Typespec *type;
} Typearray;

typedef struct Typepointer {
        Typespec *type;
} Typepointer;

typedef struct Typename {
        const char *name;
} Typename;

typedef struct Typespec {
        TypespecKind kind;
        union {
                Typename name;
                Typefunc func_type;
                Typearray array_type;
                Typepointer pointer_type;
        };
} Typespec;

typedef struct ConstDecl {
        Expr *expr
};

typedef struct VarDecl {
        Typespec *type;
        Expr *expr;
} VarDecl;

typedef struct TypedefDecl {
        Typespec *type;
};
        
typedef struct FuncParam {
        const char *name;
        Typespec *type;
} FuncParam;

typedef struct FuncDecl {
        FuncParam *params;
        size_t num_params;
        Typespec *ret;
        StmtBlock block;
} FuncDecl;

typedef struct AggregateItems {
        const char **name;
        size_t num_items;
        Typespec *type;
} AggregateItem;

typedef struct AggregateDecl {
        AggregateItem *items;
        size_t num_items;
} AggregateDecl;

typedef struct EnumItem {
        const char *name;
        Expr *init;
} EnumItem;

typedef struct EnumDecl {
        EnumItem *items;
        size_t num_items;
} EnumDecl;


Stmt *stmt_continue()
{
        Stmt *s = stmt_new();
        return s;
}

Stmt *stmt_break()
{
        Stmt *s = stmt_new();
        return s;
}

Stmt *stmt_return(Expr *expr)
{
        Stmt *s = stmt_new();
        s->ret.ret = expr;
        return s;
}

Stmt *stmt_ifstmt(Expr *condition, StmtBlock if_block, ElseIf *elseifs, size_t num_elseifs, StmtBlock else_block)
{
        Stmt *s = stmt_new();
        s->if_stmt.condition = condition;
        s->if_stmt.if_block = if_block;
        s->if_stmt.elseifs = elseifs;
        s->if_stmt.num_elseifs = num_elseifs;
        s->if_stmt.else_block = else_block;
        return s;
}

Stmt *stmt_for(Stmt *init, Stmt *expr, Stmt *next, StmtBlock block)
{
        Stmt *s = stmt_new();
        s->for_stmt.init = init;
        s->for_stmt.expr = expr;
        s->for_stmt.next = next;
        s->for_stmt.block = block;
        return s;
}

Stmt *stmt_while(Expr *expr, StmtBlock block)
{
        Stmt *s = stmt_new();
        s->while_stmt.expr = expr;
        s->while_stmt.block = block;
        return s;
}

Stmt *stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases)
{
        Stmt *s = stmt_new();
        s->switch_stmt.expr = expr;
        s->switch_stmt.cases = cases;
        s->switch_stmt.num_cases = num_cases;
        return s;
}

Stmt *stmt_block(StmtBlock block)
{
        Stmt *s = stmt_new();
        s->block = block;
        return s;
}

Stmt *stmt_assign(StmtKind kind, Expr *left, Expr *right, TokenKind kind)
{
        Stmt *s = stmt_new();
        s->assign.left = left;
        s->assign.right = right;
        s->assign.kind = kind;
        return s;
}

Stmt *stmt_expr(Expr *expr)
{
        Stmt *s = stmt_new();
        s->expr = expr;
        return s;
}

Stmt *stmt_decl(Decl *decl)
{
        Stmt *s = stmt_new();
        s->decl = decl;
        return s;
}

Expr *expr_new(ExprKind kind)
{
}

Expr* expr_ternary(Expr *condition, Expr *then_expr, Expr *else_expr)
{
        Expr *e = expr_new();
        e->ternary.condition = condition;
        e->ternary.then_expr = then_expr;
        e->ternary.else_expr = else_expr;
        return e;
}

Expr* expr_binary(Expr *left, Expr *right, TokenKind kind)
{
        Expr *e = expr_new();
        e->binary.left = left;
        e->binary.right = right;
        e->binary.kind = kind;
}

Expr* expr_unary(Expr *expr, TokenKind kind)
{
        Expr *e = expr_new();
        e->unary.expr = expr;
        e->unary.kind = kind;
        return e;
}

Expr* expr_field(Expr *expr, const char *field)
{
        Expr *e = expr_new();
        e->field.expr = expr;
        e->field.field = field;
        return e;
}

Expr* expr_index(Expr *expr, Expr *index)
{
        Expr *e = expr_new();
        e->index.expr = expr;
        e->index.index = index;
        return e;
}

Expr* expr_compound(Typespec *type, Expr **args, size_t num_args)
{
        Expr *e = expr_new();
        e->compound.type = type;
        e->compound.args = args;
        e->compound.num_args = num_args;
        return e;
}

Expr* expr_call(Expr *expr, Expr **args, size_t num_args)
{
        Expr *e = expr_new();
        e->call.expr = expr;
        e->call.args = args;
        e->call.num_args = num_args;
        return e;
}

Expr* expr_cast(Typespec *type, Expr *expr)
{
        Expr *e = expr_new();
        e->cast.type = type;
        e->cast.expr = expr;
        return e;
}

Expr* expr_name(const char *name)
{
        Expr *e = expr_new();
        e->name = name;
        return e;
}

Expr* expr_str(const char *str)
{
        Expr *e = expr_new();
        e->str = str;
        return str;
}

Expr* expr_float(double val)
{
        Expr *e = expr_new();
        e->val_float = val;
        return e;
}

Expr *expr_int(uint64_t val)
{
        Expr *e = expr_new();
        e->val_int = val;
        return e;
}

Typespec *type_new(TypespecKind kind)
{
        
}
 
Typespec *type_pointer(Typespec *type)
{
        Typespec *t = type_new();
        t->pointer_type.type = type;
        return t;
}

Typespec *type_array(Expr *expr, Typespec *type)
{
        Typespec *t = type_new();
        t->array_type.expr = expr;
        t->array_type.type = type;
        return t;
}

Typespec *type_func(Typespec **args, size_t num_items, Typespec *ret)
{
        Typespec *t = type_new();
        t->func_type.args = args;
        t->func_type.num_items = num_items;
        t->func_type.ret = ret;
        return t;
}

Typespec *type_name(const char *name)
{
        Typespec *t = type_new();
        t->name.name = name;
        return t;
}

Decl *decl_new(DeclKind kind, const char *name)
{
        Decl *d = ;
        decl->kind = kind;
        decl->name = name;
        return decl;
}

Decl *decl_enum(const char *name, EnumItem *items, size_t num_items)
{
        Decl *d = decl_new(DECL_ENUM, name);
        d->enum_decl.items = items;
        d->enum_decl.num_items = num_items;
        return d;
}

Decl *decl_aggregate(DeclKind kind, const char *name, AggregateItem *items, size_t num_items)
{
        assert(kind == DECL_STRUCT || kind == DECL_UNION);
        Decl *d = decl_new(kind, name);
        d->aggregate.items = items;
        d->aggreagate.num_items = num_items;
        return d;
}

Decl *decl_func(const char *name, FuncParam *params, size_t num_params, Typespec *ret, StmtBlock block)
{
        Decl *d = decl_new(DECL_FUNC, name);
        d->func.params = params;
        d->func.num_params = num_params;
        d->func.ret = ret;
        d->func.block = block;
        return d;
}

Decl *decl_typedef(const char *name, Typespec *type)
{
        Decl *d = decl_new(DECL_TYPEDEF, name);
        d->typedef_decl.type = type;
        return d;
}

Decl *decl_var(const char *name, Typespec *type, Expr *expr)
{
        Decl *d = decl_new(DECL_VAR, name);
        d->var.type = type;
        d->var.expr = expr;
        return d;
}

Decl *decl_const(const char *name, Expr *expr)
{
        Decl *d = decl_new(DECL_CONST, name);
        d->const_decl.expr = expr;
        return d;
}

