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

typedef struct Decl {
        DeclKind kind;
        const char *name;
        union {
                EnumDecl enum_decl;
                AggregateDecl aggregate;
                FuncDecl func;
                TypedefDecl typedef_decl;
                VarDecl var;
                ConstDecl const_decl;
        };
} Decl;

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
}T

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

Stmt *stmt_while(StmtKind kind, Expr *expr, StmtBlock block)
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

/* #include <stdio.h> */
/* #include <stdlib.h> */

/* char *token; */

/* typedef enum typeexpr { */
/*         NUM, */
/*         COMP */
/* }Type; */

/* typedef enum oper { */
/*         ADD, */
/*         MUL */
/* }; */

/* typedef struct expr { */
/*         Type t; */
/*         union { */
/*                 struct { */
/*                         struct expr *e1; */
/*                         enum oper op; */
/*                         struct expr *e2; */
/*                 }st; */

/*                 int num; */
/*         }; */
/* }; */

/* struct expr *E3() */
/* { */
/*         struct expr *e; */

/*         if (*token >= '0' && *token <= '9') { */
/*                 e = malloc(sizeof(struct expr)); */
                
/*                 e->t = NUM; */
/*                 e->num = *token - '0'; */
/*                 token++; */
/*         } */

/*         return e; */
/* } */

/* struct expr * E2() { */
/*         struct expr *e = E3(); */

/*         while (*token != '\0') { */
/*                 if (*token == '*') { */
/*                         token++; */
/*                         struct expr *e2 = E3(); */

/*                         struct expr *comb = malloc(sizeof(struct expr)); */
/*                         comb->t = COMP; */
/*                         comb->st.e1 = e; */
/*                         comb->st.e2 = e2; */
/*                         comb->st.op = MUL; */

/*                         e = comb; */
/*                 } else { */
/*                         break; */
/*                 } */
/*         } */

/*         return e; */
/* } */

/* struct expr * E1() { */
/*         struct expr *e = E2(); */

/*         while (*token != '\0') { */
/*                 if (*token == '+') { */
/*                         token++; */
/*                         struct expr *e2 = E2(); */
/*                         struct expr *comb = malloc(sizeof(struct expr)); */

/*                         comb->t = COMP; */
/*                         comb->st.e1 = e; */
/*                         comb->st.e2 = e2; */
/*                         comb->st.op = ADD; */

/*                         e = comb; */
/*                 } else { */
/*                         break; */
/*                 } */
/*         } */

/*         return e; */
/* } */

/* void print_ast(struct expr *e) */
/* { */
/*         if (e->t == NUM) { */
/*                 printf("%d", e->num); */
/*         } else { */
/*                 printf("( "); */
/*                 if (e->st.op == ADD) { */
/*                         printf("+");  */
/*                 } else { */
/*                         printf("*"); */
/*                 } */

/*                 print_ast(e->st.e1); */
/*                 printf(" "); */
/*                 print_ast(e->st.e2); */
/*                 printf(")"); */
/*         } */
/* } */

/* struct expr *test_ast() */
/* { */
/*         char *str = "2+3*4+5*6"; */
/*         token = str; */

/*         return E1(); */
/* } */

/* int main() */
/* { */
/*         struct expr *e = test_ast(); */

/*         print_ast(e); */
/* } */
