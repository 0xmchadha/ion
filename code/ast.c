Arena ast_arena;

void *ast_alloc(size_t size) {
    void *mem = arena_alloc(&ast_arena, size);
    memset(mem, 0, size);
    return mem;
}

void *ast_dup(const void *src, size_t size) {
    if (size == 0) {
        return NULL;
    }

    void *mem = arena_alloc(&ast_arena, size);
    memcpy(mem, src, size);
    return mem;
}

#define AST_DUP(x) ast_dup(x, sizeof(*x) * num_##x)

Decl *decl_new(DeclKind kind, SrcPos pos, const char *name) {
    Decl *decl = arena_alloc(&ast_arena, sizeof(Decl));
    decl->pos = pos;
    decl->name = name;
    decl->kind = kind;
    return decl;
}

Decl *decl_enum(const char *name, SrcPos pos, EnumItem *items, size_t num_items) {
    Decl *decl = decl_new(DECL_ENUM, pos, name);
    decl->enum_decl.items = AST_DUP(items);
    decl->enum_decl.num_enum_items = num_items;
    return decl;
}

Decl *decl_var(const char *name, SrcPos pos, Typespec *type, Expr *expr) {
    Decl *decl = decl_new(DECL_VAR, pos, name);
    decl->var_decl.type = type;
    decl->var_decl.expr = expr;
    return decl;
}

Decl *decl_const(const char *name, SrcPos pos, Expr *expr) {
    Decl *decl = decl_new(DECL_CONST, pos, name);
    decl->const_decl.expr = expr;
    return decl;
}

Decl *decl_aggregate(AggregateKind kind, SrcPos pos, const char *name, AggregateItem *items,
                     size_t num_items) {
    Decl *decl = decl_new(DECL_AGGREGATE, pos, name);
    decl->aggregate_decl.kind = kind;
    decl->aggregate_decl.items = AST_DUP(items);
    decl->aggregate_decl.num_items = num_items;
    return decl;
}

Decl *decl_func(const char *name, SrcPos pos, FuncArg *args, size_t num_args, Typespec *type,
                StmtBlock block, bool is_foreign, bool is_variadic) {
    Decl *decl = decl_new(DECL_FUNC, pos, name);
    decl->func_decl.args = AST_DUP(args);
    decl->func_decl.num_func_args = num_args;
    decl->func_decl.type = type;
    decl->func_decl.block = block;
    decl->func_decl.is_variadic = is_variadic;
    decl->func_decl.is_foreign = is_foreign;
    return decl;
}

Decl *decl_typedef(const char *name, SrcPos pos, Typespec *type) {
    Decl *decl = decl_new(DECL_TYPEDEF, pos, name);
    decl->typedef_decl.type = type;
    return decl;
}

Expr *expr_new(ExprKind kind, SrcPos pos) {
    Expr *expr = arena_alloc(&ast_arena, sizeof(struct Expr));
    expr->pos = pos;
    expr->kind = kind;
    return expr;
}

Expr *expr_int(SrcPos pos, uint64_t int_val) {
    Expr *expr = expr_new(EXPR_INT, pos);
    expr->int_val = int_val;
    return expr;
}

Expr *expr_float(SrcPos pos, double float_val) {
    Expr *expr = expr_new(EXPR_FLOAT, pos);
    expr->float_val = float_val;
    return expr;
}

Expr *expr_str(SrcPos pos, const char *str) {
    Expr *expr = expr_new(EXPR_STR, pos);
    expr->str_val = str;
    return expr;
}

Expr *expr_name(SrcPos pos, const char *name) {
    Expr *expr = expr_new(EXPR_NAME, pos);
    expr->name = name;
    return expr;
}

CompoundVal *compound_new(CompoundValKind kind) {
    CompoundVal *new = arena_alloc(&ast_arena, sizeof(CompoundVal));
    new->kind = kind;
    return new;
}

CompoundVal *compound_simple(Expr *expr) {
    CompoundVal *compound_simple = compound_new(SIMPLE_EXPR);
    compound_simple->expr = expr;
    return compound_simple;
}

CompoundVal *compound_index(Expr *index, Expr *val) {
    CompoundVal *compound_index = compound_new(INDEX_EXPR);
    compound_index->index.index = index;
    compound_index->index.val = val;
    return compound_index;
}

CompoundVal *compound_name(const char *name, Expr *val) {
    CompoundVal *compound_name = compound_new(NAME_EXPR);
    compound_name->name.name = name;
    compound_name->name.val = val;
    return compound_name;
}

Expr *expr_compound(SrcPos pos, Typespec *type, CompoundVal **args, size_t num_args) {
    Expr *expr = expr_new(EXPR_COMPOUND, pos);
    expr->compound_expr.type = type;
    expr->compound_expr.args = AST_DUP(args);
    expr->compound_expr.num_args = num_args;
    return expr;
}

Expr *expr_sizeof_type(SrcPos pos, Typespec *type) {
    Expr *expr = expr_new(EXPR_SIZEOF_TYPE, pos);
    expr->sizeof_type = type;
    return expr;
}

Expr *expr_sizeof_expr(SrcPos pos, Expr *sizeof_expr) {
    Expr *expr = expr_new(EXPR_SIZEOF_EXPR, pos);
    expr->sizeof_expr = sizeof_expr;
    return expr;
}

Expr *expr_cast(SrcPos pos, Typespec *type, Expr *e) {
    Expr *expr = expr_new(EXPR_CAST, pos);
    expr->cast_expr.type = type;
    expr->cast_expr.expr = e;
    return expr;
}

Expr *expr_unary(SrcPos pos, TokenKind op, Expr *e) {
    Expr *expr = expr_new(EXPR_UNARY, pos);
    expr->unary_expr.op = op;
    expr->unary_expr.expr = e;
    return expr;
}

Expr *expr_call(SrcPos pos, Expr *e, Expr **args, size_t num_args) {
    Expr *expr = expr_new(EXPR_CALL, pos);
    expr->call_expr.expr = e;
    expr->call_expr.args = AST_DUP(args);
    expr->call_expr.num_args = num_args;
    return expr;
}

Expr *expr_field(SrcPos pos, Expr *e, const char *field) {
    Expr *expr = expr_new(EXPR_FIELD, pos);
    expr->field_expr.expr = e;
    expr->field_expr.name = field;
    return expr;
}

Expr *expr_index(SrcPos pos, Expr *e, Expr *index) {
    Expr *expr = expr_new(EXPR_INDEX, pos);
    expr->index_expr.expr = e;
    expr->index_expr.index = index;
    return expr;
}

Expr *expr_binary(SrcPos pos, Expr *left, TokenKind op, Expr *right) {
    Expr *expr = expr_new(EXPR_BINARY, pos);
    expr->binary_expr.left = left;
    expr->binary_expr.right = right;
    expr->binary_expr.op = op;
    return expr;
}

Expr *expr_ternary(SrcPos pos, Expr *eval, Expr *then_expr, Expr *else_expr) {
    Expr *expr = expr_new(EXPR_TERNARY, pos);
    expr->ternary_expr.eval = eval;
    expr->ternary_expr.then_expr = then_expr;
    expr->ternary_expr.else_expr = else_expr;
    return expr;
}

Typespec *typespec_new(TypespecKind kind, SrcPos pos) {
    Typespec *type = arena_alloc(&ast_arena, sizeof(Typespec));
    type->kind = kind;
    type->pos = pos;
    type->pos = (SrcPos){file_name, line_num};
    return type;
}

Typespec *typespec_name(SrcPos pos, const char *name) {
    Typespec *type = typespec_new(TYPESPEC_NAME, pos);
    type->name = name;
    return type;
}

Typespec *typespec_func(SrcPos pos, Typespec **args, size_t num_args, Typespec *ret) {
    Typespec *type = typespec_new(TYPESPEC_FUNC, pos);
    type->func.args = AST_DUP(args);
    type->func.num_args = num_args;
    type->func.ret = ret;
    return type;
}

Typespec *typespec_array(SrcPos pos, Typespec *type, Expr *expr) {
    Typespec *t = typespec_new(TYPESPEC_ARRAY, pos);
    t->array.type = type;
    t->array.expr = expr;
    return t;
}

Typespec *typespec_ptr(SrcPos pos, Typespec *ptr_type) {
    Typespec *type = typespec_new(TYPESPEC_PTR, pos);
    type->ptr.type = ptr_type;
    return type;
}

Stmt *stmt_new(StmtKind kind, SrcPos pos) {
    Stmt *stmt = arena_alloc(&ast_arena, sizeof(Stmt));
    stmt->pos = pos;
    stmt->kind = kind;
    return stmt;
}

Stmt *stmt_return(SrcPos pos, Expr *expr) {
    Stmt *stmt = stmt_new(STMT_RETURN, pos);
    stmt->stmt_return.expr = expr;
    return stmt;
}

Stmt *stmt_break(SrcPos pos) {
    return stmt_new(STMT_BREAK, pos);
}

Stmt *stmt_continue(SrcPos pos) {
    return stmt_new(STMT_CONTINUE, pos);
}

Stmt *stmt_block(SrcPos pos, StmtBlock block) {
    Stmt *stmt = stmt_new(STMT_BLOCK, pos);
    stmt->block = block;
    return stmt;
}

Stmt *stmt_if(SrcPos pos, Expr *expr, StmtBlock if_block, ElseIf *elseifs, size_t num_elseifs,
              StmtBlock else_block) {
    Stmt *stmt = stmt_new(STMT_IF, pos);
    stmt->stmt_if.expr = expr;
    stmt->stmt_if.if_block = if_block;
    stmt->stmt_if.else_ifs = AST_DUP(elseifs);
    stmt->stmt_if.num_elseifs = num_elseifs;
    stmt->stmt_if.else_block = else_block;
    return stmt;
}

Stmt *stmt_while(SrcPos pos, Expr *expr, StmtBlock block) {
    Stmt *stmt = stmt_new(STMT_WHILE, pos);
    stmt->stmt_while.expr = expr;
    stmt->stmt_while.block = block;
    return stmt;
}

Stmt *stmt_do_while(SrcPos pos, StmtBlock block, Expr *expr) {
    Stmt *stmt = stmt_new(STMT_DO_WHILE, pos);
    stmt->stmt_while.block = block;
    stmt->stmt_while.expr = expr;
    return stmt;
}

Stmt *stmt_for(SrcPos pos, Stmt *init, Expr *cond, Stmt *next, StmtBlock block) {
    Stmt *stmt = stmt_new(STMT_FOR, pos);
    stmt->stmt_for.init = init;
    stmt->stmt_for.cond = cond;
    stmt->stmt_for.next = next;
    stmt->stmt_for.block = block;
    return stmt;
}

Stmt *stmt_switch(SrcPos pos, Expr *expr, SwitchCase *cases, size_t num_cases) {
    Stmt *stmt = stmt_new(STMT_SWITCH, pos);
    stmt->stmt_switch.expr = expr;
    stmt->stmt_switch.cases = AST_DUP(cases);
    stmt->stmt_switch.num_cases = num_cases;
    return stmt;
}

Stmt *stmt_init(SrcPos pos, const char *name, Expr *expr) {
    Stmt *stmt = stmt_new(STMT_INIT, pos);
    stmt->stmt_init.name = name;
    stmt->stmt_init.expr = expr;
    return stmt;
}

Stmt *stmt_assign(SrcPos pos, Expr *left_expr, TokenKind op, Expr *right_expr) {
    Stmt *stmt = stmt_new(STMT_ASSIGN, pos);
    stmt->stmt_assign.left_expr = left_expr;
    stmt->stmt_assign.right_expr = right_expr;
    stmt->stmt_assign.op = op;
    return stmt;
}

Stmt *stmt_expr(SrcPos pos, Expr *expr) {
    Stmt *stmt = stmt_new(STMT_EXPR, pos);
    stmt->expr = expr;
    return stmt;
}

Stmt *stmt_decl(SrcPos pos, Decl *decl) {
    Stmt *stmt = stmt_new(STMT_DECL, pos);
    stmt->decl = decl;
    return stmt;
}

DeclSet *decl_set(Decl **decls, size_t num_decls) {
    DeclSet *declset = ast_alloc(sizeof(DeclSet));
    declset->decls = AST_DUP(decls);
    declset->num_decls = num_decls;
    return declset;
}
