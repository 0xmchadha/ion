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
                     size_t num_items) {
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
    decl->func_decl.type = type;
    decl->func_decl.block = block;
}

Decl *decl_typedef(const char *name, Typespec *type) {
    Decl *decl = decl_new(DECL_TYPEDEF, name);
    decl->typedef_decl.type = type;
}

Expr *expr_new(ExprKind kind) {
    Expr *expr = xmalloc(sizeof(struct Expr));
    expr->kind = kind;
    return expr;
}

Expr *expr_int(uint64_t int_val) {
    Expr *expr = expr_new(EXPR_INT);
    expr->int_val = int_val;
    return expr;
}

Expr *expr_float(double float_val) {
    Expr *expr = expr_new(EXPR_FLOAT);
    expr->float_val = float_val;
    return expr;
}

Expr *expr_str(const char *str) {
    Expr *expr = expr_new(EXPR_STR);
    expr->str_val = str;
    return expr;
}

Expr *expr_name(const char *name) {
    Expr *expr = expr_new(EXPR_NAME);
    expr->name = name;
    return expr;
}

Expr *expr_compound(Typespec *type, Expr **args, size_t num_args) {
    Expr *expr = expr_new(EXPR_COMPOUND);
    expr->compound_expr.type = type;
    expr->compound_expr.args = args;
    expr->compound_expr.num_args = num_args;
    return expr;
}

Expr *expr_sizeof_type(Typespec *type) {
    Expr *expr = expr_new(EXPR_SIZEOF_TYPE);
    expr->sizeof_type = type;
    return expr;
}

Expr *expr_sizeof_expr(Expr *sizeof_expr) {
    Expr *expr = expr_new(EXPR_SIZEOF_EXPR);
    expr->sizeof_expr = sizeof_expr;
    return expr;
}

Expr *expr_unary(TokenKind op, Expr *e) {
    Expr *expr = expr_new(EXPR_UNARY);
    expr->unary_expr.op = op;
    expr->unary_expr.expr = e;
    return expr;
}

Expr *expr_call(Expr *e, Expr **args, size_t num_args) {
    Expr *expr = expr_new(EXPR_CALL);
    expr->call_expr.expr = e;
    expr->call_expr.args = args;
    expr->call_expr.num_args = num_args;
    return expr;
}

Expr *expr_field(Expr *e, const char *field) {
    Expr *expr = expr_new(EXPR_FIELD);
    expr->field_expr.expr = e;
    expr->field_expr.name = field;
    return expr;
}

Expr *expr_index(Expr *e, Expr *index) {
    Expr *expr = expr_new(EXPR_INDEX);
    expr->index_expr.expr = e;
    expr->index_expr.index = index;
    return expr;
}

Expr *expr_binary(Expr *left, TokenKind op, Expr *right) {
    Expr *expr = expr_new(EXPR_BINARY);
    expr->binary_expr.left = left;
    expr->binary_expr.right = right;
    expr->binary_expr.op =op;
}

Expr *expr_ternary(Expr *eval, Expr *then_expr, Expr *else_expr) {
    Expr *expr = expr_new(EXPR_TERNARY);
    expr->ternary_expr.eval = eval;
    expr->ternary_expr.then_expr = then_expr;
    expr->ternary_expr.else_expr = else_expr;
    return expr;
}

Typespec *typespec_new(TypespecKind kind) {
    Typespec *type = xmalloc(sizeof(Typespec));
    type->kind = kind;
    return type;
}

Typespec *typespec_name(const char *name) {
    Typespec *type = typespec_new(TYPESPEC_NAME);
    type->name = name;
    return type;
}

Typespec *typespec_func(Typespec **args, size_t num_args, Typespec *ret) {
    Typespec *type = typespec_new(TYPESPEC_FUNC);
    type->func.args = args;
    type->func.num_args = num_args;
    type->func.ret = ret;
    return type;
}

Typespec *typespec_array(Typespec *type, Expr *expr) {
    Typespec *t = typespec_new(TYPESPEC_ARRAY);
    t->array.type = type;
    t->array.expr = expr;
    return t;
}

Typespec *typespec_ptr(Typespec *ptr_type) {
    Typespec *type = typespec_new(TYPESPEC_PTR);
    type->ptr.type = ptr_type;
    return type;
}

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

Stmt *stmt_if(Expr *expr, StmtBlock if_block, ElseIf *else_ifs, size_t num_elseifs,
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

Stmt *stmt_do_while(StmtBlock block, Expr *expr) {
    Stmt *stmt = stmt_new(STMT_DO_WHILE);
    stmt->stmt_do_while.block = block;
    stmt->stmt_do_while.expr = expr;
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
