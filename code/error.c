typedef struct SrcPos {
    const char *file;
    size_t line_num;
} SrcPos;

extern size_t line_num;
extern const char *file_name;

void syntax_error(SrcPos pos, const char *fmt, ...) {
    va_list args;
    printf("%s(%llu): ", pos.file, pos.line_num);
    va_start(args, fmt);
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
}

#define fatal_error(elem, ...) _fatal(elem->pos, __VA_ARGS__)

void _fatal(SrcPos pos, const char *fmt, ...) {
    va_list args;
    printf("%s(%llu): ", pos.file, pos.line_num);
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}

void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    printf("FATAL: ");
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
    exit(1);
}
