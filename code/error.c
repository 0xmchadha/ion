typedef struct SrcPos {
    const char *file;
    size_t line_num;
}SrcPos;

extern size_t line_num;
extern const char *file_name;

void syntax_error(const char *fmt, ...) {
    va_list args;
    printf("%s(%llu): ", file_name, line_num);
    va_start(args, fmt);
    vprintf(fmt, args);
    printf("\n");
    va_end(args);
}

