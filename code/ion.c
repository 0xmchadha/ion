
const char *ion_compile_str(const char *path, const char *str) {
    init_stream(path, str);

    create_base_types();
    export_c_func();
    install_global_decls(parse_file());

    resolve_symbols();

    gen_preamble();
    forward_declare_types();
    generate_types();
    generate_functions();

    const char *result = gen_buf;
    gen_buf = NULL;
    return result;
}

const char *ion_compile_file(const char *path) {
    const char *result = ion_compile_str(path, read_file(path));
    const char *c_path = replace_ext(path, "c");

    write_file(c_path, result, buf_len(result));
}

int ion_main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage %s <file_name>", argv[0]);
        exit(1);
    }

    ion_compile_file(argv[1]);
}
