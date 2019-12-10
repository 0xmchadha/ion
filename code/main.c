#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.c"

#include "error.c"

#include "lex.c"

#include "ast.h"

#include "ast.c"

#include "parse.c"

#include "print.c"

#include "resolve.c"

#include "gen.c"

#include "ion.c"

int main(int argc, char **argv) {
    /* common_test();  */
    /* lex_test();  */
    /* print_decl_test();  */
    init_keywords();
    resolve_test();
    /* //gen_test();  */

    // common_test();
    /* init_keywords(); */
    /* ion_main(argc, argv); */
 }
