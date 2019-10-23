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

#include "lex.c"

#include "ast.h"

#include "ast.c"

#include "parse.c"

#include "print.c"

#include "resolve.c"

#include "gen.c"

int main() {
    common_test();
    lex_test();
    // print_decl_test();
    resolve_test();
    //   gen_test();
}
