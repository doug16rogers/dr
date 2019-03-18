/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

/**
 * @file
 * @brief Unit test main program.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cut.h"

/* ------------------------------------------------------------------------- */
static void usage(FILE* f, int exit_code) CUT_GNU_ATTRIBUTE((noexit));
static void usage(FILE* f, int exit_code) {
    fprintf(f, "\n");
    fprintf(f, "Usage: example_unit_test [test-substring...]\n");
    fprintf(f, "\n");
    fprintf(f, "  -h, -help                     Print this usage information.\n");
    fprintf(f, "\n");
    cut_usage(f);
    exit(exit_code);
}

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[])
{
    int i = 0;
    cut_parse_command_line(&argc, argv);

    CUT_INSTALL_SUITE(text_canvas_test);

    for (i = 1; i < argc; ++i) {
        if ((0 == strcmp(argv[i], "-h")) || (0 == strcmp(argv[i], "-help"))) {
            usage(stdout, 0);
        } else {
            if (!cut_include_test(argv[i])) {
                fprintf(stderr, "unit_test: no test names match '%s'\n", argv[i]);
                fprintf(stderr, "unit_test: use -h for usage information\n");
                exit(1);
            }
        }
    }

    return cut_run(1);
}
