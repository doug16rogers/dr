/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include "wd-util.h"

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char* g_program = "unnamed";

int g_verbose = 0;

/**
 * Set this to a non-zero value to optimize circular buffer shifting for
 * space instead of speed. When this is zero the code will use up to twice as
 * much buffer space on the stack. When this is non-zero the code will use a
 * backwards copy for some of the data which will usually be slightly slower
 * for right shifts (it cannot use machine-code string copying).
 */
#if !defined(OPTIMIZE_CIRCULAR_SHIFT_FOR_SPACE)
#define OPTIMIZE_CIRCULAR_SHIFT_FOR_SPACE 1
#endif

/* ------------------------------------------------------------------------- */
const char* SetProgramName(char* path_or_name) {
    if (NULL != path_or_name) {
        g_program = NamePartOfPath(path_or_name);
    }
    return g_program;
}   /* SetProgramName() */

/* ------------------------------------------------------------------------- */
char* CircularShiftLeft(char* s, size_t len, size_t n) {
    if ((NULL != s) && (len > 1)) {
        n %= len;
#if (OPTIMIZE_CIRCULAR_SHIFT_FOR_SPACE)
        if ((len - n) < n) {
            return CircularShiftRight(s, len, len - n);
        } else
#endif
        if (n > 0) {
            char overlap[n];
            memcpy(overlap, s, n);
            memcpy(s, &s[n], len - n);
            memcpy(&s[len - n], overlap, n);
        }
    }
    return s;
}   /* CircularShiftLeft() */

/* ------------------------------------------------------------------------- */
char* CircularShiftRight(char* s, size_t len, size_t n) {
    if ((NULL != s) && (len > 1)) {
        n %= len;
#if (OPTIMIZE_CIRCULAR_SHIFT_FOR_SPACE)
        if ((len - n) < n) {
            return CircularShiftLeft(s, len, len - n);
        } else if (n > 0) {
            char overlap[n];
            memcpy(overlap, &s[len - n], n);
            memcpy_backward(&s[n], s, len - n);
            memcpy(s, overlap, n);
        }
#else
        return CircularShiftLeft(s, len, len - n);
#endif
    }
    return s;
}   /* CircularShiftRight() */

/* ------------------------------------------------------------------------- */
int IsOption(const char* input, const char** value_ptr, const char* descriptor) {
    int rval = 0;
    int finished = 0;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        ++input;
        if ('-' == *input) {
            ++input;
        }
    } else {
        finished = 1;
    }
    while (!finished) {
        if ((0 == *input) || ('=' == *input)) {
            finished = 1;
            rval = (0 == *descriptor) || (':' == *descriptor);
        } else if ((0 == *descriptor) || ((':' != *descriptor) && (*input != *descriptor))) {
            finished = 1;
        } else {
            if (':' != *descriptor) {
                ++input;
            }
            ++descriptor;
        }
    }
    if (NULL != value_ptr) {
        *value_ptr = (rval && ('=' == *input)) ? (input + 1) : NULL;
    }
    return rval;
}   /* IsOption() */

/* ------------------------------------------------------------------------- */
int IsFlagOption(const char* input, int* flag_value_ptr, const char* descriptor) {
    int flag_value = 1;
    int rval = 0;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        rval = IsOption(input, NULL, descriptor);
        if (!rval) {
            flag_value = 0;
            const int k = ('-' == input[1]) ? 1 : 0;
            if (('n' == input[k+1]) && ('o' == input[k+2]) && ('-' == input[k+3])) {
                rval = IsOption(&input[k+3], NULL, descriptor);
            }
        }
    }
    if (rval && (NULL != flag_value_ptr)) {
        *flag_value_ptr = flag_value;
    }
    return rval;
}   /* IsFlagOption() */

/* ------------------------------------------------------------------------- */
int IsIntOption(const char* input, int* int_value_ptr, const char* descriptor, int default_value) {
    int int_value = default_value;
    int rval = 0;
    const char* opt_value = NULL;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        rval = IsOption(input, &opt_value, descriptor);
        if (rval && (NULL != opt_value) && (0 != opt_value[0])) {
            rval = (1 == sscanf(opt_value, "%d", &int_value));
        }
    }
    if (rval && (NULL != int_value_ptr)) {
        *int_value_ptr = int_value;
    }
    return rval;
}   /* IsIntOption() */

/* ------------------------------------------------------------------------- */
void* memcpy_backward(void* tgt, const void* src, size_t n) {
    size_t i;
    assert(n > 0);
    assert(NULL != tgt);
    assert(NULL != src);
    for (i = 0; i < n; ++i) {
        ((char*) tgt)[n - i - 1] = ((const char*) src)[n - i - 1];
    }
    return tgt;
}   /* memcpy_backward() */

/* ------------------------------------------------------------------------- */
const char* NamePartOfPath(const char* path) {
    const char* rval = path;
    if (NULL != path) {
        for (; 0 != *path; ++path) {
            if ((('/' == path[0]) || ('\\' == path[0])) &&
                !((0 == path[1]) || ('/' == path[1]) || ('\\' == path[1]))) {
                rval = &path[1];
            }
        }
    }
    return rval;
}   /* NamePartOfPath() */

/* ------------------------------------------------------------------------- */
void PrintUsageError(const char* format, ...) {
    char text[0x0100] = "";
    va_list va;
    va_start(va, format);
    vsnprintf(text, sizeof(text), format, va);
    va_end(va);
    fprintf(stderr, "%s: %s\n", g_program, text);
    fprintf(stderr, "%s: Use '%s --help' for usage information.\n", g_program, g_program);
    exit(1);
}   /* PrintUsageError() */

/* ------------------------------------------------------------------------- */
void PrintVerbose(const char* format, ...) {
    if (g_verbose) {
        char text[0x400] = "";
        va_list va;
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        printf("%s: %s\n", g_program, text);
    }
}   /* PrintVerbose() */

