/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include "str.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define kStrInitSize    (0x0100)

struct str_s {
    char* data;
    int len;
    int size;
    int error;
};

/* ------------------------------------------------------------------------- */
void str_delete(str_t str) {
    free(str);
}   /* str_delete() */

/* ------------------------------------------------------------------------- */
static str_t str_new_size(int size) {
    assert(size > 0);
    str_t str = malloc(sizeof(struct str_s));
    if (NULL != str) {
        str->data = malloc(size);
        if (NULL == str->data) {
            free(str);
            str = NULL;
        } else {
            str->len = 0;
            str->size = size;
            str->error = 0;
            str->data[0] = 0;
        }
    }
    return str;
}   /* str_new_size() */

/* ------------------------------------------------------------------------- */
str_t str_new(void) {
    return str_new_size(kStrInitSize);
}   /* str_new() */

/* ------------------------------------------------------------------------- */
str_t str_new_string(const char* initial_string) {
    return str_append_string(str_new(), initial_string);
}   /* str_new_string() */

/* ------------------------------------------------------------------------- */
char* str_data(str_t str) {
    return (NULL == str) ? NULL : str->data;
}   /* str_data() */

/* ------------------------------------------------------------------------- */
int str_len(str_t str) {
    return (NULL == str) ? 0 : str->len;
}   /* str_len() */

/* ------------------------------------------------------------------------- */
str_t str_append_char(str_t str, char c) {
    if ((NULL == str) || (str->error)) {
        return str;
    }
    if ((str->len + 1) >= str->size) {
        char* new_data = NULL;
        int new_size = str->size + str->size;
        if (new_size <= str->size) {
            /* Integer overflow. We can't double our size again. */
            str->error = 1;
            return str;
        }
        new_data = malloc(new_size);
        if (NULL == new_data) {
            str->error = 1;
            return str;
        }
        memcpy(new_data, str->data, str->size);
        free(str->data);
        str->size = new_size;
        str->data = new_data;
    }
    str->data[str->len++] = c;
    str->data[str->len] = 0;
    return str;
}

/* ------------------------------------------------------------------------- */
str_t str_append_string(str_t str, const char* s) {
    if (!str_error(str) && (NULL != s)) {
        /*
         * @todo(dr) Yeah, uh, this could be optimized.
         */
        for (; *s && !str_error(str); ++s) {
            str_append_char(str, *s);
        }
    }
    return str;
}   /* str_append_string() */

/* ------------------------------------------------------------------------- */
str_t str_reverse(str_t str) {
    if (!str_error(str)) {
        const int kHalfLen = str->len / 2;
        char* data = str->data;
        char b;
        int i = 0;
        int k = 0;
        for (i = 0, k = str->len - 1; i < kHalfLen; ++i, --k) {
            b = data[i];
            data[i] = data[k];
            data[k] = b;
        }
    }
    return str;
}   /* str_reverse() */

/* ------------------------------------------------------------------------- */
int str_error(str_t str) {
    return (NULL == str) || str->error;
}   /* str_error() */
