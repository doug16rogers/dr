/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include "str.h"

#include <stdlib.h>
#include <string.h>

#define kStrInitSize    (0x0100)

static char g_shared_string[kStrInitSize] = "";

struct str_s {
    char* data;
    int len;
    int size;
};

/* ------------------------------------------------------------------------- */
void str_delete(str_t str) {
    free(str);
}   /* str_delete() */

/* ------------------------------------------------------------------------- */
static str_t str_new_size(int size) {
    str_t str = malloc(sizeof(struct str_s) + size);
    if (NULL != str) {
        str->data = (char*) &str[1];
        str->len = 0;
        str->size = size;
        str->data[0] = 0;
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
    if (NULL == str) {
        return g_shared_string;
    } else {
        return str->data;
    }
}   /* str_data() */

/* ------------------------------------------------------------------------- */
int str_len(str_t str) {
    return (NULL == str) ? 0 : str->len;
}   /* str_len() */

/* ------------------------------------------------------------------------- */
str_t str_append_char(str_t str, char c) {
    if (NULL == str) {
        return str;
    }
    if ((str->len + 1) >= str->size) {
        int new_full_size = str->size + str->size + sizeof(struct str_s);
        if (new_full_size > (str->size + sizeof(struct str_s))) {
            str_t new_str = malloc(new_full_size);
            if (NULL == new_str) {
                str->len--;         /* If we've run out of memory, overwrite previous char. */
            } else {
                memcpy(new_str, str, str->size + sizeof(struct str_s));
                free(str);
                str = new_str;
                str->size += str->size;
            }
        } else {
            /* Integer overflow. We can't double our size again. */
        }
    }
    str->data[str->len++] = c;
    str->data[str->len] = 0;
    return str;
}

/* ------------------------------------------------------------------------- */
str_t str_append_string(str_t str, const char* s) {
    if ((NULL != str) && (NULL != s)) {
        /*
         * @todo(dr) Yeah, uh, this could be optimized.
         */
        for (; *s; ++s) {
            str_append_char(str, *s);
        }
    }
    return str;
}   /* str_append_string() */

/* ------------------------------------------------------------------------- */
str_t str_reverse(str_t str) {
    if (NULL != str) {
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
