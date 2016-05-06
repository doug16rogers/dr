/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#ifndef RADIXIFY_STR_H_
#define RADIXIFY_STR_H_

typedef struct str_s *str_t;

str_t str_new(void);
str_t str_new_string(const char* initial_string);
void str_delete(str_t str);

char* str_data(str_t str);      /* string pointer; this is NEVER NULL; ALWAYS NUL-terminated. */
int str_len(str_t str);         /* characters in string */

str_t str_append_char(str_t str, char c);
str_t str_append_string(str_t str, const char* s);

str_t str_reverse(str_t str);

#endif  // RADIXIFY_STR_H_
