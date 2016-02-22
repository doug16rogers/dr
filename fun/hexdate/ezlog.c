/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include "ezlog.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Opaque ezlog structure.
 */
struct ezlog_s {
    ezlog_level_t level;
    ezlog_printer_t printer;
    void* printer_context;
};

static void ezlog_default_printer(const char* s, void* user_context);

static ezlog_t g_ezlog_default = {
    EZLOG_LEVEL_INFO,
    NULL, /* ezlog_default_printer */
    NULL, /* stderr */
};

/* ------------------------------------------------------------------------- */
/**
 * The default printer posts @a s to stderr - rather, to the file
 * stream given by @a user_context.
 */
static void ezlog_default_printer(const char* s, void* user_context) {
    FILE* stream = (FILE*) user_context;
    if ((NULL != s) && (NULL != stream)) {
        fprintf(stream, "%s\n", s);
    }
}   /* ezlog_default_printer() */

/* ------------------------------------------------------------------------- */
static ezlog_t* ezlog_check_default(ezlog_t* ez) {
    if (NULL == ez) {
        ez = &g_ezlog_default;
        if (NULL == ez->printer) {
            ez->printer = ezlog_default_printer;
            ez->printer_context = (void*) stderr;
        }
    }
    return ez;
}   /* ezlog_check_default() */

/* ------------------------------------------------------------------------- */
char ezlog_level_letter(ezlog_level_t level) {
    switch (level) {
    case EZLOG_LEVEL_FATAL:   return 'F';
    case EZLOG_LEVEL_ERROR:   return 'E';
    case EZLOG_LEVEL_WARNING: return 'W';
    case EZLOG_LEVEL_INFO:    return 'I';
    case EZLOG_LEVEL_DEBUG:   return 'D';
    }
    return '?';
}   /* ezlog_level_letter() */

/* ------------------------------------------------------------------------- */
ezlog_t* ezlog_new(ezlog_level_t level, ezlog_printer_t printer, void* printer_context) {
    ezlog_t* ez = (ezlog_t*) malloc(sizeof(ezlog_t));
    if (NULL != ez) {
        ez->level = level;
        ez->printer = printer;
        ez->printer_context = printer_context;
    }
    return ez;
}   /* ezlog_new() */

/* ------------------------------------------------------------------------- */
int ezlog_delete(ezlog_t** ez_p) {
    int rval = EINVAL;
    if ((NULL != ez_p) && (NULL != *ez_p)) {
        free(*ez_p);
        *ez_p = NULL;
        rval = 0;
    }
    return rval;
}   /* ezlog_delete() */


/* int ezlog_set_default(ezlog_t* ez);     /\* Sets the default logger. *\/ */
/* int ezlog_set_printer(ezlog_t* ez, ezlog_printer_t printer, void* user_context); */

/* ------------------------------------------------------------------------- */
int ezlog_set_level(ezlog_t* ez, ezlog_level_t level) {
    ez = ezlog_check_default(ez);
    ez->level = level;
    return 0;
}   /* ezlog_set_level() */

/* ------------------------------------------------------------------------- */
int ezlog_vprintf(ezlog_t* ez, ezlog_level_t level, const char* format, va_list va) {
    int rval = -1;
    ez = ezlog_check_default(ez);
    if ((level <= ez->level) && (NULL != ez->printer)) {
        char text[0x0400] = "[?] ";
        text[1] = ezlog_level_letter(level);
        vsnprintf(&text[4], sizeof(text) - 4, format, va);
        text[sizeof(text) - 1] = 0;
        ez->printer(text, ez->printer_context);
        rval = 0;
    }
    return rval;
}   /* ezlog_vprintf() */

/* ------------------------------------------------------------------------- */
int ezlog_printf(ezlog_t* ez, ezlog_level_t level, const char* format, ...) {
    int rval = 0;
    va_list va;
    va_start(va, format);
    rval = ezlog_vprintf(ez, level, format, va);
    va_end(va);
    return rval;
}   /* ezlog_printf() */
