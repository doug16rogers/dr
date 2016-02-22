/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#ifndef HEXDATE_EZLOG_H_
#define HEXDATE_EZLOG_H_

/*
 * Provide an easy line-based logging facility.
 *
 * Log messages do not need newlines. They are added by the logger.
 *
 * If NULL is passed for the 'ez' parameter and no default logger is set, an
 * internal logger is used which writes to stderr.
 */

#include <stdarg.h>

#ifdef GNUC
#  define GNU_ATTRIBUTES(_x)    __attributes__(_x)
#else
#  define GNU_ATTRIBUTES(_x)
#endif

typedef enum ezlog_level_e {
    EZLOG_LEVEL_FATAL,
    EZLOG_LEVEL_ERROR,
    EZLOG_LEVEL_WARNING,
    EZLOG_LEVEL_INFO,
    EZLOG_LEVEL_DEBUG,
} ezlog_level_t;

typedef struct ezlog_s ezlog_t;

typedef void (*ezlog_printer_t)(const char* s, void* printer_context);

char ezlog_level_letter(ezlog_level_t level);

ezlog_t* ezlog_new(ezlog_level_t level, ezlog_printer_t printer, void* printer_context);
int ezlog_delete(ezlog_t** ez_p);      /* Will NULL-ify *ez_p. */

/* int ezlog_set_default(ezlog_t* ez);     /\* Sets the default logger. *\/ */
/* int ezlog_set_printer(ezlog_t* ez, ezlog_printer_t printer, void* printer_context); */
int ezlog_set_level(ezlog_t* ez, ezlog_level_t level);
int ezlog_vprintf(ezlog_t* ez, ezlog_level_t level, const char* format, va_list va);
int ezlog_printf(ezlog_t* ez, ezlog_level_t level, const char* format, ...) GNU_ATTRIBUTES((format (printf, 3, 4)));

#define ezlogf(_ez, ...) ezlog_printf(_ez, EZLOG_LEVEL_FATAL,   __VA_ARGS__)
#define ezloge(_ez, ...) ezlog_printf(_ez, EZLOG_LEVEL_ERROR,   __VA_ARGS__)
#define ezlogw(_ez, ...) ezlog_printf(_ez, EZLOG_LEVEL_WARNING, __VA_ARGS__)
#define ezlogi(_ez, ...) ezlog_printf(_ez, EZLOG_LEVEL_INFO,    __VA_ARGS__)
#define ezlogd(_ez, ...) ezlog_printf(_ez, EZLOG_LEVEL_DEBUG,   __VA_ARGS__)

#define EZLOGF(...)     ezlog_printf(NULL, EZLOG_LEVEL_FATAL,   __VA_ARGS__)
#define EZLOGE(...)     ezlog_printf(NULL, EZLOG_LEVEL_ERROR,   __VA_ARGS__)
#define EZLOGW(...)     ezlog_printf(NULL, EZLOG_LEVEL_WARNING, __VA_ARGS__)
#define EZLOGI(...)     ezlog_printf(NULL, EZLOG_LEVEL_INFO,    __VA_ARGS__)
#define EZLOGD(...)     ezlog_printf(NULL, EZLOG_LEVEL_DEBUG,   __VA_ARGS__)

#endif  // HEXDATE_EZLOG_H_
