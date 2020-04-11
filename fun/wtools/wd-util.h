/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */
#ifndef WTOOLS_WD_UTIL_H_
#define WTOOLS_WD_UTIL_H_

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * The name of the main program. Use SetProgramName() below.
 */
extern const char* g_program;

/**
 * When non-zero, PrintVerbose() will print its messages.
 */
extern int g_verbose;

/* ------------------------------------------------------------------------- */
/**
 * Set the name of this program. Usually this is done with
 * SetProgramName(argv[0]) in main().
 *
 * @note This does not make a copy of @p path_or_name; it stores a pointer
 *       in @p g_program to the basename inside @p path_or_name (as in
 *       NamePartOfPath() below).
 */
const char* SetProgramName(char* path_or_name);

/* ------------------------------------------------------------------------- */
/**
 * Shift a string's characters to the left by @p n characters.
 *
 * @param s - string to shift.
 * @param len - length of string.
 * @param n - number of character positions to shift.
 *
 * @return s.
 */
char* CircularShiftLeft(char* s, size_t len, size_t n);

/* ------------------------------------------------------------------------- */
/**
 * Shift a string's characters to the right by @p n characters.
 *
 * @param s - string to shift.
 * @param len - length of string.
 * @param n - number of character positions to shift.
 *
 * @return s.
 */
char* CircularShiftRight(char* s, size_t len, size_t n);

/* ------------------------------------------------------------------------- */
/**
 * Look for an option of the form "[-[-]]option[=value]".
 *
 * If @p input contains '=' then non-null @p *value_ptr is set to point
 * to the character after '=' (or is set to NULL if there is no argument).
 *
 * @p descriptor may contain ':' characters which indicate abbreviation
 * points for the option. For example, "o:pt:ion" will match "-o",
 * "-o=value", "-opt", "-opt=value", "-option" and "-option=value".
 *
 * @return 1 if @p input matches @p descriptor, 0 otherwise.
 */
int IsOption(const char* input, const char** value_ptr, const char* descriptor);

/* ------------------------------------------------------------------------- */
/**
 * Look for flag option of the form "-[-][no-]option".
 *
 * @p descriptor may contain ':' characters which indicate abbreviation
 * points for the option. See IsOption() for more information.
 *
 * If @p input matches the descriptor then the value of @p *flag_value_ptr (if
 * not NULL) will be set to 1. If @p input matches the descriptor with "no-"
 * prefixed then @p *flag_value_ptr will be set to 0. If @p input does not
 * match @p descriptor, @p *flag_value_ptr is not modified.
 *
 * @return 1 if @p input matches @p descriptor with or without a "no-" prefix,
 * 0 otherwise.
 */
int IsFlagOption(const char* input, int* flag_value_ptr, const char* descriptor);

/* ------------------------------------------------------------------------- */
/**
 * Look for flag option of the form "-[-]option[=value]".
 *
 * @p descriptor may contain ':' characters which indicate abbreviation
 * points for the option. See IsOption() for more information.
 *
 * If @p input matches the descriptor then the value of @p *int_value_ptr (if
 * not NULL) will be set to the "[=value]" portion of the input as an integer.
 * If no "[=value]"  portion is present, then @p default_value is used for
 * the integer.
 *
 * @return 1 if @p input matches @p descriptor with a valid integer value, 0
 * otherwise.
 */
int IsIntOption(const char* input, int* int_value_ptr, const char* descriptor, int default_value);

/* ------------------------------------------------------------------------- */
/**
 * Copy @p n bytes from @p src to @p tgt, but backwards (last byte first).
 *
 * @note No checking is performed on any arguments.
 *
 * @return @p tgt.
 */
void* memcpy_backward(void* tgt, const void* src, size_t n);

/* ------------------------------------------------------------------------- */
/**
 * Print an error message to stderr then exit the program with exit code 1.
 */
void PrintUsageError(const char* format, ...);

/* ------------------------------------------------------------------------- */
/**
 * Find and return a pointer to the file name portion of @p path.
 *
 * @param path - a path whose name is desired. Typically this is argv[0] from
 * main().
 *
 * @return a pointer the first character after the last directory delimiter
 * (forward or back slash) in @p path, or @p path if none is found.
 */
const char* NamePartOfPath(const char* path);

/* ------------------------------------------------------------------------- */
/**
 * If global g_verbose is non-zero, print the message described by @p format.
 */
void PrintVerbose(const char* format, ...);

#ifdef __cplusplus
}
#endif

#endif  // WTOOLS_WD_UTIL_H_
