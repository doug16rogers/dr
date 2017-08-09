#ifndef CHARTS_TEXT_CANVAS_H_
#define CHARTS_TEXT_CANVAS_H_

#include <limits.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(TEXT_CANVAS_DEFINE_ABBREVIATIONS)
#define TEXT_CANVAS_DEFINE_ABBREVIATIONS 1
#endif

typedef struct text_canvas_s text_canvas_t;

/**
 * This represents the furthest point within the current context.
 */
#define TEXT_CANVAS_MIN INT_MIN
#define TEXT_CANVAS_MAX INT_MAX

/**
 * Create a text canvas that can be drawn and written upon.
 *
 * The coordinates range from 0 to the axis size minus one. Note that the y
 * coordinates increase upward.
 *
 * @param x_size - width of canvas, in characters.
 *
 * @param y_size - height of canvas, in characters.
 *
 * @return a pointer to initialized canvas, or NULL on error. All cells will
 * contain space characters.
 */
text_canvas_t* text_canvas_new(int x_size, int y_size);

void text_canvas_del(text_canvas_t* tc);

int text_canvas_x(const text_canvas_t* tc);
int text_canvas_y(const text_canvas_t* tc);
int text_canvas_x_size(const text_canvas_t* tc);
int text_canvas_y_size(const text_canvas_t* tc);

text_canvas_t* text_canvas_move_to(text_canvas_t* tc, int x, int y);
text_canvas_t* text_canvas_move_by(text_canvas_t* tc, int dx, int dy);
text_canvas_t* text_canvas_line_x(text_canvas_t* tc, int n);
text_canvas_t* text_canvas_line_y(text_canvas_t* tc, int n);

/**
 * If a newline is seen then the cursor will move to the next lower y
 * coordinate and to the same x coordinate that started the string. If a
 * carriage return is seen then just the x coordinate is reset.
 */
text_canvas_t* text_canvas_puts(text_canvas_t* tc, const char* s);
text_canvas_t* text_canvas_printf(text_canvas_t* tc, const char* format, ...);

text_canvas_t* text_canvas_push_subcanvas(text_canvas_t* tc, int x_size, int y_size);
text_canvas_t* text_canvas_pop_subcanvas(text_canvas_t* tc);

/**
 * Function to capture the final canvas by printing a line at at time. The
 * line does *not* have a newline.
 */
typedef void (*text_canvas_line_print_t)(const char* line, void* cookie);

/**
 * This essentially calls fputs(line, file) followed by fputc('\n', file).
 */
void text_canvas_file_print_line(const char* line, void* cookie_is_file);

/**
 * Prints from the highest y first. Prints the *entire* canvas, not the
 * current subcanvas.
 */
text_canvas_t* text_canvas_print_canvas(text_canvas_t* tc, text_canvas_line_print_t print_line, void* cookie);

/**
 * Same as:
 * ```
 *     text_canvas_print_canvas(tc, text_canvas_file_print_line, file)
 * ```
 */
text_canvas_t* text_canvas_fprint_canvas(text_canvas_t* tc, FILE* file);

/*
 * Short names can be easier to combine.
 */
#if (TEXT_CANVAS_DEFINE_ABBREVIATIONS != 0)
#define TC_MIN          TEXT_CANVAS_MIN
#define TC_MAX          TEXT_CANVAS_MAX
#define tc_new          text_canvas_new
#define tc_del          text_canvas_del
#define tc_x            text_canvas_x
#define tc_y            text_canvas_y
#define tc_x_size       text_canvas_x_size
#define tc_y_size       text_canvas_y_size
#define tc_move_to      text_canvas_move_to
#define tc_move_by      text_canvas_move_by
#define tc_line_x       text_canvas_line_x
#define tc_line_y       text_canvas_line_y
#define tc_puts         text_canvas_puts
#define tc_printf       text_canvas_printf
#define tc_push         text_canvas_push_subcanvas
#define tc_pop          text_canvas_pop_subcanvas
#define tc_print_canvas text_canvas_print_canvas
#define tc_fprint_canvas text_canvas_fprint_canvas
#endif

#ifdef __cplusplus
}
#endif

#endif  // CHARTS_TEXT_CANVAS_H_
