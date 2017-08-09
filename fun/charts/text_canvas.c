#include "text_canvas.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "link_list.h"

/**
 * A coordinate descriptor.
 */
typedef struct coord_s {
    int value;          /**< Value of the coordinate. */
    int base;           /**< Context's coordinate's position in the base context. */
    int size;           /**< Context's coordinate's size. */
} coord_t;

static void _coord_set(coord_t* p, int value, int base, int size) {
    assert(NULL != p);
    p->value = value;
    p->base = base;
    p->size = size;
}   /* _coord_set() */

/**
 * Subcanvas is a context that has an origin and size.
 */
typedef struct text_canvas_context_s {
    link_t link;        /**< Link for putting the context into a list. */
    text_canvas_t* tc;  /**< Pointer back to the main containing text canvas. */
    coord_t x;
    coord_t y;
} text_canvas_context_t;

/**
 * A text canvas.
 */
struct text_canvas_s {
    text_canvas_context_t base_context;
    text_canvas_context_t* context;
    list_t sub_context_list;
    /* char line_start; */
    /* char line_middle; */
    /* char line_end; */
    char grid[];
};   /* struct text_canvas_s */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_new(int x_size, int y_size) {
    text_canvas_t* tc = NULL;
    if ((x_size > 0) && (y_size > 0)) {
        int chars = y_size * (x_size + 1);
        if ((chars > 0) && (chars > x_size) && (chars > y_size)) {
            tc = calloc(1, sizeof(struct text_canvas_s) + chars);
            if (NULL != tc) {
                tc->base_context.tc = tc;
                _coord_set(&tc->base_context.x, 0, 0, x_size);
                _coord_set(&tc->base_context.y, 0, 0, y_size);
                tc->context = &tc->base_context;
                list_init(&tc->sub_context_list);
                memset(tc->grid, ' ', chars);
                for (int y = 0; y < y_size; ++y) {
                    tc->grid[(y * (x_size + 1)) + x_size] = 0;  /* Set terminator. */
                }
            }
        }
    }
    return tc;
}   /* text_canvas_new() */

/* ------------------------------------------------------------------------- */
void text_canvas_del(text_canvas_t* tc) {
    if (NULL != tc) {
        list_foreach_struct(&tc->sub_context_list, context, text_canvas_context_t, link,
                            link_remove(&context->link);
                            free(context));
        free(tc);
    }
}   /* text_canvas_del() */

/* ------------------------------------------------------------------------- */
int text_canvas_x(const text_canvas_t* tc) {
    return (NULL == tc) ? 0 : tc->context->x.value;
}   /* text_canvas_x() */

/* ------------------------------------------------------------------------- */
int text_canvas_y(const text_canvas_t* tc) {
    return (NULL == tc) ? 0 : tc->context->y.value;
}   /* text_canvas_y() */

/* ------------------------------------------------------------------------- */
int text_canvas_x_size(const text_canvas_t* tc) {
    return (NULL == tc) ? 0 : tc->context->x.size;
}   /* text_canvas_x_size() */

/* ------------------------------------------------------------------------- */
int text_canvas_y_size(const text_canvas_t* tc) {
    return (NULL == tc) ? 0 : tc->context->y.size;
}   /* text_canvas_u_size() */

/* ------------------------------------------------------------------------- */
/* static int _int_between(int x, int lo, int hi) { */
/*     return (x < lo) ? lo : ((x > hi) ? hi : x); */
/* }   /\* _int_between() *\/ */

/* ------------------------------------------------------------------------- */
/**
 * Set the grid at the current location in @a context to @a value.
 *
 * @return if the coordinate was in range so the value was set.
 */
static int _context_set_grid(text_canvas_context_t* context, char value) {
    assert(NULL != context);
    text_canvas_t* tc = context->tc;
    int rval = 0;
    int base_x = context->x.base + context->x.value;
    int base_y = context->y.base + context->y.value;
    if ((0 <= context->x.value) && (context->x.value < context->x.size) &&
        (0 <= context->y.value) && (context->y.value < context->y.size) &&
        (0 <= base_x) && (base_x < tc->base_context.x.size) &&
        (0 <= base_y) && (base_y < tc->base_context.y.size)) {
        int grid_offset = (base_y * (tc->base_context.x.size + 1)) + base_x;
        rval = 1;
        tc->grid[grid_offset] = (0 == value) ? ' ' : value;
    }
    return rval;
}   /* _context_set_grid() */

/* ------------------------------------------------------------------------- */
static int _coord_minmax_value(const coord_t* p, int value) {
    assert(NULL != p);
    if (TEXT_CANVAS_MIN == value) {
        return 0;
    } else if (TEXT_CANVAS_MAX == value) {
        return p->size - 1;
    } else {
        return value;
    }
}   /* _coord_minmax_value() */

/* ------------------------------------------------------------------------- */
static int _coord_minmax_delta(const coord_t* p, int delta) {
    assert(NULL != p);
    if (TEXT_CANVAS_MIN == delta) {
        return 0;
    } else if (TEXT_CANVAS_MAX == delta) {
        return p->size - 1;
    } else {
        return p->value + delta;
    }
}   /* _coord_minmax_delta() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_move_to(text_canvas_t* tc, int x, int y) {
    if (NULL != tc) {
        tc->context->x.value = _coord_minmax_value(&tc->context->x, x);
        tc->context->y.value = _coord_minmax_value(&tc->context->y, y);
    }
    return tc;
}   /* text_canvas_move_to() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_move_by(text_canvas_t* tc, int dx, int dy) {
    if (NULL != tc) {
        tc->context->x.value = _coord_minmax_delta(&tc->context->x, dx);
        tc->context->y.value = _coord_minmax_delta(&tc->context->y, dy);
    }
    return tc;
}   /* text_canvas_move_by() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_line_x(text_canvas_t* tc, int n) {
    if (NULL != tc) {
        int end_x = _coord_minmax_delta(&tc->context->x, n);
        int inc = (end_x < tc->context->x.value) ? -1 : 1;
        /* @todo Optimize - skip to setting x = end_x when outside grid. */
        _context_set_grid(tc->context, '+');
        while (tc->context->x.value != end_x) {
            tc->context->x.value += inc;
            if (tc->context->x.value == end_x) {
                break;
            }
            _context_set_grid(tc->context, '-');
        }
        _context_set_grid(tc->context, '+');
    }
    return tc;
}   /* text_canvas_line_x() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_line_y(text_canvas_t* tc, int n) {
    if (NULL != tc) {
        int end_y = _coord_minmax_delta(&tc->context->y, n);
        int inc = (end_y < tc->context->y.value) ? -1 : 1;
        /* @todo Optimize - skip to setting y = end_y when outside grid. */
        _context_set_grid(tc->context, '+');
        while (tc->context->y.value != end_y) {
            tc->context->y.value += inc;
            if (tc->context->y.value == end_y) {
                break;
            }
            _context_set_grid(tc->context, '|');
        }
        _context_set_grid(tc->context, '+');
    }
    return tc;
}   /* text_canvas_line_y() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_puts(text_canvas_t* tc, const char* s) {
    if ((NULL != tc) && (NULL != s)) {
        int start_x = tc->context->x.value;
        for (; *s; s++) {
            /* @todo Support tabs? Support Unicode????? */
            if (isprint(*s)) {
                _context_set_grid(tc->context, *s);
            }
            if ('\r' == *s) {
                tc->context->x.value = start_x;
            } else if ('\n' == *s) {
                tc->context->x.value = start_x; /* Assumes Unix use of newline. */
                tc->context->y.value--;         /* @todo Add row/column mode support? */
            } else {
                tc->context->x.value++;         /* @todo Support setting text direction? */
            }
        }
    }
    return tc;
}   /* text_canvas_puts() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_printf(text_canvas_t* tc, const char* format, ...) {
    if ((NULL != tc) && (NULL != format)) {
        va_list va;
        char text[0x4000] = {0};
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        return text_canvas_puts(tc, text);
    }
    return tc;
}   /* text_canvas_printf() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_push_subcanvas(text_canvas_t* tc, int x_size, int y_size) {
    if ((NULL != tc) && (x_size > 0) && (y_size > 0)) {
        text_canvas_context_t* context = calloc(1, sizeof(struct text_canvas_context_s));
        if (NULL != context) {
            context->tc = tc;
            context->x.base = tc->context->x.base + tc->context->x.value;
            context->y.base = tc->context->y.base + tc->context->y.value;
            context->x.value = 0;
            context->y.value = 0;
            context->x.size = x_size;
            context->y.size = y_size;
            list_push(&tc->sub_context_list, &context->link);
            tc->context = context;
        }
        /* Nothing to do if it fails. Won't be pretty. */
    }
    return tc;
}   /* text_canvas_push_subcanvas() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_pop_subcanvas(text_canvas_t* tc) {
    if (NULL != tc) {
        text_canvas_context_t* sub = list_pop_struct(&tc->sub_context_list, text_canvas_context_t, link);
        free(sub);
        if (list_is_empty(&tc->sub_context_list)) {
            tc->context = &tc->base_context;
        } else {
            tc->context = list_top_struct(&tc->sub_context_list, text_canvas_context_t, link);
        }
    }
    return tc;
}   /* text_canvas_pop_subcanvas() */

/* ------------------------------------------------------------------------- */
void text_canvas_file_print_line(const char* line, void* cookie_is_file) {
    if ((NULL != line) && (NULL != cookie_is_file)) {
        FILE* file = cookie_is_file;
        fputs(line, file);
        fputc('\n', file);
    }
}   /* text_canvas_file_print_line() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_print_canvas(text_canvas_t* tc, text_canvas_line_print_t print_line, void* cookie) {
    /* @todo Should this print just the current subcontext? */
    if ((NULL != tc) && (NULL != print_line)) {
        for (int y = tc->base_context.y.size - 1; y >= 0; y--) {
            print_line(&tc->grid[y * (tc->base_context.x.size + 1)], cookie);
        }
    }
    return tc;
}   /* text_canvas_print_canvas() */

/* ------------------------------------------------------------------------- */
text_canvas_t* text_canvas_fprint_canvas(text_canvas_t* tc, FILE* file) {
    return text_canvas_print_canvas(tc, text_canvas_file_print_line, file);
}   /* text_canvas_fprint_canvas() */
