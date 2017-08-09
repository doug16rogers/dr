/**
 * @file
 * @brief Example suite consisting of tests that require initialization.
 */

#include <stdio.h>

#define TEXT_CANVAS_DEFINE_ABBREVIATIONS 1

#include "cut.h"
#include "text_canvas.h"

#if defined(_WIN32)
#define fscanf fscanf_s
#endif

/**
 * Wrapper to reap a text canvas.
 */
typedef struct test_s {
    text_canvas_t* tc;
} test_t;

/**
 * Test initialization function. Nothing to do.
 */
static cut_result_t test_init(test_t* test) {
    CUT_TEST_PASS();
}   /* test_init() */

/**
 * Test finalization function. It's always called, even if test_init() fails
 * (because it might have partially failed). So the state of test->variables
 * must be checked before operating on them.
 */
static void test_exit(test_t* test) {
    if (NULL != test->tc) {
        tc_del(test->tc);
    }
}   /* test_exit() */

/* ------------------------------------------------------------------------- */
static cut_result_t simple_test(test_t* test) {
    const int W = 80;
    const int H = 24;
    test->tc = text_canvas_new(W, H);
    CUT_ASSERT_NONNULL(test->tc);
/* printf("\n"); */
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_x(test->tc, (W-1));
/* text_canvas_fprint_canvas(test->tc, stdout); */
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_y(test->tc, H-1);
/* text_canvas_fprint_canvas(test->tc, stdout); */
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_x(test->tc, -(W-1));
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_y(test->tc, -(H-1));
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_move_to(test->tc, 10, 5);
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_x(test->tc, 20);
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_y(test->tc, 8);
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_x(test->tc, -20);
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_line_y(test->tc, -8);
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
    tc_puts(test->tc, "test");
/* printf("%s:%d: (%d,%d)\n", __FILE__, __LINE__, tc_x(test->tc), tc_y(test->tc)); */
/* printf("\n"); */
/* text_canvas_fprint_canvas(test->tc, stdout); */
    CUT_TEST_PASS();
}   /* simple_test() */

/* ------------------------------------------------------------------------- */
void text_canvas_test(void) {
    CUT_CONFIG_SUITE(sizeof(test_t), test_init, test_exit);
    CUT_ADD_TEST(simple_test);
}   /* text_canvas_test() */
