/* Copyright (c) 2003, 2010 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html. */
/* $Id$ */

#ifndef __cut_h__
#define __cut_h__

/**
 * @file
 * C Unit Testing
 *
 * Provides a unit testing framework for C.
 *
 * This is a simplification of my earlier version.
 *
 * Examples are provided in example_unit_test.c, example_simple.c and
 * example_complex.c. See those files.
 *
 * To build and run the example, use the following:
 *
 * @code
 * gcc -o example_unit_test example_unit_test.c example_simple.c example_complex.c cut.c
 * ./example_unit_test -help
 * ./example_unit_test
 * ./example_unit_test -show-cases
 * @endcode
 *
 * The "complex" example needs "input-data.txt", which consists of the first
 * 10 Fibonacci numbers (1 1 2 3 5 8 13 21 34 55).
 */
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Test or initialization results.
 *
 * - PASS, the test was run and completely successfully.
 *
 * - FAIL, the test was run and an assertion failed.
 *
 * - SKIP, the test was not run.
 *
 * - ERROR, the test could not be run due to a failure to establish a proper
 * testing environment, usually due to a failure in the initialization
 * function.
 */
typedef enum {
  CUT_RESULT_FIRST = 0,
  CUT_RESULT_PASS = CUT_RESULT_FIRST,
  CUT_RESULT_FAIL,
  CUT_RESULT_SKIP,
  CUT_RESULT_ERROR,
  CUT_RESULT_LAST = CUT_RESULT_ERROR
} cut_result_t;

#define CUT_RESULT_COUNT (1 + CUT_RESULT_LAST - CUT_RESULT_FIRST)

/**
 * Text name of result.
 */
extern const char* cut_result_name[CUT_RESULT_COUNT];

/*
 * Types for various callbacks.
 */
typedef void         (*cut_install_func_t)(void);
typedef cut_result_t (*cut_init_func_t)(void* data);
typedef void         (*cut_exit_func_t)(void* data);
typedef cut_result_t (*cut_test_func_t)(void* data);

cut_result_t cut_install_suite(const char* name, cut_install_func_t suite_install);
cut_result_t cut_config_suite(size_t size, cut_init_func_t test_init, cut_exit_func_t test_exit);
cut_result_t cut_add_test(const char* test_name, cut_test_func_t test_func);

/**
 * In your main test program (that is, not a particular test suite), use this
 * macro to install a test suite with the given @a _name. Supply the name of
 * the suite without quotes. This results in a call to the test suite's
 * installer function, _name(), which must have external linkage.
 *
 * _name() should optionally use CUT_CONFIG_SUITE() and then a sequence of
 * CUT_ADD_TEST() to add each of the suite's tests.
 */
#define CUT_INSTALL_SUITE(_name)       \
  do {                                 \
    extern void _name(void);           \
    cut_install_suite(# _name, _name); \
  } while (0)

/**
 * Should be used in the test suite's installer function (see
 * CUT_INSTALL_SUITE()). This adds full data and initialization/finalization
 * functionality for the current test suite.  This is not necessary for
 * simple tests that do not require pre-test or post-test processing.
 *
 * Before running each test in a suite, a data buffer of @a _size bytes will
 * be allocated and zeroed, then passed to the test suite's @a _init()
 * function. This allows a suite to pre-initialize data for each test. After
 * testing (or failure of @a _init()), the data buffer is passed to @a
 * _exit() for finalization before the buffer is zeroed for the next test.
 */
#define CUT_CONFIG_SUITE(_size,_init,_exit) cut_config_suite(_size, (cut_init_func_t) _init, (cut_exit_func_t) _exit)

/**
 * Called from the suite's installer, this macro adds a test for the function
 * with the given @a _name.
 */
#define CUT_ADD_TEST(_name)  cut_add_test( # _name, (cut_test_func_t) _name)

/**
 * Processes command line arguments for cut-specific settings. If an error is
 * found in a cut-specific setting then a message is printed to stderr and
 * CUT_RESULT_FAIL is returned. The cut-specific arguments are removed as
 * they are consumed.
 *
 * See cut_usage() in cut.c for the available options.
 *
 * @return CUT_RESULT_PASS on success, CUT_RESULT_FAIL if an error was detected.
 */
cut_result_t cut_parse_command_line(int* argc, char* argv[]);

/**
 * Prints cut-specific usage information to the given @a file, or to stdout
 * if @a file is NULL.
 */
void cut_usage(FILE* file);

/**
 * Select suite.tests that include @a substring in their names to run.
 *
 * Initially all tests are selected to be run during cut_run(). Upon the
 * first call of cut_include_test(), all non-matching tests are set NOT to
 * run and from that point on tests are added to the list with this and each
 * subsequent call to cut_include_test().
 *
 * @param substring - string to find within a full test name ("suite.test")
 * for including that test when cut_run() is called.
 *
 * @return the number of tests selected to run by the given @a substring,
 * even if they were already included by a previous call. The idea here is to
 * indicate that the value of @a substring actually selects a test - not
 * operator error. If this returns 0 then the caller should report an error
 * that no tests are selected.
 */
int cut_include_test(const char* substring);

/**
 * Run the entire suite - all tests that are currently enabled.
 *
 * @param print_summary - if non-zero, a summary will be printed to stdout.
 *
 * @return a cut_result_t code, the first of each of these conditions:
 * - CUT_RESULT_ERROR if any of the tests reported an error condition.
 * - CUT_RESULT_FAIL if any of the tests reported a failure.
 * - CUT_RESULT_SKIP if *all* of the tests were skipped.
 * - CUT_RESULT_PASS if at least one test passed and there were no errors or
 * failures.
 */
cut_result_t cut_run(int print_summary);

/**
 * Flags for results.
 */
#define CUT_RESULT_FLAG(_res)   ((_res > CUT_RESULT_LAST) ? 0 : (1 << _res))

#define CUT_FLAG_PASS   CUT_RESULT_FLAG(CUT_RESULT_PASS)
#define CUT_FLAG_FAIL   CUT_RESULT_FLAG(CUT_RESULT_FAIL)
#define CUT_FLAG_SKIP   CUT_RESULT_FLAG(CUT_RESULT_SKIP)
#define CUT_FLAG_ERROR  CUT_RESULT_FLAG(CUT_RESULT_ERROR)

#define CUT_FLAG_ALL    ((1 << CUT_RESULT_COUNT) - 1)

/**
 * Default name delimiter.
 */
#define CUT_NAME_DELIMITER_DEFAULT      "."

/**
 * The delimiter between suite and test names.
 */
extern const char* cut_name_delimiter;

/**
 * The default value used for whether or not to shorten suite and test names.
 */
#define CUT_SHORTEN_NAMES_DEFAULT       1

/**
 * The default value used for whether or not to print assertion information
 * from initialization functions.
 */
#define CUT_PRINT_INIT_CASES_DEFAULT    0

/**
 * Set this to remove "_test" or "test" from the end of suite and test names
 * (not case-sensitive).
 *
 * For example, if you use CUT_INSTALL_SUITE(module_test) to install a module
 * test, which in turn calls CUT_ADD_TEST(simple_test), then with this flag
 * set the name that is reported will be "module.simple". Without the flag
 * set the name will be "module_test.simple_test".
 */
extern int cut_shorten_names;

/**
 * Bit flags indicating whether test names should be printed based on the
 * test result. Use CUT_RESULT_FLAG(_res) to set or clear the bits.
 */
extern int cut_print_test_flags;

/**
 * Bit flags indicating whether individual test cases (assertions) should be
 * printed based on the case result. Use CUT_RESULT_FLAG(_res) to set or
 * clear the bits.
 */
extern int cut_print_case_flags;

/**
 * By default, assertions (cases) are not printed when the assertions arise
 * from a test's initialization function. Set this to non-zero to enable
 * printing (along with cut_print_case_flags above) of assertions from the
 * initialization function.
 */
extern int cut_print_init_cases;

/**
 * Registers the result of an assertion.
 * All of the other assertion functions and macros end up calling this.
 */
cut_result_t cut_assertion_result(const char*  file,
                                  int          line,
                                  cut_result_t result,
                                  const char*  message);

/**
 * Makes an assertion for the currently running test or test_init() function.
 * All of the other assertion functions and macros end up calling this.
 */
cut_result_t cut_assert(const char* file,
                        int         line,
                        int         condition,
                        const char* message);


#if defined(GNUC)
#define CUT_GNU_ATTRIBUTE(...)  __attribute(__VA_ARGS__)
#else
#define CUT_GNU_ATTRIBUTE(...)
#endif

/**
 * Allows freely formatted printing of the message associated with the
 * assertion.
 */
cut_result_t cut_assertf(const char* file,
                         int         line,
                         int         condition,
                         const char* format,
                         ...) CUT_GNU_ATTRIBUTE((format(printf,4,5)));

/**
 * Sets the type of an int, which should be large enough to hold a pointer,
 * too.
 */
typedef long long cut_int_t;

cut_result_t cut_assert_pointer(const char* file, int line, const void* proper, const void* actual,
                                const char* extra_message);
cut_result_t cut_assert_int_in(const char* file, int line, cut_int_t proper_lo, cut_int_t proper_hi, cut_int_t actual,
                               const char* extra_message);
cut_result_t cut_assert_double_in(const char* file, int line, double proper_lo, double proper_hi, double actual,
                                  const char* extra_message);
cut_result_t cut_assert_string(const char* file, int line, const char* proper, const char* actual,
                               const char* extra_message);
cut_result_t cut_assert_memory(const char* file, int line, const void* proper, const void* actual, size_t n,
                               const char* extra_message);

/**
 * Default epsilon value for a comparision of doubles. The following
 * two assertions are made:
 *
 *   actual >= proper * (1.0 - epsilon)
 *   actual <= proper * (1.0 + epsilon)
 *
 * Both must be true for the assertion to succeed.
 */
#define CUT_EPSILON   0.000001

/**
 * Examines the result of running @a _code and return
 */
#define CUT_RETURN(_code)                  \
  do {                                     \
    cut_result_t _cut_result = _code ;     \
    if (CUT_RESULT_PASS != _cut_result) {  \
      return _cut_result;                  \
    }                                      \
  } while (0)

/*
 * Most of the time you'll want to use CUT_ASSERT_xxx and not the macros in
 * this section.
 *
 * Sometimes it is necessary to pass the file and line directly to the
 * assertion. This is useful when you want to call a sub-function repeatedly
 * to test some assertions, but when an error is reported you want it to
 * indicate the line number of the calling function, not the assertion.
 *
 * The first two arguments are the file name and line number.
 */
#define CUT_FL_ASSERT_MESSAGE(_f,_l,_cond,_msg)     CUT_RETURN(cut_assert(_f,_l, ( _cond ), _msg ))
#define CUT_FL_ASSERT(_f,_l,_cond)                  CUT_FL_ASSERT_MESSAGE(_f,_l, _cond, # _cond )
#define CUT_FL_ASSERT_INT_IN(_f,_l,_lo,_hi,_a)      CUT_RETURN(cut_assert_int_in(_f,_l, (cut_int_t) (_lo), (cut_int_t) (_hi), (cut_int_t) (_a), NULL))
#define CUT_FL_ASSERT_INT(_f,_l,_p,_a)              CUT_FL_ASSERT_INT_IN(_f,_l, (_p), (_p), (_a))
#define CUT_FL_ASSERT_POINTER(_f,_l,_p,_a)          CUT_RETURN(cut_assert_pointer(_f,_l, (_p), (_a), NULL))
#define CUT_FL_ASSERT_DOUBLE_IN(_f,_l,_lo,_hi,_a)   CUT_RETURN(cut_assert_double_in(_f,_l, (_lo), (_hi), (_a), NULL))
#define CUT_FL_ASSERT_DOUBLE_NEAR(_f,_l,_p,_a,_eps) CUT_FL_ASSERT_DOUBLE_IN(_f,_l, (_p) * (1.0 - (_eps)), (_p) * (1.0 + (_eps)), (_a))
#define CUT_FL_ASSERT_DOUBLE(_f,_l,_p,_a)           CUT_FL_ASSERT_DOUBLE_NEAR(_f,_l, (_p), (_a), CUT_EPSILON)
#define CUT_FL_ASSERT_DOUBLE_EXACT(_f,_l,_p,_a)     CUT_FL_ASSERT_DOUBLE_NEAR(_f,_l, (_p), (_a), 0.0)
#define CUT_FL_ASSERT_STRING(_f,_l,_p,_a)           CUT_RETURN(cut_assert_string(_f,_l, (_p), (_a), NULL))
#define CUT_FL_ASSERT_MEMORY(_f,_l,_p,_a,_n)        CUT_RETURN(cut_assert_memory(_f,_l, (_p), (_a), (_n), NULL))
#define CUT_FL_ASSERT_NULL(_f,_l,_a)                CUT_FL_ASSERT(_f,_l, ((_a) == NULL))
#define CUT_FL_ASSERT_NONNULL(_f,_l,_a)             CUT_FL_ASSERT(_f,_l, ((_a) != NULL))

/*
 * These include an extra message in addition to the file and line; they are
 * intended for developing your own aggregate assertion checkers, where the
 * message could be the name of a struct field, say.
 */
#define CUT_FLM_ASSERT_INT_IN(_f,_l,_lo,_hi,_a,_m)   CUT_RETURN(cut_assert_int_in(_f,_l, (cut_int_t) (_lo), (cut_int_t) (_hi), (cut_int_t) (_a), _m))
#define CUT_FLM_ASSERT_INT(_f,_l,_p,_a,_m)           CUT_FLM_ASSERT_INT_IN(_f,_l, (_p), (_p), (_a), _m)
#define CUT_FLM_ASSERT_POINTER(_f,_l,_p,_a,_m)       CUT_RETURN(cut_assert_pointer(_f,_l, (_p), (_a), _m))
#define CUT_FLM_ASSERT_DOUBLE_IN(_f,_l,_lo,_hi,_a,_m)   CUT_RETURN(cut_assert_double_in(_f,_l, (_lo), (_hi), (_a), _m))
#define CUT_FLM_ASSERT_DOUBLE_NEAR(_f,_l,_p,_a,_eps,_m) CUT_FLM_ASSERT_DOUBLE_IN(_f,_l, (_p) * (1.0 - (_eps)), (_p) * (1.0 + (_eps)), (_a), _m)
#define CUT_FLM_ASSERT_DOUBLE(_f,_l,_p,_a,_m)        CUT_FLM_ASSERT_DOUBLE_NEAR(_f,_l, (_p), (_a), CUT_EPSILON, _m)
#define CUT_FLM_ASSERT_DOUBLE_EXACT(_f,_l,_p,_a,_m)  CUT_FLM_ASSERT_DOUBLE_NEAR(_f,_l, (_p), (_a), 0.0, _m)
#define CUT_FLM_ASSERT_STRING(_f,_l,_p,_a,_m)        CUT_RETURN(cut_assert_string(_f,_l, (_p), (_a), _m))
#define CUT_FLM_ASSERT_MEMORY(_f,_l,_p,_a,_n,_m)     CUT_RETURN(cut_assert_memory(_f,_l, (_p), (_a), (_n), _m))
    
/*
 * Use these directly in your test function (good for most cases).
 */
#define CUT_ASSERT_MESSAGE(_cond,_msg)     CUT_FL_ASSERT_MESSAGE(__FILE__,__LINE__, (_cond),(_msg))
#define CUT_ASSERT(_cond)                  CUT_FL_ASSERT(__FILE__,__LINE__, _cond)
#define CUT_ASSERTF(_cond,...)             CUT_RETURN(cut_assertf(__FILE__,__LINE__,(_cond),__VA_ARGS__))
#define CUT_ASSERT_INT_IN(_lo,_hi,_a)      CUT_FL_ASSERT_INT_IN(__FILE__,__LINE__, (_lo),(_hi),(_a))
#define CUT_ASSERT_INT(_p,_a)              CUT_FL_ASSERT_INT(__FILE__,__LINE__, (_p),(_a))
#define CUT_ASSERT_POINTER(_p,_a)          CUT_FL_ASSERT_POINTER(__FILE__,__LINE__, (_p),(_a))
#define CUT_ASSERT_DOUBLE_IN(_lo,_hi,_a)   CUT_FL_ASSERT_DOUBLE_IN(__FILE__,__LINE__, (_lo),(_hi),(_a))
#define CUT_ASSERT_DOUBLE_NEAR(_p,_a,_eps) CUT_FL_ASSERT_DOUBLE_NEAR(__FILE__,__LINE__, (_p),(_a),(_eps))
#define CUT_ASSERT_DOUBLE(_p,_a)           CUT_FL_ASSERT_DOUBLE(__FILE__,__LINE__, (_p),(_a))
#define CUT_ASSERT_DOUBLE_EXACT(_p,_a)     CUT_FL_ASSERT_DOUBLE_EXACT(__FILE__,__LINE__, (_p),(_a))
#define CUT_ASSERT_STRING(_p,_a)           CUT_FL_ASSERT_STRING(__FILE__,__LINE__, (_p),(_a))
#define CUT_ASSERT_MEMORY(_p,_a,_n)        CUT_FL_ASSERT_MEMORY(__FILE__,__LINE__, (_p),(_a),(_n))
#define CUT_ASSERT_NULL(_a)                CUT_FL_ASSERT_NULL(__FILE__,__LINE__, (_a))
#define CUT_ASSERT_NONNULL(_a)             CUT_FL_ASSERT_NONNULL(__FILE__,__LINE__, (_a))

/**
 * Use this to end the current test with the given result (just the short
 * result name, not with CUT_RESULT_). For example, CUT_TEST_END(PASS).
 */
#define CUT_TEST_END(_res)                                              \
  do {                                                                  \
  return cut_assertion_result(__FILE__, __LINE__, CUT_RESULT_ ## _res, "end test: " # _res); \
  } while (0)

/**
 * Use this to end a test with a passing result.
 */
#define CUT_TEST_PASS()    CUT_TEST_END(PASS)

/**
 * Use this to skip the current test.
 */
#define CUT_TEST_SKIP()    CUT_TEST_END(SKIP)

/**
 * Use this to end a test with a failure result.
 */
#define CUT_TEST_FAIL()    CUT_TEST_END(FAIL)

/*
 * Wrapper functions:
 */
typedef cut_result_t (*cut_wrap_init_func_t)(cut_init_func_t init, void* data, void* wrapper_cookie);
typedef void         (*cut_wrap_exit_func_t)(cut_exit_func_t exit, void* data, void* wrapper_cookie);
typedef cut_result_t (*cut_wrap_test_func_t)(cut_test_func_t test, void* data, void* wrapper_cookie);

/**
 * This interface is intended to be used by the C++ wrapper, ccut.hh.
 */
void cut_set_wrapper(cut_wrap_init_func_t wrap_init,
                     cut_wrap_exit_func_t wrap_exit,
                     cut_wrap_test_func_t wrap_test,
                     void* wrapper_cookie);

#ifdef __cplusplus
}
#endif

#endif
