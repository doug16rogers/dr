/* Copyright (c) 2003, 2010 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html. */

#include "cut.h"

#if defined(_WIN32)
#include <windows.h>
#endif

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#if !defined(_WIN32)
#include <sys/time.h>
#endif
#include <time.h>

#if defined(GNUC)
#define FIELD(_name)   ._name =
#else
#define FIELD(_name)
#endif

#if defined(_WIN32)
#define snprintf(_s,_n,...)         _snprintf_s(_s, _n, _TRUNCATE, __VA_ARGS__)
#define vsnprintf(_s,_n,_fmt,_va)   vsnprintf_s(_s, _n, _TRUNCATE, _fmt, _va)
#define strncpy(_d,_s,_n)           strncpy_s(_d,_n,_s,_TRUNCATE)
#define strcasecmp(_s1,_s2)         _strnicmp(_s1, _s2, CUT_NAME_LEN_MAX)
typedef          __int32  int32_t;
typedef unsigned __int32 uint32_t;
typedef          __int64  int64_t;
typedef unsigned __int64 uint64_t;
#endif

/**
 * A microsecond value.
 */
typedef uint64_t usec_t;

/**
 * Text name of result.
 */
const char* cut_result_name[CUT_RESULT_COUNT] =
{
  "PASS",
  "FAIL",
  "SKIP",
  "ERROR",
};

/**
 * The delimiter between suite and test names.
 */
const char* cut_name_delimiter = CUT_NAME_DELIMITER_DEFAULT;

/**
 * Set this to remove "_test" or "test" from the end of suite and test names
 * (not case-sensitive).
 *
 * For example, if you use CUT_INSTALL_SUITE(module_test) to install a module
 * test, which in turn calls CUT_ADD_TEST(simple_test), then with this flag
 * set the name that is reported will be "module.simple". Without the flag
 * set the name will be "module_test.simple_test".
 */
int cut_shorten_names = CUT_SHORTEN_NAMES_DEFAULT;

/**
 * Bit flags indicating whether test names should be printed based on the
 * test result. Use CUT_RESULT_FLAG(_res) to set or clear the bits.
 */
int cut_print_test_flags = CUT_FLAG_PASS | CUT_FLAG_FAIL | CUT_FLAG_ERROR;

/**
 * Bit flags indicating whether individual test cases (assertions) should be
 * printed based on the case result. Use CUT_RESULT_FLAG(_res) to set or
 * clear the bits.
 */
int cut_print_case_flags = CUT_FLAG_FAIL | CUT_FLAG_ERROR;

/**
 * Whether to print assertion information from a test's init function.
 */
int cut_print_init_cases = CUT_PRINT_INIT_CASES_DEFAULT;

/**
 * Maximum length of a suite or test name. Note that the test name includes
 * the suite name, so suites must actually be shorter.
 */
#define CUT_NAME_LEN_MAX  0x100

typedef struct cut_test_s  cut_test_t;
typedef struct cut_suite_s cut_suite_t;

/**
 * Flag for a test to be excluded from the run. It will report its result as
 * SKIP.
 */
#define CUT_TEST_FLAG_EXCLUDE   0x0001

/**
 * Test type.
 */
struct cut_test_s
{
  /**
   * The name includes the suite name ("suite.test").
   */
  char name[CUT_NAME_LEN_MAX];

  /**
   * Flags for this test, a bitwise OR of CUT_TEST_FLAG_xxx.
   */
  int flags;

  /**
   * Test function.
   */
  cut_test_func_t func;

  /**
   * Parent suite.
   */
  cut_suite_t* suite;

  /**
   * Next test in list, or NULL if the current test is the last.
   */
  cut_test_t* next;

};  /* struct cut_test_s */

/**
 * Suite type.
 */
struct cut_suite_s
{
  /**
   * The name includes the suite name ("suite.test").
   */
  char name[CUT_NAME_LEN_MAX];

  /**
   * Number of data bytes to send.
   */
  size_t size;

  /**
   * Data pointer. This is allocated up front.
   */
  void* data;

  /**
   * Initialization function for each test in suite.
   */
  cut_init_func_t init;

  /**
   * Finalization function for each test in suite.
   */
  cut_exit_func_t exit;

  /**
   * Linked list of tests.
   */
  cut_test_t* test;

  /**
   * Next suite in list.
   */
  cut_suite_t* next;
};   /* struct cut_suite_s */

/**
 * Global cut object, just in case I want to objectify it some day. The whole
 * point of this exercise was to simplify my existing unit test framework to
 * get rid of all that, but I can't seem to resist this concession.
 */
typedef struct cut_s
{
  /**
   * List of suites.
   */
  cut_suite_t* suite;

  /**
   * Suite currently in use.
   */
  cut_suite_t* active_suite;

  /**
   * Test currently in use.
   */
  cut_test_t* active_test;

  /**
   * Number of assertions made with each type of result.
   */
  unsigned int assertions[CUT_RESULT_COUNT];

  /**
   * Number of tests finished with each type of result.
   */
  unsigned int tests[CUT_RESULT_COUNT];

  /**
   * Set when the test name is printed in order to track when to add
   * newlines.
   */
  int test_name_hanging;

  /**
   * Set when a call is made to cut_include_test() is called. Until that
   * point, all tests are included.
   */
  int include_test_called;
} cut_t;

/**
 * Global (singleton) C unit test object.
 */
static cut_t g_cut_info =
{
  FIELD(suite)              NULL,
  FIELD(active_suite)       NULL,
  FIELD(active_test)        NULL,
  FIELD(assertions)         { 0, 0, 0, 0 },
  FIELD(tests)              { 0, 0, 0, 0 },
  FIELD(test_name_hanging)  0,
  FIELD(include_test_called) 0
};   /* g_cut_info */

/**
 * Pointer to global C unit test object.
 */
static cut_t* g_cut = &g_cut_info;

/**
 * Maximum length of character image function.
 */
#define CHAR_IMAGE_MAX_LEN 8

/**
 * Maximum length of a string to be printed when a mismatch is detected. This
 * should be used by all strings passed to printable_diff_string().
 */
#define PRINTABLE_DIFF_STRING_MAX_LEN  64

/* ------------------------------------------------------------------------- */
/**
 * @return a monotonically increasing time in microseconds since the first
 * call.
 */
static uint64_t usec_time(void)
{
    static int started = 0;
#if defined(_WIN32)
    FILETIME       ftime;
    ULARGE_INTEGER ul;
    static ULARGE_INTEGER start = {0};
    GetSystemTimeAsFileTime(&ftime);
    ul.LowPart  = ftime.dwLowDateTime;
    ul.HighPart = ftime.dwHighDateTime;
    if (!started) {
        started = 1;
        start = ul;
    }
    ul.QuadPart -= start.QuadPart;
    return ul.QuadPart / 10;
#else
    struct timeval tv = { 0, 0 };
    static uint64_t start = 0;
    uint64_t now;
    gettimeofday(&tv, NULL);
    now = (uint64_t) tv.tv_sec;
    now = (1000000 * now) + tv.tv_usec;
    if (!started) {
        started = 1;
        start = now;
    }
    now -= start;
    return now;
#endif
}   /* usec_time() */

/* ------------------------------------------------------------------------- */
/**
 * @param c - the character whose image is sought.
 *
 * @param image - buffer to hold the image, or NULL for a shared buffer. This
 * must point to a buffer of at least CHAR_IMAGE_MAX_LEN characters.
 *
 * @return a printable image of character @a c.
 */
static const char* char_image(char c, char* image)
{
  static char shared_image[CHAR_IMAGE_MAX_LEN] = "";

  image = (NULL == image) ? shared_image : image;

  switch (c)
  {
  case '\'': strncpy(image, "'\\''", CHAR_IMAGE_MAX_LEN); break;
  case '\t': strncpy(image, "'\\t'", CHAR_IMAGE_MAX_LEN); break;
  case '\r': strncpy(image, "'\\r'", CHAR_IMAGE_MAX_LEN); break;
  case '\n': strncpy(image, "'\\n'", CHAR_IMAGE_MAX_LEN); break;

  default:
    if (isprint(c))
    {
      image[0] = '\'';
      image[1] = c;
      image[2] = '\'';
      image[3] = 0;
      return image;
    }
    if (c < 10) snprintf(image, CHAR_IMAGE_MAX_LEN - 1, "'\\%u'", (unsigned) (unsigned char) c);
    else        snprintf(image, CHAR_IMAGE_MAX_LEN - 1, "'\\x%02X'", (unsigned) (unsigned char) c);
  }

  image[CHAR_IMAGE_MAX_LEN-1] = 0;
  return image;
}   /* char_image() */

/* ------------------------------------------------------------------------- */
static char* printable_diff_string(char* printable, const char* src, size_t len, size_t diff_index)
{
  const size_t half_len = (PRINTABLE_DIFF_STRING_MAX_LEN / 2) - 2;   /* Allow for two dots on either side. */

  assert(src && printable);

  if (len < PRINTABLE_DIFF_STRING_MAX_LEN)
  {
    /*
     * If there's no need to truncate.
     */
    strncpy(printable, src, PRINTABLE_DIFF_STRING_MAX_LEN-1);
  }
  else if (diff_index > half_len)
  {
    /*
     * If the difference appears more than halfway through the buffer, use a
     * leading ellipsis.
     */
    printable[0] = '.';
    printable[2] = '.';
    strncpy(&printable[2], &src[diff_index - half_len], PRINTABLE_DIFF_STRING_MAX_LEN-1-2);

    if (len > (diff_index + half_len))
    {
      /*
       * If a trailing ellipsis is needed, too.
       */
      printable[PRINTABLE_DIFF_STRING_MAX_LEN-1-2] = '.';
      printable[PRINTABLE_DIFF_STRING_MAX_LEN-1-1] = '.';
    }
  }
  else   /* else the difference occurs early in a long string... */
  {
    strncpy(printable, src, PRINTABLE_DIFF_STRING_MAX_LEN-1);
    printable[PRINTABLE_DIFF_STRING_MAX_LEN-1-2] = '.';
    printable[PRINTABLE_DIFF_STRING_MAX_LEN-1-1] = '.';
  }

  printable[PRINTABLE_DIFF_STRING_MAX_LEN-1] = 0;
  return printable;
}   /* printable_diff_string() */

/* ------------------------------------------------------------------------- */
static int remove_tail(char* name, const char* tail)
{
  const size_t name_length = strlen(name);
  const size_t tail_length = strlen(tail);

  if ((name_length > tail_length) &&
      (0 == strcasecmp(&name[name_length - tail_length], tail)))
  {
    name[name_length - tail_length] = 0;
    return 1;
  }

  return 0;
}   /* remove_tail() */

/* ------------------------------------------------------------------------- */
static void shorten_name(char* name)
{
  if (cut_shorten_names)
  {
    if (!remove_tail(name, "_test")) remove_tail(name, "test" );
  }
}   /* shorten_name() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_install_suite(const char* name, cut_install_func_t suite_install)
{
  cut_suite_t* suite = NULL;
  int result = CUT_RESULT_PASS;

  assert(NULL != name);
  assert(NULL != suite_install);

  suite = (cut_suite_t*) malloc(sizeof(*suite));

  if (NULL == suite)
  {
    return CUT_RESULT_FAIL;
  }

  memset(suite, 0, sizeof(*suite));
  strncpy(suite->name, name, CUT_NAME_LEN_MAX);
  suite->name[CUT_NAME_LEN_MAX-1] = 0;
  shorten_name(suite->name);
  g_cut->active_suite = suite;

  if (NULL == g_cut->suite)
  {
    g_cut->suite = suite;
  }
  else
  {
    /*
     * Yeah, this is inefficient. But it only happens on startup and I don't
     * feel like implementing a doubly-linked list to maintain insertion
     * order.
     */
    cut_suite_t* last = NULL;

    for (last = g_cut->suite; last->next != NULL; last = last->next)
    {
        /* do nothing; finding end of list. */
    }

    last->next = suite;
  }

  suite_install();
  return result;
}   /* cut_install_suite() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_config_suite(size_t size, cut_init_func_t test_init, cut_exit_func_t test_exit)
{
  cut_result_t result = CUT_RESULT_PASS;
  cut_suite_t* suite = NULL;
  assert(NULL != g_cut);
  assert(NULL != g_cut->active_suite);

  suite = g_cut->active_suite;

  if (NULL != suite->data)
  {
    free(suite->data);
    suite->data = NULL;
    suite->size = 0;
  }

  suite->init = NULL;
  suite->exit = NULL;

  if (size > 0)
  {
    suite->data = malloc(size);

    if (NULL == suite->data)
    {
      result = CUT_RESULT_FAIL;
    }
    else
    {
      suite->size = size;
      suite->init = test_init;
      suite->exit = test_exit;
    }
  }

  return result;
}   /* cut_config_suite() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_add_test(const char* test_name, cut_test_func_t test_func)
{
  cut_result_t result = CUT_RESULT_PASS;
  cut_test_t* test = NULL;
  cut_suite_t* suite = NULL;

  assert(NULL != test_name);
  assert(NULL != test_func);
  assert(NULL != g_cut);
  assert(NULL != g_cut->active_suite);

  suite = g_cut->active_suite;

  test = (cut_test_t*) malloc(sizeof(*test));

  if (NULL == test)
  {
    return CUT_RESULT_FAIL;
  }

  memset(test, 0, sizeof(*test));
  snprintf(test->name, CUT_NAME_LEN_MAX, "%s%s%s", suite->name, cut_name_delimiter, test_name);
  test->name[CUT_NAME_LEN_MAX-1] = 0;
  shorten_name(test->name);
  test->func = test_func;
  test->suite = suite;

  if (NULL == suite->test)
  {
    suite->test = test;
  }
  else
  {
    /*
     * Yeah, this is inefficient. But it only happens on startup and I don't
     * feel like implementing a doubly-linked list to maintain insertion
     * order.
     */
    cut_test_t* last = NULL;

    for (last = suite->test; last->next != NULL; last = last->next)
    {
    }

    last->next = test;
  }

  return result;
}   /* cut_add_test() */

/* ------------------------------------------------------------------------- */
/**
 * Processes command line arguments for cut-specific settings. If an error is
 * found in a cut-specific setting, exit() is called with error code 1 and
 * the cut-specific usage information is printed (along with any registered
 * client usage information).
 *
 * See cut_usage() in cut.c for the available options.
 */
cut_result_t cut_parse_command_line(int* argc, char* argv[])
{
  int i = 0;

  assert(NULL != argc);
  assert(NULL != argv);

  for (i = 1; i < *argc; i++)
  {
    char* arg = argv[i];
    int arg_used = 1;

    assert(NULL != arg);

    if (*arg != '-')
    {
      continue;
    }

    if (*arg == '-') arg++;
    if (*arg == '-') arg++;
    if      (strcmp(arg, "show-cases"         ) == 0) cut_print_case_flags  = CUT_FLAG_ALL;
    else if (strcmp(arg, "show-pass-cases"    ) == 0) cut_print_case_flags |= CUT_FLAG_PASS;
    else if (strcmp(arg, "show-fail-cases"    ) == 0) cut_print_case_flags |= CUT_FLAG_FAIL;
    else if (strcmp(arg, "show-skip-cases"    ) == 0) cut_print_case_flags |= CUT_FLAG_SKIP;
    else if (strcmp(arg, "show-error-cases"   ) == 0) cut_print_case_flags |= CUT_FLAG_ERROR;
    else if (strcmp(arg, "show-no-cases"      ) == 0) cut_print_case_flags  = 0;
    else if (strcmp(arg, "no-show-cases"      ) == 0) cut_print_case_flags  = 0;
    else if (strcmp(arg, "no-show-pass-cases" ) == 0) cut_print_case_flags &= ~CUT_FLAG_PASS;
    else if (strcmp(arg, "no-show-fail-cases" ) == 0) cut_print_case_flags &= ~CUT_FLAG_FAIL;
    else if (strcmp(arg, "no-show-skip-cases" ) == 0) cut_print_case_flags &= ~CUT_FLAG_SKIP;
    else if (strcmp(arg, "no-show-error-cases") == 0) cut_print_case_flags &= ~CUT_FLAG_ERROR;
    else if (strcmp(arg, "show-init-cases"    ) == 0) cut_print_init_cases = 1;
    else if (strcmp(arg, "no-show-init-cases" ) == 0) cut_print_init_cases = 0;
    else if (strcmp(arg, "show-tests"         ) == 0) cut_print_test_flags  = CUT_FLAG_ALL;
    else if (strcmp(arg, "show-pass-tests"    ) == 0) cut_print_test_flags |= CUT_FLAG_PASS;
    else if (strcmp(arg, "show-fail-tests"    ) == 0) cut_print_test_flags |= CUT_FLAG_FAIL;
    else if (strcmp(arg, "show-skip-tests"    ) == 0) cut_print_test_flags |= CUT_FLAG_SKIP;
    else if (strcmp(arg, "show-error-tests"   ) == 0) cut_print_test_flags |= CUT_FLAG_ERROR;
    else if (strcmp(arg, "show-no-tests"      ) == 0) cut_print_test_flags  = 0;
    else if (strcmp(arg, "no-show-tests"      ) == 0) cut_print_test_flags  = 0;
    else if (strcmp(arg, "no-show-pass-tests" ) == 0) cut_print_test_flags &= ~CUT_FLAG_PASS;
    else if (strcmp(arg, "no-show-fail-tests" ) == 0) cut_print_test_flags &= ~CUT_FLAG_FAIL;
    else if (strcmp(arg, "no-show-skip-tests" ) == 0) cut_print_test_flags &= ~CUT_FLAG_SKIP;
    else if (strcmp(arg, "no-show-error-tests") == 0) cut_print_test_flags &= ~CUT_FLAG_ERROR;
    else
    {
      arg_used = 0;
    }

    if (arg_used)
    {
      int j = 0;

      /*
       * Remove argument from list.
       */
      for (j = i + 1; j < *argc; j++)
      {
        argv[j-1] = argv[j];
      }

      (*argc)--;
      i--;
    }
  }   /* for each argument */

  return CUT_RESULT_PASS;
}   /* cut_parse_command_line() */

/* ------------------------------------------------------------------------- */
/**
 * Prints cut-specific usage information to the given @a file, or to stdout
 * if @a file is NULL.
 */
void cut_usage(FILE* file)
{
  fprintf(
    file,
    "  -[no-]show-cases              Do [not] show all test assertions.\n"
    "  -[no-]show-[type]-cases       Turn on showing of assertions for result <type>.\n"
    "  -show-no-cases                Same as -no-show-cases; shows no assertions.\n"
    "  -[no-]show-init-cases         Do [not] show assertions from init functions.\n"
    "  -[no-]show-tests              Do [not] show all test results.\n"
    "  -[no-]show-[type]-tests       Turn on showing of test results for <type>.\n"
    "  -show-no-tests                Same as -no-show-tests; shows no test results.\n"
    "\n"
    "  <type> - Result types may be pass, fail, skip, or error.\n"
    "\n"
    );
}   /* cut_usage() */

/* ------------------------------------------------------------------------- */
int cut_include_test(const char* substring) {
  int rval = 0;

  if (NULL == substring)
  {
    return 0;
  }

  /*
   * If this is the first time it has been called, set all the tests to be
   * excluded.
   */
  if (!g_cut_info.include_test_called)
  {
    g_cut_info.include_test_called = 1;

    for (cut_suite_t* suite = g_cut_info.suite; NULL != suite; suite = suite->next)
    {
      for (cut_test_t* test = suite->test; NULL != test; test = test->next)
      {
        test->flags |= CUT_TEST_FLAG_EXCLUDE;
      }
    }
  }

  /*
   * For each excluded test
   */
  for (cut_suite_t* suite = g_cut_info.suite; NULL != suite; suite = suite->next)
  {
    for (cut_test_t* test = suite->test; NULL != test; test = test->next)
    {
      if (NULL != strstr(test->name, substring))
      {
        rval++;
        test->flags &= ~CUT_TEST_FLAG_EXCLUDE;
      }
    }
  }

  return rval;
}   /* cut_include_test() */

/* ------------------------------------------------------------------------- */
static cut_wrap_init_func_t g_cut_wrap_init = NULL;
static cut_wrap_exit_func_t g_cut_wrap_exit = NULL;
static cut_wrap_test_func_t g_cut_wrap_test = NULL;
static void* g_cut_wrap_cookie = NULL;

/* ------------------------------------------------------------------------- */
/**
 * Sets the wrapper functions to be used when calling into a test suite. This
 * is provided so that the C++ wrapper may catch exceptions and indicate an
 * error or failure.
 */
void cut_set_wrapper(cut_wrap_init_func_t wrap_init,
                     cut_wrap_exit_func_t wrap_exit,
                     cut_wrap_test_func_t wrap_test,
                     void* wrapper_cookie)
{
  g_cut_wrap_init = wrap_init;
  g_cut_wrap_exit = wrap_exit;
  g_cut_wrap_test = wrap_test;
  g_cut_wrap_cookie = wrapper_cookie;
}   /* cut_set_wrapper() */

/* ------------------------------------------------------------------------- */
/**
 * Registers the result of an assertion.
 * All of the other assertion functions and macros end up calling this.
 */
cut_result_t cut_assertion_result(const char*  file,
                                  int          line,
                                  cut_result_t result,
                                  const char*  message)
{
  assert((result >= CUT_RESULT_FIRST) && (result <= CUT_RESULT_LAST));
  assert(NULL != g_cut);

  /*
   * Do not include assertions in init function.
   */
  if ((NULL != g_cut->active_test) || cut_print_init_cases)
  {
    g_cut->assertions[result]++;

    if (cut_print_case_flags & CUT_RESULT_FLAG(result))
    {
      if (g_cut->test_name_hanging)
      {
        printf("\n");
        g_cut->test_name_hanging = 0;
      }

      printf("%s:%d: %-5s %s\n", file, line, cut_result_name[result], message);
    }
  }

  return result;
}   /* cut_assertion_result() */

/* ------------------------------------------------------------------------- */
/**
 * Makes an assertion for the currently running test or test_init() function.
 * All of the other assertion functions and macros end up calling this.
 */
cut_result_t cut_assert(const char* file,
                        int         line,
                        int         condition,
                        const char* message)
{
  cut_result_t result = CUT_RESULT_PASS;

  assert(NULL != g_cut);
  assert(NULL != g_cut->active_suite);

  if (!condition)
  {
    result = (NULL != g_cut->active_test) ? CUT_RESULT_FAIL : CUT_RESULT_ERROR;
  }

  return cut_assertion_result(file, line, result, message);
}   /* cut_assert() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_assertf(const char* file,
                         int         line,
                         int         condition,
                         const char* format,
                         ...)
{
  char message[0x100] = "";
  va_list va;
  va_start(va, format);
  vsnprintf(message, sizeof(message), format, va);
  va_end(va);
  message[sizeof(message) - 1] = 0;
  return cut_assert(file, line, condition, message);
}   /* cut_assertf() */

/**
 * String used to separate the actual value from any extra message.
 */
#define CUT_EXTRA_MESSAGE_PAD   " - "

/* ------------------------------------------------------------------------- */
cut_result_t cut_assert_pointer(const char* file, int line, const void* proper, const void* actual,
                                const char* extra_message)
{
  return cut_assertf(file, line, proper == actual, "\n  Proper: @%p\n  Actual: @%p%s%s", proper, actual,
                     (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                     (extra_message == NULL) ? "" : extra_message);
}   /* cut_assert_pointer() */

/* ------------------------------------------------------------------------- */
/**
 * If @a lo greater than @a hi, swap them.
 */
#define CHECK_LO_HI(_type,_lo,_hi) \
  do {                             \
    if (_hi < _lo) {               \
      _type _temp = _lo;           \
      _lo = _hi;                   \
      _hi = _temp;                 \
    }                              \
  } while (0)

/* ------------------------------------------------------------------------- */
cut_result_t cut_assert_int_in(const char* file, int line, long long proper_lo, long long proper_hi, long long actual,
                               const char* extra_message)
{
  const char* comparison_text = (actual < proper_lo) ? " (< Lower)" : ((actual > proper_hi) ? " (> Upper)" : "");
#define MINGW 0
#if defined(MINGW)
  if (proper_lo == proper_hi)
  {
    return cut_assertf(file, line, proper_lo == actual,
                       "\n  Proper: %10ld (0x%08lX)\n  Actual: %10ld (0x%08lX)",
                       (long) proper_lo, (long) proper_lo, (long) actual, (long) actual,
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else
  {
    CHECK_LO_HI(long long, proper_lo, proper_hi);
    return cut_assertf(file, line, (proper_lo <= actual) && (actual <= proper_hi),
                       "\n  Lower:  %10ld (0x%08lX)\n  Actual: %10ld (0x%08lX)%s%s%s\n  Upper:  %10ld (0x%08lX)",
                       (long) proper_lo, (long) proper_lo,
                       (long) actual, (long) actual, comparison_text,
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message,
                       (long) proper_hi, (long) proper_hi);
  }
#else
  if (proper_lo == proper_hi)
  {
    return cut_assertf(file, line, proper_lo == actual,
                       "\n  Proper: %10lld (0x%08llX)\n  Actual: %10lld (0x%08llX)%s%s",
                       proper_lo, proper_lo, actual, actual,
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else
  {
    CHECK_LO_HI(long long, proper_lo, proper_hi);
    return cut_assertf(file, line, (proper_lo <= actual) && (actual <= proper_hi),
                       "\n  Lower:  %10lld (0x%08llX)\n  Actual: %10lld (0x%08llX)%s%s%s\n  Upper:  %10lld (0x%08llX)",
                       proper_lo, proper_lo, actual, actual, comparison_text,
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message,
                       proper_hi, proper_hi);
  }
#endif
}   /* cut_assert_int_in() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_assert_double_in(const char* file, int line, double proper_lo, double proper_hi, double actual,
                                  const char* extra_message)
{
  if (proper_lo == proper_hi)
  {
    return cut_assertf(file, line, proper_lo == actual,
                       "\n  Proper: %18.15E (%g)\n  Actual: %18.15E (%g)%s%s",
                       proper_lo, proper_lo, actual, actual,
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else
  {
    CHECK_LO_HI(double, proper_lo, proper_hi);
    const char* comparison_text = (actual < proper_lo) ? " (< Lower)" : ((proper_hi < actual) ? " (> Upper)" : "");
    return cut_assertf(file, line, (proper_lo <= actual) && (actual <= proper_hi),
                       "\n  Lower:  %18.15E (%g)\n  Actual: %18.15E (%g)%s%s%s\n  Upper:  %18.15E (%g)",
                       proper_lo, proper_lo, actual, actual, comparison_text,
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message,
                       proper_hi, proper_hi);
  }
}   /* cut_assert_double_in() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_assert_string(const char* file, int line, const char* proper, const char* actual,
                               const char* extra_message)
{
  size_t plen = 0;
  size_t alen = 0;
  size_t max_len = 0;
  size_t i = 0;

  /*
   * Handle the NULL cases first.
   */
  if ((NULL == proper) && (NULL == actual))
  {
    return cut_assertf(file, line, 1, "\n  Proper: NULL\n  Actual: NULL%s%s",
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else if (NULL == proper)
  {
    return cut_assertf(file, line, 0, "\n  Proper: NULL\n  Actual: non-NULL%s%s",
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else if (NULL == actual)
  {
    return cut_assertf(file, line, 0, "\n  Proper: non-NULL\n  Actual: NULL%s%s",
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }

  plen = strlen(proper);
  alen = strlen(actual);

  /*
   * Eventually I need to do the pretty string slice printing, but for now I
   * just run the memory comparison, being careful to check up to the
   * readable portion of either.
   */
/*   if (alen < plen) */
/*   { */
/*     return cut_assert_memory(file, line, proper, actual, alen + 1); */
/*   } */

/*   return cut_assert_memory(file, line, proper, actual, plen); */

  max_len = plen > alen ? plen : alen;
  max_len++;   /* Include terminating NUL. */

  for (i = 0; i < max_len; i++)
  {
    if (proper[i] != actual[i])
    {
      char pcimg[CHAR_IMAGE_MAX_LEN] = "";
      char acimg[CHAR_IMAGE_MAX_LEN] = "";
      char pdiff[PRINTABLE_DIFF_STRING_MAX_LEN] = "";
      char adiff[PRINTABLE_DIFF_STRING_MAX_LEN] = "";

      return cut_assertf(file, line, 0,
                         "\n  Proper at [%d]: 0x%02X %3d %-6s \"%s\"\n  Actual at [%d]: 0x%02X %3d %-6s \"%s\"%s%s",
                         (int) i, (unsigned) proper[i], (unsigned) proper[i], char_image(proper[i], pcimg),
                         printable_diff_string(pdiff, proper, plen, i),
                         (int) i, (unsigned) actual[i], (unsigned) actual[i], char_image(actual[i], acimg),
                         printable_diff_string(adiff, actual, alen, i),
                         (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                         (extra_message == NULL) ? "" : extra_message);
    }
  }

  return cut_assertf(file, line, 1, "strings of length %d (0x%02X) match%s%s", (int) plen, (int) plen,
                     (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                     (extra_message == NULL) ? "" : extra_message);
}   /* cut_assert_string() */

/* ------------------------------------------------------------------------- */
cut_result_t cut_assert_memory(const char* file, int line, const void* proper, const void* actual, size_t n,
                               const char* extra_message)
{
  size_t i = 0;
  const unsigned char* p = (const unsigned char*) proper;
  const unsigned char* a = (const unsigned char*) actual;

  /*
   * Handle the NULL cases first.
   */
  if ((NULL == proper) && (NULL == actual))
  {
    return cut_assertf(file, line, 1, "\n  Proper: NULL\n  Actual: NULL%s%s",
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else if (NULL == proper)
  {
    return cut_assertf(file, line, 0, "\n  Proper: NULL\n  Actual: non-NULL%s%s",
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }
  else if (NULL == actual)
  {
    return cut_assertf(file, line, 0, "\n  Proper: non-NULL\n  Actual: NULL%s%s",
                       (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                       (extra_message == NULL) ? "" : extra_message);
  }

  for (i = 0; i < n; i++)
  {
    if (p[i] != a[i])
    {
      char pcimg[CHAR_IMAGE_MAX_LEN] = "";
      char acimg[CHAR_IMAGE_MAX_LEN] = "";

      return cut_assertf(file, line, 0,
                         "\n  Proper at [%d]: 0x%02X (%d, %s)\n  Actual at [%d]: 0x%02X (%d, %s)%s%s",
                         (int) i, p[i], p[i], char_image(p[i], pcimg),
                         (int) i, a[i], a[i], char_image(a[i], acimg),
                         (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                         (extra_message == NULL) ? "" : extra_message);
    }
  }

  return cut_assertf(file, line, 1, "buffers of length %d (0x%02X) match%s%s", (int) n, (int) n,
                     (extra_message == NULL) ? "" : CUT_EXTRA_MESSAGE_PAD,
                     (extra_message == NULL) ? "" : extra_message);
}   /* cut_assert_memory() */

/* ------------------------------------------------------------------------- */
void cut_print_summary(FILE* file, cut_result_t result)
{
  size_t total_assertions = 0;
  size_t total_tests = 0;
  int i = 0;

  assert(NULL != file);
  assert((CUT_RESULT_FIRST <= result) && (result <= CUT_RESULT_LAST));

  fprintf(file, "%12s", "");

  for (i = CUT_RESULT_FIRST; i <= CUT_RESULT_LAST; i++)
  {
    fprintf(file, " %7s", cut_result_name[i]);
  }

  fprintf(file, " %8s", "Total");
  fprintf(file, "\n");

  fprintf(file, "%-12s", "Assertions");

  for (i = CUT_RESULT_FIRST; i <= CUT_RESULT_LAST; i++)
  {
    total_assertions += g_cut->assertions[i];
    fprintf(file, " %7d", g_cut->assertions[i]);
  }

  fprintf(file, " %8d\n", (int) total_assertions);

  fprintf(file, "%-12s", "Tests");

  for (i = CUT_RESULT_FIRST; i <= CUT_RESULT_LAST; i++)
  {
    total_tests += g_cut->tests[i];
    fprintf(file, " %7d", g_cut->tests[i]);
  }

  fprintf(file, " %8d\n", (int) total_tests);
  fprintf(file, "Result: %s\n", cut_result_name[result]);
}   /* cut_print_summary() */

/* ------------------------------------------------------------------------- */
static void cut_print_test_name(const char* name, struct tm* stamp)
{
  size_t i = 0;

  assert(g_cut);
  assert(name);
  assert(stamp);

  printf("%02u:%02u:%02u %s ", stamp->tm_hour, stamp->tm_min, stamp->tm_sec, name);
  for (i = strlen(name); i < 50; i++) printf(".");
  printf(" ");
  g_cut->test_name_hanging = 1;
  fflush(stdout);
}   /* cut_print_test_name() */

/* ------------------------------------------------------------------------- */
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
cut_result_t cut_run(int print_summary)
{
  cut_result_t run_result = CUT_RESULT_PASS;
  cut_suite_t* suite = NULL;

  assert(g_cut != NULL);

  memset(g_cut->assertions, 0, sizeof(g_cut->assertions));
  memset(g_cut->tests,      0, sizeof(g_cut->tests));

  for (suite = g_cut->suite; suite != NULL; suite = suite->next)
  {
    cut_test_t* test = NULL;

    g_cut->active_suite = suite;

    for (test = suite->test; test != NULL; test = test->next)
    {
      usec_t       start_time = 0;
      uint64_t     usec = 0;
      time_t       stamp_time;
      struct tm    stamp;
      cut_result_t result = CUT_RESULT_PASS;
      int          exclude_test = 0;

      assert(test);
      assert(test->name);

      exclude_test = (test->flags & CUT_TEST_FLAG_EXCLUDE) != 0;

      if (suite->data)
      {
        memset(suite->data, 0, suite->size);
      }

      stamp_time = time(NULL);
#if defined(_WIN32)
      localtime_s(&stamp, &stamp_time);
#elif defined(MINGW)
      stamp = *localtime(&stamp_time);
#else
      localtime_r(&stamp_time, &stamp);
#endif
      cut_print_test_name(test->name, &stamp);

      start_time = usec_time();

      if (suite->init && !exclude_test)
      {
        if (NULL != g_cut_wrap_init)
        {
          result = g_cut_wrap_init(suite->init, suite->data, g_cut_wrap_cookie);
        }
        else
        {
          result = suite->init(suite->data);
        }
      }

      /*
       * Only run test if init() succeeded.
       */
      if (CUT_RESULT_PASS == result)
      {
        if ((NULL == test->func) || exclude_test)
        {
          result = CUT_RESULT_SKIP;
        }
        else
        {
          g_cut->active_test = test;

          if (NULL != g_cut_wrap_test)
          {
            result = g_cut_wrap_test(test->func, suite->data, g_cut_wrap_cookie);
          }
          else
          {
            result = test->func(suite->data);
          }

          g_cut->active_test = NULL;
        }
      }

      if ((result < CUT_RESULT_FIRST) ||
          (result > CUT_RESULT_LAST))
      {
        result = CUT_RESULT_ERROR;
      }

      g_cut->tests[result]++;

      /*
       * Always run the finalization function if it exists.
       */
      if (suite->exit && !exclude_test)
      {
        if (NULL != g_cut_wrap_exit)
        {
          g_cut_wrap_exit(suite->exit, suite->data, g_cut_wrap_cookie);
        }
        else
        {
          suite->exit(suite->data);
        }
      }

      /*
       * If the test name was removed due to an assertion being printed, put
       * it back.
       */
      if (!g_cut->test_name_hanging)
      {
        cut_print_test_name(test->name, &stamp);
      }

      usec = usec_time() - start_time;
      printf("%-5s %02u:%02u.%06u\n", cut_result_name[result],
             (int) (usec / (60 * 1000000)), (int) ((usec / 1000000) % 60), (int) (usec % 1000000));
      g_cut->test_name_hanging = 0;
    }   /* for each test in the suite */
  }   /* for each suite */

  if (g_cut->tests[CUT_RESULT_ERROR] > 0)
  {
    run_result = CUT_RESULT_ERROR;
  }
  else if (g_cut->tests[CUT_RESULT_FAIL] > 0)
  {
    run_result = CUT_RESULT_FAIL;
  }
  else if (g_cut->tests[CUT_RESULT_PASS] > 0)
  {
    run_result = CUT_RESULT_PASS;
  }
  else
  {
    run_result = CUT_RESULT_SKIP;
  }

  if (print_summary)
  {
    printf("\n");
    cut_print_summary(stdout, run_result);
  }

  return run_result;
}   /* cut_run() */
