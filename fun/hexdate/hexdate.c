#include <stdio.h>
#include <stdlib.h>     /* For exit() */
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <math.h>

#include "daysec.h"
#include "hexon.h"

#define PROGRAM_NAME "hexdate"

#define SECONDS_PER_MINUTE  (0x3C)
#define MINUTES_PER_HOUR    (0x3C)
#define HOURS_PER_DAY       (0x18)
#define DAYS_PER_YEAR       (0x16D)      // Non-leapyear.

#define SECONDS_PER_HOUR    (SECONDS_PER_MINUTE * MINUTES_PER_HOUR)
#define SECONDS_PER_DAY     (SECONDS_PER_MINUTE * MINUTES_PER_HOUR * HOURS_PER_DAY)   // 0x15180.

#define DOUG_START_YEAR     (0x07AB)
#define DOUG_START_MONTH    (0x0C)        // 1-based.
#define DOUG_START_DAY      (0x1F)        // 1-based.
#define DOUG_START_HOUR     (0x05)
#define DOUG_START_MINUTE   (0x3A)
#define DOUG_START_SECOND   (0x3A)
#define DOUG_START_DAYS     (0x016C)      // 0-based.
#define DOUG_ZONE_SECONDS   (-5 * SECONDS_PER_HOUR)          // I was born during Eastern Standard Time.
#define DOUG_START_SECONDS  ((5 * SECONDS_PER_HOUR) + \
                             (58 * SECONDS_PER_MINUTE) -\
                             DOUG_ZONE_SECONDS)              // 0x09A38.

#define DOUG_DAYS_TO_1970     ((6 * DAYS_PER_YEAR) + 2)      // 0x0890, including leap days during 1964 and 1968.
#define DOUG_SECONDS_TO_1970  ((DOUG_DAYS_TO_1970 * SECONDS_PER_DAY) + \
                               (SECONDS_PER_DAY - DOUG_START_SECONDS))    // 0x0B4A8F48.

// These are NOT used below.
#define UNIX_START_YEAR     (0x07B2)
#define UNIX_START_DAY      (0x000)
#define UNIX_START_SECOND   (0x00000)

// These are NOT used below.
#define Y2K_START_YEAR      (0x07D0)
#define Y2K_START_DAY       (0x000)
#define Y2K_START_SECOND    (0x00000)

#define DEFAULT_EPOCH    "doug"
#define DEFAULT_FORMAT   "days"      // See hexon.c.
#define DEFAULT_VERBOSE  0

//
// These are set initially by calls in main().
//
struct DAYSEC_STRUCT epoch;
const char* output_format    = DEFAULT_FORMAT;
int         verbose = 0;

const char* on_off[] = { "off", "on" };

#define MAX_TIMES  0x40

const char* time_list[MAX_TIMES] = { 0 };
size_t      time_count = 0;

// ---------------------------------------------------------------------------
void usage(void)
{
  fprintf(stderr, "\n");
  fprintf(stderr, "Usage: " PROGRAM_NAME " [options] [time...]\n");
  fprintf(stderr, "\n");
  fprintf(stderr, PROGRAM_NAME " displays each [time] as a hex time in hex days and hexons.\n");
  fprintf(stderr, "[time] should by given in ISO 8601 form, YYYY-MM-DD[Thh:mm[:ss]].\n");
  fprintf(stderr, "If [time] is not given then the current time is converted and displayed.\n");
  fprintf(stderr, "There are 0x10000 hexons in a standard civil day, so each hexon is a little\n");
  fprintf(stderr, "longer in duration than a civil second. Note that there are no time zones in\n");
  fprintf(stderr, "hexon time.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Epoch times are calculated using the cyclic Gregorian calendar, without leap\n");
  fprintf(stderr, "seconds, synchronized to the year 2000.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  --help, -h        Show this usage information.\n");
  fprintf(stderr, "  --epoch=<epoch>   Use <epoch> for epoch [%s]:\n", DEFAULT_EPOCH);
  fprintf(stderr, "                      YYYY-MM-DDThh:mm:ss  Time of epoch, GMT ISO 8601\n");
  fprintf(stderr, "                      doug         Doug's birthday (1963-12-31T10:58:00 GMT)\n");
  fprintf(stderr, "                      unix         Unix zero time  (1970-01-01T00:00:00 GMT)\n");
  fprintf(stderr, "                      y2k          Year 2000       (2000-01-01T00:00:00 GMT)\n");
  fprintf(stderr, "  --format=<format> Display using <format> [%s]:\n", DEFAULT_FORMAT);
  fprintf(stderr, "                      days         DDDD.HHHH       (H=hexons in day).\n");
  fprintf(stderr, "                      days+        Like 'days', but with hexal display.\n");
  fprintf(stderr, "                      days/        Like 'days+', but diamond hexals.\n");
  fprintf(stderr, "                      hexons       HHHHHHHH.hhhh   (H=hexons from epoch).\n");
  fprintf(stderr, "                      hexons+      Like 'hexons', but with hexal display.\n");
  fprintf(stderr, "                      hexons/      Like 'hexons+', but diamond hexals.\n");
  fprintf(stderr, "  --verbose         Display verbose information [%s].\n", on_off[DEFAULT_VERBOSE != 0]);
  fprintf(stderr, "\n");
  exit(1);
}   // usage()

// ---------------------------------------------------------------------------
int strequ(const char* actual, const char* expected)
//
// Returns 1 if the actual matches the expected up to the length of the
// expected. Returns 0 if either is NULL, or if either is empty, or if there
// is a character mismatch before the length of the expected string is
// reached.
//
{
  if ((actual == NULL) || (expected == NULL)) return 0;
  if ((*actual == 0)   || (*expected == 0))   return 0;

  while (*actual && *expected)
  {
    if (*actual++ != *expected++) return 0;
  }

  if (*expected) return 0;    // There's still an unmatched character.
  return 1;
}   // strequ()

// ---------------------------------------------------------------------------
const char* null_check(const char* string)
{
  if (string == NULL) return "<<NULL>>";
  return string;
}   // null_check()

// ---------------------------------------------------------------------------
void set_epoch_civil_time(int time_zone_minutes_west, int year, int month, int day, int hour, int minute, int second)
{
  if (!daysec_set_civil(&epoch, time_zone_minutes_west, year, month, day, hour, minute, (double) second))
  {
    fprintf(stderr, PROGRAM_NAME ": *** error: bad epoch item range for %d.%u.%u/%02u:%02u:%02u (zone=%d minutes).\n",
            year, month, day, hour, minute, second, time_zone_minutes_west);
    exit (1);
  }


}   // set_epoch_civil_time()

// ---------------------------------------------------------------------------
void set_unix_time(DAYSEC time, const struct timeval* tv)
{
  if (!daysec_set_civil(time, 0 * MINUTES_PER_HOUR /* GMT */, 0x7B2, 0x1, 0x01, 0x00, 0x00, 0x00))
  {
    fprintf (stderr, PROGRAM_NAME ": *** internal error: could not convert constant Unix epoch!\n");
    exit (1);
  }

  daysec_add_seconds(time, (double) tv->tv_sec + (1.0E-6 * (double) tv->tv_usec));
}   // set_unix_time()

// ---------------------------------------------------------------------------
// @return 1 on success, 0 otherwise.
int daysec_from_text(DAYSEC daysec, const char* text)
{
  int year = 0;
  int month = 0;
  int day = 0;
  int hour = 0;
  int minute = 0;
  int second = 0;
  char dummy = 0;
  char separator = 0;

  if (NULL == text) return 0;
  else if (strcmp(text, "now") == 0)
  {
    struct timeval tv = { 0, 0 };
    gettimeofday(&tv, NULL);
    set_unix_time(daysec, &tv);
  }
  else if (strcmp(text, "doug") == 0) return daysec_set_civil(daysec, 5 * MINUTES_PER_HOUR /* EST */,
                                                              0x7AB, 0xC, 0x1F, 0x05, 0x3A, 0x00);
  else if (strcmp(text, "unix") == 0) return daysec_set_civil(daysec, 0 * MINUTES_PER_HOUR /* GMT */,
                                                              0x7B2, 0x1, 0x01, 0x00, 0x00, 0x00);
  else if (strcmp(text, "y2k")  == 0) return daysec_set_civil(daysec, 0 * MINUTES_PER_HOUR /* GMT */,
                                                              0x7D0, 0x1, 0x01, 0x00, 0x00, 0x00);
  else if ((sscanf(text, "%d-%u-%u%c%u:%u:%u%c",
                   &year, &month, &day, &separator, &hour, &minute, &second, &dummy) == 7) &&
           ((separator == 'T') || (separator == 't') || (separator == ' ')))
  {
    return daysec_set_civil(daysec, 0 * MINUTES_PER_HOUR /* GMT */, year, month, day, hour, minute, second);
  }
  else if ((sscanf(text, "%d-%u-%uT%u:%u%c", &year, &month, &day, &hour, &minute, &dummy) == 6) &&
           ((separator == 'T') || (separator == 't') || (separator == ' ')))
  {
    return daysec_set_civil(daysec, 0 * MINUTES_PER_HOUR /* GMT */, year, month, day, hour, minute, 0);
  }
  else if (sscanf(text, "%d-%u-%u%c", &year, &month, &day, &dummy) == 3)
  {
    return daysec_set_civil(daysec, 0 * MINUTES_PER_HOUR /* GMT */, year, month, day, 0, 0, 0);
  }
  else
  {
    return 0;
  }

  return 1;
}   // daysec_from_text()

// ---------------------------------------------------------------------------
void set_epoch(const char* epoch)
{
  int year = 0;
  int month = 0;
  int day = 0;
  int hour = 0;
  int minute = 0;
  int second = 0;
  char dummy;

  if (epoch == NULL) goto Exception;
  else if (strcmp(epoch, "doug") == 0) set_epoch_civil_time(5 * MINUTES_PER_HOUR /* EST */,
                                                            0x7AB, 0xC, 0x1F, 0x05, 0x3A, 0x00);
  else if (strcmp(epoch, "unix") == 0) set_epoch_civil_time(0 * MINUTES_PER_HOUR /* GMT */,
                                                            0x7B2, 0x1, 0x01, 0x00, 0x00, 0x00);
  else if (strcmp(epoch, "y2k")  == 0) set_epoch_civil_time(0 * MINUTES_PER_HOUR /* GMT */,
                                                            0x7D0, 0x1, 0x01, 0x00, 0x00, 0x00);
  else if (sscanf(epoch, "%d-%u-%uT%u:%u:%u%c", &year, &month, &day, &hour, &minute, &second, &dummy) == 6)
  {
    set_epoch_civil_time(0 * MINUTES_PER_HOUR /* GMT */, year, month, day, hour, minute, second);
  }
  else if (sscanf(epoch, "%d-%u-%uT%u:%u%c", &year, &month, &day, &hour, &minute, &dummy) == 5)
  {
    set_epoch_civil_time(0 * MINUTES_PER_HOUR /* GMT */, year, month, day, hour, minute, 0);
  }
  else if (sscanf(epoch, "%d-%u-%u%c", &year, &month, &day, &dummy) == 3)
  {
    set_epoch_civil_time(0 * MINUTES_PER_HOUR /* GMT */, year, month, day, 0, 0, 0);
  }
  else
  {
    goto Exception;
  }

  return;

Exception:
  fprintf(stderr, PROGRAM_NAME ": *** error: I can't handle an epoch of \"%s\"; use --help.\n", null_check(epoch));
  exit (1);
}   // set_epoch()

// ---------------------------------------------------------------------------
void set_format(const char* format)
{
  // Eventually I may add something like "%4D.%4H.%4h".
  if (format == NULL) goto Exception;
  else if (strcmp (format, "days"   ) == 0) output_format = format;
  else if (strcmp (format, "days+"  ) == 0) output_format = format;
  else if (strcmp (format, "days/"  ) == 0) output_format = format;
  else if (strcmp (format, "hexons" ) == 0) output_format = format;
  else if (strcmp (format, "hexons+") == 0) output_format = format;
  else if (strcmp (format, "hexons/") == 0) output_format = format;
  else goto Exception;

  return;

Exception:
  fprintf(stderr, PROGRAM_NAME ": *** error: I can't handle a format of \"%s\".\n", null_check(format));
  exit(1);
}   // set_format()

// ---------------------------------------------------------------------------
int main(int argc, char* argv[])
{
  int i;
  struct timeval           time;
  struct HEXON_TIME_STRUCT hexon;
  struct DAYSEC_STRUCT     daysec;

  gettimeofday(&time, NULL);
  set_epoch(DEFAULT_EPOCH);
  set_format(DEFAULT_FORMAT);

  for (i = 1; i < argc; i++)
  {
    char* argument = argv[i];
    if ((strcmp(argument, "--help") == 0) || (strcmp (argument, "-h") == 0)) usage();
    else if (strequ(argument, "--epoch=" )) set_epoch(strchr(argument, '=') + 1);
    else if (strequ(argument, "--format=")) set_format(strchr(argument, '=') + 1);
    else if (strequ(argument, "--verbose")) verbose = 1;
    else if ('-' == argument[0])
    {
      fprintf(stderr, PROGRAM_NAME ": *** error: unknown option \"%s\".\n\n", argument);
      usage();
    }
    else
    {
      if (time_count >= MAX_TIMES)
      {
        fprintf(stderr, PROGRAM_NAME ": limited to %d explicit times on command line; continuing.\n", MAX_TIMES);
        break;
      }

      time_list[time_count++] = argument;
    }
  }

  /*
   * If no time is specified, use now.
   */
  if (0 == time_count)
  {
    time_list[time_count++] = "now";
  }

  /*
   * Go through the list of times to check.
   */
  for (i = 0; i < time_count; i++)
  {
    if (!daysec_from_text(&daysec, time_list[i]))
    {
      fprintf(stderr, PROGRAM_NAME ": *** error: invalid time \"%s\".\n", null_check(time_list[i]));
      usage();
    }

    if (verbose)
    {
      printf("Unix time    (seconds.microseconds): %llu.%06lu (hex 0x%llx/0x%05lx.\n",
             (long long) time.tv_sec, (long) time.tv_usec, (long long) time.tv_sec, (long) time.tv_usec);
      printf("Unix daysec  (days.seconds): %lu.%05lu (hex 0x%lx/0x%05lx.\n",
             daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);
      printf("Epoch daysec (days.seconds): %lu.%05lu (hex 0x%lx/0x%05lx.\n",
             epoch.day, (long) epoch.sec, epoch.day, (long) epoch.sec);
    }

    daysec_sub(&daysec, &daysec, &epoch);

    if (verbose)
    {
      printf("Daysec since epoch  (days.seconds): %lu.%05lu (hex 0x%lx/0x%05lx.\n",
             daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);
    }

    Hexon_Set_Day_Second(&hexon, daysec.day, daysec.sec);

    if (verbose)
    {
      printf("Decimal hexon = %20.6f.\n", hexon.hexon);
    }

    if (!Hexon_Print(stdout, output_format, &hexon))
    {
      fprintf(stderr, PROGRAM_NAME ": *** error: could not print using format \"%s\".\n", null_check(output_format));
      return 1;
    }
  }   /* For each time to check */

  return 0;
}   // main
