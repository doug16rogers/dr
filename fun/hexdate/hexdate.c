// Copyright (c) 2014 Doug Rogers under the terms of the MIT License.
// See http://www.opensource.org/licenses/mit-license.html..
// $Id$

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef WIN32
#include <Windows.h>
#include <time.h>
#else
#include <sys/time.h>
#include <unistd.h>
#endif
#include <math.h>

#include "daysec.h"
#include "ezlog.h"
#include "hexon.h"

#define PROGRAM_NAME "hexdate"

#define kSecondsPerMinute   (0x3C)
#define kMinutesPerHour     (0x3C)
#define kHoursPerDay        (0x18)
#define kDaysPerYear        (0x16D)      // Non-leapyear.

#define kSecondsPerHour     (kSecondsPerMinute * kMinutesPerHour)
#define kSecondsPerDay      (kSecondsPerMinute * kMinutesPerHour * kHoursPerDay)   // 0x15180.

#define kDefaultEpoch   "doug"
#define kDefaultFormat  "days"
#define kDefaultVerbose 0

const DAYSEC_SHORTCUT kDateShortcuts[] = {
  {"doug",  "1963-12-31T10:58:00Z"},
  {"unix",  "1970-01-01T00:00:00Z"},
  {"y2k",   "2000-01-01T00:00:00Z"},
  {NULL, NULL}
};

//
// These are set initially by calls in main().
//
struct DAYSEC_STRUCT epoch;
const char* output_format = kDefaultFormat;

const char* on_off[] = {"off", "on"};

#define kMaxTimes  0x100

const char* time_list[kMaxTimes] = {0};
size_t      time_count = 0;

// ---------------------------------------------------------------------------
void Usage(FILE* file) {
  const DAYSEC_SHORTCUT* dss = NULL;
  fprintf(file, "\n");
  fprintf(file, "Usage: " PROGRAM_NAME " [options] [time...]\n");
  fprintf(file, "\n");
  fprintf(file, PROGRAM_NAME " displays each [time] as a hex time in hex days and hexons.\n");
  fprintf(file, "There are 0x10000 hexons in a standard civil day, so each hexon is a little\n");
  fprintf(file, "longer in duration than a civil second. Note that there are no time zones in\n");
  fprintf(file, "hexon time.\n");
  fprintf(file, "\n");
  fprintf(file, "[time] should by given in ISO 8601 form, [[YY]YY-]MM-DD[Thh:mm[:ss][Z]]. By\n");
  fprintf(file, "default the local time zone is used. Append 'Z' to specify UTC. If [time]\n");
  fprintf(file, "includes just the time of day, hh:mm[:ss][Z], then the current date is used. If\n");
  fprintf(file, "[time] includes just a date then the current time of day will be used for the\n");
  fprintf(file, "time of day portion of [time].\n");
  fprintf(file, "\n");
  fprintf(file, "If [time] is not given then the current time is converted and displayed.\n");
  fprintf(file, "\n");
  fprintf(file, "Epoch times are calculated using the cyclic Gregorian calendar, without leap\n");
  fprintf(file, "seconds, synchronized to the year 2000.\n");
  fprintf(file, "\n");
  fprintf(file, "Options:\n");
  fprintf(file, "  --help, -h        Show this usage information.\n");
  fprintf(file, "  --epoch=<epoch>   Use <epoch> for epoch [%s]:\n", kDefaultEpoch);
  fprintf(file, "                      [YYYY-MM-DDT]hh:mm[:ss][Z]  Time of epoch, ISO 8601; Z=UTC\n");
  for (dss = &kDateShortcuts[0]; dss->name != NULL; ++dss) {
    fprintf(file, "                      %-6s (%s)\n", dss->name, dss->value);
  }
  fprintf(file, "  --format=<format> Display using <format> [%s]:\n", kDefaultFormat);
  fprintf(file, "                      days         DDDD.HHHH       (H=hexons in day).\n");
  fprintf(file, "                      days+        Like 'days', but with hexal display.\n");
  fprintf(file, "                      days/        Like 'days+', but diamond hexals.\n");
  fprintf(file, "                      hexons       HHHHHHHH.hhhh   (H=hexons from epoch).\n");
  fprintf(file, "                      hexons+      Like 'hexons', but with hexal display.\n");
  fprintf(file, "                      hexons/      Like 'hexons+', but diamond hexals.\n");
  fprintf(file, "  --verbose         Display verbose information [%s].\n", on_off[kDefaultVerbose != 0]);
  fprintf(file, "\n");
  exit(1);
}   // Usage()

// ---------------------------------------------------------------------------
void UsageReference(FILE* file) {
  fprintf(file, "Use '%s --help' for usage information.\n", PROGRAM_NAME);
  exit(1);
}   /* UsageReference() */

// ---------------------------------------------------------------------------
//
// Returns 1 if the actual matches the expected up to the length of the
// expected. Returns 0 if either is NULL, or if either is empty, or if there
// is a character mismatch before the length of the expected string is
// reached.
//
int strequ(const char* actual, const char* expected) {
  if ((actual == NULL) || (expected == NULL)) return 0;
  if ((*actual == 0)   || (*expected == 0))   return 0;

  while (*actual && *expected) {
    if (*actual++ != *expected++) return 0;
  }

  if (*expected) return 0;    // There's still an unmatched character.
  return 1;
}   // strequ()

// ---------------------------------------------------------------------------
const char* NullCheck(const char* string) {
  if (string == NULL) return "<<NULL>>";
  return string;
}   // NullCheck()

// ---------------------------------------------------------------------------
void SetUnixTime(DAYSEC time, const struct timeval* tv) {
  if (!daysec_set_civil(time, 0 * kSecondsPerHour /* GMT */, 0x7B2, 0x1, 0x01, 0x00, 0x00, 0x00)) {
    fprintf (stderr, PROGRAM_NAME ": *** internal error: could not convert constant Unix epoch!\n");
    exit (1);
  }

  daysec_add_seconds(time, (double) tv->tv_sec + (1.0E-6 * (double) tv->tv_usec));
}   // SetUnixTime()

// ---------------------------------------------------------------------------
void SetEpoch(const char* epoch_text) {
  if (!daysec_from_text(&epoch, epoch_text, kDateShortcuts)) {
    fprintf(stderr, PROGRAM_NAME ": *** error: invalid time for epoch \"%s\".\n", NullCheck(epoch_text));
    UsageReference(stderr);
  }
}   // SetEpoch()

// ---------------------------------------------------------------------------
void SetFormat(const char* format) {
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
  fprintf(stderr, PROGRAM_NAME ": *** error: unknown format \"%s\".\n", NullCheck(format));
  exit(1);
}   // SetFormat()

// ---------------------------------------------------------------------------
int main(int argc, char* argv[]) {
  size_t i = 0;
  struct HEXON_TIME_STRUCT hexon;
  struct DAYSEC_STRUCT     daysec;

  SetEpoch(kDefaultEpoch);
  SetFormat(kDefaultFormat);

  for (i = 1; i < (size_t) argc; i++) {
    char* argument = argv[i];
    if ((strcmp(argument, "--help") == 0) || (strcmp (argument, "-h") == 0)) Usage(stdout);
    else if (strequ(argument, "--epoch=" )) SetEpoch(strchr(argument, '=') + 1);
    else if (strequ(argument, "--format=")) SetFormat(strchr(argument, '=') + 1);
    else if (strequ(argument, "--verbose")) ezlog_set_level(NULL, EZLOG_LEVEL_DEBUG);
    else if ('-' == argument[0]) {
      fprintf(stderr, PROGRAM_NAME ": *** error: unknown option \"%s\".\n\n", argument);
      Usage(stderr);
    } else {
      if (time_count >= kMaxTimes) {
        fprintf(stderr, PROGRAM_NAME ": limited to %d explicit times on command line; continuing.\n", kMaxTimes);
        break;
      }
      time_list[time_count++] = argument;
    }
  }

  /*
   * If no time is specified, use the special shortcut "now".
   */
  if (0 == time_count) {
    time_list[time_count++] = "now";
  }

  /*
   * Go through the list of times to check.
   */
  for (i = 0; i < time_count; i++) {
    if (!daysec_from_text(&daysec, time_list[i], kDateShortcuts)) {
      fprintf(stderr, PROGRAM_NAME ": *** error: invalid time \"%s\".\n", NullCheck(time_list[i]));
      UsageReference(stderr);
    }
    EZLOGD("converting time: %s", time_list[i]);
    EZLOGD("days.seconds source time : %10lu.%05lu (hex 0x%lx/0x%05lx.",
           daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);
    EZLOGD("days.seconds epoch time  : %10lu.%05lu (hex 0x%lx/0x%05lx.",
           epoch.day, (long) epoch.sec, epoch.day, (long) epoch.sec);

    daysec_sub(&daysec, &daysec, &epoch);

    EZLOGD("days.seconds since epoch : %10lu.%05lu (hex 0x%lx/0x%05lx.",
           daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);

    Hexon_Set_Day_Second(&hexon, daysec.day, daysec.sec);

    EZLOGD("decimal hexon (double): %20.6f.", hexon.hexon);

    if (!Hexon_Print(stdout, output_format, &hexon)) {
      fprintf(stderr, PROGRAM_NAME ": *** error: could not print using format \"%s\".\n", NullCheck(output_format));
      return 1;
    }
  }   /* For each time to check */

  return 0;
}   // main
