/* Copyright (c) 2010-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef WIN32
#include <Windows.h>
#include <time.h>
#else
#include <time.h>
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

int g_newline = 1;

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
  fprintf(file, "[time] may be given in ISO 8601 form, '[[YY]YY-]MM-DD[Thh:mm[:ss][Z]]'. By\n");
  fprintf(file, "default the local time zone is used. Append 'Z' to specify UTC. If [time]\n");
  fprintf(file, "includes just the time of day, hh:mm[:ss][Z], then the current date is used. If\n");
  fprintf(file, "[time] includes just a date then the current time of day will be used for the\n");
  fprintf(file, "time of day portion of [time].\n");
  fprintf(file, "\n");
  fprintf(file, "[time] may be in the form 'XXXX[.XXXX][Z]', where X are hexadecimal digits, to\n");
  fprintf(file, "specify a hex time to convert to standard civil time for the specified epoch.\n");
  fprintf(file, "If [time] ends in 'Z' then the time displayed will be in UTC, otherwise it will\n");
  fprintf(file, "be in the current time zone.\n");
  fprintf(file, "\n");
  fprintf(file, "If [time] is not given then the current time is converted and displayed.\n");
  fprintf(file, "\n");
  fprintf(file, "Epoch times are calculated using the cyclic Gregorian calendar, without leap\n");
  fprintf(file, "seconds, synchronized to the year 2000.\n");
  fprintf(file, "\n");
  fprintf(file, "Options:\n");
  fprintf(file, "  --help, -h        Show this usage information.\n");
  fprintf(file, "  -n                Do not print a newline with single-line formats.\n");
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
static void HandleDaysecToHexon(struct DAYSEC_STRUCT *daysec, const char *time_text) {
  struct HEXON_TIME_STRUCT hexon;

  EZLOGD("converting text time: %s", time_text);
  EZLOGD("days.seconds source time : %10lu.%05lu (hex 0x%lx/0x%05lx.",
         daysec->day, (long) daysec->sec, daysec->day, (long) daysec->sec);
  EZLOGD("days.seconds epoch time  : %10lu.%05lu (hex 0x%lx/0x%05lx.",
         epoch.day, (long) epoch.sec, epoch.day, (long) epoch.sec);

  daysec_sub(daysec, daysec, &epoch);

  EZLOGD("days.seconds since epoch : %10lu.%05lu (hex 0x%lx/0x%05lx.",
         daysec->day, (long) daysec->sec, daysec->day, (long) daysec->sec);

  Hexon_Set_Day_Second(&hexon, daysec->day, daysec->sec);

  EZLOGD("decimal hexon (double): %20.6f.", hexon.hexon);

  if (!Hexon_Print(stdout, output_format, g_newline, &hexon)) {
    fprintf(stderr, PROGRAM_NAME ": *** error: could not print using format \"%s\".\n", NullCheck(output_format));
    exit(1);
  }
}   // HandleDaysecToHexon()

// ---------------------------------------------------------------------------
static int HexonFromText(struct HEXON_TIME_STRUCT *hexon, int *zone_seconds, const char *time_text) {
  long days = 0;
  char *end = NULL;
  double hexons = 0.0;

  days = strtol(time_text, &end, 16);

  if ((days < 0) || (end == time_text) || ((*end != '\0') && (NULL == strchr(".zZ", *end)))) {
    return 0;
  }

  hexons = 65536.0 * days;

  if ((end[0] == '.') && (end[1] != '\0')) {
    int digits = 0;
    double day_fraction_denominator = 1.0;
    long day_fraction_numerator = 0;
    digits = strlen(end + 1);
    day_fraction_numerator = strtol(end + 1, &end, 16);

    if ((*end != '\0') && (*end != 'z') && (*end != 'Z')) {
      return 0;
    }

    // 4 digits for hexons, the rest are fractional hexons.
    day_fraction_denominator = exp(log(16.0) * digits);
    hexons += 65536.0 * day_fraction_numerator / day_fraction_denominator;
  }

  if (*end == 'z' || *end == 'Z') {
    *zone_seconds = 0;
  } else {
    struct tm tm;
    time_t time_now = time(NULL);
#ifdef WIN32
    localtime_s(&tm, time_now);
    _get_timezone(zone_seconds);
#else
    if (NULL != localtime_r(&time_now, &tm)) {
      *zone_seconds = (int) tm.tm_gmtoff;
    }
#endif
  }

  hexon->hexon = hexons;
  return 1;
}  // HexonFromText()

// ---------------------------------------------------------------------------
static void HandleHexonToDate(struct HEXON_TIME_STRUCT *hexon, int zone_seconds, const char *time_text) {
  struct DAYSEC_STRUCT daysec;
  int year = 0;
  int month = 0;
  int day = 0;
  int hour = 0;
  int minute = 0;
  int second = 0;
  EZLOGD("converting text days.hexons: %s", time_text);
  EZLOGD("decimal hexon (double): %20.6f.", hexon->hexon);
  daysec.day = hexon->hexon / 65536.0;
  daysec.sec = (hexon->hexon - (65536.0 * daysec.day)) * 86400.0 / 65536.0;
  EZLOGD("days.seconds since epoch : %10lu.%05lu (hex 0x%lx/0x%05lx.",
         (long) daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);
  EZLOGD("days.seconds epoch time  : %10lu.%05lu (hex 0x%lx/0x%05lx.",
         (long) epoch.day, (long) epoch.sec, epoch.day, (long) epoch.sec);
  daysec_add(&daysec, &daysec, &epoch);
  EZLOGD("days.seconds target time : %10lu.%05lu (hex 0x%lx/0x%05lx.",
         (long) daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);

  if (!daysec_get_civil(&daysec, zone_seconds, &year, &month, &day, &hour, &minute, &second)) {
    fprintf(stderr, "Failed to convert day.sec %d.%05ld to civil time.\n", daysec.day, (long) daysec.sec);
    exit(1);
  }

  printf("%04d-%02d-%02dT%02d:%02d:%02d%s\n", year, month, day, hour, minute, second, zone_seconds ? "" : "Z");
}   // HandleDaysecToHexon()

// ---------------------------------------------------------------------------
int main(int argc, char* argv[]) {
  size_t i = 0;
  struct HEXON_TIME_STRUCT hexon;
  struct DAYSEC_STRUCT     daysec;
  int zone_seconds = 0;

  SetEpoch(kDefaultEpoch);
  SetFormat(kDefaultFormat);

  for (i = 1; i < (size_t) argc; i++) {
    char* argument = argv[i];
    if ((strcmp(argument, "--help") == 0) || (strcmp (argument, "-h") == 0)) Usage(stdout);
    else if (strequ(argument, "-n" )) g_newline = 0;
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
    if (daysec_from_text(&daysec, time_list[i], kDateShortcuts)) {
      HandleDaysecToHexon(&daysec, time_list[i]);
    } else if (HexonFromText(&hexon, &zone_seconds, time_list[i])) {
      HandleHexonToDate(&hexon, zone_seconds, time_list[i]);
    } else {
      fprintf(stderr, PROGRAM_NAME ": *** error: could not convert time \"%s\".\n",
              NullCheck(time_list[i]));
      UsageReference(stderr);
      return 1;
    }
  }   /* For each time to check */

  return 0;
}   // main
