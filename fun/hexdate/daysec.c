#ifdef WIN32
#include <Windows.h>
#pragma warning(disable: 4996)  // *sigh* VS2013 warns even with the #define's below.
#define snprintf _snprintf
// I don't use sscanf_s() because it requires a buffer size parameter for
// 'c' and 's' formats.
#endif

#include <assert.h>
#include <ctype.h>
#include <stdio.h>      // For NULL.
#include <string.h>
#include <time.h>

#ifndef WIN32
#include <sys/time.h>
#endif

#include "daysec.h"

extern int g_verbose;   // This is in hexdate.c. @todo(dr) Put this elsewhere.

#define SECONDS_PER_DAY  86400
#define ZONE_SECONDS_MIN (-12*60*60)
#define ZONE_SECONDS_MAX (+12*60*60)

#define dPrint(...) \
  if (g_verbose) {                              \
    printf("%s:%u: ", __FILE__, __LINE__);      \
    printf(__VA_ARGS__);                        \
  } while (0)

typedef void (*DAYSEC_BINARY_OPERATION)(DAYSEC result, const DAYSEC left, const DAYSEC right);

// ---------------------------------------------------------------------------
void daysec_add (DAYSEC sum,  const DAYSEC left, const DAYSEC right)
{
  signed long day;
  double      sec;

  if (sum == NULL) return;

  day = left->day + right->day;
  sec = left->sec + right->sec;

  if (sec >= SECONDS_PER_DAY)
  {
    day++;
    sec -= SECONDS_PER_DAY;
  }

  sum->day = day;
  sum->sec = sec;
}   // daysec_add

// ---------------------------------------------------------------------------
void daysec_sub (DAYSEC diff, const DAYSEC left, const DAYSEC right)
{
  signed long day;
  double      sec;

  if (diff == NULL) return;

  day = left->day - right->day;
  sec = left->sec - right->sec;

  if (sec < 0.0)
  {
    day--;
    sec += SECONDS_PER_DAY;
  }

  diff->day = day;
  diff->sec = sec;
}   // daysec_sub

// ---------------------------------------------------------------------------
void daysec_add_seconds (DAYSEC time, double seconds)
{
  struct DAYSEC_STRUCT    operand;
  DAYSEC_BINARY_OPERATION operation = daysec_add;

  if (seconds < 0)
  {
    seconds = -seconds;
    operation = daysec_sub;
  }

  operand.day = (long) (seconds / (double) SECONDS_PER_DAY);
  operand.sec = seconds - (SECONDS_PER_DAY * operand.day);

  operation (time, time, &operand);
}   // daysec_add_seconds

// ---------------------------------------------------------------------------
int daysec_set_civil (DAYSEC time,
                      int    time_zone_seconds_west,    // -720 .. +720
                      int    year,
                      int    month,    // Jan=1 .. Dec=12
                      int    day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int    hour,     // 0 .. 23
                      int    minute,   // 0 .. 59
                      int    second)   // 0 .. 59
{
#define DAYS_PER_400_YEARS   ((400 * 365) + 97)          // 0x23ab1 (146097).
  double      seconds;
  signed long days;
  int         is_leap_year;

  // Days up to the first of ... Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
  const int days_in_month[]  = {  31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 };
  const int days_into_year[] = {   0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334 };

  if (time == NULL) return 0;
  if ((time_zone_seconds_west < ZONE_SECONDS_MIN) || (time_zone_seconds_west > ZONE_SECONDS_MAX)) return 0;
  if ((month  < 1) || (month  > 12)) return 0;
  if ((hour   < 0) || (hour   > 23)) return 0;
  if ((minute < 0) || (minute > 59)) return 0;
  if ((second < 0) || (second > 59)) return 0;
  if ((day    < 1)) return 0;

  if ((year % 4) != 0)
  {
    is_leap_year = 0;
  }
  else if ((year % 100) != 0)
  {
    is_leap_year = 1;
  }
  else if ((year % 400) != 0)
  {
    is_leap_year = 0;
  }
  else   // Divisible by 400:
  {
    is_leap_year = 1;
  }

  if (is_leap_year && (month == 2) && (day > 29)) return 0;
  else if (day > days_in_month[month-1])          return 0;

  days = days_into_year[month-1];

  if (is_leap_year && (month > 2))
  {
    days++;
  }

  dPrint("%d-%02u-%02uT%02u:%02u:%02u %7d (leap=%s)\n",
         year, month, day, hour, minute, second, time_zone_seconds_west,
         is_leap_year ? "yes" : "no");

  days += ((year / 400) * DAYS_PER_400_YEARS);

  year %= 400;

  days += year * 365;

  days += (year+3) /   4;      // This will incorrectly include leap days in year 100, 200, and 300.

  if (year > 100)
  {
    days -= (year-1) / 100;    // This will take them back out.
  }

  days += day - 1;

  seconds  = (3600.0 * (double) hour) +
             (  60.0 * (double) minute) +
             (   1.0 * (double) second);

  seconds += (double) time_zone_seconds_west;

  if (seconds < 0.0)
  {
    days--;
    seconds += SECONDS_PER_DAY;
  }
  else if (seconds > SECONDS_PER_DAY)
  {
    days++;
    seconds -= SECONDS_PER_DAY;
  }

  time->day = days;
  time->sec = seconds;
  return 1;
}   // daysec_set_civil

// ---------------------------------------------------------------------------
int daysec_get_civil (DAYSEC time,
                      int    time_zone_seconds_west,    // -12h .. +12h.
                      int*   year,
                      int*   month,    // Jan=1 .. Dec=12
                      int*   day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int*   hour,     // 0 .. 23
                      int*   minute,   // 0 .. 59
                      int*   second)   // 0 .. 59
{
  if ((time_zone_seconds_west < ZONE_SECONDS_MIN) || (time_zone_seconds_west > ZONE_SECONDS_MAX)) return 0;
  //
  // I need to code this...
  //
  return 0;
}   // daysec_get_civil

/* ------------------------------------------------------------------------- */
static void tm_get_fields(const struct tm* tm,
                          int* year, int* month, int* day,
                          int* hour, int* minute, int* second)
{
  assert(NULL != tm);
  assert(NULL != year);
  assert(NULL != month);
  assert(NULL != day);
  assert(NULL != hour);
  assert(NULL != minute);
  assert(NULL != second);

  *year = tm->tm_year + 1900;
  *month = tm->tm_mon + 1;
  *day = tm->tm_mday;
  *hour = tm->tm_hour;
  *minute = tm->tm_min;
  *second = tm->tm_sec;
}   /* tm_get_fields() */

/* ------------------------------------------------------------------------- */

// ---------------------------------------------------------------------------
// @return 1 on success, 0 otherwise.
int daysec_from_text(DAYSEC daysec,
                     const char* text,
                     const DAYSEC_SHORTCUT* shortcuts)
{
  if (NULL == text)
  {
    return 0;
  }

  dPrint("text=\"%s\"\n", text);
  int year = 0;
  int month = 0;
  int day = 0;
  int hour = 0;
  int minute = 0;
  int second = 0;
  int time_zone_seconds = 0;   /* West. */
  char zone = 0;
  char dummy = 0;
  char separator = 0;

  // First see if there's a shortcut. No shortcut nesting allowed!
  if (NULL != shortcuts)
  {
    for (; (NULL != shortcuts->name) && (NULL != shortcuts->value); ++shortcuts)
    {
      if (0 == strcmp(shortcuts->name, text))
      {
        text = shortcuts->value;
        break;
      }
    }
  }

  dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
         year, month, day, hour, minute, second, time_zone_seconds, text);

  // Set fields to local time first.
  struct tm tm = {0};
  time_t now = 0;
  time(&now);
#ifdef WIN32
  localtime_s(&tm, &now);
  _get_timezone(&time_zone_seconds);
  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
#else
  if (NULL != localtime_r(&now, &tm))
  {
      time_zone_seconds = -(int)tm.tm_gmtoff;
      tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  }
#endif

  // Special shortcut for current time.
  if (0 == strcmp("now", text))
  {
    // TODO(dr) Not thread-safe! Fix me!
    static char now_time[0x80] = "";
    snprintf(now_time, sizeof(now_time), "%04u-%02u-%02uT%02u:%02u:%02u", year, month, day, hour, minute, second);
    text = now_time;
  }

  dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
         year, month, day, hour, minute, second, time_zone_seconds, text);

  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  if ((sscanf(text, "%d-%u-%u%c%u:%u:%u%c%c",
              &year, &month, &day, &separator, &hour, &minute, &second, &zone, &dummy) >= 7) &&
      ((separator == 'T') || (separator == 't') || (separator == ' ') || (separator == ',') || (separator == ';')))
  {
    dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
           year, month, day, hour, minute, second, time_zone_seconds, text);
    time_zone_seconds = (tolower(zone) == 'z') ? 0 : time_zone_seconds;
    return daysec_set_civil(daysec, time_zone_seconds, year, month, day, hour, minute, second);
  }

  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  if ((sscanf(text, "%d-%u-%u%c%u:%u:%u%c%c",
              &year, &month, &day, &separator, &hour, &minute, &second, &zone, &dummy) >= 6) &&
      ((separator == 'T') || (separator == 't') || (separator == ' ')))
  {
    dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
           year, month, day, hour, minute, second, time_zone_seconds, text);
    time_zone_seconds = (tolower(zone) == 'z') ? 0 : time_zone_seconds;
    return daysec_set_civil(daysec, time_zone_seconds, year, month, day, hour, minute, second);
  }

  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  if (sscanf(text, "%u:%u:%u%c%c", &hour, &minute, &second, &zone, &dummy) >= 2)
  {
    dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
           year, month, day, hour, minute, second, time_zone_seconds, text);
    time_zone_seconds = (tolower(zone) == 'z') ? 0 : time_zone_seconds;
    return daysec_set_civil(daysec, time_zone_seconds, year, month, day, hour, minute, second);
  }

  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  if (sscanf(text, "%u:%u%c%c", &hour, &minute, &zone, &dummy) >= 2)
  {
    dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
           year, month, day, hour, minute, second, time_zone_seconds, text);
    time_zone_seconds = (tolower(zone) == 'z') ? 0 : time_zone_seconds;
    return daysec_set_civil(daysec, time_zone_seconds, year, month, day, hour, minute, second);
  }

  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  if (sscanf(text, "%d-%u-%u%c", &year, &month, &day, &dummy) == 3)
  {
    dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
           year, month, day, hour, minute, second, time_zone_seconds, text);
    return daysec_set_civil(daysec, 0 /* GMT */, year, month, day, hour, minute, second);
  }

  tm_get_fields(&tm, &year, &month, &day, &hour, &minute, &second);
  if (sscanf(text, "%u-%u%c", &month, &day, &dummy) == 2)
  {
    dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
           year, month, day, hour, minute, second, time_zone_seconds, text);
    return daysec_set_civil(daysec, 0 /* GMT */, year, month, day, hour, minute, second);
  }

  dPrint("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"\n",
         year, month, day, hour, minute, second, time_zone_seconds, text);
  return 0;
}   // daysec_from_text()
