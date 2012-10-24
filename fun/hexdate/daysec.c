#include <stdio.h>      // For NULL.

#include "daysec.h"

#define SECONDS_PER_DAY  86400

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
                      int    time_zone_minutes_west,    // -720 .. +720
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
  if ((time_zone_minutes_west < -720) || (time_zone_minutes_west > +720)) return 0;
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

  seconds += 60.0 * (double) time_zone_minutes_west;

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
                      int    time_zone_minutes_west,    // -720 .. +720
                      int*   year,
                      int*   month,    // Jan=1 .. Dec=12
                      int*   day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int*   hour,     // 0 .. 23
                      int*   minute,   // 0 .. 59
                      int*   second)   // 0 .. 59
{
  if ((time_zone_minutes_west < -720) || (time_zone_minutes_west > +720)) return 0;
  //
  // I need to code this...
  //
  return 0;
}   // daysec_get_civil
