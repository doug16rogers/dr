#ifndef __day_sec_hpp__
#define __day_sec_hpp__
// ---------------------------------------------------------------------------
// This object provides routines to manipulate a day-second pair. 'day' is
// the number of days since year 0, assuming that the Gregorian calendar is
// applied backwards from the year 2000. 'sec' is the number of seconds into
// the day.
//

#include <sys/time.h>      // For struct timeval and struct timezone.

// ---------------------------------------------------------------------------
class Day_Sec
{
public:
  Day_Sec() :
    day (0), sec (0.0)
  {
  }
  
public:
  Day_Sec(int time_zone_minutes_west,    // -720 .. +720
          int year,
          int month,        // Jan=1 .. Dec=12
          int day,          // 1 .. 28/29/30/31 (depending on year/month)
          int hour   = 0,   // 0 .. 23
          int minute = 0,   // 0 .. 59
          int second = 0)   // 0 .. 59
    : day (0), sec (0.0)
  {
    set_civil (time_zone_minutes_west, year, month, day, hour, minute, second);
  }
  
public:
  Day_Sec& add (double seconds);
  Day_Sec& sub (double seconds);

public:
  Day_Sec& operator + (double seconds) { return add (seconds); }
  Day_Sec& operator - (double seconds) { return sub (seconds); }

public:
  Day_Sec& add (Day_Sec& right);
  Day_Sec& sub (Day_Sec& right);

public:
  Day_Sec& operator + (Day_Sec& right) { return add (right); }
  Day_Sec& operator - (Day_Sec& right) { return sub (right); }

public:
  int set_civil (int time_zone_minutes_west,    // -720 .. +720
                 int year,
                 int month,        // Jan=1 .. Dec=12
                 int day,          // 1 .. 28/29/30/31 (depending on year/month)
                 int hour   = 0,   // 0 .. 23
                 int minute = 0,   // 0 .. 59
                 int second = 0);  // 0 .. 59
  //
  // Returns 1 on success, 0 if any of the values were invalid.
  //

public:
  int set_unix (const struct timeval*  tv,
                const struct timezone* tz);
  //
  // Sets the day-sec pair according to the given timeval. The timezone is
  // ignored (currently), especially if it is NULL.
  //
  // Returns 1 on success, 0 if any of the values were invalid.
  //

public:
#if 0      // So far unsupported.
  int get_civil (int  time_zone_minutes_west,    // -720 .. +720
                 int* year,
                 int* month,    // Jan=1 .. Dec=12
                 int* day,      // 1 .. 28/29/30/31 (depending on year/month)
                 int* hour,     // 0 .. 23
                 int* minute,   // 0 .. 59
                 int* second);  // 0 .. 59
#endif
  //
  // Returns 1 on success, 0 if time_zone_minutes_west is invalid.
  //

public:
  signed long day;
  double      sec;
};   // clas Day_Sec

#endif   // If not already included.
