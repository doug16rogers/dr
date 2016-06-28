#ifndef __hexon_h__
#define __hexon_h__

#include <stdio.h>        // For FILE.
#ifdef WIN32
#include <time.h>
#else
#include <sys/time.h>     // For struct timeval.
#endif

#define HEXONS_PER_DAY      (0x10000)
#define HEXONS_PER_SECOND   ((double) HEXONS_PER_DAY / 86400.0)
#define HEXICLES_PER_HEXON  (0x10000)

typedef struct HEXON_TIME_STRUCT *HEXON_TIME;

struct HEXON_TIME_STRUCT
{
  double hexon;
};   // struct HEXON_TIME


#ifdef __cplusplus
extern "C" {
#endif

// ---------------------------------------------------------------------------
void Hexon_Set_As_Timeval (HEXON_TIME            time,
                           const struct timeval* tv);
//
// Converts the given seconds/microseconds value into a hexon time.
//

// ---------------------------------------------------------------------------
void Hexon_Set_Day_Second (HEXON_TIME   time,
                           signed long  day,
                           double       second);
//
// Converts the given day/seconds since year 0 into a hexon time.
//

// ---------------------------------------------------------------------------
int Hexon_Print (FILE*            file,
                 const char*      format,
                 int              newline,
                 const HEXON_TIME time);
//
// Prints the hexon to the given file stream with the given format. Right
// now only a limited number of formats are supported. See the C source.
//
// The 'newline' setting is used only for single-line formats.
//


#ifdef __cplusplus
};
#endif

#endif   // If not already included.
