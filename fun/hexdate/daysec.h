#ifndef __daysec_h__
#define __daysec_h__

#if defined (__cplusplus)
extern "C"
//{
#endif

typedef struct DAYSEC_STRUCT *DAYSEC;


struct DAYSEC_STRUCT
{
  signed long day;
  double      sec;
};

typedef struct DAYSEC_SHORTCUT_STRUCT
{
  const char* name;
  const char* value;
} DAYSEC_SHORTCUT;

// ---------------------------------------------------------------------------
void daysec_add (DAYSEC sum,  const DAYSEC left, const DAYSEC right);
void daysec_sub (DAYSEC diff, const DAYSEC left, const DAYSEC right);
void daysec_add_seconds (DAYSEC time, double seconds);

// ---------------------------------------------------------------------------
int daysec_set_civil (DAYSEC time,
                      int    time_zone_seconds_west,    // -12h .. +12h
                      int    year,
                      int    month,    // Jan=1 .. Dec=12
                      int    day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int    hour,     // 0 .. 23
                      int    minute,   // 0 .. 59
                      int    second);  // 0 .. 59
//
// Returns 1 on success, 0 if the date given is invalid.
//

// ---------------------------------------------------------------------------
int daysec_get_civil (DAYSEC time,
                      int    time_zone_seconds_west,    // -12h .. +12h
                      int*   year,
                      int*   month,    // Jan=1 .. Dec=12
                      int*   day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int*   hour,     // 0 .. 23
                      int*   minute,   // 0 .. 59
                      int*   second);  // 0 .. 59
//
// Returns 1 on success, 0 if time_zone_minutes_west is invalid.
//

// ---------------------------------------------------------------------------
// 'text' is any reasonable substring of ISO-8601.
// @return 1 on success, 0 otherwise.
int daysec_from_text(DAYSEC daysec,
                     const char* text,
                     const DAYSEC_SHORTCUT* shortcuts);  // Ends with NULL name or value.

#if defined (__cplusplus)
};
#endif

#endif
