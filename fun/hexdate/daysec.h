#ifndef __daysec_h__
#define __daysec_h__

#if defined (__cplusplus)
extern "C"
//{
#endif

typedef struct DAYSEC_STRUCT *DAYSEC;


struct DAYSEC_STRUCT {
    int day;
    double sec;
};

typedef struct DAYSEC_SHORTCUT_STRUCT {
    const char* name;
    const char* value;
} DAYSEC_SHORTCUT;

// ---------------------------------------------------------------------------
void daysec_add(DAYSEC sum,  const DAYSEC left, const DAYSEC right);
void daysec_sub(DAYSEC diff, const DAYSEC left, const DAYSEC right);
void daysec_add_seconds(DAYSEC time, double seconds);

/* ------------------------------------------------------------------------- */
/**
 * Set the @a daysec value, if not NULL, to the days and seconds
 * according to the given individual fields.
 *
 * Use NULL for @a daysec to simply check the fields for validity.
 *
 * @return 1 if the fields constitute a valid time, 0 otherwise.
 */
int daysec_set_civil (DAYSEC daysec,
                      int    time_zone_seconds_west,    // -12h .. +12h
                      int    year,
                      int    month,    // Jan=1 .. Dec=12
                      int    day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int    hour,     // 0 .. 23
                      int    minute,   // 0 .. 59
                      int    second);  // 0 .. 59

// ---------------------------------------------------------------------------
/**
 * Not yet implemented.
 */
#if 0
int daysec_get_civil (DAYSEC daysec,
                      int    time_zone_seconds_west,    // -12h .. +12h
                      int*   year,
                      int*   month,    // Jan=1 .. Dec=12
                      int*   day,      // 1 .. 28/29/30/31 (depending on year/month)
                      int*   hour,     // 0 .. 23
                      int*   minute,   // 0 .. 59
                      int*   second);  // 0 .. 59
#endif

// ---------------------------------------------------------------------------
/**
 * @a text is any reasonable substring of ISO-8601.
 *
 * @return 1 on success, 0 otherwise.
 */
int daysec_from_text(DAYSEC daysec,
                     const char* text,
                     const DAYSEC_SHORTCUT* shortcuts);  /* Ends with NULL name or value. */

#if defined (__cplusplus)
};
#endif

#endif
