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
#include "ezlog.h"

#define kSecondsPerDay  86400
#define kZoneSecondsMin (-12*60*60)
#define kZoneSecondsMax (+12*60*60)

typedef void (*DAYSEC_BINARY_OPERATION)(DAYSEC result, const DAYSEC left, const DAYSEC right);

// ---------------------------------------------------------------------------
void daysec_add (DAYSEC sum,  const DAYSEC left, const DAYSEC right) {
    if (sum != NULL) {
        signed long day;
        double      sec;
        day = left->day + right->day;
        sec = left->sec + right->sec;
        if (sec >= kSecondsPerDay) {
            day++;
            sec -= kSecondsPerDay;
        }
        sum->day = day;
        sum->sec = sec;
    }
}   // daysec_add

// ---------------------------------------------------------------------------
void daysec_sub(DAYSEC diff, const DAYSEC left, const DAYSEC right) {
    if (diff != NULL) {
        int    day = left->day - right->day;
        double sec = left->sec - right->sec;
        if (sec < 0.0) {
          day--;
          sec += kSecondsPerDay;
        }
        diff->day = day;
        diff->sec = sec;
    }
}   // daysec_sub

// ---------------------------------------------------------------------------
void daysec_add_seconds(DAYSEC sum, double seconds) {
    struct DAYSEC_STRUCT operand;
    DAYSEC_BINARY_OPERATION operation = daysec_add;
    if (seconds < 0) {
        seconds = -seconds;
        operation = daysec_sub;
    }
    operand.day = (int) (seconds / (double) kSecondsPerDay);
    operand.sec = seconds - (kSecondsPerDay * operand.day);
    operation(sum, sum, &operand);
}   // daysec_add_seconds

// ---------------------------------------------------------------------------
int daysec_set_civil (DAYSEC daysec,
                      int    time_zone_seconds_west,    // -720 .. +720
                      int    year,
                      int    month,     // Jan=1 .. Dec=12
                      int    day,       // 1 .. 28/29/30/31 (depending on year/month)
                      int    hour,      // 0 .. 23
                      int    minute,    // 0 .. 59
                      int    second) {  // 0 .. 59

    // Days up to the first of ... Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
    const int kDaysInMonth[]  = {  31,  28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31 };
    int is_leap_year = 0;
    if ((time_zone_seconds_west < kZoneSecondsMin) ||
        (time_zone_seconds_west > kZoneSecondsMax)) {
        return 0;
    }
    if ((month  < 1) || (month  > 12) ||
        (hour   < 0) || (hour   > 23) ||
        (minute < 0) || (minute > 59) ||
        (second < 0) || (second > 59) ||        /* @TODO(dr) Handle leap seconds. */
        (day    < 1)) {
        return 0;
    }
    if ((year % 4) != 0) {
        is_leap_year = 0;
    } else if ((year % 100) != 0) {
        is_leap_year = 1;
    }  else if ((year % 400) != 0) {
        is_leap_year = 0;
    } else {
        is_leap_year = 1;
    }
    if ((is_leap_year && (month == 2) && (day > 29)) ||
        (day > kDaysInMonth[month-1])) {
        return 0;
    }
    if (NULL != daysec) {
        const int kDaysPer400Years = ((400 * 365) + 97);    /* 0x00023AB1 (146097). */
        const int kDaysIntoYear[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
        int days = kDaysIntoYear[month-1];
        double seconds = 0;
        if (is_leap_year && (month > 2)) {
            days++;
        }
        EZLOGD("%d-%02u-%02uT%02u:%02u:%02u %7d (leap=%s)",
               year, month, day, hour, minute, second, time_zone_seconds_west,
               is_leap_year ? "yes" : "no");
        days += (year / 400) * kDaysPer400Years;
        year %= 400;
        days += year * 365;
        days += (year+3) / 4;   /* This will incorrectly include leap days in year 100, 200, and 300. */
        if (year > 100) {       /* @TODO(dr) This seems wrong to me. Check it. */
            days -= (year-1) / 100;    /* This will take them back out. */
        }
        days += day - 1;
        seconds  = (3600.0 * (double) hour) +
                   (  60.0 * (double) minute) +
                   (   1.0 * (double) second);
        seconds += (double) time_zone_seconds_west;
        if (seconds < 0.0) {
            days--;
            seconds += kSecondsPerDay;
        } else if (seconds > kSecondsPerDay) {
            days++;
            seconds -= kSecondsPerDay;
        }

        daysec->day = days;
        daysec->sec = seconds;
    }
    return 1;
}   // daysec_set_civil

// ---------------------------------------------------------------------------
int daysec_get_civil (DAYSEC time,
                      int    time_zone_seconds_west,    // -12h .. +12h.
                      int*   year,
                      int*   month,     // Jan=1 .. Dec=12
                      int*   day,       // 1 .. 28/29/30/31 (depending on year/month)
                      int*   hour,      // 0 .. 23
                      int*   minute,    // 0 .. 59
                      int*   second) {  // 0 .. 59
    if ((time_zone_seconds_west < kZoneSecondsMin) ||
        (time_zone_seconds_west > kZoneSecondsMax)) {
        return 0;
    }
    /*
     * @TODO(dr) Write this code!
     */
    return 0;
}   /* daysec_get_civil() */

/* ------------------------------------------------------------------------- */
static void tm_get_fields(const struct tm* tm,
                          int* year, int* month, int* day,
                          int* hour, int* minute, int* second) {
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
static void get_local_time(time_t* time, struct tm* tm, int* time_zone_seconds) {
    assert(NULL != time);
    assert(NULL != tm);
    assert(NULL != time_zone_seconds);
#ifdef WIN32
    localtime_s(tm, time);
    _get_timezone(time_zone_seconds);
#else
    if (NULL != localtime_r(time, tm)) {
        *time_zone_seconds = - (int) tm->tm_gmtoff;
    }
#endif
}   /* get_local_time() */

/* ------------------------------------------------------------------------- */
inline static int is_date_delimiter(char c) { return ('-' == c); }
inline static int is_time_delimiter(char c) { return (':' == c); }
inline static int is_date_time_separator(char c) { return ('T' == c) || ('t' == c) || (' ' == c) || (',' == c); }
inline static int is_zone(char c) { return ('z' == c) || ('Z' == c) || (0 == c); }

/* ------------------------------------------------------------------------- */
/**
 * This does *not* check for valid numerical combinations of date/time
 * fields (for example, the month might by 27).
 *
 * @return 1 if @a text is of the proper form - ISO8601-esque, 0 otherwise.
 */
int date_time_from_text(const char* text,
                        int* inout_year, int* inout_month, int* inout_day,
                        int* inout_hour, int* inout_minute, int* inout_second,
                        char* inout_zone) {
    int rval = 0;
    assert(NULL != text);
    assert(NULL != inout_year);
    assert(NULL != inout_month);
    assert(NULL != inout_day);
    assert(NULL != inout_hour);
    assert(NULL != inout_minute);
    assert(NULL != inout_second);
    assert(NULL != inout_zone);
    int year = *inout_year;
    int month = *inout_month;
    int day = *inout_day;
    int hour = *inout_hour;
    int minute = *inout_minute;
    int second = *inout_second;
    char zone = *inout_zone;
    const int original_year = *inout_year;
    typedef enum state_e {
        STATE_IDLE,
        STATE_YEAR,
        STATE_MONTH,
        STATE_DAY,
        STATE_HOUR,
        STATE_MINUTE,
        STATE_SECOND,
        STATE_END,
        STATE_ERROR
    } state_t;
#define StateUpdate(_cond,_newstate,_code) \
    if (_cond) {                           \
        _code ;                            \
        state = _newstate;                 \
        break;                             \
    }
    state_t state = STATE_IDLE;
    int i = 0;
    for (i = 0; (0 != text[i]) && (STATE_ERROR != state) && (STATE_END != state); ++i) {
        const char ch = text[i];
        switch (state) {
        case STATE_IDLE:
            StateUpdate(isspace(ch), STATE_IDLE, );
            StateUpdate(isdigit(ch), STATE_YEAR, year = ch - '0');
            state = STATE_ERROR; break;
        case STATE_YEAR:
            StateUpdate(isdigit(ch), STATE_YEAR, year = (10 * year) + ch - '0');
            StateUpdate(is_date_delimiter(ch), STATE_MONTH, month = 0);
            StateUpdate(is_time_delimiter(ch), STATE_MINUTE,
                        hour = year;
                        year = original_year;
                        minute = second = 0);
            state = STATE_ERROR; break;
        case STATE_MONTH:
            StateUpdate(isdigit(ch), STATE_MONTH, month = (10 * month) + ch - '0');
            StateUpdate(is_date_delimiter(ch), STATE_DAY, day = 0);
            StateUpdate(is_date_time_separator(ch), STATE_HOUR,
                        day = month;
                        month = year;
                        year = original_year;
                        hour = minute = second = 0);
            StateUpdate(is_zone(ch), STATE_END, zone = ch; day = month; month = year; year = original_year);
            state = STATE_ERROR; break;
        case STATE_DAY:
            StateUpdate(isdigit(ch), STATE_DAY, day = (10 * day) + ch - '0');
            StateUpdate(is_date_time_separator(ch), STATE_HOUR, hour = minute = second = 0);
            state = STATE_ERROR; break;
        case STATE_HOUR:
            StateUpdate(isdigit(ch), STATE_HOUR, hour = (10 * hour) + ch - '0');
            StateUpdate(is_time_delimiter(ch), STATE_MINUTE, );
            StateUpdate(is_zone(ch), STATE_END, zone = ch);
            state = STATE_ERROR; break;
        case STATE_MINUTE:
            StateUpdate(isdigit(ch), STATE_MINUTE, minute = (10 * minute) + ch - '0');
            StateUpdate(is_time_delimiter(ch), STATE_SECOND, );
            StateUpdate(is_zone(ch), STATE_END, zone = ch);
            state = STATE_ERROR; break;
        case STATE_SECOND:
            StateUpdate(isdigit(ch), STATE_SECOND, second = (10 * second) + ch - '0');
            StateUpdate(is_zone(ch), STATE_END, zone = ch);
            state = STATE_ERROR; break;
        case STATE_END:         /* No chars allowed at end. */
        case STATE_ERROR:
            state = STATE_ERROR;
        }   /* switch on state */
    }
    /* Check final state for validity and possible cleanup. */
    switch (state) {
    case STATE_IDLE:
    case STATE_YEAR:
        break;
    case STATE_MONTH:   /* "MM-DD" which was interpreted above as "YYYY-MM". */
        day = month;
        month = year;
        year = original_year;
        rval = 1;
        break;
    case STATE_DAY:
    case STATE_HOUR:
    case STATE_MINUTE:
    case STATE_SECOND:
    case STATE_END:
        rval = 1;
    case STATE_ERROR:
        break;
    }
    if (rval) {
        *inout_year = year;
        *inout_month = month;
        *inout_day = day;
        *inout_hour = hour;
        *inout_minute = minute;
        *inout_second = second;
        *inout_zone = zone;
    }
    return rval;
}   /* date_time_from_text() */

// ---------------------------------------------------------------------------
// @return 1 on success, 0 otherwise.
int daysec_from_text(DAYSEC daysec, const char* text, const DAYSEC_SHORTCUT* shortcuts) {
    if (NULL == text) {
        return 0;
    }
    EZLOGD("text=\"%s\"", text);

    /* Apply any short-cuts. */
    if (NULL != shortcuts) {
        for (; (NULL != shortcuts->name) && (NULL != shortcuts->value); ++shortcuts) {
            if (0 == strcasecmp(shortcuts->name, text)) {
                EZLOGD("shortcut \"%s\" => text=\"%s\"", text, shortcuts->value);
                text = shortcuts->value;
                break;
            }
        }
    }

    /* Set fields to local time first. */
    int year = 0;
    int month = 0;
    int day = 0;
    int hour = 0;
    int minute = 0;
    int second = 0;
    int time_zone_seconds = 0;   /* West. */
    char zone = 0;
    int rval = 0;

    struct tm now_tm = {0};
    time_t now = 0;
    time(&now);
    get_local_time(&now, &now_tm, &time_zone_seconds);
    tm_get_fields(&now_tm, &year, &month, &day, &hour, &minute, &second);
    EZLOGD("%04d-%02u-%02uT%02u:%02u:%02u %7d current time",
           year, month, day, hour, minute, second, time_zone_seconds, text);

    /* The special name "now" means we already have the time. */
    if (0 == strcasecmp("now", text)) {
        rval = 1;
    } else {
        if (date_time_from_text(text, &year, &month, &day, &hour, &minute, &second, &zone)) {
            time_zone_seconds = (tolower(zone) == 'z') ? 0 : time_zone_seconds;
            rval = 1;
        } else {
            EZLOGD("failed to parse time text=\"%s\"", text);
        }
    }
    if (rval) {
        EZLOGD("%04d-%02u-%02uT%02u:%02u:%02u %7d text=\"%s\"",
               year, month, day, hour, minute, second, time_zone_seconds, text);
        rval = daysec_set_civil(daysec, time_zone_seconds, year, month, day, hour, minute, second);
    }
    return rval;
}   // daysec_from_text()
