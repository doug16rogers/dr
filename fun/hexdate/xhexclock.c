#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>

#include <X11/xpm.h>


#include "daysec.h"
#include "hexon.h"

#define USE_PIXMAP_FROM_DATA  1
#undef  USE_PIXMAP_FROM_DATA

#ifdef USE_PIXMAP_FROM_DATA
#define static
#include "hexal-0-10x10.xpm"
#include "hexal-1-10x10.xpm"
#include "hexal-2-10x10.xpm"
#include "hexal-3-10x10.xpm"
#include "hexal-4-10x10.xpm"
#include "hexal-5-10x10.xpm"
#include "hexal-6-10x10.xpm"
#include "hexal-7-10x10.xpm"
#include "hexal-8-10x10.xpm"
#include "hexal-9-10x10.xpm"
#include "hexal-A-10x10.xpm"
#include "hexal-B-10x10.xpm"
#include "hexal-C-10x10.xpm"
#include "hexal-D-10x10.xpm"
#include "hexal-E-10x10.xpm"
#include "hexal-F-10x10.xpm"
#include "hexal-dot-10x10.xpm"
#undef static
#endif

#define PROGRAM_NAME "xhexclock"

#define SECONDS_PER_MINUTE  (0x3C)
#define MINUTES_PER_HOUR    (0x3C)
#define HOURS_PER_DAY       (0x18)
#define DAYS_PER_YEAR       (0x16D)      // Non-leapyear.

#define SECONDS_PER_HOUR    (SECONDS_PER_MINUTE * MINUTES_PER_HOUR)
#define SECONDS_PER_DAY     (SECONDS_PER_MINUTE * MINUTES_PER_HOUR * HOURS_PER_DAY)   // 0x15180.

#define UNIX_START_YEAR     (0x07B2)
#define UNIX_START_DAY      (0x000)
#define UNIX_START_SECOND   (0x00000)

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

#define DEFAULT_EPOCH    "unix"
#define DEFAULT_FORMAT   "days"      // See hexon.c.
#define DEFAULT_VERBOSE  0

//
// These are set initially by calls in main().
//
struct DAYSEC_STRUCT epoch;
const char* output_format    = DEFAULT_FORMAT;
int         verbose = 0;

const char* on_off[] = { "off", "on" };

// ---------------------------------------------------------------------------
void Usage (void)
{
  fprintf (stderr, "Usage: " PROGRAM_NAME " [options]\n");
  fprintf (stderr, "\n");
  fprintf (stderr, PROGRAM_NAME " displays the current time as a hex time in hex days and hexons.\n");
  fprintf (stderr, "There are 0x10000 hexons in a standard civil day, so each hexon is slightly\n");
  fprintf (stderr, "longer in duration than a civil second. Note that there are no time zones in\n");
  fprintf (stderr, "hexon time.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Epoch times are calculated using the cyclic Gregorian calendar, without leap\n");
  fprintf (stderr, "seconds, synchronized to the year 2000.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Options:\n");
  fprintf (stderr, "  --help, -h        Show this usage information.\n");
  fprintf (stderr, "  --epoch=<epoch>   Use <epoch> for epoch [%s]:\n", DEFAULT_EPOCH);
  fprintf (stderr, "                      Y.M.D/h:m:s  Civil time of epoch, GMT\n");
  fprintf (stderr, "                      doug         Doug's birthday (1963.12.31/10:58:00 GMT)\n");
  fprintf (stderr, "                      unix         Unix zero time  (1970.01.01/00:00:00 GMT)\n");
  fprintf (stderr, "  --format=<format> Display using <format> [%s]:\n", DEFAULT_FORMAT);
  fprintf (stderr, "                      days         DDDD.HHHH.hhhh  (H=hexons in day).\n");
  fprintf (stderr, "                      days+        Like 'days', but with hexal display.\n");
  fprintf (stderr, "                      hexons       HHHHHHHH.hhhh   (H=hexons from epoch).\n");
  fprintf (stderr, "  --verbose         Display verbose information [%s].\n", on_off[DEFAULT_VERBOSE != 0]);
  exit (1);
}   // Usage

// ---------------------------------------------------------------------------
int strequ (const char* actual, const char* expected)
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
}   // strequ

// ---------------------------------------------------------------------------
const char* Null_Check (const char* string)
{
  if (string == NULL) return "<<NULL>>";
  return string;
}   // Null_Check

// ---------------------------------------------------------------------------
void Set_Epoch_Civil_Time (int time_zone_minutes_west, int year, int month, int day, int hour, int minute, int second)
{
  if (!daysec_set_civil (&epoch, time_zone_minutes_west, year, month, day, hour, minute, (double) second))
  {
    fprintf (stderr, PROGRAM_NAME ": *** error: bad epoch item range for %d.%u.%u/%02u:%02u:%02u (zone=%d minutes).\n",
             year, month, day, hour, minute, second, time_zone_minutes_west);
    exit (1);
  }


}   // Set_Epoch_Civil_Time

// ---------------------------------------------------------------------------
void Set_Unix_Time (DAYSEC time, const struct timeval* tv, const struct timezone* tz)
{
  if (!daysec_set_civil (time, 0 * MINUTES_PER_HOUR /* GMT */, 0x7B2, 0x1, 0x01, 0x00, 0x00, 0x00))
  {
    fprintf (stderr, PROGRAM_NAME ": *** internal error: could not convert constant Unix epoch!\n");
    exit (1);
  }

  daysec_add_seconds (time, (double) tv->tv_sec + (1.0E-6 * (double) tv->tv_usec));
}   // Set_Unix_Time

// ---------------------------------------------------------------------------
void Set_Epoch (const char* epoch)
{
  int year = 0;
  int month = 0;
  int day = 0;
  int hour = 0;
  int minute = 0;
  int second = 0;
  char dummy;

  if (epoch == NULL) goto Exception;
  else if (strcmp (epoch, "doug") == 0) Set_Epoch_Civil_Time (5 * MINUTES_PER_HOUR /* EST */,
                                                              0x7AB, 0xC, 0x1F, 0x05, 0x3A, 0x00);
  else if (strcmp (epoch, "unix") == 0) Set_Epoch_Civil_Time (0 * MINUTES_PER_HOUR /* GMT */,
                                                              0x7B2, 0x1, 0x01, 0x00, 0x00, 0x00);
  else if (sscanf (epoch, "%d.%u.%u/%u:%u:%u%c", &year, &month, &day, &hour, &minute, &second, &dummy) == 6)
  {
    Set_Epoch_Civil_Time (0 * MINUTES_PER_HOUR /* GMT */, year, month, day, hour, minute, second);
  }
  else if (sscanf (epoch, "%d.%u.%u/%u:%u%c", &year, &month, &day, &hour, &minute, &dummy) == 5)
  {
    Set_Epoch_Civil_Time (0 * MINUTES_PER_HOUR /* GMT */, year, month, day, hour, minute, 0);
  }
  else if (sscanf (epoch, "%d.%u.%u%c", &year, &month, &day, &dummy) == 3)
  {
    Set_Epoch_Civil_Time (0 * MINUTES_PER_HOUR /* GMT */, year, month, day, 0, 0, 0);
  }
  else
  {
    goto Exception;
  }

  return;

Exception:
  fprintf (stderr, PROGRAM_NAME ": *** error: I can't handle an epoch of \"%s\"; use --help.\n", Null_Check (epoch));
  exit (1);
}   // Set_Epoch

// ---------------------------------------------------------------------------
void Set_Format (const char* format)
{
  if (format == NULL) goto Exception;
  else if (strcmp (format, "days"  ) == 0) output_format = format;  // Eventually I may add something like "%4D.%4H.%4h".
  else if (strcmp (format, "days+" ) == 0) output_format = format;  // Eventually I may add something like "%4D.%4H.%4h".
  else if (strcmp (format, "hexons") == 0) output_format = format;
  else goto Exception;

  return;

Exception:
  fprintf (stderr, PROGRAM_NAME ": *** error: I can't handle a format of \"%s\".\n", Null_Check (format));
  exit (1);
}   // Set_Format

// ---------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  int i;
  struct timezone          zone;
  struct timeval           time;
  struct HEXON_TIME_STRUCT hexon;
  struct DAYSEC_STRUCT     daysec;

  Widget       shell;
  XtAppContext app;
  Widget       button[0x11];
  Pixmap       pix[0x11];
  Pixel        fg;
  Pixel        bg;

  const char* pixmap_filename[] =
  {
    "hexal-0-10x10.xpm",
    "hexal-1-10x10.xpm",
    "hexal-2-10x10.xpm",
    "hexal-3-10x10.xpm",
    "hexal-4-10x10.xpm",
    "hexal-5-10x10.xpm",
    "hexal-6-10x10.xpm",
    "hexal-7-10x10.xpm",
    "hexal-8-10x10.xpm",
    "hexal-9-10x10.xpm",
    "hexal-A-10x10.xpm",
    "hexal-B-10x10.xpm",
    "hexal-C-10x10.xpm",
    "hexal-D-10x10.xpm",
    "hexal-E-10x10.xpm",
    "hexal-F-10x10.xpm"
    "hexal-dot-10x10.xpm"
  };

#ifdef USE_PIXMAP_FROM_DATA
  const char** pixmap_data[] =
  {
    (const char**) hexal_0_10x10_xpm,
    (const char**) hexal_1_10x10_xpm,
    (const char**) hexal_2_10x10_xpm,
    (const char**) hexal_3_10x10_xpm,
    (const char**) hexal_4_10x10_xpm,
    (const char**) hexal_5_10x10_xpm,
    (const char**) hexal_6_10x10_xpm,
    (const char**) hexal_7_10x10_xpm,
    (const char**) hexal_8_10x10_xpm,
    (const char**) hexal_9_10x10_xpm,
    (const char**) hexal_A_10x10_xpm,
    (const char**) hexal_B_10x10_xpm,
    (const char**) hexal_C_10x10_xpm,
    (const char**) hexal_D_10x10_xpm,
    (const char**) hexal_E_10x10_xpm,
    (const char**) hexal_F_10x10_xpm,
    (const char**) hexal_dot_10x10_xpm
  };
#endif

  Set_Epoch  (DEFAULT_EPOCH);
  Set_Format (DEFAULT_FORMAT);

  shell = XtAppInitialize (&app, "xHexClock", NULL, 0,
                           &argc, argv, NULL, NULL, 0);

  for (i = 1; i < argc; i++)
  {
    char* argument = argv[i];
    if ((strcmp (argument, "--help") == 0) || (strcmp (argument, "-h") == 0)) Usage();
    else if (strequ (argument, "--epoch=" )) Set_Epoch  (strchr (argument, '=') + 1);
    else if (strequ (argument, "--format=")) Set_Format (strchr (argument, '=') + 1);
    else if (strequ (argument, "--verbose")) verbose = 1;
    else
    {
      fprintf (stderr, PROGRAM_NAME ": *** error: unknown option \"%s\".\n\n", argument);
      Usage();
    }
  }

  gettimeofday (&time, &zone);

  Set_Unix_Time (&daysec, &time, &zone);

  if (verbose)
  {
    printf ("Unix time    (seconds.microseconds): %lu.%06lu (hex 0x%lx/0x%05lx.\n",
            time.tv_sec, time.tv_usec, time.tv_sec, time.tv_usec);
    printf ("Unix daysec  (days.seconds): %lu.%05lu (hex 0x%lx/0x%05lx.\n",
            daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);
    printf ("Epoch daysec (days.seconds): %lu.%05lu (hex 0x%lx/0x%05lx.\n",
            epoch.day, (long) epoch.sec, epoch.day, (long) epoch.sec);
  }

  daysec_sub (&daysec, &daysec, &epoch);

  if (verbose)
  {
    printf ("Daysec since epoch  (days.seconds): %lu.%05lu (hex 0x%lx/0x%05lx.\n",
            daysec.day, (long) daysec.sec, daysec.day, (long) daysec.sec);
  }

  Hexon_Set_Day_Second (&hexon, daysec.day, daysec.sec);

  if (verbose)
  {
    printf ("Decimal hexon = %20.6f.\n", hexon.hexon);
  }

//  if (!Hexon_Print (stdout, output_format, &hexon))
//  {
//    fprintf (stderr, PROGRAM_NAME ": *** error: could not print using format \"%s\".\n", Null_Check (output_format));
//    return 1;
//  }

  XtVaGetValues (shell,
                 XmNforeground, &fg,
                 XmNbackground, &bg,
                 NULL);

  XtRealizeWidget (shell);

  for (i = 0; i < 3 /* 0x10 */; i++)
  {
#if defined (USE_PIXMAP_FROM_DATA)
    XpmAttributes  attributes;
    int            status;
    Display*       display = XtDisplay (shell);
    Pixmap         mask;
    XpmColorSymbol symbols[2];
#endif

//    button[i] = XtCreateManagedWidget (pixmap_filename[i], xmLabelWidgetClass, shell, NULL, 0);
    button[i] = XtVaCreateManagedWidget (pixmap_filename[i], xmLabelWidgetClass, shell,
                                         XmNx, i * 32,
                                         XmNy, 0,
                                         NULL);

#if !defined (USE_PIXMAP_FROM_DATA)
    pix[i] = XmGetPixmap (XtScreen (shell), (char*) pixmap_filename[i], fg, bg);

    if (pix[i])
    {
      XtVaSetValues (button[i],
                     XmNlabelType,   XmPIXMAP,
                     XmNlabelPixmap, pix[i],
#if 0
                     XmNx,           0,
                     XmNy,           0,
                     XmNwidth,       16,
                     XmNheight,      16,
#endif
                     NULL);
    }
#else
    symbols[0].name  = "background";
    symbols[0].value = NULL;
    symbols[0].pixel = bg;
//    symbols[1].name  = "foreground";
//    symbols[1].value = NULL;
//    symbols[1].pixel = fg;

    attributes.colorsymbols = symbols;
    attributes.numsymbols = 1;

    XtVaGetValues (button[i],
                   XmNdepth,      &attributes.depth,
                   XmNcolormap,   &attributes.colormap,
                   NULL);

    attributes.visual = DefaultVisual (display, DefaultScreen (display));
    attributes.valuemask = 0; // XpmDepth | XpmColormap | XpmVisual | XpmColorSymbols;

    status = XpmCreatePixmapFromData (display,
                                      DefaultRootWindow (display),
                                      (char**) pixmap_data[i],
                                      &pix[i],
                                      &mask,
                                      &attributes);

    if (mask /* != NULL */) XFreePixmap (display, mask);

    if (status == XpmSuccess)
    {
      XtVaSetValues (button[i],
                     XmNlabelType,   XmPIXMAP,
                     XmNlabelPixmap, pix[i],
                     XmNx,           i * 16,
                     XmNy,           0,
                     XmNwidth,       16,
                     XmNheight,      16,
                     NULL);
    }
#endif
  }   // For each hexal symbol pixmap.

//  XtRealizeWidget (shell);
  XtAppMainLoop (app);

  return 0;
}   // main
