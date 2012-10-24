#include <qapplication.h>
#include <string.h>
#include <stdio.h>

#include "day_sec.hpp"
#include "digital_hexal_clock.hpp"

#define PROGRAM_NAME     "dhexclock"

#define DAYS_FORMAT      "%4D.%4H"
#define DAYS_FULL_FORMAT "%4D.%4H.%4h"
#define HEXONS_FORMAT    "%4D%4H"

#define DEFAULT_EPOCH    "unix"
#define DEFAULT_FORMAT   "days"
#define DEFAULT_VERBOSE  0

Day_Sec epoch;
char    output_format[0x100] = DAYS_FORMAT;   // Set early in main().
int     verbose = DEFAULT_VERBOSE;

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
  fprintf (stderr, "  -help, -h        Show this usage information.\n");
  fprintf (stderr, "  -epoch=<epoch>   Use <epoch> for epoch [%s]:\n", DEFAULT_EPOCH);
  fprintf (stderr, "                     Y.M.D/h:m:s  Civil time of epoch, UTC\n");
  fprintf (stderr, "                     doug         Doug's birthday (1963.12.31/10:58:00 UTC)\n");
  fprintf (stderr, "                     unix         Unix zero time  (1970.01.01/00:00:00 UTC)\n");
  fprintf (stderr, "  -format=<format> Display using <format> [%s]:\n", DEFAULT_FORMAT);
  fprintf (stderr, "                     days         Same as \"%s\".\n", DAYS_FORMAT);
  fprintf (stderr, "                     hexons       Same as \"%s\".\n", HEXONS_FORMAT);
  fprintf (stderr, "  -geometry <g>    Standard xterm geometry settings.\n");
  fprintf (stderr, "  -verbose         Display verbose information [%s].\n", on_off[DEFAULT_VERBOSE != 0]);
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
  if (!epoch.set_civil (time_zone_minutes_west, year, month, day, hour, minute, second))
  {
    fprintf (stderr, PROGRAM_NAME ": *** error: bad epoch item range for %d.%u.%u/%02u:%02u:%02u (zone=%d minutes).\n",
             year, month, day, hour, minute, second, time_zone_minutes_west);
    exit (1);
  }
}   // Set_Epoch_Civil_Time

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

#define MINUTES_PER_HOUR 0x3C

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
  else if (strcmp(format, "days"   ) == 0) strcpy(output_format, DAYS_FORMAT);
  else if (strcmp(format, "hexons" ) == 0) strcpy(output_format, HEXONS_FORMAT);
  else
  {
    strncpy(output_format, format, sizeof(output_format));
    output_format[sizeof(output_format)-1] = 0;
  }

  return;

Exception:
  fprintf (stderr, PROGRAM_NAME ": *** error: I can't handle a format of \"%s\".\n", Null_Check (format));
  exit (1);
}   // Set_Format

// ---------------------------------------------------------------------------
void Load_Arguments (int& argc, char* argv[])
{
  for (int i = 1; i < argc; i++)
  {
    char* argument = argv[i];
    if ((strcmp (argument, "--help") == 0) || strequ(argument, "-h")) Usage();
    else if (strequ (argument, "-epoch=" )) Set_Epoch  (strchr (argument, '=') + 1);
    else if (strequ (argument, "-format=")) Set_Format (strchr (argument, '=') + 1);
    else if (strequ (argument, "-verbose")) verbose = 1;
    else
    {
      fprintf (stderr, PROGRAM_NAME ": *** error: unknown option \"%s\".\n\n", argument);
      Usage();
      return;
    }

    for (int j = i; (j+1) < argc; j++)
    {
      argv[j] = argv[j+1];
    }
    i--;
    argc--;
  }
}   // Load_Arguments

// ---------------------------------------------------------------------------
int main (int argc, char **argv)
{
  Set_Epoch  (DEFAULT_EPOCH);
  Set_Format (DEFAULT_FORMAT);

  Load_Arguments (argc, argv);
  QApplication app (argc, argv);

  Digital_Hexal_Clock *clock = new Digital_Hexal_Clock (epoch, output_format);
//  clock->resize (160, 64);
  app.setMainWidget (clock);
  clock->show();
  return app.exec();
}   // main
