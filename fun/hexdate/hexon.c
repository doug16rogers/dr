#include <stdio.h>
#include <string.h>
#include <math.h>

#include "hexon.h"

#ifndef SECONDS_PER_DAY
#define SECONDS_PER_DAY  0x15180
#endif

// ---------------------------------------------------------------------------
void Hexon_Set_As_Timeval (HEXON_TIME            time,
                           const struct timeval* tv)
{
  time->hexon = ((double) tv->tv_sec + (double) tv->tv_usec / 1.0E6) * HEXONS_PER_SECOND;
}   // Hexon_Set_As_Timeval

// ---------------------------------------------------------------------------
void Hexon_Set_Day_Second (HEXON_TIME   time,
                           signed long  day,
                           double       second)
{
  time->hexon = ((double) day * (double) (HEXONS_PER_DAY)) +
                 ((second / (double) SECONDS_PER_DAY) * (double) (HEXONS_PER_DAY));

}   // Hexon_Set_Day_Second

// ---------------------------------------------------------------------------
void Hexal_Seven_Segment_Print (FILE* file, const char* text)
{
  int i;

  //
  // Top row:
  //
  for (i = 0; text[i] != 0; i++)
  {
    switch (text[i])
    {
    case '0':
    case '1':
    case '2':
    case '3':
      fprintf (file, "    ");
      break;

    case '4':
    case '5':
    case '6':
    case '7':
      fprintf (file, "  | ");
      break;

    case '8':
    case '9':
    case 'A': case 'a':
    case 'B': case 'b':
      fprintf (file, "|   ");
      break;

    case 'C': case 'c':
    case 'D': case 'd':
    case 'E': case 'e':
    case 'F': case 'f':
      fprintf (file, "| | ");
      break;

    case '\n':
      fprintf (file, "\n");
      break;

    default:
      fprintf (file, "    ");
    }   // switch
  }   // for each character in top row

  //
  // Middle row:
  //
  for (i = 0; text[i] != 0; i++)
  {
    switch (text[i])
    {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case 'A': case 'a':
    case 'B': case 'b':
    case 'C': case 'c':
    case 'D': case 'd':
    case 'E': case 'e':
    case 'F': case 'f':
      fprintf (file, "+-+ ");
      break;


    case '\n':
      fprintf (file, "\n");
      break;

    default:
      fprintf (file, " %c  ", text[i]);
    }   // switch
  }   // for each character in middle row

  //
  // Bottom row:
  //
  for (i = 0; text[i] != 0; i++)
  {
    switch (text[i])
    {
    case '0':
    case '4':
    case '8':
    case 'C': case 'c':
      fprintf (file, "    ");
      break;

    case '1':
    case '5':
    case '9':
    case 'D': case 'd':
      fprintf (file, "|   ");
      break;

    case '2':
    case '6':
    case 'A': case 'a':
    case 'E': case 'e':
      fprintf (file, "  | ");
      break;

    case '3':
    case '7':
    case 'B': case 'b':
    case 'F': case 'f':
      fprintf (file, "| | ");
      break;

    case '\n':
      fprintf (file, "\n");
      break;

    default:
      fprintf (file, "    ");
    }   // switch
  }   // for each character in bottom row

}   // Hexal_Seven_Segment_Print

// ---------------------------------------------------------------------------
void Hexal_Diamond_Print (FILE* file, const char* text)
{
  int i;

  //
  // Top row:
  //
  for (i = 0; text[i] != 0; i++)
  {
    switch (text[i])
    {
    case '0':
    case '1':
    case '2':
    case '3':
      fprintf (file, "  |   ");
      break;

    case '4':
    case '5':
    case '6':
    case '7':
      fprintf (file, "  |\\  ");
      break;

    case '8':
    case '9':
    case 'A': case 'a':
    case 'B': case 'b':
      fprintf (file, " /|   ");
      break;

    case 'C': case 'c':
    case 'D': case 'd':
    case 'E': case 'e':
    case 'F': case 'f':
      fprintf (file, " /|\\  ");
      break;

    case '\n':
      fprintf (file, "\n");
      break;

    default:
      fprintf (file, "    ");
    }   // switch
  }   // for each character in top row

  //
  // Middle row:
  //
  for (i = 0; text[i] != 0; i++)
  {
    switch (text[i])
    {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case 'A': case 'a':
    case 'B': case 'b':
    case 'C': case 'c':
    case 'D': case 'd':
    case 'E': case 'e':
    case 'F': case 'f':
      fprintf (file, "--+-- ");
      break;


    case '\n':
      fprintf (file, "\n");
      break;

    default:
      fprintf (file, " %c  ", text[i]);
    }   // switch
  }   // for each character in middle row

  //
  // Bottom row:
  //
  for (i = 0; text[i] != 0; i++)
  {
    switch (text[i])
    {
    case '0':
    case '4':
    case '8':
    case 'C': case 'c':
      fprintf (file, "  |   ");
      break;

    case '1':
    case '5':
    case '9':
    case 'D': case 'd':
      fprintf (file, " \\|   ");
      break;

    case '2':
    case '6':
    case 'A': case 'a':
    case 'E': case 'e':
      fprintf (file, "  |/  ");
      break;

    case '3':
    case '7':
    case 'B': case 'b':
    case 'F': case 'f':
      fprintf (file, " \\|/  ");
      break;

    case '\n':
      fprintf (file, "\n");
      break;

    default:
      fprintf (file, "    ");
    }   // switch
  }   // for each character in bottom row

}   // Hexal_Diamond_Print

// ---------------------------------------------------------------------------
int Hexon_Print (FILE*            file,
                 const char*      format,
                 const HEXON_TIME time)
{
  unsigned long hexons   = (unsigned long) floor (time->hexon);
  unsigned long hexicles = (unsigned long) floor ((time->hexon - hexons) * (double) HEXICLES_PER_HEXON);
  char text[0x80];

  if (format == NULL) goto Exception;
  else if (strcmp (format, "days"  ) == 0)
  {
    fprintf (file, "%04lX.%04lX\n", hexons / HEXONS_PER_DAY, hexons % HEXONS_PER_DAY);
  }
  else if (strcmp (format, "days+" ) == 0)
  {
    sprintf (text, "%04lX.%04lX\n", hexons / HEXONS_PER_DAY, hexons % HEXONS_PER_DAY);
    Hexal_Seven_Segment_Print (file, text);
  }
  else if (strcmp (format, "days/" ) == 0)
  {
    sprintf (text, "%04lX.%04lX\n", hexons / HEXONS_PER_DAY, hexons % HEXONS_PER_DAY);
    Hexal_Diamond_Print (file, text);
  }
  else if (strcmp (format, "hexons") == 0)
  {
    fprintf (file, "%08lX.%04lX\n", hexons, hexicles);
  }
  else if (strcmp (format, "hexons+") == 0)
  {
    sprintf (text, "%08lX.%04lX\n", hexons, hexicles);
    Hexal_Seven_Segment_Print (file, text);
  }
  else if (strcmp (format, "hexons/") == 0)
  {
    sprintf (text, "%08lX.%04lX\n", hexons, hexicles);
    Hexal_Diamond_Print (file, text);
  }
  else goto Exception;

  return 1;

Exception:
  return 0;
}   // Hexon_Print
