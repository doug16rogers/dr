#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define PROGRAM_NAME "hexflash"

#define DEFAULT_A_FIRST 0x2
#define DEFAULT_A_LAST  0xF

#define DEFAULT_B_FIRST 0x8
#define DEFAULT_B_LAST  0xF

int a_first = DEFAULT_A_FIRST;
int a_last  = DEFAULT_A_LAST;

int b_first = DEFAULT_B_FIRST;
int b_last  = DEFAULT_B_LAST;

// ---------------------------------------------------------------------------
void Usage (int exit_code)
{
  printf ("Usage: " PROGRAM_NAME " [options]\n");
  printf ("\n");
  printf ("Options:\n");
  printf ("  -a<range>   Range for first multiplicand [%X%X]\n",
          DEFAULT_A_FIRST, DEFAULT_A_LAST);
  printf ("  -b<range>   Range for second multiplicand [%X%X]\n",
          DEFAULT_B_FIRST, DEFAULT_B_LAST);
  printf ("  -h          Show usage information.\n");
  printf ("  -s<hex>     Hexadecimal random seed [randomized on time]\n");
  printf ("\n");
  printf ("Ranges are <F:L> where 'F' and 'L' are the first and last hex values\n");
  printf ("to be used in the flashcards being presented.\n");
  printf ("\n");
  printf ("For example, '" PROGRAM_NAME " -aD:D' would generate flashcards for\n");
  printf ("the D's table, whereas '" PROGRAM_NAME " -aD:E -bD:E' would generate\n");
  printf ("cards only for DxD, DxE, ExD, and ExE.\n");

  exit (exit_code);
}    // Usage

// ---------------------------------------------------------------------------
void Load_Arguments (int argc, char* argv[])
{
  int i;

  for (i = 1; i < argc; i++)
  {
    char* argument = argv[i];

    if (*argument != '-')
    {
      fprintf (stderr, PROGRAM_NAME ": all arguments must be options (\"%s\").\n\n", argument);
      Usage (1);
    }

    argument++;
    if (*argument == '-') argument++;  // allow --option

    switch (*argument)
    {
    case 'a':
      argument++;

      if ((sscanf (argument, "%x:%x", &a_first, &a_last) != 2) ||
          (a_first < 0) || (a_first > 0xf) ||
          (a_last  < 0) || (a_last  > 0xf) ||
          (a_first > a_last))
      {
        fprintf (stderr, PROGRAM_NAME ": bad hex range (\"%s\").\n\n", argument);
        Usage (1);
      }
      break;

    case 'b':
      argument++;

      if ((sscanf (argument, "%x:%x", &b_first, &b_last) != 2) ||
          (b_first < 0) || (b_first > 0xf) ||
          (b_last  < 0) || (b_last  > 0xf) ||
          (b_first > b_last))
      {
        fprintf (stderr, PROGRAM_NAME ": bad hex range (\"%s\").\n\n", argument);
        Usage (1);
      }
      break;

    case 's':
      argument++;

      {
        int seed;

        if (sscanf (argument, "%x", &seed) != 1)
        {
          fprintf (stderr, PROGRAM_NAME ": bad hex seed (\"%s\").\n\n", argument);
          Usage (1);
        }

        srand (seed);
      }
      break;

    case 'h':
      Usage (1);
      break;

    default:
      fprintf (stderr, PROGRAM_NAME ": unknown option \"-%s\".\n\n", argument);
      Usage (1);
    }
  }
}    // Load_Arguments

// ---------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  struct timeval now;
  struct timezone zone;

  gettimeofday (&now, &zone);
  srand (now.tv_usec >> 16);

  Load_Arguments (argc, argv);

  while (1)
  {
    int a = a_first + (rand() % (a_last + 1 - a_first));
    int b = b_first + (rand() % (b_last + 1 - b_first));
    int c;

    printf ("  %X x %X  = ", a, b);
    if (scanf ("%x", &c) != 1) break;

    while (c != (a * b))
    {
      printf ("Sorry. %X x %X = %X. Now type it.\n", a, b, a * b);
      printf ("  %X x %X  = ", a, b);
      if (scanf ("%x", &c) != 1) break;
    }
  }

  printf ("\n");

  return 0;
}   // main
