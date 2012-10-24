#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#define PROGRAM_NAME  "hexpaper"

const char* prolog =
"%!PS-Adobe-2.0\n"
"%%Title: graph_paper.ps\n"
"%%Creator: fig2dev Version 3.2 Patchlevel 1\n"
"%%CreationDate: Mon Aug 20 23:42:56 2001\n"
"%%For: rogers@netretu.innocon.com (Doug Rogers,811?,703.893.2007x220,)\n"
"%%Orientation: Portrait\n"
"%%BoundingBox: 34 34 578 758\n"
"%%Pages: 1\n"
"%%BeginSetup\n"
"%%IncludeFeature: *PageSize Letter\n"
"%%EndSetup\n"
"%%Magnification: 1.0000\n"
"%%EndComments\n"
"/$F2psDict 200 dict def\n"
"$F2psDict begin\n"
"$F2psDict /mtrx matrix put\n"
"/col-1 {0 setgray} bind def\n";

const char* post_colordef =
"\n"
"end\n"
"save\n"
"0.0 792.0 translate\n"
"1 -1 scale\n"
"\n"
"/cp {closepath} bind def\n"
"/ef {eofill} bind def\n"
"/gr {grestore} bind def\n"
"/gs {gsave} bind def\n"
"/sa {save} bind def\n"
"/rs {restore} bind def\n"
"/l {lineto} bind def\n"
"/m {moveto} bind def\n"
"/rm {rmoveto} bind def\n"
"/n {newpath} bind def\n"
"/s {stroke} bind def\n"
"/sh {show} bind def\n"
"/slc {setlinecap} bind def\n"
"/slj {setlinejoin} bind def\n"
"/slw {setlinewidth} bind def\n"
"/srgb {setrgbcolor} bind def\n"
"/rot {rotate} bind def\n"
"/sc {scale} bind def\n"
"/sd {setdash} bind def\n"
"/ff {findfont} bind def\n"
"/sf {setfont} bind def\n"
"/scf {scalefont} bind def\n"
"/sw {stringwidth} bind def\n"
"/tr {translate} bind def\n"
"/tnt {dup dup currentrgbcolor\n"
"  4 -2 roll dup 1 exch sub 3 -1 roll mul add\n"
"  4 -2 roll dup 1 exch sub 3 -1 roll mul add\n"
"  4 -2 roll dup 1 exch sub 3 -1 roll mul add srgb}\n"
"  bind def\n"
"/shd {dup dup currentrgbcolor 4 -2 roll mul 4 -2 roll mul\n"
"  4 -2 roll mul srgb} bind def\n"
"/$F2psBegin {$F2psDict begin /$F2psEnteredState save def} def\n"
"/$F2psEnd {$F2psEnteredState restore end} def\n"
"%%EndProlog\n";

const char* begin_code =
"$F2psBegin\n"
"10 setmiterlimit\n"
"n -1000 13622 m -1000 -1000 l 10622 -1000 l 10622 13622 l cp clip\n"
" 0.06000 0.06000 sc\n";

const char* end_code =
"$F2psEnd\n"
"rs\n"
"showpage\n";

#define GRAY(x)   x,x,x

#define DEFAULT_COLOR_10   GRAY(0.50)
#define DEFAULT_COLOR_08   GRAY(0.50)
#define DEFAULT_COLOR_04   GRAY(0.50)
#define DEFAULT_COLOR_02   GRAY(0.75)
#define DEFAULT_COLOR_01   GRAY(0.75)

#define DEFAULT_WIDTH_10  3.0
#define DEFAULT_WIDTH_08  3.0
#define DEFAULT_WIDTH_04  3.0
#define DEFAULT_WIDTH_02  3.0
#define DEFAULT_WIDTH_01  3.0

typedef struct RGB_COLOR_STRUCT
{
  double r;
  double g;
  double b;
}* RGB_COLOR;

typedef struct LINE_DESCRIPTOR_STRUCT
{
  struct RGB_COLOR_STRUCT rgb;
  double                  width;
}* LINE_DESCRIPTOR;

#define MAX_BIT_HANDLED  4
#define MAX_NUM_HANDLED  (1 << MAX_BIT_HANDLED)

struct LINE_DESCRIPTOR_STRUCT line_info[MAX_BIT_HANDLED+1] =
{
  { { DEFAULT_COLOR_01 }, DEFAULT_WIDTH_01 },
  { { DEFAULT_COLOR_02 }, DEFAULT_WIDTH_02 },
  { { DEFAULT_COLOR_04 }, DEFAULT_WIDTH_04 },
  { { DEFAULT_COLOR_08 }, DEFAULT_WIDTH_08 },
  { { DEFAULT_COLOR_10 }, DEFAULT_WIDTH_10 },
};   // line_info

#define DEFAULT_HORIZONTAL_LINES     0xC8
#define DEFAULT_HORIZONTAL_SPACE     56.0
#define DEFAULT_HORIZONTAL_START    1024.0
#define DEFAULT_VERTICAL_LINES       0x90
#define DEFAULT_VERTICAL_SPACE       56.0
#define DEFAULT_VERTICAL_START      1280.0

unsigned int horizontal_lines = DEFAULT_HORIZONTAL_LINES;
double       horizontal_space = DEFAULT_HORIZONTAL_SPACE;
double       horizontal_start = DEFAULT_HORIZONTAL_START;
unsigned int vertical_lines = DEFAULT_VERTICAL_LINES;
double       vertical_space = DEFAULT_VERTICAL_SPACE;
double       vertical_start = DEFAULT_VERTICAL_START;

#define DEFAULT_VERBOSE 0

int verbose = DEFAULT_VERBOSE;

const char* on_off[] = { "off", "on" };

#define Find_Char(s,c)  (const char*) strchr (s, c)


// ---------------------------------------------------------------------------
void Usage (void)
{
  fprintf (stderr, "Usage: " PROGRAM_NAME " [options]\n");
  fprintf (stderr, "\n");
  fprintf (stderr, PROGRAM_NAME " creates hexadecimal-based graph paper. The program allows\n");
  fprintf (stderr, "control of the thickness, color, and spacing of lines of graph paper for\n");
  fprintf (stderr, "powers of two up to 0x10. For example, the color and thickness of each odd line\n");
  fprintf (stderr, "can be set as well as the same properties for each 2nd line, then each 4th\n");
  fprintf (stderr, "line, each 8th line, and finally each 16th line.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Output is PostScript(tm) written to stdout.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Note that the R,G,B weights below are comma-separated floating point values in\n");
  fprintf (stderr, "the range 0.0 to 1.0.\n");
  fprintf (stderr, "\n");
  fprintf (stderr, "Options:\n");
  fprintf (stderr, "  --help, -h       Show this usage information.\n");
  fprintf (stderr, "  --color16=R,G,B  Set RGB weights for each 16th line [%g,%g,%g]\n", DEFAULT_COLOR_10);
  fprintf (stderr, "  --color8=R,G,B   Set RGB weights for each 8th line [%g,%g,%g]\n",  DEFAULT_COLOR_08);
  fprintf (stderr, "  --color4=R,G,B   Set RGB weights for each 4th line [%g,%g,%g]\n",  DEFAULT_COLOR_04);
  fprintf (stderr, "  --color2=R,G,B   Set RGB weights for each 2nd line [%g,%g,%g]\n",  DEFAULT_COLOR_02);
  fprintf (stderr, "  --color1=R,G,B   Set RGB weights for each odd line [%g,%g,%g]\n",  DEFAULT_COLOR_01);
  fprintf (stderr, "  --hlines=N       Set number of horizontal lines [%u]\n",             DEFAULT_HORIZONTAL_LINES);
  fprintf (stderr, "  --hspace=N       Set horizontal line spacing [%g]\n",                DEFAULT_HORIZONTAL_SPACE);
  fprintf (stderr, "  --hstart=N       Set starting position for horizontal lines [%g]\n", DEFAULT_HORIZONTAL_START);
  fprintf (stderr, "  --vlines=N       Set number of vertical lines [%u]\n",             DEFAULT_VERTICAL_LINES);
  fprintf (stderr, "  --vspace=N       Set vertical line spacing [%g]\n",                DEFAULT_VERTICAL_SPACE);
  fprintf (stderr, "  --vstart=N       Set starting position for vertical lines [%g]\n", DEFAULT_VERTICAL_START);
  fprintf (stderr, "  --width16=N      Set width for each 16th line [%g]\n", DEFAULT_WIDTH_10);
  fprintf (stderr, "  --width8=N       Set width for each 8th line [%g]\n",  DEFAULT_WIDTH_08);
  fprintf (stderr, "  --width4=N       Set width for each 4th line [%g]\n",  DEFAULT_WIDTH_04);
  fprintf (stderr, "  --width2=N       Set width for each 2nd line [%g]\n",  DEFAULT_WIDTH_02);
  fprintf (stderr, "  --width1=N       Set width for each odd line [%g]\n",  DEFAULT_WIDTH_01);
  fprintf (stderr, "  --verbose        Display verbose information [%s].\n", on_off[DEFAULT_VERBOSE != 0]);
  exit (1);
}   // Usage

// ---------------------------------------------------------------------------
void Write_Width_And_Get_Color (FILE* file, int line_number, unsigned int *color_id)
{
  static double previous_width = -1.0;
  double width = 0.0;

  static struct RGB_COLOR_STRUCT invalid_rgb = { -1.0, -1.0, -1.0 };
  static RGB_COLOR    previous_rgb = &invalid_rgb;
  RGB_COLOR rgb;

  unsigned int bit_index;
  unsigned int bit;

  //
  // This loop stops one index short intentionally in order to guarantee a
  // valid value of bit_index.
  //
  for (bit_index = 0, bit = 1; bit_index < MAX_BIT_HANDLED; bit_index++, bit <<= 1)
  {
    if ((line_number & bit) != 0x00)
    {
      break;
    }
  }

  rgb   = &line_info[bit_index].rgb;
  width = line_info[bit_index].width;

  if (width != previous_width)
  {
    previous_width = width;
    fprintf (stdout, "%g slw\n", width);
  }

  if (memcmp (rgb, previous_rgb, sizeof (*rgb)) != 0)
  {
    previous_rgb = rgb;
    fprintf (stdout, "col%u\n", bit_index);
  }

  if (color_id != NULL) *color_id = bit_index;
}   // Write_Width_And_Get_Color

// ---------------------------------------------------------------------------
void Set_Color (RGB_COLOR rgb, const char* text)
{
  if ((sscanf (text, "%lg,%lg,%lg", &rgb->r, &rgb->g, &rgb->b) != 3) ||
      (rgb->r < 0.0) || (rgb->r > 1.0) ||
      (rgb->g < 0.0) || (rgb->g > 1.0) ||
      (rgb->b < 0.0) || (rgb->b > 1.0))
  {
    fprintf (stderr, PROGRAM_NAME ": Invalid RGB value \"%s\".\n", text);
    fprintf (stderr, PROGRAM_NAME ":   Values must be comma-separated values between 0.0 and 1.0 inclusive.\n");
    Usage();
  }
}   // Set_Color

// ---------------------------------------------------------------------------
void Set_Int (unsigned int* value, const char* text, const char* name)
{
  if (sscanf (text, "%u", value) != 1)
  {
    fprintf (stderr, PROGRAM_NAME ": Invalid %s value \"%s\"; specify a decimal integer.\n", name, text);
    Usage();
  }
}   // Set_Int

// ---------------------------------------------------------------------------
void Set_Double (double* value, const char* text, const char* name)
{
  if (sscanf (text, "%lg", value) != 1)
  {
    fprintf (stderr, PROGRAM_NAME ": Invalid %s value \"%s\"; specify a floating point value.\n", name, text);
    Usage();
  }
}   // Set_Double

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
void Load_Arguments (int argc, char* argv[])
{
  int i;

  for (i = 1; i < argc; i++)
  {
    char* argument = argv[i];

    if (*argument == '-') argument++;
    if (*argument == '-') argument++;
    if ((strcmp (argument, "help") == 0) || (strcmp (argument, "-h") == 0)) Usage();
    else if (strequ (argument, "color16=" )) Set_Color  (&line_info[4].rgb, Find_Char (argument, '=') + 1);
    else if (strequ (argument, "color8="  )) Set_Color  (&line_info[3].rgb, Find_Char (argument, '=') + 1);
    else if (strequ (argument, "color4="  )) Set_Color  (&line_info[2].rgb, Find_Char (argument, '=') + 1);
    else if (strequ (argument, "color2="  )) Set_Color  (&line_info[1].rgb, Find_Char (argument, '=') + 1);
    else if (strequ (argument, "color1="  )) Set_Color  (&line_info[0].rgb, Find_Char (argument, '=') + 1);
    else if (strequ (argument, "hlines="  )) Set_Int    (&horizontal_lines, Find_Char (argument, '=') + 1, "hlines");
    else if (strequ (argument, "hspace="  )) Set_Double (&horizontal_space, Find_Char (argument, '=') + 1, "hspace");
    else if (strequ (argument, "hstart="  )) Set_Double (&horizontal_start, Find_Char (argument, '=') + 1, "hstart");
    else if (strequ (argument, "vlines="  )) Set_Int    (&vertical_lines,   Find_Char (argument, '=') + 1, "vlines");
    else if (strequ (argument, "vspace="  )) Set_Double (&vertical_space,   Find_Char (argument, '=') + 1, "vspace");
    else if (strequ (argument, "vstart="  )) Set_Double (&vertical_start,   Find_Char (argument, '=') + 1, "vstart");
    else if (strequ (argument, "width16=" )) Set_Double (&line_info[4].width, Find_Char (argument, '=') + 1, "width10");
    else if (strequ (argument, "width8="  )) Set_Double (&line_info[3].width, Find_Char (argument, '=') + 1, "width08");
    else if (strequ (argument, "width4="  )) Set_Double (&line_info[2].width, Find_Char (argument, '=') + 1, "width04");
    else if (strequ (argument, "width2="  )) Set_Double (&line_info[1].width, Find_Char (argument, '=') + 1, "width02");
    else if (strequ (argument, "width1="  )) Set_Double (&line_info[0].width, Find_Char (argument, '=') + 1, "width01");
    else if (strequ (argument, "verbose")) verbose = 1;
    else
    {
      fprintf (stderr, PROGRAM_NAME ": *** error: unknown option \"%s\".\n\n", argument);
      Usage();
    }
  }
}   // Load_Arguments

// ---------------------------------------------------------------------------
int main (int argc, char* argv[])
{
  int i;
  double stop;
  unsigned int color_id;

  Load_Arguments (argc, argv);

  fputs (prolog, stdout);

  for (i = 0; i <= MAX_BIT_HANDLED; i++)
  {
    LINE_DESCRIPTOR line = &line_info[i];
    fprintf (stdout, "/col%u {%g %g %g srgb} bind def\n", i, line->rgb.r, line->rgb.g, line->rgb.b);
  }

  fputs (post_colordef, stdout);
  fputs (begin_code, stdout);

  //
  // Vertical lines:
  //
  stop = horizontal_start + (horizontal_space * horizontal_lines);

  for (i = 0; i <= vertical_lines; i++)
  {
    Write_Width_And_Get_Color (stdout, i, &color_id);

    fprintf (stdout, "n %g %g m %g %g l cp gs col%u s gr \n",
             vertical_start + (vertical_space * i), horizontal_start,
             vertical_start + (vertical_space * i), stop,
             color_id);
  }   // for each vertical line.

  //
  // Horizontal lines:
  //
  stop = vertical_start + (vertical_space * vertical_lines);

  for (i = 0; i <= horizontal_lines; i++)
  {
    Write_Width_And_Get_Color (stdout, i, &color_id);

    fprintf (stdout, "n %g %g m %g %g l cp gs col%u s gr \n",
             vertical_start, horizontal_start + (horizontal_space * i),
             stop,           horizontal_start + (horizontal_space * i),
             color_id);
  }   // for each horizontal line.

  fputs (end_code, stdout);

  return 0;
}   // main
