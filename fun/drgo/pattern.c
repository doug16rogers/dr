#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "pattern.h"


typedef struct TEXT_PATTERN_STRUCT
{
  int  rows;
  int  columns;
  char letter[MAX_PATTERN_SIZE][MAX_PATTERN_SIZE];
  int  positions;
  int  row[MAX_PATTERN_POSITIONS];
  int  col[MAX_PATTERN_POSITIONS];
  double value[MAX_PATTERN_POSITIONS];
}* TEXT_PATTERN;

//
// Rotation/flip patterns for row and column:
//
int delta_positions_initialized = 0;

#define PATTERN_PERMUTATIONS 8
int delta_from_row[PATTERN_PERMUTATIONS][MAX_PATTERN_GRID_SIZE] = { { 0 } };
int delta_from_col[PATTERN_PERMUTATIONS][MAX_PATTERN_GRID_SIZE] = { { 0 } };
int pattern_verbose = 0;

// --------------------------------------------------------------------------
// Get_Line:
//
static char* Get_Line (FILE* file, int* line_number)
{
  static char line[0x100] = "\0";
  
  if (fgets (line, sizeof(line), file) == NULL) return NULL;
  line[sizeof(line) - 1] = '\0';
  (*line_number)++;
  
  return line;
}   // Get_Line

// --------------------------------------------------------------------------
// Pattern_Load:
//
static TEXT_PATTERN Pattern_Load (FILE*            file,
                                  const char*      filename,
                                  int*             line_number,
                                  TEXT_PATTERN pattern)
{
  const char NUL = '\0';
  const char* comment_characters = "#;!$/";
  const char* valid_letters = "-Ee+XxBbWw123456789";
  
  char* s = NULL;
  int i = 0;
  int row = 0;
  int column = 0;
  
  //
  // First find start of pattern. If no start, return NULL without error
  // message.
  //
  while (!feof (file))
  {
    if ((s = Get_Line (file, line_number)) == NULL)
    {
      return NULL;
    }
    
    for (; *s != NUL; s++)
    {
      if (!isspace (*s)) break;
    }
  
    //
    // Check for non-comment line.
    //
    if ((*s != '\0') && (strchr (comment_characters, *s) == NULL))
    {
      break;
    }
  }   // while looking for start of pattern

  if (sscanf (s, "%d %d", &pattern->rows, &pattern->columns) != 2)
  {
    fprintf (stderr, "%s:%d: bad pattern start \"%s\"; should be two decimal numbers\n",
                     filename, *line_number, s);
    return NULL;
  }
  
  if ((pattern->rows    < 1) || (pattern->rows    > MAX_PATTERN_SIZE) ||
      (pattern->columns < 1) || (pattern->columns > MAX_PATTERN_SIZE))
  {
    fprintf (stderr, "%s:%d: pattern size %dx%d bad; should be 1x1 to %dx%d\n",
                     filename, *line_number, pattern->rows, pattern->columns, MAX_PATTERN_SIZE, MAX_PATTERN_SIZE);
    return NULL;
  }
  
  pattern->positions = 0;
  
  for (i = 0; i < MAX_PATTERN_POSITIONS; i++)
  {
    pattern->row[i] = -1;
    pattern->col[i] = -1;
  }
  
  for (row = 0; row < pattern->rows; row++)
  {
    if ((s = Get_Line (file, line_number)) == NULL)
    {
      fprintf (stderr, "%s:%d: expected %d rows of pattern data, found only %d\n",
                       filename, *line_number, pattern->rows, row);
      return NULL;
    }
    
    for (column = 0; column < pattern->columns; column++)
    {
      while ((*s != NUL) && isspace (*s))
      {
        s++;
      }
      
      if (*s == NUL)
      {
        fprintf (stderr, "%s:%d: expected %d columns of pattern data for row %d, found only %d\n",
                         filename, *line_number, pattern->columns, row, column);
        return NULL;
      }
      
      if (strchr (valid_letters, *s) == NULL)
      {
        fprintf (stderr, "%s:%d: invalid pattern entry '%c' for row %d, column %d\n",
                         filename, *line_number, *s, row, column);
        return NULL;
      }
      
      pattern->letter[row][column] = *s;
      
      if ((*s >= '1') && (*s <= ('0' + MAX_PATTERN_POSITIONS)))
      {
        int position = *s - '0';
        
        pattern->row[position - 1] = row;
        pattern->col[position - 1] = column;
        
        if (position > pattern->positions)
        {
          pattern->positions = position;
        }
      }   // if loading a position letter
      
      s++;
    }   // for each column
  }   // for each row
  
  
  if ((s = Get_Line (file, line_number)) == NULL)
  {
    fprintf (stderr, "%s:%d: file ended trying to read position weights\n", filename, *line_number);
    return NULL;
  }
  
  for (i = 0; i < pattern->positions; i++)
  {
    if ((pattern->row[i] < 0) || (pattern->row[i] >= pattern->rows) ||
        (pattern->row[i] < 0) || (pattern->col[i] >= pattern->columns))
    {
      fprintf (stderr, "%s:%d: expected %d position indicators, but '%c' was missing\n",
                       filename, *line_number, pattern->positions, '1' + i);
      return NULL;
    }
    
    while ((*s != NUL) && isspace (*s))
    {
      s++;
    }
    
    if (*s == NUL)
    {
      fprintf (stderr, "%s:%d: expected %d position values; found only %d\n",
                       filename, *line_number, pattern->positions, i);
      return NULL;
    }
    
    if (sscanf (s, "%lg", &pattern->value[i]) != 1)
    {
      fprintf (stderr, "%s:%d: invalid floating point position value \"%s\"\n",
                       filename, *line_number, s);
      return NULL;
    }
    
    while ((*s != NUL) && !isspace (*s))
    {
      s++;
    }
  }   // for each position value
  
  return pattern;
}   // Pattern_Load

// --------------------------------------------------------------------------
// Initialize_Deltas:
//
static void Initialize_Deltas (int* delta_row,
                               int* delta_col,
                               int  row_sign,
                               int  col_sign)
{
  int row = 0;
  int col = 0;
  int i = 0;
  
  delta_positions_initialized = 1;
  
  //
  // Get middle position.
  //
  *delta_row++ = 0;
  *delta_col++ = 0;
  
  for (i = 1; i <= MAX_ROWS_ABOVE; i++)
  {
    //
    // Get row along top.
    //
    row = -i;
    for (col = -i; col <= +i; col++)
    {
      *delta_row++ = row_sign * row;
      *delta_col++ = col_sign * col;
    }
    
    //
    // Get two columns along each row in between.
    //
    for (row = (-i) + 1; row <= ((+i) - 1); row++)
    {
      col = -i;
      *delta_row++ = row_sign * row;
      *delta_col++ = col_sign * col;
      
      col = +i;
      *delta_row++ = row_sign * row;
      *delta_col++ = col_sign * col;
    }
    
    //
    // Get row along bottom.
    //
    row = +i;
    for (col = -i; col <= +i; col++)
    {
      *delta_row++ = row_sign * row;
      *delta_col++ = col_sign * col;
    }
  }   // for each ring around the center position
}   // Initialize_Deltas

// --------------------------------------------------------------------------
// Initialize_Delta_Positions:
//
static void Initialize_Delta_Positions ()
{
  if (!delta_positions_initialized)
  {
    delta_positions_initialized = 1;
    
    //
    // Initialize the 8 different symmetries of a go board.
    //
    Initialize_Deltas (delta_from_row[0], delta_from_col[0],  1,  1);
    Initialize_Deltas (delta_from_row[1], delta_from_col[1],  1, -1);
    Initialize_Deltas (delta_from_row[2], delta_from_col[2], -1,  1);
    Initialize_Deltas (delta_from_row[3], delta_from_col[3], -1, -1);
    Initialize_Deltas (delta_from_col[4], delta_from_row[4],  1,  1);
    Initialize_Deltas (delta_from_col[5], delta_from_row[5],  1, -1);
    Initialize_Deltas (delta_from_col[6], delta_from_row[6], -1,  1);
    Initialize_Deltas (delta_from_col[7], delta_from_row[7], -1, -1);
  }
}   // Initialize_Delta_Positions

// --------------------------------------------------------------------------
// Text_To_Mask:
//
MASK_PATTERN Text_To_Mask (const TEXT_PATTERN text_pattern,
                           int                position,
                           int                delta_row[],
                           int                delta_col[],
                           MASK_PATTERN       pattern)
{
  int d = 0;
  
  pattern->value  = text_pattern->value[position];
  
  for (d = 0; d < MAX_PATTERN_GRID_SIZE; d++)
  {
    int row = text_pattern->row[position] + delta_row[d];
    int col = text_pattern->col[position] + delta_col[d];
    
    if ((row < 0) || (row >= text_pattern->rows) ||
        (col < 0) || (col >= text_pattern->columns))
    {
      pattern->mask[d] = PATTERN_ANY;   // when off board, don't care - match everything
    }
    else
    {
      switch (text_pattern->letter[row][col])
      {
      case 'E': pattern->mask[d] = PATTERN_EDGE;                   break;
      case 'e': pattern->mask[d] = PATTERN_EDGE  | PATTERN_EMPTY;  break;
      case 'B': pattern->mask[d] = PATTERN_BLACK;                  break;
      case 'b': pattern->mask[d] = PATTERN_BLACK | PATTERN_EMPTY;  break;
      case 'W': pattern->mask[d] = PATTERN_WHITE;                  break;
      case 'w': pattern->mask[d] = PATTERN_WHITE | PATTERN_EMPTY;  break;
      case '-': pattern->mask[d] = PATTERN_EMPTY; break;
      case '+': pattern->mask[d] = PATTERN_EMPTY; break;
      case 'X': pattern->mask[d] = PATTERN_ANY;   break;
      case 'x': pattern->mask[d] = PATTERN_ANY;   break;
      
      default:
        if ((text_pattern->letter[row][col] >= '1') &&
            (text_pattern->letter[row][col] <= ('0' + MAX_PATTERN_POSITIONS)))
        {
          pattern->mask[d] = PATTERN_EMPTY;   // use empty for numbered positions
        }
        else
        {
          fprintf (stderr, "illegal letter '%c' when converting to mask pattern\n", text_pattern->letter[row][col]);
          return NULL;
        }
      }   // switch on entry
    }   // else we're in the pattern
  }   // for each delta mapping pattern onto spiral
  
  //
  // Now find length of mask pattern - the number of masks such that the
  // remainder are don't-cares.
  //
  for (d = MAX_PATTERN_GRID_SIZE; d > 0; d--)
  {
    if (pattern->mask[d - 1] != PATTERN_ANY)
    {
      break;
    }
  }
  
  pattern->length = d;
  
  return pattern;
}   // Text_To_Mask

// --------------------------------------------------------------------------
// Text_Pattern_Show:
//
static void Text_Pattern_Show (FILE* file, const TEXT_PATTERN pattern)
{
  int row = 0;
  int col = 0;
  int i = 0;
  
  fprintf (file, "%d %d\n", pattern->rows, pattern->columns);
  
  for (row = 0; row < pattern->rows; row++)
  {
    for (col = 0; col < pattern->columns; col++)
    {
      fprintf (file, "%c ", pattern->letter[row][col]);
    }
    
    fprintf (file, "\n");
  }
  
  for (i = 0; i < pattern->positions; i++)
  {
    fprintf (file, "%g ", pattern->value[i]);
  }
  
  fprintf (file, "\n");
}   // Text_Pattern_Show

// --------------------------------------------------------------------------
// Mask_Pattern_Show:
//
void Mask_Pattern_Show (FILE* file, const MASK_PATTERN pattern)
{
  int i;
  
  fprintf (file, "%2d %6.3f ", pattern->length, pattern->value);
  
  for (i = 0; i < pattern->length; i++)
  {
    fprintf (file, "%x", (int) pattern->mask[i]);
  }
  
  fprintf (file, "\n");
}   // Mask_Pattern_Show

// --------------------------------------------------------------------------
// Pattern_Load_Text_File:
//
int Pattern_Load_Text_File (struct MASK_PATTERN_STRUCT* list,
                            int                         list_length,
                            const char*                 filename)
{
  FILE* file = NULL;
  struct TEXT_PATTERN_STRUCT text_pattern = { 0 };
  struct MASK_PATTERN_STRUCT mask_pattern = { 0 };
  int line_number = 0;
  int patterns_loaded = 0;
  int mask_patterns_loaded;
  int count = 0;
  int i;
  
  Initialize_Delta_Positions ();
  
  file = fopen (filename, "rt");
  
  if (file == NULL)
  {
    return 0;
  }
  
  while ((mask_patterns_loaded < list_length) && !feof (file))
  {
    if (Pattern_Load (file, filename, &line_number, &text_pattern) == NULL)
    {
      break;
    }
    
    patterns_loaded++;
    
    if (pattern_verbose)
    {
      printf ("\n#####################################################################\n"
              "Pattern #%d:\n", patterns_loaded);
      Text_Pattern_Show (stdout, &text_pattern);
    }
    
    for (i = 0; i < text_pattern.positions; i++)
    {
      int permutation = 0;
      
      if (pattern_verbose)
      {
        printf ("------ Position %d ------\n", i + 1);
      }
      
      for (permutation = 0; permutation < PATTERN_PERMUTATIONS; permutation++)
      {
        Text_To_Mask (&text_pattern, i, delta_from_row[permutation], delta_from_col[permutation], &mask_pattern);
        
        if (pattern_verbose)
        {
          printf ("%d: ", permutation);
          Mask_Pattern_Show (stdout, &mask_pattern);
        }
        
        if (count < list_length) list[count++] = mask_pattern;
      }   // for each permutation of a go board's coordinates
    }   // for each position in this pattern
  }   // while there's room to load the patterns and the file isn't empty
  
  fclose (file);
  
  return count;
}   // Pattern_Load_Text_File

// --------------------------------------------------------------------------
// Pattern_Value:
//  Returns the value of the given pattern if it matches the given mask.
//  Returns 0.0 if the pattern does not match.
//
double Pattern_Value (const MASK_PATTERN board_position,
                      const MASK_PATTERN pattern)
{
  int length = board_position->length < pattern->length ? board_position->length : pattern->length;
  int i = 0;
  
  for (i = 0; i < length; i++)
  {
    if ((board_position->mask[i] & pattern->mask[i]) == 0x00)
    {
      return 0.0;
    }
  }
  
  return pattern->value;
}   // Pattern_Value

