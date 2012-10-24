#ifndef __pattern_h__
#define __pattern_h__


#define MAX_ROWS_ABOVE             4
#define MAX_PATTERN_SIZE           ((2 * MAX_ROWS_ABOVE) + 1)
#define MAX_PATTERN_GRID_SIZE      (MAX_PATTERN_SIZE * MAX_PATTERN_SIZE)
#define MAX_PATTERN_POSITIONS      9

#define PATTERN_EMPTY  0x01
#define PATTERN_BLACK  0x02
#define PATTERN_WHITE  0x04
#define PATTERN_EDGE   0x08

#define PATTERN_ANY      (PATTERN_EMPTY | PATTERN_BLACK | PATTERN_WHITE | PATTERN_EDGE)
#define PATTERN_NONEMPTY (PATTERN_BLACK | PATTERN_WHITE)



typedef struct MASK_PATTERN_STRUCT
{
  int           length;     // number of masks before rest are don't-cares
  double        value;      // pattern value
  unsigned char mask[MAX_PATTERN_GRID_SIZE];
}* MASK_PATTERN;

#define PATTERN_PERMUTATIONS 8
extern int delta_from_row[PATTERN_PERMUTATIONS][MAX_PATTERN_GRID_SIZE];
extern int delta_from_col[PATTERN_PERMUTATIONS][MAX_PATTERN_GRID_SIZE];
extern int pattern_verbose;   // controls whether pattern info is dumped when loaded

// --------------------------------------------------------------------------
// Pattern_Value:
//  Returns the value of the given pattern if it matches the given mask.
//  Returns 0.0 if the pattern does not match.
//
double Pattern_Value (const MASK_PATTERN board_position,
                      const MASK_PATTERN pattern);

// --------------------------------------------------------------------------
// Mask_Pattern_Show:
//  Displays the given mask pattern to the given file.
//
void Mask_Pattern_Show (FILE* file, const MASK_PATTERN pattern);

// --------------------------------------------------------------------------
// Pattern_Load_Text_File:
//  Loads a set of patterns from the given text file into the list of
//  the given length as mask patterns. Returns the number of mask patterns
//  loaded (they include all rotations and flips).
//
int Pattern_Load_Text_File (struct MASK_PATTERN_STRUCT* list,
                            int                         list_length,
                            const char*                 filename);

#endif
