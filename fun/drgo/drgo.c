// -----------------------------------------------------------------------
// drgo:
//  Plays go.
//
//  This program uses Bill Shubert's Go Modem Protocol. See "gmp.h".
//
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gmp.h"
#include "go_board.h"
// #include "go_game.h"
#include "pattern.h"

const char* lettername = "ABCDEFGHJKLMNOPQRSTUVWXYZ";
const char* black_debug_file = "/home/rogers/Projects/Go/C/debug-black.out";
const char* white_debug_file = "/home/rogers/Projects/Go/C/debug-white.out";

const char* pattern_filename = "patterns.txt";
const char* joseki_filename  = "joseki.txt";

int main_seconds    = 3600;
int byoyomi_seconds = 3600;


char pattern_file[0x100]     = "/home/rogers/Projects/Go/C/patterns.txt";
char joseki_file[0x100]      = "/home/rogers/Projects/Go/C/joseki.txt";

char program_path[0x100] = "./";

#define MAX_PATTERNS 0x1000
struct MASK_PATTERN_STRUCT pattern_list[MAX_PATTERNS];
int pattern_count = 0;
struct MASK_PATTERN_STRUCT joseki_list[MAX_PATTERNS];
int joseki_count = 0;

FILE* debug = NULL;
int  passes = 0;

void waitForNewGame (Gmp* ge);
void playGame(Gmp *ge, int size, int handicap, int himFirst);

#define NO_SUITABLE_MOVE 0.001

// -----------------------------------------------------------------------
// stradd:
//  Adds two strings to the destination of the given length.
//
void stradd (char*       target,
             int         length,
             const char* source1,
             const char* source2)
{
  length--;   // leaves room for terminating NUL character
  
  while ((length > 0) && (*source1 != '\0'))
  {
    *target++ = *source1++;
    length--;
  }
  
  while ((length > 0) && (*source2 != '\0'))
  {
    *target++ = *source2++;
    length--;
  }
  
  *target = 0;
}   // stradd

// -----------------------------------------------------------------------
// Load_Arguments:
//
void Load_Arguments (int argc, char* argv[])
{
  int i;
  char *s;
  
  strncpy (program_path, argv[0], sizeof (program_path));
  program_path[sizeof (program_path) - 1] = '\0';
  s = strrchr (program_path, '/');
  
  if (s != NULL)
  {
    *(s+1) = '\0';
  }
  
  strcpy (pattern_file, program_path);
  strcat (pattern_file, pattern_filename);
  
  strcpy (pattern_file, program_path);
  strcat (pattern_file, pattern_filename);
  
  for (i = 1; i < (argc - 1); i += 2)     // all arguments must be option-argument pairs
  {
    char* arg = argv[i];
    
    if (arg[0] != '-')
    {
      fprintf (stderr, "drgo: no non-switch arguments (\"%s\") allowed.\n", arg);
      exit (1);
    }
    
    if ((arg[1] == 'B') || (strcmp (arg, "--byoyomi") == 0))
    {
    }
    else if ((arg[1] == 'M') || (strcmp (arg, "--main") == 0))
    {
    }
    else if ((arg[1] == 'p') || (strcmp (arg, "--patternfile") == 0))
    {
      strncpy (pattern_file, argv[i+1], sizeof (pattern_file));
      pattern_file[sizeof (pattern_file) - 1] = '\0';
    }
    else if ((arg[1] == 'j') || (strcmp (arg, "--josekifile") == 0))
    {
      strncpy (joseki_file, argv[i+1], sizeof (joseki_file));
      joseki_file[sizeof (joseki_file) - 1] = '\0';
    }
  }   // while
}   // Load_Arguments

// -----------------------------------------------------------------------
// main:
//  The main program parses commandline arguments and starts a game.
//
int main (int argc, char *argv[])
{
  Gmp *gmp;
  
  Load_Arguments (argc, argv);
  
  joseki_count  = Pattern_Load_Text_File (joseki_list,  MAX_PATTERNS, joseki_file);
  pattern_count = Pattern_Load_Text_File (pattern_list, MAX_PATTERNS, pattern_file);
  
  srand(time(NULL));
  gmp = gmp_create(0, 1);

  while (1)
  {
    gmp_startGame (gmp, -1, -1, 5.5, -1, -1);  // This allows the other side to set the parameters.
    waitForNewGame(gmp);
    
    if (gmp_handicap(gmp) > 9)
    {
      fprintf(stderr, "I don't know how to play handicap games greater than 9.\n");
      exit(1);
    }
    
    playGame(gmp, gmp_size(gmp), gmp_handicap(gmp), gmp_iAmWhite(gmp));
  }
  
  fclose (debug);
  exit(0);
}   // main

// -----------------------------------------------------------------------
// waitForNewGame:
//  Waits for the other size to start the game.
//
void waitForNewGame(Gmp *ge)
{
  GmpResult  result;
  const char  *error;

  do
  {
    result = gmp_check(ge, 1, NULL, NULL, &error);
  } while ((result == gmp_nothing) || (result == gmp_reset));
  
  if (result == gmp_err)
  {
    fprintf(stderr, "dummy: Error \"%s\" occurred.\n", error);
    exit(1);
  } else if (result != gmp_newGame)
  {
    fprintf(stderr, "Expecting a newGame, got %s\n",
	    gmp_resultString(result));
    exit(1);
  }
}   // waitForNewGame

#if 0
// -----------------------------------------------------------------------
// Show_Snapshot:
//
static void Show_Snapshot (const GO_BOARD_SNAPSHOT shot)
{
  const char piece_name[GO_PIECES] = "-BW";
  int i;
  
  fprintf (debug, "Snapshot for %dx%d board.\n", shot->size, shot->size);
  
  for (i = 0; i < shot->size * shot->size; i++)
  {
    fprintf (debug, "%c", piece_name[shot->piece[i]]);

    if ((i % shot->size) == (shot->size - 1))
    {
      fprintf (debug, "\n");
    }
  }
  
  fprintf (debug, "Ko is at %d,%d\n", shot->ko_x, shot->ko_y);
  fprintf (debug, "Captured White: %d\n", shot->captured_white);
  fprintf (debug, "Captured Black: %d\n", shot->captured_black);
  fprintf (debug, "Passes:         %d\n", shot->passes);
  fprintf (debug, "Next to play:   %c\n", piece_name[shot->to_play]);
}   // Show_Snapshot
#endif

typedef enum
{
  NUM_CAPTURED,
  NUM_STRINGS,
  NUM_STONES,
  NUM_TOTAL_LIBERTIES,
  NUM_ONE_LIBERTY_STRINGS,
  NUM_TWO_LIBERTY_STRINGS,
  NUM_TRHEE_LIBERTY_STRINGS,
  NUM_FOUR_LIBERTY_STRINGS,
  
  STATE_VARIABLES
} STATE_VARIABLE;

typedef struct SCORING_STATE_STRUCT
{
  double my_captured;
  double my_strings;
  double my_stones;
  double my_liberties;
  double my_one_liberties;
  double my_two_liberties;
  double my_three_liberties;
  
  double other_captured;
  double other_strings;
  double other_stones;
  double other_liberties;
  double other_one_liberties;
  double other_two_liberties;
  double other_three_liberties;

  double empty_strings;
}* SCORING_STATE;

// -----------------------------------------------------------------------
// Delta_Scoring_State:
//
static void Delta_Scoring_State (const SCORING_STATE previous,
                                 const SCORING_STATE current,
                                       SCORING_STATE delta)
{
  delta->my_captured           = current->my_captured           - previous->my_captured;
  delta->my_strings            = current->my_strings            - previous->my_strings;
  delta->my_stones             = current->my_stones             - previous->my_stones;
  delta->my_liberties          = current->my_liberties          - previous->my_liberties;
  delta->my_one_liberties      = current->my_one_liberties      - previous->my_one_liberties;
  delta->my_two_liberties      = current->my_two_liberties      - previous->my_two_liberties;
  delta->my_three_liberties    = current->my_three_liberties    - previous->my_three_liberties;
  delta->other_captured        = current->other_captured        - previous->other_captured;
  delta->other_strings         = current->other_strings         - previous->other_strings;
  delta->other_stones          = current->other_stones          - previous->other_stones;
  delta->other_liberties       = current->other_liberties       - previous->other_liberties;
  delta->other_one_liberties   = current->other_one_liberties   - previous->other_one_liberties;
  delta->other_two_liberties   = current->other_two_liberties   - previous->other_two_liberties;
  delta->other_three_liberties = current->other_three_liberties - previous->other_three_liberties;
  delta->empty_strings         = current->empty_strings         - previous->empty_strings;
}   // Delta_Scoring_State

// -----------------------------------------------------------------------
// Get_Scoring_State:
//
static void Get_Scoring_State (const GO_BOARD board,
                               GO_PIECE       my_stone,
                               SCORING_STATE  state)
{
  //
  // This stuff uses GO_BOARD internals.
  //

  GO_PIECE other_stone = BLACK;
  int i;
  
  if (my_stone == BLACK)
  {
    other_stone = WHITE;
  }
  
  Delta_Scoring_State (state, state, state);  // clears state
  
  state->my_captured    = board->captured[my_stone];
  state->other_captured = board->captured[other_stone];
  
  state->my_strings    = board->colored_strings[my_stone];
  state->other_strings = board->colored_strings[other_stone];
  state->empty_strings = board->colored_strings[EMPTY];
  
  for (i = 0; i < board->strings; i++)
  {
    GO_STRING string = &board->string[i];
    int liberties = string->liberties;
    int stones = string->stones;
    int one_liberty   = (liberties == 1);
    int two_liberties = (liberties == 2);
    int three_liberties = (liberties == 3);
    
    if (string->color == my_stone)
    {
      state->my_stones += stones;
      state->my_liberties += liberties;
      state->my_one_liberties += one_liberty;
      state->my_two_liberties += two_liberties;
      state->my_three_liberties += three_liberties;
    }
    else if (string->color == other_stone)
    {
      state->other_stones += stones;
      state->other_liberties += liberties;
      state->other_one_liberties += one_liberty;
      state->other_two_liberties += two_liberties;
      state->other_three_liberties += three_liberties;
    }
    else   // empty string
    {
      ;
    }
  }   // for each string on the board
}   // Get_Scoring_State

// -----------------------------------------------------------------------
// Pattern_Score:
//
double Pattern_Score (const MASK_PATTERN position_pattern, struct MASK_PATTERN_STRUCT list[], int count)
{
  int i = 0;
  double score = 0.0;
  int patterns_matched = 0;
  
  for (i = 0; i < count; i++)
  {
    double pattern_score = Pattern_Value (position_pattern, &list[i]);
    
    if (pattern_score > 0.0)
    {
      score += pattern_score;
      patterns_matched++;
    }
  }
  
  if (patterns_matched <= 0)
  {
    return 0.0;
  }
  
  return score; // / (double) patterns_matched;
}   // Pattern_Score

// -----------------------------------------------------------------------
// Build_Board_Pattern:
//
void Build_Board_Pattern (const GO_BOARD board, int move_x, int move_y, GO_PIECE my_stone, MASK_PATTERN pattern)
{
  int d = 0;
  int* delta_row = NULL;
  int* delta_col = NULL;
  //
  // Now build the pattern.
  //
  delta_row = delta_from_row[0];
  delta_col = delta_from_col[0];
  
  pattern->length = MAX_PATTERN_GRID_SIZE;
  
  for (d = 0; d < MAX_PATTERN_GRID_SIZE; d++)
  {
    int x = move_x + delta_col[d];
    int y = move_y + delta_row[d];
    
    if ((x < 0) || (x >= board->size) || (y < 0) || (y >= board->size))
    {
      pattern->mask[d] = PATTERN_EDGE;
    }
    else
    {
      GO_PIECE stone = board->position[LOCATION_INDEX(board,x,y)].piece;
      
      if (stone == EMPTY)
      {
        pattern->mask[d] = PATTERN_EMPTY;
      }
      else if (stone == my_stone)
      {
        pattern->mask[d] = PATTERN_WHITE;   // White is the pattern's point-of-view
      }
      else
      {
        pattern->mask[d] = PATTERN_BLACK;
      }
    }
  }   // for each concentric pattern position
}   // Build_Board_Pattern

// -----------------------------------------------------------------------
// Score_Move:
//
static double Score_Move (const GO_BOARD board,
                          GO_PIECE       my_stone,
                          int            move_number,
                          int            x,
                          int            y,
                          const SCORING_STATE previous,
                          SCORING_STATE       current)
{
  struct SCORING_STATE_STRUCT delta = { 0 };
  double score = 0.0;
  struct MASK_PATTERN_STRUCT position_pattern = { 0 };
  double pattern_score = 0.0;
  double joseki_score = 0.0;
  GO_POSITION position;
  int x_line, y_line, line;
  
  position = &board->position[LOCATION_INDEX(board,x,y)];
  
  if ((position->piece == WHITE) || (position->piece == BLACK) || (position == board->ko_position))
  {
// fprintf (debug, "  Move %c%d is not allowed.\n", lettername[x], board->size - y, Go_Board_Result (board));
    return 0.0;
  }
  
  Build_Board_Pattern (board, x, y, my_stone, &position_pattern);
  
  if (move_number < 50)
  {
    joseki_score = Pattern_Score (&position_pattern, joseki_list, joseki_count);
  }
  
  pattern_score = Pattern_Score (&position_pattern, pattern_list, pattern_count);
  
  if (Go_Board_Place_Stone (board, my_stone, x, y) != GO_BOARD_OK)
  {
fprintf (debug, "  Move %c%d is not allowed because: %s.\n", lettername[x], board->size - y, Go_Board_Result (board));
    return 0.0;
  }
  
  x_line = (x < (board->size / 2)) ? x : board->size - x - 1;
  y_line = (y < (board->size / 2)) ? y : board->size - y - 1;
  line = (x_line < y_line) ? x_line : y_line;
  
  Get_Scoring_State (board, my_stone, current);
  Delta_Scoring_State (previous, current, &delta);
  
  score = + 15.0 * joseki_score
          + 15.0 * pattern_score
          + 75.0 * ((delta.empty_strings > 0) &&
                     ((delta.my_strings == 0) || (delta.other_captured > 0)))   // creating possible eye

          + 14.0 * (delta.my_strings < 0)
          + 30.0 * ((delta.my_strings > 0) && (delta.my_liberties == 4) && (move_number < 20))
          +  2.0 * (delta.my_strings == 0)
          + 18.0 * (delta.my_liberties > 0)
          +  0.0 * delta.my_liberties
         + 110.0 * (delta.my_one_liberties < 0)
          + 25.0 * (delta.my_two_liberties < 0)
          - 70.0 * (delta.my_one_liberties > 0)
          - 20.0 * (delta.my_two_liberties > 0)
          -  0.5 * (delta.my_three_liberties > 0)
          -  0.0 * current->my_one_liberties
          - 10.0 * (delta.empty_strings < 0)
  
         + 100.0 * delta.other_captured
          + 15.0 * (delta.other_strings < 0)
          + 35.0 * (delta.other_one_liberties > 0)
         + 100.0 * (delta.other_one_liberties > 1)
          + 15.0 * (delta.other_two_liberties > 0)
          +  5.0 * (delta.other_three_liberties > 0)
          
          -  3.0 * (line == 0)
          +  5.0 * (line == 1)
          + 12.0 * (line == 2)
          + 10.0 * (line == 3)
          +  8.0 * (line == 4)
          +  5.0 * (line == 5)
          
          + 12.0 * (((rand() % 1000) / 1000.0) - 0.5)
          ;
  
  if ((score <= NO_SUITABLE_MOVE) || (delta.my_captured > 0.0))
  {
    score = NO_SUITABLE_MOVE;
  }
  
fprintf (debug, "  Move %c%d has pattern score %g, overall score %g\n", lettername[x], board->size - y, pattern_score, score);
  return score;
}   // Score_Move

// -----------------------------------------------------------------------
// Get_Move:
//
double Get_Move (GO_BOARD  board,
                 GO_PIECE  my_stone,
                 int       move_number,
                 int      *move_x,
                 int      *move_y)
{
  int x = 0;
  int y = 0;
  int size = Go_Board_Size (board);
  double score = 0.0;
  double best_score = 0.0;
  int best_x = -1;
  int best_y = -1;
  struct GO_BOARD_SNAPSHOT_STRUCT snapshot = { 0 };
  struct SCORING_STATE_STRUCT previous_scoring_state = { 0 };
  struct SCORING_STATE_STRUCT scoring_state = { 0 };
    
  Go_Board_Get_Snapshot (board, &snapshot);
  Get_Scoring_State (board, my_stone, &previous_scoring_state);
  
  for (x = 0; x < size; x++)
  {
    for (y = 0; y < size; y++)
    {
      score = Score_Move (board, my_stone, move_number, x, y, &previous_scoring_state, &scoring_state);
      
      if (score > 0.0)
      {
        if (score > best_score)
        {
          best_x = x;
          best_y = y;
          best_score = score;
        }
        
        Go_Board_Set_Snapshot (board, &snapshot);
      }   // if move was valid
    }   // for each y
  }   // for each x
  
#define GOOD_MOVE_THRESHOLD  (1.0 + NO_SUITABLE_MOVE)

  if ((best_score > GOOD_MOVE_THRESHOLD) && (best_x >= 0) && (best_y >= 0))
  {
    *move_x = best_x;
    *move_y = best_y;
  }
  else
  {
    //
    // Pass.
    //
    score = NO_SUITABLE_MOVE;
    *move_x = -1;
    *move_y = -1;
  }
  
  Go_Board_Set_Snapshot (board, &snapshot);
fprintf (debug, "Best move is %c%d with score %g\n", lettername[*move_x], board->size - *move_y, best_score);
// fflush (debug);

  return best_score;
}   // Get_Move

// -----------------------------------------------------------------------
// Wait_For_GMP_Move:
//
void Wait_For_GMP_Move (Gmp* ge, GO_BOARD board, GO_PIECE his_stone, int* his_turn)
{
  GmpResult message;
  int x, y;
  const char* error;
  
  do
  {
    message = gmp_check(ge, 1, &x, &y, &error);
  } while (message == gmp_nothing);

  switch (message)
  {
  case gmp_err:
    fprintf(stderr, "dummy: Got error \"%s\"\n", error);
    exit(1);
  
  case gmp_move:
    passes = 0;
    Go_Board_Place_Stone (board, his_stone, x, y);
    break;
  
  case gmp_pass:
    if (++passes == 2)
    {
      Go_Board_Destroy (&board);
      exit (0);
    }
    
    break;
    
  case gmp_reset:
    Go_Board_Destroy (&board);
    exit (0);
  
  case gmp_undo:
    fprintf(stderr, "dummy: I am asked to undo %d moves.\n", x);
    
    if ((x & 1) == 0)
    {
    //
    //  Since an even number of undos were made, it is still the other
    //  player's turn.  This flag makes me skip my turn.
    //
      *his_turn = 1;
    }
    
    break;
    
  default:
    fprintf(stderr, "Got a \"%s\" command during game play.  Exiting.\n",
    gmp_resultString(message));
    exit(1);
  }   // switch on return type
}   // Wait_For_GMP_Move

// -----------------------------------------------------------------------
// Make_My_Move:
//
void Make_My_Move (Gmp* ge, GO_BOARD board, GO_PIECE my_stone, int move_number)
{
  int  x, y;
fprintf (debug, "\nPOSITION BEFORE OUR MOVE:\n");
Go_Board_Show (board, debug);

  if ((Get_Move (board, my_stone, move_number, &x, &y) > NO_SUITABLE_MOVE) &&
       (x >= 0) && (y >= 0))
  {
    Go_Board_Place_Stone (board, my_stone, x, y);
fprintf (debug, "\nPOSITION AFTER OUR MOVE:\n");
Go_Board_Show (board, debug);
fflush (debug);

    passes = 0;
    gmp_sendMove(ge, x, y);
  }
  else
  {
    //
    // No suitable move. Pass.
    //
    Go_Board_Set_Player (board, my_stone);
    Go_Board_Pass (board);
fprintf (debug, "\nPOSITION AFTER OUR MOVE (Pass):\n");
Go_Board_Show (board, debug);
fflush (debug);
    gmp_sendPass(ge);
  	
    if (++passes >= 2)
    {
      Go_Board_Destroy (&board);
      exit (0);
    }   // 2nd pass, game over, man!
  }   // else pass
}   // Make_My_Move

// -----------------------------------------------------------------------
// playGame:
//
void playGame(Gmp *ge, int size, int handicap, int his_turn)
{
  GO_BOARD board;
  GO_PIECE my_stone;
  GO_PIECE his_stone;
  int move_number = 1;
  const char* debug_filename = NULL;
  
  if (his_turn)
  {
    debug_filename = white_debug_file;
    my_stone  = WHITE;
    his_stone = BLACK;
  }
  else
  {
    debug_filename = black_debug_file;
    my_stone  = BLACK;
    his_stone = WHITE;
  }
  
  debug = fopen (debug_filename, "w");

  fprintf (debug, "Loaded %d patterns from \"%s\".\n", pattern_count, pattern_file);
  fprintf (debug, "Loaded %d joseki patterns from \"%s\".\n", joseki_count, joseki_file);
  
  board = Go_Board_Create (size);

  if (gmp_chineseRules (ge) != 0)
  {
    //
    // Chinese rules allow for suicide and for any placement of
    // handicap stones.
    //
    Go_Board_Allow_Suicide (board, 1);
    
    if (handicap > 0)
    {
      if (his_turn)
      {
        while (handicap-- > 1)   // go until he's got one move left
        {
          Wait_For_GMP_Move (ge, board, his_stone, &his_turn);
          move_number++;
        }
      }   // if he needs to place his handicap stones
      else
      {
        while (handicap-- > 0)
        {
          Make_My_Move (ge, board, my_stone, move_number++);
        }
      }
      
      his_turn = 1;
    }
  }
  else
  {
    //
    // Japanese rules have prescribed locations for the handicap stones.
    //
    if (handicap > 0)
    {
      Go_Board_Place_Handicap_Stones (board, handicap);
      move_number = handicap + 1;
      
      if (his_turn)
      {
        his_turn = 0;
      }
      else
      {
        his_turn = 1;
      }
    }
  }   // else Japanese handicap rules
  
  for (;;)
  {
    if (!his_turn)
    {
      Make_My_Move (ge, board, my_stone, move_number++);
    }   // if our turn
    
    his_turn = 0;
    Wait_For_GMP_Move (ge, board, his_stone, &his_turn);
    move_number++;
  }   // forever
  
  Go_Board_Destroy (&board);
}   // playGame
