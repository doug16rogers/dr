// -----------------------------------------------------------------------
// go_board:
//  This module provides services to maintain a go board.
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string.h>

#include "go_board.h"

const GO_BOARD_RESULT GO_BOARD_OK                   = "ok";

GO_BOARD_RESULT go_board_result = "ok";  // GO_BOARD_OK;


#define  CAPTURED_LIBERTY_ADDED      0x0001

static void Recalculate_Strings (GO_BOARD board);


// -----------------------------------------------------------------------
// Go_Board_Result:
//  Returns the most recent result message. If NULL, it's the most
//  recent result from any board.
//
GO_BOARD_RESULT Go_Board_Result (const GO_BOARD board)
{
  return board->result;
}

// -----------------------------------------------------------------------
// Go_Board_Set_Result:
//
GO_BOARD_RESULT Go_Board_Set_Result (GO_BOARD board, GO_BOARD_RESULT message)
{
  go_board_result = message;
  
  if (board != NULL)
  {
    board->result = message;
  }
  
  return message;
}   // Go_Board_Set_Result

// -----------------------------------------------------------------------
// Go_Board_Calculate_Star_Points:
//
void Go_Board_Calculate_Star_Points (int size, int* five_starpoint_rows)
{
  int first_point = 3;
  
  if (size < 7)
  {
    first_point = 1;
  }
  else if (size < 13)
  {
    first_point = 2;
  }
  
  five_starpoint_rows[0] = first_point;
  five_starpoint_rows[1] = size / 2;
  five_starpoint_rows[2] = size - first_point - 1;
  
  five_starpoint_rows[3] = (five_starpoint_rows[0] + five_starpoint_rows[1]) / 2;
  five_starpoint_rows[4] = (five_starpoint_rows[2] + five_starpoint_rows[2]) / 2;
}   // Go_Board_Calculate_Star_Points

// -----------------------------------------------------------------------
// Go_Board_Create:
//
GO_BOARD Go_Board_Create (int size)        // width of board
{
  GO_BOARD board = NULL;
  
  if ((size < GO_BOARD_MIN_SIZE) || (size > GO_BOARD_MAX_SIZE))
  {
    Go_Board_Set_Result (NULL, "invalid board size");
    return NULL;
  }
  
  board = (GO_BOARD) malloc (sizeof (*board));
  
  if (board == NULL)
  {
    Go_Board_Set_Result (NULL, "out of memory allocating GO_BOARD object");
    return NULL;
  }
  
  board->size = size;
  board->n    = size * size;
  board->suicide_allowed = 0;   // default is non-Chinese
  
  Go_Board_Calculate_Star_Points (board->size, board->star);
  Go_Board_Clear (board);
  
  return board;
}   // Go_Board_Create

// -----------------------------------------------------------------------
// Go_Board_Size:
//
int Go_Board_Size (const GO_BOARD board)
{
  return board->size;
}   // Go_Board_Size

// -----------------------------------------------------------------------
// Go_Board_Allow_Suicide:
//
void Go_Board_Allow_Suicide (GO_BOARD board, int suicide_allowed)
{
  board->suicide_allowed = suicide_allowed != 0;
}   // Go_Board_Allow_Suicide

// -----------------------------------------------------------------------
// Go_Board_Suicide_Allowed:
//
int  Go_Board_Suicide_Allowed (const GO_BOARD board)
{
  return board->suicide_allowed;
}   // Go_Board_Suicide_Allowed

// -----------------------------------------------------------------------
// Go_Board_Clear:
//
GO_BOARD_RESULT Go_Board_Clear (GO_BOARD board)
{
  int x;
  int y;
  int i;
  int size;
  GO_POSITION position;
  
  board->strings = 0;
  size = board->size;
  
  board->passes = 0;
  board->to_play = BLACK;
  
  //
  // Initialize positions.
  //
  for (x = 0; x < size; x++)
  {
    for (y = 0; y < size; y++)
    {
      position = &board->position[LOCATION_INDEX(board,x,y)];
      position->east  = (x > 0)          ? &board->position[LOCATION_INDEX(board,x-1,y)] : NULL;
      position->west  = (x < (size - 1)) ? &board->position[LOCATION_INDEX(board,x+1,y)] : NULL;
      position->south = (y > 0)          ? &board->position[LOCATION_INDEX(board,x,y-1)] : NULL;
      position->north = (y < (size - 1)) ? &board->position[LOCATION_INDEX(board,x,y+1)] : NULL;
      
      position->neighbors = 0;
      if (position->east  != NULL) position->neighbor[position->neighbors++] = position->east;
      if (position->west  != NULL) position->neighbor[position->neighbors++] = position->west;
      if (position->south != NULL) position->neighbor[position->neighbors++] = position->south;
      if (position->north != NULL) position->neighbor[position->neighbors++] = position->north;

      position->x = x;
      position->y = y;
      position->piece = EMPTY;
      position->mark  = 0;
      position->string = NULL;
      position->next_in_string = NULL;
    }
  }
  
  for (i = 0; i < GO_PIECES; i++)
  {
    board->captured[i] = 0;
    board->colored_strings[i] = 0;
  }
  
  board->ko_position = NULL;
  
  Recalculate_Strings (board);
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Clear

// -----------------------------------------------------------------------
// Go_Board_Star_Points:
//
const int* Go_Board_Star_Points (const GO_BOARD board)  // pointer to 5 handicap rows; 0, 1, and 2 are for 9-stone handicap
{
  return board->star;
}   // Go_Board_Star_Points

// -----------------------------------------------------------------------
// Go_Board_Get_Snapshot:
//
GO_BOARD_RESULT Go_Board_Get_Snapshot (const GO_BOARD    board,
                                             GO_BOARD_SNAPSHOT snapshot)
{
  int i;
  
  if ((board == NULL) || (snapshot == NULL))
  {
    return Go_Board_Set_Result (board, "invalid shapshot pointer");
  }
  
  snapshot->size = board->size;
  
  if (board->ko_position == NULL)
  {
    snapshot->ko_x = -1;
    snapshot->ko_y = -1;
  }
  else
  {
    snapshot->ko_x = board->ko_position->x;
    snapshot->ko_y = board->ko_position->y;
  }
  
  snapshot->captured_black = board->captured[BLACK];
  snapshot->captured_white = board->captured[WHITE];
  snapshot->passes = board->passes;
  snapshot->to_play = board->to_play;
  
  for (i = 0; i < board->n; i++)
  {
    snapshot->piece[i] = board->position[i].piece;
  }
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Get_Snapshot

// -----------------------------------------------------------------------
// Go_Board_Set_Snapshot:
//
GO_BOARD_RESULT Go_Board_Set_Snapshot (GO_BOARD                board,
                                             const GO_BOARD_SNAPSHOT snapshot)
{
  int i;
  
  if ((board == NULL) || (snapshot == NULL) || (snapshot->size != board->size))
  {
    return Go_Board_Set_Result (board, "invalid shapshot");
  }
  
  if ((snapshot->ko_x < 0) || (snapshot->ko_x >= board->size) ||
      (snapshot->ko_y < 0) || (snapshot->ko_y >= board->size))
  {
    board->ko_position = NULL;
  }
  else
  {
    board->ko_position = &board->position[LOCATION_INDEX(board,snapshot->ko_x,snapshot->ko_y)];
  }
  
  board->captured[BLACK] = snapshot->captured_black;
  board->captured[WHITE] = snapshot->captured_white;
  board->passes  = snapshot->passes;
  board->to_play = snapshot->to_play;
  
  for (i = 0; i < board->n; i++)
  {
    board->position[i].piece = snapshot->piece[i];
  }
  
  Recalculate_Strings (board);
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Set_Snapshot

// -----------------------------------------------------------------------
// Go_Board_Place_Handicap_Stones:
//
GO_BOARD_RESULT Go_Board_Place_Handicap_Stones (GO_BOARD board, int handicap)   // number of handicap stones to place
{
  int stones_placed = handicap;
  
  if (handicap < 0)
  {
    return Go_Board_Set_Result (board, "handicap too small");
  }
  
  if (handicap > 25)
  {
    return Go_Board_Set_Result (board, "handicap too large");
  }
  
  Go_Board_Clear (board);
  
  switch (handicap)
  {
  case 25:
  case 24:
  case 23:
  case 22:
  case 21:
  case 20:
  case 19:
  case 18:
  case 17:
  case 16:
  case 15:
  case 14:
  case 13:
  case 12:
//    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[4]);
  case 11:
//    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[4]);
  case 10:
//    Go_Board_Place_Stone (board, BLACK, board->star[3], board->star[0]);
    stones_placed = 9;
  case 9:
    Go_Board_Place_Stone (board, BLACK, board->star[1], board->star[1]);
  case 8:
    Go_Board_Place_Stone (board, BLACK, board->star[1], board->star[0]);
    Go_Board_Place_Stone (board, BLACK, board->star[1], board->star[2]);
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[1]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[1]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[0]);
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[2]);
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[0]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[2]);
    break;
  case 7:
    Go_Board_Place_Stone (board, BLACK, board->star[1], board->star[1]);
  case 6:
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[1]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[1]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[0]);
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[2]);
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[0]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[2]);
    break;
  case 5:
    Go_Board_Place_Stone (board, BLACK, board->star[1], board->star[1]);
  case 4:
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[0]);
  case 3:
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[0]);
  case 2:
    Go_Board_Place_Stone (board, BLACK, board->star[0], board->star[2]);
    Go_Board_Place_Stone (board, BLACK, board->star[2], board->star[0]);
    board->to_play = WHITE;
    break;
  case 1:
  case 0:
    board->to_play = BLACK;
  }
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Place_Handicap_Stones

// -----------------------------------------------------------------------
// Capture_String:
//
static int Capture_String (GO_BOARD  board,
                           GO_STRING string)
{
  GO_POSITION position;
  int captured;
  
  captured = string->stones;
  
  for (position = string->first_stone; position != NULL; position = position->next_in_string)
  {
    position->piece = EMPTY;
  }   // for each position in the string
  
  board->captured[string->color] += captured;
  
  return captured;
}   // Capture_String

// -----------------------------------------------------------------------
// Extend_Empty_String:
//
static void Extend_Empty_String (GO_STRING   string,
                                 GO_POSITION position)
{
  int i;
  
  //
  // Check for position that has already been counted.
  //
  if (position->string != NULL)
  {
    return;
  }
  
  //
  // It's empty, so add it and extend to its neighbors.
  //
  position->string = string;
  position->next_in_string = string->first_stone;
  string->first_stone = position;
  string->stones++;
  
  for (i = 0; i < position->neighbors; i++)
  {
    if ((position->neighbor[i]->piece == EMPTY) && (position->neighbor[i]->string == NULL))  // save some calls
    {
      Extend_Empty_String (string, position->neighbor[i]);
    }
  }
}   // Extend_Empty_String

// -----------------------------------------------------------------------
// Extend_String:
//
static void Extend_String (GO_STRING   string,
                           GO_POSITION position)
{
  //
  // Check for position that has already been counted.
  //
  if ((position->piece == string->color) && (position->string != NULL))
  {
    return;
  }
  
  //
  // Check for an uncounted liberty.
  //
  if ((position->piece == EMPTY) && (position->string != string))
  {
    string->liberties++;
    position->string = string;   // mark empty position as liberty of this string
    return;
  }
  
  if (position->piece == string->color)
  {
    int i;
    
    position->string = string;
    position->next_in_string = string->first_stone;
    string->first_stone = position;
    string->stones++;
    
    for (i = 0; i < position->neighbors; i++)
    {
      Extend_String (string, position->neighbor[i]);
    }
  }
}   // Extend_String

// -----------------------------------------------------------------------
// Recalculate_Strings:
//
static void Recalculate_Strings (GO_BOARD board)
{
  int i;
  
  board->strings = 0;
  
  for (i = 0; i < GO_PIECES; i++)
  {
    board->colored_strings[i] = 0;
  }
  
  for (i = 0; i < board->n; i++)
  {
    GO_POSITION position = &board->position[i];
    
    position->mark = 0;
    position->string = NULL;
    position->next_in_string = NULL;
  }
  
  for (i = 0; i < board->n; i++)
  {
    GO_POSITION position = &board->position[i];
    
    //
    // Don't bother with empty or already-stringed positions.
    //
    if ((position->piece != EMPTY) && (position->string == NULL))
    {
      GO_STRING string = &board->string[board->strings];
      
      //
      // Create null string.
      //
      string->number = board->strings;
      string->mark = 0;
      string->stones = 0;
      string->liberties = 0;
      string->first_stone = NULL;
      string->color = position->piece;
      board->strings++;
      board->colored_strings[position->piece]++;
      
      Extend_String (string, position);
    }   // if an unmarked position
  }   // for each position
  
  //
  // Okay, now go back and generate the empty strings.
  //
  for (i = 0; i < board->n; i++)
  {
    GO_POSITION position = &board->position[i];
    
    if (position->piece == EMPTY)
    {
      position->string = NULL;
    }
  }   // for clearing the empty strings
  
  for (i = 0; i < board->n; i++)
  {
    GO_POSITION position = &board->position[i];
    
    if ((position->piece == EMPTY) && (position->string == NULL))
    {
      GO_STRING string = &board->string[board->strings];
      
      //
      // Create null string.
      //
      string->number = board->strings;
      string->mark = 0;
      string->stones = 0;
      string->liberties = 0;
      string->first_stone = NULL;
      string->color = position->piece;
      board->strings++;
      board->colored_strings[position->piece]++;
      
      Extend_Empty_String (string, position);
    }
  }   // for each empty position
}   // Recalculate_Strings

// -----------------------------------------------------------------------
// Go_Board_Place_Stone:
//
GO_BOARD_RESULT Go_Board_Place_Stone (GO_BOARD board,
                                            GO_PIECE stone,
                                            int      x,
                                            int      y)
{
  GO_POSITION position;
  GO_POSITION neighbor;
  int i;
  int liberties;
  int stones;
  int capturable;
  GO_POSITION captured_neighbor;
  
  if ((x < 0) || (x >= board->size) ||
      (y < 0) || (y >= board->size))
  {
    return Go_Board_Set_Result (board, "illegal move - location is off board");
  }
  
  if ((stone != BLACK) && (stone != WHITE))
  {
    return Go_Board_Set_Result (board, "illegal move - unknown stone color");
  }
  
  position = &board->position[LOCATION_INDEX(board,x,y)];
  
  if (position->piece != EMPTY)
  {
    return Go_Board_Set_Result (board, "illegal move - position is non-empy");
  }
  
  if (position == board->ko_position)
  {
    return Go_Board_Set_Result (board, "illegal move - position is in ko");
  }
  
  //
  // Check for suicide and for a new ko position.
  //
  liberties = 0;
  stones = 1;      // This group will at least have the one stone being placed.
  capturable = 0;
  captured_neighbor = NULL;
  
  for (i = 0; i < position->neighbors; i++)
  {
    neighbor = position->neighbor[i];
    
    switch (neighbor->piece)
    {
    case EMPTY:
      liberties++;
      break;
    case BLACK:
    case WHITE:
      if (neighbor->piece == stone)
      {
        //
        // If it's the same color piece, then combine the liberty count of
        // the attached group, subtracting off the one taken by the new
        // stone.
        //
        liberties += neighbor->string->liberties - 1;
        stones    += neighbor->string->stones; 
      }
      else
      {
        //
        // If it's the opposite color, then we capture if the neighbor's
        // string has only one liberty, so we get an extra.
        //
        if (neighbor->string->liberties == 1)
        {
          capturable += neighbor->string->stones;
          liberties++;
        }
      }   // else it's the opposite color
    }   // switch on what's at the neighbor's location
  }   // for each neighbor
  
  if (liberties == 0)
  {
    if ((stones <= 1) || !board->suicide_allowed)
    {
      return Go_Board_Set_Result (board, "illegal move - position is suicide");
    }
    
    //
    // Now capture self since it's allowed.
    //
    position->piece = stone;
    Recalculate_Strings (board);
    Capture_String (board, position->string);
    Recalculate_Strings (board);
    
    //
    // Ko is not possible, so just exit.
    //
    board->ko_position = NULL;
    board->passes = 0;
    Go_Board_Next_Player (board);

    return Go_Board_Set_Result (board, GO_BOARD_OK);   // ----> return!
  }
  
  //
  // Okay, the move is legal.
  // First go back and look for neighbors to be captured.
  //
  if (capturable > 0)
  {
    for (i = 0; i < position->neighbors; i++)
    {
      neighbor = position->neighbor[i];
      
      if ((neighbor->piece != EMPTY) && (neighbor->piece != stone) && (neighbor->string->liberties == 1))
      {
        captured_neighbor = neighbor;
        Capture_String (board, neighbor->string);
      }
    }   // for each neighboring stone
  }
  
  //
  // Create own group.
  //
  position->piece = stone;
  
  Recalculate_Strings (board);

  //
  // Now see if the resulting position is ko.
  //
  if ((capturable == 1) && (position->string->stones == 1) && (position->string->liberties == 1))
  {
    board->ko_position = captured_neighbor;
  }
  else
  {
    board->ko_position = NULL;
  }
  
  board->passes = 0;
  Go_Board_Next_Player (board);
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Place_Stone

// -----------------------------------------------------------------------
// Go_Board_Player:
//
GO_PIECE Go_Board_Player (const GO_BOARD board)
{
  return board->to_play;
}   // Go_Board_Player

// -----------------------------------------------------------------------
// Go_Board_Set_Player:
//
GO_BOARD_RESULT Go_Board_Set_Player (GO_BOARD board, GO_PIECE player)
{
  if ((player != BLACK) && (player != WHITE))
  {
    return Go_Board_Set_Result (board, "illegal player");
  }
  
  if (player == WHITE)
  {
    board->to_play = WHITE;
  }
  else
  {
    board->to_play = BLACK;
  }
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Set_Player

// -----------------------------------------------------------------------
// Go_Board_Next_Player:
//
GO_BOARD_RESULT Go_Board_Next_Player (GO_BOARD board)
{
  if (board->to_play == BLACK)
  {
    board->to_play = WHITE;
  }
  else
  {
    board->to_play = BLACK;
  }
  
  return Go_Board_Set_Result (board, GO_BOARD_OK);
}   // Go_Board_Next_Player

// -----------------------------------------------------------------------
// Go_Board_Pass:
//
GO_BOARD_RESULT Go_Board_Pass (GO_BOARD board)
{
  board->ko_position = NULL;
  board->passes++;
  Go_Board_Next_Player (board);
  
  return Go_Board_Next_Player (board);
}   // Go_Board_Pass

// -----------------------------------------------------------------------
// Go_Board_Piece_At:
//
GO_PIECE Go_Board_Piece_At (const GO_BOARD board,
                            int            x,
                            int            y)
{
  if ((x < 0) || (x >= board->size) ||
      (y < 0) || (y >= board->size))
  {
    Go_Board_Set_Result (board, "illegal location");
    return EMPTY;
  }
  
  Go_Board_Set_Result (board, GO_BOARD_OK);
  return board->position[LOCATION_INDEX(board,x,y)].piece;
}   // Go_Board_Piece_At

// -----------------------------------------------------------------------
// Go_Board_Liberties:
//
int Go_Board_Liberties (const GO_BOARD board,
                        int            x,
                        int            y)
{
  GO_POSITION position;
  
  if ((x < 0) || (x >= board->size) ||
      (y < 0) || (y >= board->size))
  {
    Go_Board_Set_Result (board, "illegal location");
    return -1;
  }
  
  position = &board->position[LOCATION_INDEX(board,x,y)];
  
  if ((position->piece != BLACK) && (position->piece != WHITE))
  {
    Go_Board_Set_Result (board, "empty location");
    return -1;
  }
  
  Go_Board_Set_Result (board, GO_BOARD_OK);
  return position->string->liberties;
}   // Go_Board_Liberties

// -----------------------------------------------------------------------
// Go_Board_String_Length:
//
int Go_Board_String_Length (const GO_BOARD board,
                            int            x,
                            int            y)
{
  GO_POSITION position;
  
  if ((x < 0) || (x >= board->size) ||
      (y < 0) || (y >= board->size))
  {
    Go_Board_Set_Result (board, "illegal location");
    return -1;
  }
  
  position = &board->position[LOCATION_INDEX(board,x,y)];
  
  if ((position->piece != BLACK) && (position->piece != WHITE))
  {
    Go_Board_Set_Result (board, "empty location");
    return -1;
  }
  
  return position->string->stones;
}   // Go_Board_String_Length


// -----------------------------------------------------------------------
// Go_Board_Remove_String:
//
GO_BOARD_RESULT Go_Board_Remove_String (GO_BOARD board,
                                        int      x,
                                        int      y)
{
  return Go_Board_Set_Result (board, "not yet implemented");
}   // Go_Board_Remove_String

// -----------------------------------------------------------------------
// Go_Board_Destroy:
//
void Go_Board_Destroy (GO_BOARD* board)    // nulls out your pointer for you
{
  if ((board != NULL) && (*board != NULL))
  {
    free (*board);
    *board = NULL;
    Go_Board_Set_Result (NULL, GO_BOARD_OK);
  }
}

// -----------------------------------------------------------------------
// Go_Board_Show:
//
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  - (O) -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  - (O)(O)<#><#> -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  - (O)<#>(O) -  -  -  -  -  *  -  -  -  -  -  *  -  -  -
//  - <#> - <#> -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  *  -  -  -  -  -  *  -  -  -  -  -  *  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  - <#>(O) -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  - <#>(O) x (O) -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  - <#>(O) -  -  -
//  -  -  -  *  -  -  -  -  -  *  -  -  -  -  -  *  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//
void Go_Board_Show (const GO_BOARD board, FILE* file)
{
  const char lettername[25] = "ABCDEFGHJKLMNOPQRSTUVWXYZ";
  const char colorname[4] = ".BW"; 
  int x;
  int y;
  int i;
  GO_POSITION position;
  
  fprintf (file, "\n#####################################################################\n");

  fprintf (file, "    ");
  for (x = 0; x < board->size; x++)
  {
    fprintf (file, " %c ", lettername[x]);
  }
  fprintf (file, "\n");
  
//  for (y = board->size - 1; y >= 0; y--)
  for (y = 0; y < board->size; y++)           // for some reason cgoban uses 0, 0 in upper left
  {
    fprintf (file, "%2d: ", board->size - y);
    
    for (x = 0; x < board->size; x++)
    {
      int star_x = (x == board->star[0]) || (x == board->star[1]) || (x == board->star[2]);
      int star_y = (y == board->star[0]) || (y == board->star[1]) || (y == board->star[2]);
      
      position = &board->position[LOCATION_INDEX(board,x,y)];
      
      switch (position->piece)
      {
      case EMPTY:
        if (position == board->ko_position)
        {
          fprintf (file, " x ");
        }
        else if (star_x && star_y)
        {
          fprintf (file, " + ");
        }
        else
        {
          fprintf (file, " - ");
        }
        break;
      
      case BLACK: fprintf (file, "<#>"); break;
      case WHITE: fprintf (file, "(O)"); break;
      }   // switch on stone type
    }   // for x
    
    fprintf (file, " :%-2d\n", board->size - y);
  }   // for y, backwards
  
  fprintf (file, "    ");
  for (x = 0; x < board->size; x++)
  {
    fprintf (file, " %c ", lettername[x]);
  }
  fprintf (file, "\n");
  
  fprintf (file, "Captured White stones: %d\n", board->captured[WHITE]);
  fprintf (file, "Captured Black stones: %d\n", board->captured[BLACK]);
  
  if ((board->strings > 0))  // commented out for now
  {
    fprintf (file, "\n");
    fprintf (file, "There are %d strings of stones on the board.\n", board->strings);
    fprintf (file, "Idx Col Cnt Lib Positions\n");
    fprintf (file, "--- --- --- --- ------------------------------------------------------\n");
    
    for (i = 0; i < board->strings; i++)
    {
      GO_STRING string = &board->string[i];
      
      fprintf (file, "%3d  %c  %2d  %2d ",
              string->number,
              colorname[string->color],
              string->stones,
              string->liberties);
              
      for (position = string->first_stone; position != NULL; position = position->next_in_string)
      {
        fprintf (file, " %c%d", lettername[position->x], board->size - position->y);
      }   // for each stone in the string
      
      fprintf (file, "\n");
    }   // for each string
  }   // if there are strings
}   // Go_Board_Show

