#ifndef __go_board_h__
#define __go_board_h__
// -----------------------------------------------------------------------
// go_board:
//  This module provides services to maintain a go board.
//

typedef enum
{
  EMPTY,
  BLACK,
  WHITE,
} GO_PIECE;

#define GO_PIECES 3

#define GO_BOARD_MIN_SIZE 5
#define GO_BOARD_MAX_SIZE 25   // just in case there are some masochists out there
#define GO_BOARD_MAX_N    (GO_BOARD_MAX_SIZE * GO_BOARD_MAX_SIZE)


typedef char* GO_BOARD_RESULT;
typedef struct GO_POSITION_STRUCT* GO_POSITION;
typedef struct GO_STRING_STRUCT*   GO_STRING;
typedef struct GO_BOARD_STRUCT*    GO_BOARD;

//
// Note: The GO_BOARD object only knows how to place stones,
//       not whose turn it is.
//
typedef struct GO_BOARD_SNAPSHOT_STRUCT
{
  int      size;     // for sanity check
  int      ko_x;     // -1 if no ko after most recent move
  int      ko_y;     // ditto
  int      captured_black;
  int      captured_white;
  GO_PIECE piece[GO_BOARD_MAX_N];
  int      passes;
  GO_PIECE to_play;
}* GO_BOARD_SNAPSHOT;

extern const GO_BOARD_RESULT GO_BOARD_OK;   // anything else is a text message

///////////////////////////////////////////////////////////////////////////
//       THE FOLLOWING BELONG IN THE .C FILE
//////////////////////////////////////////////////////////////////////////
typedef enum
{
  EAST  = 0,
  WEST  = 1,
  SOUTH = 2,
  NORTH = 3,
  GO_POSITION_NEIGHBORS
} GO_POSITION_NEIGHBOR;

struct GO_STRING_STRUCT
{
  int         number;
  int         stones;
  int         liberties;
  GO_PIECE    color;
  GO_POSITION first_stone;
  int         mark;
};

struct GO_POSITION_STRUCT
{
  int neighbors;
  GO_POSITION neighbor[GO_POSITION_NEIGHBORS];
  GO_POSITION east;
  GO_POSITION west;
  GO_POSITION south;
  GO_POSITION north;
  int x;
  int y;
  int mark;
  GO_PIECE    piece;
  GO_STRING   string;
  GO_POSITION next_in_string;
};

struct GO_BOARD_STRUCT
{
  int                       size;      // board size
  int                       n;         // number of locations on the board (size * size)
  int                       suicide_allowed;
  GO_BOARD_RESULT           result;    // most recent result message
  int                       star[5];   // the five rows that are star-points; 0, 1, and 2 are used for up to 9 handicap
  struct GO_POSITION_STRUCT position[GO_BOARD_MAX_N];
  int                       strings;                     // number of strings
  int                       colored_strings[GO_PIECES];  // count of strings in a particular color
  struct GO_STRING_STRUCT   string[GO_BOARD_MAX_N];      // group info
  GO_POSITION               ko_position;                 // NULL if there is no ko
  int                       captured[GO_PIECES];
  int                       passes;
  GO_PIECE                  to_play;
};

#define LOCATION_INDEX(board,x,y)  ((x)*((board)->size) + (y))
///////////////////////////////////////////////////////////////////////////
//       THE ABOVE BELONG IN THE .C FILE
//////////////////////////////////////////////////////////////////////////



GO_BOARD_RESULT Go_Board_Result (const GO_BOARD board);

GO_BOARD Go_Board_Create (int size);       // board width

void Go_Board_Allow_Suicide (GO_BOARD board, int suicide_allowed);
int  Go_Board_Suicide_Allowed (const GO_BOARD board);

int Go_Board_Size (const GO_BOARD board);

GO_BOARD_RESULT Go_Board_Clear (GO_BOARD board);

const int* Go_Board_Star_Points (const GO_BOARD board);  // pointer to 5 handicap rows; 0, 1, and 2 are used for 9-stone handicap

GO_BOARD_RESULT Go_Board_Get_Snapshot (const GO_BOARD    board,
                                       GO_BOARD_SNAPSHOT snapshot);

GO_BOARD_RESULT Go_Board_Set_Snapshot (GO_BOARD                board,
                                       const GO_BOARD_SNAPSHOT snapshot);

GO_BOARD_RESULT Go_Board_Place_Handicap_Stones (GO_BOARD board, int handicap);   // number of handicap stones to place

GO_BOARD_RESULT Go_Board_Place_Stone (GO_BOARD board,
                                            GO_PIECE stone,
                                            int      x,
                                            int      y);

GO_PIECE Go_Board_Player (const GO_BOARD board);

GO_BOARD_RESULT Go_Board_Set_Player (GO_BOARD board, GO_PIECE player);

GO_BOARD_RESULT Go_Board_Next_Player (GO_BOARD board);

GO_BOARD_RESULT Go_Board_Pass (GO_BOARD board);

GO_PIECE Go_Board_Piece_At (const GO_BOARD board,
                            int            x,
                            int            y);

int Go_Board_Liberties (const GO_BOARD board,
                        int            x,
                        int            y);

int Go_Board_String_Length (const GO_BOARD board,
                            int            x,
                            int            y);

GO_BOARD_RESULT
Go_Board_Remove_String (GO_BOARD board,
                        int      x,
                        int      y);

void Go_Board_Destroy (GO_BOARD* board);   // nulls out your pointer for you

void Go_Board_Show (const GO_BOARD board, FILE* file);

#endif
