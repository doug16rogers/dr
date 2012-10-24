#ifndef __go_game_h__
#define __go_game_h__
// -----------------------------------------------------------------------
// go_game:
//  This module provides services for playing a game of go.
//

typedef struct GO_GAME_STRUCT* GO_GAME;

//
// The move history should really be moved out of the board object and into a
// go game object which would manage the board.
//
typedef enum
{
  HANDICAP,
  MOVE,
  PASS,
  UNDO
} GO_MOVE;

struct GO_MOVE_INFO
{
  GO_MOVE type;
  
  union
  {
    //
    // HANDICAP:
    //
    struct
    {
      int stones;
    } handicap;
    
    //
    // MOVE:
    //
    struct
    {
      GO_PIECE piece;
      int x;
      int y;
    } move;
    
    //
    // PASS needs no argument.
    //
    struct
    {
      GO_PIECE piece;
    } pass;
    
    //
    // UNDO:
    //
    struct
    {
      int count;
    } undo;
  } argument;
};

int                       moves;
struct GO_MOVE_INFO       history[2 * GO_BOARD_MAX_N];


GO_GAME Go_Game_Create (int         size_x,      // board size across
                        int         size_y,      // board size up
                        GO_RULESET  rule_set,    // rules by which to play
                        int         play_black,  // program is to play black
                        int         handicap,    // handicap stones
                        double      komi,
                        int         initial_second,    // number of seconds in initial unlimited play period
                        int         byo_yomi_periods,  // number of byo-yomi periods allowed (-1 for unlimited)
                        int         byo_yomi_seconds,  // number of seconds in each byo-yomi period
                        int         byo_yomi_stones);  // number of stones that must be played per byo-yomi period

