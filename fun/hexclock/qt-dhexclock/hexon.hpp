#ifndef __hexon_hpp__
#define __hexon_hpp__

#include "day_sec.hpp"

#define HEXONS_PER_DAY      (0x10000)
#define HEXONS_PER_SECOND   ((double) HEXONS_PER_DAY / 86400.0)
#define HEXICLES_PER_HEXON  (0x10000)

//
// The default epoch is the Unix epoch, 1970.01.01 00:00:00.
//
#define HEXON_DEFAULT_EPOCH_ZONE   0x0000     // Minutes west.
#define HEXON_DEFAULT_EPOCH_YEAR   0x07B2
#define HEXON_DEFAULT_EPOCH_MONTH  0x01
#define HEXON_DEFAULT_EPOCH_DAY    0x01
#define HEXON_DEFAULT_EPOCH_HOUR   0x00
#define HEXON_DEFAULT_EPOCH_MINUTE 0x00
#define HEXON_DEFAULT_EPOCH_SECOND 0  // 0.0

// ---------------------------------------------------------------------------
class Hexon
{
  const static Day_Sec default_epoch;

public:
  Hexon();
  //
  // Creates a hexon time value initialized to the current time in the
  // default epoch.
  //

public:
  Hexon (const Day_Sec& epoch);
  //
  // Creates a hexon time value initialized to the current time in the given
  // epoch.
  //

public:
  Hexon& Now (void);
  //
  // Sets the object to the current time with the given epoch.
  //

public:
  unsigned int Get_Day();
  unsigned int Get_Hexon();    // Into day.
  unsigned int Get_Hexicle();

public:
  Hexon& Print (char*        text,
                unsigned int text_length,
                const char*  format);
  //
  // Prints the hexon value according to the given format into the given text
  // string, using at most text_length - 1 characters. The text string will
  // always be NUL-terminated.
  //
  // Use "%<width><code>" combinations, where <width> is the decimal width of
  // the field, and where <code> is 'D', 'H', or 'h', for day, hexon (in
  // day), and hexicle, respectively.
  //

public:
  void Get_Epoch (      Day_Sec& epoch);
  void Set_Epoch (const Day_Sec& epoch);
  //
  // Gets/sets the object's epoch.
  //

public:
  Day_Sec epoch;
  double  hexon;             // Hexons since epoch.
};   // class Hexon

#endif   // If not already included.
