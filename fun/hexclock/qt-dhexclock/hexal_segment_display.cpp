#include "hexal_segment_display.hpp"
#include <qpainter.h>

//
// Constructs a seven-segment display.
//

Hexal_Segment_Display::Hexal_Segment_Display(QWidget*    parent,
                                             const char* name)
    : Seven_Segment_Display (parent, name)
{
}

void Hexal_Segment_Display::setHexal (unsigned int value)
{
  int hexal_segments = SSD_SEG_G;

  if (value & 0x01) hexal_segments |= SSD_SEG_E;
  if (value & 0x02) hexal_segments |= SSD_SEG_C;
  if (value & 0x04) hexal_segments |= SSD_SEG_B;
  if (value & 0x08) hexal_segments |= SSD_SEG_F;
  setSegments (hexal_segments);
//  paintEvent(NULL);
}   // setHexal

void Hexal_Segment_Display::setHexalASCII (char value)
{
  if (('0' <= value) && (value <= '9'))
  {
    setHexal (value - '0');
  }
  else if (('A' <= value) && (value <= 'F'))
  {
    setHexal (value - 'A' + 10);
  }
  else if (('a' <= value) && (value <= 'f'))
  {
    setHexal (value - 'a' + 10);
  }
  else  // Else not a hexadecimal character...
  {
    switch (value)
    {
    case ' ':
    case '.':
      setSegments (0);
      break;

    case '_':
      setSegments (SSD_SEG_D);
      break;

    case '=':
      setSegments (SSD_SEG_A | SSD_SEG_D | SSD_SEG_G);
      break;

    case ':':
      setSegments (SSD_SEG_A | SSD_SEG_D);
      break;

    case '-':
      setSegments (SSD_SEG_A | SSD_SEG_C | SSD_SEG_D | SSD_SEG_E | SSD_SEG_G);
      break;

    case '+':
      setSegments (SSD_SEG_A | SSD_SEG_B | SSD_SEG_D | SSD_SEG_F | SSD_SEG_G);
      break;

    case '!':
      setSegments (SSD_SEG_E | SSD_SEG_F);
      break;

    case '|':
      setSegments (SSD_SEG_B | SSD_SEG_C);
      break;

    case '#':
      setSegments (SSD_SEG_B | SSD_SEG_C | SSD_SEG_E | SSD_SEG_F);
      break;

    case '@':
      setSegments (SSD_SEG_A | SSD_SEG_B | SSD_SEG_C | SSD_SEG_D | SSD_SEG_E | SSD_SEG_F | SSD_SEG_G);
      break;

    default:     // Display nothing for unsupported characters.
      setSegments (0);
    }
  }
}   // setHexal
