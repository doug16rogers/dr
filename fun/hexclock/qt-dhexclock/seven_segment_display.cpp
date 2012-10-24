#include "seven_segment_display.hpp"
#include <qpainter.h>

//
// Constructs a seven-segment display.
//

Seven_Segment_Display::Seven_Segment_Display (QWidget *parent, const char *name)
    : QWidget (parent, name)
{
  segments = 0;
}


void Seven_Segment_Display::setSegments (int new_segments)
{
  segments = new_segments;
}

// void Seven_Segment_Display::resize(int w, int h)
// {
// printf("%s\n", __FUNCTION__);
// }

void Seven_Segment_Display::paintEvent (QPaintEvent *)	// paint clock
{
//printf("%s:%u: %s()\n", __FILE__, __LINE__, __FUNCTION__);
  if (!isVisible()) return;

  //
  // Draw each segment:
  //
  QPainter paint (this);  // Create painter object.

  QPointArray segment;

  // Clear region to background color.
  paint.setPen (backgroundColor());
  paint.setBrush (backgroundColor());
  segment.setPoints(4, 0, 0, width(), 0, width(), height(), 0, height());
  paint.drawPolygon(segment);

  paint.setPen (foregroundColor());
  paint.setBrush (foregroundColor());      // fill with foreground color.

  int xh = width()  / 0x08;
  int yh = height() / 0x10;

  if (segments & SSD_SEG_A)
  {
    segment.setPoints (6, 0x01*xh,0x01*yh, 0x02*xh,0x00*yh, 0x06*xh,0x00*yh, 0x07*xh,0x01*yh, 0x06*xh,0x02*yh, 0x02*xh,0x02*yh);
    paint.drawPolygon (segment);
  }

  if (segments & SSD_SEG_B)
  {
    segment.setPoints (6, 0x07*xh,0x01*yh, 0x08*xh,0x02*yh, 0x08*xh,0x07*yh, 0x07*xh,0x08*yh, 0x06*xh,0x07*yh, 0x06*xh,0x02*yh);
    paint.drawPolygon (segment);
  }

  if (segments & SSD_SEG_C)
  {
    segment.setPoints (6, 0x07*xh,0x08*yh, 0x08*xh,0x09*yh, 0x08*xh,0x0E*yh, 0x07*xh,0x0F*yh, 0x06*xh,0x0E*yh, 0x06*xh,0x09*yh);
    paint.drawPolygon (segment);
  }

  if (segments & SSD_SEG_D)
  {
    segment.setPoints (6, 0x01*xh,0x07*yh, 0x02*xh,0x06*yh, 0x0E*xh,0x06*yh, 0x0F*xh,0x07*yh, 0x0E*xh,0x08*yh, 0x02*xh,0x08*yh);
    paint.drawPolygon (segment);
  }

  if (segments & SSD_SEG_E)
  {
    segment.setPoints (6, 0x01*xh,0x08*yh, 0x02*xh,0x09*yh, 0x02*xh,0x0E*yh, 0x01*xh,0x0F*yh, 0x00*xh,0x0E*yh, 0x00*xh,0x09*yh);
    paint.drawPolygon (segment);
  }

  if (segments & SSD_SEG_F)
  {
    segment.setPoints (6, 0x01*xh,0x01*yh, 0x02*xh,0x02*yh, 0x02*xh,0x07*yh, 0x01*xh,0x08*yh, 0x00*xh,0x07*yh, 0x00*xh,0x02*yh);
    paint.drawPolygon (segment);
  }

  if (segments & SSD_SEG_G)
  {
    segment.setPoints (6, 0x01*xh,0x08*yh, 0x02*xh,0x07*yh, 0x06*xh,0x07*yh, 0x07*xh,0x08*yh, 0x06*xh,0x09*yh, 0x02*xh,0x09*yh);
    paint.drawPolygon (segment);
  }
}   // paintEvent
