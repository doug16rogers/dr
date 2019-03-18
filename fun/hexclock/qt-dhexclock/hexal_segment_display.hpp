// Copyright (c) 2008-2019 Doug Rogers under the Zero Clause BSD License.
// You are free to do whatever you want with this software. See LICENSE.txt.

#ifndef __hexal_segment_display_hpp__
#define __hexal_segment_display_hpp__

#include <qwidget.h>

#include "seven_segment_display.hpp"


class Hexal_Segment_Display : public Seven_Segment_Display
{
  Q_OBJECT
public:
  Hexal_Segment_Display ( QWidget *parent=0, const char *name=0 );

public slots:
  void setHexal (unsigned int value);
  void setHexalASCII (char value);
};

#endif    // If not already included.
