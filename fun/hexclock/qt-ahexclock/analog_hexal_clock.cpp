/****************************************************************************
** $Id: analog_hexal_clock.cpp 4 2008-02-09 06:21:41Z dkrogers $
**
** Copyright (C) 1992-1998 Troll Tech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "analog_hexal_clock.h"
#include <qtimer.h>
#include <qpainter.h>
// #include <stdiostream.h>
#include <math.h>

//
// Constructs an analog clock widget that uses an internal QTimer.
//

Analog_Hexal_Clock::Analog_Hexal_Clock( QWidget *parent, const char *name )
    : QWidget( parent, name )
{
    time = QTime::currentTime();		// get current time
    QTimer *internalTimer = new QTimer( this );	// create internal timer
    connect( internalTimer, SIGNAL(timeout()), SLOT(timeout()) );
    internalTimer->start( 1318 );		// emit signal every hexon
}


//
// The QTimer::timeout() signal is received by this slot.
//

void Analog_Hexal_Clock::timeout()
{
    QTime new_time = QTime::currentTime();	// get the current time
    update();
}


//
// The clock is painted using a 1000x1000 square coordinate system.
//

void Analog_Hexal_Clock::paintEvent( QPaintEvent * )	// paint clock
{
    if ( !isVisible() )				// is is invisible
	return;

    //
    // Draw the clock face:
    //
    QPainter paint (this);
    paint.setBrush (foregroundColor());      // fill with foreground color

    QPoint cp = rect().center();          // widget center point
    int d = QMIN(width(),height());       // we want a circular clock

    QWMatrix matrix;                      // setup transformation matrix

    matrix.translate (cp.x(), cp.y());    // origin at widget center
    matrix.scale (d/1000.0F, d/1000.0F);  // scale coordinate system
    paint.setWorldMatrix (matrix);

    QPointArray tick_04;
    tick_04.setPoints (4, 480,-20, 500,0, 480,+20, 400,0);

    for ( int i=0; i<0x40; i++ ) {		// draw hexon hour tick marks
      double angle = 360.0 * (double) i / 64.0;
      matrix.rotate (angle);
      paint.setWorldMatrix (matrix);

      if ((i % 0x10) == 0)
      {
        paint.drawPolygon (tick_04);
      }
      else if ((i % 0x04) == 0)
      {
        paint.drawLine (400,0, 500,0);
      }
      else
      {
        paint.drawLine (450,0, 500,0);
      }

      matrix.rotate (-angle);
    }

    //
    // Now work on the time.
    //
    time = QTime::currentTime();		// save current time

#define round(x)  floor ((x) + 0.5)

    double seconds = 60.0 * (60.0 * time.hour() + time.minute()) + time.second() + 0.001 * time.msec();
    double hexon_double = round (seconds * 65536.0 / 86400.0);
    double hexon = fmod (hexon_double, 64.0);
    hexon_double /= 64.0;
    double hexon_minute = fmod (hexon_double, 64.0);
    hexon_double /= 64.0;
    double hexon_hour = hexon_double;

    QPointArray hand;
    double this_digit;
    double angle;

    hand.setPoints (4, -32,0,  0,-32, 240,0, 0,32);
    angle = 360.0 * hexon_hour / 16.0;
    matrix.rotate (angle);	                // rotate to draw hexon digit hand
    paint.setWorldMatrix (matrix);
    paint.drawPolygon (hand);			// draw hour hand
    matrix.rotate (-angle);		        // rotate back to zero

    hand.setPoints (4, -16,0,  0,-16, 320,0, 0,16);
    angle = 360.0 * hexon_minute / 64.0;
    matrix.rotate (angle);	                // rotate to draw hexon digit hand
    paint.setWorldMatrix (matrix);
    paint.drawPolygon (hand);			// draw hour hand
    matrix.rotate (-angle);		        // rotate back to zero

    hand.setPoints (4, -8,0,  0,-8, 400,0, 0,8);
    angle = 360.0 * hexon / 64.0;
    matrix.rotate (angle);	                // rotate to draw hexon digit hand
    paint.setWorldMatrix (matrix);
    paint.drawPolygon (hand);			// draw hour hand
    matrix.rotate (-angle);		        // rotate back to zero
}
