/****************************************************************************
** $Id: analog_hexal_clock.h 4 2008-02-09 06:21:41Z dkrogers $
**
** Copyright (C) 1992-1998 Troll Tech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#ifndef __analog_hexal_clock_h__
#define __analog_hexal_clock_h__

#include <qwidget.h>
#include <qdatetime.h>


class Analog_Hexal_Clock : public QWidget
{
    Q_OBJECT
public:
    Analog_Hexal_Clock ( QWidget *parent=0, const char *name=0 );

protected:
    void	paintEvent( QPaintEvent * );

private slots:
    void	timeout();

private:
    QTime	time;
};


#endif    // If not already included.
