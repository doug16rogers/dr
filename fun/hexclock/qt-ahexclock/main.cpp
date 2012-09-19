/****************************************************************************
** $Id: main.cpp 4 2008-02-09 06:21:41Z dkrogers $
**
** Copyright (C) 1992-1998 Troll Tech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "analog_hexal_clock.h"
#include <qapplication.h>


int main( int argc, char **argv )
{
    QApplication a( argc, argv );
    Analog_Hexal_Clock *clock = new Analog_Hexal_Clock;
    clock->resize( 100, 100 );
    a.setMainWidget( clock );
    clock->show();
    return a.exec();
}
