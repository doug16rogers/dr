// Copyright (c) 2008-2019 Doug Rogers under the Zero Clause BSD License.
// You are free to do whatever you want with this software. See LICENSE.txt.

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
