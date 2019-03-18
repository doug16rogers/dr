// Copyright (c) 2008-2019 Doug Rogers under the Zero Clause BSD License.
// You are free to do whatever you want with this software. See LICENSE.txt.

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
