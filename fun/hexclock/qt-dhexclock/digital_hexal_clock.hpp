#ifndef __digital_hexal_clock_hpp__
#define __digital_hexal_clock_hpp__

#include <qwidget.h>
#include <qdatetime.h>

#include "hexon.hpp"
#include "hexal_segment_display.hpp"

#define MAX_HEX_DISPLAYS   0x10

class Digital_Hexal_Clock : public QWidget
{
  Q_OBJECT
public:
  Digital_Hexal_Clock (QWidget*    parent = NULL,
                       const char* name = NULL);

  Digital_Hexal_Clock (const Day_Sec& epoch,
                       const char*    format = NULL,
                       QWidget*       parent = NULL,
                       const char*    name=0 );

  ~Digital_Hexal_Clock();

public:
  void paintEvent (QPaintEvent*);
//  void resize (int w, int h);
  void resizeEvent (QResizeEvent* ev);

private slots:
  void	timeout();

private:
  QTimer        *timer;
  Hexon         hexon;
  const char*   format;

  Hexal_Segment_Display* hex_display[MAX_HEX_DISPLAYS];
  unsigned int           hex_display_count;

private:
  void Initialize ();
};


#endif    // If not already included.
