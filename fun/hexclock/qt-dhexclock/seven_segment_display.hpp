#ifndef __seven_segment_display_hpp__
#define __seven_segment_display_hpp__

#include <qwidget.h>

//
// The segments are labelled as follows:
//
//      ###########
//     #     A     #
//     #           #
//     # F       B #
//     #           #
//     #     G     #
//      ###########
//     #           #
//     #           #
//     # E       C #
//     #           #
//     #     D     #
//      ###########
//
#define SSD_SEG_A    0x01
#define SSD_SEG_B    0x02
#define SSD_SEG_C    0x04
#define SSD_SEG_D    0x08
#define SSD_SEG_E    0x10
#define SSD_SEG_F    0x20
#define SSD_SEG_G    0x40

class Seven_Segment_Display : public QWidget
{
  Q_OBJECT
public:
  Seven_Segment_Display ( QWidget *parent=0, const char *name=0 );

public slots:
  void setSegments (int new_segments);

public:
  void	paintEvent( QPaintEvent * );
//   void  resize(int w, int h);

private:
  int         segments;
};


#endif    // If not already included.
