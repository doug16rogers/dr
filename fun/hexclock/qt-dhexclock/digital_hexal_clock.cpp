#include "digital_hexal_clock.hpp"
#include "hexal_segment_display.hpp"
#include <qtimer.h>
#include <qpainter.h>
// #include <stdiostream.h>
#include <math.h>

#define X_DIGIT_RATIO_NUM    15
#define X_DIGIT_RATIO_DEN    16
#define Y_DIGIT_RATIO_NUM    16
#define Y_DIGIT_RATIO_DEN    16

#define MILLISECONDS_PER_SECOND 1000.0

// ---------------------------------------------------------------------------
void Digital_Hexal_Clock::Initialize ()
{
  timer = new QTimer (this);    // create internal timer
  connect (timer, SIGNAL(timeout()), SLOT(timeout()));

  // emit signal roughly every hexon
  timer->start ((int) (MILLISECONDS_PER_SECOND / HEXONS_PER_SECOND));

  char text[0x40];

  hexon.Print (text, sizeof (text), format);
  hex_display_count = strlen (text);

  if (hex_display_count > MAX_HEX_DISPLAYS)
  {
    hex_display_count = MAX_HEX_DISPLAYS;
  }

  for (int i = 0; i < hex_display_count; i++)
  {
    hex_display[i] = new Hexal_Segment_Display (this);
  }

  resize (width(), height());
}   // Digital_Hexal_Clock::Initialize ()

// ---------------------------------------------------------------------------
Digital_Hexal_Clock::Digital_Hexal_Clock (QWidget *parent, const char *name)
    : QWidget (parent, name)
{
  this->format = strdup ("days+");
  Initialize();
}

// ---------------------------------------------------------------------------
Digital_Hexal_Clock::Digital_Hexal_Clock (const Day_Sec& epoch,
                                          const char*    format, 
                                          QWidget*       parent,
                                          const char*    name)
  : QWidget (parent, name)
{
  this->hexon.Set_Epoch (epoch);
  this->format = strdup (format);
  Initialize();
}

// ---------------------------------------------------------------------------
Digital_Hexal_Clock::~Digital_Hexal_Clock()
{
  if (format != NULL) free ((void*) format);
  if (timer  != NULL) delete timer;

  for (int i = 0; i < hex_display_count; i++)
  {
    if (hex_display[i] != NULL) delete hex_display[i];
  }
}

// // ---------------------------------------------------------------------------
// void Digital_Hexal_Clock::resize (int w, int h)
// {
// //  int hex_x_inc = width()  / hex_display_count;
// //  int hex_y_inc = height();
//   int hex_x_inc = w  / hex_display_count;
//   int hex_y_inc = h;
//   int hex_w = X_DIGIT_RATIO_NUM * hex_x_inc / X_DIGIT_RATIO_DEN;
//   int hex_h = Y_DIGIT_RATIO_NUM * hex_y_inc / Y_DIGIT_RATIO_DEN;

//   for (int i = 0; i < hex_display_count; i++)
//   {
//     hex_display[i]->resize (hex_w, hex_h);
//     hex_display[i]->move (i * hex_x_inc, 0);
//   }
// }   // Digital_Hexal_Clock::resize (int w, int h)

// ---------------------------------------------------------------------------
void Digital_Hexal_Clock::resizeEvent (QResizeEvent* ev)
{
  int w = ev->size().width();
  int h = ev->size().height();
//  int hex_x_inc = width()  / hex_display_count;
//  int hex_y_inc = height();
  int hex_x_inc = w  / hex_display_count;
  int hex_y_inc = h;
  int hex_w = X_DIGIT_RATIO_NUM * hex_x_inc / X_DIGIT_RATIO_DEN;
  int hex_h = Y_DIGIT_RATIO_NUM * hex_y_inc / Y_DIGIT_RATIO_DEN;

  for (int i = 0; i < hex_display_count; i++)
  {
    hex_display[i]->resize (hex_w, hex_h);
    hex_display[i]->move (i * hex_x_inc, 0);
  }
}   // Digital_Hexal_Clock::resize (int w, int h)

// ---------------------------------------------------------------------------
void Digital_Hexal_Clock::timeout()
{
  update();
}

// ---------------------------------------------------------------------------
void Digital_Hexal_Clock::paintEvent( QPaintEvent * ev)	// paint clock
{
  if (!isVisible()) return;

  char text[0x40];
  hexon.Now().Print (text, sizeof (text), format);

  for (int i = 0; (i < hex_display_count) && (text[i] != 0); i++)
  {
    hex_display[i]->setHexalASCII (text[i]);
    hex_display[i]->paintEvent(ev);
  }
}   // Digital_Hexal_Clock::paintEvent (QPaintEvent*)
