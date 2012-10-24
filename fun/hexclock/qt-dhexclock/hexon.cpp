#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include "hexon.hpp"

#ifndef SECONDS_PER_DAY
#define SECONDS_PER_DAY  0x15180
#endif

const Day_Sec Hexon::default_epoch (HEXON_DEFAULT_EPOCH_ZONE,
                                    HEXON_DEFAULT_EPOCH_YEAR,
                                    HEXON_DEFAULT_EPOCH_MONTH,
                                    HEXON_DEFAULT_EPOCH_DAY,
                                    HEXON_DEFAULT_EPOCH_HOUR,
                                    HEXON_DEFAULT_EPOCH_MINUTE,
                                    HEXON_DEFAULT_EPOCH_SECOND);

// ---------------------------------------------------------------------------
Hexon::Hexon()
{
  Set_Epoch (default_epoch);
  Now();
}   // Hexon::Hexon()

// ---------------------------------------------------------------------------
Hexon::Hexon (const Day_Sec& epoch)
{
  Set_Epoch (epoch);
  Now();
}   // Hexon::Hexon (Day_Sec&)

// ---------------------------------------------------------------------------
unsigned int Hexon::Get_Day()
{
  return (unsigned int) (hexon / HEXONS_PER_DAY);
}   // Hexon::Get_Day()

// ---------------------------------------------------------------------------
unsigned int Hexon::Get_Hexon()    // Into day
{
  return ((unsigned long) hexon) % HEXONS_PER_DAY;
}   // Hexon::Get_Hexon()

// ---------------------------------------------------------------------------
unsigned int Hexon::Get_Hexicle()
{
  return (unsigned int) ((double) HEXONS_PER_DAY * (hexon - floor (hexon)));   // Shift up 16 bits.
}   // Hexon::Get_Hexicle()

// ---------------------------------------------------------------------------
Hexon& Hexon::Now (void)
{
  struct timeval  time;
  struct timezone zone;
  Day_Sec now;

  gettimeofday (&time, &zone);
  now.set_unix (&time, &zone);
  now.sub (epoch);
  hexon = ((double) now.day * (double) HEXONS_PER_DAY) + (now.sec * HEXONS_PER_SECOND);
  return *this;
}   // Hexon::Now

// ---------------------------------------------------------------------------
static void Write_Value (char*         text,
                         unsigned int  text_length,
                         unsigned int  width,
                         unsigned int& index,
                         unsigned long value)
{
  char         hex_text[9];
  unsigned int i;

  if (width > 8)
  {
    width = 8;
  }

  if (width < 1)
  {
    width = 1;
  }

  i = 8 - width;

  sprintf (hex_text, "%08lx", value);

  while (index < text_length)
  {
    text[index++] = hex_text[i++];
  }
}   // Write_Value
                         
// ---------------------------------------------------------------------------
struct TextIter
{
  char*        m_text;
  unsigned int m_size;
  unsigned int m_index;

  TextIter(char* text, unsigned int size, unsigned int index = 0) :
    m_text(text), m_size(size), m_index(index)
  {
    text[0] = 0;
  }

  bool full() const
  {
    return (m_index + 1) >= m_size;
  }

  void put(const char c)
  {
    if (!full())
    {
      m_text[m_index++] = c;
      m_text[m_index] = 0;
    }
  }

  void put(unsigned int n,
           unsigned int width,
           unsigned int base = 10)
  {
    const char   digit[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    unsigned int start_index = m_index;

    if (0 == width)
    {
      do
      {
        put(digit[n % base]);
        n /= base;
      } while (n != 0);
    }
    else
    {
      do
      {
        put(digit[n % base]);
        n /= base;
        width--;
      } while (width > 0);
    }   // Else limit the width.

    // Now reverse the digits.
    const unsigned int reverse_count = (m_index - start_index) / 2;

    for (unsigned int i = 0; i < reverse_count; i++)
    {
      char c = m_text[start_index + i];
      m_text[start_index + i] = m_text[m_index - 1 - i];
      m_text[m_index - 1 - i] = c;
    }
  }   // TextIter::put(n,width,base)

  void reset()
  {
    m_index = 0;
  }
};

// ---------------------------------------------------------------------------
Hexon& Hexon::Print (char*        text,
                     unsigned int text_length,
                     const char*  format)
{
  if (text_length == 0)
  {
    return *this;
  }

  TextIter iter(text, text_length, 0);

  typedef enum { READING_TEXT, READING_FORMAT, READING_WIDTH } state_t;

  state_t      state = READING_TEXT;
  unsigned int format_start_i = 0;
  unsigned int format_i = 0;
#define DEFAULT_WIDTH 4
  unsigned int width = DEFAULT_WIDTH;

  while ((format[format_i] != 0) && (!iter.full()))
  {
    switch (state)
    {
    case READING_TEXT:
      if (format[format_i] == '%')
      {
        width = DEFAULT_WIDTH;
        format_i++;
        format_start_i = format_i;
        state = READING_FORMAT;
      }
      else
      {
        iter.put(format[format_i]);
        format_i++;
      }

      break;

    case READING_FORMAT:
      switch (format[format_i])
      {
      case '%':
        iter.put('%');   // Initial '%'.
        format_i = format_start_i;
        state = READING_TEXT;
        break;

      case 'D':
        iter.put(Get_Day(), width, 16);
        format_i++;
        state = READING_TEXT;
        break;

      case 'H':
        iter.put(Get_Hexon(), width, 16);
        format_i++;
        state = READING_TEXT;
        break;

      case 'h':
        iter.put(Get_Hexicle(), width, 16);
        format_i++;
        state = READING_TEXT;
        break;

      default:
        if ((format[format_i] >= '0') &&
            (format[format_i] <= '9'))
        {
          width = 0;
          format_i++;
          state = READING_WIDTH;
        }
      }   // Switch on format specifier character.
      break;   // state case READING_FORMAT.

    case READING_WIDTH:
      if ((format[format_i] >= '0') &&
          (format[format_i] <= '9'))
      {
        width = (10 * width) + (format[format_i] - '0');
        format_i++;
      }
      else
      {
        state = READING_FORMAT;
      }
      break;
    }   // Switch on state.
  }   // While format characters are still available.

  if (iter.full())
  {
    return *this;
  }

  // Now clean up if the final state was not READING_TEXT.
  switch (state)
  {
  case READING_TEXT:
    // No cleanup necessary.
    break;

  case READING_FORMAT:
  case READING_WIDTH:
    // Never got format character to copy from start of formatter.
    iter.put('%');
    format_i = format_start_i;

    while (format[format_i] && !iter.full())
    {
      iter.put(format[format_i++]);
    }
    break;
  }   // switch on final state for cleaning up.
}   // Hexon::Print()

// ---------------------------------------------------------------------------
void Hexon::Get_Epoch (Day_Sec& epoch)
{
  epoch = this->epoch;
}   // Hexon::Get_Epoch

// ---------------------------------------------------------------------------
void Hexon::Set_Epoch (const Day_Sec& epoch)
{
  this->epoch = epoch;
}   // Hexon::Set_Epoch
