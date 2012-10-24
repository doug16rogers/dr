/* Copyright (c) 2006 Doug Rogers. ALL RIGHTS RESERVED. */

/**
 * @file
 *
 * This program emits UTF-8 byte sequences for the Unicode values
 * specified in hexadecimal on the commandline.
 */
#include <stdio.h>
#include <strings.h>
#include <stdlib.h>

/**
 * This program's name.
 */
#define PROGRAM "echo-utf8"

/**
 * The maximum number of bytes in a single UTF8 character encoding.
 */
#define MAX_UTF8_ENCODED_LENGTH  6

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information then exits with the given @a exit_code.
 *
 * @param exit_code - exit code for the program. 0 means no error.
 */
void usage(int exit_code) __attribute__((noreturn));
void usage(int exit_code)
{
  fprintf(stderr, "\nUsage: %s <hex-unicode-value>...\n\n", PROGRAM);
  exit(exit_code);
}   /* usage() */

/* ------------------------------------------------------------------------- */
/**
 * Converts a Unicode character value into a sequence of bytes conformant
 * with UTF-8. See http://www.cl.cam.ac.uk/~mgk25/unicode.html.
 *
 * @param target  - buffer to hold UTF-8 characters.
 * @param unicode - 31-bit Unicode value.
 *
 * @return the number of UTF-8 bytes placed in the buffer, or 0 on error.
 */
unsigned int unicode2utf8(unsigned char* target,
                          unsigned long  unicode)
{
  if ((unicode & ~0x0000007Ful) == 0ul)
  {
    *target = (unsigned char) (unicode & 0x7Ful);
    return 1;
  }

  if ((unicode & ~0x0000007FFul) == 0ul)
  {
    *target++ = 0xC0 | ((unsigned char) (unicode >> 6) & 0x1F);
    *target   = 0x80 | ((unsigned char) unicode & 0x3F);
    return 2;
  }

  if ((unicode & ~0x00000FFFFul) == 0ul)
  {
    *target++ = 0xE0 | ((unsigned char) (unicode >> 12) & 0x0F);
    *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
    *target   = 0x80 | ((unsigned char) unicode & 0x3F);
    return 3;
  }

  if ((unicode & ~0x0001FFFFFul) == 0ul)
  {
    *target++ = 0xF0 | ((unsigned char) (unicode >> 18) & 0x07);
    *target++ = 0x80 | ((unsigned char) (unicode >> 12) & 0x3F);
    *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
    *target   = 0x80 | ((unsigned char) unicode & 0x3F);
    return 4;
  }

  if ((unicode & ~0x003FFFFFFul) == 0ul)
  {
    *target++ = 0xF8 | ((unsigned char) (unicode >> 24) & 0x03);
    *target++ = 0x80 | ((unsigned char) (unicode >> 18) & 0x3F);
    *target++ = 0x80 | ((unsigned char) (unicode >> 12) & 0x3F);
    *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
    *target   = 0x80 | ((unsigned char) unicode & 0x3F);
    return 5;
  }

  if ((unicode & ~0x7FFFFFFFul) == 0ul)
  {
    *target++ = 0xFC | ((unsigned char) (unicode >> 30) & 0x01);
    *target++ = 0x80 | ((unsigned char) (unicode >> 24) & 0x3F);
    *target++ = 0x80 | ((unsigned char) (unicode >> 18) & 0x3F);
    *target++ = 0x80 | ((unsigned char) (unicode >> 12) & 0x3F);
    *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
    *target   = 0x80 | ((unsigned char) unicode & 0x3F);
    return 6;
  }

  return 0;
}   /* unicode2utf8() */

/* ------------------------------------------------------------------------- */
/**
 * Main program. Goes through each argument attempting to read it as a
 * hexadecimal number, then prints the UTF-8 byte sequence corresponding to
 * that Unicode character.
 *
 * @param argc - number of arguments on commandline, including program name.
 * @param argv - list of pointers to the arguments.
 *
 * @return 0 on success, something else if an argument could not be read as
 * a hexadecimal value.
 */
int main(int argc, char* argv[])
{
  unsigned long        unicode = 0;
  int                  i = 0;
  int                  return_code = 0;
  static unsigned char utf8_buffer[0x200];
  unsigned char*       utf8 = &utf8_buffer[0];
  unsigned int         total_bytes = 0;
  unsigned int         bytes = 0;

  if (argc < 2)
  {
    usage(1);
  }

  bzero(utf8_buffer, sizeof(utf8_buffer));

  for (i = 1; i < argc; i++)
  {
    if (sscanf(argv[i], "%lx", &unicode) != 1)
    {
      fprintf(stderr, "%s: invalid hexadecimal unicode value '%s' (arg %u)\n", PROGRAM, argv[i], i);
      usage(2);
    }
    else if ((total_bytes + MAX_UTF8_ENCODED_LENGTH) < sizeof(utf8_buffer))
    {
      bytes = unicode2utf8(utf8, unicode);

/*       printf("U+%08lX => %u bytes: %02X %02X %02X %02X %02X %02X\n", */
/*              unicode, bytes, */
/*              (int) utf8[0], (int) utf8[1], (int) utf8[2], */
/*              (int) utf8[3], (int) utf8[4], (int) utf8[5]); */

      if (bytes == 0)
      {
        return_code |= 0x02;
      }
      else
      {
        total_bytes += bytes;
        utf8 += bytes;
      }
    }
    else
    {
      fprintf(stderr, "%s: buffer overflow.\n", PROGRAM);
      break;
    }
  }    /* For each argument. */

  fwrite(utf8_buffer, 1, total_bytes, stdout);
  fwrite("\n", 1, 1, stdout);
  fflush(stdout);

  return return_code;
}   /* main() */
