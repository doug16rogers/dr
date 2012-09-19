#include <stdio.h>
#include <string.h>

#include "dms.h"

/* ------------------------------------------------------------------------- */
const dms_op_t dms_op[DMS_OPS] =
{
  { "comp", dms_comp },
  { "flip", dms_flip },
  { "spin", dms_spin },
  { "bull", dms_bull },
  { "quad", dms_quad },
/*   { "spew", dms_spew }, */
/*   { "barf", dms_barf }, */
/*   { "binx", dms_binx }, */
/*   { "xor2", dms_xor2 }, */
};   /* dms_op_t dms_op[] */

/* ------------------------------------------------------------------------- */
const char* dms_dex_digit = "0123456789ABCDEF";

/* ------------------------------------------------------------------------- */
int dms_equal(const dms_t* target, const dms_t* source)
{
  return strcmp(target->line, source->line) == 0;
}   /* dms_equal() */

/* ------------------------------------------------------------------------- */
unsigned dms_dex_value(char c)
{
  if ((c >= '0') && (c <= '9')) return c - '0';
  if ((c >= 'A') && (c <= 'F')) return c - 'A' + 10;
  if ((c >= 'a') && (c <= 'a')) return c - 'a' + 10;
  return 0;
}   /* dms_dex_value() */

/* ------------------------------------------------------------------------- */
char dms_char_invert(char c)
{
  if ((c >= '0') && (c <= '9')) c -= '0';
  else if ((c >= 'A') && (c <= 'F')) c = c - 'A' + 10;
  else if ((c >= 'a') && (c <= 'a')) c = c - 'a' + 10;
  else c = 15;

  return dms_dex_digit[15 - c];
}   /* dms_char_invert() */

/* ------------------------------------------------------------------------- */
/**
 * F's complement.
 */
dms_t* dms_comp(dms_t* target, const dms_t* source)
{
  int i;

  for (i = 0; i < 0x10; i++)
  {
    target->line[i] = dms_char_invert(source->line[i]);
  }

  target->line[0x10] = 0;

  return target;
}   /* dms_comp() */

/* ------------------------------------------------------------------------- */
/**
 * Transpose.
 */
dms_t* dms_flip(dms_t* target, const dms_t* source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      target->array[j][i] = source->array[i][j];
    }
  }

  target->line[0x10] = 0;
  return target;
}   /* dms_flip() */

/* ------------------------------------------------------------------------- */
dms_t* dms_spin(dms_t* target, const dms_t* source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      target->array[j][3-i] = source->array[i][j];
    }
  }

  target->line[0x10] = 0;
  return target;
}   /* dms_spin() */

/* ------------------------------------------------------------------------- */
/**
 * This one looks like a bull's eye when you label the transitions as
 * undirected graphs.
 */
dms_t* dms_bull(dms_t* target, const dms_t* source)
{
  strcpy(target->line, source->line);
  target->array[0][1] = source->array[0][2];
  target->array[0][2] = source->array[0][1];
  target->array[1][0] = source->array[2][0];
  target->array[1][1] = source->array[2][2];
  target->array[1][2] = source->array[2][1];
  target->array[1][3] = source->array[2][3];
  target->array[2][0] = source->array[1][0];
  target->array[2][1] = source->array[1][2];
  target->array[2][2] = source->array[1][1];
  target->array[2][3] = source->array[1][3];
  target->array[3][1] = source->array[3][2];
  target->array[3][2] = source->array[3][1];
  return target;
}   /* dms_bull() */

/* ------------------------------------------------------------------------- */
/**
 * Within each quadrant, swap corners of 2x2.
 */
dms_t* dms_quad(dms_t* target, const dms_t* source)
{
  strcpy(target->line, source->line);
  target->array[0][0] = source->array[1][1];
  target->array[0][1] = source->array[1][0];
  target->array[0][2] = source->array[1][3];
  target->array[0][3] = source->array[1][2];
  target->array[1][0] = source->array[0][1];
  target->array[1][1] = source->array[0][0];
  target->array[1][2] = source->array[0][3];
  target->array[1][3] = source->array[0][2];
  target->array[2][0] = source->array[3][1];
  target->array[2][1] = source->array[3][0];
  target->array[2][2] = source->array[3][3];
  target->array[2][3] = source->array[3][2];
  target->array[3][0] = source->array[2][1];
  target->array[3][1] = source->array[2][0];
  target->array[3][2] = source->array[2][3];
  target->array[3][3] = source->array[2][2];
  return target;
}   /* dms_quad() */

/* ------------------------------------------------------------------------- */
/**
 * Just a mess. Move top two rows down two, but rearrange diagonals.
 * WORKS FOR JUST A FEW SQUARES.
 */
dms_t* dms_spew(dms_t* target, const dms_t* source)
{
  strcpy(target->line, source->line);
  target->array[0][0] = source->array[2][1];
  target->array[0][1] = source->array[2][3];
  target->array[0][2] = source->array[2][0];
  target->array[0][3] = source->array[2][2];
  target->array[1][0] = source->array[3][1];
  target->array[1][1] = source->array[3][3];
  target->array[1][2] = source->array[3][0];
  target->array[1][3] = source->array[3][2];
  target->array[2][0] = source->array[0][1];
  target->array[2][1] = source->array[0][3];
  target->array[2][2] = source->array[0][0];
  target->array[2][3] = source->array[0][2];
  target->array[3][0] = source->array[1][1];
  target->array[3][1] = source->array[1][3];
  target->array[3][2] = source->array[1][0];
  target->array[3][3] = source->array[1][2];
  return target;
}   /* dms_spew() */

/* ------------------------------------------------------------------------- */
/**
 * Another mess. Move top two rows down two with different diagonals.
 * WORKS FOR JUST A FEW SQUARES.
 */
dms_t* dms_barf(dms_t* target, const dms_t* source)
{
  strcpy(target->line, source->line);
  target->array[0][0] = source->array[2][1];
  target->array[0][1] = source->array[2][0];
  target->array[0][2] = source->array[2][3];
  target->array[0][3] = source->array[2][2];
  target->array[1][0] = source->array[3][1];
  target->array[1][1] = source->array[3][0];
  target->array[1][2] = source->array[3][3];
  target->array[1][3] = source->array[3][2];
  target->array[2][0] = source->array[0][1];
  target->array[2][1] = source->array[0][0];
  target->array[2][2] = source->array[0][3];
  target->array[2][3] = source->array[0][2];
  target->array[3][0] = source->array[1][1];
  target->array[3][1] = source->array[1][0];
  target->array[3][2] = source->array[1][3];
  target->array[3][3] = source->array[1][2];
  return target;
}   /* dms_barf() */

/* ------------------------------------------------------------------------- */
/**
 * Swapping bit 0 with bit 1 fails.
 * Swapping bit 0 with bit 2 fails.
 * Swapping bit 0 with bit 3 fails.
 * Swapping bit 1 with bit 2 fails.
 * Swapping bit 1 with bit 3 fails.
 * Swapping bit 2 with bit 3 fails.
 * So this type of transformation does not keep magic.
 */
dms_t* dms_binx(dms_t* target, const dms_t* source)
{
#define BINX_LSB 2
#define BINX_MSB 3
#define BINX_LSB_MASK (1 << BINX_LSB)
#define BINX_MSB_MASK (1 << BINX_MSB)
#define BINX_SHIFT    (BINX_MSB - BINX_LSB)

  int i;
  strcpy(target->line, source->line);
  for (i = 0; i < 0x10; i++)
  {
    unsigned value = dms_dex_value(source->line[i]);
    value = (value & ~(BINX_LSB_MASK | BINX_MSB_MASK)) |
        ((value & BINX_LSB_MASK) << BINX_SHIFT) |
        ((value & BINX_MSB_MASK) >> BINX_SHIFT);
    target->line[i] = dms_dex_digit[value];
  }
  return target;
}   /* dms_binx() */

/* ------------------------------------------------------------------------- */
/**
 * Xoring against bit 0 and bit 1 fails.
 * Xoring against bit 0 and bit 2 fails.
 * Xoring against bit 0 and bit 3 fails.
 * Xoring against bit 1 and bit 2 fails.
 * Xoring against bit 1 and bit 3 fails.
 * Xoring against bit 2 and bit 3 fails.
 * So this type of transformation does not keep magic.
 */
dms_t* dms_xor2(dms_t* target, const dms_t* source)
{
#define XOR2_LSB  2
#define XOR2_MSB  3
#define XOR2_MASK ((1 << XOR2_LSB) | (1 << XOR2_MSB))

  int i;
  strcpy(target->line, source->line);
  for (i = 0; i < 0x10; i++)
  {
    unsigned value = dms_dex_value(source->line[i]);
    value ^= XOR2_MASK;
    target->line[i] = dms_dex_digit[value];
  }
  return target;
}   /* dms_xor2() */

/* ------------------------------------------------------------------------- */
void dms_print_array(const char* label, const dms_t* m)
{
  unsigned i, j;

  printf("%s{", label);

  for (i = 0; i < 4; i++)
  {
    printf("{");

    for (j = 0; j < 4; j++)
    {
      printf("0x%X", m->array[i][j]);
      if (j != 3) printf(",");
    }

    printf("}");
    if (i != 3) printf(",");
  }

  printf("}\n");
}   /* dms_print_array() */

/* ------------------------------------------------------------------------- */
void dms_print(dms_t* m)
{
  m->line[16] = 0;
  printf("%s", m->line);
}   /* dms_print() */
