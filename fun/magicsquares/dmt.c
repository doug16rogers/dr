#include <stdio.h>

#include "dmt.h"

/* ------------------------------------------------------------------------- */
/**
 * Converts a set of 4 indexes into a single index into a 1-D representation
 * of the tesseract in row-major form.
 */
unsigned dmt_line_index(const unsigned index[])
{
  return index[3] + (4 * (index[2] + (4 * (index[1] + (4 * index[0])))));
}   /* dmt_line_index() */

/* ------------------------------------------------------------------------- */
unsigned index_inc(unsigned* list,
                   unsigned  list_length,
                   unsigned  increment,
                   unsigned  overflow_value)
{
  unsigned i = 0;

  list[0] = list[0] + 1;
  list_length--;

  while ((list[i] >= overflow_value) &&
         (i < list_length))
  {
    list[i++] = 0;
    list[i]++;
  }

  if (list[i] >= overflow_value)
  {
    list[i] = 0;
    return 1;
  }

  return 0;
}   /* index_inc() */

/* ------------------------------------------------------------------------- */
void dmt_print_all_rows(void)
{
  unsigned sum;
  unsigned i0, i1, i2, i3;
  unsigned index = 0;
  sum = 0;

  for (i0 = 0; i0 < 0x100; i0++)
  {
    for (i1 = 0; i1 < 0x100; i1++)
    {
      if (i1 == i0) continue;

      for (i2 = 0; i2 < 0x100; i2++)
      {
        if ((i2 == i1) || (i2 == i0)) continue;
        sum = i0 + i1 + i2;
        if (sum >= DMT_MAGIC_SUM) break;
        i3 = DMT_MAGIC_SUM - sum;
        if (i3 >= 0x100) continue;
        printf("#%06X: %02X%02X%02X%02X\n", index++, i0, i1, i2, i3);
      }   /* i2 */
    }   /* i1 */
  }   /* i0 */
}   /* dmt_print_all_rows() */

/* ------------------------------------------------------------------------- */
void dmt_print_ordered_rows(void)
{
  unsigned sum;
  unsigned i0, i1, i2, i3;
  unsigned index = 0;
  sum = 0;

  for (i0 = 0; i0 < 0x100; i0++)
  {
    for (i1 = i0 + 1; i1 < 0x100; i1++)
    {
      for (i2 = i1 + 1; i2 < 0x100; i2++)
      {
        sum = i0 + i1 + i2;
        if ((sum + i2) >= DMT_MAGIC_SUM) break;
        i3 = DMT_MAGIC_SUM - sum;
        if (i3 >= 0x100) continue;
        printf("#%06X: %02X%02X%02X%02X\n", index++, i0, i1, i2, i3);
      }   /* i2 */
    }   /* i1 */
  }   /* i0 */
}   /* dmt_print_ordered_rows() */

/* ------------------------------------------------------------------------- */
void dmt_gen_ordered_rows(dmt_row_t* list)
{
  unsigned sum;
  unsigned i0, i1, i2, i3;
  unsigned index = 0;
  sum = 0;

  for (i0 = 0; i0 < 0x100; i0++)
  {
    for (i1 = i0 + 1; i1 < 0x100; i1++)
    {
      for (i2 = i1 + 1; i2 < 0x100; i2++)
      {
        sum = i0 + i1 + i2;
        if ((sum + i2) >= DMT_MAGIC_SUM) break;
        i3 = DMT_MAGIC_SUM - sum;
        if (i3 >= 0x100) continue;
        list[index][0] = i0;
        list[index][1] = i1;
        list[index][2] = i2;
        list[index][3] = i3;
        index++;
      }   /* i2 */
    }   /* i1 */
  }   /* i0 */
}   /* dmt_gen_ordered_rows() */

/* ------------------------------------------------------------------------- */
void dmt_permute_row(const dmt_row_t row,
                     dmt_row_t*      permutations)
{
#define PERMUTE(index,j0,j1,j2,j3) \
   do { \
     permutations[index][0] = row[j0]; \
     permutations[index][1] = row[j1]; \
     permutations[index][2] = row[j2]; \
     permutations[index][3] = row[j3]; \
   } while (0)

  PERMUTE(0x00, 0, 1, 2, 3);
  PERMUTE(0x01, 1, 0, 2, 3);
  PERMUTE(0x02, 0, 2, 1, 3);
  PERMUTE(0x03, 2, 0, 1, 3);
  PERMUTE(0x04, 1, 2, 0, 3);
  PERMUTE(0x05, 2, 1, 0, 3);
  PERMUTE(0x06, 0, 1, 3, 2);
  PERMUTE(0x07, 1, 0, 3, 2);
  PERMUTE(0x08, 0, 3, 1, 2);
  PERMUTE(0x09, 3, 0, 1, 2);
  PERMUTE(0x0A, 1, 3, 0, 2);
  PERMUTE(0x0B, 3, 1, 0, 2);
  PERMUTE(0x0C, 0, 2, 3, 1);
  PERMUTE(0x0D, 2, 0, 3, 1);
  PERMUTE(0x0E, 0, 3, 2, 1);
  PERMUTE(0x0F, 3, 0, 2, 1);
  PERMUTE(0x10, 2, 3, 0, 1);
  PERMUTE(0x11, 3, 2, 0, 1);
  PERMUTE(0x12, 1, 2, 3, 0);
  PERMUTE(0x13, 2, 1, 3, 0);
  PERMUTE(0x14, 1, 3, 2, 0);
  PERMUTE(0x15, 3, 1, 2, 0);
  PERMUTE(0x16, 2, 3, 1, 0);
  PERMUTE(0x17, 3, 2, 1, 0);
}   /* dmt_permute_ordered_row() */

/* ------------------------------------------------------------------------- */
int dmt_row_elements_different(dmt_row_t row)
{
  return
    (row[0] != row[1]) &&
    (row[0] != row[2]) &&
    (row[0] != row[3]) &&
    (row[1] != row[2]) &&
    (row[1] != row[3]) &&
    (row[2] != row[3]);
}   /* dmt_row_elements_different() */

/* ------------------------------------------------------------------------- */
unsigned dmt_row_sum(dmt_row_t row)
{
  unsigned i;
  unsigned sum = row[0];
  for (i = 1; i < 4; i++) sum += row[i];
  return sum;
}   /* dmt_row_sum_okay() */

/* ------------------------------------------------------------------------- */
int dmt_row_okay(dmt_row_t row)
{
  return dmt_row_elements_different(row) &&
        (dmt_row_sum(row) == DMT_MAGIC_SUM);
}   /* dmt_row_okay() */

/* ------------------------------------------------------------------------- */
int dmt_check_square(const dmt_t* self,
                     unsigned     fixed_i,
                     unsigned     fixed_j)
{
/*   unsigned vary_i; */
/*   unsigned vary_j; */

/*   switch (fixed_i) */
/*   { */
/*   case 0: */
/*     break; */
/*   } */
/*   if (fixed_i == 0) */
/*   { */
/*   } */
/*   else if (f */
/*   { */
/*   } */
  return 0;
}   /* dmt_check_square() */

/* ------------------------------------------------------------------------- */
int dmt_check(const dmt_t* self)
{
/*   dmt_row_t row; */
  unsigned  i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 1; j < 4; j++)
    {
      if (!dmt_check_square(self, i, j)) return 0;
    }
  }

  return 1;
}   /* dmt_check() */
