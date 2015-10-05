#include <stdio.h>

typedef struct row_info_s row_info_t;

struct row_info_s
{
  unsigned int dex_used_mask;
  unsigned int dex_list;
};   /* struct row_info_s */


char     row_adds_ok[0x10000] = { 0 };
unsigned row_ok_list[0x10000] = { 0 };
unsigned used_for_row[0x10000] = { 0 };
unsigned row_ok_count = 0;

/* ------------------------------------------------------------------------- */
void generate_rows(void)
{
  unsigned i0, i1, i2, i3;
  unsigned compact_row;
  unsigned dexes_used;

  for (i0 = 0; i0 < 0x10; i0++)
  {
    for (i1 = 0; i1 < 0x10; i1++)
    {
      if (i1 == i0) continue;
      for (i2 = 0; i2 < 0x10; i2++)
      {
        if (i2 == i0) continue;
        if (i2 == i1) continue;
        for (i3 = 0; i3 < 0x10; i3++)
        {
          if (i3 == i0) continue;
          if (i3 == i1) continue;
          if (i3 == i2) continue;
          if ((i0 + i1 + i2 + i3) == 0x1E)
          {
            compact_row = (i0 << 0xC) | (i1 << 0x8) | (i2 << 0x4) | i3;
            dexes_used = (1 << i0) | (1 << i1) | (1 << i2) | (1 << i3);
            used_for_row[compact_row] = dexes_used;
            row_adds_ok[compact_row] = 1;
            row_ok_list[row_ok_count++] = compact_row;
          }
        }
      }
    }
  }
}   /* generate_rows() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[])
{
  unsigned i0, i1, i2, i3;
  unsigned r0, r1, r2, r3;

  generate_rows();
/*    printf("%04X rows\n", row_ok_count); */

  for (i0 = 0; i0 < row_ok_count; i0++)
  {
    r0 = row_ok_list[i0];

    for (i1 = 0; i1 < row_ok_count; i1++)
    {
      r1 = row_ok_list[i1];
      if ((used_for_row[r0] & used_for_row[r1]) != 0) continue;

      for (i2 = 0; i2 < row_ok_count; i2++)
      {
        r2 = row_ok_list[i2];
        if ((used_for_row[r0] & used_for_row[r2]) ||
            (used_for_row[r1] & used_for_row[r2])) continue;

        for (i3 = 0; i3 < row_ok_count; i3++)
        {
          r3 = row_ok_list[i3];
          if ((used_for_row[r0] & used_for_row[r3]) ||
              (used_for_row[r1] & used_for_row[r3]) ||
              (used_for_row[r2] & used_for_row[r3])) continue;

          /*
           * Columns:
           */
          if (!row_adds_ok[((r0 & 0xF000) >> 0x0) |
                           ((r1 & 0xF000) >> 0x4) |
                           ((r2 & 0xF000) >> 0x8) |
                           ((r3 & 0xF000) >> 0xC)]) continue;
          if (!row_adds_ok[((r0 & 0x0F00) << 0x4) |
                           ((r1 & 0x0F00) >> 0x0) |
                           ((r2 & 0x0F00) >> 0x4) |
                           ((r3 & 0x0F00) >> 0x8)]) continue;
          if (!row_adds_ok[((r0 & 0x00F0) << 0x8) |
                           ((r1 & 0x00F0) << 0x4) |
                           ((r2 & 0x00F0) >> 0x0) |
                           ((r3 & 0x00F0) >> 0x4)]) continue;
          if (!row_adds_ok[((r0 & 0x000F) << 0xC) |
                           ((r1 & 0x000F) << 0x8) |
                           ((r2 & 0x000F) << 0x4) |
                           ((r3 & 0x000F) >> 0x0)]) continue;

          /*
           * Diagonals:
           */
          if (!row_adds_ok[(r0 & 0xF000) |
                           (r1 & 0x0F00) |
                           (r2 & 0x00F0) |
                           (r3 & 0x000F)]) continue;
          if (!row_adds_ok[(r0 & 0x000F) |
                           (r1 & 0x00F0) |
                           (r2 & 0x0F00) |
                           (r3 & 0xF000)]) continue;

          printf("%04X%04X%04X%04X\n", r0, r1, r2, r3);
        }   /* For row 3. */
      }   /* For row 2. */
    }
  }
  return 0;
}   /* main() */
