#include <stdio.h>

#include "dmt.h"

dmt_row_t dmt_row[DMT_ORDERED_ROWS];

/* ------------------------------------------------------------------------- */
int main(void)
{
  unsigned  i, j;
  dmt_row_t permuted[DMT_ROW_PERMUTATIONS];

  dmt_gen_ordered_rows(dmt_row);
  printf("generated ordered rows\n");

  for (i = 0; i < DMT_ORDERED_ROWS; i++)
  {
    dmt_permute_row(dmt_row[i], permuted);

    for (j = 0; j < DMT_ROW_PERMUTATIONS; j++)
    {
      if (!dmt_row_okay(permuted[j]))
      {
        printf("#%05X/%2X: %02X%02X%02X%02X sum=%03X alldiffer=%u\n",
               i, j,
               dmt_row[i][0], dmt_row[i][1], dmt_row[i][2], dmt_row[i][3],
               dmt_row_sum(dmt_row[i]),
               dmt_row_elements_different(dmt_row[i]));
      }
    }
  }

  return 0;
}   /* main() */
