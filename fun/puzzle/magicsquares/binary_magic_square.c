#include <stdio.h>

/* ------------------------------------------------------------------------- */
/**
 * @param word - word of bits.
 *
 * @return number of 1's in the word.
 */
unsigned int word_bit_count(unsigned int word)
{
  unsigned int count = 0;

  while (word != 0)
  {
    count += word & 1;
    word >>= 1;
  }

  return count;
}   /* word_bit_count() */

/* ------------------------------------------------------------------------- */
/**
 * Checks the given word to see if it represents a binary magic square.
 *
 * @param word  - word to check, lsb-first.
 * @param width - number of bits per row.
 *
 * @return 1 if the word is a binary magic square, 0 otherwise.
 */
int is_binary_magic_square(unsigned int word,
                           unsigned int width)
{
  unsigned int row_mask = (1 << width) - 1;
  unsigned int bits_in_first_row = word_bit_count(word & row_mask);
  unsigned int i;
  unsigned int col_mask = 1;
  unsigned int diag1_mask = 1;
  unsigned int diag2_mask = (1 << (width - 1));

  for (i = 1; i < width; i++)
  {
    if (word_bit_count((word >> (i * width)) & row_mask) != bits_in_first_row)
    {
      return 0;
    }

    col_mask = (col_mask << width) | 1;
    diag1_mask = (diag1_mask << (width+1)) | 1;
    diag2_mask = (diag2_mask << (width-1)) | diag2_mask;
  }

/*   printf("col_mask=%04X diag1_mask=%04X diag2_mask=%04X\n", */
/*          col_mask, diag1_mask, diag2_mask); */

  for (i = 0; i < width; i++)
  {
    if (word_bit_count(word & col_mask) != bits_in_first_row)
    {
      return 0;
    }

    col_mask <<= 1;
  }

  if (word_bit_count(word & diag1_mask) != bits_in_first_row)
  {
    return 0;
  }

  if (word_bit_count(word & diag2_mask) != bits_in_first_row)
  {
    return 0;
  }

  return 1;
}   /* is_binary_magic_square() */

/* ------------------------------------------------------------------------- */
/**
 * Runs the binary magic square checker. The optional commandline parameters
 * are the number of rows and then number of 1s that must be present in each
 * row. By default the number of rows is 5 and there is no check on the
 * number of 1s in each row - all combinations are printed.
 *
 * @param argc - number of commandline arguments.
 * @param argv - list of pointers to the commandline arguments.
 *
 * @return 0 on succes, 1 on error.
 */
int main(int argc, char* argv[])
{
#define N 5
  unsigned int n = N;
  int          per_row = -1;
  unsigned int i;
  unsigned int last;
  unsigned int mask;
  unsigned int digits;

  if ((argc > 1) &&
      ((sscanf(argv[1], "%u", &n) != 1) || (n > 6)))
  {
    fprintf(stderr, "invalid N '%s'.\n", argv[1]);
    return 1;
  }

  if ((argc > 2) &&
      ((sscanf(argv[2], "%d", &per_row) != 1) || (per_row > n)))
  {
    fprintf(stderr, "invalid per-row count '%s'.\n", argv[2]);
    return 1;
  }

  last = 1 << (n * n);
  mask = (1 << n) - 1;
  digits = ((n * n) + 3) / 4;

  if (per_row >= 0)
  {
    for (i = 0; i < last; i++)
    {
      if ((word_bit_count(i & mask) == per_row) &&
          is_binary_magic_square(i, n))
      {
        printf("%0*X\n", digits, i);
      }
    }
  }
  else
  {
    for (i = 0; i < last; i++)
    {
      if (is_binary_magic_square(i, n))
      {
        printf("%0*X\n", digits, i);
      }
    }
  }

  return 0;
}   /* main() */

