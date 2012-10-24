/* $Id: 016.c 45 2008-09-12 07:44:07Z rogers $ */
#include <stdio.h>
#include <stdlib.h>

/*
 * I stole this from the code I wrote for 097 and modified it to do the sum
 * of digits. This is for 2^1000, so 126 bytes will do - a 0x01 in the 126th.
 */
#define D ((1000 / 8) + 1)

/*
 * Mod-2 and Mod-5 pairs uniquely determine the Mod-10 value.
 *   Mod-10 Mod-2 Mod-5  (5 * Mod-2) + Mod-5
 *      0     0     0            0
 *      1     1     1            6
 *      2     0     2            2
 *      3     1     3            8
 *      4     0     4            4
 *      5     1     0            5
 *      6     0     1            1
 *      7     1     2            7
 *      8     0     3            3
 *      9     1     4            9
 */                                /* 0123456789 */
const unsigned char digit_5mod2_plus_mod5[] =
{
  0, 6, 2, 8, 4, 5, 1, 7, 3, 9
};

/* ------------------------------------------------------------------------- */
int mod5(unsigned char* n, size_t s)
{
  int mod5 = 0;

  while (s--)
  {
    mod5 = ((*n++ % 5) + mod5) % 5;
  }

  return mod5;
}

/* ------------------------------------------------------------------------- */
void subbyte(unsigned char* n, size_t s, unsigned char b)
{
  while (*n < b)
  {
    *n++ -= b;
    b = 1;
  }

  *n -= b;
}

/* ------------------------------------------------------------------------- */
void divA(unsigned char* n, size_t s, int* nonzero)
{
  unsigned carry = 0;

  *nonzero = 0;

  while (s-- > 0)
  {
    carry <<= 8;
    carry += n[s];
    n[s] = carry / 10;
    *nonzero = *nonzero || n[s];
    carry %= 10;
  }
}

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[])
{
  unsigned char* n = calloc(1, D);
  int i;
  unsigned sum = 0;
  int is_non_zero = 1;

  n[D-1] = 0x01;

  for (i = 0; is_non_zero; i++)
  {
    int m2 = n[0] & 1;
    int m5 = mod5(n, D);
    unsigned mod10 = digit_5mod2_plus_mod5[5*m2 + m5];
    sum += mod10;
    subbyte(n, D, mod10);
    divA(n, D, &is_non_zero);
  }

  printf("sum = %u\n", sum);
  /* sum = 1366 */
  return 0;
}   /* main() */
