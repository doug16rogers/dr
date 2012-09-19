/* $Id: 097.c 43 2008-09-12 04:51:21Z rogers $ */
#include <stdio.h>
#include <stdlib.h>

#define E 7830457
#define D ((E-1) / 8)

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
  0,
  6,
  2,
  8,
  4,
  5,
  1,
  7,
  3,
  9
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
void divA(unsigned char* n, size_t s)
{
  unsigned short carry = 0;

  while (s-- > 0)
  {
    carry <<= 8;
    carry += n[s];
    n[s] = carry / 10;
    carry %= 10;
  }
}

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[])
{
#define DIGITS 10
  unsigned char* n = calloc(1, D+2);
  char digits[DIGITS+1] = {0};
  int i;

  n[D+1] = 0xDE;   /* This is 28433 (0x6F11) << 1. */
  n[D+0] = 0x22;
  n[  0] = 0x01;

  for (i = 0; i < DIGITS; i++)
  {
    int m2 = n[0] & 1;
    int m5 = mod5(n, D+2);
    unsigned char mod10 = digit_5mod2_plus_mod5[5*m2 + m5];
    digits[DIGITS-i-1] = '0' + mod10;
    subbyte(n, D+2, mod10);
/*     printf("<%02X><%02X><%02X>...<%02X><%02X><%02X>\n", */
/* 	   (int) n[0], */
/* 	   (int) n[1], */
/* 	   (int) n[2], */
/* 	   (int) n[D-1], */
/* 	   (int) n[D], */
/* 	   (int) n[D+1]); */
    if ((i+1) < DIGITS)     /* Not necessary but a tad faster. */
    {
      divA(n, D+2);
/*       printf("<%02X><%02X><%02X>...<%02X><%02X><%02X>\n", */
/* 	     (int) n[0], */
/* 	     (int) n[1], */
/* 	     (int) n[2], */
/* 	     (int) n[D-1], */
/* 	     (int) n[D], */
/* 	     (int) n[D+1]); */
    }
  }

  printf("Last ten digits ...%s.\n", digits);
  return 0;
}   /* main() */
