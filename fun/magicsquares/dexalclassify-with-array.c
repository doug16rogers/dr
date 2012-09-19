#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef unsigned long long uint64_t;

#define MAX_DEXAL_MAGIC_SQUARES 0x10000

typedef unsigned dmsa_t[4][4];
typedef char dms_string_t[4][4];

typedef struct xref_s xref_t;

struct xref_s
{
  char value[5][4];   /* Extra row for NULs. */
  int  rot_index[3];
  int  inv_index;
  int  transpose_index;
  int  base_index;      /* Lowest index of any in group. */
};   /* struct xref_s */

xref_t   xref[MAX_DEXAL_MAGIC_SQUARES];
unsigned xref_count = 0;

/* ------------------------------------------------------------------------- */
uint64_t uint64_invert(uint64_t source)
{
  return source ^ 0xFFFFFFFFFFFFFFFFull;
}   /* uint64_invert() */

/* ------------------------------------------------------------------------- */
uint64_t uint64_transpose(uint64_t source)
{
  uint64_t target = source & 0xF0000F0000F0000Full;
  unsigned i, j;

#define RC_INDEX(i,j)    ((4 * (i)) + (j))
#define RC_SHIFT(i,j)    (4 * RC_INDEX((i),(j)))
#define RC_MASK64(i,j)   (0xF000000000000000ull >> RC_SHIFT(i,j))

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      if (j != i)
      {
/*          target |= (source & RC_MASK(i,j)) >> RC_SHIFT(i,j) << RC_SHIFT(j,i); */
      }
    }
  }

  return target;
}   /* uint64_transpose() */

/* ------------------------------------------------------------------------- */
uint64_t uint64_rotate(uint64_t source)
{
  uint64_t target = 0;
  return target;
}   /* uint64_rotate() */

/* ------------------------------------------------------------------------- */
int dms_equal(dmsa_t* target, dmsa_t* source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      if ((*target)[i][j] != (*source)[i][j])
      {
        return 0;
      }
    }
  }

  return 1;
}   /* dms_equal() */

/* ------------------------------------------------------------------------- */
dmsa_t* dms_invert(dmsa_t* target, dmsa_t* source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      (*target)[i][j] = 0xF ^ (*source)[i][j];
    }
  }

  return target;
}   /* dms_invert() */

/* ------------------------------------------------------------------------- */
dmsa_t* dms_transpose(dmsa_t* target, dmsa_t* source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      (*target)[j][i] = (*source)[i][j];
    }
  }

  return target;
}   /* dms_transpose() */

/* ------------------------------------------------------------------------- */
dmsa_t* dms_rotate(dmsa_t* target, dmsa_t* source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      (*target)[j][3-i] = (*source)[i][j];
    }
  }

  return target;
}   /* dms_rotate() */

/* ------------------------------------------------------------------------- */
void dms_print_array(const char* label, dmsa_t* m)
{
  unsigned i, j;

  printf("%s{", label);

  for (i = 0; i < 4; i++)
  {
    printf("{");

    for (j = 0; j < 4; j++)
    {
      printf("0x%X", (*m)[i][j]);
      if (j != 3) printf(",");
    }

    printf("}");
    if (i != 3) printf(",");
  }

  printf("}\n");
}   /* dms_print_array() */

/* ------------------------------------------------------------------------- */
void dms_print(dmsa_t* m)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      printf("%X", (*m)[i][j]);
    }

    if (i != 3) printf("-");
  }
}   /* dms_print() */

/* ------------------------------------------------------------------------- */
void check_ops(void)
{
  dmsa_t m =
  {{0x0,0x1,0x2,0x3}, {0x4,0x5,0x6,0x7}, {0x8,0x9,0xA,0xB}, {0xC,0xD,0xE,0xF}};
  dmsa_t t;

  dms_print_array("m=", &m);
  dms_print_array("i=", dms_invert(&t, &m));
  dms_print_array("t=", dms_transpose(&t, &m));
  dms_print_array("r=", dms_rotate(&t, &m));
}   /* check_ops() */

/* ------------------------------------------------------------------------- */
void uint64_to_array(dmsa_t* target, uint64_t source)
{
  unsigned i, j;

  for (i = 0; i < 4; i++)
  {
    for (j = 0; j < 4; j++)
    {
      (*target)[i][j] = (source >> 0x3C);
      source <<= 4;
    }
  }
}   /* uint64_to_array() */

/* ------------------------------------------------------------------------- */
unsigned xref_load(xref_t*     xref_list,
                   unsigned    xref_list_max_count,
                   const char* filename)
{
  unsigned count = 0;
  FILE*    file = NULL;

  file = fopen(filename, "rt");

  if (file == NULL)
  {
    fprintf(stderr, "could not open'%s'\n", filename);
    exit(1);
  }

  memset(xref_list, 0, sizeof(xref_t));

  while (count <= xref_list_max_count)
  {
    if (fscanf(file, "%s", &xref_list[count].value[0][0]) != 1)
    {
      break;
    }

    count++;
  }

  fclose(file);

  return count;
}   /* xref_load() */

/* ------------------------------------------------------------------------- */
void xref_check(int    xref_index,
                int    original_index,
                dmsa_t* result,
                const char* reason)
{
  if (xref[xref_index].composite)
  {
    return;
  }

  if (dms_equal(&xref[xref_index].array, result))
  {
    xref[xref_index].composite = 1;
    xref[xref_index].prev_index = prev_index;
    xref[xref_index].reason = reason;
  }
}   /* xref_check() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[])
{
  dmsa_t inv[4];
  dmsa_t trn[4];
  dmsa_t inv_trn[4];
  int i;

/*    check_ops(); */
  xref_count = xref_load(&xref[0], MAX_DEXAL_MAGIC_SQUARES,
                         "dexal-magic-squares.txt");

  for (i = 0; i < xref_count; i++)
  {
    int j;

    if (!xref[i].composite)
    {
      xref[i].original_index = i;
      xref[i].reason = "original";
    }

    dms_invert(&inv[0], &xref[i].array);
    dms_rotate(&inv[1], &inv[0]);
    dms_rotate(&inv[2], &inv[1]);
    dms_rotate(&inv[3], &inv[2]);
    dms_transpose(&trn[0], &xref[i].array);
    dms_rotate(&trn[1], &trn[0]);
    dms_rotate(&trn[2], &trn[1]);
    dms_rotate(&trn[3], &trn[2]);
    dms_invert(&inv_trn[0], &trn[0]);
    dms_rotate(&inv_trn[1], &inv_trn[0]);
    dms_rotate(&inv_trn[2], &inv_trn[1]);
    dms_rotate(&inv_trn[3], &inv_trn[2]);

/*      dms_print(&xref[i].array); printf(" value\n"); */
/*      dms_print(&inv[0]); printf(" inverted\n"); */
/*      dms_print(&inv[1]); printf(" inverted, rotated 1\n"); */
/*      dms_print(&inv[2]); printf(" inverted, rotated 2\n"); */
/*      dms_print(&inv[3]); printf(" inverted, rotated 3\n"); */
/*      dms_print(&trn[0]); printf(" transposed\n"); */
/*      dms_print(&trn[1]); printf(" transposed, rotated 1\n"); */
/*      dms_print(&trn[2]); printf(" transposed, rotated 2\n"); */
/*      dms_print(&trn[3]); printf(" transposed, rotated 3\n"); */
/*      dms_print(&inv_trn[0]); printf(" inverted, transposed\n"); */
/*      dms_print(&inv_trn[1]); printf(" inverted, transposed, rotated 1\n"); */
/*      dms_print(&inv_trn[2]); printf(" inverted, transposed, rotated 2\n"); */
/*      dms_print(&inv_trn[3]); printf(" inverted, transposed, rotated 3\n"); */
/*      exit(1); */

    for (j = i+1; j < xref_count; j++)
    {
      if (xref[i].composite) continue;
      xref_check(j, i, &inv[0], "inverted");
      xref_check(j, i, &inv[1], "inverted, rotated 1");
      xref_check(j, i, &inv[2], "inverted, rotated 2");
      xref_check(j, i, &inv[3], "inverted, rotated 3");
      xref_check(j, i, &trn[0], "transposed");
      xref_check(j, i, &trn[1], "transposed, rotated 1");
      xref_check(j, i, &trn[2], "transposed, rotated 2");
      xref_check(j, i, &trn[3], "transposed, rotated 3");
      xref_check(j, i, &inv_trn[0], "inverted, transposed");
      xref_check(j, i, &inv_trn[1], "inverted, transposed, rotated 1");
      xref_check(j, i, &inv_trn[2], "inverted, transposed, rotated 2");
      xref_check(j, i, &inv_trn[3], "inverted, transposed, rotated 3");
    }
  }

  for (i = 0; i < xref_count; i++)
  {
    printf("#%04X: ", i);
/*      dms_print(&xref[i].array); */
    printf("%016llX", xref[i].value);
    printf(" #%04X %s\n", xref[i].original_index, xref[i].reason);
  }

  return 0;
}   /* main() */
