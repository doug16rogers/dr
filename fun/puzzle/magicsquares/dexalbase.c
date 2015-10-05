#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#include "dms.h"

#define MAX_DEXAL_MAGIC_SQUARES 0x10000

typedef struct xref_s xref_t;

struct xref_s
{
  dms_t    dms;
  unsigned decomposed[4];
  unsigned binary_magic_code;
  int      index;
  int      group;
  int      visited;
};   /* struct xref_s */

typedef struct xtab_s xtab_t;

struct xtab_s
{
  xref_t*      list;
  unsigned int size;
  unsigned int count;
};   /* struct xtab_s */

xtab_t* xtab = NULL;

const char* hex_digit = "0123456789ABCDEF";

const unsigned binary4x4[] =
{
  0x35AC, 0x3C3C, 0x3CC3, 0x55AA, 0x5AA5, 0x5C3A, 0x6699, 0x6969,
  0x9696, 0x9966, 0xA3C5, 0xA55A, 0xAA55, 0xC33C, 0xC3C3, 0xCA53
};    /* binary4x4 */

/* ------------------------------------------------------------------------- */
unsigned hex_from_char(char c)
{
  if ((c >= '0') && (c <= '9')) return c - '0';
  if ((c >= 'A') && (c <= 'F')) return c - 'A' + 10;
  if ((c >= 'a') && (c <= 'a')) return c - 'a' + 10;
  return 0;
}   /* hex_from_char() */

/* ------------------------------------------------------------------------- */
unsigned bits_set(unsigned n)
{
  unsigned count = 0;

  while (n != 0)
  {
    count += n & 1;
    n >>= 1;
  }

  return count;
}   /* bits_set() */

/* ------------------------------------------------------------------------- */
int is_binary_magic(unsigned m, int include_diagonals)
{
  unsigned sum = bits_set(m & 0xF000);

  /*
   * Rows.
   */
  if (bits_set(m & 0x0F00) != sum) return 0;
  if (bits_set(m & 0x00F0) != sum) return 0;
  if (bits_set(m & 0x000F) != sum) return 0;

  /*
   * Columns.
   */
  if (bits_set(m & 0x8888) != sum) return 0;
  if (bits_set(m & 0x4444) != sum) return 0;
  if (bits_set(m & 0x2222) != sum) return 0;
  if (bits_set(m & 0x1111) != sum) return 0;

  /*
   * Diagonals.
   */
  if (include_diagonals)
  {
    if (bits_set(m & 0x8421) != sum) return 0;
    if (bits_set(m & 0x1248) != sum) return 0;
  }

  return 1;
}   /* is_binary_magic() */

/* ------------------------------------------------------------------------- */
/**
 * @return a composite flags (4+4) that indicate whether the decomposition
 * for that weight (1, 2, 4, and 8) was a binary magic square, without (msbs)
 * and with (lsbs) diagonal.
 */
unsigned dms_decompose(dms_t* self, unsigned binary[])
{
  int i;
  int magic = 0;
  binary[0] = 0;
  binary[1] = 0;
  binary[2] = 0;
  binary[3] = 0;

  for (i = 0; i < 0x10; i++)
  {
    unsigned hex = hex_from_char(self->line[i]);
    if (hex & 1) binary[0] |= 0x8000 >> i;
    if (hex & 2) binary[1] |= 0x8000 >> i;
    if (hex & 4) binary[2] |= 0x8000 >> i;
    if (hex & 8) binary[3] |= 0x8000 >> i;
  }

  if (is_binary_magic(binary[0], 1)) magic |= 0x01;
  if (is_binary_magic(binary[1], 1)) magic |= 0x02;
  if (is_binary_magic(binary[2], 1)) magic |= 0x04;
  if (is_binary_magic(binary[3], 1)) magic |= 0x08;

/*    if (is_binary_magic(binary[0], 0)) magic |= 0x10; */
/*    if (is_binary_magic(binary[1], 0)) magic |= 0x20; */
/*    if (is_binary_magic(binary[2], 0)) magic |= 0x40; */
/*    if (is_binary_magic(binary[3], 0)) magic |= 0x80; */

  return magic;
}   /* dms_decompose() */

/* ------------------------------------------------------------------------- */
dms_t* dms_add_one(dms_t* tgt, const dms_t* src)
{
  int i;

  for (i = 0; i < 0x10; i++)
  {
    tgt->line[i] = hex_digit[(hex_from_char(src->line[i]) + 1) % 0x10];
  }

  tgt->line[0x10] = 0;
  return tgt;
}   /* dms_add_one() */

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

/* ------------------------------------------------------------------------- */
xtab_t* xtab_new(unsigned max_entries)
{
  xtab_t* self = (xtab_t*) malloc(sizeof(xtab_t));

  if (self != NULL)
  {
    memset(self, 0, sizeof(*self));

    self->list = (xref_t*) malloc(max_entries * sizeof(xref_t));

    if (self->list == NULL)
    {
      free(self);
      self = NULL;
    }
    else
    {
      memset(self->list, 0, max_entries * sizeof(xref_t));
      self->size = max_entries;
    }
  }

  return self;
}   /* xtab_new() */

/* ------------------------------------------------------------------------- */
xtab_t* xtab_load(xtab_t*     self,
                  const char* filename)
{
  FILE* file = NULL;

  file = fopen(filename, "rt");

  if (file == NULL)
  {
    fprintf(stderr, "could not open'%s'\n", filename);
    exit(1);
  }

  while (self->count <= self->size)
  {
    if (fscanf(file, "#%4x: %s base\n",
               &self->list[self->count].index,
               self->list[self->count].dms.line) != 2)
    {
      break;
    }

    self->count++;
  }

  fclose(file);

  return self;
}   /* xtab_load() */

/* ------------------------------------------------------------------------- */
int xtab_index(const xtab_t* self,
               const dms_t*  dms)
{
  int bot = 0;
  int top = self->count - 1;
  int mid = self->count / 2;
  int diff;

/*    printf("compare %s to %s (bot)\n", dms->line, self->list[bot].dms.line); */
  diff = strcmp(dms->line, self->list[bot].dms.line);
  if (diff == 0) return bot;
  if (diff  < 0) return -1;
/*    printf("compare %s to %s (top)\n", dms->line, self->list[top].dms.line); */
  diff = strcmp(dms->line, self->list[top].dms.line);
  if (diff == 0) return top;
  if (diff  > 0) return -1;

  while (top >= bot)
  {
/*      printf("compare %s to %s (mid)\n", dms->line, self->list[mid].dms.line); */
    diff = strcmp(dms->line, self->list[mid].dms.line);

    if (diff == 0)
    {
      return mid;
    }

    if (diff < 0)
    {
      top = mid - 1;
    }
    else
    {
      bot = mid + 1;
    }

    mid = (top + bot) / 2;
  }

  return -1;
}   /* xtab_index() */

/* ------------------------------------------------------------------------- */
xref_t* xtab_lookup(const xtab_t* self,
                    const dms_t*  dms)
{
  int index = xtab_index(self, dms);

  if (index < 0)
  {
    return NULL;
  }

  return &self->list[index];
}   /* xtab_lookup() */

/* ------------------------------------------------------------------------- */
void xtab_clear_visited(xtab_t* self)
{
  int i;

  for (i = 0; i < self->count; i++)
  {
    self->list[i].visited = 0;
  }
}   /* xtab_clear_visited() */

/* ------------------------------------------------------------------------- */
void check_index(void)
{
  dms_t dms;
  strcpy(dms.line, "7F08A96514BEC2D3");
  printf("index=%d\n", xtab_index(xtab, &dms));
  strcpy(dms.line, "7F08A96514BEC2D1");
  printf("index=%d\n", xtab_index(xtab, &dms));
  strcpy(dms.line, "XF08A96514BEC2D1");
  printf("index=%d\n", xtab_index(xtab, &dms));
  strcpy(dms.line, " F08A96514BEC2D1");
  printf("index=%d\n", xtab_index(xtab, &dms));
}   /* check_index() */

/* ------------------------------------------------------------------------- */
dms_t* dms_single_c(dms_t* tgt, const dms_t* src)
{
  strcpy(tgt->line, src->line);
  tgt->array[0][1] = src->array[0][2];
  tgt->array[1][1] = src->array[2][2];
  tgt->array[2][1] = src->array[1][2];
  tgt->array[3][1] = src->array[3][2];
  tgt->array[0][2] = src->array[0][1];
  tgt->array[1][2] = src->array[2][1];
  tgt->array[2][2] = src->array[1][1];
  tgt->array[3][2] = src->array[3][1];
  return tgt;
}   /* dms_single_c() */

/* ------------------------------------------------------------------------- */
dms_t* dms_single_r(dms_t* tgt, const dms_t* src)
{
  strcpy(tgt->line, src->line);
  tgt->array[1][0] = src->array[2][0];
  tgt->array[1][1] = src->array[2][2];
  tgt->array[1][2] = src->array[2][1];
  tgt->array[1][3] = src->array[2][3];
  tgt->array[2][0] = src->array[1][0];
  tgt->array[2][1] = src->array[1][2];
  tgt->array[2][2] = src->array[1][1];
  tgt->array[2][3] = src->array[1][3];
  return tgt;
}   /* dms_single_r() */

/* ------------------------------------------------------------------------- */
dms_t* dms_double_d0(dms_t* tgt, const dms_t* src)
{
  strcpy(tgt->line, src->line);
  tgt->array[2][0] = src->array[3][1];
  tgt->array[2][1] = src->array[3][0];
  tgt->array[2][2] = src->array[3][3];
  tgt->array[2][3] = src->array[3][2];
  tgt->array[3][0] = src->array[2][1];
  tgt->array[3][1] = src->array[2][0];
  tgt->array[3][2] = src->array[2][3];
  tgt->array[3][3] = src->array[2][2];
  return tgt;
}   /* dms_double_d0() */

/* ------------------------------------------------------------------------- */
dms_t* dms_double_d3(dms_t* tgt, const dms_t* src)
{
  strcpy(tgt->line, src->line);
  tgt->array[2][0] = src->array[3][1];
  tgt->array[2][1] = src->array[3][0];
  tgt->array[2][2] = src->array[3][3];
  tgt->array[2][3] = src->array[3][2];
  tgt->array[3][0] = src->array[2][1];
  tgt->array[3][1] = src->array[2][0];
  tgt->array[3][2] = src->array[2][3];
  tgt->array[3][3] = src->array[2][2];
  return tgt;
}   /* dms_double_d3() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[])
{
  int i;
  xref_t* x;

/*    check_ops(); */
  xtab = xtab_new(MAX_DEXAL_MAGIC_SQUARES);

  if (xtab == NULL)
  {
    fprintf(stderr, "out of memory\n");
    return 1;
  }

  xtab_load(xtab, "dexal-base-squares.txt");
  printf("loaded %u magic squares.\n", xtab->count);

  for (i = 0; i < xtab->count; i++)
  {
    dms_t   dms;
    xref_t* other;
    x = &xtab->list[i];

    dms_single_r(&dms, &x->dms);
    other = xtab_lookup(xtab, &dms);

    if (other != NULL)
    {
      printf("#%04X: %s single-r = %s #%04X\n",
             x->index, x->dms.line,
             other->dms.line, other->index);
    }

    dms_single_c(&dms, &x->dms);
    other = xtab_lookup(xtab, &dms);

    if (other != NULL)
    {
      printf("#%04X: %s single-c = %s #%04X\n",
             x->index, x->dms.line,
             other->dms.line, other->index);
    }

    dms_double_d0(&dms, &x->dms);
    other = xtab_lookup(xtab, &dms);

    if (other != NULL)
    {
      printf("#%04X: %s double-0 = %s #%04X\n",
             x->index, x->dms.line,
             other->dms.line, other->index);
    }

    dms_double_d3(&dms, &x->dms);
    other = xtab_lookup(xtab, &dms);

    if (other != NULL)
    {
      printf("#%04X: %s double-3 = %s #%04X\n",
             x->index, x->dms.line,
             other->dms.line, other->index);
    }

/*      x->binary_magic_code = dms_decompose(&x->dms, x->decomposed); */
/*      printf("#%04X: %s binary %04X %04X %04X %04X code %01X\n", */
/*             x->index, x->dms.line, */
/*             x->decomposed[0], */
/*             x->decomposed[1], */
/*             x->decomposed[2], */
/*             x->decomposed[3], */
/*             x->binary_magic_code); */
  }   /* for each */

  return 0;
}   /* main() */
