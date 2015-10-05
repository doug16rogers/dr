#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

typedef unsigned long long uint64_t;

#define MAX_DEXAL_MAGIC_SQUARES 0x10000

typedef union dms_u dms_t;

union dms_u
{
  char array[4][4];
  char line[20];     /* To include terminator. */
};   /* struct dms_u */

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
int dms_equal(const dms_t* target, const dms_t* source)
{
  return strcmp(target->line, source->line) == 0;
}   /* dms_equal() */

/* ------------------------------------------------------------------------- */
char char_invert(char c)
{
  if ((c >= '0') && (c <= '9')) c -= '0';
  else if ((c >= 'A') && (c <= 'F')) c = c - 'A' + 10;
  else if ((c >= 'a') && (c <= 'a')) c = c - 'a' + 10;
  else c = 15;

  return hex_digit[15 - c];
}   /* char_invert() */

/* ------------------------------------------------------------------------- */
dms_t* dms_invert(dms_t* target, const dms_t* source)
{
  int i;

  for (i = 0; i < 0x10; i++)
  {
    target->line[i] = char_invert(source->line[i]);
  }

  target->line[0x10] = 0;

  return target;
}   /* dms_invert() */

/* ------------------------------------------------------------------------- */
dms_t* dms_transpose(dms_t* target, const dms_t* source)
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
}   /* dms_transpose() */

/* ------------------------------------------------------------------------- */
dms_t* dms_rotate(dms_t* target, const dms_t* source)
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
}   /* dms_rotate() */

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
void check_ops(void)
{
  dms_t m;
  dms_t t;

  strcpy(m.line, "0123456789ABCDEF");
  printf("m=%s\n", m.line);
  printf("i=%s\n", dms_invert(&t, &m)->line);
  printf("t=%s\n", dms_transpose(&t, &m)->line);
  printf("r=%s\n", dms_rotate(&t, &m)->line);
}   /* check_ops() */

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
void check_group(xref_t* x, int group)
{
  if (x->visited) return;

  if (x->group != group)
  {
    fprintf(stderr, "index #%04X group #%04X not expected #%04X\n",
            x->index, x->group, group);
    exit(1);
  }

  x->visited = 1;
/*    check_group(x->inverse, group); */
/*    check_group(x->rotated, group); */
/*    check_group(x->transpose, group); */
}   /* check_group() */

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
int main(int argc, char* argv[])
{
  int i, j;
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
    dms_t dms;

    x = &xtab->list[i];
    x->binary_magic_code = dms_decompose(&x->dms, x->decomposed);
    dms = x->dms;

    for (j = 1; j < 8; j++)
    {
      int index = xtab_index(xtab, dms_add_one(&dms, &dms));

      if (index >= 0)
      {
        printf("#%04X: %s + %d = %s #%04X\n",
               x->index, x->dms.line,
               j,
               dms.line, xtab->list[index].index);
      }
      else
      {
        printf("#%04X: %s + %d = %s not found\n",
               x->index, x->dms.line,
               j,
               dms.line);
      }
    }

/*      printf("#%04X: %s binary %04X %04X %04X %04X code %01X\n", */
/*             x->index, x->dms.line, */
/*             x->decomposed[0], */
/*             x->decomposed[1], */
/*             x->decomposed[2], */
/*             x->decomposed[3], */
/*             x->binary_magic_code); */
  }   /* for each */

#if 0
  for (i = 0; i < xtab->count; i++)
  {
    x = &xtab->list[i];
    x->binary_magic_code = dms_decompose(&x->dms, x->decomposed);
    printf("#%04X: %s binary %04X %04X %04X %04X code %01X\n",
           x->index, x->dms.line,
           x->decomposed[0],
           x->decomposed[1],
           x->decomposed[2],
           x->decomposed[3],
           x->binary_magic_code);
  }   /* for each */
#endif

  return 0;
}   /* main() */
