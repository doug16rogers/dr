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
  dms_t dms;
  int     index;
  int     group;
  int     visited;
  xref_t* rotated;
  xref_t* inverse;
  xref_t* transpose;
  xref_t* single_x;
  xref_t* double_x;
};   /* struct xref_s */

typedef struct xtab_s xtab_t;

struct xtab_s
{
  xref_t*      list;
  unsigned int size;
  unsigned int count;
};   /* struct xtab_s */

xtab_t* xtab = NULL;

/* ------------------------------------------------------------------------- */
int dms_equal(const dms_t* target, const dms_t* source)
{
  return strcmp(target->line, source->line) == 0;
}   /* dms_equal() */

/* ------------------------------------------------------------------------- */
char char_invert(char c)
{
  const char* hex_digit = "0123456789ABCDEF";

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
dms_t* dms_single_x(dms_t* target, const dms_t* source)
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
}   /* dms_single_x() */

/* ------------------------------------------------------------------------- */
dms_t* dms_double_x(dms_t* target, const dms_t* source)
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
  return target;
}   /* dms_double_x() */

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
    if (fscanf(file, "%s", self->list[self->count].dms.line) != 1)
    {
      break;
    }

    self->list[self->count].index = self->count;
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
}   /* xref_index() */

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
  check_group(x->inverse, group);
  check_group(x->rotated, group);
  check_group(x->transpose, group);
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
  dms_t dms;
  int i;
  int changed;
  xref_t* x;
  int n;

/*    check_ops(); */
  xtab = xtab_new(MAX_DEXAL_MAGIC_SQUARES);

  if (xtab == NULL)
  {
    fprintf(stderr, "out of memory\n");
    return 1;
  }

  xtab_load(xtab, "dexal-magic-squares.txt");
  printf("loaded %u magic squares.\n", xtab->count);

  for (i = 0; i < xtab->count; i++)
  {
    x = &xtab->list[i];
    x->inverse = xtab_lookup(xtab, dms_invert(&dms, &x->dms));
    x->rotated = xtab_lookup(xtab, dms_rotate(&dms, &x->dms));
    x->transpose = xtab_lookup(xtab, dms_transpose(&dms, &x->dms));
    x->group = i;
    if (x->inverse->index   < x->group) x->group = x->inverse->index;
    if (x->rotated->index   < x->group) x->group = x->rotated->index;
    if (x->transpose->index < x->group) x->group = x->transpose->index;
  }   /* for each */

  printf("finished generating inverses, etc.\n");

  n = 0;

  do
  {
    n++;
    printf("consolidating groups, iteration %u.\n", n);
    changed = 0;

    for (i = 0; i < xtab->count; i++)
    {
      x = &xtab->list[i];

      if (x->inverse->group < x->group)
      {
        x->group = x->inverse->group;
        changed = 1;
      }
        
      if (x->transpose->group < x->group)
      {
        x->group = x->transpose->group;
        changed = 1;
      }
        
      if (x->rotated->group < x->group)
      {
        x->group = x->rotated->group;
        changed = 1;
      }
    }   /* for each item, juggling groups. */
  } while (changed);   /* while no change is registered */

  for (i = 0; i < xtab->count; i++)
  {
    x = &xtab->list[i];
    printf("#%04X: %s", i, x->dms.line);

    if (x->group == i)
    {
      printf(" base\n");

      xtab_clear_visited(xtab);
      check_group(x, i);
    }
    else
    {
      printf(" in group #%04X\n", x->group);
    }
  }

  return 0;
}   /* main() */
