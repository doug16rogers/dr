#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#include "dms.h"

#define MAX_DEXAL_MAGIC_SQUARES  0x10000

typedef struct xref_s xref_t;

struct xref_s
{
  dms_t dms;
  int     index;
  int     group;
  int     visited;
  xref_t* op[DMS_OPS];   /**< DMS after operation applied. */
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
  int op;

  if (x->visited) return;

  if (x->group != group)
  {
    fprintf(stderr, "index #%04X group #%04X not expected #%04X\n",
            x->index, x->group, group);
    exit(1);
  }

  x->visited = 1;

  for (op = 0; op < DMS_OPS; op++)
  {
    check_group(x->op[op], group);
  }
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
  int op;
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
  printf("loaded 0x%04X magic squares.\n", xtab->count);

  for (i = 0; i < xtab->count; i++)
  {
/*      if ((i & 0x0F) == 0x00) printf("#%04X: being operated upon.\n", i); */

    x = &xtab->list[i];
    x->group = i;

    for (op = 0; op < DMS_OPS; op++)
    {
      dms_op[op].op(&dms, &x->dms);
      x->op[op] = xtab_lookup(xtab, &dms);

      if (x->op[op] == NULL)
      {
        printf("ERROR: #%04X: %s -%s-> %s not in table! Aborting.\n",
               i, x->dms.line,
               dms_op[op].name,
               dms.line);
        return 1;
      }

      if (x->op[op]->index < x->group)
      {
        x->group = x->op[op]->index;
      }
    }   /* for each operation */
  }   /* for each dms */

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

      for (op = 0; op < DMS_OPS; op++)
      {
        if (x->op[op]->group < x->group)
        {
          x->group = x->op[op]->group;
          changed = 1;
        }
      }   /* for each magic operation */
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
