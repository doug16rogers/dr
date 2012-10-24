/*
 * Dexal magic tesseract support code.
 */

#ifndef __dmt_h__
#define __dmt_h__

/* ------------------------------------------------------------------------- */
typedef union dmt_u dmt_t;

union dmt_u
{
  unsigned char array[4][4][4][4];  /**< 4x4x4x4 dexal tesserac. */
  unsigned char line[4*4*4*4];      /**< One-D listing. */
};   /* struct dmt_u */

/* ------------------------------------------------------------------------- */
typedef dmt_t* (*dmt_operation_t)(dmt_t* target, const dmt_t* source);

typedef struct dmt_op_s dmt_op_t;

struct dmt_op_s
{
  const char*     name;
  dmt_operation_t op;
};   /* struct dmt_op_s */

typedef unsigned char dmt_row_t[4];

#define DMT_OPS 5

extern const dmt_op_t dmt_op[];

extern int dmt_check(const dmt_t* self);

extern dmt_t* dmt_comp(dmt_t* target, const dmt_t* source);
extern dmt_t* dmt_flip(dmt_t* target, const dmt_t* source);
extern dmt_t* dmt_spin(dmt_t* target, const dmt_t* source);

extern int  dmt_equal(const dmt_t* target, const dmt_t* source);
extern char dmt_char_invert(char c);
extern void dmt_print_array(const char* label, const dmt_t* m);
extern void dmt_print(dmt_t* m);

#define DMT_MAGIC_SUM        0x01FE
#define DMT_ORDERED_ROWS     0x0006FCB2
#define DMT_ROW_PERMUTATIONS 0x18

extern void dmt_print_all_rows(void);
extern void dmt_print_ordered_rows(void);
extern void dmt_gen_ordered_rows(dmt_row_t* list);  /* Of DMT_ORDERED_ROWS. */
extern void dmt_permute_row(const dmt_row_t row,
                            dmt_row_t*      permutations);
extern int dmt_row_elements_different(dmt_row_t row);
extern unsigned dmt_row_sum(dmt_row_t row);
extern int dmt_row_okay(dmt_row_t row);

#endif
