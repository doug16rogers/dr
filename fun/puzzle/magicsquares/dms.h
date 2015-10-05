/*
 * Dexal magic square support code.
 */

#ifndef __dms_h__
#define __dms_h__

/* ------------------------------------------------------------------------- */
typedef union dms_u dms_t;

union dms_u
{
  char array[4][4];  /**< 4x4 dexal square. */
  char line[20];     /**< To include terminator and keep aligned. */
};   /* struct dms_u */

/* ------------------------------------------------------------------------- */
typedef dms_t* (*dms_operation_t)(dms_t* target, const dms_t* source);

typedef struct dms_op_s dms_op_t;

struct dms_op_s
{
  const char*     name;
  dms_operation_t op;
};   /* struct dms_op_s */

#define DMS_OPS 5

extern const dms_op_t dms_op[];

extern dms_t* dms_comp(dms_t* target, const dms_t* source);
extern dms_t* dms_flip(dms_t* target, const dms_t* source);
extern dms_t* dms_spin(dms_t* target, const dms_t* source);
extern dms_t* dms_bull(dms_t* target, const dms_t* source);
extern dms_t* dms_quad(dms_t* target, const dms_t* source);
/* extern dms_t* dms_spew(dms_t* target, const dms_t* source); */
/* extern dms_t* dms_barf(dms_t* target, const dms_t* source); */
/* extern dms_t* dms_binx(dms_t* target, const dms_t* source); */
/* extern dms_t* dms_xor2(dms_t* target, const dms_t* source); */

extern int  dms_equal(const dms_t* target, const dms_t* source);
extern char dms_char_invert(char c);
extern void dms_print_array(const char* label, const dms_t* m);
extern void dms_print(dms_t* m);

#endif
