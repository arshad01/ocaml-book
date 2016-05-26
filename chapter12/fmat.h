#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#ifndef _FMAT_H_
#define _FMAT_H_

typedef struct _float_matrix_t_ {
    int rows;
    int cols;
    double *data;
} float_matrix_t;

value array_to_fmat (value);
value fmat_to_array (value);
value fmat_add      (value, value);
value fmat_mul      (value, value);

void  fin_fmat             (value);
float_matrix_t *fmat_alloc (int, int);
void  fmat_free            (float_matrix_t *);
value make_fmat            (value);
value make_fmat2           (int,int);
value make_faa             (int,int);
double get                 (float_matrix_t*,int,int);
void   set                 (float_matrix_t*,int,int,double);

#endif