#ifndef INCLUDED_MATRIX_H
#define INCLUDED_MATRIX_H

float **matrix(int nrl, int nrh, int ncl, int nch);
void free_matrix(float **m,int nrl,int nrh,int ncl,int nch);
void matrix_multiply(float **a, float **b, float **c, int r1, int c1, int c2);

#endif
