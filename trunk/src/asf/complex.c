#include "asf.h"
#include "asf_complex.h"

complexFloat complex_new(float re, float im)
{
    complexFloat ret;
    ret.real = re;
    ret.imag = im;
    return ret;
}

complexFloat complex_new_polar(float amp, float phase)
{
    complexFloat ret;
    ret.real = amp * cos(phase);
    ret.imag = amp * sin(phase);
    return ret;
}

complexFloat complex_zero()
{
    return complex_new(0,0);
}

complexFloat complex_sub(complexFloat a, complexFloat b)
{
    return complex_new(a.real-b.real, a.imag-b.imag);
}

complexFloat complex_add(complexFloat a, complexFloat b)
{
    return complex_new(a.real+b.real, a.imag+b.imag);
}

float complex_amp(complexFloat c)
{
  return (float)hypot(c.real, c.imag);
  //return sqrt(c.real*c.real + c.imag*c.imag);
}

float complex_amp_sqr(complexFloat c)
{
  return c.real*c.real + c.imag*c.imag;
}

float complex_arg(complexFloat c)
{
  return (float)atan2(c.imag, c.real);
}

complexFloat complex_scale(complexFloat c, float f)
{
    return complex_new(c.real*f, c.imag*f);
}

complexFloat complex_conj(complexFloat c)
{
    return complex_new(c.real, -c.imag);
}

complexFloat complex_mul(complexFloat a, complexFloat b)
{
    return complex_new(a.real*b.real - a.imag*b.imag,
                       a.real*b.imag + a.imag*b.real);
}

complexFloat complex_div(complexFloat a, complexFloat b)
{
  return complex_new (
    (a.real*b.real + a.imag*b.imag)/(b.real*b.real + b.imag*b.imag),
    (a.imag*b.real - a.real*b.imag)/(b.real*b.real + b.imag*b.imag));
}

complexVector complex_vector_new(complexFloat a,
                                 complexFloat b,
                                 complexFloat c)
{
    complexVector ret;
    ret.A = a;
    ret.B = b;
    ret.C = c;
    return ret;
}

complexVector complex_vector_conj(complexVector v)
{
    return complex_vector_new(complex_conj(v.A),
        complex_conj(v.B), complex_conj(v.C));
}

complexVector complex_vector_normalize(complexVector v)
{
    complexVector ret;
    double mag = complex_amp(v.A) + complex_amp(v.B) + complex_amp(v.C);
    ret.A.real = v.A.real/mag;   ret.A.imag = v.A.imag/mag;
    ret.B.real = v.B.real/mag;   ret.B.imag = v.B.imag/mag;
    ret.C.real = v.C.real/mag;   ret.C.imag = v.C.imag/mag;
    return ret;
}

complexVector complex_vector_zero()
{
    return complex_vector_new(complex_zero(), complex_zero(), complex_zero());
}

complexMatrix *complex_matrix_new(int rows, int columns)
{
    int i,j;
    complexMatrix *ret = MALLOC(sizeof(complexMatrix));
    ret->rows = rows;
    ret->columns = columns;
    ret->coeff = (complexFloat**)MALLOC(sizeof(complexFloat*)*rows);
    for (i=0; i<rows; ++i) {
        ret->coeff[i]=(complexFloat*)MALLOC(sizeof(complexFloat)*columns);
        for (j=0; j<columns; ++j)
            ret->coeff[i][j] = complex_zero();
    }
    return ret;
}

complexMatrix *complex_matrix_mul(complexMatrix *m1, complexMatrix *m2)
{
  if (m1->columns != m2->columns)
    asfPrintError("complex_matrix_mul: Mismatched matrices.\n");

  complexMatrix *ret = complex_matrix_new(m1->rows, m2->columns);

  int i,j,k;
  for (i=0; i<m1->rows; ++i) {
    for (j=0; j<m2->columns; ++j) {
      for (k=0; k<m1->columns; ++k) {
        ret->coeff[i][j] = complex_add(ret->coeff[i][j],
                                       complex_mul(m1->coeff[i][k],
                                                   m2->coeff[k][j]));
      }
    }
  }

  return ret;
}

void complex_matrix_scale(complexMatrix *m, float s)
{
  int i,j;
  for (i=0; i<m->rows; ++i)
    for (j=0; j<m->columns; ++j) {
      m->coeff[i][j].real *= s;
      m->coeff[i][j].imag *= s;
    }
}

void complex_matrix_free(complexMatrix *doomed)
{
    int i;
    for (i=0; i<doomed->rows; ++i)
        FREE(doomed->coeff[i]);
    FREE(doomed->coeff);
    FREE(doomed);
}

complexMatrix *complex_matrix_mul3(complexMatrix *m1, complexMatrix *m2,
                                   complexMatrix *m3)
{
  complexMatrix *tmp = complex_matrix_mul(m1, m2);
  complexMatrix *ret = complex_matrix_mul(tmp, m3);
  complex_matrix_free(tmp);
  return ret;
}

void complex_matrix_set(complexMatrix *self, int row, int column,
                        complexFloat value)
{
    self->coeff[row][column] = value;
}

complexFloat complex_matrix_get(complexMatrix *self,
                                int row, int column)
{
    return self->coeff[row][column];
}

complexMatrix *complex_matrix_new22(complexFloat e00, complexFloat e01,
                                    complexFloat e10, complexFloat e11)
{
  complexMatrix *ret = complex_matrix_new(2, 2);
  complex_matrix_set(ret, 0, 0, e00);
  complex_matrix_set(ret, 0, 1, e01);
  complex_matrix_set(ret, 1, 0, e10);
  complex_matrix_set(ret, 1, 1, e11);
  return ret;
}
