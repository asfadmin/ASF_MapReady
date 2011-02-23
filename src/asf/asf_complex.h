#ifndef ASF_COMPLEX_H
#define ASF_COMPLEX_H

typedef struct {
   unsigned char real;
   unsigned char imag;
} complexByte;

typedef struct {
   short int real;
   short int imag;
} complexShortInt;

typedef struct {
   int real;
   int imag;
} complexInt;

typedef struct {
   float real;
   float imag;
} complexFloat;

typedef struct {
   double real;
   double imag;
} complexDouble;

typedef struct {
    complexFloat A;
    complexFloat B;
    complexFloat C;
} complexVector;

typedef struct {
  float A;
  float B;
  float C;
} floatVector;

typedef struct {
    int rows, columns;
    complexFloat **coeff;
} complexMatrix;

typedef struct {
   complexFloat hh;
   complexFloat hv;
   complexFloat vh;
   complexFloat vv;
} quadPolS2Float;

typedef struct {
  float c11;
  float c12_real;
  float c12_imag;
  float c13_real;
  float c13_imag;
  float c22;
  float c23_real;
  float c23_imag;
  float c33;
} quadPolC3Float;

typedef struct {
  float t11;
  float t12_real;
  float t12_imag;
  float t13_real;
  float t13_imag;
  float t22;
  float t23_real;
  float t23_imag;
  float t33;
} quadPolT3Float;

complexFloat complex_new(float re, float im);
complexFloat complex_new_polar(float amp, float phase);
complexFloat complex_zero(void);
complexFloat complex_sub(complexFloat a, complexFloat b);
complexFloat complex_add(complexFloat a, complexFloat b);
float complex_amp(complexFloat c);
float complex_amp_sqr(complexFloat c);
float complex_arg(complexFloat c);
complexFloat complex_scale(complexFloat c, float f);
complexFloat complex_conj(complexFloat c);
complexFloat complex_mul(complexFloat a, complexFloat b);
complexFloat complex_div(complexFloat a, complexFloat b);
complexVector complex_vector_new(complexFloat a, complexFloat b,
                                 complexFloat c);
complexVector complex_vector_conj(complexVector v);
complexVector complex_vector_normalize(complexVector v);
complexVector complex_vector_zero(void);
complexMatrix *complex_matrix_new(int rows, int columns);
complexMatrix *complex_matrix_mul(complexMatrix *m1, complexMatrix *m2);
void complex_matrix_scale(complexMatrix *m, float s);
void complex_matrix_free(complexMatrix *doomed);
complexMatrix *complex_matrix_mul3(complexMatrix *m1, complexMatrix *m2,
                                   complexMatrix *m3);
void complex_matrix_set(complexMatrix *self, int row, int column,
                        complexFloat value);
complexFloat complex_matrix_get(complexMatrix *self,
                                int row, int column);
complexMatrix *complex_matrix_new22(complexFloat e00, complexFloat e01,
                                    complexFloat e10, complexFloat e11);

#endif
