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

#endif
