
#ifndef __ASF_COMPLEX_H
#define __ASF_COMPLEX_H

/*-------------------------
 * define complex variables
 *-----------------------*/

/* Not used yet ***********
typedef struct {
   unsigned char real;
   unsigned char imag;
} complexByte;
**************************/

/* Not used yet ***********
typedef struct {
   short int real;
   short int imag;
} complexShortInt;
**************************/

/* Not used yet ***********
typedef struct {
   int real;
   int imag;
} complexInt;
**************************/

typedef struct {
   float real;
   float imag;
} complexFloat;

typedef struct {
   double real;
   double imag;
} complexDouble;



/******************************************************************************/
/* Depricated structs. Kept for compilability until they've been obliterated  */
/******************************************************************************/
typedef struct {
   float r;
   float i;
} FCMPLX;

#ifndef __fcomplex_var
#define __fcomplex_var
typedef struct {
    float real;
    float imag;
} FComplex;

typedef FComplex fcomplex;
#endif

#ifndef __dcomplex_var
#define __dcomplex_var
typedef struct {
    double real;
    double imag;
} DComplex;

typedef DComplex dcomplex;
#endif
/******************************************************************************/
/*************************** End depricated structs ***************************/
/******************************************************************************/


#endif
