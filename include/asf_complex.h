#ifndef __ASF_COMPLEX_H
#define __ASF_COMPLEX_H

/*-------------------------
 * define complex variables
 *-----------------------*/
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


/******************************************************************************/
/* Depricated structs. Kept for compilability until they've been obliterated  */
/******************************************************************************/
typedef struct {
   float r;
   float i;
} FCMPLX;
/******************************************************************************/
/*************************** End depricated structs ***************************/
/******************************************************************************/


#endif
