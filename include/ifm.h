/*************************************************************************** 
   NAME:  IFM.H

   Header file for the ifm.a library. Contains function declarations as 
   well as any necessary constants.

   Mike Shindle, 10/11/95

   Revisions:
   june 7, 1996 - added stdio.h call
   september 10, 1998 - Removed ancient, unneeded types.

***************************************************************************/

#ifndef __IFM_H     /* include only once */

#define __IFM_H

#include "asf.h"  /* for FILE * variable type */

/* Complex Type declarations */

typedef struct {
    unsigned char red;
    unsigned char green;
    unsigned char blue;
} RGBDATA;

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

#ifndef __complex_var
#define __complex_var
typedef struct {
    float real;
    float imag;
} Complex;
#endif

#ifndef __bool_var
#define __bool_var
typedef int bool;
#endif

#ifndef __data_t_var
#define __data_t_var
typedef int data_t;
#endif

#ifndef __Uchar_var
#define __Uchar_var
typedef unsigned char Uchar;
#endif

#ifndef __Schar_var
#define __Schar_var
typedef signed char Schar;
#endif



/* function declarations */
double Cabs_d(DComplex);
double **dmatrix(int,int,int,int);
double *dvector(int,int);
double d_vmag(double*,int);

DComplex Cadd_d(DComplex,DComplex);
DComplex Cconj_d(DComplex);
DComplex Csmul_d(double, DComplex);
FComplex Cadd(FComplex,FComplex);
FComplex Cconj(FComplex);
FComplex Cmul(FComplex,FComplex);
FComplex **cpxmatrix(int,int,int,int);
FComplex *cpxvector(int,int);
FComplex Csdiv(float, FComplex);
FComplex Csmul(float, FComplex);
FComplex Czero(void);
float Cabs(FComplex);
float Cphase(FComplex);

/****FILE *fileOpen(char*,char*);***should use FOPEN*******/
int fileExist(char *);
int fileNumLines(char *);

float get_mean(FILE *,int,int,int);
float **matrix(int,int,int,int);
float *alloc_vector(int min,int max);
float vmag(float *, int);
int checkDataType(data_t);

void Alert(char *, ...);
void copyVector(void *,void *,data_t,data_t,int);
void Exit(char *, ...);
void free_dmatrix(double **,int,int,int,int);
void free_dvector(double *,int,int);
void free_matrix(float **,int,int,int,int);
void free_vector(float *,int,int);
void matrix_multiply(float**,float**,float**,int,int,int);

void print_matrix(float **,int,int,int,int,char*);
void print_vector(float *,int,int,char*);

void readMatrix(char *,void *,data_t,int,int,int,int,int,int,int,int);
void readVector(void *,char *,data_t,int);
void svdcmp(float **,int,int,float *,float **);

void writeAsciiVector(void *,char *,data_t,int);
void writeVector(void *,char *,data_t,int);


/* general constants */
#ifndef SCANLINES
#define SCANLINES	50
#endif
#ifndef AVG
#define AVG		128.0
#endif

/* empirical number of clock ticks / second, sun IPX */
#define MY_CLK_TCK   470000

/* general constants */
#ifndef TRUE
# define TRUE      1
#endif
#define YES        1
#define ON         1
#define ACTIVE     1
#define DO_IT      1
#ifndef FALSE
# define FALSE     0
#endif
#define NO         0
#define OFF        0
#define INACTIVE   0
#define DONT_DO_IT 0
#define MAXNAME         256
#define FORWARD_FFT	  1
#define INVERSE_FFT	 -1

/* radar identification tags */
#define EOS_SAR         -8
#define E_ERS_1         -7
#define ERS1            -7
#define J_ERS_1         -6
#define JERS1           -6
#define RADARSAT        -5
#define SIR_A           -4
#define SIR_B           -2
#define SIR_C           -3
#define SEASAT          -1
#define FRAMSAT          0
#define AC_SAR_1         1
#define AC_SAR_PRE_BURN  1
#define AC_SAR_2         2
#define AC_SAR_PRE_88    2
#define AC_SAR           3
#define AC_SAR_3         3
#define AC_SAR_CURRENT   3

/* carrier frequencies */
#define ERS1_CARRIER_FREQ  5.29995789e9;
#define JERS1_CARRIER_FREQ 1.27500000e9;

/* platform tag */
#define SC     1
#define AC     2

/* data types */
#define STRING            -1
#define INT                0
#define INTEGER            0
#define SHORT_INT          1
#define FLOAT              2
#define DOUBLE             3
#define CHAR               4
#define UCHAR              5
#define COMPLEX            6
#define FLOAT_COMPLEX      6
#define DOUBLE_COMPLEX     7
#define SHORT_INT_COMPLEX  8
#define INT_COMPLEX        9

/* numerical constants */
#define PIOVER2                  1.57079632680
#define PIOVER3                  1.04719755120
#define PIOVER4                  0.785398163398
#define TWOPI                    6.28318530718
#define FOURPI                   (2.0*TWOPI)
#define DTR                      1.74532925199e-2
#define RTD                      57.2957795131
#define QUARTER                  0.25
#define HALF                     0.5
#define THREE_QUARTERS           0.75
#define ONE                      1.0
#define ROOT_TWO                 1.41421356237
#define ROOT_THREE               1.73205080757
#define ROOT_TEN                 3.16227766017
#define TWO                      2.0
#define THREE                    3.0
#define FOUR                     4.0

/* mathematical constants */
#define EPSILON 3.0e-9       /* a small number */

/* physical constants */
#define CLIGHT                   2.997924562e8

/* earth related constants */
#define RE                       6378177.8
#define EARTHRADIUS              6378177.8
#define RP                       6356792.8
#define EARTHPOLRAD              6356792.8
#define EARTHGRAV                3.986005e14
#define EARTHANGVEL              7.29211585e-5
#define EARTHELLIPTICITY         298.255

/* unitary conversion constants */
#define FEET_TO_METERS 12.0*2.54/100.0
#define METERS_TO_FEET 100.0/(12.0*2.54)

/* CEOS constants */
#define NUL 0
#define VDR 1
#define DSS 2
#define PPR 3
#define ADR 4
#define RDR 5
#define RCR 6
#define DQS 7
#define DHR 8
#define RSR 9
#define DPR 10
#define CDR 11
#define FRR 12
#define OLD 13

#define PRE_RCH        12
#define POST_RCH       192


/* 
 * Create max and min macros 
 * create square macro
 */
#ifndef max
#define max(x,y)   ((x) > (y)) ? (x) : (y)
#endif
#ifndef min
#define min(x,y)   ((x) < (y)) ? (x) : (y)
#endif
#ifndef SQR
#define SQR(a)     ( (a) * (a) )
#endif
#ifndef CUBE
#define CUBE(a)    ( (a) * (a) * (a) )
#endif


#endif  /* end of include file */
