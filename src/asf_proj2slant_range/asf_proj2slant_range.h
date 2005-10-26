#ifndef __ASF_PROJ2SLANT_RANGE_H
#define __ASF_PROJ2SLANT_RANGE_H

#include "asf.h"
#include "asf_meta.h"

/* Prototypes for functions */
void create_dem_grid(char *demName, char *sarName, char *outName);
void fit_poly(char *inName, int degree, char *outName);
void reskew_dem(char *inMetafile, char *inDEMfile, char *outDEMfile, 
                char *outAmpFile);


/************************************************************
Include file for C matrix subroutines.

Orion Sky Lawlor 3/99
*/

typedef struct {
        int rows;/*# of rows (horiz. lines) in matrix*/
        int columns;/*# of columns (vert. lines) in matrix*/
        double **coeff;/*a [rows][columns] array of matrix coefficents.*/
} matrix;

matrix *matrix_alloc(int rows,int columns);/*Create a zero-valued matrix*/
matrix *matrix_dup(const matrix *source);
void matrix_free(matrix *doomed);
void matrix_print(matrix *this,const char *message,FILE *stream);

/*Row operations: Swap rows A and B.*/
void matrix_rowSwap(matrix *this,int A,int B);

/*Row operations: multiply [row dest] by scale*/
void matrix_rowScale(matrix *this,int dest,double scale);

/*Row operations: add scale*[row source] to [row dest]*/
void matrix_rowAddScale(matrix *this,int dest,double scale,int source);

/*Solve the given (columns>rows) matrix
by Gaussian Elimination.  Exits with error if
given matrix is singular.*/
void matrix_solve(matrix *this);

/************************************************************/

/****************************************************************
Include file for: Polynomial 2D functions.

This header describes a set of routines for
manipulating a Polynomial 2D function-- there
are routines for evaluating, printing, and
finding (in a least-squares fasion) these functions.
*/

/*
Polynomial warping coefficients.


*/
typedef struct {
        /**
          Degree of polynomial: 
        0 is constant: 
                v[0]
        1 is linear:
                v[0] + v[1]*x+v[2]*y
        2 is quadratic:
                v[0] + v[1]*x+v[2]*y + v[3]*x*x+v[4]*x*y+v[5]*y*y
        3 is cubic, etc. 
        */
        int degree;

        /**
          Number of terms in polynomial.
            degree 0: 1
            degree 1: 3
            degree 2: 6
        */
        int nTerms;

        /** 
          Coefficients that go with each term of the polynomial.
          nTerms, allocated using malloc.
        */
        double *v;
} poly_2d;

/*Return the value of the i'th term of this polynomial.
  E.g., term 2 is y; term 4 is x*y, etc.
*/
double poly_term(int termNo,double x,double y);

/*Evaluate polynomial warp at given location*/
double poly_eval(const poly_2d *c,double x,double y);

/*Write the given poly to the given stream.*/
void poly_write(const poly_2d *c,FILE *stream);

/*Read a poly from the given stream.*/
poly_2d *poly_read(FILE *stream);

/* Allocate a polynomial of this degree */
poly_2d *poly_allocate(int degree);

/* Delete this polynomial */
void poly_delete(poly_2d *c);

/*******************************************************************/

/******************************************************************
  Functions used by reskew_dem
******************************************************************/

/* DEFINITIONS:
 * Ground Range: Distance measured along curve of earth, at sea level
 *                 (0 elevation).
 * Phi:          Smallest angle made between line connecting point on earth's
 *                 surface with earth's center and line connecting satellite and
 *              earth's center.
 * Slant Range:  Straight-line distance between satellite and point on earth's
 *                 surface.
 */

#define phi2grX(phi) (((phi)-minPhi)*phiMul)
#define grX2phi(gr) (minPhi+(gr)/phiMul)

extern float badDEMht;
extern int maxBreakLen;

extern double grPixelSize;
extern int gr_ns,sr_ns;

/*Array[gr_ns] indexed by ground range pixel.*/
extern double *slantGR;/*Slant range pixel #*/
extern double *heightShiftGR;

/*Array[sr_ns] indexed by slant range pixel.*/
extern double *heightShiftSR;
extern double *groundSR;/*Ground range pixel #*/
extern double *slantRangeSqr,*slantRange,*heightShift;
extern double *incidAng,*sinIncidAng,*cosIncidAng;

double calc_ranges(meta_parameters *meta);

void geo_compensate(float *srDEM,float *in,float *out,int ns);
void radio_compensate(float *grDEM, float *grDEMprev,float *inout,int ns);
void dem_sr2gr(float *inBuf,float *outBuf);
void dem_gr2sr(float *grDEM, float *srDEM,float *amp);
void dem_interp_col(float *buf,int ns,int nl);

float sr2gr(float srX,float height);
float gr2sr(float grX,float height);


#endif
