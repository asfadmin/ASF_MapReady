
#define CMLEN   30
#include "geompak.h"
#define E_SUCC  0
#define E_FAIL  1
#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif
#define ON	TRUE
#define OFF	FALSE
 
/* Edit modes
  ----------*/
#define AUTO	100
#define MANUAL	200
#define NOEDIT	300
 
/* Direction of transformation--INVERSE is normally used
  -----------------------------------------------------*/
#define FORWARD 100
#define INVERSE 200
 
/* Report destinations
  -------------------*/
#define ALL	100
#define TERM	200
#define PFILE	300
 
/* Tie point edit actions
  ----------------------*/
#define EXCLUDE	700
#define INCLUDE 800
 
/* Error flag--more error flags in geompak.h 
  -----------------------------------------*/
#define QUIT_IN_MOD  777
#define NO_FRAME     778
 
#define IMG_COORD	0
#define REF_COORD	0
#define SEA_COORD	1
#define GEO_COORD	2
#define ELEVATION	3
#define ZOOM_FACT	4
#define AFLAGCODE	5
#define MAP_IDENT	6
#define	HEADERREC	7
#define CORR_RECS	8
 
#define FLAG_DIM	9
 
#define PIE 3.141592653589793238
#define PIE2 6.283185307179586476
#define LITESPEED 2.99793e8
#define WE 7.292115152e-5
#define RPD (PIE / 180.0)
#define MU 3.9892e14
 
/* Structure holding transformation coefficients and related parameters 
   & statistics
  ------------*/
struct COEF 
   {
   int degree;			/* Degree of polynomial used for fit */
   double x_coef[MAX_COEF];	/* Coefficients in x */
   double y_coef[MAX_COEF];	/* Coefficients in y */
   int npts_fit;		/* Number of points used in fit */
   int npts;			/* Number of points in tie point file used */
   int x_ncoef;		/* Number of coefficients in x */
   int y_ncoef;		/* Number of coefficients in y */
   int x_freedom;		/* Number of degrees of freedom in x */
   int y_freedom;		/* Number of degrees of freedom in y */
   };
 
/* Structure containing report destination options & related parameters 
  --------------------------------------------------------------------*/
struct PRINT 
   {
   int option;		/* Print option:  TERM only, FILE only, or ALL (both) */
   FILE *fptr;		/* Print file pointer (NULL if print file not used) */
   char name[CMLEN];	/* Print file name (NULL if print file not used) */
   char host_name[CMLEN];/* Host name of print file */
   };
 
/* Structure containing framing parameters & related gridding information.
   These are all user-entered parameters
  -------------------------------------*/
struct FRAME 
   {
   double tol;		/* Tolerance for linear approximation--grid reduce */
   double corners[8];	/* Resulting grid projection corners */
   int window[4];	/* Window parameters for output space */
   int ngline;		/* Number of geometric mapping grid lines (rows) */
   int ngsamp;		/* Number of geometric mapping grid samples (cols) */
   char name[CMLEN];	/* Name of the Geometric Mapping Grid file */
   };
 
/* Structure containing user-entered parameters related to the fitting & 
   tie point editing process
  -------------------------*/
struct FIT 
   {
   double alpha[2];	/* Entry & exit text alpha values for regression */
   double max_res;	/* Maximum allowable residual value for auto edit */
   int degree;		/* Maximum allowable degree of polynomial */
   int edit_mode;	/* Edit mode:  MANUAL, AUTO or NOEDIT */
   int xform_mode;	/* Transformation mode:  FORWARD or INVERSE */
   };
 
/* Structure containing residual values and related statistics
  -----------------------------------------------------------*/
struct RESIDUAL
   {
   double *x_residual;	/* Pointer to array of residual values in x */
   double *y_residual;	/* Pointer to array of residual values in y */
   double *rms_residual;/* Pointer to array of rms values */
   double ave_rmse;	/* Average rms error */
   double x_ave_err;	/* Average residual in x */
   double y_ave_err;	/* Average residual in y */
   double x_max_err;	/* Maximum residual in x */
   double x_max_rms;	/* Maximum residual in x RMS error */
   double y_max_err;	/* Maximum residual in y */
   double y_max_rms;	/* Maximum residual in y RMS error */
   char x_max_ptid[CMLEN]; /* Point id of tie point with largest x residual */
   char y_max_ptid[CMLEN]; /* Point id of tie point with largest y residual */
   };

/*Prototypes:*/
 int calc_coef (struct TPLDATA *tie_data, struct FIT *fit, struct COEF *coef); 
 void calc_residuals (struct TPLDATA *tp, struct COEF *coef, struct RESIDUAL *residual); 
 void calcplane (double *refline, double *refsample, double *dline, double *dsample, unsigned int npoints, double *acoef, double *bcoef, double *ccoef); 
 void cross (double *a, double *b, double *c); 
 double dot (double *a, double *b); 
 double mag (double *a); 
 int modify (struct RESIDUAL *residual, struct TPLDATA *tie_data, struct FIT *fit, struct COEF *coef); 
 void find_max_degree (int npts, int *degree, int *max_degree); 

