#include "asf.h"
#include "fft2d.h"

#define E_SUCC  0
#define E_FAIL  1

#define STAT_FLAG status

#define GREY_CORR	1
#define EDGE_CORR	2
#define PHASE_CORR	3

#define PARAB	1
#define GAUSS	2
#define RECIP	3
#define INTEGER	4

#define REF_MIN		32
#define REF_MAX	       128
#define REF_BUFSIZE  16384
#define SRCH_MIN	48 
#define SRCH_MAX       256	
#define SRCH_BUFSIZE 65536 

/* Error messages.  Additional error messages may be found in geompak.h
  --------------------------------------------------------------------*/
#define IN_BATCH	201
#define NO_CORR		202
#define CORR_METHOD	203
#define BAD_SIZE	204
#define OUTSIDE_IMAGE	205
#define READ_ERROR	206
#define BAD_COEFS	207
#define BAD_CHIP_DDR	208
#define NO_CHIPS	209
#define BAD_PTID	210

/* Structure holding correlation parameters
  ----------------------------------------*/
struct CORRPAR
   {
   double mincorr;		/* Minimum correlate value */
   double maxdiff;		/* Maximum diff between nominal & refined */
   double edgeval;		/* Fraction of pixels to be classified edges*/
   int fitmeth;		/* Method of peak fitting */
   }; 

 void fitreg (float *cpval, int mfit, float *pkoffs, float *tlerrs); 
 void sums (float *cpval, int mfit, float *z, float *wghts, 
 	float (*b)[6], float *vector); 
 void invert (float (*matrix)[6], float (*outA)[6], int n); 
 int lu_fact (int n, float (*matrix)[6], int *ipvt); 
 void back_solve (int n, float (*matrix)[6], int *ipvt, float *b); 
 int init_pivot (int n, float (*matrix)[6], float *s); 
 int find_pivot (int n, float (*matrix)[6], int k, float *s); 
 void esterr (float *z, float *wghts, float (*bnvrs)[6], 
 	float *coeffs, float *pkoffs, float *tlerrs); 
 
 void gcorr (float *images, float *imager, int *npls, int *nplr, 
 	int mfit, float *nomoff,
 	float *streng, float *bfoffs, float *tlerrs,int verbose); 
 void cross_prod_sums (float *images, float *imager, 
 	int *npls, int *nplr, float *unormc,int *nrow,int *ncol); 
 void eval (int ncol, int nrow, float *corr,
 	int *peakLoc,float *strength,float *hood,int verbose); 
 
