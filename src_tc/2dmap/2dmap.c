/****************************************************************************
NAME:	    2dmap  -  edits a file of tie-points and creates coefficients
 
SYNOPSIS:   2dmap in_tiep out_coef
 
DESCRIPTION:
     2Dmap uses an ASCII Tie Point Location file containing tie point pairs to
     derive two bivariate polynomials using least squares regression
     analysis in a forward stepping procedure.  This defines the trans-
     formation from one coordinate system to another.  Then it creates the
     2D offset mapping coefficients that approximate the transformation
     defined by the polynomials.
 
EXTERNAL ASSOCIATES:    terrcorr.c           calling routine
 
FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    in_tiep.tpl		 Input ASCII tie point file 
    out_coef.ppf	 Output ASCII coefficients file

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:       PURPOSE:
    ---------------------------------------------------------------
     1.0     3/95    T. Logan      Combine editing and mapping coef
                                   creation in terrain correction. 
     2.0    10/95    T. Logan	   Ported program from YMP to Solaris 
     2.1     7/97    T. Logan      Modified for 2-pass correlation
     2.2     6/98    O. Lawlor     Fixed non-ANSI timer routine.
     2.3    11/98    T. Logan      Fixed memory error for refline,refsample,
				   dline,dsample arrays 
 
HARDWARE/SOFTWARE LIMITATIONS:
 
ALGORITHM DESCRIPTION:
    Read in tie point records from a tie point location file
    If generating a forward transformation, swap tie point location search
	and reference coordinate values
    Generate a polynomial model from the tie points
    If generating a forward transformation, swap tie point location search
	and reference coordinate values
    Calculates the 2D warp coefs that approximate the polynomial model
    Write coefficients to file & exit
 
ALGORITHM REFERENCES:
    Programs included here are 
            editcorr.c   7/92   D. Steinwand
                         9/94   T. Logan
            extr_tpl.c   7/92   C. Taylor
            tiecoef.c    7/92   C. Taylor
                         9/94   T. Logan        
           (show_tpl.c          D. Steinwand)
           (DSPTIE              D. Steinwand)
 
BUGS: none known
*******************************************************************************
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
#include "asf.h"
#include "2dmap.h"

#define VERSION         2.3
 
int main (argc,argv) int argc; char **argv;
{
struct COEF coef;		/* Polynomial coefficient records        */
struct FRAME frame;		/* Framing parameters                    */
struct FIT fit;			/* Coefficient modeling parameters       */
struct RESIDUAL residual;	/* Residual errors                       */
struct TPLDATA *data;     	/* Pointer to tie point location data    */
char   tpl_name[CMLEN];		/* Input Tie Point Location file name    */
char   out_file[CMLEN];	        /* Name of output file                   */
FILE   *ptr;                    /* Disk file I/O var                     */
int    count;                   /* Loop counter                          */
int    satisfied;               /* Return from modify, SATISFIED if done */
int    dummy;                   /* Unused return value                   */
int    status;                  /* Return from calculating coefficients  */
double acoef[3],bcoef[3],ccoef[3]; /* Coefficients                       */
double refline[MAX_TIE_POINTS],
       refsample[MAX_TIE_POINTS],
       dline[MAX_TIE_POINTS],
       dsample[MAX_TIE_POINTS];
unsigned int npoints = 0;

StartWatch();
 
/* Get processing parameters from the user
 ----------------------------------------*/
if (argc != 3)
  {
    printf("\n");
    printf("Usage: %s intplfile outppffile\n",argv[0]);
    printf("       inputs:  ASCII tie-point file (.tpl)\n");
    printf("       outputs: Mapping Coefficients (.ppf)\n\n");
    printf("       Version %.2f,  ASF STEP TOOLS\n\n",VERSION);
    exit(1);
  }
strcat(strcpy(tpl_name,argv[1]),".tpl");
strcat(strcpy(out_file,argv[2]),".ppf");
printf("\n*** CREATING MAPPING COEFFICIENTS FROM EDITED TIE-POINTS ***\n");

/* Set processing parameters 
 --------------------------*/ 
frame.window[0] = frame.window[1] = 1;
frame.window[2] = frame.window[3] = 1100;
frame.ngline = frame.ngsamp = 0;
frame.tol = 0.015625;
for (count = 0; count < 8; count++) frame.corners[count] = 0.0;
fit.alpha[0] = fit.alpha[1] = 0.999;
fit.degree = 1;
fit.edit_mode = AUTO;
fit.max_res = 0.7;
fit.xform_mode = INVERSE;
 
/* Allocate buffer space for tie point records
  -------------------------------------------*/
data=(struct TPLDATA*) MALLOC (sizeof(struct TPLDATA)*MAX_TIE_POINTS);
 
/* Read all tie points from input file and finish set-up 
  -----------------------------------------------------*/
if ((ptr = fopen(tpl_name,"r"))==NULL)
  { printf("Unable to open tie point file %s\n",tpl_name); exit(1); }
 
for (count = 0; count < MAX_TIE_POINTS; count++)
 {
  if (fscanf(ptr,"%lf %lf %lf %lf\n",
      &data[count].ref_coord[0], &data[count].ref_coord[1],
      &data[count].sea_coord[0], &data[count].sea_coord[1]) < 1) break;
  data[count].active=1;
  sprintf(data[count].pt_id,"gpt%d",count+1);
 }
 
fclose(ptr);
coef.npts = count;
printf("  Read %i Tie Points from Location File %s\n",coef.npts,tpl_name);
 
residual.x_residual = (double *) MALLOC (3*coef.npts*sizeof(double));
residual.y_residual = residual.x_residual + coef.npts;
residual.rms_residual = residual.y_residual + coef.npts;
coef.degree = fit.degree;
find_max_degree(coef.npts,&(coef.degree),&dummy);
 
/* Edit tie points until satisfied with fit
  ----------------------------------------*/
satisfied = FALSE;
while (!satisfied) {
  status = calc_coef(data,&fit,&coef);
  if (status != E_SUCC) {printf("Failed in calc_coef\n"); exit(1); }
  calc_residuals(data,&coef,&residual);
  satisfied = modify(&residual,data,&fit,&coef);
}
 
/* Copy tie points into arrays for calcplane
  -----------------------------------------*/
npoints = 0;
for (count=0; count < coef.npts; count++)
 if (data[count].active == 1) {
   refline[npoints]   = data[count].ref_coord[0];
   refsample[npoints] = data[count].ref_coord[1];
   dline[npoints]   = data[count].sea_coord[0]-data[count].ref_coord[0];
   dsample[npoints] = data[count].sea_coord[1]-data[count].ref_coord[1];
   npoints++;
  }

/* Calculate 2D offset coefficients and save them on disk
  ------------------------------------------------------*/
calcplane(refline,refsample,dline,dsample,npoints,acoef,bcoef,ccoef);

if ((ptr = fopen(out_file, "w"))==NULL)
  { printf("Error opening coefficients file\n"); exit(1); }
fprintf(ptr,"AZ1COEF        R 1 %f\n",acoef[1]);
fprintf(ptr,"AZ2COEF        R 1 %f\n",bcoef[1]);
fprintf(ptr,"AZ3COEF        R 1 %f\n",ccoef[1]);
fprintf(ptr,"GR1COEF        R 1 %f\n",acoef[2]);
fprintf(ptr,"GR2COEF        R 1 %f\n",bcoef[2]);
fprintf(ptr,"GR3COEF        R 1 %f\n",ccoef[2]);
fclose(ptr);
 
printf("2Dmap completed output file %s\n", out_file);
StopWatch();
exit(0);
}
