/**************************************************************************
 
NAME: make_s1a_mapping

SYNOPSIS:  make_s1a_mapping <inxml file>

DESCRIPTION:  Creates 100 coefficients to map S1A data as follows:
	
  ll2samp[25] - map lat,lon to sample
  ll2line[25] - map lat,lon to line
  ls2lat[25]  - map line,sample to lat
  ls2lon[25]  - map line,sample to lon

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    s1a*.xml		Annotation file with GCP in it.

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     9/15     T. Logan     Create geocoding mapping for s1a
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:   Can't figure out why the minimization is not working for samples.
	It seem to be working just fine for lines, lats, and lons.

**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <asf_meta.h>
#include <asf_license.h>

#include "asf.h"
#include "asf_meta.h"
#include <unistd.h>
#include <assert.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>
#include <gsl/gsl_multimin.h>

void create_mapping(double xs, double ys, double valstart,  double *xin, double *yin, double *vals, int cnt, double *coefs, int use_latlon_objective);

void make_s1_mapping(char *infile, double *ll2samp, double *ll2line, double *ls2lat, double *ls2lon)
{
  FILE *fpin, *fpout;
  char line[256];
  int  i;
  double *lats, *lons;
  double *lines, *samps;
  int cnt;

  /* Open XML file */
  fpin = FOPEN(infile,"r");
  if (fpin==NULL) { printf ("ERROR: Unable to open file %s\n",infile); exit(1);}

  /* Read in all of the GCPs from the XML file */
  while (fgets(line,sizeof(line),fpin)) {
     if (strstr(line,"geolocationGridPointList count")) {
        char *s=strstr(line,"\"");
        if (s==NULL) {printf("Can't find \" in line!!!\n"); exit(1);}
   	s++;
        cnt = atoi(s);
        printf("Found geolocationGridPointList with %i points\n",cnt);

 	lats = (double *) malloc (sizeof(double)*cnt);
 	lons = (double *) malloc (sizeof(double)*cnt);
 	lines= (double *) malloc (sizeof(double)*cnt);
 	samps= (double *) malloc (sizeof(double)*cnt);

        for (i=0; i<cnt; i++) {
 	   fgets(line,sizeof(line),fpin);  /* geolocationGridPoint Start */
	   fgets(line,sizeof(line),fpin);  /* azimuthTime          */
           fgets(line,sizeof(line),fpin);  /* slantRangeTime       */
           fgets(line,sizeof(line),fpin);  /* line                 */
           s = strstr(line,">"); s++; lines[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* pixel                */
           s = strstr(line,">"); s++; samps[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* latitude             */
           s = strstr(line,">"); s++; lats[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* longitude            */
           s = strstr(line,">"); s++; lons[i]=atof(s);
           fgets(line,sizeof(line),fpin);  /* height               */
           fgets(line,sizeof(line),fpin);  /* incidenceAngle       */
           fgets(line,sizeof(line),fpin);  /* elevationAngle       */
           fgets(line,sizeof(line),fpin);  /* geolocationGridPoint End */
	   // printf("Found (%f,%f) -> (%f,%f)\n",lines[i],samps[i],lats[i],lons[i]);
        }
     }
  }

  double max_lat = -200;
  double max_lon = -200;
  int max_samp = 0;
  int max_line = 0;
  double min_lat = 200;
  double min_lon = 200;
  int min_samp = 999999;
  int min_line = 999999;
  double mid_lat, mid_lon, mid_samp, mid_line; 

  /* Find min/max of all parameters */
  for (i=0; i<cnt; i++)
   {
     max_lat = (max_lat>lats[i]) ? max_lat : lats[i];
     min_lat = (min_lat<lats[i]) ? min_lat : lats[i];

     max_lon = (max_lon>lons[i]) ? max_lon : lons[i];
     min_lon = (min_lon<lons[i]) ? min_lon : lons[i];

     max_samp = (max_samp>samps[i]) ? max_samp : samps[i];
     min_samp = (min_samp<samps[i]) ? min_samp : samps[i];

     max_line = (max_line>lines[i]) ? max_line : lines[i];
     min_line = (min_line<lines[i]) ? min_line : lines[i];
   }

  printf("Found line min/max %i %i\n",min_line,max_line);
  printf("Found samp min/max %i %i\n",min_samp,max_samp);
  printf("Found lat min/max %f %f\n",min_lat,max_lat);
  printf("Found lon min/max %f %f\n",min_lon,max_lon);

  /* Find center points, these are used as the origin for the mappings */
  mid_lat = (min_lat+max_lat)/2;
  mid_lon = (min_lon+max_lon)/2;
  mid_samp = (min_samp+max_samp)/2+1;
  mid_line = (min_line+max_line)/2+1;

  printf("Found middle line %f\n",mid_line);
  printf("Found middle samp %f\n",mid_samp);
  printf("Found middle lat %f\n",mid_lat);
  printf("Found middle lon %f\n",mid_lon);

  /* Finally, create the mappings for each parameter */
  create_mapping(mid_lat,mid_lon,mid_line,lats,lons,lines,cnt,ll2line,0);
  create_mapping(mid_lat,mid_lon,mid_samp,lats,lons,samps,cnt,ll2samp,0);
  create_mapping(mid_line,mid_samp,mid_lat,lines,samps,lats,cnt,ls2lat,1);
  create_mapping(mid_line,mid_samp,mid_lon,lines,samps,lons,cnt,ls2lon,1);

}	

double value_at_pixel25(double inval1, double inval2, double *coef, int NUM_COEFS)
{
  double temp=0;

  if (NUM_COEFS>=3)
          temp += coef[0] + coef[1]*inval1 + coef[2]*inval2;
  if (NUM_COEFS>=6)
          temp += coef[3]*inval1*inval1 + coef[4]*inval1*inval2 + coef[5]*inval2*inval2;
  if (NUM_COEFS>=10)
          temp += coef[6]*inval1*inval1*inval1 +
                  coef[7]*inval1*inval1*inval2 +
                  coef[8]*inval1*inval2*inval2 +
                  coef[9]*inval2*inval2*inval2;
  if (NUM_COEFS>=15)
          temp += coef[10]*inval1*inval1*inval1*inval1 +
                  coef[11]*inval1*inval1*inval1*inval2 +
                  coef[12]*inval1*inval1*inval2*inval2 +
                  coef[13]*inval1*inval2*inval2*inval2 +
                  coef[14]*inval2*inval2*inval2*inval2;
  if (NUM_COEFS>=19)
          temp += coef[15]*inval1*inval1*inval1*inval1*inval2 +
                  coef[16]*inval1*inval1*inval1*inval2*inval2 +
                  coef[17]*inval1*inval1*inval2*inval2*inval2 +
                  coef[18]*inval1*inval2*inval2*inval2*inval2;
  if (NUM_COEFS>=22)
          temp += coef[19]*inval1*inval1*inval1*inval1*inval2*inval2 +
                  coef[20]*inval1*inval1*inval1*inval2*inval2*inval2 +
                  coef[21]*inval1*inval1*inval2*inval2*inval2*inval2;
  if (NUM_COEFS>=25)
          temp += coef[22]*inval1*inval1*inval1*inval1*inval2*inval2*inval2 +
                  coef[23]*inval1*inval1*inval1*inval2*inval2*inval2*inval2 +
                  coef[24]*inval1*inval1*inval1*inval1*inval2*inval2*inval2*inval2;
/*
  temp = coef[ 0]*inval1*inval1*inval1*inval1*inval2*inval2*inval2*inval2 +
         coef[ 1]*inval1*inval1*inval1*       inval2*inval2*inval2*inval2 +
         coef[ 2]*inval1*inval1*              inval2*inval2*inval2*inval2 +
         coef[ 3]*inval1*                     inval2*inval2*inval2*inval2 +
         coef[ 4]*                            inval2*inval2*inval2*inval2 +
         coef[ 5]*inval1*inval1*inval1*inval1*inval2*inval2*inval2 +
         coef[ 6]*inval1*inval1*inval1*       inval2*inval2*inval2 +
         coef[ 7]*inval1*inval1*              inval2*inval2*inval2 +
         coef[ 8]*inval1*                     inval2*inval2*inval2 +
         coef[ 9]*                            inval2*inval2*inval2 +
         coef[10]*inval1*inval1*inval1*inval1*inval2*inval2 +
         coef[11]*inval1*inval1*inval1*       inval2*inval2 +
         coef[12]*inval1*inval1*              inval2*inval2 +
         coef[13]*inval1*                     inval2*inval2 +
         coef[14]*                            inval2*inval2 +
         coef[15]*inval1*inval1*inval1*inval1*inval2 +
         coef[16]*inval1*inval1*inval1*       inval2 +
         coef[17]*inval1*inval1*              inval2 +
         coef[18]*inval1*                     inval2 +
         coef[19]*                            inval2 +
         coef[20]*inval1*inval1*inval1*inval1 +
         coef[21]*inval1*inval1*inval1       +
         coef[22]*inval1*inval1              +
         coef[23]*inval1                     +
         coef[24];
*/
  return(temp);
}

struct find_offset_params {
        double x_start, y_start;	/* origin points    */
        double *inval1;		 	/* input x values   */
	double *inval2;			/* input y values   */
	double *outval;			/* output values    */
   	double cnt;			/* number of values */
					/* here we want outval1 = f(inval1,inval2) */
        int NUM_COEFS;
};

static double err_at_pixel(struct find_offset_params *p, double *c, double val, double x, double y, int NUM_COEFS)
{
    double xs, ys, xpt, ypt, myval, diff;

    xs = p->x_start;
    ys = p->y_start;

    xpt = x - xs;
    ypt = y - ys;

    myval = value_at_pixel25(xpt,ypt,c,NUM_COEFS);

    //printf("%10.5f,%10.5f %10.2f %10.2f\n", x, y, val, myval);

    diff = (myval-val);
    return(diff);
}


static double
getObjective(const gsl_vector *x, void *params)
{
    struct find_offset_params *p = (struct find_offset_params *)params;
    int NUM_COEFS = p->NUM_COEFS;

    double c[NUM_COEFS];
    int i;
    for (i=0; i<NUM_COEFS; i++) { c[i] = gsl_vector_get(x,i); }

    double *in1 = p->inval1;
    double *in2 = p->inval2;
    double *out = p->outval;
    double cnt  = p->cnt;

    double err = 0.0;

    for (i=0; i<cnt; i++) {
      double e = err_at_pixel(p,c,out[i],in1[i],in2[i],NUM_COEFS);
      err += e*e;
    }
    return err;
}

static void print_state2(int iter, gsl_multimin_fminimizer *s, int NUM_COEFS, int long_f)
{
    int i;
    char *fmt = long_f ? "%g\n" : "%.3f";

    printf("%3d ",iter);
    for (i=0; i<NUM_COEFS;i++)
      printf(fmt,gsl_vector_get(s->x,i));
    printf(" f(x) = %.8f, size = %.3f\n", s->fval, gsl_multimin_fminimizer_size(s));
}

void create_mapping(double xs, double ys, double valstart,  double *xin, double *yin, double *vals, int cnt, double *coefs, int use_latlon_objective)
{
    int status;
    int iter = 0, max_iter = 1000000;
    int i,j;
    double size, target_size;

    const gsl_multimin_fminimizer_type *T = gsl_multimin_fminimizer_nmsimplex2;
    gsl_multimin_fminimizer *s = NULL;
    gsl_error_handler_t *prev;
    gsl_vector *ss;
    gsl_multimin_function minex_func;
    struct find_offset_params params;

    params.x_start= xs;
    params.y_start= ys;
    params.inval1 = xin;
    params.inval2 = yin;
    params.outval = vals;
    params.cnt = cnt;
    params.NUM_COEFS = 3;

    for (i=0; i<25; i++) coefs[i] = 0;
    printf("valstart = %f\n", valstart); 
    coefs[0] = valstart;

    target_size = 1e-9;
    prev = gsl_set_error_handler_off();

    int sizes[7] = {3,6,10,15,19,22,25};
    for (j=0; j<7; j++) {
        int NUM_COEFS = sizes[j];
        printf("%d: Current poly size: %d\n", j, NUM_COEFS);

        params.NUM_COEFS = NUM_COEFS;
        minex_func.n = NUM_COEFS;
        minex_func.f = getObjective;
        minex_func.params = &params;

        gsl_vector *x = gsl_vector_alloc(NUM_COEFS);
        for (i=0; i<NUM_COEFS; i++) gsl_vector_set (x, i, coefs[i]);
  
        ss = gsl_vector_alloc(NUM_COEFS);
        gsl_vector_set_all(ss, 0.1);

        s = gsl_multimin_fminimizer_alloc(T, NUM_COEFS);
        gsl_multimin_fminimizer_set(s, &minex_func, x, ss);

        do {
            ++iter;
            status = gsl_multimin_fminimizer_iterate(s);

            // abort if stuck
            if (status || iter >= max_iter) {
                printf("Stuck: %d\n", status);
                print_state2(iter, s, NUM_COEFS, TRUE);
                break;
            }

            //print_state2(iter, s, NUM_COEFS, FALSE);

            size = gsl_multimin_fminimizer_size(s);
            status = gsl_multimin_test_size(size, target_size);

            if (status == GSL_SUCCESS) {
                //printf("converged to minimum at\n");
                //print_state2(iter, s, NUM_COEFS, TRUE);
                break;
            }

        } while (status == GSL_CONTINUE && iter < max_iter);
    
        for (i=0; i<NUM_COEFS; i++) coefs[i] = gsl_vector_get(s->x,i);
        gsl_vector *retrofit = gsl_vector_alloc(NUM_COEFS);
        for (i=0; i<NUM_COEFS; i++) gsl_vector_set(retrofit,i,coefs[i]);
        gsl_vector *output = gsl_vector_alloc(NUM_COEFS);
        double val = getObjective(retrofit, (void*)&params);
        //printf("GSL Result at %2d: %f \n",NUM_COEFS,val);
        printf("Error per GCP: %.12f\n", val/(double)cnt);
        if (use_latlon_objective) printf("In m: %.5f\n", val/(double)cnt * 111000);
        gsl_vector_free(retrofit);
        gsl_vector_free(output);

        gsl_multimin_fminimizer_free(s);
        gsl_vector_free(x);
        gsl_vector_free(ss);
    }

    /* Rearrange the coefficients to match PALSAR order */
    double tmp[25];
    for (i=0; i<25; i++) tmp[i] = coefs[i];
   
    coefs[24] = tmp[0];
    coefs[23] = tmp[1];
    coefs[22] = tmp[3];
    coefs[21] = tmp[6];
    coefs[20] = tmp[10];
    coefs[19] = tmp[2];
    coefs[18] = tmp[4];
    coefs[17] = tmp[7];
    coefs[16] = tmp[11];
    coefs[15] = tmp[15];
    coefs[14] = tmp[5];
    coefs[13] = tmp[8];
    coefs[12] = tmp[12];
    coefs[11] = tmp[16];
    coefs[10] = tmp[19];
    coefs[9]  = tmp[9];
    coefs[8]  = tmp[13];
    coefs[7]  = tmp[17];
    coefs[6]  = tmp[20];
    coefs[5]  = tmp[22];
    coefs[4]  = tmp[14];
    coefs[3]  = tmp[18];
    coefs[2]  = tmp[21];
    coefs[1]  = tmp[23];
    coefs[0]  = tmp[24];

    printf("Done.\n\n\n");
    gsl_set_error_handler(prev);

}
