// Code adapted from Tom's and Kirk's efforts during day of innovation

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

void create_mapping(double xs, double ys, double valstart, double *xin, 
  double *yin, double *vals, int cnt, double *coefs, int use_latlon_objective);

meta_transform *gcp2transform(gcp_location *gcp, int gcp_count, char *type)
{
  int ii;
  double max_lat = -200;
  double max_lon = -200;
  double min_lat = 200;
  double min_lon = 200;
  int max_samp = 0;
  int max_line = 0;
  int min_samp = 999999;
  int min_line = 999999;

  asfPrintStatus("\n\n   Determining parameters of the transform block ...\n\n");

  // Find min/max of all parameters
  for (ii=0; ii<gcp_count; ii++) {
    max_lat = (max_lat > gcp[ii].lat) ? max_lat : gcp[ii].lat;
    min_lat = (min_lat < gcp[ii].lat) ? min_lat : gcp[ii].lat;

    max_lon = (max_lon > gcp[ii].lon) ? max_lon : gcp[ii].lon;
    min_lon = (min_lon < gcp[ii].lon) ? min_lon : gcp[ii].lon;

    max_samp = (max_samp > gcp[ii].pixel) ? max_samp : gcp[ii].pixel;
    min_samp = (min_samp < gcp[ii].pixel) ? min_samp : gcp[ii].pixel;

    max_line = (max_line > gcp[ii].line) ? max_line : gcp[ii].line;
    min_line = (min_line < gcp[ii].line) ? min_line : gcp[ii].line;
  }

  asfPrintStatus("   Found line min/max %i %i\n", min_line, max_line);
  asfPrintStatus("   Found samp min/max %i %i\n", min_samp, max_samp);
  asfPrintStatus("   Found lat min/max %f %f\n", min_lat, max_lat);
  asfPrintStatus("   Found lon min/max %f %f\n", min_lon, max_lon);

  // Find center points, these are used as the origin for the mappings
  double mid_lat = (min_lat+max_lat)/2;
  double mid_lon = (min_lon+max_lon)/2;
  double mid_samp = (min_samp+max_samp)/2+1;
  double mid_line = (min_line+max_line)/2+1;

  asfPrintStatus("   Found middle line %f\n", mid_line);
  asfPrintStatus("   Found middle samp %f\n", mid_samp);
  asfPrintStatus("   Found middle lat %f\n", mid_lat);
  asfPrintStatus("   Found middle lon %f\n", mid_lon);

  // Finally, create the mappings for each parameter
  double *lats = (double *) MALLOC(sizeof(double)*gcp_count);
  double *lons = (double *) MALLOC(sizeof(double)*gcp_count);
  double *lines = (double *) MALLOC(sizeof(double)*gcp_count);
  double *samps = (double *) MALLOC(sizeof(double)*gcp_count);
  for (ii=0; ii<gcp_count; ii++) {
    lats[ii] = gcp[ii].lat;
    lons[ii] = gcp[ii].lon;
    lines[ii] = gcp[ii].line;
    samps[ii] = gcp[ii].pixel;
  }
  double *ls2lat = (double *) MALLOC(sizeof(double)*MAX_FITTING_ORDER);
  double *ls2lon = (double *) MALLOC(sizeof(double)*MAX_FITTING_ORDER);
  
  double *ll2samp = (double *) MALLOC(sizeof(double)*MAX_FITTING_ORDER); 
  double *ll2line = (double *) MALLOC(sizeof(double)*MAX_FITTING_ORDER);
  create_mapping(mid_lat, mid_lon, mid_line, lats, lons, lines, gcp_count, 
    ll2line, 0);
  create_mapping(mid_lat, mid_lon, mid_samp, lats, lons, samps, gcp_count, 
    ll2samp, 0);
  
  create_mapping(mid_line, mid_samp, mid_lat, lines, samps, lats, gcp_count, 
    ls2lat, 1);
  create_mapping(mid_line, mid_samp, mid_lon, lines, samps, lons, gcp_count, 
    ls2lon, 1);

  meta_transform *mt = meta_transform_init();
  strcpy(mt->type, type);
  mt->parameter_count = MAX_FITTING_ORDER;
  mt->origin_pixel = mid_samp;
  mt->origin_line = mid_line;
  mt->origin_lat = mid_lat;
  mt->origin_lon = mid_lon;
  mt->use_reverse_transform = TRUE;
  for (ii=0; ii<MAX_FITTING_ORDER; ii++) {
    mt->x[ii] = ls2lat[ii];
    mt->y[ii] = ls2lon[ii];
    mt->l[ii] = ll2line[ii];
    mt->s[ii] = ll2samp[ii];
  }
 
  meta_parameters *meta = raw_init();
  meta->transform = mt;

  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  meta->general->line_scaling = 1;
  meta->general->sample_scaling = 1;

  double err = 0;

  for (ii=0; ii<gcp_count; ++ii) {

    double lat, lon, line, samp;
    meta_get_latLon(meta, lines[ii], samps[ii], 0, &lat, &lon);
    meta_get_lineSamp(meta, lat, lon, 0, &line, &samp);

    asfPrintStatus("GCP %5d: (%6.f, %6.f) ==> (%7.2f, %7.2f) actual: (%7.2f, %7.2f) ==> (%7.1f, %7.1f)\n", ii, lines[ii], samps[ii], lat, lon, lats[ii], lons[ii], line, samp);

    err += hypot(samp-samps[ii], line-lines[ii]);
  }

  asfPrintStatus("Overall Error per GCP: %7.2f pixels\n", err/(double)gcp_count);

  FREE(lats);
  FREE(lons);
  FREE(lines);
  FREE(samps);
  FREE(ll2line);
  FREE(ll2samp);
  FREE(ls2lat);
  FREE(ls2lon);
  
  return mt;
}	

double value_at_pixel25(double inval1, double inval2, double *coef, int NUM_COEFS)
{
  double temp=0;

  double inval1_3 = inval1*inval1*inval1;
  double inval2_3 = inval2*inval2*inval2;

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
    temp += coef[15]*inval1_3*inval1*inval2 +
            coef[16]*inval1_3*inval2*inval2 +
            coef[17]*inval1*inval1*inval2_3 +
            coef[18]*inval1*inval2*inval2_3;
  if (NUM_COEFS>=22)
    temp += coef[19]*inval1_3*inval1*inval2*inval2 +
            coef[20]*inval1_3*inval2_3 +
            coef[21]*inval1*inval1*inval2*inval2_3;
  if (NUM_COEFS>=25)
    temp += coef[22]*inval1_3*inval1*inval2_3 +
            coef[23]*inval1_3*inval2*inval2_3 +
            coef[24]*inval1_3*inval1*inval2*inval2_3;
  if (NUM_COEFS>=27)
    temp += coef[25]*inval1_3*inval1*inval1 +
            coef[26]*inval2_3*inval2*inval2;
  if (NUM_COEFS>=31)
    temp += coef[27]*inval1_3*inval1_3 +
            coef[28]*inval1_3*inval1*inval1*inval2 +
            coef[29]*inval2_3*inval2_3 +
            coef[30]*inval1*inval2*inval2*inval2_3;
  if (NUM_COEFS>=37)
    temp += coef[31]*inval1_3*inval1*inval1_3 +
            coef[32]*inval1_3*inval1_3*inval2 +
            coef[33]*inval1_3*inval1*inval1*inval2*inval2 +
            coef[34]*inval1*inval1*inval2*inval2*inval2_3 +
            coef[35]*inval1*inval2_3*inval2_3 +
            coef[36]*inval2_3*inval2*inval2_3;
  if (NUM_COEFS>=45)
    temp += coef[37]*inval1_3*inval1_3*inval1*inval1 +
            coef[38]*inval1_3*inval1_3*inval1*inval2 +
            coef[39]*inval1_3*inval1_3*inval2*inval2 +
            coef[40]*inval1_3*inval2_3*inval1*inval1 +
            coef[41]*inval2_3*inval2_3*inval2*inval2 +
            coef[42]*inval2_3*inval2_3*inval2*inval1 +
            coef[43]*inval2_3*inval2_3*inval1*inval1 +
            coef[44]*inval1_3*inval2_3*inval2*inval2;

  return(temp);
}

struct find_offset_params {
  double x_start, y_start; // origin points
  double *inval1; // input x values
	double *inval2;	// input y values
	double *outval;	// output values
  double cnt;	// number of values
  // here we want outval1 = f(inval1,inval2)
  int NUM_COEFS;
};

static double err_at_pixel(struct find_offset_params *p, double *c, double val, 
  double x, double y, int NUM_COEFS)
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

static double getObjective(const gsl_vector *x, void *params)
{
  struct find_offset_params *p = (struct find_offset_params *)params;
  int NUM_COEFS = p->NUM_COEFS;

  double c[NUM_COEFS];
  int i;
  for (i=0; i<NUM_COEFS; i++) 
    c[i] = gsl_vector_get(x,i);

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
  char *fmt = long_f ? "%g\n" : "%.3f ";
 
  printf("%3d\n",iter);
  for (i=0; i<NUM_COEFS;i++)
    printf(fmt,gsl_vector_get(s->x,i));
  printf(" f(x) = %.8f, size = %.3f\n", s->fval, gsl_multimin_fminimizer_size(s));
}

void create_mapping(double xs, double ys, double valstart,  double *xin, 
  double *yin, double *vals, int cnt, double *coefs, int use_latlon_objective)
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

  for (i=0; i<MAX_FITTING_ORDER; i++)
    coefs[i] = 0;
  asfPrintStatus("   valstart = %f\n", valstart); 
  coefs[0] = valstart;

  target_size = 1e-9;
  prev = gsl_set_error_handler_off();

  // sizes needs to match MAX_FITTING_ORDER as defined in asf_meta.h
  int sizes[11] = {3,6,10,15,19,22,25,27,31,37,45};
  for (j=0; j<sizeof(sizes)/sizeof(int); j++) {
    int NUM_COEFS = sizes[j];
    asfPrintStatus("   %d: Current poly size: %d\n", j, NUM_COEFS);

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

    iter = 0;
    do {
      ++iter;
      status = gsl_multimin_fminimizer_iterate(s);

      // abort if stuck
      if (status || iter >= max_iter) {
        asfPrintStatus("   Stuck: %d\n", status);
        print_state2(iter, s, NUM_COEFS, TRUE);
        asfPrintStatus("   Can't determine transformation parameters!\n");
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

    for (i=0; i<NUM_COEFS; i++) 
      coefs[i] = gsl_vector_get(s->x,i);
    gsl_vector *retrofit = gsl_vector_alloc(NUM_COEFS);
    for (i=0; i<NUM_COEFS; i++) 
      gsl_vector_set(retrofit,i,coefs[i]);
    gsl_vector *output = gsl_vector_alloc(NUM_COEFS);
    double val = getObjective(retrofit, (void*)&params);
    double valper = val/(double)cnt;

    int done = FALSE;
    if (use_latlon_objective)  {
      valper *= 111000.;
      asfPrintStatus("   Error per GCP: %.5f m\n", valper);
      if (valper < .1)
        done = TRUE;
    }
    else {
      asfPrintStatus("   Error per GCP: %.12f pixels\n", valper);
      if (valper < .1)
        done = TRUE;
    }

    gsl_vector_free(retrofit);
    gsl_vector_free(output);

    if (done) {
      asfPrintStatus("Close enough!  Stopping with %d coefficients.", NUM_COEFS);
      break;
    }

    gsl_multimin_fminimizer_free(s);
    gsl_vector_free(x);
    gsl_vector_free(ss);
  }

  /* Rearrange the coefficients to match PALSAR order
  double tmp[25];
  for (i=0; i<25; i++) 
    tmp[i] = coefs[i];
 
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
  */

  asfPrintStatus("   Done.\n\n");
  gsl_set_error_handler(prev);
}
