#include "asf.h"
#include "asf_meta.h"
#include "metadata.h"
#include "jpl_proj.h"
#include "least_squares.h"
#include "calibrate.h"

#ifndef PI
# define PI 3.14159265358979323846
#endif
#define RES_X 16
#define RES_Y 16
#define MAX_PTS 300
#define MAX_tableRes 512

#define SQR(X) ((X)*(X))

/* Is this complex data? (follows asf_meta.h ENUM 'data_type_t') */
int is_complex(data_type_t data_type)
{
  if ((data_type>=COMPLEX_BYTE) && (data_type<=COMPLEX_REAL64))
    return TRUE;
  else
    return FALSE;
}

/* Lots of neat quick math functions */
double get_satellite_height(double time, stateVector stVec)
{
  return sqrt(SQR(stVec.pos.x) + SQR(stVec.pos.y) + SQR(stVec.pos.z));
}

double get_earth_radius(double time, stateVector stVec, double re, double rp)
{
  double er = sqrt(SQR(stVec.pos.x) + SQR(stVec.pos.y) + SQR(stVec.pos.z));
  double lat = asin(stVec.pos.z/er);
  return (re*rp)/sqrt(SQR(rp)*cos(lat)*cos(lat)+SQR(re)*sin(lat)*sin(lat));
}

double get_slant_range(meta_parameters *meta, double er, double ht, int sample)
{
  double minPhi = acos((SQR(ht) + SQR(er) -
                    SQR(meta->sar->slant_range_first_pixel))/(2.0*ht*er));
  double phi = minPhi+sample*(meta->general->x_pixel_size/er);
  double slantRng = sqrt(SQR(ht)+SQR(er)-2.0*ht*er*cos(phi));
  return slantRng + meta->sar->slant_shift;
}

double get_look_angle(double er, double ht, double sr)
{
  return acos((SQR(sr)+SQR(ht)-SQR(er))/(2.0*sr*ht));
}

/* Not useful for now
 *double get_incidence_angle(double er, double ht, double sr)
 *{
 *  return PI-acos((SQR(sr)+SQR(er)-SQR(ht))/(2.0*sr*er));
 *}
 */

/* Get the noise array which is tucked away somewhere special for FOCUS (RSI)*/
/* NOT YET IN USE!
 *void get_noise_array_focus(char *ceosMetaFile)
 *{
 *  FILE fp = FOPEN(ceosMetaFile, "rb");
 *  int ii;
 *
 *  FSEEK64 (fp, START_OF_RDR + 85, SEEK_SET);
 *
 *  fscanf (fF, " %lg", &noise_inc);
 *
 *  for (ii=0; ii<512; ii++)
 *    fscanf (fF, " %lf ", &(noise_array[ii]));
 *
 *  fscanf (fF, " %lf ", &noise_scale);
 *  fscanf (fF, " %lf ", &offset);
 *}
 */

/* Computes sigma0 for an ASCENDING image. Approach is from CSA's product
 * description, section 5.3.2
 * NOTE: dispite the assertation that this is only for ascending data,
 * sprocket's import for FOCUS data (vexcel_complex_data_planes.c) used it for
 * all FOCUS data, so that's what we'll do here too. */
/* NOT YET IN USE!!
 *float sigma_naught_focus (float dn, float look_angle, float incidenceAngle,
 *                          float slantRange)
 *{
 *  double dn2 = SQR(dn);
 *
 *  if (dn2 == 0)
 *    return 1E-10;
 *
 *  beta = 20.0 * log10 (dn2 / slantRange);
 *  sigma = pow (10.0, (beta + incidenceAngle) / 10.0);
 *
 *  return sigma;
 *}
 */

/* Print the percent thats been completed to stdout */
void print_layer_progress(const char *msg, int currentLine, int totalLines,
                          int *progress)
{
  if ((currentLine+1)*100/totalLines == *progress) {
    printf("\r%s: %3d%% complete.",msg,(*progress)++);
    fflush(NULL);
 }
  if (currentLine+1 == totalLines)
    printf("\n");
}


void create_sprocket_layers(const char *asfName, const char *importName)
{
  FILE *ampFp=NULL, *lookFp=NULL, *sigmaFp=NULL;
  FILE *lookTiePointFp=NULL;
/*FILE *latFp=NULL, *lonFp=NULL;*/
  FILE *inFp=NULL;
  struct dataset_sum_rec *dssr=NULL;
  meta_parameters *metaIn, *metaOut;
  stateVector stVec;
  cal_params *cal_param=NULL;
  int nl, ns;
  int yy/*lineIndex*/, xx/*sampleIndex*/, chunk, size;
  int overall_line;
  int percent_done;
  int lookTiePointFlag=FALSE;
  int tableRes=MAX_tableRes, tablePix=0;
  char metaName[256], sprocketMetaName[256];
  char ampName[256], lookName[256], sigmaName[256];
  char lookTiePointName[256];
/*char latName[256], lonName[256];*/
  float *ampBuf=NULL, *lookBuf=NULL, *sigmaBuf=NULL;
/*float *latBuf=NULL, *lonBuf=NULL;*/
  double latitude, longitude, time, doppler, earth_radius;
  double satellite_height, range, look_angle/*, incidence_angle*/;
  double re=6378144.0, rp=6356754.9;
/*Calibration stuff*/
  double noise_table[MAX_tableRes];


  /* Tell user we're starting */
  printf("Creating SProCKET metadata and data planes...\n");

  /* Set up file names */
  create_name(metaName, asfName, ".meta");
  create_name(sprocketMetaName, asfName, METADATA_EXT);
  create_name(ampName,asfName, DATA_EXT);
  create_name(lookName, asfName, LOOK_EXT);
  create_name(sigmaName, asfName, SIGMA_EXT);
/*  create_name(latName, asfName, LATITUDE_EXT);
 *  create_name(lonName, asfName, LONGITUDE_EXT);
 */
  /* Get metadata */
  metaIn = meta_read(metaName);
  nl = metaIn->general->line_count;
  ns = metaIn->general->sample_count;
  if (meta_is_valid_double(metaIn->general->re_major))
    re = metaIn->general->re_major;
  if (meta_is_valid_double(metaIn->general->re_minor))
    re = metaIn->general->re_minor;
  metaOut = meta_copy(metaIn);
  metaOut->general->data_type = REAL32;

  /* Used for creating tie point files */
  doppler = 0.0;

  /* We can get some useful stuff from the CEOS leader file, like the
   * calibration parameters */
  if (has_ceos_leader_extension(importName)) {
    char tmp[256];
    cal_param = create_cal_params(importName);
    dssr = (struct dataset_sum_rec*) MALLOC (sizeof(struct dataset_sum_rec));
    get_dssr(importName, dssr);
    if (nl<1500) tableRes=128;
    else if (nl<3000) tableRes=256;
    tablePix=( (ns+(tableRes-1))/tableRes );
    if (cal_param==NULL) {
      sprintf(tmp,
              "   Calibration parameters could not be extracted out of CEOS file\n"
              "   There will be no sigma0 data layer.\n");
      printf(tmp);
      if (logflag) printLog(tmp);
    }
  }

  /* Write the sprocket format metadata */
  meta_write_sprocket(sprocketMetaName, metaOut, dssr);
  FREE(dssr);

/* First create all layers except the look angle layer (and maybe in the future
 * lat, lon, and incidence angle layers)
   --------------------------------------------------------------------------*/

  /* Create I & Q data planes real quick if we got complex data */
  if (is_complex(metaIn->general->data_type)) {
    char IName[256], QName[256];
    FILE *IFp, *QFp;
    complexFloat *iqBuf;
    float *IBuf, *QBuf;

    /* File naming, opening, & memory allocation */
    create_name(IName, asfName, COMPLEX_I_PLANE);
    create_name(QName, asfName, COMPLEX_Q_PLANE);
    inFp = fopenImage(asfName,"rb");
    IFp = fopenImage(IName,"wb");
    QFp = fopenImage(QName,"wb");
    ampFp = fopenImage(ampName,"wb");
    sigmaFp = fopenImage(sigmaName,"wb");
    iqBuf = (complexFloat*) MALLOC (sizeof(complexFloat)*ns*CHUNK_OF_LINES);
    IBuf = (float *) MALLOC (sizeof(float) * ns * CHUNK_OF_LINES);
    QBuf = (float *) MALLOC (sizeof(float) * ns * CHUNK_OF_LINES);
    ampBuf = (float *) MALLOC (sizeof(float) * ns * CHUNK_OF_LINES);
    sigmaBuf = (float *) MALLOC (sizeof(float) * ns * CHUNK_OF_LINES);

    /* Break complex data into I and Q data planes and create amp & sigma0*/
    size = CHUNK_OF_LINES;
    percent_done=0;
    for (chunk=0; chunk<nl; chunk+=size) {
      if ((nl-chunk)<CHUNK_OF_LINES)
        size = nl-chunk;
      get_complexFloat_lines(inFp, metaIn, chunk, size, iqBuf);
      for (yy=0; yy<size; yy++) {
        overall_line = chunk+yy;
        print_layer_progress("Creating I, Q, amp, & sigma0 planes",
                             overall_line, nl, &percent_done);
        /*Allocate noise table entries and/or update if needed.*/
        if (overall_line==0
             || (overall_line%(nl/tableRes)==0
                 && cal_param->noise_type!=by_pixel)) {
          int qq;
          for (qq=0; qq<tableRes; qq++)
            noise_table[qq]=get_noise(cal_param, qq*tablePix, overall_line);
        }
        for (xx=0; xx<ns; xx++) {
          int bufInd = yy*ns + xx; /* buffer index */
          IBuf[bufInd] = iqBuf[bufInd].real;
          QBuf[bufInd] = iqBuf[bufInd].imag;
          ampBuf[bufInd] = sqrt(SQR(QBuf[bufInd]) + SQR(IBuf[bufInd]));
          if (ampBuf[bufInd]) {
          /*Interpolate noise table to find this pixel's noise.*/
              double index = (float) xx / tablePix;
              int    base  = (int)index;
              double frac  = index - base;
              double noise = noise_table[base] + frac
                             * (noise_table[base+1] - noise_table[base]);
              sigmaBuf[bufInd] = sprocket_get_cal_dn(cal_param,noise, 1.0,
                                                     (int)ampBuf[bufInd]);
          }
          else {
            sigmaBuf[bufInd]=0;
          }
        }
      }
      put_float_lines(IFp, metaOut, chunk, size, IBuf);
      put_float_lines(QFp, metaOut, chunk, size, QBuf);
      put_float_lines(ampFp, metaOut, chunk, size, ampBuf);
      put_float_lines(sigmaFp, metaOut, chunk, size, sigmaBuf);
    }
    /* Clean up after ourselves */
    FCLOSE(inFp);    FREE(iqBuf);
    FCLOSE(IFp);     FREE(IBuf);
    FCLOSE(QFp);     FREE(QBuf);
    FCLOSE(ampFp);   FREE(ampBuf);
    FCLOSE(sigmaFp); FREE(sigmaBuf);
  }

/* If it's not complex data, then it's detected (main function should make sure
 * of that!) */
  else {
    inFp = fopenImage(asfName,"rb");
    sigmaFp = fopenImage(sigmaName,"wb");
    ampBuf = (float *) MALLOC (sizeof(float) * ns * CHUNK_OF_LINES);
    sigmaBuf = (float *) MALLOC (sizeof(float) * ns * CHUNK_OF_LINES);

    /* Process sigma0 from the amplitude data  */
    size = CHUNK_OF_LINES;
    percent_done=0;
    for (chunk=0; chunk<nl; chunk+=size) {
      if ((nl-chunk)<CHUNK_OF_LINES)
        size = nl-chunk;
      get_float_lines(inFp, metaIn, chunk, size, ampBuf);
      for (yy=0; yy<size; yy++) {
        overall_line = chunk+yy;
        print_layer_progress("Creating sigma0 plane", overall_line, nl,
                             &percent_done);
        /*Allocate noise table entries and/or update if needed.*/
        if (overall_line==0
            || (overall_line%(nl/tableRes)==0
                && cal_param->noise_type!=by_pixel)) {
          int qq;
          for (qq=0; qq<tableRes; qq++)
            noise_table[qq]=get_noise(cal_param, qq*tablePix, overall_line);
        }
        for (xx=0; xx<ns; xx++) {
          int bufInd = yy*ns + xx; /* buffer index */
          if (ampBuf[bufInd]) {
          /*Interpolate noise table to find this pixel's noise.*/
              double index = (float) xx / tablePix;
              int    base  = (int)index;
              double frac  = index - base;
              double noise = noise_table[base] + frac
                             * (noise_table[base+1] - noise_table[base]);
              sigmaBuf[bufInd] = sprocket_get_cal_dn(cal_param, noise, 1.0,
                                                     (int)ampBuf[bufInd]);
          }
          else {
            sigmaBuf[bufInd]=0;
          }
        }
      }
      put_float_lines(sigmaFp, metaOut, chunk, size, sigmaBuf);
    }
    /* Clean up after ourselves */
    FCLOSE(inFp);    FREE(ampBuf);
    FCLOSE(sigmaFp); FREE(sigmaBuf);
  }

/* Now create the look angle layer (and maybe in the future lat, lon, and
 * incidence angle layers)
   ---------------------------------------------------------------------*/

/* Look angle calculations for map projected data */
  if (metaIn->sar->image_type=='P') {
    double px, py;

    /* Open up the files */
    lookFp = fopenImage(lookName,"wb");
/*    latFp = fopenImage(latName,"wb");
 *    lonFp = fopenImage(lonName,"wb"); */

    /* Allocate memory */
    lookBuf  = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
    sigmaBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
/*    latBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
 *    lonBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES); */

    size = CHUNK_OF_LINES;
    re = metaIn->projection->re_major;
    rp = metaIn->projection->re_minor;

    /* create layers */
    percent_done=0;
    for (chunk=0; chunk<nl; chunk+=size) {
      if ((nl-chunk)<CHUNK_OF_LINES)
        size = nl-chunk;
      for (yy=0; yy<size; yy++) {
        print_layer_progress("Creating look layer",
                             chunk+yy, nl, &percent_done);
        for (xx=0; xx<ns; xx++) {
          px = metaIn->projection->startX
               + metaIn->projection->perX * xx;
          py = metaIn->projection->startY
                + metaIn->projection->perY * (chunk+yy);
          proj_to_ll(metaIn->projection, metaIn->sar->look_direction, px, py,
                     &latitude, &longitude);
          latLon2timeSlant(metaIn, latitude, longitude, &time, &range, &doppler);
          stVec = meta_get_stVec(metaIn, time);
          earth_radius = get_earth_radius(time, stVec, re, rp);
          satellite_height = get_satellite_height(time, stVec);
          look_angle = get_look_angle(earth_radius, satellite_height, range);
/*          incidence_angle = get_incidence_angle(earth_radius, satellite_height,
 *                                                range);
 *          incidence_angle = meta_incid(metaIn, chunk+yy, xx);
 *          latBuf[xx+yy*ns]   = (float) latitude;
 *          lonBuf[xx+yy*ns]   = (float) longitude; */
          lookBuf[xx+yy*ns]  = (float) look_angle*R2D;
        }
      }
      put_float_lines(lookFp, metaOut, chunk+yy, size, lookBuf);
/*      put_float_lines(latFp, metaOut, chunk+yy, size, latBuf);
 *      put_float_lines(lonFp, metaOut, chunk+yy, size, lonBuf); */
    }
    FCLOSE(lookFp);
    FREE(lookBuf);
/*  FCLOSE(latFp);
 *  FREE(latBuf);
 *  FCLOSE(lonFp);
 *  FREE(lonBuf);*/
  }


/* Look angle (and maybe lat/lon) calculations for slant or ground range */
  else {
    quadratic_2d quad2D;
    char inLine[256];
    int ii, kk, ll;
    int nPoints, x, y;
    double ignored;
    double *l, *s, *value, line, sample;
    double firstLook=0.0/*, firstLat=0.0, firstLon=0.0*/;

    create_name(lookTiePointName, lookName, ".tps");
    lookTiePointFp = FOPEN(lookTiePointName,"wb");
    lookTiePointFlag = TRUE;
    /* Create tie point files for each layer... each of these files will be
     * written over with its associated data layer */
    for (ll=0; ll<=RES_X; ll++) {
      for (kk=0; kk<=RES_Y; kk++) {
        line = ll * nl / RES_Y;
        sample = kk * ns / RES_X;
        time = meta_get_time(metaIn, line, sample);
        stVec = meta_get_stVec(metaIn, time);
        earth_radius = get_earth_radius(time, stVec, re, rp);
        satellite_height = get_satellite_height(time, stVec);
        range = get_slant_range(metaIn, earth_radius, satellite_height, sample);
        look_angle = get_look_angle(earth_radius, satellite_height, range);
/*      incidence_angle = get_incidence_angle(earth_radius, satellite_height,
 *                                            range); */
        if (!metaIn->sar->deskewed)
          doppler = meta_get_dop(metaIn, line, sample);
        fixed2gei(&stVec, 0.0); /* subtracting the Earth's spin */
        getLatLongMeta(stVec, metaIn, range, doppler, 0, &latitude, &longitude,
                       &ignored);

        if (ll==0 && kk==0) {
          firstLook = look_angle * R2D;
/*          firstLat = latitude;
 *          firstLon = longitude; */
        }

        fprintf(lookTiePointFp, "%.18f %.12f %.12f\n", (float)look_angle*R2D, line, sample);
/*        fprintf(latFp, "%.18f %.12f %.12f\n", (float)latitude, line, sample);
 *        fprintf(lonFp, "%.18f %.12f %.12f\n", (float)longitude, line, sample); */
      }
    }
    FCLOSE(lookTiePointFp);
/*    FCLOSE(latFp);
 *    FCLOSE(lonFp); */

    /* set things up for least square calculation */
    value=(double *)MALLOC(sizeof(double)*MAX_PTS);
    l=(double *)MALLOC(sizeof(double)*MAX_PTS);
    s=(double *)MALLOC(sizeof(double)*MAX_PTS);

    /* Write Look Angle layer file */
    size = CHUNK_OF_LINES;
    nPoints = 0;
    lookTiePointFp = FOPEN(lookTiePointName, "r");
    while (NULL!=(fgets(inLine, 255, lookTiePointFp))) {
      sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]);
      nPoints++;
    }
    FCLOSE(lookTiePointFp);
    quad2D   = find_quadratic(value, l, s, nPoints);
    quad2D.A = firstLook;
    lookFp  = fopenImage(lookName, "wb");
    lookBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
    percent_done=0;
    for (ii=0; ii<nl; ii+=size) {
      if ((nl-ii)<CHUNK_OF_LINES)
        size = nl-ii;
      for (ll=0; ll<size; ll++) {
        print_layer_progress("Creating look angle layer", ii+ll, nl,
                             &percent_done);
        for (kk=0; kk<ns; kk++) {
          x = ii + ll;
          y = kk;
          look_angle = quad2D.A + quad2D.B*x + quad2D.C*y + quad2D.D*x*x
                       + quad2D.E*x*y + quad2D.F*y*y + quad2D.G*x*x*y
                       + quad2D.H*x*y*y + quad2D.I*x*x*y*y + quad2D.J*x*x*x
                       + quad2D.K*y*y*y;
          lookBuf[kk+ll*ns] = (float) look_angle;
        }
      }
      put_float_lines(lookFp, metaOut, ii, size, lookBuf);
    }
    FCLOSE(lookFp);
    FREE(lookBuf);

    /* Write Latitude file */
/*    size = CHUNK_OF_LINES;
 *    nPoints = 0;
 *    inFp = FOPEN(latName, "r");
 *    while (NULL!=(fgets(inLine, 255, inFp))) {
 *      sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]);
 *      nPoints++;
 *    }
 *    FCLOSE(inFp);
 *    quad2D   = find_quadratic(value, l, s, nPoints);
 *    quad2D.A = firstLat;
 *    latFp  = fopenImage(latName, "wb");
 *    latBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
 *    percent_done=0;
 *    for (ii=0; ii<nl; ii+=size) {
 *      if ((nl-ii)<CHUNK_OF_LINES)
 *        size = nl-ii;
 *      for (ll=0; ll<size; ll++) {
 *        print_layer_progress("Creating latitude layer", ii+ll, nl, &percent_done);
 *        for (kk=0; kk<ns; kk++) {
 *          x = ii + ll;
 *          y = kk;
 *          latitude = quad2D.A + quad2D.B*x + quad2D.C*y + quad2D.D*x*x
 *                     + quad2D.E*x*y+ quad2D.F*y*y + quad2D.G*x*x*y
 *                     + quad2D.H*x*y*y + quad2D.I*x*x*y*y + quad2D.J*x*x*x
 *                     + quad2D.K*y*y*y;
 *          latBuf[kk+ll*ns] = (float) latitude;
 *        }
 *      }
 *      put_float_lines(latFp, metaOut, ii, size, latBuf);
 *    }
 *    FCLOSE(latFp);
 *    FREE(latBuf); */

    /* Write Longitude file */
/*    size = CHUNK_OF_LINES;
 *    nPoints = 0;
 *    inFp = FOPEN(lonName, "r");
 *    while (NULL!=(fgets(inLine, 255, inFp))) {
 *      sscanf(inLine,"%lf%lf%lf", &value[nPoints], &l[nPoints], &s[nPoints]);
 *      nPoints++;
 *    }
 *    quad2D = find_quadratic(value, l, s, nPoints);
 *    quad2D.A = firstLon;
 *    lonFp = fopenImage(lonName, "wb");
 *    lonBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
 *    percent_done=0;
 *    for (ii=0; ii<nl; ii+=size) {
 *      if ((nl-ii)<CHUNK_OF_LINES) size = nl-ii;
 *      for (ll=0; ll<size; ll++) {
 *        print_layer_progress("Creating longitude layer", ii+ll, nl, &percent_done);
 *        for (kk=0; kk<ns; kk++) {
 *          x = ii + ll;
 *          y = kk;
 *          longitude = quad2D.A + quad2D.B*x + quad2D.C*y + quad2D.D*x*x
 *                      + quad2D.E*x*y+ quad2D.F*y*y + quad2D.G*x*x*y
 *                      + quad2D.H*x*y*y + quad2D.I*x*x*y*y + quad2D.J*x*x*x
 *                      + quad2D.K*y*y*y;
 *          lonBuf[kk+ll*ns] = (float) longitude;
 *        }
 *      }
 *      put_float_lines(lonFp, metaOut, ii, size, lonBuf);
 *    }
 *    FCLOSE(lonFp);
 *    FREE(lonBuf); */

  }

  /* Clean up & Report */
  if (lookTiePointFlag) remove(lookTiePointName);
  meta_free(metaIn);
  meta_free(metaOut);
  printf("SProCKET data planes and metadata have been created.\n");
}
