/******************************************************************************
PURPOSE:
  Create data layers for the SAR Processing Calibration Kit and Evaluation Tool
  (SProCKET)

PROGRAM HISTORY:
  DATE:   AUTHOR:    PURPOSE:
  -----   -------    --------
  5/04    P. Denny   Initial development. Creates sprocket style data layers
                      right after data is imported to asf tools format
  6/04    P. Denny   Fix to properly create the sigma0 layer for RSI CEOS
                      detected data (not done for slc!)

******************************************************************************/

#include "asf.h"
#include "asf_nan.h"
#include "asf_meta.h"
#include "ceos.h"
#include "metadata.h"
#include "jpl_proj.h"
#include "least_squares.h"
#include "calibrate.h"
#include "get_ceos_names.h"

#ifndef PI
# define PI 3.14159265358979323846
#endif
#define RES_X 16
#define RES_Y 16
#define MAX_PTS 300
#define MAX_tableRes 512

#define SQR(X) ((X)*(X))

/******************************************************************************
 * Is this complex data? (follows asf_meta.h ENUM 'data_type_t') */
int is_complex(data_type_t data_type)
{
  if ((data_type>=COMPLEX_BYTE) && (data_type<=COMPLEX_REAL64))
    return TRUE;
  else
    return FALSE;
}

/* Lots of neat quick math functions */
/******************************************************************************
 * */
double get_satellite_height(double time, stateVector stVec)
{
 return sqrt(SQR(stVec.pos.x) + SQR(stVec.pos.y) + SQR(stVec.pos.z));
}

/******************************************************************************
 * */
double get_earth_radius(double time, stateVector stVec, double re, double rp)
{
  double er = sqrt(SQR(stVec.pos.x) + SQR(stVec.pos.y) + SQR(stVec.pos.z));
  double lat = asin(stVec.pos.z/er);
  return (re*rp)/sqrt(SQR(rp)*cos(lat)*cos(lat)+SQR(re)*sin(lat)*sin(lat));
}

/******************************************************************************
 * */
double get_slant_range(meta_parameters *meta, double er, double ht, int sample)
{
  double minPhi = acos((SQR(ht) + SQR(er) -
                    SQR(meta->sar->slant_range_first_pixel))/(2.0*ht*er));
  double phi = minPhi+sample*(meta->general->x_pixel_size/er);
  double slantRng = sqrt(SQR(ht)+SQR(er)-2.0*ht*er*cos(phi));
  return slantRng + meta->sar->slant_shift;
}

/******************************************************************************
 * */
double get_look_angle(double er, double ht, double sr)
{
  return acos((SQR(sr)+SQR(ht)-SQR(er))/(2.0*sr*ht));
}

/******************************************************************************
 * NOT USEFUL FOR NOW:
 *double get_incidence_angle(double er, double ht, double sr)
 *{
 *  return PI-acos((SQR(sr)+SQR(er)-SQR(ht))/(2.0*sr*er));
 *}
 */
/******************************************************************************
 * KEPT FOR REFERENCE:
 * Calculate the look angle from the slant range, height and Earth Radius **
 *double slant2look (double slant_range,
 *                   double Re,          **Radius of the earth at nadir**
 *                   double ht           **satellite height from earth center**
 *                  )
 *{
 *  double ht_from_cent = ht - Re;
 *  double temp1 = SQR(slant_range) + 2.0*Re*ht_from_cent + SQR(ht_from_cent);
 *  double temp2 = 2.0 * slant_range * (ht);
 *  return R2D * acos ( temp1 / temp2);
 *}
 */
/******************************************************************************
 * KEPT FOR REFERENCE:
 * Convert Look angle to Incidence angle **
 *double look2incidence (double look,
 *                       double Re,    **Radius of the earth at nadir **
 *                       double ht     **satellite height from earth center **
 *                      )
 *{
 *   return R2D * asin( (ht) * sin((D2R*look)/Re) );
 *}
 */
/******************************************************************************
 * KEPT FOR REFERENCE:
 * Figure out sigma naught (in dB) for a detected product in the RSI CEOS file
 * format (currently would be a product from the FOCUS processor)
 *float sigma0_rsi_detected (float dn, float sineIncidence, float scalingGain)
 *{
 *  double dn2 = SQR(dn);
 *  double sigma0;
 *
 *  if (dn2 == 0)
 *    return 1E-10;
 *
 *   * How we got here (Detected data):
 *   * beta0 (dB) = 10*log ( dn^2 / scalingGain)
 *   * sigma0 (pow) = 10^[(beta0 + 10*log(sine(incidAngle))) / 10]
 *   * sigma0 (pow) = 10^[(10*log(dn^2/scalingGain) + 10*log(sine(incidAngle))) / 10]
 *   * sigma0 (pow) = 10^[log(dn^2/scalingGain) + log(sine(incidAngle))]
 *   * sigma0 (pow) = dn^2/scalingGain * sine(incidAngle)
 *  sigma0 = dn2 / scalingGain * sineIncidence;
 *
 *   * if you want it in dB instead of power
 *   * sigma0 (dB) = 10*log( dn^2/scalingGain * sine(incidAngle) ) *
 *  return sigma0;
 *}
 */
/******************************************************************************
 * NOT IN USE
 * Compute sigma0: CSA's product description, section 5.3.2
 *float sigma0_rsi_slc (float dn, float sineIncidence, float scalingGain)
 *{
 *  float beta, sigma;
 *  double dn2 = SQR(dn);
 *
 *  if (dn2 == 0)
 *    return 1E-10;
 *
 *  beta = 20.0 * log10 (dn2 / scalingGain);
 *  sigma = pow (10.0, (beta + sineIncidence) / 10.0);
 *
 *  return sigma;
 *}
 */

#define ASC 12
#define DESC 22
/******************************************************************************
* Gets radiometric info from the leader file and creates a lookup table the
 * width of a data line. Allocates memory that needs to be freed */
float *create_rsi_scaling_table(struct RSI_VRADDR rdr, int direction, int ns)
{
  int lut_end;           /* 0 indexed end of the radiometric lookup table */
  float *scalingGain;    /* Scaling gain array */
  double A2;             /* Scaling gain value for iith pixel */
  int Il;                /* Index at lower end of lookup table */
  int Iu;                /* Index at upper end of lookup table */
  int ii;                /* Sample index */

  lut_end = rdr.n_samp-1;
  scalingGain = (float*)MALLOC(sizeof(float)*ns);

  for (ii=0; ii<ns; ii++) {
    if (direction == ASC) {
      Il = ii / rdr.samp_inc;
      if (Il >= lut_end) {
        A2 = rdr.lookup_tab[lut_end]
             + (rdr.lookup_tab[lut_end]-rdr.lookup_tab[lut_end-1])
               * ((ii/rdr.samp_inc)-lut_end);
      }
      else {
        Iu = Il + 1;
        A2 = rdr.lookup_tab[Il]
             + (rdr.lookup_tab[Iu]-rdr.lookup_tab[Il])
               * ((ii/rdr.samp_inc)-Il);
      }
    }
    else {
      Il = (ns-ii-1.0) / rdr.samp_inc;
      if (Il >= lut_end) {
        A2 = rdr.lookup_tab[lut_end]
             + (rdr.lookup_tab[lut_end]-rdr.lookup_tab[lut_end-1])
               * ((ns-1-ii)/rdr.samp_inc - lut_end);
      }
      else {
        Iu = Il - 1;
        A2 = rdr.lookup_tab[Il]
             + (rdr.lookup_tab[Iu]-rdr.lookup_tab[Il])
               * ((ii/rdr.samp_inc)-Il);
      }
    }
    scalingGain[ii] = A2;
  }

  return scalingGain;
}



/******************************************************************************
 * The actual point of this file! This function creates the data layers
* necessary for SProCKET to do its magic */
void create_sprocket_layers(const char *asfName, char *leaderName)
{
  FILE *ampFp=NULL, *lookFp=NULL, *sigmaFp=NULL;
  FILE *lookTiePointFp=NULL;
/*FILE *latFp=NULL, *lonFp=NULL;*/
  FILE *inFp=NULL;
  struct dataset_sum_rec *dssr=NULL;
  meta_parameters *metaIn=NULL, *metaOut=NULL;
  stateVector stVec;
  cal_params *cal_param=NULL;
  int nl, ns;
  int yy/*lineIndex*/, xx/*sampleIndex*/, chunk, size;
  int overall_line;
  int lookTiePointFlag=FALSE;
  int tableRes=MAX_tableRes, tablePix=0;
  int doComplex=FALSE;
  int rsiCeos=FALSE;
  char junk[256];
  char metaName[256], sprocketMetaName[256];
  char ampName[256], lookName[256], sigmaName[256];
  char lookTiePointName[256];
  char **junk1, junk2[256];
/*char latName[256], lonName[256];*/
  float *lookLayerBuf=NULL; /*Entire look layer has its own buffer*/
  float *ampBuf=NULL, *lookBuf=NULL, *sigmaBuf=NULL;
/*float *latBuf=NULL, *lonBuf=NULL;*/
  double latitude, longitude, time, doppler, earth_radius;
  double satellite_height, range, look_angle/*, incidence_angle*/;
  double re=NAN, rp=NAN;
/*Calibration stuff*/
  double noise_table[MAX_tableRes];
  int ii, nBands=1;

  junk1 = (char **) MALLOC(512*MAX_BANDS*sizeof(char));
  for (ii=0; ii<MAX_BANDS; ii++)
    junk1[ii] = (char *) MALLOC (512*sizeof(char));

  if (CEOS_dat_lea_PAIR==get_ceos_names(leaderName, junk1, junk2, &nBands)) {
    rsiCeos=TRUE;
  }

  /* Tell user we're starting */
  asfPrintStatus("Now creating SProCKET metadata and data layers...\n");

  /* Set up file names */
  strcat(strcpy(metaName, asfName), ".meta");
  strcat(strcpy(sprocketMetaName, asfName), METADATA_EXT);
  strcat(strcpy(ampName,asfName), DATA_EXT);
  strcat(strcpy(lookName, asfName), LOOK_EXT);
  strcat(strcpy(sigmaName, asfName), SIGMA_EXT);
/*  strcat(strcpy(latName, asfName), LATITUDE_EXT);
 *  strcat(strcpy(lonName, asfName), LONGITUDE_EXT);
 */
  /* Get metadata */
  metaIn = meta_read(metaName);
  nl = metaIn->general->line_count;
  ns = metaIn->general->sample_count;
  if (meta_is_valid_double(metaIn->general->re_major))
    re = metaIn->general->re_major;
  if (meta_is_valid_double(metaIn->general->re_minor))
    rp = metaIn->general->re_minor;
  metaOut = meta_copy(metaIn);
  metaOut->general->data_type = REAL32;

  /* Used for creating tie point files */
  doppler = 0.0;

  /* We can get some useful stuff from the CEOS leader file, like the
   * calibration parameters */
  if (get_ceos_metadata_name(leaderName,junk) != NO_CEOS_METADATA) {
    cal_param = create_cal_params(leaderName);
    dssr = (struct dataset_sum_rec*) MALLOC (sizeof(struct dataset_sum_rec));
    get_dssr(leaderName, dssr);
    if (nl<1500) tableRes=128;
    else if (nl<3000) tableRes=256;
    tablePix=( (ns+(tableRes-1))/tableRes );
    if (cal_param==NULL) {
      asfPrintStatus(
          "*************************** WARNING! ***************************\n"
          " Calibration parameters could not be extracted out of CEOS file\n"
          " There will be no sigma0 data layer.\n"
          "***************************************************************\n");
    }
  }

  /* Write the sprocket format metadata */
  asfPrintStatus("Creating metadata file.\n");
  meta_write_sprocket(sprocketMetaName, metaOut, dssr);
  FREE(dssr);
  asfPrintStatus("Done.\n");

  doComplex = (is_complex(metaIn->general->data_type)) ? TRUE : FALSE;

  /* We'll be using the look buffer for the entire time so allocate it now */
  lookLayerBuf  = (float *) MALLOC(ns * nl * sizeof(float));


/* First create the look angle layer (and maybe in the future lat, lon, and
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
/*    latBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES);
 *    lonBuf = (float *) MALLOC(ns * sizeof(float) * CHUNK_OF_LINES); */

    size = CHUNK_OF_LINES;

    /* create layers */
    asfPrintStatus("Creating look angle layer:\n");
    for (chunk=0; chunk<nl; chunk+=size) {
      if ((nl-chunk)<CHUNK_OF_LINES)
        size = nl-chunk;
      for (yy=0; yy<size; yy++) {
        asfPercentMeter((double)(chunk+yy+1)/(double)nl);
        for (xx=0; xx<ns; xx++) {
          px = metaIn->projection->startX
               + metaIn->projection->perX * xx;
          py = metaIn->projection->startY
                + metaIn->projection->perY * (chunk+yy);
          proj_to_latlon(metaIn->projection, px, py,
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
          lookLayerBuf[xx+(chunk+yy)*ns] = lookBuf[xx+yy*ns];
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
    asfPrintStatus("Creating look angle layer:\n");
    for (ii=0; ii<nl; ii+=size) {
      if ((nl-ii)<CHUNK_OF_LINES)
        size = nl-ii;
      for (ll=0; ll<size; ll++) {
        asfPercentMeter((double)(ii+ll+1)/(double)nl);
        for (kk=0; kk<ns; kk++) {
          x = ii + ll;
          y = kk;
          look_angle = quad2D.A + quad2D.B*x + quad2D.C*y + quad2D.D*x*x
                       + quad2D.E*x*y + quad2D.F*y*y + quad2D.G*x*x*y
                       + quad2D.H*x*y*y + quad2D.I*x*x*y*y + quad2D.J*x*x*x
                       + quad2D.K*y*y*y;
          lookBuf[kk+ll*ns] = (float) look_angle;
          lookLayerBuf[kk+ii*ns] = lookBuf[kk+ll*ns];
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
 *    asfPrintStatus("Creating latitude layer:\n");
 *    for (ii=0; ii<nl; ii+=size) {
 *      if ((nl-ii)<CHUNK_OF_LINES)
 *        size = nl-ii;
 *      for (ll=0; ll<size; ll++) {
 *        asfPercentMeter((double)(ii+ll+1)/(double)nl);
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
 *    asfPrintStatus("Creating longitude layer:\n");
 *    for (ii=0; ii<nl; ii+=size) {
 *      if ((nl-ii)<CHUNK_OF_LINES) size = nl-ii;
 *      for (ll=0; ll<size; ll++) {
 *        asfPercentMeter((double)(ii+ll+1)/(double)nl);
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

  /* A little clean up */
  if (lookTiePointFlag) remove(lookTiePointName);

/* Now create all layers except the look angle layer (and maybe in the future
 * lat, lon, and incidence angle layers)
   --------------------------------------------------------------------------*/

  /* Create I & Q data layers if we got complex data */
  if (doComplex) {
    if (!rsiCeos) {
      char IName[256], QName[256];
      FILE *IFp, *QFp;
      complexFloat *iqBuf;
      float *IBuf, *QBuf;

      /* File naming, opening, & memory allocation */
      strcat(strcpy(IName, asfName), COMPLEX_I_PLANE);
      strcat(strcpy(QName, asfName), COMPLEX_Q_PLANE);
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

      /* Break complex data into I and Q data layers and create amp & sigma0*/
      size = CHUNK_OF_LINES;
      asfPrintStatus("Creating I, Q, amp, & sigma0 layers:\n");
      for (chunk=0; chunk<nl; chunk+=size) {
        if ((nl-chunk)<CHUNK_OF_LINES)
          size = nl-chunk;
        get_complexFloat_lines(inFp, metaIn, chunk, size, iqBuf);
        for (yy=0; yy<size; yy++) {
          overall_line = chunk+yy;
          asfPercentMeter((double)(overall_line+1)/(double)nl);
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
                sigmaBuf[bufInd] = get_cal_dn(cal_param,noise, 1.0,
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

    /* What to do if it's in ASF CEOS format */
    if (!rsiCeos) {
      asfPrintStatus("Creating sigma0 layer:\n");
      for (chunk=0; chunk<nl; chunk+=size) {
        if ((nl-chunk)<CHUNK_OF_LINES)
          size = nl-chunk;
        get_float_lines(inFp, metaIn, chunk, size, ampBuf);
        for (yy=0; yy<size; yy++) {
          overall_line = chunk+yy;
          asfPercentMeter((double)(overall_line+1)/(double)nl);
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
    }

    /* What to do if it's in RSI CEOS format */
    else /*if (rsiCeos)*/ {
      int dir = (metaIn->general->orbit_direction=='A') ? ASC : DESC;
      float *scalingGain;
      double ht = meta_get_sat_height(metaIn, nl/2, ns/2);
      double Re = meta_get_earth_radius(metaIn, nl/2, ns/2);
      struct RSI_VRADDR rdr;

      get_rsi_raddr(leaderName, &rdr);
      scalingGain = create_rsi_scaling_table(rdr, dir, ns);

      asfPrintStatus("Creating sigma0 layer:\n");
      for (chunk=0; chunk<nl; chunk+=size) {
        if ((nl-chunk)<CHUNK_OF_LINES)
        size = nl-chunk;
        get_float_lines(inFp, metaIn, chunk, size, ampBuf);
        for (yy=0; yy<size; yy++) {
          overall_line = chunk+yy;
          asfPercentMeter((double)(overall_line+1)/(double)nl);
          for (xx=0; xx<ns; xx++) {
            int bufInd = yy*ns+xx;
            float sinIncid = ht * sin((D2R*lookLayerBuf[xx])/Re);
            /* How we got this sigma0 equation for RSI detected data:
             * Start with equation in Document number RSI-GS-026 Section 5.4
             * beta0 (dB) = 10*log ( (dn^2+offset) / scalingGain)
             * sigma0 (pow) = 10^[(beta0 + 10*log(sin(incidAngle))) / 10]
             * sigma0 (pow) = 10^[(10*log((dn^2+offset)/scalingGain) + 10*log(sin(incidAngle))) / 10]
             * sigma0 (pow) = 10^[log((dn^2+offset)/scalingGain) + log(sin(incidAngle))]
             * sigma0 (pow) = (dn^2+offset)/scalingGain * sin(incidAngle) */
            sigmaBuf[bufInd] = ( SQR(ampBuf[bufInd])+rdr.offset )
                               / scalingGain[xx] * sinIncid;
          }
        }
        put_float_lines(sigmaFp, metaOut, chunk, size, sigmaBuf);
      }
      FREE(scalingGain);
    }

    /* Clean up after ourselves */
    FCLOSE(inFp);    FREE(ampBuf);
    FCLOSE(sigmaFp); FREE(sigmaBuf);
  }

  /* Clean up a bit & report */
  FREE(lookLayerBuf);

  asfPrintStatus("Finished creating SProCKET layers.\n");
}
