/*******************************************************************************
FUNCTION NAME:  meta_init_ceos

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0  - O. Lawlor.           9/98   CEOS Independence.
  1.5  - P. Denny.            8/02   Formatted for new meta structure
  2.0  - P. Denny / R. Gens   9/03   ASF facility Data Record independence
                                      And merged this with meta_init_asf.c
  3.0  - R. Gens              7/07   Rewrite to clean up the mess and clutter
*******************************************************************************/
#include <assert.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include <ctype.h>
#include <string.h> /* for memset */
#include "typlim.h"
#include "meta_init.h"
#include "asf_endian.h"
#include "dateUtil.h"
#include "get_ceos_names.h"
#include "libasf_proj.h"
#include "spheroids.h"
#include "meta_project.h"
#include "frame_calc.h"

#ifndef MIN
#  define MIN(a,b)  (((a) < (b)) ? (a) : (b))
#endif

// ALOS beam modes
char *alos_beam_mode[132]={
  "FBS1","FBS2","FBS3","FBS4","FBS5","FBS6","FBS7","FBS8","FBS9","FBS10",
  "FBS11","FBS12","FBS13","FBS14","FBS15","FBS16","FBS17","FBS18",
  "FBS1","FBS2","FBS3","FBS4","FBS5","FBS6","FBS7","FBS8","FBS9","FBS10",
  "FBS11","FBS12","FBS13","FBS14","FBS15","FBS16","FBS17","FBS18",
  "FBD1","FBD2","FBD3","FBD4","FBD5","FBD6","FBD7","FBD8","FBD9","FBD10",
  "FBD11","FBD12","FBD13","FBD14","FBD15","FBD16","FBD17","FBD18",
  "FBD1","FBD2","FBD3","FBD4","FBD5","FBD6","FBD7","FBD8","FBD9","FBD10",
  "FBD11","FBD12","FBD13","FBD14","FBD15","FBD16","FBD17","FBD18",
  "WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2","WD1","WD2",
  "DSN1","DSN2","DSN3","DSN4","DSN5","DSN6","DSN7","DSN8","DSN9","DSN10",
  "DSN11","DSN12","DSN13","DSN14","DSN15","DSN16","DSN17","DSN18",
  "DSN1","DSN2","DSN3","DSN4","DSN5","DSN6","DSN7","DSN8","DSN9","DSN10",
  "DSN11","DSN12","DSN13","DSN14","DSN15","DSN16","DSN17","DSN18",
  "PLR1","PLR2","PLR3","PLR4","PLR5","PLR6","PLR7","PLR8","PLR9","PLR10",
  "PLR11","PLR12"};


// Internal Prototypes
// Importing CEOS SAR data
void ceos_init_sar(ceos_description *ceos, const char *in_fName,
       meta_parameters *meta);
void ceos_init_sar_asf(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta);
void ceos_init_sar_focus(ceos_description *ceos, const char *in_fName,
       meta_parameters *meta);
void ceos_init_sar_esa(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta);
void ceos_init_sar_eoc(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta);
void ceos_init_sar_rsi(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta);
void ceos_init_sar_jpl(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta);
void ceos_init_sar_dpaf(ceos_description *ceos, const char *in_fName,
      meta_parameters *meta);
void ceos_init_sar_ipaf(ceos_description *ceos, const char *in_fName,
      meta_parameters *meta);
void ceos_init_sar_beijing(ceos_description *ceos, const char *in_fName,
         meta_parameters *meta);
int  meta_sar_to_startXY (meta_parameters *meta,
                          double *startX, double *startY);
double spheroidDiffFromAxis (spheroid_type_t spheroid, double n_semi_major, double n_semi_minor);

// Importing CEOS optical data
void ceos_init_optical(const char *in_fName,meta_parameters *meta);
void ceos_init_scansar(const char *leaderName, meta_parameters *meta,
           struct dataset_sum_rec *dssr,
           struct VMPDREC *mpdr, struct VFDRECV *asf_facdr);

// Importing specific metadata blocks
void ceos_init_location_block(meta_parameters *meta);
void ceos_init_proj(meta_parameters *meta,  struct dataset_sum_rec *dssr,
                    struct VMPDREC *mpdr, struct scene_header_rec *shr,
                    struct alos_map_proj_rec *ampr);
double get_firstTime(const char *fName);
double get_alos_firstTime (const char *fName);

double get_chirp_rate (const char *fName);
double get_sensor_orientation (const char *fName);

void get_azimuth_time(ceos_description *ceos, const char *in_fName,
          meta_parameters *meta);


/* Prototypes from meta_init_stVec.c */
void ceos_init_stVec(const char *fName,ceos_description *ceos,meta_parameters *sar);
double get_timeDelta(ceos_description *ceos,struct pos_data_rec *ppdr,
                     meta_parameters *meta);

/* Prototypes from jpl_proj.c */
int UTM_zone(double lon);

/* Utility function: convert CEOS-style beam type to ASF meta-style beam name. */
static void beam_type_to_asf_beamname(
    const char *beam_type,int btlen,
    char *beamname,int bnlen)
{
    if (strlen(beam_type) <= 0 || strlen(beamname) <= 0 ||
        btlen <= 0 || bnlen <= 0 ||
        strncmp(beam_type, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) == 0 ||
        strncmp(beamname, MAGIC_UNSET_STRING, strlen(MAGIC_UNSET_STRING)) == 0)
    {
        strcpy(beamname, MAGIC_UNSET_STRING);
    }
    if (strncmp(beam_type, "F5", 2) == 0)
      strcpy(beamname, "FN5");
    else if (strncmp(beam_type, "F4", 2) == 0)
      strcpy(beamname, "FN4");
    else if (strncmp(beam_type, "F3", 2) == 0)
      strcpy(beamname, "FN3");
    else if (strncmp(beam_type, "F2", 2) == 0)
      strcpy(beamname, "FN2");
    else if (strncmp(beam_type, "F1", 2) == 0)
      strcpy(beamname, "FN1");
    else if (strncmp(beam_type, "S7", 2) == 0)
      strcpy(beamname, "ST7");
    else if (strncmp(beam_type, "S6", 2) == 0)
      strcpy(beamname, "ST6");
    else if (strncmp(beam_type, "S5", 2) == 0)
      strcpy(beamname, "ST5");
    else if (strncmp(beam_type, "S4", 2) == 0)
      strcpy(beamname, "ST4");
    else if (strncmp(beam_type, "S3", 2) == 0)
      strcpy(beamname, "ST3");
    else if (strncmp(beam_type, "S2", 2) == 0)
      strcpy(beamname, "ST2");
    else if (strncmp(beam_type, "S1", 2) == 0)
      strcpy(beamname, "ST1");
    else {
      strncpy(beamname, beam_type, btlen);
      beamname[btlen]=0; /* NUL terminate, because CEOS fields aren't */
    }
}

/*******************************************************************************
 * ceos_init:
 * Reads structure parameters from CEOS into existing meta_parameters
 * structure.  Calls the facility-specific decoders below. */
void ceos_init(const char *in_fName, meta_parameters *meta, report_level_t level)
{
   ceos_description *ceos = get_ceos_description(in_fName, level);

   if (ceos->sensor == SAR || ceos->sensor == PALSAR)
     ceos_init_sar(ceos, in_fName, meta);
   else if (ceos->sensor == AVNIR || ceos->sensor == PRISM)
     ceos_init_optical(in_fName, meta);

   FREE(ceos);
}


/*******************************************************************************
 * ceos_init_sar:
 * Reads SAR structure parameters from CEOS into existing meta_parameters
 * structure.  Calls the facility-specific decoders below. */
void ceos_init_sar(ceos_description *ceos, const char *in_fName,
       meta_parameters *meta)
{
  if (ceos->facility == ASF && ceos->processor != FOCUS && ceos->processor != LZP)
    ceos_init_sar_asf(ceos, in_fName, meta);
  else if (ceos->facility == ASF &&
     (ceos->processor == FOCUS || ceos->processor == LZP))
    ceos_init_sar_focus(ceos, in_fName, meta);
  else if (ceos->facility == ESA)
    ceos_init_sar_esa(ceos, in_fName, meta);
  else if (ceos->facility == CDPF)
    ceos_init_sar_focus(ceos, in_fName, meta);
  else if (ceos->facility == EOC)
    ceos_init_sar_eoc(ceos, in_fName, meta);
  else if (ceos->facility == RSI)
    ceos_init_sar_rsi(ceos, in_fName, meta);
  else if (ceos->facility == JPL)
    ceos_init_sar_jpl(ceos, in_fName, meta);
  else if (ceos->facility == CSTARS)
    ceos_init_sar_focus(ceos, in_fName, meta);
  else if (ceos->facility == DPAF)
    ceos_init_sar_dpaf(ceos, in_fName, meta);
  else if (ceos->facility == IPAF)
    ceos_init_sar_ipaf(ceos, in_fName, meta);
  else if (ceos->facility == BEIJING)
    ceos_init_sar_beijing(ceos, in_fName, meta);
  else if (ceos->facility == unknownFacility)
    asfPrintError("Unknown CEOS facility! Data cannot be imported "
      "at this time.\n");
  else
    asfPrintError("Should never get here!\n");

  create_cal_params(in_fName, meta);

}

void ceos_init_sar_general(ceos_description *ceos, const char *in_fName,
         meta_parameters *meta)
{
  char fac[50],sys[50],ver[50];     /* Fields describing the SAR processor   */
  struct dataset_sum_rec *dssr=NULL;/* Data set summary record               */
  struct IOF_VFDR *iof=NULL;        /* Image File Descriptor Record          */
  struct VMPDREC *mpdr=NULL;        /* Map Projection Data Record            */
  int dataSize;
  int nBands=1;
  ymd_date date;
  hms_time time;

  meta->sar = meta_sar_init();

  dssr = &ceos->dssr;
  iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
  get_ifiledr(in_fName, iof);
  mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
  if (get_mpdr(in_fName, mpdr) == -1) {
    FREE(mpdr);
    mpdr = NULL;
  }

  // Fill meta->general structure
  char *basename = get_basename(in_fName);
  strcpy(meta->general->basename, basename);
  if(basename)FREE(basename);
  strcpy(meta->general->sensor, dssr->mission_id);
  strtok(meta->general->sensor, " "); // Remove spaces from field.
  sprintf(meta->general->sensor_name, "SAR");
  if (strlen(dssr->beam1) > 0 && strlen(dssr->beam1) <= (MODE_FIELD_STRING_MAX)) {
    strcpy(meta->general->mode, dssr->beam1);
  }
  strcpy(fac,dssr->fac_id); strtok(fac," "); // Remove spaces from field
  strcpy(sys,dssr->sys_id); strtok(sys," "); // Remove spaces from field
  strcpy(ver,dssr->ver_id); strtok(ver," "); // Remove spaces from field
  sprintf(meta->general->processor, "%s/%s/%s", trim_spaces(fac), sys, ver);
  // FOCUS data header is erroneous, hence the if statement
  if ((iof->bitssamp*iof->sampdata)>(iof->bytgroup*8)) iof->bitssamp /= 2;
  dataSize = (iof->bitssamp+7)/8 + (iof->sampdata-1)*5;
  if ((dataSize<6) && (strncmp(iof->formatid, "COMPLEX", 7)==0))
    dataSize += (10 - dataSize)/2;
  switch (dataSize)
    {
    case 2:  meta->general->data_type = INTEGER16;         break;
    case 4:  meta->general->data_type = INTEGER32;         break;
    case 6:  meta->general->data_type = COMPLEX_BYTE;      break;
    case 7:  meta->general->data_type = COMPLEX_INTEGER16; break;
    case 9:  meta->general->data_type = COMPLEX_REAL32;    break;
    default: meta->general->data_type = BYTE;              break;
    }
  strcpy(meta->general->system, meta_get_system());
  if (ceos->facility == BEIJING)
    date_dssr2time(dssr->inp_sctim, &date, &time);
  else
    date_dssr2date(dssr->inp_sctim, &date, &time);
  date_dssr2time_stamp(&date, &time, meta->general->acquisition_date);
  meta->general->orbit = atoi(dssr->revolution);
  meta->general->orbit_direction  = dssr->asc_des[0];
  meta->general->band_count = nBands;
  strcpy(meta->general->bands, "");
  meta->general->line_count = iof->numofrec;
  meta->general->sample_count  =
    (iof->reclen       // record length
     -iof->predata     // prefix data
     -iof->sufdata)    // suffix data
    /iof->bytgroup
    -iof->lbrdrpxl    // left border pixels
    -iof->rbrdrpxl;   // right border pixels
  meta->general->start_line       = 0;
  meta->general->start_sample     = 0;
  meta->general->x_pixel_size     = dssr->pixel_spacing;
  meta->general->y_pixel_size     = dssr->line_spacing;
  meta->general->center_latitude  = dssr->pro_lat;
  meta->general->center_longitude = dssr->pro_long;
  meta->general->re_major = (dssr->ellip_maj < 10000.0) ?
    dssr->ellip_maj*1000.0 : dssr->ellip_maj;
  meta->general->re_minor = (dssr->ellip_min < 10000.0) ?
    dssr->ellip_min*1000.0 : dssr->ellip_min;
  if (ceos->product == SSG ||
      ceos->product == SPG ||
      ceos->product == GEC ||
      ceos->product == SCANSAR ||
      ceos->product == SCN ||
      ceos->product == LEVEL_1B2G)
    meta->general->no_data = 0;
  else
    meta->general->no_data = MAGIC_UNSET_DOUBLE;

  // Fill meta->sar structure
  meta->sar->look_direction = (dssr->clock_ang>=0.0) ? 'R' : 'L';
  meta->sar->original_line_count   = iof->numofrec;
  meta->sar->original_sample_count =
    (iof->reclen-iof->predata-iof->sufdata-iof->lbrdrpxl-iof->rbrdrpxl)
    / iof->bytgroup;
  if ( meta->sar->original_line_count==0
       || meta->sar->original_sample_count==0) {
    meta->sar->original_line_count   = dssr->sc_lin*2;
    meta->sar->original_sample_count = dssr->sc_pix*2;
  }
  meta->sar->line_increment   = 1.0;
  meta->sar->sample_increment = 1.0;
  meta->sar->range_time_per_pixel = dssr->n_rnglok
    / (dssr->rng_samp_rate * get_units(dssr->rng_samp_rate,EXPECTED_FS));
  meta->sar->slant_shift = 0.0;

  // Azimuth time per pixel needs to be known for state vector propagation
  char **dataName;
  double firstTime, centerTime;
  if ((ceos->facility == CSTARS || ceos->facility == ESA ||
       ceos->facility == DPAF || ceos->facility == IPAF) &&
      ceos->satellite == ERS) {
    date_dssr2time(dssr->az_time_first, &date, &time);
    firstTime = date_hms2sec(&time);
  }
  else {
    require_ceos_data(in_fName, &dataName, &nBands);
    firstTime = get_firstTime(dataName[0]);
    free_ceos_names(dataName, NULL);
  }
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel =
      (centerTime - firstTime) / (meta->sar->original_line_count/2);
  //printf("firstTime: %lf, centerTime: %lf\n", firstTime, centerTime);
  //printf("azimuth time per pixel: %f\n", meta->sar->azimuth_time_per_pixel);
  //meta->sar->azimuth_time_per_pixel = -0.0075685327312;
  //meta->sar->range_time_per_pixel = 1.9339617657e-07;

  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
         meta->sar->azimuth_time_per_pixel);
  meta->sar->wavelength = dssr->wave_length *
    get_units(dssr->wave_length,EXPECTED_WAVELEN);
  meta->sar->prf = dssr->prf *
    get_units(dssr->prf,EXPECTED_PRF);
  strcpy(meta->sar->satellite_binary_time,dssr->sat_bintim);
  strtok(meta->sar->satellite_binary_time," "); // Remove spaces from field
  strcpy(meta->sar->satellite_clock_time, dssr->sat_clktim);
  strtok(meta->sar->satellite_clock_time, " "); // Remove spaces from field
  meta->sar->azimuth_doppler_coefficients[0] = dssr->alt_dopcen[0];
  meta->sar->azimuth_doppler_coefficients[1] = dssr->alt_dopcen[1];
  meta->sar->azimuth_doppler_coefficients[2] = dssr->alt_dopcen[2];
  // check Doppler number whether they make sense, otherwise set to 'NaN'
  if (fabs(meta->sar->range_doppler_coefficients[0])>=15000) {
    meta->sar->range_doppler_coefficients[0]=MAGIC_UNSET_DOUBLE;
    meta->sar->range_doppler_coefficients[1]=MAGIC_UNSET_DOUBLE;
    meta->sar->range_doppler_coefficients[2]=MAGIC_UNSET_DOUBLE;
  }
  if (fabs(meta->sar->azimuth_doppler_coefficients[0])>=15000) {
    meta->sar->azimuth_doppler_coefficients[0]=MAGIC_UNSET_DOUBLE;
    meta->sar->azimuth_doppler_coefficients[1]=MAGIC_UNSET_DOUBLE;
    meta->sar->azimuth_doppler_coefficients[2]=MAGIC_UNSET_DOUBLE;
  }
  meta->sar->azimuth_processing_bandwidth = dssr->bnd_azi;
  meta->sar->chirp_rate = dssr->phas_coef[2];
  meta->sar->pulse_duration = dssr->rng_length / 1000000;
  meta->sar->range_sampling_rate = dssr->rng_samp_rate *
    get_units(dssr->rng_samp_rate,EXPECTED_SAMP_RATE);
  meta->sar->multilook = 1;

  /* FREE(dssr); Don't free dssr; it points to the ceos struct (ceos->dssr) */
  FREE(iof);
  if (mpdr)
    FREE(mpdr);
}

////////////////////////////////////////
// Individual facility initialization //
////////////////////////////////////////

void ceos_init_sar_asf(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct VFDRECV *asf_facdr=NULL;
  struct VMPDREC *mpdr=NULL;
  int ii;
  char beamname[32], buf[50];

  dssr = &ceos->dssr;
  asf_facdr=(struct VFDRECV*)MALLOC(sizeof(struct VFDRECV));
  get_asf_facdr(in_fName, asf_facdr);
  for (ii=0; ii<32; ii++)
    beamname[ii] = '\0';

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS-1", 5) == 0) {
    strcpy(meta->general->sensor,"ERS1");
    strcpy(meta->general->mode, "STD");
    meta->general->frame =
      asf_frame_calc(meta->general->sensor, meta->general->center_latitude,
                     meta->general->orbit_direction);
    meta->sar->look_count = 5;
    sprintf(meta->sar->polarization, "VV");
  }
  else if (strncmp(dssr->mission_id, "ERS-2", 5) == 0) {
    strcpy(meta->general->sensor,"ERS2");
    strcpy(meta->general->mode, "STD");
    meta->general->frame =
      asf_frame_calc(meta->general->sensor, meta->general->center_latitude,
                     meta->general->orbit_direction);
    meta->sar->look_count = 5;
    sprintf(meta->sar->polarization, "VV");
  }
  else if (strncmp(dssr->mission_id, "JERS-1", 6) == 0) {
    strcpy(meta->general->sensor,"JERS1");
    strcpy(meta->general->mode, "STD");
    meta->general->frame =
      asf_frame_calc(meta->general->sensor, meta->general->center_latitude,
                     meta->general->orbit_direction);
    meta->sar->look_count = 3;
    sprintf(meta->sar->polarization, "HH");
  }
  else if (strncmp(dssr->mission_id, "RSAT", 4) == 0) {
    if (ceos->product == SCANSAR) {
      if (strncmp(dssr->beam3,"WD3",3)==0) strcpy(beamname,"SWA");
      else if (strncmp(dssr->beam3,"ST5",3)==0) strcpy(beamname,"SWB");
      else if (strncmp(dssr->beam3,"ST6",3)==0) strcpy(beamname,"SNA");
      else strcpy(beamname,"SNB");
      mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
      get_mpdr(in_fName, mpdr);
    }
    else {
     int beamnum = atoi(&(dssr->beam1[2]));
      switch(dssr->beam1[0]) {
      case 'S': sprintf(beamname,"ST%i",beamnum); break;
      case 'W': sprintf(beamname,"WD%i",beamnum); break;
      case 'F': sprintf(beamname,"FN%i",beamnum); break;
      case 'E':
  if (dssr->beam1[1]=='H') sprintf(beamname,"EH%i",beamnum);
  else sprintf(beamname,"EL%i",beamnum);
  break;
      }
    }
    strcpy(meta->general->mode, beamname);
    strncpy(buf, &dssr->product_id[7], 3);
    buf[3]=0;
    meta->general->frame = atoi(buf);
    if (dssr->rng_samp_rate < 20.0) /* split finebeam from the rest */
      meta->sar->look_count = 4; /* ST1-ST7, WD1-WD3, EL1, EH1-EH6 */
    else
      meta->sar->look_count = 1; /* FN1-FN5 */
    sprintf(meta->sar->polarization, "HH");
  }
  meta->general->bit_error_rate = asf_facdr->biterrrt;

  // State vector block
  //ceos_init_stVec(in_fName, ceos, meta);

  // SAR block
  if (0==strncmp(asf_facdr->grndslnt,"GROUND",6))
    meta->sar->image_type= 'G';
  else
    meta->sar->image_type= 'S';
  if (ceos->product == SCANSAR)
    meta->sar->image_type= 'P';
  // FIXME: sort out the specifics about sar->image_type
  if (toupper(asf_facdr->deskewf[0])=='Y')
    meta->sar->deskewed = 1;
  else
    meta->sar->deskewed = 0;
  meta->sar->azimuth_time_per_pixel =
    meta->general->y_pixel_size / asf_facdr->swathvel;
  if ((ceos->processor==ASP || ceos->processor==SPS ||
       ceos->processor==PREC)) {
    meta->sar->time_shift =
      meta->sar->azimuth_time_per_pixel * asf_facdr->alines;
    meta->sar->azimuth_time_per_pixel *= -1.0;
  }
  meta->sar->slant_range_first_pixel = asf_facdr->sltrngfp * 1000.0;
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_dopcen[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_dopcen[2];
  meta->sar->earth_radius = asf_facdr->eradcntr*1000.0;
  meta->sar->satellite_height =
    meta->sar->earth_radius + asf_facdr->scalt*1000;

  // Get platform orientation
  meta->sar->pitch = asf_facdr->scpitch;
  meta->sar->roll = asf_facdr->scroll;
  meta->sar->yaw = asf_facdr->scyaw;

  // State vector block
  ceos_init_stVec(in_fName, ceos, meta);

  // Initialize ScanSAR data
  if (ceos->product == SCANSAR)
    ceos_init_scansar(in_fName, meta, dssr, mpdr, asf_facdr);

  // Initialize map projection for projected images
  if (meta->sar->image_type=='P' && mpdr)
    ceos_init_proj(meta, dssr, mpdr, NULL, NULL);

  // Location block
  if (!meta->location && ceos->product != RAW)
    meta->location = meta_location_init();
  meta->location->lat_start_near_range = asf_facdr->nearslat;
  meta->location->lon_start_near_range = asf_facdr->nearslon;
  meta->location->lat_start_far_range = asf_facdr->farslat;
  meta->location->lon_start_far_range = asf_facdr->farslon;
  meta->location->lat_end_near_range = asf_facdr->nearelat;
  meta->location->lon_end_near_range = asf_facdr->nearelon;
  meta->location->lat_end_far_range = asf_facdr->farelat;
  meta->location->lon_end_far_range = asf_facdr->farelon;

  // Clean up
  FREE(asf_facdr);
  if (mpdr)
    FREE(mpdr);
}

void ceos_init_sar_focus(ceos_description *ceos, const char *in_fName,
       meta_parameters *meta)
{
  struct IOF_VFDR *iof=NULL;
  struct dataset_sum_rec *dssr=NULL;
  struct radio_comp_data_rec *rcdr=NULL;
  struct proc_parm_rec *ppr=NULL;
  struct VMPDREC *mpdr=NULL;
  struct ESA_FACDR *esa_facdr=NULL;
  char beamname[32], buf[50], **dataName;
  double re, rp, tan_lat;
  ymd_date date;
  hms_time time;
  double firstTime, centerTime;
  int nBands;

  dssr = &ceos->dssr;
  iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
  get_ifiledr(in_fName, iof);
  ppr = (struct proc_parm_rec*) MALLOC(sizeof(struct proc_parm_rec));
  memset(ppr,0,sizeof(*ppr)); /* zero out ppr, to avoid uninitialized data */
  if (get_ppr(in_fName, ppr) < 0) {
      FREE(ppr);
      ppr = NULL;
  }
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  if (get_esa_facdr(in_fName, esa_facdr) < 0) {
    if (esa_facdr) FREE(esa_facdr);
    esa_facdr=NULL;
  }

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  // Azimuth time per pixel needs to be known for state vector propagation
  require_ceos_data(in_fName, &dataName, &nBands);
  firstTime = get_firstTime(dataName[0]);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel =
          (centerTime - firstTime) / (meta->sar->original_line_count/2);
  if (meta->sar->azimuth_time_per_pixel < -0.07 ||
      meta->sar->azimuth_time_per_pixel > 0.07)
  {
      // Funky azimuth_time_per_pixel ...here's a parachute
      double er=0.0;
      double ht=0.0;
      switch (ceos->product) {
          case SCANSAR:
          case SCN:
          case SSG:
              meta->sar->image_type = 'P';
              break;
          case SLC:
          case RAW:
              meta->sar->image_type = 'S';
              break;
          case SGF:
          case SGX:
          case PRI:
              meta->sar->image_type = 'G';
              break;
          default:
              break; // Leave at original default setting
      }
      if (ceos->product == SSG) {
          struct VMPDREC *mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
          if (get_mpdr(in_fName, mpdr) >= 0) {
              ht = mpdr->distplat;
              er = mpdr->distplat - mpdr->altplat;
          }
          FREE(mpdr);
      }
      else {
          er = meta_get_earth_radius(meta,
                                     meta->general->line_count/2,
                                     meta->general->sample_count/2);
          ht = meta_get_sat_height(meta,
                                   meta->general->line_count/2,
                                   meta->general->sample_count/2);
      }
      double g = 9.81; //9.80665;
      double swath_vel = sqrt(g*er*er/ht)*er/ht; // orbit_vel*er/ht
      meta->sar->azimuth_time_per_pixel = meta->general->y_pixel_size / swath_vel;
  }

  // State vector block
  if (ceos->product != SSG)
    ceos_init_stVec(in_fName,ceos,meta);

  meta->general->frame =
    asf_frame_calc("ERS", dssr->pro_lat, meta->general->orbit_direction);
  if (meta->general->orbit_direction==' ')
  {
    meta->general->orbit_direction =
      (meta->general->frame>=1791 && meta->general->frame<=5391) ? 'D' : 'A';
  }
  strcpy(beamname, MAGIC_UNSET_STRING);

  if (strncmp(dssr->mission_id, "ERS1", 4) == 0) {
    strcpy(meta->general->sensor,"ERS1");
    strcpy(meta->general->mode, "STD");
    meta->general->frame =
      asf_frame_calc(meta->general->sensor, meta->general->center_latitude,
                     meta->general->orbit_direction);
    meta->sar->look_count = 5;
    sprintf(meta->sar->polarization, "VV");
  }
  else if (strncmp(dssr->mission_id, "ERS2", 4) == 0) {
    strcpy(meta->general->sensor,"ERS2");
    strcpy(meta->general->mode, "STD");
    meta->general->frame =
      asf_frame_calc(meta->general->sensor, meta->general->center_latitude,
                     meta->general->orbit_direction);
    meta->sar->look_count = 5;
    sprintf(meta->sar->polarization, "VV");
  }
  else if (strncmp(dssr->mission_id, "JERS1", 5) == 0 ||
           strncmp(dssr->mission_id, "JERS-1", 6) == 0)
  {
    strcpy(meta->general->sensor,"JERS1");
    strcpy(meta->general->mode, "STD");
    meta->general->frame =
      asf_frame_calc(meta->general->sensor, meta->general->center_latitude,
                     meta->general->orbit_direction);
    meta->sar->look_count = 3;
    sprintf(meta->sar->polarization, "HH");
  }
  else if (ceos->satellite == RSAT) {
    if (dssr->rng_samp_rate < 20.0) { /* split finebeam from the rest */
      meta->sar->look_count = 4; /* ST1-ST7, WD1-WD3, EL1, EH1-EH6 */
    }
    else {
      meta->sar->look_count = 1; /* FN1-FN5 */
    }
    if (ceos->product == SSG) {
        if (dssr->n_azilok <= 0) {
            meta->sar->look_count = 1;
        }
        else {
            meta->sar->look_count = dssr->n_azilok;
        }
    }
    else if (ceos->product == SCANSAR || ceos->product == SCN) {
      rcdr = (struct radio_comp_data_rec *) MALLOC(sizeof(struct radio_comp_data_rec));
      get_rcdr(in_fName, rcdr);
      if (rcdr->num_rec == 2) {
        strcpy(beamname, "SNA");
      }
      else if (rcdr->num_rec == 3) {
        strcpy(beamname, "SNB");
      }
      else if (rcdr->num_rec == 4) {
        // We assume a nominal center look angle of 40.45 degrees for SWA
        if (rcdr->look_angle[3] > 40.25 && rcdr->look_angle[3] < 40.65) {
          strcpy(beamname, "SWA");
        }
        // We assume a nominal center look angle of 38.2 degrees for SWB
        else if (rcdr->look_angle[3] > 38.0 && rcdr->look_angle[3] < 38.4) {
          strcpy(beamname, "SWB");
        }
      }
      meta->sar->image_type = 'P';
      mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
      if (get_mpdr(in_fName, mpdr) < 0) {
        if (mpdr) FREE(mpdr); {
          mpdr = NULL;
        }
      }
      ceos_init_scansar(in_fName, meta, dssr, mpdr, NULL);
    }
    else if (ppr) {
      beam_type_to_asf_beamname(ppr->beam_info[0].beam_type,
                                sizeof(ppr->beam_info[0].beam_type),
                                beamname,
                                sizeof(beamname));
    }
    strcpy(meta->general->mode, beamname);
    sprintf(meta->sar->polarization, "HH");
    strncpy(buf, &dssr->product_id[7], 3);
    buf[3]=0;
    sscanf(buf, "%d", &meta->general->frame);
  }
  if (esa_facdr) {
    meta->general->bit_error_rate = esa_facdr->ber;
  }
  else {
    meta->general->bit_error_rate = 0.0;
  }

  // SAR block
  if (meta->sar->prf <= 0) {
      // Invalid prf ...so here's a parachute
      if (meta->sar->azimuth_time_per_pixel > -0.07 &&
          meta->sar->azimuth_time_per_pixel < 0.07)
      {
          meta->sar->prf = 1.0 / meta->sar->azimuth_time_per_pixel;
      }
  }
  if (ceos->product == SLC || ceos->product == RAW) {
    meta->sar->image_type = 'S';
    meta->sar->look_count = 1;
  }
  else if (ceos->product == SGF || ceos->product == SGX ||
       ceos->product == PRI) {
    meta->sar->image_type = 'G';
    meta->sar->multilook = 1;
  }
  else if (ceos->product == SSG || ceos->product == GEC) {
    meta->sar->multilook = 1;
    meta->sar->image_type = 'P';
    mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
    get_mpdr(in_fName, mpdr);
    ceos_init_proj(meta, dssr, mpdr, NULL, NULL);
    meta->sar->satellite_height = mpdr->distplat;
    meta->sar->earth_radius = mpdr->distplat - mpdr->altplat;
  }
  if (ceos->product == RAW) {
    meta->sar->deskewed = 0;
    meta->sar->multilook = 0;
  }
  else {
    meta->sar->deskewed = 1;
  }
  meta->sar->original_sample_count = iof->datgroup;
  meta->sar->slant_range_first_pixel = dssr->rng_gate
    * get_units(dssr->rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
  meta->sar->slant_shift = 0;
  if (meta->general->orbit_direction == 'D') {
    meta->sar->time_shift = 0;
  }
  else if (meta->general->orbit_direction == 'A') {
    meta->sar->time_shift = fabs(meta->sar->azimuth_time_per_pixel *
        (float)meta->sar->original_line_count);
  }
  else {
    asfPrintError("Invalid or missing orbit direction in metadata: orbit_direction = '%c'\n",
        meta->general->orbit_direction);
  }
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
            meta->general->line_count/2,
            meta->general->sample_count/2);
  re = meta->general->re_major;
  rp = meta->general->re_minor;
  tan_lat = dssr->plat_lat*D2R;
  meta->sar->earth_radius = rp*sqrt(1 + tan_lat*tan_lat) /
    sqrt(rp*rp/(re*re) + tan_lat*tan_lat);
  if (ppr && ppr->eph_orb_data[0] > 0.0)
    meta->sar->satellite_height = ppr->eph_orb_data[0];
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_dopcen[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_dopcen[2];
  meta->sar->azimuth_doppler_coefficients[0] = dssr->alt_dopcen[0];
  meta->sar->azimuth_doppler_coefficients[1] = dssr->alt_dopcen[1];
  meta->sar->azimuth_doppler_coefficients[2] = dssr->alt_dopcen[2];

  // Check to see if we need special startX / startY initialization
  if (!mpdr && !meta->transform && meta->projection) {
    if (!meta_is_valid_double(meta->projection->startX) ||
        !meta_is_valid_double(meta->projection->startY))
    {
      meta_sar_to_startXY(meta, &meta->projection->startX, &meta->projection->startY);
    }
  }

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);

  // Clean up
  if (rcdr)
    FREE(rcdr);
  if (mpdr)
    FREE(mpdr);
  FREE(ppr);
  FREE(iof);
  if (esa_facdr) FREE(esa_facdr);
}

void ceos_init_sar_esa(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct ESA_FACDR *esa_facdr=NULL;
  ymd_date date;
  hms_time time;
  char buf[50];
  double firstTime, centerTime;

  dssr = &ceos->dssr;
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  int ret = get_esa_facdr(in_fName, esa_facdr);
  if (ret < 0) {
      FREE(esa_facdr);
      esa_facdr = NULL;
  }

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS", 3) == 0) {
    strcpy(meta->general->mode, "STD");
    if (ceos->product == RAW)
      strcpy(meta->general->bands, "VV");
    meta->sar->look_count = 5;
    strcpy(meta->sar->polarization, "VV");
  }
  strncpy(buf, &dssr->scene_des[18], 5);
  buf[4] = 0;
  meta->general->frame = atoi(buf);
  if (meta->general->orbit_direction==' ')
    meta->general->orbit_direction =
      (meta->general->frame>=1791 && meta->general->frame<=5391) ? 'D' : 'A';
  if (esa_facdr) {
    meta->general->bit_error_rate = (esa_facdr->ber < 0) ? 0 :
                                    (esa_facdr->ber > 1) ? 1 : esa_facdr->ber;
  }
  else {
      meta->general->bit_error_rate = 0;
  }

  // State vector block
  ceos_init_stVec(in_fName,ceos,meta);

  // SAR block
  if (ceos->product == RAW || ceos->product == SLC)
    meta->sar->image_type = 'S';
  if (ceos->product == PRI)
    meta->sar->image_type = 'G';
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_time[0]*speedOfLight/2000.0;
  date_dssr2time(dssr->az_time_first, &date, &time);
  firstTime = date_hms2sec(&time);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel = (centerTime - firstTime)
    / (meta->sar->original_line_count/2);
  if (meta->general->orbit_direction == 'D') {
      meta->sar->time_shift = 0;
  }
  else if (meta->general->orbit_direction == 'A') {
      meta->sar->time_shift = fabs(meta->sar->azimuth_time_per_pixel *
              (float)meta->sar->original_line_count);
  }
  else {
      asfPrintError("Invalid or missing orbit direction in metadata: orbit_direction = '%c'\n",
                    meta->general->orbit_direction);
  }
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
        meta->general->line_count/2,
        meta->general->sample_count/2);
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
      meta->general->line_count/2,
      meta->general->sample_count/2);
  /*
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = // two-way range time
    dssr->crt_dopcen[1] / (speedOfLight * 2);
  meta->sar->range_doppler_coefficients[2] = // two-way range time
    dssr->crt_dopcen[2] / (speedOfLight * speedOfLight * 4);
  */
  // FIXME
  meta->sar->range_doppler_coefficients[0] = 0.0;
  meta->sar->range_doppler_coefficients[1] = 0.0;
  meta->sar->range_doppler_coefficients[2] = 0.0;
  meta->sar->azimuth_doppler_coefficients[0] = 0.0;
  meta->sar->azimuth_doppler_coefficients[1] = 0.0;
  meta->sar->azimuth_doppler_coefficients[2] = 0.0;

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);
}

static double calc_swath_velocity(struct dataset_sum_rec *dssr,
                                  const char *in_fName, meta_parameters *meta)
{
  // calculate earth radius at nadir (not at scene center)
  double re = (meta_is_valid_double(meta->general->re_major))
          ? meta->general->re_major : 6378137.0;
  double rp = (meta_is_valid_double(meta->general->re_minor))
          ? meta->general->re_minor : 6356752.31414;
  double lat = D2R*dssr->plat_lat;
  double er = (re*rp)
          / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));

  // find the state vector closest to the acquisition time
  ymd_date date;
  hms_time time;
  date_dssr2date(dssr->inp_sctim, &date, &time);
  double centerTime = date_hms2sec(&time);
  struct pos_data_rec ppdr;
  get_ppdr(in_fName,&ppdr);
  int i,closest=0;
  double closest_diff=9999999;
  for (i=0; i<ppdr.ndata; ++i) {
      double diff = fabs(ppdr.gmt_sec + i*ppdr.data_int - centerTime);
      if (diff < closest_diff) {
          closest = i; closest_diff = diff;
      }
  }

  // compute satellite height from closest state vector
  double ht = sqrt(ppdr.pos_vec[closest][0] * ppdr.pos_vec[closest][0] +
      ppdr.pos_vec[closest][1] * ppdr.pos_vec[closest][1] +
      ppdr.pos_vec[closest][2] * ppdr.pos_vec[closest][2]);

  // velocity calculation
  const double g = 9.81; //9.80665;
  double orbit_vel = sqrt(g*er*er/ht);

  // simple scaling to get swath velocity from orbit vel
  // printf("calculated swath velocity: %f\n", orbit_vel * er/ht);
  return orbit_vel * er / ht;
}

static void get_alos_linehdr(struct PHEADER *linehdr, const char *fName)
{
    int length;
    char buff[25600];
    struct HEADER hdr;

    FILE *fp = FOPEN(fName, "rb");
    FREAD(&hdr, sizeof(struct HEADER), 1, fp);
    length = bigInt32(hdr.recsiz)-12;
    FREAD(buff, length, 1, fp);
    FREAD(&hdr, sizeof(struct HEADER), 1, fp);
    FREAD(linehdr, sizeof(struct PHEADER), 1, fp);
    fclose(fp);
}

// Only deal with ALOS data
void ceos_init_sar_eoc(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct VMPDREC *mpdr=NULL;
  char buf[50], basename[512];
  char **dataName=NULL, **metaName=NULL;
  int ii, nBands, trailer;

  dssr = &ceos->dssr;
  mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
  if (get_mpdr(in_fName, mpdr) == -1) {
    FREE(mpdr);
    mpdr = NULL;
  }

  get_ceos_names(in_fName, basename, &dataName, &metaName, &nBands, &trailer);
  //require_ceos_pair(in_fName, &dataName, &metaName, &nBands, &trailer);

  // General block
  strcpy(meta->general->processor, "JAXA");
  ceos_init_sar_general(ceos, in_fName, meta);
  strcpy(meta->general->sensor,"ALOS");
  strcpy(meta->general->mode, alos_beam_mode[ceos->dssr.ant_beam_num]);
  strncpy(buf, &dssr->product_id[11], 4);
  buf[4]=0;
  sscanf(buf, "%d", &meta->general->frame);
  if (dssr->time_dir_lin[0] == 'A')
    meta->general->orbit_direction = 'A';
  else if (dssr->time_dir_lin[0] == 'D')
    meta->general->orbit_direction = 'D';
  meta->general->bit_error_rate = 0.0;

  meta->general->band_count = nBands;

  if (ceos->product != SGI) {
    // calculate azimuth time per pixel from the swath velocity
    double swath_vel = calc_swath_velocity(dssr,in_fName,meta);

    if (meta->general->y_pixel_size != 0) {
        meta->sar->azimuth_time_per_pixel =
            meta->general->y_pixel_size / swath_vel;
    }
    ceos_init_stVec(in_fName,ceos,meta);
  }
  if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);

  // For geocoded ALOS data, the doppler centroid fields are all zero.
  int is_geocoded = dssr->crt_dopcen[0] == 0.0 &&
      dssr->crt_dopcen[1] == 0.0 &&
      dssr->crt_dopcen[2] == 0.0;

  // sanity check on the geocode test -- check the filename
  if (!is_geocoded && strncmp(metaName[0], "LED-", 4) == 0 &&
      metaName[0][strlen(metaName[0])-3] == 'G') {
        asfPrintWarning(
          "The filename suggests that this data is geocoded, however the\n"
          "doppler centroid values are non-zero.  For ALOS data, the\n"
          "values for the doppler centroid should all be zero:\n"
          "  %f, %f, %f if it is actually geocoded (per spec).\n"
          "Proceeding, but marking the data as georeferenced only.\n",
          dssr->crt_dopcen[0], dssr->crt_dopcen[1], dssr->crt_dopcen[2]);
  }
  if (is_geocoded && strncmp(metaName[0], "LED-", 4) == 0 &&
      metaName[0][strlen(metaName[0])-3] == '_') {
        asfPrintWarning(
          "The filename suggests that this data is non-geocoded, however the\n"
          "doppler centroid values are all zero.  For ALOS data, the\n"
          "values for the doppler centroid should all be zero only if the\n"
          "data is geocoded.  Centroid values read are:\n"
          "  %f, %f, %f\n"
          "Proceeding, but marking the data as georeferenced only.\n",
          dssr->crt_dopcen[0], dssr->crt_dopcen[1], dssr->crt_dopcen[2]);
        is_geocoded = FALSE;
  }

  // SAR block
  if (ceos->product == SLC)
    meta->sar->image_type = 'S';
  else if (is_geocoded)
    meta->sar->image_type = 'P';
  else
    meta->sar->image_type = 'R';
  meta->sar->look_count = dssr->n_azilok;
  if (ceos->product == SLC)
    meta->sar->deskewed = 0;
  else
    meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_gate
    * get_units(dssr->rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
  meta->sar->slant_shift = 0;
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_dopcen[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_dopcen[2];
  if (mpdr) {
    meta->sar->satellite_height = mpdr->distplat + mpdr->altplat;
  }
  else {
    meta->sar->satellite_height =
        meta_get_sat_height(meta,
                            meta->general->line_count/2,
                            meta->general->sample_count/2);
  }
  if (ceos->product == SLC) {
    meta->sar->earth_radius =
      meta_get_earth_radius(meta,
          meta->general->line_count/2,
          meta->general->sample_count/2);
    meta->sar->satellite_height =
      meta_get_sat_height(meta,
        meta->general->line_count/2,
        meta->general->sample_count/2);
  }
  else {
    meta->sar->earth_radius = mpdr->distplat;
    meta->sar->chirp_rate = get_chirp_rate(dataName[0]);
  }
  for (ii=0; ii<6; ++ii)
    meta->sar->incid_a[ii] = dssr->incid_a[ii];

  if (ceos->product == SLC) {

    // Fix the azimuth time per pixel, if we have the workreport file
    // We seem to get better estimates of that value from workreport.
    asfPrintStatus("\nAzimuth Time Per Pixel Calculation:\n");
    double delta,workreport_atpp=-1;
    if (get_alos_delta_time(basename, &delta)) {
        workreport_atpp = delta / meta->sar->original_line_count;
        asfPrintStatus("From workreport: %.10f\n", workreport_atpp);
        asfPrintStatus("     Calculated: %.10f\n",
                       meta->sar->azimuth_time_per_pixel);
        asfPrintStatus("Using workreport value.\n\n");
        meta->sar->azimuth_time_per_pixel = workreport_atpp;
    }
    else {
      asfPrintStatus("From Workreport: (not available)\n");
      asfPrintStatus("Calculated: %.10f\n\n",
                     meta->sar->azimuth_time_per_pixel);
    }

    // fix x_pixel_size & y_pixel_size values if needed
    if (meta->general->x_pixel_size == 0) {
        meta->general->x_pixel_size =
            speedOfLight * meta->sar->range_time_per_pixel / 2.;
        printf("Calculated x pixel size: %f\n", meta->general->x_pixel_size);
    }

    if (meta->general->y_pixel_size == 0) {
        double lat, lon, x1, y1, x2, y2;
        meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
        latLon2UTM(lat, lon, 0, &x1, &y1);

        int nl = meta->general->line_count;
        meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
        latLon2UTM(lat, lon, 0, &x2, &y2);

        meta->general->y_pixel_size = hypot(x1-x2, y1-y2)/(double)nl;
        printf("Calculated y pixel size: %f\n", meta->general->y_pixel_size);
        
        double swath_vel = calc_swath_velocity(dssr,dataName[0],meta);
        printf("     Other method gives: %f\n",
            meta->sar->azimuth_time_per_pixel * swath_vel);

        if (meta->general->y_pixel_size == 0) {
            // use value from other method...
            meta->general->y_pixel_size =
                meta->sar->azimuth_time_per_pixel * swath_vel;
            
            if (meta->general->y_pixel_size == 0) {
                // can't figure out the azimuth pixel size...
                asfPrintWarning("Failed to determine azimuth pixel size.\n");
            }
        }
    }
  }

  if (ceos->product == SGI) {

    // calculate azimuth time per pixel from the swath velocity
    double swath_vel = calc_swath_velocity(dssr,in_fName,meta);

    meta->sar->azimuth_time_per_pixel =
        meta->general->y_pixel_size / swath_vel;
    if (meta->general->orbit_direction == 'D')
      meta->sar->time_shift = 0.0;
    else if (meta->general->orbit_direction == 'A')
      meta->sar->time_shift = fabs(meta->sar->original_line_count *
          meta->sar->azimuth_time_per_pixel);

    // for comparison, calculate using the workreport file (old method)
    // -- taking out this for now, it seems the swath velocity calculation
    //    is working out ok...
    //double delta; 
    double workreport_atpp=-1;
    //if (get_alos_delta_time (in_fName, &delta))
    //    workreport_atpp = delta / meta->sar->original_line_count;

    asfPrintStatus("\nAzimuth Time Per Pixel Calculation:\n");
    if (workreport_atpp > 0)
        asfPrintStatus("  Using workreport: %.10f\n", workreport_atpp);
    asfPrintStatus("        Calculated: %.10f\n\n", meta->sar->azimuth_time_per_pixel);

    ceos_init_stVec(in_fName,ceos,meta);
  }

  // Transformation block
  if (ceos->product != SLC) {
    struct trl_file_des_rec tfdr;
    get_tfdr((char*)in_fName, &tfdr);
    struct JAXA_FACDR facdr;
    get_jaxa_facdr(in_fName, &facdr);

    if (!meta->transform)
      meta->transform = meta_transform_init();
    if (tfdr.facdr_len[10] < 5000) {
      meta->transform->parameter_count = 4;
      meta->transform->x[0] = mpdr->a11;
      meta->transform->x[1] = mpdr->a12;
      meta->transform->x[2] = mpdr->a13;
      meta->transform->x[3] = mpdr->a14;
      meta->transform->y[0] = mpdr->a21;
      meta->transform->y[1] = mpdr->a22;
      meta->transform->y[2] = mpdr->a23;
      meta->transform->y[3] = mpdr->a24;
      meta->transform->l[0] = mpdr->b11;
      meta->transform->l[1] = mpdr->b12;
      meta->transform->l[2] = mpdr->b13;
      meta->transform->l[3] = mpdr->b14;
      meta->transform->s[0] = mpdr->b21;
      meta->transform->s[1] = mpdr->b22;
      meta->transform->s[2] = mpdr->b23;
      meta->transform->s[3] = mpdr->b24;
    }
    else {
      meta->transform->parameter_count = 25;
      for (ii=0; ii<25; ii++) {
    meta->transform->x[ii] = facdr.a[ii];
    meta->transform->y[ii] = facdr.b[ii];
    meta->transform->l[ii] = facdr.c[ii];
    meta->transform->s[ii] = facdr.d[ii];
      }
      meta->transform->origin_lat = facdr.origin_lat;
      meta->transform->origin_lon = facdr.origin_lon;
      meta->transform->origin_pixel = facdr.origin_pixel;
      meta->transform->origin_line = facdr.origin_line;
    }

    for (ii=0; ii<10; ++ii) {
      meta->transform->map2ls_a[ii] = facdr.a_map[ii];
      meta->transform->map2ls_b[ii] = facdr.b_map[ii];
    }
  }
  else {
    // SLC images -- no transform block
    // See if we can get better geolocations using the workreport
    double time_shift, slant_shift;
    asfPrintStatus("Refining geolocation estimates using workreport...\n");
    if (refine_slc_geolocation_from_workreport(metaName[0], basename, meta,
                                               &time_shift, &slant_shift))
    {
          asfPrintStatus("SLC Geolocation refinement results:\n");
          asfPrintStatus("  Time Shift: %f -> %f\n"
                         "  Slant Shift: %f -> %f\n",
                         meta->sar->time_shift,
                         meta->sar->time_shift + time_shift,
                         meta->sar->slant_shift,
                         meta->sar->slant_shift + slant_shift);
          // Figure out how much shift we had to do
          double lat, lon, px1, py1, px2, py2;
          meta_get_latLon(meta, meta->general->line_count/2.,
                          meta->general->sample_count/2., 0, &lat, &lon);
          asfPrintStatus("Before center lat/lon: %f,%f\n", lat, lon);
          latLon2UTM(lat, lon, 0, &px1, &py1);
          int zone = utm_zone(lon);
          meta->sar->time_shift += time_shift;
          meta->sar->slant_shift += slant_shift;
          meta_get_latLon(meta, meta->general->line_count/2.,
                          meta->general->sample_count/2., 0, &lat, &lon);
          asfPrintStatus("After center lat/lon: %f,%f\n", lat, lon);
          latLon2UTM_zone(lat, lon, 0, zone, &px2, &py2);
          double dx = px2-px1;
          double dy = py2-py1;
          if (fabs(dx)<2000 && fabs(dy)<2000) {
            asfPrintStatus("  E-W Shift: %.1fm\n", dx);
            asfPrintStatus("  N-S Shift: %.1fm\n", dy);
            asfPrintStatus("  Total    : %.1fm\n", hypot(dx,dy));
          }
          else {
            asfPrintStatus("  E-W Shift: %.2fkm\n", dx/1000.);
            asfPrintStatus("  N-S Shift: %.2fkm\n", dy/1000.);
            asfPrintStatus("  Total    : %.2fkm\n", hypot(dx,dy)/1000.);
          }
    }
  }

  // Initialize map projection for projected images
  if (meta->sar->image_type=='P' && mpdr)
    ceos_init_proj(meta, dssr, mpdr, NULL, NULL);

  // Location block
  if (!meta->location)
    meta->location = meta_location_init();
  if (ceos->product != SLC) {
    if (meta->general->orbit_direction == 'A') {
      meta->location->lat_start_near_range = mpdr->blclat;
      meta->location->lon_start_near_range = mpdr->blclong;
      meta->location->lat_start_far_range = mpdr->brclat;
      meta->location->lon_start_far_range = mpdr->brclong;
      meta->location->lat_end_near_range = mpdr->tlclat;
      meta->location->lon_end_near_range = mpdr->tlclong;
      meta->location->lat_end_far_range = mpdr->trclat;
      meta->location->lon_end_far_range = mpdr->trclong;
    }
    else {
      meta->location->lat_start_near_range = mpdr->trclat;
      meta->location->lon_start_near_range = mpdr->trclong;
      meta->location->lat_start_far_range = mpdr->tlclat;
      meta->location->lon_start_far_range = mpdr->tlclong;
      meta->location->lat_end_near_range = mpdr->brclat;
      meta->location->lon_end_near_range = mpdr->brclong;
      meta->location->lat_end_far_range = mpdr->blclat;
      meta->location->lon_end_far_range = mpdr->blclong;
    }
  }
  else {
    double min_lat;
    double max_lat;
    double min_lon;
    double max_lon;
    meta_get_corner_coords(meta);
    // If the scene center lat/long is not valid (usually because of
    // 0's in the dssr) then calculate it from the center line/sample
    min_lat = meta->location->lat_start_near_range;
    if (min_lat > meta->location->lat_start_far_range)
      min_lat = meta->location->lat_start_far_range;
    if (min_lat > meta->location->lat_end_near_range)
      min_lat = meta->location->lat_end_near_range;
    if (min_lat > meta->location->lat_end_far_range)
      min_lat = meta->location->lat_end_far_range;
    max_lat = meta->location->lat_start_near_range;
    if (max_lat < meta->location->lat_start_far_range)
      max_lat = meta->location->lat_start_far_range;
    if (max_lat < meta->location->lat_end_near_range)
      max_lat = meta->location->lat_end_near_range;
    if (max_lat < meta->location->lat_end_far_range)
      max_lat = meta->location->lat_end_far_range;
    min_lon = meta->location->lon_start_near_range;
    if (min_lon > meta->location->lon_start_far_range)
      min_lon = meta->location->lon_start_far_range;
    if (min_lon > meta->location->lon_end_near_range)
      min_lon = meta->location->lon_end_near_range;
    if (min_lon > meta->location->lon_end_far_range)
      min_lon = meta->location->lon_end_far_range;
    max_lon = meta->location->lon_start_near_range;
    if (max_lon < meta->location->lon_start_far_range)
      max_lon = meta->location->lon_start_far_range;
    if (max_lon < meta->location->lon_end_near_range)
      max_lon = meta->location->lon_end_near_range;
    if (max_lon < meta->location->lon_end_far_range)
      max_lon = meta->location->lon_end_far_range;
    if (meta->general->center_latitude < min_lat   ||
        meta->general->center_latitude > max_lat   ||
        meta->general->center_longitude < min_lon ||
        meta->general->center_longitude > max_lon)
    {
      asfPrintStatus("\nApproximate scene center latitude and longitude\n"
          "(%0.2f, %0.2f) appear outside the bounds of the image\n"
          "...Recalculating from center line and sample instead.\n\n",
          meta->general->center_latitude, meta->general->center_longitude);
      meta_get_latLon(meta, meta->general->line_count/2,
                      meta->general->sample_count/2, 0,
                      &(meta->general->center_latitude),
                      &(meta->general->center_longitude));
    }
  }

  set_alos_look_count(meta, metaName[0]);

  // Clean up
  FREE(mpdr);
  free_ceos_names(dataName, metaName);
}


// Only deals with Radarsat data
void ceos_init_sar_rsi(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct VMPDREC *mpdr=NULL;
  struct radio_comp_data_rec *rcdr=NULL;
  struct proc_parm_rec *ppr=NULL;
  char beamname[32], beamtype[32], buf[50];
  char **dataName=NULL, **metaName=NULL;
  ymd_date date;
  hms_time time;
  double firstTime, centerTime;
  int nBands, trailer;

  strcpy(beamname,"");
  strcpy(beamtype,"");
  dssr = &ceos->dssr;
  ppr = (struct proc_parm_rec*) MALLOC(sizeof(struct proc_parm_rec));
  memset(ppr,0,sizeof(*ppr)); /* zero out ppr, to avoid uninitialized data */
  get_ppr(in_fName, ppr);
  require_ceos_pair(in_fName, &dataName, &metaName, &nBands, &trailer);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  // Azimuth time per pixel need to be known for state vector propagation
  firstTime = get_firstTime(dataName[0]);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel =
    (centerTime - firstTime) / (meta->sar->original_line_count/2);

  // State vector block
  ceos_init_stVec(in_fName, ceos, meta);

  if (ceos->product == SCANSAR || ceos->product == SCN) {
    meta->sar->image_type = 'P';
    ceos_init_scansar(in_fName, meta, dssr, NULL, NULL);
    rcdr = (struct radio_comp_data_rec *)
      MALLOC(sizeof(struct radio_comp_data_rec));
    get_rcdr(in_fName, rcdr);
    if (rcdr->num_rec == 2)
      strcpy(beamname, "SNA");
    else if (rcdr->num_rec == 3)
      strcpy(beamname, "SNB");
    else if (rcdr->num_rec == 4) {
      strcpy(beamtype,rcdr->beam_type[3]);
      strtok(beamtype," ");
      if (strcmp(beamtype," S7")==0)
        strcpy(beamname, "SWA");
      else if (strcmp(beamtype," S6")==0)
        strcpy(beamname, "SWB");
    }
    strcpy(meta->general->mode, beamname);
  }
  else if (ppr) {
    beam_type_to_asf_beamname(ppr->beam_info[0].beam_type,
                              sizeof(ppr->beam_info[0].beam_type),
                              beamname, sizeof(beamname));
  }
  strcpy(meta->general->mode, beamname);
  sprintf(meta->sar->polarization, "HH");
  strncpy(buf, &dssr->product_id[7], 4);
  buf[3] = 0;
  if (!isdigit((int)buf[0])) {
    if (ceos->facility==RSI) {
      meta->general->frame = asf_frame_calc("ERS", dssr->pro_lat, meta->general->orbit_direction);
    }
    else {
      // The Scene Descriptor Record is not the usual R1ooooofff-- type of
      // descriptor ("fff" being the frame number) ...probably this is MDA (CDPF, RSI, etc)
      // type of data
      asfPrintWarning("Cannot determine frame number from leader data - Might be\n"
          "MDA (CDPF, RSI etc) type of data.  Frame number defaulting to zero (0).\n");
    }
  }
  else {
    meta->general->frame = atoi(buf);
  }
  if (meta->general->line_count == 0 || meta->general->sample_count == 0) {
    meta->general->line_count   = dssr->sc_lin*2;
    meta->general->sample_count = dssr->sc_pix*2;
  }
  meta->general->bit_error_rate = 0;

  // SAR block
  if (ceos->product == RAW) {
    meta->sar->image_type = 'S';
    meta->sar->look_count = 1;
  }
  if (ceos->product == SGF || ceos->product == SGX) {
    meta->sar->image_type = 'G';
    meta->sar->look_count = 1;
  }
  else if (ceos->product == SCN) {
    meta->sar->image_type = 'P';
    meta->sar->look_count = 4;
  }
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_gate
    * get_units(dssr->rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
  meta->sar->slant_shift = 0;
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_dopcen[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_dopcen[2];
  meta->sar->azimuth_doppler_coefficients[0] = dssr->alt_dopcen[0];
  meta->sar->azimuth_doppler_coefficients[1] = dssr->alt_dopcen[1];
  meta->sar->azimuth_doppler_coefficients[2] = dssr->alt_dopcen[2];
  meta->sar->earth_radius =
      meta_get_earth_radius(meta,
        meta->general->line_count/2,
        meta->general->sample_count/2);

  // Initialize map projection for projected images
  if (meta->sar->image_type=='P' && mpdr) {
    ceos_init_proj(meta, dssr, mpdr, NULL, NULL);
  }

  // Check to see if we need special startX / startY initialization
  if (!mpdr && !meta->transform && meta->projection) {
      if (!meta_is_valid_double(meta->projection->startX) ||
          !meta_is_valid_double(meta->projection->startY))
      {
          meta_sar_to_startXY(meta, &meta->projection->startX, &meta->projection->startY);
      }
  }
  meta->sar->satellite_height =
          meta_get_sat_height(meta,
                              meta->general->line_count/2,
                              meta->general->sample_count/2);

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);

  // Clean up
  if (mpdr)
    FREE(mpdr);
  FREE(ppr);
  free_ceos_names(dataName, metaName);
}

// Only deals with SIR-C data
void ceos_init_sar_jpl(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;

  dssr = &ceos->dssr;

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strcmp(meta->general->sensor, "STS-68")==0)
    sprintf(meta->general->sensor, "SIR-C");
  if (strncmp(dssr->sensor_id,"SIR-C",6)==0) {
    strcpy(meta->general->sensor,"SIR_C");
    strcpy(meta->general->mode, "STD");
    sprintf(meta->sar->polarization, "VV");
  }
  if (strcmp(meta->general->sensor, "SIR-C") == 0)
    meta->general->sample_count += 12;
  meta->general->bit_error_rate = 0.0;

  // SAR block
  // FIXME: sort out the specifics about sar->image_type
  // meta->sar->deskewed = 1;
  if (strcmp(meta->general->sensor, "SIR-C") == 0) {
    double delta = 15;
    meta->sar->azimuth_time_per_pixel =
      delta / meta->sar->original_line_count;
  }
  meta->sar->slant_range_first_pixel = dssr->rng_gate
    * get_units(dssr->rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_dopcen[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_dopcen[2];
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
        meta->general->line_count/2,
        meta->general->sample_count/2);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
      meta->general->line_count/2,
      meta->general->sample_count/2);

  // State vector block
  ceos_init_stVec(in_fName,ceos,meta);

  // Location block
  ceos_init_location_block(meta);
}

void ceos_init_sar_dpaf(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct ESA_FACDR *esa_facdr=NULL;
  char buf[50];

  dssr = &ceos->dssr;
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  get_esa_facdr(in_fName, esa_facdr);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS", 3) == 0) {
    strcpy(meta->general->mode, "STD");
    if (ceos->product == RAW)
      strcpy(meta->general->bands, "VV");
    meta->sar->look_count = 5;
    strcpy(meta->sar->polarization, "VV");
  }
  strncpy(buf, &dssr->scene_des[18], 5);
  buf[4] = 0;
  meta->general->frame = atoi(buf);
  if (meta->general->orbit_direction==' ')
    meta->general->orbit_direction =
      (meta->general->frame>=1791 && meta->general->frame<=5391) ? 'D' : 'A';
  meta->general->bit_error_rate = esa_facdr->ber;

  // State vector block
  ceos_init_stVec(in_fName,ceos,meta);

  // SAR block
  if (ceos->product == RAW || ceos->product == SLC)
    meta->sar->image_type = 'S';
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_time[0]*speedOfLight/2000.0;
  meta->sar->slant_shift = 0;
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = // two-way range time
    dssr->crt_dopcen[1] / (speedOfLight * 2);
  meta->sar->range_doppler_coefficients[2] = // two-way range time
    dssr->crt_dopcen[2] / (speedOfLight * speedOfLight * 4);
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
        meta->general->line_count/2,
        meta->general->sample_count/2);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
      meta->general->line_count/2,
      meta->general->sample_count/2);

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);

  // Clean up
  FREE(esa_facdr);
}

void ceos_init_sar_ipaf(ceos_description *ceos, const char *in_fName,
           meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct ESA_FACDR *esa_facdr=NULL;
  ymd_date date;
  hms_time time;
  char buf[50];

  dssr = &ceos->dssr;
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  get_esa_facdr(in_fName, esa_facdr);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS", 3) == 0) {
    strcpy(meta->general->mode, "STD");
    if (ceos->product == RAW)
      strcpy(meta->general->bands, "VV");
    meta->sar->look_count = 5;
    strcpy(meta->sar->polarization, "VV");
  }
  strncpy(buf, &dssr->scene_des[18], 5);
  buf[4] = 0;
  meta->general->frame = atoi(buf);
  if (meta->general->orbit_direction==' ')
    meta->general->orbit_direction =
      (meta->general->frame>=1791 && meta->general->frame<=5391) ? 'D' : 'A';
  meta->general->bit_error_rate = esa_facdr->ber;

  // State vector block
  ceos_init_stVec(in_fName,ceos,meta);

  // SAR block
  if (ceos->product == RAW || ceos->product == SLC)
    meta->sar->image_type = 'S';
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_time[0]*speedOfLight/2000.0;
  meta->sar->slant_shift = 0;
  date_dssr2time(dssr->az_time_first, &date, &time);
  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = // two-way range time
    dssr->crt_dopcen[1] / (speedOfLight * 2);
  meta->sar->range_doppler_coefficients[2] = // two-way range time
    dssr->crt_dopcen[2] / (speedOfLight * speedOfLight * 4);
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
        meta->general->line_count/2,
        meta->general->sample_count/2);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
      meta->general->line_count/2,
      meta->general->sample_count/2);

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);

  // Clean up
  FREE(esa_facdr);
}

void ceos_init_sar_beijing(ceos_description *ceos, const char *in_fName,
         meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct ESA_FACDR *esa_facdr=NULL;
  ymd_date date;
  hms_time time;
  double firstTime, centerTime;
  char buf[50];

  dssr = &ceos->dssr;
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  get_esa_facdr(in_fName, esa_facdr);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS", 3) == 0) {
    strcpy(meta->general->mode, "STD");
    if (ceos->product == RAW)
      strcpy(meta->general->bands, "VV");
    meta->sar->look_count = 5;
    strcpy(meta->sar->polarization, "VV");
  }
  strncpy(buf, &dssr->scene_des[18], 5);
  buf[4] = 0;
  meta->general->frame = atoi(buf);
  if (meta->general->orbit_direction==' ')
    meta->general->orbit_direction =
      (meta->general->frame>=1791 && meta->general->frame<=5391) ? 'D' : 'A';
  meta->general->bit_error_rate = esa_facdr->ber;

  // SAR block
  if (ceos->product == RAW || ceos->product == SLC)
    meta->sar->image_type = 'S';
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_time[0]*speedOfLight/2000.0;
  meta->sar->slant_shift = 0;
  firstTime = get_firstTime(in_fName);
  date_dssr2time(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel =
    (centerTime - firstTime) / (meta->sar->original_line_count/2);

  get_azimuth_time(ceos, in_fName, meta);

  if (meta->general->orbit_direction == 'D')
    meta->sar->time_shift = 0.0;
  else if (meta->general->orbit_direction == 'A')
    meta->sar->time_shift = fabs(meta->sar->original_line_count *
        meta->sar->azimuth_time_per_pixel);

  ceos_init_stVec(in_fName,ceos,meta);
  /*
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = // two-way range time
    dssr->crt_dopcen[1] / (speedOfLight * 2);
  meta->sar->range_doppler_coefficients[2] = // two-way range time
    dssr->crt_dopcen[2] / (speedOfLight * speedOfLight * 4);
  */
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
                          meta->general->line_count/2,
                          meta->general->sample_count/2);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
                        meta->general->line_count/2,
                        meta->general->sample_count/2);

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);

  // Clean up
  FREE(esa_facdr);
}

/*******************************************************************************
 * ceos_init_optical:
 * Reads structure parameters from CEOS into existing meta_parameters
 * structure.  Calls the facility-specific decoders below. */
void ceos_init_optical(const char *in_fName,meta_parameters *meta)
{
  ymd_date date;
  hms_time time;
  ceos_description *ceos=NULL;
  struct alos_map_proj_rec *ampr=NULL;
  char *substr;
  int ii;

  ceos = get_ceos_description(in_fName, NOREPORT);
  meta->optical = meta_optical_init();

  // General block
  char *basename = get_basename(in_fName);
  strcpy(meta->general->basename, basename);
  if(basename)FREE(basename);
  strcpy(meta->general->sensor, ceos->shr.mission_id);
  strtok(meta->general->sensor, " "); // Remove spaces from field
  if (ceos->sensor == AVNIR)
    sprintf(meta->general->sensor_name,"AVNIR");
  else if (ceos->sensor == PRISM)
    sprintf(meta->general->sensor_name,"PRISM");
  if (ceos->sensor == AVNIR || ceos->sensor == PRISM) {
    sprintf(meta->general->processor, "JAXA");
    ampr = (struct alos_map_proj_rec *) MALLOC(sizeof(struct alos_map_proj_rec));
    if (get_ampr(in_fName, ampr) == -1) {
      FREE(ampr);
      ampr = NULL;
    }
  }
  substr = ceos->shr.product_id;
  substr++;
  if (strncmp(substr,"1A",2)==0)
    strcpy(meta->general->mode, "1A");
  else if (strncmp(substr,"1B1",3)==0)
    strcpy(meta->general->mode, "1B1");
  else if (strncmp(substr,"1B2R",4)==0)
    strcpy(meta->general->mode, "1B2R");
  else if (strncmp(substr,"1B2G",4)==0)
    strcpy(meta->general->mode, "1B2G");
  if (strcmp(meta->general->mode,"1A")==0 ||
      strcmp(meta->general->mode,"1B1")==0) {
    meta->general->center_latitude = ceos->shr.sc_lat;
    meta->general->center_longitude = ceos->shr.sc_lon;
  }
  else if (strncmp(meta->general->mode,"1B2",3)==0) {
    meta->general->center_latitude = ceos->shr.sc_lat2;
    meta->general->center_longitude = ceos->shr.sc_lon2;
    if (strncmp(meta->general->mode,"1B2R",4)==0)
      meta->transform = meta_transform_init();
  }
  // Don't create a projection block for georeferenced images
  if (substr[3] == 'G' && (substr[5] == 'U' || substr[5] == 'P'))
    ceos_init_proj(meta, NULL, NULL, &(ceos->shr), ampr);

  meta->general->data_type = BYTE;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  strcpy(meta->general->system, meta_get_system());
  if (ceos->shr.sc_time[0] == ' ') {
    date_shr2date_stamp(ceos->shr.acq_date, meta->general->acquisition_date);
  } else if (strlen(ceos->dssr.inp_sctim) > 0) {
    if (ceos->facility == BEIJING)
      date_dssr2time(ceos->dssr.inp_sctim, &date, &time);
    else
      date_dssr2date(ceos->dssr.inp_sctim, &date, &time);
    date_dssr2time_stamp(&date, &time, meta->general->acquisition_date);
  } else {
    strcpy(meta->general->acquisition_date, MAGIC_UNSET_STRING);
  }
  meta->general->orbit = ceos->shr.orbit;
  meta->general->orbit_direction = ceos->shr.orbit_dir[0];
  substr = ceos->shr.work_scene_id;
  for (ii=0; ii<11; ii++)
    substr++;
  meta->general->frame = atoi(substr);
  meta->general->band_count = ceos->shr.no_bands;;
  strcpy(meta->general->bands, "");
  meta->general->line_count = ceos->shr.lines;
  meta->general->sample_count = ceos->shr.samples;
  meta->general->start_line = 0;
  meta->general->start_sample = 0;
  if (ampr) {
    if (strcmp(meta->general->mode, "1A")==0 ||
  strcmp(meta->general->mode, "1B1")==0) {
      meta->general->x_pixel_size = ampr->x_pixel_size;
      meta->general->y_pixel_size = ampr->y_pixel_size;
    }
    else if (strncmp(meta->general->mode, "1B2", 3)==0) {
      meta->general->x_pixel_size = ampr->x_pixel_size2;
      meta->general->y_pixel_size = ampr->y_pixel_size2;
    }
    meta->general->re_major = ampr->ref_major_axis;
    meta->general->re_minor = ampr->ref_minor_axis;
  }
  meta->general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  meta->general->missing_lines = MAGIC_UNSET_INT;
  if (ceos->product == LEVEL_1B2G)
    meta->general->no_data = 0;
  else
    meta->general->no_data = MAGIC_UNSET_DOUBLE;

  // Optical block
  substr = ceos->shr.product_id;
  if (ceos->sensor == PRISM) {
    if (substr[7] == 'F')
      sprintf(meta->optical->pointing_direction, "Forward");
    else if (substr[7] == 'B')
      sprintf(meta->optical->pointing_direction, "Backward");
    else // assume nadir direction
      sprintf(meta->optical->pointing_direction, "Nadir");
  }
  substr = ceos->shr.off_nadir_angle;
  meta->optical->off_nadir_angle = atof(substr);
  if (ceos->sensor == AVNIR) {
    if (meta->optical->off_nadir_angle > 0.0)
      sprintf(meta->optical->pointing_direction, "Off-nadir");
    else
      sprintf(meta->optical->pointing_direction, "Nadir");
  }
  if (strcmp(meta->general->mode,"1A")==0)
    strcpy(meta->optical->correction_level,"N");
  else {
    substr = ceos->shr.proc_code;
    for (ii=0; ii<6; ii++)
      substr++;
    substr[3] = '\0';
    strncpy(meta->optical->correction_level, substr, 4);
  }
  substr = ceos->shr.sun_angle;
  for (ii=0; ii<6; ii++)
    substr++;
  substr[3] = '\0';
  meta->optical->sun_elevation_angle = atof(substr);
  for (ii=0; ii<5; ii++)
    substr++;
  substr[3] = '\0';
  meta->optical->sun_azimuth_angle = atof(substr);

  // Location block
  meta->location = meta_location_init();
  meta->location->lat_start_near_range = ceos->shr.lat_ul;
  meta->location->lon_start_near_range = ceos->shr.lon_ul;
  meta->location->lat_start_far_range = ceos->shr.lat_ur;
  meta->location->lon_start_far_range = ceos->shr.lon_ur;
  meta->location->lat_end_near_range = ceos->shr.lat_ll;
  meta->location->lon_end_near_range = ceos->shr.lon_ll;
  meta->location->lat_end_far_range = ceos->shr.lat_lr;
  meta->location->lon_end_far_range = ceos->shr.lon_lr;

  // Projection block
  if (meta->projection) {
    double lat = meta->location->lat_start_near_range;
    double lon = meta->location->lon_start_near_range;
    double projZ;

    // If the image is georeferenced set the upper left corner to unknown
    // This is the safest way to convey that this image has some projection
    // information but only for the corners
    if (strcmp(meta->general->mode, "1B2R")==0) {
      meta->projection->startX = MAGIC_UNSET_DOUBLE;
      meta->projection->startY = MAGIC_UNSET_DOUBLE;
    }
    meta->projection->perX = ampr->x_pixel_size2;
    meta->projection->perY = -ampr->y_pixel_size2;
    strcpy(meta->projection->units, "meters");
    if (ampr->hem == 0)
      meta->projection->hem = 'N';
    else
      meta->projection->hem = 'S';
    if (strncmp(ampr->ref_ellipsoid, "GRS80", 5) == 0)
      meta->projection->spheroid = GRS1980_SPHEROID;
    meta->projection->re_major = ampr->ref_major_axis;
    meta->projection->re_minor = ampr->ref_minor_axis;
    if (strncmp(ampr->geod_coord_name, "ITRF97", 6) == 0)
      meta->projection->datum = ITRF97_DATUM;
    meta->projection->height = 0.0;
    latlon_to_proj(meta->projection, 'R', lat*D2R, lon*D2R, 0.0,
       &meta->projection->startX, &meta->projection->startY,
       &projZ);
  }

  // Map transformation coefficients
  if (meta->transform) {
    meta->transform->parameter_count = 10;
    for (ii=0; ii<meta->transform->parameter_count; ii++) {
      meta->transform->x[ii] = ampr->lambda[ii];
      meta->transform->y[ii] = ampr->phi[ii];
      meta->transform->l[ii] = ampr->j[ii];
      meta->transform->s[ii] = ampr->i[ii];
    }
  }

  FREE(ceos);
}

// ceos_init_scansar
// Non-ASF ScanSAR data do not follow JPL's along-track/cross-track convention
// Have to come up with a new initialization strategy, otherwise we run into
// problems, especially with earth radius calculations
void ceos_init_scansar(const char *leaderName, meta_parameters *meta,
           struct dataset_sum_rec *dssr,
           struct VMPDREC *mpdr, struct VFDRECV *asf_facdr)
{
  meta_projection *projection;

  if (asf_facdr) {
    meta->sar->earth_radius = asf_facdr->eradcntr*1000.0;
    meta->sar->satellite_height = meta->sar->earth_radius + asf_facdr->scalt*1000;
  }

  meta->projection = projection = meta_projection_init();
  projection->type = SCANSAR_PROJECTION;
  projection->param.atct.rlocal =
      meta_get_earth_radius(meta, meta->general->line_count/2, 0);
  atct_init_from_leader(leaderName, projection);

  if (mpdr) { // all ASF ScanSAR have a map projection record
    projection->startY = mpdr->tlceast;
    projection->startX = mpdr->tlcnorth;
    projection->perY   = (mpdr->blceast - mpdr->tlceast) / (mpdr->nlines-1);
    projection->perX   = (mpdr->trcnorth - mpdr->tlcnorth) / (mpdr->npixels-1);
  }
  else { // RSI ScanSAR does not - needs some calulation
    // RSI ScanSAR does not follow JPL's along-track/cross-track scheme
    // Need to calculate a couple of the parameters

    // void ll_ac(meta_projection *proj, char look_dir, double lat_d, double lon,
    // double *c1, double *c2)
    // needs atct initialized
    // center lat/lon at mid-point of first line

    // CSTARS ScanSAR is missing a map projection record, so startX and startY need
    // to be initialized with meta_sar_to_latLon() after the SAR block is populated

    projection->startX = MAGIC_UNSET_DOUBLE;
    projection->startY = MAGIC_UNSET_DOUBLE;
    projection->perX = meta->general->x_pixel_size;
    projection->perY = -meta->general->y_pixel_size;
  }

  strcpy(projection->units,"meters");
  projection->hem = (dssr->pro_lat>0.0) ? 'N' : 'S';
  projection->re_major = dssr->ellip_maj*1000;
  projection->re_minor = dssr->ellip_min*1000;
  projection->height = 0.0;

  if (strncmp(uc(meta->general->sensor), "ALOS", 4) == 0) {
    projection->spheroid = GRS1980_SPHEROID; // This is an assumption
    projection->datum = ITRF97_DATUM; // This is in the spec
  }
  else if (strncmp(uc(dssr->ellip_des), "GRS80", 5) == 0) {
    projection->spheroid = GRS1980_SPHEROID;
    projection->datum = NAD83_DATUM; // This is an assumption
  }
  else if (strncmp(uc(dssr->ellip_des), "GEM06", 5) == 0) {
    projection->spheroid = GEM6_SPHEROID;
    projection->datum = WGS84_DATUM;
  }
  else {
    projection->spheroid = axis_to_spheroid(projection->re_major, projection->re_minor); //WGS84_SPHEROID;
    projection->datum = spheroid_datum(projection->spheroid); //WGS84_DATUM;
  }

  // Overrides if an mpdr exists and it's a PS-SMM/I projection
  if (mpdr &&
      (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0 || strncmp(mpdr->mpdesig, "PS-SSM/I", 8) == 0))
  {
    projection->spheroid = HUGHES_SPHEROID;
    projection->datum    = HUGHES_DATUM;
    projection->re_major = HUGHES_SEMIMAJOR;
    projection->re_minor = HUGHES_SEMIMAJOR - (1.0 / HUGHES_INV_FLATTENING) * HUGHES_SEMIMAJOR;
  }
}

// ceos_init_proj_x functions
void ceos_init_proj(meta_parameters *meta,  struct dataset_sum_rec *dssr,
        struct VMPDREC *mpdr, struct scene_header_rec *shr,
        struct alos_map_proj_rec *ampr)
{
   meta_projection *projection;
   if (meta->projection == NULL) {
     projection = (meta_projection *)MALLOC(sizeof(meta_projection));
     meta->projection = projection;
   }
   else
     projection = meta->projection;

   if (meta->sar) {
     meta->sar->image_type = 'P';/*Map-Projected image.*/
     meta->general->sample_count = mpdr->npixels;

     if ((strncmp(mpdr->mpdesc, "SLANT RANGE", 11) == 0) ||
   (strncmp(mpdr->mpdesc, "Slant range", 11) == 0)) {
       /* FOCUS processor populates map projection record for slant range! */
       /* ESA (I-PAF) apparently does the same */
       meta->sar->image_type='S';
       projection->type=MAGIC_UNSET_CHAR;
     }
     else if ((strncmp(mpdr->mpdesc, "GROUND RANGE", 12) == 0) ||
        (strncmp(mpdr->mpdesc, "Ground range", 12) == 0)) {
       /* ESA populates map projection record also for ground range! */
       meta->sar->image_type='G';
       projection->type=MAGIC_UNSET_CHAR;
     }
     else if (strncmp(mpdr->mpdesig, "GROUND RANGE",12) == 0) {
       projection->type=SCANSAR_PROJECTION;/*Along Track/Cross Track.*/
     }
     else if (strncmp(mpdr->mpdesig, "LAMBERT", 7) == 0) {
       projection->type=LAMBERT_CONFORMAL_CONIC;/*Lambert Conformal Conic.*/
       printf("WARNING: * Images geocoded with the Lambert Conformal Conic "
        "projection may not\n"
        "         * be accurately geocoded!\n");
       projection->param.lamcc.plat1=mpdr->nsppara1;
       projection->param.lamcc.plat2=mpdr->nsppara2;
       projection->param.lamcc.lat0=mpdr->blclat+0.023;/*NOTE: Hack.*/
       projection->param.lamcc.lon0=mpdr->blclong+2.46;/*NOTE: Hack */
       /* NOTE: We have to hack the lamcc projection because the true lat0 and lon0,
  * as far as we can tell, are never stored in the CEOS
  */
       projection->param.lamcc.false_easting=MAGIC_UNSET_DOUBLE;
       projection->param.lamcc.false_northing=MAGIC_UNSET_DOUBLE;
       projection->param.lamcc.scale_factor=MAGIC_UNSET_DOUBLE;
     }
     else if (strncmp(mpdr->mpdesig, "UPS", 3) == 0) {
       projection->type=POLAR_STEREOGRAPHIC;/*Polar Stereographic*/
       projection->param.ps.slat=(mpdr->upslat >= 0.0) ? 90.0 : -90.0; //70.0;
       projection->param.ps.slon=mpdr->upslong; //-45.0;
       projection->param.ps.is_north_pole=1;
       projection->param.ps.false_easting=MAGIC_UNSET_DOUBLE;
       projection->param.ps.false_northing=MAGIC_UNSET_DOUBLE;     }
     else if (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0 ||
              strncmp(mpdr->mpdesig, "PS-SSM/I", 8) == 0)
     {
       projection->type=POLAR_STEREOGRAPHIC;/*Polar Stereographic: radarsat era.*/
       projection->param.ps.slat=mpdr->upslat;
       projection->param.ps.slon=mpdr->upslong;
       if (projection->param.ps.slat>0) {
        if (projection->param.ps.slon==0.0) {
          projection->param.ps.slon=-45.0;/*Correct reference longitude bug*/
        }
        projection->param.ps.is_north_pole=1;
       }
       else {
         projection->param.ps.is_north_pole=0;
       }
       projection->param.ps.false_easting=MAGIC_UNSET_DOUBLE;
       projection->param.ps.false_northing=MAGIC_UNSET_DOUBLE;
     }
     else if
       (strncmp(mpdr->mpdesig, "UTM", 3) == 0 ||
        strncmp(mpdr->mpdesig, "UNIVERSAL TRANSVERSE MERCATOR", 29) == 0) {
       projection->type=UNIVERSAL_TRANSVERSE_MERCATOR;
       projection->param.utm.zone=atoi(mpdr->utmzone);
       if (projection->param.utm.zone == 0)
     sscanf(mpdr->utmzone, "UT%d", &projection->param.utm.zone);
       projection->param.utm.false_easting=mpdr->utmeast;
       projection->param.utm.false_northing=mpdr->utmnorth;
       projection->param.utm.lat0=mpdr->utmlat;
       projection->param.utm.lon0=mpdr->utmlong;
       projection->param.utm.scale_factor=mpdr->utmscale;
     }
     else {
       printf("Cannot match projection '%s',\n"
        "in map projection data record.\n",mpdr->mpdesig);
       exit(EXIT_FAILURE);
     }

     if(strcmp(meta->general->sensor, "ALOS") == 0){
         double x,y,z,lat,lon;
         char look_direction;
         lat = mpdr->tlclat;
         lon = mpdr->tlclong;
         if (meta->sar) {
             look_direction = meta->sar->look_direction == 'L' ? 'L' : 'R';
         }
         else if (meta->optical && strncmp(meta->optical->pointing_direction,"Off-nadir",9) == 0) {
             look_direction = meta->optical->off_nadir_angle >= 0.0 ? 'R' : 'L';
         }
         else {
             asfPrintWarning("Cannot determine look direction from metadata ...guessing right-looking\n"
                     "Geolocations may turn out wrong.\n");
             look_direction = 'R';
         }
         latlon_to_proj(projection, look_direction, lat*D2R, lon*D2R, 0.0, &x, &y, &z);
         projection->startY = y;
         projection->startX = x;
         projection->perY = -mpdr->nomild;
         projection->perX = mpdr->nomipd;
         projection->datum = ITRF97_DATUM;
     }
     else if (projection->type != SCANSAR_PROJECTION){
       projection->startY = mpdr->tlcnorth;
       projection->startX = mpdr->tlceast;
       projection->perY   = (mpdr->blcnorth - mpdr->tlcnorth) / mpdr->nlines;
       projection->perX   = (mpdr->trceast - mpdr->tlceast) / mpdr->npixels;
       // bogus information but that is what MDA offers for SSG data
       if (strncmp(mpdr->refelip, "NAD 83", 6)==0) {
         projection->datum = NAD83_DATUM;
       }
     }

     /* Default the units to meters */
     strcpy(projection->units,"meters");

     projection->hem = (dssr->pro_lat>0.0) ? 'N' : 'S';
     if (strncmp(dssr->ellip_des,"GRS80",5)==0) {
       projection->spheroid = GRS1980_SPHEROID;
       if (strncmp(meta->general->sensor, "ALOS", 4) != 0) {
         projection->datum = NAD83_DATUM; // A wild guess because NAD83 usually goes with GRS-1980
         // This is probably a bad guess for some stations (Australia)
       }
       else {
         // ALOS uses GRS-1980 and the specs say a ITRF-97 datum
         projection->datum = ITRF97_DATUM;
       }
     }
     else if (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0 ||
              strncmp(mpdr->mpdesig, "PS-SSM/I", 8) == 0)
     {
       projection->spheroid = HUGHES_SPHEROID;
       projection->datum = HUGHES_DATUM;
     }
     else if (strncmp(dssr->ellip_des,"GEM06",5)==0) {
       projection->spheroid = GEM6_SPHEROID;
       projection->datum = WGS84_DATUM; // A best guess ...WGS66 is a better fit but not well supported
     }

     if (strncmp(mpdr->mpdesig, "PS-SMM/I", 8) == 0 ||
         strncmp(mpdr->mpdesig, "PS-SSM/I", 8) == 0)
     {
       projection->re_major = HUGHES_SEMIMAJOR;
       projection->re_minor = HUGHES_SEMIMAJOR - (1.0 / HUGHES_INV_FLATTENING) * HUGHES_SEMIMAJOR;
     }
     else {
       projection->re_major = dssr->ellip_maj*1000;
       projection->re_minor = dssr->ellip_min*1000;
     }
     projection->height = 0.0;
   }
   else {
     if (shr->product_id[6] == 'U') {
       projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
       projection->param.utm.zone = ampr->utm_zone;
       projection->param.utm.false_easting = 500000.0;
       if (ampr->hem == 0)
         projection->param.utm.false_northing = 0.0;
       else
         projection->param.utm.false_northing = 10000000.0;
       projection->param.utm.lat0 = 0.0;
       projection->param.utm.lon0 = (double) (ampr->utm_zone - 1) * 6.0 - 177.0;
       projection->param.utm.scale_factor = 0.9996;
     }
     else if (shr->product_id[6] == 'P') {
       projection->type = POLAR_STEREOGRAPHIC;
       projection->param.ps.slat = ampr->lat_map_origin;
       projection->param.ps.slon = ampr->lon_map_origin;
       if (projection->param.ps.slat>0) {
         projection->param.ps.is_north_pole=1;
       }
       else {
         projection->param.ps.is_north_pole=0;
       }
       projection->param.ps.false_easting=MAGIC_UNSET_DOUBLE;
       projection->param.ps.false_northing=MAGIC_UNSET_DOUBLE;
       if (strncmp(meta->general->sensor, "ALOS", 4) == 0) {
         projection->datum = ITRF97_DATUM;
       }
       else if (strncmp(dssr->ellip_des, "GEM06", 5) == 0) {
         projection->datum = WGS84_DATUM;
       }
       else if (strncmp(dssr->ellip_des, "GRS80", 5) == 0) {
         projection->datum = NAD83_DATUM;
       }
       else {
         projection->datum = WGS84_DATUM;
       }
     }
   }
}

void ceos_init_location_block(meta_parameters *meta)
{
  // Location block
  double line_count = meta->general->line_count;
  double sample_count = meta->general->sample_count;
  if (!meta->location)
    meta->location = meta_location_init();
  meta_get_latLon(meta, 0, 0, 0,
      &meta->location->lat_start_near_range,
      &meta->location->lon_start_near_range);
  meta_get_latLon(meta, line_count, 0, 0,
      &meta->location->lat_end_near_range,
      &meta->location->lon_end_near_range);
  meta_get_latLon(meta, line_count, sample_count, 0,
      &meta->location->lat_end_far_range,
      &meta->location->lon_end_far_range);
  meta_get_latLon(meta, 0, sample_count, 0,
      &meta->location->lat_start_far_range,
      &meta->location->lon_start_far_range);
}

/* Parts that need to come out of jpl_proj.c, once we have sorted out all other
   dependencies.

// atct_init:
// calculates alpha1, alpha2, and alpha3, which are some sort of coordinate
// rotation amounts, in degrees.  This creates a latitude/longitude-style
// coordinate system centered under the satellite at the start of imaging.
// You must pass it a state vector from the start of imaging.
void atct_init(meta_projection *proj,stateVector st)
{
  vector up={0.0,0.0,1.0};
  vector z_orbit, y_axis, a, nd;
  double alpha3_sign;
  double alpha1,alpha2,alpha3;

  vecCross(st.pos,st.vel,&z_orbit);vecNormalize(&z_orbit);

  vecCross(z_orbit,up,&y_axis);vecNormalize(&y_axis);

  vecCross(y_axis,z_orbit,&a);vecNormalize(&a);

  alpha1 = atan2_check(a.y,a.x)*R2D;
  alpha2 = -1.0 * asind(a.z);
  if (z_orbit.z < 0.0)
  {
    alpha1 +=  180.0;
    alpha2 = -1.0*(180.0-fabs(alpha2));
  }

  vecCross(a,st.pos,&nd);vecNormalize(&nd);
  alpha3_sign = vecDot(nd,z_orbit);
  alpha3 = acosd(vecDot(a,st.pos)/vecMagnitude(st.pos));
  if (alpha3_sign<0.0)
    alpha3 *= -1.0;

  proj->param.atct.alpha1=alpha1;
  proj->param.atct.alpha2=alpha2;
  proj->param.atct.alpha3=alpha3;
}
*/

/*******************************************************************************
 * get_ceos_description:
 * Extract a ceos_description structure from given CEOS file. This contains
 * "meta-meta-"data, data about the CEOS, such as the generating facility, a
 * decoded product type, etc.*/
ceos_description *get_ceos_description(const char *fName, report_level_t level)
{
  struct IOF_VFDR *iof=NULL;
  int sar_image, dataSize;;
  char *versPtr,*satStr;
  char *sensorStr,*prodStr,*procStr, *facStr;
  ceos_description *ceos = (ceos_description *)MALLOC(sizeof(ceos_description));
  memset(ceos, 0, sizeof(ceos_description));

  // Determine data type
  iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
  get_ifiledr(fName, iof);
  if ((iof->bitssamp*iof->sampdata)>(iof->bytgroup*8)) iof->bitssamp /= 2;
  dataSize = (iof->bitssamp+7)/8 + (iof->sampdata-1)*5;
  if ((dataSize<6) && (strncmp(iof->formatid, "COMPLEX", 7)==0))
    dataSize += (10 - dataSize)/2;
  switch (dataSize)
    {
    case 2:  ceos->ceos_data_type = CEOS_AMP_DATA;       break;
    case 4:  ceos->ceos_data_type = CEOS_AMP_DATA;       break;
    case 6:  ceos->ceos_data_type = CEOS_RAW_DATA;       break;
    case 7:  ceos->ceos_data_type = CEOS_SLC_DATA_INT;   break;
    case 9:  ceos->ceos_data_type = CEOS_SLC_DATA_FLOAT; break;
    default: ceos->ceos_data_type = CEOS_AMP_DATA;       break;
    }
  FREE(iof);

  // Get dataset summary record for SAR image. Otherwise try scene header record.
  sar_image = get_dssr(fName,&ceos->dssr);
  if (sar_image == -1)
    get_shr(fName,&ceos->shr);

  if (sar_image == 1) {

    // Determine the satellite
    satStr=ceos->dssr.mission_id;

    if (0==strncmp(satStr,"E",1))
      ceos->satellite = ERS;
    else if (0==strncmp(satStr,"J",1))
      ceos->satellite = JERS;
    else if (0==strncmp(satStr,"R",1))
      ceos->satellite = RSAT;
    else if (0==strncmp(satStr,"A",1))
      ceos->satellite = ALOS;
    else if (0==strncmp(satStr,"S",1))
      ceos->satellite = SIR_C;
    else {
      asfReport(level,"get_ceos_description Warning! Unknown satellite '%s'!\n",
                satStr);
      ceos->satellite = unknownSatellite;
    }

    // Determine the sensor
    if (ceos->satellite == ALOS)
      ceos->sensor = PALSAR;
    else
      ceos->sensor = SAR;

    // Determine the processor version.
    ceos->version = 0.0;// Default is zero.
    versPtr = ceos->dssr.ver_id;
    while (!isdigit(*versPtr))
      versPtr++;
    sscanf(versPtr,"%lf",&ceos->version);

    /*Set other fields to unknown (to be filled out by other init routines)*/
    procStr = ceos->dssr.sys_id;
    prodStr = ceos->dssr.product_type;
    facStr = trim_spaces(ceos->dssr.fac_id);
    ceos->processor = unknownProcessor;
    ceos->product = unknownProduct;

    /*Determine the facility that processed the data.*/
    if (0==strncmp(facStr, "ASF", 3))
    {/*Alaska SAR Facility Image*/
      /*Determine the image type and processor ID.*/
      ceos->facility=ASF;
      if (0==strncmp(procStr,"ASP",3))
        ceos->processor = ASP;
      else if (0==strncmp(procStr,"SPS",3))
        ceos->processor = SPS;
      else if (0==strncmp(procStr,"PREC",3))
        ceos->processor = PREC;
      else if (0==strncmp(procStr,"ARDOP",5))
        ceos->processor = ARDOP;
      else if (0==strncmp(procStr,"PP",2))
        ceos->processor = PP;
      else if (0==strncmp(procStr,"SP2",3))
        ceos->processor = SP2;
      else if (0==strncmp(procStr,"AMM",3))
        ceos->processor = AMM;
      else if (0==strncmp(procStr,"DPS",3))
        ceos->processor = DPS;
      else if (0==strncmp(procStr,"MSSAR",5))
        ceos->processor = MSSAR;
      /* VEXCEL Focus processor */
      else if (0==strncmp(procStr,"FOCUS",5))
        ceos->processor = FOCUS;
      else if (0==strncmp(procStr,"SKY",3))
        {/*Is VEXCEL level-0 processor, not ASF*/
          ceos->facility = ASF;
          ceos->processor = LZP;
          ceos->product = RAW;
          return ceos;
        }
      else if (0==strncmp(procStr, "PC", 2)) {
        if (0==strncmp(prodStr,"SCANSAR",7))
          ceos->processor = SP3;
        else if (0==strncmp(prodStr,"FUL",3))
          ceos->processor = PREC;
      }
      // Taken out any warnings about unknown processors. With the processing
      // on the grid these names will keep changing, so there is no point in
      // tracking those.

      if (0==strncmp(prodStr,"LOW",3))
        ceos->product = LOW_REZ;
      else if (0==strncmp(prodStr,"FUL",3))
        ceos->product = HI_REZ;
      else if (0==strncmp(prodStr,"SCANSAR",7))
        ceos->product = SCANSAR;
      else if (0==strncmp(prodStr,"CCSD",4))
        ceos->product = CCSD;
      else if (0==strncmp(prodStr,"COMPLEX",7))
        ceos->product = SLC;
      else if (0==strncmp(prodStr,"RAMP",4))
        ceos->product = RAMP;

      // Non-ASF data
      else if (0==strncmp(prodStr,"SPECIAL PRODUCT(SINGL-LOOK COMP)",32))
        ceos->product = SLC;
      else if (0==strncmp(prodStr, "SLANT RANGE COMPLEX",19))
        ceos->product = SLC;
      else if (0==strncmp(prodStr, "SAR PRECISION IMAGE",19))
        ceos->product = PRI;
      else if (0==strncmp(prodStr, "SAR GEOREF FINE",15))
        ceos->product = SGF;
      else if (0==strncmp(prodStr, "STANDARD GEOCODED IMAGE",23))
        ceos->product = SGI;
      else {
        asfReport(level, "get_ceos_description Warning! "
            "Unknown ASF product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }

    }
    else if (0==strncmp(facStr, "ES", 2))
    {/*European Space Agency Image*/
      asfReport(level, "   Data set processed by ESA\n");
      ceos->facility = ESA;

      if (0==strncmp(prodStr,"SAR RAW SIGNAL",14))
        ceos->product = RAW;
      else if (0==strncmp(prodStr,"SAR PRECISION IMAGE",19))
        ceos->product = PRI;
      else {
        asfReport(level, "Get_ceos_description Warning! "
            "Unknown ESA product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "CDPF", 4))
    {
      asfReport(level, "   Data set processed by CDPF\n");
      ceos->facility = CDPF;

      if (0==strncmp(prodStr,"SPECIAL PRODUCT(SINGL-LOOK COMP)",32))
        ceos->product = SLC;
      else if (0==strncmp(prodStr,"SCANSAR WIDE",12))
        ceos->product = SCANSAR;
      else if (0==strncmp(prodStr, "SAR GEOREF FINE",15))
        ceos->product = SGF;
      else if (0==strncmp(prodStr, "SAR GEOREF EXTRA FINE",21))
        ceos->product = SGX;
      else if (0==strncmp(prodStr, "SYSTEMATIC GEOCODED UTM", 23))
        ceos->product = SSG;
      else {
        asfReport(level, "Get_ceos_description Warning! "
              "Unknown CDPF product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "D-PAF", 5)) {
      asfReport(level, "   Data set processed by D-PAF\n");
      ceos->facility = DPAF;
      if (0==strncmp(prodStr,"SAR RAW SIGNAL",14))
        ceos->product = RAW;
      else if (0==strncmp(prodStr,"SAR SINGLE LOOK COMPLEX IMAGE",29))
        ceos->product = SLC;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown D-PAF product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "I-PAF", 5)) {
      asfReport(level, "   Data set processed by I-PAF\n");
      ceos->facility = IPAF;
      if (0==strncmp(prodStr,"SAR RAW SIGNAL",14))
        ceos->product = RAW;
      else if (0==strncmp(prodStr,"SAR SINGLE LOOK COMPLEX IMAGE",29))
        ceos->product = SLC;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown I-PAF product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "EOC", 3)) {
      asfReport(level, "   Data set processed by EOC\n");
      ceos->facility = EOC;
      if (0==strncmp(ceos->dssr.lev_code, "1.0", 3))
        ceos->product = RAW;
      else if (0==strncmp(ceos->dssr.lev_code, "1.1", 3))
        ceos->product = SLC;
      else if (0==strncmp(prodStr, "STANDARD GEOCODED IMAGE",23))
        ceos->product = SGI;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown EOC product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "RSI", 3)) {
      asfReport(level, "   Data set processed by RSI\n");
      ceos->facility = RSI;
      if (0==strncmp(prodStr, "SCANSAR WIDE",12))
        ceos->product = SCANSAR;
      else if (0==strncmp(prodStr, "SAR GEOREF EXTRA FINE",21))
        ceos->product = SGF;
      else if (0==strncmp(prodStr, "SCANSAR NARROW",14))
        ceos->product = SCN;
      else if (0==strncmp(prodStr, "SAR GEOREF FINE",15))
        ceos->product = SGF;
      else if (0==strncmp(prodStr, "UNPROCESSED SIGNAL DATA",23))
  ceos->product = RAW;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown RSI product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "JPL", 3)) {
      asfReport(level, "   Data set processed by JPL\n");
      ceos->facility = JPL;
      if (0==strncmp(prodStr, "REFORMATTED SIGNAL DATA", 23))
        ceos->product = RAW;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown JPL product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "CSTARS", 6)) {
      asfReport(level, "   Data set processed by CSTARS\n");
      ceos->facility = CSTARS;
      if (0==strncmp(procStr,"FOCUS",5))
        ceos->processor = FOCUS;
      if (0==strncmp(prodStr, "SCANSAR NARROW", 14))
        ceos->product = SCN;
      else if (0==strncmp(prodStr, "SCANSAR WIDE", 12))
    ceos->product = SCANSAR;
      else if (0==strncmp(prodStr, "SAR GEOREF FINE", 15))
    ceos->product = SGF;
      else if (0==strncmp(prodStr, "SAR GEOREF EXTRA FINE", 21))
    ceos->product = SGX;
      else if (0==strncmp(prodStr, "SAR PRECISION IMAGE", 19))
    ceos->product = PRI;
      else if (0==strncmp(prodStr, "SYSTEMATIC  GEOCODED UTM", 24))
    ceos->product = SSG;
      else if (0==strncmp(prodStr, "GEC", 3))
    ceos->product = GEC;
      else if (0==strncmp(prodStr, "SLANT RANGE COMPLEX", 19))
    ceos->product = SLC;
      else if (0==strncmp(prodStr, "SPECIAL PRODUCT(SINGL-LOOK COMP)", 32))
    ceos->product = SLC;
      else if (0==strncmp(prodStr, "SAR RAW SIGNAL DATA", 19))
    ceos->product = RAW;
      else if (0==strncmp(prodStr, "UNPROCESSED SIGNAL DATA", 23))
    ceos->product = RAW;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown CSTARS product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(facStr, "Beijing", 7)) {
      asfReport(level, "   Data set processed by Beijing\n");
      ceos->facility = BEIJING;
      if (0==strncmp(prodStr,"SAR RAW DATA",12))
        ceos->product = RAW;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                    "Unknown Beijing product type '%s'!\n", prodStr);
        ceos->product = unknownProduct;
      }
    }
  }
  else {
    // Determine satellite
    satStr = ceos->shr.mission_id;
    if (0==strncmp(satStr,"A",1))
      ceos->satellite = ALOS;
    else {
      asfReport(level, "get_ceos_description Warning! "
         "Unknown satellite '%s'!\n", satStr);
      ceos->satellite = unknownSatellite;
    }

    // Determine sensor
    sensorStr = ceos->shr.sensor_id;
    if (ceos->satellite == ALOS) {
      if (0==strncmp(sensorStr,"AVNIR",5))
        ceos->sensor = AVNIR;
      else if (0==strncmp(sensorStr, "PRISM",5))
        ceos->sensor = PRISM;
      else {
        asfReport(level, "Get_ceos_description Warning! "
                  "Unknown sensor '%s'!\n", sensorStr);
        ceos->sensor = unknownSensor;
      }
    }

    // Determine product
    prodStr = ceos->shr.product_id;
    prodStr++;
    if (strncmp(prodStr, "1A", 2) == 0)
      ceos->product = LEVEL_1A;
    else if (strncmp(prodStr, "1B1", 3) == 0)
      ceos->product = LEVEL_1B1;
    else if (strncmp(prodStr, "1B2R", 4) == 0)
      ceos->product = LEVEL_1B2R;
    else if (strncmp(prodStr, "1B2G", 4) == 0)
      ceos->product = LEVEL_1B2G;
    else {
      asfReport(level, "Get_ceos_description Warning! "
         "Unknown product '%s'!\n", prodStr);
      ceos->product = unknownProduct;
    }
    // Determine processor
  }

  // Tell user what product we are importing
  if (ceos->product == CCSD)
    asfReport(level, "   Product: CCSD\n");
  else if (ceos->product == RAW)
    asfReport(level, "   Product: RAW\n");
  else if (ceos->product == LOW_REZ)
    asfReport(level, "   Product: LOW_REZ\n");
  else if (ceos->product == HI_REZ)
    asfReport(level, "   Product: HI_REZ\n");
  else if (ceos->product == RAMP)
    asfReport(level, "   Product: RAMP\n");
  else if (ceos->product == SCANSAR)
    asfReport(level, "   Product: SCANSAR\n");
  else if (ceos->product == SLC)
    asfReport(level, "   Product: SLC\n");
  else if (ceos->product == PRI)
    asfReport(level, "   Product: PRI\n");
  else if (ceos->product == SGF)
    asfReport(level, "   Product: SGF\n");
  else if (ceos->product == SGX)
    asfReport(level, "   Product: SGX\n");
  else if (ceos->product == SGI)
    asfReport(level, "   Product: SGI\n");
  else if (ceos->product == SSG)
    asfReport(level, "   Product: SSG\n");
  else if (ceos->product == SPG)
    asfReport(level, "   Product: SPG\n");
  else if (ceos->product == SCN)
    asfReport(level, "   Product: SCN\n");
  else if (ceos->product == LEVEL_1A)
    asfReport(level, "   Product: LEVEL_1A\n");
  else if (ceos->product == LEVEL_1B1)
    asfReport(level, "   Product: LEVEL_1B1\n");
  else if (ceos->product == LEVEL_1B2R)
    asfReport(level, "   Product: LEVEL_1B2R\n");
  else if (ceos->product == LEVEL_1B2G)
    asfReport(level, "   Product: LEVEL_1B2G\n");

  return ceos;
}

// Function extracts the acquisition time of the first line
// out of the line header
double get_firstTime (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct RHEADER linehdr;
   int length;
   char *buff;

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct RHEADER), 1, fp);
   length = bigInt32(hdr.recsiz) - (sizeof(struct RHEADER)
            + sizeof(struct HEADER));
   buff = (char *) MALLOC(sizeof(char)*(length+5));
   //buff = (char *) MALLOC(sizeof(char)*30000);
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct RHEADER), 1, fp);
   FCLOSE(fp);

   FREE(buff);
   //printf("lat 1: %.4lf, lon 1: %.4lf\n",
   //   (double)bigInt32((unsigned char *)&(linehdr.lat_first)),
   //   (double)bigInt32((unsigned char *)&(linehdr.long_first)));
   //printf("lat 2: %.4lf, lon 2: %.4lf\n",
   //   (double)bigInt32((unsigned char *)&(linehdr.lat_last)),
   //   (double)bigInt32((unsigned char *)&(linehdr.long_last)));
   //printf("Time: %lf\n",
   //   (double)bigInt32((unsigned char *)&(linehdr.acq_msec))/1000.0);
   return (double)bigInt32((unsigned char *)&(linehdr.acq_msec))/1000.0;
}

// Function extracts the acquisition time of the first line
// out of the ALOS line header
double get_alos_firstTime (const char *fName)
{
   struct PHEADER linehdr;
   get_alos_linehdr(&linehdr, fName);
   return ((double)bigInt32(linehdr.acq_msec)/1000.0);
}

// Function that reads polarization out of the line header
char *get_polarization (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct SHEADER linehdr;
   int length;
   char *buff;
   char *polarization;

   polarization = (char *) MALLOC(sizeof(char)*3);

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   length = bigInt32(hdr.recsiz)-12;
   buff = (char *) MALLOC(sizeof(char)*(length+5));
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
   FCLOSE(fp);

   if (bigInt16(linehdr.tran_polar) == 0)
     polarization[0] = 'H';
   else if (bigInt16(linehdr.tran_polar) == 1)
     polarization[0] = 'V';
   if (bigInt16(linehdr.recv_polar) == 0)
     polarization[1] = 'H';
   else if (bigInt16(linehdr.recv_polar) == 1)
     polarization[1] = 'V';
   polarization[2] = 0;

   FREE(buff);
   return polarization;
}

// Function that reads the band number out of the line header
int get_alos_band_number(const char *fName)
{
  FILE *fp;
  struct HEADER hdr;
  struct SHEADER linehdr;
  int length;
  char *buff;

  fp = FOPEN(fName, "r");
  FREAD (&hdr, sizeof(struct HEADER), 1, fp);
  length = bigInt32(hdr.recsiz)-12;
  buff = (char *) MALLOC(sizeof(char)*(length+5));
  FREAD (buff, length, 1, fp);
  FREAD (&hdr, sizeof(struct HEADER), 1, fp);
  FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
  FCLOSE(fp);

  FREE(buff);
  return bigInt32(linehdr.rec_num);
}

// Function that reads chirp out of the line header
double get_chirp_rate (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct SHEADER linehdr;
   int length;
   char *buff;

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   length = bigInt32(hdr.recsiz)-12;
   buff = (char *) MALLOC(sizeof(char)*(length+5));
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
   FCLOSE(fp);

   FREE(buff);
   return (double)bigInt16(linehdr.chirp_linear);
}

// Function that reads sensor orientation out of the line header
double get_sensor_orientation (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct SHEADER linehdr;
   int length;
   char *buff;

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   length = bigInt32(hdr.recsiz)-12;
   buff = (char *) MALLOC(sizeof(char)*(length+5));
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
   FCLOSE(fp);

   FREE(buff);
   printf("roll: %lf\n", (double)bigInt16(linehdr.roll));
   printf("yaw: %lf\n", (double)bigInt16(linehdr.yaw));
   printf("pitch: %lf\n", (double)bigInt16(linehdr.pitch));
   return (double)bigInt16(linehdr.yaw);
}

void get_azimuth_time(ceos_description *ceos, const char *in_fName,
          meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  ymd_date date;
  hms_time time;
  double centerTime;

  dssr = &ceos->dssr;

  // calculate earth radius at nadir (not at scene center)
  double re = (meta_is_valid_double(meta->general->re_major))
    ? meta->general->re_major : 6378137.0;
  double rp = (meta_is_valid_double(meta->general->re_minor))
    ? meta->general->re_minor : 6356752.31414;
  double lat = D2R*dssr->plat_lat;
  double er = (re*rp)
    / sqrt(rp*rp*cos(lat)*cos(lat)+re*re*sin(lat)*sin(lat));

  // find the state vector closest to the acquisition time
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  struct pos_data_rec ppdr;
  get_ppdr(in_fName,&ppdr);
  int i,closest=0;
  double closest_diff=9999999;
  for (i=0; i<ppdr.ndata; ++i) {
    double diff = fabs(ppdr.gmt_sec + i*ppdr.data_int - centerTime);
    if (diff < closest_diff) {
      closest = i; closest_diff = diff;
    }
  }

  // compute satellite height from closest state vector
  double ht = sqrt(ppdr.pos_vec[closest][0] * ppdr.pos_vec[closest][0] +
       ppdr.pos_vec[closest][1] * ppdr.pos_vec[closest][1] +
       ppdr.pos_vec[closest][2] * ppdr.pos_vec[closest][2]);

  // velocity calculation
  const double g = 9.81; //9.80665;
  double orbit_vel = sqrt(g*er*er/ht);

  // simple scaling to get swath velocity from orbit vel
  double swath_vel = orbit_vel * er / ht;

  // calculate azimuth time per pixel from the swath velocity
  meta->sar->azimuth_time_per_pixel =
    meta->general->y_pixel_size / swath_vel;
}

// Returns startX and startY in meters given meta->sar and meta->state_vectors
// => Use this when missing startX, startY and a transform block.
//
// FIXME: No height correction
int meta_sar_to_startXY (meta_parameters *meta, double *startX, double *startY)
{
  int ret;
  double hgt = 0.0;
  double time, slant, doppler;
  double lat, lon;
  double x, y, z;
  projection_type_t old_projection_type = UNKNOWN_PROJECTION;

  // Zooper datacheck to make sure we have all we need to proceed
  if (!meta->general) asfPrintError("Missing General block in metadata.\n");
  if (!meta->sar) asfPrintError("Missing SAR block in metadata.\n");
  if (!meta->state_vectors) asfPrintError("Missing State Vectors block in metadata.\n");
  if (!meta_is_valid_int(meta->general->start_line)                     ||
      !meta_is_valid_int(meta->general->start_sample)                   ||
      !meta_is_valid_double(meta->sar->slant_range_first_pixel)         ||
      !meta_is_valid_double(meta->sar->slant_shift)                     ||
      !meta_is_valid_double(meta->sar->azimuth_time_per_pixel)          ||
      !meta_is_valid_double(meta->sar->time_shift)                      ||
      !meta_is_valid_double(meta->sar->deskewed)                        ||
      !meta_is_valid_double(meta->sar->range_doppler_coefficients[0])   ||
      !meta_is_valid_double(meta->sar->azimuth_doppler_coefficients[0]) ||
      (meta->sar->look_direction != 'R' && meta->sar->look_direction != 'L'))
  {
    asfPrintError("One or more necessary metadata values are missing.  Need:\n"
        "SAR block, State Vectors block, start_line, start_sample, slant_range_first_pixel\n"
        "slant_shift, azimuth_time_per_pixel, time_shift, look_direction, and all doppler\n"
        "coefficients.\n");
  }

  slant = meta->sar->slant_range_first_pixel +
      meta->general->start_sample +
      meta->sar->slant_shift;
  time  = meta->general->start_line * meta->sar->azimuth_time_per_pixel +
      meta->sar->time_shift;
  if (meta->sar->deskewed) {
    doppler = 0.0;
  }
  else {
    double sample = meta->general->start_sample;
    doppler = meta->sar->range_doppler_coefficients[0] +
          meta->sar->range_doppler_coefficients[1] * sample +
          meta->sar->range_doppler_coefficients[2] * sample * sample;
  }
  if (meta->projection->type == LAT_LONG_PSEUDO_PROJECTION) {
    // Kludge so meta_timeSlantDop2latlon() won't bail...
    old_projection_type = meta->projection->type;
    meta->projection->type = SCANSAR_PROJECTION;
  }
  ret = meta_timeSlantDop2latLon(meta, time, slant, doppler, hgt, &lat, &lon);
  if (!ret) {
    latlon_to_proj(meta->projection, meta->sar->look_direction,
                   lat*D2R, lon*D2R, hgt, &x, &y, &z);
    *startX = x;
    *startY = y;
  }
  else {
    *startX = MAGIC_UNSET_DOUBLE;
    *startY = MAGIC_UNSET_DOUBLE;
  }
  if (old_projection_type == LAT_LONG_PSEUDO_PROJECTION) {
    // Undo the kludge if necessary
    meta->projection->type = old_projection_type;
  }

  return ret;
}

spheroid_type_t axis_to_spheroid (double re_major, double re_minor)
{
  struct fit {
    spheroid_type_t spheroid;
    double diff;
  }
  diff_array[11];

  // Find the fits (note: no guarantee that the enums will differ by whole numbers, so
  // step through manually rather than in a for-loop... )
  diff_array[0].spheroid = BESSEL_SPHEROID;
  diff_array[0].diff = spheroidDiffFromAxis(diff_array[0].spheroid, re_major, re_minor);

  diff_array[1].spheroid = CLARKE1866_SPHEROID;
  diff_array[1].diff = spheroidDiffFromAxis(diff_array[1].spheroid, re_major, re_minor);

  diff_array[2].spheroid = CLARKE1880_SPHEROID;
  diff_array[2].diff = spheroidDiffFromAxis(diff_array[2].spheroid, re_major, re_minor);

  diff_array[3].spheroid = GEM6_SPHEROID;
  diff_array[3].diff = spheroidDiffFromAxis(diff_array[3].spheroid, re_major, re_minor);

  diff_array[4].spheroid = GEM10C_SPHEROID;
  diff_array[4].diff = spheroidDiffFromAxis(diff_array[4].spheroid, re_major, re_minor);

  diff_array[5].spheroid = GRS1980_SPHEROID;
  diff_array[5].diff = spheroidDiffFromAxis(diff_array[5].spheroid, re_major, re_minor);

  diff_array[6].spheroid = INTERNATIONAL1924_SPHEROID;
  diff_array[6].diff = spheroidDiffFromAxis(diff_array[6].spheroid, re_major, re_minor);

  diff_array[7].spheroid = INTERNATIONAL1967_SPHEROID;
  diff_array[7].diff = spheroidDiffFromAxis(diff_array[7].spheroid, re_major, re_minor);

  diff_array[8].spheroid = WGS72_SPHEROID;
  diff_array[8].diff = spheroidDiffFromAxis(diff_array[8].spheroid, re_major, re_minor);

  diff_array[9].spheroid = WGS84_SPHEROID;
  diff_array[9].diff = spheroidDiffFromAxis(diff_array[9].spheroid, re_major, re_minor);

  diff_array[10].spheroid = HUGHES_SPHEROID;
  diff_array[10].diff = spheroidDiffFromAxis(diff_array[10].spheroid, re_major, re_minor);

  // NOTE: Counting down (see below) rather than up puts a preference on using a newer or
  // more common spheroids rather than an older or less common ...look at the list above.
  // => GRS1980 and GEM10C will have similar results in general, so in this case in particular,
  // counting down will 'prefer' GRS1980 rather than GEM10C
  int min = 0;
  int i;
  for (i=9; i>=0; i--) {
    min = (diff_array[i].diff < diff_array[min].diff) ? i : min;
  }

  return diff_array[min].spheroid;
}

double spheroidDiffFromAxis (spheroid_type_t spheroid, double n_semi_major, double n_semi_minor)
{
  double s_semi_major = 0.0;
  double s_semi_minor = 0.0;

  asfRequire(n_semi_major >= 0.0 && n_semi_minor >= 0,
             "Negative semi-major or semi-minor values found\n");
  asfRequire(n_semi_major <= MAXREAL && n_semi_minor <= MAXREAL,
             "Semi-major and/or semi-minor axis too large.\n");

  switch (spheroid) {
    case BESSEL_SPHEROID:
      s_semi_major = BESSEL_SEMIMAJOR;
      s_semi_minor = BESSEL_SEMIMAJOR * (1.0 - 1.0/BESSEL_INV_FLATTENING);
      break;
    case CLARKE1866_SPHEROID:
      s_semi_major = CLARKE1866_SEMIMAJOR;
      s_semi_minor = CLARKE1866_SEMIMAJOR * (1.0 - 1.0/CLARKE1866_INV_FLATTENING);
      break;
    case CLARKE1880_SPHEROID:
      s_semi_major = CLARKE1880_SEMIMAJOR;
      s_semi_minor = CLARKE1880_SEMIMAJOR * (1.0 - 1.0/CLARKE1880_INV_FLATTENING);
      break;
    case GEM6_SPHEROID:
      s_semi_major = GEM6_SEMIMAJOR;
      s_semi_minor = GEM6_SEMIMAJOR * (1.0 - 1.0/GEM6_INV_FLATTENING);
      break;
    case GEM10C_SPHEROID:
      s_semi_major = GEM10C_SEMIMAJOR;
      s_semi_minor = GEM10C_SEMIMAJOR * (1.0 - 1.0/GEM10C_INV_FLATTENING);
      break;
    case GRS1980_SPHEROID:
      s_semi_major = GRS1980_SEMIMAJOR;
      s_semi_minor = GRS1980_SEMIMAJOR * (1.0 - 1.0/GRS1980_INV_FLATTENING);
      break;
    case INTERNATIONAL1924_SPHEROID:
      s_semi_major = INTERNATIONAL1924_SEMIMAJOR;
      s_semi_minor = INTERNATIONAL1924_SEMIMAJOR * (1.0 - 1.0/INTERNATIONAL1924_INV_FLATTENING);
      break;
    case INTERNATIONAL1967_SPHEROID:
      s_semi_major = INTERNATIONAL1967_SEMIMAJOR;
      s_semi_minor = INTERNATIONAL1967_SEMIMAJOR * (1.0 - 1.0/INTERNATIONAL1967_INV_FLATTENING);
      break;
    case WGS72_SPHEROID:
      s_semi_major = WGS72_SEMIMAJOR;
      s_semi_minor = WGS72_SEMIMAJOR * (1.0 - 1.0/WGS72_INV_FLATTENING);
      break;
    case WGS84_SPHEROID:
      s_semi_major = WGS84_SEMIMAJOR;
      s_semi_minor = WGS84_SEMIMAJOR * (1.0 - 1.0/WGS84_INV_FLATTENING);
      break;
    case HUGHES_SPHEROID:
      s_semi_major = HUGHES_SEMIMAJOR;
      s_semi_minor = HUGHES_SEMIMAJOR * (1.0 - 1.0/HUGHES_INV_FLATTENING);
      break;
    default:
      asfPrintError("ERROR: Unsupported spheroid type in spheroid_axis_fit()\n");
      break;
  }

  // The following calculates an approximation of the sum-squared-error (SSE)
  // over a quarter-span of an ellipse defined by the passed-in semi-major/semi-minor
  // axis versus an ellipse defined by the semi-major/semi-minor axis from one of
  // our supported types of spheroids.  The square root of this value is returned
  // as a measure of fit between the two.
  //
  // Method:
  // The calculated x,y for one ellipse is used as a first reference point, then
  // for that x and y, points are found on the second ellipse by first using the
  // x and then by using the y.  This defines 2 points on the second ellipse over
  // a short span.  The average of these 2 points is fairly close to where a
  // normal (from either ellipse) would subtend the second ellipse.  We define this
  // as a second reference point.  The distance between these two reference points
  // is interpreted as the 'error' between the curves and what we square and sum
  // over the quarter-ellipse.
  //
  // This summation is a very good approximation of the true SSE if the two ellipsis
  // are not too different from each other, and for map projection spheroids,
  // this is a good assumption to make.
  //
  // FIXME: We can probably optimize the math below for better speed, but for now,
  // the code shows the goings-on in an intuitive manner instead.  I'll profile the
  // code later... although it won't get called much and speed shouldn't really BE
  // an issue :)
         //
  // Off we go...
         double SSE = 0.0;
     double x1, y1, x2, y2, xt1, yt1, xt2, yt2; // 2 ref pts, 2 tmp pts
  // 100,000 angular steps from 0 to PI/2 radians results in steps of about
  // 1 km in size along the surface.  The worst-case SSE caculated below
  // will still be far within max double limits ...but a higher or lower number
  // will slow or speed these calcs.  I think this number of steps is somewhat
  // optimal, noting that this function should rarely be called anyway.
     double dx, dy;
     double num_steps = 100000.0;
     double theta;
     double angular_step_size = (PI/2.0) / num_steps;

  // Use the minimum of the major axis for converting angle to x
  // since this is the upper limit for x, not because it is the
  // best answer (least-bias answer would be to use average of
  // all 4 axii).  A circle approximation should be close 'nuf
  // for determining x and x-step size.  Angular steps result in
  // (nearly) equal-distance steps along the ellipse's locus of
  // points (no polar or equatorial bias.)
     double h = MIN(s_semi_major, n_semi_major);
     double pi_over_2 = PI / 2.0;

     for (theta = 0.0; theta <= pi_over_2; theta += angular_step_size) {
    // Find first reference point, (x1, y1), on first ellipse
       x1 = h * cos(theta);
       y1 = sqrt(fabs(s_semi_minor * s_semi_minor *
           (1.0 - (x1 * x1)/(s_semi_major * s_semi_major))));

    // Find first temporary point, (xt1, yt1), on second ellipse
       xt1 = x1;
       yt1 = sqrt(fabs(n_semi_minor * n_semi_minor *
           (1.0 - (xt1 * xt1)/(n_semi_major * n_semi_major))));

    // Find second temporary point, (xt2, yt2), on second ellipse and
    // average the two temporary points
       yt2 = y1;
       xt2 = sqrt(fabs(n_semi_major * n_semi_major *
           (1.0 - (yt2 * yt2)/(n_semi_minor * n_semi_minor))));

    // On the chord from (xt1, yt1) to (xt2, yt2), find the
    // mid-point and 'pretend' it's on the second ellipse and
    // along the normal from the first to second (or vice versa).
    // => For small (dxt, dyt), this is a valid approximation.
       x2 = (xt1 + xt2)/2.0;
       y2 = (yt1 + yt2)/2.0;

    // Sum the squared Euclidean distance (the error between ellipsis)
    // into the sum-squared-error (SSE)
       dx = x2 - x1;
       dy = y2 - y1;
       SSE += dx*dx + dy*dy;
     }
  // Add in the error at theta = 0 and theta = PI/2
     dy = s_semi_minor - n_semi_minor;
     dx = s_semi_major - n_semi_major;
     SSE +=  dx*dx + dy*dy;

  // Return the square root of the SSE as a measure of fit
     return sqrt(SSE);
}

// This function makes some rash assumptions based on
// typical associations ...and when no 'typical association'
// exists, it just returns WGS84.  Awful...
datum_type_t spheroid_datum (spheroid_type_t spheroid)
{
  datum_type_t datum = WGS84_DATUM;

  switch(spheroid) {
    case CLARKE1866_SPHEROID:
      datum = NAD27_DATUM;
      break;
    case GRS1980_SPHEROID:
      datum = NAD83_DATUM;
      break;
    case HUGHES_SPHEROID:
      datum = HUGHES_DATUM;
      break;
    case WGS84_SPHEROID:
    case BESSEL_SPHEROID:
    case CLARKE1880_SPHEROID:
    case GEM6_SPHEROID:
    case GEM10C_SPHEROID:
    case INTERNATIONAL1924_SPHEROID:
    case INTERNATIONAL1967_SPHEROID:
    case WGS72_SPHEROID:
    default:
      datum = WGS84_DATUM;
      break;
  }

  return datum;
}

void set_alos_look_count(meta_parameters *meta, const char *inMetaName)
{
  if (strcmp_case(meta->general->sensor, "ALOS") == 0)
  {
      // The ALOS spec says that the number of looks is:
      //   high-rez mode (single polarization), pixel-spacing  6.25 => 2
      //                                                      12.5  => 4
      //   direct down-link mode, pixel-spacing 12.5                => 4
      //   wide observation mode, pixel-spacing 100                 => 8
      //   polarimetry mode, pixel-spacing 12.5                     => 4

      // We can figure out which mode we are in from the sensor_id in
      // the dataset summary record, characters 12 & 13 of that string.
      // we can distinguish between single/dual high resolution mode by
      // checking the nchn field.

      struct dataset_sum_rec dssr;
      get_dssr(inMetaName, &dssr);
      char D='-', E='-';
      if (strlen(dssr.sensor_id) >= 13) {
        D = dssr.sensor_id[12];
        E = dssr.sensor_id[13];
      }

      // now we see if the mode matches the list above
      if (D == '6' && E == '0' && dssr.nchn == 1) {
          // high-rez mode, single polarization
          asfPrintStatus("   High-resolution, single-pol (look count = 2)\n");
          meta->sar->look_count = 2;
      } else if (D == '6' && E == '0' && dssr.nchn == 2) {
          // high-rez mode, dual polarization
          asfPrintStatus("   High-resolution, dual-pol (look count = 4)\n");
          meta->sar->look_count = 4;
      } else if (D == '6' && E == '1') {
          // wide observation mode
          asfPrintStatus("   Wide observation data (look count = 8)\n");
          meta->sar->look_count = 8;
      } else if (D == '6' && E == '2') {
          // polarimetry mode
          asfPrintStatus("   Polarimetric data (look count = 8)\n");
          meta->sar->look_count = 8;
      } else if (D == '6' && E == '3') {
          // direct downlink mode
          asfPrintStatus("   Direct downlink data (look count = 4)\n");
          meta->sar->look_count = 4;
      } else {
        // We have the following kludge for now ... but we should never get
        // here.  perhaps an asfPrintError would be better.
        asfPrintStatus(
            "   Estimating look count based on square pixel assumption...\n");
        int pix_ratio = (int)floor(0.5 + meta->general->x_pixel_size /
                                        meta->general->y_pixel_size);
        if (pix_ratio == 2 || pix_ratio == 4 || pix_ratio == 8) {
            meta->sar->look_count = pix_ratio;
            asfPrintStatus("   Estimated look count: %d\n", pix_ratio);
        } else {
            asfPrintStatus("   Couldn't figure out look count.  Leaving as %d.\n",
                meta->sar->look_count);
        }
      }
      if (meta->sar->look_count > 1)
          meta->sar->multilook = 0;
  }
}
