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
#include "meta_init.h"
#include "asf_endian.h"
#include "dateUtil.h"
#include "get_ceos_names.h"
#include "libasf_proj.h"
#include "spheroids.h"

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
void ceos_init_sar_general(ceos_description *ceos, const char *in_fName,
			   meta_parameters *meta);
void ceos_init_sar_beijing(ceos_description *ceos, const char *in_fName,
			   meta_parameters *meta);

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
int get_alos_delta_time (const char *fileName, double *delta);
double get_alos_firstTime (const char *fName);

double get_chirp_rate (const char *fName);

/* Prototypes from meta_init_stVec.c */
void ceos_init_stVec(const char *fName,ceos_description *ceos,meta_parameters *sar);
double get_timeDelta(ceos_description *ceos,struct pos_data_rec *ppdr,
                     meta_parameters *meta);

/* Prototypes from jpl_proj.c */
void atct_init(meta_projection *proj,stateVector st);
int UTM_zone(double lon);

/* Prototype from frame_calc.c */
int asf_frame_calc(char *sensor, float latitude, char orbit_direction);


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
    asfPrintError("Should never got here!\n");
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
  strcpy(meta->general->sensor, dssr->mission_id);
  strtok(meta->general->sensor, " "); // Remove spaces from field.
  sprintf(meta->general->sensor_name, "SAR");
  if (strlen(dssr->beam1) <= (MODE_FIELD_STRING_MAX)) {
    strcpy(meta->general->mode, dssr->beam1);
  }
  strcpy(fac,dssr->fac_id); strtok(fac," "); // Remove spaces from field
  strcpy(sys,dssr->sys_id); strtok(sys," "); // Remove spaces from field
  strcpy(ver,dssr->ver_id); strtok(ver," "); // Remove spaces from field
  sprintf(meta->general->processor, "%s/%s/%s", fac, sys, ver);
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
  meta->sar->pulse_duration = dssr->rng_length / 10000000;
  meta->sar->range_sampling_rate = dssr->rng_samp_rate * 1000000;
  
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
  ceos_init_stVec(in_fName, ceos, meta);

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
  struct PPREC *ppr=NULL;
  struct VMPDREC *mpdr=NULL;
  struct ESA_FACDR *esa_facdr=NULL;
  char beamname[32], buf[50], **dataName;
  ymd_date date;
  hms_time time;
  double firstTime, centerTime;
  int nBands;

  dssr = &ceos->dssr;
  iof = (struct IOF_VFDR*) MALLOC(sizeof(struct IOF_VFDR));
  get_ifiledr(in_fName, iof);
  ppr = (struct PPREC*) MALLOC(sizeof(struct PPREC));
  get_ppr(in_fName, ppr);
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  get_esa_facdr(in_fName, esa_facdr);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);
  
  // Azimuth time per pixel needs to be known for state vector propagation
  require_ceos_data(in_fName, &dataName, &nBands);
  firstTime = get_firstTime(dataName[0]);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel = 
    (centerTime - firstTime) / (meta->sar->original_line_count/2);

  // State vector block
  if (ceos->product != SSG)
    ceos_init_stVec(in_fName,ceos,meta);

  meta->general->frame =
    asf_frame_calc("ERS", dssr->pro_lat, meta->general->orbit_direction);
  if (meta->general->orbit_direction==' ')
    meta->general->orbit_direction =
      (meta->general->frame>=1791 && meta->general->frame<=5391) ? 'D' : 'A';
  if (ceos->satellite == RSAT) {
    if (ceos->product == SCANSAR || ceos->product == SCN) {
      meta->sar->look_count = 4;
      rcdr = (struct radio_comp_data_rec *)
	MALLOC(sizeof(struct radio_comp_data_rec));
      get_rcdr(in_fName, rcdr);
      if (rcdr->num_rec == 2)
	strcpy(beamname, "SNA");
      else if (rcdr->num_rec == 3)
	strcpy(beamname, "SNB");
      else if (rcdr->num_rec == 4) {
	// We assume a nominal center look angle of 40.45 degrees for SWA
	if (rcdr->look_angle[3] > 40.25 && rcdr->look_angle[3] < 40.65)
	  strcpy(beamname, "SWA");
	// We assume a nominla center look angle of 38.2 degrees for SWB
	else if (rcdr->look_angle[3] > 38.0 && rcdr->look_angle[3] < 38.4)
	  strcpy(beamname, "SWB");
      }
      meta->sar->image_type = 'P';
      mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
      get_mpdr(in_fName, mpdr);
      ceos_init_scansar(in_fName, meta, dssr, mpdr, NULL);
    }
    else if (ppr)
      strcpy(beamname, ppr->beam_type);
    strcpy(meta->general->mode, beamname);
    sprintf(meta->sar->polarization, "HH");
    strncpy(buf, &dssr->product_id[7], 3);
    buf[3]=0;
    sscanf(buf, "%d", &meta->general->frame);
  }
  meta->general->bit_error_rate = esa_facdr->ber;

  // SAR block
  if (ceos->product == SLC || ceos->product == RAW) {
    meta->sar->image_type = 'S';
    meta->sar->look_count = 1;
  }
  else if (ceos->product == SGF) {
    meta->sar->image_type = 'G';
    meta->sar->look_count = 1;
  }
  else if (ceos->product == SSG) {
    meta->sar->image_type = 'P';
    mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
    get_mpdr(in_fName, mpdr);
    ceos_init_proj(meta, dssr, mpdr, NULL, NULL);
    meta->sar->satellite_height = mpdr->distplat;
    meta->sar->earth_radius = mpdr->distplat - mpdr->altplat;
  }
  if (ceos->product == RAW)
    meta->sar->deskewed = 0;
  else
    meta->sar->deskewed = 1;
  meta->sar->original_sample_count = iof->datgroup;
  meta->sar->slant_range_first_pixel = dssr->rng_gate
    * get_units(dssr->rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
  meta->sar->slant_shift = 0;
  meta->sar->time_shift = 0;
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
  if (rcdr)
    FREE(rcdr);
  if (mpdr)
    FREE(mpdr);
  FREE(ppr);
  FREE(esa_facdr);
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
  get_esa_facdr(in_fName, esa_facdr);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS", 3) == 0) {
    strcpy(meta->general->mode, "STD");
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
  if (ceos->product == PRI)
    meta->sar->image_type = 'G';
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_time[0]*speedOfLight/2000.0;
  meta->sar->time_shift = 0;
  date_dssr2time(dssr->az_time_first, &date, &time);
  firstTime = date_hms2sec(&time);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel = (centerTime - firstTime)
    / (meta->sar->original_line_count/2);  
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
			  meta->general->line_count/2,
			  meta->general->sample_count/2);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
			meta->general->line_count/2,
			meta->general->sample_count/2);
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = // two-way range time
    dssr->crt_dopcen[1] / (speedOfLight * 2);
  meta->sar->range_doppler_coefficients[2] = // two-way range time
    dssr->crt_dopcen[2] / (speedOfLight * speedOfLight * 4);

  // Location block
  if (ceos->product != RAW)
    ceos_init_location_block(meta);
}

// Only deal with ALOS data
void ceos_init_sar_eoc(ceos_description *ceos, const char *in_fName, 
		       meta_parameters *meta)
{
  struct dataset_sum_rec *dssr=NULL;
  struct VMPDREC *mpdr=NULL;
  ymd_date date;
  hms_time time;
  char buf[50];
  double firstTime, centerTime;
  char **dataName=NULL, **metaName=NULL;
  int nBands, trailer;
  
  dssr = &ceos->dssr;
  mpdr = (struct VMPDREC*) MALLOC(sizeof(struct VMPDREC));
  get_mpdr(in_fName, mpdr);
  require_ceos_pair(in_fName, &dataName, &metaName, &nBands, &trailer);

  // General block
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
    firstTime = get_alos_firstTime(dataName[0]);
    date_dssr2date(dssr->inp_sctim, &date, &time);
    centerTime = date_hms2sec(&time);
    meta->sar->azimuth_time_per_pixel = (centerTime - firstTime)
      / (meta->sar->original_line_count/2);
    ceos_init_stVec(in_fName,ceos,meta);
  }

  // For ALOS data, the doppler centroid fields are all zero.
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
          "  %f, %f, %f\n"
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
          "Proceeding, marking the data as georeferenced only.\n",
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
  meta->sar->deskewed = 1;
  meta->sar->slant_range_first_pixel = dssr->rng_gate
    * get_units(dssr->rng_gate,EXPECTED_RANGEGATE) * speedOfLight / 2.0;
  meta->sar->slant_shift = 0;
  meta->sar->time_shift = 0;
  meta->sar->range_doppler_coefficients[0] = dssr->crt_dopcen[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_dopcen[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_dopcen[2];
  meta->sar->satellite_height = mpdr->distplat + mpdr->altplat;
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

  if (ceos->product == SGI) {
    // experimental calculation -- not sure if this is really going to work!

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
    const double g = 9.81;
    double orbit_vel = sqrt(g*er*er/ht);

    // simple scaling to get swath velocity from orbit vel
    double swath_vel = orbit_vel * er / ht;

    // calculate azimuth time per pixel from the swath velocity
    meta->sar->azimuth_time_per_pixel =
        meta->general->y_pixel_size / swath_vel;

    // for comparison, calculate using the workreport file (old method)
    double delta, workreport_atpp=-1;
    if (get_alos_delta_time (in_fName, &delta))
        workreport_atpp = delta / meta->sar->original_line_count;
    
    asfPrintStatus("\nAzimuth Time Per Pixel Calculation:\n");
    if (workreport_atpp > 0)
        asfPrintStatus("  Using workreport: %.10f\n", workreport_atpp);
    asfPrintStatus("        Calculated: %.10f\n\n", meta->sar->azimuth_time_per_pixel);

    ceos_init_stVec(in_fName,ceos,meta);
  } 


  // Transformation block
  if (ceos->product != SLC) {
    if (!meta->transform)
      meta->transform = meta_transform_init();
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
  else
    meta_get_corner_coords(meta);

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
  struct PPREC *ppr=NULL;
  char beamname[32], beamtype[32], buf[50];
  char **dataName=NULL, **metaName=NULL;
  ymd_date date;
  hms_time time;
  double firstTime, centerTime;
  int nBands, trailer;

  dssr = &ceos->dssr;
  ppr = (struct PPREC*) MALLOC(sizeof(struct PPREC));
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
  else if (ppr)
    strcpy(beamname, ppr->beam_type);
  strcpy(meta->general->mode, beamname);
  sprintf(meta->sar->polarization, "HH");
  strncpy(buf, &dssr->product_id[7], 4);
  buf[3] = 0;
  meta->general->frame = atoi(buf);
  if (meta->general->line_count == 0 || meta->general->sample_count == 0) {
    meta->general->line_count   = dssr->sc_lin*2;
    meta->general->sample_count = dssr->sc_pix*2;
  }
  meta->general->bit_error_rate = 0;

  // SAR block
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
  meta->sar->time_shift = 0;
  meta->sar->range_doppler_coefficients[0] = dssr->crt_rate[0];
  meta->sar->range_doppler_coefficients[1] = dssr->crt_rate[1];
  meta->sar->range_doppler_coefficients[2] = dssr->crt_rate[2];
  meta->sar->earth_radius =
    meta_get_earth_radius(meta,
			  meta->general->line_count/2,
			  meta->general->sample_count/2);
  meta->sar->satellite_height =
    meta_get_sat_height(meta,
			meta->general->line_count/2,
			meta->general->sample_count/2);

  // Initialize map projection for projected images
  if (meta->sar->image_type=='P' && mpdr)
    ceos_init_proj(meta, dssr, mpdr, NULL, NULL);
  
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
  meta->sar->time_shift = 0;
  date_dssr2time(dssr->az_time_first, &date, &time);
  firstTime = date_hms2sec(&time);
  date_dssr2time(dssr->az_time_center, &date, &time);
  centerTime = date_hms2sec(&time);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel = 
    (centerTime - firstTime) / (meta->sar->original_line_count/2);
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
  double firstTime, centerTime;
  char buf[50];

  dssr = &ceos->dssr;
  esa_facdr = (struct ESA_FACDR*) MALLOC(sizeof(struct ESA_FACDR));
  get_esa_facdr(in_fName, esa_facdr);

  // General block
  ceos_init_sar_general(ceos, in_fName, meta);

  if (strncmp(dssr->mission_id, "ERS", 3) == 0) {
    strcpy(meta->general->mode, "STD");
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
  meta->sar->time_shift = 0;
  date_dssr2time(dssr->az_time_first, &date, &time);
  firstTime = date_hms2sec(&time);
  date_dssr2time(dssr->az_time_center, &date, &time);
  centerTime = date_hms2sec(&time);
  date_dssr2date(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel = 
    (centerTime - firstTime) / (meta->sar->original_line_count/2);
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
  meta->sar->time_shift = 0;
  firstTime = get_firstTime(in_fName);
  date_dssr2time(dssr->inp_sctim, &date, &time);
  centerTime = date_hms2sec(&time);
  meta->sar->azimuth_time_per_pixel =
    (centerTime - firstTime) / (meta->sar->original_line_count/2);
  ceos_init_stVec(in_fName,ceos,meta);
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
  strcpy(meta->general->sensor, ceos->shr.mission_id);
  strtok(meta->general->sensor, " "); // Remove spaces from field
  if (ceos->sensor == AVNIR)
    sprintf(meta->general->sensor_name,"AVNIR");
  else if (ceos->sensor == PRISM)
    sprintf(meta->general->sensor_name,"PRISM");
  if (ceos->sensor == AVNIR || ceos->sensor == PRISM) {
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

  // processor ???
  meta->general->data_type = BYTE;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  strcpy(meta->general->system, meta_get_system());
  if (ceos->shr.sc_time[0] == ' ')
    date_shr2date_stamp(ceos->shr.acq_date, meta->general->acquisition_date);
  else {
    if (ceos->facility == BEIJING)
      date_dssr2time(ceos->dssr.inp_sctim, &date, &time);
    else
      date_dssr2date(ceos->dssr.inp_sctim, &date, &time);
    date_dssr2time_stamp(&date, &time, meta->general->acquisition_date);
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
  // bit_error_rate
  // missing_lines
  // no_data

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
  stateVector st_start;
  meta_projection *projection;

  if (asf_facdr) {
    meta->sar->earth_radius = asf_facdr->eradcntr*1000.0;
    meta->sar->satellite_height = meta->sar->earth_radius + asf_facdr->scalt*1000;
  }

  projection = (meta_projection *)MALLOC(sizeof(meta_projection));
  meta->projection = projection;

  projection->type = SCANSAR_PROJECTION;
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

    projection->startX = 0.0;
    projection->startY = 0.0;
    projection->perX = meta->general->x_pixel_size;
    projection->perY = -meta->general->y_pixel_size;
  }

  projection->param.atct.rlocal =
    meta_get_earth_radius(meta, meta->general->line_count/2, 0);
  st_start = meta_get_stVec(meta,0.0);
  fixed2gei(&st_start,0.0);/*Remove earth's spin JPL's AT/CT projection
           requires this*/
  atct_init(meta->projection,st_start);

  strcpy(projection->units,"meters");
  projection->hem = (dssr->pro_lat>0.0) ? 'N' : 'S';
  projection->re_major = dssr->ellip_maj*1000;
  projection->re_minor = dssr->ellip_min*1000;
  projection->height = 0.0;
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
       projection->param.ps.slat=70.0;
       projection->param.ps.slon=-45.0;
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
       // might need to have a check for ALOS coordinates - look like km, not m
       projection->startY = mpdr->tlcnorth*1000;
       projection->startX = mpdr->tlceast*1000;
       //projection->perY   = (mpdr->blcnorth - mpdr->tlcnorth) * 1000 / mpdr->nlines;
       //projection->perX   = (mpdr->trceast - mpdr->tlceast) * 1000 / mpdr->npixels;
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
  char *sensorStr,*prodStr,*procStr;
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
      asfReport(level,"get_ceos_description Warning! "
		"Unknown satellite '%s'!\n", satStr);
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
    ceos->processor = unknownProcessor;
    ceos->product = unknownProduct;
    
    /*Determine the facility that processed the data.*/
    if (0==strncmp(ceos->dssr.fac_id,"ASF",3))
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
	else {
	  asfReport(level, "get_ceos_description Warning! "
		    "Unknown ASF processor '%s'!\n", procStr);
	  ceos->processor = unknownProcessor;
	}
	
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
    else if (0==strncmp(ceos->dssr.fac_id,"ES",2))
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
    else if (0==strncmp(ceos->dssr.fac_id,"CDPF",4))
      {
	asfReport(level, "   Data set processed by CDPF\n");
	ceos->facility = CDPF;
	
	if (0==strncmp(prodStr,"SPECIAL PRODUCT(SINGL-LOOK COMP)",32))
	  ceos->product = SLC;
	else if (0==strncmp(prodStr,"SCANSAR WIDE",12)) 
	  ceos->product = SCANSAR;
	else if (0==strncmp(prodStr, "SAR GEOREF FINE",15)) 
	  ceos->product = SGF;
	else if (0==strncmp(prodStr, "SYSTEMATIC GEOCODED UTM", 23)) 
	  ceos->product = SSG;
	else {
	  asfReport(level, "Get_ceos_description Warning! "
		      "Unknown CDPF product type '%s'!\n", prodStr);
	  ceos->product = unknownProduct;
	}
      }
    else if (0==strncmp(ceos->dssr.fac_id,"D-PAF",5)) {
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
    else if (0==strncmp(ceos->dssr.fac_id,"I-PAF",5)) {
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
    else if (0==strncmp(ceos->dssr.fac_id,"EOC",3)) {
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
    else if (0==strncmp(ceos->dssr.fac_id,"RSI",3)) {
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
      else {
	asfReport(level, "Get_ceos_description Warning! "
		    "Unknown RSI product type '%s'!\n", prodStr);
	ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(ceos->dssr.fac_id,"JPL",3)) {
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
    else if (0==strncmp(ceos->dssr.fac_id,"CSTARS",6)) {
      asfReport(level, "   Data set processed by CSTARS\n");
      ceos->facility = CSTARS;
      if (0==strncmp(procStr,"FOCUS",5)) 
	ceos->processor = FOCUS;
      if (0==strncmp(prodStr, "SCANSAR NARROW", 14)) 
	ceos->product = SCN;
      else {
	asfReport(level, "Get_ceos_description Warning! "
		       "Unknown CSTARS product type '%s'!\n", prodStr);
	ceos->product = unknownProduct;
      }
    }
    else if (0==strncmp(ceos->dssr.fac_id,"         Beijing",16)) {
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
   char buff[25600];

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct RHEADER), 1, fp);
   length = bigInt32(hdr.recsiz) - (sizeof(struct RHEADER)
            + sizeof(struct HEADER));
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct RHEADER), 1, fp);
   FCLOSE(fp);

   return (double)bigInt32((unsigned char *)&(linehdr.acq_msec))/1000.0;
}

// Function extracts the acquisition time of the first line
// out of the ALOS line header
double get_alos_firstTime (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct SHEADER linehdr;
   int length;
   char buff[25600];

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   length = bigInt32(hdr.recsiz)-12;
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
   FCLOSE(fp);

   return ((double)bigInt32(linehdr.acq_msec)/1000.0);
}

// Function that reads polarization out of the line header
char *get_polarization (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct SHEADER linehdr;
   int length;
   char buff[25600];
   char *polarization;

   polarization = (char *) MALLOC(sizeof(char)*3);

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   length = bigInt32(hdr.recsiz)-12;
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
   
   return polarization;
}

// Function that reads the band number out of the line header
int get_alos_band_number(const char *fName)
{
  FILE *fp;
  struct HEADER hdr;
  struct SHEADER linehdr;
  int length;
  char buff[25600];

  fp = FOPEN(fName, "r");
  FREAD (&hdr, sizeof(struct HEADER), 1, fp);
  length = bigInt32(hdr.recsiz)-12;
  FREAD (buff, length, 1, fp);
  FREAD (&hdr, sizeof(struct HEADER), 1, fp);
  FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
  FCLOSE(fp);

  return bigInt32(linehdr.rec_num);
}

// Function that reads chirp out of the line header
double get_chirp_rate (const char *fName)
{
   FILE *fp;
   struct HEADER hdr;
   struct SHEADER linehdr;
   int length;
   char buff[25600];

   fp = FOPEN(fName, "r");
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   length = bigInt32(hdr.recsiz)-12;
   FREAD (buff, length, 1, fp);
   FREAD (&hdr, sizeof(struct HEADER), 1, fp);
   FREAD (&linehdr, sizeof(struct SHEADER), 1, fp);
   FCLOSE(fp);

   return (double)bigInt16(linehdr.chirp_linear);
}

// Get the delta image time for ALOS data out of the summary file
int get_alos_delta_time (const char *fileName, double *delta)
{
  FILE *fp;
  struct dataset_sum_rec dssr;
  hms_time dssr_time, summary_time, start_time, end_time;
  ymd_date dssr_date, summary_date, start_date, end_date;
  char *summaryFile, line[512], dateStr[30], *str;

  get_dssr(fileName, &dssr);
  date_dssr2date(dssr.inp_sctim, &dssr_date, &dssr_time);
  summaryFile = (char *) MALLOC(sizeof(char)*(strlen(fileName)+5));
  // Assume that workreport is following the basename paradigm
  sprintf(summaryFile, "%s.txt", fileName);
  if (!fileExists(summaryFile)) {
      asfPrintWarning("Summary file '%s' not found.\nWill try 'workreport'\n",
                      summaryFile);

      // try "path/workreport"
      char *path = getPath(fileName);
      if (strlen(path) > 0)
          sprintf(summaryFile, "%s%cworkreport", path, DIR_SEPARATOR);
      else
          strcpy(summaryFile, "workreport");
      FREE(path);

      if (!fileExists(summaryFile)) {

        asfPrintWarning("Summary file '%s' does not exist.\n"
                        "If you received a 'workreport' file with this data "
                        "please make sure it is\nin the same directory as "
                        "the data file.\n",
                        summaryFile);
        FREE(summaryFile);
        *delta = 0;
        return 0;
      } else
          asfPrintStatus("Summary file 'workreport' found.\n");
  }

  fp = FOPEN(summaryFile, "r");
  while (fgets(line, 512, fp)) {
    if (strstr(line, "Img_SceneCenterDateTime")) {
      str = strchr(line, '"');
      sprintf(dateStr, "%s", str+1);
      dateStr[strlen(dateStr)-2] = '\0';
      date_alos2date(dateStr, &summary_date, &summary_time);
      if (date_difference(&dssr_date, &dssr_time,
			  &summary_date, &summary_time) > 0.0) {
	asfPrintWarning("Summary file does not correspond to leader file.\n"
                        "DSSR: %s\nSummary: %s\n", dssr.inp_sctim, dateStr);
        *delta = 0;
        FCLOSE(fp);
        FREE(summaryFile);
        return 0;
      }
    }
    else if (strstr(line, "Img_SceneStartDateTime")) {
      str = strchr(line, '"');
      sprintf(dateStr, "%s", str+1);
      dateStr[strlen(dateStr)-2] = '\0';
      date_alos2date(dateStr, &start_date, &start_time);
    }
    else if (strstr(line, "Img_SceneEndDateTime")) {
      str = strchr(line, '"');
      sprintf(dateStr, "%s", str+1);
      dateStr[strlen(dateStr)-2] = '\0';
      date_alos2date(dateStr, &end_date, &end_time);
    }
  }

  *delta = date_difference(&start_date, &start_time, &end_date, &end_time);
  FREE(summaryFile);
  FCLOSE(fp);
  return 1;
}
