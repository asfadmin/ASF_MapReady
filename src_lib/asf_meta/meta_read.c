#include "asf.h"
#include "coniFetch.h"
#include "asf_meta.h"
#include "err_die.h"
#include "regex_wrapper.h"
#include "metadata_parser.h"

void meta_read_old(meta_parameters *meta, char *fileName);
void meta_new2old(meta_parameters *meta);

/***************************************************************
 * meta_read:
 * Reads a meta file and returns a meta structure filled with
 * both old backward compatability and new fields filled in.
 * Note that the appropriate extension is appended to the given
 * base name automagically if needed.  */
meta_parameters *meta_read(const char *inName)
{
 /* Maximum line length.  */
#define MAX_METADATA_LINE 1024 
 /* Version where new metadata was adopted.  */
#define NEW_FORMAT_VERSION 1.0 

  char              *meta_name      = appendExt(inName,".meta");
  FILE              *meta_file      = FOPEN(meta_name, "r");
  meta_parameters   *meta           = raw_init();	/* To be filled.  */
  char              line[MAX_METADATA_LINE]; /* Metadata line.  */
  /* For pattern matching for version string.  */
  matched_subexps_t version_subexps = MATCHED_SUBEXPS_INITIALIZER;
	
  
  /* Scan for the version string.  */
  if ( fgets(line, MAX_METADATA_LINE, meta_file) == '\0' ) {
    err_die("%s function: metadata file is empty\n", __func__);
  }
  while ( !regex_match(&version_subexps, line,
		       "^[[:space:]]*meta_version[[:space:]]*:[[:space:]]*([[:digit:]]+(\\.[[:digit:]]+)?)"
		       ) ) {
    if ( fgets(line, MAX_METADATA_LINE, meta_file) == '\0' ) {
      err_die("meta_read function: didn't find Meta version field\n");
    }
  }
  FCLOSE(meta_file);   /* Done using meta file directly.  */

  /* Read file with appropriate reader for version.  */
  if ( strtod(get_subexp_string(&version_subexps, 1), NULL)
       < NEW_FORMAT_VERSION ) {
    meta_read_old(meta, meta_name);
  } else {
    parse_metadata(meta, meta_name);
  }
  matched_subexps_free(&version_subexps);   /* Done with matches.  */
  
  /* Fill old structure parameters */
  meta_new2old(meta);
  
  free(meta_name);
  
  return meta;
}

/***************************************************************
 * meta_io_state:
 * Called by meta_io, below, this routine reads/writes
 * the given state vector structure.
 */
void meta_io_state(coniStruct *coni, meta_state_vectors *state)
{
	int ii;
	coniIO_structOpen(coni,"state {","begin list of state vectors for satellite, over image");
	coniIO_int   (coni,"state.","year:",  &state->year,        "Year of image start");
	coniIO_int   (coni,"state.","day:",   &state->julDay,      "Julian day of the year for image start");
	coniIO_double(coni,"state.","second:",&state->second,      "Second of the day for image start");
	coniIO_int   (coni,"state.","number:",&state->vector_count,"Number of state vectors below");
/* Fill state->num for backwards compatibility */
	state->num = state->vector_count;
	for (ii=0; ii<state->vector_count; ii++)
	{
		coniIO_structOpen(coni,"vector {","begin a single state vector");
		coniIO_double(coni,"state.vector.","time:",&state->vecs[ii].time,     "Time, relative to image start [s]");
		coniIO_double(coni,"state.vector.","x:",   &state->vecs[ii].vec.pos.x,"X Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","y:",   &state->vecs[ii].vec.pos.y,"Y Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","z:",   &state->vecs[ii].vec.pos.z,"Z Coordinate, earth-fixed [m]");
		coniIO_double(coni,"state.vector.","vx:",  &state->vecs[ii].vec.vel.x,"X Velocity, earth-fixed [m/s]");
		coniIO_double(coni,"state.vector.","vy:",  &state->vecs[ii].vec.vel.y,"Y Velocity, earth-fixed [m/s]");
		coniIO_double(coni,"state.vector.","vz:",  &state->vecs[ii].vec.vel.z,"Z Velocity, earth-fixed [m/s]");
		coniIO_structClose(coni,"end vector");
	}
	coniIO_structClose(coni,"end of list of state vectors\n");
}

/***************************************************************
 * meta_read_old:
 * Reads in old style meta file and fills new style meta struct
 * Note that the appropriate extension is appended to the given
 * base name automagically.  */
void meta_read_old(meta_parameters *meta, char *fileName)
{
	int reading = 1;
	char *ddrName = appendExt(fileName,".ddr");
	struct DDR ddr;
	char *metaName = appendExt(fileName,".meta");
	meta_general *general = meta->general;
	meta_sar     *sar     = meta->sar = (meta_sar *)MALLOC(sizeof(meta_sar));
	meta_stats   *stats   = meta->stats;
	coniStruct   *coni    = coniOpen(metaName, asciiIn);

	coniIO_double(coni,"","meta_version:",&meta->meta_version,"ASF APD Metadata File.\n");

/*Geolocation parameters.*/
	coniIO_structOpen(coni,"geo {","begin parameters used in geolocating the image.");
	coniIO_char(coni,"geo.","type:",&sar->image_type,"Image type: [S=slant range; G=ground range; P=map projected]");
	if (sar->image_type=='P') {
	/*Projection Parameters.*/
		meta_projection *projection = meta->projection = (meta_projection *)MALLOC(sizeof(meta_projection));
		coniIO_structOpen(coni,"proj {","Map Projection parameters");
		coniIO_char  (coni,"geo.proj.","type:",  &projection->type,  "Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
		coniIO_double(coni,"geo.proj.","startX:",&projection->startX,"Projection Coordinate at top-left, X direction");
		coniIO_double(coni,"geo.proj.","startY:",&projection->startY,"Projection Coordinate at top-left, Y direction");
		coniIO_double(coni,"geo.proj.","perX:",  &projection->perX,  "Projection Coordinate per pixel, X direction");
		coniIO_double(coni,"geo.proj.","perY:",  &projection->perY,  "Projection Coordinate per pixel, Y direction");
		coniIO_char  (coni,"geo.proj.","hem:",   &projection->hem,   "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
		if (meta->meta_version>0.8) {
		/*Read geoid from file*/
			coniIO_double(coni,"geo.proj.","re_major:",&projection->re_major,"Major (equator) Axis of earth (meters)");
			coniIO_double(coni,"geo.proj.","re_minor:",&projection->re_minor,"Minor (polar) Axis of earth (meters)");
		} else {
		/*Default: use the GEM-06 (Goddard Earth Model 6) Ellipsoid*/
			projection->re_major=6378144.0;
			projection->re_minor=6356754.9;
		}
		switch(projection->type) {
			case 'A':/*Along-track/cross-track projection.*/
				coniIO_double(coni,"geo.proj.","rlocal:",     &projection->param.atct.rlocal,"Local earth radius [m]");
				coniIO_double(coni,"geo.proj.","atct_alpha1:",&projection->param.atct.alpha1,"at/ct projection parameter");
				coniIO_double(coni,"geo.proj.","atct_alpha2:",&projection->param.atct.alpha2,"at/ct projection parameter");
				coniIO_double(coni,"geo.proj.","atct_alpha3:",&projection->param.atct.alpha3,"at/ct projection parameter");
				break;
			case 'L':/*Lambert Conformal Conic projection.*/
				coniIO_double(coni,"geo.proj.","lam_plat1:",&projection->param.lambert.plat1,"Lambert first standard parallel");
				coniIO_double(coni,"geo.proj.","lam_plat2:",&projection->param.lambert.plat2,"Lambert second standard parallel");
				coniIO_double(coni,"geo.proj.","lam_lat:",  &projection->param.lambert.lat0, "Lambert original latitude");
				coniIO_double(coni,"geo.proj.","lam_lon:",  &projection->param.lambert.lon0, "Lambert original longitude");
				break;
			case 'P':/*Polar Stereographic Projection.*/
				coniIO_double(coni,"geo.proj.","ps_lat:",&projection->param.ps.slat,"Polar Stereographic reference Latitude");
				coniIO_double(coni,"geo.proj.","ps_lon:",&projection->param.ps.slon,"Polar Stereographic reference Longitude");
				break;
			case 'U':/*Universal Trasnverse Mercator Projection.*/
				coniIO_int(coni,"geo.proj.","utm_zone:",&projection->param.utm.zone,"UTM Zone Code");
				break;
			default:
				printf("ERROR! Unrecognized map projection code '%c!'\n",projection->type);
				exit(1);
		}
		coniIO_structClose(coni,"end proj");
	}
	coniIO_char(coni,"geo.","lookDir:",       &sar->look_direction,         "SAR Satellite look direction (normally R) [R=right; L=left]");
	coniIO_int(coni,"geo.","deskew:",         &sar->deskewed,               "Image moved to zero doppler? [1=yes; 0=no]");
	coniIO_double(coni,"geo.","xPix:",        &general->x_pixel_size,       "Pixel size in X direction [m]");
	coniIO_double(coni,"geo.","yPix:",        &general->y_pixel_size,       "Pixel size in Y direction [m]");
	coniIO_double(coni,"geo.","rngPixTime:",  &sar->range_time_per_pixel,   "Time/pixel, range (xPix/(c/2.0), or 1/fs) [s]");
	coniIO_double(coni,"geo.","azPixTime:",   &sar->azimuth_time_per_pixel, "Time/pixel, azimuth (yPix/swathVel, or 1/prf) [s]");
	coniIO_double(coni,"geo.","slantShift:",  &sar->slant_shift,            "Error correction factor, in slant range [m]");
	coniIO_double(coni,"geo.","timeShift:",   &sar->time_shift,             "Error correction factor, in time [s]");
	coniIO_double(coni,"geo.","slantFirst:",  &sar->slant_range_first_pixel,"Slant range to first image pixel [m]");
	coniIO_double(coni,"geo.","wavelength:",  &sar->wavelength,             "SAR Carrier Wavelength [m]");
	coniIO_double(coni,"geo.","dopRangeCen:", &sar->range_doppler_coefficients[0],  "Doppler centroid [Hz]");
	coniIO_double(coni,"geo.","dopRangeLin:", &sar->range_doppler_coefficients[1],  "Doppler per range pixel [Hz/pixel]");
	coniIO_double(coni,"geo.","dopRangeQuad:",&sar->range_doppler_coefficients[2],  "Doppler per range pixel sq. [Hz/(pixel^2)]");
	coniIO_double(coni,"geo.","dopAzCen:",    &sar->azimuth_doppler_coefficients[0],"Doppler centroid [Hz]");
	coniIO_double(coni,"geo.","dopAzLin:",    &sar->azimuth_doppler_coefficients[1],"Doppler per azimuth pixel [Hz/pixel]");
	coniIO_double(coni,"geo.","dopAzQuad:",   &sar->azimuth_doppler_coefficients[2],"Doppler per azimuth pixel sq. [Hz/(pixel^2)]");
	coniIO_structClose(coni,"end geo\n");

/*Interferometry parameters:*/
	coniIO_structOpen(coni,"ifm {","begin interferometry-related parameters");
	if (meta->meta_version>0.6)
		coniIO_int(coni,"ifm.","nLooks:",&sar->look_count,      "Number of looks to take from SLC");
	coniIO_int(coni,"ifm.","orig_lines:",    &general->line_count,  "Number of lines in original image");
	coniIO_int(coni,"ifm.","orig_samples:",  &general->sample_count,"Number of samples in original image");
	coniIO_structClose(coni,"end ifm\n");

/*State Vectors:*/
	{ /*Check to see if the state vectors even exist.*/
	    int err,nVec;
	    nVec=coniInt(coni,"state.number:",&err);
	    coniReopen(coni);/*Seek back to beginning of file.*/
	    if (err==CONI_OK && nVec!=0) {
	    /*We have state vectors!*/
		    meta->state_vectors->vecs=(state_loc *)MALLOC(nVec * sizeof(state_loc)); /*Allocate state vectors.*/
		    meta_io_state(coni, meta->state_vectors);  /*And initialize them.*/
	    }
	}

/*Extra Info:*/
	{ /* Check to see if extra info exists.*/
	    int err;
	    coniStr(coni,"extra.sensor:",&err);
	    coniReopen(coni);/*Seek back to beginning of file.*/
	    if (err==CONI_OK) {
		coniIO_str     (coni,"extra.","sensor:",       general->sensor,           "Imaging sensor");
		if (meta->meta_version>0.7)
		  coniIO_str   (coni,"extra.","mode:",         general->mode,             "Imaging mode");
		if (meta->meta_version>0.6)
		  coniIO_str   (coni,"extra.","processor:",    general->processor,        "Name & Version of SAR Processor");
		if (meta->meta_version>0.7) {
 		  coniIO_int   (coni,"extra.","orbit:",       &general->orbit,            "Orbit Number for this datatake");
		  coniIO_double(coni,"extra.","bitErrorRate:",&general->bit_error_rate,   "Bit Error Rate");
		  coniIO_str   (coni,"extra.","satBinTime:",   sar->satellite_binary_time,"Satellite Binary Time");
		  coniIO_str   (coni,"extra.","satClkTime:",   sar->satellite_clock_time, "Satellite Clock Time (UTC)");
		  coniIO_double(coni,"extra.","prf:",         &sar->prf,                  "Pulse Repition Frequency");
		}
	    }
	}
	
/* Info from ddr */
	c_getddr(ddrName, &ddr);
	strcpy( general->system,  ddr.system);
	general->start_line     = ddr.master_line;
	general->start_sample   = ddr.master_sample;
	if (sar->image_type=='P')
		{strcpy(meta->projection->units, ddr.proj_units);}
	switch ( ddr.dtype ) {
	    case 0: /* BYTE */
	    case 1: strcpy(general->data_type, "BYTE"); break;
	    case 2: strcpy(general->data_type, "INTEGER*2"); break;
	    case 3: strcpy(general->data_type, "INTEGER*4"); break;
	    case 4: strcpy(general->data_type, "REAL*4"); break;
	    case 5: strcpy(general->data_type, "REAL*8"); break;
	    default:
	        printf("ERROR: Unrecognized data type identifier: %d\n",ddr.dtype);
		exit(1);
	}

/* stats structure is not yet in use */
/*	stats->max                = NAN;
 *	stats->min                = NAN;
 *	stats->mean               = NAN;
 *	stats->rms                = NAN;
 *	stats->std_deviation      = NAN; */

/* Fields not yet filled */
	general->re_major = (meta->projection) ? meta->projection->re_major : 6378144.0;
	general->re_minor = (meta->projection) ? meta->projection->re_minor : 6356754.9;
	
/* Fields that cannot be filled from the old structures */
	general->frame            = -1;
	general->band_number      = -1;
	general->orbit_direction  = '\0';
	general->center_latitude  = NAN;
	general->center_longitude = NAN;
	sar->look_angle           = NAN;

/* Close coni structure */
	coniClose(coni);

} /* End pre-1.0 version read */


/***************************************************************
 * meta_new2old:
 * Fills in old meta structures using existing new structures
 * so that both old and new structures are populated. */
void meta_new2old(meta_parameters *meta)
{
  /* 'meta->stVec is now 'meta->state_vectors, but some code still
     looks for the old name. */
  meta->stVec = meta->state_vectors;

/* Fill geo_parameters structure */
	meta->geo->type        = meta->sar->image_type;
	/* Projection structure is the same for both old and new */
	meta->geo->proj        = meta->projection;
	meta->geo->lookDir     = meta->sar->look_direction;
	meta->geo->deskew      = meta->sar->deskewed;
	meta->geo->xPix        = meta->general->x_pixel_size;
	meta->geo->yPix        = meta->general->y_pixel_size;
	meta->geo->rngPixTime  = meta->sar->range_time_per_pixel;
	meta->geo->azPixTime   = meta->sar->azimuth_time_per_pixel;
	meta->geo->timeShift   = meta->sar->time_shift;
	meta->geo->slantShift  = meta->sar->slant_shift;
	meta->geo->slantFirst  = meta->sar->slant_range_first_pixel;
	meta->geo->wavelen     = meta->sar->wavelength;
	meta->geo->dopRange[0] = meta->sar->range_doppler_coefficients[0];
	meta->geo->dopRange[1] = meta->sar->range_doppler_coefficients[1];
	meta->geo->dopRange[2] = meta->sar->range_doppler_coefficients[2];
	meta->geo->dopAz[0]    = meta->sar->azimuth_doppler_coefficients[0];
	meta->geo->dopAz[1]    = meta->sar->azimuth_doppler_coefficients[1];
	meta->geo->dopAz[2]    = meta->sar->azimuth_doppler_coefficients[2];

/* Fill ifm_parameters structure */
	meta->ifm->ht            = meta_get_sat_height(meta, meta->general->line_count/2, 0);
	meta->ifm->er            = meta_get_earth_radius(meta, meta->general->line_count/2, 0);
	meta->ifm->nLooks        = meta->sar->look_count;
	meta->ifm->orig_nLines   = meta->general->line_count;
	meta->ifm->orig_nSamples = meta->general->sample_count;

/* Allocate and fill extra_info structure */
	if (meta->info == NULL)
		{meta->info = MALLOC(sizeof(extra_info));}
	strcpy( meta->info->sensor,     meta->general->sensor);
	strcpy( meta->info->mode,       meta->general->mode);
	strcpy( meta->info->processor,  meta->general->processor);
	meta->info->orbit             = meta->general->orbit;
	meta->info->bitErrorRate      = meta->general->bit_error_rate;
	strcpy( meta->info->satBinTime, meta->sar->satellite_binary_time);
	strcpy( meta->info->satClkTime, meta->sar->satellite_clock_time);
	meta->info->prf               = meta->sar->prf;

/* Calculated values for the old structure */
	if (meta->sar->image_type!='P') /*Image not map projected-- compute look angle to beam center*/
		meta->ifm->lookCenter = meta_look(meta, 0, meta->ifm->orig_nSamples/2);
	else 
	{/*Image *is* map projected-- compute earth's eccentricity*/
		double re = meta->general->re_major;
		double rp = meta->general->re_minor;
		meta->geo->proj->ecc = sqrt(1.0-rp*rp/(re*re));
	}

	return;
}
