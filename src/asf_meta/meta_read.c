#include "asf.h"
#include "coniFetch.h"
#include "asf_meta.h"
#include "asf_nan.h" /* Needed for asf_meta's MAGIC_UNSET_DOUBLE */
#include "err_die.h"
#include "metadata_parser.h"
#include "worgen.h"
#include "earth_radius2datum.h"

/* Local prototypes */
void meta_read_old(meta_parameters *meta, char *fileName);
void meta_read_only_ddr(meta_parameters *meta, const char *ddr_name);
int meta_is_new_style(const char *file_name);
meta_projection *meta_projection_init(void);

/* Prototypes from meta_init.c */
void add_meta_ddr_struct(const char *name, meta_parameters *meta, struct DDR *ddr);
meta_state_vectors *meta_state_vectors_init(int vector_count);

/* Prototype from unpacked_deg */
double unpacked_deg(double angle);

/***************************************************************
 * meta_read:
 * Reads a meta file and returns a meta structure filled with
 * both old backward compatability and new fields filled in.
 * Note that the appropriate extension is appended to the given
 * base name automagically if needed.  */
meta_parameters *meta_read(const char *inName)
{
  char              *meta_name      = appendExt(inName,".meta");
  char              *ddr_name       = appendExt(inName,".ddr");
  meta_parameters   *meta           = raw_init(); /* Allocate and initialize basic structs */

  /* Read file with appropriate reader for version.  */
  if ( !fileExists(meta_name) && fileExists(ddr_name)) {

    meta_read_only_ddr(meta, ddr_name);
/*    printf("WARNING: * Unable to locate '%s';\n"
           "         * Using only '%s' for meta data;\n"
           "         * Errors due to lack of meta data are very likely.\n",
	   meta_name, ddr_name);*/
  }
  else if ( fileExists(meta_name) ) {
    if ( !meta_is_new_style(meta_name) ) {
      meta_read_old(meta, meta_name);
    }
    else {
      parse_metadata(meta, meta_name);
    }
  }

  /* Remember the name and location of the meta struct */
  //add_meta_ddr_struct(inName, meta, NULL);

  FREE(ddr_name);
  FREE(meta_name);
  
  return meta;
}

/***********************************************************
 * meta_is_new_style:
 * Does the metadata file with the given base name exist and
 * conform to the new all-in-one standard?  */
int meta_is_new_style(const char *file_name)
{
 /* Maximum line length.  */
#define MAX_METADATA_LINE 1024 
 /* Version where new metadata was adopted.  */
#define NEW_FORMAT_VERSION 1.0 
  int return_value;		/* Value to be returned.  */
  char *meta_name = appendExt(file_name, ".meta");
  FILE *meta_file = FOPEN(meta_name, "r");
  char              line[MAX_METADATA_LINE]; /* Metadata line.  */
  char version_string[MAX_METADATA_LINE]; /* Actual version string.  */

  /* Scan to the version field and read the actual version number,
     then return the appropriate response.  */
  int return_count = 0;
  char *end_ptr;		/* Used by strtod.  */
  double version;		/* Version as floating point.  */

  if (meta_file != NULL) {
    do {
      if ( fgets(line, MAX_METADATA_LINE, meta_file) == NULL ) {
        err_die("%s function: didn't find Meta version field\n",
                "meta_is_new_style");
      }
      /* Note: whitespace in a scanf conversion matches zero or more
         white space characters, %s matches non-white-space.  */
    } while ( (return_count = sscanf (line, " meta_version: %s \n", 
				      version_string)) != 1 );
    version = strtod (version_string, &end_ptr);
    if ( *end_ptr != '\0' ) {
        err_die ("%s function: error parsing Meta vesion field\n",
              "meta_is_new_style");
    }
    if ( strtod(version_string, &end_ptr)
        < NEW_FORMAT_VERSION - 0.0002 /* <-- for sloppy float compare.  */ ) {
      return_value = 0;
    } else {
      return_value = 1;
    }

    FCLOSE(meta_file);            /* Done using meta file directly.  */
  }
  free(meta_name);		/* Done with file name with extension.  */

  return return_value;
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
	char *ddrName = appendExt(fileName,".ddr");
	struct DDR ddr;
	char *metaName = appendExt(fileName,".meta");
	meta_general *general = meta->general;
	meta_sar     *sar     = meta->sar;
/*	meta_stats   *stats   = meta->stats;*/
	coniStruct   *coni    = coniOpen(metaName, asciiIn);

/* Fields that cannot be filled from the old structures */
	general->frame            = 0;
	general->band_count       = 1;
	general->center_latitude  = MAGIC_UNSET_DOUBLE;
	general->center_longitude = MAGIC_UNSET_DOUBLE;
	general->missing_lines    = -999999999;

/* Read old style meta file */
	coniIO_double(coni,"","meta_version:",&meta->meta_version,"ASF Metadata File.\n");

/*Geolocation parameters.*/
	coniIO_structOpen(coni,"geo {","begin parameters used in geolocating the image.");
	coniIO_char(coni,"geo.","type:",&sar->image_type,"Image type: [S=slant range; G=ground range; P=map projected]");
	if (sar->image_type=='P') {
	/*Projection Parameters.*/
		char projection_type[256];
		meta_projection *projection = meta->projection = meta_projection_init();
		coniIO_structOpen(coni,"proj {","Map Projection parameters");
		coniIO_str  (coni,"geo.proj.","type:",  projection_type,  "Projection Type: [U=utm; P=ps; L=Lambert; A=at/ct]");
		if ( !strcmp(projection_type, "UNIVERSAL_TRANSVERSE_MERCATOR") ) 
			projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
		else if ( !strcmp(projection_type, "POLAR_STEREOGRAPHIC") ) 
			projection->type = POLAR_STEREOGRAPHIC;
		else if ( !strcmp(projection_type, "ALBERS_EQUAL_AREA") )
			projection->type = ALBERS_EQUAL_AREA;
		else if ( !strcmp(projection_type, "LAMBERT_CONFORMAL_CONIC") ) 
			projection->type = LAMBERT_CONFORMAL_CONIC;
		else if ( !strcmp(projection_type, "LAMBERT_AZIMUTHAL_EQUAL_AREA") ) 
			projection->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
		else if ( !strcmp(projection_type, "STATE_PLANE") ) 
			projection->type = STATE_PLANE;
		else if ( !strcmp(projection_type, "SCANSAR_PROJECTION") ) 
			projection->type = SCANSAR_PROJECTION;
		else if ( !strcmp(projection_type, "U") ) 
			projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
		else if ( !strcmp(projection_type, "P") ) 
			projection->type = POLAR_STEREOGRAPHIC;
		else if ( !strcmp(projection_type, "L") ) 
			projection->type = LAMBERT_CONFORMAL_CONIC;
		else if ( !strcmp(projection_type, "A") ) 
			projection->type = SCANSAR_PROJECTION;
		else if ( !strcmp(projection_type, "G") ) 
			projection->type = LAT_LONG_PSEUDO_PROJECTION;
		else  projection->type = -1;
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
		case SCANSAR_PROJECTION:
		  coniIO_double(coni,"geo.proj.","rlocal:",     &projection->param.atct.rlocal,"Local earth radius [m]");
		  coniIO_double(coni,"geo.proj.","atct_alpha1:",&projection->param.atct.alpha1,"at/ct projection parameter");
		  coniIO_double(coni,"geo.proj.","atct_alpha2:",&projection->param.atct.alpha2,"at/ct projection parameter");
		  coniIO_double(coni,"geo.proj.","atct_alpha3:",&projection->param.atct.alpha3,"at/ct projection parameter");
		  break;
		case LAMBERT_CONFORMAL_CONIC:
		  coniIO_double(coni,"geo.proj.","lam_plat1:",&projection->param.lamcc.plat1,"Lambert first standard parallel");
		  coniIO_double(coni,"geo.proj.","lam_plat2:",&projection->param.lamcc.plat2,"Lambert second standard parallel");
		  coniIO_double(coni,"geo.proj.","lam_lat:",  &projection->param.lamcc.lat0, "Lambert original latitude");
		  coniIO_double(coni,"geo.proj.","lam_lon:",  &projection->param.lamcc.lon0, "Lambert original longitude");
		  break;
		case POLAR_STEREOGRAPHIC:
		  coniIO_double(coni,"geo.proj.","ps_lat:",&projection->param.ps.slat,"Polar Stereographic reference Latitude");
		  coniIO_double(coni,"geo.proj.","ps_lon:",&projection->param.ps.slon,"Polar Stereographic reference Longitude");
		  break;
		case UNIVERSAL_TRANSVERSE_MERCATOR:
		  coniIO_int(coni,"geo.proj.","utm_zone:",&projection->param.utm.zone,"UTM Zone Code");
		  break;
		case LAT_LONG_PSEUDO_PROJECTION:
		  break;
		/*
		default:
		  printf("ERROR! Unrecognized map projection code '%c!'\n",projection->type);
		  exit(1);
		*/
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
	coniIO_double(coni,"ifm.","er:",&sar->earth_radius,"Local earth radius [m]");
	coniIO_double(coni,"ifm.","ht:",&sar->satellite_height,"Satellite height, from center of earth [m]");
	if (meta->meta_version>0.6)
		coniIO_int(coni,"ifm.","nLooks:",&sar->look_count,           "Number of looks to take from SLC");
	coniIO_int(coni,"ifm.","orig_lines:",    &sar->original_line_count,  "Number of lines in original image");
	coniIO_int(coni,"ifm.","orig_samples:",  &sar->original_sample_count,"Number of samples in original image");
	coniIO_structClose(coni,"end ifm\n");

/*State Vectors:*/
	{ /*Check to see if the state vectors even exist.*/
	    int err,nVec;
	    nVec=coniInt(coni,"state.number:",&err);
	    coniReopen(coni);/*Seek back to beginning of file.*/
	    if (err==CONI_OK && nVec!=0) {
	    /*We have state vectors!*/
		    meta->state_vectors = meta_state_vectors_init(nVec); /*Allocate state vectors.*/
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
		  coniIO_double(coni,"extra.","prf:",         &sar->prf,                  "Pulse Repetition Frequency");
		}
	    }
	}
	
/* Info from ddr (if its there) */
	if (fileExists(ddrName)) {
		c_getddr(ddrName, &ddr);
		general->line_count     = ddr.nl;
		general->sample_count   = ddr.ns;
		general->start_line     = ddr.master_line - 1;
		general->start_sample   = ddr.master_sample - 1;
		sar->line_increment     = ddr.line_inc;
		sar->sample_increment   = ddr.sample_inc;
		if (0==strcmp(ddr.system,"ieee-std"))
			strcpy(meta->general->system,"big_ieee");
		else if (0==strcmp(ddr.system,"ieee-lil"))
			strcpy(meta->general->system,"lil_ieee");
		else if (0==strcmp(ddr.system,"cray-unicos"))
			strcpy(meta->general->system,"cray_float");
		else /* "ibm-mvs" or "other-msc" */
			strcpy(meta->general->system,MAGIC_UNSET_STRING);
		if (sar->image_type=='P')
			{strcpy(meta->projection->units, ddr.proj_units);}
		switch ( ddr.dtype ) {
		    case 0: /* BYTE */
		    case DTYPE_BYTE:    general->data_type = BYTE;      break;
		    case DTYPE_SHORT:   general->data_type = INTEGER16; break;
		    case DTYPE_LONG:    general->data_type = INTEGER32; break;
		    case DTYPE_FLOAT:   general->data_type = REAL32;    break;
		    case DTYPE_DOUBLE:  general->data_type = REAL64;    break;
		    case DTYPE_COMPLEX: general->data_type = COMPLEX_REAL32; break;
		    default:
	        	printf("ERROR in meta_read_old(): Unrecognized DDR data type: (code %d)... Exit program.\n",ddr.dtype);
			exit(EXIT_FAILURE);
		}
	}
/*	else {
		printf("\n"
		       "WARNING: * Failed to get DDR file while reading old style metadata.\n"
		       "         * Some meta fields will not be correctly initialized.\n");
	}*/

/* Fields not yet filled */
	general->orbit_direction  = MAGIC_UNSET_CHAR;
	if (meta->state_vectors) {
		if (meta->state_vectors->vecs[0].vec.vel.z > 0)
			general->orbit_direction  = 'A';
		else if (meta->state_vectors->vecs[0].vec.vel.z < 0)
			general->orbit_direction  = 'D';
	}
	general->re_major = (meta->projection) ? meta->projection->re_major : 6378144.0;
	general->re_minor = (meta->projection) ? meta->projection->re_minor : 6356754.9;
	
/* Close coni structure */
	coniClose(coni);

} /* End pre-1.1 version read */


#include "proj.h"
/***************************************************************
 * meta_read_only_ddr:
 * Fills all possible fields that ddr can fill. All other fields
 * remain in their 'invalid' state */
void meta_read_only_ddr(meta_parameters *meta, const char *ddr_name)
{
	struct DDR ddr;
	c_getddr(ddr_name, &ddr);
	
        ddr2meta(&ddr, meta);
} /* End function meta_read_only_ddr() */

/****************************************************************
 * ddr2meta
 * Convert the values in a DDR struct into an already-allocated
 * and raw_init()-ed metadata structure. */
void ddr2meta(struct DDR *ddr, meta_parameters *meta)
{
	int ii=0;
	meta->general->line_count = ddr->nl;
	meta->general->sample_count = ddr->ns;
	meta->general->band_count = 1;
	meta->general->start_line = ddr->master_line - 1;
	meta->general->start_sample = ddr->master_sample - 1;
	switch (ddr->dtype) {
	  case 0:/*Equivalent to DTYPE_BYTE*/
	  case DTYPE_BYTE:    meta->general->data_type = BYTE;      break;
	  case DTYPE_SHORT:   meta->general->data_type = INTEGER16; break;
	  case DTYPE_LONG:    meta->general->data_type = INTEGER32; break;
	  case DTYPE_FLOAT:   meta->general->data_type = REAL32;    break;
	  case DTYPE_DOUBLE:  meta->general->data_type = REAL64;    break;
	  case DTYPE_COMPLEX: meta->general->data_type = COMPLEX_REAL32; break;
	  default:
		printf("ERROR in meta_read_only_ddr(): Unrecognized DDR data type (%d)... Exit program.\n",ddr->dtype);
		exit (EXIT_FAILURE);
	}
	if (strcmp(ddr->system,"ieee-std")==0)
		strcpy(meta->general->system,"big_ieee");
	else if (strcmp(ddr->system,"ieee-lil")==0)
		strcpy(meta->general->system,"lil_ieee");
	else if (strcmp(ddr->system,"cray-unicos")==0)
		strcpy(meta->general->system,"cray_float");
	else
		strcpy(meta->general->system,MAGIC_UNSET_STRING);
	if (ddr->valid[DDPDV] == VALID) {
		meta->general->x_pixel_size = ddr->pdist_x;
		meta->general->y_pixel_size = ddr->pdist_y;
	}
	if (ddr->valid[DDINCV] == VALID && meta->sar != NULL) {
		meta->sar->line_increment = ddr->line_inc;
		meta->sar->sample_increment = ddr->sample_inc;
	}
    /* Projection stuff */
	for (ii=0; ii<DDNVAL; ii++) {
	  if ((ddr->valid[ii]==VALID) && (ii!=DDINCV)) {
		meta->projection = meta_projection_init();
		/* Projection units */
		if (ddr->valid[DDPUV] == VALID)
			strncpy(meta->projection->units, ddr->proj_units, 12);
		else
			strcpy(meta->projection->units, "meters");
		meta->projection->hem = MAGIC_UNSET_CHAR;
		if (ddr->valid[DDZCV] == VALID)
		  datum2earth_radius(ddr->datum_code, &meta->projection->re_major, &meta->projection->re_minor);
		else {
		  meta->projection->re_major = MAGIC_UNSET_DOUBLE;
		  meta->projection->re_minor = MAGIC_UNSET_DOUBLE;
		}
		if ((ddr->valid[DDPCV]==VALID) && (ddr->valid[DDPPV]==VALID)) {
		   switch (ddr->proj_code) {
		     case ALBERS:
		        meta->projection->type = ALBERS_EQUAL_AREA;
			if (ddr->proj_coef[2]) {
				meta->projection->re_major = ddr->proj_coef[0];
				meta->projection->re_minor = ddr->proj_coef[0];
			}
			else {
				meta->projection->re_major = 6370997;
				meta->projection->re_minor = 6370997;
			}
		        meta->projection->param.albers.std_parallel1 = unpacked_deg(ddr->proj_coef[2]);
		        meta->projection->param.albers.std_parallel2 = unpacked_deg(ddr->proj_coef[3]);
		        meta->projection->param.albers.center_meridian  = unpacked_deg(ddr->proj_coef[4]);
		        meta->projection->param.albers.orig_latitude  = unpacked_deg(ddr->proj_coef[5]);
		        break;
		     case LAMCC:
		        meta->projection->type = LAMBERT_CONFORMAL_CONIC;
			if (ddr->proj_coef[2]) {
				meta->projection->re_major = ddr->proj_coef[0];
				meta->projection->re_minor = ddr->proj_coef[0];
			}
			else {
				meta->projection->re_major = 6370997;
				meta->projection->re_minor = 6370997;
			}
		        meta->projection->param.lamcc.plat1 = unpacked_deg(ddr->proj_coef[2]);
		        meta->projection->param.lamcc.plat2 = unpacked_deg(ddr->proj_coef[3]);
		        meta->projection->param.lamcc.lon0  = unpacked_deg(ddr->proj_coef[4]);
		        meta->projection->param.lamcc.lat0  = unpacked_deg(ddr->proj_coef[5]);
		        break;
		     case PS:
		        meta->projection->type = POLAR_STEREOGRAPHIC;
		        meta->projection->re_major      = ddr->proj_coef[0];
		        meta->projection->re_minor      = ddr->proj_coef[1];
		        meta->projection->param.ps.slon = unpacked_deg(ddr->proj_coef[4]);
		        meta->projection->param.ps.slat = unpacked_deg(ddr->proj_coef[5]);
		        break;
		     case UTM:
		        meta->projection->type = UNIVERSAL_TRANSVERSE_MERCATOR;
		        if (ddr->valid[DDZCV] == VALID)
		          meta->projection->param.utm.zone = ddr->zone_code;
		        else
		          meta->projection->param.utm.zone = MAGIC_UNSET_INT;
		        break;
		     default:
			meta->projection->type = MAGIC_UNSET_CHAR;
		        printf("WARNING: DDR projection code '%d' not supported by meta file\n",ddr->proj_code);
		        break;
		   } /* End switch(ddr->proj_code) */
		} /* End if (ddr->valid[DDPPV] == VALID) */
		if (ddr->valid[DDCCV] == VALID) {
			meta->projection->startY = ddr->upleft[0];
			meta->projection->startX = ddr->upleft[1];
                        meta->projection->perY = (ddr->loright[0] - ddr->upleft[0]) / (double)(ddr->nl-1);
                        meta->projection->perX = (ddr->loright[1] - ddr->upleft[1]) / (double)(ddr->ns-1);
		}
		break;
	  } /* End if ((ddr->valid[ii]==VALID) && (ii!=DDINCV)) */
	} /* End for (ii=0; ii<DDNVAL; ii++) */
    /* Make some guesses */
	if (meta->projection && (meta->projection->type != MAGIC_UNSET_CHAR) &&
            meta->sar)
		meta->sar->image_type = 'P';
} 



/***************************************************************
 * meta_new2old:
 * Fills in old meta structures using existing new structures
 * so that both old and new structures are populated. */
void meta_new2old(meta_parameters *meta)
{
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
/** Do this when meta->sar->earth_radius & meta->sar->satellite_height go away**
 *	if (meta->state_vectors && meta_is_valid_char(meta->sar->image_type)) {
 *	    meta->ifm->ht = meta_get_sat_height(meta,
 *                                              meta->general->line_count/2, 0);
 *	    meta->ifm->er = meta_get_earth_radius(meta,
 *                                              meta->general->line_count/2, 0);
 *	}
 *	else {
 *	    meta->ifm->ht = MAGIC_UNSET_DOUBLE;
 *	    meta->ifm->er = MAGIC_UNSET_DOUBLE;
 *	}
 */
	meta->ifm->ht            = meta->sar->satellite_height;
	meta->ifm->er            = meta->sar->earth_radius;
	meta->ifm->nLooks        = meta->sar->look_count;
	meta->ifm->orig_nLines   = meta->sar->original_line_count;
	meta->ifm->orig_nSamples = meta->sar->original_sample_count;

/* point meta->stVec at meta->state_vectors */
	meta->stVec = meta->state_vectors;

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

/* DEPRICATED (& not used in client code)
 * -- Calculated values for the old structure --
 *	if (meta->sar->image_type!='P')
 *	{ *Image not map projected-- compute look angle to beam center*
 *		if (meta->state_vectors)
 *			meta->ifm->lookCenter = meta_look(meta, 0, meta->general->sample_count/2);
 *		else
 *			meta->ifm->lookCenter = MAGIC_UNSET_DOUBLE;
 *	}
 *	else 
 *	{ *Image *is* map projected-- compute earth's eccentricity*
 *		double re = meta->general->re_major;
 *		double rp = meta->general->re_minor;
 *		meta->geo->proj->ecc = sqrt(1.0-rp*rp/(re*re));
 *	}
 */
	return;
}
