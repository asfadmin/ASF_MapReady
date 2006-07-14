/*
"meta_parameters" is a struct that represents in memory what the 
.meta file stores on disk.

Orion Sky Lawlor, olawlor@acm.org, 2006/07/13 (ASF)
*/
#include "asf_meta/meta_parameters.h"
#include "asf/caplib.h"
#include "asf/util.h"
#include "coniFetch.h"
using namespace asf;


/********************************************************
 * meta_general_init():
 * Allocate memory for and initialize elements of a meta
 * general structure */
meta_general *meta_general_init(void)
{
  meta_general *general = (meta_general *)MALLOC(sizeof(meta_general));

  /* Fill with ludicrous values.  */
  strcpy(general->sensor, MAGIC_UNSET_STRING);
  strcpy(general->mode, MAGIC_UNSET_STRING);
  strcpy(general->processor, MAGIC_UNSET_STRING);
  general->data_type = (data_type_t)MAGIC_UNSET_INT;
  general->image_data_type = (image_data_type_t)MAGIC_UNSET_INT;
  strcpy(general->system, MAGIC_UNSET_STRING);
  general->orbit = MAGIC_UNSET_INT;
  general->orbit_direction = MAGIC_UNSET_CHAR;
  general->frame = MAGIC_UNSET_INT;
  general->band_number = MAGIC_UNSET_INT;
  general->line_count = MAGIC_UNSET_INT;
  general->sample_count = MAGIC_UNSET_INT;
  general->start_line = MAGIC_UNSET_INT;
  general->start_sample = MAGIC_UNSET_INT;
  general->x_pixel_size = MAGIC_UNSET_DOUBLE;
  general->y_pixel_size = MAGIC_UNSET_DOUBLE;
  general->center_latitude = MAGIC_UNSET_DOUBLE;
  general->center_longitude = MAGIC_UNSET_DOUBLE;
  general->re_major = MAGIC_UNSET_DOUBLE;
  general->re_minor = MAGIC_UNSET_DOUBLE;
  general->bit_error_rate = MAGIC_UNSET_DOUBLE;
  general->missing_lines = MAGIC_UNSET_INT;

  return general;
}

/********************************************************
 * meta_sar_init():
 * Allocate memory for and initialize elements of a meta
 * sar structure */
meta_sar *meta_sar_init(void)
{
  meta_sar *sar = (meta_sar *)MALLOC(sizeof(meta_sar));

  /* Fill with ludicrous values.  */
  sar->image_type = MAGIC_UNSET_CHAR; 
  sar->look_direction = MAGIC_UNSET_CHAR;
  sar->look_count = MAGIC_UNSET_INT;
  sar->deskewed = MAGIC_UNSET_INT;
  sar->original_line_count = MAGIC_UNSET_INT;
  sar->original_sample_count = MAGIC_UNSET_INT;
  sar->line_increment = MAGIC_UNSET_DOUBLE;
  sar->sample_increment = MAGIC_UNSET_DOUBLE;
  sar->range_time_per_pixel = MAGIC_UNSET_DOUBLE;
  sar->azimuth_time_per_pixel = MAGIC_UNSET_DOUBLE;
  sar->slant_shift = MAGIC_UNSET_DOUBLE;
  sar->time_shift = MAGIC_UNSET_DOUBLE;
  sar->slant_range_first_pixel = MAGIC_UNSET_DOUBLE;
  sar->wavelength = MAGIC_UNSET_DOUBLE;
  sar->prf = MAGIC_UNSET_DOUBLE;
  sar->earth_radius = MAGIC_UNSET_DOUBLE;
  sar->earth_radius_pp = MAGIC_UNSET_DOUBLE;
  sar->satellite_height = MAGIC_UNSET_DOUBLE;
  strcpy(sar->satellite_binary_time, MAGIC_UNSET_STRING);
  strcpy(sar->satellite_clock_time, MAGIC_UNSET_STRING);
  sar->range_doppler_coefficients[0] = MAGIC_UNSET_DOUBLE;
  sar->range_doppler_coefficients[1] = MAGIC_UNSET_DOUBLE;
  sar->range_doppler_coefficients[2] = MAGIC_UNSET_DOUBLE;
  sar->azimuth_doppler_coefficients[0] = MAGIC_UNSET_DOUBLE; 
  sar->azimuth_doppler_coefficients[1] = MAGIC_UNSET_DOUBLE; 
  sar->azimuth_doppler_coefficients[2] = MAGIC_UNSET_DOUBLE; 

  return sar;
}

/********************************************************
 * meta_projection_init():
 * Allocate memory for and initialize elements of a meta
 * projection structure */
meta_projection *meta_projection_init(void)
{
  meta_projection *projection = (meta_projection *)MALLOC(sizeof(meta_projection));
  projection->type = (projection_type_t)MAGIC_UNSET_INT;
  projection->startX = MAGIC_UNSET_DOUBLE;
  projection->startY = MAGIC_UNSET_DOUBLE;
  projection->perX = MAGIC_UNSET_DOUBLE;
  projection->perY = MAGIC_UNSET_DOUBLE;
  strcpy (projection->units, MAGIC_UNSET_STRING);
  projection->hem = MAGIC_UNSET_CHAR;
  projection->re_major = MAGIC_UNSET_DOUBLE;
  projection->re_minor = MAGIC_UNSET_DOUBLE;
  projection->height = 0.0;
/*  projection->ecc = MAGIC_UNSET_DOUBLE;        * DEPRECATED */
/* Can't really initalize projection->param to a dummy value, so just leave it.*/
  return projection;
}

/*******************************************************************************
 * meta_state_vectors_init():
 * Allocate memory for and initialize elements of a meta_state_vectors structure.
 * This one is a little hairy as it REQUIRES that the 'vecs' element be an array
 * rather than a pointer in order to initialize correctly. This puts the entire
 * state vectors block (including vecs) in one block of memory, which is how our
 * client code expects it to be.  */
meta_state_vectors *meta_state_vectors_init(int vector_count)
{
  meta_state_vectors *state_vectors;
  int ii=0;
  state_vectors = (meta_state_vectors *)MALLOC(  sizeof(meta_state_vectors)
                                               + vector_count * sizeof(state_loc)
                                              );
  /* Fill with ludicrous values.  */
  state_vectors->year = MAGIC_UNSET_INT;
  state_vectors->julDay = MAGIC_UNSET_INT;
  state_vectors->second = MAGIC_UNSET_DOUBLE;
  state_vectors->vector_count = vector_count;
  state_vectors->num = state_vectors->vector_count;
  for (ii=0; ii<state_vectors->vector_count; ii++) {
    state_vectors->vecs[ii].time = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.pos.x = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.pos.y = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.pos.z = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.vel.x = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.vel.y = MAGIC_UNSET_DOUBLE;
    state_vectors->vecs[ii].vec.vel.z = MAGIC_UNSET_DOUBLE;
  }
  return state_vectors;
}

/********************************************************
 * meta_stats_init():
 * Allocate memory for and initialize elements of a meta
 * stats structure */
meta_stats *meta_stats_init(void)
{
  meta_stats *stats = (meta_stats *)MALLOC(sizeof(meta_stats));
  stats->min = MAGIC_UNSET_DOUBLE;
  stats->max = MAGIC_UNSET_DOUBLE;
  stats->mean = MAGIC_UNSET_DOUBLE;
  stats->rmse = MAGIC_UNSET_DOUBLE;
  stats->std_deviation = MAGIC_UNSET_DOUBLE;
  stats->mask = MAGIC_UNSET_DOUBLE;
  return stats;
}

/*******************************************************************
 * meta_location_init():
 * Allocate memory for and initialize elements of a meta
 * location structure */
meta_location *meta_location_init(void)
{
  meta_location *location = (meta_location *)MALLOC(sizeof(meta_location));
  location->lat_start_near_range = MAGIC_UNSET_DOUBLE;
  location->lon_start_near_range = MAGIC_UNSET_DOUBLE;
  location->lat_start_far_range = MAGIC_UNSET_DOUBLE;
  location->lon_start_far_range = MAGIC_UNSET_DOUBLE;
  location->lat_end_near_range = MAGIC_UNSET_DOUBLE;
  location->lon_end_near_range = MAGIC_UNSET_DOUBLE;
  location->lat_end_far_range = MAGIC_UNSET_DOUBLE;
  location->lon_end_far_range = MAGIC_UNSET_DOUBLE;
  return location;
}

/****************************************************
 * raw_init:
 * Allocate memory for a fresh meta structure and fill
 * existing elements with bogus values */
meta_parameters *asf::meta_raw_init(void)
{
  meta_parameters *meta = (meta_parameters *)MALLOC(sizeof(meta_parameters));
  memset(meta,0,sizeof(*meta)); /* set all unused fields to NULL */
  meta->general         = meta_general_init();
  meta->sar             = meta_sar_init();
  meta->optical         = NULL;  /* Not yet in use */
  meta->thermal         = NULL;  /* Not yet in use */
  meta->projection      = NULL;  /* Allocated later if geocoded */
  meta->stats           = NULL;
  meta->state_vectors   = NULL;  /* Allocated upon discovery of state vectors */
  meta->location        = meta_location_init();  
  meta->stats           = NULL; 
  
  meta->meta_version = 1.6; // FIXME: was META_VERSION macro

  return meta;
}

void coniIO_meta    (coniStruct *coni,meta_parameters *meta);

/** Read a brand new metadata structure from this .meta file */
meta_parameters *asf::meta_read(const char *inName)
{
	meta_parameters *meta=meta_raw_init();
	coniStruct *coni=coniOpen(appendExt(inName,".meta").c_str(),asciiIn);
	coniIO_meta(coni,meta);
	coniClose(coni);
	return meta;
}

/** Write this meta_parameters object to a .meta file. */
void asf::meta_write(meta_parameters *meta,const char *outName)
{
	coniStruct *coni=coniOpen(appendExt(outName,".meta").c_str(),asciiOut);
	coniIO_meta(coni,meta);
	coniClose(coni);
}

/** Destroy and deallocate this meta_parameters object. */
void asf::meta_free(meta_parameters *meta)
{
	if (meta == NULL) return;

/* Terminate a MALLOC'd pointer with extreme prejudice */
#define KILL_OFF(pointer) \
	memset(pointer,0,sizeof(*pointer)); \
	FREE(pointer);\
	pointer=0; 

	KILL_OFF(meta->general);
	KILL_OFF(meta->sar);
	KILL_OFF(meta->optical);
	KILL_OFF(meta->thermal);
	KILL_OFF(meta->projection);
	KILL_OFF(meta->stats);
	KILL_OFF(meta->state_vectors);
	KILL_OFF(meta->location);
	KILL_OFF(meta);
}


/*************** coniIO routine: the guts of .meta I/O ****************/

/**
 Read or write this enumerated value, storing in the file a string name.
 
  \param coni Where to read or write.
  \param name The file identifier to read or write.
  \param value Points to the enum to read or write.
  \param table Maps enum values to strings.
  \param comment Human-readable description of enum's meaning.
  \param append_end  Optional string to append to values in table.
*/
void coniIO_enum   (coniStruct *coni,const char *name,int *value,
	const enum_value_description_t *table,
	const char *comment,const char *append_end=0)
{
	if (coniIsUnpacking(coni)) 
	{ /* Reading: compute value from string */
		char strVal[255];
		coniIO_fixstr(coni,name,strVal,comment);
		if (0==strcmp(strVal,"Unknown") || 0==strncmp(strVal,"?",1)) 
		{
			*value=MAGIC_UNSET_INT;
		} else {
			std::string v=strVal;
			if (append_end) v+=append_end; /* paste on enum ending */
			*value=lookup_enum_name(v.c_str(),table)->value;
		}
	} else {
	/* Writing: compute string from value */
		std::string v;
		if (*value==MAGIC_UNSET_INT) {
			v="Unknown";
		} else {
			v=lookup_enum_value(*value,table)->name;
			if (append_end) /* trim off enum ending */
				v=v.substr(0,v.size()-strlen(append_end));
		}
		coniIO_str(coni,name,(char *)v.c_str(),0,comment);
	}
}

void coniIO_meta (coniStruct *coni,meta_parameters *meta)
{
  if (!coniIsUnpacking(coni))
    coniWrite(coni,0,0,"This ASF Metadata file describes the satellite image of the same base name.");
  coniIO_double(coni,"meta_version",meta->meta_version,"Metadata formatting version used in this file\n\n");
  
/* General block.  */
  coniIO_structOpen(coni,"general","Begin parameters generally used in remote sensing");
  coniIO_fixstr(coni,"sensor", meta->general->sensor, "Imaging sensor");
  coniIO_fixstr(coni,"mode", meta->general->mode,"Imaging mode");
  coniIO_fixstr(coni,"processor", meta->general->processor,"Name and Version of Processor");
  coniIO_enum(coni,"data_type",(int *)&meta->general->data_type,
      	data_type_t_table,"Type of samples (e.g. REAL64)");
  if (meta->meta_version >= 1.2) {
    coniIO_enum(coni,"image_data_type",(int *)&meta->general->image_data_type,
      	image_data_type_t_table,"Image data type (e.g. AMPLITUDE_IMAGE)");
  }
  
  coniIO_fixstr(coni,"system", meta->general->system,
		  "System of samples (e.g. big_ieee)");
  coniIO_int   (coni,"orbit", meta->general->orbit,
		  "Orbit Number for this datatake");
  coniIO_char  (coni,"orbit_direction", meta->general->orbit_direction,
		  "Ascending 'A', or descending 'D'");
  coniIO_int   (coni,"frame", meta->general->frame,
		  "Frame for this image [-1 if n/a]");
  coniIO_int   (coni,"band_number", meta->general->band_number,
		  "Band number; first band is 0");
  coniIO_int   (coni,"line_count", meta->general->line_count,
		  "Number of lines in image");
  coniIO_int   (coni,"sample_count", meta->general->sample_count,
		  "Number of samples in image");
  coniIO_int   (coni,"start_line", meta->general->start_line,
		  "First line relative to original image");
  coniIO_int   (coni,"start_sample", meta->general->start_sample,
		  "First sample relative to original image");
  coniIO_double(coni,"x_pixel_size", meta->general->x_pixel_size,
		  "Range pixel size [m]");
  coniIO_double(coni,"y_pixel_size", meta->general->y_pixel_size,
		  "Azimuth pixel size [m]");
  coniIO_double(coni,"center_latitude", meta->general->center_latitude,
		  "Approximate image center latitude");
  coniIO_double(coni,"center_longitude", meta->general->center_longitude,
		  "Approximate image center longitude");
  coniIO_double(coni,"re_major", meta->general->re_major,
		  "Major (equator) Axis of earth [m]");
  coniIO_double(coni,"re_minor", meta->general->re_minor,
		  "Minor (polar) Axis of earth [m]");
  coniIO_double(coni,"bit_error_rate", meta->general->bit_error_rate,
		  "Fraction of bits which are in error");
  coniIO_int   (coni,"missing_lines", meta->general->missing_lines,
		  "Number of missing lines in data take");
  coniIO_structClose(coni,"general","End general\n");

/* SAR block.  */
  coniIO_structOpen(coni,"sar","Begin parameters used specifically in SAR imaging");
  coniIO_char  (coni,"image_type", meta->sar->image_type,
		  "[S=slant range; G=ground range; P=map projected]");
  coniIO_char  (coni,"look_direction",meta->sar->look_direction,
		  "SAR Satellite look direction [R=right; L=left]");
  coniIO_int   (coni,"look_count",meta->sar->look_count,
		  "Number of looks to take from SLC");
  coniIO_int   (coni,"deskewed",meta->sar->deskewed,
		  "Image moved to zero doppler? [1=yes; 0=no]");
  coniIO_int   (coni,"original_line_count",meta->sar->original_line_count,
		  "Number of lines in original image");
  coniIO_int   (coni,"original_sample_count",meta->sar->original_sample_count,
		  "Number of samples in original image");
  coniIO_double(coni,"line_increment",meta->sar->line_increment,
		  "Line increment for sampling");
  coniIO_double(coni,"sample_increment",meta->sar->sample_increment,
		  "Sample increment for sampling");
  coniIO_double(coni,"range_time_per_pixel",meta->sar->range_time_per_pixel,
		  "Time per pixel in range [s]");
  coniIO_double(coni,"azimuth_time_per_pixel",meta->sar->azimuth_time_per_pixel,
		  "Time per pixel in azimuth [s]");
  coniIO_double(coni,"slant_range_first_pixel",meta->sar->slant_range_first_pixel,
		  "Slant range to first pixel [m]");
  coniIO_double(coni,"slant_shift",meta->sar->slant_shift,
		  "Error correction factor, in slant range [m]");
  coniIO_double(coni,"time_shift",meta->sar->time_shift,
		  "Error correction factor, in time [s]");
  coniIO_double(coni,"wavelength",meta->sar->wavelength,
		  "SAR carrier wavelength [m]");
  coniIO_double(coni,"prf",meta->sar->prf,"Pulse Repetition Frequency [Hz]");
  coniIO_double(coni,"earth_radius",meta->sar->earth_radius,
		  "Earth radius at scene center [m]");
  coniIO_double(coni,"earth_radius_pp",meta->sar->earth_radius_pp,
		  "Earth radius used by the PP during L0 processsing. [m]");
  coniIO_double(coni,"satellite_height",meta->sar->satellite_height,
		  "Satellite height from earth's center [m]");
  coniIO_fixstr(coni,"satellite_binary_time",meta->sar->satellite_binary_time,
		  "Satellite Binary Time");
  coniIO_fixstr(coni,"satellite_clock_time",meta->sar->satellite_clock_time,
		  "Satellite Clock Time (UTC)");
  coniIO_double(coni,"dopRangeCen",meta->sar->range_doppler_coefficients[0],
		  "Range doppler centroid [Hz]");
  coniIO_double(coni,"dopRangeLin",meta->sar->range_doppler_coefficients[1],
		  "Range doppler per range pixel [Hz/pixel]");
  coniIO_double(coni,"dopRangeQuad",meta->sar->range_doppler_coefficients[2],
		  "Range doppler per range pixel sq. [Hz/(pixel^2)]");
  coniIO_double(coni,"dopAzCen",meta->sar->azimuth_doppler_coefficients[0],
		  "Azimuth doppler centroid [Hz]");
  coniIO_double(coni,"dopAzLin",meta->sar->azimuth_doppler_coefficients[1],
		  "Azimuth doppler per azimuth pixel [Hz/pixel]");
  coniIO_double(coni,"dopAzQuad",meta->sar->azimuth_doppler_coefficients[2],
		  "Azimuth doppler per azimuth pixel sq. [Hz/(pixel^2)]");
  if (meta->meta_version >= 1.4) {
    coniIO_double(coni,"azimuth_bandwidth",meta->sar->azimuth_processing_bandwidth,
                    "Azimuth processing bandwidth [Hz]");
    coniIO_double(coni,"chirp_rate",meta->sar->chirp_rate,
                    "Chirp rate [Hz/sec]");
    coniIO_double(coni,"pulse_duration",meta->sar->pulse_duration,
                    "Pulse duration [s]");
    coniIO_double(coni,"range_samp_rate",meta->sar->range_sampling_rate,
                    "Range sampling rate [Hz]");
  }
  coniIO_structClose(coni,"sar","End sar\n");

/* State block.  */
  if (meta->state_vectors) {
    int count=0;
    if (meta->state_vectors)
    	count=meta->state_vectors->vector_count;
    coniIO_int   (coni,"vector_count",count,
		    "Number of state vectors below");
    if (!meta->state_vectors) 
    	meta->state_vectors=meta_state_vectors_init(count);
    coniIO_structOpen(coni,"state",
		    "Begin list of state vectors for satellite, over image");
    coniIO_int   (coni,"year",meta->state_vectors->year,"Year of image start");
    coniIO_int   (coni,"julDay",meta->state_vectors->julDay,
		    "Julian day of the year for image start");
    coniIO_double(coni,"second",meta->state_vectors->second,
		    "Second of the day for image start");
    {
      int ii;
      for (ii = 0; ii < meta->state_vectors->vector_count; ii++ ) {
	coniIO_structOpen(coni,"vector","Begin a single state vector");
	coniIO_double(coni,"time",meta->state_vectors->vecs[ii].time,
			"Time, relative to image start [s]");
	coniIO_double(coni,"x",meta->state_vectors->vecs[ii].vec.pos.x,
			"X Coordinate, earth-fixed [m]");
	coniIO_double(coni,"y",meta->state_vectors->vecs[ii].vec.pos.y,
			"Y Coordinate, earth-fixed [m]");
	coniIO_double(coni,"z",meta->state_vectors->vecs[ii].vec.pos.z,
			"Z Coordinate, earth-fixed [m]");
	coniIO_double(coni,"vx",meta->state_vectors->vecs[ii].vec.vel.x,
			"X Velocity, earth-fixed [m/s]");
	coniIO_double(coni,"vy",meta->state_vectors->vecs[ii].vec.vel.y,
			"Y Velocity, earth-fixed [m/s]");
	coniIO_double(coni,"vz",meta->state_vectors->vecs[ii].vec.vel.z,
			"Z Velocity, earth-fixed [m/s]");
	coniIO_structClose(coni,"vector","End a single state vector");
      }
    }
    coniIO_structClose(coni,"state","End the list of state vectors");
  }

/* Projection parameters block, if appropriate.  */
  if ( meta->sar->image_type == 'P' ) {
    if (!meta->projection) meta->projection=meta_projection_init();
    coniIO_structOpen(coni,"projection","Map Projection parameters");
    coniIO_enum(coni,"type",(int *)&meta->projection->type,
      	projection_type_t_table,"Projection Type");
    coniIO_double(coni,"startX",meta->projection->startX,
		    "Projection Coordinate at top-left, X direction");
    coniIO_double(coni,"startY",meta->projection->startY,
		    "Projection Coordinate at top-left, Y direction");
    coniIO_double(coni,"perX",meta->projection->perX,
		    "Projection Coordinate per pixel, X direction");
    coniIO_double(coni,"perY",meta->projection->perY,
		    "Projection Coordinate per pixel, Y direction");
    coniIO_fixstr(coni,"units",meta->projection->units,
		    "Units of projection [meters, seconds, degrees]");
    coniIO_char  (coni,"hem",meta->projection->hem,
		    "Hemisphere: [N=northern hemisphere; S=southern hemisphere]");
    if (meta->meta_version >= 1.3) {
      coniIO_enum(coni,"spheroid",(int *)&meta->projection->spheroid,
      	spheroid_type_t_table,"Dimensions of the Earth","_SPHEROID");
    }
    coniIO_double(coni,"re_major",meta->projection->re_major,
		    "Major Axis (equator) of earth [m]");
    coniIO_double(coni,"re_minor",meta->projection->re_minor,
		    "Minor Axis (polar) of earth [m]");
    if (meta->meta_version >= 1.3) {
      coniIO_enum(coni,"datum",(int *)&meta->projection->datum,
      	datum_type_t_table,"Geodetic Datum","_DATUM");
    }
    if (meta->meta_version >= 1.6)
      coniIO_double(coni, "height", meta->projection->height,
		      "Height [m]");
    coniIO_structOpen(coni,"param","Projection specific parameters");
    switch ( meta->projection->type ) {
    case SCANSAR_PROJECTION: /* Along-track/cross-track projection.  */
      coniIO_structOpen(coni,"atct","Begin along-track/cross-track projection");
      coniIO_double(coni,"rlocal",meta->projection->param.atct.rlocal,
		      "Local earth radius [m]");
      coniIO_double(coni,"alpha1",meta->projection->param.atct.alpha1,
		      "First rotation angle [degrees]");
      coniIO_double(coni,"alpha2",meta->projection->param.atct.alpha2,
		      "Second rotation angle [degrees]");
      coniIO_double(coni,"alpha3",meta->projection->param.atct.alpha3,
		      "Third rotation angle [degrees]");
      coniIO_structClose(coni,"atct","End atct");
      break;
    case ALBERS_EQUAL_AREA:
      coniIO_structOpen(coni,"albers","Begin Albers Conical Equal Area projection");
      coniIO_double(coni,"std_parallel1",
		      meta->projection->param.albers.std_parallel1,
		      "First standard parallel [degrees]");
      coniIO_double(coni,"std_parallel2",
		      meta->projection->param.albers.std_parallel2,
		      "Second standard parallel [degrees]");
      coniIO_double(coni,"center_meridian",
		      meta->projection->param.albers.center_meridian,
		      "Longitude of center meridian [degrees]");
      coniIO_double(coni,"orig_latitude",
		      meta->projection->param.albers.orig_latitude,
		      "Latitude of the projection origin [degrees]");
      if (meta->meta_version >= 1.3) {
	coniIO_double(coni,"false_easting",
			meta->projection->param.albers.false_easting,
			"False easting [m]");
	coniIO_double(coni,"false_northing",
			meta->projection->param.albers.false_northing,
			"False northing [m]");
      }
      coniIO_structClose(coni,"albers","End albers");
      break;
    case LAMBERT_AZIMUTHAL_EQUAL_AREA:
      coniIO_structOpen(coni,"lamaz","Begin Lambert Azimuthal Equal Area projection");
      coniIO_double(coni,"center_lon",meta->projection->param.lamaz.center_lon,
		      "Longitude at center of projection");
      coniIO_double(coni,"center_lat",meta->projection->param.lamaz.center_lat,
		      "Latitude at center of projection");
      if (meta->meta_version >= 1.3) {
	coniIO_double(coni,"false_easting",
			meta->projection->param.lamaz.false_easting,
			"False easting [m]");
	coniIO_double(coni,"false_northing",
			meta->projection->param.lamaz.false_northing,
			"False northing [m]");
      }
      coniIO_structClose(coni,"lamaz","End albers");
      break;
    case LAMBERT_CONFORMAL_CONIC:/*Lambert conformal conic projection.*/
      coniIO_structOpen(coni,"lamcc","Begin Lambert Conformal Conic projection");
      coniIO_double(coni,"plat1",meta->projection->param.lamcc.plat1,
		      "First standard parallel");
      coniIO_double(coni,"plat2",meta->projection->param.lamcc.plat2,
		      "Second standard parallel");
      coniIO_double(coni,"lat0",meta->projection->param.lamcc.lat0,
		      "Original latitude");
      coniIO_double(coni,"lon0",meta->projection->param.lamcc.lon0,
		      "Original longitude");
      if (meta->meta_version >= 1.3) {
	coniIO_double(coni,"false_easting",
			meta->projection->param.lamcc.false_easting,
			"False easting [m]");
	coniIO_double(coni,"false_northing",
			meta->projection->param.lamcc.false_northing,
			"False northing [m]");
	coniIO_double(coni,"scale_factor", 
			meta->projection->param.lamcc.scale_factor,
			"Scaling factor");
      }
      coniIO_structClose(coni,"lamacc","End lamcc");
      break;
    case POLAR_STEREOGRAPHIC:/*Polar stereographic projection.*/
      coniIO_structOpen(coni,"ps","Begin Polar Stereographic Projection");
      coniIO_double(coni,"slat",meta->projection->param.ps.slat,"Reference Latitude");
      coniIO_double(coni,"slon",meta->projection->param.ps.slon,"Reference Longitude");
      if (meta->meta_version >= 1.3) {
	coniIO_double(coni,"false_easting",
			meta->projection->param.ps.false_easting, "False easting [m]");
	coniIO_double(coni,"false_northing",
			meta->projection->param.ps.false_northing, 
			"False northing [m]");
      }
      coniIO_structClose(coni,"ps","End ps");
      break;
    case UNIVERSAL_TRANSVERSE_MERCATOR:/*Universal transverse mercator projection.*/
      coniIO_structOpen(coni,"utm","Begin Universal Transverse Mercator Projection");
      coniIO_int   (coni,"zone",meta->projection->param.utm.zone,"Zone Code");
      if (meta->meta_version >= 1.3) {
	coniIO_double(coni,"false_easting",
			meta->projection->param.utm.false_easting,
			"False easting [m]");
	coniIO_double(coni,"false_northing",
			meta->projection->param.utm.false_northing,
			"False northing [m]");
	coniIO_double(coni,"latitude",meta->projection->param.utm.lat0,
			"Latitude [degrees]");
	coniIO_double(coni,"longitude",meta->projection->param.utm.lon0,
			"Longitude [degrees]");
	coniIO_double(coni,"scale_factor", meta->projection->param.utm.scale_factor,
			"Scaling factor");
      }
      coniIO_structClose(coni,"utm","End utm");
      break;
    case STATE_PLANE:/*State plane coordinates projection.*/
      coniIO_structOpen(coni,"state","Begin State Plane Coordinates Projection");
      coniIO_int   (coni,"zone",meta->projection->param.state.zone,"Zone Code");
      coniIO_structClose(coni,"state","End state");
      break;
    case LAT_LONG_PSEUDO_PROJECTION:
      /* This projection type doesn't need its own parameter block,
	 since all its values are specified in the main projection
	 structure.  */
      break;
    default: /* other projections have no projection data */
      break;
      /*
    default:
      printf("WARNING in asf_meta library function '%s': unknown projection type '%c'.\n",
             "meta_write", meta->projection->type);
      */
    }
    coniIO_structClose(coni,"param","End param");
    coniIO_structClose(coni,"projection","End projection\n");
  }

/* Write out statistics block */
  if (meta->stats) {
    if (!meta->stats) meta->stats=meta_stats_init();
    coniIO_structOpen(coni,"stats","Block containing basic image statistics");
    coniIO_double(coni,"min",meta->stats->min,"Minimum sample value");
    coniIO_double(coni,"max",meta->stats->max,"Maximum sample value");
    coniIO_double(coni,"mean",meta->stats->mean,"Mean average of sample values");
    coniIO_double(coni,"rmse",meta->stats->rmse,"Root mean squared error");
    coniIO_double(coni,"std_deviation",meta->stats->std_deviation,
		    "Standard deviation");
    coniIO_double(coni,"mask",meta->stats->mask,
		    "Value ignored while taking statistics");
    coniIO_structClose(coni,"stats","End stats\n");
  }

  /* Write out location block - version 1.5 *
  coniIO_structOpen(coni,"location","Block containing image corner coordinates");
  coniIO_double(coni,"lat_start_near_range",meta->location->lat_start_near_range,
		  "Latitude at image start in near range");
  coniIO_double(coni,"lon_start_near_range",meta->location->lon_start_near_range,
		  "Longitude at image start in near range");
  coniIO_double(coni,"lat_start_far_range",meta->location->lat_start_far_range,
		  "Latitude at image start in far range");
  coniIO_double(coni,"lon_start_far_range",meta->location->lon_start_far_range,
		  "Longitude at image start in far range");
  coniIO_double(coni,"lat_end_near_range",meta->location->lat_end_near_range,
		  "Latitude at image end in near range");
  coniIO_double(coni,"lon_end_near_range",meta->location->lon_end_near_range,
		  "Longitude at image end in near range");
  coniIO_double(coni,"lat_end_far_range",meta->location->lat_end_far_range,
		  "Latitude at image end in far range");
  coniIO_double(coni,"lon_end_far_range",meta->location->lon_end_far_range,
		  "Longitude at image end in far range");
  coniIO_structClose(coni,"location","End location");
  */
}



