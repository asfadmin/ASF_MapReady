/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from various
metadata files and meta_parameters structure.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"


/****************************************************
 * raw_init:
 * Allocate memory for structures that always exist and
 * fill with initial bogus value */
meta_parameters *raw_init(void)
{
  meta_parameters *meta = MALLOC(sizeof(meta_parameters));
  meta->general         = MALLOC(sizeof(meta_general));
  meta->sar             = MALLOC(sizeof(meta_sar));
  meta->optical         = NULL;  /* Not yet in use */
  meta->thermal         = NULL;  /* Not yet in use */
  meta->projection      = NULL;  /* Allocated later if geocoded */
  meta->stats           = NULL;  /* Not yet in use */
  meta->state_vectors   = MALLOC(sizeof(meta_state_vectors));
  
  meta->meta_version = META_VERSION;

  /* Fill with ludicrous values.  */
  strcpy(meta->general->sensor, "???");
  strcpy(meta->general->mode, "???");
  strcpy(meta->general->processor, "???");
  strcpy(meta->general->data_type, "???");
  strcpy(meta->general->system, "???");
  meta->general->orbit = -2147283648;
  meta->general->orbit_direction = '?';
  meta->general->frame = -2147283648;
  meta->general->line_count = -2147283648;
  meta->general->sample_count = -2147283648;
  meta->general->start_line = -2147283648;
  meta->general->start_sample = -2147283648;
  meta->general->x_pixel_size = NAN;
  meta->general->y_pixel_size = NAN;
  meta->general->center_latitude = NAN;
  meta->general->center_longitude = NAN;
  meta->general->re_major = NAN;
  meta->general->re_minor = NAN;
  meta->general->bit_error_rate = NAN;
  meta->general->missing_lines = -2147283648;

  meta->sar->image_type = '?'; 
  meta->sar->look_direction = '?';
  meta->sar->look_count = -2147283648;
  meta->sar->deskewed = -2147283648;
  meta->sar->line_increment = NAN;
  meta->sar->sample_increment = NAN;
  meta->sar->range_time_per_pixel = NAN;
  meta->sar->azimuth_time_per_pixel = NAN;
  meta->sar->slant_shift = NAN;
  meta->sar->time_shift = NAN;
  meta->sar->slant_range_first_pixel = NAN;
  meta->sar->wavelength = NAN;
  meta->sar->prf = NAN;
  strcpy(meta->sar->satellite_binary_time, "???");
  strcpy(meta->sar->satellite_clock_time, "???");
  meta->sar->range_doppler_coefficients[0] = NAN;
  meta->sar->range_doppler_coefficients[1] = NAN;
  meta->sar->range_doppler_coefficients[2] = NAN;
  meta->sar->azimuth_doppler_coefficients[0] = NAN; 
  meta->sar->azimuth_doppler_coefficients[1] = NAN; 
  meta->sar->azimuth_doppler_coefficients[2] = NAN; 

  meta->state_vectors->year = -2147283648;
  meta->state_vectors->julDay = -2147283648;
  meta->state_vectors->second = NAN;
  meta->state_vectors->vector_count = -2147283648;
  meta->state_vectors->num = -2147283648;
  meta->state_vectors->vecs = NULL;

/* Initialize deprecated structure elements: Creates and initializes a
   meta_parameters structure, guessing at conceivable values.  These
   bogus values always end in "989", so you can tell them from real
   values.  */
  meta->geo   = MALLOC(sizeof(geo_parameters));
  meta->ifm   = MALLOC(sizeof(ifm_parameters));
  meta->stVec = meta->state_vectors; /* Compatability alias.  */
  meta->info = MALLOC(sizeof(extra_info));
  
  /* Guess at conceivable values for deprecated elements.  */
  meta->geo->type = 'G';
  meta->geo->proj = NULL;
  meta->geo->lookDir = 'R';
  meta->geo->deskew = 0;
  meta->geo->xPix = meta->geo->yPix = 10.989;
  meta->geo->rngPixTime = 5.989e-8;
  meta->geo->azPixTime = 7.989e-4;
  meta->geo->timeShift = meta->geo->slantShift = 0;
  meta->geo->slantFirst = 800000.989;
  meta->geo->wavelen = 0.056989;
  meta->geo->dopRange[0] = NAN;
  meta->geo->dopRange[1] = NAN;
  meta->geo->dopRange[2] = NAN;
  meta->geo->dopAz[0] = NAN;
  meta->geo->dopAz[1] = NAN;
  meta->geo->dopAz[2] = NAN;
  
  meta->ifm->er = 6360000.989;
  meta->ifm->ht = meta->ifm->er+700000.0;
  meta->ifm->nLooks = 5;
  meta->ifm->orig_nLines = 25989;
  meta->ifm->orig_nSamples = 5989;
  meta->ifm->lookCenter = 19.989;
  
  return meta;
}

/*meta_init_old: 
	Reads in a new meta_parameters record from
disk with the given filename.  If no .meta
exists, it calls create_meta to construct 
one.
*/
#ifndef WIN32
#include "unistd.h"/*For getpid()*/
#endif
meta_parameters *meta_init_old(const char *fName)
{
	if (extExists(fName,".meta")) /*Read .meta file if possible*/
		return meta_read(fName);
	else
		return meta_create(fName);
}
meta_parameters *meta_init(const char *fName)
{
  return meta_init_old(fName);
}

/*  Disposes of a given metadata parameters record.  */
void meta_free(meta_parameters *meta)
{
  free(meta->general);
  free(meta->sar);
  if ( meta->projection != NULL ) 
    free(meta->projection);
  /* If any state vectors were allocated, free them.  */
  if ( meta->state_vectors->vector_count ) 
    free(meta->state_vectors->vecs);
  free(meta->state_vectors);

  /* Dispose of deprecated structure elements that are not alias for
     new elements.  */
  free(meta->geo);
  free(meta->ifm);
  meta->geo=NULL;
  meta->ifm=NULL;
  meta->stVec=NULL;
  free(meta);
}
