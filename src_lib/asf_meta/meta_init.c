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


/* Raw_init_state: Creates a meta_state_vectors array big enough to
   hold the given number of state vectors.  */
meta_state_vectors *raw_init_state(int nState)
{
	meta_state_vectors *s;
	s=(meta_state_vectors *)MALLOC(sizeof(meta_state_vectors)
				       + nState*sizeof(state_loc));
	s->num=nState;
	return s;
}

/* Creates and initializes a meta_parameters structure, guessing at
   conceivable values.  These bogus values always end in "989", so you
   can tell them from real values.  */
meta_parameters *raw_init(void)
{
  meta_parameters *meta = MALLOC(sizeof(meta_parameters));
  meta->general         = MALLOC(sizeof(meta_general));
  meta->sar             = MALLOC(sizeof(meta_sar));
  meta->optical         = NULL;  /* Not yet in use */
  meta->thermal         = NULL;  /* Not yet in use */
  meta->projection      = MALLOC(sizeof(meta_projection));
  meta->stats           = NULL;  /* Not yet in use */
  meta->state_vectors   = MALLOC(sizeof(meta_state_vectors));
  meta->state_vectors->vecs = NULL;

  /* Fill with ludicrous values.  */
  meta->general->sensor[0] = '\0';
  meta->general->mode[0] = '\0';
  meta->general->processor[0] = '\0';
  meta->general->data_type[0] = '\0';
  meta->general->system[0] = '\0';
  meta->general->orbit = -1;
  meta->general->frame = -1;
  meta->general->orbit_direction = '\0';
  meta->general->line_count = -1;
  meta->general->sample_count = -1;
  meta->general->start_line = -1;
  meta->general->start_sample = -1;
  meta->general->x_pixel_size = NAN;
  meta->general->y_pixel_size = NAN;
  meta->general->center_latitude = NAN;
  meta->general->center_longitude = NAN;
  meta->general->re_major = NAN;
  meta->general->re_minor = NAN;
  meta->general->bit_error_rate = NAN;
  meta->general->missing_lines = -1;

  meta->geo  = NULL; /* DEPRECATED */
  meta->ifm  = NULL; /* DEPRECATED */
  meta->info = NULL; /* DEPRECATED */

/* Initialize deprecated structure elements: Creates and initializes a
   meta_parameters structure, guessing at conceivable values.  These
   bogus values always end in "989", so you can tell them from real
   values.  */

  meta->geo = MALLOC(sizeof(geo_parameters));
  meta->ifm = MALLOC(sizeof(ifm_parameters));
  meta->stVec=NULL;
  meta->info=NULL;
  
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
		return meta_read_old(fName);
	else
		return meta_create/*_old*/(fName);
}
meta_parameters *meta_init(const char *fName)
{
  meta_parameters *meta = raw_init(); /* Allocate empty metadata structure.  */

  
  return meta_init_old(fName);
}

/*  Disposes of a given metadata parameters record.  */
void meta_free(meta_parameters *meta)
{
  free(meta->general);
  free(meta->sar);
  free(meta->projection);
  /* If any state vectors were allocated, free them.  */
  if ( meta->state_vectors->num ) 
    free(meta->state_vectors->vecs);
  free(meta->state_vectors);

  /* Dispose of deprecated structure elements, if they were ever
     allocated.  */
  if (meta->geo->proj!=NULL)
    free(meta->geo->proj);
  free(meta->geo);
  free(meta->ifm);
  if (meta->stVec!=NULL)
    free(meta->stVec);
  meta->geo=NULL;
  meta->ifm=NULL;
  meta->stVec=NULL;
  free(meta);
}
