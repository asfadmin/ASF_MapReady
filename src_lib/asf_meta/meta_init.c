/****************************************************************
FUNCTION NAME:

DESCRIPTION:

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"

META_DDR_STRUCT meta_ddr_structs[NUM_META_DDR_STRUCTS] = {
	{ "", NULL, NULL},
	{ "", NULL, NULL},
	{ "", NULL, NULL}
};

meta_general       *meta_general_init(void);
meta_sar           *meta_sar_init(void);
meta_projection    *meta_projection_init(void);
meta_state_vectors *meta_state_vectors_init(int vector_count);


/********************************************************
 * meta_general_init():
 * Allocate memory for and initialize elements of a meta
 * general structure */
meta_general *meta_general_init(void)
{
  meta_general *general = (meta_general *)MALLOC(sizeof(meta_general));

  /* Fill with ludicrous values.  */
  strcpy(general->sensor, "???");
  strcpy(general->mode, "???");
  strcpy(general->processor, "???");
  general->data_type = -999999999;
  strcpy(general->system, "???");
  general->orbit = -999999999;
  general->orbit_direction = '?';
  general->frame = -999999999;
  general->band_number = -999999999;
  general->line_count = -999999999;
  general->sample_count = -999999999;
  general->start_line = -999999999;
  general->start_sample = -999999999;
  general->x_pixel_size = NAN;
  general->y_pixel_size = NAN;
  general->center_latitude = NAN;
  general->center_longitude = NAN;
  general->re_major = NAN;
  general->re_minor = NAN;
  general->bit_error_rate = NAN;
  general->missing_lines = -999999999;

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
  sar->image_type = '?'; 
  sar->look_direction = '?';
  sar->look_count = -999999999;
  sar->deskewed = -999999999;
  sar->original_line_count = -999999999;
  sar->original_sample_count = -999999999;
  sar->line_increment = NAN;
  sar->sample_increment = NAN;
  sar->range_time_per_pixel = NAN;
  sar->azimuth_time_per_pixel = NAN;
  sar->slant_shift = NAN;
  sar->time_shift = NAN;
  sar->slant_range_first_pixel = NAN;
  sar->wavelength = NAN;
  sar->prf = NAN;
  strcpy(sar->satellite_binary_time, "???");
  strcpy(sar->satellite_clock_time, "???");
  sar->range_doppler_coefficients[0] = NAN;
  sar->range_doppler_coefficients[1] = NAN;
  sar->range_doppler_coefficients[2] = NAN;
  sar->azimuth_doppler_coefficients[0] = NAN; 
  sar->azimuth_doppler_coefficients[1] = NAN; 
  sar->azimuth_doppler_coefficients[2] = NAN; 

  return sar;
}

/********************************************************
 * meta_projection_init():
 * Allocate memory for and initialize elements of a meta
 * projection structure */
meta_projection *meta_projection_init(void)
{
  meta_projection *projection = (meta_projection *)MALLOC(sizeof(meta_projection));
  projection->type = '?';
  projection->startX = NAN;
  projection->startY = NAN;
  projection->perX = NAN;
  projection->perY = NAN;
  strcpy (projection->units, "???");
  projection->hem = '?';
  projection->re_major = NAN;
  projection->re_minor = NAN;
  projection->ecc = NAN;        /* DEPRECATED */
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
  state_vectors->year = -999999999;
  state_vectors->julDay = -999999999;
  state_vectors->second = NAN;
  state_vectors->vector_count = vector_count;
  state_vectors->num = state_vectors->vector_count;
  for (ii=0; ii<state_vectors->vector_count; ii++) {
    state_vectors->vecs[ii].time = NAN;
    state_vectors->vecs[ii].vec.pos.x = NAN;
    state_vectors->vecs[ii].vec.pos.y = NAN;
    state_vectors->vecs[ii].vec.pos.z = NAN;
    state_vectors->vecs[ii].vec.vel.x = NAN;
    state_vectors->vecs[ii].vec.vel.y = NAN;
    state_vectors->vecs[ii].vec.vel.z = NAN;
  }
  return state_vectors;
}

/****************************************************
 * raw_init:
 * Allocate memory for a fresh meta structure and fill
 * existing elements with bogus values */
meta_parameters *raw_init(void)
{
  meta_parameters *meta = (meta_parameters *)MALLOC(sizeof(meta_parameters));
  meta->general         = meta_general_init();
  meta->sar             = meta_sar_init();
  meta->optical         = NULL;  /* Not yet in use */
  meta->thermal         = NULL;  /* Not yet in use */
  meta->projection      = NULL;  /* Allocated later if geocoded */
  meta->stats           = NULL;  /* Not yet in use */
  meta->state_vectors   = NULL;  /* Allocated upon discovery of state vectors */
  
  meta->meta_version = META_VERSION;

/* Initialize deprecated structure elements: Creates and initializes a
   meta_parameters structure, guessing at conceivable values.  These
   bogus values always end in "989", so you can tell them from real
   values.  */
  meta->geo   = MALLOC(sizeof(geo_parameters));
  meta->ifm   = MALLOC(sizeof(ifm_parameters));
  meta->stVec = meta->state_vectors; /* Compatability alias.  */
  meta->info  = NULL;
  
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

/******************************************************
 * meta_init_old: 
 * Reads in a new meta_parameters record from disk with
 * the given filename.  If no .meta exists, it calls
 * meta_create to construct one.  
 * THIS SHOULD EVENTUALLY BE REMOVED AND raw_init SHOULD
 * TAKE ITS NAME   */
meta_parameters *meta_init(const char *fName)
{
  if (extExists(fName,".meta")) /*Read .meta file if possible*/
    return meta_read(fName);
  else
    return meta_create(fName);
}

/************************************************************
 * add_meta_ddr_struct:
 * Adds or updates meta & ddr pointers associated with a
 * particular file name. If you are adding a meta pointer,
 * it expects NULL in the ddr argument, and visa versa for
 * a ddr addition/update. Hey, I know this ain't the best
 * system to keep track of different metadata structures.
 * How about YOU write a better one! */
void add_meta_ddr_struct(const char *name, meta_parameters *meta, struct DDR *ddr)
{
  int ii;
  char base_name[1024];
/* Put name in base_name but without the extention */
  create_name(base_name, name, "");
/* First check to see if this filename already has a structure in it */
  for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++)
  {
    if (0==strcmp(meta_ddr_structs[ii].base_name, base_name)) {
      if (meta) meta_ddr_structs[ii].meta = meta;
      if (ddr) meta_ddr_structs[ii].ddr = ddr;
      return;
    }
  }
/* Otherwise look for an empty slot and fill it */
  for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++)
  {
    if (0==strcmp(meta_ddr_structs[ii].base_name, "")) {
      strcpy(meta_ddr_structs[ii].base_name, base_name);
      meta_ddr_structs[ii].meta = meta;
      meta_ddr_structs[ii].ddr = ddr;
      return;
    }
  }
/* Report if we made it here */
  printf("\nWARNING: function add_meta_ddr_struct failed in its duties.\n"
         "           Metadata may not be properly updated when written to file.\n");
}

/************************************************************
 * get_meta_ddr_struct_index:
 * Find the meta/ddr pointers associated with a particular
 * file name */
int get_meta_ddr_struct_index(const char *name)
{
  int ii;
  char base_name[1024];
/* Put name in base_name but without the extention */
  create_name(base_name, name, "");
  for (ii=0; ii<NUM_META_DDR_STRUCTS; ii++)
  {
    if (0==strcmp(meta_ddr_structs[ii].base_name, base_name)) {
      return ii;
    }
  }
  return -1;
}


/****************************************************
 * meta_free:
 * Disposes of a given metadata parameters record.  */
void meta_free(meta_parameters *meta)
{
  if (meta != NULL) {
    FREE(meta->general);
    meta->general = NULL;
    FREE(meta->sar);
    meta->sar = NULL;
    FREE(meta->optical);
    meta->optical = NULL;
    FREE(meta->thermal);
    meta->thermal = NULL;
    FREE(meta->projection);
    meta->projection = NULL;
    FREE(meta->stats);
    meta->stats = NULL;
    FREE(meta->state_vectors);
    meta->state_vectors = NULL;

    FREE(meta->geo);
    meta->geo = NULL;
    FREE(meta->ifm);
    meta->ifm = NULL;
    meta->stVec = NULL;
    FREE(meta->info);
    meta->info = NULL;

    FREE(meta);
    meta = NULL;
  }
}
