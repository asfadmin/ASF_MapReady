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
#include "asf_nan.h" /* needed for MAGIC_UNSET_DOUBLE */

META_DDR_STRUCT meta_ddr_structs[NUM_META_DDR_STRUCTS] = {
	{ "", NULL, NULL},
	{ "", NULL, NULL},
	{ "", NULL, NULL}
};

meta_general       *meta_general_init(void);
meta_sar           *meta_sar_init(void);
meta_projection    *meta_projection_init(void);
meta_state_vectors *meta_state_vectors_init(int vector_count);
meta_stats         *meta_stats_init(void);


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
  general->data_type = MAGIC_UNSET_INT;
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
  projection->type = MAGIC_UNSET_CHAR;
  projection->startX = MAGIC_UNSET_DOUBLE;
  projection->startY = MAGIC_UNSET_DOUBLE;
  projection->perX = MAGIC_UNSET_DOUBLE;
  projection->perY = MAGIC_UNSET_DOUBLE;
  strcpy (projection->units, MAGIC_UNSET_STRING);
  projection->hem = MAGIC_UNSET_CHAR;
  projection->re_major = MAGIC_UNSET_DOUBLE;
  projection->re_minor = MAGIC_UNSET_DOUBLE;
  projection->ecc = MAGIC_UNSET_DOUBLE;        /* DEPRECATED */
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
  meta->geo->dopRange[0] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopRange[1] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopRange[2] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopAz[0] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopAz[1] = MAGIC_UNSET_DOUBLE;
  meta->geo->dopAz[2] = MAGIC_UNSET_DOUBLE;
  
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
