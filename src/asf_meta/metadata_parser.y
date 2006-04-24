/* Parser for ASF image metadata files.  */

%{

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asf_meta.h"
#include "caplib.h"
#include "lex_yacc.h"

/* We don't always have strdup() around.  */
static char *
strdup (const char *s)
{
  size_t ii = 0;
  while ( s[ii] != '\0' ) ii++;

  char *ret = malloc ((ii + 1) * sizeof (char));
  return strcpy (ret, s);
}

/* Lex provides this parser function.  */
int yylex(void);

/* Node type for stack of pointers to structure subelements.  */
typedef struct block_stack_node_struct {
  char block_name[MAX_SYMBOL_STRING + 1]; /* Name of block.  */
  void *block;                  /* Pointer to corresponding (sub)structure.  */
  struct block_stack_node_struct *next;
} block_stack_node;

/* Top of the push down stack.  */
block_stack_node *stack_top;

/* Global pointer to meta structure getting read in */
meta_parameters *global_meta;

static void block_stack_push(block_stack_node **stack_top_p,
                             const char *block_name, void *new_block)
{
  block_stack_node *new_node = malloc(sizeof(block_stack_node));
  assert(strlen (block_name) <= MAX_SYMBOL_STRING);
  strncpy(new_node->block_name, block_name, MAX_SYMBOL_STRING + 1);
  new_node->block = new_block;
  new_node->next = *stack_top_p;
  *stack_top_p = new_node;
}

static void *block_stack_pop(block_stack_node **stack_top_p)
{
  void *ret_val = (*stack_top_p)->block;
  block_stack_node *old_top = *stack_top_p;
  *stack_top_p = (*stack_top_p)->next;
  free(old_top);
  return ret_val;
}

/* Arrays of vectors are stored in the metadata structure, this keeps
   track of how many of them we have seen so far.  */
static int vector_count;
/* allocation routine for meta_state_vectors */
meta_state_vectors *meta_state_vectors_init(int num_of_vectors);


char current_file[MAX_FILE_NAME];
extern int line_number;         /* Line number of file being parsed.  */

/* Help parser handle errors.  */
int yyerror(char *s)
{
  if ( !strcmp(s, "parse error") ){
    fprintf(stderr,
            " ** Error parsing %s around line %d:\n"
            " ** Untrapped parse error, dying in yyerror\n",
            current_file, line_number);
    exit(EXIT_FAILURE);
  }
  return -1;                    /* No error codes yet.  */
}


/* Allow parser to spit out warnings about metadata values.  */
void warning_message(const char *warn_msg, ...)
{
#define MAX_MESSAGE_LENGTH 4096
  va_list ap;
  char message_to_print[MAX_MESSAGE_LENGTH];
  char temp1[MAX_MESSAGE_LENGTH];
  char temp2[MAX_MESSAGE_LENGTH];
  int ii;

/* Format string for pretty terminal display */
  strncpy (message_to_print, warn_msg, MAX_MESSAGE_LENGTH);
  message_to_print[MAX_MESSAGE_LENGTH-1] = '\0';
  for (ii=0; ii<MAX_MESSAGE_LENGTH || message_to_print[ii]=='\0'; ii++) {
    if (message_to_print[ii] == '\n') {
      strncpy (temp1, message_to_print, ii);
      temp1[ii] = '\0';
      strncat(temp1, "\n         * ", ii+13);
      strncpy(temp2, (message_to_print+ii+1), MAX_MESSAGE_LENGTH);
      strncat(temp1, temp2, MAX_MESSAGE_LENGTH);
      strncpy(message_to_print, temp1, MAX_MESSAGE_LENGTH);
      ii += 12;
    }
  }

/* Print warning with some diagnostics */
  printf("\n");
  printf("WARNING: * Parsing %s around line %d:\n"
         "         * ", current_file, line_number);
  va_start(ap, message_to_print);
  vprintf(message_to_print, ap);
  va_end(ap);
  printf("\n");
}

/* Have parser choke on bad metadata values.  */
void error_message(const char *err_mes, ...)
{
  va_list ap;
  fprintf(stderr, " ** Error: Parsing %s around line %d: ", current_file, line_number);
  va_start(ap, err_mes);
  vfprintf(stderr, err_mes, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(EXIT_FAILURE);
}


/* Casting shorthand macros for metadata structure subelements.  */
#define MTL ( (meta_parameters *) current_block)
#define MGENERAL ( (meta_general *) current_block)
#define MSAR ( (meta_sar *) current_block)
#define MSTATE ( (meta_state_vectors *) current_block)
#define MVECTOR ( (state_loc *) current_block)
#define MPROJ ( (meta_projection *) current_block)
#define MPARAM ( (param_t *) current_block)
#define MSTATS ( (meta_stats *) current_block)
#define MLOCATION ( (meta_location *) current_block)

void select_current_block(char *block_name)
{
  void *current_block = stack_top->block;

  if ( !strcmp(block_name, "general") )
    { current_block = MTL->general; goto MATCHED; }
  if ( !strcmp(block_name, "sar") )
    { current_block = MTL->sar; goto MATCHED; }
  if ( !strcmp(block_name, "state") ) {
    if (MTL->state_vectors == NULL)
      { MTL->state_vectors = meta_state_vectors_init(vector_count); }
    current_block = MTL->state_vectors;
    goto MATCHED;
  }
  if ( !strcmp(block_name, "vector") ) {
    global_meta->state_vectors = realloc( global_meta->state_vectors,
                     sizeof(meta_state_vectors) + (vector_count+1)*sizeof(state_loc));
    current_block = &( global_meta->state_vectors->vecs[vector_count++]);
    goto MATCHED;
  }

  if ( !strcmp(block_name, "projection") ) {
    if (MTL->projection == NULL)
      { MTL->projection = meta_projection_init(); }
    current_block = MTL->projection;
    goto MATCHED;
  }
  if ( !strcmp(block_name, "param") )
    { current_block = &(MPROJ->param); goto MATCHED; }
  if ( !strcmp(block_name, "atct") )
    { current_block = &((*( (param_t *) current_block)).atct); goto MATCHED; }
  if ( !strcmp(block_name, "lamaz") )
    { current_block = &((*( (param_t *) current_block)).lamaz); goto MATCHED; }
  if ( !strcmp(block_name, "lamcc") )
    { current_block = &((*( (param_t *) current_block)).lamcc); goto MATCHED; }
  if ( !strcmp(block_name, "albers") ){ 
    current_block = &((*( (param_t *) current_block)).albers); 
    goto MATCHED; 
  }
  if ( !strcmp(block_name, "ps") )
    { current_block = &((*( (param_t *) current_block)).ps); goto MATCHED; }
  if ( !strcmp(block_name, "utm") )
    { current_block = &((*( (param_t *) current_block)).utm); goto MATCHED; }
  if ( !strcmp(block_name, "state") )
    { current_block = &((*( (param_t *) current_block)).state); goto MATCHED; }

  if ( !strcmp(block_name, "stats") ) {
    if (MTL->stats == NULL)
       { MTL->stats = meta_stats_init(); }
    current_block = MTL->stats;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "location") )
    { current_block = MTL->location; goto MATCHED; }

  /* Got an unknown block name, so report.  */
  warning_message("unknown block name: %s", block_name);

MATCHED:
  block_stack_push(&stack_top, block_name, current_block);
  return;
}

/* Shorthand for casting and dereferenceing the symbol table value pointer.  */
#define VALP_AS_INT (floor(*( (double *) valp) + 0.5))
#define VALP_AS_DOUBLE *( (double *) valp)
#define VALP_AS_CHAR_POINTER ( (char *) valp)

void fill_structure_field(char *field_name, void *valp)
{
  /* Pointer to substructure corresponding to current block.  */
  void *current_block = stack_top->block;

#ifdef DEBUG_METADATA_PARSER
    extern int yydebug;
    yydebug = 1;
#endif

  /* Top-level fields (these normally go outside all blocks).  */
  if ( !strcmp(field_name, "meta_version") ) {
    MTL->meta_version = VALP_AS_DOUBLE;
    return;
  }

  /* Fields which normally go in the general block of the metadata file.  */
  if ( !strcmp(stack_top->block_name, "general") ) {
    if ( !strcmp(field_name, "sensor") )
      { !strcpy(MGENERAL->sensor, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "mode") ) {
      if ( strlen(VALP_AS_CHAR_POINTER) > MODE_FIELD_STRING_MAX - 1 ) {
                                          /* (-1 for trailing null)  */
        error_message("mode = '%s'; string should not exceed %d characters.",
                      VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX-1);
     }
      strncpy(MGENERAL->mode, VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX);
      return;
    }
    if ( !strcmp(field_name, "processor") )
      { strcpy(MGENERAL->processor, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "data_type") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "BYTE") )
        MGENERAL->data_type = BYTE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTEGER16") )
        MGENERAL->data_type = INTEGER16;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTEGER32") )
        MGENERAL->data_type = INTEGER32;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "REAL32") )
        MGENERAL->data_type = REAL32;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "REAL64") )
        MGENERAL->data_type = REAL64;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COMPLEX_BYTE") )
        MGENERAL->data_type = COMPLEX_BYTE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COMPLEX_INTEGER16") )
        MGENERAL->data_type = COMPLEX_INTEGER16;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COMPLEX_INTEGER32") )
        MGENERAL->data_type = COMPLEX_INTEGER32;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COMPLEX_REAL32") )
        MGENERAL->data_type = COMPLEX_REAL32;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COMPLEX_REAL64") )
        MGENERAL->data_type = COMPLEX_REAL64;
      else {
        warning_message("Unrecognized data_type (%s).\n",VALP_AS_CHAR_POINTER);
        MGENERAL->data_type = MAGIC_UNSET_INT;
      }
      return;
    }
    if ( !strcmp(field_name, "image_data_type") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "RAW_IMAGE") )
        MGENERAL->image_data_type = RAW_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COMPLEX_IMAGE") )
        MGENERAL->image_data_type = COMPLEX_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "AMPLITUDE_IMAGE") )
        MGENERAL->image_data_type = AMPLITUDE_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POWER_IMAGE") )
        MGENERAL->image_data_type = POWER_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "PHASE_IMAGE") )
        MGENERAL->image_data_type = PHASE_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SIGMA_IMAGE") )
        MGENERAL->image_data_type = SIGMA_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GAMMA_IMAGE") )
        MGENERAL->image_data_type = GAMMA_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "BETA_IMAGE") )
        MGENERAL->image_data_type = BETA_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COHERENCE_IMAGE") )
        MGENERAL->image_data_type = COHERENCE_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GEOCODED_IMAGE") )
        MGENERAL->image_data_type = GEOCODED_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ELEVATION") )
        MGENERAL->image_data_type = ELEVATION;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "DEM") )
        MGENERAL->image_data_type = DEM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "IMAGE") )
        MGENERAL->image_data_type = IMAGE;
      else {
        warning_message("Unrecognized image_data_type (%s).\n",VALP_AS_CHAR_POINTER);
        MGENERAL->image_data_type = MAGIC_UNSET_INT;
      }
      return;
   }
   if ( !strcmp(field_name, "system") )
      { strcpy(MGENERAL->system, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "orbit") )
      { MGENERAL->orbit = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "orbit_direction") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "A") ) {
        MGENERAL->orbit_direction = 'A';
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "D") ) {
        MGENERAL->orbit_direction = 'D';
        return;
      }
      /* If its a question mark don't bother the user with a warning,
	 this happens often with DDRs.  */
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "?") ) {
        MGENERAL->orbit_direction = '?';
        return;
      }
      else {
        warning_message("Bad value: orbit_direction = '%s'.",
			VALP_AS_CHAR_POINTER);
        return;
      }
    }
    if ( !strcmp(field_name, "frame") )
      { MGENERAL->frame = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "band_number") )
      { MGENERAL->band_number = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "line_count") )
      { MGENERAL->line_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "sample_count") )
      { MGENERAL->sample_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "start_line") )
      { MGENERAL->start_line = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "start_sample") )
      { MGENERAL->start_sample = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "x_pixel_size") )
      { MGENERAL->x_pixel_size = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "y_pixel_size") )
      { MGENERAL->y_pixel_size = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "center_latitude") )
      { MGENERAL->center_latitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "center_longitude") )
      { MGENERAL->center_longitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "average_height") )
      { MGENERAL->average_height = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "re_major") )
      { MGENERAL->re_major = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "re_minor") )
      { MGENERAL->re_minor = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "bit_error_rate") )
      { MGENERAL->bit_error_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "missing_lines") )
      { MGENERAL->missing_lines = VALP_AS_INT; return; }
  }

  /* Fields which normally go in the sar block of the metadata file.  */
  if ( !strcmp(stack_top->block_name, "sar") ) {
    if ( !strcmp(field_name, "image_type") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "S") ) {
        MSAR->image_type = 'S';
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "G") ) {
        MSAR->image_type = 'G';
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "P") ) {
        MSAR->image_type = 'P';
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "?") ) {
       /* if its a question mark don't bother the user with a warning, this happens often with DDRs */
        MSAR->image_type = '?';
        return;
      }
      else {
        warning_message("Bad value: image_type = '%s'.",VALP_AS_CHAR_POINTER);
        return;
      }
    }
    if ( !strcmp(field_name, "look_direction") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "R") ) {
        MSAR->look_direction = 'R'; return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "L") ) {
        MSAR->look_direction = 'L'; return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "?") ) {
       /* if its a question mark don't bother the user with a warning, this happens often with DDRs */
        MSAR->look_direction = '?'; return;
      }
      warning_message("Bad value: look_direction = '%c'.",
		      VALP_AS_CHAR_POINTER[0]);
      return;
    }
    if ( !strcmp(field_name, "look_count") )
      { MSAR->look_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "deskewed") )
      { MSAR->deskewed = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "original_line_count") )
      { MSAR->original_line_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "original_sample_count") )
      { MSAR->original_sample_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "line_increment") )
      { MSAR->line_increment = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "sample_increment") )
      { MSAR->sample_increment = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "range_time_per_pixel") )
      { MSAR->range_time_per_pixel = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "azimuth_time_per_pixel") )
      { MSAR->azimuth_time_per_pixel = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "slant_range_first_pixel") )
      { MSAR->slant_range_first_pixel = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "slant_shift") )
      { MSAR->slant_shift = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "time_shift") )
      { MSAR->time_shift = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "wavelength") )
      { MSAR->wavelength = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "prf") )
      { MSAR->prf = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "earth_radius") )
      { MSAR->earth_radius = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "satellite_height") )
      { MSAR->satellite_height = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "satellite_binary_time") )
      { strcpy(MSAR->satellite_binary_time, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "satellite_clock_time") )
      { strcpy(MSAR->satellite_clock_time, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "dopRangeCen") )
      { MSAR->range_doppler_coefficients[0] = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "dopRangeLin") )
      { MSAR->range_doppler_coefficients[1] = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "dopRangeQuad") )
      { MSAR->range_doppler_coefficients[2] = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "dopAzCen") )
      { MSAR->azimuth_doppler_coefficients[0] = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "dopAzLin") )
      { MSAR->azimuth_doppler_coefficients[1] = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "dopAzQuad") )
      { MSAR->azimuth_doppler_coefficients[2] = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "azimuth_bandwidth") )
      { MSAR->azimuth_processing_bandwidth = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "chirp_rate") )
      { MSAR->chirp_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "pulse_duration") )
      { MSAR->pulse_duration = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "range_samp_rate") )
      { MSAR->range_sampling_rate = VALP_AS_DOUBLE; return; }
}

  /* Fields which normally go in the state block of the metadata file.  */
  if ( !strcmp(stack_top->block_name, "state") ) {
    if ( !strcmp(field_name, "year") )
      { MSTATE->year = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "julDay") )
      { MSTATE->julDay = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "second") )
      { MSTATE->second = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "vector_count") )
      { /* This field will be compared to the counted the number of
         * state vector blocks that we actually see.  */
        MSTATE->vector_count = VALP_AS_INT;
        MSTATE->num = VALP_AS_INT; /* Compatability alias */
        return;
      }
  }

    /* Fields which normally go in a vector block.  */
  if ( !strcmp(stack_top->block_name, "vector") ) {
    if ( !strcmp(field_name, "time") )
      { MVECTOR->time = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "x") )
      { MVECTOR->vec.pos.x = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "y") )
      { MVECTOR->vec.pos.y = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "z") )
      { MVECTOR->vec.pos.z = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "vx") )
      { MVECTOR->vec.vel.x = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "vy") )
      { MVECTOR->vec.vel.y = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "vz") )
      { MVECTOR->vec.vel.z = VALP_AS_DOUBLE; return; }
  }

  /* Code for dealing with optical and/or thermal blocks could be
     added here.  */

  /* Fields which normaly go in the projection block of the metadata file.  */

  if ( !strcmp(stack_top->block_name, "projection") ) {
    if ( !strcmp(field_name, "type") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "UNIVERSAL_TRANSVERSE_MERCATOR") )
        MPROJ->type = UNIVERSAL_TRANSVERSE_MERCATOR;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLAR_STEREOGRAPHIC") )
        MPROJ->type = POLAR_STEREOGRAPHIC;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ALBERS_EQUAL_AREA") )
        MPROJ->type = ALBERS_EQUAL_AREA;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LAMBERT_CONFORMAL_CONIC") )
        MPROJ->type = LAMBERT_CONFORMAL_CONIC;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LAMBERT_AZIMUTHAL_EQUAL_AREA") )
        MPROJ->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "STATE_PLANE") )
        MPROJ->type = STATE_PLANE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SCANSAR_PROJECTION") )
        MPROJ->type = SCANSAR_PROJECTION;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LAT_LONG_PSEUDO_PROJECTION") )
	MPROJ->type = LAT_LONG_PSEUDO_PROJECTION;
      else {
        warning_message("Bad value: type = '%s'.",VALP_AS_CHAR_POINTER);
      }
      return;
    }
    if ( !strcmp(field_name, "startX") )
      { MPROJ->startX = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "startY") )
      { MPROJ->startY = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "perX") )
      { MPROJ->perX = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "perY") )
      { MPROJ->perY = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "units") )
      { strcpy(MPROJ->units, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "hem") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "S") ) { MPROJ->hem = 'S'; return; }
      else { MPROJ->hem = 'N'; return; }
    }
    if ( !strcmp(field_name, "spheroid") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "BESSEL") )
        MPROJ->spheroid = BESSEL_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "CLARKE1866") )
        MPROJ->spheroid = CLARKE1866_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "CLARKE1880") )
        MPROJ->spheroid = CLARKE1880_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GEM6") )
        MPROJ->spheroid = GEM6_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GEM10C") )
        MPROJ->spheroid = GEM10C_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GRS1980") )
        MPROJ->spheroid = GRS1980_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTERNATIONAL1924") )
        MPROJ->spheroid = INTERNATIONAL1924_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTERNATIONAL1967") )
        MPROJ->spheroid = INTERNATIONAL1967_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS72") )
        MPROJ->spheroid = WGS72_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS84") )
        MPROJ->spheroid = WGS84_SPHEROID;
      else {
        warning_message("Bad value: spheroid = '%s'.",VALP_AS_CHAR_POINTER);
      }
      return;
    }
    if ( !strcmp(field_name, "re_major") )
      { MPROJ->re_major = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "re_minor") )
      { MPROJ->re_minor = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "datum") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "EGM96") )
	MPROJ->datum = EGM96_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ED50") )
	MPROJ->datum = ED50_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ETRF89") )
	MPROJ->datum = ETRF89_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ETRS89") )
	MPROJ->datum = ETRS89_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ITRF") )
	MPROJ->datum = ITRF_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "NAD27") )
	MPROJ->datum = NAD27_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "NAD83") )
	MPROJ->datum = NAD83_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS72") )
	MPROJ->datum = WGS72_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS84") )
	MPROJ->datum = WGS84_DATUM;
      else {
	warning_message("Bad value: datum = '%s'.", VALP_AS_CHAR_POINTER);
      }
      return;
    }
  }

  /* Fields that go in the (proj->param).atct block.  */
  if ( !strcmp(stack_top->block_name, "atct") ) {
    if ( !strcmp(field_name, "rlocal") )
      { (*MPARAM).atct.rlocal = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "alpha1") )
      { (*MPARAM).atct.alpha1 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "alpha2") )
      { (*MPARAM).atct.alpha2 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "alpha3") )
      { (*MPARAM).atct.alpha3 = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).albers block.  */
  /* Check for both lamcc and lambert for backwards compatibility */
  if ( !strcmp(stack_top->block_name, "albers")) {
    if ( !strcmp(field_name, "std_parallel1") )
      { (*MPARAM).albers.std_parallel1 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "std_parallel2") )
      { (*MPARAM).albers.std_parallel2 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "center_meridian") )
      { (*MPARAM).albers.center_meridian = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "orig_latitude") )
      { (*MPARAM).albers.orig_latitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).albers.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).albers.false_northing = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).lamaz block.  */
  /* Check for both lamcc and lambert for backwards compatibility */
  if ( !strcmp(stack_top->block_name, "lamaz")) {
    if ( !strcmp(field_name, "center_lon") )
      { (*MPARAM).lamaz.center_lon = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "center_lat") )
      { (*MPARAM).lamaz.center_lat = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).lamaz.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).lamaz.false_northing = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).lamcc block.  */
  /* Check for both lamcc and lambert for backwards compatibility */
  if ( !strcmp(stack_top->block_name, "lamcc") ||  !strcmp(stack_top->block_name, "lambert")) {
    if ( !strcmp(field_name, "plat1") )
      { (*MPARAM).lamcc.plat1 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "plat2") )
      { (*MPARAM).lamcc.plat2 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lat0") )
      { (*MPARAM).lamcc.lat0 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon0") )
      { (*MPARAM).lamcc.lon0 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).lamcc.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).lamcc.false_northing = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "scale_factor") )
      { (*MPARAM).lamcc.scale_factor = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).ps block.  */
  if ( !strcmp(stack_top->block_name, "ps") ) {
    if ( !strcmp(field_name, "slat") )
      { (*MPARAM).ps.slat = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "slon") )
      { (*MPARAM).ps.slon = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).ps.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).ps.false_northing = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).utm block.  */
  if ( !strcmp(stack_top->block_name, "utm") ) {
    if ( !strcmp(field_name, "zone") )
      { (*MPARAM).utm.zone = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).utm.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).utm.false_northing = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "scale_factor") )
      { (*MPARAM).utm.scale_factor = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).state block.  */
  if ( !strcmp(stack_top->block_name, "state") ) {
    if ( !strcmp(field_name, "zone") )
      { (*MPARAM).state.zone = VALP_AS_INT; return; }
  }

  /* Note that the projection-specific param data block associated
     with LAT_LONG_PSEUDO_PROJECTION type files is empty, since all
     the parameters required for that projection are stored in the
     main block.  */

  /* Fields which normally go in the statistics block of the metadata file. */
  if ( !strcmp(stack_top->block_name, "stats") ) {
    if ( !strcmp(field_name, "min") )
      { (MSTATS)->min = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "max") )
      { (MSTATS)->max = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "mean") )
      { (MSTATS)->mean = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "rmse") )
      { (MSTATS)->rmse = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "std_deviation") )
      { (MSTATS)->std_deviation = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "mask") )
      { (MSTATS)->mask = VALP_AS_DOUBLE; return; }
  }

  /* Fields which go in the location block of the metadata file. */
  if ( !strcmp(stack_top->block_name, "location") ) {
    if ( !strcmp(field_name, "lat_start_near_range") )
      { (MLOCATION)->lat_start_near_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon_start_near_range") )
      { (MLOCATION)->lon_start_near_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lat_start_far_range") )
      { (MLOCATION)->lat_start_far_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon_start_far_range") )
      { (MLOCATION)->lon_start_far_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lat_end_near_range") )
      { (MLOCATION)->lat_end_near_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon_end_near_range") )
      { (MLOCATION)->lon_end_near_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lat_end_far_range") )
      { (MLOCATION)->lat_end_far_range = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon_end_far_range") )
      { (MLOCATION)->lon_end_far_range = VALP_AS_DOUBLE; return; }
  }

  /* Got an unknown field name, so report & choke */
  fprintf (stderr, "Warning: Unknown field name: %s\n", field_name);
  return;
  // error_message("Unknown field name: %s", field_name);
}

%}

%start element_seq

%union {                           /* Define parser stack type.  */
  double double_val;
  char string_val[MAX_SYMBOL_STRING + 1];   /* Null terminated so +1.  */
  void *var_type;
}

%token <double_val> DOUBLE
%token <string_val> NAME
%token <string_val> STRING

%type <var_type> field_value

%%

element_seq:   element
             | element_seq element
             ;

element:   field
         | block
         ;

field:   NAME ':' field_value
             { fill_structure_field($1, $3);
               free($3); }
       ;

field_value:   DOUBLE { double *tmp = (double *) malloc(sizeof(double));
                        *tmp = $1;
                        $$ = tmp; }
             | STRING { $$ = strdup($1); }
             ;

block:   block_start element_seq '}'
             { block_stack_pop(&stack_top); }
       | block_start '}'
             { block_stack_pop(&stack_top); }
       ;
block_start:   NAME '{'
                   { select_current_block($1); }
             ;

%%

/* Main parser interface function.  Gets passed in a pointer to a
   structure to be filled in and a file name to fill it from.  Returns
   true if the parse succeeded, false otherwise.  */
int parse_metadata(meta_parameters *dest, char *file_name)
{
  extern FILE *meta_yyin;
  int ret_val;

  global_meta = dest;

  /* Put file name in a global for error reporting.  */
  strncpy(current_file, file_name, MAX_FILE_NAME);

  /* (Re)set line_number to first line */
  line_number = 1;

  /* (Re)set file scope variable which counts number of vector blocks seen.  */
  vector_count = 0;

  meta_yyin = FOPEN(file_name, "r");
  stack_top = NULL;
  block_stack_push(&stack_top, "outermost_section", dest);

  /* Parse metadata file.  */
  ret_val = yyparse();

  /* Fill in number of state vectors seen.  */
  if ((dest->state_vectors) && (dest->state_vectors->vector_count != vector_count)) {
    warning_message("Said number of vectors in state vector block (%d)\n"
                    "differs from the actual amount of vectors (%d)...\n"
                    "Using actual number of vectors for vector_count.",
                    dest->state_vectors->vector_count, vector_count);
    dest->state_vectors->vector_count = vector_count;
    dest->state_vectors->num = vector_count; /* Backward compat alias.  */
  }

  /* Done with the block stack, so empty it.  */
  while ( stack_top != NULL ) {
    block_stack_pop (&stack_top);
  }

  FCLOSE(meta_yyin);

  return ret_val;
}
