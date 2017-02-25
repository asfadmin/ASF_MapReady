/* Parser for ASF image metadata files.  */

%{

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DOUBLE
#undef DOUBLE
#endif

#define DOUBLE __double
#include "asf.h"

#include "asf_meta.h"
#undef DOUBLE
#include "caplib.h"
#include "lex_yacc.h"

extern report_level_t level; // default: WARNING

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

static int doppler_count;

/* Arrays of stats blocks are stored in the metadata structure, this keeps
   track of how many of them we have seen so far.  */
static int stats_block_count;

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
  char buffer[MAX_MESSAGE_LENGTH];
  char temp[MAX_MESSAGE_LENGTH];

  sprintf(buffer, "Parsing %s around line %d:\n", current_file, line_number);
  va_start(ap, warn_msg);
  vsprintf(temp, warn_msg, ap);
  strcat(buffer, temp);
  va_end(ap);
  asfReport(g_report_level, buffer);
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
#define MOPTICAL ( (meta_optical *) current_block)
#define MSTATE ( (meta_state_vectors *) current_block)
#define MVECTOR ( (state_loc *) current_block)
#define MPROJ ( (meta_projection *) current_block)
#define MPARAM ( (param_t *) current_block)
#define MSTATS ( (meta_stats *) current_block)
#define MSTATISTICS ( (meta_statistics *) current_block)
#define MSTATSBLOCK ( (meta_stats *) current_block)
#define MLOCATION ( (meta_location *) current_block)
#define MTRANSFORM ( (meta_transform *) current_block)
#define MAIRSAR ( (meta_airsar *) current_block)
#define MUAVSAR ( (meta_uavsar *) current_block)
#define MCALIBRATION ( (meta_calibration *) current_block)
#define MCOLORMAP ( (meta_colormap *) current_block)
#define MRGB ( (meta_rgb *) current_block)
#define MDOPPLER ( (meta_doppler *) current_block)
#define MESTIMATE ( (tsx_doppler_t *) current_block)
#define MINSAR ( (meta_insar *) current_block)
#define MDEM ( (meta_dem *) current_block)
#define MQUALITY ( (meta_quality *) current_block)

void select_current_block(char *block_name)
{
  void *current_block = stack_top->block;

  if ( !strcmp(block_name, "general") ) {
    current_block = MTL->general;
    goto MATCHED;
  }
  if ( !strcmp(block_name, "sar") ) {
    if (MTL->sar == NULL)
      { MTL->sar = meta_sar_init(); }
    current_block = MTL->sar;
    goto MATCHED;
  }
  if ( !strcmp(block_name, "optical") ) {
    if (MTL->optical == NULL)
      { MTL->optical = meta_optical_init(); }
    current_block = MTL->optical;
    goto MATCHED;
  }
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
  if ( !strcmp(block_name, "albers") )
    { current_block = &((*( (param_t *) current_block)).albers); goto MATCHED; }
  if ( !strcmp(block_name, "ps") )
    { current_block = &((*( (param_t *) current_block)).ps); goto MATCHED; }
  if ( !strcmp(block_name, "utm") )
    { current_block = &((*( (param_t *) current_block)).utm); goto MATCHED; }
  if ( !strcmp(block_name, "state") )
    { current_block = &((*( (param_t *) current_block)).state); goto MATCHED; }
  if ( !strcmp(block_name, "mer") )
    { current_block = &((*( (param_t *) current_block)).mer); goto MATCHED; }
  if ( !strcmp(block_name, "eqr") )
    { current_block = &((*( (param_t *) current_block)).eqr); goto MATCHED; }
  if ( !strcmp(block_name, "eqc") )
    { current_block = &((*( (param_t *) current_block)).eqc); goto MATCHED; }
  if ( !strcmp(block_name, "sin") )
    { current_block = &((*( (param_t *) current_block)).sin); goto MATCHED; }
  if ( !strcmp(block_name, "cea") )
    { current_block = &((*( (param_t *) current_block)).cea); goto MATCHED; }

  if ( !strcmp(block_name, "transform") ) {
    if (MTL->transform == NULL)
       { MTL->transform = meta_transform_init();}
    current_block = MTL->transform;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "airsar") ) {
    if (MTL->airsar == NULL)
       { MTL->airsar = meta_airsar_init();}
    current_block = MTL->airsar;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "uavsar") ) {
    if (MTL->uavsar == NULL)
       { MTL->uavsar = meta_uavsar_init();}
    current_block = MTL->uavsar;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "dem") ) {
    if (MTL->dem == NULL)
       { MTL->dem = meta_dem_init();}
    current_block = MTL->dem;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "stats") ) { // Stats block for versions lower than v2.4 (single-band stats)
    if (MTL->stats == NULL)
    { MTL->stats = meta_statistics_init(1); stats_block_count++;}
    current_block = MTL->stats;
    goto MATCHED;
  }
  if ( !strcmp(block_name, "statistics") ) { // Stats block for v2.4+ (multi-band stats)
    if (MTL->stats == NULL)
       { MTL->stats = meta_statistics_init(stats_block_count); }
    current_block = MTL->stats;
    goto MATCHED;
  }
  if ( !strcmp(block_name, "band_stats") ) { // Band stats blocks for v2.4+ (multi-band stats)
    global_meta->stats = realloc( global_meta->stats,
                      sizeof(meta_statistics) + (stats_block_count+1)*sizeof(meta_stats));
    current_block = &( global_meta->stats->band_stats[stats_block_count++]);
    goto MATCHED;
  }

  if ( !strcmp(block_name, "location") ) {
    if (MTL->location == NULL)
      { MTL->location = meta_location_init(); }
    current_block = MTL->location;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "calibration") ) {
    if (MTL->calibration == NULL)
      { MTL->calibration = meta_calibration_init(); }
    current_block = MTL->calibration;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "colormap") ) {
      if (MTL->colormap == NULL)
      { MTL->colormap = meta_colormap_init(); }
      current_block = MTL->colormap;
      goto MATCHED;
  }

  if ( !strcmp(block_name, "insar") ) {
      if (MTL->insar == NULL)
      { MTL->insar = meta_insar_init(); }
      current_block = MTL->insar;
      goto MATCHED;
  }

	if ( !strcmp(block_name, "quality") ) {
		if (MTL->quality == NULL)
		{ MTL->quality = meta_quality_init(); }
		current_block = MTL->quality;
		goto MATCHED;
	}

  if ( !strcmp(block_name, "doppler") ) {
    if (MTL->doppler == NULL)
      { MTL->doppler = meta_doppler_init(); }
    current_block = MTL->doppler;
    goto MATCHED;
  }

  if ( !strcmp(block_name, "estimate") ) {
    current_block = &( global_meta->doppler->tsx->dop[doppler_count++]);
    goto MATCHED;
  }

  /* Got an unknown block name, so report.  */
  warning_message("unknown block name: %s\n", block_name);

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

  /* FIXME: Because the yacc parser returns after each token is found, there
  is no token-to-token state inherent in the code.  I added 2 static ints for
  'remembering' the sar block image_type and a 'map projected' flag that indicates
  whether or not a map-projected type of projection was found in the projection
  block ('type' field).  These are utilized in calls to warning_message() after
  parsing projection type, spheroid, and datum type tokens from the metadata.

  This presents 2 problems that we should probably resolve in the
  future:
          1) If we re-order the SAR and Projection blocks, then the logic
      on the warning messages will break
          2) This solution only works for sequentially-called parsing, e.g.
      meta_read().  If child processes are forked off and both are
          reading a metadata file then the shared static vars will
          be in a race condition that will likely break the logic on
          the warning messages.

  Neither of these two issues is critical since it only impacts warning messages,
  but we ought to re-think the warning message methodology and maybe consider moving
  the warning message logic to the code that uses the parser rather than having the
  parser itself issue the warnings ...just let it return tokens silently.
  */

  /* Maintains fact that a 'P' was found in the sar block image_type
     field
  */
  static int sar_projected;

  /* Maintains the fact that a recognized map projection type
     was found in the projection block
  */
  static int map_projection_type;

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
    if ( !strcmp(field_name, "name") )
      { strcpy(MGENERAL->basename, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "sensor") )
      { strcpy(MGENERAL->sensor, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "sensor_name") )
      { strcpy(MGENERAL->sensor_name, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "mode") ) {
      if ( strlen(VALP_AS_CHAR_POINTER) > MODE_FIELD_STRING_MAX - 1 ) {
                                          /* (-1 for trailing null)  */
        error_message("mode = '%s'; string should not exceed %d characters.",
                      VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX-1);
     }
      strncpy(MGENERAL->mode, VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX);
      return;
    }
    if ( !strcmp(field_name, "receiving_station") ) {
      if ( strlen(VALP_AS_CHAR_POINTER) > MODE_FIELD_STRING_MAX - 1 ) {
                                          /* (-1 for trailing null)  */
        error_message("receiving_station = '%s'; string should not exceed %d characters.",
                      VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX-1);
     }
      strncpy(MGENERAL->receiving_station, VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX);
      return;
    }
    if ( !strcmp(field_name, "processor") )
      { strcpy(MGENERAL->processor, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "data_type") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "BYTE") )
        MGENERAL->data_type = ASF_BYTE;
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
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "PHASE_IMAGE") )
          MGENERAL->image_data_type = PHASE_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POWER_IMAGE") )
          MGENERAL->image_data_type = POWER_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SIGMA_IMAGE") )
          MGENERAL->image_data_type = SIGMA_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GAMMA_IMAGE") )
          MGENERAL->image_data_type = GAMMA_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "BETA_IMAGE") )
          MGENERAL->image_data_type = BETA_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTERFEROGRAM") )
          MGENERAL->image_data_type = INTERFEROGRAM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "UNWRAPPED_PHASE") )
          MGENERAL->image_data_type = UNWRAPPED_PHASE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "COHERENCE_IMAGE") )
        MGENERAL->image_data_type = COHERENCE_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GEOREFERENCED_IMAGE") )
          MGENERAL->image_data_type = GEOREFERENCED_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GEOCODED_IMAGE") )
          MGENERAL->image_data_type = GEOCODED_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_IMAGE") )
	MGENERAL->image_data_type = POLARIMETRIC_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_SEGMENTATION") )
        MGENERAL->image_data_type = POLARIMETRIC_SEGMENTATION;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_DECOMPOSITION") )
        MGENERAL->image_data_type = POLARIMETRIC_DECOMPOSITION;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_PARAMETER") )
        MGENERAL->image_data_type = POLARIMETRIC_PARAMETER;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_C2_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_C2_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_C3_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_C3_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_C4_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_C4_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_T3_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_T3_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_T4_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_T4_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_S2_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_S2_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLARIMETRIC_STOKES_MATRIX") )
        MGENERAL->image_data_type = POLARIMETRIC_STOKES_MATRIX;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LUT_IMAGE") )
          MGENERAL->image_data_type = LUT_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ELEVATION") )
        MGENERAL->image_data_type = ELEVATION;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "DEM") )
        MGENERAL->image_data_type = DEM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "IMAGE") )
        MGENERAL->image_data_type = IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "BROWSE_IMAGE") )
	MGENERAL->image_data_type = BROWSE_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "MASK") )
        MGENERAL->image_data_type = MASK;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SIMULATED_IMAGE") )
        MGENERAL->image_data_type = SIMULATED_IMAGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "IMAGE_LAYER_STACK") )
	MGENERAL->image_data_type = IMAGE_LAYER_STACK;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INSAR_STACK") )
	MGENERAL->image_data_type = INSAR_STACK;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "RGB_STACK") )
	MGENERAL->image_data_type = RGB_STACK;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "MOSAIC") )
	MGENERAL->image_data_type = MOSAIC;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ICE_AGE") )
	MGENERAL->image_data_type = ICE_AGE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ICE_THICKNESS") )
	MGENERAL->image_data_type = ICE_THICKNESS;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "BACKSCATTER_HISTOGRAM") )
	MGENERAL->image_data_type = BACKSCATTER_HISTOGRAM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "MULTIYEAR_ICE_FRACTION") )
	MGENERAL->image_data_type = MULTIYEAR_ICE_FRACTION;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "DIVERGENCE") )
	MGENERAL->image_data_type = DIVERGENCE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "VORTICITY") )
	MGENERAL->image_data_type = VORTICITY;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SHEAR") )
	MGENERAL->image_data_type = SHEAR;
      else if (!strcmp(VALP_AS_CHAR_POINTER, "MODEL_OUTPUT"))
        MGENERAL->image_data_type = MODEL_OUTPUT;
      else {
        warning_message("Unrecognized image_data_type (%s).\n",VALP_AS_CHAR_POINTER);
        MGENERAL->image_data_type = MAGIC_UNSET_INT;
      }
      return;
   }
    if ( !strcmp(field_name, "radiometry") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "AMPLITUDE") )
        MGENERAL->radiometry = r_AMP;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SIGMA") )
        MGENERAL->radiometry = r_SIGMA;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "BETA") )
        MGENERAL->radiometry = r_BETA;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GAMMA") )
        MGENERAL->radiometry = r_GAMMA;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SIGMA_DB") )
        MGENERAL->radiometry = r_SIGMA_DB;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "BETA_DB") )
        MGENERAL->radiometry = r_BETA_DB;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GAMMA_DB") )
        MGENERAL->radiometry = r_GAMMA_DB;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POWER") )
        MGENERAL->radiometry = r_POWER;
      else {
        // Only print the warning for image types that should have radiometry.
        // Means the parser is order-dependent... image_data_type MUST
        // occur before radiometry in the meta file.
        if (MGENERAL->image_data_type == AMPLITUDE_IMAGE ||
            MGENERAL->image_data_type == POLARIMETRIC_SEGMENTATION ||
	    MGENERAL->image_data_type == POLARIMETRIC_DECOMPOSITION ||
	    MGENERAL->image_data_type == POLARIMETRIC_PARAMETER ||
	    (MGENERAL->image_data_type >= POLARIMETRIC_C2_MATRIX &&
	     MGENERAL->image_data_type <= POLARIMETRIC_STOKES_MATRIX))
        {
          warning_message("Unrecognized radiometry (%s).\n",VALP_AS_CHAR_POINTER);
        }
        MGENERAL->radiometry = r_AMP;
      }
      return;
   }
    if ( !strcmp(field_name, "acquisition_date") )
      { strcpy(MGENERAL->acquisition_date, VALP_AS_CHAR_POINTER); return; }
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
        warning_message("Bad value: orbit_direction = '%s'.\n",
                        VALP_AS_CHAR_POINTER);
        return;
      }
    }
    if ( !strcmp(field_name, "frame") )
      { MGENERAL->frame = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "band_count") )
      { MGENERAL->band_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "bands") ) {
      if (strlen(VALP_AS_CHAR_POINTER) == 0) {
        char tmp[10];
        sprintf(tmp, "0%.0lf" , VALP_AS_DOUBLE);
        strcpy(MGENERAL->bands, tmp);
      }
      else
        strcpy(MGENERAL->bands, VALP_AS_CHAR_POINTER);
      return;
    }
    if ( !strcmp(field_name, "line_count") )
      { MGENERAL->line_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "sample_count") )
      { MGENERAL->sample_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "start_line") )
      { MGENERAL->start_line = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "start_sample") )
      { MGENERAL->start_sample = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "line_scaling") )
      { MGENERAL->line_scaling = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "sample_scaling") )
      { MGENERAL->sample_scaling = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "x_pixel_size") )
      { MGENERAL->x_pixel_size = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "y_pixel_size") )
      { MGENERAL->y_pixel_size = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "center_latitude") )
      { MGENERAL->center_latitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "center_longitude") )
      { MGENERAL->center_longitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "re_major") )
      { MGENERAL->re_major = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "re_minor") )
      { MGENERAL->re_minor = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "bit_error_rate") )
      { MGENERAL->bit_error_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "missing_lines") )
      { MGENERAL->missing_lines = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "no_data") )
      { MGENERAL->no_data = (float) VALP_AS_DOUBLE; return; }
  }

  /* Fields which normally go in the sar block of the metadata file.  */
  if ( !strcmp(stack_top->block_name, "sar") ) {
    int ii;
    char coeff[15];
    if ( !strcmp(field_name, "polarization") )
      { strcpy(MSAR->polarization, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "image_type") ) {
      if ( !strcmp(VALP_AS_CHAR_POINTER, "S") ) {
        MSAR->image_type = 'S';
        sar_projected = 0;
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "G") ) {
        MSAR->image_type = 'G';
        sar_projected = 0;
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "P") ) {
        MSAR->image_type = 'P';
        sar_projected = 1;
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "R") ) {
        MSAR->image_type = 'R';
        return;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "?") ) {
       /* if its a question mark don't bother the user with a warning, this happens often with DDRs */
        MSAR->image_type = '?';
        sar_projected = 0;
        return;
      }
      else {
        warning_message("Bad value: image_type = '%s'.\n",
      VALP_AS_CHAR_POINTER);
        sar_projected = 0;
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
      warning_message("Bad value: look_direction = '%c'.\n",
          VALP_AS_CHAR_POINTER[0]);
      return;
    }
    if ( !strcmp(field_name, "look_count") )
      { MSAR->azimuth_look_count = VALP_AS_INT; 
	MSAR->range_look_count = 1; return; }
    if ( !strcmp(field_name, "azimuth_look_count") )
      { MSAR->azimuth_look_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "range_look_count") )
      { MSAR->range_look_count = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "multilook") )
      { MSAR->multilook = VALP_AS_INT; return; }
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
    if ( !strcmp(field_name, "earth_radius_pp") )
      { MSAR->earth_radius_pp = VALP_AS_DOUBLE; return; }
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
    if ( !strcmp(field_name, "pitch") )
      { MSAR->pitch = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "roll") )
      { MSAR->roll = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "yaw") )
      { MSAR->yaw = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "azimuth_bandwidth") )
      { MSAR->azimuth_processing_bandwidth = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "chirp_rate") )
      { MSAR->chirp_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "pulse_duration") )
      { MSAR->pulse_duration = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "range_samp_rate") )
      { MSAR->range_sampling_rate = VALP_AS_DOUBLE; return; }
    for (ii=0; ii<6; ii++) {
      sprintf(coeff, "incid_a(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MSAR->incid_a[ii] = VALP_AS_DOUBLE; return; }
    }
}

  /* Fields which normally go in the optical block of the metadata file.  */
  if ( !strcmp(stack_top->block_name, "optical") ) {
    if ( !strcmp(field_name, "pointing_direction") )
      { strcpy(MOPTICAL->pointing_direction, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "off_nadir_angle") )
      { MOPTICAL->off_nadir_angle = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "correction_level") )
      { strcpy(MOPTICAL->correction_level, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "cloud_percentage") )
      { MOPTICAL->cloud_percentage = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "sun_azimuth_angle") )
      { MOPTICAL->sun_azimuth_angle = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "sun_elevation_angle") )
      { MOPTICAL->sun_elevation_angle = VALP_AS_DOUBLE; return; }
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
      if ( !strcmp(VALP_AS_CHAR_POINTER, "UNIVERSAL_TRANSVERSE_MERCATOR") ) {
        MPROJ->type = UNIVERSAL_TRANSVERSE_MERCATOR;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "POLAR_STEREOGRAPHIC") ) {
        MPROJ->type = POLAR_STEREOGRAPHIC;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ALBERS_EQUAL_AREA") ) {
        MPROJ->type = ALBERS_EQUAL_AREA;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LAMBERT_CONFORMAL_CONIC") ) {
        MPROJ->type = LAMBERT_CONFORMAL_CONIC;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LAMBERT_AZIMUTHAL_EQUAL_AREA") ) {
        MPROJ->type = LAMBERT_AZIMUTHAL_EQUAL_AREA;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "STATE_PLANE") ) {
        MPROJ->type = STATE_PLANE;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SCANSAR_PROJECTION") ) {
        MPROJ->type = SCANSAR_PROJECTION;
        map_projection_type = 0;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "LAT_LONG_PSEUDO_PROJECTION") ) {
        MPROJ->type = LAT_LONG_PSEUDO_PROJECTION;
        map_projection_type = 0;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "MERCATOR") ) {
        MPROJ->type = MERCATOR;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "EQUI_RECTANGULAR") ) {
        MPROJ->type = EQUI_RECTANGULAR;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "EQUIDISTANT") ) {
        MPROJ->type = EQUIDISTANT;
        map_projection_type = 1;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SINUSOIDAL") ) {
        MPROJ->type = SINUSOIDAL;
        map_projection_type = 0;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "EASE_GRID_GLOBAL") ) {
	MPROJ->type = EASE_GRID_GLOBAL;
	map_projection_type = 0;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "EASE_GRID_NORTH") ) {
	MPROJ->type = EASE_GRID_NORTH;
	map_projection_type = 0;
      }
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "EASE_GRID_SOUTH") ) {
	MPROJ->type = EASE_GRID_SOUTH;
	map_projection_type = 0;
      }
      else {
        MPROJ->type = UNKNOWN_PROJECTION;
        // Only complain if the image is truly map projected
        if (sar_projected && map_projection_type) {
          warning_message("Bad value: type = '%s'.\n",VALP_AS_CHAR_POINTER);
        }
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
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "GRS1967") )
	MPROJ->spheroid = GRS1967_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTERNATIONAL1924") )
        MPROJ->spheroid = INTERNATIONAL1924_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "INTERNATIONAL1967") )
        MPROJ->spheroid = INTERNATIONAL1967_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS72") )
        MPROJ->spheroid = WGS72_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS84") )
        MPROJ->spheroid = WGS84_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "HUGHES") )
        MPROJ->spheroid = HUGHES_SPHEROID;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SINUSOIDAL_SPHERE") )
        MPROJ->spheroid = SINUSOIDAL_SPHERE;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "AUTHALIC_SPHERE") )
	MPROJ->spheroid = AUTHALIC_SPHERE;
      else {
        MPROJ->spheroid = UNKNOWN_SPHEROID;
        // Only complain if the image is truly map projected
        if (sar_projected && map_projection_type) {
          warning_message("Bad value: spheroid = '%s'.\n",
        VALP_AS_CHAR_POINTER);
        }
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
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "ITRF97") )
        MPROJ->datum = ITRF97_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "NAD27") )
        MPROJ->datum = NAD27_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "NAD83") )
        MPROJ->datum = NAD83_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "SAD69") )
	MPROJ->datum = SAD69_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS72") )
        MPROJ->datum = WGS72_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "WGS84") )
        MPROJ->datum = WGS84_DATUM;
      else if ( !strcmp(VALP_AS_CHAR_POINTER, "HUGHES") )
        MPROJ->datum = HUGHES_DATUM;
      else {
        MPROJ->datum = UNKNOWN_DATUM;
        // Only complain if the mage is truly map projected
        if (sar_projected && map_projection_type) {
          warning_message("Bad value: datum = '%s'.\n", VALP_AS_CHAR_POINTER);
        }
      }
      return;
    }
    if ( !strcmp(field_name, "height") )
      { MPROJ->height = VALP_AS_DOUBLE; return; }
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
  // Could also be EASE polar grids - fill in just in case
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
      { (*MPARAM).ps.slat = VALP_AS_DOUBLE;
        (*MPARAM).ps.is_north_pole = (*MPARAM).ps.slat > 0;
        return;
      }
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
    if ( !strcmp(field_name, "latitude") )
      { (*MPARAM).utm.lat0 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "longitude") )
      { (*MPARAM).utm.lon0 = VALP_AS_DOUBLE; return; }
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

  /* Fields that go in the (proj->param).mer block.  */
  if ( !strcmp(stack_top->block_name, "mer")) {
    if ( !strcmp(field_name, "standard_parallel") )
      { (*MPARAM).mer.standard_parallel = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "central_meridian") )
      { (*MPARAM).mer.central_meridian = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "orig_latitude") )
      { (*MPARAM).mer.orig_latitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).mer.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).mer.false_northing = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).eqr block.  */
  if ( !strcmp(stack_top->block_name, "eqr")) {
    if ( !strcmp(field_name, "central_meridian") )
      { (*MPARAM).eqr.central_meridian = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "orig_latitude") )
      { (*MPARAM).eqr.orig_latitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).eqr.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).eqr.false_northing = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).eqc block.  */
  if ( !strcmp(stack_top->block_name, "eqc")) {
    if ( !strcmp(field_name, "central_meridian") )
      { (*MPARAM).eqc.central_meridian = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "orig_latitude") )
      { (*MPARAM).eqc.orig_latitude = VALP_AS_DOUBLE; return; }
  }

  /* Fields that go in the (proj->param).sin block.  */
  if ( !strcmp(stack_top->block_name, "sin")) {
    if ( !strcmp(field_name, "longitude_center") )
      { (*MPARAM).sin.longitude_center = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).sin.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).sin.false_northing = VALP_AS_DOUBLE; return; }
  }

  // Fields that go in the (proj->param).cea block
  if ( !strcmp(stack_top->block_name, "cea")) {
    if ( !strcmp(field_name, "standard_parallel") )
      { (*MPARAM).cea.standard_parallel = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "central_meridian") )
      { (*MPARAM).cea.central_meridian = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_easting") )
      { (*MPARAM).cea.false_easting = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "false_northing") )
      { (*MPARAM).cea.false_northing = VALP_AS_DOUBLE; return; }
  }

  /* Note that the projection-specific param data block associated
     with LAT_LONG_PSEUDO_PROJECTION type files is empty, since all
     the parameters required for that projection are stored in the
     main block.  */

  // Fields which normally go in the transform block of the metadata file. */
  if ( !strcmp(stack_top->block_name, "transform") ) {
    int ii;
    char coeff[15];
    if ( !strcmp(field_name, "type") )
      { strcpy(MTRANSFORM->type, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "source pixel size") )
      { MTRANSFORM->source_pixel_size = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "target pixel size") )
      { MTRANSFORM->target_pixel_size = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "parameter_count") )
      { MTRANSFORM->parameter_count = VALP_AS_INT; return; }
    for (ii=0; ii< MTRANSFORM->parameter_count; ii++) {
      sprintf(coeff, "phi(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MTRANSFORM->y[ii] = VALP_AS_DOUBLE; return; }
    }
    for (ii=0; ii< MTRANSFORM->parameter_count; ii++) {
      sprintf(coeff, "lambda(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MTRANSFORM->x[ii] = VALP_AS_DOUBLE; return; }
    }
    if (MTRANSFORM->parameter_count >= 25) {
      if ( !strcmp(field_name, "origin pixel") )
    { MTRANSFORM->origin_pixel = VALP_AS_DOUBLE; return; }
      if ( !strcmp(field_name, "origin line") )
    { MTRANSFORM->origin_line = VALP_AS_DOUBLE; return; }
    }
    for (ii=0; ii< MTRANSFORM->parameter_count; ii++) {
      sprintf(coeff, "i(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MTRANSFORM->s[ii] = VALP_AS_DOUBLE; return; }
    }
    for (ii=0; ii< MTRANSFORM->parameter_count; ii++) {
      sprintf(coeff, "j(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MTRANSFORM->l[ii] = VALP_AS_DOUBLE; return; }
    }
    if (MTRANSFORM->parameter_count >= 25) {
      if ( !strcmp(field_name, "origin lat") )
    { MTRANSFORM->origin_lat = VALP_AS_DOUBLE; return; }
      if ( !strcmp(field_name, "origin lon") )
    { MTRANSFORM->origin_lon = VALP_AS_DOUBLE; return; }
    }
    // Deprecated in the transform block - Read in for SAR block
    for (ii=0; ii<6; ii++) {
      sprintf(coeff, "incid_a(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MSAR->incid_a[ii] = VALP_AS_DOUBLE; return; }
    }
    for (ii=0; ii<10; ii++) {
      sprintf(coeff, "map_a(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MTRANSFORM->map2ls_a[ii] = VALP_AS_DOUBLE; return; }
    }
    for (ii=0; ii<10; ii++) {
      sprintf(coeff, "map_b(%d)", ii);
      if ( !strcmp(field_name, coeff) )
      { MTRANSFORM->map2ls_b[ii] = VALP_AS_DOUBLE; return; }
    }
  }

  // Fields which normally go in the airsar block of the metadata file
  if ( !strcmp(stack_top->block_name, "airsar") ) {
    if ( !strcmp(field_name, "scale_factor") )
      { MAIRSAR->scale_factor = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "gps_altitude") )
      { MAIRSAR->gps_altitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lat_peg_point") )
      { MAIRSAR->lat_peg_point = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon_peg_point") )
      { MAIRSAR->lon_peg_point = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "head_peg_point") )
      { MAIRSAR->head_peg_point = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "along_track_offset") )
      { MAIRSAR->along_track_offset = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "cross_track_offset") )
      { MAIRSAR->cross_track_offset = VALP_AS_DOUBLE; return; }
  }

  // Fields which normally go in the uavsar block of the metadata file
  if ( !strcmp(stack_top->block_name, "uavsar") ) {
    if ( !strcmp(field_name, "id") )
      { strcpy(MUAVSAR->id, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "scale_factor") )
      { MUAVSAR->scale_factor = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "gps_altitude") )
      { MUAVSAR->gps_altitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lat_peg_point") )
      { MUAVSAR->lat_peg_point = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "lon_peg_point") )
      { MUAVSAR->lon_peg_point = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "head_peg_point") )
      { MUAVSAR->head_peg_point = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "along_track_offset") )
      { MUAVSAR->along_track_offset = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "cross_track_offset") )
      { MUAVSAR->cross_track_offset = VALP_AS_DOUBLE; return; }
  }

  // Fields which normally go in the dem block of the metadata file
  if ( !strcmp(stack_top->block_name, "dem") ) {
    if ( !strcmp(field_name, "source") )
      { strcpy(MDEM->source, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "format") )
      { strcpy(MDEM->format, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "tiles") )
      { strcpy(MDEM->tiles, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "min_value") )
      { MDEM->min_value = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "max_value") )
      { MDEM->max_value = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "mean_value") )
      { MDEM->mean_value = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "standard_deviation") )
      { MDEM->standard_deviation = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "unit_type") )
      { strcpy(MDEM->unit_type, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "no_data") )
      { MDEM->no_data = VALP_AS_DOUBLE; return; }
  }
  
  if ( !strcmp(stack_top->block_name, "quality") ) {
		if ( !strcmp(field_name, "bit_error_rate") )
			{ MQUALITY->bit_error_rate = VALP_AS_DOUBLE; return; }
		if ( !strcmp(field_name, "azimuth_resolution") )
			{ MQUALITY->azimuth_resolution = VALP_AS_DOUBLE; return; }
		if ( !strcmp(field_name, "range_resolution") )
			{ MQUALITY->range_resolution = VALP_AS_DOUBLE; return; }
		if ( !strcmp(field_name, "signal_to_noise_ratio") )
			{ MQUALITY->signal_to_noise_ratio = VALP_AS_DOUBLE; return; }
		if ( !strcmp(field_name, "peak_sidelobe_ratio") )
			{ MQUALITY->peak_sidelobe_ratio = VALP_AS_DOUBLE; return; }
		if ( !strcmp(field_name, "integrated_sidelobe_ratio") )
			{ MQUALITY->integrated_sidelobe_ratio = VALP_AS_DOUBLE; return; }
  }

  /* Fields which normally go in the statistics block of the metadata file. */
  //
  //
  // Statistics block(s) for metadata v2.4 and beyond (multi-band)
  if ( !strcmp(stack_top->block_name, "statistics") ) {
    if ( !strcmp(field_name, "band_count") )
    { (MSTATISTICS)->band_count = VALP_AS_INT; return; }
  }
  // Band stats block for metadata v2.4+
  if ( !strcmp(stack_top->block_name, "band_stats") )
  {
    if ( !strcmp(field_name, "band_id") )
    { strcpy((MSTATSBLOCK)->band_id, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "min") )
    { (MSTATSBLOCK)->min = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "max") )
    { (MSTATSBLOCK)->max = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "mean") )
    { (MSTATSBLOCK)->mean = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "rmse") )
    { (MSTATSBLOCK)->rmse = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "std_deviation") )
    { (MSTATSBLOCK)->std_deviation = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "percent_valid") )
    { (MSTATSBLOCK)->percent_valid = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "mask") )
    { (MSTATSBLOCK)->mask = VALP_AS_DOUBLE; return; }
  }
  // Band stats block for metadata v2.3-
  if ( !strcmp(stack_top->block_name, "stats") )
  {
    if ( !strcmp(field_name, "band_id") )
    { strcpy((MSTATS)->band_id, VALP_AS_CHAR_POINTER); return; }
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
    if ( !strcmp(field_name, "percent_valid") )
    { (MSTATS)->percent_valid = VALP_AS_DOUBLE; return; }
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

  // Fields which go in the insar block of the metadata file.
  if ( !strcmp(stack_top->block_name, "insar") ) {
    if ( !strcmp(field_name, "processor") )
      { strcpy((MINSAR)->processor, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "master_image") )
      { strcpy((MINSAR)->master_image, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "slave_image") )
      { strcpy((MINSAR)->slave_image, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "master_acquisition_date") )
      { strcpy((MINSAR)->master_acquisition_date, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "slave_acquisition_date") )
      { strcpy((MINSAR)->slave_acquisition_date, VALP_AS_CHAR_POINTER); return; }
    if ( !strcmp(field_name, "center_look_angle") )
      { (MINSAR)->center_look_angle = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "doppler") )
      { (MINSAR)->doppler = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "doppler_rate") )
      { (MINSAR)->doppler_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "baseline_length") )
      { (MINSAR)->baseline_length = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "baseline_parallel") )
      { (MINSAR)->baseline_parallel = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "baseline_parallel_rate") )
      { (MINSAR)->baseline_parallel_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "baseline_perpendicular") )
      { (MINSAR)->baseline_perpendicular = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "baseline_perpendicular_rate") )
      { (MINSAR)->baseline_perpendicular_rate = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "baseline_temporal") )
      { (MINSAR)->baseline_temporal = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "baseline_critical") )
      { (MINSAR)->baseline_critical = VALP_AS_DOUBLE; return; }
  }

  // Fields which go in the doppler block of the metadata file.
  if ( !strcmp(stack_top->block_name, "doppler") ) {
    int ii;
    char str[25];
    if ( !strcmp(field_name, "type") ) {
      // Initialize all the type pointers
      if ( ! strcmp(VALP_AS_CHAR_POINTER, "TSX") ) {
	tsx_doppler_params *tsx = 
	  (tsx_doppler_params *) MALLOC(sizeof(tsx_doppler_params));
	(MDOPPLER)->tsx = tsx;
	(MDOPPLER)->type = tsx_doppler;
	return;
      }
      if ( ! strcmp(VALP_AS_CHAR_POINTER, "RADARSAT2") ) {
	radarsat2_doppler_params *r2 =
	  (radarsat2_doppler_params *) MALLOC(sizeof(radarsat2_doppler_params));
	(MDOPPLER)->r2 = r2;
	(MDOPPLER)->type = radarsat2_doppler;
	return;
      }
    }
    // TSX Doppler
    if ( !strcmp(field_name, "year") && (MDOPPLER)->type == tsx_doppler)
      { (MDOPPLER)->tsx->year = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "julDay") && (MDOPPLER)->type == tsx_doppler)
      { (MDOPPLER)->tsx->julDay = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "second") && (MDOPPLER)->type == tsx_doppler)
      { (MDOPPLER)->tsx->second = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "doppler_count") && (MDOPPLER)->type == tsx_doppler)
      { (MDOPPLER)->tsx->doppler_count = VALP_AS_INT;
	tsx_doppler_t *dop = (tsx_doppler_t *) MALLOC(sizeof(tsx_doppler_t) * 
					      (MDOPPLER)->tsx->doppler_count);
	(MDOPPLER)->tsx->dop = dop;
	return; }
    // RADARSAT2 Doppler
    if ( !strcmp(field_name, "doppler_count") && 
	 (MDOPPLER)->type == radarsat2_doppler)
      { (MDOPPLER)->r2->doppler_count = VALP_AS_INT;
	double *centroid = 
	  (double *) MALLOC(sizeof(double)*(MDOPPLER)->r2->doppler_count);
	(MDOPPLER)->r2->centroid = centroid;
	double *rate =
	  (double *) MALLOC(sizeof(double)*(MDOPPLER)->r2->doppler_count);
	(MDOPPLER)->r2->rate = rate;
	return;
      }
    if ( !strcmp(field_name, "centroid_time") &&
	 (MDOPPLER)->type == radarsat2_doppler)
      { (MDOPPLER)->r2->ref_time_centroid = VALP_AS_DOUBLE; return; }
    for (ii=0; ii<(MDOPPLER)->r2->doppler_count; ii++) {
      sprintf(str, "centroid[%d]", ii);
      if ( !strcmp(field_name, str) && (MDOPPLER)->type == radarsat2_doppler)
	{ (MDOPPLER)->r2->centroid[ii] = VALP_AS_DOUBLE; return; }
    } 
    if ( !strcmp(field_name, "rate_time") &&
	 (MDOPPLER)->type == radarsat2_doppler)
      { (MDOPPLER)->r2->ref_time_rate = VALP_AS_DOUBLE; return; }
    for (ii=0; ii<(MDOPPLER)->r2->doppler_count; ii++) {
      sprintf(str, "rate[%d]", ii);
      if ( !strcmp(field_name, str) && (MDOPPLER)->type == radarsat2_doppler)
	{ (MDOPPLER)->r2->rate[ii] = VALP_AS_DOUBLE; return; }
    }
    if ( !strcmp(field_name, "time_first_sample") &&
	 (MDOPPLER)->type == radarsat2_doppler)
      { (MDOPPLER)->r2->time_first_sample = VALP_AS_DOUBLE; return; }
  }
  // TSX Doppler estimates
  if ( !strcmp(stack_top->block_name, "estimate") ) {
    int ii;
    char str[15];
    if ( !strcmp(field_name, "time") )
      { (MESTIMATE)->time = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "first_range_time") )
      { (MESTIMATE)->first_range_time = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "reference_time") )
      { (MESTIMATE)->reference_time = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "polynomial_degree") )
      { (MESTIMATE)->poly_degree = VALP_AS_INT;
	double *coefficient = 
	  (double *) MALLOC(sizeof(double) * ((MESTIMATE)->poly_degree + 1));
	(MESTIMATE)->coefficient = coefficient;
	return; }
    for (ii=0; ii<=(MESTIMATE)->poly_degree; ii++) {
      sprintf(str, "coefficient[%d]", ii);
      if ( !strcmp(field_name, str) )
	{ (MESTIMATE)->coefficient[ii] = VALP_AS_DOUBLE; return; }
    }
  }

  /* Fields which go in the calibration block of the metadata file. */
  if ( !strcmp(stack_top->block_name, "calibration") ) {
    int ii;
    char str[15];
    if ( !strcmp(field_name, "type") ) {
      // Initialize all the type pointers
      if ( !strcmp(VALP_AS_CHAR_POINTER, "ASF") ) {
        asf_cal_params *asf = (asf_cal_params *)MALLOC(sizeof(asf_cal_params));
        (MCALIBRATION)->asf = asf;
        for (ii=0; ii<256; ii++) {
            (MCALIBRATION)->asf->noise[ii] = 0.0;
        }
        (MCALIBRATION)->type = asf_cal;
        return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "ASF_SCANSAR") ) {
        asf_scansar_cal_params *asf = (asf_scansar_cal_params *)MALLOC(sizeof(asf_scansar_cal_params));
        (MCALIBRATION)->asf_scansar = asf;
        for (ii=0; ii<256; ii++) {
            (MCALIBRATION)->asf_scansar->noise[ii] = 0.0;
        }
        (MCALIBRATION)->type = asf_scansar_cal;
        return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "ESA") ) {
        esa_cal_params *esa = (esa_cal_params *) MALLOC(sizeof(esa_cal_params));
        (MCALIBRATION)->esa = esa;
        (MCALIBRATION)->type = esa_cal;
        return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "RSAT") ) {
        rsat_cal_params *rsat = (rsat_cal_params *) MALLOC(sizeof(rsat_cal_params));
        (MCALIBRATION)->rsat = rsat;
        (MCALIBRATION)->type = rsat_cal;
        return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "ALOS") ) {
        alos_cal_params *alos = (alos_cal_params *) MALLOC(sizeof(alos_cal_params));
        (MCALIBRATION)->alos = alos;
        (MCALIBRATION)->type = alos_cal;
	(MCALIBRATION)->alos->cf_hh = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->alos->cf_hv = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->alos->cf_vh = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->alos->cf_vv = MAGIC_UNSET_DOUBLE;
        return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "TSX") ) {
	tsx_cal_params *tsx = (tsx_cal_params *) MALLOC(sizeof(tsx_cal_params));
	(MCALIBRATION)->tsx = tsx;
	(MCALIBRATION)->type = tsx_cal;
	(MCALIBRATION)->tsx->k = MAGIC_UNSET_DOUBLE;
	return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "RSAT2") ) {
	r2_cal_params *r2 = (r2_cal_params *) MALLOC(sizeof(r2_cal_params));
	(MCALIBRATION)->r2 = r2;
	(MCALIBRATION)->type = r2_cal;
	(MCALIBRATION)->r2->num_elements = MAGIC_UNSET_INT;
        for (ii=0; ii<8192; ii++) {
	  (MCALIBRATION)->r2->a_beta[ii] = 0.0;
	  (MCALIBRATION)->r2->a_gamma[ii] = 0.0;
	  (MCALIBRATION)->r2->a_sigma[ii] = 0.0;
        }
	(MCALIBRATION)->r2->b = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->r2->slc = MAGIC_UNSET_INT;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "UAVSAR") ) {
	uavsar_cal_params *uavsar =
	  (uavsar_cal_params *) MALLOC(sizeof(uavsar_cal_params));
	(MCALIBRATION)->uavsar = uavsar;
	(MCALIBRATION)->type = uavsar_cal;
	(MCALIBRATION)->uavsar->semi_major = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->slant_range_first_pixel = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->range_spacing = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->azimuth_spacing = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->pitch = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->steering_angle = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->altitude = MAGIC_UNSET_DOUBLE;
	(MCALIBRATION)->uavsar->terrain_height = MAGIC_UNSET_DOUBLE;	
	return;
      }
      if ( !strcmp(VALP_AS_CHAR_POINTER, "SENTINEL") ) {
        sentinel_cal_params *sentinel =
          (sentinel_cal_params *) MALLOC(sizeof(sentinel_cal_params));
        (MCALIBRATION)->sentinel = sentinel;
        (MCALIBRATION)->type = sentinel_cal;
        (MCALIBRATION)->sentinel->noise_mean = MAGIC_UNSET_DOUBLE;
        return;
      }
    }
    // ASF calibration
    if ( !strcmp(field_name, "a(0)") && (MCALIBRATION)->type == asf_cal)
      { (MCALIBRATION)->asf->a0 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "a(1)") && (MCALIBRATION)->type == asf_cal)
      { (MCALIBRATION)->asf->a1 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "a(2)") && (MCALIBRATION)->type == asf_cal)
      { (MCALIBRATION)->asf->a2 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "sample_count") &&
     (MCALIBRATION)->type == asf_cal)
      { (MCALIBRATION)->asf->sample_count = VALP_AS_INT; return; }
    for (ii=0; ii<256; ii++) {
      sprintf(str, "noise(%d)", ii);
      if ( !strcmp(field_name, str) && (MCALIBRATION)->type == asf_cal)
    { (MCALIBRATION)->asf->noise[ii] = VALP_AS_DOUBLE; return; }
      if ( !strcmp(field_name, str) &&
       (MCALIBRATION)->type == asf_scansar_cal)
    { (MCALIBRATION)->asf_scansar->noise[ii] = VALP_AS_DOUBLE; return; }
    }
    // ASF ScanSAR calibration
    if ( !strcmp(field_name, "a(0)") &&
     (MCALIBRATION)->type == asf_scansar_cal)
      { (MCALIBRATION)->asf_scansar->a0 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "a(1)") &&
     (MCALIBRATION)->type == asf_scansar_cal)
      { (MCALIBRATION)->asf_scansar->a1 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "a(2)") &&
     (MCALIBRATION)->type == asf_scansar_cal)
      { (MCALIBRATION)->asf_scansar->a2 = VALP_AS_DOUBLE; return; }
    // ESA calibration
    if ( !strcmp(field_name, "k") && (MCALIBRATION)->type == esa_cal)
      { (MCALIBRATION)->esa->k = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "ref_incid") &&
     (MCALIBRATION)->type == esa_cal)
      { (MCALIBRATION)->esa->ref_incid = VALP_AS_DOUBLE; return; }
    // RSAT calibration
    if ( !strcmp(field_name, "table_entries") &&
     (MCALIBRATION)->type == rsat_cal)
      { (MCALIBRATION)->rsat->n = VALP_AS_INT; return; }
    if ( !strncmp(field_name, "lut", 3) && (MCALIBRATION)->type == rsat_cal)
    {
        for (ii=0; ii<(MCALIBRATION)->rsat->n; ii++) {
            sprintf(str, "lut(%d)", ii);
            if ( !strcmp(field_name, str) && (MCALIBRATION)->type == rsat_cal) {
                    (MCALIBRATION)->rsat->lut[ii] = VALP_AS_DOUBLE;
                    return;
            }
        }
    }
    if ( !strcmp(field_name, "sample_inc") && (MCALIBRATION)->type == rsat_cal)
      { (MCALIBRATION)->rsat->samp_inc = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "a3") && (MCALIBRATION)->type == rsat_cal)
      { (MCALIBRATION)->rsat->a3 = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "slc") && (MCALIBRATION)->type == rsat_cal)
      { (MCALIBRATION)->rsat->slc = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "focus") && (MCALIBRATION)->type == rsat_cal)
      { (MCALIBRATION)->rsat->focus = VALP_AS_INT; return; }
    // ALOS calibration
    if ( !strcmp(field_name, "cf_hh") && (MCALIBRATION)->type == alos_cal)
      { (MCALIBRATION)->alos->cf_hh = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "cf_hv") && (MCALIBRATION)->type == alos_cal)
      { (MCALIBRATION)->alos->cf_hv = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "cf_vh") && (MCALIBRATION)->type == alos_cal)
      { (MCALIBRATION)->alos->cf_vh = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "cf_vv") && (MCALIBRATION)->type == alos_cal)
      { (MCALIBRATION)->alos->cf_vv = VALP_AS_DOUBLE; return; }
    // long live backwards compatibility
    // assign the value to all bands, since we don't know any better
    if ( !strcmp(field_name, "cf") && (MCALIBRATION)->type == alos_cal)
      { (MCALIBRATION)->alos->cf_hh = VALP_AS_DOUBLE;
	(MCALIBRATION)->alos->cf_hv = VALP_AS_DOUBLE;
	(MCALIBRATION)->alos->cf_vh = VALP_AS_DOUBLE;
	(MCALIBRATION)->alos->cf_vv = VALP_AS_DOUBLE; 
	return; }
    // TSX calibration
    if ( !strcmp(field_name, "k") && (MCALIBRATION)->type == tsx_cal)
      { (MCALIBRATION)->tsx->k = VALP_AS_DOUBLE; return; }
    // RADARSAT-2 calibration
    if ( !strcmp(field_name, "num_elements") && (MCALIBRATION)->type == r2_cal)
      { (MCALIBRATION)->r2->num_elements = VALP_AS_INT; return; }
    if ( !strncmp(field_name, "gain", 4) && (MCALIBRATION)->type == r2_cal)
    {
      char val[256], val2[256];
      for (ii=0; ii<(MCALIBRATION)->r2->num_elements; ii++) {
	sprintf(val, "gain(%03d)", ii);
	sprintf(val2, "gain(%d)", ii);
	if ( !strcmp(field_name, val) || !strcmp(field_name, val2) ) {
	  double beta, gamma, sigma;
	  char vals[256];
	  strcpy(vals, VALP_AS_CHAR_POINTER);
	  sscanf(vals, "%lf %lf %lf", &beta, &gamma, &sigma);
	  (MCALIBRATION)->r2->a_beta[ii] = beta;
	  (MCALIBRATION)->r2->a_gamma[ii] = gamma;
	  (MCALIBRATION)->r2->a_sigma[ii] = sigma;
	}
      }
    }
    if ( !strcmp(field_name, "b") && (MCALIBRATION)->type == r2_cal)
      { (MCALIBRATION)->r2->b = VALP_AS_INT; return; }
    if ( !strcmp(field_name, "slc") && (MCALIBRATION)->type == r2_cal)
      { (MCALIBRATION)->r2->slc = VALP_AS_INT; return; }

    // UAVSAR calibration
    if ( !strcmp(field_name, "semi_major") && 
	 (MCALIBRATION)->type == uavsar_cal) 
      { (MCALIBRATION)->uavsar->semi_major = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "slant_range_first_pixel") &&
	 (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->slant_range_first_pixel = VALP_AS_DOUBLE; 
	return; }
    if ( !strcmp(field_name, "range_spacing") &&
	 (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->range_spacing = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "azimuth_spacing") &&
	 (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->azimuth_spacing = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "pitch") && (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->pitch = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "steering_angle") &&
	 (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->steering_angle = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "altitude") && (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->altitude = VALP_AS_DOUBLE; return; }
    if ( !strcmp(field_name, "terrain_height") &&
	 (MCALIBRATION)->type == uavsar_cal)
      { (MCALIBRATION)->uavsar->terrain_height = VALP_AS_DOUBLE; return; }
      
    // SENTINEL calibration  
    if ( !strcmp(field_name, "noise_mean") &&
      (MCALIBRATION)->type == sentinel_cal)
      { (MCALIBRATION)->sentinel->noise_mean = VALP_AS_DOUBLE; return; }
  }

  /* Fields which normally go in a colormap block.  */
  if ( !strcmp(stack_top->block_name, "colormap") ) {
      int ii;
      char val[256], val2[256];
      if ( !strcmp(field_name, "look_up_table") )
      { strcpy((MCOLORMAP)->look_up_table, VALP_AS_CHAR_POINTER); return; }
      if ( !strcmp(field_name, "band_id") )
	{ strcpy((MCOLORMAP)->band_id, VALP_AS_CHAR_POINTER); return; }
      if ( !strcmp(field_name, "num_elements") )
      {
          (MCOLORMAP)->num_elements = VALP_AS_INT;
          (MCOLORMAP)->rgb = (meta_rgb *)CALLOC((MCOLORMAP)->num_elements, sizeof(meta_rgb));
          return;
      }
      for (ii=0; ii < (MCOLORMAP)->num_elements; ii++) {
          sprintf(val, "idx(%03d)", ii);
          sprintf(val2, "idx(%d)", ii);
          if ( !strcmp(field_name, val) || !strcmp(field_name, val2) ) {
              int red, green, blue;
              char vals[256];
              strcpy(vals, VALP_AS_CHAR_POINTER);
              sscanf(vals, "%d %d %d", &red, &green, &blue);
              (MCOLORMAP)->rgb[ii].red = red;
              (MCOLORMAP)->rgb[ii].green = green;
              (MCOLORMAP)->rgb[ii].blue = blue;
          }
      }
  }

  /* Got an unknown field name, so report & choke */
  /* During testing it makes sense to have this warning, but for the release
     let's just comment it out... very annoying */
  // fprintf (stderr, "Warning: Unknown field name: %s\n", field_name);
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
             | STRING { $$ = STRDUP($1); }
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

  /***                                             ***/
  /* FIXME: current_file should be forcefully null-  */
  /*   terminated after the strncpy() below.         */
  /***                                             ***/
  /* Put file name in a global for error reporting.  */
  strncpy(current_file, file_name, MAX_FILE_NAME);

  /* (Re)set line_number to first line */
  line_number = 1;

  /* (Re)set file scope variable which counts number of vector blocks seen.  */
  vector_count = 0;
  doppler_count = 0;
  stats_block_count = 0;

  meta_yyin = FOPEN(file_name, "r");
  stack_top = NULL;
  block_stack_push(&stack_top, "outermost_section", dest);

  /* Parse metadata file.  */
  ret_val = yyparse();

  /* Fill in number of state vectors seen.  */
  if ((dest->state_vectors) && (dest->state_vectors->vector_count != vector_count)) {
    warning_message("Said number of vectors in state vector block (%d)\n"
                    "differs from the actual amount of vectors (%d)...\n"
                    "Using actual number of vectors for vector_count.\n",
                    dest->state_vectors->vector_count, vector_count);
    dest->state_vectors->vector_count = vector_count;
    dest->state_vectors->num = vector_count; /* Backward compat alias.  */
  }

  /* Fill in number of stats blocks seen.  */
  if (dest->stats             &&
      dest->stats->band_count != stats_block_count)
  {
    warning_message("Said number of stats blocks in stats (%d)\n"
        "differs from the actual amount of stats blocks (%d)...\n"
        "Using actual number of stats blocks for stats_blocks_count.\n",
    dest->stats->band_count, stats_block_count);
    dest->stats->band_count = stats_block_count;
  }

  /* Done with the block stack, so empty it.  */
  while ( stack_top != NULL ) {
    block_stack_pop (&stack_top);
  }

  FCLOSE(meta_yyin);

  return ret_val;
}
