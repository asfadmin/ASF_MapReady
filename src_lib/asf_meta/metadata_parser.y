/* Parser for ASF image metadata files.  */

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asf_meta.h"
#include "lex_yacc.h"

/* Lex provides this parser function.  */
int yylex(void);

/* Node type for stack of pointers to structure subelements.  */
typedef struct block_stack_node_struct {
  struct block_stack_node_struct *next;
  void *block;
} block_stack_node;

/* Top of the push down stack.  */
block_stack_node *stack_top;

static void block_stack_push(block_stack_node **stack_top_p, void *new_block) 
{
  block_stack_node *new_node = malloc(sizeof(block_stack_node));
  new_node->next = *stack_top_p;
  new_node->block = new_block;
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
int vector_count = 0;

/* Help parser handle errors.  */
int yyerror(char *s)
{
  fprintf(stderr, "%s\n", s);
  return -1;			/* No error codes yet.  */
}

/* Casting shorthand macros for metadata structure subelements.  */
#define MTL ( (meta_parameters *) current_block)
#define MGENERAL ( (meta_general *) current_block)
#define MSAR ( (meta_sar *) current_block)
#define MSTATE ( (meta_state_vectors *) current_block)
#define MVECTOR ( (state_loc *) current_block)
#define MPROJ ( (meta_projection *) current_block)
#define MPARAM ( (param_t *) current_block)

void select_current_block(char *block_name)
{
  void *current_block = stack_top->block;

  if ( !strcmp(block_name, "general") )
    { current_block = MTL->general; goto MATCHED; }
  if ( !strcmp(block_name, "sar") )
    { current_block = MTL->sar; goto MATCHED; }
  if ( !strcmp(block_name, "state") )
    { current_block = MTL->state_vectors; goto MATCHED; }
  if ( !strcmp(block_name, "vector") ) { 
    ( (meta_state_vectors *) current_block)->vecs 
      = realloc(( (meta_state_vectors *) current_block)->vecs, 
		(vector_count + 1) * sizeof(state_loc));
    current_block = &( ((meta_state_vectors *) current_block)
		       ->vecs[vector_count++]); 
    goto MATCHED; 
  }

  if ( !strcmp(block_name, "projection") )
    { current_block = MTL->projection; goto MATCHED; }
  if ( !strcmp(block_name, "param") )
    { current_block = &(MPROJ->param); goto MATCHED; }
  if ( !strcmp(block_name, "atct") )
    { current_block = &((*( (param_t *) current_block)).atct); goto MATCHED; }
  if ( !strcmp(block_name, "lambert") ) { 
    current_block = &((*( (param_t *) current_block)).lambert); 
    goto MATCHED; 
  }
  if ( !strcmp(block_name, "ps") )
    { current_block = &((*( (param_t *) current_block)).ps); goto MATCHED; }
  if ( !strcmp(block_name, "utm") )
    { current_block = &((*( (param_t *) current_block)).utm); goto MATCHED; }

  /* Got an unknown block name, so report a parse error.  */
  { 
    char err_string[MAX_ERROR_STRING + 1] = "unknown block name: ";
    strncat(err_string, block_name, MAX_ERROR_STRING - strlen(err_string));
    yyerror(err_string);
    exit(EXIT_FAILURE);
  }

MATCHED: block_stack_push(&stack_top, current_block);
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

  /* Uncomment these lines and enable yacc debug flags in the make
   * file to debug the parser.
   *
   * extern int yydebug;
   * yydebug = 1; 
   */

  /* Top-level fields (these normally go outside all blocks).  */
  if ( !strcmp(field_name, "meta_version") ) {
    MTL->meta_version = VALP_AS_DOUBLE; 
    return;
  }

  /* Fields which normally go in the general block of the metadata file.  */
  if ( !strcmp(field_name, "sensor") ) 
    { !strcpy(MGENERAL->sensor, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "mode") ) {
    strncpy(MGENERAL->mode, VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX); 
    return; 
  }
  if ( !strcmp(field_name, "processor") )
    { strcpy(MGENERAL->processor, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "data_type") )
    { strcpy(MGENERAL->data_type, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "system") )
    { strcpy(MGENERAL->system, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "orbit") )
    { MGENERAL->orbit = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "frame") )
    { MGENERAL->frame = VALP_AS_INT; return; }
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
  if ( !strcmp(field_name, "bit_error_rate") )
    { MGENERAL->bit_error_rate = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "missing_lines") )
    { MGENERAL->missing_lines = VALP_AS_INT; return; }

  /* Fields which normally go in the sar block of the metadata file.  */
  if ( !strcmp(field_name, "proj_type") ) { 
    if ( !strcmp(VALP_AS_CHAR_POINTER, "S") ) { 
      MSAR->proj_type = 'S'; 
      return; 
    }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "G") ) { 
      MSAR->proj_type = 'G'; 
      return; 
    }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "P") ) { 
      MSAR->proj_type = 'P'; 
      return; 
    }
    yyerror("bad Image type field in metadata file");
    exit(EXIT_FAILURE);
  }
  if ( !strcmp(field_name, "look_direction") ) { 
    if ( !strcmp(VALP_AS_CHAR_POINTER, "R") ) { 
      MSAR->look_direction = 'R'; return; 
    }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "L") ) { 
      MSAR->look_direction = 'L'; return; 
    }
    yyerror("bad Satellite look direction field in metadata file");
    exit(EXIT_FAILURE);
  }
  if ( !strcmp(field_name, "look_angle") )
    { MSAR->look_angle = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "look_count") )
    { MSAR->look_count = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "deskewed") )
    { MSAR->deskewed = VALP_AS_INT; return; }
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
  if ( !strcmp(field_name, "doppler range center frequency") )
    { MSAR->range_doppler_coefficients[0] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "doppler range linear coefficient") )
    { MSAR->range_doppler_coefficients[1] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "doppler range quadratic coefficient") )
    { MSAR->range_doppler_coefficients[2] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "doppler azimuth center frequency") )
    { MSAR->azimuth_doppler_coefficients[0] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "doppler azimuth linear coefficient") )
    { MSAR->azimuth_doppler_coefficients[1] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "doppler azimuth quadratic coefficient") )
    { MSAR->azimuth_doppler_coefficients[2] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "satellite_binary_time") )
    { strcpy(MSAR->satellite_binary_time, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "satellite_clock_time") )
    { strcpy(MSAR->satellite_clock_time, VALP_AS_CHAR_POINTER); return; }

  /* Fields which normally go in the state block of the metadata file.  */
  if ( !strcmp(field_name, "year") )
    { MSTATE->year = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "julDay") )
    { MSTATE->julDay = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "second") )
    { MSTATE->second = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "vector_count") )
    { /* This field is allowed but ignored, we count the number of state
         vector blocks that we actually see.  */ 
      return;
    }

  /* Fields which normally go in a vector block.  */
  if ( !strcmp(field_name, "time") )
    { MVECTOR->time = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "X coordinate, earth-fixed [m]") )
    { MVECTOR->vec.pos.x = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Y coordinate, earth-fixed [m]") )
    { MVECTOR->vec.pos.y = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Z coordinate, earth-fixed [m]") )
    { MVECTOR->vec.pos.z = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "X velocity, earth-fixed [m]") )
    { MVECTOR->vec.vel.x = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Y velocity, earth-fixed [m]") )
    { MVECTOR->vec.vel.y = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Z velocity, earth-fixed [m]") )
    { MVECTOR->vec.vel.z = VALP_AS_DOUBLE; return; }

  /* Code for dealing with optical and/or thermal blocks could be
     added here.  */

  /* Fields which normaly go in the projection block of the metadata file.  */
  if ( !strcmp(field_name, "type") ) {
    if ( !strcmp(VALP_AS_CHAR_POINTER, "A") ) { MPROJ->type = 'A'; return; }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "P") ) { MPROJ->type = 'P'; return; }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "L") ) { MPROJ->type = 'L'; return; }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "U") ) { MPROJ->type = 'U'; return; }
    yyerror("bad Projection type field in metadata file");
    exit(EXIT_FAILURE);
  }
  if ( !strcmp(field_name, "startX") )
    { MPROJ->startX = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "startY") )
    { MPROJ->startY = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "perX") )
    { MPROJ->perX = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "perY") )
    { MPROJ->perY = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "hem") ) {
    if ( !strcmp(VALP_AS_CHAR_POINTER, "S") ) { MPROJ->type = 'S'; return; }
    else { MPROJ->type = 'N'; return; }
  }
  if ( !strcmp(field_name, "re_major") )
    { MPROJ->re_major = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "re_minor") )
    { MPROJ->re_minor = VALP_AS_DOUBLE; return; }

  /* Fields that go in the (proj->param).atct block.  */
  if ( !strcmp(field_name, "rlocal") )
    { (*MPARAM).atct.rlocal = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "alpha1") )
    { (*MPARAM).atct.alpha1 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "alpha2") )
    { (*MPARAM).atct.alpha2 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "alpha3") )
    { (*MPARAM).atct.alpha3 = VALP_AS_DOUBLE; return; }

  /* Fields that go in the (proj->param).lambert block.  */
  if ( !strcmp(field_name, "plat1") )
    { (*MPARAM).lambert.plat1 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "plat2") )
    { (*MPARAM).lambert.plat2 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "lat0") )
    { (*MPARAM).lambert.lat0 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "lon0") )
    { (*MPARAM).lambert.lon0 = VALP_AS_DOUBLE; return; }

  /* Fields that go in the (proj->param).ps block.  */
  if ( !strcmp(field_name, "lat") )
    { (*MPARAM).ps.slat = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "lon") )
    { (*MPARAM).ps.slon = VALP_AS_DOUBLE; return; }


  if ( !strcmp(field_name, "zone") )
    { (*MPARAM).utm.zone = VALP_AS_INT; return; }

  /* Code for dealing with statistics blocks could be added here.  */

  /* Got an unknown field name, so report a parse error.  */
  { 
    char err_string[MAX_ERROR_STRING + 1] = "unknown field name: ";
    strncat(err_string, field_name, MAX_ERROR_STRING - strlen(err_string));
    yyerror(err_string);
    exit(EXIT_FAILURE);
  }


  /* Got an unknown field name, so report a parse error.  */
  yyerror("unknown field name");
}

%}

%start element_seq

%union {			   /* Define parser stack type.  */
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
  extern FILE *yyin;
  int ret_val;

  yyin = fopen(file_name, "r");
  if ( !(stack_top = malloc(sizeof(block_stack_node))) ) {
    fprintf(stderr, "malloc failed");
    exit(EXIT_FAILURE);
  }
  block_stack_push(&stack_top, dest);

  /* Parse metadata file.  */
  ret_val = yyparse();

  /* Fill in number of state vectors seen.  */
  dest->state_vectors->vector_count = vector_count;
  dest->state_vectors->num = vector_count; /* Backward compar alias.  */

  return ret_val;
}
