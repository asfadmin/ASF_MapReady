/* Parser for ASF image metadata files.  */

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asf_meta.h"
#include "lex_yacc.h"

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
    MSTATE->num = vector_count;
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
#define VALP_AS_INT *( (int *) valp)
#define VALP_AS_DOUBLE *( (double *) valp)
#define VALP_AS_CHAR_POINTER ( (char *) valp)

void fill_structure_field(char *field_name, void *valp)
{
  /* Pointer to substructure corresponding to current block.  */ 
  void *current_block = stack_top->block;

  extern int yydebug;
  yydebug = 1;

  /* Top-level fields (these normally go outside all blocks).  */
  if ( !strcmp(field_name, "Meta version") ) {
    MTL->meta_version = VALP_AS_DOUBLE; 
    return;
    
  }

  /* Fields which normally go in the general block of the metadata file.  */
  if ( !strcmp(field_name, "Imaging satellite") ) 
    { !strcpy(MGENERAL->sensor, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "Imaging mode") ) {
    strncpy(MGENERAL->mode, VALP_AS_CHAR_POINTER, MODE_FIELD_STRING_MAX); 
    return; 
  }
  if ( !strcmp(field_name, "Name and version of processor") )
    { strcpy(MGENERAL->processor, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "Sample data type") )
    { strcpy(MGENERAL->data_type, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "Sample system type") )
    { strcpy(MGENERAL->system, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "Orbit number") )
    { MGENERAL->orbit = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Frame number") )
    { MGENERAL->frame = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Number of lines") )
    { MGENERAL->line_count = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Number of samples") )
    { MGENERAL->sample_count = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Image start line") )
    { MGENERAL->start_line = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Image start sample") )
    { MGENERAL->start_sample = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Pixel size in x direction") )
    { MGENERAL->x_pixel_size = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Pixel size in y direction") )
    { MGENERAL->y_pixel_size = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Approximate image center latitude") )
    { MGENERAL->center_latitude = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Approximate image center longitude") )
    { MGENERAL->center_longitude = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Bit error rate") )
    { MGENERAL->bit_error_rate = VALP_AS_DOUBLE; return; }

  if ( !strcmp(field_name, "Simple field") ) {
    printf("SAW SIMPLE FIELD\n");
    return;
  }

  /* Fields which normally go in the sar block of the metadata file.  */
  if ( !strcmp(field_name, "Image type [S=slant range; G=ground range; P=map projected]") ) { 
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
  if ( !strcmp(field_name, "SAR Satellite look direction (normally R) [R=right; L=left]") ) { 
    if ( !strcmp(VALP_AS_CHAR_POINTER, "R") ) { 
      MSAR->look_direction = 'R'; return; 
    }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "L") ) { 
      MSAR->look_direction = 'L'; return; 
    }
    yyerror("bad Satellite look direction field in metadata file");
    exit(EXIT_FAILURE);
  }
  if ( !strcmp(field_name, "Image moved to zero doppler [1=yes, 0=no]") )
    { MSAR->deskewed = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Time/pixel, range") )
    { MSAR->range_time_per_pixel = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Time/pixel, azimuth") )
    { MSAR->azimuth_time_per_pixel = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Look angle") )
    { MSAR->look_angle = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Slant range to first image pixel") )
    { MSAR->slant_range_first_pixel = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "SAR carrier wavelength") )
    { MSAR->wavelength = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Pulse repetition frequency") )
    { MSAR->prf = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Number of looks to take from SLC") )
    { MSAR->look_count = VALP_AS_INT; return; }
  if ( !strcmp(field_name, "Range doppler centroid") )
    { MSAR->range_doppler_coefficients[0] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Range doppler per pixel") )
    { MSAR->range_doppler_coefficients[1] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Range doppler per pixel squared") )
    { MSAR->range_doppler_coefficients[2] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Azimuth doppler centroid") )
    { MSAR->azimuth_doppler_coefficients[0] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Azimuth doppler per pixel") )
    { MSAR->azimuth_doppler_coefficients[1] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Azimuth doppler per pixel squared") )
    { MSAR->azimuth_doppler_coefficients[2] = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Satellite binary time") )
    { strcpy(MSAR->satellite_binary_time, VALP_AS_CHAR_POINTER); return; }
  if ( !strcmp(field_name, "Satellite clock time (UTC)") )
    { strcpy(MSAR->satellite_clock_time, VALP_AS_CHAR_POINTER); return; }

  /* Fields which normally go in the state block of the metadata file.  */
  if ( !strcmp(field_name, "Year of image start") )
    { MSTATE->year = *( (int *) valp); return; }
  if ( !strcmp(field_name, "Julian day of the year for image start") )
    { MSTATE->julDay = *( (int *) valp); return; }
  if ( !strcmp(field_name, "Second of the day for image start") )
    { MSTATE->second = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Number of state vector blocks") )
    { /* This field is allowed but ignored, we count the number of state
         vector blocks that we actually see.  */ 
      ;
    }

  /* Fields which normally go in a vector block.  */
  if ( !strcmp(field_name, "Time relative to image start") )
    { MVECTOR->time = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "X coordinate, earth-fixed") )
    { MVECTOR->vec.pos.x = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Y coordinate, earth-fixed") )
    { MVECTOR->vec.pos.y = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Z coordinate, earth-fixed") )
    { MVECTOR->vec.pos.z = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "X velocity, earth-fixed") )
    { MVECTOR->vec.vel.x = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Y velocity, earth-fixed") )
    { MVECTOR->vec.vel.y = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Z velocity, earth-fixed") )
    { MVECTOR->vec.vel.z = VALP_AS_DOUBLE; return; }

  /* Code for dealing with optical and/or thermal blocks could be
     added here.  */

  /* Fields which normaly go in the projection block of the metadata file.  */
  if ( !strcmp(field_name, "Projection type") ) {
    if ( !strcmp(VALP_AS_CHAR_POINTER, "A") ) { MPROJ->type = 'A'; return; }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "P") ) { MPROJ->type = 'P'; return; }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "L") ) { MPROJ->type = 'L'; return; }
    if ( !strcmp(VALP_AS_CHAR_POINTER, "U") ) { MPROJ->type = 'U'; return; }
    yyerror("bad Projection type field in metadata file");
    exit(EXIT_FAILURE);
  }
  if ( !strcmp(field_name, "Projection coordinate upper left in x") )
    { MPROJ->startX = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Projection coordinate upper left in y") )
    { MPROJ->startY = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Projection coordinates per pixel in x") )
    { MPROJ->perX = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Projection coordinates per pixel in y") )
    { MPROJ->perY = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Hemisphere code ['S'->southern; other northern]") 
       ) {
    if ( !strcmp(VALP_AS_CHAR_POINTER, "S") ) { MPROJ->type = 'S'; return; }
    else { MPROJ->type = 'N'; return; }
  }
  if ( !strcmp(field_name, "Semi-major axis length") )
    { MPROJ->re_major = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Semi-minor axis length") )
    { MPROJ->re_minor = VALP_AS_DOUBLE; return; }

  /* Fields that go in the (proj->param).atct block.  */
  if ( !strcmp(field_name, "Earth radius at scene center") )
    { (*MPARAM).atct.rlocal = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Rotation angle 1") )
    { (*MPARAM).atct.alpha1 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Rotation angle 2") )
    { (*MPARAM).atct.alpha2 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Rotation angle 3") )
    { (*MPARAM).atct.alpha3 = VALP_AS_DOUBLE; return; }

  /* Fields that go in the (proj->param).lambert block.  */
  if ( !strcmp(field_name, "First standard parallel") )
    { (*MPARAM).lambert.plat1 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Second standard parallel") )
    { (*MPARAM).lambert.plat2 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Original latitude") )
    { (*MPARAM).lambert.lat0 = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Original longitude") )
    { (*MPARAM).lambert.lon0 = VALP_AS_DOUBLE; return; }

  /* Fields that go in the (proj->param).ps block.  */
  if ( !strcmp(field_name, "Reference latitude") )
    { (*MPARAM).ps.slat = VALP_AS_DOUBLE; return; }
  if ( !strcmp(field_name, "Reference longitude") )
    { (*MPARAM).ps.slon = VALP_AS_DOUBLE; return; }


  if ( !strcmp(field_name, "Zone") )
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
  int int_val;
  double double_val;
  char string_val[MAX_SYMBOL_STRING + 1];   /* Null terminated.  */
  void *var_type;			    /* Pointer to other type value.  */
}

%token <int_val> INT
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

field_value:   INT    { int *tmp = (int *) malloc(sizeof(int));
                        *tmp = $1; 
                        $$ = tmp; }
             | DOUBLE { double *tmp = (double *) malloc(sizeof(double));
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
  yyin = fopen(file_name, "r");
  if ( !(stack_top = malloc(sizeof(block_stack_node))) ) {
    fprintf(stderr, "malloc failed");
    exit(EXIT_FAILURE);
  }
  block_stack_push(&stack_top, dest);

  return !yyparse();
}
