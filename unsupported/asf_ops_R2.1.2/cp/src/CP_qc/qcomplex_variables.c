#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qcomplex_variables_c[] = "@(#)qcomplex_variables.c	1.12 97/02/12 09:06:07";

/* 
 * Sets up global variables found in a non-averaged / non-detected 
 * complex ceos file 
 */

void
set_up_unaveraged_complex_global_variables()
{
 
   _image_size_x = (_records_length - _left_pixel_pad
                    - _suffix_pixels) / 4;
   
/*** it was
   _image_size_x = (_records_length - _left_border_pixels
                    - _right_border_pixels - _suffix_pixels) / 4;
***/
   
   if (_image_size_x != _pixels_per_line) {
      printf("_image size x is %d, _pixels_per_line is %d.\n",
 	      _image_size_x, _pixels_per_line);
      make_announcement("CEOS file error: inconsistancy in complex image width", LOG);
      exit(QC_LINE_SIZE_MISMATCH);
   }
   
   _image_size_y = _records_count - _top_border_lines - _bottom_border_lines;
   
   if (_image_size_y != _lines_per_channel) {
      make_announcement("CEOS file error: inconsistancy in complex image height", LOG);
      exit(QC_CHANNEL_SIZE_MISMATCH);
   }
   
   if (_image_scale_factor == 0)
      /* If this is the very first time through... */
      _image_scale_factor = figure_out_scale_factor();
}



/* 
 * Sets up global variables found in a averaged / detected 
 * complex ceos file 
 */

void
set_up_averaged_complex_global_variables() 
{
 
   _image_size_x = _records_length - _left_border_pixels;
/***
     - _right_border_pixels - _suffix_pixels ;
***/
   
   if (_image_size_x != _pixels_per_line) {
      printf("_image size x is %d, _pixels_per_line is %d.\n",
 	      _image_size_x, _pixels_per_line);
       
      make_announcement("CEOS file error: inconsistancy in complex image width", LOG);
       
      exit(QC_LINE_SIZE_MISMATCH);
   }
   
   _image_size_y = _records_count - _top_border_lines - _bottom_border_lines;
   
   if (_image_size_y != _lines_per_channel) {
      printf("image size y is %d, lines per channel is %d.\n",
 	      _image_size_y, _lines_per_channel);
 
      make_announcement("CEOS file error: inconsistancy in complex image height", LOG);
      exit(QC_CHANNEL_SIZE_MISMATCH);
   }
   
   if (_image_scale_factor == 0)
      /* If this is the very first time through... */
      _image_scale_factor = figure_out_scale_factor();
 
}
