#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gcomplex_variables_c[] = "@(#)gcomplex_variables.c	1.3 96/12/30 14:14:43";

/* 
 * Sets up global variables found in a non-averaged / non-detected 
 * complex ceos file 
 */

void
set_up_unaveraged_complex_global_variables()
{
 
   _image_size_x = (_records_length - _left_pixel_pad) / 4;
   
   if (_image_size_x != _pixels_per_line) {
      /*
      printf("_image size x is %d, _pixels_per_line is %d.\n",
 	      _image_size_x, _pixels_per_line);
      */
	printfLLog(LOG_ERR,
	    "ERROR:  Image x-dimension (%d bytes) inconsistent with expected length (%d bytes)",
	    _image_size_x, _pixels_per_line);

      exit(QC_LINE_SIZE_MISMATCH);
   }
   
   _image_size_y = _records_count - _top_border_lines - _bottom_border_lines;
   
   if (_image_size_y != _lines_per_channel) {
	printfLLog(LOG_ERR,
	    "ERROR:  Image y-dimension (%d records) inconsistent with expected number (%d records)",
	    _image_size_y, _lines_per_channel);
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
                   - _right_border_pixels;
***/
   
   if (_image_size_x != _pixels_per_line) {
      /*
      printf("_image size x is %d, _pixels_per_line is %d.\n",
 	      _image_size_x, _pixels_per_line);
      */
       
	printfLLog(LOG_ERR,
	    "ERROR:  Image x-dimension (%d bytes) inconsistent with expected length (%d bytes)",
	    _image_size_x, _pixels_per_line);
       
      exit(QC_LINE_SIZE_MISMATCH);
   }
   
   _image_size_y = _records_count - _top_border_lines - _bottom_border_lines;
   
   if (_image_size_y != _lines_per_channel) {
      /*
      printf("image size y is %d, lines per channel is %d.\n",
 	      _image_size_y, _lines_per_channel);
      */
 
	printfLLog(LOG_ERR,
	    "ERROR:  Image y-dimension (%d records) inconsistent with expected number (%d records)",
	    _image_size_y, _lines_per_channel);
      exit(QC_CHANNEL_SIZE_MISMATCH);
   }
   
   if (_image_scale_factor == 0)
      /* If this is the very first time through... */
      _image_scale_factor = figure_out_scale_factor();
 
}
