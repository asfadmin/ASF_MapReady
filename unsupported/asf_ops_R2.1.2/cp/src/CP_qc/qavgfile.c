#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qavgfile_c[] = "@(#)qavgfile.c	1.12 97/02/12 09:05:58";

/* 
 * This function re-sets global variables and 
 * sets up buffers if an average file is used. 
 */

void
clean_up_after_avgfile(picture_only_byte_data)
   byte  *picture_only_byte_data;     
{
 
   int   unaveraged_file_descriptor;
   byte  *first_record_byte_data;
 
   /* Now, set up the global variables to their proper values,
    * not those found in the average file.
    * Check to see if the picture only byte data
    * was altered to get rid of the background.
    */
   if (_unaltered_bytes) {
      /* Small picture has to hold a copy of the unaltered
 	 bytes, since we free the unaltered bytes later */
      _small_picture = (byte *) malloc(_image_size_x *
 			_image_size_y * sizeof(byte));
 
      if (! _small_picture) { 
         make_announcement("Problem mallocing", LOG);
 	 exit(QC_MALLOC_ERROR);
      }
 
      memmove(_small_picture, _unaltered_bytes,
              (_image_size_x * _image_size_y));
   }
   else {
      _small_picture = picture_only_byte_data;
   }
 
   /* Put together values from the imagefile (full-sized) */
   unaveraged_file_descriptor = open(_filename, O_RDONLY);
 
   read_first_record_byte_data(unaveraged_file_descriptor, 
                               &first_record_byte_data);
       
   /* you're done with the file, */
   close(unaveraged_file_descriptor);
 
   /* remove avgfile option */
   _avg_file_flag = 0;
   get_information_from_file(first_record_byte_data);
   free(first_record_byte_data);
   _avg_file_flag = 1;
 
   if (_image_type == COMPLEX) {
      /* set up global variables */
      _image_size_x = _pixels_per_line;
 
      _image_size_y = _lines_per_channel;
 	  
      _image_scale_factor = figure_out_scale_factor();
 
      /* This is done here since the figure_out_scale_factor
       * function is normally executed before the image is scaled
       * in the y dimension */
      _image_size_y /= 4;
   }
   else {    
      /* set up global variables */
      _image_size_x = (_records_length - _left_border_pixels
	- _suffix_pixels) / _byte_factor ;
/***
	- _right_border_pixels - _suffix_pixels) / _byte_factor ;
***/ 

      if (_image_size_x != _pixels_per_line) {
         printf("image size x = %d, pixels per line = %d.\n",
 		_image_size_x, _pixels_per_line);
 
 	 make_announcement("CEOS file error: inconsistancy in image width",LOG);
 	 exit(QC_LINE_SIZE_MISMATCH);
      }
 	  
      _image_size_y = _records_count - _top_border_lines - _bottom_border_lines;
 	  
      if (_image_size_y != _lines_per_channel) {
 	 make_announcement("CEOS file error: inconsistancy in image height", LOG);
 	 exit(QC_CHANNEL_SIZE_MISMATCH);
      }
 	  
      _image_scale_factor = figure_out_scale_factor();
 
   }
}
