#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gcomplex_image_c[] = "@(#)gcomplex_image.c	1.7 97/05/19 18:16:49";
         
/* 
 * This function basically is the starting point for dealing
 * with a complex data file. 
 */

byte*
set_up_complex_image(ceos_file_descriptor)
   int ceos_file_descriptor;
{
   byte  *scaled_complex_image_byte_data;
 
   /* If we've got a small picture, and we're trying to make a small picture */
   if ((_image_scale_factor > 1) && (_small_picture)) {  
      scaled_complex_image_byte_data = _small_picture;
   }
   else if ((_x_zoom_offset >= 0) && (_y_zoom_offset >= 0)) {  
      /* If we're zooming in on a box of an image */
      /*
       * not needed for CP_image_avg
      scaled_complex_image_byte_data = 
         read_subset_of_complex_image(ceos_file_descriptor);      
      */
      ;
 
   }
   else {
      /* If we're creating an image from scratch, */
      _image_type = COMPLEX;
 
      if (! _average_in_file) {
 	 /* set up global variables */
 	 set_up_unaveraged_complex_global_variables();
 	 scaled_complex_image_byte_data = 
 	    read_unaveraged_complex_file(ceos_file_descriptor);
      }
      else {
 	 /* If we've got an average file to deal with */
 	 /* set up global variables */
 	 set_up_averaged_complex_global_variables();
 	  
 	 scaled_complex_image_byte_data = 
 	    read_averaged_complex_file(ceos_file_descriptor);
      }
   }
   return scaled_complex_image_byte_data;
}


/* 
 * This function takes in a row and a column, and a file descriptor
 * for an image options file, and returns either the byte for that
 * location(if the image isn't scaled down) or the average of the 
 * enclosing 64 bytes (if the image is scaled by 8).  This 
 * also does some of the transformation from complex to integer.
 */

byte
complex_average_around(swath_number, location_in_swath, ceos_file_descriptor)
   int   swath_number, location_in_swath, ceos_file_descriptor;
{
 
   int   row_index, column_index, number_added = 0,
         start_row, end_row, start_column, end_column;
   byte  *current_picture_data_swath;
   short *current_raw_data_swath;
   float total_holder, average;

   /* for the very first swath, allocate data & picture swaths */
   if ((swath_number == 0) && (location_in_swath == 0)) {
      current_picture_data_swath = (byte *) malloc((_image_size_x * 
 			            _image_scale_factor) * sizeof(byte));
      if (!current_picture_data_swath) {
 	 char malloc_announcement[200];
 	 sprintf(malloc_announcement, 
            "Cannot allocate %d bytes in memory for current_picture_data_swath",
            _image_size_x * _image_scale_factor);
	 printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
 	 exit(QC_MALLOC_ERROR);
      }
 
      current_raw_data_swath = (short *) malloc((_records_length * 
 			            _image_scale_factor) * sizeof(short));
      if (!current_raw_data_swath) {
 	 char malloc_announcement[200];
 	 sprintf(malloc_announcement, 
            "Cannot allocate %d bytes in memory for current_raw_data_swath", 
            _records_length * _image_scale_factor);
	 printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
 	 exit(QC_MALLOC_ERROR);
      }
   }
 
   /* 
    * for starting a new swath.  
    * This is done(_image_size_x / _image_scale_factor) times 
    */
   if (location_in_swath == 0) {       
      set_up_new_complex_swath(ceos_file_descriptor, swath_number,
 			current_picture_data_swath, current_raw_data_swath);
   }
 
   /* 
    * loop through a square of side length _image_scale_factor * of 
    * the swath picture array, and total up all the values in that square 
    */
 
   start_row    = 0;
   end_row      = (_image_scale_factor - 1) * _image_size_x;
   start_column = (_image_scale_factor * (location_in_swath + 1)) 
                  - _image_scale_factor;
   end_column   = (_image_scale_factor * (location_in_swath + 1)) - 1;
 
   total_holder = 0.0;
 
   for (row_index = start_row; row_index <= end_row; row_index += _image_size_x)
     for (column_index = start_column; column_index <= end_column;
 	  column_index++ ) {
 	total_holder += 
 	  ((float) current_picture_data_swath [row_index + column_index] *
 	   (float) current_picture_data_swath [row_index + column_index]);
     }
 
   /* if we're done with the last swath, */
   if ((swath_number ==  ((_image_size_y / _image_scale_factor) - 1)) &&
       (location_in_swath == (_image_size_x / _image_scale_factor) - 1)) {
      free(current_picture_data_swath);
      free(current_raw_data_swath);
   }
 
   average = 0.0;
 
   /* find the average of all values in the square */
   average = sqrt(total_holder / (_image_scale_factor * _image_scale_factor ));
 
   return (byte) average;
}

/* 
 * This function takes in picture data (which has been converted from
 * complex to integer), and a pointer to an empty buffer, and
 * scales down the picture data by 4 in the y dimension only,
 * leaving the scaled data in the (what was once) empty buffer 
 */

void
cut_y_by_4(picture_data, small_pic) 
   byte *picture_data, *small_pic;
{
 
   int   writeable_file_descriptor, number_of_bytes_written;
   int   zoomed_out_factor;
 
   int  new_image_size_y = (_image_size_y / _image_scale_factor) / 4,
        new_image_size_x = _image_size_x / _image_scale_factor,
        total_holder, row_index, column_index, element_index = 0;
 
   if ((_x_zoom_offset >= 0) || (_y_zoom_offset >= 0)) {
      /* If we're only zooming in on a portion of the image, */
      if (_is_sys_pp == 1) {
         zoomed_out_factor = figure_out_scale_factor();
         new_image_size_y = _zoom_box_width  * zoomed_out_factor;
         new_image_size_x = _zoom_box_height * zoomed_out_factor;
      }
      else {
         new_image_size_y = ZOOM_IN_WIDTH;
         new_image_size_x = ZOOM_IN_WIDTH;
      }
   }
 
   for (row_index = 0; row_index < new_image_size_y; row_index++) {
      for (column_index = 0; column_index < new_image_size_x; column_index++) {
 	 small_pic [element_index++] = (byte) 
 	(((int) picture_data[(new_image_size_x*(row_index*4))+column_index] 
 	+ (int) picture_data[(new_image_size_x*((row_index*4)+1))+column_index] 
 	+ (int) picture_data[(new_image_size_x*((row_index*4)+2))+column_index] 
 	+ (int) picture_data[(new_image_size_x*((row_index*4)+3))+column_index])
 	/ 4 );
      }
   }
   _image_size_y = _image_size_y / 4;
}

/* 
 * This function is called to read in either a new row (if the
 * image is unscaled), or a set of n rows (if the image is scaled
 * down by a factor of n). The function also does some of the
 * complex->integer conversion. 
 */

void
set_up_new_complex_swath(ceos_file_descriptor, swath_number, 
			 current_picture_data_swath, raw_complex_data_swath) 
   int ceos_file_descriptor, swath_number;
   byte  *current_picture_data_swath;
   short *raw_complex_data_swath;
{
   int   row_index, column_index, number_of_bytes_read,
         unscaled_picture_element_index = 0,
         raw_swath_size = (_records_length * _image_scale_factor),
         location;

   float float_value;

   /* jump to the right place in the file */
   lseek(ceos_file_descriptor, 
         _records_length + (swath_number * raw_swath_size), 0);                                  
   /* read data from the file, into raw_complex_data_swath */
   number_of_bytes_read = read(ceos_file_descriptor,
			       raw_complex_data_swath, raw_swath_size);

   /* check and see that you got enough from the file */
   if (number_of_bytes_read != raw_swath_size) {
      char read_announcement[200];

      sprintf(read_announcement, "Can't read swath %d (%d bytes)", 
	      swath_number, raw_swath_size);
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
      exit(QC_CEOS_FILE_READ_ERROR);
   }
  
   /* Loop through the raw data, assign the proper values to the picture data */

   /* Figure out what we're going to scale with, */
   if (_complex_scale_parameter != 0.0 && _is_sys_pp != 1)
     _complex_multiply_factor = _complex_scale_parameter;
   else if (_is_sys_pp == 1) { /* PP */
     _complex_multiply_factor = (100.0 / (double) _data_mean) / 1.4;
   }
   else 
     _complex_multiply_factor = QC_DEFAULT_SCALE ;

   for (row_index = 0; row_index < _image_scale_factor; row_index++)
      for (column_index = 0; column_index < _image_size_x; column_index++) {
	 /* find the proper location in the raw data array */
	 location = 2 * ((row_index * (_image_size_x + _left_border_pixels/4)) 
                    + column_index + _left_border_pixels/4);

	 /* 
          * find the square root of the sum of the real value squared
	  * and the complex value squared. 
          * Note : fetch a short value is a 16 bits data 
          */
	 float_value = 
            (sqrt( ((double)(raw_complex_data_swath[location])  *
                    (double)(raw_complex_data_swath[location])  ) +
                   ((double)(raw_complex_data_swath[location+1])*
                    (double)(raw_complex_data_swath[location+1]))
                 ) * _complex_multiply_factor);

	 /* assign that value (of 255 or lower) to the picture array */
	 current_picture_data_swath[unscaled_picture_element_index++] = 
	        float_value > 255.0 ? (byte) 255 : (byte) float_value;
      }
      unscaled_picture_element_index = 0;
} /* end of set_up_new_complex_swath */


byte*
read_unaveraged_complex_file(ceos_file_descriptor)
   int   ceos_file_descriptor;
{
   int   scaled_picture_element_index,
         row_index, column_index,
         scaled_image_size_x, scaled_image_size_y;

   byte  *scaled_complex_image_byte_data;

   byte  *small_pic, *tmp_block;
  

   scaled_image_size_x = (_image_size_x / _image_scale_factor);
   scaled_image_size_y = (_image_size_y / _image_scale_factor);
      

   scaled_complex_image_byte_data = (byte *) malloc((scaled_image_size_x *
			            scaled_image_size_y) * sizeof(byte));
  
   if (!scaled_complex_image_byte_data) {
      char *malloc_announcement = (char *) malloc(100 * sizeof(char));
      sprintf(malloc_announcement, 
         "Can't allocate %d bytes in memory for the image portion of the file",
         (scaled_image_size_x * scaled_image_size_y) );
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
  
   scaled_picture_element_index = 0;
  
   /* assign the image data to the newly malloc'd array */
   for (row_index = 0; row_index < scaled_image_size_y; row_index++) {
      for (column_index = 0; column_index < scaled_image_size_x;
	   column_index++ ) {
	 scaled_complex_image_byte_data[scaled_picture_element_index++] =
	    complex_average_around(row_index, column_index,
				   ceos_file_descriptor);
      }
   }
  
   small_pic = (byte *) malloc((((_image_size_y / _image_scale_factor) / 4) *
	       (_image_size_x / _image_scale_factor)) * sizeof(byte));
/*
  
   small_pic = (byte *) malloc(((_image_size_y / _image_scale_factor) *
	       (_image_size_x / _image_scale_factor)) * sizeof(byte));
*/

   if (!small_pic) {
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Cannot allocate memory for small picture");
      exit(QC_MALLOC_ERROR);
   }
  
   /* Now scale the image down by 4 in the y dimension only */
   cut_y_by_4(scaled_complex_image_byte_data, small_pic); 
  
   /* Free up the block used by the old unscaled image */
   tmp_block = scaled_complex_image_byte_data;
   
   /* assign picture only byte data to what the image is supposed to be... */
   scaled_complex_image_byte_data = small_pic;
   free(tmp_block);
   return scaled_complex_image_byte_data;
}

byte*
read_averaged_complex_file(ceos_file_descriptor)
   int ceos_file_descriptor;
{

   int   scaled_picture_element_index,
         row_index, column_index;

   byte  *scaled_complex_image_byte_data;

   scaled_complex_image_byte_data  = (byte *) malloc((_image_size_x *
			             _image_size_y ) * sizeof(byte));
  
   if (!scaled_complex_image_byte_data) {
      char *malloc_announcement = (char *) malloc(100 * sizeof(char));
      sprintf(malloc_announcement,
         "Can't allocate %d bytes in memory for the image portion of the file", 
         (_image_size_x * _image_size_y) );
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
  
   scaled_picture_element_index = 0;
  
   /* assign the image data to the newly malloc'd array */
   for (row_index = 0; row_index < _image_size_y; row_index++) {
      for (column_index = 0; column_index < _image_size_x; column_index++ ) {
	 scaled_complex_image_byte_data[scaled_picture_element_index++] =
	    average_around(row_index, column_index, ceos_file_descriptor);
      }
   }
   return scaled_complex_image_byte_data;
}

