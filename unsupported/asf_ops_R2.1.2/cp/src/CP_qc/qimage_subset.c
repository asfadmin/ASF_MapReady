#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qimage_subset_c[] = "@(#)qimage_subset.c	1.17 97/06/12 16:52:59";

byte *
read_subset_of_image(ceos_file_descriptor)
   int ceos_file_descriptor;
{

   int   zoomed_out_factor = figure_out_scale_factor();

   unsigned short short_value; 
   unsigned short *one_line_16_bit_buffer; 
   unsigned char  *zoom_box_picture_data, *one_line_buffer;
 
   int   boxed_image_width  = _zoom_box_width  * zoomed_out_factor;
   int   boxed_image_height = _zoom_box_height * zoomed_out_factor;

   int   picture_element_index, i, j, line_increment;

   int   number_of_bytes_read;

   _boxed_image_width = boxed_image_width;
   _boxed_image_height = boxed_image_height;
   zoom_box_picture_data = (unsigned char *) malloc(boxed_image_width *
					   boxed_image_height *
					   sizeof(byte) * _byte_factor);
   /* check to make sure the malloc worked */
   if (!zoom_box_picture_data) {
      char malloc_announcement[100]; 
      sprintf(malloc_announcement, "Can't allocate %d bytes in memory for the zoom box image portion", (boxed_image_width * boxed_image_height) );
      make_announcement(malloc_announcement, LOG);
      exit(QC_MALLOC_ERROR);
   }

   one_line_buffer = (unsigned char *) malloc(boxed_image_width * sizeof(byte) * _byte_factor);

   /* check to make sure the malloc worked */
   if (!one_line_buffer) {
      char malloc_announcement[100];
      
      sprintf(malloc_announcement,
	      "Can't allocate %d bytes in memory for one line of the box",
	      boxed_image_width * _byte_factor);
      make_announcement(malloc_announcement, LOG);
      exit(QC_MALLOC_ERROR);
   }
  
   /* If the image isn't being zoomed in on, 
    * the _y_zoom_offset and _x_zoom_offset have been
    * initialized to negative numbers 
    */
   if ((_y_zoom_offset < 0) && (_x_zoom_offset < 0)) {
      _y_zoom_offset = 0;
      _x_zoom_offset = _left_border_pixels/_byte_factor;
   }

   line_increment = 0;
   picture_element_index = 0;
   for (i = 0; i < boxed_image_height; i++) {
      lseek(ceos_file_descriptor, ((_y_zoom_offset + i + _top_border_lines
	    + 1) /* Add 1 for the first record */
	    * _records_length) + _x_zoom_offset * _byte_factor, 0);

      if (_byte_factor == 1) {
         number_of_bytes_read = read(ceos_file_descriptor, 
				one_line_buffer, boxed_image_width);

         if (number_of_bytes_read != boxed_image_width) {
	     make_announcement("Couldn't read one line of the boxed image",LOG);
	     exit(QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR);	  
         }

         for (j = 0; j < boxed_image_width; j++) {
   	    zoom_box_picture_data[picture_element_index++] = one_line_buffer[j];
         }
      }
      else {
         one_line_16_bit_buffer = (unsigned short *) one_line_buffer;
         number_of_bytes_read = read(ceos_file_descriptor, 
	      			  one_line_16_bit_buffer,
				  boxed_image_width * _byte_factor);

         if (number_of_bytes_read != boxed_image_width * _byte_factor) {
	     make_announcement("Couldn't read one line of the boxed image",LOG);
	     exit(QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR);	  
         }

         for (j = 0; j < boxed_image_width; j++) {
            short_value = one_line_16_bit_buffer[j];
            short_value /= 256;
	    zoom_box_picture_data[picture_element_index++] = 
                short_value > 255 ? 255 : (unsigned char) short_value;;
         }
      }
   }
   free(one_line_buffer);
   return zoom_box_picture_data;
}


byte*
read_subset_of_complex_image(ceos_file_descriptor)
   int ceos_file_descriptor;
{

   byte  *zoom_box_picture_data, *cut_picture,
         *one_line_picture_buffer, *temp_pointer;

   short *one_line_data_buffer;
   int   zoomed_out_factor;
   int   boxed_image_width ;
   int   boxed_image_height;
   int   memory_buffer_width ;
   int   memory_buffer_height;
   int   picture_element_index, i, j, line_increment;
   int   number_of_bytes_read;

   /* The image height needs to be set back to 4x what it's at,
    * since the figure_out_scale_factor assumes that it hasn't
    * been scaled down in the y dimension... 
    */
 
   _image_size_y *= 4;
   zoomed_out_factor = figure_out_scale_factor();
   _image_size_y /= 4;
   
   boxed_image_width  = _zoom_box_width  * zoomed_out_factor;

   boxed_image_height = _zoom_box_height * zoomed_out_factor;
   _boxed_image_width = boxed_image_width;
   _boxed_image_height = boxed_image_height;
 
   /* This will hold the picture... It is 4x larger than
      it needs to be, since the image will be 4x too large
      in the y dimension before it's scaled down */
   zoom_box_picture_data = (byte *) malloc(boxed_image_width *
 					   boxed_image_height *
					   sizeof(byte) *
					   4);

   /* check to make sure the malloc worked */
   if (!zoom_box_picture_data) {
      make_announcement("Memory allocation problem", LOG);
      exit(QC_MALLOC_ERROR);
   }


   /* The width is 2x too large, 'cause there
    * are 2 elements for every value. */
   memory_buffer_width  = boxed_image_width * 2 * sizeof(short);

   /* The height is 4x too large, 'cause the image needs
    * to be scaled down by 4 in the y dimension later. */
   memory_buffer_height = boxed_image_height * 4;
  
   one_line_data_buffer = (short *) malloc(memory_buffer_width);

   /* check to make sure the malloc worked */
   if (!one_line_data_buffer) {
      make_announcement("Memory alloc failure", LOG);
      exit(QC_MALLOC_ERROR);
   }

   one_line_picture_buffer = (byte *) malloc(boxed_image_width * sizeof(byte));

   /* check to make sure the malloc worked */
   if (!one_line_picture_buffer) {
      make_announcement("Memory alloc failure", LOG);
      exit(QC_MALLOC_ERROR);
   }
  
   /* If the image isn't being zoomed in on, 
    * the _y_zoom_offset and _x_zoom_offset have been
    * initialized to negative numbers */
   if ((_y_zoom_offset < 0) && (_x_zoom_offset < 0)) {
      _y_zoom_offset = _x_zoom_offset = 0;
   }

   line_increment = 0;
   picture_element_index = 0;

   /* Set up complex_multiply_factor if need be */
   if ((_complex_multiply_factor == 0.0) &&
       (_complex_scale_parameter != 0.0) && 
       (_is_sys_pp != 1))
     _complex_multiply_factor = _complex_scale_parameter ;
   else if (_is_sys_pp == 1) { /* PP */
     _complex_multiply_factor = (100.0 / (double) _data_mean_pp) / 1.4;
   }
   else
     _complex_multiply_factor = QC_DEFAULT_SCALE ;
     

   for (i = 0; i < memory_buffer_height; i++) {
      lseek(ceos_file_descriptor, ((_y_zoom_offset + i
	       + _top_border_lines + 1)   /* Add 1 for the first record */
	       * _records_length) + _x_zoom_offset, 0);

      /* Read in one line */
      number_of_bytes_read = read(ceos_file_descriptor, 
				   one_line_data_buffer,
				   memory_buffer_width);

      if (number_of_bytes_read != memory_buffer_width) {
	 make_announcement("Couldn't read one line of the boxed image", LOG);
	 exit(QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR);
      }

      /* Detect the data in the line that's just been
       * read in, and leave the picture data in 
       * one_line_picture_buffer */
      detect_data_in_line(one_line_data_buffer, 
			  one_line_picture_buffer, boxed_image_width * 2);

      for (j = 0; j < boxed_image_width; j++) {
	 zoom_box_picture_data[picture_element_index++] =
	    one_line_picture_buffer[j];

      }

   }

   cut_picture = (byte *) malloc(boxed_image_width *
				 boxed_image_height *
				 sizeof(byte));

   if (! cut_picture) {
      char malloc_announcement[100];
      sprintf(malloc_announcement,
	      "Can't allocate %d bytes in memory for a picture",
	      boxed_image_width * boxed_image_height * 4);
      make_announcement(malloc_announcement, LOG);
      exit(QC_MALLOC_ERROR);
   }

   /* cut_y_by_4 expects that you haven't altered the global variable yet */
   _image_size_y *= 4;

   /* Now scale the image down by 4 in the y dimension only */
   cut_y_by_4(zoom_box_picture_data, cut_picture); 
  
   /* Keep a pointer to the unscaled data... */
   temp_pointer = zoom_box_picture_data;

   /* assign picture only byte data to what the image is supposed to be... */
   zoom_box_picture_data = cut_picture;
  
   /* Free up the block used by the old unscaled image */
   free(temp_pointer);

   /* Free the buffers used to read in complex data */
   free(one_line_data_buffer);
   free(one_line_picture_buffer);

   return zoom_box_picture_data;
}


void
detect_data_in_line(one_line_data_buffer, 
		    one_line_picture_buffer, line_data_width)
   short *one_line_data_buffer;
   byte  *one_line_picture_buffer;
   int   line_data_width;
{
   int i;
   float float_value;
   int picture_index = 0;

   for (i = 0; i < line_data_width; i += 2 ) {
      float_value = 
      	   (sqrt ( ((double)  one_line_data_buffer[i]    *
		   (double)  one_line_data_buffer[i])    +
		   ((double) one_line_data_buffer[i + 1] *
		   (double) one_line_data_buffer[i + 1])  )
	    * _complex_multiply_factor);
	
      /* assign that value (of 255 or lower) to the picture array */
      one_line_picture_buffer [picture_index++] = 
	 float_value > 255.0 ? (byte) 255 : (byte) float_value;
   }
}

