#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gimage_subset_c[] = "@(#)gimage_subset.c	1.5 97/06/12 17:22:13";

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

   zoom_box_picture_data = (unsigned char *) malloc(boxed_image_width *
					   boxed_image_height *
					   sizeof(byte) * _byte_factor);

   /* check to make sure the malloc worked */
   if (!zoom_box_picture_data) {
      char malloc_announcement[100]; 
      sprintf(malloc_announcement, 
 "Can't allocate %d bytes in memory for the zoom box image portion of the file",         (boxed_image_width * boxed_image_height));
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }

   one_line_buffer = (unsigned char *) malloc(boxed_image_width * sizeof(byte) * _byte_factor);

   /* check to make sure the malloc worked */
   if (!one_line_buffer) {
      char malloc_announcement[100];
      
      sprintf(malloc_announcement,
	      "Can't allocate %d bytes in memory for one line of the box",
	      boxed_image_width * _byte_factor);
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
  
   /* If the image isn't being zoomed in on, 
    * the _y_zoom_offset and _x_zoom_offset have been
    * initialized to negative numbers 
    */
   if ((_y_zoom_offset < 0) && (_x_zoom_offset < 0)) {
      _y_zoom_offset = 0; 
      _x_zoom_offset = _left_pixel_pad/_byte_factor;
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
            char malloc_announcement[100];
      
            sprintf(malloc_announcement,
               "Couldn't read one line of the boxed image");
            printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
            exit(QC_CEOS_FILE_READ_ERROR);
         }

         for (j = 0; j < boxed_image_width; j++) {
            zoom_box_picture_data[picture_element_index++] = one_line_buffer[j];         }
      }
      else {
         one_line_16_bit_buffer = (unsigned short *) one_line_buffer;
         number_of_bytes_read = read(ceos_file_descriptor,
                                  one_line_16_bit_buffer,
                                  boxed_image_width * _byte_factor);

         if (number_of_bytes_read != boxed_image_width * _byte_factor) {
            char malloc_announcement[100];
      
            sprintf(malloc_announcement,
               "Couldn't read one line of the boxed image");
            printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
            exit(QC_CEOS_FILE_READ_ERROR);
         }

         for (j = 0; j < boxed_image_width; j++) {
            short_value = one_line_16_bit_buffer[j];
            short_value /= 256;
            zoom_box_picture_data[picture_element_index++] =
                short_value > 255 ? 255 : (unsigned char) short_value;
         }
      }
   }
   free(one_line_buffer);
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
		   (double) one_line_data_buffer[i + 1]) )
	    * _complex_multiply_factor);
	
      /* assign that value (of 255 or lower) to the picture array */
      one_line_picture_buffer [picture_index++] = 
	 float_value > 255.0 ? (byte) 255 : (byte) float_value;
   }
}

