#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gunsigned_image_c[] = "@(#)gunsigned_image.c	1.4 97/06/12 17:22:02";

/* This function sets up a few global variables, and
 * begins interpretation of an image options file that
 * contains an unsigned integer image. 
 */

byte*
set_up_unsigned_image(ceos_file_descriptor)
   int   ceos_file_descriptor;
{

   byte  *scaled_picture_data;
   int   scaled_picture_element_index, row_index, column_index;
   int   scaled_image_size_x, scaled_image_size_y;
 
   /* If we've got a small picture, try to make a small picture, */
   if ((_image_scale_factor > 1) && (_small_picture)) {  
      scaled_picture_data = _small_picture;
   }
   /* If we're zooming in on a box of an image */
   else if (_image_scale_factor == 1) { 
      scaled_picture_data = read_subset_of_image(ceos_file_descriptor);
   }
   else {  
      /* If we're displaying a scaled down image... */
 
      /* set up global variables */
      _image_size_x = (_records_length - _left_pixel_pad) / _byte_factor ;
       
      if (_image_size_x != _pixels_per_line) {
	 /*
         printf("image size x is %d, pixels per line is %d.\n",
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
 
      /* If this is the very first time through... */
      if (_image_scale_factor == 0)
         _image_scale_factor = figure_out_scale_factor();
 
      scaled_image_size_x = (_image_size_x / _image_scale_factor);
      scaled_image_size_y = (_image_size_y / _image_scale_factor);
       
      scaled_picture_data = (byte *) malloc(scaled_image_size_x * 
 		            scaled_image_size_y * sizeof(byte) * _byte_factor);
 
      /* check to make sure the malloc worked */
      if (!scaled_picture_data) {
         char *malloc_announcement = (char *) malloc(100 * sizeof(char));
         sprintf(malloc_announcement,
            "Can't allocate %d bytes in memory for the scaled image portion", 
            (scaled_image_size_x * scaled_image_size_y) );
	 printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
         free(malloc_announcement);
         exit(QC_MALLOC_ERROR);
      }
       
      scaled_picture_element_index = 0;
       
      /* Make the scaled picture */
      for (row_index = 0; row_index < scaled_image_size_y; row_index++) {
         for (column_index = 0; column_index < scaled_image_size_x;
 	       column_index++ ) {
            scaled_picture_data [scaled_picture_element_index++] =
               average_around(row_index, column_index, ceos_file_descriptor);
         }
      }
   }
   return scaled_picture_data;
}
          
/* 
 * This function takes in a row and a column, and a file descriptor
 * location (if the image isn't scaled down) or the average of the 
 * enclosing ZOOMED_OUT_FACTOR^2 bytes (if the image is scaled 
 * by ZOOMED_OUT_FACTOR).
 */
byte
average_around(swath_number, location_in_swath, ceos_file_descriptor)
   int   swath_number, location_in_swath, ceos_file_descriptor;
{
 
   int   total_holder, average;
   int   row_index, column_index;
   int   start_row, end_row, start_column, end_column;
   byte  *current_picture_data_swath, *current_raw_data_swath;
   short *current_raw_data_short_swath;
 
   /* for the very first swath, allocate data & picture swaths */
   if ((swath_number == 0) && (location_in_swath == 0)) {
      current_picture_data_swath = (byte *) malloc((_image_size_x * 
	     _image_scale_factor) * sizeof(byte) * _byte_factor);
      if (!current_picture_data_swath) {
         char *malloc_announcement = (char *) malloc(200 * sizeof(char));
         sprintf(malloc_announcement, 
             "Cannot allocate %d bytes memory for current_picture_data_swath",
 	     _image_size_x * _image_scale_factor * sizeof(byte) * _byte_factor);
	 printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
         free(malloc_announcement);
 	 exit(QC_MALLOC_ERROR);
      }
      current_raw_data_swath = (byte *) malloc((_records_length * 
                 _image_scale_factor) * sizeof(byte));
       
      if (!current_raw_data_swath) {
         char *malloc_announcement = (char *) malloc(200 * sizeof(char));
         sprintf(malloc_announcement, 
 		  "Cannot allocate %d bytes memory for current_raw_data_swath",
 		  _records_length * _image_scale_factor);
	 printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
         free(current_picture_data_swath);
         free(malloc_announcement);
         exit(QC_MALLOC_ERROR);
      }
   }
 
   /* for starting a new swath, set up the current_picture_data_swath
      and current_raw_data_swath buffers */
   if (location_in_swath == 0) {       
      if (_byte_factor == 1)
         set_up_new_swath(ceos_file_descriptor, swath_number,
            current_picture_data_swath, current_raw_data_swath);
      else {
         current_raw_data_short_swath = (short *) current_raw_data_swath;
         set_up_new_16_swath(ceos_file_descriptor, swath_number,
            current_picture_data_swath, current_raw_data_short_swath);
      }
   }
 
   start_row = 0;
   end_row = (_image_scale_factor) * _image_size_x;
 
   start_column = (_image_scale_factor * (location_in_swath + 1)) 
                  - _image_scale_factor;
   end_column   = (_image_scale_factor * (location_in_swath + 1));
 
   total_holder = 0;
 
   for (row_index = start_row; row_index<end_row; row_index += _image_size_x)
     for (column_index=start_column; column_index<end_column; column_index++) {
 	total_holder += 
 	   (int) current_picture_data_swath [row_index + column_index];
   }
 
   /* if we're done with the last swath, */
   if ((swath_number == ((_image_size_y / _image_scale_factor) - 1)) &&
       (location_in_swath == (_image_size_x / _image_scale_factor) - 1)) {
      free(current_picture_data_swath);
      free(current_raw_data_swath);
   }
 
   average = total_holder / (_image_scale_factor * _image_scale_factor);
   return(byte) average;
}
 
/* This function is called to read 8 bits data in either a new row
   (if the image is unscaled), or a set of ZOOMED_OUT_FACTOR rows
   (if the image is scaled).   */
 
void
set_up_new_swath(ceos_file_descriptor, swath_number, 
 		  current_picture_data_swath, raw_byte_data_swath) 
   int   ceos_file_descriptor, swath_number;
   byte  *current_picture_data_swath, *raw_byte_data_swath;
{
   int  row_index, column_index, number_of_bytes_read;
   int  unscaled_picture_element_index = 0;
   int  raw_swath_size = (_records_length * _image_scale_factor);
 
   /* If data needs to be read from the file... */ 
   
   /* jump to the right place in the file */
   lseek(ceos_file_descriptor, 
 	 _records_length + (swath_number * raw_swath_size), 0);                                  
   /* read data from the file, into raw_byte_data_swath */
   number_of_bytes_read = read(ceos_file_descriptor,
 			       raw_byte_data_swath, raw_swath_size);
   
   /* check and see that you got enough from the file */
   if (number_of_bytes_read != raw_swath_size) {
       char *read_announcement = (char *) malloc(200 * sizeof(char));
       
       sprintf(read_announcement, "Can't read swath %d (%d bytes)", 
 	       swath_number, raw_swath_size);
       printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
       free(read_announcement);
       exit(QC_CEOS_FILE_READ_ERROR);
   }
   
   for (row_index = 0; row_index < _image_scale_factor; row_index++)
     for (column_index = _left_border_pixels; column_index < _records_length;
 	 column_index++) {
 	current_picture_data_swath [unscaled_picture_element_index++] = 
 	  raw_byte_data_swath [ (row_index * _records_length) + column_index ];
     }
   row_index = 0; /*** unscaled_picture_element_index = 2024 ???? ***/
}
 

/* This function is called to read 16 bits data in either a new row
   (if the image is unscaled), or a set of ZOOMED_OUT_FACTOR rows
   (if the image is scaled).   */

void
set_up_new_16_swath(ceos_file_descriptor, swath_number,
                  current_picture_data_swath, raw_byte_data_swath)
   int   ceos_file_descriptor, swath_number;
   unsigned char  *current_picture_data_swath;
   unsigned short *raw_byte_data_swath;
{
   int  row_index, column_index, number_of_bytes_read;
   int  unscaled_picture_element_index = 0;
   int  raw_swath_size = (_records_length * _image_scale_factor);

   unsigned short short_value;
   /* If data needs to be read from the file... */

   /* jump to the right place in the file */
   lseek(ceos_file_descriptor,
         _records_length + (swath_number * raw_swath_size), 0);

   /* read data from the file, into raw_byte_data_swath */
   number_of_bytes_read = read(ceos_file_descriptor,
                               raw_byte_data_swath, raw_swath_size);
  
   /* check and see that you got enough from the file */
   if (number_of_bytes_read != raw_swath_size) {

       char *read_announcement = (char *) malloc(200 * sizeof(char));

       sprintf(read_announcement, "Can't read swath %d (%d bytes)",
               swath_number, raw_swath_size);
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);

       free(read_announcement);
       exit(QC_CEOS_FILE_READ_ERROR);
   }

   for (row_index = 0; row_index < _image_scale_factor; row_index++) {
     for (column_index = (_left_border_pixels/_byte_factor);
             column_index < ((_records_length)/_byte_factor); column_index++) {
        short_value = raw_byte_data_swath [ (row_index *
                      (_records_length/_byte_factor)) + column_index ];
        short_value /= 256;
        current_picture_data_swath[unscaled_picture_element_index++] =
                short_value > 255 ? 255 : (unsigned char) short_value;
     }
   }

   row_index = 0; /*** unscaled_picture_element_index = 2024 ???? ***/
}

 
/* nobody call this routine */
byte*
read_averaged_file()
{
   int   average_file_descriptor, number_of_bytes_read, average_file_size;
   byte  *average_file_byte_data;
   struct   stat average_file_buffer;
 
   stat(_average_in_file, &average_file_buffer );   
 
   average_file_size = average_file_buffer.st_size;   
 
   average_file_descriptor = open(_average_in_file, O_RDONLY);
 
   if (average_file_descriptor < 0) {
      char  *open_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(open_announcement, 
              "Cannot open the input file %s", _average_in_file);
 
      printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
      free(open_announcement);
      exit(QC_OPEN_AVERAGE_FILE_ERROR);
   }
 
   average_file_byte_data = (byte *) malloc(average_file_size);
 
   if (!average_file_byte_data) {
      char *malloc_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(malloc_announcement, 
 	      "Cannot allocate %d bytes in memory for average file", 
 	      average_file_size);
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   /* 
    * read average_file_size from ceos_file_descriptor, and
    * place into average_file_byte_data.  Return the number of bytes
    * that were read.  
    */
   number_of_bytes_read = read(average_file_descriptor, 
 			       average_file_byte_data, average_file_size);
   
   if (number_of_bytes_read != average_file_size) {
      char *read_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(read_announcement, 
             "Can't read average file (%d bytes)", average_file_size);
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
      free(read_announcement);
      exit(QC_AVERAGE_FILE_READ_ERROR);
   }
   return average_file_byte_data;
}
