#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qavg_out_c[] = "@(#)qavg_out.c	1.14 97/02/12 09:06:48";

void
put_together_average_file()
{
   int   ceos_file_descriptor;
   byte  *first_record_bytes ;

   ceos_file_descriptor = open(_filename, O_RDONLY);
   read_first_record_byte_data(ceos_file_descriptor,
 			       &first_record_bytes);
   close(ceos_file_descriptor);
   alter_first_record(first_record_bytes);
   write_new_file(first_record_bytes);
   free(first_record_bytes);
   make_announcement("Averaged image file written\n", LOG);
}


void
alter_first_record(first_record_bytes) 
   byte *first_record_bytes;
{
   int first_record_index, new_string_index;

   int new_recs_count = (_image_size_y / _image_scale_factor)
                         + _top_border_lines + _bottom_border_lines;

   int new_recs_length = (_image_size_x / _image_scale_factor) 
     + _left_border_pixels;
/***
     + _right_border_pixels
     + _suffix_pixels ;
***/

   int new_lines_per_channel = (_image_size_y / _image_scale_factor);

   int new_pixels_per_line = (_image_size_x / _image_scale_factor);

   int new_data_byte_count;

   char new_recs_count_string[SAR_DATA_RECS_COUNT_LENGTH+1],
        new_recs_length_string[SAR_DATA_RECS_LENGTH_LENGTH+1];

   char new_lines_per_channel_string[LINES_PER_CHANNEL_LENGTH+1],
        new_pixels_per_line_string[PIXELS_PER_LINE_LENGTH+1];

   char new_data_byte_count_string[SAR_DATA_BYTE_COUNT_LENGTH+1];

   /* Set up strings holding the proper values... */
   sprintf(new_recs_count_string, "%6d", new_recs_count);

   sprintf(new_recs_length_string, "%6d", new_recs_length);

   sprintf(new_lines_per_channel_string, "%8d", new_lines_per_channel);

   sprintf(new_pixels_per_line_string, "%8d", new_pixels_per_line);

   if (_image_type == COMPLEX)
      new_data_byte_count = (_data_byte_count / pow(_image_scale_factor, 2));
   else
      new_data_byte_count = (_data_byte_count / _image_scale_factor);
   sprintf(new_data_byte_count_string, "%8d", new_data_byte_count);

   /* Then, move the strings into the first record. */

   new_string_index = 0;

   for (first_record_index = SAR_DATA_RECS_COUNT_OFFSET;
        first_record_index < (SAR_DATA_RECS_COUNT_OFFSET 
                           + SAR_DATA_RECS_COUNT_LENGTH);
        first_record_index++) {
      first_record_bytes[first_record_index] =
         new_recs_count_string[new_string_index++];
   }

   new_string_index = 0;

   for (first_record_index = SAR_DATA_RECS_LENGTH_OFFSET;
        first_record_index < (SAR_DATA_RECS_LENGTH_OFFSET
                             + SAR_DATA_RECS_LENGTH_LENGTH);
        first_record_index++) {
      first_record_bytes[first_record_index] =
         new_recs_length_string[new_string_index++];
   }

   new_string_index = 0;

   for (first_record_index = PIXELS_PER_LINE_OFFSET;
        first_record_index < (PIXELS_PER_LINE_OFFSET
                             + PIXELS_PER_LINE_LENGTH);
        first_record_index++) {
      first_record_bytes[first_record_index] =
         new_pixels_per_line_string[new_string_index++];
   }

   new_string_index = 0;

   for (first_record_index = LINES_PER_CHANNEL_OFFSET;
        first_record_index < (LINES_PER_CHANNEL_OFFSET
                             + LINES_PER_CHANNEL_LENGTH);
        first_record_index++) {
      first_record_bytes[first_record_index] =
        new_lines_per_channel_string[new_string_index++];
   }

   new_string_index = 0;

   for (first_record_index = SAR_DATA_BYTE_COUNT_OFFSET;
        first_record_index < (SAR_DATA_BYTE_COUNT_OFFSET
                             + SAR_DATA_BYTE_COUNT_LENGTH);
        first_record_index++) {
      first_record_bytes[first_record_index] =
        new_data_byte_count_string[new_string_index++];
   }
}


void
write_new_file(first_record_bytes)
     byte *first_record_bytes;
{
   FILE  *writeable_file_pointer;
 
   int   image_index;
 
   int   file_size_x, file_size_y; 
   int   i, row_index, column_index;
   int    scaled_image_size_x,
          scaled_image_size_y; 
   char   new_file_name[300];
 
   printf("Write image to output file\n");
/* *
   writeable_file_pointer = fopen(_output_to_file, "w");
* */
 
   if (writeable_file_pointer == NULL) {
      printf("Cannot open the output file %s.\n", new_file_name);
      /*exit(QC_OPEN_AVG_OUT_FILE_ERROR); */
      exit(-1);
   }
 
   scaled_image_size_x = (_image_size_x / _image_scale_factor);
   scaled_image_size_y = (_image_size_y / _image_scale_factor);
 
   file_size_x = scaled_image_size_x + _left_border_pixels; 
/***
     + _right_border_pixels + _suffix_pixels;
***
   
   /* First, put in the file descriptor record */
   for (i = 0; i < FIRST_RECORD_LENGTH; i++) {
      putc(first_record_bytes[i], writeable_file_pointer);
   }
 
   /* Fill up the rest of that line, */
   for(; i < file_size_x; i++) {
      putc(NULL, writeable_file_pointer);
   }
 
   /* Then do the top border lines */
   for (i = 0; i < _top_border_lines; i++) {
      putc(NULL, writeable_file_pointer);
   }
 
   /* then do the image block */
   image_index = 0;
   
   for (row_index = 0; row_index < scaled_image_size_y; row_index++) {
      /* Left border, */
      for (i = 0; i < _left_border_pixels; i++) {
 	 putc(NULL, writeable_file_pointer);
      }
      /* Picture data, */
      for (column_index=0; column_index<scaled_image_size_x; column_index++) {
 	 putc(_x_image->data[image_index++], writeable_file_pointer);
      }
 
      /* And right border. */
/***
      for (i = 0; i < _right_border_pixels; i++) {
         putc(NULL, writeable_file_pointer);
      }
***/
      /* And suffix. */
      for (i = 0; i < _suffix_pixels; i++) {
         putc(NULL, writeable_file_pointer);
      }
   }
 
   /* Finally, do the bottom border lines */
   for (i = 0; i < _bottom_border_lines; i++) {
      putc(NULL, writeable_file_pointer);
   }
   (void) fclose(writeable_file_pointer);
}
