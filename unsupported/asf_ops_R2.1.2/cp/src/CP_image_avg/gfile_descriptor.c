#include "ginclude.h" 
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gfile_descriptor_c[] = "@(#)gfile_descriptor.c	1.3 96/12/30 14:14:53";


/*
 * This function interprets the data in the first record
 * of an image options file, and sets up a few global variables. 
 */

char*
get_information_from_file(byte_data)
   byte  *byte_data;
{
   char  *format_type = (char *) malloc(29 * sizeof(char));
 
   if (! format_type) {
      char *malloc_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(malloc_announcement, 
 	      "Cannot allocate 29 bytes in memory for a char *");
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
 
   /* bytes 180-185 hold the SAR data records count (file height) */
   _records_count = read_n_bytes(SAR_DATA_RECS_COUNT_LENGTH, 
 				 SAR_DATA_RECS_COUNT_OFFSET, 
 				 byte_data, "records count", CANNOT_BE_ZERO);
 
   /* bytes 186-191 hold the SAR data records length (file width) */
   _records_length = read_n_bytes(SAR_DATA_RECS_LENGTH_LENGTH,
 				  SAR_DATA_RECS_LENGTH_OFFSET,
 				  byte_data, "records length", CANNOT_BE_ZERO);
 
   /* check out number of bits per sample, bytes 216-219 */
   _bits_per_sample = read_n_bytes(BITS_PER_SAMPLE_LENGTH,
 				   BITS_PER_SAMPLE_OFFSET,
 				   byte_data, "bits per sample",
 				   CANNOT_BE_ZERO);
 
   if (_bits_per_sample != 8 && _bits_per_sample != 16) {
      char *bits_per_sample_announcement = (char *) malloc(100 * sizeof(char));
      sprintf(bits_per_sample_announcement, 
 	      "This isn't an 8-bit or 16-bit image, it's a %d\-bit image",
 	      _bits_per_sample);
      printfLLog(LOG_ERR, "ERROR:  %s", bits_per_sample_announcement);
      free(bits_per_sample_announcement);
      exit(QC_NON_8_BIT_IMAGE);
   }

   _byte_factor = _bits_per_sample / SAMPLE_IN_8_BITS;
   /* bytes 280-287 hold the bytes per pixel */
   _data_byte_count = read_n_bytes(SAR_DATA_BYTE_COUNT_LENGTH, 
 				   SAR_DATA_BYTE_COUNT_OFFSET, 
 				   byte_data, "SAR data byte count",
 				   CANNOT_BE_ZERO);
   if ((_records_length - _data_byte_count) == LEFT_PIXEL_PAD)
      _left_pixel_pad = LEFT_PIXEL_PAD;          /* 12 for R1A */
   else
      _left_pixel_pad = LEFT_PIXEL_PAD_NEW;      /* 192 for R1B after */
 
   /* bytes 236-243 hold the lines per channel (image height) */
   _lines_per_channel = read_n_bytes(LINES_PER_CHANNEL_LENGTH, 
 				     LINES_PER_CHANNEL_OFFSET, 
 				     byte_data, "lines per channel",
 				     CANNOT_BE_ZERO);
 
   /* bytes 244-247 hold the  number of left border pixels per line */
   _left_border_pixels = read_n_bytes(LEFT_BORDER_PIXELS_LENGTH,
 				      LEFT_BORDER_PIXELS_OFFSET, 
 				      byte_data, "left border pixels",
 				      CAN_BE_ZERO);
 
   /* for everything there's a 12 pixel "implied" pad */
   _left_border_pixels += _left_pixel_pad;  
 
   /* bytes 248-255 hold the number of pixels per line (image width) */
   _pixels_per_line = read_n_bytes(PIXELS_PER_LINE_LENGTH,
 				   PIXELS_PER_LINE_OFFSET, 
 				   byte_data, "pixels per line",
 				   CANNOT_BE_ZERO);
 
   /* bytes 256-259 hold the  number of right border pixels per line */
   _right_border_pixels = read_n_bytes(RIGHT_BORDER_PIXELS_LENGTH,
 				       RIGHT_BORDER_PIXELS_OFFSET, 
 				       byte_data, "right border pixels",
 				       CAN_BE_ZERO);
 
   /* bytes 260-263 hold the  number of top border lines */
   _top_border_lines = read_n_bytes(TOP_BORDER_LINES_LENGTH,
 				    TOP_BORDER_LINES_OFFSET, 
 				    byte_data, "top border lines",
 				    CAN_BE_ZERO);
 
   /* bytes 264-267 hold the  number of bottom border lines */
   _bottom_border_lines = read_n_bytes(BOTTOM_BORDER_LINES_LENGTH,
 				       BOTTOM_BORDER_LINES_OFFSET, 
 				       byte_data, 
 				       "bottom border lines",
 				       CAN_BE_ZERO);
 
   /* bytes 400-427 hold the format type indicator */
   memmove(format_type, &byte_data[FORMAT_TYPE_INDICATOR_OFFSET], 
 	   FORMAT_TYPE_INDICATOR_LENGTH);
 
   return format_type;
}
 
 
void
read_first_record_byte_data(ceos_file_descriptor, first_record_byte_data)
   int   ceos_file_descriptor;
   byte  **first_record_byte_data;
{
   int      ceos_file_size, number_of_bytes_read,
            first_record_length = FIRST_RECORD_LENGTH;
   struct   stat ceos_file_buffer;
 
   if (ceos_file_descriptor < 0) {
      char *open_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(open_announcement, "Cannot open the input file %s", _filename);
      printfLLog(LOG_ERR, "ERROR:  %s", open_announcement);
      free(open_announcement);
      exit(QC_OPEN_CEOS_FILE_ERROR);
   }
 
   /* to get the size of the file */
   /* I don't think this data is needed anymore... */
   stat(_filename, &ceos_file_buffer);   
   ceos_file_size = ceos_file_buffer.st_size;   
 
   *first_record_byte_data = 
      (byte *) malloc(first_record_length * sizeof(byte));
 
   if (!*first_record_byte_data) {
      char *malloc_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(malloc_announcement, 
 	      "Cannot allocate %d bytes in memory for first record", 
 	      first_record_length);
      printfLLog(LOG_ERR, "ERROR:  %s", malloc_announcement);
      free(malloc_announcement);
      exit(QC_MALLOC_ERROR);
   }
   
   /* 
    * read first_record_length bytes from ceos_file_descriptor, 
    * and place into first_record_byte_data.  Return the number of 
    * bytes that were read.  
    */
   number_of_bytes_read = read(ceos_file_descriptor, 
 	       *first_record_byte_data, first_record_length);
   
   if (number_of_bytes_read != first_record_length) {
      char *read_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(read_announcement, "Can't read first record (%d bytes)", 
              first_record_length);
      printfLLog(LOG_ERR, "ERROR:  %s", read_announcement);
      free(read_announcement);
      exit(QC_CEOS_FILE_READ_ERROR);
   }
}
