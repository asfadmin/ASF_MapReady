#include "qinclude.h" 
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qfile_descriptor_c[] = "@(#)qfile_descriptor.c	1.14 96/12/30 15:25:39";


/*
 * This function interprets the data in the first record
 * of an image options file, and sets up a few global variables. 
 */

char*
get_information_from_file(byte_data)
   byte  *byte_data;
{
   char  *format_type = (char *) malloc(29 * sizeof(char));
 
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
      make_announcement(bits_per_sample_announcement, LOG);
      free(bits_per_sample_announcement);
      exit(QC_NON_8_BIT_IMAGE);
   }
 
   if (! _avg_file_flag)
      _byte_factor = _bits_per_sample / SAMPLE_IN_8_BITS;
   else 
      _byte_factor = 1;

   /* Bytes 276-279 hold the prefix data per record */
   _left_pixel_pad = read_n_bytes(N_PREFIX_LENGTH,
				  N_PREFIX_OFFSET,
				  byte_data, "Prefix data per record",
				  CANNOT_BE_ZERO);

   /* bytes 280-287 hold the bytes per pixel */
   _data_byte_count = read_n_bytes(SAR_DATA_BYTE_COUNT_LENGTH, 
 				   SAR_DATA_BYTE_COUNT_OFFSET, 
 				   byte_data, "SAR data byte count",
 				   CANNOT_BE_ZERO);

 
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
 
   /* Include the left pixel pad in the border pixels */
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
 
   /* bytes 288-291 hold the number of suffix pixels per line */
   _suffix_pixels = read_n_bytes(N_SUFFIX_LENGTH,
				 N_SUFFIX_OFFSET, 
				 byte_data, "suffix data per record",
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
      make_announcement(open_announcement, LOG);
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
      make_announcement(malloc_announcement, LOG);
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
      make_announcement(read_announcement, LOG);
      free(read_announcement);
      exit(QC_CEOS_FILE_UNEXPECTED_READ_RETURN_ERROR);
   }
}
