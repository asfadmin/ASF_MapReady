#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gimagefile_c[] = "@(#)gimagefile.c	1.2 96/04/10 19:46:22";

/*
 *  This function is the starting point for interpreting
 *  the ceos file.  All action on the image options file
 *  takes place either here or in a child of this function. 
 */

void
read_ceos_file()
{
   static int   ceos_file_descriptor;
  
   static byte  *picture_only_byte_data,
                *first_record_byte_data;    /* freed after used */

   /* char *_filename is set up previously, and is a global variable */
 
   /* open the file argument, and check it out */
   ceos_file_descriptor = open(_filename, O_RDONLY);
       
   /* read first_record_length bytes from ceos_file_descriptor */
   read_first_record_byte_data(ceos_file_descriptor, &first_record_byte_data);
       
   /*
    * set up global variables (_records_count, _records_length, 
    * _bits_per_sample, _lines_per_channel, _left_border_pixels, 
    * _pixels_per_line, _right_border_pixels, _top_border_lines, 
    * _bottom_border_lines), whose values are found in the first 
    * record of the file.  Return the _format_type with (char *).  
    */
   _format_type = get_information_from_file(first_record_byte_data);
  
   free(first_record_byte_data);
 
   /* get back to the start of the file */
   lseek(ceos_file_descriptor, 0, 0);
 
   /* Set up the unsigned/complex image according to the format of the file */
   handle_typed_file(ceos_file_descriptor, &picture_only_byte_data);
 
   /* Set up the XImage* and the pixmap.. */
   /* set_up_picture(picture_only_byte_data); */
 
   /* See if we're going to keep a copy of a scaled-down picture around */
   if ((_image_scale_factor > 1) && (! _small_picture)) {
      /*
       * But: check to see if the picture only byte data
       * was altered to get rid of the background 
       */
      if (_unaltered_bytes) {
         int scaled_x = _image_size_x / _image_scale_factor;
         int scaled_y = _image_size_y / _image_scale_factor;
 
         /* Small picture has to hold a copy of the unaltered
            bytes, since we free the unaltered bytes later */
      	 _small_picture = (byte *) malloc(scaled_x * scaled_y * sizeof(byte));
 	   
 	 if (! _small_picture) { 
	    printfLLog(LOG_ERR, "ERROR:  %s",
		"Cannot allocate space for small picture");
 	    exit(QC_MALLOC_ERROR);
 	 }
 	 memmove(_small_picture, _unaltered_bytes, (scaled_x * scaled_y));
      }
      else {
         _small_picture = picture_only_byte_data;
      }
   }
   else if ((_image_scale_factor > 1) && (_small_picture)       &&
 	    (_unaltered_bytes)        && (_background_colorcell != 0)) {
      /* In this case, small picture got screwed up by the altering process */
      int scaled_x = _image_size_x / _image_scale_factor;
      int scaled_y = _image_size_y / _image_scale_factor;
      memmove(_small_picture, _unaltered_bytes, (scaled_x * scaled_y));
   }
 
   if (_average_in_file) { 
      clean_up_after_avgfile(picture_only_byte_data);
   }
}
 
 
/* This function makes a quick decision and sets things up */
void
handle_typed_file(ceos_file_descriptor, picture_only_byte_data)
   int   ceos_file_descriptor;
   byte  **picture_only_byte_data;
{ 
 
   /* Now, check the file type, and take actions to each particular file type */
   if (! strncmp(_format_type, "UNSIGNED INTEGER", 16)) {
      if (_pixel_spacing == (double) 12.5)
         _image_type = FULL_REZ;
      else
         _image_type = LOW_REZ;
 
      *picture_only_byte_data = set_up_unsigned_image(ceos_file_descriptor);
   }
   else if (! strncmp(_format_type, "COMPLEX INTEGER", 15)) {
      /* if the image was complex, */
      *picture_only_byte_data = set_up_complex_image(ceos_file_descriptor);
   }
   else {    /* the image isn't unsigned or complex */
      char image_type_failure_announcement[200];
      sprintf(image_type_failure_announcement, "Image type %s unknown", 
 	       _format_type);
      printfLLog(LOG_ERR, "ERROR:  %s", image_type_failure_announcement);
      exit(QC_UNKNOWN_IMAGE_TYPE);
   }
 
   /* you're done with the file, */
   close(ceos_file_descriptor);
}
