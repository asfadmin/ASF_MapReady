#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qbackground_pixel_c[] = "@(#)qbackground_pixel.c	1.10 95/12/21 14:08:25";

/* 
 * This function figures out what colorcell the main form
 * is using for the default background, and sets up a 
 * global variable 
 */

void
find_background_pixel_value(main_form) 
   Widget main_form;
{

   Pixel background;

   XtVaGetValues(main_form, XmNbackground, &background, NULL);
   _background_colorcell = (int) background;
}

/* 
 * This function changes all the bytes that happen to be
 * the same value as the background color to one byte higher 
 */

void
remove_background_from_x_image(height, width, data)
   int   height, width;
   byte  *data;
{
   int   i, total_size;

   total_size = height * width;

   for (i = 0; i < total_size; i++) {
      if ((int) data[i] == _background_colorcell) {
         /* change background color */
	 data[i] = (byte) (_background_colorcell + 1);
      }
   }
}



/* 
 * This function is the beginning of the process to
 * copy and remove the background pixel from an 
 * image  buffer 
 */

void
copy_and_alter_bytes(byte_data, picture_height, picture_width)
   byte  *byte_data;
   int   picture_height, picture_width;
{
   
   /*
    * Here, remove all values in the data that are the same
    * value as the background of the application (if needed) 
    */
   
   if ((_image_type == FULL_REZ) || (_image_type == COMPLEX)) {
      /* 
       * If the image is full-rez, then the histogram will be screwy 
       * if we don't keep a copy of the unaltered data around 
       */
      /* Check to see if we've got an old buffer lying around */
      if (_unaltered_bytes) {
 	 free(_unaltered_bytes);
 	 _unaltered_bytes = NULL;
      }
 	  
      _unaltered_bytes = (byte *) malloc(picture_width *
 					 picture_height * sizeof(byte));
 	  
      if (! _unaltered_bytes) {
 	 make_announcement("Malloc problem", LOG);
 	 exit(QC_MALLOC_ERROR);
      }
      memmove(_unaltered_bytes, byte_data, (picture_width * picture_height));
   }
   /* Then remove the background color from the image */
   remove_background_from_x_image(picture_height, picture_width, byte_data);
}
 
