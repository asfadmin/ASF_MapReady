#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qpixmap_c[] = "@(#)qpixmap.c	1.10 96/03/11 13:23:39";

void
change_pixmaps()
{

   if (_image_pixmap == XmUNSPECIFIED_PIXMAP) {
      make_announcement("Problem changing pixmaps..", LOG);
      exit(QC_PIXMAP_CREATE_PROBLEM);
   }  
   else {
      Pixmap old;
      XtVaGetValues(_pic_label, XmNlabelPixmap, &old, NULL);
 
      /* Use an Xlib call, since the pixmap was created with an xlib call... */
      XFreePixmap(_display_pointer, old);
 
      XtVaSetValues(_pic_label, XmNlabelPixmap, _image_pixmap, NULL);
   }
}


/*
 * This function takes in a pointer to a picture buffer, and
 * turns it into an XImage* and a pixmap (if neccessary), 
 */

void
set_up_picture(picture_only_byte_data)
   byte  *picture_only_byte_data;
{
   int picture_width, picture_height;
 
   if (_is_sys_pp == 1 && _image_scale_factor == -1) {
      picture_width = _boxed_image_width;
      picture_height = _boxed_image_height;
   }
   else {
   get_visible_width_and_height (&picture_width,
				 &picture_height);
   }
 
   if (_background_colorcell != 0) {
      /* 
       * In this case, the histogram is dynamic, and the fact that we've 
       * changed the data to hide the background color will be a problem. 
       */
      copy_and_alter_bytes(picture_only_byte_data, picture_height, picture_width);
   }
 
   /* set up the XImage typed variable */
   _x_image = XCreateImage(_display_pointer,     
 		  DefaultVisual(_display_pointer, _screen_number), 
 		  DisplayPlanes(_display_pointer, _screen_number),
 		  ZPixmap,     /* format type */
 		  0,           /* pixels to skip @ start of each line */
 		  (char *) picture_only_byte_data,   /* gotten! */
 		  picture_width,
 		  picture_height,
 		  8,           /* multiples of this many bits */
 		  0);          /* scan lines are continuous */
 
   _x_image->byte_order = MSBFirst; 
 
   /* set the pixmap depth */
   _image_pixmap = XCreatePixmap(_display_pointer, 
 			 RootWindow(_display_pointer, _screen_number),
 			 _x_image->width, _x_image->height,
 			 DisplayPlanes(_display_pointer, _screen_number));
 
   /* make a pixmap! */
   XPutImage(_display_pointer, 
 	     _image_pixmap, 
 	     DefaultGC(_display_pointer, _screen_number),
 	     _x_image,
 	     0, 0,                     /* upper left corner */
 	     0, 0,                     /* place image */
 	     _x_image->width, _x_image->height);
}
