#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qbox_c[] = "@(#)qbox.c	1.15 95/12/21 14:08:31";

void
set_up_box_gc()
{

   XGCValues   gc_values;
 
   gc_values.foreground = (unsigned long) ZOOM_BOX_FOREGROUND;
 
   _pixmap_gc = XCreateGC(_display_pointer, XtWindow(_pic_label),
 			  GCForeground, &gc_values);
   
   /* 
    * initialze old covered x & y to negative, so we can tell when 
    * the first time the event handler is called 
    */
 
   _old_covered_x = _old_covered_y = -1;
 
   /* set the pixmap depth */
   _spare_pixmap = XCreatePixmap(_display_pointer, 
 			RootWindow(_display_pointer, _screen_number),
 			1050, 1050,
			DisplayPlanes(_display_pointer, _screen_number));
}


void
move_zoom_box(Widget widget, XtPointer clientData, XEvent *event, Boolean *flag) 
{

   int image_width  = _image_size_x / _image_scale_factor;
   int image_height = _image_size_y / _image_scale_factor;

   int box_center_x, box_center_y;

   /* Check and see where in the image the box should be... */

   /* Check the x dimension */
   if ((event->xbutton.x > (image_width - (_zoom_box_width / 2)))) {
      /* Are we too close to the right? */
      box_center_x = image_width - (_zoom_box_width / 2);
   }
   else if (event->xbutton.x < (_zoom_box_width / 2)) {
      /* Or too close to the left? */
      box_center_x = _zoom_box_width / 2;
   }
   else {
      /* If the x is OK */
      box_center_x = event->xbutton.x;
   }


   /* Now, check the y dimension */
   if (event->xbutton.y > (image_height - (_zoom_box_height / 2))) {
      /* Are we too close to the bottom? */
      box_center_y = image_height - (_zoom_box_height / 2);
   }
   else if (event->xbutton.y < (_zoom_box_height / 2)) {
      /* Are we too close to the top? */
      box_center_y = _zoom_box_height / 2;
   }
   else {
      /* The y is OK */
      box_center_y = event->xbutton.y;
   }

   /* Get the old area back */
   if ((_old_covered_x >= 0) && (_old_covered_y >= 0)) {                
      /* If this isn't the first time the even handler was called... */
      XCopyArea(_display_pointer,
		_spare_pixmap,
		XtWindow(_pic_label),
		_pixmap_gc,
		/* src x & y */
		6, 6,
		_zoom_box_width + 6, _zoom_box_height + 6,
		_old_covered_x - 3, _old_covered_y - 3);
   }

   /* set old covered x & y to the current event's x & y 
    * location, so that the next time the event handler 
    * is called, the proper location is cleaned up. 
    */

   _old_covered_x =  box_center_x - (_zoom_box_width   / 2); 
   _old_covered_y =  box_center_y - (_zoom_box_height  / 2); 

   /* Stash the new area */
   XCopyArea(_display_pointer,
	      XtWindow(_pic_label),
	      _spare_pixmap,
	      _pixmap_gc,
	      /* src x & y */
	      box_center_x - (_zoom_box_width  / 2) - 3, 
	      box_center_y - (_zoom_box_height / 2) - 3,
	      _zoom_box_width + 6, _zoom_box_height + 6,
	      6, 6);

   /* And draw in the rectangle! */
   XDrawRectangle(_display_pointer,
		  XtWindow(_pic_label),
		  _pixmap_gc,
		  /* upper left x & y: */
		  box_center_x - (_zoom_box_width  / 2), 
		  box_center_y - (_zoom_box_height / 2),
		  /* Height & width */
		  _zoom_box_width, _zoom_box_height);
}


