#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qzoom_c[] = "@(#)qzoom.c	1.21 96/12/30 15:28:58";

/*  The zoom box button's callback */
void
zoom_box_cb(widget, client_data, call_data)
Widget widget;		
XtPointer client_data;	
XtPointer call_data;	
{

   if (_image_scale_factor == -1) {
       zoom_back_out();
   }
   else if (_image_scale_factor > 1) {   /* If we're zooming in */
      _zoom_box_width = _zoom_box_height = 
 	 (ZOOM_IN_WIDTH / _image_scale_factor);
       
      /* See if a box will be useful at all! */
      if ((_image_size_y <= 1024) || (_image_size_x <= 1024)) {
 	 /* When we don't need a zoom box */
 	 zoom_in_without_box();
      }
      else { /* We're gonna need a zoom box */
 	 zoom_in_with_box();
      }
   }
   else {
      make_announcement("Can't figure out how to zoom", LOG);
      exit(QC_PROGRAMMER_ERROR);
   }
}


/* 
 * This is the event handler invoked when the zoom box is pressed
 * on the picture somewhere. 
 */
void
accept_zoom_location(Widget widget, XtPointer clientData, 
		      XEvent *event, Boolean *flag) 
{
   int image_width  = _image_size_x / _image_scale_factor;
   int image_height = _image_size_y / _image_scale_factor;
   XmString generic_string;

   /* Free up the stuff used by the box niceties */
   if (_spare_pixmap)
      XFreePixmap(_display_pointer, _spare_pixmap);
   if (_pixmap_gc)
      XFreeGC(_display_pointer, _pixmap_gc);

   /* And re-set all the buttons */
   change_all_sensitivity(RESENSITIZE_AS_NEEDED);

   /* Remove the event handlers. */
   XtRemoveEventHandler(_pic_label, ButtonPressMask, FALSE,
			accept_zoom_location, NULL);

   XtRemoveEventHandler(_pic_label, PointerMotionMask, FALSE,
			move_zoom_box, NULL);

   /* The _(x|y)_zoom_offset is used to set up the histogram
     and figure out where to zoom into */

   /* X Dimension */
   if (event->xbutton.x > (image_width - (_zoom_box_width / 2))) {
      /* Are we too close to the right? */
      _x_zoom_offset = (image_width - _zoom_box_width) 
	               * _image_scale_factor;
   }
   else if (event->xbutton.x < ((_zoom_box_width / 2))) {
      /* Or too close to the left? */
      _x_zoom_offset = 0;
   }
   else {
      /* If not, just plain accept the x location */
      _x_zoom_offset = (event->xbutton.x - (_zoom_box_width  / 2)) 
                       * _image_scale_factor;
   }

   /* Y Dimension */
   if (event->xbutton.y > (image_height - (_zoom_box_height / 2))) {
      /* Are we too close to the bottom? */
      _y_zoom_offset = (event->xbutton.y - _zoom_box_height) 
                       * _image_scale_factor;
      /* set to lowest if it's a negative offset */
      if (_y_zoom_offset < 0) {
         _y_zoom_offset = (image_height - _zoom_box_height)
                       * _image_scale_factor;
      }
   }
   else if (event->xbutton.y < ((_zoom_box_height / 2))) {
      /* Or to the top? */
      _y_zoom_offset = 0;
   }
   else {
      /* If not, just accept the y location. */
      _y_zoom_offset = (event->xbutton.y - (_zoom_box_height / 2)) 
                       * _image_scale_factor;
   }


   if (_image_type == COMPLEX) {
      _x_zoom_offset *= 4;
      _y_zoom_offset *= 4;
   }

   _x_zoom_offset += _left_border_pixels/_byte_factor;

   /* Start all over... */
   _image_scale_factor = -1;
   read_ceos_file();
   change_pixmaps();
   set_zoom_button_sensitivity();
   draw_processed_histogram(NULL, (XtPointer) _data_graph_drawing_area, NULL);

   /* Finally, change the label of the button */
   generic_string = XmStringCreateLocalized("Zoom out");
   XtVaSetValues(_zoom_box_pb, XmNlabelString,
		 generic_string, NULL);
   XmStringFree (generic_string);
}


/* Figures out the scale factor based on a few global variables */
int
figure_out_scale_factor()
{
   if (_image_size_y < SCALED_IMAGE_MAX_HEIGHT) {
      return 1;
   }
   else {
      int i, scaled_height;
 
      for (i = 2; i < 1000 ; i += 2) {
 	 scaled_height = _image_size_y / (_image_type == COMPLEX ? (i*4) : i);
 	 if (scaled_height < SCALED_IMAGE_MAX_HEIGHT) {
 	    return i;
 	 }
      }
      /* If it made it here, then it didn't find a good scale factor */
      make_announcement("figure_out_scale_factor didn't work!", LOG);
      exit(QC_PROGRAMMER_ERROR);
   }
}
 

/* For when you don't need a box to zoom in on the picture */
void
zoom_in_without_box()
{
  XmString generic_string;

   _x_zoom_offset = _y_zoom_offset = -1;
  
   _image_scale_factor = -1;
  
   read_ceos_file();
  
   change_pixmaps();
  
   set_zoom_button_sensitivity();
  
   draw_processed_histogram(NULL, (XtPointer) _data_graph_drawing_area, NULL);
  
   /* Finally, change the label of the button */
   generic_string = XmStringCreateLocalized("Zoom out");
   XtVaSetValues(_zoom_box_pb, XmNlabelString,
		 generic_string, NULL);
   XmStringFree (generic_string);
}


/* For when you need a box to zoom in on the picture */

void
zoom_in_with_box() 
{
   /* Tell the user what to do */
   make_announcement("Move cursor in the picture, and click somewhere", NO_LOG);
   make_announcement("to see that subset at full resolution.", NO_LOG);

   /* desensitize everything, so the user can only click in the pixmap  */     
   change_all_sensitivity(DESENSITIZE_ALL);
  
   set_up_box_gc();
  
   XtAddEventHandler(_pic_label, PointerMotionMask, FALSE, 
		     move_zoom_box, NULL);
  
   XtAddEventHandler(_pic_label, ButtonPressMask, FALSE,
		     accept_zoom_location, NULL);
}


/* For when you need to zoom out after you've already zoomed in, */
void
zoom_back_out() 
{

  XmString generic_string;

   /* We no long need the currently zoomed-in-on picture */
   XDestroyImage(_x_image); 
  
   if ((_image_type == COMPLEX) && ((_x_zoom_offset >= 0) ||
       (_y_zoom_offset >= 0))) {
      /* If we've just finished zooming in on a portion of a complex image */
      _image_size_y *= 4;
      _image_scale_factor = figure_out_scale_factor();
      _image_size_y /= 4;
   }
   else
      /* In any other case, */
      _image_scale_factor = figure_out_scale_factor();
  
   read_ceos_file();
  
   change_pixmaps();
   
   set_zoom_button_sensitivity();
  
   /* These are no longer valid! */
   _x_zoom_offset = -1;
   _y_zoom_offset = -1;
  
   draw_processed_histogram(NULL, (XtPointer) _data_graph_drawing_area, NULL);
  
   /* Change the button label */
   generic_string = XmStringCreateLocalized("Zoom  in");
   XtVaSetValues(_zoom_box_pb, XmNlabelString,
		generic_string, NULL);
   XmStringFree (generic_string);
}
