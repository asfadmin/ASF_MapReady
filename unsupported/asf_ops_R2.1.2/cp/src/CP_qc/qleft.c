/*
   Layout of the left side (pic side) of the quality
   analysis app 
   */

#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qleft_c[] = "@(#)qleft.c	1.16 96/03/11 13:23:37";

/* 
 * This sets up the layout and adds callbacks (whenever appropriate)
 * for the left side of the app.
 */

Widget
set_up_left(parent_form)
   Widget parent_form ;
{
   Widget  scrolled_window_widget ;
 
   scrolled_window_widget = XtVaCreateManagedWidget("pic",
 		    xmScrolledWindowWidgetClass, parent_form,
 		    XmNscrollBarDisplayPolicy, XmAS_NEEDED,
 		    XmNscrollingPolicy,        XmAUTOMATIC,
 		    XmNtopAttachment,          XmATTACH_WIDGET,
 		    XmNtopWidget,              _menubar,
 		    XmNbottomAttachment,       XmATTACH_WIDGET,
                    XmNbottomWidget,           _major_info_frame,
 		    XmNleftAttachment,         XmATTACH_FORM,
 		    XmNrightAttachment,        XmATTACH_WIDGET,
 		    XmNrightWidget,            _right_outer_frame,
 		    NULL);
 
   if ((_image_type == FULL_REZ) || (_image_type == COMPLEX)) {
      XtVaGetValues(scrolled_window_widget,
 		    XmNhorizontalScrollBar, &_horizontal_sb,
 		    XmNverticalScrollBar,   &_vertical_sb,
 		    XmNclipWindow,          &_clip_window,
 		    NULL) ;
 
      /* use same callback for both ScrollBars and all callback reasons */
#if 0
      /* For callbacks called after a drag operation... */
      /* I don't this that this is needed. */
      XtAddCallback(_vertical_sb, 
 		    XmNvalueChangedCallback, 
 		    draw_histogram, (XtPointer) _data_graph_drawing_area) ;
      XtAddCallback(_horizontal_sb, 
 		    XmNvalueChangedCallback, 
 		    draw_histogram, (XtPointer) _data_graph_drawing_area) ;
#endif
 
      /* FOR DRAWING THE HISTOGRAM */
 	  
      /* For callbacks during page increment / decrement */
      XtAddCallback(_vertical_sb, 
                    XmNpageIncrementCallback, 
 		    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
      XtAddCallback(_vertical_sb, 
                    XmNpageDecrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
 	  
      XtAddCallback(_horizontal_sb, 
                    XmNpageIncrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
      XtAddCallback(_horizontal_sb, 
                    XmNpageDecrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
 	  
   /* For callbacks called during a drag operation... */
      XtAddCallback(_vertical_sb, 
                    XmNdragCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
      XtAddCallback(_horizontal_sb, 
                    XmNdragCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
 	  
   /* for callbacks called after any increment / decrement,
      i.e. when the arrows are pressed. */
      XtAddCallback(_vertical_sb, 
                    XmNincrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
      XtAddCallback(_horizontal_sb, 
                    XmNincrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
 	  
      XtAddCallback(_vertical_sb, 
                    XmNdecrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
      XtAddCallback(_horizontal_sb, 
                    XmNdecrementCallback, 
                    draw_processed_histogram, (XtPointer) _data_graph_drawing_area) ;
 
   }
 
   if (_image_pixmap == XmUNSPECIFIED_PIXMAP) {
      char *pixmap_announcement = (char *) malloc(400 * sizeof(char)) ;
      sprintf(pixmap_announcement, "can't create pixmap from %s\n", _filename);
      make_announcement(pixmap_announcement, LOG) ;
      free(pixmap_announcement) ;
      exit(QC_PIXMAP_CREATE_PROBLEM) ;
   }
 
   /* Now create _pic_label using the pixmap */
   _pic_label = XtVaCreateManagedWidget("pic_label", xmLabelWidgetClass, 
                   scrolled_window_widget, XmNshadowThickness, 0,
 		   XmNlabelType, XmPIXMAP, XmNlabelPixmap, _image_pixmap,
 		   XmNcolormap, _colormap, NULL);
 
   return scrolled_window_widget ;
}
