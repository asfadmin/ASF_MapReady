#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */
 
static char sccsid_qhistogram_c[] = "@(#)qhistogram.c	1.19 97/02/12 09:07:00";
 
/*  This function fills up the histogram array with values from
 *  the picture, constrained to fit within the clip window on 
 *  the left side.  
 */
 
void
create_histogram_array_from_clipped_picture() 
/* to be set up: _histogram_array, _histogram_max, min, mean, sdev. */
{
 
   int       i, beginning_row, beginning_column,
             ending_row, ending_column, row_index, column_index, total_size;
   int       window_int_width, window_int_height, byte_data;
   byte      *bytes_for_histogram;
   float     accumulator = 0.0, tmp1, tmp2;
   Widget    clip_window;
   Dimension window_dimension_width, window_dimension_height;
 
   if (((_image_type == FULL_REZ) || (_image_type == COMPLEX)) &&
       (_background_colorcell != 0)) {
      /* If the XImage contains data that was altered to avoid color problems */
      if (_unaltered_bytes) {
 	 bytes_for_histogram = _unaltered_bytes;
      }
      else {
 	 make_announcement("Problem using histogram picture data", LOG);
 	 exit(QC_PROGRAMMER_ERROR);
      }
   }
   else {
      /* If the XImage wasn't corrupted */
      bytes_for_histogram = _x_image->data;
   }
 
   /* Get a hold of the clip window */
   XtVaGetValues(_left_scrolled_window, XmNclipWindow, &clip_window, NULL);
 
   /* Find out the size of the clipped window */
   XtVaGetValues(clip_window, XmNwidth,  &window_dimension_width,
 		 XmNheight, &window_dimension_height, NULL);
 
   /* 
    * Caste dimensions to integers... The clip window has a border
    * of 6 pixels, so take care of that here (boy, *that* took forever
    * to figure out!). 
    */
   window_int_width  = ((int) window_dimension_width)  - 6;
   window_int_height = ((int) window_dimension_height) - 6;
 
   /* clean out the histogram */
   for (i = 0; i < _processed_histogram_size ; i++) 
      _processed_histogram_array[i] = 0;
 
   /* figure out how to put the histogram array together. */
   if ((window_int_height > _x_image->height) &&
       (window_int_width  > _x_image->width)) {
      /* if the entire image fits in the clip window... */
      total_size = _x_image->width * _x_image->height;
 
      /* left original zero untouched in histogram */
      for (i = 0; i < total_size ; i++) {
         _processed_histogram_array[(int) bytes_for_histogram[i]]++;
      }
   }
   else {
      /* Only read stuff inside the clip window... */
 
      int horizontal_slider_position, horizontal_slider_size, 
      /* not really needed: */
          horizontal_increment, horizontal_page_increment;
       
      int vertical_slider_position, vertical_slider_size, 
      /* not really needed: */
          vertical_increment, vertical_page_increment;
 
      /* Nab some of the scroll bar values... */
      XmScrollBarGetValues(_horizontal_sb,
 			   &horizontal_slider_position,
 			   &horizontal_slider_size,
 			   &horizontal_increment,
 			   &horizontal_page_increment);
       
      XmScrollBarGetValues(_vertical_sb,
 			   &vertical_slider_position,
 			   &vertical_slider_size,
 			   &vertical_increment,
 			   &vertical_page_increment);
       
      beginning_column = horizontal_slider_position;
       
      beginning_row  = vertical_slider_position * _x_image->width;
       
      /* And where we stop looking... */
      ending_column = beginning_column + window_int_width;
      ending_row = beginning_row + ((window_int_height) * _x_image->width);
 
      total_size = ((ending_row - beginning_row) / _x_image->width)  *
                    (ending_column - beginning_column);
 
      for (row_index = beginning_row; row_index < ending_row;
           row_index += _x_image->width) 
         for (column_index = beginning_column; column_index < ending_column;
              column_index++) {
            byte_data = (int) bytes_for_histogram[row_index + column_index];
            /* exclude the zero in histogram while zoom in */
            if (byte_data != 0) {
               _processed_histogram_array[byte_data]++;
            }
         }
   }
 
   /* Set up max, min, and their locations.. */
   _histogram_max = -1;      /* initialize to the opposite */
   _histogram_min = 10000000;
 
   for (i = 0; i < 256 ; i++) {
      if (_processed_histogram_array[i] > _histogram_max) {
         _histogram_max = _processed_histogram_array[i];
 	 _histogram_max_location = i;
      }
 
      if (_processed_histogram_array[i] < _histogram_min) {
 	 _histogram_min = _processed_histogram_array[i];
 	 _histogram_min_location = i;
      }
   }
 
   /* Finally, compute the mean / standard deviation... */
 
   tmp1 = 0.0;
   tmp2 = 0.0;
 
   /* exclude the zero element of histogram array into mean and sdev */
   for (i = 1; i < 256; i++) {
      tmp1 += (float) (_processed_histogram_array[i] * (i * i));
      tmp2 += (float) (_processed_histogram_array[i] * i);
      /* This is used to compute the mean .... */
      accumulator += (float) _processed_histogram_array[i];
   }
 
   _data_mean = tmp2 / accumulator;
 
   _data_sdev = fsqrt(fabs((tmp1 / accumulator) - (_data_mean * _data_mean)));
 
   /* Now figure out the data min and max */
   for (i = 0; i < 256 ; i++) {
      if (_processed_histogram_array[i] != 0) {
 	 _data_min = i;
 	 break;
      }
   }
 
   for (i = 255; i >= 0 ; i--) {
      if (_processed_histogram_array[i] != 0) {
 	 _data_max = i;
 	 break;
      }
   }
}
 
 
/* 
 * This function draws lines in the histogram's graphics
 * context -- horizontal dashed lines, and little vertical lines.  
 */
 
void
draw_processed_histogram_delimiting_lines(widget, graph_height, graph_width) 
   Widget   widget;
   int      graph_height, graph_width;
{
   int   i, j;
 
   /* First, set up the bottom border */
   XDrawLine(_display_pointer,
 	     XtWindow(widget),
 	     _data_histogram_gc,
 	     0, graph_height - 1,
 	     graph_width, graph_height - 1);
 
   for (i = (graph_height / 8); i <= (graph_height - (graph_height / 8)); 
        i += (graph_height / 8)) {
      /* here's a bunch of "dotted horizontal lines" */
      for (j = 0; j < graph_width; j += (graph_width / 32)) {
 	 XDrawLine(_display_pointer,
 		   XtWindow(widget),
 		   _data_histogram_gc,
 		   /* from: */
 		   j, i,
 		   /* to: */
 		   j + 1, i);
      }
   }
 
   /* vertical lines... */
   for (i = (graph_width / 8); i < (graph_width - (graph_width / 8)); 
        i += (graph_width / 8)) {
      /* top: */
      XDrawLine(_display_pointer,
 		XtWindow(widget),
 		_data_histogram_gc,
 		i, 1,
 		i, 6);
      /* bottom: */
      XDrawLine(_display_pointer,
 		XtWindow(widget),
 		_data_histogram_gc,
 		i, graph_height - 1,
 		i, graph_height - 6);
   }
}
 
     
/* 
 * This function draws the lines & the graph of the histogram
 * on the graphics context for the histogram 
 */
 
void
draw_processed_histogram(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   int         i, j, graph_int_width, graph_int_height;
   float       scale_down_divisor, scale_sideways_divisor;
   Dimension   graph_dimension_width, graph_dimension_height;
   Widget      histogram_widget = (Widget) client_data;
 
   /* First of all, see if we can draw anything.  */
   if (histogram_widget == NULL)
      return;
 
   /* clear the window */
   XClearWindow(_display_pointer, 
 		XtWindow(histogram_widget));
 
   /* if client_data comes in, you don't need to find out the info again */
   if (_image_type == COMPLEX) {
/* * 
   if ((_image_type == FULL_REZ)) {
   if ((_image_type == FULL_REZ) || (_image_type == COMPLEX)) {
   if ((_image_type == FULL_REZ) && (! client_data))  {
* */
      /* If you need to re-compute the histogram... */
      create_histogram_array_from_clipped_picture(); 
      set_up_histogram_dependant_labels(); 
   }
       
   /* Nab the dimensions of the drawing area... */
   XtVaGetValues(histogram_widget, XmNwidth,  &graph_dimension_width,
 		 XmNheight, &graph_dimension_height, NULL);
   
   /* Convert the dimesions to integers... */
   graph_int_width = (int) graph_dimension_width;
   graph_int_height = (int) graph_dimension_height;
 
   /* Figure out how we're gonna scale the graph to fit into the given widget */
   scale_down_divisor = (float) _histogram_max / (float) graph_int_height;
 
   scale_down_divisor *= 1.1;
 
   scale_sideways_divisor = 255.0 / (float) graph_int_width;
 
   /* Set up some graph delimiting lines.. */
   draw_processed_histogram_delimiting_lines(histogram_widget, graph_int_height, graph_int_width);
 
   /* put a line where the max location is at */
   XDrawLine(_display_pointer,
 	     XtWindow(histogram_widget),
 	     _data_histogram_gc,
 	     /* from: */
 	     (int) ((float) _histogram_max_location 
             / scale_sideways_divisor),
 	     graph_int_height - 1,
 	     /* to: */
 	     (int) ((float) _histogram_max_location
 	     / scale_sideways_divisor),
 	     graph_int_height - 25);
 
   /* Draw all the lines of the graph itself in */
   for (i = 1; i < (_processed_histogram_size - 1) ; i++) {
       XDrawLine(_display_pointer,
 		 XtWindow(histogram_widget),
 		 _data_histogram_gc,
 		 /* from: */
 		 (int) ((float) i / scale_sideways_divisor),
 		 (int) ((float) graph_int_height - 
 		 ((float) _processed_histogram_array[i] / scale_down_divisor)),
 		 /* to: */
 		 (int) ((float) (i + 1) / scale_sideways_divisor), 
 		 (int) ((float) graph_int_height - 
 		       ((float) _processed_histogram_array[i + 1] / scale_down_divisor)));
   }
}

void
draw_i_histogram(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   int         i, j, graph_int_width, graph_int_height;
   float       scale_down_divisor, scale_sideways_divisor;
   Dimension   graph_dimension_width, graph_dimension_height;
   Widget      histogram_widget = (Widget) client_data;
 
   /* First of all, see if we can draw anything.  */
   if (histogram_widget == NULL)
      return;
 
   /* clear the window */
   XClearWindow(_display_pointer, 
 		XtWindow(histogram_widget));
 
   /* Nab the dimensions of the drawing area... */
   XtVaGetValues(histogram_widget, XmNwidth,  &graph_dimension_width,
 		 XmNheight, &graph_dimension_height, NULL);
   
   /* Convert the dimesions to integers... */
   graph_int_width = (int) graph_dimension_width;
   graph_int_height = (int) graph_dimension_height;
 
   /* Figure out how we're gonna scale the graph to fit into the given widget */
   scale_down_divisor = (float) _i_histogram_max / (float) graph_int_height;
 
   scale_down_divisor *= 1.1;
 
   scale_sideways_divisor = 255.0 / (float) graph_int_width;
 
   /* Set up some graph delimiting lines.. */
   draw_i_histogram_delimiting_lines(histogram_widget, graph_int_height, graph_int_width);
 
   /* put a line where the max location is at */
   XDrawLine(_display_pointer,
 	     XtWindow(histogram_widget),
 	     _i_histogram_gc,
 	     /* from: */
 	     (int) ((float) _i_histogram_max_location 
             / scale_sideways_divisor),
 	     graph_int_height - 1,
 	     /* to: */
 	     (int) ((float) _i_histogram_max_location
 	     / scale_sideways_divisor),
 	     graph_int_height - 25);
 
   /* Draw all the lines of the graph itself in */
   for (i = 1; i < (_i_histogram_size - 1) ; i++) {
       XDrawLine(_display_pointer,
 		 XtWindow(histogram_widget),
 		 _i_histogram_gc,
 		 /* from: */
 		 (int) ((float) i / scale_sideways_divisor),
 		 (int) ((float) graph_int_height - 
 		 ((float) _i_histogram_array[i] / scale_down_divisor)),
 		 /* to: */
 		 (int) ((float) (i + 1) / scale_sideways_divisor), 
 		 (int) ((float) graph_int_height - 
 		       ((float) _i_histogram_array[i + 1] / scale_down_divisor)));
   }
}


/* 
 * This function draws lines in the histogram's graphics
 * context -- horizontal dashed lines, and little vertical lines.  
 */
 
void
draw_i_histogram_delimiting_lines(widget, graph_height, graph_width) 
   Widget   widget;
   int      graph_height, graph_width;
{
   int   i, j;
 
   /* First, set up the bottom border */
   XDrawLine(_display_pointer,
 	     XtWindow(widget),
 	     _i_histogram_gc,
 	     0, graph_height - 1,
 	     graph_width, graph_height - 1);
 
   for (i = (graph_height / 8); i <= (graph_height - (graph_height / 8)); 
        i += (graph_height / 8)) {
      /* here's a bunch of "dotted horizontal lines" */
      for (j = 0; j < graph_width; j += (graph_width / 32)) {
 	 XDrawLine(_display_pointer,
 		   XtWindow(widget),
 		   _i_histogram_gc,
 		   /* from: */
 		   j, i,
 		   /* to: */
 		   j + 1, i);
      }
   }
 
   /* vertical lines... */
   for (i = (graph_width / 8); i < (graph_width - (graph_width / 8)); 
        i += (graph_width / 8)) {
      /* top: */
      XDrawLine(_display_pointer,
 		XtWindow(widget),
 		_i_histogram_gc,
 		i, 1,
 		i, 6);
      /* bottom: */
      XDrawLine(_display_pointer,
 		XtWindow(widget),
 		_i_histogram_gc,
 		i, graph_height - 1,
 		i, graph_height - 6);
   }
}

void
draw_q_histogram(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   int         i, j, graph_int_width, graph_int_height;
   float       scale_down_divisor, scale_sideways_divisor;
   Dimension   graph_dimension_width, graph_dimension_height;
   Widget      histogram_widget = (Widget) client_data;
 
   /* First of all, see if we can draw anything.  */
   if (histogram_widget == NULL)
      return;
 
   /* clear the window */
   XClearWindow(_display_pointer, 
 		XtWindow(histogram_widget));
 
   /* Nab the dimensions of the drawing area... */
   XtVaGetValues(histogram_widget, XmNwidth,  &graph_dimension_width,
 		 XmNheight, &graph_dimension_height, NULL);
   
   /* Convert the dimesions to integers... */
   graph_int_width = (int) graph_dimension_width;
   graph_int_height = (int) graph_dimension_height;
 
   /* Figure out how we're gonna scale the graph to fit into the given widget */
   scale_down_divisor = (float) _q_histogram_max / (float) graph_int_height;
 
   scale_down_divisor *= 1.1;
 
   scale_sideways_divisor = 255.0 / (float) graph_int_width;
 
   /* Set up some graph delimiting lines.. */
   draw_q_histogram_delimiting_lines(histogram_widget, graph_int_height, graph_int_width);
 
   /* put a line where the max location is at */
   XDrawLine(_display_pointer,
 	     XtWindow(histogram_widget),
 	     _q_histogram_gc,
 	     /* from: */
 	     (int) ((float) _q_histogram_max_location 
             / scale_sideways_divisor),
 	     graph_int_height - 1,
 	     /* to: */
 	     (int) ((float) _q_histogram_max_location
 	     / scale_sideways_divisor),
 	     graph_int_height - 25);
 
   /* Draw all the lines of the graph itself in */
   for (i = 1; i < (_q_histogram_size - 1) ; i++) {
       XDrawLine(_display_pointer,
 		 XtWindow(histogram_widget),
 		 _q_histogram_gc,
 		 /* from: */
 		 (int) ((float) i / scale_sideways_divisor),
 		 (int) ((float) graph_int_height - 
 		 ((float) _q_histogram_array[i] / scale_down_divisor)),
 		 /* to: */
 		 (int) ((float) (i + 1) / scale_sideways_divisor), 
 		 (int) ((float) graph_int_height - 
 		       ((float) _q_histogram_array[i + 1] / scale_down_divisor)));
   }
}


/* 
 * This function draws lines in the histogram's graphics
 * context -- horizontal dashed lines, and little vertical lines.  
 */
 
void
draw_q_histogram_delimiting_lines(widget, graph_height, graph_width) 
   Widget   widget;
   int      graph_height, graph_width;
{
   int   i, j;
 
   /* First, set up the bottom border */
   XDrawLine(_display_pointer,
 	     XtWindow(widget),
 	     _q_histogram_gc,
 	     0, graph_height - 1,
 	     graph_width, graph_height - 1);
 
   for (i = (graph_height / 8); i <= (graph_height - (graph_height / 8)); 
        i += (graph_height / 8)) {
      /* here's a bunch of "dotted horizontal lines" */
      for (j = 0; j < graph_width; j += (graph_width / 32)) {
 	 XDrawLine(_display_pointer,
 		   XtWindow(widget),
 		   _q_histogram_gc,
 		   /* from: */
 		   j, i,
 		   /* to: */
 		   j + 1, i);
      }
   }
 
   /* vertical lines... */
   for (i = (graph_width / 8); i < (graph_width - (graph_width / 8)); 
        i += (graph_width / 8)) {
      /* top: */
      XDrawLine(_display_pointer,
 		XtWindow(widget),
 		_q_histogram_gc,
 		i, 1,
 		i, 6);
      /* bottom: */
      XDrawLine(_display_pointer,
 		XtWindow(widget),
 		_q_histogram_gc,
 		i, graph_height - 1,
 		i, graph_height - 6);
   }
}
