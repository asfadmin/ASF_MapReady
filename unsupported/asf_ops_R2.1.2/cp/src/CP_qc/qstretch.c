#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qstretch_c[] = "@(#)qstretch.c	1.22 97/05/19 15:43:59";

/* 
 * This is used when the stretching is changed (either by 
 * buttons or direct text input). 
 */

void
change_stretching(stretch_type, amount)
   int stretch_type;
   int amount;
{
   char  low_string[3],
         high_string[3];

   int   new_pixel_stretch_value;

   switch (stretch_type) { 
   case LOW: {
      new_pixel_stretch_value = _low_pixel_stretch_value + amount;

      /* if you're not going too low, and you're not going too high, */
      if (new_pixel_stretch_value > -1) { 
	 if (new_pixel_stretch_value <= _high_pixel_stretch_value) {
	    _low_pixel_stretch_value = new_pixel_stretch_value;
	    sprintf(low_string, "%3d", _low_pixel_stretch_value);
	    XmTextSetString(_low_stretch_value_text, low_string);
	    set_stretch_button_sensitivity();
	 } 
         else {  /* if you tried to go too high, */
	    _low_pixel_stretch_value = _high_pixel_stretch_value;
	    sprintf(low_string, "%3d", _low_pixel_stretch_value);
	    XmTextSetString(_low_stretch_value_text, low_string);
	    /* announce something in the major_info_label */
	    make_announcement("Stretch values must not cross", NO_LOG);
	    set_stretch_button_sensitivity();
	  }
      } 
      else {     /* if you tried to go too low, */
	 _low_pixel_stretch_value = 0;
	 sprintf(low_string, "%3d", _low_pixel_stretch_value);
	 XmTextSetString(_low_stretch_value_text, low_string);
	 set_stretch_button_sensitivity();
      }
      break;     /* get out of the CASE statement */
   }

   case LOW_INPUT: {
      new_pixel_stretch_value = amount;
      /* if you're not going too low,  and you're not going too high, */
      if (new_pixel_stretch_value > -1) {      
	 if (new_pixel_stretch_value <= _high_pixel_stretch_value) {
	    _low_pixel_stretch_value = new_pixel_stretch_value;
	    sprintf(low_string, "%3d", _low_pixel_stretch_value);
	    XmTextSetString(_low_stretch_value_text, low_string);
	    set_stretch_button_sensitivity();
	 } 
         else {         /* if you tried to go too high, */
	    _low_pixel_stretch_value = _high_pixel_stretch_value;
	    sprintf(low_string, "%3d", _low_pixel_stretch_value);
	    XmTextSetString(_low_stretch_value_text, low_string);
	    /* announce something in the major_info_label */
	    make_announcement("Stretch values must not cross", NO_LOG);
	    set_stretch_button_sensitivity();
	 }
      } 
      else {            /* if you tried to go too low, */
	 char announcement[50];
	 _low_pixel_stretch_value = 0;
	 sprintf(low_string, "%3d", _low_pixel_stretch_value);
	 XmTextSetString(_low_stretch_value_text, low_string);
	 sprintf(announcement, "Low stretch value cannot go below 0");
	 make_announcement(announcement, NO_LOG);
	 set_stretch_button_sensitivity();
      }
      break;    /* get out of the CASE statement */
   }

   case HIGH: {         /* change upper stretching */
      new_pixel_stretch_value = _high_pixel_stretch_value + amount;
      /* if you're not going too high, and you're not going too low, */
      if (new_pixel_stretch_value < 256 ) {
	 if (new_pixel_stretch_value >= _low_pixel_stretch_value) {
	    _high_pixel_stretch_value = new_pixel_stretch_value;
	    sprintf(high_string, "%3d", _high_pixel_stretch_value);
	    XmTextSetString(_high_stretch_value_text, high_string);
	    set_stretch_button_sensitivity();
	 } 
         else {         /* if you tried to go too low, */
	    _high_pixel_stretch_value = _low_pixel_stretch_value;
	    sprintf(high_string, "%3d", _high_pixel_stretch_value);
	    XmTextSetString(_high_stretch_value_text, high_string);
	    make_announcement("Stretch values must not cross", NO_LOG);
	    set_stretch_button_sensitivity();
	 }
      } 
      else {            /* if you tried to go too high, */
	 _high_pixel_stretch_value = 255;
  	 sprintf(high_string, "%3d", _high_pixel_stretch_value);
	 XmTextSetString(_high_stretch_value_text, high_string);
	 set_stretch_button_sensitivity();
      }
      break;            /* get out of the CASE statement */
   }

   case HIGH_INPUT: {   /* change upper stretching */
      new_pixel_stretch_value = amount;
      /* if you're not going too high, and you're not going too low, */
      if (new_pixel_stretch_value < 256 ) {        
	 if (new_pixel_stretch_value >= _low_pixel_stretch_value) {
	    _high_pixel_stretch_value = new_pixel_stretch_value;
	    sprintf(high_string, "%3d", _high_pixel_stretch_value);
	    XmTextSetString(_high_stretch_value_text, high_string);
	    set_stretch_button_sensitivity();
	 } 
         else {         /* if you tried to go too low, */
	    _high_pixel_stretch_value = _low_pixel_stretch_value;
	    sprintf(high_string, "%3d", _high_pixel_stretch_value);
	    XmTextSetString(_high_stretch_value_text, high_string);
	    make_announcement("Stretch values must not cross", NO_LOG);
	    set_stretch_button_sensitivity();
	 }
      } 
      else {            /* if you tried to go too high, */
	 _high_pixel_stretch_value = 255;
	 sprintf(high_string, "%3d", _high_pixel_stretch_value);
	 XmTextSetString(_high_stretch_value_text, high_string);
	 make_announcement("High stretch value cannot exceed 255", NO_LOG);
	 set_stretch_button_sensitivity(); 
      }
      break;             /* get out of the CASE statement */
   }

   case INITIALIZE: {   /* initialize all relevant variables */
      _fast_change_amount = 20;
      _slow_change_amount = 1 ;
      break;             /* get out of the CASE statement */
   }

   default : {
      make_announcement("Change stretching called improperly", LOG);
      exit(QC_PROGRAMMER_ERROR);
   }
   }

   manipulate_xcolors();
}
  

/*  The high_up button's callback */

void
high_up_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(HIGH, _slow_change_amount);
}



/*  The high_fast_up button's callback */

void
high_fast_up_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(HIGH, _fast_change_amount);
}


/*  The high_down button's callback */

void
high_down_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(HIGH, - _slow_change_amount);
}


/*  The high_fast_down button's callback */

void
high_fast_down_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(HIGH, - _fast_change_amount);
}


/*  The low_up button's callback */

void
low_up_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(LOW, _slow_change_amount);
}


/*  The low_fast_up button's callback */

void
low_fast_up_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(LOW, _fast_change_amount);
}


/*  The low_down button's callback */

void
low_down_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(LOW, - _slow_change_amount);
}


/*  The low_fast_down button's callback */

void
low_fast_down_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   change_stretching(LOW, - _fast_change_amount);
}

/*  The callback invoked when the low stretch text
    is manually changed. */

void
low_stretch_text_change_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   char  *input_text;

   if (input_text = XmTextGetString(_low_stretch_value_text)) {
      change_stretching(LOW_INPUT, atoi(input_text) );
      XtFree(input_text);
   }
}

/*  The callback invoked when the high stretch text
    is manually changed. */

void
high_stretch_text_change_cb(widget, client_data, call_data)
   Widget widget;		
   XtPointer client_data;	
   XtPointer call_data;	
{
   char *input_text;

   if (input_text = XmTextGetString(_high_stretch_value_text)) {
      change_stretching(HIGH_INPUT, atoi(input_text) );
      XtFree(input_text);
   }
}

void
manipulate_xcolors()
{
   /* Fitting the colors into the available colormap:
    *  1. grab the server so no one can goof with the colormap.
    *  2. count the available colors using XAllocColorCells.
    *  3. free the colors we just allocated.
    *  4. reduce the depth of the image to fit.
    *  5. allocate the colors again shareable.
    *  6. ungrab the server and continue on our way.
    * someone should shoot the people who designed X color allocation.
    */

   int      i, number_of_colors_available;
   unsigned int  red[256], blue[256], green[256];
   float    xstep;

   /* Figure out the number of cells we've got to work with */
   number_of_colors_available = DisplayCells(_display_pointer, _screen_number);

   if (number_of_colors_available > 256)
      number_of_colors_available = 256;

   for(i = 0; i < (255<=_low_pixel_stretch_value ? 255:_low_pixel_stretch_value); i++) {
      red[i] = blue[i] = green[i] = 0;
   }

   xstep = 255.0 / ((float) (_high_pixel_stretch_value 
           - _low_pixel_stretch_value));
  
   if (_high_pixel_stretch_value > 255)
      _high_pixel_stretch_value = 255;
   for(i = _low_pixel_stretch_value; i <= _high_pixel_stretch_value; i++)
      red[i] = blue[i] = green[i] = 
             (int) (xstep * ((float) (i - _low_pixel_stretch_value)));

   for(i = _high_pixel_stretch_value; i < number_of_colors_available; i++)
      red[i] = blue[i] = green[i] = 255;
			      
			    
   /* load up the allocated colorcells */
   for (i = 0; i < _background_colorcell; i++) {
      _xcolors[i].pixel =  i;
      _xcolors[i].flags =  DoRed | DoBlue | DoGreen;
      _xcolors[i].red   =  (unsigned short) (red   [i]   << 8);
      _xcolors[i].blue  =  (unsigned short) (blue  [i]   << 8);
      _xcolors[i].green =  (unsigned short) (green [i]   << 8);
   }

   /* Make 2 loops, skipping the background colorcell in the middle */
  
   for(i = _background_colorcell + 1; i < number_of_colors_available; i++) {
      _xcolors[i].pixel =  i;
      _xcolors[i].flags =  DoRed | DoBlue | DoGreen;
      _xcolors[i].red   =  (unsigned short) (red[i]   << 8);
      _xcolors[i].blue  =  (unsigned short) (blue[i]  << 8);
      _xcolors[i].green =  (unsigned short) (green[i] << 8);
   }
  
   if (_colormap_option == NEW_CMAP) {
      XStoreColors(_display_pointer, _colormap, _xcolors, 
		   number_of_colors_available); 
   }
   else if (_colormap_option == DEFAULT_CMAP) {
      /* Figure out how many colors are in the image */
      int   colors_in_image = _high_pixel_stretch_value 
                              - _low_pixel_stretch_value;
      int colorcell_index = 0;
      Pixel *pixel_buffer;
      XGrabServer(_display_pointer);
      pixel_buffer = (Pixel *) malloc(sizeof(Pixel) * colors_in_image);
      if (! pixel_buffer ) {
	 make_announcement("Cannot allocate a few bytes for pixels", LOG);
	 exit(QC_MALLOC_ERROR);
      }
  
      /* Count the entries already there, */
      for (colorcell_index = 0; colorcell_index < colors_in_image;
	   colorcell_index++) {
	 if (! XAllocColorCells(_display_pointer, _colormap, False, NULL, 
                                0, (pixel_buffer + colorcell_index), 1)) {
            break;
	 }
      }

      if (colorcell_index == 0) {
	 make_announcement("There are no colors left in the colormap", LOG);
	 exit(QC_XAllocColorCells_ERROR);
      }

      /* 
       * Check to see if there are more colors used
       * in the image than are available in the colormap 
       */
      if (colorcell_index < colors_in_image) {
	 dither_image();
      }
/*
      for (i = 0; i < number_of_colors_available; i++) {
	 if (!XAllocColor(_display_pointer, _colormap, &_xcolors[i])) {
	    printf("Xalloc failed for %d\n", i);
	 }
      }
*/
      XUngrabServer(_display_pointer);
   }
}



void
dither_image()
{
   unsigned int   source_pixel_length = 1;  /* in bytes */
   unsigned int   destination_line_length;  /* in bytes */
   byte           *undithered_bytes;	    /* source data */
   byte           *dithered_bytes;	    /* destination data */
   int            *curr;		    /* current line buffer */
   int            *next;		    /* next line buffer */
   int            *swap;		    /* for swapping line buffers */
   Pixel          color;	            /* pixel color */
   unsigned int   level;	            /* grey level */
   unsigned int   i, j;		            /* loop counters */

   /*
    * dither setup
    */
   destination_line_length = 
      (_x_image->width / 8) + (_x_image->width % 8 ? 1 : 0);

   undithered_bytes = _x_image->data;
   dithered_bytes   = _x_image->data;

   curr  = (int *) malloc(sizeof(int) * (_x_image->width + 2));
   next  = (int *) malloc(sizeof(int) * (_x_image->width + 2));
  
   if ((! curr) || (! next)) {
      make_announcement("Cannot allocate space for dithering", LOG);
      exit(QC_MALLOC_ERROR);
   }

   curr += 1;
   next += 1;
   for (j=0; j < _x_image->width; j++) {
      curr[j] = 0;
      next[j] = 0;
   }

   /*
    * primary dither loop
    */
   for (i = 0; i < _x_image->height; i++) {
      /* copy the row into the current line */
      for (j = 0; j < _x_image->width; j++) {
         color = (unsigned long) (*(undithered_bytes));
         undithered_bytes  += source_pixel_length;
         level = tone_scale_adjust(_xcolors[color].red >> 1);
         curr[j] += level;
      }

      /* dither the current line */
      if (i & 0x01)
         RightToLeft(curr, next, _x_image->width);
      else
         LeftToRight(curr, next, _x_image->width);

      /* copy the dithered line to the destination image */
      for (j = 0; j < _x_image->width; j++) {
         if (curr[j] < Threshold)
	    dithered_bytes[j / 8] |= 1 << (7 - (j & 7));
      }
      dithered_bytes += destination_line_length;
    
      /* circulate the line buffers */
      swap = curr;
      curr = next;
      next = swap;
      for (j = 0; j < _x_image->width; j++)
         next[j] = 0;
   }
   /*
    * clean up
    */
   free((byte *) (curr-1));
   free((byte *) (next-1));
}


/*
 * dither a line from left to right
 */
static void LeftToRight(curr, next, width)
   int   *curr;
   int   *next;
   int   width;
{
   int   idx;
   int   error;
   int   output;

   for (idx=0; idx<width; idx++) {
      output       = (curr[idx] > Threshold) ? MaxGrey : MinGrey;
      error        = curr[idx] - output;
      curr[idx]    = output;
      next[idx-1] += error * 3 / 16;
      next[idx]   += error * 5 / 16;
      next[idx+1] += error * 1 / 16;
      curr[idx+1] += error * 7 / 16;
   }
}


/*
 * dither a line from right to left
 */
static void RightToLeft(curr, next, width)
   int   *curr;
   int   *next;
   int   width;
{
   int   idx;
   int   error;
   int   output;

   for (idx=(width-1); idx>=0; idx--) {
      output       = (curr[idx] > Threshold) ? MaxGrey : MinGrey;
      error        = curr[idx] - output;
      curr[idx]    = output;
      next[idx+1] += error * 3 / 16;
      next[idx]   += error * 5 / 16;
      next[idx-1] += error * 1 / 16;
      curr[idx-1] += error * 7 / 16;
   }
}


/*
 * a _very_ simple tone scale adjustment routine. provides a piecewise
 * linear mapping according to the following:
 *
 *      input:          output:
 *     0 (MinGrey)    0 (MinGrey)
 *     Threshold      Threshold/2
 *     MaxGrey        MaxGrey
 * 
 * this should help things look a bit better on most displays.
 */

static unsigned int 
tone_scale_adjust(input_value)
   unsigned int input_value;
{
   unsigned int result;
  
   if (input_value < Threshold)
      result = input_value / 2;
   else
      result = (((input_value - Threshold) * (MaxGrey-(Threshold/2))) /
               (MaxGrey-Threshold)) + (Threshold/2);
   return result;
}
