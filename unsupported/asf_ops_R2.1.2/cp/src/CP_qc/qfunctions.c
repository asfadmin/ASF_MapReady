#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */
#include "qversion.h"

static char sccsid_qfunctions_c[] = "@(#)qfunctions.c	1.25 97/07/14 15:16:51";

/* 
 * This function sets up numerous global variables, and
 * sets up the program to work in the given X environment. 
 */

void
set_up_environment(app, program_name, main_form) 
   XtAppContext app;
   char         *program_name;
   Widget       *main_form;
{

   unsigned long  planemask[256], colorcells[256];
  
   char title_string[333];
   char *executable_name = strrchr(program_name, '/');
  
   if (executable_name == NULL)
      /* if there was no slash in the execution name... */
      executable_name = program_name;
   else
      /* if there was a slash, point to the first char after the slash */
      executable_name++;
   

   /* Change the title string for the window */
   sprintf(title_string, "%s v%s (Image %s)",
	   executable_name,
	   CP_QC_VERSION,
	   _filename);
 
   XtVaSetValues(_toplevel, XmNtitle, title_string, NULL);
 
   /* This is for the zooming... */
   _x_zoom_offset = _y_zoom_offset = -1;

   /* This is for the colormap problem */
   _background_colorcell = 0;

   *main_form = XtVaCreateWidget("main_form", xmFormWidgetClass, 
                                  _toplevel, NULL);

   if (_colormap_option == NEW_CMAP) {  
      find_background_pixel_value(*main_form);
   }      
  
   /* open the display */
   _display_pointer = XtDisplay(_toplevel);

   if (_display_pointer == NULL) {
      char display_error[200];

      sprintf(display_error, "%s: can not open display: %s\n",
	      program_name, XDisplayName(NULL));
      make_announcement(display_error, LOG);
      free(display_error);

      exit(QC_DISPLAY_ERROR);
   }


   /* set up global X stuff*/
   _screen_number = XDefaultScreen(_display_pointer);

   /* Setup the colormap */
   if ((_xcolors = (XColor *) calloc(DisplayCells(_display_pointer, 
        _screen_number), sizeof(XColor))) == NULL) {
      make_announcement("There isn't enough memory for setting up the colormap", LOG);
      exit(QC_CALLOC_ERROR);
   }


   /* Tek's terminals make pseudocolor windows...*/

   if (_colormap_option == DEFAULT_CMAP) {
      Colormap default_cmap = XDefaultColormap(_display_pointer, _screen_number);
      _colormap = XDefaultColormap(_display_pointer, _screen_number);
/*
      _colormap = XCopyColormapAndFree(_display_pointer, default_cmap);
*/
   }
   else if (_colormap_option == NEW_CMAP) {
      _colormap = XCreateColormap(_display_pointer, 
		  RootWindow(_display_pointer, _screen_number), 
		  DefaultVisual(_display_pointer, _screen_number), AllocAll);
      
      if (XAllocColorCells(_display_pointer, _colormap, True, planemask, 
          DisplayPlanes(_display_pointer, _screen_number), colorcells, 1)) {
         make_announcement("Can't open the colormap", LOG);
         exit(QC_XAllocColor_ERROR);
      }
   }
   else {
      make_announcement("Colormap not set up", LOG);
      exit(QC_PROGRAMMER_ERROR);
   }
}

/* 
 *This is a utility which either prints stuff on the 
 * tty, prints stuff in the announcement window while the
 * app is running, or prints stuff on the tty and exits.
 */

void
make_announcement(announcement_string, log_option)
     char *announcement_string;
     int  log_option;
{
  extern char *QC_SYSLOG_VARIABLE;
  char        *fixed_string;
  int         prefix_size = strlen (_qc_log_prefix) + 1;

  QC_SYSLOG_VARIABLE = (char *) malloc (prefix_size * sizeof (char));
  strcpy (QC_SYSLOG_VARIABLE, _qc_log_prefix);

  fixed_string = (char *) malloc ((prefix_size +
				   strlen (announcement_string))
				  * sizeof (char));

  strcpy (fixed_string, _qc_log_prefix);
  strcpy (&fixed_string[ strlen(_qc_log_prefix)],
	  announcement_string);

  if (log_option == INFO) {
      /* If this is NOT going to be an error, */

      /* First, log the info */
      printfLLog(LOG_INFO, announcement_string);

      /* Then print it out on the terminal */
      fprintf(stderr, "%s\n", fixed_string);
      fflush(stderr);

  }
  else if (log_option == LOG) {
    /* If this is going to be an error, */
    
    /* First, log the error */
    printfLLog(LOG_ERR, announcement_string);
    
    /* Then print it out on the terminal */
    fprintf(stderr, "%s\n", fixed_string);
    fflush(stderr);
    
  }
  else if (log_option == NO_LOG) {

    if (_major_info_frame != NULL) {
      /* If this is going to go on the bottom of the app, */
      int  old_item_count;
      XmString generic_string;

      XtVaGetValues(major_info_scrolled_list,
		    XmNitemCount, &old_item_count, NULL);

      generic_string = XmStringCreateLocalized(fixed_string);
      XmListAddItem(major_info_scrolled_list,
		    generic_string,
		    old_item_count + 1);
      XmStringFree (generic_string);
      
      XmListSetPos(major_info_scrolled_list, old_item_count);
      
    } 
    else {  /* Just printf out something on the terminal... */
      fprintf(stdout, "%s\n", fixed_string);
      fflush(stdout);
    }
  }

  free (fixed_string);
  free (syslog_tag);
}
    
    
/* 
 * This is a utility for reading a given number of bytes 
 * from a buffer and returning their integer value. 
 */

int
read_n_bytes(n, location, byte_data, value_name, zero_possibility)
   int  n;
   int  location;
   byte *byte_data;
   char *value_name;
   int  zero_possibility;

{
   char    bit_holding_string[400];

   int     return_int;

   memmove(bit_holding_string, &byte_data[location], n);
   bit_holding_string[n] = NULL;

   if ((sscanf(bit_holding_string, "%d", &return_int)) <= 0) {
      if (zero_possibility == CAN_BE_ZERO)
	return_int = atoi(bit_holding_string);
      else {
	 char *error_holding_string = (char *) malloc(200 * sizeof(char));
	 sprintf(error_holding_string, 
		   "CEOS file error: improper %s value", value_name);
	 make_announcement(error_holding_string, LOG);
	 free(error_holding_string);

	 exit(QC_IMPROPER_CEOS_VALUE);
      }
   }
  
   if (_verbose_option == VERBOSE) {
      char *announcement_string = (char *) malloc(200 * sizeof(char));
      sprintf(announcement_string, "%s = %d", value_name, return_int);
      make_announcement(announcement_string, NO_LOG);
      free(announcement_string);
   }

  return return_int;
}


/* 
 * This is used to make the time string (taken out of the leader file)
 * readable 
 */

void
add_colons_to(time_string_with_colons, time_string_without_colons)
   char *time_string_with_colons, *time_string_without_colons;
{

  /* substitute ":" for any instances of "T" or "-", but don't try to
     insert anything */

  int i, j;
  int new_increment = 0;

  /* First, find the initial non-null character */
  for (i = 0; i < TIME_LENGTH ; i++) {
    if (time_string_without_colons[i] != ' ') {
      break;
    }
  }

/***
  for (; i < TIME_LENGTH ; i++) {
    switch (time_string_without_colons[i]) {
    case '-':
    case 'T':
      time_string_with_colons[new_increment++] = ':' ;
      break;
    case ' ':
      time_string_with_colons[new_increment++] = NULL;
      return;
      break;
    default:
      time_string_with_colons[new_increment++] = time_string_without_colons[i] ;
    }
  }
  
***/

#if 1     /* This is no longer what we want, */

   /* first, the year: */
   for (i = 0, j = 0; i < 4 ; i++, j++) 
      time_string_with_colons[j] = time_string_without_colons[i];

   time_string_with_colons[j++] = '/';
  
   /* then, the month */
   for (i = 4; i < 6; i++, j++) 
      time_string_with_colons[j] = time_string_without_colons[i];

   time_string_with_colons[j++] = '/';

   /* then, the day */
   for (i = 6; i < 8; i++, j++) 
      time_string_with_colons[j] = time_string_without_colons[i];

   time_string_with_colons[j++] = '-';

   /* then, the hours */
   for (i = 8; i < 10 ; i++, j++) 
      time_string_with_colons[j] = time_string_without_colons[i];

   time_string_with_colons[j++] = ':';

   /* then, the minutes */
   for (i = 10; i < 12 ; i++, j++) 
      time_string_with_colons[j] = time_string_without_colons[i];

   time_string_with_colons[j++] = ':';

   /* then, the seconds */
   for (i = 12; i < 14 ; i++, j++) 
      time_string_with_colons[j] = time_string_without_colons[i];

   time_string_with_colons[j++] = NULL;

   /* then, the milliseconds 
   for (i = 14; i < 16 ; i++) 
      time_string_with_colons[j] = time_string_without_colons[i];
    
   time_string_with_colons[23] = NULL;
   */
#endif
}

/* 
 * This function copies non-space characters into
 * a buffer, and null-terminates the fresh buffer.
 */

void
null_terminate_properly(untrue_string, final_string, string_max_length)
   char  untrue_string[];
   char  *final_string;
   int   string_max_length;
{
   int   i = 0, 
         increment_counter = 0;

   for (i = 0; i < string_max_length; i++) {
      if (untrue_string[i] != ' ') {
	 /* If the character isn't a space, copy it into the final string */
         final_string[increment_counter++] = untrue_string[i];
      }
   }

   /* Now, null-terminate the final string properly */
   final_string[increment_counter] = NULL;

}

/* This function figures out what the width and height
   of the image will be when viewed. */

void
get_visible_width_and_height (picture_width, picture_height)
int *picture_width, *picture_height;
{

  /* If we're zooming in on a portion of a larger image */
  if ((_image_scale_factor == -1)  && 
      (_image_size_y > SCALED_IMAGE_MAX_HEIGHT)) {
 
    int multiplier = figure_out_scale_factor();
 
      /* If we're zooming in on a portion of a complex image */
    if ((_image_type == COMPLEX) && 
	((_x_zoom_offset >= 0) || (_y_zoom_offset >= 0))) {
      (*picture_width)  = ZOOM_IN_WIDTH;
      (*picture_height) = ZOOM_IN_WIDTH;
    }
    else {
      (*picture_width)  = _zoom_box_width  * multiplier;
      (*picture_height) = _zoom_box_height * multiplier;
    }
  }
  else {
    /* 
     * If we're looking at a picture which hasn't been scaled, 
     * or if we're looking at a zoomed-out picture.
     */
    (*picture_width)  = _image_size_x / abs(_image_scale_factor);
    (*picture_height) = _image_size_y / abs(_image_scale_factor);
  }

}

void
reverse_picture (picture_only_byte_data)
byte *picture_only_byte_data;
{

  int picture_width, picture_height, i, j, midpoint;
  byte tmp_byte;

  get_visible_width_and_height (&picture_width,
				&picture_height);

  midpoint = picture_width / 2;

  for (i = 0; i < picture_height; i++) {
    for (j = 0; j < midpoint; j++) {
      tmp_byte = picture_only_byte_data[(i * picture_width) + j];

      picture_only_byte_data[(i * picture_width) + j] =
	picture_only_byte_data[(i * picture_width) + (picture_width -j)];

      picture_only_byte_data[(i * picture_width) + (picture_width - j)] =
	tmp_byte;
    }
  }
}

/* A function to hold all the "cleaning up" that needs to be done. */

void
clean_up()
{
  /*
  if (_number_of_histogram_records <= 3) {
    XFreeGC(_display_pointer, _data_histogram_gc);
    XFreeGC(_display_pointer, _i_histogram_gc);
    XFreeGC(_display_pointer, _q_histogram_gc);
  }
  else if (_number_of_histogram_records <= 2) {
    XFreeGC(_display_pointer, _data_histogram_gc);
    XFreeGC(_display_pointer, _i_histogram_gc);
  }
  else if (_number_of_histogram_records == 1) {
    XFreeGC(_display_pointer, _data_histogram_gc);
  }
  */
  XFreeGC(_display_pointer, _pixmap_gc);
  closelog();
}

/* A wrapper around read, checking that the proper number
   of bytes were read. */

void
Read(file_descriptor, data, size, name, log_option, exit_option, exit_status)
int    file_descriptor;
byte  *data;
int    size;
char  *name;
int    log_option;
int    exit_option;
int    exit_status;
{

  int   number_of_bytes_read;
  char  read_announcement[200];

  number_of_bytes_read = read(file_descriptor, data, size);

  if (number_of_bytes_read != size) {
    sprintf (read_announcement,
	     "Cannot read %s (%d bytes)",
	     name, size);
    make_announcement (read_announcement, log_option);
    if (exit_option == DO_EXIT)
      exit (exit_status);
  }
}

void
set_up_log(program_name)
char *program_name;
{

   char *executable_name = strrchr(program_name, '/');
  
   if (executable_name == NULL)
      /* if there was no slash in the execution name... */
      executable_name = program_name;
   else
      /* if there was a slash, point to the first char after the slash */
      executable_name++;
   
   /* Set up _qc_log_prefix, which is used in prinfLLog() */
   sprintf (_qc_log_prefix, "(%s) ", executable_name);
   _qc_log_prefix[ strlen(executable_name) + 4] = NULL;
   
   openlog("CP", LOG_PID|LOG_CONS|LOG_NDELAY, LOG_CP);
}
