#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gfunctions_c[] = "@(#)gfunctions.c	1.4 96/04/10 20:14:24";

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

   /* Change the title string for the window */
   sprintf(title_string, "image_avg v1.0 (Image %s)", _filename);
 
   XtVaSetValues(_toplevel, XmNtitle, title_string, NULL);
 
   /* This is for the zooming... */
   _x_zoom_offset = _y_zoom_offset = -1;

   /* This is for the colormap problem */
   _background_colorcell = 0;

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
	 printfLLog(LOG_ERR, "ERROR:  %s", error_holding_string);
	 free(error_holding_string);

	 exit(QC_IMPROPER_CEOS_VALUE);
      }
   }
  
   if (_verbose_option == VERBOSE) {
      char *announcement_string = (char *) malloc(200 * sizeof(char));
      sprintf(announcement_string, "%s = %d", value_name, return_int);
      printfLLog(LOG_DEBUG, "%s", announcement_string);
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

   int  i;

   /* first, the year: */
   for (i = 0; i < 4 ; i++) 
      time_string_with_colons[i] = time_string_without_colons[i];

   time_string_with_colons[4] =  ':';
  
   /* then, the month */
   for (i = 4; i < 6 ; i++)
      time_string_with_colons[i+1] = time_string_without_colons[i];

   time_string_with_colons[7] = ':';

   /* then, the day */
   for (i = 6; i < 8; i++) 
      time_string_with_colons[i+2] = time_string_without_colons[i];

   time_string_with_colons[10] = ':';

   /* then, the hours */
   for (i = 8; i < 10 ; i++) 
      time_string_with_colons[i+3] = time_string_without_colons[i];

   time_string_with_colons[13] = ':';

   /* then, the minutes */
   for (i = 10; i < 12 ; i++) 
      time_string_with_colons[i+4] = time_string_without_colons[i];

   time_string_with_colons[16] = ':';

   /* then, the seconds */
   for (i = 12; i < 14 ; i++) 
      time_string_with_colons[i+5] = time_string_without_colons[i];

   time_string_with_colons[19] = ':';

   /* then, the milliseconds */
   for (i = 14; i < 17 ; i++) 
      time_string_with_colons[i+6] = time_string_without_colons[i];
    
   time_string_with_colons[23] = NULL;
}
