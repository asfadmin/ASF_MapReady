/* setting up the bottom info widget*/

static char sccsid_qinfo_widgets_c[] = "@(#)qinfo_widgets.c	1.21 97/06/03 11:02:33";

#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

/* 
 * This function sets up the major info scrolled list, which sits
 * on the bottom of the app & is used for announcements 
 */

Widget
initialize_major_info_widget(parent_form)
   Widget parent_form;
{
 
   char *initialization_string = (char *) malloc(200 * sizeof(char));
 
   _major_info_frame = XtVaCreateWidget("major_info_frame",
                          xmFrameWidgetClass,  parent_form,
                          XmNshadowType,       XmSHADOW_ETCHED_OUT,
                          XmNleftAttachment,   XmATTACH_FORM,
                          XmNrightAttachment,  XmATTACH_WIDGET,
                          XmNrightWidget,      _right_outer_frame,
                          XmNbottomAttachment, XmATTACH_FORM,
                          NULL);
 
   major_info_scrolled_list =  XmCreateScrolledList(_major_info_frame, 
 				    "major_info_scrolled_list", NULL, 0);
 
   sprintf(initialization_string, 
 	   "Quality Control application running...");
 
   XtVaSetValues(major_info_scrolled_list, 
 		 XmNheight,                   41,
 		 XtVaTypedArg, XmNitems, XmRString,
 		 initialization_string,    strlen(initialization_string),
 		 XmNitemCount,                1,
 		 XmNvisibleItemCount,         2,
 		 NULL);
 
   free(initialization_string);
   XtManageChild(major_info_scrolled_list);
   return _major_info_frame;
 
}

/* This sets up the metadata labels */

void
initialize_info_label_values()
{
 
   char  *low_stretch_string, *high_stretch_string,
         *load_string, *image_size_x_string,
         *image_size_y_string, *psf_string,
         *type_string;
 
   int   format_length;

   XmString generic_string;
 

   if (! strncmp (_format_type, "UNSIGNED INTEGER1", 17))
     /* Annoying little exception here, */
     format_length = 16 ;
   else
     /* Every normal case, get the number of characters before the
        asterisk */
     format_length = strcspn(_format_type, "*");


   /* allocate the proper number of bytes */
   type_string = (char *) malloc((format_length + 1) * sizeof(char));
 
   /* copy the proper number of bytes from _format_type into type_string */
   strncpy(type_string, _format_type, format_length);
   type_string[format_length] = NULL;
 
   /* initialize the "type" to the data type */
   generic_string = XmStringCreateLocalized(type_string);
   XtVaSetValues(type_value_label, XmNlabelString,  
                 generic_string, NULL);

   XmStringFree (generic_string);
   free(type_string);
   free(_format_type);
 
   /* Initialize the major label with the input file name */
   load_string = (char *) malloc(100 * sizeof(char));
   sprintf(load_string, "Loaded \"%s\"", _filename); 
   make_announcement(load_string, NO_LOG); 
   free(load_string);
 
   /* Make an announcement if the image was scaled down */
   if (_image_scale_factor != 1) {
      char *scale_announcement = (char *) malloc(200 * sizeof(char));
      sprintf(scale_announcement, 
              "The current image was scaled down from %dx%d to %dx%d",
              _image_size_x, _image_size_y,
              (_image_size_x / _image_scale_factor), 
              (_image_size_y / _image_scale_factor));
 
      make_announcement(scale_announcement, NO_LOG);
      free(scale_announcement);
   }
 
   /* Initialize the _high_pixel_stretch_value label */
   high_stretch_string = (char *) malloc(3 * sizeof(int));
   sprintf(high_stretch_string, "%3d", _high_pixel_stretch_value);
   XmTextSetString(_high_stretch_value_text, high_stretch_string);
   free(high_stretch_string);
 
   /* Initialize the _low_pixel_stretch_value label */
   low_stretch_string  = (char *) malloc(3 * sizeof(int));
   sprintf(low_stretch_string, "%3d", _low_pixel_stretch_value);
   XmTextSetString(_low_stretch_value_text, low_stretch_string);
   free(low_stretch_string);
   
   /* Initialize the x and y size labels */
   image_size_x_string = (char *) malloc(5 * sizeof(int));
   sprintf(image_size_x_string, "%5d", _image_size_x);
   generic_string = XmStringCreateLocalized(image_size_x_string);
   XtVaSetValues(image_size_x_value_label, XmNlabelString, 
 		generic_string, NULL);
   free(image_size_x_string);
   XmStringFree(generic_string);
 
   image_size_y_string = (char *) malloc(5 * sizeof(int));
   sprintf(image_size_y_string, "%5d", _image_size_y);
   generic_string = XmStringCreateLocalized(image_size_y_string);
   XtVaSetValues(image_size_y_value_label, XmNlabelString, 
 		generic_string, NULL);
   free(image_size_y_string);
   XmStringFree(generic_string);
 
   /* set up pixel scale factor label */
   psf_string = (char *) malloc(3 * sizeof(int));
   sprintf(psf_string, "%3d", 
          (_image_scale_factor==-1 ? 1 : _image_scale_factor));
   XmTextSetString(_psf_value_text, psf_string);
   free(psf_string);
}


/* This sets up values labels that are directly 
   related to the histogram */

void
set_up_histogram_dependant_labels()
{
   char  *histogram_max_string, *histogram_max_location_string, 
         *histogram_min_string, *histogram_min_location_string,
         *max_pixel_string, *min_pixel_string,
         *zpix_string, *satp_string,
         *mean_string, *standard_dev_string;
 
   /* 
    * If the image is full rez, then the histogram labels are 
    * dynamic.  Otherwise, they're taken from the leader file. 
    */
#if 0
   if (_image_type != FULL_REZ)
      return;
#endif
   /* set up the data min and data max pixel labels ... */
   max_pixel_string = (char *) malloc(3 * sizeof(int));
   sprintf(max_pixel_string, "%7d", _high_pixel_stretch_value);
   XmTextSetString(_dmax_value_text, max_pixel_string);
   free(max_pixel_string);
 
   min_pixel_string = (char *) malloc(3 * sizeof(int));
   sprintf(min_pixel_string, "%7d", _low_pixel_stretch_value);
   XmTextSetString(_dmin_value_text, min_pixel_string);
   free(min_pixel_string);
 
   /* set up the histogram min and histogram max pixel labels ... */
   histogram_max_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_max_string, "%8d", _histogram_max);
   XmTextSetString(_hmax_value_text, histogram_max_string);
   free(histogram_max_string);
 
   histogram_min_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_min_string, "%7d", _histogram_min);
   XmTextSetString(_hmin_value_text, histogram_min_string);
   free(histogram_min_string);
 
   /* and their locations... */
   histogram_max_location_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_max_location_string, "%7d", _histogram_max_location);
   XmTextSetString(_atmax_value_text, histogram_max_location_string);
   free(histogram_max_location_string);

   histogram_min_location_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_min_location_string, "%7d", _histogram_min_location);
   XmTextSetString(_atmin_value_text, histogram_min_location_string);
   free(histogram_min_location_string);
 
   /* And set up the mean and sdev.... */
   mean_string = (char *) malloc(200 * sizeof(float));
   if (_is_sys_ramp ==1 && _is_sys_pp == 1)
   sprintf(mean_string, "%7.1f", _data_mean/256);
   else
   sprintf(mean_string, "%7.1f", _data_mean);
   XmTextSetString(_mean_value_text, mean_string);
   free(mean_string);
 
   standard_dev_string = (char *) malloc(200 * sizeof(float));
   sprintf(standard_dev_string, "%7.1f", _data_sdev);
   XmTextSetString(_sdev_value_text, standard_dev_string);
   free(standard_dev_string);
 
   /* Set up the zpix and satp values...*/
   zpix_string = (char *) malloc(200 * sizeof(int));
   sprintf(zpix_string, "%7d", _processed_histogram_array[0]);
   XmTextSetString(_zpix_value_text, zpix_string);
   free(zpix_string);
 
   satp_string = (char *) malloc(200 * sizeof(int));
   sprintf(satp_string, "%7d", _processed_histogram_array[255]);
   XmTextSetString(_satp_value_text, satp_string);
   free(satp_string);

   if (_number_of_histogram_records == 1)
      return;

   /* set up the data min and data max pixel labels ... */
   max_pixel_string = (char *) malloc(3 * sizeof(int));
   sprintf(max_pixel_string, "%7d", _i_high_pixel_stretch_value);
   XmTextSetString(_i_dmax_value_text, max_pixel_string);
   free(max_pixel_string);
 
   min_pixel_string = (char *) malloc(3 * sizeof(int));
   sprintf(min_pixel_string, "%7d", _i_low_pixel_stretch_value);
   XmTextSetString(_i_dmin_value_text, min_pixel_string);
   free(min_pixel_string);
 
   /* set up the histogram min and histogram max pixel labels ... */
   histogram_max_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_max_string, "%8d", _i_histogram_max);
   XmTextSetString(_i_hmax_value_text, histogram_max_string);
   free(histogram_max_string);
 
   histogram_min_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_min_string, "%7d", _i_histogram_min);
   XmTextSetString(_i_hmin_value_text, histogram_min_string);
   free(histogram_min_string);
 
   /* and their locations... */
   histogram_max_location_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_max_location_string, "%7d", _i_histogram_max_location);
   XmTextSetString(_i_atmax_value_text, histogram_max_location_string);
   free(histogram_max_location_string);

   histogram_min_location_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_min_location_string, "%7d", _i_histogram_min_location);
   XmTextSetString(_i_atmin_value_text, histogram_min_location_string);
   free(histogram_min_location_string);
 
   /* And set up the mean and sdev.... */
   mean_string = (char *) malloc(200 * sizeof(float));
   sprintf(mean_string, "%7.1f", _i_data_mean);
   XmTextSetString(_i_mean_value_text, mean_string);
   free(mean_string);
 
   standard_dev_string = (char *) malloc(200 * sizeof(float));
   sprintf(standard_dev_string, "%7.1f", _i_data_sdev);
   XmTextSetString(_i_sdev_value_text, standard_dev_string);
   free(standard_dev_string);
 
   /* Set up the zpix and satp values...*/
   zpix_string = (char *) malloc(200 * sizeof(int));
   sprintf(zpix_string, "%7d", _i_histogram_array[0]);
   XmTextSetString(_i_zpix_value_text, zpix_string);
   free(zpix_string);
 
   satp_string = (char *) malloc(200 * sizeof(int));
   sprintf(satp_string, "%7d", _i_histogram_array[_i_high_pixel_stretch_value - _i_low_pixel_stretch_value]);
   XmTextSetString(_i_satp_value_text, satp_string);
   free(satp_string);

   /* set up the data min and data max pixel labels ... */
   max_pixel_string = (char *) malloc(3 * sizeof(int));
   sprintf(max_pixel_string, "%7d", _q_high_pixel_stretch_value);
   XmTextSetString(_q_dmax_value_text, max_pixel_string);
   free(max_pixel_string);
 
   min_pixel_string = (char *) malloc(3 * sizeof(int));
   sprintf(min_pixel_string, "%7d", _q_low_pixel_stretch_value);
   XmTextSetString(_q_dmin_value_text, min_pixel_string);
   free(min_pixel_string);
 
   /* set up the histogram min and histogram max pixel labels ... */
   histogram_max_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_max_string, "%8d", _q_histogram_max);
   XmTextSetString(_q_hmax_value_text, histogram_max_string);
   free(histogram_max_string);
 
   histogram_min_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_min_string, "%7d", _q_histogram_min);
   XmTextSetString(_q_hmin_value_text, histogram_min_string);
   free(histogram_min_string);
 
   /* and their locations... */
   histogram_max_location_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_max_location_string, "%7d", _q_histogram_max_location);
   XmTextSetString(_q_atmax_value_text, histogram_max_location_string);
   free(histogram_max_location_string);

   histogram_min_location_string = (char *) malloc(200 * sizeof(int));
   sprintf(histogram_min_location_string, "%7d", _q_histogram_min_location);
   XmTextSetString(_q_atmin_value_text, histogram_min_location_string);
   free(histogram_min_location_string);
 
   /* And set up the mean and sdev.... */
   mean_string = (char *) malloc(200 * sizeof(float));
   sprintf(mean_string, "%7.1f", _q_data_mean);
   XmTextSetString(_q_mean_value_text, mean_string);
   free(mean_string);
 
   standard_dev_string = (char *) malloc(200 * sizeof(float));
   sprintf(standard_dev_string, "%7.1f", _q_data_sdev);
   XmTextSetString(_q_sdev_value_text, standard_dev_string);
   free(standard_dev_string);
 
   /* Set up the zpix and satp values...*/
   zpix_string = (char *) malloc(200 * sizeof(int));
   sprintf(zpix_string, "%7d", _q_histogram_array[0]);
   XmTextSetString(_q_zpix_value_text, zpix_string);
   free(zpix_string);
 
   satp_string = (char *) malloc(200 * sizeof(int));
   sprintf(satp_string, "%7d", _q_histogram_array[_q_high_pixel_stretch_value - _q_low_pixel_stretch_value]);
   XmTextSetString(_q_satp_value_text, satp_string);
   free(satp_string);
}
