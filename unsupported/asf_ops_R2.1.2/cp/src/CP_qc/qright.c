/*
*   Right hand side of application :
*      The layout is a scrolled window, which contains a frame,
*      which contains a form.  All the pieces of information
*      are either managed by the right_form or one of its children.
*/

static char sccsid_qright_c[] = "@(#)qright.c	1.21 97/07/14 15:17:20";

#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

/* This sets up the layout, and adds callbacks for the right side of
   the app */

Widget
set_up_right(parent_form)
   Widget parent_form;
{
 
   XGCValues        gc_values;
 
   Widget           scrolled_right, right_form, 
                    generic_widget, 
 
                    graph_frame, graph_major_form, 
                    graph_bottom_label, 
 
                    i_graph_frame, i_graph_major_form, 
                    i_graph_bottom_label, 
 
                    q_graph_frame, q_graph_major_form, 
                    q_graph_bottom_label, 
 
                    big_text_frame, big_text_form, 
                    small_text_frame, small_text_form,
                    i_text_frame, i_text_form,
                    q_text_frame, q_text_form,
 
                    button_frame, button_form, 
 
                    psf_label, is_label, by_label, 
 
                    /* from _filename */
                    scene_id_label,
                    scene_id_value_label,
 
                    lotsa_stuff_label,
                    lotsa_stuff_value_label,
 
                    type_label, 
 
                    order_id_label, 
                    order_id_value_label,
 
                    pixel_spacing_label, 
                    pixel_spacing_value_label,
 
                    projection_label, 
                    projection_value_label,

                    /* from leader file */
                    latitude_label, latitude_value_label,
                    longitude_label, longitude_value_label,
                    time_label, time_value_label,
 
                    snr_label, snr_value_label,
                    ber_label, ber_value_label,
                    
                    /* from the ceos file */
                    number_of_records_label, 
                    number_of_records_value_label,
 
                    samples_per_record_label, 
                    samples_per_record_value_label,
 
                    high_stretch_frame, high_stretch_form,
                    low_stretch_frame, low_stretch_form,
   
                    zoom_form, zoom_frame;
 
   XmString         generic_string;
 
   int              right_side_width, scroll_bar_width;
 
   char             *number_of_records_string,
                    *samples_per_record_string,
                    *satellite_rev_string, 
                    *snr_string, *ber_string, 
                    *pixel_spacing_string,
                    *projection_string,
                    scene_id_string[333],
                    *lotsa_stuff_string;
 
   /* for the fonts */
   XmFontList       fontlist;
   XmFontListEntry  entry;
   XFontStruct      *font;
 
   /* 
    * Create a fontlist based on the font we're using.  These are the
    * fonts that are going to be hardcoded in the Label and List widgets.
    */
   font = XLoadQueryFont(_display_pointer, "default");
   entry = XmFontListEntryCreate("tag2", XmFONT_IS_FONT, font);
   fontlist = XmFontListAppendEntry(NULL, entry);
   /*  XtFree(entry);     /* this causes a warning... but why? */
 
   right_side_width = 290;  
   scroll_bar_width = 37;
 
   scrolled_right = XtVaCreateWidget("scrolled_window",
 	 xmScrolledWindowWidgetClass, parent_form,
         XmNscrollBarDisplayPolicy,   XmAS_NEEDED,
         XmNscrollingPolicy,          XmAUTOMATIC,
         XmNrightAttachment,          XmATTACH_FORM,
         XmNtopAttachment,            XmATTACH_WIDGET,
         XmNtopWidget,                _menubar,
         XmNbottomAttachment,         XmATTACH_WIDGET,
         XmNbottomWidget,             _major_info_frame,
         XmNwidth,                    right_side_width,
         XmNminWidth,                 right_side_width,
         XmNmaxWidth,                 right_side_width,
         NULL);
 
   _right_outer_frame = XtVaCreateManagedWidget("right_frame",
 		       xmFrameWidgetClass,     scrolled_right,
 		       XmNshadowType,          XmSHADOW_ETCHED_OUT,
 		       NULL);
   
   right_form = XtVaCreateManagedWidget("right_form",
 			 xmFormWidgetClass, _right_outer_frame,
                         XmNwidth,          right_side_width - scroll_bar_width,
                         XmNmaxWidth,       right_side_width - scroll_bar_width,
 			 NULL);
 
   /* 
    * UPPER RIGHT CORNER TEXT INFORMATION 
    * 
    * big stuff:  
    */
   big_text_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
 
   big_text_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,    big_text_frame,
         XmNfractionBase,      150,
         XmNheight,            345,
         XmNmaxHeight,         345,
         XmNminHeight,         345,
         NULL);
 
   /* The scene id and its associated value label */
   generic_string = XmStringCreateLocalized("Scene ID:");
   scene_id_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   10,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNalignment,        XmALIGNMENT_CENTER, 
         NULL);
   XmStringFree(generic_string);
 
   sprintf(scene_id_string, "%s", _scene_id_string);
 
   /* this value comes off the command line, as the -P option. */
   generic_string = XmStringCreateLocalized(scene_id_string);
   scene_id_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   10,
         XmNrightAttachment,  XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   /* The production request id: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Platform/Rev:");
   lotsa_stuff_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      11,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   20,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNalignment,        XmALIGNMENT_CENTER, 
         NULL);
   XmStringFree(generic_string);
 
   lotsa_stuff_string = (char *) malloc(333 * sizeof(char));
   sprintf(lotsa_stuff_string, "%s / %s",
 	   _satellite_string, _revolution_string);
 
   /* this value comes off the command line, as the -P option. */
   generic_string = XmStringCreateLocalized(lotsa_stuff_string);
   lotsa_stuff_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      11,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   20,
         XmNrightAttachment,  XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);

   free(_scene_id_string);
   free(_satellite_string);
   free(_revolution_string);
   free(_sequence_number_string);
   free(_frame_string);
   free(lotsa_stuff_string);
 
   /* The ORDER ID: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Job ID:"); 
   order_id_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      21,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   30,
         NULL);
   XmStringFree(generic_string);
 
   /* this value comes off the command line, as the -job option */
   generic_string = XmStringCreateLocalized(_order_id_string);
   order_id_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      21,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   30,
         NULL);
   XmStringFree(generic_string);
 
   /* The Type: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Data Type:"); 
   type_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      31,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   40,
         XmNleftAttachment,   XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   type_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      31,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   40,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       type_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* the Latitude: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Center Lat (deg):"); 
   latitude_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      41,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   50,
         XmNleftAttachment,   XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in read_leader_file() */
   generic_string = XmStringCreateLocalized(_latitude_string);
   latitude_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      41,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   50,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       latitude_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   XmStringFree(generic_string);
 
   /* the Longitude: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Center Lon (deg):"); 
   longitude_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      51,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   60,
         XmNleftAttachment,   XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in read_leader_file() */
   generic_string = XmStringCreateLocalized(_longitude_string);
   longitude_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      51,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   60,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       longitude_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   XmStringFree(generic_string);
 
   /* the Time: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Time :"); 
   time_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      61,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   70,
         XmNleftAttachment,   XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in read_leader_file() */
   generic_string = XmStringCreateLocalized(_time_string);
   time_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,         
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      61,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   70,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       time_label,
         XmNalignment,        XmALIGNMENT_END,
   /*    XmNrecomputeSize,    True,   /* make it fit */
         NULL);
   XmStringFree(generic_string);

   free(_latitude_string);
   free(_longitude_string);
   free(_time_string);
 
   /* the Number of Records: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Number of Records:"); 
   number_of_records_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      71,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   80,
         XmNleftAttachment,   XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   /* ask joanne what this is supposed to be */
   number_of_records_string = (char *) malloc(200 * sizeof(char));
   sprintf(number_of_records_string, "%d", _records_count);
 
   generic_string = XmStringCreateLocalized(number_of_records_string);
   number_of_records_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      71,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   80,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       number_of_records_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   XmStringFree(generic_string); 
   free(number_of_records_string);
 
   /* the Valid samples/record: label, and its associated value label */
   generic_string = XmStringCreateLocalized("Valid samples / record:"); 
   samples_per_record_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
   /*    XmNfontList,         fontlist,  */
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      81,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   90,
         XmNleftAttachment,   XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);
 
   samples_per_record_string = (char *) malloc(200 * sizeof(char));
   sprintf(samples_per_record_string, "%d", _samples_per_group);
 
   generic_string = XmStringCreateLocalized(samples_per_record_string);
   samples_per_record_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      81,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   90,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       samples_per_record_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   XmStringFree(generic_string);
   free(samples_per_record_string);
 
   /* The pixel scale factor label, and its associated value label */
   generic_string = XmStringCreateLocalized("Pixel scale factor = "); 
   psf_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      91,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   100,
         XmNleftAttachment,   XmATTACH_FORM,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _psf_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      91,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   100,
         XmNrightAttachment,  XmATTACH_FORM,
   /*    XmNfontList,         fontlist,  */ 
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     122,
         XmNalignment,        XmALIGNMENT_END,
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         NULL);
     
   /* The image size label, and its associated X and Y value labels,
      with an "x" between them. */
   generic_string = XmStringCreateLocalized("Image size:"); 
   is_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      101,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   110,
         XmNlabelString,      generic_string,
         XmNleftAttachment,   XmATTACH_FORM,
   /*    XmNfontList,         fontlist, */
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   image_size_y_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      101,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   110,
         XmNrightAttachment,  XmATTACH_FORM,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   generic_string = XmStringCreateLocalized(" x "); 
   by_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      101,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   110,
         XmNrightAttachment,  XmATTACH_WIDGET,
         XmNrightWidget,      image_size_y_value_label,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   image_size_x_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      101,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   110,
         XmNrightAttachment,  XmATTACH_WIDGET,
         XmNrightWidget,      by_label,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
 
   /* The signal-to-noise-ratio stuff */
   generic_string = XmStringCreateLocalized("Estimated SNR (dB):"); 
   snr_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      111,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   120,
         XmNlabelString,      generic_string,
         XmNleftAttachment,   XmATTACH_FORM,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   snr_string = (char *) malloc(200 * sizeof(char));
   sprintf(snr_string, "%16.7f", _snr);
 
   generic_string = XmStringCreateLocalized(snr_string);
   snr_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      111,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   120,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       snr_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   free(snr_string);
   XmStringFree(generic_string);

   /* The bit error rate stuff */
   generic_string = XmStringCreateLocalized("Actual BER:"); 
   ber_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      121,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   130,
         XmNlabelString,      generic_string,
         XmNleftAttachment,   XmATTACH_FORM,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   ber_string = (char *) malloc(200 * sizeof(char));
   sprintf(ber_string, "%16.7E", _ber);
 
   generic_string = XmStringCreateLocalized(ber_string);
   ber_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      121,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   130,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       ber_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   free(ber_string);
   XmStringFree (generic_string);
 
   /* The pixel spacing stuff */
   generic_string = XmStringCreateLocalized("Spacing Pixel/Line (m):"); 
   pixel_spacing_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      131,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   140,
         XmNlabelString,      generic_string,
         XmNleftAttachment,   XmATTACH_FORM,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   pixel_spacing_string = (char *) malloc(200 * sizeof(char));
   sprintf(pixel_spacing_string, "%.1f/%.1f", _pixel_spacing, _line_spacing);
 
   generic_string = XmStringCreateLocalized(pixel_spacing_string);
   pixel_spacing_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      131,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   140,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       pixel_spacing_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   free(pixel_spacing_string);
   XmStringFree(generic_string);
 
   /* The projection stuff */
   generic_string = XmStringCreateLocalized("Projection:"); 
   projection_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      141,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   150,
         XmNlabelString,      generic_string,
         XmNleftAttachment,   XmATTACH_FORM,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   projection_string = (char *) malloc(200 * sizeof(char));
   sprintf(projection_string, "%s", _projection_string);
 
   generic_string = XmStringCreateLocalized(projection_string);
   projection_value_label = XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  big_text_form,
         XmNlabelString,      generic_string,
   /*    XmNfontList,         fontlist,  */
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      141,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   150,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_WIDGET,
         XmNleftWidget,       projection_label,
         XmNalignment,        XmALIGNMENT_END,
         NULL);
   free(projection_string);
   XmStringFree(generic_string);

   if (_number_of_histogram_records < 1) goto high_stretch_start;

   /* SMALLER STUFF: the info of the processed data */
   small_text_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           big_text_frame,
         XmNtopOffset,           2,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   small_text_form = XtVaCreateManagedWidget("text_form",
         xmFormWidgetClass,    small_text_frame,
         XmNheight,            115,
         XmNmaxHeight,         115,
         XmNminHeight,         115,
         XmNfractionBase,      50,
         NULL);
 
   /* The DMIN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("DMin="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _dmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The DMAX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("DMax="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _dmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
 
   /* The MEAN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("Mean="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _mean_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The SDEV= label, and its associated value label */
   generic_string = XmStringCreateLocalized("SDev="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _sdev_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The ZPIX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("ZPix="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   _zpix_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The SATP= label, and its associated value label */
   generic_string = XmStringCreateLocalized("SATP="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _satp_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The HMIN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("HMin="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _hmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* HMIN's associated at: label, and its associated value label */
   generic_string = XmStringCreateLocalized("at:"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _atmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,  small_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The HMAX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("HMax="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _hmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   small_text_form,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNbottomPosition,   49,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* HMAX's associated at: label, and its associated value label */
   generic_string = XmStringCreateLocalized("at:"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  small_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   _atmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,  small_text_form,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNshadowThickness,  0,  
         XmNrightAttachment,  XmATTACH_FORM,
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* END of the labels and their dynamic values */
 
   /* set up the graph */
   graph_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           small_text_frame,
         XmNtopOffset,           2,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
 
   graph_major_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,    graph_frame,
         XmNheight,            200,
         XmNmaxHeight,         200,
         XmNminHeight,         200,
         NULL);
 
   generic_string = XmStringCreateLocalized(_histogram_description);
   graph_bottom_label = XtVaCreateManagedWidget("info_label",
         xmLabelWidgetClass,   graph_major_form,
         XmNlabelString,       generic_string,
         XmNrightAttachment,   XmATTACH_FORM,
         XmNbottomAttachment,  XmATTACH_FORM,
         XmNleftAttachment,    XmATTACH_FORM,
         NULL);
   XmStringFree (generic_string);
 
   _data_graph_drawing_area = XtVaCreateManagedWidget("right_form",
         xmDrawingAreaWidgetClass,    graph_major_form,
         XmNbottomAttachment,  XmATTACH_WIDGET,
         XmNbottomWidget,      graph_bottom_label,
         XmNrightAttachment,   XmATTACH_FORM,
         XmNtopAttachment,     XmATTACH_FORM,
         XmNleftAttachment,    XmATTACH_FORM,
         NULL);
 
   /* set up the graphics context for the drawing area... */
   gc_values.foreground = BlackPixelOfScreen(XtScreen (_data_graph_drawing_area));
   _data_histogram_gc = XCreateGC(_display_pointer,
				  RootWindowOfScreen 
				  (XtScreen(_data_graph_drawing_area)),
				  GCForeground,
				  &gc_values);
 
   XtAddCallback(_data_graph_drawing_area, XmNexposeCallback, 
 		 draw_processed_histogram, (XtPointer) _data_graph_drawing_area);
 
   /* Set up the "stretching" */
high_stretch_start:
   if (_number_of_histogram_records < 1) {
   /*  Hi stretching */
   high_stretch_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           big_text_frame,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   }
   else {
   /*  Hi stretching */
   high_stretch_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           graph_frame, 
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   }
 
   high_stretch_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,      high_stretch_frame,
         XmNfractionBase,        6,
         XmNheight,              45,
         NULL);
 
   generic_string = XmStringCreateLocalized("High stretch"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,        high_stretch_frame,
         XmNlabelString,            generic_string,
         XmNchildType,              XmFRAME_TITLE_CHILD,
         XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
   /*    XmNfontList,               fontlist, */
         XmNalignment,              XmALIGNMENT_CENTER,
         NULL);
   XmStringFree(generic_string);
 
   _high_fast_down_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   high_stretch_form,
         XmNleftAttachment,          XmATTACH_FORM,
         XmNtopAttachment,           XmATTACH_FORM,
         XmNbottomAttachment,        XmATTACH_FORM,
         XmNrightAttachment,         XmATTACH_POSITION,
         XmNrightPosition,           1,
         XmNarrowDirection,          XmARROW_DOWN,
         NULL);
 
   XtAddCallback(_high_fast_down_pb, XmNactivateCallback, 
         high_fast_down_cb, NULL);
 
   _high_down_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   high_stretch_form,
         XmNleftAttachment,          XmATTACH_WIDGET,
         XmNleftWidget,              _high_fast_down_pb,
         XmNtopAttachment,           XmATTACH_POSITION,
         XmNtopPosition,             1,
         XmNbottomAttachment,        XmATTACH_POSITION,
         XmNbottomPosition,          5,
         XmNrightAttachment,         XmATTACH_POSITION,
         XmNrightPosition,           2,
         XmNarrowDirection,          XmARROW_DOWN,
         NULL);
 
   XtAddCallback(_high_down_pb, XmNactivateCallback, 
 	 high_down_cb, NULL);
 
   _high_fast_up_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   high_stretch_form,
         XmNrightAttachment,         XmATTACH_FORM,
         XmNtopAttachment,           XmATTACH_FORM,
         XmNbottomAttachment,        XmATTACH_FORM,
         XmNleftAttachment,          XmATTACH_POSITION,
         XmNleftPosition,            5,
         XmNarrowDirection,          XmARROW_UP,
         NULL);
 
   XtAddCallback(_high_fast_up_pb, XmNactivateCallback, 
 	 high_fast_up_cb, NULL);
 
   _high_up_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   high_stretch_form,
         XmNrightAttachment,         XmATTACH_WIDGET,
         XmNrightWidget,             _high_fast_up_pb,
         XmNtopAttachment,           XmATTACH_POSITION,
         XmNtopPosition,             1,
         XmNbottomAttachment,        XmATTACH_POSITION,
         XmNbottomPosition,          5,
         XmNleftAttachment,          XmATTACH_POSITION,
         XmNleftPosition,            4,
         XmNarrowDirection,          XmARROW_UP,
         NULL);
 
   XtAddCallback(_high_up_pb, XmNactivateCallback, 
         high_up_cb, NULL);
 
   /* this value is set up in set_up_info_label_values() */
   _high_stretch_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   high_stretch_form,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     2,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    4,
         XmNbottomAttachment, XmATTACH_FORM,
         XmNbottomOffset,     4,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNtopOffset,        5,
         XmNalignment,        XmALIGNMENT_CENTER,
   /*    XmNfontList,         fontlist,*/
         NULL);
 
   XtAddCallback(_high_stretch_value_text, XmNactivateCallback,
 	 high_stretch_text_change_cb, _high_stretch_value_text );
 
   /*  Low stretching */
   low_stretch_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           high_stretch_frame, 
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
 
   low_stretch_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,      low_stretch_frame,
         XmNfractionBase,        6,
         XmNheight,              45,
         NULL);
 
   generic_string = XmStringCreateLocalized("Low stretch"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,        low_stretch_frame,
         XmNlabelString,            generic_string,
         XmNchildType,              XmFRAME_TITLE_CHILD,
         XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
   /*    XmNfontList,               fontlist, */
         XmNalignment,              XmALIGNMENT_CENTER,
         NULL);
   XmStringFree(generic_string);
 
   _low_fast_down_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   low_stretch_form,
         XmNleftAttachment,          XmATTACH_FORM,
         XmNtopAttachment,           XmATTACH_FORM,
         XmNbottomAttachment,        XmATTACH_FORM,
         XmNrightAttachment,         XmATTACH_POSITION,
         XmNrightPosition,           1,
         XmNarrowDirection,          XmARROW_DOWN,
         NULL);
 
   XtAddCallback(_low_fast_down_pb, XmNactivateCallback, 
         low_fast_down_cb, NULL);
 
   _low_down_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   low_stretch_form,
         XmNleftAttachment,          XmATTACH_WIDGET,
         XmNleftWidget,              _low_fast_down_pb,
         XmNtopAttachment,           XmATTACH_POSITION,
         XmNtopPosition,             1,
         XmNbottomAttachment,        XmATTACH_POSITION,
         XmNbottomPosition,          5,
         XmNrightAttachment,         XmATTACH_POSITION,
         XmNrightPosition,           2,
         XmNarrowDirection,          XmARROW_DOWN,
         NULL);
 
   XtAddCallback(_low_down_pb, XmNactivateCallback, low_down_cb, NULL);
 
   _low_fast_up_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   low_stretch_form,
         XmNrightAttachment,         XmATTACH_FORM,
         XmNtopAttachment,           XmATTACH_FORM,
         XmNbottomAttachment,        XmATTACH_FORM,
         XmNleftAttachment,          XmATTACH_POSITION,
         XmNleftPosition,            5,
         XmNarrowDirection,          XmARROW_UP,
         NULL);
 
   XtAddCallback(_low_fast_up_pb, XmNactivateCallback, 
         low_fast_up_cb, NULL);
 
   _low_up_pb = XtVaCreateManagedWidget("right_button",
         xmArrowButtonWidgetClass,   low_stretch_form,
         XmNrightAttachment,         XmATTACH_WIDGET,
         XmNrightWidget,             _low_fast_up_pb,
         XmNtopAttachment,           XmATTACH_POSITION,
         XmNtopPosition,             1,
         XmNbottomAttachment,        XmATTACH_POSITION,
         XmNbottomPosition,          5,
         XmNleftAttachment,          XmATTACH_POSITION,
         XmNleftPosition,            4,
         XmNarrowDirection,          XmARROW_UP,
         NULL);
 
   XtAddCallback(_low_up_pb, XmNactivateCallback, low_up_cb, NULL);
 
   _low_stretch_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   low_stretch_form,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     2,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    4,
         XmNbottomAttachment, XmATTACH_FORM,
         XmNbottomOffset,     4,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNtopOffset,        5,
         XmNalignment,        XmALIGNMENT_CENTER,
   /*    XmNfontList,         fontlist,*/
         NULL);
 
   XtAddCallback(_low_stretch_value_text, XmNactivateCallback,
         low_stretch_text_change_cb, _low_stretch_value_text );
 
   /* set up the sensitivity of the stretch buttons */
   set_stretch_button_sensitivity();  
   
   /* set up zooming stuff: */
   zoom_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           low_stretch_frame, 
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
 
   zoom_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,      zoom_frame,
         XmNfractionBase,        30,
         XmNheight,              45,
         XmNminHeight,           45,
         XmNmaxHeight,           45,
         NULL);
 
   generic_string = XmStringCreateLocalized("Zoom  in");
   _zoom_box_pb = XtVaCreateManagedWidget("right_button",
         xmPushButtonWidgetClass,    zoom_form,
         XmNleftAttachment,          XmATTACH_FORM,
         XmNrightAttachment,         XmATTACH_FORM,
         XmNtopAttachment,           XmATTACH_FORM,
         XmNbottomAttachment,        XmATTACH_FORM,
         XmNlabelString,             generic_string,
         XmNalignment,               XmALIGNMENT_CENTER,
         XmNfontList,                fontlist,
         NULL);
   XmStringFree (generic_string);

 
   XtAddCallback(_zoom_box_pb, XmNactivateCallback, zoom_box_cb, NULL);
 
   /* push buttons: */
   if (_number_of_histogram_records == 1) {
      button_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNbottomAttachment,    XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           zoom_frame,
         XmNtopOffset,           8,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   }
   else {
      button_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNleftAttachment,      XmATTACH_FORM,
      /* XmNbottomAttachment,    XmATTACH_FORM,    */
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           zoom_frame,
         XmNtopOffset,           8,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   }
 
   button_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,      button_frame,
         XmNfractionBase,        30,
         XmNminHeight,           45,
         XmNheight,              45,
         NULL);
 
   generic_string = XmStringCreateLocalized("Accept"); 
   _accept_pb = XtVaCreateManagedWidget("right_button",
         xmPushButtonWidgetClass, button_form,
         XmNlabelString,          generic_string,
         XmNleftAttachment,       XmATTACH_FORM,
         XmNrightAttachment,      XmATTACH_POSITION,
         XmNrightPosition,        10,
         XmNtopAttachment,        XmATTACH_FORM,
         XmNbottomAttachment,     XmATTACH_FORM,
   /*    XmNfontList,             fontlist,*/
         NULL);
   XmStringFree(generic_string);
   XtAddCallback(_accept_pb, XmNactivateCallback, accept_cb, NULL);
 
   generic_string = XmStringCreateLocalized("Hold"); 
   _hold_pb = XtVaCreateManagedWidget("right_button",
         xmPushButtonWidgetClass, button_form,
         XmNlabelString,          generic_string,
         XmNleftAttachment,       XmATTACH_POSITION,
         XmNleftPosition,         10,
         XmNrightAttachment,      XmATTACH_POSITION,
         XmNrightPosition,        20,
         XmNtopAttachment,        XmATTACH_FORM,
         XmNbottomAttachment,     XmATTACH_FORM,
   /*    XmNfontList,             fontlist, */
         NULL);
   XmStringFree(generic_string);
   XtAddCallback(_hold_pb, XmNactivateCallback, hold_cb, NULL);
 
   generic_string = XmStringCreateLocalized("Reject"); 
   _reject_pb = XtVaCreateManagedWidget("right_button",
         xmPushButtonWidgetClass, button_form,
         XmNlabelString,          generic_string,
         XmNleftAttachment,       XmATTACH_POSITION,
         XmNleftPosition,         20,
         XmNrightAttachment,      XmATTACH_FORM,
         XmNtopAttachment,        XmATTACH_FORM,
         XmNbottomAttachment,     XmATTACH_FORM,
   /*    XmNfontList,             fontlist, */
         NULL);
   XmStringFree(generic_string);
   
   XtAddCallback(_reject_pb, XmNactivateCallback, reject_cb, NULL);

   /*
    * The processed data histogram has been done above.
    * Now take care of signal data histogram if exists.
    */

   if (_number_of_histogram_records <= 1)
      goto end_of_set_up_right;

   /* set up the I info of signal data */
   i_text_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           button_frame,
         XmNtopOffset,           2,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   i_text_form = XtVaCreateManagedWidget("text_form",
         xmFormWidgetClass,    i_text_frame,
         XmNheight,            115,
         XmNmaxHeight,         115,
         XmNminHeight,         115,
         XmNfractionBase,      50,
         NULL);
 
   /* The DMIN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("DMin="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_dmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The DMAX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("DMax="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_dmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
 
   /* The MEAN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("Mean="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_mean_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The SDEV= label, and its associated value label */
   generic_string = XmStringCreateLocalized("SDev="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_sdev_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The ZPIX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("ZPix="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   _i_zpix_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The SATP= label, and its associated value label */
   generic_string = XmStringCreateLocalized("SATP="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_satp_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The HMIN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("HMin="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_hmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* HMIN's associated at: label, and its associated value label */
   generic_string = XmStringCreateLocalized("at:"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_atmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The HMAX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("HMax="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _i_hmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNbottomPosition,   49,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* HMAX's associated at: label, and its associated value label */
   generic_string = XmStringCreateLocalized("at:"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  i_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   _i_atmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   i_text_form,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNshadowThickness,  0,  
         XmNrightAttachment,  XmATTACH_FORM,
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* completed the I info of signal data */
   /* set up the I graph for signal data */
   i_graph_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           i_text_frame,
         XmNtopOffset,           2,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);

   i_graph_major_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,    i_graph_frame,
         XmNheight,            200,
         XmNmaxHeight,         200,
         XmNminHeight,         200,
         NULL);

   generic_string = XmStringCreateLocalized(_i_histogram_description);
   i_graph_bottom_label = XtVaCreateManagedWidget("info_label",
         xmLabelWidgetClass,   i_graph_major_form,
         XmNlabelString,       generic_string,
         XmNrightAttachment,   XmATTACH_FORM,
         XmNbottomAttachment,  XmATTACH_FORM,
         XmNleftAttachment,    XmATTACH_FORM,
         NULL);
   XmStringFree (generic_string);

   _i_graph_drawing_area = XtVaCreateManagedWidget("right_form",
         xmDrawingAreaWidgetClass,    i_graph_major_form,
         XmNbottomAttachment,  XmATTACH_WIDGET,
         XmNbottomWidget,      i_graph_bottom_label,
         XmNrightAttachment,   XmATTACH_FORM,
         XmNtopAttachment,     XmATTACH_FORM,
         XmNleftAttachment,    XmATTACH_FORM,
         NULL);

   /* set up the graphics context for the drawing area... */
   gc_values.foreground = BlackPixelOfScreen(XtScreen (_i_graph_drawing_area));
   _i_histogram_gc = XCreateGC(_display_pointer,
			       RootWindowOfScreen
			       (XtScreen(_i_graph_drawing_area)),
			       GCForeground,
			       &gc_values);

   XtAddCallback(_i_graph_drawing_area, XmNexposeCallback,
                 draw_i_histogram, (XtPointer) _i_graph_drawing_area);

   /* completed the I graph of signal data */
   /* set up the Q info of signal data */

   q_text_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           i_graph_frame,
         XmNtopOffset,           2,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);
   q_text_form = XtVaCreateManagedWidget("text_form",
         xmFormWidgetClass,    q_text_frame,
         XmNheight,            115,
         XmNmaxHeight,         115,
         XmNminHeight,         115,
         XmNfractionBase,      50,
         NULL);
 
   /* The DMIN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("DMin="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_dmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The DMAX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("DMax="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_dmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   9,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
 
   /* The MEAN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("Mean="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_mean_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The SDEV= label, and its associated value label */
   generic_string = XmStringCreateLocalized("SDev="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_sdev_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      10,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   19,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The ZPIX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("ZPix="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   _q_zpix_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The SATP= label, and its associated value label */
   generic_string = XmStringCreateLocalized("SATP="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_satp_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      20,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   29,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The HMIN= label, and its associated value label */
   generic_string = XmStringCreateLocalized("HMin="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_hmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0, 
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* HMIN's associated at: label, and its associated value label */
   generic_string = XmStringCreateLocalized("at:"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_atmin_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,  q_text_form,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      30,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   39,
         XmNrightAttachment,  XmATTACH_FORM,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* The HMAX= label, and its associated value label */
   generic_string = XmStringCreateLocalized("HMax="); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNleftAttachment,   XmATTACH_FORM,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    10,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   /* this value is set up in set_up_info_label_values() */
   _q_hmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,   q_text_form,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNshadowThickness,  0,  
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNbottomPosition,   49,
         XmNrightAttachment,  XmATTACH_POSITION,
         XmNrightPosition,    25,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     10,
   /*    XmNfontList,         fontlist,  */
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* HMAX's associated at: label, and its associated value label */
   generic_string = XmStringCreateLocalized("at:"); 
   XtVaCreateManagedWidget("info_label", 
         xmLabelWidgetClass,  q_text_form,
         XmNlabelString,      generic_string,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     26,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_BEGINNING,
         NULL);
   XmStringFree(generic_string);
 
   _q_atmax_value_text = XtVaCreateManagedWidget("info_label", 
         xmTextWidgetClass,  q_text_form,
         XmNleftAttachment,   XmATTACH_POSITION,
         XmNleftPosition,     36,
         XmNtopAttachment,    XmATTACH_POSITION,
         XmNtopPosition,      40,
         XmNbottomAttachment, XmATTACH_POSITION,
         XmNbottomPosition,   49,
         XmNshadowThickness,  0,  
         XmNrightAttachment,  XmATTACH_FORM,
         XmNborderWidth,      0,  
         XmNmarginWidth,      0,
         XmNmarginHeight,     0,
         XmNeditable,         False, 
         XmNverifyBell,       False,
         XmNcursorPositionVisible, False,
   /*    XmNfontList,         fontlist,*/
         XmNalignment,        XmALIGNMENT_END,
         NULL);
 
   /* completed the Q graph of signal data */
 
   /* set up the Q graph of signal data */
   q_graph_frame = XtVaCreateManagedWidget("right_frame",
         xmFrameWidgetClass,     right_form,
         XmNrightAttachment,     XmATTACH_FORM,
         XmNtopAttachment,       XmATTACH_WIDGET,
         XmNtopWidget,           q_text_frame,
         XmNtopOffset,           2,
         XmNleftAttachment,      XmATTACH_FORM,
         XmNbottomAttachment,    XmATTACH_FORM,
         XmNshadowType,          XmSHADOW_OUT,
         NULL);

   q_graph_major_form = XtVaCreateManagedWidget("right_form",
         xmFormWidgetClass,    q_graph_frame,
         XmNheight,            200,
         XmNmaxHeight,         200,
         XmNminHeight,         200,
         NULL);

   generic_string = XmStringCreateLocalized(_q_histogram_description);
   q_graph_bottom_label = XtVaCreateManagedWidget("info_label",
         xmLabelWidgetClass,   q_graph_major_form,
         XmNlabelString,       generic_string,
         XmNrightAttachment,   XmATTACH_FORM,
         XmNbottomAttachment,  XmATTACH_FORM,
         XmNleftAttachment,    XmATTACH_FORM,
         NULL);
   XmStringFree(generic_string);

   _q_graph_drawing_area = XtVaCreateManagedWidget("right_form",
         xmDrawingAreaWidgetClass,    q_graph_major_form,
         XmNbottomAttachment,  XmATTACH_WIDGET,
         XmNbottomWidget,      q_graph_bottom_label,
         XmNrightAttachment,   XmATTACH_FORM,
         XmNtopAttachment,     XmATTACH_FORM,
         XmNleftAttachment,    XmATTACH_FORM,
         NULL);

   /* set up the graphics context for the drawing area... */
   gc_values.foreground = BlackPixelOfScreen(XtScreen (_q_graph_drawing_area));
   _q_histogram_gc = XCreateGC(_display_pointer,
			       RootWindowOfScreen
			       (XtScreen(_q_graph_drawing_area)),
			       GCForeground,
			       &gc_values);

   XtAddCallback(_q_graph_drawing_area, XmNexposeCallback,
                 draw_q_histogram, (XtPointer) _q_graph_drawing_area);


end_of_set_up_right:

   /* we're all done with the small font */
   XmFontListFree(fontlist);
 
   free(_order_id_string); 
   free(_production_request_id_string); 
 
   return scrolled_right;
}
