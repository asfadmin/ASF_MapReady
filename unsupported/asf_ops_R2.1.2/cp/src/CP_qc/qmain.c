/*-----------------------------------------------------------------------
 *
 * Executable:   CP_qc
 *
 * Function:     Quality Control
 *
 * Date:         10/25/96
 *
 * Notes:        This process  displays image products whose format 
 *               conforms to the current CEOS specification.
 *
 * SCCS Header:  sccsid_qmain_c[] = "@(#)qmain.c	1.28 96/12/30 15:28:21"
 *
 * CP_qc  J. Ho  11/08/96 correct the definition of _image_size_x 
 *               2/14/97 Time format and 16 bits color
 *----------------------------------------------------------------------*/
#define IN_MAIN
#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */
#include "qversion.h"

static char sccsid_qmain_c[] = "@(#)qmain.c	1.30 97/04/14 15:57:16";
static char cp_qc_version[] = CP_QC_VERSION;

/* This function does basically what you'd expect a main() to do... */

main(argc, argv)
   int argc;
   char *argv[];
{
   Widget right_side, major_info_block;
   Widget main_form;
   XtAppContext app;

   /* Cris's stuff */
   signal(SIGTERM, sig_term);   

   set_up_log(argv[0]);

   XtSetLanguageProc(NULL, NULL, NULL);

   _toplevel = XtVaAppInitialize(&app, "CP_Qc", NULL, 0, &argc, argv, NULL, 
	          XmNwidth, SCREEN_SIZE_X, XmNheight, SCREEN_SIZE_Y, NULL);

   parse_command_line(argc, argv);  

   set_up_environment(app, argv[0], &main_form);

   /* This may take a while, so.... */
   make_announcement("Displaying image -- please wait ....", INFO);

   read_leader_file();

   read_ceos_file();

   if ((_average_in_file == NULL) && (_image_scale_factor > 1)) {
      /* If an averaged file wasn't given, the image is being scaled down */

      /* Uncomment the next line if you want qc to automatically generate 
       * an averaged output file if it hasn't already been specified and 
       * the image has been scaled. 
       */

      /* put_together_average_file(); */
   }
   else if (_average_in_file) {
      /*
       * Get rid of any reference to the average in file,
       * so we don't try to read it again 
       */
      free(_average_in_file);
      _average_in_file = NULL;
      _avg_file_flag = 0;
   }

   change_stretching(INITIALIZE, 0);

   _menubar = set_up_menubar(main_form);
   XtManageChild(_menubar);
 
   right_side = set_up_right(main_form);
   XtManageChild(right_side);

   if (_number_of_histogram_records >= 1)
      set_up_histogram_dependant_labels();

   major_info_block = initialize_major_info_widget(main_form);
   XtManageChild(major_info_block);

   _left_scrolled_window = set_up_left(main_form);
   XtManageChild(_left_scrolled_window);

   initialize_info_label_values();

   set_zoom_button_sensitivity();

   XtManageChild(main_form);

   XtRealizeWidget(_toplevel);
      
   /* the _toplevel must be realized before this command be executed properly */
   if (_image_type == FULL_REZ) {
      /* Add a callback to handle the re-sizing of the clip window */
      XtAddCallback(_clip_window, XmNresizeCallback,
		    draw_processed_histogram, NULL);
   }

   /* free(_font_string); */

   XSetWindowColormap(_display_pointer, XtWindow (_toplevel) , _colormap);

   XtAppMainLoop(app);

   clean_up();
}



/*----------------------------------------------------------
 * NAME:
 *  void sig_term()
 *
 *
 * DESCRIPTION:  Chris needed this to be here,
 *               for reasons that i'm not precisly
 *               aware of.
 *              
 * NOTES:
 *
 *
 *---------------------------------------------------------*/
void sig_term()
{
   printf("rutil in sigterm\n");
   exit(0);
} /* end sig_term..........................................*/

