#define IN_MAIN
#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gmain_c[] = "@(#)gmain.c	1.5 96/04/10 20:12:08";

/* This function does basically what you'd expect a main() to do... */

main(argc, argv)
   int argc;
   char *argv[];
{
   Widget right_side, major_info_block;
   Widget main_form;
   XtAppContext app;

   if (initASFlog(argc, argv, "CP_image_avg") != 0) {
       exit(QC_LOG_INIT_FAILED);
   }

   signal(SIGTERM, sig_term);

   _toplevel = XtVaAppInitialize(&app, "Qc", NULL, 0, &argc, argv, NULL, 
	          XmNwidth, SCREEN_SIZE_X, XmNheight, SCREEN_SIZE_Y, NULL);
   parse_command_line(argc, argv);  

   set_up_environment(app, argv[0], &main_form);

   read_leader_file();

   read_ceos_file();

   if ((_average_in_file == NULL) && (_image_scale_factor > 1)) {
      /* If an averaged file wasn't given, the image is being scaled down */

      /* the next line if you want image_avg to automatically generate 
       * an averaged output file if it hasn't already been specified and 
       * the image has been scaled. 
       */

      printfLLog(LOG_INFO, "%s", "Creating detected image ....");
      put_together_average_file();
   }
   else {
      /*
       * Get rid of any reference to the average in file,
       * so we don't try to read it again 
       */
      free(_average_in_file);
      _average_in_file = NULL;
   }

   closelog();
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

