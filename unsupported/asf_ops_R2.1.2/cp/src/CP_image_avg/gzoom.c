#include "ginclude.h"
#include "gdefines.h"     /*  contains all the #define's  */
#include "gfunc_decl.h"   /*  contains all the function declarations  */
#include "gglobal_var.h"  /*  contains all the global variables  */

static char sccsid_gzoom_c[] = "@(#)gzoom.c	1.2 96/04/10 18:36:24";

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
      printfLLog(LOG_ERR, "ERROR:  %s",
	  "Unable to find good scale factor");
      exit(QC_PROGRAMMER_ERROR);
   }
}
