#include "qinclude.h"
#include "qdefines.h"     /*  contains all the #define's  */
#include "qfunc_decl.h"   /*  contains all the function declarations  */
#include "qglobal_var.h"  /*  contains all the global variables  */

static char sccsid_qset_labels_c[] = "@(#)qset_labels.c	1.16 97/06/03 11:02:16";

/* This function sensitizes or desensitizes the stretching buttons,
 * as appropriate */

void
set_stretch_button_sensitivity()
{
   int   pixel_stretch_value_buffer = 1,
         pixel_stretch_value_max = 255, 
         pixel_stretch_value_min = 0;
 
   /* check low_fast_down */
   if (_low_pixel_stretch_value >= 
       (pixel_stretch_value_min + _fast_change_amount))
      XtVaSetValues (_low_fast_down_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_low_fast_down_pb, XmNsensitive, FALSE, NULL);
     
   /* check low_down */
   if (_low_pixel_stretch_value > pixel_stretch_value_min)
      XtVaSetValues (_low_down_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_low_down_pb, XmNsensitive, FALSE, NULL);
 
   /* check low_up */
   if (_low_pixel_stretch_value < 
       (_high_pixel_stretch_value - pixel_stretch_value_buffer))
      XtVaSetValues (_low_up_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_low_up_pb, XmNsensitive, FALSE, NULL);
 
   /* check low_fast_up */
   if (_low_pixel_stretch_value <= (_high_pixel_stretch_value - 
       (_fast_change_amount + pixel_stretch_value_buffer)))
      XtVaSetValues (_low_fast_up_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_low_fast_up_pb, XmNsensitive, FALSE, NULL);
 
   /* check high_fast_down */
   if (_high_pixel_stretch_value >= _low_pixel_stretch_value + 
       (_fast_change_amount + pixel_stretch_value_buffer))
      XtVaSetValues (_high_fast_down_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_high_fast_down_pb, XmNsensitive, FALSE, NULL);
     
   /* check high_down */
   if (_high_pixel_stretch_value > 
       (_low_pixel_stretch_value + pixel_stretch_value_buffer))
      XtVaSetValues (_high_down_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_high_down_pb, XmNsensitive, FALSE, NULL);
 
   /* check high_up */
   if (_high_pixel_stretch_value < pixel_stretch_value_max)
      XtVaSetValues (_high_up_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_high_up_pb, XmNsensitive, FALSE, NULL);
 
   /* check high_fast_up */
   if (_high_pixel_stretch_value <= 
       (pixel_stretch_value_max - _fast_change_amount))
      XtVaSetValues (_high_fast_up_pb, XmNsensitive, TRUE, NULL);
   else
      XtVaSetValues (_high_fast_up_pb, XmNsensitive, FALSE, NULL);
}

/* This function sensitizes or desensitizes the  buttons,
 * as appropriate */

void
set_zoom_button_sensitivity()
{
   char  scale_factor_string[30];

   int   zoomed_out_scale_factor = figure_out_scale_factor();

   if (zoomed_out_scale_factor > 1)
      XtVaSetValues (_zoom_box_pb, XmNsensitive, True, NULL);
   else
      XtVaSetValues (_zoom_box_pb, XmNsensitive, False, NULL);

   /* Now, change the pixel scale factor text value
   sprintf (scale_factor_string, "%3d", _image_scale_factor); */
   sprintf(scale_factor_string, "%3d",
          (_image_scale_factor==-1 ? 1 : _image_scale_factor));

   XmTextSetString (_psf_value_text, scale_factor_string);
}


/* This is used to desensitize / resensitize everything but the 
 * scroll bars of the app, when the zoom box is activated / 
 * de-activated. */

void
change_all_sensitivity (sensitivity_change)
   int sensitivity_change;
{
   switch (sensitivity_change) {
   case (DESENSITIZE_ALL) : {
      /* first the low stretching... */
      XtVaSetValues (_low_fast_down_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_low_down_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_low_up_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_low_fast_up_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_low_stretch_value_text, XmNsensitive, FALSE, NULL);

      /* then the high stretching */
      XtVaSetValues (_high_fast_down_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_high_down_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_high_up_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_high_fast_up_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_high_stretch_value_text, XmNsensitive, FALSE, NULL);

#if 0 /* not needed, since these buttons don't exist anymore! */

      XtVaSetValues (_zoom_in_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_zoom_out_pb, XmNsensitive, FALSE, NULL);
#endif
      /* then the zoom buttons, */
      XtVaSetValues (_zoom_box_pb, XmNsensitive, FALSE, NULL);

      /* Then the accept / reject / hold buttons */
      XtVaSetValues (_accept_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_reject_pb, XmNsensitive, FALSE, NULL);
      XtVaSetValues (_hold_pb, XmNsensitive, FALSE, NULL);

      /* and finally the _menubar */
      XtVaSetValues (_menubar, XmNsensitive, FALSE, NULL);
      break;
    }
    
    case (RESENSITIZE_AS_NEEDED) : {
       set_stretch_button_sensitivity();
       set_zoom_button_sensitivity();

       XtVaSetValues (_menubar, XmNsensitive, TRUE, NULL);
       XtVaSetValues (_low_stretch_value_text, XmNsensitive, TRUE, NULL);
       XtVaSetValues (_high_stretch_value_text, XmNsensitive, TRUE, NULL);

       /* Then the accept / reject / hold buttons */
       XtVaSetValues (_accept_pb, XmNsensitive, TRUE, NULL);
       XtVaSetValues (_reject_pb, XmNsensitive, TRUE, NULL);
       XtVaSetValues (_hold_pb, XmNsensitive, TRUE, NULL);
       break;
    }

    default : {
       make_announcement ("change_all_sensitivity called incorrectly", LOG);
       exit (QC_PROGRAMMER_ERROR);
    }
    }
}

