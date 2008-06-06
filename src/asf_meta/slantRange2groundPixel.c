#include "asf.h"
#include "asf_meta.h"
#include <math.h>

/*******************************************************
 * slantRange2groundPixel:
 * Calculate the ground range pixel from the total slant
 * range*/
int slantRange2groundPixel(meta_parameters *meta,double slant)
{
    double a,x,y,Gr0, GrX;
    double Rsc, Re;
    int    groundPixel;

    Rsc = meta_get_sat_height(meta, meta->general->line_count/2, 0);
    Re  = meta_get_earth_radius(meta, meta->general->line_count/2, 0);

    a = (Rsc-Re)/Re;
    x = 1+a;

    y = meta->sar->slant_range_first_pixel / Re;

    Gr0 = Re * acos((1.0 + x*x - y*y) / (2.0 * x));

    y = slant/Re;
    GrX = Re * acos((1.0 + x*x - y*y) / (2.0 * x));

    if (!meta_is_valid_double(Gr0) || !meta_is_valid_double(GrX)) {
        asfPrintError("Cannot convert slant range to ground range.\n");
    }

    if (meta && meta->projection && meta_is_valid_double(meta->projection->perX)) {
        groundPixel = (int) ((GrX-Gr0)/meta->projection->perX)+0.5;
//        printf("Slant %f :: ",slant);
//        printf("GP = (%f-%f) / %f = %i\n",GrX,Gr0,meta->projection->perX,groundPixel);
    }
    else if (meta &&
             meta->general->data_type != COMPLEX_BYTE &&
             meta->general->data_type != COMPLEX_INTEGER16 &&
             meta->general->data_type != COMPLEX_INTEGER32 &&
             meta->general->data_type != COMPLEX_REAL32 &&
             meta->general->data_type != COMPLEX_REAL64
            )
    {
        groundPixel = (int) ((GrX-Gr0)/meta->general->x_pixel_size)+0.5;
//        printf("Slant %f :: ",slant);
//        printf("GP = (%f-%f) / %f = %i\n",GrX,Gr0,meta->general->x_pixel_size,groundPixel);
    }
    else {
        groundPixel=0;    // just to quiet the compiler warning
        asfPrintError("Cannot convert slant range to ground range without a valid perX size.\n");
    }

    return(groundPixel);}

