/********************************************************************
NAME:     PRN_MPDR.C

PURPOSE:  Print out values for the value structure of the
          Map Projection Data Record. 

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           3/96   T. Logan   Based on Code by Mike Shindle (ASF)
				  Updating for metadata project
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

void prn_mpdr(FILE *fp, struct VMPDREC *vmpdr)
{
  fprintf(fp, "\n*********** begin of Map Projection Data record **************\n");
  fprintf(fp, "\nMap Projection Descriptor =\t%s\n",vmpdr->mpdesc);
  fprintf(fp, "Number of Pixels =\t%i\n",vmpdr->npixels);
  fprintf(fp, "Number of Lines  =\t%i\n",vmpdr->nlines);
  fprintf(fp, "Nominal inter-pixel distance in output (m) = \t%f\n",vmpdr->nomipd);
  fprintf(fp, "Nominal inter-line distance in output (m) =\t%f\n",vmpdr->nomild);
  fprintf(fp, "Orientation at center =\t%f\n",vmpdr->orient);
  fprintf(fp, "Actual platform orbital inclination (deg) =\t%f\n",vmpdr->orbitincl);
  fprintf(fp, "Actual ascending node (deg) =\t%f\n",vmpdr->ascnode);
  fprintf(fp, "Platform distance from center to geocenter =\t%f\n",vmpdr->distplat);
  fprintf(fp, "Altitude of platform rel. to ellipsoid (m) =\t%f\n",vmpdr->altplat);
  fprintf(fp, "Actual groundspeed at nadir =\t%f\n",vmpdr->velnadir);
  fprintf(fp, "Platform heading (deg) =\t%f\n",vmpdr->plathead);
  fprintf(fp, "Name of reference ellipsoid =\t%s\n",vmpdr->refelip);
  fprintf(fp, "Semimajor axis of ellipsoid (m) =\t%f\n",vmpdr->remajor);
  fprintf(fp, "Semiminor axis of ellipsoid (m) =\t%f\n",vmpdr->reminor);
  fprintf(fp, "Datum shift reference dx (m) =\t%f\n",vmpdr->datshiftx);
  fprintf(fp, "Datum shift perpendicular dy (m) =\t%f\n",vmpdr->datshifty);
  fprintf(fp, "Datum shift direction of rotation dz (m) =\t%f\n",vmpdr->datshiftz);
  fprintf(fp, "Datum shift 1, rotation angle =\t%f\n",vmpdr->datshift1); 
  fprintf(fp, "Datum shift 2, rotation angle =\t%f\n",vmpdr->datshift2); 
  fprintf(fp, "Datum shift 3, rotation angle =\t%f\n",vmpdr->datshift3); 
  fprintf(fp, "Scale factor of reference ellipsoid =\t%f\n",vmpdr->rescale);
  fprintf(fp, "Alphanumeric description of map projection =\t%s\n",vmpdr->mpdesig);
  fprintf(fp, "UTM descriptor =\t%s\n",vmpdr->utmdesc);
  fprintf(fp, "Signature of UTM zone =\t%s\n",vmpdr->utmzone);
  fprintf(fp, "Map origin - false east =\t%f\n",vmpdr->utmeast);
  fprintf(fp, "Map origin - false north =\t%f\n",vmpdr->utmnorth);
  fprintf(fp, "Center of projection longitude =\t%f\n",vmpdr->utmlong);
  fprintf(fp, "Center of projection latitude =\t%f\n",vmpdr->utmlat);
  fprintf(fp, "1st standard parallel (deg) =\t%f\n",vmpdr->utmpara1);
  fprintf(fp, "2nd standard parallel (deg) =\t%f\n",vmpdr->utmpara2);
  fprintf(fp, "UTM scale factor =\t%f\n",vmpdr->utmscale);
  fprintf(fp, "UPS descriptor =\t%s\n",vmpdr->upsdesc);
  fprintf(fp, "Center of projection longitude =\t%f\n",vmpdr->upslong);
  fprintf(fp, "Center of projection latitude =\t%f\n",vmpdr->upslat);
  fprintf(fp, "UPS scale factor =\t%f\n",vmpdr->upsscale);
  fprintf(fp, "NPS projection description =\t%s\n",vmpdr->nspdesc);
  fprintf(fp, "Map origin - fasle east =\t%f\n",vmpdr->nspeast);
  fprintf(fp, "Map origin - false north =\t%f\n",vmpdr->nspnorth);
  fprintf(fp, "Center of projection longitude =\t%f\n",vmpdr->nsplong);
  fprintf(fp, "Center of projection latitude =\t%f\n",vmpdr->nsplat);
  fprintf(fp, "Standard parallels =\t%f\n",vmpdr->nsppara1);
  fprintf(fp, "Standard parallels =\t%f\n",vmpdr->nsppara2);
  fprintf(fp, "Standard parallels =\t%f\n",vmpdr->nsppara3);
  fprintf(fp, "Standard parallels =\t%f\n",vmpdr->nsppara4);
  fprintf(fp, "Central meridian =\t%f\n",vmpdr->nspcm1);
  fprintf(fp, "Central meridian =\t%f\n",vmpdr->nspcm2);
  fprintf(fp, "Central meridian =\t%f\n",vmpdr->nspcm3);
  fprintf(fp, "Top left corner north (m) =\t%f\n",vmpdr->tlcnorth);
  fprintf(fp, "Top left corner east (m) =\t%f\n",vmpdr->tlceast);
  fprintf(fp, "Top right corner north (m) =\t%f\n",vmpdr->trcnorth);
  fprintf(fp, "Top right corner east (m) =\t%f\n",vmpdr->trceast);
  fprintf(fp, "Bottom right corner north (m) =\t%f\n",vmpdr->brcnorth);
  fprintf(fp, "Bottom right corner east (m) =\t%f\n",vmpdr->brceast);
  fprintf(fp, "Bottom left corner north (m) =\t%f\n",vmpdr->blcnorth);
  fprintf(fp, "Bottom left corner east (m) =\t%f\n",vmpdr->blceast);
  fprintf(fp, "Top left corner latitude (deg) =\t%f\n",vmpdr->tlclat);
  fprintf(fp, "Top left corner longitude (deg) =\t%f\n",vmpdr->tlclong);
  fprintf(fp, "Top right corner latitude (deg) =\t%f\n",vmpdr->trclat);
  fprintf(fp, "Top right corner longitude (deg) =\t%f\n",vmpdr->trclong);
  fprintf(fp, "Bottom right corner latitude (deg) =\t%f\n",vmpdr->brclat);
  fprintf(fp, "Bottom right corner longitude (deg) =\t%f\n",vmpdr->brclong);
  fprintf(fp, "Bottom left corner latitude (deg) =\t%f\n",vmpdr->blclat);
  fprintf(fp, "Bottom left corner longitude (deg) =\t%f\n",vmpdr->blclong);
  fprintf(fp, "Top left corner terrain height (m) =\t%f\n",vmpdr->tlcheight);
  fprintf(fp, "Top right corner terrain height (m) =\t%f\n",vmpdr->trcheight);
  fprintf(fp, "Bottom right corner terrain height (m) =\t%f\n",vmpdr->brcheight);
  fprintf(fp, "Bottom left corner terrain height (m) =\t%f\n",vmpdr->blcheight);
  fprintf(fp, "Coefficients to convert a line & pixel to a projection reference\n");
  fprintf(fp, "a11 =\t%f\n",vmpdr->a11);
  fprintf(fp, "a12 =\t%f\n",vmpdr->a12);
  fprintf(fp, "a13 =\t%f\n",vmpdr->a13);
  fprintf(fp, "a14 =\t%f\n",vmpdr->a14);
  fprintf(fp, "a21 =\t%f\n",vmpdr->a21);
  fprintf(fp, "a22 =\t%f\n",vmpdr->a22);
  fprintf(fp, "a23 =\t%f\n",vmpdr->a23);
  fprintf(fp, "a24 =\t%f\n",vmpdr->a24);
  fprintf(fp, "Coefficients to convert a projection reference to a line & pixel\n");
  fprintf(fp, "b11 =\t%f\n",vmpdr->b11);
  fprintf(fp, "b12 =\t%f\n",vmpdr->b12);
  fprintf(fp, "b13 =\t%f\n",vmpdr->b13);
  fprintf(fp, "b14 =\t%f\n",vmpdr->b14);
  fprintf(fp, "b21 =\t%f\n",vmpdr->b21);
  fprintf(fp, "b22 =\t%f\n",vmpdr->b22);
  fprintf(fp, "b23 =\t%f\n",vmpdr->b23);
  fprintf(fp, "b24 =\t%f\n",vmpdr->b24);
  fprintf(fp, "************* end of Map Projection Data record ***************\n");
return;
}
