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

void prn_mpdr(struct VMPDREC *vmpdr)
{
  printf("\n*********** begin of Map Projection Data record *****************\n");
  printf("\nMap Projection Descriptor =\t%s\n",vmpdr->mpdesc);
  printf("Number of Pixels =\t%i\n",vmpdr->npixels);
  printf("Number of Lines  =\t%i\n",vmpdr->nlines);
  printf("Nominal inter-pixel distance in output (m) = \t%f\n",vmpdr->nomipd);
  printf("Nominal inter-line distance in output (m) =\t%f\n",vmpdr->nomild);
  printf("Orientation at center =\t%f\n",vmpdr->orient);
  printf("Actual platform orbital inclination (deg) =\t%f\n",vmpdr->orbitincl);
  printf("Actual ascending node (deg) =\t%f\n",vmpdr->ascnode);
  printf("Platform distance from center to geocenter =\t%f\n",vmpdr->distplat);
  printf("Altitude of platform rel. to ellipsoid (m) =\t%f\n",vmpdr->altplat);
  printf("Actual groundspeed at nadir =\t%f\n",vmpdr->velnadir);
  printf("Platform heading (deg) =\t%f\n",vmpdr->plathead);
  printf("Name of reference ellipsoid =\t%s\n",vmpdr->refelip);
  printf("Semimajor axis of ellipsoid (m) =\t%f\n",vmpdr->remajor);
  printf("Semiminor axis of ellipsoid (m) =\t%f\n",vmpdr->reminor);
  printf("Datum shift reference dx (m) =\t%f\n",vmpdr->datshiftx);
  printf("Datum shift perpendicular dy (m) =\t%f\n",vmpdr->datshifty);
  printf("Datum shift direction of rotation dz (m) =\t%f\n",vmpdr->datshiftz);
  printf("Datum shift 1, rotation angle =\t%f\n",vmpdr->datshift1); 
  printf("Datum shift 2, rotation angle =\t%f\n",vmpdr->datshift2); 
  printf("Datum shift 3, rotation angle =\t%f\n",vmpdr->datshift3); 
  printf("Scale factor of reference ellipsoid =\t%f\n",vmpdr->rescale);
  printf("Alphanumeric description of map projection =\t%s\n",vmpdr->mpdesig);
  printf("UTM descriptor =\t%s\n",vmpdr->utmdesc);
  printf("Signature of UTM zone =\t%s\n",vmpdr->utmzone);
  printf("Map origin - false east =\t%f\n",vmpdr->utmeast);
  printf("Map origin - false north =\t%f\n",vmpdr->utmnorth);
  printf("Center of projection longitude =\t%f\n",vmpdr->utmlong);
  printf("Center of projection latitude =\t%f\n",vmpdr->utmlat);
  printf("1st standard parallel (deg) =\t%f\n",vmpdr->utmpara1);
  printf("2nd standard parallel (deg) =\t%f\n",vmpdr->utmpara2);
  printf("UTM scale factor =\t%f\n",vmpdr->utmscale);
  printf("UPS descriptor =\t%s\n",vmpdr->upsdesc);
  printf("Center of projection longitude =\t%f\n",vmpdr->upslong);
  printf("Center of projection latitude =\t%f\n",vmpdr->upslat);
  printf("UPS scale factor =\t%f\n",vmpdr->upsscale);
  printf("NPS projection description =\t%s\n",vmpdr->nspdesc);
  printf("Map origin - fasle east =\t%f\n",vmpdr->nspeast);
  printf("Map origin - false north =\t%f\n",vmpdr->nspnorth);
  printf("Center of projection longitude =\t%f\n",vmpdr->nsplong);
  printf("Center of projection latitude =\t%f\n",vmpdr->nsplat);
  printf("Standard parallels =\t%f\n",vmpdr->nsppara1);
  printf("Standard parallels =\t%f\n",vmpdr->nsppara2);
  printf("Standard parallels =\t%f\n",vmpdr->nsppara3);
  printf("Standard parallels =\t%f\n",vmpdr->nsppara4);
  printf("Central meridian =\t%f\n",vmpdr->nspcm1);
  printf("Central meridian =\t%f\n",vmpdr->nspcm2);
  printf("Central meridian =\t%f\n",vmpdr->nspcm3);
  printf("Top left corner north (m) =\t%f\n",vmpdr->tlcnorth);
  printf("Top left corner east (m) =\t%f\n",vmpdr->tlceast);
  printf("Top right corner north (m) =\t%f\n",vmpdr->trcnorth);
  printf("Top right corner east (m) =\t%f\n",vmpdr->trceast);
  printf("Bottom right corner north (m) =\t%f\n",vmpdr->brcnorth);
  printf("Bottom right corner east (m) =\t%f\n",vmpdr->brceast);
  printf("Bottom left corner north (m) =\t%f\n",vmpdr->blcnorth);
  printf("Bottom left corner east (m) =\t%f\n",vmpdr->blceast);
  printf("Top left corner latitude (deg) =\t%f\n",vmpdr->tlclat);
  printf("Top left corner longitude (deg) =\t%f\n",vmpdr->tlclong);
  printf("Top right corner latitude (deg) =\t%f\n",vmpdr->trclat);
  printf("Top right corner longitude (deg) =\t%f\n",vmpdr->trclong);
  printf("Bottom right corner latitude (deg) =\t%f\n",vmpdr->brclat);
  printf("Bottom right corner longitude (deg) =\t%f\n",vmpdr->brclong);
  printf("Bottom left corner latitude (deg) =\t%f\n",vmpdr->blclat);
  printf("Bottom left corner longitude (deg) =\t%f\n",vmpdr->blclong);
  printf("Top left corner terrain height (m) =\t%f\n",vmpdr->tlcheight);
  printf("Top right corner terrain height (m) =\t%f\n",vmpdr->trcheight);
  printf("Bottom right corner terrain height (m) =\t%f\n",vmpdr->brcheight);
  printf("Bottom left corner terrain height (m) =\t%f\n",vmpdr->blcheight);
  printf("Coefficients to convert a line & pixel to a projection reference\n");
  printf("a11 =\t%f\n",vmpdr->a11);
  printf("a12 =\t%f\n",vmpdr->a12);
  printf("a13 =\t%f\n",vmpdr->a13);
  printf("a14 =\t%f\n",vmpdr->a14);
  printf("a21 =\t%f\n",vmpdr->a21);
  printf("a22 =\t%f\n",vmpdr->a22);
  printf("a23 =\t%f\n",vmpdr->a23);
  printf("a24 =\t%f\n",vmpdr->a24);
  printf("Coefficients to convert a projection reference to a line & pixel\n");
  printf("b11 =\t%f\n",vmpdr->b11);
  printf("b12 =\t%f\n",vmpdr->b12);
  printf("b13 =\t%f\n",vmpdr->b13);
  printf("b14 =\t%f\n",vmpdr->b14);
  printf("b21 =\t%f\n",vmpdr->b21);
  printf("b22 =\t%f\n",vmpdr->b22);
  printf("b23 =\t%f\n",vmpdr->b23);
  printf("b24 =\t%f\n",vmpdr->b24);
  printf("************* end of Map Projection Data record *****************\n");
return;
}
