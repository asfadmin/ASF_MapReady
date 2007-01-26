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

char *sprn_mpdr(struct VMPDREC *vmpdr)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n*********** begin of Map Projection Data record **************\n");
  add(&ret, "\nMap Projection Descriptor =\t%s\n",vmpdr->mpdesc);
  add(&ret, "Number of Pixels =\t%i\n",vmpdr->npixels);
  add(&ret, "Number of Lines  =\t%i\n",vmpdr->nlines);
  add(&ret, "Nominal inter-pixel distance in output (m) = \t%f\n",vmpdr->nomipd);
  add(&ret, "Nominal inter-line distance in output (m) =\t%f\n",vmpdr->nomild);
  add(&ret, "Orientation at center =\t%f\n",vmpdr->orient);
  add(&ret, "Actual platform orbital inclination (deg) =\t%f\n",vmpdr->orbitincl);
  add(&ret, "Actual ascending node (deg) =\t%f\n",vmpdr->ascnode);
  add(&ret, "Platform distance from center to geocenter =\t%f\n",vmpdr->distplat);
  add(&ret, "Altitude of platform rel. to ellipsoid (m) =\t%f\n",vmpdr->altplat);
  add(&ret, "Actual groundspeed at nadir =\t%f\n",vmpdr->velnadir);
  add(&ret, "Platform heading (deg) =\t%f\n",vmpdr->plathead);
  add(&ret, "Name of reference ellipsoid =\t%s\n",vmpdr->refelip);
  add(&ret, "Semimajor axis of ellipsoid (m) =\t%f\n",vmpdr->remajor);
  add(&ret, "Semiminor axis of ellipsoid (m) =\t%f\n",vmpdr->reminor);
  add(&ret, "Datum shift reference dx (m) =\t%f\n",vmpdr->datshiftx);
  add(&ret, "Datum shift perpendicular dy (m) =\t%f\n",vmpdr->datshifty);
  add(&ret, "Datum shift direction of rotation dz (m) =\t%f\n",vmpdr->datshiftz);
  add(&ret, "Datum shift 1, rotation angle =\t%f\n",vmpdr->datshift1); 
  add(&ret, "Datum shift 2, rotation angle =\t%f\n",vmpdr->datshift2); 
  add(&ret, "Datum shift 3, rotation angle =\t%f\n",vmpdr->datshift3); 
  add(&ret, "Scale factor of reference ellipsoid =\t%f\n",vmpdr->rescale);
  add(&ret, "Alphanumeric description of map projection =\t%s\n",vmpdr->mpdesig);
  add(&ret, "UTM descriptor =\t%s\n",vmpdr->utmdesc);
  add(&ret, "Signature of UTM zone =\t%s\n",vmpdr->utmzone);
  add(&ret, "Map origin - false east =\t%f\n",vmpdr->utmeast);
  add(&ret, "Map origin - false north =\t%f\n",vmpdr->utmnorth);
  add(&ret, "Center of projection longitude =\t%f\n",vmpdr->utmlong);
  add(&ret, "Center of projection latitude =\t%f\n",vmpdr->utmlat);
  add(&ret, "1st standard parallel (deg) =\t%f\n",vmpdr->utmpara1);
  add(&ret, "2nd standard parallel (deg) =\t%f\n",vmpdr->utmpara2);
  add(&ret, "UTM scale factor =\t%f\n",vmpdr->utmscale);
  add(&ret, "UPS descriptor =\t%s\n",vmpdr->upsdesc);
  add(&ret, "Center of projection longitude =\t%f\n",vmpdr->upslong);
  add(&ret, "Center of projection latitude =\t%f\n",vmpdr->upslat);
  add(&ret, "UPS scale factor =\t%f\n",vmpdr->upsscale);
  add(&ret, "NPS projection description =\t%s\n",vmpdr->nspdesc);
  add(&ret, "Map origin - fasle east =\t%f\n",vmpdr->nspeast);
  add(&ret, "Map origin - false north =\t%f\n",vmpdr->nspnorth);
  add(&ret, "Center of projection longitude =\t%f\n",vmpdr->nsplong);
  add(&ret, "Center of projection latitude =\t%f\n",vmpdr->nsplat);
  add(&ret, "Standard parallels =\t%f\n",vmpdr->nsppara1);
  add(&ret, "Standard parallels =\t%f\n",vmpdr->nsppara2);
  add(&ret, "Standard parallels =\t%f\n",vmpdr->nsppara3);
  add(&ret, "Standard parallels =\t%f\n",vmpdr->nsppara4);
  add(&ret, "Central meridian =\t%f\n",vmpdr->nspcm1);
  add(&ret, "Central meridian =\t%f\n",vmpdr->nspcm2);
  add(&ret, "Central meridian =\t%f\n",vmpdr->nspcm3);
  add(&ret, "Top left corner north (m) =\t%f\n",vmpdr->tlcnorth);
  add(&ret, "Top left corner east (m) =\t%f\n",vmpdr->tlceast);
  add(&ret, "Top right corner north (m) =\t%f\n",vmpdr->trcnorth);
  add(&ret, "Top right corner east (m) =\t%f\n",vmpdr->trceast);
  add(&ret, "Bottom right corner north (m) =\t%f\n",vmpdr->brcnorth);
  add(&ret, "Bottom right corner east (m) =\t%f\n",vmpdr->brceast);
  add(&ret, "Bottom left corner north (m) =\t%f\n",vmpdr->blcnorth);
  add(&ret, "Bottom left corner east (m) =\t%f\n",vmpdr->blceast);
  add(&ret, "Top left corner latitude (deg) =\t%f\n",vmpdr->tlclat);
  add(&ret, "Top left corner longitude (deg) =\t%f\n",vmpdr->tlclong);
  add(&ret, "Top right corner latitude (deg) =\t%f\n",vmpdr->trclat);
  add(&ret, "Top right corner longitude (deg) =\t%f\n",vmpdr->trclong);
  add(&ret, "Bottom right corner latitude (deg) =\t%f\n",vmpdr->brclat);
  add(&ret, "Bottom right corner longitude (deg) =\t%f\n",vmpdr->brclong);
  add(&ret, "Bottom left corner latitude (deg) =\t%f\n",vmpdr->blclat);
  add(&ret, "Bottom left corner longitude (deg) =\t%f\n",vmpdr->blclong);
  add(&ret, "Top left corner terrain height (m) =\t%f\n",vmpdr->tlcheight);
  add(&ret, "Top right corner terrain height (m) =\t%f\n",vmpdr->trcheight);
  add(&ret, "Bottom right corner terrain height (m) =\t%f\n",vmpdr->brcheight);
  add(&ret, "Bottom left corner terrain height (m) =\t%f\n",vmpdr->blcheight);
  add(&ret, "Coefficients to convert a line & pixel to a projection reference\n");
  add(&ret, "a11 =\t%f\n",vmpdr->a11);
  add(&ret, "a12 =\t%f\n",vmpdr->a12);
  add(&ret, "a13 =\t%f\n",vmpdr->a13);
  add(&ret, "a14 =\t%f\n",vmpdr->a14);
  add(&ret, "a21 =\t%f\n",vmpdr->a21);
  add(&ret, "a22 =\t%f\n",vmpdr->a22);
  add(&ret, "a23 =\t%f\n",vmpdr->a23);
  add(&ret, "a24 =\t%f\n",vmpdr->a24);
  add(&ret, "Coefficients to convert a projection reference to a line & pixel\n");
  add(&ret, "b11 =\t%f\n",vmpdr->b11);
  add(&ret, "b12 =\t%f\n",vmpdr->b12);
  add(&ret, "b13 =\t%f\n",vmpdr->b13);
  add(&ret, "b14 =\t%f\n",vmpdr->b14);
  add(&ret, "b21 =\t%f\n",vmpdr->b21);
  add(&ret, "b22 =\t%f\n",vmpdr->b22);
  add(&ret, "b23 =\t%f\n",vmpdr->b23);
  add(&ret, "b24 =\t%f\n",vmpdr->b24);
  add(&ret, "************* end of Map Projection Data record ***************\n");
  return ret;
}

void prn_mpdr(FILE *fp, struct VMPDREC *vmpdr)
{
    char *rec = sprn_mpdr(vmpdr);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
