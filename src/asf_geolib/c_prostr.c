/*******************************************************************************
NAME:				C_PROSTR

PURPOSE:	Returns a title string of the input projection code

PROGRAM HISTORY:
	PROGRAMMER	  DATE		REASON
	----------	  ----		------
	B. Ailts	  7/88		Original Development
	M. Shindle        6/95          Removed TAE dependencies

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:

PROJECT:			LAS

ALGORITHM:
	Depending on the input projection code each image
 	   Return the correct title

******************************************************************************/

#include "worgen.h"
#include "proj.h"

void c_prostr(proj_code,string)
int *proj_code;
char *string;
{  
switch (*proj_code)
   {
   case GEO:strcpy(string,"(0)GEOGRAPHIC");
	   break;
   case UTM:strcpy(string,"(1)UTM");
	   break;
   case SPCS:strcpy(string,"(2)STATE PLANE");
	   break;
   case ALBERS:strcpy(string,"(3)ALBERS CONICAL EQUAL AREA");
	   break;
   case LAMCC:strcpy(string,"(4)LAMBERT CONFORMAL CONIC");
	   break;
   case MERCAT:strcpy(string,"(5)MERCATOR");
	   break;
   case PS:strcpy(string,"(6)POLAR STEREOGRAPHIC");
	   break;
   case POLYC:strcpy(string,"(7)POLYCONIC");
	   break;
   case EQUIDC:strcpy(string,"(8)EQUIDISTANT CONIC");
	   break;
   case TM:strcpy(string,"(9)TRANSVERSE MERCATOR");
	   break;
   case STEREO:strcpy(string,"(10)STEREOGRAPHIC");
	   break;
   case LAMAZ:strcpy(string,"(11)LAMBERT AZIMUTHAL EQUAL AREA");
	   break;
   case AZMEQD:strcpy(string,"(12)AZIMUTHAL EQUIDISTANT");
	   break;
   case GNOMON:strcpy(string,"(13)GNOMONIC");
	   break;
   case ORTHO:strcpy(string,"(14)ORTHOGRAPHIC");
	   break;
   case GVNSP:strcpy(string,"(15)GENERAL VERTICAL NEAR-SIDE PERSPECTIVE");
	   break;
   case SNSOID:strcpy(string,"(16)SINUSOIDAL");
	   break;
   case EQRECT:strcpy(string,"(17)EQUIRECTANGULAR");
	   break;
   case MILLER:strcpy(string,"(18)MILLER CYLINDRICAL");
	   break;
   case VGRINT:strcpy(string,"(19)VAN DER GRINTEN");
	   break;
   case HOM:strcpy(string,"(20)OBLIQUE MERCATOR (Hotine)");
	   break;
   case ROBIN:strcpy(string,"(21)ROBINSON");
	   break;
   case SOM:strcpy(string,"(22)SOM");
	   break;
   case ALASKA:strcpy(string,"(23)ALASKA CONFORMAL");
	   break;
   case GOOD:strcpy(string,"(24)INTERRUPTED GOODE HOMOLOSINE");
	   break;
   case MOLL:strcpy(string,"(25)MOLLWEIDE");
	   break;
   case IMOLL:strcpy(string,"(26)INTERRUPTED MOLLWEIDE");
	   break;
   case HAMMER:strcpy(string, "(27)HAMMER");
	   break;
   case WAGIV:strcpy(string, "(28)WAGNER IV");
	   break;
   case WAGVII:strcpy(string, "(29)WAGNER VII");
	   break;
   case OBEQA:strcpy(string, "(30)OBLATED EQUAL AREA");
	   break;
   default:sprintf(string,"(%-2d)USER DEFINED",*proj_code);
           break;
   }

return;
}  /* prostr  */
