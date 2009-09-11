/*
	Earthloc.h:
		This header file contains structures and parameters used 
	by the JPL image location software contained in the geolocate library.
	These routines are low-level, and are usually called by asf_meta,
	in the same library.
	
		Also see the geolocation.h header file, which contains external
	entry points to this software.
*/

/************************* FORTRAN compatibility routines *************/
#ifndef pi
#define pi M_PI
#endif

#define DEBUGF printf
#define DEBUGGER printf("*************** GEOLOCATION ERROR **************\n");

/************ Callable routines ************/

#include "geolocate.h"

void makeMatrix(GEOLOCATE_REC *g);/*In getLatLon.c*/
