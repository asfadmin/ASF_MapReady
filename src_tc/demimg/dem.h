/****************************************************************
HEADER NAME:  dem.h
 
DESCRIPTION:  This include file defines constants and macros
              necessary for the DEMIMG & DEMPHASE routine.
 
PROGRAM HISTORY:
  Modified from sarsim.h file, M. Shindle (7/96)
****************************************************************/
#ifndef __DEM_H
#define __DEM_H

#include <sarsim.h>
#include <sarmodel.h>

void init_meta (char *demfile, char *sarfile, 
 	int *in_nl, int *in_ns, int *dem_nl, int *dem_ns, 
 	int *zone, double *pixsiz, double *upleft);
void usage(char *name);
void copyfile(char *to, char *from);
#define bye(mess) do {printf mess ;exit(1);} while (0)

#endif

