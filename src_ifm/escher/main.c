/****************************************************************
NAME: escher

SYNOPSIS:  escher <source.phase> <output.phase> [X Y]
  	
	<source.phase>  is an input phase file (.phase and .ddr).
  	<output.phase>  is an output unwrapped phase file.
        [X, Y]		is the unwrapping start location. (default: center)
    
DESCRIPTION:
       Escher, named after the famous Dutch artist M.C. Escher, 
  performs phase unwrapping via the Goldstien method.  This is
  used to turn a wrapped phase file (for example, from igram(1))
  into an unwrapped phase file for later conversion into
  a DEM-- a Digital Elevation Map.

       Escher is critical during interferometry.
  
  Here is Mike Shindle's original description (for entertainment value):
  Here's a cool idea:  When making branch cuts...  while doing the branch
  cut from X to Y and looping through coordinates, if you hit a point which 
  has already been cut, then just stop the cut at that point (call it Z) 
  and call the branch cut function (recursive function call), this time 
  making a cut from Z to Y.  You'll have to use some sort of semaphor to
  indicate 'remainder of cut'.  The point is, if on the first leg you hit
  a cut, and on the second part you hit a different cut, but they are all
  part of the current tree, then you don't need to do anymore; everything
  is attached.  This will eliminate, we hope and pray, a lot of intermediate
  cutting.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        AUTHOR:	PURPOSE:
    ---------------------------------------------------------------
    1.0                M. Shindle - Original Development
    2.0                M. Shindle - Default Phase value is value
			            at seed location, not 0.0.
    2.1		       O. Lawlor  - Get image size from DDR.
     "      07/11/97   D. Corbett - updated version number
    2.11	       O. Lawlor  - Check to see if seed point is out of bounds.
     "      03/08/98   D. Corbett - updated version number
    2.2		       O. Lawlor  - Ground areas of zero phase.
    2.3	     2/16/99   O. Lawlor  - Check more carefully for bad seed points.
                                    Use only 1 phase array (50% less memory!)
 
HARDWARE/SOFTWARE LIMITATIONS:

  Coherence function not yet implemented.


ALGORITHM DESCRIPTION:
  
  1. generate a mask
  2. install cuts in the mask
  3. integrate the remaining un-cut phase

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   escher  -  unwrapped an interferogram phase file using the branch cut   *
*	       method.							    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include "escher.h"

#define VERSION         2.3

/* globals */
PList list;
Uchar *mask;     /* phase-state mask  */
Uchar *im;       /* integration mask  */
float *phase;   /* input phase       */
float *coh;     /* coherence 'rho'   */
/*This is now "phase": float *uwp;   unwrapped phase   */
int wid;
int len;
int size;


int
main(int argc, char *argv[])
{
  int seedX=-1,seedY=-1; 
  char szWrap[MAXNAME], szUnwrap[MAXNAME];
  struct DDR ddr,newddr;

  /* check usage & set variables*/
  StartWatch();
  switch (argc) {
    case 5:
      seedY = atoi(argv[4]);
    case 4:
      seedX = atoi(argv[3]);
    case 3:
      create_name(szWrap, argv[1], ".phase");
      create_name(szUnwrap, argv[2], ".phase");
      break; 
    default:
      usage(argv[0]);
  }

  if (0!=c_getddr(szWrap,&ddr)) 
  	{printf("Couldn't open ddr file '%s.ddr'.\n",argv[1]);exit(1);}
  wid=ddr.ns;
  len=ddr.nl;
  if ((seedX==-1)&&(seedY==-1))
  {
  	seedX=wid/2;
  	seedY=len/2;
  }
  
  newddr=ddr;
  newddr.dtype=4;
  newddr.nbands=1;
  c_putddr(szUnwrap,&newddr);
  

  size = wid*len;
  mask  = (Uchar *)calloc(size, sizeof(Uchar));
  im    = (Uchar *)calloc(size, sizeof(Uchar));
  phase = (float *)MALLOC(sizeof(float)*size);
  /*coh   = (float *)MALLOC(sizeof(float)*size);*/

  /* perform steps*/
  printf("escher: begin unwrapping phase...\n");
  loadWrappedPhase(szWrap);
  groundBorder();
  makeMask();
  
  doStats("after makeMask():");

  installCordon("cordon");
  cutMask();

  doStats("after cutMask():");

#if DO_DEBUG_CHECKS
  saveMask((char *)mask, "test");

  verifyCuts();                                           
#endif

  checkSeed(&seedX, &seedY);

  integratePhase(seedX, seedY);

  doStats("after integratePhase():");

  finishUwp();

  saveMask(mask, szUnwrap);

  saveUwp(szUnwrap);
  
  /* clean up & save*/
  free(mask);
  free(im);
  free(phase);
  StopWatch();
  return(0);
}


void usage(char *name)
{
  printf("\n");
  printf("USAGE:  %s <source.phase> <output.phase> [X Y]\n\n",name);
  printf("  <source.phase> is an input phase file (.phase and .ddr).\n");
  printf("  <output.phase> is an output unwrapped phase file.\n");
  printf("          (X, Y) is the unwrapping start location. (default: center)\n");
  printf("  (escher will also generate a mask file 'output.phase.mask.)\n");
  printf("\n"
  "Escher uses Goldstein branch-cut phase unwrapping\n"
  "to unwrap the given [-pi,pi) input file into the phase-\n"
  "unwrapped output file.\n");
  printf("\nVersion: %.2f, ASF SAR Tools\n\n",VERSION);
  exit(1);
}
