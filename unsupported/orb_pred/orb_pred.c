/****************************************************************
NAME:  orb_pred

SYNOPSIS:

DESCRIPTION:
	Predicts the orbit position for a given location for both
	ERS-1 and JRES-1 SAR images.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0     7/95         Original development (based on code written 
			 by Shusun Li)

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:
****************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.


This software was authored by:

RICK GURITZ      rguritz@images.alaska    (907)474-7886
Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu


NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include "asf.h" 
 

#define __MAIN_C 
#include "orb_pred.h"

int main(argc, argv) 
int argc;
char *argv[];
{
  /* cmd line variables */
  extern int optind;
  short int option = 0;
  char dir = 0;
  short int errflg=0;
  int c;

  /* parse command line */
  while ((c=getopt(argc,argv,"ABCDEFGadb")) != EOF)
    switch (c) {
      case 'a':
	dir |= DIR_A;
        break;
      case 'd':
	dir |= DIR_D;
        break;
      case 'b':
	dir |= DIR_A;
	dir |= DIR_D;
	break;
      case 'A':
        option |= PHASE_A;
        break;
      case 'B':
        option |= PHASE_B;
        break;
      case 'C':
        option |= PHASE_C;
        break;
      case 'D':
        option |= PHASE_D;
        break;
      case 'E':
	 option |= PHASE_E;
	 break;
      case 'F':
        option |= PHASE_F;
        break;
      case 'G':
	option |= PHASE_G;
	break;
      default:
	errflg++;
    }
  if (optind != argc - 1) 
    errflg++;
  if (option == 0)
    errflg++;
  if (errflg) {
    print_usage();
    exit(errflg);
  }

  /* Calculate each specified phase & what direction. */
  if (option & PHASE_A) {
    printf("\nPHASE A Orbits ==>\n");
    if (dir & DIR_A) {
      printf("\nASCENDING:\n");
      predict_orbit(hdAasc, odAasc, A_ASC_LINES, argv[optind], TRUE);
    }
    if (dir & DIR_D) {
      printf("\nDESCENDING:\n");
      predict_orbit(hdAdesc, odAdesc, A_DESC_LINES, argv[optind], FALSE);
    }
  }
  
  if (option & PHASE_B) {
    printf("\nPHASE B Orbits ==>\n");
    if (dir & DIR_A) {
      printf("\nASCENDING:\n");
      predict_orbit(hdBasc, odBasc, B_ASC_LINES, argv[optind], TRUE);
    }
    if (dir & DIR_D) {
      printf("\nDESCENDING:\n");
      predict_orbit(hdBdesc, odBdesc, B_DESC_LINES, argv[optind], FALSE);
    }
  }
  
  if (option & PHASE_C) {
    printf("PHASE C Orbits ==>\n");
    if (dir & DIR_A) {
       printf("\nASCENDING:\n");
       predict_orbit(hdCasc, odCasc, C_ASC_LINES, argv[optind], TRUE);
    } 
    if (dir & DIR_D) {
       printf("\nDESCENDING:\n");
       predict_orbit(hdCdesc, odCdesc, C_DESC_LINES, argv[optind], FALSE);
    }
  }

  if (option & PHASE_D) {
    printf("\nPHASE D Orbits ==>\n");
    if (dir & DIR_A) {
      printf("\nASCENDING:\n");
      predict_orbit(hdDasc, odDasc, D_ASC_LINES, argv[optind], TRUE);
    }
    if (dir & DIR_D) {
      printf("\nDESCENDING:\n");
      predict_orbit(hdDdesc, odDdesc, D_DESC_LINES, argv[optind], FALSE);
    }
  }

  if (option & PHASE_E) {
    printf("\nPHASE E Orbits ==>\n");
    if (dir & DIR_A) {
      printf("\nASCENDING:\n");
      predict_orbit(hdEasc, odEasc, E_ASC_LINES, argv[optind], TRUE);
    }
    if (dir & DIR_D) {
      printf("\nDESCENDING:\n");
      predict_orbit(hdEdesc, odEdesc, E_DESC_LINES, argv[optind], FALSE);
    }
  }
  
  if (option & PHASE_F) {
    printf("\nPHASE F Orbits ==>\n");
    if (dir & DIR_A) {
      printf("\nASCENDING:\n");
      predict_orbit(hdFasc, odFasc, F_ASC_LINES, argv[optind], TRUE);
    }
    if (dir & DIR_D) {
      printf("\nDESCENDING:\n");
      predict_orbit(hdFdesc, odFdesc, F_DESC_LINES, argv[optind], FALSE);
    }
  }

  if (option & PHASE_G) {
    printf("\nPHASE G Orbits ==>\n");
    if (dir & DIR_A) {
      printf("\nASCENDING:\n");
      predict_orbit(hdGasc, odGasc, G_ASC_LINES, argv[optind], TRUE);
    }
    if (dir & DIR_D) {
      printf("\nDESCENDING:\n");
      predict_orbit(hdGdesc, odGdesc, G_DESC_LINES, argv[optind], FALSE);
    }
  }

  return 0;
}

void print_usage() {
  fprintf(stderr,"USAGE: orb_pred -abd -ABCDEF infile\n");
  fprintf(stderr,"\t-a\t\tcompute ascending paths\n");
  fprintf(stderr,"\t-d\t\tcompute descending paths\n");
  fprintf(stderr,"\t-b\t\tcompute ascending & descending paths\n\n");
  fprintf(stderr,"\t-A\t\tCalculate Phase A orbits\n");
  fprintf(stderr,"\t-B\t\tCalculate Phase B orbits\n");
  fprintf(stderr,"\t-C\t\tCalculate Phase C orbits\n");
  fprintf(stderr,"\t-D\t\tCalculate Phase D orbits\n");
  fprintf(stderr,"\t-E\t\tCalculate Phase E orbits\n");
  fprintf(stderr,"\t-F\t\tCalculate Phase F orbits\n");
  fprintf(stderr,"\t-G\t\tCalculate Phase G orbits\n\n");
  fprintf(stderr,"\tinfile\t\tUser specified input file.\n\n\n");
  fprintf(stderr,"\nVersion %.2f, ASF STEP Tools\n",VERSION);
}

void assign_orbit_values(hd,c,rd,ro,np) 
HEADER_DATA hd;
double *c;
int *rd, *ro, *np;
{
  if (hd.sat_flag[0] == 'J' || hd.sat_flag[0] == 'j') {
    *c  = J_CYCLE;
    *rd = J_DAYS;
    *ro = J_ORBS;
    *np = J_N_PATH;
  } else if (hd.r_rev == E3_ORBS) {
    *c  = E3_CYCLE;
    *rd = E3_DAYS;
    *ro = E3_ORBS;
    *np = E3_N_PATH;
  } else if (hd.r_rev == E35_ORBS) {
    *c  = E35_CYCLE;
    *rd = E35_DAYS;
    *ro = E35_ORBS;
    *np = E35_N_PATH;
  } else if (hd.r_rev == E168_ORBS) {
    *c  = E168_CYCLE;
    *rd = E168_DAYS;
    *ro = E168_ORBS;
    *np = E168_N_PATH;
  }

  return;
}

