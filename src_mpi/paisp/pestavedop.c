/******************************************************************************
NAME:	      estavedop - Estimates the average doppler for two input images

SYNOPSIS:     estavedop ifile1 ifile2 ofile

		ifile1, ifile2	- ASF CCSD Product pairs
		ofile		- Output file containing a single float value
				  that is the estimate of the average doppler

DESCRIPTION:  This program calculates the doppler centroid in the range
	      direction for the two scenes that are input and creates
	      an output file that contains a single floating point value
	      that is the doppler centroid estimate.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    estdop		estdop(file, nlines, &ret_dop)

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    ifile1.D, ifile1.L	Input ASF CCSD file pair
    ifile2.D, ifile2.L	Input ASF CCSD file pair
    ofile		Output value

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    2/97   T. Logan     Initial creation for tandem_aisp run
    1.1     7/97   O. Lawlor    Linear and quadratic doppler terms.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:	Howard Zebker (zebker@jakey.stanford.edu)

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   estavedop - Estimates the average doppler for two input images          *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* You should have received an ASF SOFTWARE License Agreement with this source *
* code. Please consult this agreement for license grant information.          *
*                                                                             *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*	Alaska Satellite Facility	    	                              *
*	Geophysical Institute			www.asf.alaska.edu            *
*       University of Alaska Fairbanks		uso@asf.alaska.edu	      *
*	P.O. Box 757320							      *
*	Fairbanks, AK 99775-7320					      *
*									      *
******************************************************************************/
#include "asf.h"
#include "estdop.h"
#include <mpi.h>

#define VERSION 1.1

int my_pe; 

int main(int argc, char *argv[])
{
  char 		fileA[256], fileB[256], ofile[256],outLine[255];
  int 		nDopLines;
  float		dop1A,dop2A,dop3A, dop1B,dop2B,dop3B;
  FILE 		*fpo;
  MPI_Status    recv_status;

  MPI_Init(&argc,&argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_pe);

  if (argc != 4)
    {
     if (my_pe == 0) {
      	printf("\nUsage: %s ccsd_file1 ccsd_file2 ave_dop_file\n",argv[0]); 
	printf("       ccsd_file1        Input CCSD file 1\n"); 
	printf("       ccsd_file2        Input CCSD file 2\n");
      	printf("       dop_file          Output Average Doppler file\n");
      	printf("\n  Version %.2f   ASF STEP Tools\n\n",VERSION);
     }
     exit(1);
    }

  nDopLines = 4000;

  if (my_pe == 0)
   {
    StartWatch();

    strcpy(fileA,argv[1]);
    strcpy(ofile, argv[3]);
    estdop(fileA, nDopLines, &dop1A,&dop2A,&dop3A);

    MPI_Recv(&dop1B,1,MPI_DOUBLE,1,1,MPI_COMM_WORLD,&recv_status);
    MPI_Recv(&dop2B,1,MPI_DOUBLE,1,2,MPI_COMM_WORLD,&recv_status);
    MPI_Recv(&dop3B,1,MPI_DOUBLE,1,3,MPI_COMM_WORLD,&recv_status);

    sprintf(outLine,"%20.18f %20.18f %20.18f",(dop1A+dop1B)/2,(dop2A+dop2B)/2,(dop3A+dop3B)/2);
    printf("\nProgram: pestavedop\n");
    printf("Doppler estimate:\n"
  	"Constant term      Linear term      Quadratic term\n"
  	"%s\n",outLine);

    fpo = fopen(ofile,"w");
    fprintf(fpo,"%s\n",outLine);
    fclose(fpo);

    StopWatch();
   }
  else if (my_pe == 1)
   {
    strcpy(fileB,argv[2]);
    estdop(fileB, nDopLines, &dop1B,&dop2B,&dop3B);

    MPI_Send(&dop1B,1,MPI_DOUBLE,0,1,MPI_COMM_WORLD);
    MPI_Send(&dop2B,1,MPI_DOUBLE,0,2,MPI_COMM_WORLD);
    MPI_Send(&dop3B,1,MPI_DOUBLE,0,3,MPI_COMM_WORLD);
    
   }
  MPI_Finalize();

  exit(0);
 }
