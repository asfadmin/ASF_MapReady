/*****************************************************************************
NAME: aisp -- patch mode SAR processor

SYNOPSIS:     aisp [options] ifile ofile

              ifile   input ASF CCSD (.D & .L), or raw file (.raw & .in)
                      of raw SAR signal data.
              ofile   output file name.  AISP will create ofile_amp.img,
                      a multilooked amplitude image, and ofile.cpx, a
                      single-look-complex image.
DESCRIPTION:

    This program takes a raw Computer Compatible Signal Data (CCSD)
    file as input, and creates a complex valued SAR output image.

    Because of the huge amount of memory required, this algorithm
    operates by processing one "patch" of the CCSD data at a time.
    The main loop contains the following steps:
	(1) read the patch from the input file,
	(2) perform range compression,
	(3) perform range migration,
	(4) perform azimuth compression,
	(5) transpose the data the range line format
	(6) write the complex image patch to the output file.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    ifile.D		 Input CCSD data file
    ifile.L		 Input CCSD leader file
    ofile.cpx		 Output float complex image file
    ofile.in		 Parameter file used to create ofile
    ofile_amp.img	 Multilooked float amplitude file 

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0	    1996   T. Logan     Initial C implementation from H. Zebker roi.f
    0.1	    2/97   T. Logan     Added Doppler Estimation Subroutine Call
    0.2     2/97   T. Logan     Added read_dopplr & read_offsets
    1.0		1/98	O. Lawlor	Added ability to process RADARSAT data
    1.1		5/98	O. Lawlor	Generalized I/O, slight cleaning.
    2.0		8/98	O. Lawlor	Globals now confined to aisp_setup.c.
    					Can read image info from .fmt, .replica.
    2.5		3/99	O. Lawlor	Can now read from .in file just like 
                                        it reads from .L file.
    2.6     6/00    D. Koster   changed fseek->fseek64, to read files > 2GB
    2.7     7/00    O. Lawlor   Replaced doppler rate estimation with Curlander's.
    2.8     7/00    M. Ayers	Added Kaiser and Hamming windows for azimuth 
				reference function.
    2.81    7/01    R. Gens	Added logfile and quiet switch
    2.9	    8/01    R. Gens	Added power image switch
    2.91    9/01    S. Watts	Made sure input file is CCSD (not ceos)
				Looks for <name>.D 1st, then looks for
				<name>.raw if *.D is invalid. 

HARDWARE/SOFTWARE LIMITATIONS:
    This program requires large amounts of memory to run.  The main
    buffer is 
	      size(trans) = n_az * n_range * sizeof(FCMPLX)

              where  n_az = number of lines in a patch (azimuth samples)
		     n_range = number of lines in the azimuth (range samples) 

    n_az is defined is aisp_def.h as 4096, and a full swath of ERS CCSD
    data includes 5616 range samples, so size(trans) = 176 Mbytes.  When 
    this is combined with the rest of the storage requirements for the
    program, 200+ Mbytes are needed.

    Because of the 1000+ azimuth lines of overhead per patch, it is best
    NOT to decrease the defined value of n_az.  Rather, one should decrease
    n_range by processing fewer range bins at a time.  Regardless, n_az should
    always be a power of 2 because the FFTs operate best that way.

ALGORITHM DESCRIPTION:
    Deal with setting all of the parameters
    For each patch
	read the patch and range compress (rciq)
	transform via fft
	perform range migration (rmpatch)
	perform azimuth compression (acpatch)
	transpose the patch and write it in azimuth lines to output

ALGORITHM REFERENCES:
    This program and all subroutines were converted from Fortran programs
    donated by Howard Zebker, and extensively modified.

BUGS:

*****************************************************************************/
/****************************************************************************
*								            *
*   aisp - ASF Sar Processor (SAR Software Correlator)			    *
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


#include "asf.h"
#include "aisp_defs.h"

/*Usage:*/

void give_usage(char *name)
{
	printf("\nUsage:    %s [options] ifile ofile\n",name);
	printf("      ifile            input signal data file (.D & .L or .raw & .in)\n");
	printf("      ofile            output file name\n\n");
	printf("   This program creates a SAR image from SAR signal data.\n\n");
	printf("   The optional switches provide the ability to\n");
	printf("   override any or all of:\n\n");
	printf("        1) the default values given below,\n");
	printf("        2) parameters read from metadata (.D & .L),\n");
	printf("        3) parameters read from a parameter file (.raw and .in)\n\n");
	printf("   Argument     Default  Description\n");
	printf("   ----------------------------------------------------------------\n");
	printf("   -l first_line   1     First line to process (from 0)\n");
	printf("   -p patches      8     Number of patches to process (@ 4K lines)\n");
	printf("   -v valid_lines  3000  Valid output lines per patch\n");
	printf("   -s skip_samp    0     range samples to skip (of INVALID samps)\n");
	printf("   -f first_samp   0     1st range samp to process (of VALID samps)\n");
	printf("   -e 1            0     remove doppler skew from image (flag).\n");
	printf("   -n num_samps    META  Number of range samples to process\n");
	printf("                         (Default is read from metadata)\n");
	printf("   -r output_res   8.0   Desired output azimuth resolution (m)\n");
	printf("   -d dbg_flg      1     Debug: 1=amplitude,2=ref_fcn,4=rangemig\n");
	printf("                                8=rangecomp,16=rangespecs,64=acpatch\n");
	printf("   -c dfile        NO    Read doppler centroid from dfile\n");
	printf("   -o off_file     NO    Read resampling coeg.fs from off_file\n");
	printf("   -hamming 	   NO    Use a Hamming window instead of a rectangular one\n");
	printf("                         for the azimuth reference function weighting\n");
/*	printf("   -kaiser	   NO    Use a Kaiser window instead of a rectangular one\n");
	printf("                         for the azimuth reference function weighting\n"); */
	printf("   -m CAL_PARAMS   NO    Read the Elevation Angle and Gain vectors from the\n");
	printf("			 CAL_PARAMS file to correct for the antenna gain\n");
	printf("   -log logfile	   NO	 Allows output to be written to a log file\n");
	printf("   -quiet	   NO	 Suppresses the output to the essential\n");
	printf("   -power	   NO	 Creates a power image\n");
	printf("   -sigma	   NO	 Creates a sigma image\n");
	printf("   -gamma	   NO	 Creates a gamma image\n");
	printf("   -beta	   NO	 Creates a beta image\n");
	printf("\nVersion %3.1f, ASF SAR TOOL\n\n",VERSION);
	exit(1);
}

int main (int argc, char *argv [])
{
/*Structures: these are passed to the sub-routines which need them.*/
	patch *p;
	satellite *s;
	rangeRef *r;
	getRec *signalGetRec;
	file *f;
/*Variables.*/
	int n_az,n_range;/*Region to be processed.*/
	int patchNo;/*Loop counter.*/
	struct AISP_PARAMS params;
	meta_parameters *meta;
	
	StartWatch();
	system("date");
	printf("Program: aisp\n");

	logflag=quietflag=0;

	if (!parse_cla(argc,argv,&params,&meta))
		give_usage(argv[0]);

	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: aisp\n");
	}
	aisp_setup(&params,meta,&n_az,&n_range,&s,&r,&f,&signalGetRec);
	if (!quietflag) {
	  printf("   Processing %dx %d az by %d range patches...\n",f->nPatches,n_az,n_range);
	  printf("   Of the %d azimuth lines, only %d are valid.\n",n_az,f->n_az_valid);
	}
	
/*
Create "patch" of data.  This patch is re-used to process
all of the input data.
*/	
	p=newPatch(n_az,n_range);

/*Loop over each patch of data present, and process it.*/
	for (patchNo=1; patchNo<=f->nPatches; patchNo++)
	{
		int lineToBeRead;
		printf("\n   *****    PROCESSING PATCH %i    *****\n\n",patchNo);
		
		lineToBeRead = f->firstLineToProcess + (patchNo-1) * f->n_az_valid;
		if (lineToBeRead+p->n_az>signalGetRec->nLines) {
		  printf("   Read all the patches in the input file.\n");
		  if (logflag) printLog("   Read all the patches in the input file.\n");
		  break;
		}

		/*Update patch parameters for location.*/
		setPatchLoc(p,s,meta,f->skipFile,f->skipSamp,lineToBeRead);
		processPatch(p,signalGetRec,r,s);/*SAR Process patch.*/
		writePatch(p,s,meta,f,patchNo);/*Output patch data to file.*/
	} /***********************end patch loop***********************************/


	destroyPatch(p);
/*	printf("\nPROGRAM COMPLETED\n\n");*/
	
	if (logflag) {
	  if (f->nPatches==1) 
	    printLog("\n   Processed 1 patch.\n\n");
	  else {
	    sprintf(logbuf,"\n   Processed %d patches.\n\n", f->nPatches);
	    printLog(logbuf);
	  }
	  StopWatchLog(fLog);
	}
	StopWatch();
	return(0);
}
