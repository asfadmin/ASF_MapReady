/*****************************************************************************
NAME: ardop -- patch mode SAR processor

SYNOPSIS:     ardop [options] ifile ofile

              ifile   input ASF CCSD (.D & .L), or raw file (.raw & .in)
                      of raw SAR signal data.
              ofile   output file name.  ARDOP will create ofile_amp.img,
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
    2.0		8/98	O. Lawlor	Globals now confined to ardop_setup.c.
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
    3.0	    10/02   J. Nicoll	Made calibrateable, added beta, sigma, gamma 
    				products. Fixed deskew to exclude data wedges.
    3.1     6/03    J. Nicoll   Expanded debug options and change debug flags.			

HARDWARE/SOFTWARE LIMITATIONS:
    This program requires large amounts of memory to run.  The main
    buffer is 
	      size(trans) = n_az * n_range * sizeof(complexFloat)

              where  n_az = number of lines in a patch (azimuth samples)
		     n_range = number of lines in the azimuth (range samples) 

    n_az is defined is ardop_def.h as 4096, and a full swath of ERS CCSD
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


#include "asf.h"
#include "ardop_defs.h"

int ardop(struct ARDOP_PARAMS * params)
{
        meta_parameters *meta;

/*Structures: these are passed to the sub-routines which need them.*/
	patch *p;
	satellite *s;
	rangeRef *r;
	getRec *signalGetRec;
	file *f;

/*Variables.*/
	int n_az,n_range;/*Region to be processed.*/
	int patchNo;/*Loop counter.*/

        meta = meta_read(params->in1);
	ardop_setup(params,meta,&n_az,&n_range,&s,&r,&f,&signalGetRec);
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
		writePatch(p,s,f,patchNo);/*Output patch data to file.*/
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

	return(0);
}
