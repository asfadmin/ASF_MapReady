/*****************************************************************************
NAME: aisp -- patch mode SAR processor

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
    2.6         7/01    R. Gens		Added log file and quiet switch
    2.7		8/01	R. Gens		Added power image calculation
    2.71    9/01	S. Watts	Make sure infile is CCSD not ceos.
					Looks for *.raw if *.D is invalid.

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
#include "asf.h"
#include "paisp_defs.h"
#include <mpi.h>

int my_pe;
int n_pes;


/*Usage:*/

void give_usage(char *name)
{
 if (IM_DSP) {
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
	printf("   -p patches      8     Number of patches to process\n");
	printf("   -a azi_lines	   4096  Number of azimuth lines per patch\n");
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
	printf("   -h hamming	   NO    Use a Hamming window on the az. ref. func.\n");
/*	printf("   -k kaiser	   NO    Use a Kaiser  window on the az. ref. func.\n"); */
	printf("   -m CAL_PARAMS   NO    Read the Elevation Angle and Gain vectors from the\n");
        printf("                         CAL_PARAMS file to correct for the antenna gain\n");
	printf("   -x LOGFILE	   NO    Allows the output to be written to a log file\n");
	printf("   -q 1		   NO    Suppresses the output to the essential\n");
	printf("   -z 1		   NO	 Calculates power image\n");
	printf("\n   Version %3.1f, ASF Tools\n\n\n",VERSION);
 }
	
	MPI_Finalize();
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

	MPI_Init(&argc,&argv);
	StartWatch();

        MPI_Comm_size(MPI_COMM_WORLD, &n_pes);
        MPI_Comm_rank(MPI_COMM_WORLD, &my_pe);

	logflag=quietflag=0;

	if (!parse_cla(argc,argv,&params,&meta)) give_usage(argv[0]);

	if (IM_DSP) {
	    system("date");
	    printf("Program: paisp\n");
	}
	if (logflag && IM_DSP) {
	    StartWatchLog(fLog);
	    printLog("Program: paisp\n");
	}

	aisp_setup(&params,meta,&n_az,&n_range,&s,&r,&f,&signalGetRec);

	if (f->nPatches == 1000)
	  f->nPatches = signalGetRec->nLines / f->n_az_valid;

	if (IM_DSP)
 	  {
	    FILE *fp;
	    if (!quietflag) {
  	      printf("\n   Processing %dx %d az by %d range patches...\n",f->nPatches,n_az,n_range);
	      printf("   Of the %d azimuth lines, only %d are valid.\n",n_az,f->n_az_valid);
	    }

	    /* Zero-out or create output files */
	    fp = FOPEN(f->out_cpx,"wb"); FCLOSE(fp);	
	    fp = FOPEN(f->out_amp,"wb"); FCLOSE(fp);
	    if (s->imageType.power){fp = FOPEN(f->out_pwr,"wb"); FCLOSE(fp);}	
	    if (s->imageType.sigma){fp = FOPEN(f->out_sig,"wb"); FCLOSE(fp);}
	    if (s->imageType.gamma){fp = FOPEN(f->out_gam,"wb"); FCLOSE(fp);}
	    if (s->imageType.beta) {fp = FOPEN(f->out_bet,"wb"); FCLOSE(fp);}
	  }
	
	
/*
Create "patch" of data.  This patch is re-used to process
all of the input data.
*/	
	p=newPatch(n_az,n_range);

/*Loop over each patch of data present, and process it.*/
	for (patchNo=1; patchNo<=f->nPatches; patchNo+=n_pes)
	{
  	  int lineToBeRead;

	  if (IM_DSP)
	    {
	     if (patchNo+n_pes-1 > f->nPatches) 
	       printf("\n   *****     PROCESSING PATCHES %i TO %i   *****\n",patchNo,f->nPatches);
	     else
	       printf("\n   *****     PROCESSING PATCHES %i TO %i    *****\n",patchNo,patchNo+n_pes-1);
	    }
	
	  lineToBeRead = f->firstLineToProcess + (patchNo-1) * f->n_az_valid;

	  if (lineToBeRead+p->n_az>signalGetRec->nLines)
	    {
	     if (IM_DSP) printf("   Read all the patches in the input file.\n"); 
	     if (logflag && IM_DSP) printLog("   Read all the patches in the input file.\n"); 
	     break;
	    }
	  else
	    lineToBeRead += my_pe*f->n_az_valid; 

	  if ((lineToBeRead+p->n_az<=signalGetRec->nLines) && ((patchNo + my_pe) <= f->nPatches))
	   { 
	    setPatchLoc(p,s,meta,f->skipFile,f->skipSamp,lineToBeRead,f->firstLineToProcess);/*Update patch.*/
	    processPatch(p,signalGetRec,r,s); /*SAR Process patch.*/
	    writePatch(p,s,meta,f,patchNo+my_pe);  /*Output patch data to file.*/
	   }
	  else 
	   {
	    /* Even if no more data is left, all processors  
	       Still must participate in global reduce mpi calls */

	    int final_line = 0;
	    int max_line = 0;

            MPI_Reduce(&final_line, &max_line, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	    MPI_Reduce(&final_line, &max_line, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
            if (s->imageType.power) MPI_Reduce(&final_line, &max_line, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	    if (s->imageType.sigma) MPI_Reduce(&final_line, &max_line, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	    if (s->imageType.gamma) MPI_Reduce(&final_line, &max_line, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
	    if (s->imageType.beta)  MPI_Reduce(&final_line, &max_line, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD); 
	    
	   }


	  MPI_Barrier(MPI_COMM_WORLD);
	} /***********************end patch loop***********************************/

	destroyPatch(p);	
/*	if (IM_DSP) printf("\nPROGRAM COMPLETED\n\n");*/

	MPI_Barrier(MPI_COMM_WORLD);
	if (IM_DSP) {
		printf("\n");
		StopWatch();
	}
	if (IM_DSP && logflag) {
		sprintf(logbuf,"\n   Number of patches processed: %d\n\n",f->nPatches);
		printLog(logbuf);
		StopWatchLog(fLog);
	}
	MPI_Finalize();

	return(0);
}
