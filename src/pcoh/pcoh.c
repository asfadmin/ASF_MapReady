/******************************************************************************
NAME:		pcoh - Calculates interferogram coherence 

SYNOPSIS:   pcoh [-l nl x ns] [-s step_line x step_sample] [-x <file>] In1 In2 Out

	-l 	set look box line and sample.  Default 15x3
	-s	set step boc line and sample.  Default 5x1
	-x	allows the output to be written to a log file
	In1	image 1 complex file
	In2 	image 2 complex file
	Out	Output file name (Out.img, Out.ddr)

DESCRIPTION:

     Calculates the coherence for the Igram using the following
     formula:
 
		|         Sum (n pixels) [ a x b* ] 	     |
         rho =  | __________________________________________ |
		|					     |
		|  Sqrt { Sum  [ a x a* ]  Sum  [ b x b* ] } |

 	the 'x' in the equation above indicates multiplication and
	the '*' indicates complex conjugation.
	In this program n = 45 pixels; 15 in azimuth x 3 in range.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/97    T. Logan    To compare our interferogram with H. Zebker's.
    1.1	    6/00    D. Koster	Accept files larger than 2GB
    1.2     7/00    M. Ayers    Fixed so that coherence is calculated correctly.
    1.3	    8/00    D. Koster	Fixed bug in seek position var that caused 
				  it to 'roll over' to 0.
    2.0	    8/00    T. Logan & 
		    D. Koster,  Modified for use with parallel processors.
    2.1	    9/00    M. Ayers 	Repaired bug in code to change the calculation from
                                Sum(|A x B*|) to |Sum(A x B*)| for the igram term
    2.2	    9/00    T. Logan	Fixed parallel bugs, reads nLooks from meta file
    2.3	    10/00   M. Ayers    Minor modifications
    2.4     3/01    T. Logan    Fixed parallel bug for nLooks bcast
    2.41    7/01    R. Gens	Added logfile switch
    2.42    10/01   T. Logan    rP is now 0.0 if denomProduct is 0


HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

*******************************************************************************
*                                                                             *
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
#include <mpi.h>
#include "ddr.h"
#include "asf_meta.h"
#include "ifm.h"

#define VERSION           2.42
#define STEP_LINE         5
#define STEP_SAMPLE       1
#define WIN_LINE          15
#define WIN_SAMPLE        3
#define WINDOW_SIZE	  3
#define EBYTE             1
#define MN 	  	  256
#define HIST_SIZE         10
#define MIN(a,b) 	((a)<(b) ? (a) : (b))

void pcoh_usage(int );
void bye(int, char *);

int
main(int argc, char *argv[])
{
	int 	 line;					/* Line index counter */
	float    *a1sum, *a2sum;			/* Sums for calculation */
	complexFloat *igram_sum;
	char     a1f[MN], a2f[MN], cohf[MN], ddrf[MN];	/* File name buffers */
	FILE 	 *fin1, *fin2, *fcoh;			/* File pointers */
	complexFloat *in1,  *in2;				/* Ptrs for complex vals */
	float    *rP, *rho;				/* Holds results */
	int	 ns = -1, nl = -1;			/* Num sample num lines */
	int 	 sl = STEP_LINE, ss = STEP_SAMPLE;	/* Line & sample step size */
	int 	 wl = WIN_LINE, ws = WIN_SAMPLE;	/* Line & sample window size */
	int 	 c;					/* ?? */

	struct DDR ddr,newddr;				/* Structure for holding ddr info */
	extern int optind;				/* Variable for the getopt() function */
	extern char *optarg;				/* Variable for the getopt() function */
	int my_start, my_end, linesPerPE;		/* Defines 'chunk' size for each processor. */
	int my_pe, n_pes;				/* my_pe: current processor
							   rank, 0 -> num processors - 1
							   n_pes: number of processing 
							   elements to use. */

	long long hist_val[HIST_SIZE];			/* Histogram value table */
	double hist_sum = 0.0;				/* Histogram sum */
	long long hist_cnt = 0;				/* Histogram count */
	int cnt, nPercent=5;				/* Counter variable */
	MPI_Status recv_status;				/* MPI variable */
	long long global_hist_val[HIST_SIZE];
	double global_hist_sum = 0.0;
	long long global_hist_cnt = 0; 
	float bin_high=0, bin_low=0;
	double percent=0, percent_sum=0;
	float average=0, max=0;
	long long out_offset;		


/* Initialize the MPI first */
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &my_pe);
	MPI_Comm_size(MPI_COMM_WORLD, &n_pes);
	
/*	if(my_pe==0)printf("Message Passing Interface initialized on Processors %d to %d\n",my_pe,n_pes-1);*/
	MPI_Barrier(MPI_COMM_WORLD);	
	
        if (my_pe == 0) {
          system("date");
          printf("Program: pcoh\n\n");
	}

/* Initialize the histogram table */
	for (cnt = 0; cnt < HIST_SIZE; cnt++) 
		hist_val[cnt] = (long long) 0;

	logflag=0;

/* Process CLA's */
	StartWatch();
        while ((c=getopt(argc,argv,"l:s:x:")) != EOF)
		switch(c) {
		case 'l':
			if (2!=sscanf(optarg,"%dx%d",&wl,&ws))
			{
			    if(my_pe == 0) {
			      MPI_Finalize();
			      sprintf(errbuf,"   ERROR: '%s' does not look like a line x sample (a good one would \
						be '5x1').\n",optarg);
 			      printErr(errbuf);
			    }
			}
			break;
		case 's': 
			if (2!=sscanf(optarg,"%dx%d",&sl,&ss))
			{
			    if (my_pe == 0)
			    	MPI_Finalize();
				sprintf("   ERROR: '%s' does not look like a line x sample (a good one would \
						be '5x1').\n",optarg);
			   	printErr(errbuf);
			}
			break;
		case 'x': 
			if (1!=sscanf(optarg, "%s", logFile)) {
			  if (my_pe == 0) 
				MPI_Finalize();
				printErr("   ERROR: No log file name given\n");
			}
			logflag=1;
			break;
		default : 
			pcoh_usage(my_pe);
			
		}

	if (argc-optind != 3) { pcoh_usage(my_pe); }

        if (logflag && (my_pe == 0)) {
          fLog = FOPEN(logFile, "a");
          StartWatchLog(fLog);
          printLog("Program: pcoh\n\n");
	}

/* Create the filenames */
	create_name(a1f, argv[optind],".cpx");
	create_name(a2f, argv[optind+1],".cpx");
	create_name(cohf,argv[optind+2],".img");
	create_name(ddrf,argv[optind+2],"");

/* Read nlooks from meta file if no CLA's were given */
	if (argc==4)
	  {
	    meta_parameters *meta;
	    if (extExists(argv[optind],".meta") || extExists(argv[optind+1],".meta"))
	    {
		if (my_pe == 0)
		{
	    	   if (extExists(argv[optind],".meta")) meta = meta_read(argv[optind]);
		   else meta = meta_read(argv[optind+1]);
	
                   sl = meta->ifm->nLooks;
		   ss = 1; 	     
		   if (sl == 1)
		     {
			/* We have fine-beam RSAT - make the window larger, 7x7 */
			wl = 7;
			ws = 7;
		     }
		   else
		     {	
                        wl = WINDOW_SIZE * sl;
		        ws = WINDOW_SIZE; 
		     }
                   meta_free(meta);
		}
		
		MPI_Bcast(&sl,1,MPI_INT,0,MPI_COMM_WORLD);
	        MPI_Bcast(&ss,1,MPI_INT,0,MPI_COMM_WORLD);
	        MPI_Bcast(&wl,1,MPI_INT,0,MPI_COMM_WORLD);
	        MPI_Bcast(&ws,1,MPI_INT,0,MPI_COMM_WORLD);
	   }
	   else { bye(my_pe,"   ERROR: Unable to either find or open metaFile."); }
	 }

/* Get the DDR information and fill up the new DDR structure */
	if (ns == -1 || nl == -1) {
		if (c_getddr(a1f, &ddr) != 0) 
		{
		  bye(my_pe, "Unable to get ddr file for igram");
		}
		nl = ddr.nl; 
		ns = ddr.ns;
		newddr=ddr;
		newddr.nl=nl/sl;
		newddr.ns=ns/ss;
		newddr.dtype=4;
		newddr.nbands=1;
		newddr.line_inc *= sl;
		newddr.sample_inc *= ss;
		newddr.pdist_x *= ss;
		newddr.pdist_y *= sl; 
		if(my_pe == 0)
			c_putddr(cohf,&newddr);
	}

/* Create buffers */
	rho   = (float*)MALLOC(sizeof(float)*ns/ss);
	in1  = (complexFloat*)MALLOC(sizeof(complexFloat)*ns*wl);
	in2  = (complexFloat*)MALLOC(sizeof(complexFloat)*ns*wl);
	a1sum = (float*)MALLOC(sizeof(float)*ns);
	a2sum = (float*)MALLOC(sizeof(float)*ns);
	igram_sum = (complexFloat*)MALLOC(sizeof(complexFloat)*ns);

/* File open */
	fin1 = fopenImage(a1f,"rb");
	fin2 = fopenImage(a2f,"rb");
	fcoh = fopenImage(cohf, "w+b");

/* Begin reading & processing the chunks of data. */
	
	MPI_Barrier(MPI_COMM_WORLD);
	if(my_pe == 0)
	{
/*		printf("Input Files are %s and %s\n",a1f,a2f);
		printf("   Input Size    => line: %5d  sample: %5d\n",nl,ns);
		printf("   Step Interval => line: %5d  sample: %5d\n",sl,ss);
		printf("   Window Size   => line: %5d  sample: %5d\n\n",wl,ws);*/
	}

/* Calculate the start and end lines for each processor */
	linesPerPE = nl / n_pes;
	my_start = my_pe * linesPerPE;


/* This line appears to really mess things up by putting segments between chunks */
	if(my_start % sl != 0) 
		my_start += sl - (my_start%sl); 
	
	my_end = (my_pe+1)*linesPerPE - 1;
	my_end += (sl-(my_end%sl))-1;

	if(my_pe == n_pes-1) my_end = nl;

/* For debugging */
/*        printf("PE %i: my_start = %i, my_end = %i\n",my_pe,my_start,my_end);  */

/* Set the output file pointer for each processor to the to the right spot */
        out_offset = (long long)((my_start/sl)*(ns/ss)*sizeof(float));                      
	FSEEK64(fcoh,out_offset, SEEK_SET);

/* Process the data line by line */
	for (line = my_start; line < my_end; line+=sl)
	{
		register int off,row,col,limitLine;
		register float denXYS1,denXYS2;
		complexFloat igram;
		int inCol;
		long long seek_pos=0;
		limitLine=MIN(wl,nl-line);

		rP = rho;
		if(my_pe == 0) {
		  if((line*100/linesPerPE) == nPercent) {
		    printf("   Completed %3d percent\n",nPercent);
		    nPercent+=5;
		  }
		}

		/*  read in the next lines of data */
		seek_pos = (long long) line*ns*sizeof(complexFloat);
		FSEEK64(fin1,seek_pos,SEEK_SET);
		FREAD(in1, sizeof(complexFloat)*limitLine*ns,1,fin1);
		
		FSEEK64(fin2,seek_pos,SEEK_SET);
		FREAD(in2, sizeof(complexFloat)*limitLine*ns,1,fin2);

#define amp(cpx) sqrt((cpx).real*(cpx).real + (cpx).imag*(cpx).imag)

		/* 
		 * Add the remaining rows into sum vectors
 		 */
		off=ns;
		for (col=0; col<ns; col++)
		{
			off=col;
			denXYS1=0.0;
			denXYS2=0.0;
			igram.real=0.0;
			igram.imag=0.0;

			for (row=0; row<limitLine; row++)
			{
				igram.real += in1[off].real*in2[off].real + in1[off].imag*in2[off].imag;
				igram.imag += in1[off].imag*in2[off].real - in1[off].real*in2[off].imag;
				denXYS1 += amp(in1[off])*amp(in1[off]);
				denXYS2 += amp(in2[off])*amp(in2[off]);
				off+=ns;
			}
			igram_sum[col] = igram;			
			a1sum[col] = denXYS1;
			a2sum[col] = denXYS2;
		} /* end for col < ns */
		
		/* 
		 * Calculate the coherence by adding from sum vectors
		 */
		for (inCol=0; inCol<ns; inCol+=ss)
		{
			register float  denomProduct;
			register int limitSample=MIN(ws,ns-inCol);
			denXYS1=0.0;
			denXYS2=0.0;
			igram.real=0.0;
			igram.imag=0.0;

			/* 
			 * step over coherence area; Sum output columns
			 */
			for (col = 0; col < limitSample; col++)
			{
				igram.real += igram_sum[inCol+col].real;
				igram.imag += igram_sum[inCol+col].imag;
				denXYS1 += a1sum[inCol+col];
				denXYS2 += a2sum[inCol+col];
			}
			denomProduct=denXYS1*denXYS2;
			
			if (denomProduct == 0.0)
				*rP = 0.0;
			else 
			{
				*rP = (float)Cabs(igram)/sqrt(denomProduct);
				if (*rP>1.0001)
				{ 
					printf("   coh = %f -- setting to 1.0\n",*rP);
					printf("   You shouldn't have seen this!\n");
					bye(my_pe,"   Exiting");
					*rP=1.0;
				}
			}
			rP++;
		} /* End for inCol */

		/*
		 * Dump out coherence values for this line.
		 */
		FWRITE(rho,sizeof(float),ns/ss,fcoh);

		for (cnt = 0; cnt <ns/ss; cnt++)
		 {
			register int tmp;
			tmp = (int) (rho[cnt]*HIST_SIZE); /* Figure out which bin this value is in */

			/* This shouldn't happen */
			if(tmp >= HIST_SIZE) tmp = HIST_SIZE-1;
			if(tmp < 0) tmp = 0;

			hist_val[tmp]++;		 /* Increment that bin for the histogram */
			hist_sum += rho[cnt];		 /* Add up the values for the sum */
			hist_cnt++;			 /* Keep track of the total number of values */
			if (rho[cnt]>max) max=rho[cnt];	 /* Calculate maximum coherence */
		 }
	} /* End for line this group of lines */
	
	/*  
	 * Free and halt. 
	 */
	FREE(rho); 
	FREE(in1); 
	FREE(in2);
	FCLOSE(fcoh); 
	FCLOSE(fin1); 
	FCLOSE(fin2);
/*	if(my_pe == 0) { printf("Finished with Coherence Calculation, Beginning Histogram\n"); StopWatch(); }*/
	MPI_Barrier(MPI_COMM_WORLD);

/* Sum the statistics */
	for (cnt = 0; cnt < HIST_SIZE; cnt++) global_hist_val[cnt] = 0;		
	
	MPI_Reduce(hist_val,global_hist_val,HIST_SIZE,MPI_LONG_LONG_INT,MPI_SUM,0,MPI_COMM_WORLD);
	MPI_Reduce(&hist_sum,&global_hist_sum,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);
	MPI_Reduce(&hist_cnt,&global_hist_cnt,1,MPI_LONG_LONG,MPI_SUM,0,MPI_COMM_WORLD);
	
	MPI_Barrier(MPI_COMM_WORLD);
	if (my_pe == 0)
	{
		printf("   Completed 100 percent\n\n");
		printf("   Wrote %d bytes of data\n", ns*nl*4/sl);
		printf("\n   Coherence     :  Occurances  :  Percent\n");
		printf("   ---------------------------------------\n");
		if (logflag) {
		  sprintf(logbuf,"   Wrote %d bytes of data\n\n", ns*nl*4/sl);
		  printLog(logbuf);
		  printLog("   Coherence     :  Occurances  :  Percent\n");
		  printLog("   ---------------------------------------\n");
		}
		for (cnt = 0; cnt < HIST_SIZE; cnt++)
		{
			bin_low = (float)(cnt)/(float)(HIST_SIZE);
			bin_high = (float)(cnt+1)/(float)(HIST_SIZE);
			percent = (double)(global_hist_val[cnt])/(double)(global_hist_cnt);
			printf("    %.2f -> %.2f :   %.8lld       %.3lf \n",bin_low,bin_high, (long long)
global_hist_val[cnt],100*percent); 
			if (logflag) {
			  sprintf(logbuf,"    %.2f -> %.2f :   %.8lld       %.3lf \n",bin_low,bin_high, (long long)
global_hist_val[cnt],100*percent);
			  printLog(logbuf);
			} 
			percent_sum+=(float)100*percent;
		}
		average = (float)(global_hist_sum)/(float)(global_hist_cnt);
		printf("   ---------------------------------------\n");
		printf("   Maximum Coherence: %.3f\n", max);
		printf("   Average Coherence: %.3f  (%.1f / %lld) %f\n\n",average, 
global_hist_sum,global_hist_cnt,percent_sum);
		if (logflag) {
		  printLog("   ---------------------------------------\n");
		  sprintf(logbuf,"   Maximum Coherence: %.3f\n", max);
		  printLog(logbuf);
		  sprintf(logbuf,"   Average Coherence: %.3f  (%.1f / %lld) %f\n\n",average, 
global_hist_sum,global_hist_cnt,percent_sum);
		  printLog(logbuf);
		  StopWatchLog(fLog);
		}
		StopWatch();
	}
 
	MPI_Finalize();
	return(0);
}

void pcoh_usage(int my_pe)
{
    if (my_pe == 0)
      {
	printf("\n");
	printf("coh: A Correlation Calculator to Estimate Interferogram Quality\n");
	printf("\n");
	printf("Usage:\n");
	printf("  coh [-l line x sample] [-s line x sample] [-x <file>] In1 In2_corr Out\n");
	printf("\n");
	printf("-l\t\tchange look box line and sample. Default = %dx%d\n",WIN_LINE,WIN_SAMPLE);
	printf("-s\t\tchange step box line and sample. Default = %dx%d\n",STEP_LINE,STEP_SAMPLE);
	printf("-x\t\tallows the output to be written to a log file\n");
	printf("In1\t\timage 1 complex file\n");
	printf("In2_corr\tcoregistered image 2 complex file\n");
	printf("Out\t\tfloating-point coherence file (.img).\n");
	printf("  Version %4.2f, ASF InSAR Tools.\n",VERSION);
	printf("\n");
      }
    MPI_Finalize();
    exit(EXIT_FAILURE);
}

void bye(int my_pe, char *msg) { 
	if (my_pe==0) fprintf(stderr,"   %s\n",msg); 
	MPI_Finalize();
	exit(1);
}
