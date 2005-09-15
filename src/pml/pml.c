/****************************************************************
NAME:  pml

SYNOPSIS: 
	multilook a deramped interferogram (parallel implementation)

DESCRIPTION:
	Multilook is a low pass filter which also decreases the image 
	dimensions by performing a box average of some number of pixels.
	Whereas amplitude SAR images are typically multilooked in a 
	root-mean-square or power domain sense, SLC images are multilooked by
	simply adding up the constituent pixels in a box.  For ERS-1 the box 
	is often some multiple of 1 pixel in range x 5 in azimuth in order 
	to produce multilooked pixels with near-unity aspect ratio.

	Changes: 
	Look area is now variable with default set to 1 col and 5 rows, equal
	to the step area. For noisy data, step at 2 cols by 10 rows. Output
	amp. and phase files are still float values. The new code comes from 
	mldata() used by amp2img.
        
	Calculating Multilooked Amp:
	Add the square of each amp. in look area. Divide by the number of
	amp. entries. Take square root of this value.

	Calculating Multilooked Phase:
	Add all the real parameters of each entry in look area. Add all the
	imag. parameters of each entry in look area. Take atan2() of imag
	over real.

	Multilook creates an amp, phase, and a corresponding LAS image file
	including a DDR file. The image file has byte data in 3 bands. The
	first band corresponds to red, the second to green, and the third to
	blue. The bands are in sequential order.


PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0			Rob Fatland - Original Development
    1.1                 M. Shindle - Revise & Clean
    2.0                 M. Shindle - Allow multilooking on variable size
			window. ML will still move in 1 x 5 step, but can now
			look at an area of StepLine and StepSample. AmpOut
			and PhaseOut are now byte images instead of float. 
    3.0                 Modify StepSline & StepSample
    3.2			O. Lawlor - Get image size from DDR.
     "     07/11/97     D.Corbett - updated version number
    3.3    10/24/97     Bug fix - now allows ss <> 1 & fixed for loop for 
			Sin & Cos table creation. 
    3.4			O. Lawlor - Updated CLA's.
    3.5			M. Ayers  - Almost Complete rewrite to make it 
			readable, parallel implementation
    3.6 		M. Ayers - Fixed the amplitude calculation to use
			Cabs(z) instead of sqrt(amp^2*ampScale)


HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/******************************************************************************
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
#include "las.h"
#include "ifm.h"
#include "asf_meta.h"
#include "pml.h"
#include <mpi.h>

/* local constants */
#define VERSION      3.6
#define MPI_IN_LEN 1000		/* Set the block size for each processor (in lines)*/

/* function declarations */
void parse_cla(int, char **,int *,int *,int, int *);
int c2i(float*,float*,RGBDATA *,RGBDATA *,int,float);
void print_usage();

/* external variables for the usage subroutine */
extern int optind;
extern char *optarg;

int main(int argc, char *argv[])
{
	char fnm1[BUF],fnm2[BUF],fnm3[BUF],fnm4[BUF];	/* Character filename buffers */
	char imgfile[BUF],outname[BUF];			/* Character filename buffers */
	FILE *fiamp, *fiphase, *foamp, *fophase,*flas;	/* File Pointers */
	
	int inWid, inLen;				/* Input image length and width */
	int lookLines, lookSamples;			/* Look Line and Sample */
	int i,line,sample,band;				/* Loop counters and indexing arguments */
	int row, col;					/* Loop counters and indexing arguments */
	int outWid, outLen;				/* Output image length and width */
	int ds;						/* Input data size, number of samples read so far.*/
	int mem1,mem2,mem4,mem5,mem6;			/* Memory size variables, just to notify the user */
	int num_prcsrs, prcsr_rank;			/* MPI variables */
	int mpi_offset;					/* MPI seek offset */ 
	int MPI_OUT_LEN,outChunkLen;			/* Variables to deal with how big the output is */
	int numChunksInt, chunkLen, lastChunk, chunk;	/* Variables to deal with which chunk to process */
	int lasFlag=1;					/* Flag to tell whether or not to include the amp in the _las.img*/	

	float *ampIn, *phaseIn, ampScale;		/* Input buffers*/
	float *ampOut, *phaseOut, Sin[256], Cos[256];	/* Output buffers, Sine and Cosine Tables */
	float *ampBuf;					/* A buffer in case you just want a color phase image */
	float avg;					/* Average value variable */
	float numChunksFloat;				/* Number of chunks of data */
	const float convers=256.0/(2*3.14159265358979);	/* Conversion from a Byte to Modulo 2*pi */

	RGBDATA *table, *imgData;			/* Color image buffers */
	Uchar *bandPtr;					/* Pointer to which band we are processing */
	struct DDR ddr,newddr;				/* DDR structure pointers */
	
	long long in_offset, out_offset;		/* 64 bit offset variables */	

/* Initialize the MPI by calling these routines */
	MPI_Init(&argc,&argv);
	MPI_Comm_size(MPI_COMM_WORLD,&num_prcsrs);
	MPI_Comm_rank(MPI_COMM_WORLD,&prcsr_rank);
	if(prcsr_rank==0) {
	  system("date");
	  printf("Program: pml\n\n");
/*          printf("MPI initialized, using %d processors\n",num_prcsrs);*/
	}

	logflag=0;

/* Check command line args */
	StartWatch();
	parse_cla(argc, argv, &lookLines, &lookSamples, prcsr_rank, &lasFlag);

/* Wait here to make sure everyone got through the parse_cla() call */
	MPI_Barrier(MPI_COMM_WORLD);

/* Create filenames and open files for reading */
	c_getddr(argv[optind],&ddr);
	
	inWid=ddr.ns;
	inLen=ddr.nl;
	MPI_OUT_LEN=MPI_IN_LEN/lookLines;
	
	strcpy(outname,argv[optind+1]);
	create_name(fnm1,argv[optind],".amp");
	create_name(fnm2,argv[optind],".phase");
	create_name(fnm3,argv[optind+1],".amp");
	create_name(fnm4,argv[optind+1],".phase");
	create_name(imgfile,argv[optind+1],"_las.img");

	if(logflag && prcsr_rank==0) {
	  fLog = FOPEN(logFile, "a");
	  StartWatchLog(fLog);
	  printLog("Program: pml\n\n");
	}

/* Open the Input and Output Image Files */  
	fiamp   = fopenImage(fnm1,"rb");
	fiphase = fopenImage(fnm2,"rb");
	foamp   = fopenImage(fnm3,"wb");
	fophase = fopenImage(fnm4,"wb");
	flas    = fopenImage(imgfile,"wb");
  
/* Get Mean From Input Amplitude File */
	avg = get_mean(fiamp,inWid,inLen,TRUE);
  
/*********************************
 * Create chunk size parameters  *
 *********************************/
	chunkLen       = MPI_IN_LEN;
	outChunkLen    = MPI_OUT_LEN;
	numChunksFloat = (float)inLen/MPI_IN_LEN;
	numChunksInt   = (int)inLen/MPI_IN_LEN;
	lastChunk      = (int) ((numChunksFloat-numChunksInt)*MPI_IN_LEN+0.5);

	outWid=inWid/lookSamples;
	outLen=MPI_OUT_LEN*numChunksInt+(int)(lastChunk/lookLines+0.5);
	
	if(prcsr_rank==0)
	{
		printf("   Input size  is %d X %d\n",inLen,inWid);
		printf("   Output size is %d X %d\n\n",outLen,outWid);
/*		printf("Each chunk is %d lines, the last one is %d\n",MPI_IN_LEN,lastChunk);*/
	}
	if(logflag && (prcsr_rank==0))
	{
		sprintf(logbuf,"   Input size  is %d X %d\n",inLen,inWid);
		printLog(logbuf);
		sprintf(logbuf,"   Output size is %d X %d\n\n",outLen,outWid);
		printLog(logbuf);
	}

/* Build a Table of Values for Sine and Cosine */
	for (i=0;i<256;i++)
	{
		float phas=((float)i)/256.0*(2*3.14159265358979);
		Sin[i]=sin(phas);
		Cos[i]=cos(phas);
	}
  
/* Allocate some serious memory */
	ampScale = 1.0/lookLines;

	ds       = sizeof(float);
	ampIn    = (float *)MALLOC(ds*MPI_IN_LEN*inWid);
	phaseIn  = (float *)MALLOC(ds*MPI_IN_LEN*inWid);
	ampOut   = (float *)MALLOC(ds*MPI_OUT_LEN*outWid);
	ampBuf   = (float *)MALLOC(ds*MPI_OUT_LEN*outWid);
	phaseOut = (float *)MALLOC(ds*MPI_OUT_LEN*outWid);
	table    = (RGBDATA *)MALLOC(sizeof(RGBDATA)*MAXENTRIES);
	imgData  = (RGBDATA *)MALLOC(sizeof(RGBDATA)*MPI_OUT_LEN*outWid);
	bandPtr  = (Uchar *)MALLOC(sizeof(Uchar)*MPI_OUT_LEN*outWid);

	/* Memory Notification */
	mem1=ds*MPI_IN_LEN*inWid;
	mem2=ds*MPI_OUT_LEN*outWid;
	mem4=sizeof(RGBDATA)*MAXENTRIES;
	mem5=sizeof(RGBDATA)*MPI_OUT_LEN*outWid;
	mem6=sizeof(Uchar)*MPI_OUT_LEN*outWid;
/*	if(prcsr_rank==0)
		printf("You've just allocated %d MB's of memory\n",
			(2*mem1+2*mem2+mem4+mem5+mem6)*num_prcsrs/1000000);	*/

/* Create a Colortable to be Used With c2i */
	colortable(table);
  
/* Wait for all of the processors to catch up 
	if(prcsr_rank==0) printf("Waiting for all the processors to catch up\n");*/
	MPI_Barrier(MPI_COMM_WORLD);

/****************************************************************
 * Run through all the chunks of data using as many processors	*
 * as necessary until there are no more chunks to process	*
 ****************************************************************/

	for(chunk=0; chunk<(numChunksInt/num_prcsrs)+1; chunk++) 
	{
		/* Set the mpi_offset so that each processor knows its rank after each loop */
		mpi_offset    = prcsr_rank+(num_prcsrs*chunk);

		if(mpi_offset<numChunksInt+1)
		{
			/* 64 Bit File offset numbers */
			in_offset  = (long long) MPI_IN_LEN*inWid*sizeof(float)*mpi_offset;
			out_offset = (long long) MPI_OUT_LEN*outWid*sizeof(float)*mpi_offset;

			FSEEK64(fiamp,in_offset,SEEK_SET);
			FSEEK64(fiphase,in_offset,SEEK_SET);	
			
			FSEEK64(foamp,out_offset,SEEK_SET);
			FSEEK64(fophase,out_offset,SEEK_SET);

			/* Check to see if we are on the last chunk */
    			if(mpi_offset==numChunksInt)
			{
/*				printf("Last Chunk is %d Lines\n",lastChunk);*/
				chunkLen    = lastChunk;
				outChunkLen = lastChunk/lookLines;
			}

			/* Read in a data chunk */
			FREAD(ampIn,ds,chunkLen*inWid,fiamp);
			FREAD(phaseIn,ds,chunkLen*inWid,fiphase);

			/* Loop through the whole data buffer */
			for (line=0; line<outChunkLen; line++)
			{ 				
				int outOffset = line*outWid;

				for (sample=0;sample<outWid;sample++)
				{
					/* Set these variables to zero since they are local summations */
					complexFloat z;         /* Complex number pointer */
					float register ampSqrd; /* High speed variables */

					z.real=0.0;
					z.imag=0.0;
					ampSqrd=0.0;
				
					/* Do the actual multi-look operation here */
					for(col=0;col<lookSamples;col++)
					{
					  register int offset = (line*inWid*lookLines)+(sample*lookSamples)+col;

					  for(row=0;row<lookLines;row++)
					  {
						register float ampI;
						register int   index;	

						ampI=ampIn[offset];
						index=0xFF&((int)(phaseIn[offset]*convers));
						ampSqrd += ampI*ampI;
						z.real += ampI * Cos[index];
						z.imag += ampI * Sin[index];
						offset+=inWid;
					  }
					}
					
					/* Place each sample in the output buffer */
					ampOut[outOffset+sample] = Cabs(z)*ampScale;
					phaseOut[outOffset+sample] = Cphase(z);
					if(lasFlag == 0)
						ampBuf[outOffset+sample]=avg*1.5;
					else
						ampBuf[outOffset+sample]=ampOut[outOffset+sample];
				}
				/* Convert Amp & Phase to RGB, operating on each line. */
				if(!c2i(&ampBuf[outOffset], &phaseOut[outOffset],
					&imgData[outOffset], table,outWid,avg))
					Exit("ml: Error in c2i()");

			}
			/* Write out amplitude and phase data to file */
			FWRITE(ampOut,ds,outWid*outChunkLen,foamp);
			FWRITE(phaseOut,ds,outWid*outChunkLen,fophase);   
		
			/* Write out the pretty color image to file*/
  			for (band=0;band<3;band++)
			{
				for (i=0;i<outChunkLen*outWid;i++)
					bandPtr[i] = *((char *)imgData+(i*sizeof(RGBDATA))+ band);
				/* Watch out here, the file indexing is kind of tricky, each processor write one band in a 
				   different spot in the file */
				FSEEK64(flas,(long long)((out_offset/sizeof(float))*sizeof(char)+(band*outLen*outWid*sizeof(char))),SEEK_SET);
				FWRITE(bandPtr,1,outWid*outChunkLen,flas);
			}
			
/*			printf("\tWrote chunk %d of %d\n",mpi_offset+1,numChunksInt+1);
			if(prcsr_rank==0) {
	 			printf("Completed %.0f percent\n", percent);
			}*/
    
		} /* End of IF Statement */
		else
		{
			/* Make the processors that didn't get a chunk of data wait here 
			printf("Not using Processor %d\n",prcsr_rank);*/
		}

	} /* End of FOR loop */
	
	/* Wait for everyone to catch up here */
	MPI_Barrier(MPI_COMM_WORLD);  


	if(prcsr_rank==0)
	 {
		/*Create the DDR for the amplitude and phase.*/
		newddr=ddr;
		newddr.nl=outLen;
		newddr.ns=outWid;
		newddr.dtype=EREAL;
		newddr.nbands=1;
  		newddr.line_inc*=lookLines;
  		newddr.sample_inc*=lookSamples;
  		newddr.pdist_x*=lookSamples;
  		newddr.pdist_y*=lookLines;
		c_putddr(outname,&newddr);
  
		/*Create 3-band image's DDR.*/
		newddr.dtype=EBYTE;
		newddr.nbands=3;
		c_putddr(imgfile,&newddr);
	 }
  
/* Free all the memory that we used */
	FREE(ampIn);
	FREE(phaseIn);
	FREE(ampOut);
	FREE(phaseOut);
	FREE(table);
	FREE(bandPtr);
	FREE(imgData);

/* Close all of our files */
	FCLOSE(fiamp);
	FCLOSE(fiphase);
	FCLOSE(foamp);
	FCLOSE(fophase);
	FCLOSE(flas);

	if(prcsr_rank==0)
		StopWatch();
	if(logflag && (prcsr_rank==0))
		StopWatchLog(fLog);

	MPI_Finalize();	
	exit(0);
}

void parse_cla(int argc, char *argv[], int *ll, int *ls, int prcsr_rank, int *lasFlag)
{
	
	int c; 			/* Switch Variable */
	char metaFileName[256];	/* Meta-file name */
	meta_parameters *meta;	/* Meta-parameters structure */
  
/* Set variables to default */
	*ll = LOOKLINE;
	*ls = LOOKSAMPLE;

/* Grab any command line optons */
	while ((c=getopt(argc,argv,"l:n:ax:")) != EOF)
	switch (c)
	{
		case 'l':
			if (2!=sscanf(optarg,"%dx%d",ll,ls))
			{
				if(prcsr_rank==0)
				sprintf(errbuf,"   ERROR: '%s' does not look like a line x sample (a good one would \
						be '5x1').\n",optarg);
				printErr(errbuf);
			}
			break;
		case 'n':
			if (1!=sscanf(optarg,"%s",metaFileName))
                        {
                                sprintf(errbuf,"   ERROR: Unable to parse file name '%s'.\n",optarg);
                                printErr(errbuf);
                        }
                        if (extExists(metaFileName,".meta")) /*Read .meta file if possible*/
                        {
				meta = meta_read(metaFileName);
                                *ll = meta->ifm->nLooks; *ls = 1;
/*                                if(prcsr_rank == 0)
					printf("Number of looks read from metafile, looking %dX%d\n",*ll,*ls);*/
				
                                meta_free(meta);
                        }
                        else
                        {
                                sprintf(errbuf,"   ERROR: Unable to either find or open metaFile.\n");
                                printErr(errbuf);
                        }
                        break;
 
		case 'a':
			*lasFlag = 0;
			if(prcsr_rank==0)
				printf("   Will print a color phase image without the amplitude\n");
			break;
                case 'x':
                        if (1!=sscanf(optarg,"%s",logFile))
                        {
                                sprintf(errbuf,"   ERROR: Unable to parse file name '%s'.\n",optarg);
                                printErr(errbuf);
                        }
			logflag=1;
			break;

		default:
			if(prcsr_rank==0) 
			  print_usage();
			MPI_Finalize();
			exit(1);
	}
	switch (argc-optind)
	{
		case 2:
			return;
		case 3:
			return;
		default:
			if(prcsr_rank==0) 
				print_usage();
			MPI_Finalize();
			exit(1);
	}

}

void print_usage()
{
	printf("\nUsage:\npml [-l lxs] [-n METAFILE] [-a] [-x LOGFILE] <interferogram> <ml>\n");
	printf(" -l changes the look box to l by s. Default is %dx%d\n",LOOKLINE,LOOKSAMPLE);
	printf(" -n read the number of looks from METAFILE\n");
	printf(" -a produce a color phase image _las.img without the amplitude added in\n");
	printf(" -x allows the output to be written to a log file\n");
	printf(" <interferogram>.amp is a float amplitude file,\n\
	<interferogram>.phase is a float phase file.\n");
	printf(" <ml> is a multilook 'base' file name; the program will produce\n");
	printf("   four output files:  <ml>.amp and <ml>.phase\n");
	printf("                       <ml>.img and <ml>.ddr\n\n");
	printf("Parallel Multilook will do two things: it will shrink the image\n\
	vertically to make its aspect ratio 1.0, and it can apply a low\n\
 	pass filter over the image, to remove speckle.\n");
 	printf("Version: %.2f, ASF IFSAR Tools\n",VERSION);
}

