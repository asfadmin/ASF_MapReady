/*****************************************************************************
NAME: quicklook - SPECAN SAR processor

SYNOPSIS: quicklook [ -k <nLooks> ] [ -q <quality> ] [-a <aspect ratio>]
        <input signal> <output image>

DESCRIPTION:

    This program takes a raw Computer Compatible Signal Data (CCSD)
    file as input, and creates a low-resolution SAR amplitude image.

    The process is *much* faster than conventional SAR processing
    (e.g. AISP); but the output image is grainy and much smaller.
    You can make the image slightly less grainy by using more
    (range) looks; and make the image bigger by using -q.
    
    The algorithm used is SPECAN- the SPECtral ANalysis approach
    to SAR processing.  SPECAN works the same way in both range
    and azimuth-- in fact, both directions use the same code,
    deep down (in specan.c & .h).
    

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    The basic call sequence is:
   
>main.c figures out the various chirp parameters and calls specan_file.c:
	>specan_file.c (which can be make MPI-parallel by setting "MPI=1")
	 loops through each azimuth patch of data and calls
		>specan_process_patch (in specan_patch.c) figures out
		 how to break patch into patchlets, calls 
		 	>specan_process (in specan.c)
		 to process the data in range, then azimuth.  It then
		 averages each patchlet together using
		 	>specan_ml_look (in specan_ml.c)
		 and returns a smooth amplitude image.


FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0		11/98	O. Lawlor	Needed capability for LZP browse.
    2.5		3/99	O. Lawlor	Integrated with AISP parse_cla.c

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
    Set all parameters
    For each azimuth patch
    	read in signal data for this patch
    	For each range patchlet (multiple looks -> more range patchlets, 
    	                         whose outputs overlap & are averaged together)
    		"Deramp" lines of patchlet by negative range chirp slope
    		FFT lines of patchlet, throwing away wasted bandwidth
    		"Deramp" columns of patchlet by negative azimth chirp slope
    		FFT columns of patchlet, thowing away more wasted bandwidth
    		Average this patchlet's output into output buffer
    	end for
    	Write averaged outputs to output file.
    end for
	

ALGORITHM REFERENCES:
    We did not invent SPECAN SAR processing.

BUGS:

*****************************************************************************/
/****************************************************************************
*								            *
*   Quicklook SAR-processes the given input SAR signal data into the given  *
*	      LAS output image.						    *
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
#include "specan.h"
#include "ddr.h"


/*Default Parameters:
sampling freqency, chirp slope, chirp center frequency,
bandwidth to keep, FFT lengths.*/
#define RFS (18.95999E6)
#define RSLOPE (4.191375E11)
#define RCEN 0.0
#define RBW 0.87 /*Range bandwidth to keep*/

#define AFS (1679.9023)
#define ASLOPE (-2127.0)
#define ACEN 500.0
#define ABW 0.70 /*Azimuth Bandwidth to keep*/

#define RLEN_INIT 32
#define ALEN_INIT 32

void usage(char *mess1,char *mess2);

void prep_specan_file(int nLooks,int quality,double aspect,
	struct AISP_PARAMS *g,meta_parameters *meta)
{
	getRec *inFile;
	FILE *outFile;
	int outLines,outSamples/*,x*/;
	/*char command[255];*/
	int i;

	specan_struct rng={1.0/RFS,
		RSLOPE,
		RCEN,
		RBW,
		RLEN_INIT};
	
	specan_struct az={1.0/AFS,
		ASLOPE,
		ACEN,
		ABW,
		ALEN_INIT};
	
/*Set quality: longer FFT's -> bigger
image, better quality.*/
	for (i=0;i<quality;i++)
	{
		rng.fftLen*=2;
		az.fftLen*=2;
	}
	rng.fftLen=(int)(rng.fftLen*aspect);

/*Open files.*/
	printf("Opening input files...\n");
	inFile=fillOutGetRec(g->in1);
	outFile=fopenImage(g->out,"wb");
	
/*Init. Range Variables*/
	printf("Init SPECAN\n");
	rng.iSamp=1.0/g->fs;/*Sample size, range (s)= 1.0/sample freqency (Hz)*/
	rng.chirpSlope=g->slope;/*Range chirp slope (Hz/Sec)*/
	specan_init(&rng);
	
/*Init. Azimuth Variables*/
	printf("Estimate Doppler\n");
	az.iSamp=1.0/g->prf;/*Sample size, azimuth (s)=1.0/sample freqency (Hz)*/
	
	if (meta->stVec==NULL)
		{printf("Can only quicklook scenes with state vectors!\n");exit(1);}
	else {  /*Find doppler rate (Hz/sec) <=> azimuth chirp slope*/
		double dopRate,yaw=0.0;
		GEOLOCATE_REC *g=init_geolocate_meta(&meta->stVec->vecs[0].vec,meta);
		double look=getLook(g,meta->geo->slantFirst,yaw);
		getDoppler(g,look,yaw,NULL,&dopRate,NULL,NULL);
		az.chirpSlope=dopRate;
		free_geolocate(g);
	}
	
	az.chirpCenter=g->fd*(1.0/az.iSamp);/*Convert doppler central freqency to Hz*/
	/*For de-scalloping, compute the most powerful doppler freqency.*/
	az.powerCenter=(1.0/az.iSamp)*
		fftEstDop(inFile,inFile->nLines/2,1,az.fftLen);
	
	printf("Doppler centroid at %.0f Hz, %.0f Hz/sec\n",az.chirpCenter,az.chirpSlope);
	specan_init(&az);
	
	printf("Efficiency: Range(%d): %.0f%%   Azimuth(%d): %.0f%%\n",
		rng.fftLen,100.0*rng.oNum/rng.fftLen,
		az.fftLen,100.0*az.oNum/az.fftLen);

/*Process image.*/
	specan_file(inFile,nLooks,outFile,&rng,&az,&outLines,&outSamples);
	FCLOSE(outFile);
	freeGetRec(inFile);
	
/*Create DDR for output file.*/
	{
		struct DDR ddr;/*Output DDR*/
		c_intddr(&ddr);
		ddr.dtype=DTYPE_FLOAT;
		ddr.nbands=1;
		ddr.nl=outLines;
		ddr.ns=outSamples;
		ddr.sample_inc=rng.oSamp/rng.iSamp;
		ddr.line_inc=az.oSamp/az.iSamp;
		c_putddr(g->out,&ddr);
	}
/*Copy over metadata*/
	if (meta->info)
		sprintf(meta->info->processor,"ASF/QUICKLOOK/%.2f",VERSION);
	meta_write(meta,g->out);
}

#include "my_mpi.h"

int main(int argc,char *argv[])
{
	struct AISP_PARAMS g;
	meta_parameters *meta;
	int nLooks=4;/*Default: take 4 looks of data*/
	int quality=1;/*Default: make a small image*/
	double aspect=2;/*Default: compress slightly in range*/
	int argNo=1;
	mpi_init(&argc,&argv);
/*Check CLA's.*/
	if (argc<2)
		usage("No command line arguments given\n",NULL);
	
	while (argNo<argc)
	{
		if (argv[argNo][0]=='-')
		/*We were given a command-line option*/
		switch(argv[argNo][1])
		{
			case 'k': /* -k option: number of looks*/
				argNo++;
				nLooks=atoi(argv[argNo++]);
				break;
			case 'q': /* -q option: set "quality"*/
				argNo++;
				quality=atoi(argv[argNo++]);
				break;
			case 'a':/* -a options: set aspect ratio*/
				argNo++;
				aspect=atof(argv[argNo++]);
				break;
			default:/*Unrecognized option-pass on to parse-cla*/
				if (0==parse_cla(argc-argNo+1,&argv[argNo-1],&g,&meta))
					usage("Error in command-line options\n",NULL);
				else
					argNo=argc;/*parse_cla sucess-- get out of loop*/
				break;
		} else
		{ /*Not a command-line option*/
			if (0==parse_cla(argc-argNo+1,&argv[argNo-1],&g,&meta))
				usage("Error in command-line arguments\n",NULL);
			else
				argNo=argc;/*parse_cla sucess-- get out of loop*/
		}
	}

	prep_specan_file(nLooks,quality,aspect,&g,meta);
	mpi_end();
	return 0;
}

void usage(char *mess1,char *mess2)
{
	printf("\nUsage:\n"
		"   quicklook [ -k <nLooks> ] [ -q <quality> ] [-a <aspect ratio>]\n"
		"	<input signal> <output image>\n"
		"\n\t -k    number of range looks."
		"\n\t -q    size of the FFTs.\n\t -a    ratio of output image."
		"\n\nQuicklook SAR-processes the given input SAR signal\n"
		"data (without any extension) into the given LAS output\n"
		"image (with extension).\n"
		"Quicklook is much faster than a regular SAR processor (e.g. aisp),\n"
		"but the images are low-resolution and grainy.\n\n");
	printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
	
	if (mess1)
	{
		printf("Error:\n");
		printf(mess1,mess2);
	}
	exit(1);
}
