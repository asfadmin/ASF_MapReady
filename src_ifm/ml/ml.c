/****************************************************************
NAME:  ml

SYNOPSIS: ml [-l hxw] [-s hxw] [-n metafile] [-a] <interferogram> <output>
    
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

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

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
    3.5	   9/00		T. Logan - Fixed amp calculation, added nLooks switch
    3.6	   7/01		R. Gens - Added log file switch

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   ml  -  convert an interferogram's .amp & .phase files into an image.    *
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
#include "las.h"
#include "ifm.h"
#include "asf_meta.h"
#include "ml.h"

/* local constants */
#define VERSION      3.6

/* function declaration */
void parse_clas(int, char **,int *,int *,int *,int *,int *);
int c2i(float*,float*,RGBDATA *,RGBDATA *,int,float);
void usage(char *name);

/* external variables */
extern int optind;
extern char *optarg;

int main(int argc, char *argv[])
{
	char fnm1[BUF],fnm2[BUF],fnm3[BUF],fnm4[BUF],outname[BUF];
	char imgfile[BUF];
	FILE *fiamp, *fiphase, *foamp, *fophase, *flas;
	long long inWid, inLen;
	int ll, ls;   /* look line and sample */
	int sl, ss;   /* step line and sample */
	int i,line, sample;
	int row, col, lasFlag = 0;
	long long nitems, newitems;
	long long outWid, outLen;
	long long ds/*,samplesRead*/;       /* input data size, number of samples read so far.*/
	long long red_offset, grn_offset, blu_offset;
	register float *ampIn, *phaseIn, ampScale;
	float *ampOut, *phaseOut,*ampBuf,Sin[256],Cos[256];
	float avg, percent=5.0;
	RGBDATA *table, *imgData;
	Uchar *redPtr, *grnPtr, *bluPtr;
	complexFloat z;
	struct DDR ddr,newddr;
	register float tmp,zImag,zReal,ampI;
	register int index,offset;
	const float convers=256.0/(2*3.14159265358979);
   
	/* check command line args */
	StartWatch();
	parse_clas(argc, argv, &ll, &ls, &sl, &ss, &lasFlag);
	system("date");
	printf("Program: ml\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: ml\n\n");
	}

	/* create filenames and open files for reading */
	c_getddr(argv[optind],&ddr);
  
	inWid=ddr.ns;
  	inLen=ddr.nl;
  	strcpy(outname,argv[optind+1]);
  	create_name(fnm1,argv[optind],".amp");
  	create_name(fnm2,argv[optind],".phase");
  	create_name(fnm3,argv[optind+1],".amp");
  	create_name(fnm4,argv[optind+1],".phase");
  	create_name(imgfile,argv[optind+1],"_las.img");
 	
  	outWid  = inWid/ss;
  	outLen = inLen / sl;
  
	/*Create the DDR for the amplitude and phase.*/
	newddr=ddr;
	newddr.nl=outLen;
	newddr.ns=outWid;
	newddr.dtype=EREAL;
	newddr.nbands=1;
	newddr.line_inc*=sl;
	newddr.sample_inc*=ss;
	newddr.pdist_x*=ss;
	newddr.pdist_y*=sl;
	c_putddr(outname,&newddr);
  
  
	/*Create 3-band image's DDR.*/
	newddr.dtype=EBYTE;
	newddr.nbands=3;
	c_putddr(imgfile,&newddr);
  
  
	fiamp = fopenImage(fnm1,"rb");
	fiphase = fopenImage(fnm2,"rb");
	foamp = fopenImage(fnm3,"wb");
	fophase = fopenImage(fnm4,"wb");
	flas = fopenImage(imgfile,"wb");

	/* get mean from input amplitude file */
	avg = get_mean(fiamp,inWid,inLen,TRUE);
 
	/*
	* create data buffers 
	*/
	for (i=0;i<256;i++)
	{
		float phas=((float)i)/256.0*(2*3.14159265358979);
		Sin[i]=sin(phas);
		Cos[i]=cos(phas);
	}
  
	/* set data variables */
	ampScale = 1.0/(ll*ls);
	nitems   = (ll-sl)*inWid;
	newitems = sl*inWid;
  
	ds       = sizeof(float);
	ampIn    = (float *)MALLOC(ds*(newitems+nitems+ls));
	phaseIn  = (float *)MALLOC(ds*(newitems+nitems+ls));
	ampOut   = (float *)MALLOC(ds*outWid);
	ampBuf   = (float *)MALLOC(ds*outWid);
	phaseOut = (float *)MALLOC(ds*outWid);
	table    = (RGBDATA *)MALLOC(sizeof(RGBDATA)*MAXENTRIES);
	imgData  = (RGBDATA *)MALLOC(sizeof(RGBDATA)*outWid);
	redPtr   = (Uchar *)MALLOC(sizeof(Uchar)*outWid);
	grnPtr   = (Uchar *)MALLOC(sizeof(Uchar)*outWid);
	bluPtr   = (Uchar *)MALLOC(sizeof(Uchar)*outWid);
	

	/* create a colortable to be used with c2i */
	colortable(table);
  
	/* start conversion */
/*	printf("   Skipping every %d col and %d row\n",ss,sl);
	printf("   Looking at every %d col and %d row\n",ls,ll);*/
  	printf("   Input is %lld lines by %lld samples\n",inLen,inWid);
	printf("   Ouput is %lld lines by %lld samples\n\n",outLen,outWid);
	if (logflag) {
  	  sprintf(logbuf, "   Input is %lld lines by %lld samples\n",inLen,inWid);
	  printLog(logbuf);
	  sprintf(logbuf, "   Ouput is %lld lines by %lld samples\n\n",outLen,outWid);
	  printLog(logbuf);
	}
 	
	/*
	* Run through all lines in which data needs to be read so that
	* amount of data will be equal to ll * inWid.
	*/
	for(line=0; line<outLen; line++)
	{
		long long foffset = sl*inWid*ds*line;
		long long ftotal = inWid*inLen*ds;
		int bytesPerChunk = ll*inWid*ds;

		if(foffset>=ftotal-bytesPerChunk)
			foffset=ftotal-bytesPerChunk;
			

		/* Seek to the correct line to read a ll*inWid chunk from */
		FSEEK64(fiamp,foffset,SEEK_SET);
		FSEEK64(fiphase,foffset,SEEK_SET);

		/* Read in a ll*inWid size chunk */
		FREAD(ampIn,ds,ll*inWid,fiamp);
		FREAD(phaseIn,ds,ll*inWid,fiphase);
		
		/* begin adding data */
		for (sample=0; sample<outWid; sample++)
		{ 
			tmp = 0.0, zReal=0.0, zImag=0.0;
			/* add up looking area */
			for (col=0;col<ls;col++)
			{
				offset=sample*ss+col;
				for (row=0;row<ll;row++)
				{
					ampI=ampIn[offset];
					index=0xFF&((int)(phaseIn[offset]*convers));
					tmp += ampI * ampI;
					zReal += ampI * Cos[index];
					zImag += ampI * Sin[index];
					offset+=inWid;
 				}
			}
     
			/* get phase from complex values */
			z.real=zReal;
			z.imag=zImag;
			/* place in output buffer */
		/*	ampOut[sample] = sqrt(tmp*ampScale); */
			ampOut[sample] = Cabs(z)*ampScale; 
			phaseOut[sample] = Cphase(z);
			if(lasFlag == 0)
				ampBuf[sample]=ampOut[sample];
			else
				ampBuf[sample]=avg*1.5;		
		}
    
		/* convert amp & phase to RGB. */
		if (!c2i(ampBuf,phaseOut,imgData,table,outWid,avg))
			Exit("ml: Error in c2i()");

		/* write out data to file */
		FWRITE(ampOut,ds,outWid,foamp);
		FWRITE(phaseOut,ds,outWid,fophase);   

		if ((line*100/outLen)>percent) {
			printf("   Completed %3.0f percent\n", percent);
			percent+=5.0;
		}
    
		for (i=0;i<outWid;i++)
		{
			redPtr[i] = imgData[i].red;
			grnPtr[i] = imgData[i].green;
			bluPtr[i] = imgData[i].blue;
		} 
		red_offset=(long long)(line*outWid);
		grn_offset=(long long)(line*outWid+outWid*outLen);
		blu_offset=(long long)(line*outWid+(2*outWid*outLen));		

		FSEEK64(flas,red_offset,SEEK_SET);
		FWRITE(redPtr,1,outWid,flas);
		FSEEK64(flas,grn_offset,SEEK_SET);
		FWRITE(grnPtr,1,outWid,flas);
		FSEEK64(flas,blu_offset,SEEK_SET);
		FWRITE(bluPtr,1,outWid,flas);
		
    
		/* reposition data for next read */
		for (i=0;i<nitems;i++)
		{
			ampIn[i] = ampIn[i + newitems];
			phaseIn[i] = phaseIn[i + newitems];
		}
	}
  
	/* 
	* free up unneeded memory and prepare to write 
	* a 3 sequential band image
	*/
/*	printf("\n\tdone with multilook\n");
	printf("writing out LAS/RGB image file\n");*/
	printf("   Completed 100 percent\n\n   Wrote %lld bytes of data\n\n", (long long)(outLen*outWid*4));
	if (logflag) {
	  sprintf(logbuf, "   Wrote %lld bytes of data\n\n", (long long)(outLen*outWid*4));
	  printLog(logbuf);
	  StopWatchLog(fLog);
	  FCLOSE(fLog);
	}

	FREE(ampIn);
	FREE(phaseIn);
	FREE(ampOut);
	FREE(phaseOut);
	FREE(table);
	FCLOSE(fiamp);
	FCLOSE(fiphase);
	FCLOSE(foamp);
	FCLOSE(fophase);
  
	/* free all memory, close files, print out time elapsed */
	FREE(redPtr);
	FREE(grnPtr);
	FREE(bluPtr);
	FREE(imgData);
	FCLOSE(flas);
	StopWatch();

	return 0;
}

void parse_clas(int argc, 
	   char *argv[],
	   int *ll, 
	   int *ls,
	   int *sl,
	   int *ss,
	   int *lasFlag
	   )
{
	int c;
	int lines=0, samples=0;
	char metaFileName[256];
	meta_parameters *meta;
  
	/* print beginning output and set variables to default 
	printf("\nml:\n");*/

	logflag=0;

	*ll = 0;
	*ls = 1;

	*ss = STEPSAMPLE;
	*sl = STEPLINE;

	/* grab any command line optons */
	while ((c=getopt(argc,argv,"l:s:n:ax:")) != EOF)
		switch (c)
		{
			case 'l':
			if (2!=sscanf(optarg,"%dx%d",&lines,&samples))
			{
				sprintf(errbuf,"   ERROR: '%s' does not look like a line x sample "
						"(a good one would be '5x1').\n",optarg);
				printErr(errbuf);
			}
			if (*ll==0) { *ll = lines; *ls = samples; }
				break;
		
			case 's': 
			if (2!=sscanf(optarg,"%dx%d",sl,ss))
			{
				sprintf(errbuf, "   ERROR: '%s' does not look like a line x sample "
						"(a good one would be '5x1').\n",optarg);
				printErr(errbuf);
			}
			break;
			
			case 'n':
			if (1!=sscanf(optarg,"%s",metaFileName))
			{
				sprintf(errbuf, "   ERROR: Unable to parse file name '%s'.\n",optarg);
				printErr(errbuf);
			}
			if (extExists(metaFileName,".meta")) /*Read .meta file if possible*/
			{
				meta = meta_read(metaFileName);
				*ll = meta->ifm->nLooks; *ls = 1;
				*sl = *ll; *ss = *ls;
				meta_free(meta);
			}
			else
			{
				sprintf(errbuf, "   ERROR: Unable to either find or open metaFile.\n");
				printErr(errbuf);
			}
			break;

			case 'a':
				printf("   Will remove amplitude part of color image\n");
				*lasFlag = 1;
			break;
			
			case 'x':
			if (1!=sscanf(optarg,"%s", logFile)) {
				sprintf(errbuf, "   ERROR: Unable to parse file name '%s'.\n",optarg);
				printErr(errbuf);
			}
			fLog = FOPEN(logFile, "a");
			logflag=1;
			break;
			
			default:
			usage(argv[0]);
		}

	if (*ll==0)
	{
		*ll = LOOKLINE;
		*ls = LOOKSAMPLE;	
	}

	switch (argc-optind)
	{
		case 2:
		return;

		case 3:
		return;

		default:
		usage(argv[0]);
	}
}

void usage(char *name) {
  printf("\nUSAGE: %s [-l hxw] [-s hxw] [-n metafile] [-a] [-x logfile] <interferogram> <output>\n",name);
  printf("    -l changes the look box to h by w. Default is %dx%d\n",LOOKLINE,LOOKSAMPLE);
  printf("    -s changes the step box to h by w. Default is %dx%d\n",STEPLINE,STEPSAMPLE);
  printf("    -n changes step & look box to nLooks by 1, as read from meta file\n");
  printf("    -a removes the amplitude image from the _las.img, producing only color phase\n");
  printf("    -x allows the output to be written to a log file\n");
  printf("    <interferogram>  Input iterferogram files (.amp, .phase, .ddr).\n"
	 "                     Usually the output from igram(1).\n");
  printf("    <output> is a multilook 'base' file name; the program will produce\n");
  printf("             four output files:  <ml>.amp and <ml>.phase\n");
  printf("             <ml>.img and <ml>.ddr\n\n");
  printf("Multilook will do two things: it will shrink the image\n\
vertically to make its aspect ratio 1.0, and it can apply a low\n\
pass filter over the image, to remove speckle.\n");
  printf("\nVersion: %.2f, ASF SAR Tools\n",VERSION);
  Exit("ml:  not enough command line args");
}

