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
/******************************************************************************
NAME:  multilook

SYNOPSIS:
	multilook [-look hxw] [-step hxw] [-meta metafile] [-amplitude]
                  [-log logfile] <interferogram> <output>
    
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
	amp. and phase files are still float values.

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
    VERS: DATE:  AUTHOR       PURPOSE:
    ---------------------------------------------------------------
    1.0          Rob Fatland  Original Development
    1.1          M. Shindle   Revise & Clean
    2.0          M. Shindle   Allow multilooking on variable size window. ML
                                will still move in 1 x 5 step, but can now look
                                at an area of StepLine and StepSample. AmpOut
                                and PhaseOut are now byte images instead of
                                float.
    3.0                       Modify StepSline & StepSample
    3.2          O. Lawlor    Get image size from DDR.
     "     7/97  D.Corbett    Updated version number
    3.3   10/97               Bug fix - now allows ss <> 1 & fixed for loop for 
                                 Sin & Cos table creation. 
    3.4          O. Lawlor    Updated CLA's.
    3.5    9/00  T. Logan     Fixed amp calculation, added nLooks switch
    3.6    7/01  R. Gens      Added log file switch
    3.7    2/04  P. Denny     Changed name from ml to multilook, changed license
                               from GPL to our own ASF license.
    3.8    2/04  R. Gens      Updated command line parsing
    3.9    6/05  R. Gens      First cut on conversion to new metadata

HARDWARE/SOFTWARE LIMITATIONS:
ALGORITHM DESCRIPTION:
ALGORITHM REFERENCES:
BUGS:
*******************************************************************************/

#include "asf.h"
#include "las.h"
#include "ifm.h"
#include "asf_meta.h"
#include "multilook.h"
#include "lzFetch.h"
#include "proj.h"

#include "../../include/asf_endian.h"

/* local constants */
#define VERSION      3.9

/* function declaration */
void parse_clas(int, char **,int *,int *,int *,int *,int *);
int c2i(float*,float*,RGBDATA *,RGBDATA *,int,float);
void usage(char *name);

/* This function I think can probably be replaced by meta2ddr */

/* But... we need to get this working so I'll check this in, it is
   a chopped version of asf_meta1x_to_meta09 from libasf_meta
   which was causing problems due to inconsistencies between the
   old metadata declarations and the new ones.  With this code
   moved here (the meta conversion part of asf_meta1x_to_meta09
   we didn't need - just the ddr), multilook no longer depends 
   on that library and so now works ok.                        */
void old_meta2ddr(meta_parameters *meta, struct DDR *ddr)
{
  int ii;
  int proj_invalid = 0;
  
  /************************************
   * Fill in the DDR structure 
   ************************************/

  /* Initialize ddr values */
  c_intddr(ddr);
  
  /* Number of lines & samples; both int */
  ddr->nl = meta->general->line_count;
  ddr->ns = meta->general->sample_count;
  /* Number of bands; int */
  ddr->nbands = 1;
  /* Data type; int */
  switch (meta->general->data_type) 
    {
    case BYTE:           ddr->dtype = DTYPE_BYTE;    break;
    case INTEGER16:      ddr->dtype = DTYPE_SHORT;   break;
    case INTEGER32:      ddr->dtype = DTYPE_LONG;    break;
    case REAL32:         ddr->dtype = DTYPE_FLOAT;   break;
    case REAL64:         ddr->dtype = DTYPE_DOUBLE;  break;
    case COMPLEX_REAL32: ddr->dtype = DTYPE_COMPLEX; break;
    default:
      /*      ddr->dtype = -1;*/
      /* Let's work with the assumption for the moment that we only deal 
	 with floating point imagery */
      ddr->dtype = DTYPE_FLOAT;
      break;
    }
  /* Worthless date & time fields; both char[12] */
  strcpy (ddr->last_used_date,"");
  strcpy (ddr->last_used_time,"");
  /* System byte ordering style; char[12] */
  if (0==strcmp(meta->general->system,"big_ieee"))
    strcpy(ddr->system,"ieee-std");
  else if (0==strcmp(meta->general->system,"lil_ieee"))
    strcpy(ddr->system,"ieee-lil");
  else if (0==strcmp(meta->general->system,"cray_float"))
    strcpy(ddr->system,"cray-unicos");
  else /* "???" ... no meta equivalent of "ibm-mvs" */
    strcpy(meta->general->system,"other-msc");
  
  /* Projection units; char[12] */
  if (meta->projection) /* if projection struct has been allocated */
    strncpy(ddr->proj_units, meta->projection->units, 12);
  else
    strcpy(ddr->proj_units, "meters"); /* Safe to assume meters */
  ddr->valid[DDPUV] = VALID;
  
  /* Increment per sample in x & y directions; both double */
  ddr->line_inc   = meta->sar->line_increment;
  ddr->sample_inc = meta->sar->sample_increment;
  ddr->valid[DDINCV] = 
    ((meta->sar->line_increment == meta->sar->line_increment)
     &&(meta->sar->sample_increment == meta->sar->sample_increment))
    ? VALID : INVAL;
  
  /* Line/sample relative to master image; both int */
  ddr->master_line   = meta->general->start_line + 1;
  ddr->master_sample = meta->general->start_sample + 1;
  
  /* Projection distance per pixel; both double (pixel size)*/
  if (meta->projection) {
    ddr->pdist_y = fabs(meta->projection->perY);
    ddr->pdist_x = fabs(meta->projection->perX);
  }
  else {
    ddr->pdist_y = meta->general->y_pixel_size;
    ddr->pdist_x = meta->general->x_pixel_size;
  }
  ddr->valid[DDPDV] = VALID;
  
  /* Projection dependent stuff */
  if (meta->sar->image_type=='P') {
    meta_projection *proj = meta->projection;
    /* UTM zone code or 62 if n/a; int */
    ddr->zone_code = (proj->type==UNIVERSAL_TRANSVERSE_MERCATOR) ? 
      proj->param.utm.zone : 62;
    ddr->valid[DDZCV] = VALID;
    /* Projection type; int
     * AND
     * Projection coefficients array; double[15]
     *  Entire coefficients array is 0.0 for Geographic and UTM;
     *  meta structure does not currently support geographic or 
     albers projections */
    switch (proj->type) 
      {
      case SCANSAR_PROJECTION: 
	/* Along-track/cross-track... ddr has no atct projection, 
	   default to UTM */
	/*Can't do anything here until we add AT/CT to asf_geolib.*/
	proj_invalid=1;
	break;
      case ALBERS_EQUAL_AREA:/* Albers Conic Equal Area */
	ddr->proj_code = ALBERS;
	ddr->valid[DDPCV] = VALID;
	ddr->proj_coef[0] = proj->re_major;
	ddr->proj_coef[1] = proj->re_minor;
	ddr->proj_coef[2] = 
	  packed_deg(proj->param.albers.std_parallel1); /*standard parallel1*/
	ddr->proj_coef[3] = 
	  packed_deg(proj->param.albers.std_parallel2); /*standard parallel2*/
	ddr->proj_coef[4] = 
	  packed_deg(proj->param.albers.center_meridian);  /*Center longitude 
							     of proj*/
	ddr->proj_coef[5] = 
	  packed_deg(proj->param.albers.orig_latitude);  /*Center latitude 
							   of proj*/
	ddr->proj_coef[6] = 0.0; /*False Easting*/
	ddr->proj_coef[7] = 0.0; /*False Northing*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      case LAMBERT_CONFORMAL_CONIC:/* Lambert Conformal Conic */
	ddr->proj_code = LAMCC;
	ddr->valid[DDPCV] = VALID;
	ddr->proj_coef[0] = proj->re_major;
	ddr->proj_coef[1] = proj->re_minor;
	ddr->proj_coef[2] = 
	  packed_deg(proj->param.lamcc.plat1); /*standard parallel1*/
	ddr->proj_coef[3] = 
	  packed_deg(proj->param.lamcc.plat2); /*standard parallel2*/
	ddr->proj_coef[4] = 
	  packed_deg(proj->param.lamcc.lon0);  /*Center longitude of proj*/
	ddr->proj_coef[5] = 
	  packed_deg(proj->param.lamcc.lat0);  /*Center latitude of proj*/
	ddr->proj_coef[6] = 0.0; /*False Easting*/
	ddr->proj_coef[7] = 0.0; /*False Northing*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      case POLAR_STEREOGRAPHIC:/* Polar Stereographic */
	ddr->proj_code = PS;
	ddr->valid[DDPCV] = VALID;
	ddr->proj_coef[0] = proj->re_major;
	ddr->proj_coef[1] = proj->re_minor;
	ddr->proj_coef[4] = 
	  packed_deg(proj->param.ps.slon);/*Longitude down below pole of map*/
	ddr->proj_coef[5] = 
	  packed_deg(proj->param.ps.slat);/*Latitude of true scale*/
	ddr->proj_coef[6] = 0.0; /*False Easting*/
	ddr->proj_coef[7] = 0.0; /*False Northing*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      case UNIVERSAL_TRANSVERSE_MERCATOR:/* Universal Transverse Mercator */
	ddr->proj_code = UTM;
	ddr->valid[DDPCV] = VALID;
	/*Unnecessary since the zone is specified*/
	ddr->proj_coef[0] = 
	  meta->general->center_latitude; /*any longitude in proj*/
	ddr->proj_coef[1] = 
	  meta->general->center_longitude;/*any latitude in proj*/
	ddr->valid[DDPPV] = VALID; /* Validity of proj_coef array */
	break;
      default:/*Der?*/
	proj_invalid=1;
	ddr->proj_code = -1;
	break;
      }
    /* Datum Code; int */
    ddr->datum_code = earth_radius2datum(proj->re_major, proj->re_minor);
    ddr->valid[DDDCV] = (ddr->datum_code==-1) ? INVAL : VALID;
    /* Corner Coordinates; all double[2] */
    ddr->upleft[0] = proj->startY;
    ddr->upleft[1] = proj->startX;
    ddr->loleft[0] = proj->startY + meta->general->line_count * proj->perY;
    ddr->loleft[1] = proj->startX;
    ddr->upright[0] = proj->startY;
    ddr->upright[1] = proj->startX + 
      meta->general->sample_count * proj->perX;
    ddr->loright[0] = proj->startY + 
      meta->general->line_count * proj->perY;
    ddr->loright[1] = proj->startX + 
      meta->general->sample_count * proj->perX;
    ddr->valid[DDCCV] = ((proj->startY == proj->startY)
			 &&(proj->startX == proj->startX))
      ? VALID : INVAL;
    if (proj_invalid)
      {
	for (ii=0;ii<4;ii++) 
	  ddr->valid[ii] = UNKNOW;
	ddr->valid[DDCCV] = INVAL;
      }
  } /* End projection info */
}

int main(int argc, char *argv[])
{
  meta_parameters *meta, *meta_old, *meta_stat;
	char fnm1[BUF],fnm2[BUF],fnm3[BUF],fnm4[BUF],outname[BUF];
	char imgfile[BUF],metaFile[BUF],cmd[BUF],metaIn[BUF],metaOut[BUF];
	FILE *fiamp, *fiphase, *foamp, *fophase, *flas;
	int ll=0, ls=1;   /* look line and sample */
	int sl=STEPLINE, ss=STEPSAMPLE;   /* step line and sample */
	int i,line, sample;
	int row, col, ampFlag = 0;
	long long nitems, newitems, inWid, inLen, outWid, outLen;
	long long ds/*,samplesRead*/;       /* input data size, number of samples read so far.*/
	long long red_offset, grn_offset, blu_offset;
	register float *ampIn, *phaseIn, ampScale;
	float *ampOut, *phaseOut,*ampBuf,Sin[256],Cos[256];
	float avg, percent=5.0;
	RGBDATA *table, *imgData;
	Uchar *redPtr, *grnPtr, *bluPtr;
	complexFloat z;
	struct DDR newddr;
	register float tmp,zImag,zReal,ampI;
	register int index,offset;
	const float convers=256.0/(2*3.14159265358979);
   
	logflag = 0;

  /* parse command line */
  while (currArg < (argc-2)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag = 1;
    }
    else if (strmatch(key,"-look")) {
      CHECK_ARG(1);
      if (2!=sscanf(GET_ARG(1),"%dx%d",&ll,&ls)) {
        printf("   ***ERROR: -look '%s' does not look like line x sample (e.g. '10x2').\n",GET_ARG(1));
        usage(argv[0]);
      }
    }
    else if (strmatch(key,"-step")) {
      CHECK_ARG(1);
      if (2!=sscanf(GET_ARG(1),"%dx%d",&sl,&ss)) {
        printf("   ***ERROR: -step '%s' does not look like line x sample (e.g. '5x1').\n",GET_ARG(1));
        usage(argv[0]);
      }
    }
    else if (strmatch(key,"-meta")) {
      CHECK_ARG(1);
      if (1!=sscanf(GET_ARG(1),"%s",metaFile)) {
        printf("   ***ERROR: Could not open '%s'.\n",GET_ARG(1));
        usage(argv[0]);
      }
      strcat(metaFile, "");
      ls = ss = 1;
      ll = sl = lzInt(metaFile, "sar.look_count:", NULL);
    }
    else if (strmatch(key,"-amplitude")) {
      printf("   Will remove amplitude part of color image\n");
      ampFlag = 1;
    }
    else {printf("\n   ***Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 2) {printf("   Insufficient arguments.\n"); usage(argv[0]);}

	system("date");
	printf("Program: multilook\n\n");
	if (logflag) {
	  StartWatchLog(fLog);
	  printLog("Program: multilook\n\n");
	}

	/* Create filenames and open files for reading */
  	create_name(fnm1,argv[currArg],"_amp.img");
  	create_name(fnm2,argv[currArg],"_phase.img");
	meta_stat = meta_read(fnm1);
	meta_old = meta_read(fnm2);
	meta = meta_read(fnm2);
  	create_name(fnm3,argv[++currArg],"_amp.img");
  	create_name(fnm4,argv[currArg],"_phase.img");
  	create_name(imgfile,argv[currArg],"_rgb.img");
	create_name(metaOut,argv[currArg],"_rgb.meta");
 	
	inWid = meta->general->sample_count;
	inLen = meta->general->line_count;
  	meta->general->sample_count /= ss;
  	meta->general->line_count /= sl;
	outWid = meta->general->sample_count;
	outLen = meta->general->line_count;
  
	/* Create new metadata file for the amplitude and phase.*/
	meta->sar->line_increment = 1;
	meta->sar->sample_increment = 1;
	meta->sar->azimuth_time_per_pixel *= sl;
	meta->general->x_pixel_size *= ss;
	meta->general->y_pixel_size *= sl;
	create_name(metaIn,argv[currArg],"_amp.meta");
	meta_write(meta, metaIn);
	create_name(metaIn,argv[currArg],"_phase.meta");
	meta_write(meta, metaIn);

        old_meta2ddr(meta, &newddr);

	/* Create 3-band image's DDR.
	   Currently metadata file don't know anything about multiband imagery.
	   We will need to convert the current version for single band amplitude
	   image back to metadata version 0.9 and change a couple of values 
	sprintf(cmd, "convert_meta %s 1.3 %s 0.9", metaIn, metaOut);
	asfSystem(cmd);
        */
	
	newddr.dtype=EBYTE;
	newddr.nbands=3;
	c_putddr(imgfile,&newddr);
  
	fiamp = fopenImage(fnm1,"rb");
	fiphase = fopenImage(fnm2,"rb");
	foamp = fopenImage(fnm3,"wb");
	fophase = fopenImage(fnm4,"wb");
	flas = fopenImage(imgfile,"wb");

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
	
        /* calculate mean value */
        if (meta_stat->stats)
          avg = meta_stat->stats->mean;
        else {
          sprintf(cmd, "stats -overmeta -overstat \"%s\"\n", fnm1);
          asfSystem(cmd);
          meta_free(meta_stat);
          meta_stat = meta_read(fnm1);
          avg = meta_stat->stats->mean;
        }

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

		/* Read in a ll*inWid size chunk */
		get_float_lines(fiamp, meta_old, line*sl, ll, ampIn);
		get_float_lines(fiphase, meta_old, line*sl, ll, phaseIn);

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
			if(!ampFlag)
				ampBuf[sample]=ampOut[sample];
			else
				ampBuf[sample]=avg*1.5;		
		}
    
		/* convert amp & phase to RGB. */
		if (!c2i(ampBuf,phaseOut,imgData,table,outWid,avg))
			Exit("ml: Error in c2i()");

		/* write out data to file */
		put_float_line(foamp, meta, line, ampOut);
		put_float_line(fophase, meta, line, phaseOut);

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
	printf("writing out LAS/RGB image file\n");*
	printf("   Completed 100 percent\n\n   Wrote %lld bytes of data\n\n", 
	       (long long)(outLen*outWid*4));
	if (logflag) {
	  sprintf(logbuf, "   Wrote %lld bytes of data\n\n", 
		  (long long)(outLen*outWid*4));
	  printLog(logbuf);
	  StopWatchLog(fLog);
	  FCLOSE(fLog);
	}
*/

	FREE(ampIn);
	FREE(phaseIn);
	FREE(ampOut);
        FREE(ampBuf);
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

        meta_free(meta);
        meta_free(meta_stat);
        meta_free(meta_old);

	return 0;
}

void usage(char *name) {

 printf("\n"
	"USAGE: %s [-look hxw] [-step hxw] [-meta metafile] [-amplitude] [-log logfile]\n"
	"                 <interferogram> <output>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <interferogram>  Input iterferogram files (.amp, .phase, .meta).\n"
	"                    Usually the output from igram(1).\n"
	"   <output> is a multilook 'base' file name; the program will produce\n"
	"            four output files: <output>.amp and <output>.phase\n"
	"                               <output>.img and <output>.meta\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -look changes the look box to h by w. Default is %dx%d\n"
	"   -step changes the step box to h by w. Default is %dx%d\n"
	"   -meta changes step & look box to nLooks by 1, as read from meta file\n"
	"   -amplitde removes the amplitude image from the _rgb.img, producing only color phase\n"
	"   -log allows the output to be written to a log file\n",
	LOOKLINE,LOOKSAMPLE,STEPLINE,STEPSAMPLE);
 printf("\n"
	"DESCRIPTION:\n"
	"   This program will do two things: it will shrink the image vertically\n"
	"   to make its aspect ratio 1.0, and it can apply a low pass filter over\n"
	"   the image, to remove speckle.\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}

