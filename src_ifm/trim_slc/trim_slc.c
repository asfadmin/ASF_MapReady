/******************************************************************************
NAME: trim_slc

SYNOPSIS: trim_slc <inSLCfile> <outCPXfile> <new top line> <new left sample>
	[<trim new height> <trim new width>]

DESCRIPTION:
	Trim_slc does two things: it converts a single-int-complex ASF CEOS
format image into a LAS 6.0 float-complex image, and allows you to trim that image.

	You specify the input and output filenames (WITH extention), and 
a top left coordinate.  Trim_slc will create a new image in outfile which
starts at these coordinates.  Optionally, you can also specify the new height 
and width of the output image (if not specified, trim will use the old size
minus the top left coordinates-- i.e. minus the part that is trimmed off).

	Trim_slc creates a valid LAS 6.0 DDR (data descriptor record) from
the CEOS metadata.

	This program should work with, but has not been tested with,
RADARSAT era datafiles and metadata.

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    6/97   Orion Lawlor Needed to import and trim SIC files
    				for interferometry.
    1.1	    5/98   Orion Lawlor Caplib/bug fix.
    1.2     12/98  Orion Lawlor Modified language for dataset-correctness.
    1.3     4/00   Mark Ayers   Added .meta file creation to trim_slc
    1.4     6/00   D. Koster    Modified to handle files > 2GB
    1.5     7/01   R. Gens	Added a flag for Focus processor

HARDWARE/SOFTWARE LIMITATIONS: 

SEE ALSO:
	trim(1)

BUGS: none known

******************************************************************************/
/****************************************************************************
*								            *
*   Trim_slc: lets you trim/add any # of samples to a Single Int Complex    *
*	      image file.						    *
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
#include "ceos.h"
#include "las.h"
#include "asf_meta.h"

float version=1.5;

int create_good_ddr(char *fname,int nl,int ns,int ml,int ms,double azpixsiz,double rnpixsiz,int data_type);

/* Remove the first extention from a filename, and return the extention.*/
char * strip_extention(char *filename)
{
/*We play "Hunt the first extention." by searching for periods starting at the end.*/
	int i=strlen(filename)-1;
	while ((i>0)&&(filename[i]!='.')) 
		i--;
	if (i!=0)
	{
	/*We have an extention, so first we remove it from the filename...*/
		filename[i]=0;
	/*And now we return it.*/
		return &filename[i+1];
	} else return "";
}

void usage(char *name);

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

int main(int argc, char *argv[])
{
  int era;
  int in_bytes_per_line,line_header,swflag;
  struct IOF_VFDR vfdr;
  struct VFDRECV facdr;
  int pixelSize=8;
  int x,y,startX,startY,endX,endY,inMaxX,inMaxY,outMaxX,outMaxY;
  int lastReadY,firstReadX,numInX;
  FILE *in,*out;
  float *outbuffer;
  short *inbuffer;
  meta_parameters *meta;
 
/*Process CLA's*/
  char infileData[256],*outfileData,infile[256],*outfileDDR;
  if (argc!=7 && argc!=5) usage(argv[0]);
   /*Compute filenames*/
  
  era=set_era(argv[1],infileData,0);
  era=set_era(infileData,infile,1);

  get_ifiledr(infile,&vfdr);
  get_facdr(infile,&facdr);
  

  outfileData=appendExt(argv[2],".cpx");
  outfileDDR=appendExt(argv[2],".ddr");
  printf("trim_slc: input SLC: '%s'. output Float-Complex: '%s'.\n",infileData,outfileData);
  in=fopenImage(infileData,"rb");
  out=fopenImage(outfileData,"wb");

  swflag=strncmp("FOCUS",vfdr.software_id,5);

  if (swflag!=0) {
    meta=meta_create(infileData);
    meta_write(meta,outfileData);
  } 

  inMaxX=facdr.apixels;
  inMaxY=facdr.alines;
  
  if (era==0) /*pre-RADARSAT data-- vfdr invalid, 12 byte header per line always.*/
  	line_header=12;
  else line_header=vfdr.predata; /*post-RADARSAT data-- get header size from vfdr.*/
  in_bytes_per_line=vfdr.reclen;
  
  if (0!=strncmp(vfdr.formcode,"CI*4",4)) {printf("Fatal Error: This program only works with\n\
  Single Int Complex input data ('COMPLEX INTEGER*4').  \n\
This data file is ('%s').\n",vfdr.formatid);exit(1);}
  
  startX=atoi(argv[4]);
  startY=atoi(argv[3]);
  if (argc==7)
  {
  	endX=startX+atoi(argv[6]);
  	endY=startY+atoi(argv[5]);
  } else
  {
  	endX=inMaxX-startX;
  	endY=inMaxY-startY;
  }
  outMaxX=endX-startX;
  outMaxY=endY-startY;

  printf("\tInput image size: Lines=%i, Samples=%i\n",inMaxY,inMaxX);
  printf("\tOutput image size: Lines=%i, Samples=%i\n",outMaxY,outMaxX);
  
/*Write out our outDDR*/
  create_good_ddr(outfileDDR,outMaxY,outMaxX,startY+1,startX+1,facdr.azpixspc,facdr.rapixspc,4);
    
/*If everything's OK, then allocate an input and output buffer big enough for */
 /*one line of input and output data, respectively.*/
  outbuffer= (float *)MALLOC(2*sizeof(float)*outMaxX);
  inbuffer= (short *)MALLOC(2*sizeof(short)*inMaxX);
  
/*If necessary, fill the top of the output with zeros, by loading up a buffer and writing.*/
  for (x=0;x<outMaxX*2;x++)
  	outbuffer[x]=0.0;
  for (y=0;y<-startY && y<outMaxY;y++)
  {
  	FWRITE(outbuffer,pixelSize,outMaxX,out);
  	if (0==y%100) printf("\tWriting output image line %i...\n",y);
  }

/*Do some calculations on where we should read.*/
  firstReadX=MAX(0,-startX);
  numInX=MIN(MIN(outMaxX,inMaxX-(firstReadX+startX)),outMaxX-firstReadX);
  lastReadY=MIN(outMaxY,inMaxY-startY);
  
  for (;y<lastReadY;y++)
  {
  	const int SLCPixSize=4;
  	int inputY=y+startY,
  		inputX=firstReadX+startX;
	FSEEK64(in,(inputY+1)*in_bytes_per_line+line_header+SLCPixSize*inputX,0);
  	FREAD(inbuffer,SLCPixSize,numInX,in);
  	for (x=0;x<numInX;x++)
  	{	
  		outbuffer[2*(x+firstReadX)]=inbuffer[x*2];
  		outbuffer[2*(x+firstReadX)+1]=inbuffer[x*2+1];
  	}
  	FWRITE(outbuffer,pixelSize,outMaxX,out);
  	if (0==y%100) printf("\tWriting output image line %i...\n",y);
  }
/*Reset buffer to zeros and fill remaining pixels.*/
  for (x=0;x<outMaxX*2;x++)
  	outbuffer[x]=0.0;
  for (;y<outMaxY;y++)
  {
  	FWRITE(outbuffer,pixelSize,outMaxX,out);
  	if (0==y%100) printf("\tWriting output image line %i...\n",y);
  }
  
  printf("trim_slc completed sucessfully!\n");
/*Now close up the files, 'cause we're done.*/
  FCLOSE(in);
  FCLOSE(out);
  return(0);
}

int create_good_ddr(char *fname,int nl,int ns,int ml,int ms,double azpixsiz,double rnpixsiz,int data_type)
{
	struct DDR ddr;
	int stati;

        c_intddr(&ddr);

	ddr.valid[4] = VALID;		/* Projection Units    */
	ddr.valid[5] = VALID;		/* Projection Distance */
	ddr.valid[7] = VALID;		/* Increment	       */
        ddr.nl = nl;
        ddr.ns = ns;
        ddr.nbands = 1;
        ddr.dtype = data_type;
        ddr.line_inc = 1.0;
        ddr.sample_inc = 1.0;
        ddr.master_line = ml;
        ddr.master_sample = ms;
        ddr.pdist_x = rnpixsiz;
        ddr.pdist_y = azpixsiz;
        strcpy(ddr.proj_units,"METERS");

        stati = c_putddr(fname, &ddr);
        if (stati != E_SUCC)
          {
            printf("Error returned from putddr\n");
            exit(1);
          }

	return(0);
}

void usage(char *name)
{
  printf("\n"); 
  printf("Usage:\n");
  printf("  %s <inSLC> <outCPX> <top line> <left samp> [<trim ht> "
	 "<trim wth>]\n\n",name); 
  printf("\t<inSLC>:  Single-Look-Complex image files (.D and .L or .dat"
	 " and .ldr)"
 	 "\n\t<outCPX>: Output file of <inSLC> in float complex format.\n"
	 "\t<top line>: and <left samp>:\n\t\t"
	 "  These are the top left coordinates.  Line and sample.\n"
	 "\t<trim ht> and <trim wth>:\n\t\t"
	 "  The new height and width (to be trimmed or enlarged");
  printf("\n\n");
  printf("Trim_slc: lets you trim/add any # of samples to a Single\n"
	"Int Complex image file, while creating a DDR, a .meta, and "
	"converting to \nfloat complex format.\n");
  printf("\n");
  printf("Version %4.2f, ASF SAR TOOLS\n\n",version);
  exit(1);
 
}

