/******************************************************************************
NAME:  ssar_crop.c - Crop SCANSAR data to bounding box & correct geolocations

SYNOPSIS: ssar_crop infile outfile [-c]

DESCRIPTION:

     ssar_crop clips a scansar image to a minimum external bounding box
     and creates a CEOS formatted output file with the map projection data
     record modified to fit the new (cropped) size of the image.  It also
     searches the image for the corner locations and modifies the facility
     data record geolocations to match those found in the actual data file.

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    image_resample	Resamples input buffer by desired factor
    find_corners	Searches image edges for corner locations
    find_tlc,	 	Finds the exact location of corner pixel
	_trc,	 	given an approximate location	
	_blc,
	_brc
    compareWithFacdr	Compares specified corner line,sample locations
			with those reported in the FACDR for the iamge.
    bounds		Determines the bounding box for an image buffer
    writeAsfCeosDatafile	Just what it says
    modifyAsfLeader		Just what is says	

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    inSAR.D,.L		Input SAR data and leader files
    ofile.D,.L		Output SAR data and leader files

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1/99   T. Logan	Fix SCANSAR image geolocations and cropping
    1.1     1/99   T. Logan     Remove ALL black (-c) switch added

HARDWARE/SOFTWARE LIMITATIONS:
	
	Because this algorithm involves a thresh hold type search, it
        may fail.  Several internal checks are made to trap any errors,
        but if problems arise please contact the Alaska SAR Facility.

ALGORITHM DESCRIPTION:

	Read input file into memory
	Strip the CEOS wrapper from image
	Resample the image to 800m pixels
	Find corner locations in resampled image
	Refine corner locations in full SAR image
	Rotate corners to match FACDR standards
	Determine bounding box for full image
	Crop full image to bounding box
	Write ASF format image file
	Correct and write out leader file for image

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   ssar_crop.c - Crop SCANSAR images to minimum and fix corner locations   *
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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "caplib.h"
#include "ceos.h"
#include "const.h"

#define VERSION 1.1

extern char   proj_id[15];
extern double proj_x, proj_y, pd_x, pd_y;

void ssar_tool_init(char *inSAR, int nl,int ns);
void bounds(unsigned char *buf, int nl, int np, int *topLine, int *bottomLine,
            int *leftSamp, int *rightSamp);
void find_corners(unsigned char *buf,int nl,int np, 
		int *tlc_line,int *tlc_samp, int *trc_line, int *trc_samp,
		int *blc_line,int *blc_samp, int *brc_line, int *brc_samp);
void find_tlc(unsigned char *buf, int nl, int np, int kernel, int tlc_line,
	 int tlc_samp, int *ret_line, int *ret_samp,double rotation);
void find_trc(unsigned char *buf, int nl, int np, int kernel, int trc_line,
	 int trc_samp, int *ret_line, int *ret_samp,double rotation);
void find_blc(unsigned char *buf, int nl, int np, int kernel, int blc_line,
	 int blc_samp, int *ret_line, int *ret_samp,double rotation);
void find_brc(unsigned char *buf, int nl, int np, int kernel, int brc_line,
	 int brc_samp, int *ret_line, int *ret_samp,double rotation);
int image_resample(unsigned char *inbuf, int nl, int np, float factor,
                unsigned char *outbuf, int *return_nl, int *return_np);
int writeAsfCeosDatafile(struct IOF_VFDR *vfdr, unsigned char *rhdr,
	 unsigned char *ibuf,int nl,int ns,char *ofile);
void compareWithFacdr(char *inSAR, int nl, int np, struct VFDRECV *facdr,
	int o_tlc_line, int o_tlc_samp, int o_trc_line, int o_trc_samp,
	int o_blc_line, int o_blc_samp, int o_brc_line, int o_brc_samp);

main(int argc,char *argv[])
{
   struct VFDRECV facdr;	   /* Facility data record         */
   struct dataset_sum_rec dssr;    /* Data Set Summary Record      */
   struct IOF_VFDR iof_vfdr;       /* Imagery option file descriptor */

   char inSAR[80], ofile[80];
   int  nl,np,onl,onp,anl,anp;
   int  i,j;
   FILE *fp;
   int  datasize = 1;
   unsigned char *buf;
   unsigned char *ibuf;
   int tlc_line, tlc_samp, trc_line, trc_samp;
   int blc_line, blc_samp, brc_line, brc_samp;
   int o_tlc_line, o_tlc_samp, o_trc_line, o_trc_samp;
   int o_blc_line, o_blc_samp, o_brc_line, o_brc_samp;
   int topLine,bottomLine,rightSamp,leftSamp;
   int era, headerBytes;
   int kernel;
   float pixsiz, factor;
   unsigned char rhdr_buf[192];
   double rotation;

   /* Parse cla's
    ------------*/
   if (argc!=3 && argc!=4)
    { 
      printf("\nUsage:: %s inSAR outfile [-c]\n\n",argv[0]);
      printf("\tinSAR   - scansar image for input.\n"
	     "\toutfile - CEOS output file.\n");
      printf("\t-c      - clip to minimum corners (remove all black)\n");
      printf("\tDefault - clip to minimum external bounding box (leave all image data)\n");
      printf("\nClips ScanSAR images and adjusts metadata geolocations\n");
      printf("Version %.2f, ASF SAR TOOLS\n\n",VERSION);
      exit(1);
    }
   StartWatch();

   printf("==============================\n");
   printf("  ASF SCANSAR CROP TOOL\n"); 
   printf("==============================\n");
   era = set_era(argv[1], inSAR, 0);
   strcpy(ofile,argv[2]);

   /* Determine format and size of input file 
    ----------------------------------------*/
   get_facdr(inSAR,&facdr);
   pixsiz = facdr.azpixspc;
   get_ifiledr(inSAR, &iof_vfdr);
   nl = iof_vfdr.numofrec;
   anl = nl+1;
   headerBytes = (era) ? 192 : 12;
   np = iof_vfdr.reclen-headerBytes;
   anp = np + headerBytes;

   ssar_tool_init(inSAR,nl,np);

   if (argc==4 && strcmp(proj_id,"atct")!=0)
     {
	printf("\nCorner clipping should only be used on AT/CT products\n");
        printf("Bizarre results and program crashing may occur!!!\n\n");
     }

   /* Allocate memory for image buffer 
    ---------------------------------*/
   ibuf = (unsigned char *) MALLOC (anl*anp*datasize);
   buf =  (unsigned char *) MALLOC (nl*np*datasize);

   /* Read the image into memory 
    ---------------------------*/
   printf(" Reading file %s,(%i lines x %i samples)\n",inSAR,anl,anp);
   fp=FOPEN(inSAR,"rb");
   FSEEK(fp,anp,0);	 	   /* Skip header line  */
   FREAD(ibuf,nl*anp,1,fp);        /* Read rest of file */
   FCLOSE(fp);

   /* Save a copy of the rhdr 
    ------------------------*/
   for (i=0; i<192; i++) rhdr_buf[i] = ibuf[i];

   /* Strip off the CEOS wrapper 
    ---------------------------*/
   printf(" Stripping CEOS Wrapper\n");
   for (i=0; i<nl; i++)
    for (j=0; j<np; j++)
      buf[i*np+j] = ibuf[i*(anp)+j+headerBytes];
   FREE(ibuf); 
   ibuf = buf;

   /* Allocate buffer for resampled image
    ------------------------------------*/
   factor = 800.0 / pixsiz;
   onl = nl * 1.0/factor;
   onp = np * 1.0/factor;
   buf = (unsigned char*) MALLOC ((unsigned)onl*onp*sizeof(char));

   /* Resample full image to 800 meter pixels
    ----------------------------------------*/
   printf(" Resampling SAR image to size %i by %i\n",onl,onp);
   image_resample(ibuf,nl,np,factor,buf,&onl,&onp);

   /* Find corners of the small size data 
    ------------------------------------*/
   printf(" Searching small SAR image for corner locations:\n");
   find_corners(buf, onl, onp, &tlc_line,&tlc_samp,&trc_line,&trc_samp,
	&blc_line,&blc_samp, &brc_line,&brc_samp);
   printf("\tTop Left Corner    : line %i, samp %i\n",tlc_line,tlc_samp);
   printf("\tTop Right Corner   : line %i, samp %i\n",trc_line,trc_samp);
   printf("\tBottom Left Corner : line %i, samp %i\n",blc_line,blc_samp);
   printf("\tBottom Right Corner: line %i, samp %i\n",brc_line,brc_samp);

   free(buf);
   buf = ibuf;

   /* Scale corners to full image size 
    ---------------------------------*/
   tlc_line *= (int) factor; trc_line *= (int) factor;
   blc_line *= (int) factor; brc_line *= (int) factor;
   tlc_samp *= (int) factor; trc_samp *= (int) factor;
   blc_samp *= (int) factor; brc_samp *= (int) factor;

  /* Refine corners to exact locations in the full image
   ----------------------------------------------------*/
  kernel = (int) 4*factor+1;      
  get_dssr(inSAR, &dssr);
  rotation = dssr.plat_head_scene - dssr.pro_head;
  /* printf("Image rotation is %lf\n",rotation); */

  printf(" Refining corner locations in full SAR image:\n");
  find_tlc(buf,nl,np,kernel,tlc_line,tlc_samp,&o_tlc_line,&o_tlc_samp,rotation);
  find_trc(buf,nl,np,kernel,trc_line,trc_samp,&o_trc_line,&o_trc_samp,rotation);
  find_blc(buf,nl,np,kernel,blc_line,blc_samp,&o_blc_line,&o_blc_samp,rotation);
  find_brc(buf,nl,np,kernel,brc_line,brc_samp,&o_brc_line,&o_brc_samp,rotation);
  printf("\tTop Left Corner    : line %i, samp %i\n",o_tlc_line,o_tlc_samp);
  printf("\tTop Right Corner   : line %i, samp %i\n",o_trc_line,o_trc_samp);
  printf("\tBottom Left Corner : line %i, samp %i\n",o_blc_line,o_blc_samp);
  printf("\tBottom Right Corner: line %i, samp %i\n",o_brc_line,o_brc_samp);

  compareWithFacdr(inSAR,nl,np,&facdr,o_tlc_line,o_tlc_samp,o_trc_line,
	o_trc_samp,o_blc_line,o_blc_samp,o_brc_line,o_brc_samp);

  /* Rotate to match standard swath orientation for near,far & start,end  
   --------------------------------------------------------------------*/
  if (strcmp(proj_id,"atct")!=0)
   {
    if (rotation < 40.0)
     {  /* Rotate corners 1 position clockwise */ 
	tlc_line = o_tlc_line; tlc_samp = o_tlc_samp;
	o_tlc_line = o_blc_line; o_tlc_samp = o_blc_samp;
	o_blc_line = o_brc_line; o_blc_samp = o_brc_samp;
	o_brc_line = o_trc_line; o_brc_samp = o_trc_samp;
	o_trc_line = tlc_line; o_trc_samp = tlc_samp;
     }
    else if (rotation > 130.0)
     {  /* Rotate corners 1 position counter-clockwise */
	tlc_line = o_tlc_line; tlc_samp = o_tlc_samp;
	o_tlc_line = o_trc_line; o_tlc_samp = o_trc_samp;
	o_trc_line = o_brc_line; o_trc_samp = o_brc_samp;
	o_brc_line = o_blc_line; o_brc_samp = o_blc_samp;
	o_blc_line = tlc_line; o_blc_samp = tlc_samp;
     }
   }

   /* Determine bounding box for the full image
    ------------------------------------------*/
   printf(" Determining bounding box for full SAR image");
   if (argc==4)
    {
	/* Use corners to decide clipping */
 	printf(" using corners\n");	
	leftSamp   = MAX(o_tlc_samp,o_blc_samp);
	topLine    = MAX(o_tlc_line,o_trc_line);
        rightSamp  = MIN(o_trc_samp,o_brc_samp);
	bottomLine = MIN(o_blc_line,o_brc_line);

        /*
	printf("Clipping to top left %i,%i bottom right %i,%i\n",
	 	leftSamp,topLine,rightSamp,bottomLine);
	*/

	/* Reset corner based on clipping */ 
        o_tlc_line = topLine;
	o_tlc_samp = leftSamp;
        o_trc_line = topLine;
        o_trc_samp = rightSamp; 
 	o_blc_line = bottomLine;
        o_blc_samp = leftSamp;
        o_brc_line = bottomLine;
        o_brc_samp = rightSamp;
    }
   else
    { 
	/* Use image data to decide clipping */
	printf(" using image data\n");
   	bounds(buf,nl,np,&topLine,&bottomLine,&leftSamp,&rightSamp);
    }

   /* Allocate buffer for cropped image
    ------------------------------------*/
   onl = bottomLine - topLine;
   onp = rightSamp - leftSamp;
   ibuf = (unsigned char*) malloc ((unsigned)onl*onp*sizeof(char));

   /* Trim full image to bounding box & write out
    --------------------------------------------*/
   printf(" Clipping SAR image to bounding box and writing output\n"); 
   for (i=0; i<onl; i++)
    for (j=0; j<onp; j++)
       ibuf[i*onp+j] = buf[ (i+topLine)*np + (j+leftSamp) ];
   free(buf);
    
   writeAsfCeosDatafile(&iof_vfdr,rhdr_buf,ibuf,onl,onp,ofile);

   /* Fix the leader file & write out
    ---------------------------------*/
   printf(" Correcting leader file records\n");
   modifyAsfLeader(inSAR,topLine,onl,leftSamp,onp,ofile,
		o_tlc_line,o_tlc_samp,o_trc_line,o_trc_samp,
		o_blc_line,o_blc_samp,o_brc_line,o_brc_samp);

   StopWatch();

   exit(1);
}

void compareWithFacdr(char *inSAR, int nl, int np, struct VFDRECV *facdr,
	int o_tlc_line, int o_tlc_samp, int o_trc_line, int o_trc_samp,
	int o_blc_line, int o_blc_samp, int o_brc_line, int o_brc_samp)
 {
   void ll_to_proj(double lat_d,double lon,double *op1,double *op2);
   void proj_to_ll(double p1,double p2,double *lat,double *lat_d,double *lon);
   double calc_distance(double R,double lon0,double lat0,
				 double lon1,double lat1);
   double x,y/*,lat,lon,dum1*/;
   int line, samp;
   /*double D,R;*/
   double min;
   double d1,d2,d3,d4;
   int    which;

   printf(" Comparison with FACDR:\n");


   ll_to_proj(facdr->nearslat,facdr->nearslon,&x,&y);
   line = (int) ((y-proj_y)/pd_y);
   samp = (int) ((x-proj_x)/pd_x);
   printf("\tFACDR NS: %4i, %4i ",line,samp);

   d1 = sqrt( SQR(line-o_tlc_line)+ SQR(samp-o_tlc_samp));
   d2 = sqrt( SQR(line-o_trc_line)+ SQR(samp-o_trc_samp));
   d3 = sqrt( SQR(line-o_blc_line)+ SQR(samp-o_blc_samp));
   d4 = sqrt( SQR(line-o_brc_line)+ SQR(samp-o_brc_samp));
   min = d1; which = 1;
   if (d2 < min) { min=d2; which = 2;}
   if (d3 < min) { min=d3; which = 3;}
   if (d4 < min) { min=d4; which = 4;}
   switch (which)
    {
	case 1: printf(" (closest to top left)\n"); break;
	case 2: printf(" (closest to top right)\n"); break;
	case 3: printf(" (closest to bottom left)\n"); break;
	case 4: printf(" (closest to bottom right)\n"); break;
     }

   /* Reverse transform to check projection routines 
   proj_to_ll(x,y,&dum1,&lat,&lon);
   R = facdr->eradcntr;
   D = calc_distance(R,lat,lon,facdr->nearslat,facdr->nearslon);
   printf("FACDR  :\t%lf\t%lf\n",facdr->nearslat,facdr->nearslon);
   printf("PROJ   : \t%lf\t%lf\n",x,y);
   printf("REVERSE:\t%lf\t%lf\n",lat,lon);   
   printf("\tReverse Transform Distance Moved     : %lf\n",D);
   */


   ll_to_proj(facdr->nearelat, facdr->nearelon,&x,&y);
   line = (int) ((y-proj_y)/pd_y);
   samp = (int) ((x-proj_x)/pd_x);
   printf("\tFACDR NE: %4i, %4i ",line,samp);

   d1 = sqrt( SQR(line-o_tlc_line)+ SQR(samp-o_tlc_samp));
   d2 = sqrt( SQR(line-o_trc_line)+ SQR(samp-o_trc_samp));
   d3 = sqrt( SQR(line-o_blc_line)+ SQR(samp-o_blc_samp));
   d4 = sqrt( SQR(line-o_brc_line)+ SQR(samp-o_brc_samp));
   min = d1; which = 1;
   if (d2 < min) { min=d2; which = 2;}
   if (d3 < min) { min=d3; which = 3;}
   if (d4 < min) { min=d4; which = 4;}
   switch (which)
    {
	case 1: printf(" (closest to top left)\n"); break;
	case 2: printf(" (closest to top right)\n"); break;
	case 3: printf(" (closest to bottom left)\n"); break;
	case 4: printf(" (closest to bottom right)\n"); break;
     }

   /* Reverse transform to check projection routines 
   proj_to_ll(x,y,&dum1,&lat,&lon);
   D = calc_distance(R,lat,lon,facdr->nearelat,facdr->nearelon);
   printf("FACDR  :\t%lf\t%lf\n",facdr->nearelat,facdr->nearelon);
   printf("PROJ   : \t%lf\t%lf\n",x,y);
   printf("REVERSE:\t%lf\t%lf\n",lat,lon);   
   printf("\tReverse Transform Distance Moved     : %lf\n",D);
   */


   ll_to_proj(facdr->farslat, facdr->farslon,&x,&y);
   line = (int) ((y-proj_y)/pd_y);
   samp = (int) ((x-proj_x)/pd_x);
   printf("\tFACDR FS: %4i, %4i ",line,samp);

   d1 = sqrt( SQR(line-o_tlc_line)+ SQR(samp-o_tlc_samp));
   d2 = sqrt( SQR(line-o_trc_line)+ SQR(samp-o_trc_samp));
   d3 = sqrt( SQR(line-o_blc_line)+ SQR(samp-o_blc_samp));
   d4 = sqrt( SQR(line-o_brc_line)+ SQR(samp-o_brc_samp));
   min = d1; which = 1;
   if (d2 < min) { min=d2; which = 2;}
   if (d3 < min) { min=d3; which = 3;}
   if (d4 < min) { min=d4; which = 4;}
   switch (which)
    {
	case 1: printf(" (closest to top left)\n"); break;
	case 2: printf(" (closest to top right)\n"); break;
	case 3: printf(" (closest to bottom left)\n"); break;
	case 4: printf(" (closest to bottom right)\n"); break;
     }

   /* Reverse transform to check projection routines 
   proj_to_ll(x,y,&dum1,&lat,&lon);
   D = calc_distance(R,lat,lon,facdr->farslat,facdr->farslon);
   printf("FACDR  :\t%lf\t%lf\n",facdr->farslat,facdr->farslon);
   printf("PROJ   : \t%lf\t%lf\n",x,y);
   printf("REVERSE:\t%lf\t%lf\n",lat,lon);   
   printf("\tReverse Transform Distance Moved     : %lf\n",D);
   */

   ll_to_proj(facdr->farelat, facdr->farelon,&x,&y);
   line = (int) ((y-proj_y)/pd_y);
   samp = (int) ((x-proj_x)/pd_x);
   printf("\tFACDR FE: %4i, %4i ",line,samp);

   d1 = sqrt( SQR(line-o_tlc_line)+ SQR(samp-o_tlc_samp));
   d2 = sqrt( SQR(line-o_trc_line)+ SQR(samp-o_trc_samp));
   d3 = sqrt( SQR(line-o_blc_line)+ SQR(samp-o_blc_samp));
   d4 = sqrt( SQR(line-o_brc_line)+ SQR(samp-o_brc_samp));
   min = d1; which = 1;
   if (d2 < min) { min=d2; which = 2;}
   if (d3 < min) { min=d3; which = 3;}
   if (d4 < min) { min=d4; which = 4;}
   switch (which)
    {
	case 1: printf(" (closest to top left)\n"); break;
	case 2: printf(" (closest to top right)\n"); break;
	case 3: printf(" (closest to bottom left)\n"); break;
	case 4: printf(" (closest to bottom right)\n"); break;
     }

   /* Reverse transform to check projection routines 
   proj_to_ll(x,y,&dum1,&lat,&lon);
   D = calc_distance(R,lat,lon,facdr->farelat,facdr->farelon);
   printf("FACDR  :\t%lf\t%lf\n",facdr->farelat,facdr->farelon);
   printf("PROJ   : \t%lf\t%lf\n",x,y);
   printf("REVERSE:\t%lf\t%lf\n",lat,lon);   
   printf("\tReverse Transform Distance Moved     : %lf\n",D);
   */

  return;
 }
