/******************************************************************************
NAME:  diff_las

SYNOPSIS:  diff_las img1.ext img2.ext [difference.ext]

DESCRIPTION:  compare LAS 6.0 images, for debugging and verification
	      (especially between platforms).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    1997   Orion Lawlor
    1.1     7/01   P. Denny	Change byte output to signed short
    				 input for better visualization
    1.5     2/02   P. Denny	Add masking option, standardize
    				 command line arguments

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   diff_las -- Compares to LAS 6.0 images				    *
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
#include "asf_meta.h"

#define VERSION 1.5

void usage(char *name);

int main(int argc, char **argv)
{
#define hist_bins 50
	const float span=1.0;
	int x,y;
	float *line1,*line2,*outline;
	double totalDiff,totalSqDiff,smallestDiff,largestDiff,npixels;
	int *histogram;
	char *f1,*f2,*f3=NULL;
	FILE *fp1,*fp2,*fp3=NULL;
	struct DDR ddr1,ddr2,outddr;
	int ignored_pix_cnt;
	extern char *optarg;
	extern int optind;
	int c;
	float mask=-1.0;

	while ((c=getopt(argc, argv, "d:m:")) != EOF)
	{
		switch (c) {
		 case 'd':
			f3 = optarg;
			break;
		 case 'm':
			mask = atof(optarg);
			break;
		 default:
			usage(argv[0]);
		}
	}
	
	if (optind >= argc) 
		usage(argv[0]);

	f1=argv[optind];
	f2=argv[optind+1];

	if (0!=c_getddr(f1,&ddr1)) {printf("Couldn't open ddr file for %s.\n",f1);exit(1);}
	if (0!=c_getddr(f2,&ddr2)) {printf("Couldn't open ddr file for %s.\n",f2);exit(1);}
	if (ddr1.nl!=ddr2.nl)
		{printf("The number of lines is different.\n");exit(1);}
	if (ddr1.ns!=ddr2.ns)
		{printf("The number of samples is different.\n");exit(1);}
	if (ddr1.dtype!=ddr2.dtype)
		{printf("The data types are different.\n");exit(1);}
	if (ddr1.nbands!=ddr2.nbands)
		{printf("The number of bands is different.\n");exit(1);}
	if (ddr1.master_line!=ddr2.master_line)
		printf("The master line fields do not match:%i vs %i\n",ddr1.master_line,ddr2.master_line);
	if (ddr1.master_sample!=ddr2.master_sample)
		printf("The master sample fields do not match:%i vs %i\n",ddr1.master_sample,ddr2.master_sample);
	
	outddr=ddr1;
	if (outddr.dtype==DTYPE_BYTE)
		outddr.dtype=DTYPE_SHORT;
	if (f3)
	{
		c_putddr(f3,&outddr);
		fp3=fopenImage(f3,"wb");
		if (fp3==NULL)
			printf("Cannot open %s for writing.  No output will be generated.\n",f3);
	}
	fp1=fopenImage(f1,"rb");
	fp2=fopenImage(f2,"rb");
	line1=(float *)MALLOC(sizeof(float)*ddr1.ns);
	line2=(float *)MALLOC(sizeof(float)*ddr2.ns);
	outline=(float *)MALLOC(sizeof(float)*outddr.ns);
	histogram=(int *)MALLOC((hist_bins+1)*sizeof(int));
	for (x=0;x<=hist_bins;x++)
		histogram[x]=0;
	totalDiff=totalSqDiff=0.0;
	smallestDiff=100000000000.0;
	largestDiff=0;
	npixels=(float)ddr1.nl*(float)ddr1.ns;
	for (y=0;y<ddr1.nl;y++)
	{
		getFloatLine(fp1,&ddr1,y,line1);
		getFloatLine(fp2,&ddr2,y,line2);
		for (x=0;x<ddr1.ns;x++)
		{
			register float diff,l1=line1[x],l2=line2[x];
			int histIndex;
			if ((l1==mask) || (l2==mask))
			{
				diff = outline[x] = mask;
				ignored_pix_cnt++;
				continue;
			}
			if (fabs(l1)>fabs(l2))
				diff=(l1-l2)/l1;
			else if (l2!=0.0)
				diff=(l1-l2)/l2;
			else diff=0;
			histIndex=(span+diff)/span*50.0/2+0.5;
			if (histIndex<0) histIndex=0;
			if (histIndex>hist_bins) histIndex=hist_bins;
			histogram[histIndex]++;
			
			diff=l1-l2;
			outline[x]=diff;
			totalDiff+=diff;
			totalSqDiff+=diff*diff;
			if (diff<0) diff=-diff;
			if (largestDiff<diff) largestDiff=diff;
			if (smallestDiff>diff) smallestDiff=diff;
		}
		if (fp3)
			putFloatLine(fp3,&outddr,y,outline);
		if (y%100==0)
			printf("\tComparing line %i...\n",y);
	}
	fclose(fp1);
	fclose(fp2);
	if (fp3)
		fclose(fp3);
	if (histogram[hist_bins/2]/npixels<0.995)
	{
		printf("Files differ by more than 0.5%%!\n");
		printf("Relative Error Histogram:\n Diff:  Frequency:\n");
		for (x=0;x<=hist_bins;x++)
			if (histogram[x])
				printf("%6.2f %7.4f%%\n",x/(50.0/2)*span-span,histogram[x]/npixels*100);
		if (mask==-1.0)
		{
			printf("Comparing %s to %s:\n",f1,f2);
			printf("Average difference: %f\n",totalDiff/npixels);
			printf("RMS difference: %f\n",sqrt(totalSqDiff/npixels));
			printf("Largest Absolute Difference:%f\n",largestDiff);
			printf("Smallest Absolute Difference:%f\n",smallestDiff);
		}
		else
		{
			printf("Comparing %s to %s:\n",f1,f2);
			printf("Average difference: %f\n",totalDiff/(npixels-ignored_pix_cnt));
			printf("RMS difference: %f\n",sqrt(totalSqDiff/(npixels-ignored_pix_cnt)));
			printf("Largest Absolute Difference:%f\n",largestDiff);
			printf("Smallest Absolute Difference:%f\n",smallestDiff);
			printf("Masking info:\n");
			printf(" Pixels accounted for: %d\n",(int)npixels-ignored_pix_cnt);
			printf(" Pixels ignored: %d\n",ignored_pix_cnt);
			printf(" Actual pixels in each image: %d\n",(int)npixels);
		}
	}
	printf("\n");
	return(0);
}

void usage(char *name)
{
	printf("\nUsage:\n");
	printf("   %s [-m mask_value] [-d difference.ext] <img1.ext> <img2.ext>\n",name);
	printf("\nOptional Inputs:\n");
	printf("   -m mask_value      Specify a value in the comparison images to be ignored\n");
	printf("   -d difference.ext  Create an image with pixels numerically equal to:\n");
	printf("                        difference.ext = img1.ext - img2.ext\n");
	printf("Required Inputs:\n");
	printf("   <img1.ext>   First comparison image (with extention)\n");
	printf("   <img2.ext>   Second comparison image (with extention)\n");
	printf("\nDescription:\n");
	printf("   Compares two LAS 6.0 images.\n");
	printf("   This is useful for debugging.\n");
	printf("\nVersion %.2f, ASF SAR TOOLS\n\n", VERSION);
  	exit(1);
}
