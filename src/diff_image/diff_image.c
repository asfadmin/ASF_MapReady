/******************************************************************************
NAME:  diff_image

SYNOPSIS:  diff_image [-d difference.ext] [-m mask_val] <img1.ext> <img2.ext>

DESCRIPTION:  compare ASF tools format images, for debugging and verification
	      (especially between platforms).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:  DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0    1997   Orion Lawlor
    1.1    7/01   P. Denny     Change byte output to signed short input for
                                 better visualization
    1.5    2/02   P. Denny     Add masking option, standardize command line
                                 arguments
    2.0    1/03   P. Denny     Update to use meta struct instead of DDR struct
    2.1    2/04   P. Denny     Change license to BSD. Change name from diff_las
                                 to diff_image. Fixed bad initial value of mask
                                 from -1 to NaN.

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/******************************************************************************
*                                                                             *
* Copyright (c) 2004, Geophysical Institute, University of Alaska Fairbanks   *
* All rights reserved.                                                        *
*                                                                             *
* Redistribution and use in source and binary forms, with or without          *
* modification, are permitted provided that the following conditions are met: *
*                                                                             *
*    * Redistributions of source code must retain the above copyright notice, *
*      this list of conditions and the following disclaimer.                  *
*    * Redistributions in binary form must reproduce the above copyright      *
*      notice, this list of conditions and the following disclaimer in the    *
*      documentation and/or other materials provided with the distribution.   *
*    * Neither the name of the Geophysical Institute nor the names of its     *
*      contributors may be used to endorse or promote products derived from   *
*      this software without specific prior written permission.               *
*                                                                             *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" *
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE   *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE  *
* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE    *
* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR         *
* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF        *
* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS    *
* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     *
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)     *
* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE  *
* POSSIBILITY OF SUCH DAMAGE.                                                 *
*                                                                             *
*       For more information contact us at:                                   *
*                                                                             *
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/

#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"

#define VERSION 2.1
#define REQ_ARGS 2

void usage(char *name);

int main(int argc, char **argv)
{
#define hist_bins 50
	const float span=1.0;
	int xx,yy;
	float *line_buf1, *line_buf2;
	float *diff_line_buf;
	double totalDiff, totalSqDiff;          /* */
	double smallestDiff, largestDiff;       /* Largest & smallest difference found between images */
	double npixels;                         /* Number of pixels in each image           */
	float mask = NAN;                       /* Value to ignore while doing statistics   */
	int ignored_pix_cnt;                    /* Number of pixels ignored when using mask */
	int *histogram;                         /* Histogram pointer                        */
	int line_count, sample_count;           /* Number of lines & samples for all images */
	char *in_file_name1, *in_file_name2;    /* Name for images to be compared           */
	char *diff_file_name = NULL;            /* Name for difference file                 */
	FILE *fp1, *fp2;                        /* File pointers for images to be compared  */
	FILE *diff_fp = NULL;                   /* File pointer for difference image        */
	meta_parameters *meta1, *meta2;         /* Meta files for images to be compared     */
	meta_parameters *diff_meta;             /* Meta file for difference image           */
	extern int currArg;                     /* From cla.h in asf.h                      */

/* Parse command line args */
	while (currArg < (argc-REQ_ARGS))
	{
		char *key=argv[currArg++];
		if (strmatch(key,"-d")) {
			CHECK_ARG(1)
			diff_file_name = GET_ARG(1);
		}
		else if (strmatch(key,"-m")) {
			CHECK_ARG(1)
			mask = atof(GET_ARG(1));
		}
		else {printf("\n**Invalid option:  %s\n\n",argv[currArg-1]);usage(argv[0]);}
	}
	if ((argc-currArg) < REQ_ARGS) {printf("Insufficient arguments.\n"); usage(argv[0]);}
	in_file_name1 = argv[currArg++];
	in_file_name2 = argv[currArg];

	meta1 = meta_read(in_file_name1);
	meta2 = meta_read(in_file_name2);

/* Check for bad metadata differences */
	if (meta1->general->line_count != meta2->general->line_count) {
		printf("The number of lines is different.\n");
		exit(EXIT_FAILURE);
	}
	if (meta2->general->sample_count != meta2->general->sample_count) {
		printf("The number of samples is different.\n");
		exit(EXIT_FAILURE);
	}
	if (meta1->general->data_type != meta2->general->data_type) {
		printf("The data types are different.\n");
		exit(EXIT_FAILURE);
	}
	if (meta1->general->start_line != meta2->general->start_line)
		printf("The start line fields do not match:%i vs %i\n",
			meta1->general->start_line,
			meta2->general->start_line);
	if (meta1->general->start_sample != meta2->general->start_sample)
		printf("The start sample fields do not match:%i vs %i\n",
			meta1->general->start_sample,
			meta2->general->start_sample);
	
/* If user wants a diff image, prepare it */
	if (diff_file_name)
	{
		diff_meta = meta_copy(meta1);
		if (diff_meta->general->data_type == BYTE) {
			diff_meta->general->data_type = INTEGER16;
		}
		diff_fp = fopenImage(diff_file_name,"wb");
		meta_write(diff_meta, diff_file_name);
	}
/* Prepare data for processing */
	line_count    = meta1->general->line_count;
	sample_count  = meta1->general->sample_count;
	fp1           = fopenImage(in_file_name1,"rb");
	fp2           = fopenImage(in_file_name2,"rb");
	line_buf1     = (float *)MALLOC(sizeof(float)*sample_count);
	line_buf2     = (float *)MALLOC(sizeof(float)*sample_count);
	diff_line_buf = (float *)MALLOC(sizeof(float)*sample_count);
	histogram     = (int *)MALLOC((hist_bins+1)*sizeof(int));
	for (xx=0; xx<=hist_bins; xx++)
		histogram[xx] = 0;
	totalDiff    = 0.0;
	totalSqDiff  = 0.0;
	smallestDiff = 100000000000.0;
	largestDiff  = 0.0;
	npixels = (float)line_count * (float)sample_count;

/* Do the processing */
	for (yy=0; yy<line_count; yy++)
	{
		get_float_line(fp1,meta1,yy,line_buf1);
		get_float_line(fp2,meta2,yy,line_buf2);
		for (xx=0; xx<sample_count; xx++)
		{
			register float diff,l1=line_buf1[xx],l2=line_buf2[xx];
			int histIndex;
			if ((l1==mask) || (l2==mask))
			{
				diff = diff_line_buf[xx] = mask;
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
			
			diff               = l1-l2;
			diff_line_buf[xx]  = diff;
			totalDiff         += diff;
			totalSqDiff       += diff*diff;
			if (diff<0) diff=-diff;
			if (largestDiff<diff) largestDiff=diff;
			if (smallestDiff>diff) smallestDiff=diff;
		}
		if (diff_fp)
			put_float_line(diff_fp,diff_meta,yy,diff_line_buf);
		if (yy%100==0) {
			printf("\tComparing line %i...\r",yy);
			fflush(NULL);
		}
	}
	printf("\tCompared %i lines.     \n", yy);

/* If files differ enough, write out histogram and other stats for user */
	if (histogram[hist_bins/2]/npixels<0.995)
	{
		printf("\n");
		printf("Files differ by more than 0.5%%!\n");
		printf("Relative Error Histogram:\n Diff:  Frequency:\n");
		for (xx=0;xx<=hist_bins;xx++)
			if (histogram[xx])
				printf("%6.2f %7.4f%%\n",xx/(50.0/2)*span-span,histogram[xx]/npixels*100);
		if (mask==-1.0)
		{
			printf("Comparing %s to %s:\n",in_file_name1,in_file_name2);
			printf("Average difference: %f\n",totalDiff/npixels);
			printf("RMS difference: %f\n",sqrt(totalSqDiff/npixels));
			printf("Largest Absolute Difference:%f\n",largestDiff);
			printf("Smallest Absolute Difference:%f\n",smallestDiff);
		}
		else
		{
			printf("Comparing %s to %s:\n",in_file_name1,in_file_name2);
			printf("Average difference: %f\n",totalDiff/(npixels-ignored_pix_cnt));
			printf("RMS difference: %f\n",sqrt(totalSqDiff/(npixels-ignored_pix_cnt)));
			printf("Largest Absolute Difference:%f\n",largestDiff);
			printf("Smallest Absolute Difference:%f\n",smallestDiff);
			printf("Masking info:\n");
			printf(" Pixels accounted for: %d\n",(int)npixels-ignored_pix_cnt);
			printf(" Pixels ignored: %d\n",ignored_pix_cnt);
			printf(" Actual pixels in each image: %d\n",(int)npixels);
		}
		printf("\n");
	}
/* Clean up allocated memory */
	meta_free(meta1);
	FCLOSE(fp1);
	meta_free(meta2);
	FCLOSE(fp2);
	if (diff_fp) {
		meta_free(diff_meta);
		FCLOSE(diff_fp);
	}

	return(0);
}

void usage(char *name)
{
 printf("\n"
	"USAGE:\n"
	"   %s [-m mask_value] [-d difference.ext] <img1.ext> <img2.ext>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   img1.ext  First comparison image (with extention)\n"
	"   img2.ext  Second comparison image (with extention)\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -m mask_value  Specify a value in the comparison images to be ignored\n"
	"   -d diff.ext    Create an image with pixels numerically equal to:\n"
	"                        diff.ext = img1.ext - img2.ext\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   Compares two ASF tools format images.\n"
	"   This is useful for debugging.\n");
 printf("\n"
	"Version %.2f, ASF SAR Tools\n"
	"\n", VERSION);
 exit(1);
}
