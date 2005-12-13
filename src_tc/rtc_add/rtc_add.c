/****************************************************************
NAME:  image add --  add images

SYNOPSIS:	rtc_add sarfile rtcfile outfile

DESCRIPTION:
  Adds the terrain corrected byte image named sarfile with the
  float valued image named rtcfile to produce the output file
  named outfile.

EXTERNAL ASSOCIATES:  None

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    rtcf_file		 Radiometric Terrain Correction Factors
			 (DEM image size file of floats)
    sar_file		 Input Terrain Corrected SAR image
    out_file	  	 Radiometrically Corrected SAR image

PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:       PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/95    T. Logan	  Initial Creation
    1.1     8/95    T. Logan      Modified to accept pixel size
    2.0	    8/95    T. Logan	  Modified to use DEM space images
    3.0	    10/95   T. Logan      Ported to Solaris
    3.1    6/98     O. Lawlor     ASF/STEP      Removed non-ANSI Timer.

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
#include "ddr.h"

#define VERSION 3.1

int main(argc,argv)
   int argc;
   char** argv;
{
char	rtcf_file[255], sar_file[255], out_file[255], cmd[255];
FILE	*fp_rtcf, *fp_sar, *fp_out;
struct  DDR ddr;
float   *rtcf_buf, *inbuf;
int	i, j, ns, nl;
float   min, max, d_ave, l_ave;
int	dark, light, neg;
float	values[10];
int	count[10];

StartWatch();

if (argc!=4)
  {
   printf("Usage: %s sarfile rtcfile outfile\n",argv[0]);
   printf("   inputs: sarfile.img  (.img extension assumed)\n");
   printf("           rtcfile.img  (.raw extension assumed)\n");
   printf("   output: outfile.img, Byte image in LAS 6.0 format\n");
   printf("\n   Version %.2f,  ASF Tools\n",VERSION);
   exit(1);
  }

strcpy(sar_file,argv[1]);
strcat(strcpy(rtcf_file,argv[2]),".img");
strcat(strcpy(out_file,argv[3]),".img");

c_getddr(sar_file,&ddr);
strcat(sar_file,".img");
ns = ddr.ns;
nl = ddr.nl;
min = max = 1.0;
dark = light = neg = 0;
d_ave = l_ave = 0.0;
for (i = 0; i<10; i++) { values[i] = (float) i*0.2; count[i] = 0; }

printf("Applying radiometric terrain correction factors.\n");
printf("Factors: %s  Sar: %s  Output: %s\n",rtcf_file,sar_file,out_file);

inbuf = (float *) MALLOC (ns * sizeof(float));
rtcf_buf = (float *) MALLOC (ns * sizeof(float));

fp_rtcf = fopenImage(rtcf_file,"rb");
fp_sar = fopenImage(sar_file,"rb");
fp_out = fopenImage(out_file,"wb");

for (i = 0; i < nl; i++)
 {
   getFloatLine(fp_rtcf, &ddr, i, rtcf_buf);
   getFloatLine(fp_sar, &ddr, i, inbuf);
   for (j = 0; j < ns; j++)
      if (inbuf[j]>0)
       {
        if (rtcf_buf[j]>0)
	  //  Changed, since inbuf is no longer 0-255. -kh
          // rtcf_buf[j] = log10(rtcf_buf[j])*10.0 + (inbuf[j]/10.0 - 25.5);
          rtcf_buf[j] = log10(rtcf_buf[j])*10.0 + inbuf[j];
        else rtcf_buf[j] = 0;
       }
      else rtcf_buf[j] = 0;
   putFloatLine(fp_out, &ddr, i, rtcf_buf);
 }

strcat(strcpy(sar_file,argv[1]),".ddr");
strcat(strcpy(out_file,argv[3]),".ddr");
sprintf(cmd,"cp %s %s\n",sar_file,out_file);
if (system(cmd) != 0)
  { printf("Failure copying metadata: Program Exited\n\n"); exit(1); }

fclose(fp_rtcf);
fclose(fp_sar);
fclose(fp_out);

StopWatch();

printf("\nRTC_ADD Successfully Completed \n");

exit(0);
}
