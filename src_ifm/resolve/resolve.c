/****************************************************************
NAME: resolve

SYNOPSIS: resolve    <img1>  <img2>  <baseline>  <ctrl-file>   [-log
       <file>] [-quiet]

DESCRIPTION:
	
       Using  orbital  metadata  and  a  FFT  on  the data files,
       resolve calculates the pixel offset of the second image to
       the  first.   Resolve  also creates a baseline file called
       baseline. This file contains the calculated baseline coor-
       dinates  (both  parallel  and  perpendicular)  of  the two
       images plus the rate of change of these two values  across
       the scene. The file format is perpendicular baseline, rate
       of change, parallel baseline, and rate of change. The rate
       of change is determined in meters per scene.

       Resolve  also  creates  a  parameter  file to be used with
       fico, the sub-pixel image registration program.   It  does
       this  by  lining up the amplitude images of the given com-
       plex scenes with fftMatch


EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:        PURPOSE:
    ---------------------------------------------------------------
    1.0                  S. Li
    1.1			 Z. Lu
    1.2     11/95        M. Shindle - revisions and porting
    2.0      5/96        M. Shindle - rewrite to produce either amp/phase
			       files or cpx files.
    3.0      7/96        M. Shindle - completely revised. no longer modifies
				      data. Will only print results of 
				      correlation and initial baseline
				      estimation.
    3.1      3/97        T. Logan - baseline from windowing, usage of CCSD 
    3.2	     5/97	 T. Logan - added use of ddr file
    3.5	     5/97        O. Lawlor - Forced use of ddr file (They're 
good. Use them.)
    				     Did more error checking (It's good. Do it.)
    				     Make CLA's more sane.
    3.6	     10/97        O. Lawlor - Modified fico ctrl file output
    					for new,improved fico.
    "        03/08/98     D.Corbett - update version number
    
    4.0      5/98         O. Lawlor - Use fftMatch, instead of fft'ing yourself.
    4.1      6/98         O. Lawlor - Invert baseline deltas for CCSD--
                                    CCSD images are flipped vs. ASF images!
    5.0      7/98         O. Lawlor - Simpler, more accurate baseline estimator.
    5.1      9/00	  P. Denny - multilook value read from metafile
    5.2      7/01	  R. Gens - added logfile and quiet switch

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
	Resolve is used during the interferometry process in order
to perform single-pixel correlation, as a basis for fico to perform
sub-pixel correlation.  Resolve estimates an initial offset using the 
state vectors of the imaging satellite, read from the metadata;
then refines this initial guess using an FFT.

ALGORITHM REFERENCES:

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   resolve  - calculate the pixel offset and baseline between two images   *
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
#include "resolve.h"
#include "ddr.h"

/* local constants */
#define VERSION 5.2

/* function declarations */
void print_usage();
void CreateFicoControl(char *ctrl,char *img1,char *img2,int multiLook);
void WriteFicoControl(char *,int,int);
void WriteBaseline(char *fnm, baseline b);

#define BUFFER 1024
int main(int argc, char **argv) {
  int multiLook,i;
  char img1[BUFFER],img2[BUFFER];
  char szBaseFile[BUFFER], szCtrlFile[BUFFER];
  char metaFile[256];
  meta_parameters *meta;
  
  /* Establish time structures and get initial time */
  system("date");
  printf("Program: resolve\n");

  if (argc < 5) 
  	print_usage();
  
  logflag=0;	
  strcpy(img1,argv[1]);
  strcpy(img2,argv[2]);
  strcpy(szBaseFile,argv[3]);
  strcpy(szCtrlFile,argv[4]);
  create_name(metaFile, argv[optind],".meta");
  for (i=5; i<argc; i++) {
    if(strncmp(argv[i],"-log", 4)==0) {
      sscanf(argv[i+1], "%s", logFile);
      logflag=1;
      fLog = FOPEN(logFile, "a");
      i+=1;
    }
    else if(strncmp(argv[i],"-quiet", 6)==0) quietflag=1;
    else {
      sprintf(errbuf,"   ERROR: '%s' is not a valid option\n", argv[i]);
      printErr(errbuf);
    }
  }

  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: resolve\n");
  }
  
  if (!(meta=meta_init(metaFile)))
    printErr("   ERROR: Unable to either find or open metaFile.\n");
  else
	multiLook = meta->ifm->nLooks;
  meta_free(meta);
	    
  /* get baseline, write it out. */
  WriteBaseline(
  	szBaseFile,
  	find_baseline(img1,img2)
  );

  CreateFicoControl(szCtrlFile,img1,img2,multiLook);

/*  printf("resolve: ends successfully\n");*/
  return 0;
}

void execute(char *cmd)
{
/*	printf("Resolve: executing '%s'\n",cmd);*/
	if (0!=system(cmd))
	{
		sprintf(errbuf,"   ERROR: Command '%s' returned in error!\n",cmd);
		printErr(errbuf);
	}
}

void CreateFicoControl(char *ctrl,char *img1,char *img2, int multiLook)
{
	/*int createdAmp1=0,createdAmp2=0;*/
	float offX,offY;
	char cmd[256],tmp[256],*offsetF="res_offsets";
	FILE *f;
	
	sprintf(cmd,"%s_amp.img",img1);
	if (!fileExists(cmd))
	{
		/*createdAmp1=1;*/
		sprintf(cmd,"c2p %s %s",img1,img1);
		execute(cmd);
		sprintf(cmd,"amp2img -look %dx1 -step %dx1 %s.amp %s_amp.img",multiLook,multiLook,img1,img1);
		if (logflag) sprintf(cmd,"amp2img -look %dx1 -step %dx1 -log %s %s.amp %s_amp.img", 
						multiLook, multiLook, logFile, img1, img1);
		if (quietflag) sprintf(cmd,"amp2img -look %dx1 -step %dx1 -quiet %s.amp %s_amp.img",multiLook,multiLook,img1,img1);
		if (logflag && quietflag) sprintf(cmd,"amp2img -look %dx1 -step %dx1 -log %s -quiet %s.amp %s_amp.img",
						multiLook, multiLook, logFile, img1, img1);
		sprintf(tmp, "Command line: %s\n", cmd);
		printf(tmp);
		if (logflag) {
		  printLog(tmp);
		  FCLOSE(fLog);
		}
		execute(cmd);
	}
	sprintf(cmd,"%s_amp.img",img2);
	if (!fileExists(cmd))
	{
		/*createdAmp2=1;*/
		sprintf(cmd,"c2p %s %s",img2,img2);
		execute(cmd);
		sprintf(cmd,"amp2img -look %dx1 -step %dx1 %s.amp %s_amp.img",multiLook,multiLook,img2,img2);
		if (logflag) sprintf(cmd,"amp2img -look %dx1 -step %dx1 -log %s %s.amp %s_amp.img",
						multiLook, multiLook, logFile, img2, img2);
		if (quietflag) sprintf(cmd,"amp2img -look %dx1 -step %dx1 -quiet %s.amp %s_amp.img",multiLook,multiLook,img2,img2);
		if (logflag && quietflag) sprintf(cmd,"amp2img -look %dx1 -step %dx1 -log %s -quiet %s.amp %s_amp.img",
						multiLook, multiLook, logFile, img2, img2);
		sprintf(tmp, "Command line: %s\n", cmd);
		printf(tmp);
		if (logflag) {
		  fLog = FOPEN(logFile, "a");
		  printLog(tmp);
		  FCLOSE(fLog);
		}
		execute(cmd);
	}
	sprintf(cmd,"fftMatch -m %s -c reg_cor.img %s_amp.img %s_amp.img",offsetF,img1,img2);
	if (logflag) {
	  FCLOSE(fLog); 
	  sprintf(cmd,"fftMatch -m %s -c reg_cor.img -log %s %s_amp.img %s_amp.img",offsetF,logFile,img1,img2);
	}
	if (quietflag) sprintf(cmd,"fftMatch -m %s -c reg_cor.img -quiet %s_amp.img %s_amp.img",offsetF,img1,img2);
	if (logflag && quietflag) 
	  sprintf(cmd,"fftMatch -m %s -c reg_cor.img -log %s -quiet %s_amp.img %s_amp.img",offsetF,logFile,img1,img2);
	execute(cmd);
	if (logflag) fLog = FOPEN(logFile, "a");
	f=fopen(offsetF,"r");
	if (f==NULL || 2!=fscanf(f,"%f%f",&offX,&offY))
	{
		sprintf(errbuf,"   ERROR: Couldn't extract offset parameters from file %s!\n",offsetF);
		printErr(errbuf);;
	}
	fclose(f);
	unlink(offsetF);
	offY*=multiLook;
	printf("   Complex image offset is %d rows, %d columns\n\n",(int)offY,(int)offX);
	if (logflag) {
	  sprintf(logbuf,"   Complex image offset is %d rows, %d columns\n\n",(int)offY,(int)offX);
	  printLog(logbuf);
	}
	WriteFicoControl(ctrl,(int)offX,(int)offY);
/*Clean up after ourselves.*/
/* It is very annoying to have the program remove these files automatically */
/*	sprintf(cmd,"/bin/rm %s.amp %s.phase",img1,img1);
	if (createdAmp1)
		execute(cmd);
	sprintf(cmd,"/bin/rm %s.amp %s.phase %s_amp.*",img2,img2,img2);
	if (createdAmp2)
		execute(cmd);*/
}

void WriteFicoControl(char *fnm, int xoff, int yoff)
{
   int chip_size = 32;     /* this value must be a power of 2, usually 16 */
   int os = 1;             /* this value is also a power of 2, usually 4 */
   float xmep = 4.1;       /* x maximum error pixel value that is accepted */
   float ymep = 6.1;       /* y maximum error pixel value that is accepted */
   FILE *fp;
   fp=FOPEN(fnm,"w");
   fprintf(fp,"%d\n%d\n%d\n%d\n%f\n%f\n", xoff, yoff, chip_size, os,xmep,ymep);
   FCLOSE(fp);
   return;
}

void WriteBaseline(char *fnm, baseline b)
{
   FILE *fp=FOPEN(fnm,"w");
   
   printf("\n   Baseline: Bn = %f, dBn = %f, Bp = %f, dBp = %f, Btemp = %f\n",
   	b.Bn,b.dBn,b.Bp,b.dBp,b.temporal);
   if (logflag) {
     sprintf(logbuf,"\n   Baseline: Bn = %f, dBn = %f, Bp = %f, dBp = %f, Btemp = %f\n",
   	b.Bn,b.dBn,b.Bp,b.dBp,b.temporal);
     printLog(logbuf);
   }
   
   fprintf(fp,"%f  %f  %f  %f %f\n",b.Bn,b.dBn,b.Bp,b.dBp,b.temporal);
   FCLOSE(fp);
   return;
}

void print_usage() {
  fprintf(stderr,"resolve: Estimate the offset between two images.\n");
  fprintf(stderr,"\nUSAGE: resolve <img1> <img2> <basefile> <ctrlfile> [-log <file>] [-quiet]\n\n");
  fprintf(stderr,"\t<img1>\t\t1st input image (.cpx and .ddr, with metadata).\n");
  fprintf(stderr,"\t<img2>\t\t2nd input image (.cpx and .ddr, with metadata).\n");
  fprintf(stderr,"\t<basefile>\toutput file to write calculated baseline.\n");
  fprintf(stderr,"\t<ctrlfile>\toptional output file to be used as a parameter \n\
                        file for fico.\n");
  fprintf(stderr,"\t-log <file>\tallows the output to be written to a log file.\n");
  fprintf(stderr,"\t-quiet\t\tsuppresses the output to the essential.\n\n");
  fprintf(stderr,"Resolve is used during the interferometry process in order\n\
to perform single-pixel correlation, as a basis for fico to perform\n\
sub-pixel correlation.  Resolve estimates an initial offset using the \n\
state vectors of the imaging satellite, read from the metadata;\n\
then refines this initial guess using fftMatch.\n");
  fprintf(stderr,"\nVersion: %.2f, ASF SAR TOOLS\n\n",VERSION);
  exit(1);
}
