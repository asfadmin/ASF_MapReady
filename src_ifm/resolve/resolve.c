/****************************************************************
NAME: resolve

SYNOPSIS: resolve [-log <file>] [-quiet] 
                  <img1> <img2> <baseline> <ctrl-file>  
      

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
    VERS:   DATE:        AUTHOR:       PURPOSE:
    ---------------------------------------------------------------
    1.0                  S. Li
    1.1			 Z. Lu
    1.2     11/95        M. Shindle   revisions and porting
    2.0      5/96        M. Shindle   rewrite to produce either amp/phase
                                        files or cpx files.
    3.0      7/96        M. Shindle   completely revised. no longer modifies
				        data. Will only print results of 
				        correlation and initial baseline
				        estimation.
    3.1      3/97        T. Logan     baseline from windowing, usage of CCSD 
    3.2	     5/97	 T. Logan     added use of ddr file
    3.5	     5/97        O. Lawlor    Forced use of ddr file (They're good. Use
                                        them.) Did more error checking (It's
                                        good. Do it.) Made CLA's more sane.
    3.6	    10/97        O. Lawlor    Modified fico ctrl file output
    					for new,improved fico.
    3.6      3/98        D.Corbett    update version number
    4.0      5/98        O. Lawlor    Use fftMatch, instead of fft'ing yourself.
    4.1      6/98        O. Lawlor    Invert baseline deltas for CCSD--
                                        CCSD images are flipped vs. ASF images!
    5.0      7/98        O. Lawlor    Simpler, more accurate baseline estimator.
    5.1      9/00	 P. Denny     Multilook value read from metafile
    5.2      7/01	 R. Gens      Added logfile and quiet switch
    5.5      9/00	 P. Denny     Standardized command line parsing
                                        Updated to new meta struct
                                        DDR is gone by the way
                                        Changed amp2img call to convert2byte

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
#include "resolve.h"

/* local constants */
#define VERSION 5.5
#define BUFFER 1024

/* function declarations */
void usage(char *name);
void CreateFicoControl(char *ctrl,char *img1,char *img2,int multiLook);
void WriteFicoControl(char *,int,int);
void WriteBaseline(char *fnm, baseline b);

int main(int argc, char **argv)
{
  int multiLook;
  char img1[BUFFER],img2[BUFFER];
  char szBaseFile[BUFFER], szCtrlFile[BUFFER];
  char metaFile[BUFFER];
  meta_parameters *meta;
  
  /* Establish time structures and get initial time */
  system("date");
  printf("Program: resolve\n");

  /* Parse commandline */
  logflag=quietflag=FALSE;
  while (currArg < (argc-4)) {
    char *key = argv[currArg++];
    if (strmatch(key,"-log")) {
      CHECK_ARG(1);
      strcpy(logFile,GET_ARG(1));
      fLog = FOPEN(logFile, "a");
      logflag=TRUE;
    }
    else if (strmatch(key,"-quiet")) {
      quietflag=TRUE;
    }
    else {printf("\n**Invalid option:  %s\n",argv[currArg-1]); usage(argv[0]);}
  }
  if ((argc-currArg) < 4) {printf("Insufficient arguments.\n"); usage(argv[0]);}
  
  strcpy(img1,argv[currArg++]);
  strcpy(img2,argv[currArg++]);
  strcpy(szBaseFile,argv[currArg++]);
  strcpy(szCtrlFile,argv[currArg++]);
  create_name(metaFile, img1, ".meta");

  if (logflag) {
    StartWatchLog(fLog);
    printLog("Program: resolve\n");
  }
  
  if (!(meta=meta_read(metaFile)))
    printErr("   ERROR: Unable to either find or open metaFile.\n");
  else
    multiLook = meta->sar->look_count;
  meta_free(meta);
	    
  /* get baseline, write it out. */
  WriteBaseline(
    szBaseFile,
    find_baseline(img1,img2)
  );

  CreateFicoControl(szCtrlFile,img1,img2,multiLook);

  return 0;
}

void execute(char *cmd)
{
	if (0!=system(cmd))
	{
		sprintf(errbuf,"   ERROR: Command '%s' returned in error!\n",cmd);
		printErr(errbuf);
	}
}

void CreateFicoControl(char *ctrl,char *img1,char *img2, int multiLook)
{
	float offX,offY;
	char cmd[256],tmp[256],*offsetF="res_offsets";
	FILE *f;
	
	sprintf(cmd,"%s_amp.img",img1);
	if (!fileExists(cmd))
	{
		sprintf(cmd,"c2p %s %s",img1,img1);
		execute(cmd);
		sprintf(cmd,"convert2byte -look %dx1 -step %dx1 %s.amp %s_amp.img",multiLook,multiLook,img1,img1);
		if (logflag) sprintf(cmd,"convert2byte -look %dx1 -step %dx1 -log %s %s.amp %s_amp.img", 
						multiLook, multiLook, logFile, img1, img1);
		if (quietflag) sprintf(cmd,"convert2byte -look %dx1 -step %dx1 -quiet %s.amp %s_amp.img",multiLook,multiLook,img1,img1);
		if (logflag && quietflag) sprintf(cmd,"convert2byte -look %dx1 -step %dx1 -log %s -quiet %s.amp %s_amp.img",
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
		sprintf(cmd,"c2p %s %s",img2,img2);
		execute(cmd);
		sprintf(cmd,"convert2byte -look %dx1 -step %dx1 %s.amp %s_amp.img",multiLook,multiLook,img2,img2);
		if (logflag) sprintf(cmd,"convert2byte -look %dx1 -step %dx1 -log %s %s.amp %s_amp.img",
						multiLook, multiLook, logFile, img2, img2);
		if (quietflag) sprintf(cmd,"convert2byte -look %dx1 -step %dx1 -quiet %s.amp %s_amp.img",multiLook,multiLook,img2,img2);
		if (logflag && quietflag) sprintf(cmd,"convert2byte -look %dx1 -step %dx1 -log %s -quiet %s.amp %s_amp.img",
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

void usage(char *name) {
 printf("\n"
	"USAGE:\n"
	"   %s [-log <file>] [-quiet] <img1> <img2> <basefile> <ctrlfile>\n",name);
 printf("\n"
	"REQUIRED ARGUMENTS:\n"
	"   <img1>      1st input image (.cpx and .meta).\n"
	"   <img2>      2nd input image (.cpx and .meta).\n"
	"   <basefile>  Output file to write calculated baseline.\n"
	"   <ctrlfile>  Optional output file to be used as a parameter file for fico.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log <file>   Allows the output to be written to a log file.\n"
	"   -quiet        Suppresses the output to the essential.\n");
 printf("\n"
 	"DESCRIPTION:\n"
	"   This program is used during the interferometry process in order to perform\n"
	"   single-pixel correlation as a basis for fico(1) to perform sub-pixel\n"
	"   correlation. It estimates an initial offset using the state vectors of the\n"
	"   imaging satellite, read from the metadata; then refines this initial\n"
	"   guess using fftMatch.\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
