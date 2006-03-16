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
*       Alaska Satellite Facility                                             *
*       Geophysical Institute                   http://www.asf.alaska.edu     *
*       University of Alaska Fairbanks          uso@asf.alaska.edu            *
*       P.O. Box 757320                                                       *
*       Fairbanks, AK 99775-7320                                              *
*                                                                             *
******************************************************************************/
/*******************************************************************************
NAME: coregister_coarse

SYNOPSIS:
	coregister_coarse [-log <file>] [-quiet]
			  <img1> <img2> <baseline> <ctrl-file>

DESCRIPTION:
       Using orbital metadata and a FFT on the data files, coregister_coarse
       calculates the pixel offset of the second image to the first.
       Coregister_coarse also creates a baseline file called baseline. This file
       contains the calculated baseline coordinates (both parallel and
       perpendicular) of the two images plus the rate of change of these two
       values across the scene. The file format is perpendicular baseline, rate
       of change, parallel baseline, and rate of change. The rate of change is
       determined in meters per scene.

       Coregister_coarse also creates a parameter file to be used with
       coregister_fine (used to be fico), the subpixel image registration
       program. It does this by lining up the amplitude images of the given
       complex scenes with fftMatch

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
    1.1                  Z. Lu
    1.2     11/95        M. Shindle   revisions and porting
    2.0      5/96        M. Shindle   rewrite to produce either amp/phase
                                        files or cpx files.
    3.0      7/96        M. Shindle   completely revised. no longer modifies
                                        data. Will only print results of
                                        correlation and initial baseline
                                        estimation.
    3.1      3/97        T. Logan     baseline from windowing, usage of CCSD
    3.2      5/97        T. Logan     added use of ddr file
    3.5      5/97        O. Lawlor    Forced use of ddr file (They're good. Use
                                        them.) Did more error checking (It's
                                        good. Do it.) Made CLA's more sane.
    3.6     10/97        O. Lawlor    Modified fico ctrl file output for new
                                        improved fico.
    3.6      3/98        D.Corbett    update version number
    4.0      5/98        O. Lawlor    Use fftMatch, instead of fft'ing yourself.
    4.1      6/98        O. Lawlor    Invert baseline deltas for CCSD--
                                        CCSD images are flipped vs. ASF images!
    5.0      7/98        O. Lawlor    Simpler, more accurate baseline estimator.
    5.1      9/00        P. Denny     Multilook value read from metafile
    5.2      7/01        R. Gens      Added logfile and quiet switch
    5.5      6/03        P. Denny     Standardized command line parsing
                                        Updated to new meta struct
                                        DDR is gone by the way
                                        Changed amp2img call to convert2byte
    5.6      2/04        P. Denny     Change license from GPL to ASF
                                        Change name from resolve to
                                         coregister_coarse
    5.7     12/05        P. Denny     Call c2p correctly. Lots of command line
                                         call reshuffling

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
	Coregister_coarse is used during the interferometry process in order to
	perform single-pixel correlation, as a basis for coregister_fine (used
	to be fico) to perform sub-
	pixel correlation.  Coregister_coarse estimates an initial offset using
	the state vectors of the imaging satellite, read from the metadata; then
	refines this initial guess using an FFT.

ALGORITHM REFERENCES:

BUGS:

*******************************************************************************/

#include "coregister_coarse.h"

/* local constants */
#define VERSION 5.7
#define BUFFER 1024

#define SIZEOF_CMD 256

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
	char metaFile[BUFFER],meta1[BUFFER],meta2[BUFFER];
	meta_parameters *meta;

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
		else {
			printf("\n**Invalid option:  %s\n",argv[currArg-1]);
			usage(argv[0]);
		}
	}
	if ((argc-currArg) < 4) {
		printf("Insufficient arguments.\n");
		usage(argv[0]);
	}

	/* Get required arguments */
	strcpy(img1,argv[currArg++]);
	strcpy(img2,argv[currArg++]);
	strcpy(szBaseFile,argv[currArg++]);
	strcpy(szCtrlFile,argv[currArg++]);
	create_name(metaFile, img1, "_amp.meta");

	/* Start off with a bit of reporting */
	if (logflag) {
		StartWatchLog(fLog);
		printLog("Program: coregister_coarse\n");
	}
	system("date");
	printf("Program: coregister_coarse\n");

	/* Figure out if we have to do any multilooking */
	if (!(meta=meta_read(metaFile))) {
		printErr("   ERROR: Unable to either find or open metaFile.\n");
	}
	else {
		multiLook = meta->sar->look_count;
	}
	meta_free(meta);

	/* get baseline, write it out. */
	create_name(meta1, img1, "_amp");
	create_name(meta2, img2, "_amp");
	WriteBaseline( szBaseFile, find_baseline(img1,img2) );

	/* write the ctrl file for use with coregister_fine (aka fico) */
	CreateFicoControl(szCtrlFile,img1,img2,multiLook);

	exit (EXIT_SUCCESS);
}

void execute(char *cmd)
{
	char report[1024];

	/* Report (& log) the command line */
	sprintf(report,"\nExecuting: %s\n",cmd);
	printf(report);
	fflush(NULL);
	if (logflag) {
		printLog(report);
	}

	/* Make the call & report any errors */
	if (0!=system(cmd))
	{
		sprintf(errbuf,"   ERROR: Command '%s' returned in error!\n",cmd);
		printErr(errbuf);
	}
}

void cpx_2_amp_byte(char *img, int multiLook)
{
	char tmp[256];
	sprintf(tmp,"%s_amp.img",img);

	if (!fileExists(tmp)) {
		char cmd[SIZEOF_CMD];
		char tmp_cmd[SIZEOF_CMD];
		char c2p_out[256];
		char convert2byte_out[256];

		/* Convert complex to amp & phase */
		sprintf(c2p_out,"%s_c2p",img);
		sprintf(cmd,"c2p %s %s",img,c2p_out);
		execute(cmd);

		/* Convert amp image to byte */
		sprintf(convert2byte_out,"%s_amp",img);
		sprintf(cmd," -look %dx1 -step %dx1 %s.amp %s.img",
			multiLook,multiLook,c2p_out,convert2byte_out);
		if (logflag) {
			sprintf(cmd," -log %s %s",
				logFile, strcpy(tmp_cmd,cmd));
		}
		if (quietflag) {
			sprintf(cmd," -quiet %s", strcpy(tmp_cmd,cmd));
		}
		sprintf(cmd,"convert2byte %s", strcpy(tmp_cmd,cmd));
		execute(cmd);
	}
}

void CreateFicoControl(char *ctrl,char *img1,char *img2, int multiLook)
{
	float offX,offY;
	char cmd[SIZEOF_CMD];
	char tmp_cmd[SIZEOF_CMD];
	char *offsetF="res_offsets";
	FILE *f;

	cpx_2_amp_byte(img1, multiLook);
	cpx_2_amp_byte(img2, multiLook);

	/* Line up our two images to about the pixel level */
	sprintf(cmd," -m %s %s_amp.img %s_amp.img ",offsetF,img1,img2);
	if (logflag) {
		sprintf(cmd," -log %s %s ", logFile,strcpy(tmp_cmd,cmd));
	}
	if (quietflag) {
		sprintf(cmd," -quiet %s ", strcpy(tmp_cmd,cmd));
	}
	sprintf(cmd,"fftMatch %s", strcpy(tmp_cmd,cmd));
	execute(cmd);

	/* Read fftMatch output & use it */
	f=fopen(offsetF,"r");
	if (f==NULL || 2!=fscanf(f,"%f%f",&offX,&offY)) {
		sprintf(errbuf,"   ERROR: Couldn't extract offset parameters from file %s!\n",offsetF);
		printErr(errbuf);;
	}
	fclose(f);
	unlink(offsetF);
	offY*=multiLook;
	sprintf(logbuf,"   Complex image offset is %d rows, %d columns\n\n",(int)offY,(int)offX);
	printf("%s",logbuf);
	if (logflag) {
	  printLog(logbuf);
	}
	WriteFicoControl(ctrl,(int)offX,(int)offY);
}

void WriteFicoControl(char *fnm, int xoff, int yoff)
{
	int chip_size = 32;  /* this value must be a power of 2, usually 16 */
	int os = 1;          /* this value is also a power of 2, usually 4 */
	float xmep = 4.1;    /* x maximum error pixel value that is accepted */
	float ymep = 6.1;    /* y maximum error pixel value that is accepted */
	FILE *fp;
	fp=FOPEN(fnm,"w");
	fprintf(fp,"%d\n%d\n%d\n%d\n%f\n%f\n",
		xoff, yoff, chip_size, os,xmep,ymep);
	FCLOSE(fp);
	return;
}

void WriteBaseline(char *fnm, baseline b)
{
	FILE *fp=FOPEN(fnm,"w");

	sprintf(logbuf,
		"\n   Baseline: Bn = %f, dBn = %f, Bp = %f, dBp = %f, Btemp = %f\n",
		b.Bn,b.dBn,b.Bp,b.dBp,b.temporal);
	printf("%s",logbuf);
	if (logflag) { printLog(logbuf); }

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
	"   <img1>      1st input image.\n"
	"   <img2>      2nd input image.\n"
	"   <basefile>  Output file to write calculated baseline.\n"
	"   <ctrlfile>  Output file to be used as a parameter file for coregister_fine.\n");
 printf("\n"
	"OPTIONAL ARGUMENTS:\n"
	"   -log <file>   Allows the output to be written to a log file.\n"
	"   -quiet        Suppresses the output to the essential.\n");
 printf("\n"
	"DESCRIPTION:\n"
	"   This program is used during the interferometry process in order to perform\n"
	"   single-pixel correlation as a basis for coregister_fine to perform sub-pixel\n"
	"   correlation. It estimates an initial offset using the state vectors of the\n"
	"   imaging satellite, read from the metadata; then refines this initial\n"
	"   guess using fftMatch.\n");
 printf("\n"
	"Version: %.2f, ASF InSAR Tools\n"
	"\n",VERSION);
 exit(EXIT_FAILURE);
}
