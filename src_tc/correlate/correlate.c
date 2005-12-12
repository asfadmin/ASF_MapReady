/****************************************************************
NAME:	correlate.c - correlates two LAS 6.0 images

SYNOPSIS:   correlate sim sar coef [log]
	sim   - input simulated SAR file (image1)
 	sar   - input SAR file (image2)
	coef  - output coefficients file
	log   - log file name

DESCRIPTION:
	Implements a image to image correlation specifically for
   use on a SAR image and a simulated SAR image.  If the first
   correlation fails (more than 1/2 of the in-bounds points are
   bad correlations) then the images are downsampled and the process
   is repeated until the correlation suceeds, or the images are too small.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    terrcorr		 Calling Program
    resample		 Called - downsize and image
    tpl_search		 Called - search an image for tie point locations
    fftMatch             Called - finds block offset between images
    greycorr		 Called - Attempts correlation at tie points
    tpl_mult 		 Called - Scales tie point file
    2dmap		 Called - Fits a 2dmap function to tie points

FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    sim, sar		 Full-res images to be correlated
    lsim, lsar		 Downsized images to be correlated
    tpl1		 Output of tpl_search
    tpl2		 Output of greycorr
    tpl3		 Output of tpl_mult
    tpl4		 Output of greycorr (high res)
    coef		 Output of 2dmap - coefficients file 

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    2.0      8/97  T. Logan     Back Port from T3E - 2 pass correlation
    3.0      5/98  O. Lawlor    N pass correlation
    3.1      2/99  O. Lawlor    Minor tweak to correlate call; new resample
    3.2      5/99  O. Lawlor    Uses fftMatch to estimate image offset

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
#include <unistd.h>
#include "ddr.h"

#define RESAMPLE	1
#define NONE		2
#define	GREYCORR	3
#define MAP		4
#define VERSION		3.2
#define TP_FILENAME "cor_points"
#define TP_TEMP "cor_temp"
#define zoomFactor 	1.9

typedef struct {
	int good,bad,out;
	int total;
} nCorrStruct;

nCorrStruct getNumCorrPoints(char *f1,char *f2,char *tpl,float pixsiz,int zoomingOut);
void mv_tpl(char *from,char *to);
void tpl_mult(float byWhat);
void execute(char *cmd);
void display(char *text);
void bye(char *why);
void give_usage(void);
char *sar(int stage,char *buf);
char *sim(int stage,char *buf);

FILE   *log_fp=NULL;
char sarName[200], simName[200],buf1[200], buf2[200];

int main(int argc, char **argv)
{
	char   coef[255], logf[255];
	char   cmd[256];
	struct  DDR      ddr;
	float  pixsizSim,pixsizSar;
	float minStrength=2.0;/*Minimum correlation strength*/
	int    stage,foundEnoughPoints;
  
	if (argc != 4 && argc != 5) give_usage();
	strcpy(simName,argv[1]);
	strcpy(sarName,argv[2]);
	strcpy(coef,argv[3]);
	if (argc==5)
	{ 
		strcat(strcpy(logf,argv[4]),".log");
		log_fp=FOPEN(logf,"a");
	}

	c_getddr(simName, &ddr);
	pixsizSim = ddr.pdist_x;
	c_getddr(sarName, &ddr);
	pixsizSar = ddr.pdist_x;
  
	display("Performing Correlation of Images:");
  
  	stage=1;
  /*Find tie points.*/
	sprintf(cmd,"tpl_search d %s %s\n",sim(stage,buf1),TP_FILENAME);
	execute(cmd);
  
  	foundEnoughPoints=0;
  /*Zoom out until half of all points, or better than 25 points correlated.*/
	while(!foundEnoughPoints)
  	{
  		nCorrStruct nCorr;
  		nCorr=getNumCorrPoints(sim(stage,buf1),sar(stage,buf2),TP_TEMP,minStrength,1);
  		
  		if ( /*(nCorr.good>25)||*/
  			(((float)nCorr.good/nCorr.bad>0.5)  &&  (nCorr.good>6)))
  			foundEnoughPoints=1;
  		else if (nCorr.total==nCorr.out)
  		{/*Every point is out of bounds-- return a zero warping map*/
  			FILE *f=fopen(appendExt(coef,".ppf"),"w");
  			fprintf(f,"AZ1COEF        R 1 0.0\n"
				"AZ2COEF        R 1 0.0\n"
				"AZ3COEF        R 1 0.0\n"
				"GR1COEF        R 1 0.0\n"
				"GR2COEF        R 1 0.0\n"
				"GR3COEF        R 1 0.0\n");
			fclose(f);
  			bye("Correlation failed-- using zero coefficient map");
  		}
  		else
  		{
			printf("Stage %d: only %d points correlated, so zooming out...\n",stage,nCorr.good);
  			if (log_fp) fprintf(log_fp,"Stage %d: zooming out.\n",stage);
			pixsizSim *= zoomFactor;
			pixsizSar *= zoomFactor;
			sprintf(cmd,"resample_ddr %s %s %f > /dev/null\n",
				sar(stage,buf1),sar(stage+1,buf2),pixsizSar);
			execute(cmd);
			sprintf(cmd,"resample_ddr %s %s %f > /dev/null\n",
				sim(stage,buf1),sim(stage+1,buf2),pixsizSim);
 			execute(cmd);
			tpl_mult(1.0/zoomFactor);
			minStrength/=2.0;
			stage++;
		}
  	}
  	
  	printf("Stage %d: done zooming.\n",stage);
  	if (log_fp) fprintf(log_fp,"Stage %d: done zooming.\n",stage);
  	
 	mv_tpl(TP_TEMP,TP_FILENAME);
  	
  /*Now zoom back down until we get back to the original size*/
  	while (stage>1)
  	{
  		stage--;
		tpl_mult(zoomFactor);
		pixsizSim /= zoomFactor;
		pixsizSar /= zoomFactor;
  		getNumCorrPoints(sim(stage,buf1),sar(stage,buf2),TP_TEMP,minStrength,0);
		mv_tpl(TP_TEMP,TP_FILENAME);
  		printf("Stage %d: zooming back in.\n",stage);
  		if (log_fp) fprintf(log_fp,"Stage %d: zooming back in.\n",stage);
  	}
  	
 /*And write out the coefficients.*/
	sprintf(cmd,"2dmap %s %s\n",TP_FILENAME,coef);
	execute(cmd);
	if (log_fp) fclose(log_fp);

        system("rm cor_sim*.* cor_sar*.*\n");

	exit(0);
}

char *sar(int stage,char *buf)
{
	if (stage==1)
		strcpy(buf,sarName);
	else
		sprintf(buf,"cor_sar%d",stage);
	return buf;
}
char *sim(int stage,char *buf)
{
	if (stage==1)
		strcpy(buf,simName);
	else
		sprintf(buf,"cor_sim%d",stage);
	return buf;
}

nCorrStruct getNumCorrPoints(char *f1,char *f2,char *tpl,float minStrength,int zoomingOut)
{
	FILE *f;
	char *nCorrName="cor_npts";
	char cmd[256];
	nCorrStruct nPts;

/*If zooming out, estimate image offset using fftMatch*/
	char *matchFile="";
	if (zoomingOut)
	{/*Call fftMatch to estimate the image block offset*/
		matchFile="-o cor_match";
		sprintf(cmd,"fftMatch -m cor_match -c cor_image %s %s\n",f1,f2);
		execute(cmd);
	}
	
/*Call greycorr*/
	sprintf(cmd,"greycorr -n %s %s -s %f    %s %s %s %s\n",
		nCorrName,matchFile,minStrength,f1,f2,TP_FILENAME,tpl);
	printf("### > %s",cmd); 
	system(cmd);/*Ignore return.*/
	
/*Open output file, read correlation results into struct, and return struct*/
	f=fopen(nCorrName,"r");
	fscanf(f,"%d%d%d",&nPts.good,&nPts.bad,&nPts.out);
	fclose(f);
	unlink(nCorrName);
	printf("%d points correlated (%d didn't, %d out-of-bounds).\n",nPts.good,nPts.bad,nPts.out);
	if (log_fp) fprintf(log_fp,"%d points correlated (%d didn't, %d out-of-bounds).\n",nPts.good,nPts.bad,nPts.out);
	nPts.total=nPts.good+nPts.bad+nPts.out;
	return nPts;
}
void mv_tpl(char *from,char *to)
{
	char cmd[256];
	sprintf(cmd,"mv -f %s.tpl %s.tpl\n",from,to);
	system(cmd);
}

void tpl_mult(float byWhat)
{
	char cmd[256];
	sprintf(cmd,"tpl_mult %f %s %s\n",byWhat,TP_FILENAME,TP_TEMP);
	execute(cmd);
	mv_tpl(TP_TEMP,TP_FILENAME);
}
void execute(char *cmd)
{
	printf("### > %s",cmd); 

	if (system(cmd) != 0) 
		bye("last command failed");
	if (log_fp) 
		fprintf(log_fp,"\t\t<Command> %s\t\t\t<successful>\n",cmd); 
}

void display(char *text)
{
	printf("\n-----------------------------------------------------------\n");
	printf("%s\n",text);
	printf("-----------------------------------------------------------\n\n");
}

void bye(char *why)
{
	fprintf(stderr,"Correlate Program Aborted: %s!\n",why);
	if (log_fp) fprintf(log_fp,"Program aborted: %s!\n",why);
	exit(-1);
}

void give_usage(void)
{
	printf("\n");
	printf("Usage:  correlate sim sar coef [log]\n");
	printf("        sim    - input simulated SAR file (image1)\n");
	printf("        sar    - input SAR file (image2)\n");
	printf("        coef   - output coefficients file\n");
	printf("        log    - log file name\n");
	printf("\nASF Step Tools,  Version %.2f\n",VERSION);
	exit(-1);
}

