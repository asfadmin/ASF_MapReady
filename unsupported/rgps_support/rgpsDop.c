/*****************************************
rgpsDop:
	Determines which ScanSAR frames have good
and bad dopplers, by checking the yaw values.
	
*/
#include "asf.h"
#include <math.h>
#include "imageUtil.h"

/************** CONFIGURATION SECTION ******************/

/*Should we check duplicate frames for bad dopplers,
even when a newer (reprocessed) version exists?*/
#define checkDupsAnyway 1

/*E-Mail Recipients.*/
#define recipients "olawlor@images.alaska.edu \
	rguritz@images.alaska.edu \
	mmason@gi.alaska.edu"


/***************** CODE *************************
global arrays:
	These contain good, bad, and bad but
odd doppler images.
*/

#define MAXIMG 10000
/*Frames with correct dopplers:*/
int nGood=0;
image *good[MAXIMG];

/*Frames with dopplers 1 PRF too high:*/
int nBad=0;
image *bad[MAXIMG];

/*Frames with other bad dopplers:*/
int nBadOdd=0;
image *badOdd[MAXIMG];

/********************************************************
CorrectDop:
	Call this when you've determined that a doppler
needs changing.  Creates the PRM directory and calls writeDopplerDelta
to create custom processing parameter file.
*/
void writeDopplerDelta(char *ceosFile, double dopplerOffsetPRF);
void correctDop(image *img,double offsetPRF)
{
	img->dopChange=offsetPRF;
	system("mkdir prm > /dev/null 2>&1\n");
	writeDopplerDelta(img->name,offsetPRF);	
}

/*******************************************************
SortImage:
	Places given image into the arrays of good,
bad, and unknown dopplers.
*/
void sortImage_viaYaw(image *img,FILE *imgInfo)
{
/*Estimage doppler based on yaw angle.*/
	#define MIN_YAW -0.02
	#define MAX_YAW 0.14
	if ((img->yaw>=MIN_YAW) && (img->yaw<=MAX_YAW))
		good[nGood++]=img;
	else if (img->yaw>MAX_YAW)
	{
		bad[nBad++]=img;
		correctDop(img,-1.0);
	}
	else /*if (img->yaw<MIN_YAW)*/
	{
		badOdd[nBadOdd++]=img;
		correctDop(img,+1.0);
	}

/*Also output debugging file.*/
	if (imgInfo!=NULL)
	fprintf(imgInfo,"%5d  %3d  %3d %6.4f %8.2f\n",
		img->rev, /*Revolution*/
		img->frame,/*Frame*/
		img->vers,/*Image version #.*/
		img->yaw, /*Yaw in degrees.*/
		img->ceosDoppler); /*Doppler in Hz.*/
}
/*****************************************************
showFrames:
	Prettily outputs list of good and bad frames.
*/
void showFrames(void)
{
	int i;
	printf("The following frames have good dopplers:\n");
	printf("\t<yaw (deg)>\t<name>\n");
	for (i=0;i<nGood;i++)
		printf("\t%.3f\t%s\n",good[i]->yaw,good[i]->name);
	printf("\n\n");
	
	
	if (nBad!=0)
	{
		printf("The following frames have bad dopplers--\n"
			"They should be re-processed at -1 PRF:\n");
		printf("\t<yaw (deg)>\t<name>\n");
		for (i=0;i<nBad;i++)
			printf("\t%.3f\t%s\n",bad[i]->yaw,bad[i]->name);
	} else printf("No frames have bad dopplers.\n");
	printf("\n\n");
	
	if (nBadOdd!=0)
	{
		printf("The following frames have \"odd\" bad dopplers--\n"
			"\t<Reprocess At>\t<yaw (deg)>\t<File Name>\n");
		for (i=0;i<nBadOdd;i++)
			printf("\t%.2f\t%.3f\t%s\n",badOdd[i]->dopChange,badOdd[i]->yaw,badOdd[i]->name);
		printf("\n\n");
	}
	
}
/*********************************************************
main:
	Read each given frame, checking for duplicates.
	Decide whether frames are good or bad.
	Output results.
*/
int main(int argc,char *argv[])
{
	int nImages=0;
	image *images[MAXIMG];
	char response[255];
	int i;
	FILE *imgInfo,*mailFile;
	if (argc<2)
		{printf("Usage: rgpsDop <files...>\n"
		"Determines which frames have bad dopplers,\n"
		"using the yaw.\n"
		"ASF RGPS Tools, 1998.  Version 1.0\n");exit(1);}
	
/*Open output e-mail file.*/
#define mail_message "mail_message"
#define mail_info "mail_info"
	mailFile=fopen(mail_message,"w");
	if (NULL!=mailFile)
	{
		fprintf(mailFile,"Subject: RGPS Doppler determination\n"
		"This is an automated e-mail, listing bad frames\n"
		"which need to be re-processed.\n"
		"\n"
		"The following frames need to be re-processed:\n"
		"<path> <first beam> <doppler offset, in PRF>\n");
			fclose(mailFile);
	}
	
/*Open debugging output file.*/
#define imgInfoName "image_info"
	imgInfo=fopen(imgInfoName,"w");

/*Read in image files, checking for duplicate frames.*/
	for (i=1;i<argc;i++)
	{
		image *img=readImage(argv[i]);
		#if !checkDupsAnyway
		if (!dupImage(img,images,nImages))
		#endif
			images[nImages++]=img;
	}
	
/*Sort the non-duplicate frames into classes based on yaw.*/
	for (i=0;i<nImages;i++)
		sortImage_viaYaw(images[i],imgInfo);

/*List the good and bad frames.*/
	showFrames();
	
/*Display the yaw and doppler information graphically (for debugging).*/
	if (imgInfo && (nImages>1))
	{
		fclose(imgInfo);
		system("/bin/echo \"  \" >> "mail_message"\n");
		system("/bin/echo XXXXXXXXXXXX Yaw plot: XXXXXXXXX >> "mail_message"\n");
		system("/bin/cat "imgInfoName" | plotcol 2 4 >> "mail_message"\n");
		system("/bin/rm -f "mail_info" 2>&1\n");
		system("/bin/echo XXXXXXXXXXXX Image info: XXXXXXXXX > "mail_info"\n");
		system("/bin/echo rev frame vers yaw doppler >> "mail_info"\n");
		system("/bin/cat "imgInfoName" >> "mail_info"\n");
	}

/*Write out e-mail message listing bad frames, if there were any.*/
	if (imgInfo && (nBad>0 || nBadOdd>0))
	{
	/*Ask the user if they would like to send the e-mail:*/
		printf("Would you like to send an e-mail listing\n"
			"these bad frames off to Nettie LaBelle-Hammer? (y/n)\n");
		
		fgets(response,255,stdin);
		
		if (toupper(response[0])=='Y')
		{
			printf("Sending e-mail:\n");
			system("/bin/cat "mail_message"\n");
			system("/bin/cat "mail_message" "mail_info" | mail '"recipients"'\n");
			printf("/bin/cat "mail_message" "mail_info" | mail '"recipients"'\n");
			printf("e-mail sent.\n");
		} else 
			printf("Will NOT send e-mail.\n");

		system("grep '/R' mail_message > bad_files\n");
		system("reorder.ksh bad_files\n");

	}
	
	return 0;
}
