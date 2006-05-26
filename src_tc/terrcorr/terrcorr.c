/*************************************************************************
NAME:   terrcorr --  Terrain Correction Main Program

SYNOPSIS:  terrcorr [-cf{1-4}klms] SAR DEM
        -c              Clip the DEM file to the extents of the SAR image
        -m              Modify radiometric calibration sigma0 range,
                        coefficients, and noise versus range LUT.
        -s              Skip the sar preprocessing step
        -k              Create a MASK value image
        -f{1-4}         Create a radiometrically terrain corrected product
                        using the numbered formula (see SARSIM)
        -l              leave temporary files tiep.tpl, coef.ppf,
                        pdem.img, and sSAR.img
        SAR             Input SAR File -- ASF Type 100 or 200
        DEM             Input Digital Elevation Model file

DESCRIPTION:
       This shell incorporates all of the steps involved in performing terrain
    correction of low or full-resolution ASF SAR images at varying resolutions.
    This process includes DEM preprocesssing, SAR preprocessing, geocoding,
    geometric rectification, and radiometric normalization.

EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    calibrate            Applies radiometric calibration to SAR image
    resample             Resamples a SAR image to the desired resolution
    demclip              Clips a DEM to a desired geographic location
    sarsim               Creates a simulated SAR image
    despike              Runs 2 despiking passes on an image
    correlate		 performs image to image correlation
    sargeom              Creates terrain corrected SAR image
    rtc_add  		 Applies radiometric terrain correction factors

FILE REFERENCES:
    NAME:             USAGE:
    ---------------------------------------------------------------
I   SAR.dat (or .D)   SAR data file
N   SAR.ldr (or .L)   SAR leader file
P   SAR.trl (or .L)   SAR trailer file
U   DEM.img           SUN LAS format DEM image
T   DEM.meta           SUN LAS format data descriptor record for DEM
S

T
M   sSAR.img          Simulated SAR image
P   rSAR<form>.img    Radiometric Terrain Corection factors

    pSAR.img          Preprocessed SAR
O   pSAR.meta          Preprocessed SAR
U   SAR_mask.img          Mask Value Image
T   SAR_mask.meta
P   SAR_cor.img       Terrain Corrected Image
U   SAR_cor.meta
T   SAR_cor<form>.img Radiometrically Terrain Corrected Image
S   SAR_cor<form>.meta
    fsSAR.img         Filtered Simulated SAR image
    fsSAR.meta         Filtered Simulated SAR image

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:    PURPOSE:
    ---------------------------------------------------------------
    1.0     8/29/94  Tom Logan  Initial Implementation, converted
                                from original Unix C shell script
    1.1     2/95     T. Logan   Add scalable pixel size option
                                Add las 6.0 (img DEM) option
    1.2     6/95     T. Logan   Added modify calibration option
                                Modified to use new correlation process
    1.3     8/95     T. Logan   Modified to pass Mask and RTC factor
                                switches to SARSIM.  Added call to rtc_mult.
                                Renamed output files.
    2.0    10/95     T. Logan   Ported to Solaris
    2.1	    1/96     T. Logan   Added creation of zero coefficients file.
    2.2     5/96     T. Logan   Modified to use new metadata routines
    2.3     9/96     T. Logan   Modified to handle RADARSAT era data files
    2.4     8/97     T. Logan   Incorporated two-pass correlation

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:
  Preprocess the input SAR image
    -- Apply radiometric calibrations
    -- Resample the image to desired resolution, OR
    -- Strip the SAR image CEOS wrapper
  Preprocess the input DEM image
    -- If necessary, clip the DEM to a desired area of coverage
  Create Simulated Image from DEM and original
    -- Using SAR metadata and DEM height values create sim image
    -- if (MSK) Create Mask Valued image
    -- if (RTC) Create RTC multipliers image
  Perform Correlation Process, Produce mapping Coefficients
    -- Smooth images by despiking filter
    -- Search the SAR image for areas of high contrast to use as tie points
    -- Correlate images to produce tie-points
    -- Edit tie-points and make 2D mapping function coefficients
  Create Terrain Corrected Image & Outputs
    -- Using the coefficients found, locate SAR image pixel values in the
       positions given by DEM file
    -- if (RTC) perform rtc_mult to create RTC SAR image
  Clean Up Processing Files
    -- Remove intermediate processing files

ALGORITHM REFERENCES:   Based on EDC code by Charles Wivell

BUGS:
*************************************************************************/
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
#include "asf_reporting.h"
#include <unistd.h>
#include "ceos.h"
#include "ddr.h"
#define  VERSION 2.3

void meta_or_ddr(char *whichOne, char *fileName);
void display(char *text);
void execute(char *cmd);
void bye(void);
void make_zero(void);

int main(argc, argv)
        int argc; char **argv;
{
char cmd[255], SAR[255],  DEM[255], pSAR[255], mSAR[255], rSAR[255],
     SIM[255], fSIM[255], COR[255], rCOR[255];
char *metaExt=".meta", *imgExt=".img";
char *ddrExt=".ddr";
char *tmp;
char SIM_rtc[259], pSAR_mask[260];

/*char    ascdesc;                 Ascending/Descending pass flag            */
float   pixsiz;                 /* Processing pixel size in meters           */
float   pixsizDEM, pixsizSAR;   /* Pixel size for input SAR and DEM images   */
int     leave,                  /* Flag -- Leave temporary processing files? */
        clip,                   /* Flag -- Clip the DEM to match SAR image?  */
        mask,                   /* Flag -- Create a mask image?              */
        rtcf,                   /* Flag -- Apply radiometric terrain corr.   */
        form,                   /* Formula type for rtc image                */
        era,			/* Era of data files -- 0 PreRadarsat 1 Post */
        modify,                 /* Flag -- Modify calibration parameters?    */
	skip;			/* Flag -- Skip SAR preprocessing?	     */
int     error;                  /* Return status -- error checking           */
int     i, j;                   /* A humble little loop counter              */
meta_parameters *metaDEM=NULL, *metaSAR=NULL;	/* Metadata		     */
/*****************************************************************************
                  S E T    U P    P R O C E S S I N G
******************************************************************************/
StartWatch();

asfPrintStatus("\n\n");
asfPrintStatus("==================================================================\n");
asfPrintStatus("        Terrain Correction Software for ASF SAR images\n");
asfPrintStatus("==================================================================\n");
asfPrintStatus("\n");

/* Get Inputs from Command Line
 ------------------------------*/
leave = 0;              /* Leave temporary processing files?    */
modify = 0;             /* Modify calibration coefficients?     */
clip = 0;               /* Clip the DEM file to match the SAR?  */
error = 0;
mask = 0;		/* Create mask image file?		*/
rtcf = 0;		/* Create Radiometric TC file?          */
form = 0;		/* Number of formula to use for RTC     */
skip = 0;		/* Skip the SAR preprocessin step?	*/

if (argc >= 3)
  for (i = 1; i < argc; i++)
   {
     if (argv[i][0] == '-')
       {
         for (j = 1; argv[i][j] != 0; j++)
              switch (argv[i][j])
               {
                 case 'c': clip = 1; break;
                 case 'l': leave = 1; break;
                 case 'm': modify = 1; break;
		 case 's': skip = 1; break;
                 case 'k': mask = 1; break;
                 case 'f':
                           j++;
 			   form = atoi(&argv[i][j]);
                           rtcf = 1;
                           break;
                 default:
                     asfPrintStatus("\n\7\7\n INVALID OPTION: %s\n\7\n",argv[i]);
                     error = 1;
                     break;
               }
       }
     else
       {
        strcpy(SAR,argv[i++]);
        strcpy(DEM,argv[i++]);
       }
    }
else error = 1;

if (error)
  {
   asfPrintStatus(" Usage: %s [-cf{1-4}klms] SAR DEM\n\n",argv[0]);
   asfPrintStatus(" The inputs to this shell are as follows:\n\n");
   asfPrintStatus("   SAR         input SAR file\n");
   asfPrintStatus("   DEM         input DEM file\n");
   asfPrintStatus("\n");
   asfPrintStatus(" The options accepted are:\n\n");
   asfPrintStatus("   -c          Clip the DEM file to the area of the SAR image\n");
   asfPrintStatus("               (thus reducing processing time).\n");
   asfPrintStatus("   -f{1-4}     Apply radiometric terrain corrections using the\n");
   asfPrintStatus("               formula requested (1 LI  2 GO  3 SQ  4 VX).\n");
   asfPrintStatus("   -k          Create a mask value image.\n");
   asfPrintStatus("   -l          Leave temporary processing files intact.\n");
   asfPrintStatus("   -m          Modify radiometric calibration sigma0 range, \n");
   asfPrintStatus("               coefficients, and noise vs. range LUT\n");
   asfPrintStatus("   -s          Skip the SAR preprocessing steps\n");
   asfPrintStatus("\n");
   asfPrintStatus(" Version %.2f,  ASF SAR Tools\n\n",VERSION);
   exit(EXIT_FAILURE);
  }

strcat(strcpy(SIM,"s"),SAR);
strcat(strcpy(fSIM,"f"),SIM);
strcat(strcpy(pSAR,"p"),SAR);
strcat(strcpy(COR,SAR),"_cor");
if (mask) strcat(strcpy(mSAR,"m"),SAR);
if (rtcf)
 {
  strcat(strcpy(rSAR,"r"),SAR);
  switch (form)
   {
     case 1: strcat(strcpy(rCOR,COR),"li"); strcat(rSAR,"li"); break;
     case 2: strcat(strcpy(rCOR,COR),"go"); strcat(rSAR,"go"); break;
     case 3: strcat(strcpy(rCOR,COR),"sq"); strcat(rSAR,"sq"); break;
     case 4: strcat(strcpy(rCOR,COR),"vx"); strcat(rSAR,"vx"); break;
   }
 }


/* Read DEM and SAR image metadata
 --------------------------------*/
// NOTE: I'm a little queasy about this. Used to be assigned to ddr.pdist_x
//       which is directly related to meta->projection->perX, but there is not
//       always a projection block in the metadata. With the assurance,
//       metaDEM->general->x_pixel_size should suffice (it is directly related
//       to ddr.sample_inc)
metaDEM = meta_read(DEM);
if (metaDEM->projection != NULL)
 {
  pixsizDEM = metaDEM->projection->perX;
 }
else
 {
  pixsizDEM = metaDEM->general->x_pixel_size;
 }
pixsiz = pixsizDEM;

metaSAR = meta_read(SAR);
if (metaSAR->projection != NULL)
 {
  pixsizSAR = metaSAR->projection->perX;
 }
else
 {
  pixsizSAR = metaSAR->general->x_pixel_size;
 }

asfPrintStatus("Preprocessing the SAR image\n");
/*------------------------------------------------------------------------
                   Preprocess the input SAR image
 ------------------------------------------------------------------------*/
  if (!skip)
   {
    /* Resample Image / Strip CEOS Header
     -------------------------------------*/
    if (pixsizSAR < pixsiz)
      {
	display("Resampling and Low-Pass Filtering SAR Image");
        sprintf(cmd,"resample %s %s %f\n",SAR,pSAR,pixsiz);
        execute(cmd);

        asfPrintStatus("\n");
      }
    else if (pixsizSAR == pixsiz)
      {
	sprintf(cmd,"cp %s%s %s%s\n",SAR,imgExt,pSAR,imgExt); execute(cmd);
	if ( fileExists( tmp=appendExt(SAR,metaExt) ) )
	 {
	  sprintf(cmd,"cp %s%s %s%s\n",SAR,metaExt,pSAR,metaExt); execute(cmd);
	 }
	FREE(tmp);
	if ( fileExists( tmp=appendExt(SAR,ddrExt) ) )
	 {
	  sprintf(cmd,"cp %s%s %s%s\n",SAR,ddrExt,pSAR,ddrExt); execute(cmd);
	 }
	FREE(tmp);
      }
    else
      {
        asfPrintError("SAR image has a lower resolution than the DEM\n");
      }

    /* Copy CEOS metadata for resampled image
     -----------------------------------*/
    asfPrintStatus("\n");
    if (!era)
     {
       sprintf(cmd,"cp %s.L %s.L\n",SAR,pSAR);
       execute(cmd);
       sprintf(cmd,"cp %s.D %s.D\n",SAR,pSAR);
       execute(cmd);
     }

   }  /* End if !skip */
  else strcpy(pSAR,SAR);

/*--------------------------------------------------------------------------
                         Preprocess the input DEM image
 --------------------------------------------------------------------------*/
 if (clip)
  {
   display("Clipping input DEM file to area of SAR image");
   sprintf(cmd,"demclip %s %s pDEM\n",DEM,SAR);
   execute(cmd);
   strcpy(DEM,"pDEM");
  }

/*--------------------------------------------------------------------------
                 Create Simulated Image from DEM and original
 --------------------------------------------------------------------------*/
  asfPrintStatus("\n");
  display("Creating Simulated SAR image");
  if (mask && rtcf) sprintf(cmd,"sarsim %s %s %s -kr%i\n",pSAR,DEM,SIM,form);
  else if (mask)    sprintf(cmd,"sarsim %s %s %s -k\n",pSAR,DEM,SIM);
  else if (rtcf)    sprintf(cmd,"sarsim %s %s %s -r%i\n",pSAR,DEM,SIM,form);
  else              sprintf(cmd,"sarsim %s %s %s\n",pSAR,DEM,SIM);
  execute(cmd);

  if (rtcf)
    {
     strcat(strcpy(SIM_rtc,SIM),"_rtc");
     sprintf(cmd,"mv %s%s %s%s\n",SIM_rtc,imgExt,rSAR,imgExt); execute(cmd);
     if ( fileExists( tmp=appendExt(SIM_rtc,metaExt) ) )
      {
       sprintf(cmd,"mv %s%s %s%s\n",SIM_rtc,metaExt,rSAR,metaExt); execute(cmd);
      }
     FREE(tmp);
     if ( fileExists( tmp=appendExt(SIM_rtc,ddrExt) ) )
      {
       sprintf(cmd,"mv %s%s %s%s\n",SIM_rtc,ddrExt,rSAR,ddrExt); execute(cmd);
      }
     FREE(tmp);
    }
  if (mask)  /* Fix Mask Image File Name */
    {
     strcat(strcpy(pSAR_mask,pSAR),"_mask");
     sprintf(cmd,"mv %s%s %s%s\n",pSAR_mask,imgExt,SAR,imgExt); execute(cmd);
     if ( fileExists( tmp=appendExt(pSAR_mask,metaExt) ) )
      {
       sprintf(cmd,"mv %s%s %s%s\n",pSAR_mask,metaExt,SAR,metaExt); execute(cmd);
      }
     FREE(tmp);
     if ( fileExists( tmp=appendExt(pSAR_mask,ddrExt) ) )
      {
       sprintf(cmd,"mv %s%s %s%s\n",pSAR_mask,ddrExt,SAR,ddrExt); execute(cmd);
      }
     FREE(tmp);
    }


/*-----------------------------------------------------------------------
        Despike Filter SIM and SAR images for correlation process
 -----------------------------------------------------------------------*/
  display("Despike Filtering SIM Image");
  sprintf(cmd,"despike %s %s\n",SIM,fSIM);  execute(cmd);

/*-----------------------------------------------------------------------
        Perform Correlation Process, Produce mapping Coefficients
 -----------------------------------------------------------------------*/
  display("Performing Correlation of Images");
  sprintf(cmd,"correlate %s %s coef %s\n",fSIM,pSAR,SAR);
  execute(cmd);

/*--------------------------------------------------------------------------
                        Create Terrain Corrected Image
 --------------------------------------------------------------------------*/
  display("Creating Terrain Corrected Image");
  sprintf(cmd,"sargeom %s %s coef %s\n",DEM,pSAR,COR); execute(cmd);

/*--------------------------------------------------------------------------
               Create Radiometrically Terrain Corrected Image
 --------------------------------------------------------------------------*/
  if (rtcf)
   {
     display("Creating Radiometrically Terrain Corrected Image");
     sprintf(cmd,"rtc_add %s %s %s\n",COR,rSAR,rCOR); execute(cmd);
     sprintf(cmd,"cp %s%s %s%s\n",COR,metaExt,rCOR,metaExt); execute(cmd);
   }

/*--------------------------------------------------------------------------
                         Clean Up Processing Files
 --------------------------------------------------------------------------*/
  display("Cleaning Processing Files");

 if (!leave)
   {
    /* sprintf(cmd,"rm -f *.tpl\n"); execute(cmd); */
    sprintf(cmd,"rm -f %s%s %s%s\n",SIM,imgExt,SIM,metaExt); execute(cmd);
    sprintf(cmd,"rm -f cor_sar?.* cor_sim?.*\n"); execute(cmd);
    sprintf(cmd,"rm -f %s%s\n",SIM,ddrExt); execute(cmd);

    if (clip)
     {
      sprintf(cmd,"rm -f %s%s\n",DEM,imgExt); execute(cmd);
      sprintf(cmd,"rm -f %s%s\n",DEM,metaExt); execute(cmd);
      sprintf(cmd,"rm -f %s%s\n",DEM,ddrExt); execute(cmd);
     }
   }

  if (rtcf)
   {
     sprintf(cmd,"rm -f %s%s\n",rSAR,imgExt); execute(cmd);
     sprintf(cmd,"rm -f %s%s\n",rSAR,metaExt); execute(cmd);
     sprintf(cmd,"rm -f %s%s\n",rSAR,ddrExt); execute(cmd);
   }

  if (!skip)
   {
     sprintf(cmd,"rm -f %s%s\n",pSAR,metaExt); execute(cmd);
     sprintf(cmd,"rm -f %s%s\n",pSAR,ddrExt); execute(cmd);
   }



StopWatch();

asfPrintStatus("\n\n");
asfPrintStatus("==================================================================\n");
asfPrintStatus("             End of Terrain Correction Process\n");
asfPrintStatus("==================================================================\n");
asfPrintStatus("\n\n");

return(0);
}



void display(char *text)
{
  asfPrintStatus("\n-----------------------------------------------------------\n");
  asfPrintStatus("%s\n",text);
  asfPrintStatus("-----------------------------------------------------------\n\n");
}

void execute(char *cmd)
{
	asfPrintStatus("%s",cmd);
	fflush(stdin);
	if (system(cmd)!=0)
		bye();
}

void bye(void) { asfPrintStatus("Program Aborted.\n"); exit(EXIT_FAILURE); }

void make_zero(void)
{
  FILE *fp;

  fp = fopen("coef.ppf","w");
  fprintf(fp,"AZ1COEF        R 1 0.0\n");
  fprintf(fp,"AZ2COEF        R 1 0.0\n");
  fprintf(fp,"AZ3COEF        R 1 0.0\n");
  fprintf(fp,"GR1COEF        R 1 0.0\n");
  fprintf(fp,"GR2COEF        R 1 0.0\n");
  fprintf(fp,"GR3COEF        R 1 0.0\n");
  fclose(fp);
}


