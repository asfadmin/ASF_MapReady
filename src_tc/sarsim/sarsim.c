/*****************************************************************************
NAME:		SARSIM  -- Creates a simulated SAR image
 
SYNOPSIS:	sarsim inSARfile inDEMfile outSIMfile [-kpr]

	-k		Turns on creation of the mask image 
        -p		Turns on creation of the phase image 
	-r[1-4]		Turns on creation of radiometric t.c. factors
			Numbers select RTC formula to use:
				1 LI 	2 GO 	3 SQ 	4 VX
 
DESCRIPTION:
    This program uses a digital elevation model and the metadata from
  a SAR image to create a simulated SAR image.  Processing is driven 
  by the input DEM file, which is used one line at a time from top to
  bottom.  For non-zero height DEM pixels, the lon,lat coordinates are
  calculated.  These are passed to the SARMODEL, which calculates the
  backscatter based on the DEM height at the point of interest.  This
  value and the SAR image space coordinates are returned.  If the returned
  image coordinates are in the area of interest, the output pixel is
  given the value calculated by SARMODEL. 
    The sarmodel works in two modes - SLANT RANGE and GROUND RANGE.
  When the input SAR image is a complex data set, slant range processing
  is assumed.  In this case, a slant range simulated SAR is created and
  optionally a simulated phase image.  When the input is an amplitude
  image, ground range processing is assumed.  Optional products for the
  ground range mode include a radar shadow/layover mask image and a 
  radiometric terrain correction factor image.
 
EXTERNAL ASSOCIATES:
    ---------------------------------------------------------------
       init_sar_model(azcoef, grcoef, pixsiz, sarfile, form)
          initializes the SAR model using location information gained 
	  from the inSARfile header and trailer files (.L or .ldr,.trl)
 
       sar_sim(lat[PT],lon[PT],h[PT],lat[NORTH],lon[NORTH],h[NORTH],
               lat[WEST],lon[WEST],h[WEST],lat[SOUTH],lon[SOUTH],h[SOUTH],
               lat[EAST],lon[EAST],h[EAST], VIEW, &dn,
               &sini, &mask, &in_line, &in_samp);
	  given the lon,lat, and height of a point and its neighbors,
	  calculates dn (decibel noise) 'seen' by the SAR platform
	  calculates sini (sin of incidence angle) from the platform
	  calculates mask (is point visible?) from the platform
	  calculates output line,sample of the input point.
 
FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    inSARfile.L(ldr/trl) SAR metadata records
    inDEMfile.meta       DEM metadata records
    inDEMfile.img        Input DEM data file 
    outSIMfile.img       Output simulated SAR image
 
PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:       PROJECT:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    7/91    D. Steinwand  EROS/AB-ISS
    2.0     6/94    T. Logan	  SARREG/ASF	Ported to YMP
    3.0     7/94    T. Logan	  SARREG/ASF	Ported to T3D 
    3.1	    9/94    T. Logan      SARREG/ASF    Optimized for I/O
    3.2	    4/95    T. Logan	  SARREG/ASF	Fixed call to init_sar_model
    3.3     7/95    T. Logan	  SARREG/ASF	Add mask image generation
    3.4	    8/95    T. Logan      SARREG/ASF    Added radiometric terrain 
						correction factor generation
    4.0	   10/95    T. Logan	  SARREG/ASF    Ported from T3D to Solaris
    5.0    10/96    T. Logan	  IFSAR/STEP	Added slant range processing
						& creation of phase images.
    5.1    6/98     O. Lawlor     ASF/STEP      Removed non-ANSI Timer.
    
HARDWARE/SOFTWARE LIMITATIONS:
      The DEM file that is given to the SARSIM routine must be 
    already clipped to the proper area of coverage and pixel spacing.
    This is accomplished using a large area DEM that is in the UTM
    projection as input to the program demclip, or it can be accomplished
    using the LAS modules REMAP and GEOM as they were set up in the original
    EDC terrain correction code.
 
ALGORITHM DESCRIPTION:
    initialize sar model and determine DEM, SAR, and SIM file sizes
      for each line in DEM 
        for each sample in DEM
          if height of PT != 0
   	    sar_sim(PT...)
 	    if sar_sim returns a point in the output image
	       store dn value at point that sar_sim specified 
    Write final SIM image to output file
 
ALGORITHM REFERENCES:
 
BUGS:
*****************************************************************************/
/***************** Copyright Notice ***********************
                        English:
         You can freely use, modify, and re-distribute 
this code and/or binaries as long as you don't sell them,
and make sure to carry this notice along with them.
However, if you want to sell them, you can contact the 
University of Alaska Technology Corporation, below.


                        Legalese:
                 COPYRIGHT NOTIFICATION

(C) COPYRIGHT 1997 UNIVERSITY OF ALASKA. ALL RIGHTS RESERVED

This software discloses material protectable under copyright 
laws of the United States. Permission is hereby granted to 
use, reproduce, and prepare derivative works for noncommercial 
purposes at no charge, provided that this original copyright 
notice, and disclaimer are retained and any changes are 
clearly documented. Any entity desiring permission to 
incorporate this software or a work based on the software 
into a product for sale must contact the University of 
Alaska Technology Corporation.


This software was authored by:

Alaska SAR Facility, Geophysical Institute
P.O. Box 757320, University of Alaska Fairbanks
Fairbanks, Alaska 99775Ð7320
FAX: (907)474-5195

Any questions or comments on the software may be directed 
to one of the authors: Rick Guritz, Tom Logan, Mike Shindle,
Rob Fatland, Orion Lawlor, and Dorothy Corbett; or to
http://www.images.alaska.edu

NEITHER THE UNIVERSITY OF ALASKA NOR ANY SUBUNIT THEREOF, 
NOR ANY OF THEIR EMPLOYEES MAKES ANY WARRANTY, EXPRESS 
OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR 
RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR 
USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD 
NOT INFRINGE PRIVATELY OWNED RIGHTS.
LICENSING INQUIRES MAY BE DIRECTED TO THE UNIVERSITY 
OF ALASKA TECHNOLOGY DEVELOPMENT CORPORATION AT (907)451-0718.
************************************************************/
#include "asf.h"
#include <ctype.h>
#include <sys/types.h>
#include "sarsim.h"
#include "sarmodel.h"
#include "ddr.h"
#include "ceos.h"
#include "worgen.h"
 
#define SIMBUF(A,B)	*(simbuf + ((A-1) * ns) + (B-1))
#define PHZBUF(A,B)	*(phsbuf + ((A-1) * ns) + (B-1))
#define MSK		if (msk_flg)
#define RTC		if (rtc_flg)
#define PHS		if (phs_flg)
#define VERSION 	5.1
 
void bye(char *message);
int init_support(double  *line_off,double *samp_off,int *zone_code,char *file);

/*******************************************************/
int     in_block_sl;  /* simulation start line         */
int     in_block_ss;  /* simulation start sample       */
int     in_block_nl;  /* simulation number of lines    */
int     in_block_ns;  /* simulation number of samples  */
int     dem_ns;	      /* number of samples in dem line */
int     dem_nl;	      /* number of lines in dem file   */
double  upleft[2];    /* Upper left coordinates of DEM */
double  pixsiz=90.0;  /* Pixel size of DEM in meters   */
/*******************************************************/
 
int main(argc, argv) int argc; char** argv;
{
/**********************************************************************/
int 	nl,ns;                  /* Num lines & samples in SAR image   */
int 	sim_line, sim_samp;     /* Calculated SIM image coordinates   */
int 	temp_val;               /* temp storage for real to int conv  */
int 	i,j,line,sample;	/* Loop counters                      */
int  	zone;                   /* DEM UTM zone code                  */
double  proj_x, proj_y;	        /* X & Y coords input to UTM X-form   */
short   *dembuf;      		/* DEM data buffer                    */
short   *prev_dem_line;         /* pointer to previous DEM line       */
short   *curr_dem_line;         /* pointer to current DEM line        */
short   *next_dem_line;         /* pointer to next DEM line           */
short   *temp;			/* temp pointer for pointer swapping  */
double	lat[ELEVDIM],		/* Latitude for PT,N,E,S,W	      */
      	lon[ELEVDIM],		/* Longitude for PT,N,E,S,W	      */
       	h[ELEVDIM],             /* Height for PT,N,E,S,W              */
       	in_line,   in_samp,	/* Calculated Sim image coordinates   */
       	azcoef[4], grcoef[4],   /* Image Mapping coefficients	      */
       	line_off,  sample_off,  /* Offsets for windowed outputs	      */
        bperp=0,                /* Baseline Seperation                */
       	dn,sini,mask;           /* Returns from sarmodel code:        */
                                /*     dn   - decibel noise  	      */
                                /*     sini - sine of incidence angle */
                                /*     mask - shadowing/layover mask  */
unsigned char *simbuf;		/* Output buffer -- Simulated Image   */
unsigned char *maskbuf=NULL;	/* Output buffer -- Mask Values       */
float   *rtcbuf=NULL;		/* Output buffer -- RTC  Values       */
float   *phsbuf=NULL;		/* Output buffer -- Phase Values      */
char    sarfile[256],           /* Input SAR file name                */
	tempfile[256],          /* Temporary file name buffer         */
        tmp2file[256],          /* Temporary file name buffer #2      */
        demfile[256],           /* DEM file name                      */
        cmd[256];		/* String buffer for system commands  */
FILE    *dem_fp;      		/* DEM file pointer                   */
FILE 	*msk_fp=NULL;	        /* Output file stream pointer - mask  */
FILE 	*rtc_fp=NULL;	        /* Output file stream pointer - rtc   */
FILE 	*phs_fp;	        /* Output file stream pointer - rtc   */
FILE    *sim_fp; 		/* Output file stream pointer - sim   */
int	all_out=1,		/* Test to see if all points outside  */
	zeros=0,		/* How many zero points?	      */
        good=0,			/* How many non-zero points?          */
	msk_flg=0,		/* Flag for creation of mask image    */
	rtc_flg=0,		/* Flag for creation of RTC image     */	
	phs_flg=0,		/* Flag for creation of phase image   */
	error= 0,		/* Command line parameters check      */
	form = 0,		/* Formula used from RTC	      */
        GROUND_RANGE,           /* Flag for ground or slant range     */
        VIEW = 1;		/* View flag code for sarmodel code   */
				/* 0 - sargeom, 1 - sarsim,	      */
				/* 2 - sim w/ mask, 3 - sim w/ RTC    */
				/* 4 - sim w/ mask and RTC	      */
struct  DDR ddr;                /* Data descriptor - image metadata   */
struct  VFDRECV ofdr;           /* Define value of fac. data record   */
/**********************************************************************/

StartWatch();

/* Esablish processing parameters
  -------------------------------*/
if (argc > 3 && argc < 8)
  {
    strcpy(sarfile,argv[1]);
    strcpy(demfile,argv[2]);
    for (i = 4; i < argc; i++)
       for (j = 1; argv[i][j] != 0; j++)
        {
         switch (argv[i][j])
          {
	    case 'k': msk_flg = 1; break;
            case 'p': phs_flg = 1; j++; bperp= atof(&argv[i][j]); break;
	    case 'r': rtc_flg = 1; j++; form = atoi(&argv[i][j]); break;
            default:
              printf("\n\7\7\n INVALID OPTION: %s\n\7\n",argv[i]);
              error = 1;
              break;
          }
         PHS break;
        } 
  }
else error = 1;
if (error==1)
  {
   printf("Usage:%s inSAR inDEM outSIM [-kr{1-4}]\n",argv[0]);
   printf("  or :%s inSAR inDEM outSIM [-pBperp]\n",argv[0]);
   printf("      inputs:  inSAR.img \n");
   printf("               inDEM.img \n");
   printf("      output:  outSIM.img \n");
   printf("    switches:  k      Create a mask image\n");
   printf("               p      Create a phase image with baseline Bperp\n");
   printf("               ");
   printf("r[0-4] Create a radiometric terrain correction image\n");
   printf("               Number sets formula: 1 LI, 2 GO, 3 SQ, 4 VX\n");
   printf("\n      Version %.2f	ASF STEP TOOLS\n\n",VERSION);
   exit(1);
  }

if (msk_flg && rtc_flg) VIEW = 4;
else RTC VIEW = 3;
else MSK VIEW = 2;

PHS {
 if (VIEW > 1)
   bye("Cannot perform ground and slant range functions simultaneously\n");
 else form = -1;
}

if (c_getddr(sarfile, &ddr)!= 0)
  { printf("Unable to get ddr file for image %s\n",sarfile); exit(1); }
nl = ddr.nl;
ns = ddr.ns;

if (c_getddr(demfile, &ddr)!= 0)
  { printf("Unable to get ddr file for image %s\n",demfile); exit(1); }
pixsiz = ddr.pdist_x;

printf("************STARTING SARSIM PROCESS************\n");
printf("Processing resolution is %5.2f meters\n",pixsiz);
if (VIEW==4) printf("Creating Sim, Mask, and RTC factor images\n");
if (VIEW==3) printf("Creating Sim and RTC factor images\n");
if (VIEW==2) printf("Creating Sim and Mask images\n");
if (VIEW==1)
  {
    if (form==-1) printf("Creating Sim and Phase images\n");
    else  printf("Creating Sim image only\n");
  }

/* Initialize SAR Model and Get DEM and Windowing Information
 ---------------------------------------------------------- */
azcoef[1] = azcoef[2] = azcoef[3] = 0.0;
grcoef[1] = grcoef[2] = grcoef[3] = 0.0;

if (form==-1) init_sar_model(azcoef, grcoef, pixsiz, sarfile, form, bperp);
else          init_sar_model(azcoef, grcoef, pixsiz, sarfile, form);

printf("\nInitialization of SAR model complete...\n");
init_support(&line_off,&sample_off,&zone,demfile);
printf("  SAR file  %s\t  nl,ns = %i\t%i\n",sarfile,nl,ns);
printf("  DEM file  %s\t  nl,ns = %i\t%i\n\n",demfile,dem_nl,dem_ns);

get_facdr(sarfile,&ofdr);
if (strncmp(ofdr.grndslnt,"GROUN",5) == 0) GROUND_RANGE = 1;
else  GROUND_RANGE = 0;

if (GROUND_RANGE && form == -1)
  bye("Cannot perform ground and slant range functions simultaneously\n");

simbuf = (unsigned char *) MALLOC (nl*ns);

for (sample = 0; sample < nl*ns; sample++) simbuf[sample] = 0;
 
MSK
  {
   maskbuf = (unsigned char *) MALLOC (dem_ns);
   for (sample = 0; sample < dem_ns; sample++) maskbuf[sample] = 0;
   strcat(strcpy(tempfile,argv[1]),"_mask.img");
   msk_fp=fopenImage(tempfile,"wb");
  }

RTC
  {
   rtcbuf = (float *) MALLOC (dem_ns*sizeof(float));
   for (sample = 0; sample < dem_ns; sample++) rtcbuf[sample] = 1.0;
   strcat(strcpy(tempfile,argv[3]),"_rtc.img");
   rtc_fp=fopenImage(tempfile,"wb");
  }

PHS
  {
   phsbuf = (float *) MALLOC (nl*ns*sizeof(float));
   for (sample = 0; sample < nl*ns; sample++) phsbuf[sample] = 0.0;
  }

/* Prepare the DEM buffer for the first pass 
 ------------------------------------------*/
dembuf = (short *) MALLOC (3*dem_ns*sizeof(short));
strcat(demfile,".img");
dem_fp = fopenImage(demfile,"rb");
FREAD(&dembuf[dem_ns],2*dem_ns*sizeof(short),1,dem_fp);

for (i=0; i<dem_ns; i++) dembuf[i] = dembuf[i+dem_ns];
prev_dem_line = dembuf;
curr_dem_line = dembuf + dem_ns;
next_dem_line = dembuf + (2*dem_ns);

/*------------------------------------------------------------
  Main Processing Loop -- driven by lines from the input DEM
 ------------------------------------------------------------*/
h[SOUTH]   = h[EAST]   = 0.0;
lat[SOUTH] = lat[EAST] = 0.0;
lon[SOUTH] = lon[EAST] = 0.0;
proj_y = upleft[0];
for (line = 0; line < dem_nl; line++, proj_y -= pixsiz)
 {
  proj_x = upleft[1];
  for (sample = 0; sample < dem_ns; sample++, proj_x += pixsiz)
   {
    if ((h[PT]=(double) curr_dem_line[sample]) > 0.0)
     {
      /* Place correct elevation data in calling buffers
       ------------------------------------------------*/
      h[NORTH] = (double) prev_dem_line[sample];
      h[SOUTH] = (double) next_dem_line[sample];
      if (sample==0) h[WEST] = h[PT];
      else h[WEST] = (double) curr_dem_line[sample-1];
      if (sample==(dem_ns-1)) h[EAST] = h[PT];
      else h[EAST] = (double) curr_dem_line[sample+1];
 
      /* Convert UTM x,y to lon,lat
       ---------------------------*/
      tm_inverse(proj_x,proj_y,&(lon[PT]),&(lat[PT]));
      tm_inverse(proj_x,proj_y+pixsiz,&(lon[NORTH]),&(lat[NORTH]));
      tm_inverse(proj_x-pixsiz,proj_y,&(lon[WEST]),&(lat[WEST]));
      tm_inverse(proj_x,proj_y-pixsiz,&(lon[SOUTH]),&(lat[SOUTH]));
      tm_inverse(proj_x+pixsiz,proj_y,&(lon[EAST]),&(lat[EAST]));

      /* Call the sar model subroutine
       ------------------------------*/ 
      sar_sim(lat[PT],lon[PT],h[PT],lat[NORTH],lon[NORTH],h[NORTH],
              lat[WEST],lon[WEST],h[WEST],lat[SOUTH],lon[SOUTH],h[SOUTH],
              lat[EAST],lon[EAST],h[EAST], VIEW, &dn, 
              &sini, &mask, &in_line, &in_samp);

      /* Store the relevant results
       ---------------------------*/
      sim_line = in_line;
      sim_samp = in_samp;
      if ((sim_line>0)&&(sim_samp>0)&&(sim_line<=nl)&&(sim_samp<=ns))
        {
         all_out = 0;
         temp_val = SIMBUF(sim_line,sim_samp) + dn;
         SIMBUF(sim_line,sim_samp) = (temp_val > 255) ? 255: temp_val;
         PHS PHZBUF(sim_line,sim_samp) = (float) sini;
         MSK maskbuf[sample] = (unsigned char) mask;
         RTC rtcbuf[sample] = (float) sini;
        }
      good++;
     }
    else zeros++;	 
   } 

  /* Prepare dem buffer for next line of processing
   -----------------------------------------------*/
  temp = prev_dem_line;
  prev_dem_line = curr_dem_line;
  curr_dem_line = next_dem_line;
  next_dem_line = temp;	  
  if (line < dem_nl-2)
   {
    if (fread(next_dem_line,dem_ns*sizeof(short),1,dem_fp)!=1)
       bye("Demfile Read Error in dembuf\n");
   }
  else next_dem_line = curr_dem_line;
  
  MSK { fwrite(maskbuf,dem_ns,1,msk_fp);
        for (i=0;i<dem_ns;i++) maskbuf[i] = 0;
  }
  RTC { fwrite(rtcbuf,dem_ns*sizeof(float),1,rtc_fp);
        for (i=0;i<dem_ns;i++) rtcbuf[i] = 1.0;
  }
  if ((line%100)==0) printf("Processed line %4i\n",line);
}

free(dembuf);
fclose(dem_fp);
MSK fclose(msk_fp);
RTC fclose(rtc_fp);

printf("Finished all processing of sim image\n");

if (all_out)
  {
    printf("All sim points are out of the SAR scene\n"); 
    printf("Program aborted -- abnormal completion\n");
    exit(1);
  }

/* Write the output files
 ------------------------*/
strcat(strcpy(tempfile,argv[3]),".img");
if ((sim_fp = fopenImage(tempfile,"wb"))==NULL)
  { printf("SARSIM:  Unable to open file %s\n",tempfile); exit(1); }
fwrite(simbuf,nl*ns,1,sim_fp);
fclose(sim_fp);

PHS {
   strcat(strcpy(tempfile,argv[3]),"_phs.img");
   if (NULL==(phs_fp=fopenImage(tempfile,"wb")))
     { printf("\7\n Cannot Open File !! : %s\n\n",tempfile); exit(1); }
   fwrite(phsbuf,sizeof(float),nl*ns,phs_fp);
   fclose(phs_fp);

   strcat(strcpy(tempfile,argv[3]),"_phs.meta");
   strcat(strcpy(tmp2file,argv[1]),".meta");
   sprintf(cmd,"cp %s %s\n",tmp2file,tempfile);
   if (system(cmd) != 0)
    { printf("Failure copying metadata: Program Exited\n\n"); exit(1); }
   mod_ddr(tempfile,EREAL);
}

/* Copy image metadata for output files
 -------------------------------------*/
strcat(strcpy(tempfile,argv[3]),".meta");
strcat(strcpy(tmp2file,argv[1]),".meta");
sprintf(cmd,"cp %s %s\n",tmp2file,tempfile);
if (system(cmd) != 0)
  { printf("Failure copying metadata: Program Exited\n\n"); exit(1); }

MSK {
   strcat(strcpy(tempfile,argv[1]),"_mask.meta");
   strcat(strcpy(tmp2file,argv[2]),".meta");
   sprintf(cmd,"cp %s %s\n",tmp2file,tempfile);
   if (system(cmd) != 0)
     { printf("Failure copying metadata: Program Exited\n\n"); exit(1); }
   mod_ddr(tempfile,EBYTE);
 }

RTC {
   strcat(strcpy(tempfile,argv[3]),"_rtc.meta");
   strcat(strcpy(tmp2file,argv[2]),".meta");
   sprintf(cmd,"cp %s %s\n",tmp2file,tempfile);
   if (system(cmd) != 0)
     { printf("Failure copying metadata: Program Exited\n\n"); exit(1); }
   mod_ddr(tempfile,EREAL);
 }

printf("\nNumber of good points: %i \tNumber of zero points: %i\n",good,zeros);
strcat(strcpy(tempfile,argv[3]),".img");
printf("Sarsim sucessfully completed, wrote image %s\n",tempfile);

StopWatch();

exit(0);
}

/****************************************************************
FUNCTION NAME: bye
SYNTAX:  bye(message) -- Report message and exit processing
PARAMETERS:  char *message   Message to print to stdout on exit
RETURN VALUE: None
****************************************************************/
 
void bye(char *message)
 {
   printf("%s",message);
   exit(1);
 }
