/****************************************************************************
NAME:	 SARGEOM  -- Creates geometrically corrected SAR image
 
SYNOPSIS:  sargeom inDEMfile inSARfile inCOEFfile outCORfile 
 
DESCRIPTION: Geocodes & terrain corrects Synthetic Aperature Radar images
 
EXTERNAL ASSOCIATES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    utm_init             Initializes the UTM projection transformation
                         package based on the given UTM zone
    tm_inverse           returns lat,lon for given UTM x,y coordinates
    init_sar_model       intializes SARMODEL based on given SAR scene
    sar_sim              returns coordinates of SAR image corresponding
                         to the given lat,lon,height coordinates
 
FILE REFERENCES:
    NAME:                USAGE:
    ---------------------------------------------------------------
    inDEMfile.img        raw DEM file of T3D doubles
    inDEMfile.ddr        DEM file's LAS data descriptor record file
    inSARfile.img        SAR image 
    inSARfile.trl (.L)   trailer file for SAR image
    inSARfile.ldr (.L)   leader file for SAR image
    inCOEFfile.ppf       Mapping Coefficients File
    outCORfile.img       Ouput Terrain Corrected Image
    outCORfile.ddr       Ouput Terrain Corrected Image Metadata
 
PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:       PROJECT:       PURPOSE:
    ---------------------------------------------------------------
    1.0	    7/91    D. Steinwand  EROS/AB-ISS 
    2.0	    8/94    T. Logan      ASF/TerrCorr   Ported to YMP
    3.0     8/94    T. Logan      ASF/TerrCorr   Ported to T3D
    4.0	    9/94    T. Logan      ASF/TerrCorr   Changed to master/slave
                                                 Implemented Block I/O
    5.0	   10/95    T. Logan	  ASF/TerrCorr   Ported to Solaris
 
HARDWARE/SOFTWARE LIMITATIONS:
         The DEM file that is given to SARGEOM must be already clipped
         to the proper area of coverage and pixel spacing corresponding 
         to the input SAR image.  This can be accomplished by using the
 	 ASF software tool demclip.
 
ALGORITHM DESCRIPTION:
 MPP ALGORITHM:
 --------------
      for each line in this DEM file 
        for each sample in this line
           if the DEM height at line, sample > 0
             find lon,lat coords from DEM x,y projection
             call SAR model routine sar_sim to get in_line & in_sample
             use SAR image in_line,in_sample value for line,sample output
           else
             set line,sample to fill value
 
ALGORITHM REFERENCES:
 
BUGS:
 
****************************************************************************/
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

RICK GURITZ      rguritz@images.alaska    (907)474-7886
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
#include <sys/types.h>
#include "sarsim.h"
#include "worgen.h"
#include "sarmodel.h"
#define VERSION 5.0

 void bye (char *message); 
 void init_meta (char *demfile, char *sarfile, int *in_nl, int *in_ns, 
 	int *dem_nl, int *dem_ns, int *zone, double *pixsiz, double *upleft);

int main(argc, argv)
    int argc; char** argv;
{
/***************************************************************************/
double  proj_x, proj_y;         /* X & Y coordinates input to transform    */
double  upleft[2];              /* X & Y coordinates of DEM UL corner      */
double  lat, lon, height;       /* Input variables for Sar Model code      */
double  lat1, lon1, z1;         /* Input variables for Sar Model code      */
double  lat2, lon2, z2;         /* Input variables for Sar Model code      */
double  lat3, lon3, z3;         /* Input variables for Sar Model code      */
double  lat4, lon4, z4;         /* Input variables for Sar Model code      */
double  sini,mask,intensity;    /* Return variables for Sar Model code     */
double  in_line, in_samp;       /* Return variables for Sar Model code     */
double  azcoef[4], grcoef[4];   /* Azimuth and Ground Range Coefficients   */
double  pixsiz;                 /* Processing Pixel Size                   */
int    in_nl, in_ns;           /* Input SAR number of lines & samples     */
int    dem_ns, dem_nl;         /* Input Dem number of lines & samples     */
int    zone;                   /* Dem Zone Code                           */
int    viewflg;                /* Return variables for Sar Model code     */
int 	out_nl,out_ns;	        /* Number of lines and samples (output)    */
int 	s_line,s_samp;          /* current line and sample (output)        */
int 	line,sample;          /* Loop counters                           */
char 	sarfile[255];            /* Input file name                         */
char	outfile[255];            /* Output file name                        */
char	demfile[255];            /* DEM file name                           */
char    coeffile[255];           /* coefficients file name                  */
char	dum1[20],dum2[4];       /* dummy read storage                      */
short   *dembuf;                /* Input Dem Buffer                        */
short   *demptr;                /* Input Dem Buffer Pointer                */
unsigned char *sarbuf;          /* Input buffer                            */
unsigned char *outbuf;          /* Output buffer                           */
unsigned char *outptr;          /* Output buffer pointer                   */
unsigned char tmpval;           /* processing variable                     */
unsigned char fill = 0;	        /* Background value                        */
int     dumi;                   /* dummy read storage                      */
int	good=0;			/* Test to see is any points are non-zero  */
FILE    *fpout;			/* File Pointer -- Used for Output file    */
FILE    *fp;                    /* File Pointer -- Used for Demfile        */
/***************************************************************************
                  S E T    U P    P R O C E S S I N G
****************************************************************************/
 
/* Initialize processing
  ---------------------*/
StartWatch();
if (argc != 5) 
 {
  printf("\n");
  printf("Usage:%s inDEMfile inSARfile inCOEFfile outCORfile\n", argv[0]);
  printf("      inputs: inDEMfile.img   raw DEM file\n");
  printf("              inSARfile.img   SAR image\n");
  printf("              inCOEFfile.ppf  Coefficients File\n");
  printf("      output: outCORfile.img  Terrain Corrected Image\n\n");
  printf("      Version %.2f,  ASF STEP TOOLS\n\n",VERSION);
  exit(1);
 }
strcpy(demfile,argv[1]);
strcpy(sarfile,argv[2]);
strcat(strcpy(coeffile,argv[3]),".ppf");
strcat(strcpy(outfile,argv[4]),".img");

printf("******* Starting SARGEOM Process *******\n");

/* Initialize image size globals
 ------------------------------*/
init_meta(demfile,sarfile,&in_nl,&in_ns,&dem_nl,&dem_ns,&zone,&pixsiz,upleft);
 
/* Read mapping coefficients file
 -------------------------------*/
if ((fp=fopen(coeffile,"r"))==0)
  {printf("Unable to open input file %s\n",coeffile); exit(1); }
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[1]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[2]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[3]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[1]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[2]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[3]);
fclose(fp);
 
/* Report Processing Parameters and allocate buffer space
 -------------------------------------------------------*/
printf("Azcoefs = %f %f %f\n",azcoef[1],azcoef[2],azcoef[3]);
printf("Grcoefs = %f %f %f\n",grcoef[1],grcoef[2],grcoef[3]);
out_ns = dem_ns;
out_nl = dem_nl;
dembuf = (short *) MALLOC (dem_ns*sizeof(short));
outbuf = (unsigned char *) MALLOC (out_ns);
 
/* Initialize the Projection Transformation Package & geometry model
 ------------------------------------------------------------------*/
utm_init(zone);
init_sar_model(azcoef, grcoef, pixsiz, sarfile, 0);
 
/* Initialize input image and read into memory
 --------------------------------------------*/
sarbuf = (unsigned char *) MALLOC (in_nl*in_ns);
strcat(sarfile,".img");
fp = fopenImage(sarfile,"rb");
FREAD(sarbuf,in_ns,in_nl,fp);
FCLOSE(fp);
 
viewflg=0;
intensity = 0.0;
lat1 = lon1 = z1 = 0.0;
lat2 = lon2 = z2 = 0.0;
lat3 = lon3 = z3 = 0.0;
lat4 = lon4 = z4 = 0.0;
 
/* Open the output file and the DEM file
 --------------------------------------*/
fpout = fopenImage(outfile,"wb");
strcat(demfile,".img");
fp = fopenImage(demfile,"rb");
/****************************************************************************
                   M A I N     W O R K    L O O P 
****************************************************************************/
proj_y = upleft[0];
for (line = 0; line < dem_nl; line++, proj_y -= pixsiz)
 {
   FREAD(dembuf,dem_ns*sizeof(short),1,fp);
   demptr = dembuf;
   outptr = outbuf;
   proj_x = upleft[1];
   for (sample = 0; sample < dem_ns; sample++, proj_x += pixsiz)
    {
     height = (double) (*demptr++);
     if (height > 0.0)
      {
        tm_inverse(proj_x,proj_y,&lon,&lat);
        sar_sim(lat,lon,height,lat1,lon1,z1,lat2,lon2,z2,lat3,lon3,z3,lat4,
                lon4,z4,viewflg,&intensity,&sini,&mask,&in_line,&in_samp);
        s_line = (int) (in_line - 0.5);
        s_samp = (int) (in_samp - 0.5);
        if (s_line<0||s_line>=in_nl||s_samp<0||s_samp>=in_ns) tmpval = fill;
        else tmpval = sarbuf[(s_line*in_ns)+s_samp];
        *outptr++ = tmpval;
        if (tmpval > 0) good = 1;
      }
     else *outptr++ = fill;
    }
   if (fwrite(outbuf,out_ns,1,fpout)==0)
     { printf("Failure writing output image %s\n",outfile); exit(1); }
   if (line%100==0)  printf("Processing Line %4i\n",line);
 }
/****************************************************************************
               Final Processing 
****************************************************************************/
fclose(fpout);
free(dembuf);
free(sarbuf);

if (good == 0) bye("All points fell outside the SAR scene\7\7\n");

strcat(strcpy(demfile,argv[1]),".ddr");
strcat(strcpy(sarfile,argv[4]),".ddr");
sprintf(dum1,"cp %s %s\n",demfile,sarfile);
if (system(dum1) != 0)
  { printf("Failure copying metadata: Program Exited\n\n"); exit(1); }
mod_ddr(sarfile,EBYTE);

printf("Sargeom sucessfully completed, wrote image %s\n",outfile);
StopWatch();
exit(0);
}

/****************************************************************************
                        S U P P O R T   R O U T I N E S 
*****************************************************************************/

void bye(message) char *message; { printf("%s",message); exit(1); }

/****************************************************************
FUNCTION NAME: init_meta -- initializes image metadata

SYNTAX: init_meta(demfile,sarfile,&in_nl,&in_ns,&dem_nl,&dem_ns,
                  &zone,&pixsiz,upleft)

EXTERNAL ASSOCIATE: c_getddr  Reads a LAS format data descriptor record

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    ----------------------------------------------------------
    demfile     char *          name of input DEM file
    sarfile     char *          name of input SAR file
    in_nl       int *		input (SAR) number of lines
    in_ns       int *		input (SAR) number of samples
    dem_nl      int *		input (DEM) number of lines
    dem_ns	int *          input (DEM) number of samples
    zone	int *		UTM zone number of DEM
    pixsiz	double *	Pixel resolution of processing 
    upleft      double *        upper left DEM UTM coordinates

DESCRIPTION:  Read projection information from the DEM , 
              Calculate output image size and windowing,
              Set globals

RETURN VALUE: None

SPECIAL CONSIDERATIONS:
****************************************************************/
#include "ddr.h"

void init_meta(demfile,sarfile,in_nl,in_ns,dem_nl,dem_ns,zone,pixsiz,upleft)
  char *demfile;
  char *sarfile;
  int *in_nl;
  int *in_ns;
  int *dem_nl;
  int *dem_ns;
  int *zone;
  double *pixsiz;
  double *upleft;
{
  struct DDR ddr;       /* DDR metedata structure        */
  double temp;
 
  /* Read DEM ddr metadata file 
   ---------------------------*/
  if (c_getddr(demfile, &ddr)!= 0)
    { printf("Unable to get ddr file for image %s\n",demfile); exit(1); }
  if (ddr.valid[1] == INVAL) ddr.zone_code = 62;
  if (ddr.valid[2] == INVAL) ddr.datum_code= 0;
  *zone = ddr.zone_code;
  *dem_ns = (int) ddr.ns;
  *dem_nl = (int) ddr.nl;
  upleft[0] = (double) ddr.upleft[0];
  upleft[1] = (double) ddr.upleft[1];
  temp = ddr.pdist_x;

  /* Read SAR ddr metadata file
   ---------------------------*/
  if (c_getddr(sarfile, &ddr)!= 0)
    { printf("Unable to get ddr file for image %s\n",sarfile); exit(1); }
  *in_nl = ddr.nl;
  *in_ns = ddr.ns;
  *pixsiz = ddr.pdist_x;
 
  if (*pixsiz != temp) bye("DEM and SAR resolutions mismatch\n\n");

  return;
}
