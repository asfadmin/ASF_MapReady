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
#include <sys/types.h>
#include "sarsim.h"
#include "worgen.h"
#include "sarmodel.h"
#include "asf_reporting.h"
#define VERSION 5.0

 void bye (char *message); 
 void init_meta (struct DDR *dem_ddr, struct DDR *sar_ddr,
		 int *in_nl, int *in_ns, 
		 int *dem_nl, int *dem_ns, int *zone,
		 double *pixsiz, double *upleft);

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
int 	line,sample,i;          /* Loop counters                           */
char 	sarfile[255];            /* Input file name                         */
char	outfile[255];            /* Output file name                        */
char	demfile[255];            /* DEM file name                           */
char    coeffile[255];           /* coefficients file name                  */
char	dum1[20],dum2[4];       /* dummy read storage                      */
float   *dembuf;                /* Input Dem Buffer                        */
float   *demptr;                /* Input Dem Buffer Pointer                */
float   *sarbuf;                /* Input buffer                            */
float   *outbuf;                /* Output buffer                           */
float   *outptr;                /* Output buffer pointer                   */
float   tmpval;                 /* processing variable                     */
float   fill = 0;	        /* Background value                        */
int     dumi;                   /* dummy read storage                      */
int	good=0;			/* Test to see is any points are non-zero  */
FILE    *fpout;			/* File Pointer -- Used for Output file    */
FILE    *fp;                    /* File Pointer -- Used for Demfile        */
struct DDR dem_ddr;             /* DDR record for the DEM                  */
struct DDR sar_ddr;             /* DDR record for the SAR image            */
/***************************************************************************
                  S E T    U P    P R O C E S S I N G
****************************************************************************/
 
/* Initialize processing
  ---------------------*/
StartWatch();
if (argc != 5) 
 {
  asfPrintStatus("\n");
  asfPrintStatus("Usage:%s inDEMfile inSARfile inCOEFfile outCORfile\n",
		 argv[0]);
  asfPrintStatus("      inputs: inDEMfile.img   raw DEM file\n");
  asfPrintStatus("              inSARfile.img   SAR image\n");
  asfPrintStatus("              inCOEFfile.ppf  Coefficients File\n");
  asfPrintStatus("      output: outCORfile.img  Terrain Corrected Image\n\n");
  asfPrintStatus("      Version %.2f,  ASF Tools\n\n",VERSION);
  exit(1);
 }
strcpy(demfile,argv[1]);
strcpy(sarfile,argv[2]);
strcat(strcpy(coeffile,argv[3]),".ppf");
strcat(strcpy(outfile,argv[4]),".img");

asfPrintStatus("******* Starting SARGEOM Process *******\n");

/* Initialize image size globals
 ------------------------------*/
if (c_getddr(demfile, &dem_ddr)!= 0)
 { asfPrintError("Unable to get ddr file for image %s\n",demfile); }
if (c_getddr(sarfile, &sar_ddr)!= 0)
 { asfPrintError("Unable to get ddr file for image %s\n",sarfile); }

init_meta(&dem_ddr,&sar_ddr,&in_nl,&in_ns,&dem_nl,&dem_ns,
	  &zone,&pixsiz,upleft);
 
/* Read mapping coefficients file
 -------------------------------*/
if ((fp=fopen(coeffile,"r"))==0)
  {asfPrintError("Unable to open input file %s\n",coeffile); }
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[1]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[2]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[3]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[1]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[2]);
fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[3]);
fclose(fp);
 
/* Report Processing Parameters and allocate buffer space
 -------------------------------------------------------*/
asfPrintStatus("Azcoefs = %f %f %f\n",azcoef[1],azcoef[2],azcoef[3]);
asfPrintStatus("Grcoefs = %f %f %f\n",grcoef[1],grcoef[2],grcoef[3]);
out_ns = dem_ns;
out_nl = dem_nl;
dembuf = (float *) MALLOC (dem_ns*sizeof(float));
outbuf = (float *) MALLOC (out_ns*sizeof(float));
 
/* Initialize the Projection Transformation Package & geometry model
 ------------------------------------------------------------------*/
utm_init(zone);
init_sar_model(azcoef, grcoef, pixsiz, sarfile, 0);
 
/* Initialize input image and read into memory
 --------------------------------------------*/
sarbuf = (float *) MALLOC (in_nl*in_ns);
strcat(sarfile,".img");
fp = fopenImage(sarfile,"rb");
for (i = 0; i < in_nl; ++i)
  getFloatLine(fp, &sar_ddr, i, &sarbuf[i*in_ns]);
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
   getFloatLine(fp, &dem_ddr, 0, &dembuf[0]);
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
   putFloatLine(fpout,&sar_ddr,line,outbuf);
   if (line%100==0)  asfPrintStatus("Processing Line %4i\n",line);
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
  { asfPrintError("Failure copying metadata: Program Exited\n\n"); }
mod_ddr(sarfile,EBYTE);

asfPrintStatus("Sargeom sucessfully completed, wrote image %s\n",outfile);
StopWatch();
exit(0);
}

/****************************************************************************
                        S U P P O R T   R O U T I N E S 
*****************************************************************************/

void bye(message) char *message; { asfPrintError("%s",message); }

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

void init_meta(dem_ddr,sar_ddr,in_nl,in_ns,dem_nl,dem_ns,zone,pixsiz,upleft)
  struct DDR *dem_ddr;
  struct DDR *sar_ddr;
  int *in_nl;
  int *in_ns;
  int *dem_nl;
  int *dem_ns;
  int *zone;
  double *pixsiz;
  double *upleft;
{
  double temp;
 
  /* Read DEM ddr metadata file 
   ---------------------------*/
  if (dem_ddr->valid[1] == INVAL) dem_ddr->zone_code = 62;
  if (dem_ddr->valid[2] == INVAL) dem_ddr->datum_code= 0;
  *zone = dem_ddr->zone_code;
  *dem_ns = (int) dem_ddr->ns;
  *dem_nl = (int) dem_ddr->nl;
  upleft[0] = (double) dem_ddr->upleft[0];
  upleft[1] = (double) dem_ddr->upleft[1];
  temp = dem_ddr->pdist_x;

  /* Read SAR ddr metadata file
   ---------------------------*/
  *in_nl = sar_ddr->nl;
  *in_ns = sar_ddr->ns;
  *pixsiz = sar_ddr->pdist_x;
 
  if (*pixsiz != temp) bye("DEM and SAR resolutions mismatch\n\n");

  return;
}
