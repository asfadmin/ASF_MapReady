/****************************************************************************
NAME:	 DEMIMG  -- Creates geometrically corrected SAR image

SYNOPSIS:  demimg inDEMfile inSARfile inCOEFfile outfile

	inDEMfile 	Input height file in UTM projection
	inSARfile	Input SAR image file - used for metadata
	inCOEFfile	Input coefficients file used for resampling
	outfile		Output Slant range DEM file

DESCRIPTION:

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
    inDEMfile.img        raw DEM file of doubles
    inDEMfile.ddr        DEM file's LAS data descriptor record file
    inSARfile.img        SAR image
    inSARfile.trl        trailer file for SAR image
    inSARfile.ldr        leader file for SAR image
    inCOEFfile.ppf       Mapping Coefficients File
    outfile.img          Ouput Terrain Corrected Image
    outfile.ddr          Ouput Terrain Corrected Image Metadata

PROGRAM HISTORY:
    VERS:   DATE:   AUTHOR:       PROJECT:       PURPOSE:
    ---------------------------------------------------------------
    1.0	    7/96    M. Shindle    ifsar   Original - based on SARGEOM,
				          version 5.0 written by Tom Logan


HARDWARE/SOFTWARE LIMITATIONS:
         The DEM file that is given to DEMIMG must be already clipped
         to the proper area of coverage and pixel spacing corresponding
         to the input SAR image.  This can be accomplished by using the
 	 ASF software tool demclip.

ALGORITHM DESCRIPTION:
    for each line in this DEM file
      for each sample in this line
          find lon,lat coords from DEM x,y projection
          call SAR model routine sar_sim to get in_line, in_sample (from SAR)
          use DEM line, sample value for in_line, in_sample output pixel

ALGORITHM REFERENCES:

BUGS:

****************************************************************************
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
#include "worgen.h"
#include "dem.h"
#include "asf_meta.h"
#include "asf_reporting.h"

/* constants */
#define VERSION   1.0
#define BUFFER	256

/* local functions */

int
main(int argc, char *argv[])
{
  double proj_x, proj_y;        /* X & Y coordinates input to transform    */
  double upleft[2];             /* X & Y coordinates of DEM UL corner      */
  double lat, lon, height;      /* Input variables for Sar Model code      */
  double lat1, lon1, z1;        /* Input variables for Sar Model code      */
  double lat2, lon2, z2;        /* Input variables for Sar Model code      */
  double lat3, lon3, z3;        /* Input variables for Sar Model code      */
  double lat4, lon4, z4;        /* Input variables for Sar Model code      */
  double sini,mask,intensity;   /* Return variables for Sar Model code     */
  double in_line, in_samp;      /* Return variables for Sar Model code     */
  double azcoef[4], grcoef[4];  /* Azimuth and Ground Range Coefficients   */
  double pixsiz;                /* Processing Pixel Size                   */
  int   sar_nl, sar_ns;        /* Input SAR number of lines & samples     */
  int   dem_ns, dem_nl;        /* Input Dem number of lines & samples     */
  int   zone;                  /* Dem Zone Code                           */
  int   viewflg;               /* Return variables for Sar Model code     */
  int   s_line,s_samp;         /* current line and sample (output)        */
  int   i,line,sample;       	/* Loop counters                           */
  char   sarfile[BUFFER];       /* Input file name                         */
  char   outfile[BUFFER];       /* Output file name                        */
  char   demfile[BUFFER];       /* DEM file name                           */
  char   coeffile[BUFFER];      /* coefficients file name                  */
  char   dum1[20],dum2[4];      /* dummy read storage                      */
  float  *dembuf;               /* Input Dem Buffer                        */
  float  *demptr;               /* Input Dem Buffer Pointer                */
  float  *outbuf;               /* Output buffer                           */
  float  tmpval;                /* processing variable                     */
  int    dumi;                  /* dummy read storage                      */
  FILE   *fpout;	        /* File Pointer -- Used for Output file    */
  FILE   *fp;                   /* File Pointer -- Used for Demfile        */

  /*
   * SET UP PROCESSING
   */
  StartWatch();

  /* Initialize processing
    ---------------------*/
  if (argc != 5) usage(argv[0]);
  strcpy(demfile,argv[1]);
  strcpy(sarfile,argv[2]);
  strcat(strcpy(coeffile,argv[3]),".ppf");
  strcat(strcpy(outfile,argv[4]),".img");
  printf("\n\n******* Starting DEMIMG Process *******\n\n");
  viewflg=0;
  intensity = 0.0;
  lat1 = lon1 = z1 = 0.0; lat2 = lon2 = z2 = 0.0;
  lat3 = lon3 = z3 = 0.0; lat4 = lon4 = z4 = 0.0;

  /* Initialize image size globals
   ------------------------------*/
  // Read DEM metadata
  meta_parameters *metaDEM = meta_read(demfile);
  dem_ns = metaDEM->general->sample_count;
  dem_nl = metaDEM->general->line_count;
  if (metaDEM->projection != NULL) {
    if ( meta_is_valid_int(metaDEM->projection->param.utm.zone) ) {
      zone = metaDEM->projection->param.utm.zone;
    } else {
      asfPrintError("Invalid UTM zone code in the DEM metadata\n");
    }
    if (meta_is_valid_double(metaDEM->projection->startY)) {
      upleft[0] = metaDEM->projection->startY;
    } else {
      asfPrintError("Invalid upper left Y projection coordinate in the DEM metadata\n");
    }
    if (meta_is_valid_double(metaDEM->projection->startX)) {
      upleft[1] = metaDEM->projection->startX;
    } else {
      asfPrintError("Invalid upper left X projection coordinate in the DEM metadata\n");
    }
    if (meta_is_valid_double(metaDEM->projection->perX)) {
      pixsiz = metaDEM->projection->perX;
    } else {
      asfPrintError("Invalid projection distance in the DEM metadata\n");
    }
  }

  // Read SAR metadata
  meta_parameters *metaSAR = meta_read(sarfile);
  sar_ns = metaSAR->general->sample_count;
  sar_nl = metaSAR->general->line_count;

  /* Read mapping coefficients file
   -------------------------------*/
  if ((fp=fopen(coeffile,"r"))==0) {
    bye(("Unable to open input file %s\n",coeffile));
  }
  fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[1]);
  fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[2]);
  fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&azcoef[3]);
  fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[1]);
  fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[2]);
  fscanf(fp,"%s%s%i%lf",dum1,dum2,&dumi,&grcoef[3]);
  fclose(fp);

  /* Report Processing Parameters and allocate/init buffer space
   ------------------------------------------------------------*/
  printf("Azcoefs = %f %f %f\n",azcoef[1],azcoef[2],azcoef[3]);
  printf("Grcoefs = %f %f %f\n",grcoef[1],grcoef[2],grcoef[3]);
  dembuf = (float *)MALLOC(dem_ns*sizeof(float));
  outbuf = (float *)MALLOC(sar_nl*sar_ns*sizeof(float));
  for (i=0;i<sar_nl*sar_ns;i++) outbuf[i] = 0.0;

  /* Initialize the Projection Transformation Package & geometry model
   ------------------------------------------------------------------*/
  utm_init(zone);
  init_sar_model(azcoef, grcoef, pixsiz, sarfile, 0);

  /* Open the output file and the DEM file
   --------------------------------------*/
  if ((fpout = fopenImage(outfile,"wb"))==NULL)
    bye(("Failure opening output image %s\n",outfile));
  strcat(demfile,".img");
  if ((fp = fopenImage(demfile,"rb"))==NULL)
    bye(("Failure opening input image %s\n",demfile));

  /*
   *  MAIN WORK LOOP
   */
  proj_y = upleft[0];
  for (line = 0; line < dem_nl; line++, proj_y -= pixsiz)
   {
     get_float_line(fp, metaDEM, line, dembuf);
     demptr = dembuf;
     proj_x = upleft[1];
     for (sample = 0; sample < dem_ns; sample++, proj_x += pixsiz)
      {
        tmpval = *demptr;
        height = (double)(*demptr++);
        tm_inverse(proj_x,proj_y,&lon,&lat);
        sar_sim(lat,lon,height,lat1,lon1,z1,lat2,lon2,z2,lat3,lon3,z3,lat4,
                lon4,z4,viewflg,&intensity,&sini,&mask,&in_line,&in_samp);
        s_line = (int) (in_line - 0.5);
        s_samp = (int) (in_samp - 0.5);
        if (s_line>=0 && s_line<sar_nl && s_samp>=0 && s_samp<sar_ns)
          outbuf[(s_line*sar_ns)+s_samp] = tmpval;
      }
     asfLineMeter(line, dem_nl);
  }
  put_data_lines(fpout, metaSAR, 0, sar_nl, outbuf, REAL32);

  /*
   * Final Processing
   */

  fclose(fpout);
  free(dembuf);
  free(outbuf);

  strcat(strcpy(sarfile,argv[2]),".meta");
  strcat(strcpy(outfile,argv[4]),".meta");
  copyfile(sarfile,outfile);

  printf("\n\nDEMIMG completed. Version %.2f, ASF Tools\n",VERSION);
  StopWatch();

  exit(EXIT_SUCCESS);
}

void usage(char *name)
{
  printf("\n");
  printf("Usage:\n");
  printf("  %s inDEMfile inSARfile inCOEFfile outfile\n", name);
  printf("      inputs: inDEMfile.img   raw DEM file\n");
  printf("              inSARfile.img   SAR image\n");
  printf("              inCOEFfile.ppf  Coefficients File\n");
  printf("      output: outfile.img  Terrain Corrected Image\n\n");
  printf("      Version %.2f, ASF Tools\n\n",VERSION);
  exit(1);
}

void copyfile(char *to, char *from)
{
  FILE *fin, *fout;
  int nbytes, nread;
  unsigned char *buf;

  /* open files */
  fin = fopen(to,"rb");
  if (fin == NULL) bye(("Unable to open file '%s' for reading.",to));
  fout = fopen(from,"wb");
  if (fout == NULL) bye(("Unable to open file '%s' for writing.",from));

  /* determine size of input file */
  fseek(fin,0,2);
  nbytes=ftell(fin);
  fseek(fin,0,0);

  /* allocate array to hold data */
  buf = (unsigned char *)MALLOC(nbytes);

  /* read in data */
  nread=fread(buf,1,nbytes,fin);
  if (nread != nbytes)
    bye(("Error only read %d bytes of %d for file '%s'",nread,nbytes,to));

  /* write out data */
  nread=fwrite(buf,1,nbytes,fout);
  if (nread != nbytes)
    bye(("Error only wrote %d bytes of %d for file '%s'",nread,nbytes,from));

  /* exit out of here */
  free(buf);
  fclose(fin);
  fclose(fout);

  return;
}


