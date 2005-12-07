/****************************************************************
NAME:  demclip -- clips an input DEM to the coverage of a SAR scene

SYNOPSIS:  demclip inDEM inSAR [inCOEF] outDEM [u]

DESCRIPTION:
        This program uses the SAR image metadata to calculate the UTM
      projection of the corner coordinates of a SAR scene.  These are
      compared with the DEM corner coordinates and a maximum size window
      is formed.  Next, offsets for the SAR into the DEM file, and the
      nl,ns covered by the image are determined.  In its final stage,
      demclip reads the appropriate portion of the LAS 6.0 INT*2 image
      file, and creates <outDEM> (same LAS 6.0 INT*2 format with a valid
      .ddr file).

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    ex_corners          Extracts corner locations from SAR metadata
    c_getddr            Reads a DDR file into a structure
    for_init            Initialize the projection package
    for_trans		Xform lat,lon to projection X,Y

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    demfile.img         Input DEM file, raw INT*2 format
    demfile.ddr         Input DEM metadata
    outfile.img         Output INT*2 raw format DEM file
    outfile.ddr         Data descriptor for output DEM file

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:    PURPOSE:
    ---------------------------------------------------------------
     1.0    7/95   T. Logan   Incorporate DEM preprocessing in TC package
     2.0   10/95   T. Logan   Port from YMP to Solaris
     2.1   12/96   T. Logan   Allow input file of corner coordinates
     2.2    7/00   M. Jessop  Modified to set area outside swath to zero
     3.0    7/00   T. Logan   Allow use of any map projection
     4.0   11/00   J. Badgley Recombined demclip and demclip.mod.  Demclip
			      now accepts as input an optional
			      coefficients file.

HARDWARE/SOFTWARE LIMITATIONS:
        The input DEM must be INT*2, with a valid LAS 6.0 ddr file

ALGORITHM DESCRIPTION:
        Read region of interest extents 
        Read DEM ddr metadata file
        Convert the region of interest extents to the projection
        Determine the placement of the region in the DEM file
        Determine the number of line and samples in output DEM
        Read the appropriate portion of the DEM file and write
          to the output file
        Create a valid DDR file for the output DEM

ALGORITHM REFERENCES:
	This program is loosely based on the algorithm used in
     the LAS application remap.  All of the code was rewritten.

BUGS:

****************************************************************/
/****************************************************************************
*								            *
*   demclip -- Clips an input DEM file to the coverage of a SAR image       *
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
#include "ddr.h"
#include "cproj.h"
#include "proj.h"
#include "asf_meta.h"
#define RPD 0.017453293		/* Radians per Degree */
#define VERSION 4.0

int mapedge(double *gul,double *glr,double *sul,double *slr,struct DDR ddr);

void ex_add_pt(meta_parameters *meta,int x,int y,double *min,double *max)
{
	double lat,lon;
	meta_get_latLon(meta,y,x,0,&lat,&lon);
	if (min[0]>lat) min[0]=lat;
	if (max[0]<lat) max[0]=lat;
	if (min[1]>lon) min[1]=lon;
	if (max[1]<lon) max[1]=lon;
}

void ex_corners(char *fName,double *min,double *max)
{
	meta_parameters *meta=meta_init(fName);
	int nl=meta->ifm->orig_nLines,
            ns=meta->ifm->orig_nSamples;
	min[0]=min[1]=1000;
	max[0]=max[1]=-1000;
	ex_add_pt(meta,0,0,min,max);
	ex_add_pt(meta,0,nl,min,max);
	ex_add_pt(meta,ns,nl,min,max);
	ex_add_pt(meta,ns,0,min,max);
	ex_add_pt(meta,ns/2,nl/2,min,max);
}

int main(argc,argv)
    int argc; 
    char **argv;
  {
    char inFDR[255];		/* Input SAR file w/o extension 	*/
    char inDEM[255];		/* Input DEM file name 			*/
    char inDDR[255];		/* Input DDR file name 			*/
    char outDEM[255];		/* Output DEM file name 		*/
    char inCFS[255];		/* Input COEF file name			*/
    FILE *fpi;			/* Input DEM file pointer 		*/
    FILE *fpo;			/* Output DEM file pointer 		*/
    FILE *fpc;			/* Input COEF file pointer		*/    
    struct DDR ddr;		/* Data Descriptor Record structure 	*/
    double sul[2],slr[2],	/* SAR file corner coordinates in UTM 	*/
           gul[2],glr[2],	/* Lat,Lon coordinates for SAR scene 	*/
            ul[2],lr[2];	/* Output DEM corner coordinates 	*/
    int	    counter;
    double  tul[2],tlr[2];	/* Temporary corner coordinates in      */
    double  a1, b1, c1, 
	    a2, b2, c2, 	/* Coefficients read from inCOEF file	*/
	    minsmp, maxsmp, 	/* Min and max easting values for clipping */
	    nrth, east, 	/* Current northing & easting position	*/
	    tmp;	    
    int  off[2];		/* Offsets of SAR into DEM 		*/	
    int  nl,ns,i,ok=0;		/* Number lines, samples in output DEM 	*/
    int   index, cnt;
    long long find;		/* modified for fseek64			*/
    float  *obuf;		/* I/O buffer for transfer data 	*/
    forward_transform latLon2proj[100];
    int iflg=0,
        modflg=0;

    StartWatch();

    if ((argc == 6 && *argv[5] == 'u') || (argc == 5 && *argv[4] != 'u')) 
	modflg = 1;

    if ((argc < 4  || argc > 6) && !modflg)
      {
        printf("\n\nUsage: %s inDEM inSAR [inCOEF] outDEM [u]\n\n",argv[0]);
	printf( "\t<inDEM>  Input LAS 6.0 INT*2 DEM file\n"
		"\t<inSAR>  Input SAR file metadata\n"
		"\tinCOEF   Option specifies a coeficients file (.coefs)\n"
		"\t         and uses the modified demclip code.  In this\n"
		"\t         version the area outside of the swath will be cut\n"
		"\t<outDEM> Output DEM file with valid LAS 6.0 ddr metadata.\n"
		"\t  u      Option specifies user entered extend file called\n"
		"\t         inSAR of the form UL_lat UL_lon LR_lat LR_lon\n"); 
	printf("\nClips the input DEM to match the area covered by the input\n"
	      "SAR file metadata, giving an output DEM file with valid LAS\n"
	      "6.0 ddr metadata\n");
	printf("\nVersion %.2f, ASF SAR TOOLS\n\n",VERSION);
        exit(1);
      }

    strcpy(inFDR,argv[2]);
    strcat(strcpy(inDEM,argv[1]),".img");
    strcat(strcpy(inDDR,argv[1]),".ddr");
    if (modflg) {
	strcat(strcpy(outDEM,argv[4]),".img");
    	strcpy(inCFS,argv[3]);
    }
    else strcat(strcpy(outDEM,argv[3]),".img");

    
    /* Read SAR image extents & convert to radians */
    if (argc-modflg == 4) ex_corners(inFDR,gul,glr);
    else if (argc-modflg == 5)
      {
        fpi=FOPEN(inFDR,"r");
	fscanf(fpi,"%lf %lf %lf %lf",&gul[0],&gul[1],&glr[0],&glr[1]);
	fclose(fpi);
      }

    gul[0] *= RPD; gul[1] *= RPD; glr[0] *= RPD; glr[1] *= RPD;

    if (modflg) {
    	fpc = FOPEN(inCFS, "r");
    	fscanf (fpc, "%lf %lf %lf %lf %lf %lf", &a1, &b1, &c1, &a2, &b2, &c2);
    	fclose (fpc);
    }

    /*  Read input DEM DDR file */
    if (c_getddr(inDEM,&ddr) != 0)
      {
        printf("Error returned from c_getddr:  unable to read file %s\n",inDEM);
        exit(1);
      }

    /* calculate corner projection coordinates for the SAR image
     ----------------------------------------------------------*/

    if (modflg) {
    	for_init(ddr.proj_code,ddr.zone_code,ddr.proj_coef,ddr.datum_code,NULL,
	NULL,&iflg,latLon2proj);

    	latLon2proj[ddr.proj_code](gul[1], gul[0], &tul[1], &tul[0]);
    	latLon2proj[ddr.proj_code](glr[1], glr[0], &tlr[1], &tlr[0]);
    	sul[0] = (tul[0] > tlr[0]) ? tul[0] : tlr[0];
    	sul[1] = (tul[1] < tlr[1]) ? tul[1] : tlr[1];
    	slr[0] = (tul[0] < tlr[0]) ? tul[0] : tlr[0];
    	slr[1] = (tul[1] > tlr[1]) ? tul[1] : tlr[1];
    }
    else mapedge(gul,glr,sul,slr,ddr);

    printf("Calculated SAR corners (UL) : %f,%f\n",sul[0],sul[1]);
    printf("Calculated SAR corners (LR) : %f,%f\n",slr[0],slr[1]);
    printf("Actual DEM corners (UL)     : %f,%f\n",ddr.upleft[0],
	ddr.upleft[1]);
    printf("Actual DEM corners (LR)     : %f,%f\n",ddr.loright[0],
	ddr.loright[1]);
    printf("Calculated output DEM size (l,s)   : %f,%f\n",
        fabs((sul[0]-slr[0])/ddr.pdist_x)+1.0,
        fabs((sul[1]-slr[1])/ddr.pdist_x)+1.0);

    /*  Set the corner coordinates for the output DEM file 
     ----------------------------------------------------*/
    ul[0] = (ddr.upleft[0] < sul[0]) ? ddr.upleft[0] : sul[0];
    ul[1] = (ddr.upleft[1] > sul[1]) ? ddr.upleft[1] : sul[1];
    lr[0] = (ddr.loright[0] > slr[0]) ? ddr.loright[0] : slr[0];
    lr[1] = (ddr.loright[1] < slr[1]) ? ddr.loright[1] : slr[1];

    /* Determine offsets of SAR into DEM file
     ---------------------------------------*/
    off[0] = (int) ((fabs(ddr.upleft[0] - ul[0]))/ ddr.pdist_x + 0.5);
    off[1] = (int) ((fabs(ddr.upleft[1] - ul[1]))/ ddr.pdist_y + 0.5);

    /* Determine number of lines and samples needed
     ---------------------------------------------*/
    nl = ((ul[0]-lr[0])/ddr.pdist_x)+1.01;
    ns = ((lr[1]-ul[1])/ddr.pdist_y)+1.01;

    if (nl > ddr.nl-off[0]) nl = ddr.nl-off[0];
    if (ns > ddr.ns-off[1]) ns = ddr.ns-off[1];

    printf("Calculated offsets are %i,%i\n",off[0],off[1]);
    printf("Calculated image size is %i,%i\n",nl,ns);
    
    if (nl<0 || ns<0) { printf("ABORTED: nothing to clip...\n"); exit(1); }
    
    obuf = (float *) MALLOC (nl*ns*sizeof(float));
    fpi = fopenImage(inDEM,"rb");
    for (index=0; index < nl*ns; index++) {
      obuf[index] = 0.0;
    }

    find = (long long)(off[1]+ddr.ns*off[0]);
    index = 0;
    counter = 0;

    for (cnt = 0; cnt < nl; cnt++, counter++)
     {
       if (FSEEK64(fpi,find,0)!=0)
        { printf("Unable to seek byte %lli in input file",find); exit(1);}
       if (fread(&obuf[index],ns*sizeof(float),1,fpi)!= 1)
        { printf("Unable to read the input DEM file"); exit(1); }
	
       index += ns;
       find  += ddr.ns;
       if ((counter%1000)==0) { printf("Processed line %5i\n", counter); }
     }
    
    if (modflg) {
    	printf("Setting area outside swath edge to zero...\n");
    	index = 0;
    	for (cnt = 0; cnt < nl; cnt++) {
	    nrth = ul[0] - cnt*ddr.pdist_y;
	    minsmp = a1*nrth*nrth + b1*nrth + c1;
	    maxsmp = a2*nrth*nrth + b2*nrth + c2;
	    if (minsmp>maxsmp) { tmp=minsmp; minsmp=maxsmp; maxsmp=tmp; }
	    for (i=0; i<ns; i++) {
	        east = ul[1] + i*ddr.pdist_x;
	        if (east<minsmp || east>maxsmp) {
		    obuf[index+i]=0.0;
		}	    
	    }
	    index += ns;
        } 
    }
	
    for (i = 0; i < nl*ns; ++i) { 
        if (obuf[i]!=0)  { ok = 1; break; }
    }
    if (!ok) { printf("All points in the DEM file are zero!!!\n"); exit(1); }
    fclose(fpi);
    printf("Writing Output DEM...\n");
    fpo = fopenImage(outDEM,"wb");
    fwrite(obuf,ns*nl*sizeof(float),1,fpo);
    fclose(fpo);    
    free(obuf);
    
    ddr.upleft[0] = ul[0]; ddr.upleft[1] = ul[1];
    ddr.upright[0] = ul[0]; ddr.upright[1] = lr[1];
    ddr.loleft[0] = lr[0]; ddr.loleft[1] = ul[1];
    ddr.loright[0] = lr[0]; ddr.loright[1] = lr[1];
    ddr.nl = nl; ddr.ns = ns;
    c_putddr(outDEM,&ddr);
    
    printf("\n DEMCLIP successfully completed.\n");
    StopWatch();
    return(0);
}
/*--------------------------------------------------------------------------- 
  mapedge :
	Find maximum and minimum extents of an image in UTM projection 
	coordinates using input lat,lon coordinates.  Iteratively checks
	75 points along each border and keeps the minimum and maximum
	values found in both the x and y direction.  Finally, it truncates
	the minimum values and rounds the maximum values up to the nearest
        multiple of the projection distance, pdist.
 ---------------------------------------------------------------------------*/

int mapedge(double *gul,double *glr,double *sul,double *slr,struct DDR
dem_ddr)
{
   double inx,iny,outx,outy;
   double dx, dy;
   double pxmin = 100000000.0;
   double pxmax = -100000000.0;
   double pymin = 100000000.0;
   double pymax = -100000000.0;
   double inc = 75;
   double pdist;
   int i;
   forward_transform latLon2proj[100];
   int iflg=0;

   for_init(dem_ddr.proj_code,dem_ddr.zone_code,dem_ddr.proj_coef,
           dem_ddr.datum_code,NULL,NULL,&iflg,latLon2proj);
   
   pdist = dem_ddr.pdist_x;
   
   dx = (gul[1]-glr[1])/(inc-1);
   dy = (gul[0]-glr[0])/(inc-1);
   
   /* Check East */
   inx = glr[1]; iny = glr[0];
   for (i = 0; i < inc; i++, iny += dy)
     {
        latLon2proj[dem_ddr.proj_code](inx,iny,&outx,&outy);
        if (outx < pxmin) pxmin = outx; if (outx > pxmax) pxmax = outx;
        if (outy < pymin) pymin = outy; if (outy > pymax) pymax = outy;
     }
   
   /* Check West */
   inx = gul[1]; iny = glr[0];
   for (i = 0; i < inc; i++, iny += dy)
     {
        latLon2proj[dem_ddr.proj_code](inx,iny,&outx,&outy);
        if (outx < pxmin) pxmin = outx; if (outx > pxmax) pxmax = outx;
        if (outy < pymin) pymin = outy; if (outy > pymax) pymax = outy;
     }
   
   /* Check South */
   inx = glr[1]; iny = gul[0];
   for (i = 0; i < inc; i++, inx += dx)
     {
        latLon2proj[dem_ddr.proj_code](inx,iny,&outx,&outy);
        if (outx < pxmin) pxmin = outx; if (outx > pxmax) pxmax = outx;
        if (outy < pymin) pymin = outy; if (outy > pymax) pymax = outy;
     }
   

   /* Check North */
   inx = glr[1]; iny = glr[0];
   for (i = 0; i < inc; i++, inx += dx)
     {
        latLon2proj[dem_ddr.proj_code](inx,iny,&outx,&outy);
        if (outx < pxmin) pxmin = outx; if (outx > pxmax) pxmax = outx;
        if (outy < pymin) pymin = outy; if (outy > pymax) pymax = outy;
     }
   
   /* Round to nearest pixel size */
   iny = pymax - ((int)(pymax/pdist)*pdist);
   if (iny != 0) pymax += (pdist-iny);
   pymin -= pymin - ((int)(pymin/pdist)*pdist);
   pxmin -= pxmin - ((int)(pxmin/pdist)*pdist);
   inx = pxmax - ((int)(pxmax/pdist)*pdist);
   if (inx != 0.0) pxmax += (pdist-inx);
   
   sul[0] = pymax;  
   sul[1] = pxmin;
   slr[0] = pymin;
   slr[1] = pxmax;
        
   return(0);
}
     

