/******************************************************************************

    DESCRIPTION:  This program digests the ASF GPS ice motion product,
                  and draws a line from reference pt. A to pt. B

         SYNTAX:  gps_line ice_motion_file output_file 

         AUTHOR:  Mike Shindle
           DATE:  Aug. 1995
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
#include "gps_vector.h"

/* local function declaration */
void fwriteConst(void *,size_t,size_t,FILE *);

int main (argc, argv) 
int argc;
char **argv;
{
    struct gps_meta *meta;
    int i, j;
    int il;
    int options = 0;
    int nr, nc;
    char buf1[2048];
    char datfile[256], ldrfile[256];
    char imgfile[256], ddrfile[256];
    unsigned char **oimage;
    unsigned char zero = 0;
    double min_xa,max_xa,min_ya,max_ya;
    double min_xb,max_xb,min_yb,max_yb;
    double vector_res = 1.0;
    double gridspace;
    int pixel_size=100;
    int mag = 1;
    int nbands = 1;
    int linewidth = 3;
    LATLON user;
    FILE *fpldr, *fpdat,  *fpout;
    extern char *optarg;
    extern int optind;


    /* look for options */
    while ((i = getopt(argc,argv,"b:m:r:v:w:gas")) != EOF)
      switch (i) {
	case 'r':
	  pixel_size = atol(optarg);
	  break;
        case 'm':
	  mag = atol(optarg);
	  break;
	case 'v':
	  vector_res = atof(optarg);
	  break;
	case 'g':
	  options |= DRAWGRID;
	  break;
        case 'a':
	  options |= SUBTRACT_META_AVG;
	  break;
        case 'b':
	  nbands = atoi(optarg);
	  break;
        case 'w':
	  linewidth = atoi(optarg);
	  break;
	case 's':
	  options |= SUBTRACT_USER_AVG;
	  break;
	default:
	  print_usage();
	  break;
      }
    if (options & SUBTRACT_USER_AVG) {
      if (argc - optind != 4) print_usage();
      user.lat = atof(argv[optind+2]);
      user.lon = atof(argv[optind+3]);
    } else {
      if (argc - optind != 2) print_usage();
      user.lat = user.lon = 0.0;
    }

    /* Change path names to appropriate strings, read the 
       input data file. */
    strcpy(ldrfile, argv[optind]);
    strcat(ldrfile, ".ldr");
    strcpy(datfile, argv[optind]);
    strcat(datfile, ".dat");
    strcpy(imgfile, argv[optind+1]);
    strcat(imgfile, ".img");
    strcpy(ddrfile, argv[optind+1]);
    strcat(ddrfile, ".ddr");

    if ((fpldr = fopen (ldrfile, "r")) == NULL) {
	fprintf (stderr, "***** Error Message *****\n");
        fprintf (stderr, "Can't open file %s.\n", ldrfile);
        fprintf (stderr, "Could not read input data header record.\n");
	exit (1);
    }
    /* open the data file */
    if ((fpdat = fopen (datfile, "r")) == NULL) {
        fprintf (stderr, "Can't open file %s.\n", datfile);
        exit (1);
    }
    /* Open output ice motion binary file */
    if ((fpout = fopen (imgfile, "w")) == NULL) {
        fprintf (stderr, "Can't open file %s.\n", imgfile);
        exit (1);
    }

    /*
      Obtain information from the GPS leader file. You could also
      get this info from the trailer file.
    */
    printf ("\nInput Ice Motion File is %s\n", argv[optind]);
    
    /* skip the 720 bytes of ldr header */
    fseek (fpldr, 720L, 0);
    if ((il = fread (buf1, sizeof(struct gps_meta), 1, fpldr)) <= 0) {
        fprintf (stderr, "end of file %s reached.\n", ldrfile);
        exit (1);
    }
    
    /* Output the information about the ice motion data to be processed */
    meta = (struct gps_meta *) buf1;
    print_data_info(meta);

    /* 
      Read in the Max and Min grid coords in both image A and B
      and the four corners of the overlapped area in image A and B.
    */
    min_xa = s_to_dbl(meta->min_x, 10);
    max_xa = s_to_dbl(meta->max_x, 10);
    min_ya = s_to_dbl(meta->min_y, 10);
    max_ya = s_to_dbl(meta->max_y, 10);
        
    min_xb = s_to_dbl(meta->bmin_x, 10);
    max_xb = s_to_dbl(meta->bmax_x, 10);
    min_yb = s_to_dbl(meta->bmin_y, 10);
    max_yb = s_to_dbl(meta->bmax_y, 10);
    
    /* define the size of the output array */
    nr = (int)((max_yb - min_yb + CORRECTION) * 1000) / pixel_size + 1;    
    nc = (int)((max_xb - min_xb + CORRECTION) * 1000) / pixel_size + 1; 
    
    /* Create DDR file */
    create_gps_ddr(ddrfile,meta,nr,nc,pixel_size,nbands);

    /* allocate space for the output array */
    if ((oimage = (unsigned char **) 
		  alloc2d_1(nr, nc, sizeof(unsigned char))) == NULL) {
        fprintf (stderr, "Can't allocate space for output image.\n");
        exit (1);
    }

    /* zerout oimage */
    for (i = 0; i < nr; i++) 
        for (j = 0; j < nc; j++)
            oimage[i][j] = 0;

    /* read in the value of grid space */
    gridspace = 0.0;
    gridspace = s_to_dbl(meta->gridspace, 6);
    if (gridspace <= 0.0) gridspace = 5.0;
    printf("Gridspace = %.2f\n",gridspace);
 
    /* 
      Unless grid deformation is specified, draw vector 
      image. Otherwise draw grid pattern.
    */
    if (options & DRAWGRID)
      grid(oimage,fpdat,meta,gridspace,nr,nc,mag,
	   pixel_size,vector_res,options,user);
    else
      vector(oimage,fpdat,meta,gridspace,nr,nc,mag,
	     pixel_size,vector_res,options,user);

    /* 
       Image must be flipped about the horizontal axis. Pixel coordinates
       are determined from Lower Left (min x & min y) while all tools
       expect Upper Left to be in first pixel-grid spot.
    
       output the deformed grid image with the image flipped vertically
    */
    printf("\n");
    for (j=0; j<nbands; j++) {
       printf("Writing out band: %2d\r",j);
       /* write out data for first two bands only */ 
       if (j < 2)
    	 for (i = nr-1; i >= 0; i--) 
          fwrite(oimage[i], sizeof(unsigned char), nc, fpout);
       else
	  fwriteConst(&zero,sizeof(unsigned char),nr*nc,fpout);
    }

    /* free & close everything up */
    free(oimage);
    fclose(fpldr);
    fclose(fpdat);
    fclose(fpout);
    printf("Program ends successfully\n\n");
    return 0;
}

/* write out a const. value to file fp */
void fwriteConst(data,size,nitems,fp)
void *data;
size_t size;
size_t nitems;
FILE *fp;
{
  int i;

  for (i=0; i<nitems; i++)
    fwrite(data,size,1,fp);

  return;
}

/* Print the usage of the program */
void print_usage() {
  fprintf(stderr,"USAGE:\n");
  fprintf(stderr,"       %s %s %s\n                  %s %s\n\n",
	  "gps_vector",
	  "[-a] [-g]",
	  "[-b nband] [-w linewidth] [-m mag] [-v vector]",
	  "[-r resample]",
	  "ice_motion_file out_file");
  fprintf(stderr,"       %s %s %s\n                  %s %s\n\n",
	  "gps_vector",
	  "-s [-a] [-g]",
	  "[-b nband] [-w linewidth] [-m mag] [-v vector]",
	  "[-r resample]",
	  "ice_motion_file out_file lat lon");
  fprintf(stderr,"\t-s\tSubtract vector that has base closest to lat,lon\n");
  fprintf(stderr,"\t-a\tSubtract avg. vector displacement\n");
  fprintf(stderr,"\t-g\tDraw grid instead of vectors\n");
  fprintf(stderr,"\t-b\toutput an nband image. Default=1.\n");
  fprintf(stderr,"\t-w\tdraw lines with width linewidth. Default=3.\n");
  fprintf(stderr,"\t-m\tIncrease vector length by mag\n");
  fprintf(stderr,"\t-v\tDraw every <vector> vector\n");
  fprintf(stderr,"\t-r\tResample output to resample size\n");
  fprintf(stderr,"\nVersion %.2f, ASF STEP Tools\n",VERSION);
  exit (1);
}
