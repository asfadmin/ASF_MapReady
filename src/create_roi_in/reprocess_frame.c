/******************************************************************************
NAME:  reprocess_frame

SYNOPSIS:  Recreates a product using a new doppler

		-t	use TLEs instead of state vectors

DESCRIPTION:

EXTERNAL ASSOCIATES:
	NAME:               	USAGE:
   	---------------------------------------------------------------
	roi			Repeat Orbit Interferometry correlator
	roi2img			Turns ROI outputs into Seasat products

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/13     T. Logan     process a Seasat swath into frames
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <asf_meta.h>
#include <asf_license.h>
#include "seasat.h"

#define MAX_NODE	3600	// we will never have southern hemisphere SEASAT data!
#define PATCH_SIZE	8132
#define NUM_PATCHES	3
#define HALF_ESA_FRAME  12468	// this should be about correctt, as follows:
				//   Swath_Length = 4.01 m/line * 24936 lines = 99993.36 meters. 

void process_node(char *basefile);
void give_usage(char *argv[], int argc);
int fix_roi_infile(char *infile);

/* Subroutines to read values from files - used by read_roi_infile */
int get_string_val(FILE *fp, char *str);
int get_int_val(FILE *fp, int *num);
void roi_put_string(FILE *roi_file,char *value,char *comment);
void roi_put3_dop(FILE *roi_file,double fd, double fdd, double fddd,char *comment);

/* Global variables filled in read_roi_infile and used in main */
char 	datfilename[256];	// Original dat file
char *basefile; 
int orbit, node;
double 	dop1,dop2,dop3;		// doppler coefficients
double 	prf;	      		// pulse repitition frequency

/* Switches */
int USE_TLES = 0;
int USE_CLOCK_DRIFT = 1;

main(int argc, char *argv[])
{
  char tmpfile[256], productfile[256], roifile[256];
  int start_line;
  char cmd[256];
  //char metafile[256];
  int err=0;
  int val;
  int c;
 
  asfSplashScreen(argc, argv);
 
  if (argc < 2 || argc > 3) { give_usage(argv,argc); exit(1); }
  
  while ((c=getopt(argc,argv,"t")) != -1)
    switch(c) {
      case 't':
        USE_TLES = 1;
	break;
      case '?':
        printf("Unknown option %s\n",optarg);
	return(1);
      default:
        give_usage(argv,argc);
	exit(1);
    } 
  
  strcpy(productfile,argv[optind]);
  strcpy(roifile,productfile);
  strcat(roifile,".roi.in");
  //strcpy(metafile,productfile);
  //strcat(metafile,".meta");

  printf("================================================================================\n");
  printf("%s: RECREATING SEASAT PRODUCT %s\n",argv[0],productfile);
  printf("================================================================================\n");

  /* get the node number to process */
  //meta_parameters *meta = meta_read(metafile);
  //node = meta->general->frame;

  sscanf(roifile,"SS_%d_STD_F%d.roi.in", &orbit, &node);
  printf("Found orbit %d and node %d\n", orbit, node);

  /* change the doppler in the ROI .in file */
  fix_roi_infile(roifile);
 
  printf("================================================================================\n");
  printf("%s: RECREATING SEASAT PRODUCT %s with doppler %lf\n",argv[0],productfile,dop1);
  printf("================================================================================\n");

  process_node(basefile);

  printf("================================================================================\n");
  printf("%s: FINISHED\n",argv[0]);
  printf("================================================================================\n");

  exit(0);
}

/* Call roi and roi2img for the given basefile and node */
void process_node(char *basefile)
{
  char cmd[256],tmpfile[256], oldfile[256], options[256];
  int  err;

  sprintf(tmpfile,"%s_node%.4i.roi.in",basefile,node);
  sprintf(cmd,"roi < %s\n",tmpfile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from ROI\n"); exit(1);}

  sprintf(options,"-E %i",node);
  if (USE_TLES == 0) strcat(options," -v");
  if (USE_CLOCK_DRIFT == 1) strcat(options," -c");

  sprintf(tmpfile,"%s_node%.4i",basefile,node);
  sprintf(cmd,"roi2img %s %s\n",options,tmpfile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from roi2img\n"); exit(1);}
}

int fix_roi_infile(char *infile)
{
  char tmp[256], tmpfile[256];
  FILE *fproi = fopen(infile,"r");
  if (fproi==NULL) {printf("ERROR: can't open %s ROI input file\n",infile); return(1); }
  else {printf("Opened roi file\n");}
  
  get_string_val(fproi,datfilename);		// Input data file name
  printf("Original data file name %s\n",datfilename);
  basefile = get_basename(datfilename);
  
  sprintf(tmpfile,"%s_node%.4i.roi.in",basefile,node);
  FILE *fpout = fopen(tmpfile,"w");
  roi_put_string(fpout,datfilename,"First input data file");

  while (fgets(tmp,255,fproi)!=NULL) 
    {
      if (strstr(tmp,"Image 1 Doppler centroid quad coefs")!=NULL) {
        sscanf(tmp,"%lf %lf %lf",&dop1,&dop2,&dop3);
        if (dop2 > 0.0) dop1+=1;
        else dop1-=1;
        roi_put3_dop(fpout,dop1,dop2,dop3,"Image 1 Doppler centroid quad coefs (Hz/prf)");
      } else if (strstr(tmp,"Image 2 Doppler centroid quad coefs")!=NULL) {
        sscanf(tmp,"%lf %lf %lf",&dop1,&dop2,&dop3);
        if (dop2 > 0.0) dop1+=1;
        else dop1-=1;
        roi_put3_dop(fpout,dop1,dop2,dop3,"Image 2 Doppler centroid quad coefs (Hz/prf)");
      } else {
        fputs(tmp,fpout);
      }
    }
    
  fclose(fproi);
  fclose(fpout);
}

int get_int_val(FILE *fp, int *num)
{
  char tmp[256];
  fgets(tmp,255,fp);
  // printf("read int %s: ",tmp);
  sscanf(tmp,"%i",num);
  // printf("got value %i\n",*num);
  return(1);
}

int get_string_val(FILE *fp, char *str)
{
  char tmp[256];
  fgets(tmp,255,fp);
  sscanf(tmp,"%s",str);
  // printf("read string %s\n",str);
  return(1);
}

void roi_put_string(FILE *roi_file,char *value,char *comment)
{
  int ii;
  char line[1024];/*The line to be written to the file.*/
  strcpy(line,"");

/*Append parameter and value.*/
  strcat(line,value);/*Append parameter value.*/

/* Append comment if applicable */
  if (comment!=NULL)
  {
  /*Space over to the comment section.*/
    ii=strlen(line);
    while (ii < 64) /*Fill spaces out to about column 50.*/
      line[ii++]=' ';
    line[ii++]='\0';        /*Append trailing NULL.*/

  /*Add the comment.*/
    strcat(line," ! ");     /*Signal beginning of comment.*/
    strcat(line,comment);   /*Append comment.*/
  }

/*Finally, write the line to the file.*/
  int n = fprintf(roi_file,"%s\n",line);
  if (n < 0) {
    if (errno == ENOMEM)
      asfPrintError("roi_put_string: "
                    "Insufficient storage space is available\n");
    else
      asfPrintError("fprint error: %d\n", strerror(errno));
  }
}

void roi_put3_dop(FILE *roi_file,double fd, double fdd, double fddd,char *comment)
{
  char param[64];
  sprintf(param,"%lf %.8lf %.12lf",fd,fdd,fddd);
  roi_put_string(roi_file,param,comment);
}

void give_usage(char *argv[], int argc)
{
  printf("Usage: %s [-t] <product base name>\n",argv[0]);
  printf("\t-t            \tUse TLEs instead of State Vectors\n");
}
