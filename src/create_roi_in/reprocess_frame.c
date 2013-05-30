/******************************************************************************
NAME:  reprocess_frame

SYNOPSIS:  Recreates a product using a new doppler

		-v	use state vectors instead of TLEs
		-c 	use the clock drift to offset times

DESCRIPTION:

EXTERNAL ASSOCIATES:
	NAME:               	USAGE:
   	---------------------------------------------------------------
 	create_roi_in		create input configuration file for ROI
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
#include "seasat.h"

#define MAX_NODE	3600	// we will never have southern hemisphere SEASAT data!
#define PATCH_SIZE	8132
#define NUM_PATCHES	3
#define HALF_ESA_FRAME  12468	// this should be about correctt, as follows:
				//   Swath_Length = 4.01 m/line * 24936 lines = 99993.36 meters. 

void process_node_dop(char *basefile, int node, int dop_shift);
int get_line_for_node(meta_parameters *meta, int node, int nl);
int get_node_for_line(meta_parameters *meta, int line, int nl);
void give_usage(char *argv[], int argc);
int read_roi_infile(char *infile);		// read the ROI input file values
int get_values(FILE *fp,SEASAT_header_ext *s);	// read values from a header file

/* Subroutines to read values from files - used by read_roi_infile */
int get_string_val(FILE *fp, char *str);
int get_3double_vals(FILE *fp, double *num1, double *num2, double *num3);
int get_double_val(FILE *fp, double *num);
int get_2int_vals(FILE *fp,int *num1,int *num2);
int get_int_val(FILE *fp, int *num);

/* Global variables filled in read_roi_infile and used in main */
char 	datfilename[256];	// Original dat file
double 	dop1,dop2,dop3;		// doppler coefficients
double 	prf;	      		// pulse repitition frequency

/* Switches */
int USE_TLES = 1;
int USE_CLOCK_DRIFT = 0;

main(int argc, char *argv[])
{
  char *basefile, tmpfile[256], productfile[256], roifile[256];
  int node, start_line;
  char cmd[256];
  char metafile[256];
  int err=0;
  int dop_shift=0;
  int val;
  int c;
  
  if (argc < 2 || argc > 4) { give_usage(argv,argc); exit(1); }
  
  while ((c=getopt(argc,argv,"vc")) != -1)
    switch(c) {
      case 'v':
        USE_TLES = 0;
	break;
      case 'c':
        USE_CLOCK_DRIFT = 1;
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
  strcpy(metafile,productfile);
  strcat(metafile,".meta");

  /* get necessary values from product files */
  read_roi_infile(roifile);
  if (dop2 > 0.0) dop_shift=1;
  else dop_shift -=1;
  basefile = get_basename(datfilename);
 
  printf("================================================================================\n");
  printf("%s: RECREATING SEASAT PRODUCT %s with dop %i\n",argv[0],productfile,dop_shift);
  printf("================================================================================\n");

  meta_parameters *meta = meta_read(metafile);
  node = meta->general->frame;
  process_node_dop(basefile,node,dop_shift);

  printf("================================================================================\n");
  printf("%s: FINISHED\n",argv[0]);
  printf("================================================================================\n");

  exit(0);
}

/* Call create_roi_in, roi, and roi2img for the given basefile and node */
void process_node_dop(char *basefile, int node, int dop_shift)
{
  char cmd[256],tmpfile[256], options[256];
  int  err;

  sprintf(options,"-E %i -d %i",node,dop_shift);
  if (USE_TLES == 0) strcat(options," -v");
  if (USE_CLOCK_DRIFT == 1) strcat(options," -c");
  
  sprintf(cmd,"create_roi_in %s %s\n",options,basefile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
	
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

int read_roi_infile(char *infile)
{
  FILE *fp = fopen(infile,"r");
  
  if (fp==NULL) {printf("ERROR: can't open %s ROI input file\n",infile); return(1); }
  
  char ctmp[256];
  int  itmp;
  double dtmp;
  int  i;

  get_string_val(fp,datfilename);		// First input data file				
  get_string_val(fp,ctmp);			// Second input data file  				     
  get_string_val(fp,ctmp);	        	// Output data file					
  get_string_val(fp,ctmp);			// Output amplitudes file				
  get_string_val(fp,ctmp);			// 8lk output file					
  get_int_val(fp,&itmp);	 		// debug flag						
  get_int_val(fp,&itmp);			// How many input bytes per line files 1 and 2		   
  get_int_val(fp,&itmp);			// How many good bytes per line, including header	
  get_int_val(fp,&itmp); 	   		// First line to read  (start at 0)  			
  get_int_val(fp,&itmp);			// Enter # of range input patches			
  get_int_val(fp,&itmp); 			// First sample pair to use (start at zero)		
  get_int_val(fp,&itmp);			// Azimuth Patch Size (Power of 2)			
  get_int_val(fp,&itmp);			// Number of valid points in azimuth			
  get_string_val(fp,ctmp);			// Deskew the image					
  get_int_val(fp,&itmp);				
  for (i=0;i<itmp;i++) 
      get_double_val(fp,&dtmp);			// Caltone % of sample rate				
  get_2int_vals(fp,&itmp,&itmp);		// Start range bin, number of range bins to process	
  get_int_val(fp,&itmp);			// Delta azimuth, range pixels for second file  	
  get_3double_vals(fp,&dop1,&dop2,&dop3);	// Image 1 Doppler centroid quad coefs (Hz/prf) 	
  get_double_val(fp,&dtmp);			// Image 2 Doppler centroid quad coefs (Hz/prf) 	
  get_int_val(fp,&itmp);			// 1 = use file 1 doppler, 2 = file 2, 3 = avg  	
  get_double_val(fp,&dtmp);			// Earth Radius (m)					
  get_double_val(fp,&dtmp);			// Body Fixed S/C velocities 1,2 (m/s)  		
  get_double_val(fp,&dtmp);			// Spacecraft height 1,2 (m)				
  get_double_val(fp,&dtmp);			// Planet GM						
  get_double_val(fp,&dtmp);			// Left, Right or Unknown Pointing			
  get_double_val(fp,&dtmp);			// SCH Velocity Vector 1				
  get_double_val(fp,&dtmp);			// SCH Velocity Vector 2				
  get_double_val(fp,&dtmp);			// SCH Acceleration Vector 1				
  get_double_val(fp,&dtmp);			// SCH Acceleration Vector 2				
  get_double_val(fp,&dtmp);			// Range of first sample in raw data file 1,2 (m)	
  get_double_val(fp,&prf);			// PRF 1,2 (pps)					
  get_double_val(fp,&dtmp);			// i/q means, i1,q1, i2,q2				
  get_string_val(fp,ctmp);			// Flip i/q (y/n)					
  get_double_val(fp,&dtmp);			// Desired azimuth resolution (m)			
  get_double_val(fp,&dtmp);			// Number of azimuth looks				
  get_double_val(fp,&dtmp);			// Range sampling rate (Hz)				
  get_double_val(fp,&dtmp);			// Chirp Slope (Hz/s)					
  get_double_val(fp,&dtmp);			// Pulse Duration (s)					
  get_int_val(fp,&itmp);			// Chirp extension points				
  get_string_val(fp,ctmp);			// Secondary range migration correction (y/n)		
  get_double_val(fp,&dtmp);			// Radar Wavelength (m) 				
  get_int_val(fp,&itmp);			// Range Spectral Weighting (1.=none, 0.54=Hamming)	
  get_double_val(fp,&dtmp);			// Fraction of range bandwidth to remove		
  get_double_val(fp,&dtmp);			// linear resampling coefs:  sloper, intr, slopea, inta 
  get_double_val(fp,&dtmp);			// linear resampling deltas: dsloper, dintr, dslopea, dinta
  get_string_val(fp,ctmp);			// AGC file						
  get_string_val(fp,ctmp);			// DWP file						

  return(0);
}

int get_values(FILE *fp,SEASAT_header_ext *s)
{
  int val;
  if (s==NULL) {printf("empty pointer passed to get_values\n"); exit(1);}
  val = fscanf(fp,"%i %li %i %i %i %li %i %i %i %i %i %i %i %i %i %i %i %i %i %i\n",
    &(s->major_cnt),&(s->major_sync_loc),&(s->station_code),&(s->lsd_year),
    &(s->day_of_year),&(s->msec),&(s->clock_drift),&(s->no_scan_indicator_bit),
    &(s->bits_per_sample),&(s->mfr_lock_bit),&(s->prf_rate_code),&(s->delay),
    &(s->scu_bit),&(s->sdf_bit),&(s->adc_bit),&(s->time_gate_bit),&(s->local_prf_bit),
    &(s->auto_prf_bit),&(s->prf_lock_bit),&(s->local_delay_bit));
  return(val);
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

int get_2int_vals(FILE *fp,int *num1,int *num2)
{
  char tmp[256];
  fgets(tmp,255,fp);
  // printf("read 2 ints %s: ",tmp);
  sscanf(tmp,"%i %i",num1,num2);
  // printf("got values %i %i\n",*num1,*num2);
  return(1);
}

int get_double_val(FILE *fp, double *num)
{
  char tmp[256];
  fgets(tmp,255,fp);
  // printf("read double %s: ",tmp);
  sscanf(tmp,"%lf",num);
  // printf("got value %lf\n",*num);
  return(1);
}

int get_3double_vals(FILE *fp, double *num1, double *num2, double *num3)
{
  char tmp[256];
  fgets(tmp,255,fp);
  // printf("read 3 doubles %s: ",tmp);
  sscanf(tmp,"%lf %lf %lf",num1,num2,num3);
  // printf("got values %lf %lf %lf\n",*num1,*num2,*num3);
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

void give_usage(char *argv[], int argc)
{
  printf("Usage: %s [-v][-c] <product base name>\n",argv[0]);
  printf("\t-v            \tUse state vectors instead of TLEs\n");
  printf("\t-c            \tApply the clock drift to image timing\n");

}
