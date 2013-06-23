/******************************************************************************
NAME: create_roi_in - creates a ROI.in file from seasat HDR and state vectors
		      previously created by the ASF SEASAT PREP code.

SYNOPSIS: create_roi_in [-s <start_line> -e <end_line>][-d dop][-f start_line][-v][-c][-E ESA_Node] <infile_base_name> 

DESCRIPTION:

	<infile> 			base name, assume that <infile>.dat and <infile>.hdr exist.
	[-s <start_line> -e <end_line>]	optional start/end line to process
	[-d #]   			offset to calculated doppler, default zero
	[-v]				use state vectors instead of TLEs.
	[-c]				apply clock drift to image timing
	[-f start_line]			make an ESA SIZED frame starting from start_line
	[-E ESA_Node]			make an ESA sized frame centered at the specified node


	- Read hdr file to get the start time and number of lines in the data segment
		- calculate the number of patches to process
	- Run dop.f (made into a subroutine) on the dat file
	- Fit output of doppler estimator with a 2nd order function
	- Find the correct state vector in the FIXED BODY state vectors
		- calculate spacecraft velocity
	- Run the state vector through get_peg_info (made into a subroutine)
	  to get the SCH Vel, SCH Acc, local earth radius, and spacecraft height
	- Create the <infile>.roi.in output file


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    4/12   T. Logan     Seasat Proof of Concept Project - ASF
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

The order of outputs in the roi,in file is:

DESCRIPTION						     VALUE
--------------------------------------------------------     -------------------------------------------------
First input data file					     <infile>.dat
Second input data file  				     /dev/null
Output data file					     <infile>.slc
Output amplitudes file  				     /dev/null
8lk output file 					     8lk
debug flag						     0
How many input bytes per line files 1 and 2		     13680 13680
How many good bytes per line, including header  	     13680 13680
First line to read  (start at 0)			     1
Enter # of range input patches  			     {Get nl from hdr values; patches = nl/11600}
First sample pair to use (start at zero)		     0
Azimuth Patch Size (Power of 2) 			     16384
Number of valid points in azimuth			     11600
Deskew the image					     n
Caltone % of sample rate				     0.25 0.25701904296875
Start range bin, number of range bins to process	     1 6840
Delta azimuth, range pixels for second file		     0 0
Image 1 Doppler centroid quad coefs (Hz/prf)		     {calculated from dop_est - 3 parameters}
Image 2 Doppler centroid quad coefs (Hz/prf)		     {copy above 3 values}
1 = use file 1 doppler, 2 = file 2, 3 = avg		     1
Earth Radius (m)					     {calculated from get_peg_info}
Body Fixed S/C velocities 1,2 (m/s)			     {calculate from EBEF state vector - 2 parameters}
Spacecraft height 1,2 (m)				     {calculated from get_peg_info - 2 parameters}
Planet GM						     0
Left, Right or Unknown Pointing 			     Right
SCH Velocity Vector 1					     {calculated from get_peg_info - 3 parameters}
SCH Velocity Vector 2					     {copy above 3 values}
SCH Acceleration Vector 1				     {calculated from get_pef_info - 3 parameters}
SCH Acceleration Vector 2				     {copy above 3 values}
Range of first sample in raw data file 1,2 (m)  	     {calculate from the hdr infomation - 2 values}
PRF 1,2 (pps)						     {calculate from the hdr information - 2 values}
i/q means, i1,q1, i2,q2 				     15.5 15.5 15.5 15.5
Flip i/q (y/n)  					     s
Desired azimuth resolution (m)  			     5  (what should this be???)
Number of azimuth looks 				     4  (what should this be???)
Range sampling rate (Hz)				     22765000
Chirp Slope (Hz/s)					     5.62130178e11
Pulse Duration (s)					     33.8e-6
Chirp extension points  				     0
Secondary range migration correction (y/n)		     n
Radar Wavelength (m)					     0.235
Range Spectral Weighting (1.=none, 0.54=Hamming)	     1.0
Fraction of range bandwidth to remove			     0 0
linear resampling coefs:  sloper, intr, slopea, inta	     0 0 0 0
linear resampling deltas: dsloper, dintr, dslopea, dinta     0 0 0 0
AGC file						     /dev/null
DWP file						     /dev/null or DWP file


ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <asf_meta.h>
#include <asf_license.h>
#include "seasat.h"

int get_values(FILE *fp,SEASAT_header_ext *s);
int get_int_value(FILE *fp, const char token[], int *val, int from);
void estdop(FILE *fp, int sl, int nl, double *fd, double *fdd, double *fddd, double *iqmean);
void get_peg_info(double start_time, int nl, int prf, char *vecfile,
                  double *schvel, double *schacc, double *height, double *earthrad);
void spectra(FILE *fp,int sl, int nl,double iqmean,int *ocnt,double *ocal);
void give_usage(char *argv[], int argc);
int get_line_for_node(meta_parameters *meta, int node, int nl);
int get_median(int *hist, int size);

void roi_put_string(FILE *roi_file,char *value,char *comment);
void roi_put_double(FILE *roi_file,double value,char *comment);
void roi_put3_dop(FILE *roi_file,double fd, double fdd, double fddd,char *comment);
void roi_put2_double(FILE *roi_file,double val1, double val2,char *comment);
void roi_put3_double(FILE *roi_file,double val1, double val2, double val3,char *comment);
void roi_put_int(FILE *roi_file,int value,char *comment);
void roi_put2_int(FILE *roi_file,int val1, int val2,char *comment);
void roi_put_char(FILE *roi_file,char value,char *comment);
void roi_put_double_lf(FILE *roi_file,double value,int decimals,char *comment);
void roi_put4_double(FILE *roi_file,double val1, double val2, double val3, double val4, char *comment);

#define GOOD_SAMPLES  5300
#define GOOD_LINES    11800    
#define MAX_DWP_SHIFTS   20
#define DIGITIZATION_SHIFT 432
#define MAX_CALTONES  20

double r_awgs84 = 6378137.0;
double r_e2wgs84 = 0.00669437999015;
double C = 299792458.0;

main(int argc, char *argv[])
{
  FILE *fpdat, *fphdr, *fproi, *fpstarthdr;
  char infile[256], outfile[256], hdrfile[256], starthdrfile[256], dwpfile[256], metafile[256];
  char basefile[256];
  char vecfile[256];
  int err, nl, patches, prf;
  int from;
  double x,y,z,xdot,ydot,zdot,vel;
  double t;
  double srf, time_length;
  double height, earthrad;
  double schvel[3], schacc[3];
  double fd, fdd, fddd;
  double iqmean;
  SEASAT_header_ext *hdr;
  SEASAT_header_ext *hdr1;
  meta_parameters *meta;

  double   start_sec,  current_sec, end_sec;
  double   time_from_start;
  int      start_date, current_date, end_date;
  int 	   start_year, current_year, end_year;

  int tmp;
  double dtmp;
  double line_time_est;
  
  long int dwp_val[MAX_DWP_SHIFTS];
  long int dwp_line[MAX_DWP_SHIFTS];
  int      dwp_cnt, dwp_flag, dwp_min=65;
  
  int    ncaltones;
  double caltones[MAX_CALTONES];

  julian_date s_date;
  hms_time    s_time;
  
  int val, which;
  int i, start_line, end_line;
  int c;
  int dop_shift;
  int node = -1;
  int ESA_FRAME = 0;
  int MAKE_FRAME = 0;
  int USE_TLES = 1;
  int USE_CLOCK_DRIFT = 0;
  int actual_lines, actual_samps;
  
  int clock_drift_hist[MAX_CLOCK_DRIFT];
  int clock_drift_median;
  double clock_shift;

  asfSplashScreen(argc, argv);

  if (argc < 2 || argc > 9) { give_usage(argv,argc); exit(1); }
  
  opterr = 0;
  start_line = 1;
  end_line = -99;
  dop_shift = 0;  
  clock_shift = 0.0;
  
  while ((c=getopt(argc,argv,"s:e:d:f:E:vc")) != -1)
    switch(c) {
      case 's':
        if (start_line != 1) {
	  give_usage(argv,argc);
	  printf("\nERROR: Can only use one of -s or -f options; aborting\n");
	  exit(1);
	}
      	start_line = atoi(optarg);
	break;
      case 'e':
        if (end_line != -99) {
	  give_usage(argv,argc);
	  printf("\nERROR: Can only use one of -e or -f options; aborting\n");
	  exit(1);
	}
        end_line = atoi(optarg);
	break;
      case 'd':
        dop_shift = atoi(optarg);
	break;
      case 'f':
        if (start_line != 1 || end_line != -99 || ESA_FRAME == 1) {
	  give_usage(argv,argc);
	  printf("\nERROR: Can only use one of -s/-e or -f options; aborting\n");
	  exit(1);
	}
	start_line = atoi(optarg);
	MAKE_FRAME = 1;
	break;
      case 'E':
        if (start_line != 1 || end_line != -99 || MAKE_FRAME == 1) {
	  give_usage(argv,argc);
	  printf("\nERROR: Can only use one of -E, -s/-e, or -f options; aborting\n");
	  exit(1);
	}
	node = atoi(optarg);
	ESA_FRAME = 1;
	break;	
      case 'v':
        USE_TLES = 0;
	break;
      case 'c':
        USE_CLOCK_DRIFT = 1;
        for (i=0; i<MAX_CLOCK_DRIFT; i++) clock_drift_hist[i] = 0;
	break;
      case '?':
        printf("Unknown option %s\n",optarg);
	return(1);
      default:
        give_usage(argv,argc);
	exit(1);
    } 

  strcpy(basefile,argv[optind]);
  strcpy(infile,basefile); strcat(infile,".dat");
  strcpy(hdrfile,basefile); strcat(hdrfile,".hdr");
  
  hdr = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));
  hdr1 = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));  
  
  printf("\n============================================================================\n");
  printf(" CREATING ROI.IN FILE FROM DATA %s\n",hdrfile);
  printf("============================================================================\n");

  if (ESA_FRAME==1) {
    /* read the swath meta file */
    strcat(strcpy(metafile,basefile),".meta");
    meta = meta_read(metafile);

    /* get total lines in this file */  
    if ((fphdr=fopen(hdrfile,"r"))==NULL) {printf("Error opening input file %s\n",tmpfile); exit(1);}
    val = get_values(fphdr, hdr); nl = 0;

    while (val==20) { 
      nl++; 
      val = get_values(fphdr, hdr);   
    }  
    fclose(fphdr);
  }

  if ((fphdr=fopen(hdrfile,"r"))==NULL) {printf("Error opening input file %s\n",hdrfile); exit(1);}

  if (MAKE_FRAME==1) {
    printf("Using ESA frame sizes, starting processing at line %i\n",start_line);
    actual_lines = 8312;
    actual_samps = 6840;
  } else if (ESA_FRAME==1) {
    printf("Trying to find start_line for node %i\n",node);
    start_line = get_line_for_node(meta,node,nl);
    printf("Creating ESA frame at node %i, starting processing at line %i\n",node,start_line);
    actual_lines = 8312;
    actual_samps = 6840;
  } else {
    printf("Processing from Line %i to ",start_line);
    if (end_line == -99) printf("end of file\n");
    else printf("%i\n",end_line);
    actual_lines = GOOD_LINES;
    actual_samps = GOOD_SAMPLES;
  }
  if (dop_shift != 0) { printf("Using %i doppler shift\n",dop_shift); }

  for (i=0; i<MAX_DWP_SHIFTS; i++) {
    dwp_val[i] = 0;
    dwp_line[i] = 0;
  }

  prf = 1647;

  /* set start year, day, second 
  -------------------------------------------------*/
  val = get_values(fphdr, hdr);
  if (val!=20) {printf("ERROR: unable to read from header file\n"); exit(1);}
  
  s_date.year = 1970 + hdr->lsd_year;
  s_date.jd   = hdr->day_of_year;
  dtmp = (double) hdr->msec / 1000.0;
  date_sec2hms(dtmp,&s_time);
  
  start_year = 1970 + hdr->lsd_year;
  start_date = hdr->day_of_year;
  start_sec  = (double) hdr->msec / 1000.0;
  
  /* seek to the start line the user requested
   ------------------------------------------*/
  for (i=1; i<start_line; i++) {
    val = get_values(fphdr, hdr);
    if (val!=20) {printf("ERROR: unable to read to specified start line in header file\n"); exit(1);}
  }
  current_year = 1970 + hdr->lsd_year;
  current_date = hdr->day_of_year;
  current_sec  = (double) hdr->msec / 1000.0;
  
  dwp_val[0]  = hdr->delay;
  dwp_line[0] = 0;
  dwp_cnt = 1;
  
  /* seek to the last line the user requested or else the end of file
   -------------------------------------------------------------------*/
  if (end_line == -99 && ESA_FRAME == 0 && MAKE_FRAME == 0) {  /* read to the end of the file */
    which=0; nl = start_line-1;
    while (val==20) {
      nl++;
      if (which==0) { 
        if (USE_CLOCK_DRIFT==1) clock_drift_hist[hdr1->clock_drift]++;
        val=get_values(fphdr,hdr1); 
	which=1; 
	if (dwp_val[dwp_cnt-1] != hdr1->delay) {
          dwp_val[dwp_cnt] = hdr1->delay;
          dwp_line[dwp_cnt] = nl;
          dwp_cnt++;
        }
      } else { 
        if (USE_CLOCK_DRIFT==1) clock_drift_hist[hdr->clock_drift]++;
        val=get_values(fphdr,hdr);  
	which=0; 
	if (dwp_val[dwp_cnt-1] != hdr->delay) {
          dwp_val[dwp_cnt] = hdr->delay;
          dwp_line[dwp_cnt] = nl;
          dwp_cnt++;
        }
      }
    }
    if (which==1) { /* we just read into hdr1, so hdr is good  */
      end_year = 1970+ hdr->lsd_year;
      end_date = hdr->day_of_year;
      end_sec  = (double) hdr->msec / 1000.0;
    } else {       /* we just read into hdr, so hdr1 is good */
      end_year = 1970+ hdr1->lsd_year;
      end_date = hdr1->day_of_year;
      end_sec  = (double) hdr1->msec / 1000.0;
    }
  } else {  
    if (ESA_FRAME == 0 && MAKE_FRAME == 0) { 	/* read to end_line that was given by user */
      nl = end_line-start_line+1;
    } else { 					/* read to end of ESA frame length         */
      nl = 24936; 
      end_line = start_line+nl-1;
    }      
    for (i=start_line; i<end_line; i++) {
      val = get_values(fphdr, hdr);
      if (val!=20) {printf("ERROR: unable to read to specified end line in header file\n"); exit(1);}
      if (dwp_val[dwp_cnt-1] != hdr->delay) {
        dwp_val[dwp_cnt] = hdr->delay;
        dwp_line[dwp_cnt] = i;
        dwp_cnt++;
      }
      if (USE_CLOCK_DRIFT==1) clock_drift_hist[hdr->clock_drift]++;
    }
    end_year = 1970+ hdr->lsd_year;
    end_date = hdr->day_of_year;
    end_sec  = (double) hdr->msec / 1000.0;
  } 
  
  if (USE_CLOCK_DRIFT==1) {
    printf("APPLYING CLOCK DRIFT TO IMAGE TIMING.\n");
    clock_drift_median = get_median(clock_drift_hist,MAX_CLOCK_DRIFT);
    printf("\tclock_drift_median     = %li \n",clock_drift_median);
    
    clock_shift = (double) clock_drift_median / 1000.0;
    start_sec += clock_shift;
    current_sec += clock_shift;
    end_sec += clock_shift;
    
    if (start_sec > 86400.0) {start_sec -= 86400.0; start_date += 1;}
    if (current_sec > 86400.0) {current_sec -= 86400.0; current_date += 1;}
    if (end_sec > 86400.0) {end_sec -= 86400.0; end_date += 1;}

    dtmp = date_hms2sec(&s_time)+clock_shift;
    if (dtmp > 86400.0) { s_date.jd+=1; dtmp-=86400.0;}
    date_sec2hms(dtmp,&s_time);
  }
  
  printf("Found start   time: %i %i %lf\n",start_year, start_date, start_sec);
  printf("Found current time: %i %i %lf\n",current_year, current_date, current_sec);
  printf("Found end     time: %i %i %lf\n",end_year, end_date, end_sec);
  printf("Found total lines : %i\n",nl);

  /* If we have at least one DWP change, need to create the DWP file */
  if (dwp_cnt > 1)  {  
    if (ESA_FRAME==1) { 
      sprintf(dwpfile,"%s_node%.4i.dwp",basefile,node);
    } else if (start_line != 1) {  /* processing a piece of the swath, add starting line to ROI.in file name */
      sprintf(dwpfile,"%s_line%i.dwp",basefile,start_line);
    } else {
      strcpy(dwpfile,basefile); strcat(dwpfile,".dwp");
    }

    FILE *dwpfp = fopen(dwpfile,"w");
    int increasing;
    
    printf("Found DWP shifts in this scene; creating DWP file %s\n",dwpfile);
    for (i=0; i<dwp_cnt; i++) { if (dwp_val[i] < dwp_min) dwp_min = dwp_val[i]; }
    if (dwp_min == dwp_val[0]) increasing = 1;
    else increasing = 0;
    for (i=increasing; i<dwp_cnt; i++) {
        val = (dwp_val[i] - dwp_min) * DIGITIZATION_SHIFT;
	fprintf(dwpfp,"%i %i\n",dwp_line[i],val);
    }
    dwp_flag = 1;
    fclose(dwpfp);
  } else { dwp_flag = 0; dwp_min = dwp_val[0]; }

  /* Perform error checking on the times just read in 
  ---------------------------------------------------*/
  /* if years don't match, warning only - ignore it */
  // if (start_year != current_year) {printf("WARNING: Year of data take does not match!!!\n");}
    
  if (start_date != current_date) {
    if (current_date-start_date>1) { printf("ERROR: Bad current date found\n"); exit(1); }
    else current_sec += 86400.0;
  }
    
  if (end_date != current_date) {
    if (end_date-current_date>1) { printf("ERROR: Bad end date found\n"); exit(1); }
    else end_sec += 86400.0;
  }
   
  line_time_est = (double)nl / (double)prf;

  /* check for other timing errors */
  if (start_sec > end_sec) { 
      printf("WARNING: Data segment end time is before start of the datatake; fixing it (could be in error)!!!\n");
      end_sec = current_sec + line_time_est;
  }
    
  if (start_sec > current_sec) { printf("ERROR: Data segment time is before the start of the datatake!\n"); exit(1);}
  if (start_sec == 0) { printf("ERROR: Datatake time is ZERO!!!\n"); exit(1);}
  if (current_sec == 0) { printf("ERROR: Data segment start time is ZERO!!!\n"); exit(1);}
    
  time_from_start = current_sec - start_sec;
  time_length = end_sec - current_sec;
    
  if (fabs(line_time_est-time_length)>0.1) {
     printf("WARNING: Number of lines does not match time length\n"); 
     printf("WARNING: Time length from header: %lf; Line time estimate: %lf\n",time_length, line_time_est);
  }

  if (USE_TLES == 0) {
    int cnt;
    int year, month, day, hour, min;
    double sec, thisSec;
    FILE *fpvec, *fpo;
    char tmp[256];
  
    sprintf(tmp,"/home/talogan/Seasat_State_Vectors/%3i.ebf",start_date);
    fpvec = fopen(tmp,"r");
    if (fpvec == NULL) {
      printf("Unable to open state vector file for day %i\n",start_date); 
      printf("Defaulting to using TLEs instead\n");
      USE_TLES = 1;    
    } else {
      cnt = fscanf(fpvec,"%i %i %i %i %i %lf %lf %lf %lf %lf %lf %lf",&year,&month,&day,&hour,&min,&sec,&x,&y,&z,&xdot,&ydot,&zdot);
      thisSec = (double) ((hour*60+min)*60)+sec;
 
      /* seek to the correct second of the day for the START of this file 
       -----------------------------------------------------------------*/
      while (cnt == 12 && start_sec > (thisSec+1.0)) {
        cnt = fscanf(fpvec,"%i %i %i %i %i %lf %lf %lf %lf %lf %lf %lf",&year,&month,&day,&hour,&min,&sec,&x,&y,&z,&xdot,&ydot,&zdot);
        thisSec = (double) ((hour*60+min)*60)+sec;
      }
      printf("Found closest second %lf\n",thisSec);
  
      /* need to create a state vector file for this image, starting
         with the one we just read, continuing for the length of
         this scene
       ------------------------------------------------------------*/
      stateVector vec, last_vec;
      last_vec.pos.x = x;
      last_vec.pos.y = y;
      last_vec.pos.z = z;
      last_vec.vel.x = xdot;
      last_vec.vel.y = ydot;
      last_vec.vel.z = zdot;
  
      strcpy(vecfile,basefile); 
      strcat(vecfile,".stvecs");  
      fpo = fopen(vecfile,"w");
      vec = propagate(last_vec,thisSec,start_sec);
      double time_offset = 0.0;
    
      while (cnt == 12 && end_sec > thisSec) {
        fprintf(fpo,"%lf %lf %lf %lf %lf %lf %lf\n",time_offset, 
	   vec.pos.x,vec.pos.y,vec.pos.z,vec.vel.x,vec.vel.y,vec.vel.z);
        cnt = fscanf(fpvec,"%i %i %i %i %i %lf %lf %lf %lf %lf %lf %lf\n",
    	   &year,&month,&day,&hour,&min,&sec,&x,&y,&z,&xdot,&ydot,&zdot);
        thisSec = (double) ((hour*60+min)*60)+sec;
        last_vec.pos.x = x;
        last_vec.pos.y = y;
        last_vec.pos.z = z;
        last_vec.vel.x = xdot;
        last_vec.vel.y = ydot;
        last_vec.vel.z = zdot;
        time_offset += 1;
        vec = propagate(last_vec,thisSec,start_sec+time_offset);
      }
      fclose(fpo);
    }
  }
  
  if (USE_TLES == 1) {
    /* Create appropriate state vectors for this datatake
     ---------------------------------------------------*/
    printf("Propagating state vectors to requested time...\n");
    create_input_tle_file(s_date,s_time,"tle1.txt");
    propagate_state_vector("tle1.txt"); 
    printf("\n\nConverting state vectors from ECI to ECEF\n");
    fix_state_vectors(s_date.year,s_date.jd,s_time.hour,s_time.min,s_time.sec);
    remove("tle1.txt");  
    remove("propagated_state_vector.txt");

    printf("Reading first state vector\n");
    FILE *fpvec = fopen("fixed_state_vector.txt","r");
    if (fscanf(fpvec,"%lf %lf %lf %lf %lf %lf %lf\n",&t,&x,&y,&z,&xdot,&ydot,&zdot)!=7) 
      { printf("ERROR: Unable to find state vector in fixed_state_vector.txt file\n"); exit(1); }
    fclose(fpvec);
    strcpy(vecfile,"fixed_state_vector.txt");
//  remove("fixed_state_vector.txt");
  }

/* Find the correct state vector for this data segment 
-----------------------------------------------------*/
  {
    FILE *fpvec;
    fpvec = fopen(vecfile,"r");
    int which = (int) (time_from_start+0.5);
    int i;
    for (i=0; i<=which; i++)
      if (fscanf(fpvec,"%lf %lf %lf %lf %lf %lf %lf\n",&t,&x,&y,&z,&xdot,&ydot,&zdot)!=7) 
        { printf("ERROR: Unable to find state vector #%i in fixed_state_vector.txt file\n",which); exit(1); }
    fclose(fpvec);
  }

/* Get the peg information needed for ROI
 ---------------------------------------*/
  get_peg_info(time_from_start,nl,prf,vecfile,schvel,schacc,&height,&earthrad);
//  remove("fixed_state_vector.txt");

/* Calculate the slant range to the first pixel 
 ---------------------------------------------*/
  {
    double dwp, tau, pri;
    double c = 299792458.0;
    
    printf("found dwp min of %i\n",dwp_min);
    pri = 1.0 / (double)prf;
    dwp = ((double)dwp_min/64.0)*pri;
    tau = dwp + 9*pri;
    srf = tau * c / 2.0;
  }

/* Estimate the doppler centroid
 ------------------------------*/
  if ((fpdat=fopen(infile,"rb"))==NULL)  {printf("Error opening input file %s\n",infile); exit(1);}
  estdop(fpdat,start_line-1,nl,&fd,&fdd,&fddd,&iqmean);
  
  char dir;
  if (zdot > 0.0) dir = 'A'; else dir = 'D';
  double geocentric_lat_nadir = asin(z / sqrt (x*x+y*y+z*z));
  double geodetic_lat_nadir = atan(tan(geocentric_lat_nadir)/(1-r_e2wgs84));
  double lat_nadir = geodetic_lat_nadir*180/M_PI;
  
  /* TAL - "hueristics" for getting the doppler correct...
   ------------------------------------------------------*/
  if (dir=='D' && fd < 0.1) { 
    printf("Doppler Fix: descending image with low doppler, adding 1\n");
    fd += 1.0;
  }
  
  if (dir=='A' && lat_nadir < 40.0) { 
    printf("Doppler fix: ascending image low latitude (%lf), subtracting\n",lat_nadir);
    fd -=1; 
    if (fd > -0.6) fd -=1;
  } 
  
  if (dir=='A' && lat_nadir > 40.0 && fd > 0.15) {
    printf("Doppler fix: ascending image high latitude (%lf), subtracting\n",lat_nadir);
    fd -=1; 
  }
  
  fd += dop_shift;

/* Calculate the spectra and get the caltones
 -------------------------------------------*/
  fseek(fpdat,0,SEEK_SET);
  spectra(fpdat,start_line-1,nl,iqmean,&ncaltones,caltones);

/*=================================================================================
   NOW, ACTUALLY CREATE THE OUTPUT ROI FILE
 =================================================================================*/
  if (ESA_FRAME==1) { 
    sprintf(outfile,"%s_node%.4i.roi.in",basefile,node);
  } else if (start_line != 1) {  /* processing a piece of the swath, add starting line to ROI.in file name */
    sprintf(outfile,"%s_line%i.roi.in",basefile,start_line);
  } else {
    strcpy(outfile,basefile); 
    strcat(outfile,".roi.in");
  }
 
  if ((fproi=fopen(outfile,"w"))==NULL) {printf("Error opening output file %s\n",outfile); exit(1);}
  
  if (ESA_FRAME==1) {
    sprintf(outfile,"%s_node%.4i.slc",basefile,node);
  } else {
    strcpy(outfile,basefile); strcat(outfile,".slc");
  }

  printf("============================================================================\n");
  printf(" EMITTING FILE HEADER FILE NOW\n");
  printf("============================================================================\n");

/* First input data file */
  printf("First input data file: %s\n",infile);
  roi_put_string(fproi,infile,"First input data file");
  
/* Second input data file */
  printf("Second input data file: /dev/null\n");
  roi_put_string(fproi,"/dev/null","Second input data file");

/* Output data file */
  printf("Output data file: %s\n",outfile);
  roi_put_string(fproi,outfile,"Output data file");

/* Output amplitudes file */
  printf("Output amplitudes file: /dev/null\n");
  roi_put_string(fproi,"/dev/null","Output amplitude file");

/* 8lk output file */
  printf("8lk output file: 8lk\n");
  roi_put_string(fproi,"/dev/null","8lk output file");

/* debug flag */
  printf("debug flag: 0\n");
  roi_put_int(fproi,0,"debug flag");
    
/* How many input bytes per line files 1 and 2 */
  printf("How many input bytes per line files 1 and 2: 13680 13680\n");
  roi_put2_int(fproi,13680,13680,"How many input bytes per line files 1 and 2");

/* How many good bytes per line, including header */
  printf("How many good bytes per line, including header: 13680 13680\n");
  roi_put2_int(fproi,13680,13680,"How many good bytes per line, including header");

/* First line to read  (start at 0) */
  printf("First line to read: %i\n",start_line);
  roi_put_int(fproi,start_line,"First line to read  (start at 0)");

/* Enter # of range input patches */
  patches = nl / actual_lines;
  if (patches ==0) patches = 1;
  printf("# of range input patches: %i\n",patches);
  roi_put_int(fproi,patches,"Number of range input patches");
  
/* First sample pair to use (start at zero) */
  printf("First sample pair to use: 0\n");
  roi_put_int(fproi,0,"First sample pair to use (start at zero)");
  
/* Azimuth Patch Size (Power of 2) */
  printf("Azimuth Patch Size (Power of 2): 16384\n");
  roi_put_int(fproi,16384,"Azimuth Patch Size (Power of 2)");
  
/* Number of valid points in azimuth */
  printf("Number of valid points in azimuth: %i\n",actual_lines);
  roi_put_int(fproi,actual_lines,"Number of valid points in azimuth");
  
/* Deskew the image */
  printf("Deskew the image: n\n");
  roi_put_char(fproi,'n',"Deskew the image");
  
/* Caltone % of sample rate */				
  printf("Number of caltones to remove: %i\n",ncaltones);
  roi_put_int(fproi,ncaltones,"Number of Caltones to Remove");
  for (i=0;i<ncaltones;i++) {
    printf("\tCaltone %i: %lf\n",i,caltones[i]);
    roi_put_double_lf(fproi,caltones[i],14,"Caltone % of sample rate");
  }
  
/* Start range bin, number of range bins to process */		
  printf("Start range bin, number of range bins to process: 1 %i\n",actual_samps);
  roi_put2_int(fproi,1,actual_samps,"Start range bin, number of range bins to process");
  
/* Delta azimuth, range pixels for second file */
  printf("Delta azimuth, range pixels for second file: 0 0\n");
  roi_put2_int(fproi,0,0,"Delta azimuth, range pixels for second file");

/* Image 1 Doppler centroid quad coefs (Hz/prf)	*/
  printf("Doppler centroid quad coefs (Hz/prf): %lf %.8lf %.10lf\n",fd,fdd,fddd);
  roi_put3_dop(fproi,fd,fdd,fddd,"Image 1 Doppler centroid quad coefs (Hz/prf)");
  roi_put3_dop(fproi,fd,fdd,fddd,"Image 2 Doppler centroid quad coefs (Hz/prf)");

/* 1 = use file 1 doppler, 2 = file 2, 3 = avg */
  printf("1 = use file 1 doppler, 2 = file 2, 3 = avg: 1\n");
  roi_put_int(fproi,1,"1 = use file 1 doppler, 2 = file 2, 3 = avg");
  
/* Earth Radius (m) */
  printf("Earth Radius of Curvature (m): %lf\n",earthrad);
  roi_put_double(fproi,earthrad,"Earth Radius (m)");

/* Body Fixed S/C velocities 1,2 (m/s) */
  vel = sqrt(xdot*xdot+ydot*ydot+zdot*zdot);
  printf("Body Fixed S/C velocities 1,2 (m/s): %lf %lf\n",vel,vel);
  roi_put2_double(fproi,vel,vel,"Body Fixed S/C velocities 1,2 (m/s)");

/* Spacecraft height 1,2 (m) */
  printf("Spacecraft height 1,2 (m): %lf\n",height);
  roi_put2_double(fproi,height,height,"Spacecraft height 1,2 (m)");
  
/* Planet GM */
  printf("Planet GM: 0\n");
  roi_put_double(fproi,0,"Planet GM");
  
/* Left, Right or Unknown Pointing */
  printf("Left, Right or Unknown Pointing: Right\n");
  roi_put_string(fproi,"Right","Left, Right or Unknown Pointing");
  
/* SCH Velocity Vector 1 & 2 */
  printf("SCH Velocity Vector 1 & 2: %lf %lf %lf\n",schvel[0],schvel[1],schvel[2]);
  roi_put3_double(fproi,schvel[0],schvel[1],schvel[2],"SCH Velocity Vector 1");
  roi_put3_double(fproi,schvel[0],schvel[1],schvel[2],"SCH Velocity Vector 2");
  
/* SCH Acceleration Vector 1 & 2 */
  printf("SCH Acceleration Vector 1 & 2: %lf %lf %lf\n",schacc[0],schacc[1],schacc[2]);
  roi_put3_double(fproi,schacc[0],schacc[1],schacc[2],"SCH Acceleration Vector 1");
  roi_put3_double(fproi,schacc[0],schacc[1],schacc[2],"SCH Acceleration Vector 2");
  
/* Range of first sample in raw data file 1,2 (m) */
  printf("Range of first sample in raw data file 1,2 (m): %lf %lf\n",srf,srf);
  roi_put2_double(fproi,srf,srf,"Range of first sample in raw data file 1,2 (m)");

/* PRF 1,2 (pps) */
  printf("PRF 1,2: %i %i\n",prf,prf);
  roi_put2_int(fproi,prf,prf,"PRF 1,2 (pps)");
  
/* i/q means, i1,q1, i2,q2 */
  printf("i/q means, i1,q1, i2,q2: %lf %lf %lf %lf\n",iqmean,iqmean,iqmean,iqmean);
  roi_put4_double(fproi,iqmean,iqmean,iqmean,iqmean,"i/q means, i1,q1, i2,q2");
  
/* Flip i/q (y/n) */
  printf("Flip i/q (y/n): s\n");
  roi_put_char(fproi,'s',"Flip i/q (y/n)");
  
/* Desired azimuth resolution (m)  5  (what should this be???) */
  printf("Desired azimuth resolution (m): 5\n");
  roi_put_int(fproi,4,"Desired azimuth resolution (m)");

/* Number of azimuth looks         4  (what should this be???) */
  printf("Number of azimuth looks: 4\n");
  roi_put_int(fproi,4,"Number of azimuth looks");
  
/* Range sampling rate (Hz) */
  printf("Range sampling rate (Hz): 22765000\n");
  roi_put_string(fproi,"22765000","Range sampling rate (Hz)");
  
/* Chirp Slope (Hz/s) */
  printf("Chirp Slope (Hz/s): 5.62130178e11\n");
  roi_put_string(fproi,"5.62130178e11","Chirp Slope (Hz/s)");
  
/* Pulse Duration (s) */
  printf("Pulse Duration (s): 33.8e-6\n");
  roi_put_string(fproi,"33.8e-6","Pulse Duration (s)");
  
/* Chirp extension points */
  printf("Chirp extension points: 0\n");
  roi_put_int(fproi,0,"Chirp extension points");
  
/* Secondary range migration correction (y/n) */
  printf("Secondary range migration correction (y/n): y\n");
  roi_put_char(fproi,'y',"Secondary range migration correction (y/n)");
  
/* Radar Wavelength (m) */					
  printf("Radar Wavelength (m): 0.235\n");
  roi_put_double(fproi,0.235,"Radar Wavelength (m)");
  
/* Range Spectral Weighting (1.=none, 0.54=Hamming) */
  printf("Range Spectral Weighting (1.=none, 0.54=Hamming): 1.0\n");
  roi_put_double(fproi,1.0,"Range Spectral Weighting (1.=none, 0.54=Hamming)");
  
/* Fraction of range bandwidth to remove */
  printf("Fraction of range bandwidth to remove: 0 0\n");
  roi_put2_double(fproi,0,0,"Fraction of range bandwidth to remove");
  
/* linear resampling coefs:  sloper, intr, slopea, inta */
  printf("linear resampling coefs:  sloper, intr, slopea, inta: 0 0 0 0\n");
  roi_put_string(fproi,"0 0 0 0","linear resampling coefs:  sloper, intr, slopea, inta");
  
/* linear resampling deltas: dsloper, dintr, dslopea, dinta */
  printf("linear resampling deltas: dsloper, dintr, dslopea, dinta: 0 0 0 0\n");
  roi_put_string(fproi,"0 0 0 0","linear resampling deltas: dsloper, dintr, dslopea, dinta");
  
/* AGC file */
  printf("AGC file: /dev/null\n");
  roi_put_string(fproi,"/dev/null","AGC file");
  
/* DWP file */
  if (dwp_flag == 0) {
    printf("DWP file: /dev/null\n");
    roi_put_string(fproi,"/dev/null","DWP file");
  } else {
    printf("DWP file: %s\n",dwpfile);
    roi_put_string(fproi,dwpfile,"DWP file");
  }

/*  fclose(fpdat); */
  fclose(fphdr);
  fclose(fproi);

  printf("============================================================================\n");
  printf(" CREATE_ROI_IN PROGRAM COMPLETED\n");
  printf("============================================================================\n\n\n");
  
  exit(0);
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

void give_usage(char *argv[], int argc)  
{
    printf("Usage: %s [-s <start_line> -e <end_line>][-d dop][-f start_line][-v][-c][-E ESA_Node] <infile_base_name> \n\n",argv[0]);
    printf("\t<infile_base_name>\tFile to create ROI .in file from. (assumes .dat and .hdr exist)\n");
    printf("\t-s sl -e el       \tSet start and end lines to process [default - all lines]\n");
    printf("\t-d dop            \tSet doppler offset to use [default 0]\n");
    printf("\t-v                \tUse state vectors instead of TLEs\n");
    printf("\t-c                \tApply the clock drift to image timing\n");
    printf("\t-E ESA_Node       \tCreate ESA sized frame at specified node number\n"); 
    printf("\t-f start_line     \tCreate ESA sized frame starting from start_line:\n");
    printf("\t                  \t\tParameter  \tDefault\t Framed\n");
    printf("\t                  \t\t-----------------------------------------\n");
    printf("\t                  \t\tPatches    \t<Calc> \t 3\n");
    printf("\t                  \t\tNA Valid   \t11800  \t 8312\n");
    printf("\t                  \t\tRange Samps\t5300   \t 6840\n");
    printf("\n\n");
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
      asfPrintError("fprint error: %s\n", strerror(errno));
  }
}

void roi_put_double(FILE *roi_file,double value,char *comment)
{
  char param[64];
  sprintf(param,"%-16.11g",value);
  roi_put_string(roi_file,param,comment);
}

void roi_put3_dop(FILE *roi_file,double fd, double fdd, double fddd,char *comment)
{
  char param[64];
  sprintf(param,"%lf %.8lf %.12lf",fd,fdd,fddd);
  roi_put_string(roi_file,param,comment);
}

void roi_put2_double(FILE *roi_file,double val1, double val2,char *comment)
{
  char param[64];
  sprintf(param,"%lf %lf",val1,val2);
  roi_put_string(roi_file,param,comment);
}

void roi_put3_double(FILE *roi_file,double val1, double val2, double val3,char *comment)
{
  char param[64];
  sprintf(param,"%lf %lf %lf",val1,val2,val3);
  roi_put_string(roi_file,param,comment);
}

void roi_put4_double(FILE *roi_file,double val1, double val2, double val3, double val4, char *comment)
{
  char param[64];
  sprintf(param,"%lf %lf %lf %lf",val1,val2,val3,val4);
  roi_put_string(roi_file,param,comment);
}

void roi_put_int(FILE *roi_file,int value,char *comment)
{
  char param[64];
  sprintf(param,"%i",value);
  roi_put_string(roi_file,param,comment);
}

void roi_put2_int(FILE *roi_file,int val1, int val2,char *comment)
{
  char param[64];
  sprintf(param,"%i %i",val1,val2);
  roi_put_string(roi_file,param,comment);
}

void roi_put_char(FILE *roi_file,char value,char *comment)
{
  char param[2];
  sprintf(param,"%c",value);
  roi_put_string(roi_file,param,comment);
}

void roi_put_double_lf(FILE *roi_file,double value,int decimals,
            char *comment)
{
  char param[100];
  char format[15];
  sprintf(format, "%%-16.%if", decimals);
  snprintf(param,99,format,value);
  roi_put_string(roi_file,param,comment);
}

int get_median(int *hist, int size) {
  int retval = -1, max = 0, i;
  for (i=0; i<size; i++) if (hist[i]>max) {max=hist[i]; retval=i;}
  if (retval==-1) { printf("Error getting histogram median value\n"); exit(1); }
  return(retval);
}



























