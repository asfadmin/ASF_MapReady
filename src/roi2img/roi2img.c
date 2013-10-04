/******************************************************************************
NAME:  Convert a roi slc file into an ASF img file.

SYNOPSIS:  roi2img [-m][-r roi.in_file][-v][-c][-E node] <base_file_name>

DESCRIPTION:

	-m 		only create the meta file
	-r <file>	use the given ROI file for parameters
	-v		use state vectors instead of TLEs
	-c		apply clock drift to image timing
	-E <node>	create output file names using ESA node & orbit

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    11/12    T. Logan     Take complex looks from a ROI slc file
    
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

#define CPX_PIX 6840
#define LA      1
#define LD	4
#define PRI	0.00060725

typedef struct {
        int      major_cnt;
	long int major_sync_loc;
	int      lsd_year;
	int      station_code;
	long int msec;
	int      day_of_year;
	int      clock_drift;  
	int  	 no_scan_indicator_bit;
	int      bits_per_sample;
	int      mfr_lock_bit;
	int      prf_rate_code;
	int      delay;
	int      scu_bit;
	int      sdf_bit;
	int      adc_bit;
	int      time_gate_bit;
	int      local_prf_bit;
	int      auto_prf_bit;
	int      prf_lock_bit;
	int      local_delay_bit;
}  SEASAT_header_ext;


/* Global variables filled in read_roi_infile and used in main */
char 		datfilename[256];
int 		start_line;       	// first line that was processed
int 		npatches;	      	// number of patches processed
int 		patch_size;       	// valid lines per patch
int 		ncaltones;        	// number of calibration tone frequencies removed
int 		ns;		      	// number of samples
double 		dop1,dop2,dop3;		// doppler coefficients
double 		srf;	      		// slant range to the first pixel
double 		prf;	      		// pulse repitition frequency
double 		fs;	      		// sampling frequency
double 		chirp_slope;   		// chirp slope
double 		pulse_duration;		// pulse duration
double 		wavelength;    		// wave length
char 		dwpfilename[256];	// DWP file name
double		earth_rad;		// Earth Radius (m) - not actual earth radius - 
					//    radius of a best fit local sphere
double		sc_vel;			// Spacecraft body fixed velocity - IS THIS SWATH VELOCITY???
double 		sc_height;		// Spacecraft height - from surface of the earth
int		station_code;           // downlinking station (5,6,7,9,10)

/* Global variables filled in read_hdrfile and used in main */
double   	start_sec;		// start time of this segment
int      	start_date;		// start date of this segment
int 	   	start_year;		// start year of this segment
julian_date	s_date;			// start date of this segment
hms_time   	s_time;			// start time of this segment

/* Subroutines */
void give_usage(int argc, char *argv[]);
int get_values(FILE *fp,SEASAT_header_ext *s);	// read values from a header file
int read_hdrfile(char *infile);			// read correct values from header file
int read_roi_infile(char *infile);		// read the ROI input file values
void byteswap(void *buf,int len);		// just what it says
int sr2gr_pixsiz(const char *infile, const char *outfile, float srPixSize);

/* Subroutines to read values from files - used by read_roi_infile */
int get_string_val(FILE *fp, char *str);
int get_3double_vals(FILE *fp, double *num1, double *num2, double *num3);
int get_double_val(FILE *fp, double *num);
int get_2int_vals(FILE *fp,int *num1,int *num2);
int get_int_val(FILE *fp, int *num);

double r_awgs84 = 6378137.0;
double r_e2wgs84 = 0.00669437999015;
double C = 299792458.0;

int    USE_CLOCK_DRIFT = 0;   // switch to control application of clock drift

main(int argc, char *argv[]) 
{
  FILE *fpin, *fpout;
  
  float ibuff[CPX_PIX*2*LD];
  float **obuff;
  float b[CPX_PIX];
  float c[CPX_PIX/LA];

  int cla,nl;
  int i,j,k,line;
  int olines, osamps;
  int oline, osamp;
  double t;
  char basefile[256], infile[256], outbasefile[256], outfile[256], roifile[256];
  char *hdrfile;
  
  ymd_date date;
  hms_time time;
  meta_parameters *meta;

  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  
  char	 dir;			// orbit direction - A or D
  double x, y, z;		// state vector positions at start of segment
  double xdot, ydot, zdot;	// state vector veloctiy at start of segment
  int    META_ONLY = 0;		// only create meta file, no img file
  int	 SEPARATE_ROI_FILE = 0; // CLA roi file given?
  int    USE_TLES = 1;		// TLE/state vector switch
  int    ESA_FRAME = 0;		// switch to control output file names
  int    node = 0;
   
  asfSplashScreen(argc, argv); 

  if (argc<2 || argc>6) { give_usage(argc,argv); exit(1); }

  while ((cla=getopt(argc,argv,"mvcE:r:")) != -1)
    switch(cla) {
      case 'm':
        META_ONLY = 1;
	printf("Using meta only option\n");
	break;
      case 'r':
	strcpy(roifile,optarg);
	SEPARATE_ROI_FILE = 1;
	break;
      case 'E':
        ESA_FRAME = 1;
	node = atoi(optarg);
	break;
      case 'v':
        USE_TLES = 0;
	break;
      case 'c':
        USE_CLOCK_DRIFT = 1;
	break;
      case '?':
        give_usage(argc,argv);
        printf("Unknown option %s\n",optarg);
	exit(1);
      default:
        give_usage(argc,argv);
	exit(1);
    } 

  strcpy(basefile,argv[optind]);
  strcpy(infile,basefile);
  strcat(infile,".slc");

  /* if no separate roi.in file is specified, use the main name */
  if (SEPARATE_ROI_FILE == 0) {
    strcpy(roifile,basefile);
    strcat(roifile,".roi.in");
  }

  /* Read parameters from the ROI.in file */
  read_roi_infile(roifile);
  nl = npatches * patch_size;
  hdrfile = get_basename(datfilename);
  strcat(hdrfile,".hdr");
  
  /* Read the start time for this image from the hdr file */
  read_hdrfile(hdrfile);
  
  if (USE_TLES == 0) 
   {
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
  
      /* need to create a state vector file the start of this image
      ------------------------------------------------------------*/
      stateVector vec, last_vec;
      last_vec.pos.x = x; last_vec.pos.y = y; last_vec.pos.z = z;
      last_vec.vel.x = xdot; last_vec.vel.y = ydot; last_vec.vel.z = zdot;
      vec = propagate(last_vec,thisSec,start_sec);
      x = vec.pos.x; y = vec.pos.y; z = vec.pos.z;
      xdot = vec.vel.x; ydot = vec.vel.y; zdot = vec.vel.z;
    }
   }
  
  if (USE_TLES == 1) {
    /* get the correct state vector */
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
    remove("fixed_state_vector.txt");
  }
  if (zdot > 0.0) dir = 'A'; else dir = 'D';

  /* set up output image parameters */
  olines = nl / LD;
  osamps = ns / LA;

  /* Create the meta file */
  printf("Initializing the meta structure\n");
  meta = raw_init();

  /* Propagate the state vectors */  
  printf("Creating state vectors\n");
  stateVector stVec;/*Source state vector*/

  stVec.pos.x = x;
  stVec.pos.y = y;
  stVec.pos.z = z;
  stVec.vel.x = xdot;
  stVec.vel.y = ydot;
  stVec.vel.z = zdot;

  date_jd2ymd(&s_date,&date);

  meta->state_vectors = meta_state_vectors_init(1);
  meta->state_vectors->vecs[0].vec = stVec;
  meta->state_vectors->year = date.year;
  meta->state_vectors->julDay = s_date.jd;
  meta->state_vectors->second = date_hms2sec(&s_time);
  meta->state_vectors->vecs[0].time = 0;
  int num_vecs = 2 + (int)(nl*PRI)/30.0;
  propagate_state(meta, num_vecs+1, (nl*PRI)/num_vecs);

  printf("Calculating scene geometry parameters\n");

  double RE = r_awgs84;
  double RP = r_awgs84 * sqrt(1-r_e2wgs84);
  
  double imgSec=date2sec(&s_date,&s_time);					// time at start of image
  int num = meta->state_vectors->num / 2;					// closest state vector to center of image
  double sourceSec = imgSec+meta->state_vectors->vecs[num].time;		// time at closest state vector
  double destSec = imgSec+meta->state_vectors->vecs[meta->state_vectors->num-1].time/2;	// time at center of image

  printf("Finding center state vector\n");
  stateVector midVec = propagate(meta->state_vectors->vecs[num].vec,sourceSec,destSec);	// state vector at middle time of image
      
  x = midVec.pos.x;
  y = midVec.pos.y;
  z = midVec.pos.z;
  xdot = midVec.vel.x;
  ydot = midVec.vel.y;
  zdot = midVec.vel.z;
  
  double geocentric_lat_nadir = asin(z / sqrt (x*x+y*y+z*z));
  double lon_nadir = atan2(x,y)*180/M_PI;
  double RE_nadir = (RE * RP) / sqrt((RP*cos(geocentric_lat_nadir)*RP*cos(geocentric_lat_nadir)) +
  				     (RE*sin(geocentric_lat_nadir)*RE*sin(geocentric_lat_nadir)));
  double Rsc = sqrt(x*x+y*y+z*z);
  double geodetic_lat_nadir = atan(tan(geocentric_lat_nadir)/(1-r_e2wgs84));
  double lat_nadir = geodetic_lat_nadir*180/M_PI;
  double gamma = geodetic_lat_nadir - geocentric_lat_nadir;
    
  printf("Filling in meta->general parameters\n");
  
  strcpy(meta->general->sensor,"SEASAT");
  strcpy(meta->general->sensor_name,"SAR");
  strcpy(meta->general->mode,"STD");
  strcpy(meta->general->processor,"ASPS-v" ASPS_VERSION_STRING);
  meta->general->data_type = REAL32;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0f",
          date.day, mon[date.month], date.year, s_time.hour, s_time.min, s_time.sec);
  meta->general->orbit = time2rev(s_date,s_time);
  meta->general->orbit_direction = dir;
  if (ESA_FRAME == 1) meta->general->frame = node;
  meta->general->band_count = 1;
  strcpy(meta->general->bands,"HH");
  meta->general->line_count = nl/LD;
  meta->general->sample_count = ns/LA;
  meta->general->start_line = 0; 
  meta->general->start_sample = 0;
  meta->general->line_scaling = 1;
  meta->general->sample_scaling = 1;
  meta->general->x_pixel_size = (C / (2.0 * fs)) * LA;
 
  switch (station_code) {
    case 5:
      strcpy(meta->general->receiving_station, "ULA");
      break;
    case 6:
      strcpy(meta->general->receiving_station, "GDS");
      break;
    case 7:
      strcpy(meta->general->receiving_station, "MIL");
      break;
    case 9:
      strcpy(meta->general->receiving_station, "UKO");
      break;
    case 10:
      strcpy(meta->general->receiving_station, "SNF");
      break;
  }
 
  double orbit_vel = sqrt(9.81*RE_nadir*RE_nadir / Rsc);
  double swath_vel = orbit_vel * RE_nadir / Rsc;
  
  meta->general->y_pixel_size = (swath_vel * PRI) * LD;    // TAL - Check the sc_vel...
  meta->general->re_major = r_awgs84;
  meta->general->re_minor = r_awgs84 * sqrt(1-r_e2wgs84);
  
//  meta->general->bit_error_rate = ???
//  meta->general->missing_lines = ???  
//  meta->general->no_data = ???

      
  /*Create the SAR metadata block*/
  
  printf("Creating the meta->sar block\n");
  if (!meta->sar) meta->sar = meta_sar_init();

  meta->sar->image_type = 'S';
  meta->sar->look_direction = 'R';
  meta->sar->azimuth_look_count = LD;
  meta->sar->range_look_count = LA;
  meta->sar->deskewed = 0;
  meta->sar->original_line_count = nl;
  meta->sar->original_sample_count = ns;
  meta->sar->line_increment = 1;
  meta->sar->sample_increment = 1;
  meta->sar->range_time_per_pixel = 1/(2*fs);
  
  // Should be this, right???    meta->sar->azimuth_time_per_pixel = PRI;
  // Second try is this one	 meta->sar->azimuth_time_per_pixel = (destSec - imgSec) / (meta->sar->original_line_count/2);
  
  meta->sar->azimuth_time_per_pixel = meta->general->y_pixel_size / swath_vel;
  meta->sar->azimuth_time_per_pixel *= -1;
  meta->sar->time_shift = fabs(meta->general->line_count*meta->sar->azimuth_time_per_pixel);
  
//  meta->sar->slant_shift = -1080;			// emperical value from a single delta scene
//  meta->sar->time_shift = 0.18;			// emperical value from a single delta scene

  if (USE_CLOCK_DRIFT ==1) meta->sar->slant_shift = SEASAT_SLANT_SHIFT; // -1000.0;
  else meta->sar->slant_shift = 0.0;

  meta->sar->slant_range_first_pixel = srf;
  meta->sar->wavelength = wavelength;
  meta->sar->prf = prf;
  meta->sar->earth_radius = meta_get_earth_radius(meta,	meta->general->line_count/2.0, meta->general->sample_count/2.0);
  meta->sar->satellite_height = Rsc;
  meta->sar->range_doppler_coefficients[0] = dop1*prf;
  meta->sar->range_doppler_coefficients[1] = dop2*prf;
  meta->sar->range_doppler_coefficients[2] = dop3*prf;
  meta->sar->azimuth_doppler_coefficients[0] = dop1*prf;
  meta->sar->azimuth_doppler_coefficients[1] = 0;
  meta->sar->azimuth_doppler_coefficients[2] = 0;
  
///  meta->sar->azimuth_processing_bandwidth = ????
  
  meta->sar->chirp_rate = chirp_slope;
  meta->sar->pulse_duration = pulse_duration;
  meta->sar->range_sampling_rate = fs;
  strcpy(meta->sar->polarization,"HH");
  meta->sar->multilook = 1;
  meta->sar->pitch = 0;
  meta->sar->roll = 0;
  meta->sar->yaw = 0;
///  meta->sar->incid_a[0-5] = ???

  printf("Creating the meta->location block\n");
  if (!meta->location) meta->location = meta_location_init();
  meta_get_corner_coords(meta);
  meta_get_latLon(meta,meta->general->line_count/2,meta->general->sample_count/2,0,
  		  &meta->general->center_latitude, &meta->general->center_longitude);

  if (ESA_FRAME==0) {
    strcpy(outbasefile,basefile);
  } else {
    sprintf(outbasefile,"SS_%.5i_SLANT_F%.4i",meta->general->orbit,meta->general->frame);
  }

  strcpy(outfile,outbasefile);
  strcat(outfile,".img");
  strcpy(meta->general->basename,outbasefile);

  if (META_ONLY==0) { 
    obuff = (float **) malloc (sizeof(float *)*olines);
    for (i=0; i<olines; i++) obuff[i] = (float *) malloc (sizeof(float)*osamps);
  
    /* Open the input slc file and output img file*/
    fpin = fopen(infile,"rb");
    if (fpin==NULL) {printf("ERROR: Unable to open input file %s\n",infile); exit(1);}
    fpout = fopen(outfile,"wb");

    /* Take the complex looks from the slc file to create the img file */
    printf("Taking complex looks from file %s to create %s\n",infile,outfile);
    oline = 0;
    for (line=0; line < nl; line+=LD) {
      if (line%2560==0) printf("\t%i\n",line);
      fread(ibuff,sizeof(float),ns*2*LD,fpin);

      /* take looks down */
      for (j=0; j<ns; j++) {
        b[j] = 0;
        for (i=0; i<LD; i++)
          b[j] = b[j] + (ibuff[(2*j)+(i*ns*2)]*ibuff[2*j+(i*ns*2)]) 
  		      + (ibuff[(2*j+1)+(i*ns*2)]*ibuff[(2*j+1)+(i*ns*2)]);    
      }
    
      /* take looks across */
      for (j=0; j<ns/LA; j++) {
        c[j] = 0;
        for (k=0;k<LA;k++)
          c[j] = c[j] + b[j*LA+k];
        c[j] = sqrt(c[j]);
      }
      byteswap(c,ns/LA);
      for (j=0; j<osamps; j++) obuff[oline][j] = c[j];
      oline++;
    }

    /* write out image in reverse order */
    for (j=0; j<olines; j++) fwrite(obuff[olines-j-1],sizeof(float),osamps,fpout); 
 
    fclose(fpout);
    fclose(fpin);
    free(obuff);
    
  }  /* END IF META_ONLY */   

  printf("Writing out the meta file\n");
  meta_write(meta, outbasefile);

  if (META_ONLY == 0) {
    char grfilename[256];
    float grPixSiz = 12.5;
    int err = 0;
    if (ESA_FRAME == 1) {
      char tmpstr[256];
      char cropfile[256];
      char tmpfile[256];
      
      /* create the ground range image */
      sprintf(grfilename,"temp_%.5i_STD_F%.4i",meta->general->orbit,meta->general->frame);
      sr2gr_pixsiz(outbasefile, grfilename, grPixSiz);
      
      /* crop the image to exact size */
      sprintf(cropfile,"SS_%.5i_STD_F%.4i",meta->general->orbit,meta->general->frame);
      trim(grfilename,cropfile,(long long)0,(long long)0,(long long)8000,(long long)8000);
      
      /* remove the non-cropped ground range image */
      strcat(strcpy(tmpstr,grfilename),".img");
      remove(tmpstr);
      strcat(strcpy(tmpstr,grfilename),".meta");
      remove(tmpstr);

      /* geocode and export to geotiff */
      sprintf(tmpstr,"asf_geocode -p utm %s %s_utm\n",cropfile,cropfile);
      err = system(tmpstr);
      if (err) {printf("Error returned from asf_geocode\n"); exit(1);}

      sprintf(tmpstr,"asf_export -format geotiff %s_utm %s\n",cropfile,cropfile);
      err = system(tmpstr);
      if (err) {printf("Error returned from asf_export to geotiff\n"); exit(1);}
 
      /* remove the utm projected internal format image */
      strcat(strcpy(tmpstr,cropfile),"_utm.img");
      remove(tmpstr);
      strcat(strcpy(tmpstr,cropfile),"_utm.meta");
      remove(tmpstr);
 
      /* this changes the basename in the metadata from blah_SLANT to blah_STD  */
      meta_parameters *crop_meta = meta_read(cropfile);
      strcpy(crop_meta->general->basename, cropfile);
      meta_write(crop_meta, cropfile);
      meta_free(crop_meta);
 
      /* create the dowsized QC image */
      sprintf(tmpstr,"resample -scale 0.125 %s %s_small\n",cropfile,cropfile);
      err = system(tmpstr);
      if (err) {printf("Error returned from resample\n"); exit(1);}
      
      sprintf(tmpfile,"%s_QCFULL",cropfile);
      sprintf(tmpstr,"asf_export -format jpeg %s_small %s\n",cropfile,tmpfile);
      err = system(tmpstr);
      if (err) {printf("Error returned from asf_export to jpeg\n"); exit(1);}
      
      /* remove the small .img file */
      strcat(strcpy(tmpstr,cropfile),"_small.img");
      remove(tmpstr);
      strcat(strcpy(tmpstr,cropfile),"_small.meta");
      remove(tmpstr);

      /* create the subsampled QC image */
      sprintf(tmpfile,"%s_QCSUB",cropfile);
      trim(cropfile,tmpfile,(long long)3500,(long long)3500,(long long)1000,(long long)1000);

      sprintf(tmpstr,"asf_export -format jpeg %s %s\n",tmpfile,tmpfile);
      err = system(tmpstr);
      if (err) {printf("Error returned from asf_export\n"); exit(1);}

      /* run make_seasat_h5 */
      sprintf(tmpstr,"make_seasat_h5 -gap %s.dis %s %s",basefile,cropfile,cropfile);
      err = system(tmpstr);
      if (err) {printf("Error returned from make_seasat_h5\n"); exit(1);}

      /* remove the subsampled QC .img file */
      strcat(strcpy(tmpstr,tmpfile),".img");
      remove(tmpstr);
      strcat(strcpy(tmpstr,tmpfile),".meta");
      remove(tmpstr);

      /* rename the ROI.in file to match the new file name */
      sprintf(tmpfile,"%s.roi.in",cropfile);
      rename(roifile,tmpfile);

    } else {
      strcpy(grfilename,basefile);
      strcat(grfilename,"G12");
      sr2gr_pixsiz(basefile, grfilename, grPixSiz);
    } 
  }

  exit(0);
}
    
    
int read_roi_infile(char *infile)
{
  FILE *fp = fopen(infile,"r");
  
  if (fp==NULL) {printf("ERROR: can't open %s ROI input file\n",infile); return(1); }
  
  char ctmp[256];
  int  itmp;
  double dtmp;
  int  i;
  						// DESCRIPTION							VALUE
  						// ----------------------------------------------------------	-----------------------------------------------
  get_string_val(fp,datfilename);		// First input data file					<infile>.dat				               
  get_string_val(fp,ctmp);			// Second input data file  				        /dev/null
  get_string_val(fp,ctmp);	        	// Output data file						<infile>.slc
  get_string_val(fp,ctmp);			// Output amplitudes file					/dev/null
  get_string_val(fp,ctmp);			// 8lk output file						8lk
  get_int_val(fp,&itmp);	 		// debug flag							0
  get_int_val(fp,&itmp);			// How many input bytes per line files 1 and 2		   	13680 13680
  get_int_val(fp,&itmp);			// How many good bytes per line, including header		13680 13680
  get_int_val(fp,&start_line); 	   		// First line to read  (start at 0)  				1
  get_int_val(fp,&npatches);			// Enter # of range input patches				{Get nl from hdr values; patches = nl/11600}
  get_int_val(fp,&itmp); 			// First sample pair to use (start at zero)			0
  get_int_val(fp,&itmp);			// Azimuth Patch Size (Power of 2)				16384
  get_int_val(fp,&patch_size);			// Number of valid points in azimuth				11600
  get_string_val(fp,ctmp);			// Deskew the image						n
  get_int_val(fp,&ncaltones);				

  printf("Reading %i caltone values\n",ncaltones);

  for (i=0;i<ncaltones;i++) 
      get_double_val(fp,&dtmp);			// Caltone % of sample rate					0.25 0.25701904296875
  get_2int_vals(fp,&itmp,&ns);			// Start range bin, number of range bins to process		1 6840
  get_int_val(fp,&itmp);			// Delta azimuth, range pixels for second file  		0 0
  get_3double_vals(fp,&dop1,&dop2,&dop3);	// Image 1 Doppler centroid quad coefs (Hz/prf) 		{calculated from dop_est - 3 parameters}
  get_double_val(fp,&dtmp);			// Image 2 Doppler centroid quad coefs (Hz/prf) 		{copy above 3 values}
  get_int_val(fp,&itmp);			// 1 = use file 1 doppler, 2 = file 2, 3 = avg  		1
  get_double_val(fp,&earth_rad);		// Earth Radius (m)						{calculated from get_peg_info}
  get_double_val(fp,&sc_vel);			// Body Fixed S/C velocities 1,2 (m/s)  			{calculate from EBEF state vector - 2 parameters}
  get_double_val(fp,&sc_height);		// Spacecraft height 1,2 (m)					{calculated from get_peg_info - 2 parameters}
  get_double_val(fp,&dtmp);			// Planet GM							0
  get_double_val(fp,&dtmp);			// Left, Right or Unknown Pointing				Right
  get_double_val(fp,&dtmp);			// SCH Velocity Vector 1					{calculated from get_peg_info - 3 parameters}
  get_double_val(fp,&dtmp);			// SCH Velocity Vector 2					{copy above 3 values}
  get_double_val(fp,&dtmp);			// SCH Acceleration Vector 1					{calculated from get_pef_info - 3 parameters}
  get_double_val(fp,&dtmp);			// SCH Acceleration Vector 2					{copy above 3 values}
  get_double_val(fp,&srf);			// Range of first sample in raw data file 1,2 (m)		{calculate from the hdr infomation - 2 values}
  get_double_val(fp,&prf);			// PRF 1,2 (pps)						{calculate from the hdr information - 2 values}
  get_double_val(fp,&dtmp);			// i/q means, i1,q1, i2,q2					15.5 15.5 15.5 15.5
  get_string_val(fp,ctmp);			// Flip i/q (y/n)						s
  get_double_val(fp,&dtmp);			// Desired azimuth resolution (m)				5  (what should this be???)
  get_double_val(fp,&dtmp);			// Number of azimuth looks					4  (what should this be???)
  get_double_val(fp,&fs);			// Range sampling rate (Hz)					22765000
  get_double_val(fp,&chirp_slope);		// Chirp Slope (Hz/s)						5.62130178e11
  get_double_val(fp,&pulse_duration);		// Pulse Duration (s)						33.8e-6
  get_int_val(fp,&itmp);			// Chirp extension points					0
  get_string_val(fp,ctmp);			// Secondary range migration correction (y/n)			n
  get_double_val(fp,&wavelength);		// Radar Wavelength (m) 					0.235
  get_int_val(fp,&itmp);			// Range Spectral Weighting (1.=none, 0.54=Hamming)		1.0
  get_double_val(fp,&dtmp);			// Fraction of range bandwidth to remove			0 0
  get_double_val(fp,&dtmp);			// linear resampling coefs:  sloper, intr, slopea, inta 	0 0 0 0
  get_double_val(fp,&dtmp);			// linear resampling deltas: dsloper, dintr, dslopea, dinta	0 0 0 0
  get_string_val(fp,ctmp);			// AGC file							/dev/null
  get_string_val(fp,dwpfilename);		// DWP file							/dev/null or DWP file

  printf("start_line %i\n",start_line);
  printf("npatches %i\n",npatches);
  printf("patch_size %i\n",patch_size);
  printf("num samples %i\n",ns);
 
  return(0);
}

    
int read_hdrfile(char *infile)
{
  SEASAT_header_ext *hdr;
  FILE *fp = fopen(infile,"r");
  
  int val, i, end_line;
  double dtmp, t, t1;
  
  int clock_drift_hist[MAX_CLOCK_DRIFT];
  int clock_drift_median;
  double clock_shift;
  
  if (fp==NULL) {printf("ERROR: can't open %s header file\n",infile); return(1); }
  hdr = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));

  for (i=0; i<start_line; i++) {
    val = get_values(fp, hdr);
    if (val!=20) {printf("ERROR: unable to read to specified start line in header file\n"); exit(1);}
  }

  station_code = hdr->station_code;

  start_year = 1970 + hdr->lsd_year;
  start_date = hdr->day_of_year;
  start_sec  = (double) hdr->msec / 1000.0;
  
  s_date.year = 1970 + hdr->lsd_year;
  s_date.jd   = hdr->day_of_year;
  dtmp = (double) hdr->msec / 1000.0;
  date_sec2hms(dtmp,&s_time);

  if (USE_CLOCK_DRIFT==1) {
    for (i=0; i<MAX_CLOCK_DRIFT; i++) clock_drift_hist[i] = 0;
    end_line = start_line + npatches*patch_size;
    for (i = start_line; i<end_line; i++) {
      val = get_values(fp, hdr);
      if (val!=20) {printf("ERROR: unable to read to specified end line in header file\n"); exit(1);}
      clock_drift_hist[hdr->clock_drift]++;
    }
    
    printf("APPLYING CLOCK DRIFT TO IMAGE TIMING.\n");
    clock_drift_median = get_median(clock_drift_hist,MAX_CLOCK_DRIFT);
    printf("\tclock_drift_median     = %li \n",clock_drift_median);
    
    clock_shift = (double) clock_drift_median / 1000.0;
    start_sec += clock_shift;
    if (start_sec > 86400.0) {start_sec -= 86400.0; start_date += 1;}

    dtmp = date_hms2sec(&s_time)+clock_shift;
    if (dtmp > 86400.0) { s_date.jd+=1; dtmp-=86400.0;}
    date_sec2hms(dtmp,&s_time);
  }

  printf("Found start time: %i %i %lf\n",start_year, start_date, start_sec);
  fclose(fp);

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

int get_median(int *hist, int size) {
  int retval = -1, max = 0, i;
  for (i=0; i<size; i++) if (hist[i]>max) {max=hist[i]; retval=i;}
  if (retval==-1) { printf("Error getting histogram median value\n"); exit(1); }
  return(retval);
}


void byteswap(void *buf,int len)
{
   unsigned char *bufptr = (unsigned char *) buf;
   unsigned char save1, save2;
   int i, cnt;

   for (i=0; i<len; i++)
    {
        cnt = i*sizeof(float);  
        save1 = bufptr[cnt];
        bufptr[cnt] = bufptr[cnt+3];
        bufptr[cnt+3] = save1;

        save2 = bufptr[cnt+1];
        bufptr[cnt+1] = bufptr[cnt+2];
        bufptr[cnt+2] = save2;
     }   
    return; 
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

void give_usage(int argc, char *argv[])
{
    printf("Usage: %s [-m][-r roi.in_file][-v][-c][-E node] <base_file_name>\n",argv[0]);
    printf("\tbase_file_name\tinput ROI slc file; output ASF .img and .ddr\n");
    printf("\t-m            \tmeta only option - only create meta file\n");
    printf("\t-r roi.in_file\toptional roi.in file name\n");
    printf("\t-v            \tUse state vectors instead of TLEs\n");
    printf("\t-c            \tApply the clock drift to image timing\n");
    printf("\t-E node       \tCreate output file names using ESA node & orbit\n");
    printf("\n");
}
