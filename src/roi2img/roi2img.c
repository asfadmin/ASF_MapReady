/******************************************************************************
NAME:  convert a roi slc file into an ASF img file.

SYNOPSIS:

DESCRIPTION:

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

/* Global variables filled in read_hdrfile and used in main */
double   	start_sec;		// start time of this segment
int      	start_date;		// start date of this segment
int 	   	start_year;		// start year of this segment
julian_date	s_date;			// start date of this segment
hms_time   	s_time;			// start time of this segment
double 		time_offset;		// value from the clock drift

/* Subroutines */
int get_values(FILE *fp,SEASAT_header_ext *s);	// read values from a header file
int read_hdrfile(char *infile);			// read correct values from header file
int read_roi_infile(char *infile);		// read the ROI input file values
void byteswap(void *buf,int len);		// just what it says

/* Subroutines to read values from files - used by read_roi_infile */
int get_string_val(FILE *fp, char *str);
int get_3double_vals(FILE *fp, double *num1, double *num2, double *num3);
int get_double_val(FILE *fp, double *num);
int get_2int_vals(FILE *fp,int *num1,int *num2);
int get_int_val(FILE *fp, int *num);

double r_awgs84 = 6378137.0;
double r_e2wgs84 = 0.00669437999015;
double C = 299792458.0;

main(int argc, char *argv[]) 
{
  FILE *fpin, *fpout;
  
  float ibuff[CPX_PIX*2*LD];
  float obuff[CPX_PIX/LA];
  float b[CPX_PIX];
  float c[CPX_PIX/LA];
  
  int nl;
  int i,j,k,line;
  double t;
  char infile[256], outfile[256], roifile[256], hdrfile[256];
  
  ymd_date date;
  hms_time time;
  meta_parameters *meta;

  char *mon[13]={"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
     "Oct","Nov","Dec"};
  
  char	 dir;			// orbit direction - A or D
  double x, y, z;		// state vector positions at start of segment
  double xdot, ydot, zdot;	// state vector veloctiy at start of segment

  if (argc!=2) {
    printf("Usage: %s <base_file_name>\n",argv[0]);
    printf("\base_file_name\tinput ROI slc file; output ASF .img and .ddr\n");
    exit(1);
  }
  
  strcpy(infile,argv[1]);
  strcat(infile,".slc");
  strcpy(outfile,argv[1]);
  strcat(outfile,".img");
  strcpy(roifile,argv[1]);
  strcat(roifile,".roi.in");
  strcpy(hdrfile,argv[1]);
  strcat(hdrfile,".hdr");
  
  /* Read parameters from the ROI.in file */
  read_roi_infile(roifile);
  nl = npatches * patch_size;
  
  /* Read parameters from the hdr file */
  read_hdrfile(hdrfile);
  
  /* Open the input slc file and output img file*/
  fpin = fopen(infile,"rb");
  if (fpin==NULL) {printf("ERROR: Unable to open input file %s\n",infile); exit(1);}
  fpout = fopen(outfile,"wb");

  /* Take the complex looks from the slc file to create the img file */
  printf("Taking complex looks from file %s to create %s\n",infile,outfile);
  for (line=0; line < nl; line+=LD) {
    if (line%2560==0) printf("\t%i\n",line);
    fread(ibuff,sizeof(float),CPX_PIX*2*LD,fpin);

    /* take looks down */
    for (j=0; j<ns; j++) {
      b[j] = 0;
      for (i=0; i<LD; i++)
        b[j] = b[j] + (ibuff[(2*j)+(i*CPX_PIX*2)]*ibuff[2*j+(i*CPX_PIX*2)]) 
		    + (ibuff[(2*j+1)+(i*CPX_PIX*2)]*ibuff[(2*j+1)+(i*CPX_PIX*2)]);    
    }
    
    /* take looks across */
    for (j=0; j<CPX_PIX/LA; j++) {
      c[j] = 0;
      for (k=0;k<LA;k++)
        c[j] = c[j] + b[j*LA+k];
      c[j] = sqrt(c[j]);
    }
    byteswap(c,CPX_PIX/LA);
    fwrite(c,sizeof(float),CPX_PIX/LA,fpout);
  }
  
  printf("Propagating state vectors to requested time...\n");
  create_input_tle_file(s_date,s_time,"tle1.txt");
  propagate_state_vector("tle1.txt"); 
  printf("\n\nConverting state vectors from ECI to ECEF\n");
  fix_state_vectors(s_date.year,s_date.jd,s_time.hour,s_time.min,s_time.sec);

  printf("Reading first state vector\n");
  FILE *fpvec = fopen("fixed_state_vector.txt","r");
  if (fscanf(fpvec,"%lf %lf %lf %lf %lf %lf %lf\n",&t,&x,&y,&z,&xdot,&ydot,&zdot)!=7) 
    { printf("ERROR: Unable to find state vector in fixed_state_vector.txt file\n"); exit(1); }
  if (zdot > 0.0) dir = 'A'; else dir = 'D';
  fclose(fpvec);
  
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
  
  strcpy(meta->general->basename,argv[1]);
  strcpy(meta->general->sensor,"SEASAT");
  strcpy(meta->general->sensor_name,"SAR");
  strcpy(meta->general->mode,"STD");
  strcpy(meta->general->processor,"ROI301");
  meta->general->data_type = REAL32;
  meta->general->image_data_type = AMPLITUDE_IMAGE;
  meta->general->radiometry = r_AMP;
  sprintf(meta->general->acquisition_date, "%02d-%s-%4d %02d:%02d:%02.0f",
          date.day, mon[date.month], date.year, s_time.hour, s_time.min, s_time.sec);
  meta->general->orbit = time2rev(s_date,s_time);
  meta->general->orbit_direction = dir;
//  meta->general->frame = ????
  meta->general->band_count = 1;
  strcpy(meta->general->bands,"HH");
  meta->general->line_count = nl/LD;
  meta->general->sample_count = ns/LA;
  meta->general->start_line = 0; 
  meta->general->start_sample = 0;
  meta->general->line_scaling = 1;
  meta->general->sample_scaling = 1;
  meta->general->x_pixel_size = (C / (2.0 * fs)) * LA;
  
  double orbit_vel = sqrt(9.81*RE_nadir*RE_nadir / Rsc);
  double swath_vel = orbit_vel * RE_nadir / Rsc;
  
  meta->general->y_pixel_size = (swath_vel * PRI) * LD;    // TAL - Check the sc_vel...
  
//  meta->general->center_latitude = ???
//  meta->general->center_longitude = ???
  
  meta->general->re_major = r_awgs84;
  meta->general->re_minor = r_awgs84 * sqrt(1-r_e2wgs84);
  
//  meta->general->bit_error_rate = ???
//  meta->general->missing_lines = ???  
//  meta->general->no_data = ???

      
  /*Need a SAR block*/
  
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
  
  meta->sar->slant_shift = -1080;			// emperical value from a single delta scene
  meta->sar->time_shift = 0.18;				// emperical value from a single delta scene
  meta->sar->time_shift = 0;
  meta->sar->slant_range_first_pixel = srf;
  meta->sar->wavelength = wavelength;
  meta->sar->prf = prf;
  
  meta->sar->earth_radius = RE_nadir;			// This really should be the radius at the center of the image
  							// and not the radius at the nadir point...  how to fix this???
  meta->sar->satellite_height = Rsc;
  meta->sar->range_doppler_coefficients[0] = dop1;
  meta->sar->range_doppler_coefficients[1] = dop2;
  meta->sar->range_doppler_coefficients[2] = dop3;
  meta->sar->azimuth_doppler_coefficients[0] = dop1;
  meta->sar->azimuth_doppler_coefficients[1] = 0;
  meta->sar->azimuth_doppler_coefficients[2] = 0;
  
///  meta->sar->azimuth_processing_bandwidth = ????
  
  meta->sar->chirp_rate = chirp_slope;
  meta->sar->pulse_duration = pulse_duration;
  meta->sar->range_sampling_rate = 1/fs;
  strcpy(meta->sar->polarization,"HH");
  meta->sar->multilook = 1;
  meta->sar->pitch = 0;
  meta->sar->roll = 0;
  meta->sar->yaw = 0;
///  meta->sar->incid_a[0-5] = ???

  printf("Writing out the meta file\n");

  meta_write(meta, argv[1]);

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
  get_string_val(fp,ctmp);			// First input data file					<infile>.dat				               
  get_string_val(fp,ctmp);			// Second input data file  				        /dev/null
  get_string_val(fp,datfilename);		// Output data file						<infile>.slc
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
  
  int val, i;
  double dtmp, t, t1;
  double x1, y1, z1;
  double xdot1, ydot1, zdot1;

  
  if (fp==NULL) {printf("ERROR: can't open %s header file\n",infile); return(1); }
  hdr = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));

  for (i=0; i<start_line; i++) {
    val = get_values(fp, hdr);
    if (val!=20) {printf("ERROR: unable to read to specified start line in header file\n"); exit(1);}
  }

  time_offset = hdr->clock_drift / 1000.0 ;
  // time_offset = 0.0;

  start_year = 1970 + hdr->lsd_year;
  start_date = hdr->day_of_year;
  start_sec  = (double) hdr->msec / 1000.0; //  + time_offset;
  
  printf("Found start time: %i %i %lf\n",start_year, start_date, start_sec);

  s_date.year = 1970 + hdr->lsd_year;
  s_date.jd   = hdr->day_of_year;
  dtmp = (double) hdr->msec / 1000.0; //  + time_offset;
  date_sec2hms(dtmp,&s_time);
  
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
  printf("read int %s: ",tmp);
  sscanf(tmp,"%i",num);
  printf("got value %i\n",*num);
  return(1);
}

int get_2int_vals(FILE *fp,int *num1,int *num2)
{
  char tmp[256];
  fgets(tmp,255,fp);
  printf("read 2 ints %s: ",tmp);
  sscanf(tmp,"%i %i",num1,num2);
  printf("got values %i %i\n",*num1,*num2);
  return(1);
}

int get_double_val(FILE *fp, double *num)
{
  char tmp[256];
  fgets(tmp,255,fp);
  printf("read double %s: ",tmp);
  sscanf(tmp,"%lf",num);
  printf("got value %lf\n",*num);
  return(1);
}

int get_3double_vals(FILE *fp, double *num1, double *num2, double *num3)
{
  char tmp[256];
  fgets(tmp,255,fp);
  printf("read 3 doubles %s: ",tmp);
  sscanf(tmp,"%lf %lf %lf",num1,num2,num3);
  printf("got values %lf %lf %lf\n",*num1,*num2,*num3);
  return(1);
}

int get_string_val(FILE *fp, char *str)
{
  char tmp[256];
  fgets(tmp,255,fp);
  sscanf(tmp,"%s",str);
  printf("read string %s\n",str);
  return(1);
}




