/******************************************************************************
NAME:  cdpf_ingest -  Convert raw CEOS file into processed CEOS frames

SYNOPSIS:  cdpf_ingest <base_file_name>

DESCRIPTION:  Turns a CEOS raw swath into CEOS amp frames

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    focus rd		Used to process the raw SAR into images
    foucs cc		Used to convert files into CEOS format
    asf_import		Convert CEOS file into internal format

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    rd_full		this is the name of the full swath

PROGRAM HISTORY:
    VERS:   DATE:    AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    10/14    T. Logan     ingest KSAT files into ASF CEOS format
    
HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

	- Process the entire swath into rd_full image
	- Ingest the rd_full image into internal ASF format
	- Read the rd_full metadata file
	- Calculate the start and end frames to process from this swath
	- For each frame
		call focus rd
		call focus cc

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <asf_meta.h>
#include <asf_license.h>

void give_usage(int argc,char* argv[]);
void get_min_max_lat(meta_parameters *meta, double *min_lat, double *max_lat);
int asf_frame_calc(char *sensor, float latitude, char orbit_direction);

/*
 float ers_frame[901] =
  {-99,0.40,0.80,1.20,1.60,2.00,2.40,2.80,3.20,3.60,4.00,4.40,4.80,5.20,5.60,6.00,
   6.40,6.80,7.20,7.60,8.00,8.40,8.80,9.20,9.60,10.00,10.40,10.80,11.20,11.60,12.00,
   12.40,12.80,13.20,13.60,14.00,14.40,14.80,15.20,15.60,16.00,16.40,16.80,17.20,17.60,18.00,
   18.40,18.80,19.20,19.60,20.00,20.40,20.80,21.20,21.60,22.00,22.40,22.80,23.20,23.60,24.00,
   24.40,24.80,25.20,25.60,26.00,26.40,26.80,27.20,27.60,28.00,28.40,28.80,29.20,29.60,30.00,
   30.40,30.80,31.20,31.60,32.00,32.40,32.80,33.20,33.60,34.00,34.40,34.80,35.20,35.60,36.00,
   36.40,36.80,37.20,37.60,38.00,38.40,38.80,39.20,39.60,40.00,40.40,40.80,41.20,41.60,42.00,
   42.40,42.80,43.20,43.60,44.00,44.40,44.80,45.20,45.60,46.00,46.40,46.80,47.20,47.60,48.00,
   48.40,48.80,49.20,49.60,50.00,50.40,50.80,51.20,51.60,52.00,52.40,52.80,53.20,53.60,54.00,
   54.40,54.80,55.20,55.60,56.00,56.40,56.80,57.20,57.60,58.00,58.40,58.80,59.20,59.60,60.00,
   60.39,60.77,61.16,61.54,61.92,62.31,62.69,63.08,63.47,63.84,64.23,64.61,64.99,65.38,65.76,
   66.14,66.52,66.90,67.29,67.67,68.04,68.42,68.80,69.18,69.55,69.93,70.31,70.68,71.06,71.43,
   71.80,72.17,72.55,72.91,73.31,73.68,74.06,74.43,74.80,75.17,75.55,75.91,76.19,76.54,76.90,
   77.25,77.59,77.94,78.28,78.62,78.96,79.29,79.62,79.94,80.26,80.57,80.88,81.18,81.47,81.75,
   82.03,82.29,82.54,82.79,83.01,83.22,83.42,83.60,83.76,83.89,84.01,84.10,84.16,84.20,84.21,
   84.20,84.16,84.10,84.01,83.89,83.76,83.60,83.42,83.22,83.01,82.79,82.54,82.29,82.03,81.75,
   81.47,81.18,80.88,80.57,80.26,79.94,79.62,79.29,78.96,78.62,78.28,77.94,77.59,77.25,76.90,
   76.54,76.19,75.91,75.55,75.17,74.80,74.43,74.06,73.68,73.31,72.91,72.55,72.17,71.80,71.43,
   71.06,70.68,70.31,69.93,69.55,69.18,68.80,68.42,68.04,67.67,67.29,66.90,66.52,66.14,65.76,
   65.38,64.99,64.61,64.23,63.84,63.47,63.08,62.69,62.31,61.92,61.54,61.16,60.77,60.39,60.00,
   59.60,59.20,58.80,58.40,58.00,57.60,57.20,56.80,56.40,56.00,55.60,55.20,54.80,54.40,54.00,
   53.60,53.20,52.80,52.40,52.00,51.60,51.20,50.80,50.40,50.00,49.60,49.20,48.80,48.40,48.00,
   47.60,47.20,46.80,46.40,46.00,45.60,45.20,44.80,44.40,44.00,43.60,43.20,42.80,42.40,42.00,
   41.60,41.20,40.80,40.40,40.00,39.60,39.20,38.80,38.40,38.00,37.60,37.20,36.80,36.40,36.00,
   35.60,35.20,34.80,34.40,34.00,33.60,33.20,32.80,32.40,32.00,31.60,31.20,30.80,30.40,30.00,
   29.60,29.20,28.80,28.40,28.00,27.60,27.20,26.80,26.40,26.00,25.60,25.20,24.80,24.40,24.00,
   23.60,23.20,22.80,22.40,22.00,21.60,21.20,20.80,20.40,20.00,19.60,19.20,18.80,18.40,18.00,
   17.60,17.20,16.80,16.40,16.00,15.60,15.20,14.80,14.40,14.00,13.60,13.20,12.80,12.40,12.00,
   11.60,11.20,10.80,10.40,10.00,9.60,9.20,8.80,8.40,8.00,7.60,7.20,6.80,6.40,6.00,
   5.60,5.20,4.80,4.40,4.00,3.60,3.20,2.80,2.40,2.00,1.60,1.20,0.80,0.40,0.00,
   -0.40,-0.80,-1.20,-1.60,-2.00,-2.40,-2.80,-3.20,-3.60,-4.00,-4.40,-4.80,-5.20,-5.60,-6.00,
   -6.40,-6.80,-7.20,-7.60,-8.00,-8.40,-8.80,-9.20,-9.60,-10.00,-10.40,-10.80,-11.20,-11.60,-12.00,
   -12.40,-12.80,-13.20,-13.60,-14.00,-14.40,-14.80,-15.20,-15.60,-16.00,-16.40,-16.80,-17.20,-17.60,-18.00,
   -18.40,-18.80,-19.20,-19.60,-20.00,-20.40,-20.80,-21.20,-21.60,-22.00,-22.40,-22.80,-23.20,-23.60,-24.00,
   -24.40,-24.80,-25.20,-25.60,-26.00,-26.40,-26.80,-27.20,-27.60,-28.00,-28.40,-28.80,-29.20,-29.60,-30.00,
   -30.40,-30.80,-31.20,-31.60,-32.00,-32.40,-32.80,-33.20,-33.60,-34.00,-34.40,-34.80,-35.20,-35.60,-36.00,
   -36.40,-36.80,-37.20,-37.60,-38.00,-38.40,-38.80,-39.20,-39.60,-40.00,-40.40,-40.80,-41.20,-41.60,-42.00,
   -42.40,-42.80,-43.20,-43.60,-44.00,-44.40,-44.80,-45.20,-45.60,-46.00,-46.40,-46.80,-47.20,-47.60,-48.00,
   -48.40,-48.80,-49.20,-49.60,-50.00,-50.40,-50.80,-51.20,-51.60,-52.00,-52.40,-52.80,-53.20,-53.60,-54.00,
   -54.40,-54.80,-55.20,-55.60,-56.00,-56.40,-56.80,-57.20,-57.60,-58.00,-58.40,-58.80,-59.20,-59.60,-60.00,
   -60.39,-60.77,-61.16,-61.54,-61.92,-62.31,-62.69,-63.08,-63.47,-63.84,-64.23,-64.61,-64.99,-65.38,-65.76,
   -66.14,-66.52,-66.90,-67.29,-67.67,-68.04,-68.42,-68.80,-69.18,-69.55,-69.93,-70.31,-70.68,-71.06,-71.43,
   -71.80,-72.17,-72.55,-73.91,-70.31,-70.68,-71.06,-71.43,-71.80,-75.17,-72.55,-72.91,-76.19,-76.54,-76.90,
   -77.25,-77.59,-77.94,-78.28,-78.62,-78.96,-79.29,-79.62,-79.94,-80.26,-80.57,-80.88,-81.18,-81.47,-81.75,
   -82.03,-82.29,-82.54,-82.79,-83.01,-83.22,-83.42,-83.60,-83.76,-83.89,-84.01,-84.10,-84.16,-84.20,-84.21
   -84.21,-84.20,-84.16,-84.10,-84.01,-83.89,-83.76,-83.60,-83.42,-83.22,-83.01,-82.79,-82.54,-82.29,-82.03,
   -81.75,-81.47,-81.18,-80.88,-80.57,-80.26,-79.94,-79.62,-79.29,-78.96,-78.62,-78.28,-77.94,-77.59,-77.25,
   -76.90,-76.54,-76.19,-72.91,-72.55,-75.17,-71.80,-71.43,-71.06,-70.68,-70.31,-73.91,-72.55,-72.17,-71.80,
   -71.43,-71.06,-70.68,-70.31,-69.93,-69.55,-69.18,-68.80,-68.42,-68.04,-67.67,-67.29,-66.90,-66.52,-66.14,
   -65.76,-65.38,-64.99,-64.61,-64.23,-63.84,-63.47,-63.08,-62.69,-62.31,-61.92,-61.54,-61.16,-60.77,-60.39,
   -60.00,-59.60,-59.20,-58.80,-58.40,-58.00,-57.60,-57.20,-56.80,-56.40,-56.00,-55.60,-55.20,-54.80,-54.40,
   -54.00,-53.60,-53.20,-52.80,-52.40,-52.00,-51.60,-51.20,-50.80,-50.40,-50.00,-49.60,-49.20,-48.80,-48.40,
   -48.00,-47.60,-47.20,-46.80,-46.40,-46.00,-45.60,-45.20,-44.80,-44.40,-44.00,-43.60,-43.20,-42.80,-42.40,
   -42.00,-41.60,-41.20,-40.80,-40.40,-40.00,-39.60,-39.20,-38.80,-38.40,-38.00,-37.60,-37.20,-36.80,-36.40,
   -36.00,-35.60,-35.20,-34.80,-34.40,-34.00,-33.60,-33.20,-32.80,-32.40,-32.00,-31.60,-31.20,-30.80,-30.40,
   -30.00,-29.60,-29.20,-28.80,-28.40,-28.00,-27.60,-27.20,-26.80,-26.40,-26.00,-25.60,-25.20,-24.80,-24.40,
   -24.00,-23.60,-23.20,-22.80,-22.40,-22.00,-21.60,-21.20,-20.80,-20.40,-20.00,-19.60,-19.20,-18.80,-18.40,
   -18.00,-17.60,-17.20,-16.80,-16.40,-16.00,-15.60,-15.20,-14.80,-14.40,-14.00,-13.60,-13.20,-12.80,-12.40,
   -12.00,-11.60,-11.20,-10.80,-10.40,-10.00,-9.60,-9.20,-8.80,-8.40,-8.00,-7.60,-7.20,-6.80,-6.40,-6.00,
   -5.60,-5.20,-4.80,-4.40,-4.00,-3.60,-3.20,-2.80,-2.40,-2.00,-1.60,-1.20,-0.80,-0.40,0.00};
*/

extern float ers_frame[];

main(int argc, char *argv[])
{
  meta_parameters *meta;
  char raw_file[256];
  char raw_img[256];
  char cmd[1024];
  char filename[256];
  double min_lat=90.0, max_lat=-90.0;
  double time;
  char ASC;
  int  first_frame, last_frame;
  int  i, err;
  float minl, maxl;
  double mid_lat, mid_lon;
  double EPSILON = 0.01;


  if (argc != 2) { give_usage(argc,argv); exit(1); }

  strcpy(raw_file,argv[1]);

  if (access("rd_full.img",F_OK) == -1) {  /* create rd_full file if it doesn't exist */
    sprintf(cmd,"focus rd %s.raw %s.ldr rd_full.gli rd_full.gli.par -pro SGF",raw_file, raw_file);
    err = system(cmd);
    if (err!=0) {printf("ERROR: last command returned %i; exiting\n",err); exit(1);}

    sprintf(cmd,"focus cc -i rd_full.gli -p rd_full.gli.par -his %s.raw.his -pro SGF -dat rd_full.dat -lea rd_full.ldr -tra rd_full.tra -vol rd_full.vol -nul rd_full.null -log rd_full.log", raw_file);
    err = system(cmd);
    if (err!=0) {printf("ERROR: last command returned %i; exiting\n",err); exit(1);}

    sprintf(cmd,"asf_import rd_full rd_full");
    err = system(cmd);
    if (err!=0) {printf("ERROR: last command returned %i; exiting\n",err); exit(1);}
  }
 
  meta = meta_read("rd_full");
  printf("Read metadata file\n");

  get_min_max_lat(meta, &min_lat, &max_lat);
  printf("\nminimum lat: %lf\n",min_lat);
  printf("maximum lat: %lf\n",max_lat);

  time = meta_get_time(meta,meta->general->line_count/2,meta->general->sample_count/2);
  stateVector vec = meta_get_stVec(meta,time);
  if (vec.vel.z >= 0) ASC = 'A';
  else ASC = 'D';

  printf("This pass is "); if (ASC=='D') printf("descending\n"); else printf("ascending\n");

  minl = (float) min_lat+0.175; /* add 1/2 a frame */
  maxl = (float) max_lat-0.175; /* subtract 1/2 a frame */
  first_frame = asf_frame_calc("RSAT",minl,ASC);
  last_frame = asf_frame_calc("RSAT",maxl,ASC);

  if (first_frame > last_frame) {
    int tmp = first_frame;
    first_frame = last_frame;
    last_frame = tmp;
  }

  printf("First frame is %i\n",first_frame);
  printf("Last frame is %i\n",last_frame);

  for (i=first_frame; i<=last_frame; i++)
   { 

     printf("Creating frame #%i with center at %lf\n",i,ers_frame[i]);
     sprintf(filename,"R1_%.5i_ST5_F%.3i",meta->general->orbit,i);
     sprintf(cmd,"focus rd %s.raw %s.ldr %s.gli %s.gli.par -center_option LAT -center %lf -duration_option DIS -duration 100 -pro SGF", raw_file, raw_file, filename, filename, ers_frame[i]);
     printf("Running command: %s\n",cmd);
     err = system(cmd);
    if (err!=0) {printf("WARNING: last command returned %i\n",err);}
     sprintf(cmd,"focus cc -i %s.gli -p %s.gli.par -his %s.raw.his -pro SGF -dat %s.D -lea %s.L -tra %s.tra -vol %s.vol -nul %s.nul -log %s.log", filename,filename,raw_file,filename,filename,filename,filename,filename,filename);
     printf("Running command: %s\n",cmd);
     err = system(cmd);
    if (err!=0) {printf("WARNING: last command returned %i\n",err);}
   }

  /* Check the last frame to make sure the geolocations are correct */
  sprintf(cmd,"asf_import %s.D %s",filename,filename); err = system(cmd);
  if (err!=0) {printf("WARNING: last command returned %i, exiting\n",err); exit(1); }
  meta = meta_read(filename);
  printf("Read last metadata file\n");
  meta_get_latLon(meta,meta->general->line_count/2,meta->general->sample_count/2,0,&mid_lat,&mid_lon);
  
  if ( fabs(ers_frame[last_frame]-mid_lat) > EPSILON)  {
    char tmpname[256];
    printf("WARNING: Last frame geolocation suspect!!!\n");
    printf("WARNING: ERS_FRAME = %lf, Actual = %lf\n",ers_frame[last_frame],mid_lat);
    printf("WARNING: Deleting last frame that was created\n");
    sprintf(tmpname,"%s.D",filename); remove(tmpname);
    sprintf(tmpname,"%s.L",filename); remove(tmpname);
    sprintf(tmpname,"%s.tra",filename); remove(tmpname);
    sprintf(tmpname,"%s.vol",filename); remove(tmpname);
    sprintf(tmpname,"%s.nul",filename); remove(tmpname);
  }

  exit(0);

}

void get_min_max_lat(meta_parameters *meta, double *min_lat, double *max_lat)
{
  double near_start_lat, far_start_lat, near_end_lat, far_end_lat;
  double near_start_lon, far_start_lon, near_end_lon, far_end_lon;
  double mid_start_lat, mid_end_lat;
  double mid_start_lon, mid_end_lon;
  double mid_lat, mid_lon;

/*
  meta_get_latLon(meta,0,0,0, &near_start_lat, &near_start_lon);
  meta_get_latLon(meta,0,meta->general->sample_count,0, &far_start_lat, &far_start_lon);
  meta_get_latLon(meta,meta->general->line_count,0,0, &near_end_lat, &near_end_lon);
  meta_get_latLon(meta,meta->general->line_count,meta->general->sample_count,0,&far_end_lat,&far_end_lon);

  printf("near start: %lf %lf\n",near_start_lat,near_start_lon);
  printf("far  start: %lf %lf\n",far_start_lat,far_start_lon);
  printf("near   end: %lf %lf\n",near_end_lat,near_end_lon);
  printf("far    end: %lf %lf\n",far_end_lat,far_end_lon);
*/

  meta_get_latLon(meta,0,meta->general->sample_count/2,0,&mid_start_lat,&mid_start_lon);
  meta_get_latLon(meta,meta->general->line_count/2,meta->general->sample_count/2,0,&mid_lat,&mid_lon);
  meta_get_latLon(meta,meta->general->line_count,meta->general->sample_count/2,0,&mid_end_lat,&mid_end_lon);

  printf("\nCenter: %lf %lf\n",mid_lat, mid_lon);
/*
  printf("\nMiddle Start Lat: %lf\n",mid_start_lat);
  printf("\nMiddle End Lat: %lf\n",mid_end_lat);
*/

/*
  *max_lat = (near_start_lat>*max_lat) ? near_start_lat : *max_lat;
  *max_lat = (far_start_lat>*max_lat) ? far_start_lat : *max_lat;
  *max_lat = (near_end_lat>*max_lat) ? near_end_lat : *max_lat;
  *max_lat = (far_end_lat>*max_lat) ? far_end_lat : *max_lat;

  *min_lat = (near_start_lat<*min_lat) ? near_start_lat : *min_lat;
  *min_lat = (far_start_lat<*min_lat) ? far_start_lat : *min_lat;
  *min_lat = (near_end_lat<*min_lat) ? near_end_lat : *min_lat;
  *min_lat = (far_end_lat<*min_lat) ? far_end_lat : *min_lat;
*/

  *max_lat = (mid_start_lat>*max_lat) ? mid_start_lat : *max_lat;
  *max_lat = (mid_end_lat>*max_lat) ? mid_end_lat : *max_lat;

  *min_lat = (mid_start_lat<*min_lat) ? mid_start_lat : *min_lat;
  *min_lat = (mid_end_lat<*min_lat) ? mid_end_lat : *min_lat;
}

void give_usage(int argc, char *argv[])
{
  printf("Usage:  %s <filename>\n",argv[0]);
  exit(1);
}

