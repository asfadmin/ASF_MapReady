/******************************************************************************
NAME: 

SYNOPSIS: 

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0	    5/13   T. Logan     Seasat Processing Software - ASF
    
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

#define CENTER_SAMPLE 3150	// this should be refined, but will never really be a "constant"
  				// this is because of the variability in the sr2gr mapping
				// also, because we don't know exactly how much we'll chop in far range

#define HALF_ESA_FRAME 12468	// this should be almost perfect, as follows:
				//   Swath_Length = 4.01 m/line * 24936 lines = 99993.36 meters. 

#define NODES_PER_FRAME 9

double get_lat_from_node(int node);

int get_line_for_node(meta_parameters *meta, int node) 
{
  char metafile[256];
  double test_lat, test_lon;
  int    closest_line;
  double closest_lat;
  double max_diff;
  double plat_min, plat_max, plon_min, plon_max;
  double lat;
  int    line;
  double time;
  int    ASC;

  meta_get_bounding_box(meta, &plat_min, &plat_max, &plon_min, &plon_max);

  lat = get_lat_from_node(node);
  if (lat < plat_min || lat > plat_max) { 
//    printf("ERROR: Data does not overlap with node %i\n",node);
//    printf("\tnode center Lat = %lf\n",lat);
//    printf("\tScene min    Lat = %lf\n",plat_min);
//    printf("\tScene end    Lat = %lf\n\n",plat_max);
    return(-1);
  }
  
  max_diff = 180;
  for (line = 0; line < meta->general->line_count; line++)  {
      meta_get_latLon(meta,line,CENTER_SAMPLE,0,&test_lat, &test_lon);
      time = meta_get_time(meta,line,CENTER_SAMPLE);
      stateVector vec = meta_get_stVec(meta,time);
      if (vec.pos.z >= 0) ASC = 1;
      else ASC = 0;
      
      if (ASC == 1 && node >= 1800) continue;  // nodes >= 1800 are descending
      if (ASC == 0 && node < 1800)  continue;  // nodes < 1800 are asccending
      
//      printf("at line %i: lat = %lf\n",line,test_lat);
      
      if ( fabs(test_lat - lat) < max_diff) {
        closest_lat = lat;
	closest_line = line;
	max_diff = fabs(test_lat - lat);
      }
  }
  
  closest_line *= meta->sar->azimuth_look_count;
//  printf("Found lat %lf at line %i (diff = %lf)\n",closest_lat,closest_line,max_diff);
  
  if (meta->sar->azimuth_time_per_pixel >= 0.0)
    line = closest_line - HALF_ESA_FRAME;
  else
    line = meta->sar->original_line_count-closest_line-HALF_ESA_FRAME;
  
//  if (line < 0) {printf("ERROR: not able to fit this frame in the swath\n"); exit(-1);}
//  else {printf("Creating roi file at line %i\n",line);}

  return(line);
}


double get_lat_from_node(int node) {
    double retlat;
    
    if (node < 1800) {
       retlat = (double)(node*.05);
    } else if (node < 3600) {
       retlat = (double)(180.0-(node*0.05));
    } else if (node < 5400) {
       retlat = (double)(0.0-((node-5400.0)*0.05));
    } else if (node < 7200) {
       retlat = (double)(-180.0+((node-5400.0)*0.05));
    } else {
       printf("ERROR: Invalid ESA node requested: %i\n",node);
       exit(-1);
    }
    
    return(retlat);
}   



