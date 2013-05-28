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

double get_lat_from_node(int node);

/* Returns the node closest to the line in question */
int get_node_for_line(meta_parameters *meta, int line, int nl) 
{
  char metafile[256];
  double test_lat, test_lon;
  int    closest_node;
  double closest_lat;
  double max_diff;
  double lat;
  double time;
  int    ASC;
  int    node;

  if (meta->sar->azimuth_time_per_pixel < 0.0) line = nl - line;
  line = line / meta->sar->azimuth_look_count;

  meta_get_latLon(meta,line,CENTER_SAMPLE,0,&test_lat,&test_lon);
  time = meta_get_time(meta,line,CENTER_SAMPLE);
  stateVector vec = meta_get_stVec(meta,time);
  if (vec.vel.z >= 0) ASC = 1;
  else ASC = 0;
  max_diff = 180;

  printf("\tline = %i, lat = %lf, ASC = %i\n",line,test_lat,ASC);

  if (ASC == 1) {
    for (node = 0; node < 1800; node++) {
      lat = get_lat_from_node(node);
      if ((test_lat-lat)< max_diff && lat<test_lat) {  // Ascending, take node BEFORE this line
        closest_lat = lat;
	closest_node = node;
	max_diff = fabs(lat - test_lat);
      }
    }
  } else {
    for (node = 1800; node < 3600; node++) {
      lat = get_lat_from_node(node);
      if ((lat-test_lat) < max_diff && lat>test_lat) {  // Descending, take node AFTER this line
        closest_lat = lat;
	closest_node = node;
	max_diff = fabs(lat - test_lat);
      }
    }
  }

  return(closest_node);
}

/* Returns the start line for the given node center, -1 if not found */
int get_line_for_node(meta_parameters *meta, int node, int nl) 
{
  char metafile[256];
  double test_lat, test_lon;
  int    closest_line;
  double closest_lat;
  double max_diff;
  double lat;
  int    line;
  double time;
  int    ASC;

  lat = get_lat_from_node(node);
  max_diff = 180;
  for (line = 0; line < meta->general->line_count; line++)  {
      meta_get_latLon(meta,line,CENTER_SAMPLE,0,&test_lat, &test_lon);
      time = meta_get_time(meta,line,CENTER_SAMPLE);
      stateVector vec = meta_get_stVec(meta,time);
      if (vec.vel.z >= 0) ASC = 1;
      else ASC = 0;
      
      if (ASC == 1 && node >= 1800) continue;  // nodes >= 1800 are descending
      if (ASC == 0 && node < 1800)  continue;  // nodes < 1800 are asccending
      
      if ( fabs(test_lat - lat) < max_diff) {
        closest_lat = lat;
	closest_line = line;
	max_diff = fabs(test_lat - lat);
      }
  }
  
  closest_line *= meta->sar->azimuth_look_count;
  if (meta->sar->azimuth_time_per_pixel >= 0.0) line = closest_line - HALF_ESA_FRAME;
  else line = nl-closest_line-HALF_ESA_FRAME;

  printf("\tfound line %i for node %i\n",line,node);
  fflush(NULL);
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



