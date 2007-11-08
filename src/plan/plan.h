#ifndef PLAN_H
#define PLAN_H

#include "polygon.h"
#include "asf_meta.h"

/*  This is the external interface to the planner  */

// These are the options for the "pass_type"
#define ASCENDING_OR_DESCENDING 0
#define ASCENDING_ONLY 1
#define DESCENDING_ONLY 2

// Inputs:
//   satellite:    ERS-1, ERS-2, ALOS, etc
//   beam_mode: 
//   startdate:    e.g. 20070101
//   enddate:      e.g. 20071231
//   min_lat:      
//   max_lat:
//   clat,clon:    center of bounding box
//   pass_type:    as above
//   aoi:          utm coordinates for the target region
//   tle_filename: TLE file
// Output:
//   outFile:   ...
// Returns the number of acquisitions found

int plan(const char *satellite, const char *beam_mode,
         long startdate, long enddate, double min_lat, double max_lat,
         double clat, double clon, int pass_type, Polygon *aoi,
         const char *tle_filename, const char *outFile);

#endif
