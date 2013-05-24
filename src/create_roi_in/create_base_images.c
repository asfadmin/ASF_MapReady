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
#define NODES_PER_FRAME 9

int get_line_for_node(meta_parameters *meta,int node);

main(int argc, char *argv[])
{
  char basefile[256], tmpfile[256];
  double min_lat, max_lat;
  double min_lon, max_lon;
  int node;
  int start_line;
  char cmd[256];
  char metafile[256];
  int err=0;
  
  if (argc != 2) { printf("Usage: %s <swath_file>\n",argv[0]); exit(1); }
  strcpy(basefile,argv[1]);
  strcat(strcpy(metafile,basefile),".meta");
  meta_parameters *meta = meta_read(metafile);

  printf("Searching file %s for node overlaps\n",basefile);
  
  for (node = 0; node < MAX_NODE; node+=NODES_PER_FRAME) // loop through all possible nodes
    if ((start_line=get_line_for_node(meta,node))>0)     // valid node for this swath
     {
        printf("FOUND VALID NODE %i starting at line %i, Processing...\n",node,start_line);
     
	sprintf(cmd,"create_roi_in -E %i %s\n",node,basefile);
	err = system(cmd);
	if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
	
	sprintf(tmpfile,"%s_node%.4i.roi.in",basefile,node);
	sprintf(cmd,"roi < %s\n",tmpfile);
	err = system(cmd);
	if (err) {printf("Error returned from ROI\n"); exit(1);}

	sprintf(tmpfile,"%s_node%.4i",basefile,node);
	sprintf(cmd,"roi2img -E %i %s\n",node,tmpfile);
	err = system(cmd);
	if (err) {printf("Error returned from roi2img\n"); exit(1);}
    }

  exit(0);
}
