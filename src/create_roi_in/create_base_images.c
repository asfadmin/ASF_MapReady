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
#define PATCH_SIZE	8132
#define NUM_PATCHES	3
#define HALF_ESA_FRAME  12468	// this should be almost perfect, as follows:
				//   Swath_Length = 4.01 m/line * 24936 lines = 99993.36 meters. 


int get_line_for_node(meta_parameters *meta, int node, int nl);
int get_node_for_line(meta_parameters *meta, int line, int nl);


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
  FILE *fphdr;
  SEASAT_header_ext *hdr;
  int  nl, line, lines_per_frame;
  int val;
  
  if (argc != 2) { printf("Usage: %s <swath_file>\n",argv[0]); exit(1); }
  strcpy(basefile,argv[1]);
  strcat(strcpy(metafile,basefile),".meta");

  /* create the swath meta file */
  if (!fileExists(metafile)) {
    printf("Creating meta file for the swath\n\n");
    sprintf(cmd,"create_roi_in %s\n",basefile);
    err = system(cmd);
    if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
    sprintf(cmd,"roi2img -m %s\n",basefile);
    err = system(cmd);
    if (err) {printf("Error returned from roi2img\n"); exit(1);}
    sprintf(tmpfile,"%s.roi.in",basefile);
    remove(tmpfile);
  }
    
  meta_parameters *meta = meta_read(metafile);

  /* get total lines in this file */  
  sprintf(tmpfile,"%s.hdr",basefile);
  if ((fphdr=fopen(tmpfile,"r"))==NULL) {printf("Error opening input file %s\n",tmpfile); exit(1);}
  hdr = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));
  val = get_values(fphdr, hdr); nl = 0;
  while (val==20) { nl++; val = get_values(fphdr, hdr); }
  fclose(fphdr);

  printf("Searching file %s for node overlaps\n",basefile);
  lines_per_frame = NUM_PATCHES * PATCH_SIZE;
  
  for (line = HALF_ESA_FRAME; line < nl-HALF_ESA_FRAME; line += 0.85*lines_per_frame) {
  
    printf("@ line %i\n",line);
    node=get_node_for_line(meta,line,nl);		// find the closest node
    printf("\tfound node %i\n",node);
    start_line = get_line_for_node(meta,node,nl);	// find the line for chosen node		
    printf("\tfound start_line %i\n",start_line);
    
    if (start_line>=0)		
     {
        printf("FOUND VALID NODE %i starting at line %i, Processing...\n",node,start_line);
     
	sprintf(cmd,"create_roi_in -E %i %s\n",node,basefile);
	printf("Executing command: %s\n",cmd);
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
  }

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
