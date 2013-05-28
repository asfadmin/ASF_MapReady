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

void process_node(char *basefile, int node);
int get_line_for_node(meta_parameters *meta, int node, int nl);
int get_node_for_line(meta_parameters *meta, int line, int nl);

main(int argc, char *argv[])
{
  char basefile[256], tmpfile[256];
  int node, start_line;
  char cmd[256];
  char metafile[256];
  int err=0;
  FILE *fphdr;
  SEASAT_header_ext *hdr;
  int  nl, middle_line, lines_per_frame, val;
  
  if (argc != 2) { printf("Usage: %s <swath_file>\n",argv[0]); exit(1); }
  strcpy(basefile,argv[1]);
  strcat(strcpy(metafile,basefile),".meta");

  printf("================================================================================\n");
  printf("%s: CREATING SEASAT PRODUCTS FROM SWATH FILE %s\n",argv[0],basefile);
  printf("================================================================================\n");

  /* create the swath meta file */
  if (!fileExists(metafile)) {
    printf("Creating meta file for the swath\n\n");
    sprintf(cmd,"create_roi_in %s\n",basefile); err=system(cmd); if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
    sprintf(cmd,"roi2img -m %s\n",basefile); err=system(cmd); if (err) {printf("Error returned from roi2img\n"); exit(1);}
    sprintf(tmpfile,"%s.roi.in",basefile); remove(tmpfile);
  }
  meta_parameters *meta = meta_read(metafile);

  /* get total lines in this file */  
  sprintf(tmpfile,"%s.hdr",basefile);
  if ((fphdr=fopen(tmpfile,"r"))==NULL) {printf("Error opening input file %s\n",tmpfile); exit(1);}
  hdr = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));
  val = get_values(fphdr, hdr); nl = 0;
  while (val==20) { nl++; val = get_values(fphdr, hdr); }
  fclose(fphdr);
  lines_per_frame = NUM_PATCHES * PATCH_SIZE;

  /* Search node by node until we hit the first processable node */
  start_line = -1;
  middle_line = HALF_ESA_FRAME;
  node=get_node_for_line(meta,middle_line,nl)-1;	// find the closest node to middle of swath
  while (start_line < 0) {
    node++;
    start_line = get_line_for_node(meta,node,nl);	// find the start line for chosen node
  }
  middle_line = start_line + HALF_ESA_FRAME;

  /* Now process all frames until the end of the file */
  while (middle_line < nl) {
    if (start_line >= 0) {
      printf("================================================================================\n");
      printf("FOUND VALID NODE %i starting at line %i, Processing...\n",node,start_line);
      process_node(basefile,node);
      printf("================================================================================\n");
    }
    middle_line += 0.85*lines_per_frame;		// move to next frame location
    node=get_node_for_line(meta,middle_line,nl);	// find the closest node
    start_line = get_line_for_node(meta,node,nl);	// find the start line for chosen node
  } 

  exit(0);
}

/* Call create_roi_in, roi, and roi2img for the given basefile and node */
void process_node(char *basefile, int node)
{
  char cmd[256],tmpfile[256];
  int  err;
  
  sprintf(cmd,"create_roi_in -E %i %s\n",node,basefile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
	
  sprintf(tmpfile,"%s_node%.4i.roi.in",basefile,node);
  sprintf(cmd,"roi < %s\n",tmpfile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from ROI\n"); exit(1);}

  sprintf(tmpfile,"%s_node%.4i",basefile,node);
  sprintf(cmd,"roi2img -E %i %s\n",node,tmpfile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from roi2img\n"); exit(1);}
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
