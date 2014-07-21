/******************************************************************************
NAME:  create_base_images

SYNOPSIS:  Creates Seasat products from raw swaths.

		-v	use state vectors instead of TLEs
		-c 	use the clock drift to offset times

DESCRIPTION:

EXTERNAL ASSOCIATES:
	NAME:               	USAGE:
   	---------------------------------------------------------------
 	create_roi_in		create input configuration file for ROI
	roi			Repeat Orbit Interferometry correlator
	roi2img			Turns ROI outputs into Seasat products

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
#include <asf_license.h>
#include "seasat.h"

#define MAX_NODE	3600	// we will never have southern hemisphere SEASAT data!
#define PATCH_SIZE	8132
#define NUM_PATCHES	3
#define HALF_ESA_FRAME  12468	// this should be about correctt, as follows:
				//   Swath_Length = 4.01 m/line * 24936 lines = 99993.36 meters. 

void process_node(char *basefile, int node);
int get_line_for_node(meta_parameters *meta, int node, int nl);
int get_node_for_line(meta_parameters *meta, int line, int nl);
void give_usage(char *argv[], int argc);
void write_disfile(char *basefile,int node,int start_line,
                   int num_dis,int *dis_line, int *dis_gap);
int get_values(FILE *fp,SEASAT_header_ext *s);

int USE_TLES = 0;
int USE_CLOCK_DRIFT = 1;

int main(int argc, char *argv[])
{
  char basefile[256], tmpfile[256];
  int node, start_line;
  char cmd[256];
  char metafile[256], disfile[256];
  int err=0;
  FILE *fphdr;
  SEASAT_header_ext *hdr;
  int  nl, middle_line, lines_per_frame, val;
  int num_dis=0, dis_line[256], dis_gap[256];
  int c;
 
  asfSplashScreen(argc, argv);
 
  if (argc < 2 || argc > 4) { give_usage(argv,argc); exit(1); }
  
  while ((c=getopt(argc,argv,"vc")) != -1)
    switch(c) {
      case 'v':
        USE_TLES = 0;
	break;
      case 'c':
        USE_CLOCK_DRIFT = 1;
	break;
      case '?':
        printf("Unknown option %s\n",optarg);
	return(1);
      default:
        give_usage(argv,argc);
	exit(1);
    } 
  
  strcpy(basefile,argv[optind]);
  strcat(strcpy(metafile,basefile),".meta");
  strcat(strcpy(disfile,basefile),".dis");

  if (!fileExists(disfile))
    printf("No discontinuity file found for %s\n", basefile);
  else {
    FILE *fpdis = fopen(disfile,"r");
    if (!fpdis) {
      printf("Could not open discontinuity file %s\n", disfile);
    }
    else {
      char str[1024];
      fgets(str, 1024, fpdis);
      while (fgets(str, 1024, fpdis) && num_dis < 255) {
        int line, gap;
        if (sscanf(str, "%d\t%d\n", &line, &gap) == 2) {
          dis_line[num_dis] = line;
          dis_gap[num_dis] = gap;
          ++num_dis;
        }
      }
      fclose(fpdis);
      printf("Read %d gaps from discontinuity file %s\n", num_dis, disfile);
    }
  }

  printf("================================================================================\n");
  printf("%s: CREATING SEASAT PRODUCTS FROM SWATH FILE %s\n",argv[0],basefile);
  printf("================================================================================\n");

  /* create the swath meta file */
  printf("Creating meta file for the swath\n\n");
  sprintf(cmd,"create_roi_in -c -v %s\n",basefile); err=system(cmd); if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
  sprintf(cmd,"roi2img -m -c -v %s\n",basefile); err=system(cmd); if (err) {printf("Error returned from roi2img\n"); exit(1);}
  sprintf(tmpfile,"%s.roi.in",basefile); remove(tmpfile);
  meta_parameters *meta = meta_read(metafile);

  /* get total lines in this file */  
  sprintf(tmpfile,"%s.hdr",basefile);
  if ((fphdr=fopen(tmpfile,"r"))==NULL) {printf("Error opening input file %s\n",tmpfile); exit(1);}
  hdr = (SEASAT_header_ext *) malloc(sizeof(SEASAT_header_ext));
  val = get_values(fphdr, hdr); nl = 0;
  while (val==20) { nl++; val = get_values(fphdr, hdr); }
  fclose(fphdr);
  lines_per_frame = NUM_PATCHES * PATCH_SIZE;

  if (nl < lines_per_frame) {
    printf("This swath is not long enough to process any frames!\n");
    exit(0);
  }

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
  while (middle_line+HALF_ESA_FRAME < nl) {
    if (start_line >= 0) {
      printf("================================================================================\n");
      printf("FOUND VALID NODE %i starting at line %i, Processing...\n",node,start_line);
      write_disfile(basefile,node,start_line,num_dis,dis_line,dis_gap);
      process_node(basefile,node);
      printf("================================================================================\n");
    }
    middle_line += 0.85*lines_per_frame;		// move to next frame location
    node=get_node_for_line(meta,middle_line,nl);	// find the closest node
    start_line = get_line_for_node(meta,node,nl);	// find the start line for chosen node
  } 
  
  printf("================================================================================\n");
  printf("%s: FINISHED CREATING SEASAT PRODUCTS FROM SWATH FILE %s\n",argv[0],basefile);
  printf("================================================================================\n");

  exit(0);
}

void write_disfile(char *basefile,int node,int start_line,
                   int num_dis,int *dis_line, int *dis_gap)
{
  char disfile[256];
  sprintf(disfile,"%s_node%.4i.dis",basefile,node);
  FILE *fpdis = fopen(disfile, "w");
  if (fpdis) {
    fprintf(fpdis, "LINE\tGAP\n");

    // Don't like this hard-coded 8000 here....
    // It is the size of the ground range image
    int ii, sf = 2*HALF_ESA_FRAME/8000;
    int end_line = start_line + 2*HALF_ESA_FRAME;

    for (ii=0; ii<num_dis; ++ii) {
      int gap_start = dis_line[ii];
      int gap_end = dis_line[ii] + dis_gap[ii];
      if ((gap_start >= start_line && gap_start < end_line) ||
          (gap_end >= start_line && gap_end < end_line))
      {
        fprintf(fpdis, "%d\t%d\n", (dis_line[ii] - start_line)/sf, dis_gap[ii]/sf);
      }
    }
    fclose(fpdis);
  }
  else {
    printf("Failed to open discontinity file for node %d: %s\n", node, disfile);
    exit(11);
  }
}

/* Call create_roi_in, roi, and roi2img for the given basefile and node */
void process_node(char *basefile, int node)
{
  char cmd[256],tmpfile[256], options[256];
  int  err;

  sprintf(options,"-E %i",node);
  if (USE_TLES == 0) strcat(options," -v");
  if (USE_CLOCK_DRIFT == 1) strcat(options," -c");
  
  sprintf(cmd,"create_roi_in %s %s\n",options,basefile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from create_roi_in\n"); exit(1);}
	
  sprintf(tmpfile,"%s_node%.4i.roi.in",basefile,node);
  sprintf(cmd,"roi < %s\n",tmpfile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from ROI\n"); exit(1);}

  sprintf(tmpfile,"%s_node%.4i",basefile,node);
  sprintf(cmd,"roi2img %s %s\n",options,tmpfile);
  printf("Executing command: %s\n",cmd);
  err = system(cmd);
  if (err) {printf("Error returned from roi2img\n"); exit(1);}
  
  /* get rid of intermediate doppler, spectra, and stvecs */
  remove("dop.out");
  remove("dop.pre");
  remove("spectra.out");
  remove("spectra.fixed");
  remove("fixed_state_vector.txt");
  
  /* get rid of intermediate node files */
  sprintf(tmpfile,"%s_node%.4i.slc",basefile,node);
  remove(tmpfile);
  sprintf(tmpfile,"%s_node%.4i.dis",basefile,node);
  remove(tmpfile);
  sprintf(tmpfile,"%s_node%.4i.dwp",basefile,node);
  remove(tmpfile);
  
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
  //printf("Usage: %s [-v][-c] <swath_file>\n",argv[0]);
  //printf("\t-v            \tUse state vectors instead of TLEs\n");
  //printf("\t-c            \tApply the clock drift to image timing\n");
  printf("Usage: %s <swath_file>\n\n",argv[0]);
  printf("The program will now always use state vectors (instead of TLEs)\n");
  printf("and will always apply the clock drift.\n");
}
