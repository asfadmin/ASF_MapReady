#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"


int getphase(char *phasein, char *tiept, char *outfile)
{
  char metaName[255];
  FILE *fphase, *fout, *ftie;
  float phase, *phase_line;
  int ii=0;
  int wid, len;
  float x, y, ht;
  meta_parameters *meta;

  strcpy(metaName, phasein);

  /* Get input scene size*/
  meta = meta_read(metaName);
  wid = meta->general->sample_count;
  len = meta->general->line_count;

  /* open input files*/
  ftie = FOPEN(tiept,"r");
  fout = FOPEN(outfile,"w");
  fphase = fopenImage(phasein,"rb");

  /* Allocate memory for line buffer */
  phase_line = (float *) MALLOC(wid*sizeof(float));

  /* get phase for each tie pt. location*/
  while (fscanf(ftie,"%f %f %f\n",&x,&y,&ht) != EOF) {
     double orig_x = x;
     double orig_y = y;
     y -= meta->general->start_line;
     y /= meta->sar->line_increment;
     x -= meta->general->start_sample;
     x /= meta->sar->sample_increment;
     if ((x<0)||(y<0)||(x>=wid)||(y>=len)) {
       printf("   ERROR: Tie point not within image bounds. Discarding.\n");
       continue;
     }
     get_float_line(fphase, meta, y, phase_line);
     phase = phase_line[(int)x];
     if (fabs(phase)<0.0001) {
       printf("   ERROR: Phase equals zero. Discarding-- "
	      "Escher couldn't unwrap this tie point..\n");
       continue;
     }
     fprintf(fout,"%4d  %9.2f  %9.2f  %10.3f  %.8f\n",ii,orig_x,orig_y,ht,phase);
     ii++;
  }

  if (!quietflag) printf("   Read %d seed points\n\n", ii);

  /* Free memory */
  meta_free(meta);

  /* close input files*/
  FCLOSE(fout);
  FCLOSE(ftie);
  FCLOSE(fphase);
  return(0);
}


