/*
  PML.H

  General defines used in ml program.

  Mike Shindle
  April 6, 1996
*/

#ifndef __ML_H
#define __ML_H

#define MAXENTRIES 256 /*Number of values in color table.*/
#define ALL_SCALES(a)	((AVG/(a)) * (41.0/255.0))
#define BUF		256
#define WID		2048
#define LEN		12800
#define LOOKLINE	5
#define LOOKSAMPLE	1

#ifndef SQR
#define SQR(a)		((a) * (a))
#endif

/*Prototypes:*/
 int c2i (float *amp, float *phase, RGBDATA *image, RGBDATA *table, int nsamples, float avg);
 int main (int argc, char **argv); 
 void parse_cla (int argc, char **argv, int *ll, int *ls, int prcsr_rank, int *lasFlag); 
 void print_usage (void); 
 void colortable(RGBDATA *table);

#endif  /* end include file */

