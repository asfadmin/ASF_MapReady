/******************************************************************************
NAME:   gr2sr - Remaps a ground range image to a slant range image

SYNOPSIS: gr2sr <infile> <outfile> <pixsize>

DESCRIPTION:
        Remaps a ground range image to a slant range image using resampling
        vectors calculated by the  gr2ml_vec (azimuth resampling vector) and
        gr2sr_vec (range resampling vector) subroutines.  The resampling
        uses bi-linear interpolation based on the vectors calculated.

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------
    infile.img          Input data file
    infile.meta         Input metadata about image file

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "gr2sr.h"

#define VERSION 0.1

int main(int argc,char *argv[])
{
  meta_parameters *inMeta, *outMeta;

  int   np, nl;         /* in number of pixels,lines      */
  int   onp, onl;       /* out number of pixels,lines     */
  int   ii;
  float gr2sr[MAX_IMG_SIZE];    /* GR 2 SR resampling vector for Range  */
  float gr2ml[MAX_IMG_SIZE];    /* GR 2 SR resamp vector for Azimuth    */
  int   a_lower[MAX_IMG_SIZE];  /* floor of gr2ml vector                */
  int   a_upper[MAX_IMG_SIZE];  /* ceiling of gr2ml vector              */
  float a_ufrac[MAX_IMG_SIZE];  /* Upper fraction from gr2ml vector     */
  float a_lfrac[MAX_IMG_SIZE];  /* Lower fraction from gr2ml vector     */
  int   lower[MAX_IMG_SIZE];    /* floor of gr2sr vector                */
  int   upper[MAX_IMG_SIZE];    /* ceiling of gr2sr vector              */
  float ufrac[MAX_IMG_SIZE];    /* Upper fraction from gr2sr vector     */
  float lfrac[MAX_IMG_SIZE];    /* Lower fraction from gr2sr vector     */
  float srPixSize=0.0;

  float *inBuf;         /* Input buffer                 */
  float *outBuf;        /* Output buffer                */
  char  infile[256];    /* Input file name              */
  char  outfile[256];   /* Output file name             */
  FILE  *fpi, *fpo;     /* File pointers                */
  int   line;           /* Loop counter                 */

  if (argc != 4) {
    asfPrintStatus("\n");
    asfPrintStatus("Usage: %s <infile> <outfile> <pixsize>\n",argv[0]);
    asfPrintStatus("   infile   Input ground range file base name\n");
    asfPrintStatus("   outfile  Output slant range filebase name\n");
    asfPrintStatus("   pixsize  Pixel size for output slant range image\n");
    asfPrintStatus("\n");
    asfPrintStatus("Version %.2f, ASF Tools\n",VERSION);
    asfPrintStatus("\n");
    exit(EXIT_FAILURE);
  }

  create_name(infile,argv[1],".img");
  create_name(outfile,argv[2],".img");
  srPixSize = atof(argv[3]);

  inMeta = meta_read(infile);
  nl = inMeta->general->line_count;
  np = inMeta->general->sample_count;

  gr2sr_vec(inMeta, gr2sr);
  gr2ml_vec(inMeta, gr2ml);

  for (ii=MAX_IMG_SIZE; ii>0; ii--) if ((int)gr2sr[ii] > np) onp = ii;
  for (ii=MAX_IMG_SIZE; ii>0; ii--) if ((int)gr2ml[ii] > nl) onl = ii;

  outMeta = meta_copy(inMeta);
  /* FIXME: These are stolen from sr2gr and haven't been updated to be inverse */
	outMeta->sar->time_shift  += ((inMeta->general->start_line+1)
                                * inMeta->sar->azimuth_time_per_pixel);
	outMeta->sar->slant_shift += ((inMeta->general->start_sample+1)
                                * inMeta->general->x_pixel_size);
	outMeta->general->start_line   = 0.0;
	outMeta->general->start_sample = 0.0;
	outMeta->sar->azimuth_time_per_pixel *= srPixSize
                                          / inMeta->general->y_pixel_size;
	outMeta->sar->line_increment   = 1.0;
  outMeta->sar->sample_increment = 1.0;
  outMeta->sar->image_type       = 'S';
	outMeta->general->x_pixel_size = srPixSize;
	outMeta->general->y_pixel_size = srPixSize;
	outMeta->general->line_count   = onl;
	outMeta->general->sample_count = onp;

  asfPrintStatus("Input  lines, samples: %i %i\n",nl,np);
  asfPrintStatus("Output lines, samples: %i %i\n",onl,onp);

  for (ii=0; ii<MAX_IMG_SIZE; ii++) {
     lower[ii] = (int) gr2sr[ii];
     upper[ii] = lower[ii] + 1;
     ufrac[ii] = gr2sr[ii] - (float) lower[ii];
     lfrac[ii] = 1.0 - ufrac[ii];

     a_lower[ii] = (int) gr2ml[ii];
     a_ufrac[ii] = gr2ml[ii] - (float) a_lower[ii];
     a_lfrac[ii] = 1.0 - a_ufrac[ii];
  }

  fpi = FOPEN(infile,"rb");
  fpo = FOPEN(outfile,"wb");
  inBuf = (float *) MALLOC (np*sizeof(float));
  outBuf = (float *) MALLOC (onp*sizeof(float));

  for (line = 0; line < onl; line++) {
    get_float_line(fpi, inMeta, a_lower[line], inBuf);
    for (ii=0; ii<onp; ii++) {
       float tmp = inBuf[lower[ii]]*lfrac[ii]+inBuf[upper[ii]]*ufrac[ii];
       outBuf[ii] = tmp*a_lfrac[line] + tmp*a_ufrac[line];
    }
    put_float_line(fpo,outMeta,line,outBuf);
  }

  meta_write(outMeta, outfile);
  meta_free(inMeta);
  meta_free(outMeta);

  FREE(inBuf);
  FREE(outBuf);
  FCLOSE(fpi);
  FCLOSE(fpo);

  exit(EXIT_SUCCESS);
}

