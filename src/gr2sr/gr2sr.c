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
#define CLIGHT   2.997924562e8

static void usage(char *progname)
{
    asfPrintStatus("\n");
    asfPrintStatus("Usage: %s [-p <pixsize>] <infile> <outfile>\n",progname);
    asfPrintStatus("   pixsize  Pixel size for output slant range image\n");
    asfPrintStatus("   infile   Input ground range file base name\n");
    asfPrintStatus("   outfile  Output slant range filebase name\n");
    asfPrintStatus("\n");
    asfPrintStatus(" If the pixel size is not specified, it is calculated\n");
    asfPrintStatus(" as follows: (speed of light)/(sample rate * 2*10^6)\n");
    asfPrintStatus("\n");
    asfPrintStatus("Version %.2f, ASF Tools\n",VERSION);
    asfPrintStatus("\n");
    exit(EXIT_FAILURE);
}

int main(int argc,char *argv[])
{
  meta_parameters *inMeta, *outMeta;

  int   np, nl;         /* in number of pixels,lines      */
  int   onp, onl;       /* out number of pixels,lines     */
  int   ii;
  float gr2sr[MAX_IMG_SIZE];    /* GR 2 SR resampling vector for Range  */
  int   lower[MAX_IMG_SIZE];    /* floor of gr2sr vector                */
  int   upper[MAX_IMG_SIZE];    /* ceiling of gr2sr vector              */
  float ufrac[MAX_IMG_SIZE];    /* Upper fraction from gr2sr vector     */
  float lfrac[MAX_IMG_SIZE];    /* Lower fraction from gr2sr vector     */
  float srPixSize=0.0; /* output pixel size */

  float *inBuf;          /* Input buffer                  */
  float *outBuf;         /* Output buffer                 */
  char  infile[256];     /* Input file name               */
  char  outfile[256];    /* Output file name              */
  FILE  *fpi, *fpo;      /* File pointers                 */
  int   line;            /* Loop counter                  */

  if ((argc != 5 && argc != 3) || (argc == 5 && strcmp(argv[1], "-p") != 0)) {
    usage(argv[0]);
  }

  if (strcmp(argv[1],"-p") == 0) {
    create_name(infile,argv[3],".img");
    create_name(outfile,argv[4],".img");
  } else {
    create_name(infile,argv[1],".img");
    create_name(outfile,argv[2],".img");
  }

  inMeta = meta_read(infile);

  if (argc == 5) {
    srPixSize = atof(argv[2]);
  } else {
    /*
      In an e-mail from Rick:
       Slant Range Pixel Size = C (speed of light) / [SampleRate (sample 
       rate) * 2,000,000.]

       SampleRate can be extracted from the L1 metadata :
          RNG CMPLX SAMPLE RATE        18.9599991

       The meta->sar->range_sampling_rate is 10^6 times the value above,
       so we use C/(2*meta->sar->range_sampling_rate)
    */
    srPixSize = CLIGHT / (2.0 * inMeta->sar->range_sampling_rate);
  }

  asfPrintStatus("Using Range Pixel Size: %g\n", srPixSize);

  nl = inMeta->general->line_count;
  np = inMeta->general->sample_count;

  onl=nl;
  gr2sr_vec(inMeta, srPixSize, gr2sr);
  
  /* Determine the output image size */
  onp = 0;
  for (ii=0; ii<MAX_IMG_SIZE; ii++) {
     if (gr2sr[ii]<np) onp=ii; /* gr input still in range-- keep output */
     else break; /* gr input is off end of image-- stop sr output */
  }
  
  /* Split gr2sr into resampling coefficients */
  for (ii=0; ii<onp; ii++) {
     lower[ii] = (int) gr2sr[ii];
     upper[ii] = lower[ii] + 1;
     ufrac[ii] = gr2sr[ii] - (float) lower[ii];
     lfrac[ii] = 1.0 - ufrac[ii];
     if (lower[ii]>=np) lower[ii]=np-1; /* range clip */
     if (upper[ii]>=np) upper[ii]=np-1; /* range clip */
  }
  
  outMeta = meta_copy(inMeta);
  /* FIXME: These are stolen from sr2gr and haven't been updated to be inverse */
  outMeta->sar->slant_shift += ((inMeta->general->start_sample)
                                * inMeta->general->x_pixel_size);
  outMeta->general->start_sample = 0.0;
  outMeta->sar->sample_increment = 1.0;
  outMeta->sar->image_type       = 'S';
  outMeta->general->x_pixel_size = srPixSize;
  outMeta->general->sample_count = onp;

  asfPrintStatus("Input  lines, samples: %i %i\n",nl,np);
  asfPrintStatus("Output lines, samples: %i %i\n",onl,onp);

  fpi = FOPEN(infile,"rb");
  fpo = FOPEN(outfile,"wb");
  inBuf = (float *) MALLOC (np*sizeof(float));
  outBuf = (float *) MALLOC (onp*sizeof(float));

  for (line = 0; line < onl; line++) {
    get_float_line(fpi, inMeta, line, inBuf);
    for (ii=0; ii<onp; ii++) { /* resample to slant range */
       outBuf[ii] = inBuf[lower[ii]]*lfrac[ii]+inBuf[upper[ii]]*ufrac[ii];
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

