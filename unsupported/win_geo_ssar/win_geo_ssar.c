/******************************************************************************
NAME: win_geo_ssar - Extracts a window from a geocoded SAR image

SYNOPSIS: win_geo_ssar infile outfile ulx uly ns nl

          infile  - input ASF geocoded image
          outfile - output LAS geocoded image
          ulx     - Upper left X coordinate of window
          uly     - Upper left Y coordinate of window
          ns      - number of samples in window
          nl      - number of lines in window

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
    1.0	  10/30/98 T. Logan	Cut window from a geocoded SCANSAR file

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
#include "asf.h"
#include "ceos.h"
#include "las.h"
#include "ddr.h"

#define HEADER 192

main(int argc,char *argv[])
{
  FILE *fpin, *fpout;
  float out_ulx, out_uly;
  float in_ulx, in_uly;
  int   out_nl, out_ns;
  int   nl, ns;
  int   i, j;
  int   cnt;
  float pixsize;
  int   offset_line, offset_samp;
  char  infile[256], outfile[256];
  char  *inbuf, *outbuf;
  struct VMPDREC  mpdr;          /* data structure for all map proj. data */
  struct IOF_VFDR iof_vfdr;      /* Imagery option file descriptor record */

  if (argc != 7) 
   {
     printf("Usage: %s infile outfile ulx uly ns nl\n",argv[0]);
     printf("          infile  - input ASF geocoded image\n");
     printf("          outfile - output LAS geocoded image\n");
     printf("          ulx     - Upper left X coordinate of window\n");
     printf("          uly     - Upper left Y coordinate of window\n");
     printf("          ns      - number of samples in window\n");
     printf("          nl      - number of lines in window\n");
     printf("\nExtracts a window from a geocoded SAR image\n");
     exit(1);
   }

  strcat(strcpy(infile,argv[1]),".D");
  strcat(strcpy(outfile,argv[2]),".img");
  out_ulx = atof(argv[3]);
  out_uly = atof(argv[4]);
  out_ns = atoi(argv[5]);
  out_nl = atoi(argv[6]);

  get_ifiledr(infile, &iof_vfdr);
  nl = iof_vfdr.numofrec + 1;
  ns = iof_vfdr.reclen;

  get_mpdr(infile, &mpdr);
  in_ulx = mpdr.tlceast;
  in_uly = mpdr.tlcnorth;
  pixsize = mpdr.nomild;

  offset_line = (int) ((in_uly - out_uly) / pixsize);
  offset_samp = -1 * (int) ((in_ulx - out_ulx) / pixsize);

  /* make sure requested window is in the image */
  if (offset_line < 0 || offset_samp < 0 ||
      offset_line+out_nl > nl-1 || offset_samp+out_ns > ns-HEADER)
   {
     printf("Selected coordinates are outside of the input image\n");
     exit(1);
   }
 
  printf("Copying from line %i, sample %i for %i line, %i samples\n",
	offset_line, offset_samp, out_nl, out_ns);
  inbuf = (char *) MALLOC (out_nl*ns);
  outbuf = (char *) MALLOC (out_nl*out_ns);

  /* Skip to correct position, Read from offset_line for out_nl */
  fpin = fopenImage(infile,"rb");
  FSEEK(fpin,(offset_line+1)*ns,0);
  FREAD(inbuf, out_nl*ns, 1, fpin);
  FCLOSE(fpin);

  /* Copy appropriate section to outbuf */
  cnt = 0;
  for (i = 0; i<out_nl; i++)
   for (j = offset_samp+HEADER; j < offset_samp+HEADER+out_ns; j++)
     outbuf[cnt++] = inbuf[i*ns+j];

  fpout = fopenImage(outfile,"wb");
  FWRITE(outbuf, out_nl*out_ns, 1, fpout);
  FCLOSE(fpout);

  free(inbuf);
  free(outbuf);

  /* Modify mpdr for the new window & create a DDR */
  mpdr.nlines = out_nl;
  mpdr.npixels = out_ns;
  mpdr.tlcnorth = out_uly;
  mpdr.trcnorth = out_uly;
  mpdr.tlceast  = out_ulx;
  mpdr.blceast  = out_ulx;
  mpdr.blcnorth = out_uly - (pixsize*out_nl);
  mpdr.brcnorth = out_uly - (pixsize*out_nl);
  mpdr.trceast  = out_ulx + (pixsize*out_ns);
  mpdr.brceast  = out_ulx + (pixsize*out_ns);
  create_ddr(outfile,out_nl,out_ns,pixsize,EBYTE);
  make_valid_DDR(mpdr, outfile, EBYTE, 1);

  exit(0);
}
