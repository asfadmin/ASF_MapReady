/****************************************************************
FUNCTION NAME:  (datatype)_IO

   where (datatype) is: uchar
			short
			integer
			float	

SYNTAX: (void datatype)_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, 
			  int imagelength, char outfile[], int nbands);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    out		TIFF*		write LAS buffer to this file
    fpin	FILE*		ASF image file to read into buffer
    gtif	GTIF*	  	write geotiff keys and free gtif 
    imagewidth	int		Number of samp to read into buffer per line
    imagelength int		Number of lines to read into buffer
    outfile	char[]		outfile is closed in this function
    nbands      int 		for buffer I/O

DESCRIPTION:  There are four functions in this file.  They all perform the same
	tasks, except each allocate the appropriate memory space for the I/O
	buffers.  
    All four functions:
	(1).  Allocate input and output buffers
	(2).  Read image in one line at a time and write out to TIFF image
	(3).  Write out the geoTIFF keys
	(4).  Close and free all allocated memory

RETURN VALUE: NONE

SPECIAL CONSIDERATIONS: NONE

PROGRAM HISTORY:  
 VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    0.0	 8/01   	     Function part of las2geotiff program.
    1.0  8/01	S. Watts     Function moved to seperate file.  Added 
			     additional functions to handle ASF 
		  	     images of data type short, integer, and float.
****************************************************************/

#include "convert2geotiff_IO.h"

void byte_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[])
  {
  Uchar *lasbuf;                /* input line buffer from ASF image */
  Uchar *tiffbuf;       	/* line buffer for tiff image */
  int samp = 0, line = 0;       /* Counter variables */


/************Allocate input and output buffers **************/
  lasbuf = (Uchar *)MALLOC(sizeof(Uchar)*imagewidth);
  tiffbuf = (Uchar *)MALLOC(sizeof(Uchar)*imagewidth);


/******Read image in one line at a time and write out to TIFF image**/
for (line = 0; line < imagelength; line++)
  {
  FREAD(lasbuf,sizeof(Uchar)*imagewidth,1, fpin);
  for (samp=0; samp < imagewidth; ++samp)
    tiffbuf[samp] = lasbuf[samp];
  if (TIFFWriteScanline(out, tiffbuf, line, 0) < 0)
    {
    XTIFFClose(out);
    free(lasbuf);
    free(tiffbuf);
    printf("\n\nError writing to %s\n\n", outfile); 
    }
  }

/* write out the geoTIFF keys
     -------------------------- */
  GTIFWriteKeys(gtif);
  GTIFFree(gtif);
  XTIFFClose(out);
  FCLOSE(fpin);
  free(lasbuf);
  free(tiffbuf);

}

void short_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[])
  {
  short *lasbuf;                /* input line buffer from ASF image */
  short *tiffbuf;       	/* line buffer for tiff image */
  int samp = 0, line = 0;       /* Counter variables */

/************Allocate input and output buffers **************/
  lasbuf = (short *) MALLOC (sizeof(short) * imagewidth);
  tiffbuf = (short *) MALLOC (sizeof(short) * imagewidth);


/******Read image in one line at a time and write out to TIFF image**/
for (line = 0; line < imagelength; line++)
  {
  FREAD(lasbuf,imagewidth*sizeof(short),1, fpin);
  for (samp=0; samp < imagewidth; ++samp)
    tiffbuf[samp] = lasbuf[samp];
  if (TIFFWriteScanline(out, tiffbuf, line, 0) < 0)
    {
    XTIFFClose(out);
    free(lasbuf);
    free(tiffbuf);
    printf("\n\nError writing to %s\n\n", outfile); 
    }
  }
/* write out the geoTIFF keys
     -------------------------- */
  GTIFWriteKeys(gtif);
  GTIFFree(gtif);
  XTIFFClose(out);
  FCLOSE(fpin);
  free(lasbuf);
  free(tiffbuf);

  }

void integer_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[])
  {
  int *lasbuf;                /* input line buffer from ASF image */
  int *tiffbuf;       	/* line buffer for tiff image */
  int samp = 0, line = 0;       /* Counter variables */

/************Allocate input and output buffers **************/
  lasbuf = (int *) MALLOC (sizeof(int) * imagewidth);
  tiffbuf = (int *) MALLOC (sizeof(int) * imagewidth);


  /******Read image in one line at a time and write out to TIFF image**/
  for (line = 0; line < imagelength; line++)
    {
    FREAD(lasbuf,imagewidth*sizeof(int),1, fpin);
    for (samp=0; samp < imagewidth; ++samp)
      tiffbuf[samp] = lasbuf[samp];
    if (TIFFWriteScanline(out, tiffbuf, line, 0) < 0)
      {
      XTIFFClose(out);
      free(lasbuf);
      free(tiffbuf);
      printf("\n\nError writing to %s\n\n", outfile); 
      }
    }

/* write out the geoTIFF keys
     -------------------------- */
  GTIFWriteKeys(gtif);
  GTIFFree(gtif);
  XTIFFClose(out);
  FCLOSE(fpin);
  free(lasbuf);
  free(tiffbuf);

  }
void float_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[])
  {
  float *lasbuf;                /* input line buffer from ASF image */
  float *tiffbuf;       	/* line buffer for tiff image */
  int samp = 0, line = 0;       /* Counter variables */

  /************Allocate input and output buffers **************/
  lasbuf = (float *) MALLOC (sizeof(float) * imagewidth);
  tiffbuf = (float *) MALLOC (sizeof(float) * imagewidth);


  /******Read image in one line at a time and write out to TIFF image**/
  for (line = 0; line < imagelength; line++)
    {
    FREAD(lasbuf,imagewidth*sizeof(float),1, fpin);
    for (samp=0; samp < imagewidth; ++samp)
      tiffbuf[samp] = lasbuf[samp];
    if (TIFFWriteScanline(out, tiffbuf, line, 0) < 0)
      {
      XTIFFClose(out);
      free(lasbuf);
      free(tiffbuf);
      printf("\n\nError writing to %s\n\n", outfile); 
      }
    }
  /* write out the geoTIFF keys
     -------------------------- */
  GTIFWriteKeys(gtif);
  GTIFFree(gtif);
  XTIFFClose(out);
  FCLOSE(fpin);
  free(lasbuf);
  free(tiffbuf);

}



/************************************************************************/

void three_band_byte(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[], int nbands)
{
  int line, bcount, samp, i;

  Uchar *lasbuf;                /* input line buffer from ASF image */
  Uchar *tiffbuf;       	/* line buffer for tiff image */
  Uchar *tempbuf;

   /**Allocate memory space**/
  lasbuf=(Uchar *)MALLOC(sizeof(Uchar)*imagewidth);
  tempbuf=(Uchar *)MALLOC(sizeof(Uchar)*imagewidth*nbands*imagelength);
  tiffbuf=(Uchar *)MALLOC(sizeof(Uchar)*imagewidth*nbands);

  /****READ EACH LINE OF EACH BAND AND PUT INTO TEMPBUF INTERLEAVED****/
  for (bcount=0;bcount<nbands;bcount++)
    for(line=0;line<imagelength;line++)
    {
    FREAD(lasbuf,sizeof(Uchar)*imagewidth,1, fpin);
    for (samp=0; samp <= imagewidth; samp++)
      tempbuf[bcount+samp*nbands+(line*imagewidth*nbands)] = lasbuf[samp];
    }

  /****WRITE TIFFBUF ONE LINE AT A TIME****/
  i=0;
  for(line=0;line<imagelength;line++)
    {
    for (samp=0;samp<imagewidth*nbands;samp++,i++)
      tiffbuf[samp]=tempbuf[i];
    if (TIFFWriteScanline(out, tiffbuf, line, imagelength) < 0)
      {
      XTIFFClose(out);
      free(lasbuf);
      free(tiffbuf);
      printf("\n\nError writing to %s\n\n", outfile); 
      }
    }
  /* write out the geoTIFF keys
     -------------------------- */
  GTIFWriteKeys(gtif);
  GTIFFree(gtif);
  XTIFFClose(out);
  FCLOSE(fpin);
  free(lasbuf);
}
