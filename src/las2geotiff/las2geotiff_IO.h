/****************************************************************
FUNCTION NAME:  (datatype)_IO

   where (datatype) is: uchar
			short
			integer
			float	

SYNTAX: (void datatype)_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, 
			  int imagelength, char outfile[], nbands);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    out		TIFF*		write LAS buffer to this file
    fpin	FILE*		LAS file to read into buffer
    gtif	GTIF*	  	write geotiff keys and free gtif 
    imagewidth	int		Number of samp to read into buffer per line
    imagelength int		Number of lines to read into buffer
    outfile	char[]		outfile is closed in this function
    nbands      int             for buffer I/O

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
    0.0	 8/01   tae	     Function part of las2geotiff program.
    1.0  8/01	S. Watts     Function moved to seperate file.  Added 
			     additional functions to handle LAS 
		  	     images of data type short, integer, and float.
****************************************************************/
#ifndef _LAS2GEOTIFF_IO_H_
#define _LAS2GEOTIFF_IO_H_


#include "ddr.h"
#include "ifm.h"
#include "proj.h"
#include "geotiffio.h"
#include "xtiffio.h"
#define MEM_SPACE 2000    /*2000 MEGABYTES of memory available*/


void byte_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[]);
void short_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[]);
void integer_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[]);
void float_IO(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[]);
void three_band_byte(TIFF *out, FILE *fpin, GTIF *gtif, int imagewidth, int imagelength, char outfile[],int nbands);

#endif
