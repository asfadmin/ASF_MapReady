/***************************************************************************
FUNCTION NAME:	create_ddr -- make a LAS 6.0 ddr metadata file

SYNTAX:		create_ddr(fname,nl,ns,pixsiz)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    fname	char*		Name of output file
    nl 		int		number of lines in image
    ns 		int 		number of sample in the image
    pixsiz	double		size of output pixels in meters
    data_type   int            size of image data

DESCRIPTION:
	This functions creates a LAS ddr file and inserts a bdr
    into this file.  The purpose here is to retain the image
    metadata in a convenient fashion.  The ddr file is initialized
    with the size of the image, image data type, and the size of
    the image pixels.

RETURN VALUE:
	Will return 0 or exit with an error message

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  VERSION   DATE   AUTHOR	PURPOSE
  -------   ----   ------	-------
   1.0	    4/95   T. Logan	Retain image metadata after resampling
   1.1	    6/95   T. Logan     Validate DDR fields that are valid
   1.2     10/95   M. Shindle   Now accepts data type as a parameter
***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "las.h"
#include "locinc.h"

int create_ddr(char*,int,int,double,int);
int create_ddr(char *fname,int nl,int ns,double pixsiz,int data_type)
{
	struct DDR ddr;
	long stati;
        char *system;

        c_intddr(&ddr);

	ddr.valid[4] = VALID;		/* Projection Units    */
	ddr.valid[5] = VALID;		/* Projection Distance */
	ddr.valid[7] = VALID;		/* Increment	       */
        ddr.nl = nl;
        ddr.ns = ns;
        ddr.nbands = 1;
        ddr.dtype = data_type;
        ddr.line_inc = 1.0;
        ddr.sample_inc = 1.0;
        ddr.master_line = 1;
        ddr.master_sample = 1;
        ddr.pdist_x = pixsiz;
        ddr.pdist_y = pixsiz;
        strcpy(ddr.proj_units,"METERS");
        system = c_getsys();
	strcpy(ddr.system,system);

        stati = c_putddr(fname, &ddr);
        if (stati != E_SUCC)
          {
            printf("Error returned from putddr\n");
            exit(1);
          }
/*
        c_intbdr(&bddr);
        bddr.bandno = 1;
        stati = c_putbdr(fname, &bddr);
        if (stati != E_SUCC)
          {
            printf("Error returned from putbdr\n");
            exit(1);
          }
*/
	return(0);
}
