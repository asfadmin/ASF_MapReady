/***************************************************************************
FUNCTION NAME:	create_ddr -- make a LAS 6.0 ddr metadata file

SYNTAX:		create_ddr(fname,nrow,ncol)

PARAMETERS:
    NAME:       TYPE:           PURPOSE:
    --------------------------------------------------------
    fname	char*		Name of output file
    nrow 	int		number of lines in image
    ncol 	int 		number of sample in the image

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
   1.0	    8/95   M. Shindle 	Write DDR file.

***************************************************************************/
#include "asf.h"

#include "las.h"
#include "locinc.h"
#include "proj.h"
#include "gps_vector.h"

typedef struct gp_coord {
    double x;
    double y;
} COORD;

void create_gps_ddr(fname,meta,nrow,ncol,pixsize,nbands)
char *fname;
struct gps_meta *meta;
int nrow;
int ncol;
int pixsize;
int nbands;
{
   struct DDR ddr;
   int stati;
   double max_x, min_x, max_y, min_y;
   int i;

   /** calculate needed information **/
   /* pixel distance - convert max & min to meters from km */
   max_x = s_to_dbl(meta->bmax_x,10) * 1000.0;
   min_x = s_to_dbl(meta->bmin_x,10) * 1000.0;
   max_y = s_to_dbl(meta->bmax_y,10) * 1000.0;
   min_y = s_to_dbl(meta->bmin_y,10) * 1000.0;

   /* init ddr structure */
   c_intddr(&ddr);
  
   /* fill in elements */
   ddr.nl = nrow;
   ddr.ns = ncol;
   ddr.nbands = nbands;
   ddr.dtype = EBYTE;
   ddr.line_inc = 1.0;
   ddr.sample_inc = 1.0;
   ddr.master_line = 1;
   ddr.master_sample = 1;
   ddr.pdist_x = (double)pixsize;
   ddr.pdist_y = (double)pixsize;
   strcpy(ddr.proj_units,"METERS");
   
   ddr.upleft[0] = max_y;
   ddr.upleft[1] = min_x;

   ddr.upright[0] = max_y;
   ddr.upright[1] = min_x + ((ncol-1) * pixsize);

   ddr.loleft[0] = max_y - ((nrow-1) * pixsize);
   ddr.loleft[1] = min_x;

   ddr.loright[0] = max_y - ((nrow-1) * pixsize);
   ddr.loright[1] = min_x + ((ncol-1) * pixsize);
 
   ddr.proj_code = PS;
   ddr.datum_code = 0;
   ddr.zone_code = 62;
   ddr.proj_coef[0] = HUGHES_MAJ;
   ddr.proj_coef[1] = HUGHES_MIN;
   ddr.proj_coef[4] = -45.0;
   ddr.proj_coef[5] = 70.0;

   /* validate ddr fields */
   for (i=0;i<8;i++)
      ddr.valid[i] = VALID;

   stati = c_putddr(fname, &ddr);
        if (stati != E_SUCC)
          {
            printf("Error returned from putddr\n");
            exit(1);
          }

   return;
}
