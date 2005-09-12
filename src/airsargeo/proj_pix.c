/********************************************************************
Routines used by projectGeo:
Routines dealing with pixel size.

O. Lawlor, 3/99  Initial Creation
*********************************************************************/
#include "projectGeo.h"

/********************************
Return the size (m) of pixels in the given input image.
************************************/
double ex_pix_size(char *metaname)
{
	meta_parameters *meta;
	double pixsiz;
	
	if (extExists(metaname,".meta"))
	{/*Has .meta file-- read from there.*/
 		meta=meta_init(metaname);
 		pixsiz=meta->geo->xPix;
 		meta_free(meta);
 	} else 
 	{/*Fetch DDR, read pixel size from there*/
 		struct DDR ddr;
 		c_getddr(metaname,&ddr);
 		pixsiz=ddr.pdist_x;
 	}

	return pixsiz;
}






