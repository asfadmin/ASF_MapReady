/*
   IFM2PPM.H  -  Header file for taking interferogram files and 
	     creating an image. Contains function decalartions as well
	     as structure declarations

   Mike Shindle, 1.0, Nov. 1995

*/

#ifndef __IFM2PPM_H

#define __IFM2PPM_H

/* constants */
#define MAXENTRIES   256
#define IMGWIDTH     2048
#define IMGLENGTH    2560

/* function declarations */
 void igram_colortable (RGBDATA *table); 
 void grey_colortable (RGBDATA *table); 
 void mask_colortable (RGBDATA *table); 
 void user_colortable (RGBDATA *table, char *fpal); 
 void interp (RGBDATA *table, int start, int end); 
 void write_table (RGBDATA *table, char *fname); 
 void usage (char *name); 
#endif

