/**
  Image file "DDR" (Data Descriptor Record) I/O.
  This format comes from the Eros Data Center's Land Analysis System (LAS).
  
  Orion Sky Lawlor, olawlor@acm.org, 2006/05/10 (Copyright ASF)
*/
#ifndef __ASF_DDR_H
#define __ASF_DDR_H

#include "dll_support.h"


/** Data Descriptor Record (DDR) for image data.
 This format comes from the Eros Data Center's Land Analysis System (LAS)
  */
struct DDR 
{
    int nl;              /* number of lines (rows) in image         */
    int ns;              /* number of samples (columns) in image    */
    int nbands;          /* number of bands (color channels) in image */
    int dtype;           /* data type of pixels (see below) */
/** Valid dtype codes */
#define DTYPE_BYTE 1   /* unsigned char */
#define DTYPE_SHORT 2  /* 16-bit integer */
#define DTYPE_LONG 3   /* 32-bit integer */
#define DTYPE_FLOAT 4  /* 32-bit float */
#define DTYPE_DOUBLE 5 /* 64-bit float (NOT SUPPORTED) */
#define DTYPE_COMPLEX 10  /* single-precision complex */
#define DTYPE_COMPLEXREAL 11
#define DTYPE_COMPLEXIMAG 12


    int master_line;     /* line relative to master image	     */
    int master_sample;   /* sample relative to master image         */
    int valid[8];	  /* valid flags:                            */
                          /*   =0:  not valid                        */
                          /*   =1:  valid                            */
 		      	  /*   =2:  unknown		             */
/* Below are the constants to retrieve the correct validity flag from the
   validity flag array						         
-----------------------------------------------------------------------*/
#define DDPCV 0	  	/* projection code validity flag	       */
#define DDZCV 1	  	/* zone code validity flag		       */
#define DDDCV 2	  	/* datum code validity flag		       */
#define DDPPV 3	  	/* projection parameters validity flag         */
#define DDPUV 4	  	/* ground units validity flag		       */
#define DDPDV 5	  	/* ground distance validity flag	       */
#define DDCCV 6	  	/* corner coordinates validity flag	       */
#define DDINCV  7	/* line/sample increments validity flag        */

    int proj_code;       /* GCTP projection code                    */
                          /*  refer to GCTP document for valid codes */
    int zone_code;       /* UTM or State Plane zone                 */
                          /*  refer to GCTP document for valid codes */
    int datum_code;      /* GCTP datum code                         */
                          /*  refer to GCTP document for valid codes */
    int spare;		  /* spare integer value; for future use or
			     expansion				     */

    char system[12];       /* computer system data is on (with NULL)  */
    char proj_units[12];   /* Projection units (GCTP units+other) (with NULL) */
    char last_used_date[12]; /* last access date	(with NULL)	     */
			    /* NOTE: All ddr dates are stored as     */
			    /* "dd-mmm-yy".  For example, a date     */
			    /* of December 31, 1986 is stored as     */
			    /* "31-dec-86" with the month in lower   */
			    /* case.				     */
    char last_used_time[12]; /* last access time	(with NULL)  */
			    /* NOTE: All ddr times are stored        */
			    /* using a twenty-four hour clock.	     */
			    /* Seconds are speperated by a colon.    */
			    /* Example: 1:05:55 pm is stored as      */
			    /* 1305:55				     */

    double proj_coef[15]; /* GCTP projection parameters              */
                          /*  refer to GCTP document for field definitions*/
    double upleft[2];     /* Corner coordinates entered              */
    double loleft[2];     /*   in latitude/longitude or              */
    double upright[2];    /*   northing/easting order or             */
    double loright[2];    /*   projection coordinates (y/x)          */
    double pdist_y;       /* projection distance/pixel (y-direction) */
    double pdist_x;       /* projection distance/pixel (x-direction) */
    double line_inc;      /* line increment for sampling             */
    double sample_inc;    /* sample increment for sampling           */
                          /* NOTE:  The line/sample increments are   */
                          /*   the values applied to the original    */
                          /*   image to obtain this image.  If re-   */
                          /*   or sub-sampling was not applied, the  */
                          /*   value contained in these fields will  */
                          /*   be 1.0                                */

    int needs_swap; /* if true, this image needs to be byte-swapped.  This flag is only set by c_getddr. */
};


/** Read a DDR from the file with this name. Always returns 0. */
ASF_COREDLL int c_getddr(const char *hname,struct DDR *ddr);
/** Initialize a DDR to all zeros. */
ASF_COREDLL void c_intddr(struct DDR *ddr);
/** Store a DDR to the file with this name. Always returns 0. */
ASF_COREDLL int c_putddr(const char *hname,struct DDR *ddr);


#endif
