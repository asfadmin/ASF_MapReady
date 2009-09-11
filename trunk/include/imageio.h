#ifndef _IMAGEIO_H_
#define _IMAGEIO_H_

/**********************************************************
 NAME:  imageio.h

 AUTHOR: Misc.

 VERSION:
     1.0 - 
     1.1 - M. Shindle (ASF) 5/96 - added linux to list of systems
	   Using __linux__ to comply with POSIX standard
     1.2 - O. Lawlor 5/99: bumped up maximum number of lines and samples to 
     		5 million each (was 50,000; which is too small).
***********************************************************/
#define MAXBND 256   /* Maximum number of bands to be processed            */
#define MAXNS  5000000 /* Maximum number of samples per line to be processed */
#define MAXNL  5000000 /* Maximum number of lines to be processed	           */

/* Arrays used for specifying windows contain four fields, sl, ss, nl, ns.
   Here are the definitions of those array indices.
---------------------------------------------------------------------------*/
#define SL 0
#define SS 1
#define NL 2
#define NS 3

#ifdef aiws
#define MAXIMG 20    /* Maximum number of images to be processed           */
#elif defined(gould)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#elif defined(_IBMR2)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#elif defined(vms)
#define MAXIMG 56    /* Maximum number of images to be processed           */
#elif defined(sgi)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#elif defined(DGUX)
#define MAXIMG 54    /* Maximum number of images to be processed           */
#else
#define MAXIMG 50
#endif
/*  Maximum buffer size to be used for most memory allocations
--------------------------------------------------------------*/
#ifdef vms
#define MAXBUFS 65535  	 	  /* 65535 is the largest i/o vms can do */
#else
#define MAXBUFS 1024 * 1024 * 20.0 /* 20.0 megabyte of memory -estimated guess
  			             of the best amount of memory to allocate */
#endif

/* method types 
---------------*/
#define STEP 1
#define RANDOM 2

#define NLABELS 0
#define LABSIZ 512
#define ORG I_CS

/* fdesc flags 
--------------*/
#define CONVERSION 1
#define EDONE 2
#define MOVEDATA 4


/* BLOCK IMAGE PROCESSING CONSTANTS
-----------------------------------*/

/* boundary fill techniques
---------------------------*/
#define NOFIL 0
#define MIRR  1

/* end of image window return status
------------------------------------*/
#define E_EOIW   1

/* no data conversion required
------------------------------*/
#define SAME  0

/* 'next flag' options
----------------------*/
#define CREAT  -1
#define INIT    0
#define CURNT   1
#define NEXT    2

/* Compression type
-------------------*/
#define COMPRES 5
#define NO_COMPRES 6

	
lasErr FUNCTION c_upddr(const char *host_in[],char *host_out,int *nmrimg,int *window[],
		int bands[][MAXBND+1],int nbands[],
		double *line_inc,double *samp_inc,int *out_nbands,int *cflag);

#endif
