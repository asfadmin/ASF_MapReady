/*****************************************************************************
  NAME:    typlim.h

  AUTHOR:  MIsc.

  VERSION:
     1.0 -
     1.1 - M. Shindle (ASF) 5/96 - added linux portability
           Using __linux__ to comply with POSIX
     1.2 - O. Lawlor (ASF) 6/97- changed syntax so IEEE floats are default.
*****************************************************************************/

/*  Data type limits 
--------------------*/
#define	MAXUTINY	255		/* 8 bit: 2 to the 8th -1             */
#define MINUTINY	0		/* 8 bit: 0                           */

#define ASF_MAXWORD		32767		/* 16 bit: 2 to the 15th -1           */
#define ASF_MINWORD		-32768		/* 16 bit: negative 2 to the 15th     */

/* Modified MINLONG to be the same on all systems.  -2147483648 will produce
   an overflow on most systems, if not all.  (-2147483647 - 1) will work
   on any system.
------------------------------------------------------------------------------*/
#define ASF_MAXLONG    2147483647        /* 32 bit: 2 to the 31st -1*/
#define ASF_MINLONG    (-2147483647 - 1) /* VAX compiler wouldn't except */

#ifdef gould
#define MAXREAL	   7.2370e75         /* 32 bit floating point (gould)         */
#define MINREAL	  -7.2370e75         /* 32 bit floating point (gould)         */
#elif defined( vms )
#define MAXREAL	   1.7e38            /* 32 bit floating point (vax)           */
#define MINREAL	  -1.7e38            /* 32 bit floating point (vax)           */
#elif defined( _IBMR2)
#define MAXREAL	   3.402823e38       /* 32 bit floating point (ibm aix)       */
#define MINREAL	  -1.401298e45       /* 32 bit floating point (ibm aix)       */
#elif defined( DGUX)
#define MAXREAL	   3.4028e38         /* 32 bit floating point (dgux)          */
#define MINREAL	  -1.1755e38         /* 32 bit floating point (dgux)          */
#else
#define MAXREAL	   3.402823e38       /* 32 bit floating point (ieee)           */
#define MINREAL	  -3.402823e38       /* 32 bit floating point (ieee)           */
#endif
