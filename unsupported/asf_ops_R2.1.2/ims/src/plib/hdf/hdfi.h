/***************************************************************************
*
*
*                         NCSA HDF version 3.3r1
*                            September 20, 1993
*
* NCSA HDF Version 3.3 source code and documentation are in the public
* domain.  Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
*
* We ask, but do not require, that the following message be included in all
* derived works:
*
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign, in collaboration with the
* Information Technology Institute of Singapore.
*
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
*
*****************************************************************************/

#ifndef HDFI_H
#define HDFI_H

#ifdef GOT_MACHINE
#undef GOT_MACHINE
#endif

/*--------------------------------------------------------------------------*/
/*                              MT/NT constants                             */
/*  four MT nibbles represent double, float, int, uchar (from most          */
/*  significant to least significant).                                      */
/*  The values for each nibble are:                                         */
/*      1 - Big Endian                                                      */
/*      2 - VAX                                                             */
/*      3 - Cray                                                            */
/*      4 - Little Endian                                                   */
/*      5 - Convex                                                          */
/*      6 - Fijitsu VP                                                      */
/*--------------------------------------------------------------------------*/
#define     DFMT_SUN            0x1111
#define     DFMT_ALLIANT        0x1111
#define     DFMT_IRIS4          0x1111
#define     DFMT_APOLLO         0x1111
#define     DFMT_IBM6000        0x1111
#define     DFMT_HP9000         0x1111
#define     DFMT_CONVEXNATIVE   0x5511
#define     DFMT_CONVEX         0x1111
#define     DFMT_UNICOS         0x3331
#define     DFMT_CTSS           0x3331
#define     DFMT_VAX            0x2221
#define     DFMT_MIPSEL         0x4441
#define     DFMT_PC             0x4441
#define     DFMT_MAC            0x1111
#define     DFMT_SUN386         0x4441
#define     DFMT_NEXT           0x1111
#define     DFMT_MOTOROLA       0x1111
#define     DFMT_ALPHA          0x4441
#define     DFMT_VP             0x6611

/* I/O library constants */
#define UNIXUNBUFIO 1
#define UNIXBUFIO   2
#define MACIO       3
#define PCIO        4
#define WINIO       5

#include <stdio.h>

#ifdef __STDC__
#define PROTOTYPE
#include <stdlib.h>
#endif

#if defined (__cplusplus) || (defined c_plusplus)
#ifndef PROTOTYPE
#define PROTOTYPE
#endif
#endif

#if defined TEST_PC || defined TEST_WIN
#define FAR far
#else
#define FAR /* */
#endif

#if defined(SUN) || defined(sun)

#ifndef SUN
#define SUN
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#   define BSD
#include <string.h>
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <unistd.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT             DFMT_SUN
typedef void              VOID;
typedef char              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
typedef float             float32;
typedef double            float64;
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO
#ifndef __STDC__
#define const
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#ifdef __GNUC__
#define HAVE_STDC
#define INCLUDES_ARE_ANSI
#endif

#endif /* SUN */


#if defined(IBM6000) || defined(_AIX)

#ifndef IBM6000
#define IBM6000
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#   define BSD
#ifndef PROTOTYPE
#define PROTOTYPE		/* to invoke ANSI prototypes */
#endif	/* PROTOTYPE */

#include <string.h>
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT             DFMT_IBM6000
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* IBM6000 */

#if defined(HP9000) || defined(hpux)

#ifndef HP9000
#define HP9000
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#   define BSD
#include <string.h>
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT             DFMT_HP9000
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO
#ifndef __STDC__
#define const
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* HP9000 */


#if defined(IRIS4) || defined(sgi)

#ifndef IRIS4
#define IRIS4
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

#   define BSD
#include <string.h>
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT              DFMT_IRIS4
typedef void               VOID;
typedef void               *VOIDP;
typedef char               *_fcd;
typedef int                bool;
typedef signed char        char8;
typedef unsigned char      uchar8;
typedef signed char        int8;
typedef unsigned char      uint8;
typedef short int          int16;
typedef unsigned short int uint16;
typedef long int           int32;
typedef unsigned long int  uint32;
typedef int                intn;
typedef unsigned int       uintn;
typedef float              float32;
typedef double             float64;
typedef int                intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* IRIS4 */

#if defined(UNICOS) || defined(_UNICOS)

#ifndef UNICOS
#define UNICOS
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

#include <string.h>
#include <memory.h>
#include <fortran.h>
#ifndef O_RDONLY
#include <fcntl.h>              /* for unbuffered i/o stuff */
#define L_INCR  1
#endif /*O_RDONLY*/

#define DF_MT   DFMT_UNICOS
typedef void            VOID;
typedef void            *VOIDP;
typedef char            *_fcd;
typedef int             bool;
typedef signed char     char8;
typedef unsigned char   uchar8;
typedef signed char     int8;
typedef unsigned char   uint8;
typedef int             int16;
typedef unsigned int    uint16;
typedef int             int32;
typedef unsigned int    uint32;
typedef int             intn;
typedef unsigned int    uintn;
typedef float           float32;
typedef double          float64;
typedef int             intf;     /* size of INTEGERs in Fortran compiler */

#define _HUGE              /* This should only be defined to a value on the PC */
#define DF_CAPFNAMES            /* fortran names are in all caps */
#define FILELIB UNIXBUFIO


/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define RIGHT_SHIFT_IS_UNSIGNED
#define CHAR_IS_UNSIGNED

#endif /* UNICOS */

#ifdef VMS

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1
#include <file.h>               /* for unbuffered i/o stuff */

#define DF_MT              DFMT_VAX
typedef int                VOID;
typedef char               *VOIDP;
typedef char               *_fcd;
typedef int                bool;
typedef char               char8;
typedef unsigned char      uchar8;
typedef char               int8;
typedef unsigned char      uint8;
typedef short int          int16;
typedef unsigned short int uint16;
typedef long int           int32;
typedef unsigned long int  uint32;
typedef int                intn;
typedef unsigned int       uintn;
typedef float              float32;
typedef double             float64;
typedef int                intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define _fcdtocp(desc)  ((char *) *((char **) &desc[4]))

/* 
  Redef a couple of C routine names to avoid conflicts
  since the VMS link command is case-insensitive
*/
#define FILELIB UNIXBUFIO
#include "dfivms.h"


/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* VMS */

#if defined(CONVEX) || defined(CONVEXNATIVE) || defined(__convex__)

#ifndef CONVEX
#define CONVEX
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#include <sys/file.h>               /* for unbuffered i/o stuff */
#include <stdlib.h>

#define __STDC__					/* To invoke ANSI compilation */
#ifndef PROTOTYPE
#define PROTOTYPE					/* to invoke ANSI prototypes */
#endif	/* PROTOTYPE */

/* For Convex machines with native format floats */
#ifdef CONVEXNATIVE
#define DF_MT             DFMT_CONVEXNATIVE
#else
#define DF_MT             DFMT_CONVEX
#endif
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI
#define RIGHT_SHIFT_IS_UNSIGNED
#define INCLUDES_ARE_ANSI
#define HAVE_STDC

#endif /* CONVEX */


#ifdef MIPSEL

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

#include <sys/types.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT   DFMT_MIPSEL
typedef void            VOID;
typedef void            *VOIDP;
typedef char            *_fcd;
typedef int             bool;
typedef char            char8;
typedef unsigned char   uchar8;
typedef char            int8;
typedef unsigned char   uint8;
typedef short           int16;
typedef unsigned short  uint16;
typedef int             int32;
typedef unsigned int    uint32;
typedef int             intn;
typedef unsigned int    uintn;
typedef float           float32;
typedef double          float64;
typedef int             intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define _fcdtocp(desc) (desc)
#define FNAME_POST_UNDERSCORE
#define FILELIB UNIXBUFIO
#ifndef __STDC__
#define const
#endif /* __STDC__ */

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* MIPSEL */

#ifdef MAC

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1
#define PROTOTYPE

#include <memory.h>             /* malloc stuff for MPW */
#include <fcntl.h>              /* unbuffered I/O stuff for MPW */
#include <string.h>
#include <stdlib.h>
#ifdef THINK_C                  /* for THINK C */
#include <unix.h>
#define isascii(c)  (isprint(c) || iscntrl(c))
#else  /* MPW, possibly others */
#include <Files.h>              /* for unbuffered I/O stuff */
#endif /*THINK_C*/
#ifndef ABSOFT
#define DF_CAPFNAMES            /* fortran names are in all caps */
#endif /* ABSOFT */
#define DF_DYNAMIC              /* use dynamic allocation */
#define DF_MT   DFMT_MAC

typedef void              VOID;
typedef char              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
#define _HUGE              /* This should only be defined to a value on the PC */
#define _fcdtocp(desc) (desc)
void exit(int status);

#define register   /* don't mess with the MAC compiler's register allocation */
#define FILELIB MACIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /*MAC*/

#if defined WIN3 || defined __WINDOWS__
#ifndef WIN3
#define WIN3
#endif  /* WIN3 */
#define PC
#endif  /* WIN3 */

#if defined PC || defined MSDOS || defined M_I86 || defined M_I386 || defined PC386
#ifndef PC
#define PC
#endif /* PC */

#if defined M_I386 && !defined PC386
#define PC386
#endif /* M_I386 && !PC386 */

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1

#include <fcntl.h>
#include <sys\types.h>      /* for unbuffered file I/O */
#include <sys\stat.h>
#include <io.h>
#include <malloc.h>
#include <conio.h>          /* for debugging getch() calls */
#include <string.h>         /* for vaious string functions */
#include <limits.h>         /* for UINT_MAX used in various places */
#include <stdlib.h>
#include <ctype.h>          /* for character macros */
#ifdef WIN3
#ifndef GMEM_MOVEABLE       /* check if windows header is already included */
#include <windows.h>        /* include the windows headers */
#endif
#endif /* WIN3 */

#define DF_MT             DFMT_PC

#ifndef PROTOTYPE
/* Want prototypes for the PC even if we're not completely ANSI C compliant */
#define PROTOTYPE
#endif  /* !PROTOTYPE */

#ifndef VOID    /* The stupid windows.h header file uses a #define instead of a typedef */
typedef void              VOID;
#endif  /* end VOID */
#ifndef PC386
typedef void              huge *VOIDP;
typedef char              huge *_fcd;
#else   /* PC386 */
typedef void *            VOIDP;
typedef char *            _fcd;
#endif  /* PC386 */
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef float             float32;
typedef double            float64;
typedef long              intf;     /* size of INTEGERs in Fortran compiler */
#ifndef PC386
/* used to force the prototypes in hproto.h to use huge pointers */
#define _HUGE              huge
#else   /* PC386 */
#define _HUGE              /* This should only be defined to a value on the PC */
#endif  /* PC386 */

#ifdef PC386
#define DF_CAPFNAMES
#endif
#define _fcdtocp(desc) (desc)

#define register    /* don't mess with the PC compiler's register allocation */

#ifdef WIN3
#define FILELIB WINIO
#else /* ! WIN3 */
#ifdef defined PC386 /* !WIN3 */
#define FILELIB UNIXBUFIO
#else /* must be plain PC */
#define FILELIB PCIO
#endif /* PC */
#endif /* WIN3 */

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#ifndef PC386
#define JMEMSYS         MEM_DOS
#else   /* PC386 */
#define JMEMSYS         MEM_ANSI
#endif  /* PC386 */

#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#ifdef TEST_WIN
#ifdef stderr
#undef stderr
#endif  /* stderr */
extern FILE *dbg_file;
#define stderr dbg_file
#define stdout dbg_file
#endif  /* TEST_WIN */

#endif /* PC */

#if defined(NEXT) || defined(NeXT)

#ifndef NEXT
#define NEXT
#endif

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#include <string.h>
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT             DFMT_NEXT
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
typedef float             float32;
typedef double            float64;
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#define HAVE_STDC
#define INCLUDES_ARE_ANSI

#endif /* NEXT */

#ifdef MOTOROLA

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#include <string.h>
#ifndef __GNUC__
#include <memory.h>
#endif /* __GNUC__ */
#include <unistd.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#ifndef O_RDONLY
#include <fcntl.h>              /* for unbuffered i/o stuff */
#endif /*O_RDONLY*/
#define DF_MT             DFMT_MOTOROLA
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef long int          int32;
typedef unsigned long int uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
typedef float             float32;
typedef double            float64;
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO
#ifndef __STDC__
#define const
#endif

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* MOTOROLA */

#ifdef DEC_ALPHA

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE

#include <string.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DF_MT             DFMT_ALPHA
typedef void              VOID;
typedef void              *VOIDP;
typedef char              *_fcd;
typedef int               bool;
typedef char              char8;
typedef unsigned char     uchar8;
typedef char              int8;
typedef unsigned char     uint8;
typedef short int         int16;
typedef unsigned short int uint16;
typedef int               int32;
typedef unsigned int      uint32;
typedef int               intn;
typedef unsigned int      uintn;
typedef int               intf;     /* size of INTEGERs in Fortran compiler */
typedef float             float32;
typedef double            float64;
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#ifdef __GNUC__
#define HAVE_STDC
#define INCLUDES_ARE_ANSI
#endif

#endif /* DEC_ALPHA */

#ifdef VP

#ifdef GOT_MACHINE
If you get an error on this line more than one machine type has been defined.
Please check your Makefile.
#endif
#define GOT_MACHINE 1
#ifndef PROTOTYPE
#define PROTOTYPE
#endif /* !PROTOTYPE */
#include <string.h>
#include <memory.h>
#include <sys/types.h>
#define DF_MT              DFMT_VP
typedef void                VOID;
typedef void               *VOIDP;
typedef char               *_fcd;
typedef int                bool;
typedef char               char8;
typedef unsigned char      uchar8;
typedef char               int8;
typedef unsigned char      uint8;
typedef short int          int16;
typedef unsigned short int uint16;
typedef long int           int32;
typedef unsigned long int  uint32;
typedef int                intn;
typedef unsigned int       uintn;
typedef int                intf;     /* size of INTEGERs in Fortran compiler */
typedef float              float32;
typedef double             float64;
#define _HUGE              /* This should only be defined to a value on the PC */
#define FNAME_POST_UNDERSCORE
#define _fcdtocp(desc) (desc)
#define FILELIB UNIXBUFIO

/* JPEG #define's - Look in the JPEG docs before changing - (Q) */

/* Determine the memory manager we are going to use. Valid values are: */
/*  MEM_DOS, MEM_ANSI, MEM_NAME, MEM_NOBS.  See the JPEG docs for details on */
/*  what each does */
#define JMEMSYS         MEM_ANSI

#endif /* VP */

#ifndef GOT_MACHINE
No machine type has been defined.  Your Makefile needs to have someing like
-DSUN or -DUNICOS in order for the HDF internal structures to be defined
correctly.
#endif

/*-----------------------------------------------------*/
/*              encode and decode macros               */
/*-----------------------------------------------------*/

#   define INT16ENCODE(p, i) \
{ *(p) = (uint8)(((uint16)(i) >> 8) & 0xff); (p)++; \
        *(p) = (uint8)((uint16)(i) & 0xff); (p)++; }

#   define UINT16ENCODE(p, i) \
{ *(p) = (uint8)(((uint16)(i) >> 8) & 0xff); (p)++; *(p) = (uint8)((i) & 0xff); (p)++; }

#   define INT32ENCODE(p, i) \
{ *(p) = (uint8)(((uint32)(i) >> 24) & 0xff); (p)++; \
        *(p) = (uint8)(((uint32)(i) >> 16) & 0xff); (p)++; \
        *(p) = (uint8)(((uint32)(i) >> 8) & 0xff); (p)++; \
        *(p) = (uint8)((uint32)(i) & 0xff); (p)++; }

#   define UINT32ENCODE(p, i) \
{ *(p) = (uint8)(((i) >> 24) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 16) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 8) & 0xff); (p)++; \
        *(p) = (uint8)((i) & 0xff); (p)++; }

#   define INT16DECODE(p, i) \
{ (i) = (int16)((*(p) & 0xff) << 8); (p)++; \
        (i) |= (int16)((*(p) & 0xff)); (p)++; }

#   define UINT16DECODE(p, i) \
{ (i) = (uint16)((*(p) & 0xff) << 8); (p)++; \
        (i) |= (uint16)(*(p) & 0xff); (p)++; }

#   define INT32DECODE(p, i) \
{ (i) = ((int32)(*(p) & 0xff) << 24); (p)++; \
        (i) |= ((int32)(*(p) & 0xff) << 16); (p)++; \
        (i) |= ((int32)(*(p) & 0xff) << 8); (p)++; \
        (i) |= (*(p) & 0xff); (p)++; }

#   define UINT32DECODE(p, i) \
{ (i) = ((uint32)(*(p) & 0xff) << 24); (p)++; \
        (i) |= ((uint32)(*(p) & 0xff) << 16); (p)++; \
        (i) |= ((uint32)(*(p) & 0xff) << 8); (p)++; \
        (i) |= (*(p) & 0xff); (p)++; }

/**************************************************************************
*                   Conversion Routine Pointers
***************************************************************************/
#    ifndef DFKMASTER
#if defined __STDC__ || defined PC
extern int (*DFKnumin)(void _HUGE * source, void _HUGE * dest, uint32 num_elm,
            uint32 source_stride,uint32 dest_stride);
extern int (*DFKnumout)(void _HUGE * source, void _HUGE * dest, uint32 num_elm,
            uint32 source_stride,uint32 dest_stride);
#else
extern int (*DFKnumin)();
extern int (*DFKnumout)();
#endif
#     endif /* DFKMASTER */

/*****************************************************************
*  Create DFtbuf: a temporary buffer used when a small amount of
*  memory is needed, as when small conversions are done
******************************************************************/
#define DF_TBUFSZ       512     /* buffer size can be smaller */
#ifdef  HMASTER
    int    FAR int_DFtbuf[DF_TBUFSZ]; /* int declaration to force word boundary */
    uint8  FAR *DFtbuf = (uint8 *) int_DFtbuf;
#else /* !HMASTER */
extern uint8 FAR *DFtbuf;
#endif /*HMASTER*/

/*----------------------------------------------------------------
** MACRO FCALLKEYW for any special fortran-C stub keyword
**
** MacIntosh MPW LS-fortran needs pascal since it can interface
**  best with pascal functions.
** Microsoft C and Fortran need _fortran for Fortran callable C
**  routines.
**
** MACRO FRETVAL for any return value from a fortran-C stub function
**  Replaces the older FCALLKEYW macro.
**---------------------------------------------------------------*/
#ifdef FRETVAL
#undef FRETVAL
#endif

#if defined(MAC)                /* with LS FORTRAN */
#ifndef ABSOFT
#   define FCALLKEYW    pascal
#   define FRETVAL(x)   pascal x
#endif /* ABSOFT */
#endif

#if defined(PC)   /* with MS Fortran */
#   define FCALLKEYW    _fortran
#   define FRETVAL(x)   x _fortran
#endif

#ifndef FRETVAL /* !MAC && !PC */
#   define FCALLKEYW    /*NONE*/
#   define FRETVAL(x)   x
#endif


/*----------------------------------------------------------------
** MACRO FNAME for any fortran callable routine name.
**
**  This macro prepends, appends, or does not modify a name
**  passed as a macro parameter to it based on the FNAME_PRE_UNDERSCORE,
**  FNAME_POST_UNDERSCORE macros set for a specific system.
**
**---------------------------------------------------------------*/
#if defined(FNAME_PRE_UNDERSCORE) && defined(FNAME_POST_UNDERSCORE)
#ifdef __STDC__
#   define FNAME(x)     _##x##_
#else
#   define FNAME(x)     _/**/x/**/_
#endif
#endif
#if defined(FNAME_PRE_UNDERSCORE) && !defined(FNAME_POST_UNDERSCORE)
#ifdef __STDC__
#   define FNAME(x)     _##x
#else
#   define FNAME(x)     _/**/x
#endif
#endif
#if !defined(FNAME_PRE_UNDERSCORE) && defined(FNAME_POST_UNDERSCORE)
#ifdef __STDC__
#   define FNAME(x)     x##_
#else
#   define FNAME(x)     x/**/_
#endif
#endif
#if !defined(FNAME_PRE_UNDERSCORE) && !defined(FNAME_POST_UNDERSCORE)
#   define FNAME(x)     x
#endif

/**************************************************************************
*  Debugging Allocation functions
**************************************************************************/
#ifdef MALDEBUG
#include "maldebug.h"
#endif

/**************************************************************************
*  String functions defined differently under MS Windows
**************************************************************************/

#ifdef WIN3
#  define HDstrcat(s1,s2)   (_fstrcat((s1),(s2)))
#  define HDstrcmp(s,t)     (_fstrcmp((s),(t)))
#  define HDstrcpy(s,d)     (_fstrcpy((s),(d)))
#  define HDstrlen(s)       (_fstrlen(s))
#  define HDstrncmp(s1,s2,n)    (_fstrncmp((s1),(s2),(n)))
#  define HDstrncpy(s1,s2,n)    (_fstrncpy((s1),(s2),(n)))
#  define HDstrchr(s,c)    (_fstrchr((s),(c)))
#else
#  define HDstrcat(s1,s2)   (strcat((s1),(s2)))
#  define HDstrcmp(s,t)     (strcmp((s),(t)))
#  define HDstrcpy(s,d)     (strcpy((s),(d)))
#  define HDstrlen(s)       (strlen((const char *)s))
#  define HDstrncmp(s1,s2,n)    (strncmp((s1),(s2),(n)))
#  define HDstrncpy(s1,s2,n)    (strncpy((s1),(s2),(n)))
#  define HDstrchr(s,c)    (strchr((s),(c)))
/* Can't use on PCs. strdup() uses malloc() and HDgetspace uses halloc() */
#if !(defined VMS | (defined PC & !defined PC386) | defined macintosh)
#  define HDstrdup(s)      (strdup((s)))
#endif /* !(VMS | PC) */
#endif /* WIN3 */


/**************************************************************************
*  Memory functions defined differently under MS Windows
**************************************************************************/

#if defined PC & !defined PC386
#   ifdef WIN3
#       define HDmemcpy(dst,src,n)   (fmemcpy_big((dst),(src),(n)))
#       define HDmemset(dst,c,n)     (fmemset_big((dst),(c),(n)))
#       define HDmemcmp(dst,src,n)   (fmemcmp_big((dst),(src),(n)))
#   else    /* !WIN3 */
#       define HDmemcpy(dst,src,n)   (memcpy_big((dst),(src),(n)))
#       define HDmemset(dst,c,n)     (memset_big((dst),(c),(n)))
#       define HDmemcmp(dst,src,n)   (memcmp_big((dst),(src),(n)))
#   endif   /* WIN3 */
#else   /* !WIN & !PC */
# define HDmemcpy(dst,src,n)   (memcpy((dst),(src),(n)))
# define HDmemset(dst,c,n)     (memset((dst),(c),(n)))
# define HDmemcmp(dst,src,n)   (memcmp((dst),(src),(n)))
#endif /* WIN3 | PC */

/* Compatibility #define for V3.3, should be taken out by v4.0 - QAK */
#define DFSDnumber DFSDndatasets

#endif /* HDFI_H */
