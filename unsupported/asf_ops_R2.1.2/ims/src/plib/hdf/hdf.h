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

#ifndef HDF_H
#define HDF_H

#include "hdfi.h"

#ifdef PROTO
#undef PROTO
#endif /* PROTO */
#ifdef PROTOTYPE
#define    PROTO(x) x
#else
#define    PROTO(x) ()
#endif

typedef struct {
    uint16 tag, ref;
} DFdi;

/* internal file access codes */

#define DFACC_READ 1
#define DFACC_WRITE 2
#define DFACC_CREATE 4
#define DFACC_ALL 7

#define DFACC_RDONLY 1
#define DFACC_RDWR 3
#define DFACC_CLOBBER 4

/* used by Hnextread to determine where to start searching for the
   next tag/ref to read */

#define DF_START 0
#define DF_CURRENT 1
#define DF_END 2

/* Used by Hfind to determine the direction to search for tag/ref's in the */
/* file. */

#define DF_FORWARD  1
#define DF_BACKWARD 2

/* return code - since some unix/c routines use 0 and -1 as their return
   code, and some assumption had been made in the code about that, it is
   important to keep these constants the same values.  For explicitly
   boolean functions, use TRUE and FALSE */

#define SUCCEED 0
#define FAIL (-1)

/* boolean values,  reminder: NEVER compare with numeric values */

#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE (!FALSE)
#endif

#ifndef NULL
#   define NULL (void *)0
#endif

/* macros */

#ifdef WIN3
#define STREQ(s, t) (_fstrcmp((s), (t)) == 0)
#define NSTREQ(s, t, n) (_fstrncmp((s), (t), (n)) == 0)
#else
#define STREQ(s, t) (strcmp((s), (t)) == 0)
#define NSTREQ(s, t, n) (strncmp((s), (t), (n)) == 0)
#endif

#ifndef PRIVATE
#   define PRIVATE static
#endif
#ifndef PUBLIC
#define PUBLIC  /* nothing */
#endif

/* functions declarations */


/* masks for types */
#define DFNT_HDF      0x00000000    /* standard HDF format  */
#define DFNT_NATIVE   0x00001000    /* native format        */
#define DFNT_CUSTOM   0x00002000    /* custom format        */
#define DFNT_LITEND   0x00004000    /* Little Endian format */

/* type info codes */

#define DFNT_NONE        0    /* indicates that number type not set */
#define DFNT_QUERY       0    /* use this code to find the current type */
#define DFNT_VERSION     1    /* current version of NT info */

#define DFNT_FLOAT32     5
#define DFNT_FLOAT       5    /* For backward compat; don't use */
#define DFNT_FLOAT64     6
#define DFNT_DOUBLE      6    /* For backward compat; don't use */
#define DFNT_FLOAT128    7    /* No current plans for support */

#define DFNT_INT8       20
#define DFNT_UINT8      21

#define DFNT_INT16      22
#define DFNT_UINT16     23
#define DFNT_INT32      24
#define DFNT_UINT32     25
#define DFNT_INT64      26
#define DFNT_UINT64     27
#define DFNT_INT128     28    /* No current plans for support */
#define DFNT_UINT128    30    /* No current plans for support */

#define DFNT_UCHAR8      3     /* 3 chosen for backward compatibility */
#define DFNT_UCHAR       3     /* uchar=uchar8 for backward combatibility */
#define DFNT_CHAR8       4     /* 4 chosen for backward compatibility */
#define DFNT_CHAR        4     /* uchar=uchar8 for backward combatibility */
#define DFNT_CHAR16     42     /* No current plans for support */
#define DFNT_UCHAR16    43     /* No current plans for support */

/* Type info codes for Native Mode datasets */
#define DFNT_NFLOAT32   (DFNT_NATIVE | DFNT_FLOAT32)
#define DFNT_NFLOAT64   (DFNT_NATIVE | DFNT_FLOAT64)
#define DFNT_NFLOAT128  (DFNT_NATIVE | DFNT_FLOAT128)  /* Unsupported */

#define DFNT_NINT8      (DFNT_NATIVE | DFNT_INT8)
#define DFNT_NUINT8     (DFNT_NATIVE | DFNT_UINT8)
#define DFNT_NINT16     (DFNT_NATIVE | DFNT_INT16)
#define DFNT_NUINT16    (DFNT_NATIVE | DFNT_UINT16)
#define DFNT_NINT32     (DFNT_NATIVE | DFNT_INT32)
#define DFNT_NUINT32    (DFNT_NATIVE | DFNT_UINT32)
#define DFNT_NINT64     (DFNT_NATIVE | DFNT_INT64)
#define DFNT_NUINT64    (DFNT_NATIVE | DFNT_UINT64)
#define DFNT_NINT128    (DFNT_NATIVE | DFNT_INT128)   /* Unsupported */
#define DFNT_NUINT128   (DFNT_NATIVE | DFNT_UINT128)  /* Unsupported */

#define DFNT_NCHAR8     (DFNT_NATIVE | DFNT_CHAR8)
#define DFNT_NCHAR      (DFNT_NATIVE | DFNT_CHAR8)  /* backward compat */
#define DFNT_NUCHAR8    (DFNT_NATIVE | DFNT_UCHAR8)
#define DFNT_NUCHAR     (DFNT_NATIVE | DFNT_UCHAR8) /* backward compat */
#define DFNT_NCHAR16    (DFNT_NATIVE | DFNT_CHAR16)   /* Unsupported */
#define DFNT_NUCHAR16   (DFNT_NATIVE | DFNT_UCHAR16)   /* Unsupported */

/* Type info codes for Little Endian data */
#define DFNT_LFLOAT32   (DFNT_LITEND | DFNT_FLOAT32)
#define DFNT_LFLOAT64   (DFNT_LITEND | DFNT_FLOAT64)
#define DFNT_LFLOAT128  (DFNT_LITEND | DFNT_FLOAT128)   /* Unsupported */

#define DFNT_LINT8      (DFNT_LITEND | DFNT_INT8)
#define DFNT_LUINT8     (DFNT_LITEND | DFNT_UINT8)
#define DFNT_LINT16     (DFNT_LITEND | DFNT_INT16)
#define DFNT_LUINT16    (DFNT_LITEND | DFNT_UINT16)
#define DFNT_LINT32     (DFNT_LITEND | DFNT_INT32)
#define DFNT_LUINT32    (DFNT_LITEND | DFNT_UINT32)
#define DFNT_LINT64     (DFNT_LITEND | DFNT_INT64)
#define DFNT_LUINT64    (DFNT_LITEND | DFNT_UINT64)
#define DFNT_LINT128    (DFNT_LITEND | DFNT_INT128)     /* Unsupported */
#define DFNT_LUINT128   (DFNT_LITEND | DFNT_UINT128)    /* Unsupported */

#define DFNT_LCHAR8     (DFNT_LITEND | DFNT_CHAR8)
#define DFNT_LCHAR      (DFNT_LITEND | DFNT_CHAR8)      /* backward compat */
#define DFNT_LUCHAR8    (DFNT_LITEND | DFNT_UCHAR8)
#define DFNT_LUCHAR     (DFNT_LITEND | DFNT_UCHAR8)     /* backward compat */
#define DFNT_LCHAR16    (DFNT_LITEND | DFNT_CHAR16)     /* Unsupported */
#define DFNT_LUCHAR16   (DFNT_LITEND | DFNT_UCHAR16)    /* Unsupported */

/* class info codes for int */
#define        DFNTI_MBO       1       /* Motorola byte order 2's compl */
#define        DFNTI_VBO       2       /* Vax byte order 2's compl */
#define        DFNTI_IBO       4       /* Intel byte order 2's compl */

/* class info codes for float */
#define        DFNTF_NONE      0       /* indicates subclass is not set */
#define        DFNTF_HDFDEFAULT 1      /* hdf default float format is ieee */
#define        DFNTF_IEEE      1       /* IEEE format */
#define        DFNTF_VAX       2       /* Vax format */
#define        DFNTF_CRAY      3       /* Cray format */
#define        DFNTF_PC        4       /* PC floats - flipped IEEE */
#define        DFNTF_CONVEX    5       /* CONVEX native format */
#define        DFNTF_VP        6       /* Fujitsu VP native format */

/* class info codes for char */
#define        DFNTC_BYTE      0       /* bitwise/numeric field */
#define        DFNTC_ASCII     1       /* ASCII */
#define        DFNTC_EBCDIC    5       /* EBCDIC */

/* array order */
#define        DFO_FORTRAN     1       /* column major order */
#define        DFO_C           2       /* row major order */

/*******************************************************************/
/* Sizes of number types                                            */
/*******************************************************************/

/* first the standard sizes of number types */

#    define SIZE_FLOAT32    4
#    define SIZE_FLOAT64    8
#    define SIZE_FLOAT128  16    /* No current plans for support */

#    define SIZE_INT8       1
#    define SIZE_UINT8      1
#    define SIZE_INT16      2
#    define SIZE_UINT16     2
#    define SIZE_INT32      4
#    define SIZE_UINT32     4
#    define SIZE_INT64      8
#    define SIZE_UINT64     8
#    define SIZE_INT128    16   /* No current plans for support */
#    define SIZE_UINT128   16   /* No current plans for support */

#    define SIZE_CHAR8      1
#    define SIZE_CHAR       1    /* For backward compat char8 == char */
#    define SIZE_UCHAR8     1
#    define SIZE_UCHAR      1    /* For backward compat uchar8 == uchar */
#    define SIZE_CHAR16     2    /* No current plans for support */
#    define SIZE_UCHAR16    2    /* No current plans for support */

/* then the native sizes of number types */

#if !defined(UNICOS)
#    define SIZE_NFLOAT32    4
#    define SIZE_NFLOAT64    8
#    define SIZE_NFLOAT128  16    /* No current plans for support */

#    define SIZE_NINT8       1
#    define SIZE_NUINT8      1
#    define SIZE_NINT16      2
#    define SIZE_NUINT16     2
#    define SIZE_NINT32      4
#    define SIZE_NUINT32     4
#    define SIZE_NINT64      8
#    define SIZE_NUINT64     8
#    define SIZE_NINT128    16   /* No current plans for support */
#    define SIZE_NUINT128   16   /* No current plans for support */

#    define SIZE_NCHAR8      1
#    define SIZE_NCHAR       1    /* For backward compat char8 == char */
#    define SIZE_NUCHAR8     1
#    define SIZE_NUCHAR      1    /* For backward compat uchar8 == uchar */
#    define SIZE_NCHAR16     2    /* No current plans for support */
#    define SIZE_NUCHAR16    2    /* No current plans for support */
#else  /* !!!!!! SOMEBODY NEEDS TO CHECK THESE !!!!! */
#    define SIZE_NFLOAT32    8
#    define SIZE_NFLOAT64    8
#    define SIZE_NFLOAT128  16    /* No current plans for support */

#    define SIZE_NINT8       1
#    define SIZE_NUINT8      1
#    define SIZE_NINT16      8
#    define SIZE_NUINT16     8
#    define SIZE_NINT32      8
#    define SIZE_NUINT32     8
#    define SIZE_NINT64      8
#    define SIZE_NUINT64     8
#    define SIZE_NINT128    16   /* No current plans for support */
#    define SIZE_NUINT128   16   /* No current plans for support */
#    define SIZE_NCHAR8      1
#    define SIZE_NCHAR       1
#    define SIZE_NCHAR       1    /* For backward compat char8 == char */
#    define SIZE_NUCHAR8     1
#    define SIZE_NUCHAR      8    /* For backward compat uchar8 == uchar */
#    define SIZE_NCHAR16     8    /* No current plans for support */
#    define SIZE_NUCHAR16    2    /* No current plans for support */
#endif /* UNICOS */

/* then the sizes of little-endian number types */
#    define SIZE_LFLOAT32    4
#    define SIZE_LFLOAT64    8
#    define SIZE_LFLOAT128  16    /* No current plans for support */

#    define SIZE_LINT8       1
#    define SIZE_LUINT8      1
#    define SIZE_LINT16      2
#    define SIZE_LUINT16     2
#    define SIZE_LINT32      4
#    define SIZE_LUINT32     4
#    define SIZE_LINT64      8
#    define SIZE_LUINT64     8
#    define SIZE_LINT128    16   /* No current plans for support */
#    define SIZE_LUINT128   16   /* No current plans for support */

#    define SIZE_LCHAR8      1
#    define SIZE_LCHAR       1    /* For backward compat char8 == char */
#    define SIZE_LUCHAR8     1
#    define SIZE_LUCHAR      1    /* For backward compat uchar8 == uchar */
#    define SIZE_LCHAR16     2    /* No current plans for support */
#    define SIZE_LUCHAR16    2    /* No current plans for support */

          /* sizes of different number types */
#          define MACHINE_I8_SIZE     1
#          define MACHINE_I16_SIZE    2
#          define MACHINE_I32_SIZE    4
#          define MACHINE_F32_SIZE    4
#          define MACHINE_F64_SIZE    8

/* tbuf used as a temporary buffer for small jobs.  The size is
   preferably > 512 but MUST be > ~256.  It is advised that if an
   arbitrarily large buffer (> 100 bytes) is require, dynamic space be
   used.  tbuf lives in the hfile.c */

#ifndef TBUF_SZ
#if defined(macintosh) | defined(THINK_C)
#   define TBUF_SZ 256
#else /* !macintosh */
#   define TBUF_SZ 1024
#endif /* !macintosh */
#endif

extern uint8 *tbuf;

/* tags and refs */
#define DFREF_WILDCARD 0
#define DFTAG_WILDCARD 0
#define DFTAG_NULL 1
#define DFTAG_LINKED 20         /* check uniqueness */
#define DFTAG_VERSION 30

/* utility set */
#define DFTAG_FID   ((uint16)100) /* File identifier */
#define DFTAG_FD    ((uint16)101) /* File description */
#define DFTAG_TID   ((uint16)102) /* Tag identifier */
#define DFTAG_TD    ((uint16)103) /* Tag descriptor */
#define DFTAG_DIL   ((uint16)104) /* data identifier label */
#define DFTAG_DIA   ((uint16)105) /* data identifier annotation */
#define DFTAG_NT    ((uint16)106) /* number type */
#define DFTAG_MT    ((uint16)107) /* machine type */

/* raster-8 set */
#define DFTAG_ID8   ((uint16)200) /* 8-bit Image dimension */
#define DFTAG_IP8   ((uint16)201) /* 8-bit Image palette */
#define DFTAG_RI8   ((uint16)202) /* Raster-8 image */
#define DFTAG_CI8   ((uint16)203) /* RLE compressed 8-bit image */
#define DFTAG_II8   ((uint16)204) /* IMCOMP compressed 8-bit image */

/* Raster Image set */
#define DFTAG_ID    ((uint16)300) /* Image DimRec */
#define DFTAG_LUT   ((uint16)301) /* Image Palette */
#define DFTAG_RI    ((uint16)302) /* Raster Image */
#define DFTAG_CI    ((uint16)303) /* Compressed Image */

#define DFTAG_RIG   ((uint16)306) /* Raster Image Group */
#define DFTAG_LD    ((uint16)307) /* Palette DimRec */
#define DFTAG_MD    ((uint16)308) /* Matte DimRec */
#define DFTAG_MA    ((uint16)309) /* Matte Data */
#define DFTAG_CCN   ((uint16)310) /* color correction */
#define DFTAG_CFM   ((uint16)311) /* color format */
#define DFTAG_AR    ((uint16)312) /* aspect ratio */

#define DFTAG_DRAW  ((uint16)400) /* Draw these images in sequence */
#define DFTAG_RUN   ((uint16)401) /* run this as a program/script */

#define DFTAG_XYP   ((uint16)500) /* x-y position */
#define DFTAG_MTO   ((uint16)501) /* machine-type override */

/* Tektronix */
#define DFTAG_T14   ((uint16)602) /* TEK 4014 data */
#define DFTAG_T105  ((uint16)603) /* TEK 4105 data */

/* Scientific Data set */
#define DFTAG_SDG   ((uint16)700) /* Scientific Data Group */
#define DFTAG_SDD   ((uint16)701) /* Scientific Data DimRec */
#define DFTAG_SD    ((uint16)702) /* Scientific Data */
#define DFTAG_SDS   ((uint16)703) /* Scales */
#define DFTAG_SDL   ((uint16)704) /* Labels */
#define DFTAG_SDU   ((uint16)705) /* Units */
#define DFTAG_SDF   ((uint16)706) /* Formats */
#define DFTAG_SDM   ((uint16)707) /* Max/Min */
#define DFTAG_SDC   ((uint16)708) /* Coord sys */
#define DFTAG_SDT   ((uint16)709) /* Transpose */
#define DFTAG_SDLNK ((uint16)710) /* Links related to the dataset */
#define DFTAG_NDG   ((uint16)720) /* Numeric Data Group */
#define DFTAG_CAL   ((uint16)731) /* Calibration information */
#define DFTAG_FV    ((uint16)732) /* Fill Value information */
#define DFTAG_BREQ  ((uint16)799) /* Beginning of required tags   */
#define DFTAG_EREQ  ((uint16)780) /* Current end of the range   */

/* VSets */
#define DFTAG_VG     ((uint16)1965) /* Vgroup */
#define DFTAG_VH     ((uint16)1962) /* Vdata Header */
#define DFTAG_VS     ((uint16)1963) /* Vdata Storage */

/* compression schemes */
#define DFTAG_RLE   ((uint16)11)    /* run length encoding */
#define DFTAG_IMC   ((uint16)12)    /* IMCOMP compression alias */
#define DFTAG_IMCOMP ((uint16)12)   /* IMCOMP compression */
#define DFTAG_JPEG  ((uint16)13)    /* JPEG compression (24-bit data) */
#define DFTAG_GREYJPEG  ((uint16)14)    /* JPEG compression (8-bit data) */

/* Interlace schemes */
#define DFIL_PIXEL   0  /* Pixel Interlacing */
#define DFIL_LINE    1  /* Scan Line Interlacing */
#define DFIL_PLANE   2  /* Scan Plane Interlacing */

/* SPECIAL CODES */
#define SPECIAL_LINKED 1
#define SPECIAL_EXT 2

/* PARAMETERS */

#define DF_MAXFNLEN 256

/* .................................................................. */

/* MACROS for Hinquire. feb-2-92 */

#define HQueryfileid(aid, fileid)	  \
  (Hinquire ((int32)   aid,  (int32*) fileid, (uint16*) NULL,\
             (uint16*) NULL, (int32*) NULL,   (int32*)  NULL,\
             (int32*)  NULL, (int16*) NULL,   (int16*)  NULL))

#define HQuerytagref(aid, tag, ref)	\
  (Hinquire ((int32)   aid,  (int32*) NULL,   (uint16*) tag,\
             (uint16*) ref,  (int32*) NULL,   (int32*)  NULL,\
             (int32*)  NULL, (int16*) NULL,   (int16*)  NULL))

#define HQuerylength(aid, length)	  \
  (Hinquire ((int32)	aid, (int32*) NULL,	(uint16*) NULL,	  \
             (uint16*) NULL, (int32*) length,	(int32*)  NULL,	  \
             (int32*)  NULL, (int16*) NULL,	(int16*)  NULL))

#define HQueryoffset(aid, offset)	  \
  (Hinquire ((int32) 	aid, (int32*) NULL,	(uint16*) NULL,	  \
             (uint16*) NULL, (int32*) NULL, 	(int32*)  offset, \
             (int32*)  NULL, (int16*) NULL,  	(int16*)  NULL))

#define HQueryposition(aid, position) \
  (Hinquire ((int32) 	aid, (int32*) NULL,	(uint16*) NULL,	  \
             (uint16*) NULL, (int32*) NULL,	(int32*)  NULL,	  \
             (int32*) position, (int16*) NULL,	(int16*)  NULL))

#define HQueryaccess(aid, access)	  \
  (Hinquire ((int32) 	aid,	(int32*) NULL,	 (uint16*) NULL,  \
             (uint16*) NULL,	(int32*) NULL,	 (int32*)  NULL,  \
             (int32*) 	NULL, 	(int16*) access, (int16*)  NULL))

#define HQueryspecial(aid, special)	\
  (Hinquire ((int32) 	aid,	(int32*) NULL,	(uint16*) NULL,	  \
             (uint16*) NULL,	(int32*) NULL,	(int32*)  NULL,	  \
             (int32*) 	NULL,	(int16*) NULL,	(int16*)  special))

/* ----------------------------- VSet Defines ----------------------------- */
/* 
* some max lengths 
*
* Except for FIELDNAMELENMAX, change these as you please, they 
* affect memory only, not the file. 
*
*/

#define FIELDNAMELENMAX        128  /* fieldname   : 16 chars max */
#if defined macintosh | THINK_C
#define VSFIELDMAX             64  /* max no of fields per vdata */
#else /* !macintosh */
#define VSFIELDMAX             256  /* max no of fields per vdata */
#endif /* !macintosh */
#define VSNAMELENMAX		64  /* vdata name  : 64 chars max */    
#define VGNAMELENMAX		64  /* vgroup name : 64 chars max */ 

/* maximum number of files (number of slots for file records) */

#ifndef MAX_VFILE
#   define MAX_VFILE 16
#endif

/* 
* interlacing supported by the vset. 
*/

#define FULL_INTERLACE	0
#define NO_INTERLACE    1

/* 
 * default max no of objects in a vgroup 
 * VGroup will grow dynamically if needed 
 */
#define MAXNVELT       64 

/* 
 * Defaults for linked block operations with Vsets
 */
#define VDEFAULTBLKSIZE    1024
#define VDEFAULTNBLKS      32

/* Max order of a field in a Vdata */
#define MAX_ORDER          32000

/* type of ID to send to Hlevel */
typedef int32           HFILEID;

/* 
 * macros for VSinquire 
 * all these macros should be public for users 
 */
#define VSQuerycount(vs, count) \
        (VSinquire (vs, (int32 *) count, (int32*) NULL, (char*) NULL, (int32*) NULL, (char*) NULL))

#define VSQueryinterlace(vs, intr) \
        (VSinquire (vs, (int32 *) NULL, (int32*) intr, (char*) NULL, (int32*) NULL, (char*) NULL))

#define VSQueryfields(vs, flds) \
        (VSinquire (vs, (int32 *) NULL, (int32*) NULL, (char*) flds, (int32*) NULL, (char*) NULL))

#define VSQueryvsize(vs, size) \
        (VSinquire (vs, (int32 *) NULL, (int32*) NULL, (char*) NULL, (int32*) size, (char*) NULL))

#define VSQueryname(vs, name) \
        (VSinquire (vs, (int32 *) NULL, (int32*) NULL, (char*) NULL, (int32*) NULL, (char*) name))

/* .................................................................. */

/* Publically accessible functions declarations.  This includes all the
   functions that are used by application programs.  */

#include "hcomp.h"
#include "hproto.h"
#include "vproto.h"

/* these may eventaully evolve into real-life functions but not yet */
#define HDFopen(f,a,d)      Hopen((f), (a), (d))
#define HDFclose(f,a,d)     Hclose((f), (a), (d))
#define Vstart(f)           Vinitialize((f))
#define Vend(f)             Vfinish((f))

#endif /* HDF_H */
