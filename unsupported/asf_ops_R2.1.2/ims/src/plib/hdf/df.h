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

/*-----------------------------------------------------------------------------
 * File:    df.h
 * Purpose: header file for HDF routines
 * Invokes: dfi.h
 * Contents: 
 *  Structure definitions: DFddh, DFdd, DFdesc, DFdle, DF, DFdi, DFdata
 *  Procedure type definitions
 *  Global variables
 *  Tag definitions
 *  Error return codes
 *  Logical constants
 * Remarks: This file is included with user programs
 *          Since it includes stdio.h etc., do not include these after df.h
 *---------------------------------------------------------------------------*/


#ifndef DF_H              /* avoid re-inclusion */
#define DF_H

/* include DF (internal) header information */
#include "hdf.h"
#include "herr.h"
/*#include "dfi.h"*/

/*-------------------------------------------------------------------------*/
/*                      Type declarations                                   */

typedef struct DFddh {		/*format of data descriptor headers in file*/
    int16 dds;			/* number of dds in header block */
    int32 next;			/* offset of next header block */
} DFddh;

typedef struct DFdd {		/* format of data descriptors as in file */
    uint16 tag;			/* data tag */
    uint16 ref;			/* data reference number */
    int32 offset;		/* offset of data element in file */
    int32 length;		/* number of bytes */
} DFdd;

/* descriptor structure is same as dd structure.  ###Note: may be changed */
typedef DFdd DFdesc;

/* DLE is the internal structure which stores data descriptor information */
/* It is a linked list of DDs */
typedef struct DFdle {		/* Data List element */
    struct DFdle *next;		/* link to next dle */
    DFddh ddh;			/* To store headers */
    DFdd dd[1];			/* dummy size */
} DFdle;

/* DF is the internal structure associated with each DF file */
/* It holds information associated with the file as a whole */
/* ### Note: there are hooks for having multiple DF files open at a time */
typedef struct DF {
    DFdle *list;		/* Pointer to the DLE list */
    DFdle *last_dle;		/* last_dle and last_dd are used in searches */
				/* to indicate element returned */
				/* by previous call to DFfind */
    intn type;  /* 0= not in use, 1= normal, -1 = multiple */
				/* this is a hook for when */
				/* multiple files are open */
    intn access;/* permitted access types: */
				/* 0=none, 1=r, 2=w, 3=r/w */
    intn changed;   /* True if anything in DDs modified */
				/* since last write */
    uint16 last_tag;		/* Last tag searched for by DFfind */
    uint16 last_ref;		/* Last reference number searched for */
    intn last_dd;   /* see last_dle */
    intn defdds;    /* default numer of DD's in each block */
    intn up_access; /* access permissions to element being */
				/* read/updated. Used by DFstart */
    DFdd *up_dd;		/* DD of element being read/updated, */
				/* used by DFstart */
    /* File handle is a file pointer or file descriptor depending on whether */
    /* we use buffered or unbuffered I/O.  But, since this structure is a */
    /* fake, it doesn't matter whether I/O is buffered or not. */
    int file;			/* file descriptor */
} DF;


typedef struct DFdata { /* structure for returning status information */
    int32 version;        /* version number of program */
} DFdata;

/*--------------------------------------------------------------------------*/
/*                          Procedure types                                 */

/* prototypes for dfstubs.c */
extern DF *DFopen
  PROTO((char *name, int access, int ndds));

extern int DFclose
  PROTO((DF *dfile));

extern int DFdescriptors
  PROTO((DF *dfile, DFdesc ptr[], int begin, int num));

extern int DFnumber
  PROTO((DF *dfile, uint16 tag));

extern int DFsetfind
  PROTO((DF *dfile, uint16 tag, uint16 ref));

extern int DFfind
  PROTO((DF *dfile, DFdesc *ptr));

extern int DFaccess
  PROTO((DF *dfile, uint16 tag, uint16 ref, char *access));

extern int DFstart
  PROTO((DF *dfile, uint16 tag, uint16 ref, char *access));

extern int32 DFread
  PROTO((DF *dfile, char *ptr, int32 len));

extern int32 DFseek
  PROTO((DF *dfile, int32 offset));

extern int32 DFwrite
  PROTO((DF *dfile, char *ptr, int32 len));

extern int DFupdate
  PROTO((DF *dfile));

extern int DFstat
  PROTO((DF *dfile, DFdata *dfinfo));

extern int32 DFgetelement
  PROTO((DF *dfile, uint16 tag, uint16 ref, char *ptr));

extern int32 DFputelement
  PROTO((DF *dfile, uint16 tag, uint16 ref, char *ptr, int32 len));

extern int DFdup
  PROTO((DF *dfile, uint16 itag, uint16 iref, uint16 otag, uint16 oref));

extern int DFdel
  PROTO((DF *dfile, uint16 tag, uint16 ref));

extern uint16 DFnewref
  PROTO((DF *dfile));

extern int DFishdf
  PROTO((char *filename));

extern int DFerrno
  PROTO((void));

extern int DFIerr
  PROTO((DF *dfile));

extern int DFImemcopy
  PROTO((char *from, char *to, register int length));

extern void *DFIgetspace
  PROTO((uint32 qty));

extern void *DFIfreespace
  PROTO((void *ptr));

extern int DFIc2fstr
  PROTO((char *str, int len));

extern char *DFIf2cstring
  PROTO((_fcd fdesc, intn len));

#ifdef PC
extern int32 DFIspaceleft
  PROTO((void));
#endif /* PC */

/*--------------------------------------------------------------------------*/
/*                          Global Variables                                */

#ifndef DFMASTER
extern
#endif /*DFMASTER*/
int
#ifdef PC
far
#endif /* PC */
DFerror;            /* Error code for DF routines */

#define DFSETERR(error) (DFerror=(DFerror?DFerror:error))

#define DFTOFID(df) (int32)(df->list)

#endif /* DF_H */
