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
 * File:    dfi.h
 * Purpose: HDF internal header file
 * Invokes: stdio.h, sys/file.h
 * Contents: 
 *  Compilation parameters
 *  Machine-dependent definitions
 *  Flexibility definitions: i/o buffering, dynamic memory, structure i/o
 *  Size parameters
 * Remarks: To port to a new system, only dfi.h and Makefile need be modified.
 *          This file is included with user programs, but users do not see it.
 *---------------------------------------------------------------------------*/


#ifndef DFI_H
#define DFI_H

/*--------------------------------------------------------------------------*/
/*          Compilation Parameters for Flexibility and Portability          */

/* modify this line for buffered/unbuffered i/o */
#define	DF_BUFFIO

/* modify this line for dynamic/static memory allocation */
#define	DF_DYNAMIC

/* modify this line if structures cannot be read/written as is */
#undef	DF_STRUCTOK		/* leave it this way - hdfsh expects it */

#ifdef PERM_OUT
/* Current version number */
#define	DFVERSION   3.20
#endif /* PERM_OUT */

/*--------------------------------------------------------------------------*/
/*                      Machine dependencies                                */
/*--------------------------------------------------------------------------*/

#ifdef IRIS4
#undef DF_STRUCTOK
#include <sys/types.h>
#include <sys/file.h>               /* for unbuffered i/o stuff */
#ifndef DFmovmem
#define	DFmovmem(from, to, len) bcopy(from, to, len)
#endif /* DFmovmem */
#ifndef DF_STRUCTOK
#define	UINT16READ(p, x)    { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x)     { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x)     { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
                                x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x)   { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x)    { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x)    { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;   \
                                *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#ifndef DF_MT
#define DF_MT   DFMT_IRIS4
#endif /* DF_MT  */
#endif /*IRIS4*/


#ifdef IBM6000  /* NOTE: IBM6000 defines are same as for SUN */
#if ! defined mc68010 && ! defined mc68020 && ! defined mc68030
#undef DF_STRUCTOK
#endif
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_IBM6000
#endif /*IBM6000*/


#ifdef MAC
#undef DF_BUFFIO		/* use unbuffered i/o */
#include <memory.h>             /* malloc stuff for MPW 3.0 */
#include <fcntl.h>              /* unbuffered IO stuff for MPW 3.0 */
#ifdef THINK_C                  /* for LightSpeed C */
#include <unix.h>
#else /*THINK_C                   MPW, possibly others */
#include <Files.h>              /* for unbuffered i/o stuff */
#endif /*THINK_C*/
#define	DF_CAPFNAMES            /* fortran names are in all caps */
#define DF_DYNAMIC		/* use dynamic allocation */
#ifdef THINK_C                   /* LightSpeed C does not have memcpy */
#define DFmovmem(from, to, len) DFImemcopy(from, to, len)
#else /*THINK_C*/
#define DFmovmem(from, to, len) memcpy(to, from, len)
#endif /*THINK_C*/
#define malloc(x)   NewPtr((Size)   (x))    /* don't use malloc on the Mac */
#define free(x)     DisposPtr((Ptr) (x))    /* don't use free on the Nac   */ 
#undef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#define DF_CREAT(name, prot) mopen(name, O_WRONLY|O_TRUNC|O_CREAT)
#define DF_MT   DFMT_MAC
#endif /*MAC*/

#ifdef VMS
/*#undef DF_BUFFIO should be buff !!!!*/
   /* use only unbuff i/o - buff doesn't work! */
#ifndef DFopen                  /* avoid double includes */
/* #include "dfivms.h" */
#endif /*DFopen*/
#undef DF_STRUCTOK
#define DF_CAPFNAMES            /* fortran names are in all caps */
#include <file.h>               /* for unbuffered i/o stuff */
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_VAX
#endif /*VMS*/

#ifdef APOLLO
#if ! defined mc68010 && ! defined mc68020 && ! defined mc68030
#undef DF_STRUCTOK
#endif
#include <sys/file.h>               /* for unbuffered i/o stuff */
#define int8 char
#define uint8 unsigned char
#define int16 short int
#define uint16 unsigned short int
#define int32 long int
#define uint32 unsigned long int
#define float32 float
#define DFmovmem(from, to, len) memcpy(to, from, len)
#ifndef DF_STRUCTOK
#define UINT16READ(p, x) { x = ((*p++) & 255)<<8; x |= (*p++) & 255; }
#define INT16READ(p, x) { x = (*p++)<<8; x |= (*p++) & 255; }
#define INT32READ(p, x) { x = (*p++)<<24; x|=((*p++) & 255)<<16;    \
            x|=((*p++) & 255)<<8; x|=(*p++) & 255; }
#define UINT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT16WRITE(p, x) { *p++ = (x>>8) & 255; *p++ = x & 255; }
#define INT32WRITE(p, x) { *p++ = (x>>24) & 255; *p++ = (x>>16) & 255;  \
            *p++ = (x>>8) & 255; *p++ = x & 255; }
#endif /*DF_STRUCTOK*/
#define DF_CREAT(name, prot) creat(name, prot)
#define DF_MT   DFMT_APOLLO
#endif /*APOLLO*/


/*--------------------------------------------------------------------------*/
/*                      Flexibility parameters                              */
#ifdef MAC			/* MAC specific file manager calls */
#	define DF_OPEN(x,y) mopen(x,y)
#	define DF_CLOSE(x) mclose(x)
#	define DF_SEEK(x,y,z) mlseek(x,y,z)
#	define DF_SKEND(x,y,z) mlseek(x,-1*y,z)
#	define DF_TELL(x) mlseek(x,0L,1)
#	define DF_READ(a,b,c,d) mread(d,a,b*c)
#	define DF_WRITE(a,b,c,d) mwrite(d,a,b*c)
#	define DF_FLUSH(a)			/* no need to flush */
#	define DF_RDACCESS 0		/* dummy */
#	define DF_WRACCESS 0		/* dummy */
#	define DF_OPENERR(f)	((f) == -1)
#else /* !MAC */
#ifdef DF_BUFFIO            /* set all calls to do buffered I/O */
#define DF_OPEN(x,y) fopen(x,y)
#define DF_CLOSE(x) fclose(x)
#define DF_SEEK(x,y,z) fseek(x,y,z)
#define DF_SKEND(x,y,z) fseek(x,y,z)
#define DF_TELL(x) ftell(x)
#define DF_READ(a,b,c,d) fread(a,b,c,d)
#define DF_WRITE(a,b,c,d) fwrite(a,b,c,d)
#define DF_FLUSH(a) fflush(a)
#define DF_OPENERR(f)	(!(f))
#ifdef PC
#define DF_RDACCESS "rb"
#define DF_WRACCESS "rb+"
#else /*PC*/
#define DF_RDACCESS "r"
#define DF_WRACCESS "r+"
#endif /*PC*/

#else /*DF_BUFFIO         unbuffered i/o */
#ifdef PC
#ifdef WIN3
#define DF_OPEN(x,y) _lopen((LPSTR)(x),(int)(y))
#define DF_CLOSE(x) _lclose((int)(x))
#define DF_SEEK(x,y,z) _llseek((int)(x),(LONG)(y),(int)(z))
#define DF_SKEND(x,y,z) _llseek((int)(x),(LONG)(-1*(y)),(int)(z))
#define DF_TELL(x) _llseek((int)(x),(LONG)0L,(int)1)
#define DF_READ(a,b,c,d) _lread((int)(d),(LPSTR)(a),(WORD)((WORD)(b)*(WORD)(c)))
#define DF_WRITE(a,b,c,d) _lwrite((int)(d),(LPSTR)(a),(WORD)((WORD)(b)*(WORD)(c)))
#define DF_OPENERR(f)   ((f) == -1)
#define DF_FLUSH(a)                             /* no need to flush */
#define DF_RDACCESS OF_READ
#define DF_WRACCESS OF_READWRITE
#else
#define DF_OPEN(x,y) open(x,y,S_IWRITE|S_IREAD)
#define DF_CLOSE(x) close(x)
#define DF_SEEK(x,y,z) lseek(x,y,z)
#define DF_SKEND(x,y,z) lseek(x,-1*y,z)
#define DF_TELL(x) lseek(x,0L,1)
#define DF_READ(a,b,c,d) read(d,a,b*c)
#define DF_WRITE(a,b,c,d) write(d,a,b*c)
#define DF_OPENERR(f)   ((f) == -1)
#define DF_FLUSH(a)                             /* no need to flush */
#define DF_RDACCESS O_RDONLY | O_BINARY
#define DF_WRACCESS O_RDWR | O_BINARY
#endif
#else
#define DF_OPEN(x,y) open(x,y)
#define DF_CLOSE(x) close(x)
#define DF_SEEK(x,y,z) lseek(x,y,z)
#define DF_SKEND(x,y,z) lseek(x,-1*y,z)
#define DF_TELL(x) lseek(x,0L,1)
#define DF_READ(a,b,c,d) read(d,a,b*c)
#define DF_WRITE(a,b,c,d) write(d,a,b*c)
#define DF_OPENERR(f)	((f) == -1)
#define DF_FLUSH(a)                             /* no need to flush */
#define DF_RDACCESS O_RDONLY
#define DF_WRACCESS O_RDWR
#endif /* PC */
#endif /* DF_BUFFIO */
#endif /* !MAC */


    /* if not allocating memory dynamically, need buffer for compression */
#ifndef DF_DYNAMIC
#define DF_TBUF
#define DF_TBUFSZ	10000	/* buffer size */
#endif /*DF_DYNAMIC*/

    /* if reading/writing structures not ok, need buffer for conversion */
#ifdef PERM_OUT
#ifndef DF_TBUF
#ifndef DF_STRUCTOK
#define DF_TBUF
#define DF_TBUFSZ	512	/* buffer size can be smaller */
#endif /*DF_STRUCTOK*/
#endif /*DF_TBUF*/

/* 
MACRO FCALLKEYW for any special fortran-C stub keyword

MacIntosh MPW LS-fortran needs pascal since it can interface best with
pascal functions
*/
#if defined(MAC)		/* with LS FORTRAN */
#   define FCALLKEYW	pascal
#else /* !MAC */
#   define FCALLKEYW	/*NONE*/
#endif

#ifndef PC
#ifndef MAC
#ifndef IRIS4
#ifndef IBM6000
#ifndef CONVEX
#ifndef UNICOS
char *strncpy();
char *strcpy();
char *memcpy();
char *malloc();
#endif /* !UNICOS */
#endif /* !CONVEX */
#endif /* !IBM6000 */
#endif /* !IRIS4 */
#endif /* !MAC */
#endif /* !PC */
#endif /* PERM_OUT */


/*--------------------------------------------------------------------------*/
/*                          Size parameters                                 */
#ifdef PERM_OUT
#define DF_MAXDFS           32  /* How many DF's can be open at once */
#define DF_DEFAULTDDS       16  /* How many DD's a file has by default */
#define DF_MAXFNLEN         256 /* maximum length of filename parameters */
#endif /* PERM_OUT */

#ifndef FILE
#include <stdio.h>
#endif /*FILE*/

#endif /* DFI_H */
