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

/*
**  hbitio.h
**
**  Data structures and macros for bitfile access to HDF data objects.
**  These are mainly used for compression I/O and N-bit data objects.
*/

#ifndef __HBITIO_H
#define __HBITIO_H

/* maximum number of bitfile access elements */
/* (can be less than the MAX_ACC defined in hfile.h, but never greater) */
#ifndef MAX_BITFILE
#   define MAX_BITFILE 16
#endif

#define SLOT2BITID(s) ((((uint32)BITTYPE & 0xffff) << 16) | ((s) & 0xffff))
#define VALIDBITID(i) (((((uint32)(i) >> 16) & 0xffff) == BITTYPE) && \
                    (((uint32)(i) & 0xffff) < MAX_BITFILE))
#define BITID2SLOT(i) (VALIDBITID(i) ? (uint32)(i) & 0xffff : -1)
#define BITID2REC(i) ((VALIDBITID(i) ? &(bitfile_records[(uint32)(i)&0xffff]) \
                    : NULL))

/* Define the number of elements in the buffered array */
#define BITBUF_SIZE 4096
/* Macro to define the number of bits cached in the 'bits' variable */
#define BITNUM      sizeof(uint8)

typedef struct bitrec_t {
#ifdef OLD_WAY
    FILE *file;     /* stdio file pointer */
#else
    int32 acc_id;   /* Access ID for H layer I/O routines */
#endif
    bool used;      /* whether this record is in use */
    uintn count;    /* bit count to next boundary */
    uint8 mode;     /* What the operation on this file is ('r', 'w', etc..) */
    uint8 bits;     /* extra bit buffer, 0..BITNUM-1 bits */
    uint8 *bytep;   /* current position in buffer */
    uint8 *bytez;   /* end of buffer to compare */
    uint8 *bytea;   /* byte buffer */
} bitrec_t;

#ifndef BITMASTER
extern
#endif
const uint8 maskc[9]
#ifdef BITMASTER
={0,1,3,7,15,31,63,127,255}
#endif
;

#ifndef BITMASTER
extern
#endif
const uint32 maskl[33]
#ifdef BITMASTER
={ 0x00000000,
    0x00000001,0x00000003,0x00000007,0x0000000f,
    0x0000001f,0x0000003f,0x0000007f,0x000000ff,
    0x000001ff,0x000003ff,0x000007ff,0x00000fff,
    0x00001fff,0x00003fff,0x00007fff,0x0000ffff,
    0x0001ffff,0x0003ffff,0x0007ffff,0x000fffff,
    0x001fffff,0x003fffff,0x007fffff,0x00ffffff,
    0x01ffffff,0x03ffffff,0x07ffffff,0x0fffffff,
    0x1fffffff,0x3fffffff,0x7fffffff,0xffffffff}
#endif
;

#endif  /* __HBITIO_H */

