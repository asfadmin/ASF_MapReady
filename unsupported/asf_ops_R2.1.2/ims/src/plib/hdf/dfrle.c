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
 * File:    dfrle.c
 * Purpose: RLE image compression algorithm
 * Invokes:
 * Contents:
 *  DFCIrle: compress string using run length encoding
 *  DFCIunrle: decompress string using run length encoding
 * Remarks: DFCIrle() and DFCIunrle() compress and decompress RLE encoded info
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "herr.h"

/*-----------------------------------------------------------------------------
 * Name:    DFCIrle
 * Purpose: compress a string of bytes
 * Inputs:  buf: buffer containing data to be compressed
 *          bufto: space for compressed data - assumed big enough
 *          len: number of bytes to compress
 * Returns: number of compressed bytes on success, -1 on failure
 * Users:   HDF programmers, DFputcomp, other routines
 * Invokes: none
 * Remarks: Written for efficiency
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFCIrle(VOIDP buf, VOIDP bufto, int32 len)
#else
int32 DFCIrle(buf,bufto,len)
    VOIDP buf;
    VOIDP bufto;
    int32 len;
#endif
{
    register uint8 * p;
    register uint8 * q;
    register uint8 * cfoll;
    register uint8 * clead;
    uint8 * begp;
    int32 i;

    p = (uint8 *)buf;
    cfoll = (uint8 *)bufto;             /* place to copy to */
    clead = cfoll + 1;

    begp = p;
    while (len > 0) {           /* encode stuff until gone */

        q = p + 1;
        i = len-1;
        while (i && i+120 > len && *p == *q) {
            q++;
            i--;
        }

        if (q - p > 2) {        /* three in a row */
            if (p > begp) {
                *cfoll = (uint8)(p - begp);
                cfoll = clead;
            }
            *cfoll++ = (uint8)(128 | (uint8)(q-p)); /* len of seq */
            *cfoll++ = *p;      /* char of seq */
            len -= q-p;         /* subtract len of seq */
            p = q;
            clead = cfoll+1;
            begp = p;
        }
        else {
            *clead++ = *p++;    /* copy one char */
            len--;
            if (p - begp > 120) {
                *cfoll = (uint8)(p - begp);
                cfoll = clead++;
                begp = p;
            }
        }

    }
/*
 *  fill in last bytecount
 */
    if (p > begp)
        *cfoll = (uint8)(p - begp);
    else
        clead--;                    /* don't need count position */

    return((int32)((uint8*)clead - (uint8*)bufto)); /* how many encoded */
}

/*-----------------------------------------------------------------------------
 * Name:    DFCIunrle
 * Purpose: decompress run length encoding
 * Inputs:  buf: buffer containing compressed data
 *          bufto: space for returning decompressed data
 *          outlen: number of *decompressed* bytes desired.
 *          resetsave: don't use any stored state info - used for fresh image
 * Returns: number of compressed bytes used up on success, -1 on failure
 * Users:   HDF programmers, DFgetcomp, other routines
 * Invokes: none
 * Remarks: has been modified so it will decompress even non-rowwise compression
 *          Hence the static storage stuff
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFCIunrle(uint8 *buf, uint8 *bufto, int32 outlen, int resetsave)
#else
int32 DFCIunrle(buf,bufto,outlen, resetsave)
    uint8 *buf;
    uint8 *bufto;
    int32 outlen;
    int resetsave;
#endif
{
    register int cnt;
    register uint8 * p;
    register uint8 * q;
    uint8 * endp;
    static uint8 save[255], *savestart=NULL, *saveend=NULL;
    /* save has a list of decompressed bytes not returned in
       previous call.  savestart and saveend specify the position
       at which this list starts and ends in the array save */

    p = (uint8 *)buf;
    endp = (uint8 *)bufto + outlen;
    q = (uint8 *)bufto;
    if (resetsave) savestart = saveend = save; /* forget saved state */
    while ((saveend>savestart) && (q<endp)) /* copy saved stuff */
        *q++ = *savestart++;
    if (savestart>=saveend) savestart = saveend = save;        /* all copied */
    while (q < endp) {
        cnt = *p++;            /* count field */
        if (!(cnt & 128)) {    /* is set of uniques */
            while (cnt--) {
                if (q<endp)
                    *q++ = *p++; /* copy unmodified */
                else
                    *saveend++ = *p++;
            }
        }
        else {
            cnt &= 127;                /* strip high bit */
            while (cnt--) {
                if (q<endp)
                    *q++ = *p;  /* copy unmodified */
                else
                    *saveend++ = *p;
            }
            p++;                /* skip that character */
        }
    }
    return((int32)(p - buf));
}

