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
 * File:    dfrig.h
 * Purpose: header file for the Raster Image set
 * Invokes: df.h
 * Contents:
 *  Structure definitions: DFRdr, DFRrig
 * Remarks: This is included with user programs which use RIG
 *---------------------------------------------------------------------------*/


#ifndef DFRIG                  /* avoid re-inclusion */
#define DFRIG

/* description record: used to describe image data, palette data etc. */
typedef struct {
    int32 xdim, ydim;          /* dimensions of data */
    DFdi nt;                   /* number type of data */
    int16 ncomponents, interlace; /* data ordering: chunky / planar etc */
    DFdi compr;                        /* compression */
    /* ### Note: compression is currently uniquely described with a tag.
       No data is attached to this tag/ref.  But this capability is
       provided for future expansion, when this tag/ref might point to
       some data needed for decompression, such as the actual encodings */
} DFRdr;

/* structure to hold RIG info */
typedef struct {
    DFdi image;                        /* image */
    DFRdr descimage;           /* image data description */
    DFdi lut;                  /* color look-up table (palette) */
    DFRdr desclut;             /* look-up table description */
    DFdi mattechannel;
    DFRdr descmattechannel;
    int32 xpos, ypos;          /* X-Y position of image on screen */
    float32 aspectratio;         /* ratio of pixel height to width */
    float32 ccngamma, ccnred[3], ccngrren[3], ccnblue[3], ccnwhite[3];
                               /* color correction parameters */
    char *cf;                  /* color format */
} DFRrig;

#endif /*DFRIG*/
