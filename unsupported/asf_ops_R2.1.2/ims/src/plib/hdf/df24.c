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
 * File:    df24.c
 * Purpose: read and write 24-bit raster images
 * Invokes: dfgr.c
 * Contents:
 *  DF24getdims: get dimensions of image
 *  DF24reqil: use this interlace when returning image
 *  DF24getimage: read in image
 *  DF24setdims: set dimensions of image
 *  DF24setil: set interlace of image to write next
 *  DF24setcompress: set the compression to use when writing out next image
 *  DF24restart: restart looking for 24-bit images in a file
 *  DF24addimage: append image to file
 *  DF24putimage: write image to a file
 *  DF24readref: set ref of 24-bit RIG to get next
 *  DF24lastref: return reference number of last RIG read or written
 *  DF24nimages: get number of images in file
 * Missing:
 *  DF24writeref: set ref of 24-bit RIG to write next
 *
 * Remarks: A RIG specifies attributes associated with an image- lookup table,
 *          dimension, compression, color compensation etc.
 *---------------------------------------------------------------------------*/


#include "dfgr.h"
#include "herr.h"

static int Newdata = 0;                /* does Readrig contain fresh data? */
static int dimsset = 0;                /* have dimensions been set? */
static int32 last_xdim = 0;
static int32 last_ydim = 0;            /* .....gheesh.........*/

#define LUT     0
#define IMAGE   1


/*-----------------------------------------------------------------------------
 * Name:    DF24getdims
 * Purpose: get dimensions of next image RIG
 * Inputs:  filename: name of HDF file
 *          pxdim, pydim: pointer to locations for returning x,y dimensions
 *          pil: location for returning interlace of image in file
 * Returns: 0 on success, -1 on failure with DFerror set
 *          *pxdim, *pydim, *pil set on success
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24getdims(char *filename, int32 *pxdim, int32 *pydim, int *pil)
#else
int DF24getdims(filename, pxdim, pydim, pil)
    char *filename;
    int32 *pxdim, *pydim;
    int *pil;
#endif
{
    int ncomps;

    do {
        if (DFGRIgetdims(filename, pxdim, pydim, &ncomps, pil, IMAGE)<0)
            return FAIL;
    } while (ncomps!=3);

    last_xdim = *pxdim;
    last_ydim = *pydim;
    Newdata = 1;
    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DF24reqil
 * Purpose: get next image with specified interlace
 * Inputs:  il: interlace to get next image with
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIreqil
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24reqil(int il)
#else
int DF24reqil(il)
    int il;
#endif
{
    return(DFGRIreqil(il, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    DF24getimage
 * Purpose: get image from next RIG
 * Inputs:  filename: name of HDF file
 *          image: pointer to space to return image
 *          xdim, ydim: dimensions of space to return lut
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetimlut
 * Remarks: space is assumed to be xdim * ydim * 3 bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24getimage(char *filename, VOIDP image, int32 xdim, int32 ydim)
#else
int DF24getimage(filename, image, xdim, ydim)
    char *filename;
    VOIDP image;
    int32 xdim, ydim;
#endif
{
    char *FUNC="DF24getimage";
    int ret, il;
    int32 tx, ty;

    HEclear();

    if (!filename || !*filename || !image || (xdim<=0) || (ydim<=0)) {
       HERROR(DFE_ARGS);
        return FAIL;
    }

    if (!Newdata && DF24getdims(filename, &tx, &ty, &il) == FAIL)
       return FAIL;

    if (Newdata) {
      tx = last_xdim;
      ty = last_ydim;
    }

    if ((tx > xdim) || (ty > ydim)) {
       HERROR(DFE_ARGS);
        return(FAIL);
    }

    ret = DFGRIgetimlut(filename, image, xdim, ydim, IMAGE, 0);

    Newdata = 0;
    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    DF24setdims
 * Purpose: set dimensions of image to write next
 * Inputs:  xdim, ydim: dimensions of image
 *          il: interlace of image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24setdims(int32 xdim, int32 ydim)
#else
int DF24setdims(xdim, ydim)
    int32 xdim, ydim;
#endif
{
    dimsset = 1;
    return(DFGRIsetdims(xdim, ydim, 3, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    DF24setil
 * Purpose: set interlace of image to write next
 * Inputs:  il: interlace of image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetil
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24setil(int il)
#else
int DF24setil(il)
    int il;
#endif
{
    return(DFGRIsetil(il, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    DF24setcompress
 * Purpose: set compression scheme for 24-bit image
 * Inputs:
 *      type - the type of compression to perform on the next image
 *      cinfo - compression information structure
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRsetcompress
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24setcompress(int32 type,comp_info *cinfo)
#else
int DF24setcompress(type,cinfo)
    int32 type;
    comp_info *cinfo;
#endif
{
    return(DFGRsetcompress(type, cinfo));
}

/*-----------------------------------------------------------------------------
 * Name:    DF24restart
 * Purpose: restart file
 * Inputs:
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIrestart
 * Remarks: none
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int DF24restart(void)
#else
int DF24restart()
#endif
{
    return DFGRIrestart();
}

/*-----------------------------------------------------------------------------
 * Name:    DF24addimage
 * Purpose: Write out image
 * Inputs:  filename: name of HDF file
 *          image: image to write
 *          xdim, ydim: dimensions of array image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIaddimlut
 * Remarks: array image is assumed to be xdim * ydim * 3 bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24addimage(char *filename, VOIDP image, int32 xdim, int32 ydim)
#else
int DF24addimage(filename, image, xdim, ydim)
    char *filename;
    VOIDP image;
    int32 xdim, ydim;
#endif
{
    /* 0 == C */
    if (!dimsset && DFGRIsetdims(xdim, ydim, 3, IMAGE) == FAIL)
       return FAIL;
    dimsset = 0; /* reset to new rig */

    return(DFGRIaddimlut(filename, image, xdim, ydim, IMAGE, 0, 0));
}

#ifdef PROTOTYPE
int DF24putimage(char *filename, VOIDP image, int32 xdim, int32 ydim)
#else
int DF24putimage(filename, image, xdim, ydim)
    char *filename;
    VOIDP image;
    int32 xdim, ydim;
#endif
{
    /* 0 == C */
    if (!dimsset && DFGRIsetdims(xdim, ydim, 3, IMAGE) == FAIL)
       return FAIL;
    dimsset = 0; /* reset to new rig */

    return(DFGRIaddimlut(filename, image, xdim, ydim, IMAGE, 0, 1));
}


/*-----------------------------------------------------------------------------
 * Name:    DF24nimages
 * Purpose: How many 24-bit raster images are present in this file?
 * Inputs:  filename: name of HDF file
 * Returns: number of images  on success, -1 on failure with DFerror set
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFGRIopen, Hclose, Hnumber, Hfind, Hoffset
 * Remarks: the number is the number of unique 24-bit images in the file.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24nimages(char *filename)
#else
int DF24nimages(filename)
    char *filename;
#endif
{
    char *FUNC="DF24nimages";
    int32 file_id;
    int32 group_id;         /* group ID for looking at RIG's */
    uint16 elt_tag,elt_ref; /* tag/ref of items in a RIG */
    intn nimages;           /* total number of potential images */
    uint16 find_tag,find_ref;   /* storage for tag/ref pairs found */
    int32 find_off,find_len;    /* storage for offset/lengths of tag/refs found */
    uint8 GRtbuf[64];       /* local buffer to read the ID element into */

    HEclear();

    /* should use reopen if same file as last time - more efficient */
    file_id = DFGRIopen(filename, DFACC_READ);
    if (file_id == FAIL)
       return FAIL;

    /* go through the RIGs looking for 24-bit images */
    nimages=0;
    find_tag=find_ref=0;
    while(Hfind(file_id,DFTAG_RIG,DFREF_WILDCARD,&find_tag,&find_ref,&find_off,&find_len,DF_FORWARD)==SUCCEED) {
        /* read RIG into memory */
        if ((group_id=DFdiread(file_id, DFTAG_RIG,find_ref)) == FAIL) {
            HERROR(DFE_INTERNAL);
            return(FAIL);
          } /* end if */
        while(!DFdiget(group_id, &elt_tag, &elt_ref)) {  /* get next tag/ref */
            if(elt_tag==DFTAG_ID) {     /* just look for ID tags to get the number of components */
                if (Hgetelement(file_id, elt_tag, elt_ref, GRtbuf) != FAIL) {
                    int32 temp;             /* temporary holding variable */
                    int32 ncomponents;      /* number of image components */
                    uint8 *p;

                    p = GRtbuf;
                    INT32DECODE(p, temp);
                    INT32DECODE(p, temp);
                    UINT16DECODE(p, temp);
                    UINT16DECODE(p, temp);
                    INT16DECODE(p, ncomponents);
                    if(ncomponents==3)     /* whew, all that work and we finally found a 24-bit image */
                        nimages++;
                  } /* end if */
                else
                    return(FAIL);
              } /* end if */
          } /* end while */
      } /* end while */

    if (Hclose(file_id) == FAIL)
       return FAIL;
    return(nimages);
}   /* end DF24nimages() */

/*-----------------------------------------------------------------------------
 * Name:    DF24readref
 * Purpose: Set ref of 24-rig to get next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next get
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFGRreadref
 * Remarks: checks if 24-rig with this ref exists
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DF24readref(char *filename, uint16 ref)
#else
int DF24readref(filename, ref)
    char *filename;
    uint16 ref;
#endif
{
    return (DFGRreadref(filename, ref));
}

/*-----------------------------------------------------------------------------
 * Name:    DF24lastref
 * Purpose: Return reference number of last read or written RIG
 * Inputs:
 * Returns: Last reference number
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIlastref
 * Remarks: none
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
uint16 DF24lastref(void)
#else
uint16 DF24lastref()
#endif
{
    return DFGRIlastref();
}
