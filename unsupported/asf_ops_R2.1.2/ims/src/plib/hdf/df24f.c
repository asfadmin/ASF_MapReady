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
 * File:    df24F.c
 * Purpose: read and write 24-bit raster images
 * Invokes: dfgr.c df24.c
 * Contents: 
 *  d2reqil: use this interlace when returning image
 *  df24reqil: use this interlace when returning image
 *  d2sdims: set dimensions of image
 *  df24setdims: set dimensions of image
 *  d2setil: set interlace for image
 *  df24setil: set interlace for image
 *  d2first: restart 24 bit raster
 *  df24restart: restart 24 bit raster
 *  d2igdim: get dimensions of image
 *  d2igimg: read in image
 *  d2iaimg: write out image
 *  d24lref: last ref number
 *  d2scomp: set compression to use (short name)
 *  df24setcompress: set compression to use (long name)
 *  d2sjpeg:  set JPEG parameters (short name)
 *  df24setJPEG: set JPEG parameters (long name)
 *
 * Remarks:A RIG specifies attributes associated with an image - lookup table, 
 *          dimension, compression, color compensation etc.
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "dfgr.h"

#define LUT     0
#define IMAGE   1

static int dimsset = 0;


/*-----------------------------------------------------------------------------
 * Name:    d2reqil
 * Purpose: get next image with specified interlace
 * Inputs:  il: interlace to get next image with
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIreqil
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2reqil(intf *il)
#else
nd2reqil(il)
    intf *il;
#endif /* PROTOTYPE */
{
    return(DFGRIreqil((intn)*il, (intn)IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    d2sdims
 * Purpose: set dimensions of image to write next
 * Inputs:  xdim, ydim: dimensions of image
 *          il: interlace of image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2sdims(intf *xdim, intf *ydim)
#else
nd2sdims(xdim, ydim)
    intf *xdim, *ydim;
#endif /* PROTOTYPE */
{
    dimsset = 1;
    return(DFGRIsetdims(*xdim, *ydim, 3, IMAGE));
}


/*-----------------------------------------------------------------------------
 * Name:    d2igdim
 * Purpose: get dimensions of next image RIG
 * Inputs:  filename: name of HDF file
 *          pxdim, pydim: pointer to locations for returning x,y dimensions
 *          pil: location for returning interlace of image in file
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 *          *pxdim, *pydim, *pil set on success
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DF24getdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2igdim(_fcd filename, intf *pxdim, intf *pydim, intf *pil, intf *fnlen)
#else
nd2igdim(filename, pxdim, pydim, pil, fnlen)
    _fcd filename;
    intf *pxdim, *pydim;
    intf *pil, *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DF24getdims(fn, (int32 *)pxdim, (int32 *)pydim, (intn *)pil);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    d2igimg
 * Purpose: get image from next RIG
 * Inputs:  filename: name of HDF file
 *          image: pointer to space to return image
 *          xdim, ydim: dimensions of space to return image
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetimlut
 * Remarks: space is assumed to be xdim * ydim * 3 bytes
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2igimg(_fcd filename, _fcd image, intf *xdim, intf *ydim, intf *fnlen)
#else
nd2igimg(filename, image, xdim, ydim, fnlen)
    _fcd filename;
    _fcd image;
    intf *xdim, *ydim;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DF24getimage(fn, (VOIDP)_fcdtocp(image), *xdim, *ydim);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    d2iaimg
 * Purpose: Write out image
 * Inputs:  filename: name of HDF file
 *          image: image to write
 *          xdim, ydim: dimensions of array image
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIaddimlut
 * Remarks: array image is assumed to be xdim * ydim * ncomps bytes
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2iaimg(_fcd filename, _fcd image, intf *xdim, intf *ydim, intf *fnlen,
    intf *newfile)
#else
nd2iaimg(filename, image, xdim, ydim, fnlen, newfile)
    _fcd filename;
    _fcd image;
    intf *xdim, *ydim;
    intf *fnlen, *newfile;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    if (!dimsset)
        if (DFGRIsetdims(*xdim, *ydim, 3, IMAGE)<0) return(-1);

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFGRIaddimlut(fn, (VOIDP)_fcdtocp(image), *xdim, *ydim,
            IMAGE, 1, (intn)*newfile);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    d2setil
 * Purpose: set interlace store with following images
 * Inputs:  il: interlace to set
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetil
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2setil(intf *il)
#else
nd2setil(il)
    intf *il;
#endif /* PROTOTYPE */
{
    return (DFGRIsetil((intn)*il, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    d2first
 * Purpose: restart 24 bit raster file
 * Inputs:  
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIrestart
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2first(void)
#else
nd2first()
#endif /* PROTOTYPE */
{
    return (DFGRIrestart());
}

/*-----------------------------------------------------------------------------
 * Name:    d24lref
 * Purpose: return last reference number 
 * Inputs:  
 * Returns: last ref number
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIrestart
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd24lref(void)
#else
nd24lref()
#endif /* PROTOTYPE */
{
    return (DFGRIlastref());
}

/*-----------------------------------------------------------------------------
 * Name:    d2scomp
 * Purpose: set the compression to use when writing the next image
 * Inputs:
 *      scheme - the type of compression to use
 * Returns: 0 on success, -1 for error
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DF24setcompress
 * Remarks: if the compression scheme is JPEG, this routine sets up default
 *          JPEG parameters to use, if a user wants to change them, d2sjpeg
 *          must be called.
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2scomp(intf *scheme)
#else
nd2scomp(scheme)
intf *scheme;
#endif /* PROTOTYPE */
{
    comp_info *cinfo;   /* Structure containing compression parameters */

    if(*scheme==COMP_JPEG) {  /* check for JPEG compression and set defaults */
        cinfo->jpeg.quality=75;
        cinfo->jpeg.force_baseline=1;
      } /* end if */
    return (DF24setcompress((int32)*scheme,cinfo));
}   /* end d2scomp() */

/*-----------------------------------------------------------------------------
 * Name:    d2sjpeg
 * Purpose: change the JPEG compression parameters
 * Inputs:
 *      quality - what the JPEG quality rating should be
 *      force_baseline - whether to force a JPEG baseline file to be written
 * Returns: 0 on success, -1 for error
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DF24setcompress
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2sjpeg(intf *quality,intf *force_baseline)
#else
nd2sjpeg(quality,force_baseline)
intf *quality;
intf *force_baseline;
#endif /* PROTOTYPE */
{
    comp_info *cinfo;   /* Structure containing compression parameters */

    cinfo->jpeg.quality=(intn)*quality;
    cinfo->jpeg.force_baseline=(intn)*force_baseline;
    return (DF24setcompress((int32)COMP_JPEG,cinfo));
}   /* end d2sjpeg() */

/*-----------------------------------------------------------------------------
 * Name:    df24reqil
 * Purpose: get next image with specified interlace
 * Inputs:  il: interlace to get next image with
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIreqil
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndf24reqil(intf *il)
#else
ndf24reqil(il)
    intf *il;
#endif /* PROTOTYPE */
{
    return(DFGRIreqil((intn)*il, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    df24setdims
 * Purpose: set dimensions of image to write next
 * Inputs:  xdim, ydim: dimensions of image
 *          il: interlace of image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndf24setdims(intf *xdim, intf *ydim)
#else
ndf24setdims(xdim, ydim)
    intf *xdim, *ydim;
#endif /* PROTOTYPE */
{
    dimsset = 1;
    return(DFGRIsetdims(*xdim, *ydim, 3, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    df24setil
 * Purpose: set interlace store with following images
 * Inputs:  il: interlace to set
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetil
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndf24setil(intf *il)
#else
ndf24setil(il)
    intf *il;
#endif /* PROTOTYPE */
{
    return (DFGRIsetil((intn)*il, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    df24restart
 * Purpose: restart 24 bit raster file
 * Inputs:  
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIrestart
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndf24restart(void)
#else
ndf24restart()
#endif /* PROTOTYPE */
{
    return (DFGRIrestart());
}

/*-----------------------------------------------------------------------------
 * Name:    df24scompress
 * Purpose: set the compression to use when writing the next image
 * Inputs:
 *      scheme - the type of compression to use
 * Returns: 0 on success, -1 for error
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DF24setcompress
 * Remarks: if the compression scheme is JPEG, this routine sets up default
 *          JPEG parameters to use, if a user wants to change them, df24setjpeg
 *          must be called.
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndf24scompress(intf *scheme)
#else
ndf24scompress(scheme)
intf *scheme;
#endif /* PROTOTYPE */
{
    comp_info *cinfo;   /* Structure containing compression parameters */

    if(*scheme==COMP_JPEG) {  /* check for JPEG compression and set defaults */
        cinfo->jpeg.quality=75;
        cinfo->jpeg.force_baseline=1;
      } /* end if */
    return (DF24setcompress((int32)*scheme,cinfo));
}   /* end df24setcompress() */

/*-----------------------------------------------------------------------------
 * Name:    df24sjpeg
 * Purpose: change the JPEG compression parameters
 * Inputs:
 *      quality - what the JPEG quality rating should be
 *      force_baseline - whether to force a JPEG baseline file to be written
 * Returns: 0 on success, -1 for error
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DF24setcompress
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndf24sjpeg(intf *quality,intf *force_baseline)
#else
ndf24sjpeg(quality,force_baseline)
intf *quality;
intf *force_baseline;
#endif /* PROTOTYPE */
{
    comp_info *cinfo;   /* Structure containing compression parameters */

    cinfo->jpeg.quality=(intn)*quality;
    cinfo->jpeg.force_baseline=(intn)*force_baseline;
    return (DF24setcompress((int32)COMP_JPEG,cinfo));
}   /* end df24setjpeg() */

/*-----------------------------------------------------------------------------
 * Name:    d2irref
 * Purpose: Internal stub for setting ref of rig to read next
 * Inputs:  filename: name of HDF file
 *          ref: reference
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRreadref
 * Remarks: 
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nd2irref(_fcd filename, intf *ref, intf *fnlen)
#else
nd2irref(filename, ref, fnlen)
    _fcd filename;
    intf *ref;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFGRreadref(fn, (uint16)*ref);
    HDfreespace((VOIDP)fn);
    return(ret);
}
