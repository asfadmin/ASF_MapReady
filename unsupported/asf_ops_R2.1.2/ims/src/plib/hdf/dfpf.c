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
 * File:    dfpF.c
 * Purpose: C stubs for Palette Fortran routines
 * Invokes: dfp.c dfkit.c
 * Contents: 
 *  dpigpal_:   Call DFPgetpal to get palette
 *  dpippal_:   Call DFPputpal to write/overwrite palette in file
 *  dpinpal_:   Call DFPnpals to get number of palettes in file
 *  dpiwref_:    Call DFPwriteref to set ref of pal to write next
 *  dpirref_:    Call DFPreadref to set ref of pal to read next
 *  dprest_:    Call DFPrestart to get palettes afresh in file
 *  dplref_:    Call DFPlastref to get ref of last pal read/written
 *  DFPrestart_:    Call DFPrestart to get palettes afresh in file
 *  DFPlastref_:    Call DFPlastref to get ref of last pal read/written
 * Remarks: none
 *---------------------------------------------------------------------------*/

#include "hdf.h"

#ifndef DFP_FNAMES
#   define DFP_FNAMES
#ifdef DF_CAPFNAMES
#   define ndpigpal  FNAME(DPIGPAL)
#   define ndpippal  FNAME(DPIPPAL)
#   define ndpinpal  FNAME(DPINPAL)
#   define ndpiwref  FNAME(DPIWREF)
#   define ndpirref  FNAME(DPIRREF)
#   define ndprest   FNAME(DPREST)
#   define ndplref   FNAME(DPLREF)
#   define ndfprestart   FNAME(DFPRESTART)
#   define ndfplastref   FNAME(DFPLASTREF)
#else   /* !DF_CAPNAMES */
#   define ndpigpal  FNAME(dpigpal)
#   define ndpippal  FNAME(dpippal)
#   define ndpinpal  FNAME(dpinpal)
#   define ndpiwref  FNAME(dpiwref)
#   define ndpirref  FNAME(dpirref)
#   define ndprest   FNAME(dprest)
#   define ndplref   FNAME(dplref)
#   define ndfprestart   FNAME(dfprestart)
#   define ndfplastref   FNAME(dfplastref)
#endif /* DF_CAPFNAMES */
#endif /* DFP_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    dpigpal
 * Purpose: call DFPgetpal, get palette
 * Inputs:  filename, fnlen: filename, length of name
 *          pal: space to put palette
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   Fortran stub routine
 * Invokes: DFPgetpal
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndpigpal(_fcd filename, _fcd pal, intf *fnlen)
#else
ndpigpal(filename, pal, fnlen)
    _fcd filename;
    _fcd pal;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DFPgetpal(fn, (VOIDP)_fcdtocp(pal));
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dpippal
 * Purpose: Write palette to file
 * Inputs:  filename: name of HDF file
 *          palette: palette to be written to file
 *          overwrite: if 1, overwrite last palette read or written
 *                     if 0, write it as a fresh palette
 *          filemode: if "a", append palette to file
 *                    if "w", create new file
 *          fnlen:  length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF users, programmers, utilities
 * Invokes: DFPputpal
 * Remarks: To overwrite, the filename must be the same as for the previous
 *          call
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndpippal(_fcd filename, _fcd pal, intf *overwrite, _fcd filemode, intf *fnlen)
#else
ndpippal(filename, pal, overwrite, filemode, fnlen)
    _fcd filename;
    _fcd pal;
    intf *overwrite;
    _fcd filemode;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DFPputpal(fn, (VOIDP)_fcdtocp(pal), (intn)*overwrite,
            (char*)_fcdtocp(filemode));
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dpinpal
 * Purpose: How many palettes are present in this file?
 * Inputs:  filename, fnlen: name, length of HDF file
 * Returns: number of palettes on success, -1 on failure with DFerror set
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFPnpals
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndpinpal(_fcd filename, intf *fnlen)
#else
ndpinpal(filename, fnlen)
    _fcd filename;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DFPnpals(fn);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dpirref
 * Purpose: Set ref of palette to get next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next get
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFPreadref
 * Remarks: checks if palette with this ref exists
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndpirref(_fcd filename, uint16 *ref, intf *fnlen)
#else
ndpirref(filename, ref, fnlen)
    _fcd filename;
    uint16 *ref;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DFPreadref(fn, *ref);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dpiwref
 * Purpose: Set ref of palette to put next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next put
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFPwriteref
 *---------------------------------------------------------------------------*/


    FRETVAL(intf)
#ifdef PROTOTYPE
ndpiwref(_fcd filename, uint16 *ref, intf *fnlen)
#else
ndpiwref(filename, ref, fnlen)
    _fcd filename;
    uint16 *ref;
    intf *fnlen;
#endif /* PROTOTYPE */
{

    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret =  DFPreadref(fn, *ref);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dprest
 * Purpose: Do not remember info about file - get again from first palette
 * Inputs:  none
 * Returns: 0 on success
 * Users:   HDF programmers
 * Remarks: Invokes DFPrestart
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndprest(void)
#else
ndprest()
#endif /* PROTOTYPE */
{

    return(DFPrestart());
}


/*-----------------------------------------------------------------------------
 * Name:    dplref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Lastref
 * Returns: ref on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFPlastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndplref(void)
#else
ndplref()
#endif /* PROTOTYPE */
{

    return(DFPlastref());
}


/*-----------------------------------------------------------------------------
 * Name:    dfprestart
 * Purpose: Do not remember info about file - get again from first palette
 * Inputs:  none
 * Returns: 0 on success
 * Users:   HDF programmers
 * Remarks: Invokes DFPrestart
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfprestart(void)
#else
ndfprestart()
#endif /* PROTOTYPE */
{

    return(DFPrestart());
}


/*-----------------------------------------------------------------------------
 * Name:    dfplastref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Lastref
 * Returns: ref on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFPlastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfplastref(void)
#else
ndfplastref()
#endif /* PROTOTYPE */
{

    return(DFPlastref());
}
