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
 * File:    dfutilF.c
 * Purpose: C stubs for Fortran utility routines
 * Invokes: dfutil.c
 * Contents: 
 *  dfindnr_:       For a given tag, find the next ref after the given ref
 *---------------------------------------------------------------------------*/

#include "hdf.h"

#ifndef DFUTIL_FNAMES
#   define DFUTIL_FNAMES
#ifdef DF_CAPFNAMES
#   define ndfindnr          FNAME(DFINDNR)
#   define ndffindnextref    FNAME(DFFINDNEXTREF)
#else
#   define ndfindnr          FNAME(dfindnr)
#   define ndffindnextref    FNAME(dffindnextref)
#endif /* DF_CAPFNAMES */
#endif /* DFUTIL_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    dfindnr
 * Purpose: For this tag, find the ref after lref
 * Inputs:  dfile: ptr to open DF file
 *          tag:   tag to look for
 *          lref:  ref after which to search
 *
 * Returns: the desired ref if successful, on failure with	DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFfindnextref
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfindnr(intf *dfile, intf *tag, intf *lref)
#else
ndfindnr(dfile, tag, lref)
     intf *dfile;
     intf *tag, *lref;
#endif /* PROTOTYPE */
{
    return(DFfindnextref(*dfile, (uint16)*tag, (uint16)*lref));
}

/*
CEND7MAX
*/

/*-----------------------------------------------------------------------------
 * Name:    dffindnextref
 * Purpose: For this tag, find the ref after lref
 * Inputs:  dfile: ptr to open DF file
 *          tag:   tag to look for
 *          lref:  ref after which to search
 *
 * Returns: the desired ref if successful, on failure with	DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFfindnextref
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndffindnextref(intf *dfile, intf *tag, intf *lref)
#else
ndffindnextref(dfile, tag, lref)
     intf *dfile;
     intf *tag, *lref;
#endif /* PROTOTYPE */
{
    return(DFfindnextref(*dfile, (uint16)*tag, (uint16)*lref));
}
