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
 * File:    dfanF.c
 * Purpose: Fortran stubs for annotation routines
 * Invokes: dfan.c dfkit.c
 * Contents: 
 *
 *  daiganl_: get length of annotation of tag/ref
 *  daigann_: get annotation of tag/ref
 *  daipann_: put annotation of tag/ref
 *  dailist_: get list of refs and labels for a given tag
 *  dalref_ : return last ref written or read
 *  dfanlastref_: return last ref written or read
 *
 *  dfanaddfds_    : add file description
 *  dfangetfidlen_ : get length of file id  
 *  dfangetfdslen_ : get length of file description  
 *  dfangetfid_    : get file id
 *  dfangetfds_    : get file description
 *  daafds_        : get file description
 *  dagfidl_       : get file id length
 *  dagfdsl_       : get file description length
 *  dagfid_        : get file id
 *  dagfds_        : get file description
 *
 *  daiafid_       : add file id (intermediate routine)
 *---------------------------------------------------------------------------*/

#include "dfan.h"
#include "df.h"

#ifndef DFAN_FNAMES
#   define  DFAN_FNAMES
#ifdef DF_CAPFNAMES
#   define ndaiganl  FNAME(DAIGANL)
#   define ndaigann  FNAME(DAIGANN)
#   define ndaipann  FNAME(DAIPANN)
#   define ndailist  FNAME(DAILIST)
#   define ndalref   FNAME(DALREF)
#   define ndfanlastref  FNAME(DFANLASTREF)

#   define ndfanaddfds      FNAME(DFANADDFDS)
#   define ndfangetfidlen   FNAME(DFANGETFIDLEN)
#   define ndfangetfdslen   FNAME(DFANGETFDSLEN)
#   define ndfangetfid      FNAME(DFANGETFID)
#   define ndfangetfds      FNAME(DFANGETFDS)
#   define ndaafds          FNAME(DAAFDS)
#   define ndagfidl         FNAME(DAGFIDL)
#   define ndagfdsl         FNAME(DAGFDSL)
#   define ndagfid          FNAME(DAGFID)
#   define ndagfds          FNAME(DAGFDS)
#   define ndaiafid         FNAME(DAIAFID)
#else   /* !DF_CAPFNAMES */
#   define ndaiganl  FNAME(daiganl)
#   define ndaigann  FNAME(daigann)
#   define ndaipann  FNAME(daipann)
#   define ndailist  FNAME(dailist)
#   define ndalref   FNAME(dalref)
#   define ndfanlastref  FNAME(dfanlastref)

#   define ndfanaddfds      FNAME(dfanaddfds)
#   define ndfangetfidlen   FNAME(dfangetfidlen)
#   define ndfangetfdslen   FNAME(dfangetfdslen)
#   define ndfangetfid      FNAME(dfangetfid)
#   define ndfangetfds      FNAME(dfangetfds)
#   define ndaafds          FNAME(daafds)
#   define ndagfidl         FNAME(dagfidl)
#   define ndagfdsl         FNAME(dagfdsl)
#   define ndagfid          FNAME(dagfid)
#   define ndagfds          FNAME(dagfds)
#   define ndaiafid         FNAME(daiafid)
#endif /* DF_CAPFNAMES */
#endif /* DFAN_FNAMES */


/* conventions used in forming names of routines:
**
**    dfan: hdf annotation routine (<dfan>addfds)
**    add:  add item to file       dfan<add>fds
**    get:  get item from file     dfan<get>fds
**    f:    file                   dfanadd<f>ds
**    id:   id                     dfanaddf<id>
**    ds:   description            dfanaddf<ds>
**    len:  length                 dfanaddfid<len>
**    l:    length (short forms)   dagfid<l>
**    da:   dfan (short forms)     <da>gfid
**    a:    add (short forms)      da<a>fds
**    g:    get (short forms)      da<g>fds
**    i:    intermediate routine (not in user interface) da<i>afid
**/



/*---------------------------------------------------------------------------
** Routines for handling tag/ref (not file) annotations
 *-------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    daiganl
 * Purpose: get length of annotation of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want label
 *          type: DFAN_LABEL if label, DFAN_DESC if description
 *          fnlen: length of filename
 * Returns: length of annotation on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetannlen, HDf2cstring, DFIgetspace, DFIfreespace
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndaiganl(_fcd filename, intf *tag, intf *ref, intf *type, intf *fnlen)
#else
ndaiganl(filename, tag, ref, type, fnlen)
    _fcd filename;
    intf *tag, *ref;
    intf *fnlen, *type;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFANIgetannlen(fn, (uint16) *tag, (uint16) *ref, (intn)*type);
    HDfreespace((VOIDP)fn);

    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    daigann
 * Purpose: get annotation of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want label
 *          annotation: space to return label in
 *          maxlen: size of space to return label in
 *          type: DFAN_LABEL if label, DFAN_DESC if description
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndaigann(_fcd filename, intf *tag, intf *ref, _fcd annotation, intf *maxlen,
     intf *type, intf *fnlen)
#else
ndaigann(filename, tag, ref, annotation, maxlen, type, fnlen)
    _fcd filename, annotation;
    intf *tag, *ref;
    intf *maxlen;
    intf *type, *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFANIgetann(fn, (uint16) *tag, (uint16) *ref, 
                (uint8*)_fcdtocp(annotation), (int32)*maxlen, (intn)*type);
    HDfreespace((VOIDP)fn);

    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    daipann
 * Purpose: put annotation of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want label
 *          annotation: space to return label in
 *          annlen: length of annotation
 *          type: DFAN_LABEL if label, DFAN_DESC if description
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndaipann(_fcd filename, intf *tag, intf *ref, _fcd annotation,
         intf *annlen, intf *type, intf *fnlen)
#else
ndaipann(filename, tag, ref, annotation, annlen, type, fnlen)
    _fcd filename, annotation;
    intf *tag, *ref;
    intf *annlen;
    intf *type, *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFANIputann(fn, (uint16) *tag, (uint16) *ref, 
                (uint8*)_fcdtocp(annotation), (int32)*annlen, (intn)*type);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dailist
 * Purpose: Return list of refs and labels for a given tag
 * Inputs:  filename: name of HDF file
 *          tag: tag to get list of refs and labels for
 *          reflist: array to place refs in
 *          labellist: array of strings to place labels in
 *          listsize: size of ref and label lists
 *          maxlen: maximum length allowed for label
 *          startpos: beginning from the startpos'th entry, upto listsize
 *              entries will be returned.
 *          fnlen: length of filename
 * Returns: number of entries on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFANIlablist
 * Method:  call DFANIlablist
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndailist(_fcd filename, intf *tag, intf reflist[], _fcd labellist,
    intf *listsize, intf *maxlen, intf *startpos, intf *fnlen)
#else
ndailist(filename, tag, reflist, labellist,listsize, maxlen,startpos,fnlen)
     _fcd filename;
     intf *tag;
     intf reflist[];
     _fcd labellist;
     intf *listsize;
     intf *maxlen, *startpos, *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    int i;
    intf nrefs;
    uint16 *tempreflist;

    fn = HDf2cstring(filename, (intn)*fnlen);

    /* create reflist with true uint16s to maintain compatibility
    ** with machines that allocate more than 16 bits per uint16.
    */
    tempreflist = (uint16 *) DFIgetspace( (int32)(*listsize) * sizeof(uint16) );
                                                /* 1 for isfortran */
    nrefs = DFANIlablist(fn, (uint16) *tag, tempreflist,
                 (uint8*)_fcdtocp(labellist),
                (int)*listsize, (int)*maxlen, (int)*startpos, 1);
    if (nrefs< 0) return FAIL;

    /* move ref numbers into caller's reflist */
    for (i=0; i < *listsize; i++)
        reflist[i] = tempreflist[i];

    HDfreespace((VOIDP)fn);
    HDfreespace((VOIDP)tempreflist);

    return(nrefs);
}


/*-----------------------------------------------------------------------------
 * Name:    dalref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Lastref
 * Returns: ref on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFANlastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndalref(void)
#else
ndalref()
#endif /* PROTOTYPE */
{
    return(DFANlastref());
}



/*-----------------------------------------------------------------------------
 * Name:    dfanlastref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Lastref
 * Returns: ref on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFANlastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfanlastref(void)
#else
ndfanlastref()
#endif /* PROTOTYPE */
{
    return(DFANlastref());
}



/*---------------------------------------------------------------------------
** Routines for handling file annotations
 *-------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    dfanaddfds
 * Purpose: add file description (Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANaddfileann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfanaddfds(intf *dfile, _fcd desc, intf *desclen)
#else
ndfanaddfds(dfile, desc, desclen)
    int32 *dfile;
    _fcd  desc;
    intf *desclen;
#endif /* PROTOTYPE */
{
    return ( DFANIaddfann( *dfile, _fcdtocp(desc), *desclen, DFAN_DESC) );
}

/*-----------------------------------------------------------------------------
 * Name:    dfangetfidlen
 * Purpose: get length of next file ID (Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          isfirst: 1: start with first one; 0: get length of next one
 * Returns: On success: length of next file ID; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfannlen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfangetfidlen(intf *dfile, intf *isfirst)
#else
ndfangetfidlen(dfile, isfirst)
    intf *dfile;
    intf *isfirst;
#endif /* PROTOTYPE */
{
     return ( DFANIgetfannlen( *dfile, DFAN_LABEL, (intn)*isfirst) );
}


/*-----------------------------------------------------------------------------
 * Name:    dfangetfdslen
 * Purpose: get length of next file description (Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          isfirst: 1: start with first one; 0: get length of next one
 * Returns: On success: length of next file ID; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfannlen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfangetfdslen(intf *dfile, intf *isfirst)
#else
ndfangetfdslen(dfile, isfirst)
    intf *dfile;
    intf *isfirst;
#endif /* PROTOTYPE */
{
     return ( DFANIgetfannlen( *dfile, DFAN_DESC, (intn)*isfirst) );
}


/*-----------------------------------------------------------------------------
 * Name:    dfangetfid
 * Purpose: get file ID (Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANgetfann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfangetfid(intf *dfile, _fcd id, intf *maxlen, intf *isfirst)
#else
ndfangetfid(dfile, id, maxlen, isfirst)
    intf *dfile;
    _fcd  id;
    intf *maxlen;
    intf *isfirst;
#endif /* PROTOTYPE */
{
    return( DFANIgetfann( *dfile, _fcdtocp(id), *maxlen,
            DFAN_LABEL, (intn)*isfirst));
}


/*-----------------------------------------------------------------------------
 * Name:    dfangetfds
 * Purpose: get file description (Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANgetfann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfangetfds(intf *dfile, _fcd id, intf *maxlen, intf *isfirst)
#else
ndfangetfds(dfile, id, maxlen, isfirst)
    intf *dfile;
     _fcd  id;
    intf *maxlen;
    intf *isfirst;
#endif /* PROTOTYPE */
{
    return( DFANIgetfann( *dfile, _fcdtocp(id), *maxlen,
            DFAN_DESC, (intn)*isfirst));
}

/*-----------------------------------------------------------------------------
** Versions with short names
**---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    daafds
 * Purpose: add file description (short form of DFANaddfds; Fortran callable)
 * Inputs:  dfile: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANaddfileann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndaafds(intf *dfile, _fcd desc, intf *desclen)
#else
ndaafds(dfile, desc, desclen)
    intf *dfile;
    _fcd  desc;
    intf *desclen;
#endif /* PROTOTYPE */
{
    return ( DFANIaddfann( *dfile, _fcdtocp(desc), *desclen, DFAN_DESC) );
}


/*-----------------------------------------------------------------------------
 * Name:    dagfidl
 * Purpose: get length of next file ID
 * Inputs:  dfile: pointer to HDF file
 *          isfirst: 1: start with first one; 0: get length of next one
 * Returns: On success: length of next file ID; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfannlen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndagfidl(intf *dfile, intf *isfirst)
#else
ndagfidl(dfile, isfirst)
    intf *dfile;
    intf *isfirst;
#endif /* PROTOTYPE */
{
     return ( DFANIgetfannlen( *dfile, DFAN_LABEL, (intn)*isfirst) );
}


/*-----------------------------------------------------------------------------
 * Name:    dagfdsl 
 * Purpose: get length of next file description (Fortran callable C version) 
 * Inputs:  dfile: pointer to HDF file
 *          isfirst: 1: start with first one; 0: get length of next one
 * Returns: On success: length of next file ID; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfannlen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndagfdsl(intf *dfile, intf *isfirst)
#else
ndagfdsl(dfile, isfirst)
    intf *dfile;
    intf *isfirst;
#endif /* PROTOTYPE */
{
     return ( DFANIgetfannlen( *dfile, DFAN_DESC, (intn)*isfirst) );
}


/*-----------------------------------------------------------------------------
 * Name:    dagfid
 * Purpose: get file ID (short form of DFANgetfid; Fortran callable version)
 * Inputs:  dfile: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndagfid(intf *dfile, _fcd id, intf *maxlen, intf *isfirst)
#else
ndagfid(dfile, id, maxlen, isfirst)
    intf *dfile;
    _fcd  id;
    intf *maxlen;
    intf *isfirst;
#endif /* PROTOTYPE */
{
    return( DFANIgetfann( *dfile, _fcdtocp(id), *maxlen,
            DFAN_LABEL, (intn)*isfirst));
}


/*-----------------------------------------------------------------------------
 * Name:    dagfds
 * Purpose: get file description 
 *          (short form of DFANgetfds; Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANgetfann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndagfds(intf *dfile, _fcd id, intf *maxlen, intf *isfirst)
#else
ndagfds(dfile, id, maxlen, isfirst)
    intf *dfile;
    _fcd  id;
    intf *maxlen;
    intf *isfirst;
#endif /* PROTOTYPE */
{
    return( DFANIgetfann( *dfile, _fcdtocp(id), *maxlen,
            DFAN_DESC, (intn)*isfirst));
}



/*-----------------------------------------------------------------------------
** Intermediate routines called from user's fortran routines
**---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    daiafid
 * Purpose: intermediate routine to add file ID (Fortran callable C version)
 * Inputs:  dfile: pointer to HDF file
 *          id: ID to write to file
 *          idlen: length of ID string
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   Fortran user routines DFANaddfid and daafid
 * Invokes: DFANaddfann
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndaiafid(intf *dfile, _fcd id, intf *idlen)
#else
ndaiafid(dfile, id, idlen)
    intf *dfile;
    _fcd  id;
    intf *idlen;
#endif /* PROTOTYPE */
{
    return(DFANIaddfann( *dfile, _fcdtocp(id),*idlen, DFAN_LABEL));
}

