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
 * File:    dfF.c
 * Purpose: C stubs for Fortran low level routines
 * Invokes: dfF.c
 * Contents: 
 *  dfiopen:   call DFopen to open HDF file
 *  dfclose:   call DFclose to close HDF file
 *  dfdesc:    call DFdescriptors to get contents of DDs
 *  dfdup:     call DFdup to create additional DD for item
 *  dfdel:     call DFdel to delete DD of item
 *  dfiaccess: call DFaccess to set up access to item
 *  dfstart:   call DFaccess to set up access to item
 *  dfread:    call DFread to read part of item
 *  dfseek:    call DFseek to move to offset within item
 *  dfwrite:   call DFwrite to write part of item
 *  dfupdate:  call DFupdate to write out changes
 *  dfget:     call DFgetelement to read item
 *  dfput:     call DFputelement to write item
 *  dfsfind:   call DFsetfind to set up search
 *  dffind:    call DFfind to find next matching item
 *  dferrno:   call DFerrno to return value of DFerror
 *  dfnewref:  call DFnewref to get unused ref no
 *  dfnumber:  call DFnumber to get number of occurrances of given tag
 *  dfstat:    call DFstat to get status info on file
 *  dfiishdf:  call DFishdf to get HDF string
 *---------------------------------------------------------------------------*/

#include "df.h"

#ifndef DF_FNAMES
#   define DF_FNAMES
#ifdef DF_CAPFNAMES
#   define ndfiaccess   FNAME(DFIACCESS)
#   define ndfiopen  FNAME(DFIOPEN)
#   define ndfclose  FNAME(DFCLOSE)
#   define ndfdesc   FNAME(DFDESC)
#   define ndfdup    FNAME(DFDUP)
#   define ndfdel    FNAME(DFDEL)
#   define ndfstart  FNAME(DFSTART)
#   define ndfread   FNAME(DFREAD)
#   define ndfseek   FNAME(DFSEEK)
#   define ndfwrite  FNAME(DFWRITE)
#   define ndfupdate FNAME(DFUPDATE)
#   define ndfget    FNAME(DFGET)
#   define ndfput    FNAME(DFPUT)
#   define ndfsfind  FNAME(DFSFIND)
#   define ndffind   FNAME(DFFIND)
#   define ndferrno  FNAME(DFERRNO)
#   define ndfnewref FNAME(DFNEWREF)
#   define ndfnumber FNAME(DFNUMBER)
#   define ndfstat   FNAME(DFSTAT)
#   define ndfiishdf FNAME(DFIISHDF)
#else   /* !DF_CAPFNAMES */
#   define ndfiaccess   FNAME(dfiaccess)
#   define ndfiopen  FNAME(dfiopen)
#   define ndfclose  FNAME(dfclose)
#   define ndfdesc   FNAME(dfdesc)
#   define ndfdup    FNAME(dfdup)
#   define ndfdel    FNAME(dfdel)
#   define ndfstart  FNAME(dfstart)
#   define ndfread   FNAME(dfread)
#   define ndfseek   FNAME(dfseek)
#   define ndfwrite  FNAME(dfwrite)
#   define ndfupdate FNAME(dfupdate)
#   define ndfget    FNAME(dfget)
#   define ndfput    FNAME(dfput)
#   define ndfsfind  FNAME(dfsfind)
#   define ndffind   FNAME(dffind)
#   define ndferrno  FNAME(dferrno)
#   define ndfnewref FNAME(dfnewref)
#   define ndfnumber FNAME(dfnumber)
#   define ndfstat   FNAME(dfstat)
#   define ndfiishdf FNAME(dfiishdf)
#endif /* DF_CAPFNAMES */
#endif /* DF_FNAMES */

/*-----------------------------------------------------------------------------
 * Name:    dfiopen
 * Purpose: call DFopen to open HDF file
 * Inputs:  name: name of file to open
 *      access: access mode - integer with value DFACC_READ etc. 
 *      defdds: default number of DDs per header block
 *      namelen: length of name
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFopen
 * Method:  Convert filename to C string, call DFopen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfiopen(_fcd name, intf *access, intf *defdds, intf *namelen)
#else
ndfiopen(name, access, defdds, namelen)
    _fcd name;
    intf *access, *defdds;
    intf *namelen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;
    
    fn = DFIf2cstring(name, (intn)*namelen);
    ret = (int32) DFopen(fn, (intn)*access, (intn)*defdds);
    HDfreespace((VOIDP)fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dfclose
 * Purpose: Call DFclose to close HDF file
 * Inputs:  dfile: pointer to HDF file to close
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFclose
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfclose(intf *dfile)
#else
ndfclose(dfile)
    intf *dfile;
#endif /* PROTOTYPE */
{
    return(DFclose((DF *)*dfile));
}


/*-----------------------------------------------------------------------------
 * Name:    dfdesc
 * Purpose: Call DFdescriptors to obtain descriptors
 * Inputs:  dfile: pointer to HDF file
 *          ptr: pointer to array of size >= (4, num) to put descriptors in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFdesc
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfdesc(intf *dfile, intf ptr[][4], intf *begin, intf *num)
#else
ndfdesc(dfile, ptr, begin, num)
    intf *dfile, *begin, *num;
    intf ptr[][4];
#endif /* PROTOTYPE */
{
    DFdesc *ptr1;
    int i;
    intf num_desc;

            /* allocate temporary space */
    ptr1 = (DFdesc *) HDgetspace((uint32)*num * sizeof(DFdesc));
    num_desc = DFdescriptors((DF*)*dfile, ptr1, (intn)*begin, (intn)*num);
    
    /* copy ptr1 array  ptr; note row/column inversion */
    for (i=0; i<num_desc; i++)
    {
        ptr[i][0] = (int32)ptr1[i].tag;
        ptr[i][1] = (int32)ptr1[i].ref;
        ptr[i][2] = ptr1[i].offset;
        ptr[i][3] = ptr1[i].length;
    }

    HDfreespace((VOIDP)ptr1);

    return(num_desc);
}


/*-----------------------------------------------------------------------------
 * Name:    dfdup
 * Purpose: Call DFdup to create additional DD for item
 * Inputs:  dfile: pointer to HDF file
 *          tag, ref: attributes of new DD to add
 *          otag, oref: attributes of item to point to
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFdup
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfdup(intf *dfile, intf *tag, intf *ref, intf *otag, intf *oref)
#else
ndfdup(dfile, tag, ref, otag, oref)
    intf *dfile;
    intf *tag, *ref, *oref, *otag;
#endif /* PROTOTYPE */
{
    return(DFdup((DF *)*dfile, (uint16)*tag, (uint16)*ref, (uint16)*otag,
    	   (uint16)*oref));
}


/*-----------------------------------------------------------------------------
 * Name:    dfdel
 * Purpose: Call DFdel to delete DD of item
 * Inputs:  dfile: pointer to HDF file
 *          tag, ref: attributes of DD to delete
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFdel
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfdel(intf *dfile, intf *tag, intf *ref)
#else
ndfdel(dfile, tag, ref)
    intf *dfile;
    intf *tag, *ref;
#endif /* PROTOTYPE */
{
    return (DFdel((DF *)*dfile, (uint16)*tag, (uint16)*ref));
}


/*-----------------------------------------------------------------------------
 * Name:    dfiaccess
 * Purpose: Call DFaccess to set up access to item
 * Inputs:  dfile: pointer to HDF file
 *          tag, ref: attributes of item to access
 *          access: access mode
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFaccess
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfiaccess(intf *dfile, intf *tag, intf *ref, _fcd access, intf *acclen)
#else
ndfiaccess(dfile, tag, ref, access, acclen)
    intf *dfile;
    intf *tag, *ref;
    _fcd access;
    intf *acclen;
#endif /* PROTOTYPE */
{
    char *acc;
    intf ret;

    acc = DFIf2cstring(access, (intn)*acclen);
    ret = (int32)DFaccess((DF *) *dfile, (uint16)*tag, (uint16)*ref, acc);
    HDfreespace((VOIDP)acc);
    return(ret);
}


#if 0
/*-----------------------------------------------------------------------------
 * Name:    dfstart
 * Purpose: Call DFaccess to set up access to item
 * Inputs:  dfile: pointer to HDF file
 *          tag, ref: attributes of item to access
 *          access: access mode
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFaccess
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfstart(intf *dfile, intf *tag, intf *ref, char *access)
#else
ndfstart(dfile, tag, ref, access)
    intf *dfile;
    intf *tag, *ref;
    char *access;
#endif /* PROTOTYPE */
{
    return(DFaccess((DF *)*dfile, (uint16)*tag, (uint16)*ref, access));
}
#endif /* 0 */

/*-----------------------------------------------------------------------------
 * Name:    dfread
 * Purpose: Call DFread to read part of item
 * Inputs:  dfile: pointer to HDF file
 *          ptr: pointer to space to read item into
 *          len: number of bytes to read
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFread
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfread(intf *dfile, _fcd ptr, intf *len)
#else
ndfread(dfile, ptr, len)
    intf *dfile, *len;
    _fcd ptr;
#endif /* PROTOTYPE */
{
    return (DFread((DF *) *dfile, (char *)_fcdtocp(ptr), *len));
}

/*-----------------------------------------------------------------------------
 * Name:    dfseek
 * Purpose: Call DFseek to move to offset within item
 * Inputs:  dfile: pointer to HDF file
 *	    offset: number of bytes from beginning of item to move to
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFseek
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfseek(intf *dfile, intf *offset)
#else
ndfseek(dfile, offset)
    intf *dfile, *offset;
#endif /* PROTOTYPE */
{
    return (DFseek((DF *)*dfile, *offset));
}


/*-----------------------------------------------------------------------------
 * Name:    dfwrite
 * Purpose: Call DFwrite to write part of item
 * Inputs:  dfile: pointer to HDF file
 *	    ptr: pointer to data to write
 *	    len: number of bytes to write
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFwrite
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfwrite(intf *dfile, _fcd ptr, intf *len)
#else
ndfwrite(dfile, ptr, len)
    intf *dfile, *len;
    _fcd ptr;
#endif /* PROTOTYPE */
{
    return (DFwrite((DF *)*dfile, (char *)_fcdtocp(ptr), *len));
}

/*-----------------------------------------------------------------------------
 * Name:    dfupdate
 * Purpose: Call DFupdate to write out changes
 * Inputs:  dfile: pointer to HDF file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFupdate
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfupdate(intf *dfile)
#else
ndfupdate(dfile)
    intf *dfile;
#endif /* PROTOTYPE */
{
    return (DFupdate((DF *)*dfile));
}


/*-----------------------------------------------------------------------------
 * Name:    dfget
 * Purpose: Call DFget to read an element
 * Inputs:  dfile: pointer to HDF file
 *	    tag, ref: pointer to item to read
 *	    ptr: space to read item into
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFgetelement
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfget(intf *dfile, intf *tag, intf *ref, _fcd ptr)
#else
ndfget(dfile, tag, ref, ptr)
    intf *dfile;
    intf *tag, *ref;
    _fcd ptr;
#endif /* PROTOTYPE */
{
    return (DFgetelement((DF *)*dfile, (uint16)*tag, (uint16)*ref,
    	    (char *)_fcdtocp(ptr)));
}


/*-----------------------------------------------------------------------------
 * Name:    dfput
 * Purpose: Call DFput to write an element
 * Inputs:  dfile: pointer to HDF file
 *	    tag, ref: attributes of item to write
 *	    ptr: item to write
 *	    len: size of item
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFputelement
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfput(intf *dfile, intf *tag, intf *ref, _fcd ptr, intf *len)
#else
ndfput(dfile, tag, ref, ptr, len)
    intf *dfile;
    intf *tag, *ref;
    intf *len;
    _fcd ptr;
#endif /* PROTOTYPE */
{
    return (DFputelement((DF *)*dfile, (uint16)*tag, (uint16)*ref,
	    (char*)_fcdtocp(ptr), *len));
}

/*-----------------------------------------------------------------------------
 * Name:    dfsfind
 * Purpose: Call DFsetfind to set up search
 * Inputs:  dfile: pointer to HDF file
 *	    tag, ref: attributes of item to find
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFsetfind
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsfind(intf *dfile, intf *tag, intf *ref)
#else
ndfsfind(dfile, tag, ref)
    intf *dfile;
    intf *tag, *ref;
#endif /* PROTOTYPE */
{
    return (DFsetfind((DF *) *dfile, (uint16)*tag, (uint16)*ref));
}


/*-----------------------------------------------------------------------------
 * Name:    dffind
 * Purpose: Call DFfind to find next match
 * Inputs:  dfile: pointer to HDF file
 *	    itag, iref: attributes of item found
 *	    len: size of item
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFfind
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndffind(intf *dfile, intf *itag, intf *iref, intf *len)
#else
ndffind(dfile, itag, iref, len)
    intf *dfile;
    intf *itag, *iref;
    intf *len;
#endif /* PROTOTYPE */
{
    DFdesc *ptr1;
    intf ret;

    ptr1 = (DFdesc *) HDgetspace((uint32)sizeof(DFdesc));
    ret = DFfind((DF *) *dfile, ptr1);

    *itag  = (int32)(ptr1->tag);
    *iref = (int32)(ptr1->ref);
    *len = ptr1->length;

    HDfreespace((VOIDP)ptr1);

    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dferrno
 * Purpose: Call DFerrno to get value of DFerror
 * Inputs:  none
 * Returns: value of DFerror
 * Users:   HDF Fortran programmers
 * Invokes: DFerrno
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndferrno(void)
#else
ndferrno()
#endif /* PROTOTYPE */
{
    return(DFerrno());
}

/*-----------------------------------------------------------------------------
 * Name:    dfnewref
 * Purpose: Call DFnewref to get unused ref no
 * Inputs:  dfile: pointer to HDF file
 * Returns: int16: unused ref no
 * Users:   HDF Fortran programmers
 * Invokes: DFnewref
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfnewref(intf *dfile)
#else
ndfnewref(dfile)
intf *dfile;
#endif /* PROTOTYPE */
{
    return(DFnewref((DF *) *dfile));
}


/*-----------------------------------------------------------------------------
 * Name:    dfnumber
 * Purpose: Call DFnumber to get unused ref no
 * Inputs:  dfile: pointer to HDF file
 *	    tag:   pointer to (int16)tag to count
 * Returns: int: number of occurances of given tag
 * Users:   HDF Fortran programmers
 * Invokes: DFnumber
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfnumber(intf *dfile, intf *tag)
#else
ndfnumber(dfile, tag)
intf *dfile;
intf *tag;
#endif /* PROTOTYPE */
{
    return(DFnumber((DF *) *dfile, (uint16)*tag));
}


/*-----------------------------------------------------------------------------
 * Name:    dfstat
 * Purpose: Call DFstat to get staus info on file
 * Inputs:  dfile:  pointer to HDF file
 *	    dfinfo: pointer to DFdata structure to fill in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFstat
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfstat(intf *dfile, DFdata *dfinfo)
#else
ndfstat(dfile, dfinfo)
intf *dfile;
DFdata *dfinfo;
#endif /* PROTOTYPE */
{
    return(DFstat((DF *) *dfile, dfinfo));
}


/*-----------------------------------------------------------------------------
 * Name:    dfiishdf
 * Purpose: Call DFishdf to test file
 * Inputs:  name:    name of file to test
 *	    namelen: pointer to variable containing length of name string
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFishdf
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfiishdf(_fcd name, intf *namelen)
#else
ndfiishdf(name, namelen)
    _fcd name;
    intf *namelen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = DFIf2cstring(name, (intn)*namelen);
    ret = DFishdf(fn);
    HDfreespace((VOIDP)fn);
    return(ret);
}

