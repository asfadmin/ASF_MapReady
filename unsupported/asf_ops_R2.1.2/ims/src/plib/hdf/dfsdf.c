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
 * File:    dfsdF.c
 * Purpose: C stubs for Fortran SDS routines
 * Invokes: dfsd.c dfkit.c
 * Contents: 
 *  dsgdast:       Call DFSDgetdatastrs to get attributes of data
 *  dsigdis:       Call DFSDgetdimstrs to get attributes of a dimension
 *  dsgdisc:       Call DFSDgetdimscale to get scale for a dimension
 *  dsgrang:       Call DFSDgetmaxmin to get max and min data values
 *  dssdims:       Call DFSDsetdims to set dimensions for subsequent SDGs
 *  dssdisc:       Call DFSDsetdimscale to set scale for subsequent SDGs
 *  dssrang:       Call DFSDsetmaxmin to set max/min values for subsequent SDGs
 *  dsclear:       Call DFSDclear to erase values set for subsequent SDGs
 *  dsslens:       Call DFSDsetlengths to set maximum lengths of string
 *  dsgdiln:       Call DFSDgetdimlen to get lengths of strings for a dimension
 *  dsgdaln:       Call DFSDgetdatalen to get lengths of data strings
 *  dsfirst:       Call DFSDrestart to get SDGs again from beginning of file
 *  dspslc:        Call DFSDIputslice to write slice to file
 *  dseslc:        Call DFSDendslice to end slice writes, write SDG to file
 *  dssnt:         Call DFSDsetNT to set number type
 *  dsgnt:         Call DFSDgetNT to get number type for reading
 *  dsigdim:       Call DFSDgetdims to get dimensions of next SDG
 *  dsigdat:       Call DFSDgetdata to get data values
 *  dsipdat:       Call DFSDIputdata to write SDG to new file
 *  dsiadat:       Call DFSDIputdata to append SDG to existing file
 *  dsigslc:       Call DFSDIgetslice to get slice from file
 *  dsisslc:       Call DFSDstartslice to set up to write slice
 *  dslref:        Call DFSDlastref to get ref of last SDS accessed
 *  dsinum:        Call DFSDndatasets to get number of SDG in the file
 *  dsip32s:       Call DFSDpre32sdg to test if the sdg was written by HDF prior to
 *                      version 3.2
 *  dfsdgetdatastrs_:Call DFSDgetdatastrs to get attributes of data
 *  dfsdgetdimscale_:Call DFSDgetdimscale to get scale for a dimension
 *  dfsdgetrange_:  Call DFSDgetmaxmin to get max and min data values
 *  dfsdsetdims_:   Call DFSDsetdims to set dimensions for subsequent SDGs
 *  dfsdsetdimscale_:Call DFSDsetdimscale to set scale for subsequent SDGs
 *  dfsdsetrange_:  Call DFSDsetmaxmin to set max/min values for subsequent SDGs
 *  dfsdclear_:     Call DFSDclear to erase values set for subsequent SDGs
 *  dfsdsetlengths_:Call DFSDsetlengths to set maximum lengths of string
 *  dfsdgetdimlen_: Call DFSDgetdimlen to get lengths of strings for a dimension
 *  dfsdgetdatalen_:Call DFSDgetdatalen to get lengths of data strings
 *  dfsdrestart_:   Call DFSDrestart to get SDGs again from beginning of file
 *  dfsdputslice_:  Call DFSDIputslice to write slice to file
 *  dfsdendslice_:  Call DFSDendslice to end slice writes, write SDG to file
 *  dfsdsetnt_:     Call DFSDsetNT to set number type
 *  dfsdgetnt_:	    Call DFSDgetNT to get number type
 *  dfsdlastref_:   Call DFSDlastref to get ref of last SDS accessed
 *  dsiwref:        Call DFSDwriteref to set up next ref to write
 *  dssfill:        Call DFSDsetfillvalue to set fill value for SDS
 *  dsgfill:        Call DFSDgetfillvalue to get fill value from SDS
 *  dsisslab:       Call DFSDstartslab to set up write to SDS
 *  dswslab:        Call DFSDwriteslab to write slab to file
 *  dseslab:        Call DFSDendslab to end slab writes, write NDG to file
 * Remarks: no C stubs needed for the put string routines, only Fortran stubs
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "dfsd.h"

/*-----------------------------------------------------------------------------
 * Name:    dsgdisc
 * Purpose: Call DFSDgetdimscale to get scale for a dimension
 * Inputs:  dim: dimension to get attributes for
 *          maxsize: size of scale array
 *          scale: array to return scale in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdimscale
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsgdisc(intf *dim, intf *maxsize, VOIDP scale)
#else
ndsgdisc(dim, maxsize, scale)
    intf *dim;
    intf *maxsize;
    VOID *scale;
#endif /* PROTOTYPE */
{
    intn rank, cdim;
    intf ret;
    intn isndg;

    ret = DFSDIisndg(&isndg);
    if (isndg) 	{
        ret = DFSDIgetrrank(&rank);
        if (rank < *dim)
            return FAIL;
        cdim = rank - (intn)*dim + 1;
    }
    else cdim = (intn)*dim;

    return(DFSDgetdimscale(cdim, *maxsize, scale));
}


/*-----------------------------------------------------------------------------
 * Name:    dsgrang
 * Purpose: Call DFSDgetrange to get maximum and minimum data values
 * Inputs:  pmax: float to return maximum in
 *          pmin: float to return minimum in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetrange
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsgrang(VOIDP pmax, VOIDP pmin)
#else
ndsgrang(pmax, pmin)
    VOID *pmax, *pmin;
#endif /* PROTOTYPE */
{
    return(DFSDgetrange(pmax, pmin));
}


/*-----------------------------------------------------------------------------
 * Name:    dssdims
 * Purpose: Call DFSDsetdims to set dimensions for subsequent SDGs
 * Inputs:  rank: no of dimensions of SDG
 *          dimsizes: array containing dimensions of SDG
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetdims
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndssdims(intf *rank, intf dimsizes[])
#else
ndssdims(rank, dimsizes)
    intf *rank;
    intf dimsizes[];
#endif /* PROTOTYPE */
{
    int32 i, *cdims, *p;
    intf ret;

    p = (int32 *)HDgetspace((uint32)((*rank)*sizeof(int32)));
    if (p == NULL) return FAIL;
    cdims = p;
    for (i=1; i <=  *rank ; i++)	{
        *p = dimsizes[*rank - i];
        p++;
    }
   
    ret = DFSDsetdims((intn)*rank, cdims);
    HDfreespace((VOIDP)cdims);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dssdisc
 * Purpose: Call DFSDsetdimscale to set scales for subsequent SDGs
 * Inputs:  dim: dimension to set scale for
 *          dimsize: size of array scale
 *          scale: array of scale values
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetdimscale
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndssdisc(intf *dim, intf *dimsize, VOIDP scale)
#else
ndssdisc(dim, dimsize, scale)
    intf *dim;
    intf *dimsize;
    VOID *scale;
#endif /* PROTOTYPE */
{
    int cdim, ret;
    intn rank;

    ret = DFSDIgetwrank(&rank);
    if (rank < *dim) return FAIL;
    cdim = rank - (intn)*dim + 1;

    return(DFSDsetdimscale(cdim, *dimsize, scale));
}


/*-----------------------------------------------------------------------------
 * Name:    dssrang
 * Purpose: Call DFSDsetrange to set max and min values for this SDG
 * Inputs:  max, min: max and min data values
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetrange
 * Remarks: Max and Min are set only for next SDG, reset to NULL after
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndssrang(VOIDP max, VOIDP min)
#else
ndssrang(max, min)
    VOID *max, *min;
#endif /* PROTOTYPE */
{
    return(DFSDsetrange(max, min));
}


/*-----------------------------------------------------------------------------
 * Name:    dsclear
 * Purpose: Call DFSDclear to erase values set for subsequent SDGs
 * Inputs:  none
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDclear
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsclear(void)
#else
ndsclear()
#endif /* PROTOTYPE */
{
    return(DFSDclear());
}


/*-----------------------------------------------------------------------------
 * Name:    dsslens
 * Purpose: Call DFSDsetlengths to set max lengths of strings
 * Inputs:  maxlen_label, maxlen_unit, maxlen_format, maxlen_coordsys: max lens
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetlengths
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsslens(intf *maxlen_label, intf *maxlen_unit, intf *maxlen_format,
     intf *maxlen_coordsys)
#else
ndsslens(maxlen_label, maxlen_unit, maxlen_format, maxlen_coordsys)
    intf *maxlen_label,
        *maxlen_unit,
        *maxlen_format,
        *maxlen_coordsys;
#endif /* PROTOTYPE */
{
    return(DFSDsetlengths((intn)*maxlen_label, (intn)*maxlen_unit,
            (intn)*maxlen_format, (intn)*maxlen_coordsys));
}


/*-----------------------------------------------------------------------------
 * Name:    dsgdiln
 * Purpose: Call DFSDgetdimlen to get actual lengths of strings
 * Inputs:  dim: dimension to get lengths for
 *          llabel, lunit, lformat: integers to return lengths of each string in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdimlen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsgdiln(intf *dim, intf *llabel, intf *lunit, intf *lformat)
#else
ndsgdiln(dim, llabel, lunit, lformat)
    intf *dim, *llabel, *lunit, *lformat;
#endif /* PROTOTYPE */
{
    intn rank, cdim;
    intf ret;
    intn isndg;
    intn cllabel, clunit, clformat;     /* convert between intf and intn */

    ret = DFSDIisndg(&isndg);
    if (isndg) 	{
        ret = DFSDIgetrrank(&rank);
        if (rank < *dim)
            return FAIL;
        cdim = rank - (intn)*dim + 1;
    }
    else cdim = (intn)*dim;

    ret=(intf)DFSDgetdimlen(cdim, &cllabel, &clunit, &clformat);
    if(ret!=FAIL) {     /* if ok, copy the values over */
        *llabel=cllabel;
        *lunit=clunit;
        *lformat=clformat;
      } /* end if */
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dsgdaln
 * Purpose: Call DFSDgetdatalen to get actual lengths of strings
 * Inputs:  llabel, lunit, lformat, lcoordsys: integers to return lengths in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdatalen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsgdaln(intf *llabel, intf *lunit, intf *lformat, intf *lcoordsys)
#else
ndsgdaln(llabel, lunit, lformat, lcoordsys)
    intf *llabel, *lunit, *lformat, *lcoordsys;
#endif /* PROTOTYPE */
{
    intf ret;
    intn cllabel, clunit, clformat, clcoordsys;

    ret=(intf)DFSDgetdatalen(&cllabel, &clunit, &clformat, &clcoordsys);
    if(ret!=FAIL) {
        *llabel=cllabel;
        *lunit=clunit;
        *lformat=clformat;
        *lcoordsys=clcoordsys;
      } /* end if */
    return(ret);
}


/*----------------------------------------------------------------------------- 
 * Name:    dsfirst * Purpose: Call DFSDrestart to get SDGs again from the beginning
 * Inputs:  none
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDrestart
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsfirst(void)
#else
ndsfirst()
#endif /* PROTOTYPE */
{

    return(DFSDrestart());
}


/*-----------------------------------------------------------------------------
 * Name:    dspslc
 * Purpose: Call DFSDIputslice to write slice to file 
 * Inputs:  windims: array of size rank, containing size of slice 
 *          data: array containing slice 
 *          dims: dimensions of array data 
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIputslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndspslc(intf windims[], VOIDP data, intf dims[])
#else
ndspslc(windims, data, dims)
    intf windims[];
    VOID  *data;
    intf dims[];
#endif /* PROTOTYPE */
{
    int32 *cdims, *cwindims, *p, *wp;
    intn i, rank;
    intf ret;

    ret = DFSDIgetwrank(&rank);
    wp = (int32 *)HDgetspace((uint32)(rank*sizeof(int32)));
    if (wp == NULL) return FAIL;
    cwindims = wp;
    p = (int32 *)HDgetspace((uint32)(rank*sizeof(int32)));
    if (p == NULL) return FAIL;
    cdims = p;
    for (i=1; i <=  rank ; i++)	{  /* reverse dims & windims */
        *p = dims[rank - i];
        p++;
        *wp = windims[rank - i];
        wp++;
    }
   
    ret = DFSDIputslice(cwindims, data, cdims, 1);
    HDfreespace((VOIDP)cdims);
    HDfreespace((VOIDP)cwindims);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dseslc
 * Purpose: Call DFSDIendslice to finish slice writes and write out SDG
 * Inputs:  none
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIendslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndseslc(void)
#else
ndseslc()
#endif /* PROTOTYPE */
{

    return(DFSDIendslice(1));
}


/*-----------------------------------------------------------------------------
 * Name:    dssnt
 * Purpose: Call DFSDsetNT to set number type for subsequent calls to
            DFSDputdata, DFSDadddata, DFSDsetdimscales.
 * Inputs:  numbertype
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran programmers
 * Method:  Invokes DFSDsetNT
 * Remarks: 0 specifies default value
 *---------------------------------------------------------------------------*/

        FRETVAL(intf)
#ifdef PROTOTYPE
ndssnt(intf *numbertype)
#else
ndssnt(numbertype)
    intf *numbertype;
#endif /* PROTOTYPE */
{
    return(DFSDsetNT(*numbertype));
}

/*----------------------------------------------------------------------------
 * Name:    dsgnt
 * Purpose: Call DFSDgetNT to get number type for subsequent calls 
 * Inputs:  pnumbertype
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran programmers
 * Method:  Invokes DFSDgetNT
 * Remarks: 0 specifies default value
 *---------------------------------------------------------------------------*/

        FRETVAL(intf)
#ifdef PROTOTYPE
ndsgnt(intf *pnumbertype)
#else
ndsgnt(pnumbertype)
    intf *pnumbertype;
#endif /* PROTOTYPE */
{
    return(DFSDgetNT((int32 *)pnumbertype));
}


/*-----------------------------------------------------------------------------
 * Name:    dsigdim
 * Purpose: Call DFSDgetdims to get dimensions of next SDG
 * Inputs:  filename: name of HDF file
 *          prank: integer to return rank in
 *          sizes: array to return dimensions in
 *          maxrank: dimension of array sizes
 *          lenfn: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   Fortran stub routine
 * Invokes: DFSDgetdims
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsigdim(_fcd filename, intf *prank, intf sizes[], intf *maxrank,
     intf *lenfn)
#else
ndsigdim(filename, prank, sizes, maxrank, lenfn)
    _fcd filename;
    intf *prank, sizes[], *maxrank, *lenfn;
#endif /* PROTOTYPE */
{
    char *fn;
    int32 i, tmp;
    intn isndg;
    intf ret;

    fn = HDf2cstring(filename, (intn)*lenfn);
    ret = DFSDgetdims(fn,(intn *) prank, (int32 *)sizes, (intn) *maxrank);
    DFSDIisndg(&isndg);
    if (isndg) {
        for (i=0; i<((int32)*prank)/2; i++) {
            tmp = sizes[i];
            sizes[i] = sizes[(int32)*prank -i -1];
            sizes[(int32)*prank -i -1] = tmp;
        }
    }
    HDfreespace(fn);
    return ret;
}


/*-----------------------------------------------------------------------------
 * Name:    dsigdat
 * Purpose: Call DFSDgetdata to get data values
 * Inputs:  filename: name of HDF file
 *          rank: no of dimensions in array data
 *          maxsizes: array containing dimensions of the array data
 *          data: array to return the data in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIgetdata,DFSDIrefresh,DFSDIisndg
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsigdat(_fcd filename, intf *rank, intf maxsizes[], VOIDP data, intf *fnlen)
#else
ndsigdat(filename, rank, maxsizes, data, fnlen)
    _fcd filename;
    intf *rank, maxsizes[], *fnlen;
    VOID *data;
#endif /* PROTOTYPE */
{
    int32 i;
    intn isndg;
    intf ret;
    char *fn;
    int32 *p, *cmaxsizes;
    
    fn = HDf2cstring(filename, (intn) *fnlen);
      /* if DFSDgetdims has not be called call DFSDIsdginfo to */
      /* refresh Readsdg       */
    if (DFSDIrefresh(fn)<0) return FAIL;
    ret = DFSDIisndg(&isndg);
    if (isndg)	{
    	p = (int32 *)HDgetspace((uint32)((*rank)*sizeof(int32)));
    	if (p == NULL) return FAIL;
        cmaxsizes = p;

    	for (i=1; i <=  *rank ; i++)	{
            *p = maxsizes[*rank - i];
            p++;
        }
    	ret = DFSDIgetdata(fn, (intn)*rank, cmaxsizes, data, 1);
    	HDfreespace((VOIDP)cmaxsizes);
    }
    else	
        ret = DFSDIgetdata(fn, (intn)*rank, (int32 *)maxsizes, data, 1); /* 1==FORTRAN */
    HDfreespace(fn);
    return ret;
}


/*-----------------------------------------------------------------------------
 * Name:    dsipdat
 * Purpose: Call DFSDIputdata to write SDG to new file
 * Inputs:  filename: name of HDF file
 *          rank: no of dimensions of array data
 *          dimsizes: array containing size of each dimension of array data
 *          data: array containing data values
 *          fnlen: length of string filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIputdata
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsipdat(_fcd filename, intf *rank, intf dimsizes[], VOIDP data, intf *fnlen)
#else
ndsipdat(filename, rank, dimsizes, data, fnlen)
    _fcd filename;
    intf *rank;
    intf dimsizes[];
    VOID *data;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    int32 i, *cdims, *p;
    intf ret;

	/* reverse the dimsizes first  */
    p = (int32 *)HDgetspace((uint32)((*rank)*sizeof(int32)));
    if (p == NULL) return FAIL;
    cdims = p;
    for (i=1; i <=  *rank ; i++)	{
        *p = dimsizes[*rank - i];
        p++;
    }
    fn = HDf2cstring(filename, (intn) *fnlen);

    /* 0, 1 specify create mode, called from FORTRAN program */
    /* In HDF3.2 .hdf files, data and dimsizes are in C order  */
    ret = DFSDIputdata(fn, (intn)*rank, cdims, data, 0, 1);
    HDfreespace(fn);
    HDfreespace((VOIDP)cdims);

    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dsiadat
 * Purpose: Call DFSDIputdata to append SDG to existing file
 * Inputs:  filename: name of HDF file
 *          rank: no of dimensions of array data
 *          dimsizes: array containing size of each dimension of array data
 *          data: array containing data values
 *          fnlen: length of string filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIputdata
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsiadat(_fcd filename, intf *rank, intf dimsizes[], VOIDP data, intf *fnlen)
#else
ndsiadat(filename, rank, dimsizes, data, fnlen)
    _fcd filename;
    intf *rank;
    intf dimsizes[];
    VOID *data;
    intf *fnlen;
#endif /* PROTOTYPE */     
{
    char *fn;
    int32 i,  *cdims, *p;
    intf ret;

    /* reverse the dimsizes first  */
    p = (int32 *)HDgetspace((uint32)((*rank)*sizeof(int32)));
    if (p == NULL) return FAIL;
    cdims = p;
    for (i=1; i <=  *rank ; i++) {
    	*p = dimsizes[*rank - i];
    	p++;
    }
    fn = HDf2cstring(filename, (intn) *fnlen);
   
    /* 1, 1 specify create mode, called from FORTRAN program */
    /* In HDF3.2 .hdf files, data and dimsizes are in C order  */
    ret = DFSDIputdata(fn, (intn)*rank, cdims, data, 1, 1);
    HDfreespace(fn);
    HDfreespace((VOIDP)cdims);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dsigslc
 * Purpose: Call DFSDIgetslice to read slice from file
 * Inputs:  filename: name of HDF file
 *          winst: array of size = rank of data, containing start of slice
 *          windims: array of size rank, containing end of slice
 *          data: array for returning slice
 *          ndims: no of dims of array data
 *          dims: dimensions of array data
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIgetslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsigslc(_fcd filename, intf winst[], intf windims[], VOIDP data, intf dims[],
    intf *fnlen)
#else
ndsigslc(filename, winst, windims, data, dims, fnlen)
    _fcd filename;
    intf winst[], windims[];
    intf dims[];
    VOID *data;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;
    intn rank,i;
    int32 *cdims, *cwindims, *cwinst, *p, *wp, *wsp;
    intn isndg;

    fn = HDf2cstring(filename, (intn)*fnlen);
   
      /* if DFSDgetdims has not be called call DFSDIsdginfo to */
      /* refresh Readsdg       */
    if (DFSDIrefresh(fn)<0) return FAIL;
 
    ret = DFSDIisndg(&isndg);
    if (isndg)	{
	ret = DFSDIgetrrank(&rank);
    	p = (int32 *)HDgetspace((uint32)(rank*sizeof(int32)));
    	if (p == NULL) return FAIL;
        cdims = p;
    	wp = (int32 *)HDgetspace((uint32)(rank*sizeof(int32)));
    	if (wp == NULL) return FAIL;
        cwindims = wp;
    	wsp = (int32 *)HDgetspace((uint32)(rank*sizeof(int32)));
    	if (wsp == NULL) return FAIL;
        cwinst = wsp;

    	for (i=1; i <=  rank ; i++)	{
            *p = dims[rank - i];
            p++;
            *wp = windims[rank - i];
            wp++;
            *wsp = winst[rank - i];
            wsp++;
        }
    	ret = DFSDIgetslice(fn, cwinst, cwindims, data, cdims, 1);
    	HDfreespace((VOIDP)cdims);
    	HDfreespace((VOIDP)cwindims);
    	HDfreespace((VOIDP)cwinst);
    }
    else	
        ret = DFSDIgetslice(fn, (int32 *)winst, (int32 *)windims,
                data, (int32 *)dims, 1);
    HDfreespace(fn);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dsisslc
 * Purpose: Call DFSDstartslice to set up to write slice
 * Inputs:  filename: name of HDF file
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDstartslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsisslc(_fcd filename, intf *fnlen)
#else
ndsisslc(filename, fnlen)
    _fcd filename;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFSDstartslice(fn);
    HDfreespace(fn);
    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    dsirref
 * Purpose: Call DFSDreadref to set up next ref to read
 * Inputs:  filename: name of HDF file
 *	    ref: next ref to read
 *          fnlen: length of filename
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDstartslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsirref(_fcd filename, intf *ref, intf *fnlen)
#else
ndsirref(filename, ref, fnlen)
    _fcd filename;
    intf *ref;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFSDreadref(fn, (uint16) *ref);
    HDfreespace(fn);
    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    dslref
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
ndslref(void)
#else
ndslref()
#endif /* PROTOTYPE */
{
    return(DFSDlastref());
}


/*-----------------------------------------------------------------------------
 * Name:    dsinum
 * Purpose: Return number of SDGs in the file
 * Inputs:  filename: name of file
 *          len: length of Fortran string filename
 * Returns: number of SDGs on success, -1 on failure with DFerror set
 * Users:   dsnum, dfsdnumber
 * Invokes: DFSDndataset, HDf2cstring
 * Method:  convert string, call DFSDndatasets
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE  
ndsinum(_fcd filename, intf *len)
#else
ndsinum(filename, len)
    _fcd filename;
    intf *len;
#endif /* PROTOTYPE */
{
    char *cname;
    intf status;

    cname = HDf2cstring(filename, (intn) *len);
    status = DFSDndatasets(cname);
    HDfreespace(cname);

    return(status);
}


/*------------------------------------------------------------------------------
* Name:     dsip32s
* Purpose:  tests if the SDG with the specified ref was written by HDF prior to
*            version 3.2
* Input:    filename: name of HDF file
*           ref: the ref number of the SDG
*           ispre32: set to TRUE if the SDG/ref was written by old library;
*                        to FALSE otherwise.
*           len:     length of filename
* Retruns:  0 on success, -1 on failure
* Users:    HDF Fortran programmers
*------------------------------------------------------------------------------*/

      FRETVAL(intf)
#ifdef PROTOTYPE
ndsip32s(_fcd filename, intf *ref, intf *ispre32, intf *len)
#else
ndsip32s(filename, ref, ispre32, len)
    _fcd filename;
    intf *ref, *ispre32, *len;
#endif /* PROTOTYPE */
{ 
    char *cname;
    intf status;
    
    cname = HDf2cstring(filename, (intn) *len);
    status = DFSDpre32sdg(cname, (uint16)*ref, (intn *)ispre32);

    HDfreespace(cname);
    return(status);
}

/*-----------------------------------------------------------------------------
 * Name:    dfsdgetdatastrs
 * Purpose: Call DFSDgetdatastrs to get the data attributes
 * Inputs:  label, unit, format, coordsys: strings to return attributes in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdatastrs
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetdatastrs(_fcd label, _fcd unit, _fcd format, _fcd coordsys)
#else
ndfsdgetdatastrs(label, unit, format, coordsys)
    _fcd label, unit, format, coordsys;
#endif /* PROTOTYPE */
{
    return(DFSDgetdatastrs((char *)_fcdtocp(label), (char *)_fcdtocp(unit),
			 (char *)_fcdtocp(format), (char *)_fcdtocp(coordsys)));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdgetdimstrs
 * Purpose: Call DFSDgetdimstrs to get attributes of a dimension
 * Inputs:  label, unit, format: strings to return attributes in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdimstrs
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetdimstrs(intf *dim, _fcd label, _fcd unit, _fcd format)
#else
ndfsdgetdimstrs(dim, label, unit, format)
    intf *dim;
    _fcd label, unit, format;
#endif /* PROTOTYPE */
{
    intn isndg;
    intn rank, cdim;
    intf ret;

    ret = DFSDIisndg(&isndg);
    if (isndg) 	{
        ret = DFSDIgetrrank(&rank);
    	if (rank < *dim) return FAIL;
        cdim = rank - (intn)*dim + 1;
    }
    else cdim = (intn)*dim;

    return(DFSDgetdimstrs(cdim, (char *)_fcdtocp(label),
			  (char *)_fcdtocp(unit), (char *)_fcdtocp(format)));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdgetdimscale
 * Purpose: Call DFSDgetdimscale to get scale for a dimension
 * Inputs:  dim: dimension to get attributes for
 *          maxsize: size of scale array
 *          scale: array to return scale in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdimscale
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetdimscale(intf *dim, intf *maxsize, VOIDP scale)
#else
ndfsdgetdimscale(dim, maxsize, scale)
    intf *dim;
    intf *maxsize;
    VOID *scale;
#endif /* PROTOTYPE */
{

    intn isndg;
    intn  rank, cdim, ret;

    ret = DFSDIisndg(&isndg);
    if (isndg) 	{
        ret = DFSDIgetrrank(&rank);
    	if (rank < *dim) return FAIL;
        cdim = rank - (intn)*dim + 1;
    }
    else cdim = (intn)*dim;

    return(DFSDgetdimscale(cdim, *maxsize, scale));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdgetrange
 * Purpose: Call DFSDgetrange to get maximum and minimum data values
 * Inputs:  pmax: float to return maximum in
 *          pmin: float to return minimum in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetrange
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetrange(VOIDP pmax, VOIDP pmin)
#else
ndfsdgetrange(pmax, pmin)
    VOID *pmax, *pmin;
#endif /* PROTOTYPE */
{
    return(DFSDgetrange(pmax, pmin));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdsetdims
 * Purpose: Call DFSDsetdims to set dimensions for subsequent SDGs
 * Inputs:  rank: no of dimensions of SDG
 *          dimsizes: array containing dimensions of SDG
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetdims
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdsetdims(intf *rank, intf dimsizes[])
#else
ndfsdsetdims(rank, dimsizes)
    intf *rank;
    intf dimsizes[];
#endif /* PROTOTYPE */
{

    int32 i, *cdims, *p;
    intf ret;

    p = (int32 *)HDgetspace((uint32)((*rank)*sizeof(int32)));
    if (p == NULL) return FAIL;
    cdims = p;
    for (i=1; i <=  *rank ; i++)	{
        *p = dimsizes[*rank - i];
        p++;
    }
   
    ret = DFSDsetdims((intn)*rank, cdims);
    HDfreespace((VOIDP)cdims);
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdsetdimscale
 * Purpose: Call DFSDsetdimscale to set scales for subsequent SDGs
 * Inputs:  dim: dimension to set scale for
 *          dimsize: size of array scale
 *          scale: array of scale values
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetdimscale
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdsetdimscale(intf *dim, intf *dimsize, VOIDP scale)
#else
ndfsdsetdimscale(dim, dimsize, scale)
    intf *dim;
    intf *dimsize;
    VOID *scale;
#endif /* PROTOTYPE */
{
    intn rank, cdim;
    intf ret;

    ret = DFSDIgetwrank(&rank);
    if (rank < *dim) return FAIL;
    cdim = rank - (intn)*dim + 1;

    return(DFSDsetdimscale(cdim, *dimsize, scale));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdsetrange
 * Purpose: Call DFSDsetrange to set max and min values for this SDG
 * Inputs:  max, min: max and min data values
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetrange
 * Remarks: Max and Min are set only for next SDG, reset to NULL after
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdsetrange(VOIDP max, VOIDP min)
#else
ndfsdsetrange(max, min)
    VOID *max, *min;
#endif /* PROTOTYPE */
{
    return(DFSDsetrange(max, min));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdclear
 * Purpose: Call DFSDclear to erase values set for subsequent SDGs
 * Inputs:  none
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDclear
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdclear(void)
#else
ndfsdclear()
#endif /* PROTOTYPE */
{
    return(DFSDclear());
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdsetlengths
 * Purpose: Call DFSDsetlengths to set max lengths of strings
 * Inputs:  maxlen_label, maxlen_unit, maxlen_format, maxlen_coordsys: max lens
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetlengths
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdsetlengths(intf *maxlen_label, intf *maxlen_unit, intf *maxlen_format,
        intf *maxlen_coordsys)
#else
ndfsdsetlengths(maxlen_label, maxlen_unit, maxlen_format, maxlen_coordsys)
    intf *maxlen_label, *maxlen_unit, *maxlen_format, *maxlen_coordsys;
#endif /* PROTOTYPE */
{
    return(DFSDsetlengths((intn)*maxlen_label, (intn)*maxlen_unit,
            (intn)*maxlen_format, (intn)*maxlen_coordsys));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdgetdimlen
 * Purpose: Call DFSDgetdimlen to get actual lengths of strings
 * Inputs:  dim: dimension to get lengths for
 *         llabel, lunit, lformat: integers to return lengths of each string in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdimlen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetdimlen(intf *dim, intf *llabel, intf *lunit, intf *lformat)
#else
ndfsdgetdimlen(dim, llabel, lunit, lformat)
    intf *dim, *llabel, *lunit, *lformat;
#endif /* PROTOTYPE */
{
    intn isndg;
    intn rank, cdim;
    intf ret;
    intn cllabel, clunit, clformat;     /* convert between intf and intn */

    ret = DFSDIisndg(&isndg);
    if (isndg) 	{
        ret = DFSDIgetrrank(&rank);
    	if (rank < *dim) return FAIL;
        cdim = rank - (intn)*dim + 1;
    }
    else cdim = (intn)*dim;

    ret=(intf)DFSDgetdimlen(cdim, &cllabel, &clunit, &clformat);
    if(ret!=FAIL) {     /* if ok, copy the values over */
        *llabel=cllabel;
        *lunit=clunit;
        *lformat=clformat;
      } /* end if */
    return(ret);
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdgetdatalen
 * Purpose: Call DFSDgetdatalen to get actual lengths of strings
 * Inputs:  llabel, lunit, lformat, lcoordsys: integers to return lengths in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdatalen
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetdatalen(intf *llabel, intf *lunit, intf *lformat, intf *lcoordsys)
#else
ndfsdgetdatalen(llabel, lunit, lformat, lcoordsys)
    intf *llabel, *lunit, *lformat, *lcoordsys;
#endif /* PROTOTYPE */
{
    intf ret;
    intn cllabel, clunit, clformat, clcoordsys;

    ret=(intf)DFSDgetdatalen(&cllabel, &clunit, &clformat, &clcoordsys);
    if(ret!=FAIL) {
        *llabel=cllabel;
        *lunit=clunit;
        *lformat=clformat;
        *lcoordsys=clcoordsys;
      } /* end if */
    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    dfsdrestart
 * Purpose: Call DFSDrestart to get SDGs again from the beginning
 * Inputs:  none
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDrestart
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdrestart(void)
#else
ndfsdrestart()
#endif /* PROTOTYPE */
{
    return(DFSDrestart());
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdputslice
 * Purpose: Call DFSDIputslice to write slice to file
 * Inputs:  winst: array of size = rank of data, containing start of slice
 *          windims: array of size rank, containing end of slice
 *          data: array containing slice
 *          dims: dimensions of array data
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIputslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdputslice(intf windims[], VOIDP data, intf dims[])
#else
ndfsdputslice(windims, data, dims)
    intf windims[];
    VOID *data;
    intf dims[];
#endif /* PROTOTYPE */
{
    intn rank, i;
    intf ret;
    int32 *cdims, *cwindims, *p, *wp;

    ret = DFSDIgetwrank(&rank);
    wp = (int32 *)HDgetspace((uint32)((rank)*sizeof(int32)));
    if (wp == NULL) return FAIL;
    cwindims = wp;
    p = (int32 *)HDgetspace((uint32)((rank)*sizeof(int32)));
    if (p == NULL) return FAIL;
    cdims = p;
    for (i=1; i <=  rank ; i++)	{  /* reverse dims & windims */
        *p = dims[rank - i];
        p++;
        *wp = windims[rank - i];
        wp++;
    }
   
    ret = DFSDIputslice(cwindims, data, cdims, 1);
    HDfreespace((VOIDP)cdims);
    HDfreespace((VOIDP)cwindims);
    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    dfsdendslice
 * Purpose: Call DFSDendslice to finish slice writes and write out SDG
 * Inputs:  none
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIendslice
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdendslice(void)
#else
ndfsdendslice()
#endif /* PROTOTYPE */
{
    return(DFSDIendslice(1));
}


/*-----------------------------------------------------------------------------
 * Name:    dfsdsetnt
 * Purpose: Call DFSDsetNT to set number type for subsequent calls to
 *          DFSDputdata, DFSDadddata, DFSDsetdimscales.
 * Inputs:  numbertype
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran programmers
 * Method:  Invokes DFSDsetNT
 * Remarks: 0 specifies default value
 *---------------------------------------------------------------------------*/

        FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdsetnt(intf *numbertype)
#else
ndfsdsetnt(numbertype)
    intf *numbertype;
#endif /* PROTOTYPE */
{
    return(DFSDsetNT(*numbertype));
}

/*-----------------------------------------------------------------------------
 * Name:    dfsdgetnt
 * Purpose: Call DFSDgetNT to get number type for subsequent calls 
 * Inputs:  pnumbertype
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran programmers
 * Method:  Invokes DFSDgetNT
 * Remarks: 0 specifies default value
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndfsdgetnt(intf *pnumbertype)
#else
ndfsdgetnt(pnumbertype)
    intf *pnumbertype;
#endif /* PROTOTYPE */
{
    return(DFSDgetNT((int32 *)pnumbertype));
}

/*-----------------------------------------------------------------------------
 * Name:    dfsdlastref
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
ndfsdlastref(void)
#else
ndfsdlastref()
#endif /* PROTOTYPE */
{
    return(DFSDlastref());
}

/*-----------------------------------------------------------------------------
 * Name:    dsisdis
 * Purpose: For the given dimension, set label, unit, format
 *          This routine needs to be called once for each dimension whose 
 *          values the user wants to set.
 * Inputs:  dim: the dimension that this info applies to
 *          label: label to be used to describe this dimension
 *          unit: units for dimension
 *          format: format to be used in displaying
 *          llabel, lunit, lformat: lengths of corresponding strings
 * Globals: 
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIsetdimstr
 * Method:  
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsisdis(intf *dim, _fcd flabel, _fcd funit, _fcd fformat,
     intf *llabel, intf *lunit, intf *lformat)
#else
ndsisdis(dim, flabel, funit, fformat, llabel, lunit, lformat)
    intf *dim;
    _fcd flabel, funit, fformat;
    intf *llabel, *lunit, *lformat;
#endif /* PROTOTYPE */
{
    char *label  =  HDf2cstring(flabel, (intn)*llabel);
    char *unit   =  HDf2cstring(funit, (intn)*lunit);
    char *format =  HDf2cstring(fformat, (intn)*lformat);
    intf status;
    intn rank, cdim;

    status = DFSDIgetwrank(&rank);

    if (rank < *dim) return FAIL;
    cdim = rank - (intn)*dim + 1;
    
    status = DFSDIsetdimstrs(cdim, label, unit, format);

    HDfreespace(label);
    HDfreespace(unit);
    HDfreespace(format);

    return status;

}

/*-----------------------------------------------------------------------------
 * Name:    dsigdis
 * Purpose: Call DFSDgetdimstrs to get attributes of a dimension
 * Inputs:  label, unit, format: strings to return attributes in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdimstrs
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsigdis(intf *dim, _fcd label, _fcd unit, _fcd format, intf *llabel,
         intf *lunit, intf *lformat)
#else
ndsigdis(dim, label, unit, format, llabel, lunit, lformat)
     intf *dim;
     _fcd label, unit, format;
     intf *llabel, *lunit, *lformat;
#endif /* PROTOTYPE */
{
    char *ilabel, *iunit, *iformat;
    intn rank, cdim;
    intf ret;
    intn isndg, status;

    ret = DFSDIisndg(&isndg);
    if (isndg) 	{
        ret = DFSDIgetrrank(&rank);
    	if (rank < *dim) return FAIL;
        cdim = rank - (intn)*dim + 1;
    }
    else cdim = (intn)*dim;

    iunit = ilabel = iformat = NULL;

    if(*llabel)  ilabel  = (char *) HDgetspace((uint32)*llabel + 1);
    if(*lunit)   iunit   = (char *) HDgetspace((uint32)*lunit + 1);
    if(*lformat) iformat = (char *) HDgetspace((uint32)*lformat + 1);

    status = DFSDgetdimstrs(cdim, ilabel, iunit, iformat);

    HDpackFstring(ilabel,  _fcdtocp(label),  (intn)*llabel);
    HDpackFstring(iunit,   _fcdtocp(unit),   (intn)*lunit);
    HDpackFstring(iformat, _fcdtocp(format), (intn)*lformat);

    if(ilabel)  HDfreespace(ilabel);
    if(iunit)   HDfreespace(iunit);
    if(iformat) HDfreespace(iformat);

    return status;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIsetdatastrs()
 * Purpose: Set label, unit and format for displaying subsequent SDGs
 * Inputs:  label: label to be used to describe data
 *          unit: unit corresponding to data values
 *          format: format to be used in displaying data values
 *          coordsys: type of coordinate system
 * Globals: Writesdg, Ref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran stubs
 * Invokes: DFSDIsetdatastrs
 * Method:  Stores values in global structure Writesdg
 * Remarks: 
 *---------------------------------------------------------------------------*/
    FRETVAL(intf)
#ifdef PROTOTYPE
ndsisdas(_fcd flabel, _fcd funit, _fcd fformat, _fcd fcoordsys, intf *isfortran,
     intf *llabel, intf *lunit, intf *lformat, intf *lcoordsys)
#else
ndsisdas(flabel, funit, fformat, fcoordsys, isfortran, llabel, lunit,
     lformat, lcoordsys)
    _fcd flabel, funit, fformat, fcoordsys;
    intf *isfortran;
    intf *llabel, *lunit, *lformat, *lcoordsys;
#endif /* PROTOTYPE */
{
    char *label    =  HDf2cstring(flabel, (intn)*llabel);
    char *unit     =  HDf2cstring(funit, (intn)*lunit);
    char *format   =  HDf2cstring(fformat, (intn)*lformat);
    char *coordsys =  HDf2cstring(fcoordsys, (intn)*lcoordsys);
    intf status;

    status = DFSDIsetdatastrs(label, unit, format, coordsys);

    HDfreespace(label);
    HDfreespace(unit);
    HDfreespace(format);
    HDfreespace(coordsys);

    return status;
} /* ndsisdas */

/*-----------------------------------------------------------------------------
 * Name:    dsigdas
 * Purpose: Call DFSDgetdatastrs to get the data attributes
 * Inputs:  label, unit, format, coordsys: strings to return attributes in
 * Returns: 0 on success, -1 on failure with	DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdatastrs
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsigdas(_fcd label, _fcd unit, _fcd format, _fcd coordsys, intf *llabel,
        intf *lunit, intf *lformat, intf *lcoord)
#else
ndsigdas(label, unit, format, coordsys, llabel, lunit, lformat, lcoord)
     _fcd label, unit, format, coordsys;
     intf *llabel, *lunit, *lformat, *lcoord;
#endif /* PROTOTYPE */
{
    char *ilabel, *iunit, *iformat, *icoord;
    intf status;

    iunit = ilabel = iformat = icoord = NULL;

    if(*llabel)  ilabel  = (char *) HDgetspace((uint32)*llabel + 1);
    if(*lunit)   iunit   = (char *) HDgetspace((uint32)*lunit + 1);
    if(*lformat) iformat = (char *) HDgetspace((uint32)*lformat + 1);
    if(*lcoord)  icoord  = (char *) HDgetspace((uint32)*lcoord + 1);

    status = DFSDgetdatastrs(ilabel, iunit, iformat, icoord);
    
    HDpackFstring(ilabel,  _fcdtocp(label),    (intn)*llabel);
    HDpackFstring(iunit,   _fcdtocp(unit),     (intn)*lunit);
    HDpackFstring(iformat, _fcdtocp(format),   (intn)*lformat);
    HDpackFstring(icoord,  _fcdtocp(coordsys), (intn)*lcoord);

    if(ilabel)  HDfreespace(ilabel);
    if(iunit)   HDfreespace(iunit);
    if(iformat) HDfreespace(iformat);
    if(icoord)  HDfreespace(icoord);

    return status;

}

/*-----------------------------------------------------------------------------
 * Name:    dsscal
 * Purpose: Call DFSDsetcal to set calibration data
 * Inputs:  cal, cal_err   : calibration and error
 *          ioff, ioff_err : offset and error
 *          cal_type       : after calibration NT
 * Returns: 0 on success, -1 on failure
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdatastrs
 *---------------------------------------------------------------------------*/
    FRETVAL(intf)
#ifdef PROTOTYPE
ndsscal(float64 *cal, float64 *cal_err, float64 *ioff, float64 *ioff_err,
         intf * cal_type)
#else
ndsscal(cal, cal_err, ioff, ioff_err, cal_type)
     float64 *cal, *cal_err, *ioff, *ioff_err;
     int32   *cal_type;
#endif /* PROTOTYPE */
{
    return DFSDsetcal(*cal, *cal_err, *ioff, *ioff_err, *cal_type);
} /* ndsscal */

/*-----------------------------------------------------------------------------
 * Name:    dsgcal
 * Purpose: Call DFSDgetcal to get calibration data
 * Inputs:  cal, cal_err   : calibration and error
 *          ioff, ioff_err : offset and error
 *          cal_type       : after calibration NT
 * Returns: 0 on success, -1 on failure
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetdatastrs
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsgcal(float64 *cal, float64 *cal_err, float64 *ioff, float64 *ioff_err,
         intf * cal_type)
#else
ndsgcal(cal, cal_err, ioff, ioff_err, cal_type)
     float64 *cal, *cal_err, *ioff, *ioff_err;
     intf   *cal_type;
#endif /* PROTOTYPE */
{
    return DFSDgetcal(cal, cal_err, ioff, ioff_err, (int32 *)cal_type);
} /* ndsgcal */

/*-----------------------------------------------------------------------------
 * Name:    dsiwref
 * Purpose: Call DFSDwriteref to set up next ref to write
 * Inputs:  filename: name of HDF file
 *          fnlen: length of filename
 *          ref: next ref to read
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes:
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsiwref(_fcd filename, intf *fnlen, intf *ref)
#else
ndsiwref(filename, fnlen, ref)
    _fcd filename;
    intf *fnlen;
    intf *ref;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    ret = DFSDwriteref(fn, (uint16) *ref);
    HDfreespace(fn);
    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    dfsdsfill
 * Purpose: Call DFSDsetfillvalue to set fill value for SDS
 * Inputs:  fill_value: fill value for SDS
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDsetfillvalue
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndssfill(VOIDP fill_value)
#else
ndssfill(fill_value)
    VOID *fill_value;
#endif /* PROTOTYPE */
{
    return DFSDsetfillvalue(fill_value);
}

/*-----------------------------------------------------------------------------
 * Name:    dsgfill
 * Purpose: Call DFSDgetfillvalue to get fill value for SDS
 * Inputs:  fill_value: fill value of SDS
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDgetfillvalue
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsgfill(VOIDP fill_value)
#else
ndsgfill(fill_value)
    VOID *fill_value;
#endif /* PROTOTYPE */
{
    return DFSDgetfillvalue(fill_value);
}

/*-----------------------------------------------------------------------------
 * Name:    dsisslab
 * Purpose: Set up slab writes to SDS
 * Inputs:  filename: file to which this applies
 *          fnlen: file name length
 * Returns: 0 on success, FAIL on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFSDstartslab
 * Remarks:
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndsisslab(_fcd filename, intf *fnlen )
#else
ndsisslab(filename, fnlen)
    _fcd filename;
    intf *fnlen;
#endif /* PROTOTYPE */
{
    char *fn;
    intf ret;

    fn = HDf2cstring(filename, (intn)*fnlen);
    if (fn == NULL)
      return FAIL;
    ret = DFSDstartslab(fn);
    HDfreespace(fn);
    return ret;
}

/*-----------------------------------------------------------------------------
 * Name:    dswslab
 * Purpose: Call DFSDwriteslab to write slab to file
 * Inputs:  start: array of size = rank of data, containing start of slab
 *          stride: array for subsampling
 *          count: array of size rank, containing size of slab
 *          data: array of data to be written
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF Fortran programmers
 * Invokes: DFSDIgetwrank, HDgetspace, HDfreespace, HDf2cstring, DFSDwriteslab
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndswslab(intf start[], intf stride[],
         intf count[], VOIDP data)
#else
ndswslab(start, stride, count, data)
    intf start[];
    intf stride[];
    intf count[];
    VOID  *data;
#endif /* PROTOTYPE */
{
    int32 *lstart, *lstride, *lcount, *aptr, *bptr, *cptr;
    intn i, rank;
    intf ret;

    /*
    ** Lets reverse the order for the arrays since we
    ** are going from fortran to C
    */
    ret = DFSDIgetwrank(&rank);
    if (ret == FAIL)
      return FAIL;

    aptr = (int32 *)HDgetspace((uint32)(3*rank*sizeof(int32)));
    if (aptr == NULL)
      return FAIL;

    lstart  = aptr;
    lstride = bptr = aptr + rank;
    lcount  = cptr = bptr + rank;

    for (i = 1; i <=  rank ; i++)
      {  /* reverse start, stride & count */
        *aptr = start[rank - i];
        aptr++;
        *bptr = stride[rank - i];
        bptr++;
        *cptr = count[rank - i];
        cptr++;
      }

    ret = DFSDwriteslab(lstart, lstride, lcount, data);
    HDfreespace((VOIDP)lstart);

    return ret;
}

/*-----------------------------------------------------------------------------
 * Name:    dseslab
 * Purpose: End slab writes to SDS, Write out NDG
 * Inputs:  None
 * Returns: 0 on success, FAIL on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFSDendslab
 * Remarks:
 *---------------------------------------------------------------------------*/

    FRETVAL(intf)
#ifdef PROTOTYPE
ndseslab()
#else
ndseslab()
#endif /* PROTOTYPE */
{
    return DFSDendslab();
}

