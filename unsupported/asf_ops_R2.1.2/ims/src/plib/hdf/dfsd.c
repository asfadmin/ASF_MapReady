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
 File:  dfsd.c

 Purpose:
    Routines for input and output of numeric data group

 Invokes:
    df.c dfgroup.c dfkit.c dfi.h df.h dfsd.h

 Public functions:
    DFSDgetdims - get rank and dim sizes
    DFSDgetdatastrs - get label, unit, format and coord system of data
    DFSDgetdimstrs - get label, unit and format for a dimension
    DFSDgetdatalen - 
    DFSDgetdimlen -
    DFSDgetdimscale - get scale for a dimension
    DFSDgetrange - get max and min of data
    DFSDgetdata - get data values
    DFSDgetNT - get file number type for reading
    DFSDpre32sdg - tests, without calling DFSDsdginfo,  whether or
             not the SDG/ref written with 3.1
    DFSDsetlengths - set lengths of label, unit, format strings on gets
    DFSDsetdims - set rank and dim sizes
    DFSDsetdatastrs - set data label, unit, format and coord system
    DFSDsetdimstrs - set dim labels, units and formats
    DFSDsetdimscale - set scale for a dimension
    DFSDsetrange - set max and min of data
    DFSDsetorder - set array order to C or FORTRAN order
    DFSDsetNT - set number type to be written out
    DFSDputdata - output data, data info, and display info
    DFSDrestart - forget info about last file accessed - restart from beginning
    DFSDndatasets - return number of SDGs in file
    DFSDclear - forget all info set
    DFSDlastref - get reference number of last SDG read or written
    DFSDgetslice - get part of the data, specified as a slice
    DFSDstartslice - set up to write SD
    DFSDputslice - write specified number of data items to file
    DFSDendslice - end of series of writes, write out SDG
    DFSDwriteref - set reference number to be used in next SDS write slab
    DFSDsetfillvalue - set fill value to be used in next SDS written
    DFSDgetfillvalue - return fill value from SDS that is about to be read
    DFSDstartslab - set up to write slabs
    DFSDwriteslab - write hyperslab of values
    DFSDendslab   - end of series of hyperslab writes

Lower level functions:
    DFSDgetsdg - read SDG into struct
    DFSDputsdg - write SDG to file

Private functions:
    DFSDIopen - open or reopen file
    DFSDIsdginfo - find next sdg in file
    DFSDIisndg - was currently read sdg written by HDF3.2
    DFSDIrefresh -- get info of next sdg if necessary
    DFSDIgetrrank - get rank of the currently read sdg
    DFSDIgetwrank - get rank of the sdg to be written
    DFSDIclear - clear sdg data structure of all info
    DFSDIgetdata - read data from file
    DFSDIputdata - write data to file
    DFSDIgetslice - get slice
    DFSDIputslice - put slice
    DFSDIendslice -
    DFSDIsetnsdg_t - set up nsdg table
    DFSDInextnsdg - get next nsdg from nsdg table
    DFSDIgetndg - read NDG into struct
    DFSDIputndg - write NDG to file

Fortran stub functions:
    dsisdas - set data label, unit, format and coord system
    dsisdis - set dim labels, units and formats

 Remarks: 
    These functions will be copied into dfsd.c after debugging.
    This version assumes that all the values are floating point.
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "herr.h"
#include "dfsd.h"
#include "hfile.h"

/* MMM: make this definition correct and move to hfile.h, or wherever. */
#define DF_NOFILE 0

#define LABEL   0
#define UNIT    1
#define FORMAT  2
#define COORDSYS 3

#define NFGSDG_TYPE_SDG	0	/* a pure SDG  */
#define NFGSDG_TYPE_NFG	1	/* a pure NDG  */
#define NFGSDG_TYPE_SDGNDG 2	/* an SDG in NDG */

/* Init NSDG table header      */
PRIVATE DFnsdg_t_hdr *nsdghdr = NULL;

/* initialize aid to -1 and numbertype to DFNT_NONE.   S. Xu    */
PRIVATE DFSsdg Readsdg =     /* struct for reading */
{     {(uint16)0, (uint16)0}, (intn)0, NULL, NULL, { NULL, NULL, NULL },
      { NULL, NULL, NULL }, NULL, {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0},
      (int32)DFNT_NONE, DFNTF_NONE, (int32)-1, (int32)0, (int32)0,
      (float64) 1.0, (float64) 0.0, (float64) 0.0, (float64) 0.0,
      (int32) -1, 0, 0
};

PRIVATE DFSsdg Writesdg =    /* struct for writing */
{     {(uint16)0, (uint16)0}, (intn)0, NULL, NULL, { NULL, NULL, NULL },
      { NULL, NULL, NULL }, NULL, {0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0},
      (int32)DFNT_NONE, DFNTF_NONE, (int32)-1, (int32)0, (int32)0,
      (float64) 1.0, (float64) 0.0, (float64) 0.0, (float64) 0.0,
      (int32) -1, 0, 0
};

PRIVATE uint16  Writeref=0;	/* ref of next SDG/NDG to write to file */
PRIVATE intn Newdata=(-1);	/* Values in Readsdg fresh? */
				/* -1 : no descriptor read */
				/* 1 : descriptor read */
PRIVATE intn Nextsdg = 1;	/* Signal if DFSDgetdata should get the */
				/* next SDG/NDG */
PRIVATE int32 Sfile_id=DF_NOFILE;  /* pointer to file for slice writes */
PRIVATE int32  *Sddims;		/*dims written so far in slice write */

PRIVATE struct {	/* Indicators of status (s) of info:    */
    intn dims;		/* s = -1: there is no info in this category */
    intn nt;             /* s = 0: info was set, but not yet written */
    intn coordsys;       /* s > 0: info was set and written with ref no.s*/
    intn luf[3];
    intn scales;
    intn maxmin;
    intn transpose;	/* It should be taken out!!!		!!!  */
    intn cal;
    intn fill_value;
    intn new_ndg;
} Ref = {  -1, -1, -1, { -1, -1, -1 }, -1, -1 , -1, -1, -1, -1};
    
PRIVATE intn Maxstrlen[4]  = { DFS_MAXLEN, DFS_MAXLEN, DFS_MAXLEN, DFS_MAXLEN };
PRIVATE intn Ismaxmin      = 0;	/* is there a max/min value on read?  */
PRIVATE intn FileTranspose = 0;	/* is the data in column major order? */
PRIVATE intn Fortorder     = 0;	/* should data be written col major?  */
PRIVATE intn IsCal         = 0;   /* has calibration info been set?     */

/* In ver. 3.2 numbertype and file number format (subclass) are included  */
/* in DFSsdg, and  fileNTsize is local to functions . 		*/ 
/* static int fileNT=DFNTF_IEEE,	 default: all IEEE       */
/*           fileNTsize=4,			 */
/*           outNT=DFNTF_IEEE,		 default output: IEEE */
/*           outNTsize=4, 			 */
/*           userNT=DFNTF_IEEE ;	 default */

PRIVATE uint16 Readref = 0;      /* ref of next SDG/NDG to be read? */
PRIVATE char Lastfile[DF_MAXFNLEN] = "";	/* last file opened */
PRIVATE uint16 Lastref = 0;     /* Last ref to be read/written? */
PRIVATE DFdi lastnsdg;		/* last read nsdg in nsdg_t */

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdims
 * Purpose: Get dimensions of data in next SDG
 * Inputs:  filename: name of HDF file to use
 *          prank: pointer to integer for returning rank (no of dimensions)
 *          sizes: array of integers for returning size of each dimension
 *          maxrank: size of array for returning dimensions
 * Returns: 0 on success, FAIL on failure with ERROR set
 * Outputs: rank in prank, size of each dimension in sizes
 *          If rank > maxrank, rank is set, and -1 is returned
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIopen, HERROR, Hclose, DFSDIsdginfo
 * Method:  Opens file, calls DFSDIsdginfo to get SDG, copies rank etc, closes
 *          file, returns
 * Remarks: Always sequences to next SDG in file
 *          User specifies maxrank, and allocates sizes as an array of integers
 *          with dimension maxrank
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdims(char *filename, intn *prank, int32 sizes[], intn maxrank)
#else
intn DFSDgetdims(filename, prank, sizes, maxrank)
     char *filename;
     intn *prank;
     int32 sizes[];
     intn maxrank;
#endif /* PROTOTYPE */
{
    intn i;
    int32 file_id;
    char *FUNC="DFSDgetdims";

    HEclear();

    if (!prank)                         /* check if ptr is valid */
        HRETURN_ERROR(DFE_BADPTR, FAIL);

    file_id = DFSDIopen(filename, DFACC_READ); /* open/reopen file */
    if (file_id == FAIL) 
        return FAIL;

    if (DFSDIsdginfo(file_id) < 0) 
      { /* reads next SDG from file */
        Hclose(file_id); 
        return FAIL;	/* on error, close file and return */
      }

    *prank = Readsdg.rank;	/* copy rank, dimensions */
    if (maxrank < *prank) 	/* if not all dimensions copied */
        HRETURN_ERROR(DFE_NOTENOUGH, FAIL);

    for (i = 0; i < *prank; i++)
        sizes[i] = Readsdg.dimsizes[i];
    Nextsdg = 0;

    return (Hclose(file_id));
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdatastrs
 * Purpose: Get information about data: label, units, format
 * Inputs:  label: string to return label in, length Maxstrlen[LABEL]
 *          unit: string to return unit in, length Maxstrlen[UNIT]
 *          format: string to return format in, length Maxstrlen[FORMAT]
 *          coordsys: string to return coord system, length Maxstrlen[COORDSYS]
 * Returns: 0 on success, FAIL on failure with ERROR set
 * Outputs: label, unit, format, coord system in the appropriate arguments
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  get values from struct Readsdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdatastrs(char *label, char *unit, char *format, char *coordsys)
#else
intn DFSDgetdatastrs(label, unit, format, coordsys)
     char *label;
     char *unit;
     char *format;
     char *coordsys;
#endif /* PROTOTYPE */
{
    int32 luf;
    char *lufp;
    char *FUNC="DFSDgetdatastrs";

    HEclear();
    
    if (Newdata < 0) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

/* NOTE: Once DFSDsetdatastrs is changed to always write all three (label,
         unit and format) whenever it is called, this routine should be
         changed so that it returns all three, if any exist.  This means
         that it also should be changed to return -1 if none exist.
         (Currently it returns FAIL only if the SDS doesn't exist.)
*/

    /* copy label, unit, format */
    for (luf = LABEL; luf <= FORMAT; luf++) 
      {
        lufp = (luf == LABEL) ? label : (luf == UNIT) ? unit : format;
        if (lufp)
          {
            if (Readsdg.dataluf[luf])
                HIstrncpy(lufp, Readsdg.dataluf[luf], Maxstrlen[luf]);
          }
      }
    /* copy coordsys */
    if (coordsys)
      {
        if (Readsdg.coordsys)
          HIstrncpy(coordsys, Readsdg.coordsys, Maxstrlen[COORDSYS]);
        else 
          coordsys[0] = '\0';
      }

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdimstrs
 * Purpose: Get information about a dimension: label, units, format
 * Inputs:  dim: no of dimension to get information about
 *          label: string to return label in, max length Maxstrlen[LABEL]
 *          unit: string to return unit in, max length Maxstrlen[UNIT]
 *          format: string to return format in, max length Maxstrlen[FORMAT]
 * Returns: 0 on success, FAIL on failure with ERROR set
 * Outputs: label, unit, format in the appropriate arguments
 *          NULL string if no value for the arguments
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  get values from struct Readsdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdimstrs(int dim, char *label, char *unit, char *format)
#else
intn DFSDgetdimstrs(dim, label, unit, format)
     intn dim;
     char *label;
     char *unit;
     char *format;
#endif /* PROTOTYPE */
{
    intn luf;
    intn rdim;
    char *lufp;
    char *FUNC="DFSDgetdimstrs";

    HEclear();
    
    if (Newdata < 0) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

/* NOTE: Once DFSDsetdimstrs is changed to always write all three (label,
         unit and format) whenever it is called, this routine should be
         changed so that it returns all three, if any exist.  This means
         that it also should be changed to return -1 if none exist. 
         (Currently it returns FAIL only if the SDS doesn't exist.)
*/

    rdim = dim - 1;		/* translate dim to zero origin */
    if ((rdim >= Readsdg.rank) || (rdim < 0)) 
        HRETURN_ERROR(DFE_BADDIM, FAIL);

    /* copy labels etc */
    for (luf = LABEL; luf <= FORMAT; luf++) 
      {
        lufp = (luf == LABEL) ? label : (luf == UNIT) ? unit : format;
        if (lufp) 
          {
            if (!Readsdg.dimluf) 
              { /* no labels etc */
                *lufp = '\0';
                continue;
              }
            if (Readsdg.dimluf[luf])
                HIstrncpy(lufp, Readsdg.dimluf[luf][rdim], Maxstrlen[luf]);
          }
      }

    return SUCCEED;
}
                
/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdatalen()
 * Purpose: Get actual length of label, unit, format, coordsys strings
 *          Called from FORTRAN
 * Inputs:  llabel, lunit, lformat, lcoordsys - for returning lengths
 * Globals: Readsdg
 * Returns: SUCCEED on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  get lengths from Readsdg
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdatalen(intn *llabel, intn *lunit, intn *lformat, intn *lcoordsys)
#else
intn DFSDgetdatalen(llabel, lunit, lformat, lcoordsys)
     intn *llabel;
     intn *lunit;
     intn *lformat;
     intn *lcoordsys;
#endif /* PROTOTYPE */
{
    char *FUNC="DFSDgetdatalen";

    HEclear();

    if (Newdata < 0)  
        HRETURN_ERROR(DFE_BADCALL, FAIL); 

    *llabel    =  Readsdg.dataluf[LABEL] ? 
                  HDstrlen(Readsdg.dataluf[LABEL]) : 0;
    *lunit     =  Readsdg.dataluf[UNIT] ? 
                  HDstrlen(Readsdg.dataluf[UNIT]) : 0;
    *lformat   =  Readsdg.dataluf[FORMAT] ? 
                  HDstrlen(Readsdg.dataluf[FORMAT]) : 0;
    *lcoordsys =  Readsdg.coordsys ? 
                  HDstrlen(Readsdg.coordsys) : 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdimlen()
 * Purpose: Get actual length of label, unit, format strings
 *          Called from FORTRAN
 * Inputs:  dim. llabel, lunit, lformat - for returning lengths
 * Globals: Readsdg
 * Returns: SUCCEED on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  get lengths from Readsdg
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdimlen(intn dim, intn *llabel, intn *lunit, intn *lformat)
#else
intn DFSDgetdimlen(dim, llabel, lunit, lformat)
     intn dim;
     intn *llabel;
     intn *lunit;
     intn *lformat;
#endif /* PROTOTYPE */
{
    char *FUNC="DFSDgetdimlen";

    HEclear();

    if (Newdata < 0)  
        HRETURN_ERROR(DFE_BADCALL, FAIL); 

    if (dim > Readsdg.rank)  
        HRETURN_ERROR(DFE_BADDIM, FAIL); 

    *llabel  =  Readsdg.dimluf[LABEL][dim-1] ?
                HDstrlen(Readsdg.dimluf[LABEL][dim-1]) : 0;
    *lunit   =  Readsdg.dimluf[UNIT][dim-1] ?
                HDstrlen(Readsdg.dimluf[UNIT][dim-1]) : 0;
    *lformat =  Readsdg.dimluf[FORMAT][dim-1] ?
                HDstrlen(Readsdg.dimluf[FORMAT][dim-1]) : 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdimscale
 * Purpose: Get dimension scale
 * Inputs:  dim: no of dimension to get scale for
 *          size: size of scale array
 *          scale: array to return scale in
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: scale if present, else -1
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  get values from struct Readsdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdimscale(intn dim, int32 maxsize, VOIDP scale)
#else
intn DFSDgetdimscale(dim, maxsize, scale)
     int dim;
     int32 maxsize;
     VOID *scale;
#endif /* PROTOTYPE */
{
    int32 dimsize;
    int32 numtype;
    int32 localNTsize;
    intn rdim;
    uint8 *p1, *p2;
    char *FUNC="DFSDgetdimscale";

    HEclear();
    
    if (Newdata < 0) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    rdim = dim - 1;	/* translate dim to zero origin */
    if ((rdim >= Readsdg.rank) || (rdim < 0)) 
        HRETURN_ERROR(DFE_BADDIM, FAIL);

    if (maxsize < Readsdg.dimsizes[rdim]) 
        HRETURN_ERROR(DFE_NOSPACE, FAIL);

    if (!scale) 
        HRETURN_ERROR(DFE_BADPTR, FAIL);

    if (!Readsdg.dimscales || !Readsdg.dimscales[rdim])  /* no scale */
        HRETURN_ERROR(DFE_NOVALS, FAIL);

    /* get number type and copy data from Readsdg to scale */
    if (Readsdg.numbertype == DFNT_NONE)
        Readsdg.numbertype = DFNT_FLOAT32;

    numtype     = Readsdg.numbertype;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));
    dimsize     = localNTsize*Readsdg.dimsizes[rdim]; /* in bytes  */

    p1 = (uint8 *)scale;
    p2 = (uint8 *)(Readsdg.dimscales[rdim]);
    HDmemcpy(p1, p2, dimsize);

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetrange()
 * Purpose: Get maximum and minimum data values
 * Inputs:  pmax: pointer to int8 to return maximum value in
 *          pmin: pointer to int8 to return minimum value in
 * Globals: Ismaxmin
 * Returns: 0 on success, -1 if no maxmin values or if error, with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Retrieves values from Readsdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFSDgetrange(VOIDP pmax, VOIDP pmin)
#else
int DFSDgetrange(pmax, pmin)
     VOID *pmax;
     VOID *pmin;
#endif /* PROTOTYPE */
{
    int32 numtype;
    int32 localNTsize;
    uint8 *p1, *p2;
    char *FUNC="DFSDgetrange";

    HEclear();
    
    if (Newdata < 0) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    /* get number type and copy data  */
    if (Readsdg.numbertype == DFNT_NONE)
        Readsdg.numbertype = DFNT_FLOAT32;
    numtype     = Readsdg.numbertype;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));

    if (Ismaxmin)
      {
        p1 = (uint8 *)pmax;
        p2 = (uint8 *)&(Readsdg.max_min[0]);
        HDmemcpy(p1, p2, localNTsize);
        p1 = (uint8 *)pmin;
        p2 = &(Readsdg.max_min[localNTsize]);
        HDmemcpy(p1, p2, localNTsize);
        return SUCCEED;
      } 
    else 
        HRETURN_ERROR(DFE_NOVALS, FAIL);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetdata
 * Purpose: Get data from SDG.  Will sequence to next SDG if DFSDgetdims not
 *          called.
 * Inputs:  filename: name of HDF file to use
 *          rank: no of dimensions of array "data"
 *          maxsizes: actual dimensions of array "data"
 *          data: data for returning scientific data
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: actual scientific data in array
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIgetdata
 * Method:  call DFSDIgetdata
 * Remarks: maxsizes may be larger than actual size.  In that event, the actual
 *          data may not be contiguous in the array "data"
 *          User sets maxsizes before call.
 *          It is not necessary to call DFSDgetdata first if the dimensions
 *          are correct
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetdata(char *filename, intn rank, int32 maxsizes[], VOIDP data)
#else
intn DFSDgetdata(filename, rank, maxsizes, data)
     char *filename;
     intn rank;
     int32 maxsizes[];
     VOID *data;
#endif /* PROTOTYPE */
{
    return (DFSDIgetdata(filename, rank, maxsizes, data, 0));    /* 0 == C */
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetlengths()
 * Purpose: Set maximum length for label, unit, format, coordsys strings
 * Inputs:  maxlen_label, maxlen_format, maxlen_unit, maxlen_coordsys:
 *              maximum length of each string.
 * Globals: Maxstrlen
 * Returns: 0 on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Stores values in global Maxstrlen
 * Remarks: The get routines assume the array passed in by user is of this len
 *          If this routine is not called, the lengths default to DFS_MAXLEN
 *          The length includes the string terminator NULL byte
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDsetlengths(intn maxlen_label, intn maxlen_unit, intn maxlen_format,
		    intn maxlen_coordsys)
#else
intn DFSDsetlengths(maxlen_label, maxlen_unit, maxlen_format, maxlen_coordsys)
     intn maxlen_label;
     intn maxlen_unit;
     intn maxlen_format;
     intn maxlen_coordsys;
#endif /* PROTOTYPE */
{
    if (maxlen_label > 0) 
      Maxstrlen[LABEL] = maxlen_label;
    if (maxlen_unit > 0) 
      Maxstrlen[UNIT] = maxlen_unit;
    if (maxlen_format > 0) 
      Maxstrlen[FORMAT] = maxlen_format;
    if (maxlen_coordsys > 0) 
      Maxstrlen[COORDSYS] = maxlen_coordsys;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetdims()
 * Purpose: Set rank and sizes for subsequent SDGs
 * Inputs:  rank: rank of array that holds the raw data
 *          dimsizes: sizes of all of the dimensions
 * Globals: Writesdg, Ref
 * Returns: 0 on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDclear
 * Method:  Stores values in global structure Writesdg
 * Remarks: If dimensions change, all previous "set"s are cleared
 *          This routine must be called before DFSDsetdimstrs and
 *          DFSDsetdimscales.  It need not be called if these routines are
 *          not called, and the correct dimensions are supplied to DFSDputdata
 *          or DFSDadddata
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDsetdims(intn rank, int32 dimsizes[])
#else
intn DFSDsetdims(rank, dimsizes)
     intn rank;
     int32  dimsizes[];
#endif /* PROTOTYPE */
{
    intn i;
    char *FUNC="DFSDsetdims";

    HEclear();

    if (Sfile_id != DF_NOFILE)  
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    if (Writesdg.rank == rank)	/* check if dimensions same */
      {
        if (Writesdg.dimsizes) 
          {
            for (i = 0; i < rank; i++)
              {
                if (Writesdg.dimsizes[i] != dimsizes[i]) 
                  break;
              }
            if (i == rank) 
              return SUCCEED; /* Dimensions same as before */
          }   
      }
    
    /* forget all attributes set previously */
    if (DFSDIclear((DFSsdg *)&Writesdg) < 0) 
      return FAIL;

    /* allocate dimensions */
    Writesdg.dimsizes = (int32 *) HDgetspace((uint32)(rank*sizeof(int32)));
    if (Writesdg.dimsizes == NULL) 
      return FAIL;

    /* copy dimensions */
    Writesdg.rank = rank;
    for (i = 0; i < rank; i++)
        Writesdg.dimsizes[i] = dimsizes[i];

    /* Note dimensions modified */
    Ref.dims = 0;

    /* 
    *  Added side effect, allows creation of new "ref" whenever called 
    *  before DFSDwriteslab().
    */
    Ref.new_ndg = 0;
    Writeref    = 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetdatastrs()
 * Purpose: Set label, unit and format for displaying subsequent SDGs
 * Inputs:  label: label to be used to describe data
 *          unit: unit corresponding to data values
 *          format: format to be used in displaying data values
 *          coordsys: type of coordinate system
 * Globals: Writesdg, Ref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIsetdatastrs
 * Method:  
 * Remarks: 
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDsetdatastrs(char *label, char *unit, char *format, char *coordsys)
#else
intn DFSDsetdatastrs(label, unit, format, coordsys)
     char *label;
     char *unit;
     char *format;
     char *coordsys;
#endif /* PROTOTYPE */
{
    return (DFSDIsetdatastrs(label, unit, format, coordsys));
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
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Stores values in global structure Writesdg
 * Remarks: should we validate coordsys? proposed strings: "cartesian",
 *          "polar" (="spherical") and "cylindrical".  Do "spherical" and
 *          "cylindrical" make sense for 2D?
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIsetdatastrs(char *label, char *unit, char *format, char *coordsys)
#else
intn DFSDIsetdatastrs(label, unit, format, coordsys)
     char *label;
     char *unit;
     char *format;
     char *coordsys;
#endif /* PROTOTYPE */
{
    intn luf;			/* takes values LABEL, UNIT, FORMAT */
				/* in succession */
    char *lufp;			/* points to label, unit, format */
				/* in succession */

/* NOTE: The following code should be changed to write all three, even if
         one or more is an empty string.  Then, when DFSDgetdatastrs is called
         values will be returned for all three also, even though some might
         be empty strings.
*/

    for (luf = LABEL; luf <= FORMAT; luf++)
      {
	/* set lufp to point to label etc. as apppropriate */
        lufp = (luf == LABEL) ? label : (luf == UNIT) ? unit : format;

	/* free space if allocated */
        Writesdg.dataluf[luf] = HDfreespace((VOIDP)Writesdg.dataluf[luf]);

	/* copy string */
        if (lufp) 
          {
            Writesdg.dataluf[luf] = (char *)HDstrdup(lufp);
            if (Writesdg.dataluf[luf] == NULL) 
              return FAIL;
          }
      }

    Writesdg.coordsys = HDfreespace((VOIDP)Writesdg.coordsys);

    if (coordsys) 
      {
        Writesdg.coordsys = (char *)HDstrdup(coordsys);
        if (Writesdg.coordsys == NULL) 
          return FAIL;
      }

    /* indicate that label, unit, format and coordsys info modified */
    Ref.luf[LABEL] = Ref.luf[UNIT] = Ref.luf[FORMAT] = Ref.coordsys = 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetdimstrs()
 * Purpose: For the given dimension, set label, unit, format
 *          This routine needs to be called once for each dimension whose 
 *          values the user wants to set.
 * Inputs:  dim: the dimension that this info applies to
 *          label: label to be used to describe this dimension
 *          unit: units for dimension
 *          format: format to be used in displaying
 * Globals: 
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIsetdimstrs
 * Method:  
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn DFSDsetdimstrs(intn dim, char *label, char *unit, char *format)
#else
intn DFSDsetdimstrs(dim, label, unit, format)
     intn dim;
     char *label;
     char *unit;
     char *format;
#endif /* PROTOTYPE */
{
    return (DFSDIsetdimstrs(dim, label, unit, format));
} /* DFSDsetdimstrs */

/*-----------------------------------------------------------------------------
 * Name:    DFSDIsetdimstrs()
 * Purpose: For the given dimension, set label, unit, format
 *          This routine needs to be called once for each dimension whose 
 *          values the user wants to set.
 * Inputs:  dim: the dimension that this info applies to
 *          label: label to be used to describe this dimension
 *          unit: units for dimension
 *          format: format to be used in displaying
 * Globals: Writesdg, Ref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Stores values in global structure Writesdg
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIsetdimstrs(intn dim, char *label, char *unit, char *format)
#else
intn DFSDIsetdimstrs(dim, label, unit, format)
     intn dim;
     char *label;
     char *unit;
     char *format;
#endif /* PROTOTYPE */
{
    intn i;
    intn rdim;
    intn luflen;
    intn luf;			/* takes values LABEL, UNIT, FORMAT */
				/* in succession */
    char *lufp;			/* points to label, unit, format */
				/* in succession */
    char *FUNC="DFSDsetdimstrs";
    HEclear();

    /* translate from 1 to 0 origin */
    rdim = dim - 1;

    if ((rdim >= Writesdg.rank) || (rdim < 0)) 
        HRETURN_ERROR(DFE_BADDIM, FAIL);

    for (luf = LABEL; luf <= FORMAT; luf++) 
      {
	/* set lufp to point to label etc. as apppropriate */
        lufp   = (luf == LABEL) ? label : (luf == UNIT) ? unit  : format;
        luflen = HDstrlen(lufp);

	/* allocate space if necessary */
        if (!Writesdg.dimluf[luf]) 
          {
            Writesdg.dimluf[luf] =
                (char **) HDgetspace((uint32) Writesdg.rank * sizeof(char *));
            if (Writesdg.dimluf[luf] == NULL) 
              return FAIL;
            for (i = 0; i < Writesdg.rank; i++) /* set allocated pointers to NULL*/
                Writesdg.dimluf[luf][i] = NULL;
          }

	/* free string space if allocated */
        Writesdg.dimluf[luf][rdim] = HDfreespace((VOIDP)Writesdg.dimluf[luf][rdim]);

/* NOTE: The following code should be changed to write all three, even if
         one or more is an empty string.  Then, when DFSDgetdimstrs is called
         values will be returned for all three also, even though some might
         be empty strings.
*/

	/* copy string */
        if (lufp) 
          {
            Writesdg.dimluf[luf][rdim] = (char *)HDstrdup(lufp);
            if (Writesdg.dimluf[luf][rdim] == NULL) 
              return FAIL;
          }
      }
    /* Indicate that this info has not been written to file */
    Ref.luf[LABEL] = Ref.luf[UNIT] = Ref.luf[FORMAT] = 0;

    return SUCCEED;
} 

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetdimscale()
 * Purpose: For the given dimension, set scale values
 *          This routine needs to be called once for each dimension whose 
 *          values the user wants to set.
 * Inputs:  dim: the dimension that this info applies to
 *          dimsize: number of points in the scale
 *          scale: array of numbers that will make up the scale
 * Globals: Writesdg, Ref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Stores values in global structure Writesdg
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDsetdimscale(intn dim, int32 dimsize, VOIDP scale)
#else
intn DFSDsetdimscale(dim, dimsize, scale)
     intn dim;
     int32   dimsize;
     VOID *scale;
#endif /* PROTOTYPE */
{
    int32 i;
    intn rdim;
    int32 numtype;
    int32 bytesize;
    int32 localNTsize;
    uint8 *p1, *p2;
    char *FUNC="DFSDsetdimscale";

    HEclear();
    rdim = dim - 1;		/* translate from 1 to 0 origin */

    if (!Writesdg.dimsizes) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    if (Writesdg.numbertype == DFNT_NONE)
      {
	if (DFSDsetNT(DFNT_FLOAT32) < 0) 
          return FAIL;
      }
    numtype     = Writesdg.numbertype;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));

    if ((rdim >= Writesdg.rank) || (rdim < 0) /* check dimensions */
	|| (dimsize!=Writesdg.dimsizes[rdim])) 
      {
        HRETURN_ERROR(DFE_BADDIM, FAIL);
      }

    if (!scale) 
      {		/* No scale for this dimension */
        if (Writesdg.dimscales)
            Writesdg.dimscales[rdim] =
		(uint8 *) HDfreespace((VOIDP) Writesdg.dimscales[rdim]);
        Ref.scales = 0;
        return SUCCEED;
      }

    /* get number type and size of this type in this machine  */
    if (Writesdg.numbertype == DFNT_NONE)
      {
	if (DFSDsetNT(DFNT_FLOAT32) < 0) 
          return FAIL;
      }
    numtype     = Writesdg.numbertype;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));
    bytesize    = dimsize * localNTsize;
        
    /* allocate space for dimscales if necessary */
    if (!Writesdg.dimscales) 
      {
        Writesdg.dimscales =
          (uint8 **) HDgetspace((uint32)Writesdg.rank * sizeof(int8 *));
        if (Writesdg.dimscales == NULL) 
          return FAIL;
        for (i =0; i < Writesdg.rank; i++) /* set allocated pointers to NULL */
            Writesdg.dimscales[i] = NULL;
      }

    if (!Writesdg.dimscales[rdim]) 
      {
	/* allocate dimension scale space if necessary */
        Writesdg.dimscales[rdim] =
          (uint8 *) HDgetspace((uint32) bytesize);
        if (Writesdg.dimscales[rdim] == NULL) 
          return FAIL;
      }

    /* copy scale */
    p1 = (uint8 *)scale;
    p2 = (uint8 *)Writesdg.dimscales[rdim];
    HDmemcpy(p2, p1, bytesize);

    /* Indicate scales modified */
    Ref.scales = 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetrange()
 * Purpose: Set maximum and minimum data values
 * Inputs:  maxi: maximum value
 *          mini: minimum value
 * Globals: Ref
 * Returns: 0 on success, -1 if no maxmin values or if error, with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Modify Writesdg, set Ref
 * Remarks: Automatically cleared after call to DFSDputdata or DFSDadddata
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDsetrange(VOIDP maxi, VOIDP mini)
#else
intn DFSDsetrange(maxi, mini)
     VOID *maxi;
     VOID *mini;
#endif /* PROTOTYPE */
{
    int32 numtype;
    int32 localNTsize;
    intn i;
    uint8 *p1, *p2;

    HEclear();

    p1 = &(Writesdg.max_min[0]);
    for (i = 0; i < 16; i++)
        *p1++ = 0;       /* clear max_min   */

    /* get number type and copy the values to Writesdg   */
    if (Writesdg.numbertype == DFNT_NONE)
        DFSDsetNT(DFNT_FLOAT32);

    numtype     = Writesdg.numbertype;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));
    p1 = (uint8 *)maxi;
    p2 = (uint8 *)mini;

    HDmemcpy((uint8 *) &(Writesdg.max_min[0]), p1, localNTsize);
    HDmemcpy((uint8 *)&(Writesdg.max_min[localNTsize]), p2, localNTsize);

    Ref.maxmin = 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDputdata
 * Purpose: Calls DFSDIputdata to write data and SDG to file
 * Inputs:  filename: name of HDF file to use
 *          rank: rank of data array
 *          dimsizes: sizes of the dimensions of data array
 *          data: array that holds data
 * Globals: Writeref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIputdata
 * Method:  Invoke DFSDIputdata
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDputdata(char *filename, intn rank, int32 dimsizes[], VOIDP data)
#else
intn DFSDputdata(filename, rank, dimsizes, data)
     char *filename;
     intn rank;
     int32 dimsizes[];
     VOID *data;
#endif /* PROTOTYPE */
{
    /* 0, 0 specify create mode, C style array (row major) */
    return (DFSDIputdata(filename, rank, dimsizes, data, 0, 0));
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDadddata
 * Purpose: Calls DFSDIputdata to append data and SDG to file
 * Inputs:  filename: name of HDF file to use
 *          rank: rank of data array
 *          dimsizes: sizes of the dimensions of data array
 *          data: array that holds data
 * Globals: Writeref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIputdata
 * Method:  Invoke DFSDIputdata
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDadddata(char *filename, intn rank, int32 dimsizes[], VOIDP data)
#else
intn DFSDadddata(filename, rank, dimsizes, data)
     char *filename;
     intn rank;
     int32 dimsizes[];
     VOID *data;
#endif /* PROTOTYPE */
{
    /* 1, 0 specifies append mode, C style array (row major) */
    return (DFSDIputdata(filename, rank, dimsizes, data, 1, 0));
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDrestart
 * Purpose: Do not remember info about file - get again from first data set
 * Inputs:  none
 * Returns: 0 on success
 * Users:   HDF programmers
 * Remarks: Just reset Lastfile to NULL
 *          Subsequent gets will starts from first image
 *          Next put will write all "set" info to file
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDrestart(void)
#else
intn DFSDrestart()
#endif /* PROTOTYPE */
{
    Lastfile[0] = '\0';
    Readref = 0;
    return SUCCEED;
}
    
/*-----------------------------------------------------------------------------
 * Name:    DFSDndatasets
 * Purpose: Return number of NSDGs in file
 * Inputs:  filename - name of HDF file
 * Globals: none
 * Returns: number of NSDGs on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIopen, Hclose
 * Method:  open file (creating nsdgs table), then read nsdghdr->size
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFSDndatasets(char *filename)
#else
int32 DFSDndatasets(filename)
     char *filename;
#endif /* PROTOTYPE */
{
    int32 file_id;
    int32 nsdgs=0;
    char *FUNC="DFSDndatasets";

    HEclear();

    /* should use reopen if same file as last time - more efficient */
    file_id = DFSDIopen(filename, DFACC_READ);
    if (file_id == FAIL) 
        HRETURN_ERROR(DFE_BADOPEN, FAIL);

    nsdgs = nsdghdr->size;
    if (Hclose(file_id) == FAIL) 
      return FAIL;

    return nsdgs;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDclear
 * Purpose: Clear all "set" values
 * Inputs:  none
 * Globals: Writesdg, Ref
 * Returns: 0 on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIclear
 * Method:  Invoke DFSDIclear
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
#ifdef CONVEX
intn DFSDclear()
#else
intn DFSDclear(VOID)
#endif
#else
intn DFSDclear()
#endif /* PROTOTYPE */
{
    lastnsdg.tag = DFTAG_NULL;
    lastnsdg.ref = 0;
    if (DFSDIclearNT(&Writesdg) < 0)
      return FAIL;

    return (DFSDIclear(&Writesdg));
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDlastref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Lastref
 * Returns: ref on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  return Lastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
uint16 DFSDlastref(void)
#else
uint16 DFSDlastref()
#endif /* PROTOTYPE */
{
    return ((uint16) Lastref);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDreadref
 * Purpose: Set ref of SDS to get next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next get
 * Returns: 0 on success, FAIL on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFSDIopen, DFIfind
 * Remarks: checks if image with this ref exists
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDreadref(char *filename, uint16 ref)
#else
intn DFSDreadref(filename, ref)
    char *filename;
    uint16 ref;
#endif /* PROTOTYPE */
{
    int32 file_id;
    int32 aid;
    char *FUNC="DFSDreadref";

    HEclear();

    file_id = DFSDIopen(filename, DFACC_READ);
    if (file_id == DF_NOFILE) 
      return FAIL; 

    if((aid = Hstartread(file_id, DFTAG_SDG, ref)) == FAIL
       && (aid = Hstartread(file_id, DFTAG_NDG, ref)) == FAIL)      
      {
        HCLOSE_RETURN_ERROR(file_id, DFE_NOMATCH, FAIL); 
      }

    Hendaccess(aid);
    Readref = ref;
    Newdata = -1;
    return Hclose(file_id);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetslice
 * Purpose: Get slice of data from SDG.  Will sequence to next SDG if
 *          DFSDgetdims, DFSDgetdata or DFSDgetslice not called earlier.
 * Inputs:  filename: name of HDF file to use
 *          winst: array of size = rank of data, containing start of slice
 *          windims: array of size rank, containing end of slice
 *          data: array for returning slice
 *          dims: dimensions of array data
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: slice of data in data
 * Users:   DFSDIgetdata
 * Invokes: DFSDIgetslice
 * Method:  call DFSDIgetslice
 * Remarks: dims may be larger than size of slice.  In that event, the actual
 *          data may not be contiguous in the array "data".
 *          User sets dims before call.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetslice(char *filename, int32 winst[], int32 windims[], VOIDP data,
		  int32 dims[])
#else
intn DFSDgetslice(filename, winst, windims, data, dims)
     char *filename;
     int32 winst[];
     int32 windims[];
     int32 dims[];
     VOID *data;
#endif /* PROTOTYPE */
{
    return (DFSDIgetslice(filename, winst, windims, data, dims, 0));
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDstartslice
 * Purpose: Set up to write slice of data to SDG.
 * Inputs:  filename: name of HDF file to write to
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDIputdata
 * Invokes: DFSDIopen, DFnewref, DFaccess
 * Method:  call DFSDIputslice
 * Remarks: DFSDsetdims must have been called first
 *          No call which needs a file open may be made after this
 *          till DFSDendslice is called
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDstartslice(char *filename)
#else
intn DFSDstartslice(filename)
     char *filename;
#endif /* PROTOTYPE */
{
    intn i;
    int32 size;
    char *FUNC="DFSDstartslice";

    HEclear();

    if (!Writesdg.rank) 	/* dimensions not set */
        HRETURN_ERROR(DFE_BADDIM, FAIL);

    Sfile_id = DFSDIopen(filename, DFACC_WRITE);
    if (Sfile_id == DF_NOFILE) 
      return FAIL;

    if (!Writeref) 
      Writeref = Hnewref(Sfile_id);
    if (!Writeref) 
      return FAIL;

    Writesdg.data.tag = DFTAG_SD;
    Writesdg.data.ref = Writeref;
    
    if (Writesdg.numbertype == DFNT_NONE) /* if NT not set,default to float32 */
        DFSDsetNT(DFNT_FLOAT32);

    /* set up to write data */
    size = DFKNTsize(Writesdg.numbertype);
    for (i = 0; i < Writesdg.rank; i++)
        size *= Writesdg.dimsizes[i];

    Writesdg.aid = Hstartwrite(Sfile_id, DFTAG_SD, Writeref, size);
    if (Writesdg.aid == FAIL) 
      {
        Hclose(Sfile_id); 
        return FAIL;
      }

    /* allocate array for keeping track of dims written */
    Sddims = (int32 *) HDgetspace((uint32) Writesdg.rank * sizeof(int32));
    if (Sddims == NULL) 
      {
        Hclose(Sfile_id); 
        return FAIL;
      }
    
    for (i = 0; i < Writesdg.rank; i++)
        Sddims[i] = 0;		/* nothing written so far */

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDputslice
 * Purpose: Put slice of data to SDG.
 * Inputs:  winend: array of size rank, containing end of slice
 *          data: array containing slice
 *          dims: dimensions of array data
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDIputdata
 * Invokes: DFSDIputslice
 * Method:  call DFSDIputslice
 * Remarks: dims may be larger than size of slice.  In that event, the actual
 *          data may not be contiguous in the array "data".
 *          DFSDstartslice must have been called first
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDputslice(int32 winend[], VOIDP data, int32 dims[])
#else
intn DFSDputslice(winend, data, dims)
     int32 winend[];
     VOID *data;
     int32 dims[];
#endif /* PROTOTYPE */
{
    return (DFSDIputslice(winend, data, dims, 0));
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDendslice
 * Purpose: Write of data to SDG completed, write SDG and close file
 * Inputs:  none
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDIputdata
 * Invokes: DFSDIendslice 
 * Method:  call DFSDIendslice
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDendslice(void)
#else
intn DFSDendslice()
#endif /* PROTOTYPE */
{
    return (DFSDIendslice(0));
}

/*---------------------------------------------------------------------
* Name:     DFSDsetNT
* Purpose:  Set number type for writing out to the hdf file
* Inputs:   numbertype
* Return:   0 on success, FAIL on failure with error set
* Outputs:  none
* Method:   Set Writesdg.numbertype to numbertype 
*	    Calls DFKsetNT, which keeps current NT and also initializes
*	    conversion routines. 
* Remarks:
*--------------------------------------------------------------------- */

#if defined PROTOTYPE
intn DFSDsetNT(int32 numbertype)
#else
intn DFSDsetNT(numbertype)
     int32 numbertype;
#endif /* PROTOTYPE */
{
    uint8 outNT;
    char *FUNC="DFSDsetNT";

    HEclear();

    outNT = (int8)(DFKisnativeNT(numbertype)? DFKgetPNSC(numbertype, DF_MT) :
                  (DFKislitendNT(numbertype) ? DFNTF_PC : DFNTF_HDFDEFAULT));
    if ((numbertype == Writesdg.numbertype) 
        && (outNT == Writesdg.filenumsubclass))
      {
        return SUCCEED;
      }

    /* Forget previous numbertype  */
    if (DFSDIclearNT((DFSsdg *)&Writesdg) < 0) 
      return FAIL;
    Writesdg.numbertype      = numbertype;
    Writesdg.filenumsubclass = outNT;
    Ref.nt      = 0;
    Ref.dims    = (Ref.dims >= 0? 0: Ref.dims);
    Ref.new_ndg = 0;

    return (DFKsetNT(numbertype));
}

/*-------------------------------------------------------------------
* Name:    DFSDIclearNT
* Purpose: Reset all "set" values related to number types
* Inputs:  sdg: pointer to sdg struct to clear
* Globals: Ref
* Returns: 0 on success, FAIL on error with error set
* Users:   DFSDsetNT, HDF users
* Invokes: none
* Remarks:
*--------------------------------------------------------------------*/

#if defined PROTOTYPE
intn DFSDIclearNT(DFSsdg *sdg)
#else
intn DFSDIclearNT(sdg)
     DFSsdg *sdg;
#endif
{
    intn i;
    char *FUNC="DFSDIclearNT";

    HEclear();

    sdg->numbertype      = DFNT_NONE;
    sdg->filenumsubclass = DFNTF_NONE;

    /* free scale pointers. Note: scale pointer array is not freed   */
    /* sdg->dimscales will be freed only when rank is changed        */
    if (sdg->dimscales)
      {
        for (i=0; i<sdg->rank; i++)
            sdg->dimscales[i] = (uint8 *)
                                HDfreespace((VOIDP) sdg->dimscales[i]);
      }

    Ref.nt      = -1;
    Ref.maxmin  = -1;    /* maxmin and scales should be changed to */
    Ref.scales  = -1;    /* new number type              */
    Ref.new_ndg = -1;

    return SUCCEED;
}

/*--------------------------------------------------------------------
* Name:    DFSDgetNT
* Purpose: Get number type to be read from the hdf file
* Inputs:  pnumbertype: pointer to int32 to return number type in
* Return:  0 on success, FAIL on error with DFerror set
* Method:  return numbertype in sdg struct
* Remark:
*------------------------------------------------------------------- */

#if defined PROTOTYPE
intn DFSDgetNT(int32 *pnumbertype)
#else
intn DFSDgetNT(pnumbertype)
     int32 *pnumbertype;
#endif
{
    char *FUNC="DFSDgetNT";

    HEclear();

    *(pnumbertype) = Readsdg.numbertype;
    if (*(pnumbertype) == DFNT_NONE) 	
        HRETURN_ERROR(DFE_BADNUMTYPE, FAIL);

    return SUCCEED;
}
    
/* ------------------------------------------------------------------
* Name:    DFSDpre32sdg
* Purpose: tests if the SDG with given ref is HDF3.1 object 
* Inputs:  filename: the file where the SDG/ref resides in
*          ref: ref of the SDG
* Outputs: ispre32: set to TRUE--1 if it is 3.1 SDG; 
*                   to FALSE--0 otherwise
* Returns: SUCCEED--0 on sucess; FAIL (-1) otherwise 
*          with error code set
* Remarks: 
* -------------------------------------------------------------------*/
#if defined PROTOTYPE
intn DFSDpre32sdg(char *filename, uint16 ref, intn *ispre32)
#else
intn DFSDpre32sdg(filename, ref,ispre32)
     char *filename;
     uint16 ref;
     intn *ispre32;
#endif
{
    uint32 num;
    int32  file_id;
    intn   found = 0;
    DFnsdgle *ptr;
    char *FUNC="DFSDpre32sdg";
   
    file_id = DFSDIopen(filename, DFACC_READ);
    if (file_id== FAIL) 
      return FAIL; 
    ptr = nsdghdr->nsdg_t;
    num = nsdghdr->size;

    while ((num>0) && (ptr != NULL) && !found) 
      {
        if ((ptr->nsdg.tag == DFTAG_SDG) 
            && (ptr->nsdg.ref == ref))  
          { /* pure SDG  */
            found    = 1;
            *ispre32 = TRUE;
          } 
        else if ((ptr->sdg.tag == DFTAG_SDG) 
                 && (ptr->sdg.ref == ref))  
          { /* NDGSDG   */
            found    = 1;
            *ispre32 = FALSE;
          } 
        else 
          {
    	    ptr = ptr->next;
            num--;
          }
      } /* while  */

    if (((num == 0) && (ptr != NULL)) || ((num != 0) && (ptr == NULL)) 
       || !found)  
      {
        HCLOSE_RETURN_ERROR(file_id, DFE_BADTABLE, FAIL); 
      }

    if (Hclose(file_id) < 0) 
      return FAIL;

    return SUCCEED;
}   /* end of DFSDpre32sdg   */
    
/******************************************************************************/
/*--------------------- Lower level routines --------------------------------*/
/******************************************************************************/

/* Functions for NDG and SDG stuff 			 */

/*--------------------------------------------------------------------------
 * Name:    DFSDIsetnsdg_t
 * Purpose: Set up the NDG/SDG table. Each node has two
            fields: the 1st field is NDG or SDG, the 2nd
            field has value only when it is a special
            NDG, i.e. the data set is float32 and not
     	    compressed.
 * Inputs:  file_id: pointer to HDF file containing SDG
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDIopen for READ
 *--------------------------------------------------------------------------*/
#if defined PROTOTYPE
intn DFSDIsetnsdg_t(int32 file_id, DFnsdg_t_hdr *nsdghdr)
#else
intn DFSDIsetnsdg_t(file_id,nsdghdr)
     int32 file_id;
     DFnsdg_t_hdr *nsdghdr;
#endif
{
    uint32 sz_DFnsdgle = (uint32)sizeof(struct DFnsdgle);
    int32 aid;      /* access id */
    int32 ndgs;     /* number of ndg's */
    int32 sdgs;	    /* number of sdg's */
    int32 GroupID;
    uint16 intag;
    uint16 inref;
    bool moretags;
    bool found;
    DFnsdgle *ntb;
    DFnsdgle *stb;
    DFnsdgle *new;
    DFnsdgle *nf;
    DFnsdgle *nr;
    DFnsdgle *sf;
    DFnsdgle *sr;
    DFdi di;
    DFdi lnkdd[2];
    uint8 *bufp;
    char *FUNC="DFSDsetnsdg_t";
  
    if (!HDvalidfid(file_id)) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

/* MMM:  Talk to Shiming and make sure the change made to the way ndgs
         and sdgs are handled is ok.
*/
    ndgs = Hnumber(file_id, DFTAG_NDG);
    sdgs = Hnumber(file_id, DFTAG_SDG);
    if ( (ndgs == FAIL) || (sdgs == FAIL) )
        return FAIL;
    
    if ( (ndgs+sdgs) == 0 ) 
      {       /* no sdgs or ndgs in file */
        nsdghdr->size   = 0;
        nsdghdr->nsdg_t = NULL;
        return SUCCEED;
      }
    if ( (ntb = (DFnsdgle *)HDgetspace(sz_DFnsdgle) ) == NULL)  
        HRETURN_ERROR(DFE_NOSPACE, FAIL);

    /* the first node in each table is a dummy node  */
    ntb->nsdg.tag = DFTAG_NULL;	/* set up and init an ndg table  */
    ntb->nsdg.ref = 0;
    ntb->sdg.tag  = DFTAG_NULL;
    ntb->sdg.ref  = 0;
    ntb->next     = NULL;

    if ((stb = (DFnsdgle *)HDgetspace(sz_DFnsdgle)) == NULL)   
        HRETURN_ERROR(DFE_NOSPACE, FAIL);

    stb->nsdg.tag = DFTAG_NULL;	/* set up and init an sdg table  */
    stb->nsdg.ref = 0;
    stb->sdg.tag  = DFTAG_NULL; /* this field should be named as */
    stb->sdg.ref  = 0;          /* stb->ndg.tag, the ndg to which this */
    stb->next     = NULL;	/* sdg belongs. 		*/

    aid = Hstartread(file_id, DFTAG_WILDCARD, DFREF_WILDCARD);
    moretags = (aid != FAIL);
    while (moretags)	
      { /* read dd's and put each dd in ntb or stb */
        HQuerytagref(aid, &intag, &inref);
        /* put NDG or SDG on ntb or stb	*/
	if (intag == DFTAG_NDG)	
          {
	    nr = ntb;
	    nf = ntb;
	    while ((inref > nf->nsdg.ref) && (nf->next != NULL))   
              {
                nr = nf;
                nf = nf->next;
	      }
            /* MMM:  Tlk to Shiming and make sure the way this part was rearranged is ok.
             */

            /* check for duplicate nsdg */
	    if (inref == nf->nsdg.ref)      
                HRETURN_ERROR(DFE_BADNDG, FAIL);
            
            /* add a node to the table */
            if ((new = (DFnsdgle *)HDgetspace(sz_DFnsdgle)) == NULL)
                HRETURN_ERROR(DFE_NOSPACE, FAIL);
            
            new->nsdg.tag = DFTAG_NDG;
            new->nsdg.ref = inref;
            new->sdg.tag  = DFTAG_NULL;
            new->sdg.ref  = 0;
            
            if (inref < nf->nsdg.ref) 
              {  /* does it go before current node? */
                new->next = nf;
                nr->next  = new;
              } 
            else 
              {                     /* or at the end? */
                new->next = nf->next;
                nf->next  = new;
              }
            
	    /* Does this NDG have an SDG? 	*/
	    if ((GroupID = DFdiread(file_id, DFTAG_NDG, inref)) < 0)
                return FAIL;

	    found  = FALSE;
	    di.tag = DFTAG_NULL;
	    di.ref = 0;
	    while ((found == 0) 
                   && (DFdiget(GroupID, &di.tag, &di.ref) == 0))  
              {
 		if (di.tag == DFTAG_SDLNK) 
                  found = TRUE;
	      }

	    if (found)	
              {    /* read in the tag/refs in the link element */
                if (Hgetelement(file_id, di.tag, di.ref, DFtbuf) == (int32)FAIL)
                    return FAIL;
                bufp = DFtbuf;
                UINT16DECODE(bufp, lnkdd[0].tag);
                UINT16DECODE(bufp, lnkdd[0].ref);
                UINT16DECODE(bufp, lnkdd[1].tag);
                UINT16DECODE(bufp, lnkdd[1].ref);
                new->sdg.tag = lnkdd[1].tag;
                new->sdg.ref = lnkdd[1].ref;
	      }
	  }	/* end of NDG    */

	if (intag == DFTAG_SDG)	
          {
	    sr = stb;
	    sf = stb;
	    while ((inref > sf->nsdg.ref) && (sf->next != NULL))   
              {
                sr = sf;
                sf = sf->next;
	      }
	    if (inref == sf->nsdg.ref)
                HRETURN_ERROR(DFE_BADNDG, FAIL);

            /* insert a new node */
	    if ((new = (DFnsdgle *)HDgetspace(sz_DFnsdgle)) == NULL)
	        HRETURN_ERROR(DFE_NOSPACE, FAIL);

	    new->nsdg.tag = DFTAG_SDG;
	    new->nsdg.ref = inref;
            new->sdg.tag  = DFTAG_NULL;
	    new->sdg.ref  = 0;
            
	    if (inref < sf->nsdg.ref)  
              {  /* does it go before current node? */
                new->next = sf;
                sr->next  = new;
	      } 
            else 
              {                      /* or at the end? */
                new->next = sf->next;
                sf->next  = new;
	      }
	    /* Does it belong to  an NDG?    */
	    if ((GroupID = DFdiread(file_id, DFTAG_SDG, inref)) < 0) 
              return FAIL;
	    found  = FALSE;
            di.tag = DFTAG_NULL;
	    di.ref = 0;        	
            while ((found == 0) && (DFdiget(GroupID, &di.tag, &di.ref) == 0)) 
              {
                if (di.tag == DFTAG_SDLNK) 
                  found = TRUE;
              }
	    if (found)	
              {   /* read in the tag/refs in the link element */
                if (Hgetelement(file_id,  di.tag, di.ref, DFtbuf)==(int32)FAIL)
                    return FAIL;
                bufp = DFtbuf;
                UINT16DECODE(bufp, lnkdd[0].tag);
                UINT16DECODE(bufp, lnkdd[0].ref);
                UINT16DECODE(bufp, lnkdd[1].tag);
                UINT16DECODE(bufp, lnkdd[1].ref);
                new->sdg.tag = lnkdd[0].tag;
                new->sdg.ref = lnkdd[0].ref;
	      }
	  }	/* end of SDG    */

	/*   get next dd   */
        moretags = (SUCCEED == 
                  Hnextread(aid, DFTAG_WILDCARD, DFREF_WILDCARD, DF_CURRENT));
      }	/* gone through the dd blocks   */
    Hendaccess(aid);

    /* merge stb and ntb	*/
    /* remove SDGNDG from stb   */
    nf = ntb->next;
    while (nf != NULL)	
      {
        inref = nf->sdg.ref;
        if (inref != 0) 
          {   /* it has an SDG   */
            sr = stb;
            sf = stb;
            while ((sf->nsdg.ref < inref) && (sf->next != NULL))  
              {
                sr = sf;
                sf = sf->next;
              }
            if (sf->nsdg.ref == inref)      
              {
                if (sf->sdg.ref != nf->nsdg.ref) 
                  {
                    HRETURN_ERROR(DFE_BADNDG, FAIL);
                  } 
                else 
                  {
                    sr->next = sf->next;
                    if ((sf = (DFnsdgle *)HDfreespace((VOIDP) sf)) != NULL)
                        return FAIL;
                    sdgs--;
                  }
              }
          }
        nf = nf->next;
      }

    /* check all SDGNDGs were removed   */
    sf = stb->next;
    while (sf != NULL)	
      {
        if (sf->sdg.ref != 0)
            HRETURN_ERROR(DFE_BADSDG, FAIL);
        sf = sf->next;
      }

    /* merge the two tables into one */
    nf = ntb;			/* looking for the end of ntb   */
    while (nf->next != NULL) 
        nf = nf->next;
    nf->next        = stb->next; /* the first node in stb is a dummy */
    nsdghdr->size   = ndgs + sdgs;
    nsdghdr->nsdg_t = ntb->next;
    
    /* Release the first nodes in stb and ntb  */
    HDfreespace((VOIDP)stb);
    HDfreespace((VOIDP)ntb);

    return SUCCEED;
}   /* end of DFSDsdtnsdg_t   */

/*-----------------------------------------------------------------------
* Name	DFSDInextnsdg
* Purpose: Returns next ndg or sdg in the file
* Inputs:  nsdghdr: point to the nsdg table 
*	   nsdg: the structure holds the di of next sdg or ndg
* Returns: 0 on succeeds, FAIL on failure
* -------------------------------------------------------------------*/
#if defined PROTOTYPE
intn DFSDInextnsdg(DFnsdg_t_hdr *nsdghdr, DFdi *nsdg)
#else
intn DFSDInextnsdg(nsdghdr,nsdg)
     DFnsdg_t_hdr *nsdghdr;
     DFdi *nsdg;
#endif /* PROTOTYPE*/
{
    uint32 num;
    intn found=FALSE;
    DFnsdgle *ptr;
    char *FUNC="DFSDInextnsdg";

    nsdg->tag = DFTAG_NULL;
    nsdg->ref = 0;
    ptr = nsdghdr->nsdg_t;
    num = nsdghdr->size;

    if ((ptr == NULL) || (num == 0)) 
      return SUCCEED;

    if ((lastnsdg.tag == DFTAG_NULL) && (lastnsdg.ref == 0)) 
      {
        found = TRUE;
      } 
    else 
      {
        while ((num > 0) && (ptr != NULL) && !found)
          {
            if ((ptr->nsdg.tag == lastnsdg.tag)
                && (ptr->nsdg.ref == lastnsdg.ref))
              {
    	        if ((ptr = ptr->next) != NULL)
                  found = TRUE;
              }
            else
              {
    	        ptr = ptr->next;
                num--;
    	      }
          } /* while  */

        if(((num == 0) && (ptr != NULL))
           || ((num != 0) && (ptr == NULL))
           || !found)
          {
            HRETURN_ERROR(DFE_BADTABLE, FAIL);
          }
      }	/* else   */

    if (found)	
      {
        nsdg->tag = ptr->nsdg.tag;
        nsdg->ref = ptr->nsdg.ref;
      }

    return SUCCEED;
}   /* end of DFSDInextnsdg   */

/*-----------------------------------------------------------------------------
 * Name:    DFSDIgetndg
 * Purpose: Reads in NDG
 * Inputs:  file_id: pointer to HDF file containing NDG
 *          ref: ref of NDG to read
 *          sdg: pointer to DFSsdg struct to read NDG into
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF programmers, DFSDIsdginfo
 * Invokes: DFgetelement, DFdiread, DFdiget, DFaccess, DFread
	    DFSDgetsdg
 * Method:  Reads in NDG using DFdiread.  Gets each tag/ref using DFdiget.
 *          Reads in dimensions using DFgetelement.
 * 	    Call DFSDgetsdg to read in the rest info.
 *          Mallocs space for these, freeing
 *          previously allocated space.
 * Remarks: This accepts non-float32 data
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIgetndg(int32 file_id, uint16 tag, uint16 ref, DFSsdg *sdg)
#else
intn DFSDIgetndg(file_id, tag, ref, sdg)
     int32 file_id;
     uint16 tag;
     uint16 ref;
     DFSsdg *sdg;
#endif /* PROTOTYPE */
{
    int32 i;
    intn luf;
    DFdi elmt;
    DFdi nt;
    int32 length;
    int32 numtype;	/* current number type */
    int32 fileNTsize;	/* size of this NT as it is in the file */
    int32 localNTsize;	/* size of this NT as it is in this machine */
    int32 ret;
    int32 aid;
    int32 GroupID;
    int8  fileNT;	     /* file number subclass */
    int8  platnumsubclass;   /* platform number subclass */
    uint8 ntstring[4];
    uint8 *isscales;
    uint8 *buf;
    uint8 *p;           /* temporary pointer for moving things to buffer */
    char *FUNC="DFSDIgetndg";

    HEclear();

    if (!HDvalidfid(file_id))
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    if (!ref)
        HRETURN_ERROR(DFE_BADREF, FAIL);

    /* read NDG into memory */
    if ((GroupID = DFdiread(file_id, tag, ref)) < 0)
        return FAIL;

    DFSDIclear(sdg);
    if (tag == DFTAG_NDG)
      DFSDIclearNT(sdg);
    Ismaxmin = 0;
    IsCal    = FALSE;

    /*
     * Loop through all members of the group
     */
    while (!DFdiget(GroupID, &elmt.tag, &elmt.ref))
      {
        luf = -1;		              /* flag value for label/unit/ */
        /* format gets process tag/ref */
        switch (elmt.tag) {

        case DFTAG_SD:	/* data tag/ref */
            sdg->data.tag = elmt.tag; /* put tag/ref in struct */
            sdg->data.ref = elmt.ref;
            break;

        case DFTAG_SDD:	/* dimension */
            aid = Hstartread(file_id, elmt.tag, elmt.ref);
            if (aid == FAIL)
                return FAIL;

            /* read rank */
            if (Hread(aid, (int32) 2, DFtbuf) == FAIL)
              {
                Hendaccess(aid);
                return FAIL;
              }
            p = DFtbuf;
            INT16DECODE(p, sdg->rank);

            /* get space for dimensions */
            sdg->dimsizes = (int32 *) HDgetspace((uint32) sdg->rank *
                                                 sizeof(int32));
            if (sdg->dimsizes == NULL)
              {
                Hendaccess(aid);
                return FAIL;
              }

            /* read dimension record */
            if (Hread(aid, (int32) 4 * sdg->rank,DFtbuf) == FAIL)
              {
                Hendaccess(aid);
                return FAIL;
              }
            p = DFtbuf;
            for (i=0; i<sdg->rank; i++)
                INT32DECODE(p, sdg->dimsizes[i]);
            
            /* read tag/ref of NT */
            if (Hread(aid,(int32) 4,  DFtbuf) == FAIL) 
              {
                Hendaccess(aid);
                return FAIL;
              }
            p = DFtbuf;
            UINT16DECODE(p, nt.tag);
            UINT16DECODE(p, nt.ref);
            
            /* read actual NT */
            if (Hgetelement(file_id, nt.tag, nt.ref, ntstring) == FAIL) 
              {
                Hendaccess(aid);
                return FAIL;
              }
            
            /* check for any valid NT */
            if (ntstring[1] == DFNT_NONE) 
              {
                Hendaccess(aid);
                HRETURN_ERROR(DFE_BADCALL, FAIL);
              }
            
            /* if looking for an SDG type must be FLOAT32 */
            if (tag == DFTAG_SDG && ntstring[1] != DFNT_FLOAT32) 
              {
                Hendaccess(aid);
                HRETURN_ERROR(DFE_BADCALL, FAIL);
              }
                
            /* set NT info */
            numtype = ntstring[1];
            fileNT  = ntstring[3];
            platnumsubclass = DFKgetPNSC(numtype, DF_MT);
            if ((fileNT != DFNTF_HDFDEFAULT)
                && (fileNT != DFNTF_PC)
                && (fileNT != platnumsubclass))    
              {
                Hendaccess(aid);
                HRETURN_ERROR(DFE_BADCALL, FAIL);
              }
            if (fileNT != DFNTF_HDFDEFAULT)
              { /* if native or little endian */
                if(fileNT != DFNTF_PC)   /* native */
                    numtype |= DFNT_NATIVE;
                else                    /* little endian */
                    numtype |= DFNT_LITEND;
              } /* end if */

            sdg->filenumsubclass = ntstring[3];
            sdg->numbertype      = numtype;

            /* set size of NT    */
            fileNTsize  = DFKNTsize(numtype);
            localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));

            /* read and check all scale NTs */
            for (i = 0; i < sdg->rank; i++) 
              {
                if (Hread(aid, (int32) 4, DFtbuf) == FAIL) 
                  {
                    Hendaccess(aid);
                    return FAIL;
                  }
                p = DFtbuf;
                UINT16DECODE(p, nt.tag);
                UINT16DECODE(p, nt.ref);
                    
                /* read NT itself */
                if (Hgetelement(file_id, nt.tag,nt.ref, ntstring) == FAIL) 
                  {
                    Hendaccess(aid);
                    return FAIL;
                  }

                /* check for any valid NT */
                if (ntstring[1] == DFNT_NONE)
                  {
                    Hendaccess(aid);
                    HRETURN_ERROR(DFE_BADCALL, FAIL);
                  }
            
                /* if looking for an SDG type must be FLOAT32 */
                if (tag == DFTAG_SDG && ntstring[1] != DFNT_FLOAT32)
                  {
                    Hendaccess(aid);
                    HRETURN_ERROR(DFE_BADCALL, FAIL);
                  }

              } /* end for loop */
            Hendaccess(aid);
            break;
            
        case DFTAG_SDLNK:     /* SDG NDG link */
            break;      /* do nothing in 3.2  */
            
        case DFTAG_SDL:     /* labels */
            if (luf == (-1))
              luf = LABEL;
            
        case DFTAG_SDU:     /* units */
            if (luf == (-1)) 
              luf = UNIT;
            
        case DFTAG_SDF:     /* formats */
            if (luf == (-1)) 
              luf = FORMAT;
            
            if (!sdg->dimsizes) 
                HRETURN_ERROR(DFE_CORRUPT, FAIL);
            
            /* get needed size of buffer, allocate */
            length = Hlength(file_id, elmt.tag, elmt.ref);
            if (length == FAIL)
              return FAIL;
            buf = (uint8 *) HDgetspace((uint32) length);
            if (buf == NULL)
              return FAIL;

            /* read in luf */
            if (Hgetelement(file_id, elmt.tag, elmt.ref, buf) == FAIL) {
                HDfreespace((VOIDP)buf);
                return FAIL;
              }
            p = buf;

            /* allocate data luf space */
            sdg->dataluf[luf] = HDgetspace((uint32) HDstrlen((char*)p)+1);

            if (sdg->dataluf[luf] == NULL) {
                HDfreespace((VOIDP)buf);
                return FAIL;
              }

            /* extract data luf */
            HDstrcpy(sdg->dataluf[luf], (char*)p);
            p += HDstrlen(sdg->dataluf[luf])+1;

            /* get space for dimluf array */
            sdg->dimluf[luf] =
                (char **) HDgetspace((uint32) sdg->rank * sizeof(char *));
            if (sdg->dimluf[luf] == NULL) {
                HDfreespace((VOIDP)buf);
                return FAIL;
              }

            /* extract dimension lufs */
            for (i = 0; i < sdg->rank; i++)
              {
                sdg->dimluf[luf][i] =
                    HDgetspace((uint32) HDstrlen((char*)p) + 1);
                if (sdg->dimluf[luf][i] == NULL) {
                    HDfreespace((VOIDP)buf);
                    return FAIL;
                  }
                HDstrcpy(sdg->dimluf[luf][i], (char*)p);
                p += HDstrlen(sdg->dimluf[luf][i])+1;
              }
            HDfreespace((VOIDP)buf);
            break;

        case DFTAG_SDS:     /* scales */
            if (!sdg->dimsizes)
               HRETURN_ERROR(DFE_CORRUPT, FAIL);

            /* set up to read scale */
            aid = Hstartread(file_id, elmt.tag, elmt.ref);
            if (aid == FAIL)
              return FAIL;

            /* read isscales */
            isscales = (uint8 *) HDgetspace((uint32) sdg->rank);
            if (isscales == NULL) {
                Hendaccess(aid);
                return FAIL;
              }
            if (Hread(aid, (int32) sdg->rank, isscales) == FAIL) {
                Hendaccess(aid);
                return FAIL;
              }

            /* allocate scale pointers */
            sdg->dimscales =
                (uint8 **) HDgetspace((uint32) sdg->rank * sizeof(int8 *));
            if (sdg->dimscales == NULL) {
                HDfreespace((VOIDP)isscales);
                Hendaccess(aid);
                return FAIL;
              }

            /* read scales */
            for (i = 0; i < sdg->rank; i++) 
              {
                sdg->dimscales[i] = NULL;       /* default */
                if (!isscales[i]) continue;

                /* space for scale */
                sdg->dimscales[i] = (uint8 *)
                    HDgetspace((uint32) sdg->dimsizes[i] * localNTsize);
                if (sdg->dimscales[i] == NULL) {
                    HDfreespace((VOIDP)isscales);
                    Hendaccess(aid);
                    return FAIL;
                  }

                if (platnumsubclass == fileNT)
                  { /* no conversion needed */
                    ret = Hread(aid, (int32)sdg->dimsizes[i]*fileNTsize, 
                                (uint8 *) sdg->dimscales[i]);
                    if (ret == FAIL) {
                        HDfreespace((VOIDP)isscales);
                        Hendaccess(aid);
                        return FAIL;
                      }
                  } 
                else 
                  {                      /* conversion necessary */

                    /* allocate conversion buffer */
                    buf = (uint8 *) HDgetspace((uint32)sdg->dimsizes[i] * fileNTsize);
                    if (buf == NULL) {
                        HDfreespace((VOIDP)isscales);
                        Hendaccess(aid);
                        return FAIL;
                      }

                    /* read scale from file */
                    ret = Hread(aid,
                                (int32) (sdg->dimsizes[i]*fileNTsize), buf);
                    if (ret == FAIL) {
                        HDfreespace((VOIDP)buf);
                        HDfreespace((VOIDP)isscales);
                        Hendaccess(aid);
                        return FAIL;
                      }
                            
                    p = buf;

                    /* convert, all at once */
                    DFKconvert((VOIDP)p, (VOIDP)sdg->dimscales[i], numtype,
                               sdg->dimsizes[i], DFACC_READ, 0, 0);

                    HDfreespace((VOIDP)buf);
                  }
              }
            HDfreespace((VOIDP)isscales);
            Hendaccess(aid);
            break;

        case DFTAG_SDC:	/* coordsys */
            /* find and allocate necessary space */
            length =  Hlength(file_id, elmt.tag, elmt.ref);
            if (length == FAIL) 
              return FAIL;
            
            sdg->coordsys = HDgetspace((uint32) length);
            if (sdg->coordsys == NULL) 
              return FAIL;
            
            /* read coordsys */
            if (Hgetelement(file_id, elmt.tag, elmt.ref, 
                             (uint8 *) sdg->coordsys) == FAIL)
                return FAIL;
            break;
            
        case DFTAG_SDM:	/* max/min */
            if (fileNT == platnumsubclass)
              {       /* no conversion */
                if (Hgetelement(file_id, elmt.tag, elmt.ref,
                                (uint8 *) &(sdg->max_min[0])) == FAIL)
                    return FAIL;
              }
            else
              {
                /* conversion needed */
                /* allocate buffer */
                buf = (uint8 *) HDgetspace((uint32) 2 * fileNTsize);
                if (buf == NULL)
                  return FAIL;

                /* read and convert max/min */
                if (Hgetelement(file_id, elmt.tag, elmt.ref, buf) == FAIL)
                    return FAIL;

                DFKconvert((VOIDP)buf, (VOIDP)&(sdg->max_min[0]), numtype, 2,
                           DFACC_READ, 0, 0);

                HDfreespace((VOIDP)buf);
              }
            Ismaxmin = 1;
            break;

        case DFTAG_CAL:
            if (fileNT == platnumsubclass)
              {  /* no conversion */
                /* get size of element */
                intn eltSize = (intn)Hlength(file_id, elmt.tag, elmt.ref);
                if(eltSize == FAIL) 
                  return FAIL; 
                
                if(eltSize == 36) 
                  {
                    /* element is new, double based type */
                    if (Hgetelement(file_id, elmt.tag, elmt.ref,
                                    (unsigned char*) &sdg->cal) < 0)
                        return FAIL;
                  } 
                else
                  {
                    /* element is old float based type */
                    float32 buf2[4];

                    /* allocate input buffer */
                    if (Hgetelement(file_id, elmt.tag, elmt.ref,
                                    (unsigned char*) buf2) < 0)
                        return FAIL;

                    /* move 'em over */
                    sdg->ioff     = (float64) buf2[0];
                    sdg->ioff_err = (float64) buf2[1];
                    sdg->cal      = (float64) buf2[2];
                    sdg->cal_err  = (float64) buf2[3];
                    sdg->cal_type = DFNT_INT16;

                  }
              }
            else
              {
                intn eltSize;

                /* get size of element */
                eltSize = (intn)Hlength(file_id, elmt.tag, elmt.ref);
                if(eltSize == FAIL) 
                  return FAIL;

                /* allocate buffer */
                buf = (uint8 *) HDgetspace((uint32) eltSize);
                if (buf == NULL) 
                  return FAIL;
                
                /* read and convert calibration */
                if (Hgetelement(file_id, elmt.tag, elmt.ref, buf) == FAIL)
                    return FAIL;

                if (eltSize == 36) 
                  {
                    /* element is new, double based type */
                    /* read in the 64bit float factors */
                    DFKconvert((VOIDP)buf, 
                               (VOIDP) &sdg->cal,
                               DFNT_FLOAT64, 4, DFACC_READ, 0, 0);

                    /* read in the 32bit integer number type */
                    DFKconvert((VOIDP)(buf + 32),
                               (VOIDP)&sdg->cal_type, 
                               DFNT_INT32, 1, DFACC_READ, 0, 0);
                  }
                else 
                  {
                    /* element is old float based type */
                    float32 buf2[4];

                    /* convert calibration factors */
                    DFKconvert((VOIDP)buf, (VOIDP)buf2, DFNT_FLOAT32, 4,
                               DFACC_READ, 0, 0);

                    /* move 'em over */
                    sdg->ioff     = (float64) buf2[0];
                    sdg->ioff_err = (float64) buf2[1];
                    sdg->cal      = (float64) buf2[2];
                    sdg->cal_err  = (float64) buf2[3];
                    sdg->cal_type = DFNT_INT16;

                  }
                HDfreespace((VOIDP)buf);
              }
            IsCal = TRUE;
            break;

        case DFTAG_FV:
            if (fileNT == platnumsubclass)
              { /* no conversion */
                /* get size of element */
                intn eltSize = (intn)Hlength(file_id, elmt.tag, elmt.ref);
                if(eltSize == FAIL) 
                  return FAIL;

                /* get element */
                if (Hgetelement(file_id, elmt.tag, elmt.ref,
                                (unsigned char*) sdg->fill_value) == FAIL)
                return FAIL;
              }
            else
              {
                intn eltSize;

                /* get size of element  */
                eltSize = (intn)Hlength(file_id, elmt.tag, elmt.ref);
                if(eltSize == FAIL)
                  return FAIL;

                /* allocate buffer for conversion  */
                buf = (uint8 *) HDgetspace((uint32) eltSize);
                if (buf == NULL)
                  return FAIL;

                /* read fill value into buffer */
                if (Hgetelement(file_id, elmt.tag, elmt.ref, buf) == FAIL)
                    return FAIL;

                /* convert the fill value  */
                DFKconvert((VOIDP)buf, (VOIDP) sdg->fill_value,
                           numtype, 1, DFACC_READ, 0, 0);

                HDfreespace((VOIDP)buf);
             }
           break;

        case DFTAG_SDT:
            FileTranspose = 1;
            break;
        default:
            if((elmt.tag <= DFTAG_BREQ) && (elmt.tag >= DFTAG_EREQ))
                HRETURN_ERROR(DFE_BADNDG, FAIL);
            break;
        }
      }

    /* since the dataset exists, the fill value cannot be changed */
    sdg->fill_fixed=TRUE;

    return SUCCEED;
}

 /*---------------------------------------------------------------------------*
 * Name:    DFSDIputndg
 * Purpose: Write NDG out to HDF file
 * Inputs:  file_id: HDF file pointer
 *          ref: ref to put NDG with
 *          sdg: struct containing NDG info to put
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF programmers, utilities, DFSDputdata, other routines
 * Invokes: DFIcheck, DFdistart, DFdiadd, DFdiend, DFputelement, DFaccess,
 *          DFwrite
 * Remarks: Writes out NTs
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIputndg(int32 file_id, uint16 ref, DFSsdg *sdg)
#else
intn DFSDIputndg(file_id, ref, sdg)
     int32 file_id;
     uint16 ref;
     DFSsdg *sdg;
#endif /* PROTOTYPE */
{
    int32 i;
    intn j;
    intn luf;
    intn issdg = 0;    /* issdg=1 if it is NDG SDG  */
    intn len;
    uint16 luftag;
    uint8 *buf;
    uint8 *Isscales=NULL;
    uint8 *bufp;
    uint8 ntstring[4];
    uint8 platnumsubclass;
    uint8 outNT;	    /* file number type subclass */
    int32 GroupID;
    int32 numtype;	/* current number type	*/
    int32 localNTsize;  /* size of this NT on as it is on this machine */
    int32 fileNTsize;	/* size of this NT as it will be in the file */
    int32 scaleNTsize;	/* size of scale NT as it will be in the file */
    int32 ret;
    int32 aid;
    DFdi nt ;
    char *FUNC="DFSDIputndg";

    HEclear();

    if (!HDvalidfid(file_id))
      HRETURN_ERROR(DFE_BADCALL, FAIL);
    if (!ref)
      HRETURN_ERROR(DFE_BADREF, FAIL);

    /* set number type and subclass	*/
    if (sdg->numbertype == DFNT_NONE)
        DFSDsetNT(DFNT_FLOAT32);     /* default is float32  */
    numtype         = sdg->numbertype;
    fileNTsize      = DFKNTsize(numtype);
    scaleNTsize     = fileNTsize;   /* for now, assume same. MAY CHANGE */
    outNT           = sdg->filenumsubclass;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));
    platnumsubclass = DFKgetPNSC(numtype, DF_MT);

    /* prepare to start writing ndg   */
    if ((GroupID = DFdisetup(10)) < 0)
      return FAIL;

    /* put ND and ref       */
    if (DFdiput(GroupID, sdg->data.tag, sdg->data.ref) < 0)
      return FAIL;

    if (Ref.nt <= 0)
      {   /* will not execute if has been written in putsdg  */
        /* construct and write out NT */
        ntstring[0] = DFNT_VERSION;             /* version */
        ntstring[1] = (uint8)(numtype & 0xff);  /* type */
        ntstring[2] = (uint8)(fileNTsize*8);  /* width of number type in bits */
        ntstring[3] = outNT;          /* class: IEEE or machine class */
        if (Hputelement(file_id, DFTAG_NT, ref, ntstring, (int32) 4) == FAIL)
            return FAIL;
        Ref.nt = ref;
      }

    /* write out NDD (dimension record) */
    if (Ref.dims <= 0)
      {   /* new NDD; write rank, dims, data NT and scale NTs */

        /* put rank & dimensions in buffer */
        bufp = DFtbuf;
        UINT16ENCODE(bufp, sdg->rank);
        for (i=0; i<sdg->rank; i++)
            INT32ENCODE(bufp, sdg->dimsizes[i]);

        /* put data NT and scale NTs  in buffer */
        nt.tag = DFTAG_NT;
        nt.ref = (uint16)Ref.nt;           /* same NT for scales too */
        
        /* "<=" used to put 1 data NT + rank scale NTs in buffer */
        for (i = 0; i <= sdg->rank; i++) 
          {  /* scale NTs written even if no scale!*/
            UINT16ENCODE(bufp, nt.tag);
            UINT16ENCODE(bufp, nt.ref);
          }   
        /* write out NDD record */
        ret = Hputelement(file_id,DFTAG_SDD, ref,DFtbuf,(int32) (bufp-DFtbuf));
        if (ret == FAIL)
            return FAIL;
        Ref.dims = ref;
      }
    /* write dimension record tag/ref */
    if (DFdiput(GroupID, DFTAG_SDD,(uint16) Ref.dims) < 0)
      return FAIL;

    /* write out label/unit/format */
    for (luf = LABEL; luf <= FORMAT; luf++)
      {
        luftag = (uint16)((luf==LABEL) ? DFTAG_SDL :
                (luf==UNIT) ? DFTAG_SDU : DFTAG_SDF);
        bufp   = DFtbuf;
        /* this block of code checks if luf is NULL, else writes it */
        if (!Ref.luf[luf])
          {            /* if luf was set */
            Ref.luf[luf] = -1;          /* assume it is NULL */

            /* if dataluf non-NULL, set up to write */
            if (sdg->dataluf[luf] && sdg->dataluf[luf][0])
              {
                HDstrcpy( (char *)bufp, sdg->dataluf[luf]);
                bufp += HDstrlen(bufp)+1;
              }
            else
              {  /* dataluf NULL */
                *bufp++='\0';
              }

            /* for each dimluf, if non-NULL, set up to write */
            for (i = 0; i < sdg->rank; i++)
              {
                if (sdg->dimluf[luf] && sdg->dimluf[luf][i]
                    && sdg->dimluf[luf][i][0] )
                  {   /* dimluf not NULL */
                        HDstrcpy( (char *)bufp, sdg->dimluf[luf][i]);
                        bufp += HDstrlen(bufp)+1;
                  }
                else
                  {  /* dimluf NULL */
                    *bufp++='\0';
                  }
              }   /* i loop   */
            Ref.luf[luf] = ref; /* remember ref */
            ret = Hputelement(file_id, luftag, (uint16)Ref.luf[luf],
                              DFtbuf, (int32) (bufp-DFtbuf));
            if (ret == FAIL)
              return FAIL;
          }  /* luf was set */

	/* write luf tag/ref */
        if (Ref.luf[luf]>0)
          {
            if (DFdiput(GroupID, luftag, (uint16)Ref.luf[luf]) < 0)
                return FAIL;
          }
      }	/* luf loop	*/

    /* check if there is a scale and write it out */
    if (!Ref.scales)
      {	/* if scale set */
        Isscales = (uint8 *) HDgetspace((uint32) sdg->rank);
        if (Isscales == NULL) 
          return FAIL;
        Ref.scales = (-1);                  /* assume there is no scale */
        
        /* set up Isscales array */
        for (i = 0; i < sdg->rank; i++)
          {
            if (sdg->dimscales && sdg->dimscales[i]) 
              {  /* a scale exists */
                Isscales[i] = 1;
                Ref.scales = 0;             /* flag: write out scales */
              }
            else
              Isscales[i] = 0;
          }
      }

    if (!Ref.scales) {      /* write out scales */
        /* compute space needed for scales */
        len = 0;
        for (i = 0; i < sdg->rank; i++) {
            if (Isscales[i] == 1)
                len += (intn)(sdg->dimsizes[i]*scaleNTsize);
          }
        len += sdg->rank;

        aid = Hstartwrite(file_id, DFTAG_SDS, ref, len);
        if (aid == FAIL )
          {
            HDfreespace((VOIDP)Isscales);
            return FAIL;
          }

        /* write Isscales */
        if (Hwrite(aid, (int32) sdg->rank, Isscales) == FAIL)
          {
            HDfreespace((VOIDP)Isscales);
            return FAIL;
          }

        /* Write scales */
        for (j = 0; j < sdg->rank; j++)
          {
            if (!Isscales[j])
              continue;
            if (platnumsubclass == outNT)
              {       /* no conversion needed */
                if (Hwrite(aid, (int32) (fileNTsize * sdg->dimsizes[j]),
                           (uint8 *) sdg->dimscales[j]) == FAIL)
                  {
                    HDfreespace((VOIDP)Isscales);
                    return FAIL;
                  }
              }
            else
              { /* convert and write */
                /* allocate buffer */
                buf = (uint8 *) HDgetspace((uint32) (fileNTsize * sdg->dimsizes[j]));
                if (buf == NULL)
                  {
                    HDfreespace((VOIDP)Isscales);
                    return FAIL;
                  }
                /* convert, all at once */
                DFKconvert((VOIDP)sdg->dimscales[j], (VOIDP)buf, numtype,
                           sdg->dimsizes[j], DFACC_WRITE, 0, 0);
                /* write it all out */
                if (Hwrite(aid, (int32) (fileNTsize * sdg->dimsizes[j]), buf)
                     == FAIL)
                  {
                    HDfreespace((VOIDP)Isscales);
                    HDfreespace((VOIDP)buf);
                    return FAIL;
                  }
                HDfreespace((VOIDP)buf);
              }
          }

        Ref.scales = ref;
        Hendaccess(aid);
      }
    HDfreespace((VOIDP)Isscales);
    if (Ref.scales > 0)
        if (DFdiput(GroupID, DFTAG_SDS, (uint16) Ref.scales) < 0)
            return FAIL;

    /* write coordsys */
    if (!sdg->coordsys || !sdg->coordsys[0]) Ref.coordsys = (-1);
    if (!Ref.coordsys)
      {
        ret = Hputelement(file_id, DFTAG_SDC, ref, (uint8 *)sdg->coordsys,
                          (int32) (HDstrlen(sdg->coordsys)+1));
        if (ret == FAIL) 
          return FAIL;
        Ref.coordsys = ref;
      }
    if (Ref.coordsys > 0)
      {
        if (DFdiput(GroupID, DFTAG_SDC, (uint16) Ref.coordsys) < 0)
          return FAIL;
      }

    /* write max/min */
    if (!Ref.maxmin)
      {
        if (platnumsubclass == outNT)
          { /* no conversion */
            ret = Hputelement(file_id, DFTAG_SDM, ref,
                              (uint8 *) &(sdg->max_min[0]),
                              (int32) (2 * fileNTsize));
            if (ret == FAIL)
              return FAIL;
            Ref.maxmin = ref;
          }
        else
          {
	    /* allocate buffer */
            buf = (uint8 *) HDgetspace((uint32) 2*fileNTsize); /* max/min is 8 bytes */
            if (buf == NULL)
              return FAIL;

	    /* convert */
            DFKconvert((VOIDP) &(sdg->max_min[0]), (VOIDP)buf,
                       numtype, 2, DFACC_WRITE, 0, 0);

	    /* write */
            ret = Hputelement(file_id, DFTAG_SDM, ref, buf,
                              (int32) (2*fileNTsize));

            if (ret == FAIL)
              {
                HDfreespace((VOIDP)buf);
                return FAIL;
              }

            Ref.maxmin = ref;
            HDfreespace((VOIDP)buf);
          }
      }
    if (Ref.maxmin > 0)
      {
        if (DFdiput(GroupID, DFTAG_SDM, (uint16) Ref.maxmin) < 0)
            return FAIL;
      }
    Ref.maxmin = (-1);  /* max/min should be reset for each data set */

    /* Write calibration. */
    if (!Ref.cal)
      {
        if (platnumsubclass == outNT) 
          {     /* no conversion */
            if (Hputelement(file_id, DFTAG_CAL, ref,
                            (unsigned char *) &sdg->cal,
                            (int32) 36)<0)
                return(-1);
            Ref.cal = ref;
          }
        else
          {
            /* allocate buffer */
            uint8 *buf;

            /* allocate translation buffer */
            buf = (uint8 *) HDgetspace((uint32)
                                       4 * sizeof(float64) +
                                       1 * sizeof(int32));
            if(buf == NULL)
              return FAIL;

            /* convert doubles */            
            DFKconvert((VOIDP) &sdg->cal, (VOIDP)buf, 
                       DFNT_FLOAT64, 4, DFACC_WRITE, 0, 0);

            /* convert int */
            DFKconvert((VOIDP) &sdg->cal_type, (VOIDP)(buf + 32),
                       DFNT_INT32, 1, DFACC_WRITE, 0, 0);
            
            /* write it into the file */
            if (Hputelement(file_id, DFTAG_CAL, ref,
                            (unsigned char *) buf,
                            (int32) 36) < 0) 
                return(FAIL);
            Ref.cal = ref;
            HDfreespace((VOIDP)buf);
            
          }
      }
    
    if (Ref.cal > 0)
      {
        if (DFdiput(GroupID, DFTAG_CAL, (uint16) Ref.cal) < 0)
            return(FAIL);
      }
    Ref.cal = (-1);        /* Calibration should be reset for each data set */

    /* Write fill value.  */
    if (!Ref.fill_value)
      {
        if (platnumsubclass == outNT)
          {     /* No conversion  */
            if (Hputelement(file_id, DFTAG_FV, ref,
                            (unsigned char *) sdg->fill_value,
                            (int32) fileNTsize) == FAIL)
                return(FAIL);
            Ref.fill_value = ref;
          }
        else
          {
            /* Allocate buffer  */
            uint8 buf[DFSD_MAXFILL_LEN];

            /* Convert from native to IEEE  */
            DFKconvert((VOIDP) sdg->fill_value, (VOIDP)buf,
                       numtype, 1, DFACC_WRITE, 0, 0);

            /* Write it into the file  */
            if (Hputelement(file_id, DFTAG_FV, ref,
                            (unsigned char *) buf,
                            (int32) fileNTsize) == FAIL)
                return(FAIL);

            Ref.fill_value = ref;
          }
      }

    /* Check to add to DFgroup  */
    if (Ref.fill_value > 0)
      {
        if (DFdiput(GroupID, DFTAG_FV, (uint16) Ref.fill_value) == FAIL)
            return(FAIL);
      }
    Ref.fill_value = (-1); /* Fill value should be reset for each data set  */

    if (!Ref.transpose)
      {  /* if transposed, add transpose tag */
        if (Hdupdd(file_id, DFTAG_SDT, ref, DFTAG_SDD, ref) == FAIL)
            return FAIL;
        Ref.transpose = ref;
      }
    if (Ref.transpose > 0)
      {
        if (DFdiput(GroupID, DFTAG_SDT, (uint16) Ref.transpose) < 0)
            return FAIL;
      }

    if (numtype == DFNT_FLOAT32)
      {  /* if float32, add a DFTAG_SDLNK   */
        DFdi lnkdd[2];

        issdg        = 1;
        lnkdd[0].tag = DFTAG_NDG;
        lnkdd[0].ref = ref;
        lnkdd[1].tag = DFTAG_SDG;
        lnkdd[1].ref = ref;
        bufp = DFtbuf;

        for (i = 0; i < 2; i++)
          {
            UINT16ENCODE(bufp, lnkdd[i].tag);
            UINT16ENCODE(bufp, lnkdd[i].ref);
          }
        ret = Hputelement(file_id, DFTAG_SDLNK, ref,
                          DFtbuf,(int32) (bufp-DFtbuf));
        if (ret == FAIL)
          return FAIL;

	/* write DFTAG_SDLNK  */
        if (DFdiput(GroupID, DFTAG_SDLNK, ref) < 0)
          return FAIL;
      }

    /* write out NDG */
    if (DFdiwrite(file_id, GroupID, DFTAG_NDG, ref) < 0)
      return FAIL;

    /* write an SDG point to the dataset if it is an NDG SDG  */
    if (issdg)
      {
        if (Hdupdd(file_id, DFTAG_SDG, ref, DFTAG_NDG, ref) < 0)
          {
            Hclose(file_id);
            return FAIL;
          }
      }

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIendslice
 * Purpose: Write of data to SDG completed, write SDG and close file
 * Inputs:  isfortran: true if called from Fortran
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDIputdata
 * Invokes: DFSDputsdg, Hclose, HERROR
 * Method:  call DFSDputsdg, close Sfile_id
 * Remarks: checks that slice writes were completed.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIendslice(intn isfortran)
#else
intn DFSDIendslice(isfortran)
    intn isfortran;
#endif /* PROTOTYPE */
{
    intn i;
    intn ret;
    char *FUNC="DFSDIendslice";

    HEclear();

    if (Sfile_id == DF_NOFILE) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    /* check if slice writes complete */
    for (i = 0; i < Writesdg.rank; i++) 
      {
        if (!Fortorder && (i == 0) && (Sddims[i] == Writesdg.dimsizes[i]))
          continue;
        if ((isfortran||Fortorder) && (i == Writesdg.rank - 1) 
            && (Sddims[i] == Writesdg.dimsizes[i])) 
          continue;
        if ((isfortran||Fortorder || i > 0) 
            && (!Fortorder || i < Writesdg.rank - 1) && (Sddims[i] == 0)) 
          continue;

        HRETURN_ERROR(DFE_BADCALL, FAIL);
      }

    if (DFSDIputndg(Sfile_id, Writeref, &Writesdg) < 0)  
      {
        Hclose(Sfile_id); 
        return FAIL;
      }

    /* old nsdg table should be reset next time  */
    if (nsdghdr != NULL)	
      {
	if (nsdghdr->nsdg_t != NULL) 	
          {
	    DFnsdgle *rear, *front;

	    rear  = nsdghdr->nsdg_t;
	    front = rear->next;
	    while (rear != NULL)	
              {
	       if ((rear = (DFnsdgle *)HDfreespace((VOIDP) rear)) != NULL)
     	         return FAIL;
 	       rear = front;
	       if (rear != NULL) front = rear->next;
	      }
	    nsdghdr->size   = 0;
	    nsdghdr->nsdg_t = NULL;
	    lastnsdg.tag    = DFTAG_NULL;
	    lastnsdg.ref    = 0;
          }
        if ((nsdghdr=(DFnsdg_t_hdr *)HDfreespace((VOIDP)nsdghdr)) != NULL)
          return FAIL;
      }
	
    Lastref  = Writeref;	/* remember ref written */
    Writeref = 0;		/* don't know ref to write next */

    Hendaccess(Writesdg.aid);
    ret      = Hclose(Sfile_id);
    Sfile_id = 0;       /* partial write complete */
    Sddims   = (int32 *) HDfreespace((VOIDP) Sddims);

    return(ret);
}

/******************************************************************************/
/*----------------------- Internal routines ---------------------------------*/
/******************************************************************************/

/*-----------------------------------------------------------------------------
 * Name:    DFSDIopen
 * Purpose: open or reopen a file
 * Inputs:  filename: name of file to open
 *          access : access mode
 * Returns: file id on success, -1 (FAIL) on failure with error set
 * Users:   HDF systems programmers, many SD routines
 * Invokes: DFopen
 * Remarks: This is a hook for someday providing more efficient ways to
 *          reopen a file, to avoid re-reading all the headers
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFSDIopen(char *filename, intn access)
#else
int32 DFSDIopen(filename, access)
     char *filename;
     intn access;
#endif /* PROTOTYPE */
{
    int32 file_id;
    char *FUNC="DFSDIopen";

    if (Sfile_id!=DF_NOFILE)      /* in the middle of a partial write */
        HRETURN_ERROR(DFE_ALROPEN, FAIL); 

    /* use reopen if same file as last time - more efficient */
    if ((HDstrcmp(Lastfile,filename)) || (access == DFACC_CREATE)) 
      {
        /* open a new file, delete nsdg table and reset lastnsdg  */
        if (nsdghdr != NULL) 	
          {
            if (nsdghdr->nsdg_t != NULL) 	
              {
                DFnsdgle *rear, *front;

                rear = nsdghdr->nsdg_t;
                while (rear != NULL)
                  {
                    front = rear->next;
                    if ((rear = (DFnsdgle *)HDfreespace((VOIDP) rear)) != NULL)
                        return FAIL;
                    rear = front;
                  }
                nsdghdr->size   = 0;
                nsdghdr->nsdg_t = NULL;
                lastnsdg.tag    = DFTAG_NULL;
                lastnsdg.ref    = 0;
              }
            if ((nsdghdr=(DFnsdg_t_hdr *)HDfreespace((VOIDP)nsdghdr)) != NULL)
                return FAIL;
          }

        /* treat create as different file */
        file_id = Hopen(filename, access, (int16) 0);
        if (file_id == FAIL) 
            return FAIL;
        Newdata  = (-1);         /* data in Readsdg is not fresh */ 
        Readsdg.data.ref = 0;    /* No SDG read yet */

        /* remember no info written to file */
        Ref.scales      = (Ref.scales  >= 0) ? 0 : Ref.scales;
        Ref.luf[LABEL]  = (Ref.luf[LABEL]  >= 0) ? 0 : Ref.luf[LABEL];
        Ref.luf[UNIT]   = (Ref.luf[UNIT]   >= 0) ? 0 : Ref.luf[UNIT];
        Ref.luf[FORMAT] = (Ref.luf[FORMAT] >= 0) ? 0 : Ref.luf[FORMAT];
        Ref.dims      = (Ref.dims    >= 0) ? 0 : Ref.dims;
        Ref.coordsys  = (Ref.coordsys >= 0) ? 0 : Ref.coordsys;
        Ref.maxmin    = (Ref.maxmin >= 0) ? 0 : Ref.maxmin;
        Ref.nt        = (Ref.nt >= 0) ? 0 : Ref.nt;
        Ref.transpose = (Ref.transpose >= 0)? 0 : Ref.transpose;
      }
    else
      {
        file_id = Hopen(filename, access, (int16) 0);
        if (file_id == FAIL)
            return FAIL;
      }

    /* if read, set up nsdg table */
    if (nsdghdr == NULL)
      {
        nsdghdr = (DFnsdg_t_hdr *)HDgetspace((uint32)sizeof(DFnsdg_t_hdr));
        if (nsdghdr == NULL)       
            HRETURN_ERROR(DFE_NOSPACE, FAIL); 
        nsdghdr->size   = 0;
        nsdghdr->nsdg_t = NULL;
      }
    if ((nsdghdr->nsdg_t == NULL) && (access == DFACC_READ))  
      {
        if (DFSDIsetnsdg_t(file_id,nsdghdr ) < 0) 
          return(FAIL);
        lastnsdg.tag = DFTAG_NULL;
        lastnsdg.ref = 0;
      }

    HIstrncpy(Lastfile, filename, DF_MAXFNLEN);
    /* remember filename, so reopen may be used next time if same file*/

    return(file_id);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIsdginfo
 * Purpose: Locates next sdg in file
 * Inputs:  file_id: pointer to DF file
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF systems programmers, DFSDgetdims, DFSDgetdata
 * Invokes: DFIfind, DFSDIgetndg
 * Method:  Call DFIfind to find SDG, then DFSDIgetndg to read it in to Readsdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIsdginfo(int32 file_id)
#else
intn DFSDIsdginfo(file_id)
     int32 file_id;
#endif /* PROTOTYPE */
{
    DFdi ptr;
    char *FUNC="DFSDIsdginfo";
    int32 aid;

    HEclear();

    if (!HDvalidfid(file_id))
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    if (Readref != 0)
      {
        aid = Hstartread(file_id, DFTAG_NDG, Readref);
        if (aid  != FAIL)
          {
            ptr.ref = Readref;
            ptr.tag = DFTAG_NDG;
            Hendaccess(aid);
          }
        else
          {
            aid = Hstartread(file_id, DFTAG_SDG, Readref);
            if (aid != FAIL)
              {
                ptr.ref = Readref;
                ptr.tag = DFTAG_SDG;
                Hendaccess(aid);
              }
            else
              return FAIL;
          }
      }
    else
      {
        if (DFSDInextnsdg(nsdghdr, &ptr) < 0)
          return FAIL;
        if ((ptr.tag != DFTAG_NDG) && (ptr.tag != DFTAG_SDG))
            HRETURN_ERROR(DFE_BADTAG, FAIL);
        if (ptr.ref <=0)
            HRETURN_ERROR(DFE_BADREF, FAIL);
        Readref = ptr.ref;
      }

    /* find next sd object */
    if (DFSDIgetndg(file_id, ptr.tag, ptr.ref, &Readsdg) <0)
        return FAIL;

    /* remember what type of thing we just read */
    Readsdg.isndg = (ptr.tag == DFTAG_NDG) ? 1 : 0;

    Lastref      = ptr.ref;           /* remember ref read */
    lastnsdg.tag = ptr.tag;
    lastnsdg.ref = ptr.ref;

    Newdata = 1;      /* now Readsdg is fresh */
    Readref = 0;

    return(0);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIrefresh
 * Purpose: get next sdg if Readsdg is not fresh
 * Inputs:  filename
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF systems programmers, functions in dfsdF.c
 * Invokes: DFSDIopen, DFSDIsdginfo
 * Method:  test Newdata and Nextsdg, call DFSDIsdginfo if necessary
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIrefresh(char *filename)
#else
intn DFSDIrefresh(filename)
     char *filename;
#endif /* PROTOTYPE */
{
      int32 file_id;
      char *FUNC="DFSDgetdims";

      HEclear();
      if (Newdata != 1 || Nextsdg) 
        { /* if Readsdg not fresh  */
          file_id = DFSDIopen(filename, DFACC_READ); /* open/reopen file */
          if (file_id == FAIL)
              return FAIL;
          if (DFSDIsdginfo(file_id) < 0) 
            {      /* reads next SDG from file */
              Hclose(file_id); 
              return FAIL;   
		/* on error, close file and return */
            }
          if (Hclose(file_id) < 0) 
            return FAIL;
          Nextsdg = 0;
        }
     return (SUCCEED);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIisndg
 * Purpose: is the current read sds an sdg or nsdg/ndg
 * Inputs:  isndg: 0 -- pure sdg( written by 3.1); 1 -- nsdg/ndg 
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF systems programmers, functions in dfsdF.c
 * Invokes: none
 * Method:  Assigns Readsdg.isndg to isndg.
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIisndg(intn *isndg)
#else
intn DFSDIisndg(isndg)
     intn *isndg;
#endif /* PROTOTYPE */
{
    *isndg = (intn)Readsdg.isndg;
    return (SUCCEED);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIgetrrank
 * Purpose: get rank of the current sdg, to transpose dims for Fortran
 * Inputs:  &rank: address to return the rank
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF systems programmers, functions in dfsdF.c
 * Invokes: none
 * Method:  Assigns Readsdg.rank to rank.
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIgetrrank(intn *rank)
#else
intn DFSDIgetrrank(rank)
     intn *rank;
#endif /* PROTOTYPE */
{
    *rank = (intn)Readsdg.rank;
    return (SUCCEED);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIgetwrank
 * Purpose: get rank of the current sdg, to transpose dims for Fortran
 * Inputs:  &rank: address to return the rank
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF systems programmers, functions in dfsdF.c
 * Invokes: none
 * Method:  Assigns Readsdg.rank to rank.
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIgetwrank(intn *rank)
#else
intn DFSDIgetwrank(rank)
     intn *rank;
#endif /* PROTOTYPE */
{
    *rank = (intn)Writesdg.rank;
    return (SUCCEED);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIclear
 * Purpose: Reset all "set" values, free allocated space
 * Inputs:  sdg: pointer to sdg struct to clear
 * Globals: Ref
 * Returns: 0 on success, FAIL on error with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Release space in sdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIclear(DFSsdg *sdg)
#else
intn DFSDIclear(sdg)
     DFSsdg *sdg;
#endif /* PROTOTYPE */
{
    intn i;
    intn luf;
    char *FUNC="DFSDIclear";

    HEclear();

    if (Sfile_id !=DF_NOFILE)      /* cannot clear during slice writes */
        HRETURN_ERROR(DFE_BADCALL, FAIL); 

    sdg->dimsizes = (int32 *) HDfreespace((VOIDP) sdg->dimsizes);
    sdg->coordsys = HDfreespace((VOIDP)sdg->coordsys);

    /* free label/unit/format pointers */
    for (luf = LABEL; luf <= FORMAT; luf++)
      {
        if (sdg->dimluf[luf])
          {       /* free strings */
            for (i = 0; i <sdg->rank; i++)
                sdg->dimluf[luf][i] = HDfreespace((VOIDP)sdg->dimluf[luf][i]);
          }

	/* free string pointers */
        sdg->dimluf[luf] = (char **) HDfreespace((VOIDP) sdg->dimluf[luf]);

	/* free data string */
        sdg->dataluf[luf] = HDfreespace((VOIDP)sdg->dataluf[luf]);
      }

    /* free scale pointers */
    if (sdg->dimscales)
      {
        for (i = 0; i < sdg->rank; i++)
            sdg->dimscales[i] = 
              (uint8 *)HDfreespace((VOIDP) sdg->dimscales[i]);
      }

    /* free array of scale pointers */
    sdg->dimscales = (uint8 **) HDfreespace((VOIDP)sdg->dimscales);
    sdg->rank      = 0;

    /* number type is independant to dimsizes   4/7/92  sxu
    sdg->numbertype = DFNT_NONE;
    sdg->filenumsubclass = DFNTF_NONE;
    */
    sdg->aid         = (int32)-1;
    sdg->compression = (int32)0;
    FileTranspose    = 0;
    sdg->fill_fixed = FALSE;    /* allow fill_value to be changed */

    Ref.dims       = -1;
    Ref.scales     = Ref.luf[LABEL] = Ref.luf[UNIT] = Ref.luf[FORMAT] = (-1);
    Ref.coordsys   = Ref.maxmin = (-1);
    Ref.new_ndg    = -1;
    Ref.fill_value = -1;

    return(SUCCEED);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIgetdata
 * Purpose: Get data from SDG.  Will sequence to next SDG if DFSDgetdims not
 *          called.
 * Inputs:  filename: name of HDF file to use
 *          rank: no of dimensions of array "data"
 *          maxsizes: actual dimensions of array "data"
 *          data: data for returning scientific data
 *          isfortran : 0 if called from C, 1 when called from FORTRAN
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: actual scientific data in array
 * Users:   DFSDgetdata
 * Invokes: DFSDIgetslice, HDgetspace, HDfreespace, DFSDIopen, Hclose,
 *          HERROR, DFSDIsdginfo
 * Method:  Open file, call DFSDIsdginfo to read sdg if necessary, set up
 *          window start and end arrays, call DFSDIgetslice.
 * Remarks: maxsizes may be larger than actual size.  In that event, the actual
 *          data may not be contiguous in the array "data"
 *          User sets maxsizes before call.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIgetdata(char *filename, intn rank, int32 maxsizes[], VOIDP data,
	          intn isfortran)
#else
int DFSDIgetdata(filename, rank, maxsizes, data, isfortran)
     char *filename;
     intn rank;
     int32 maxsizes[];
     VOID *data;
     intn isfortran;
#endif /* PROTOTYPE */
{
    intn i;
    intn ret;
    int32 *winst;
    int32 *windims;
    int32 file_id;
    char *FUNC="DFSDIgetdata";

    HEclear();

    if (Newdata != 1 || Nextsdg) 
      { /* if Readsdg not fresh */
        file_id = DFSDIopen(filename, DFACC_READ);
        if (file_id == DF_NOFILE)
          return FAIL;
        if (DFSDIsdginfo(file_id) < 0)  
          {   /* reads next SDG from file */
            Hclose(file_id); 
            return FAIL;
          }
        if (Hclose(file_id) == FAIL) 
          return FAIL;
      }

    winst = (int32 *) HDgetspace((uint32) Readsdg.rank * sizeof(int32));
    if (winst == NULL) 
      return FAIL;

    windims = (int32 *) HDgetspace((uint32) Readsdg.rank * sizeof(int32));
    if (windims == NULL) 
      {
	HDfreespace((VOIDP) winst);
	return FAIL;
      }

    for (i = 0; i < rank; i++) 
      {
        winst[i]   = 1;
        windims[i] = Readsdg.dimsizes[i];
      }

    ret = DFSDIgetslice(filename, winst, windims, data, maxsizes, isfortran);
    Nextsdg = 1;
    HDfreespace((VOIDP) winst);
    HDfreespace((VOIDP) windims);

    return(ret);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDIputdata
 * Purpose: Writes entire SDG to file
 * Inputs:  filename: name of HDF file to use
 *          rank: rank of data array
 *          dimsizes: sizes of the dimensions of data array
 *          data: array that holds data
 *          accmode: 0 if write to new file, 1 if append to file
 *          isfortran: 0 if C, 1 if FORTRAN
 * Globals: Writeref
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFSDIopen, Hclose, HDgetspace, HDfreespace, DFSDIputslice,
 *          DFSDstartslice, DFSDIendslice
 * Method:  Create file if necessary, allocate arrays, call slice routines
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIputdata(char *filename, intn rank, int32 *dimsizes, VOIDP data,
	          intn accmode, intn isfortran)
#else
intn DFSDIputdata(filename, rank, dimsizes, data, accmode, isfortran)
     char *filename;
     intn  rank;
     int32   *dimsizes;
     VOID *data;
     intn accmode;
     intn isfortran;
#endif /* PROTOTYPE */
{
    intn ret;
    int32 file_id;
    char *FUNC="DFSDIputdata";

    HEclear();

    if (!accmode) 
      {                             /* new file */
        file_id = DFSDIopen(filename, DFACC_CREATE);
        if (file_id == DF_NOFILE) 
          return FAIL;
        if (Hclose(file_id) == FAIL) 
          return FAIL;
      }

    if (Ref.dims) 
      {       /* don't call setdims if already called */
        if (DFSDsetdims(rank, dimsizes) < 0) 
          return FAIL;
      }

    if (DFSDstartslice(filename) < 0) 
      return FAIL;

    if ((ret=DFSDIputslice(Writesdg.dimsizes, data, dimsizes, isfortran)) < 0)
        return ret;

    return DFSDIendslice(isfortran);
}

/*----------------------------------------------------------------------------
 * Name:    DFSDIgetslice
 * Purpose: Get slice of data from SDG.  Will sequence to next SDG if
 *          DFSDgetdims, DFSDgetdata or DFSDgetslice not called earlier.
 * Inputs:  filename: name of HDF file to use
 *          winst: array of size = rank of data, containing start of slice
 *          windims: array of size rank, containing size of slice
 *          data: array for returning slice
 *          dims: dimensions of array data
 *          isfortran : 0 if called from C, 1 when called from FORTRAN
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: slice of data in data
 * Users:   DFSDIgetdata
 * Invokes: DFSDIopen, Hclose, HERROR, DFSDIsdginfo, DFaccess, DFread
 * Method:  Open file, call DFSDIsdginfo to read sdg if necessary, read the
 *          data, convert types if necessary, place in data as appropriate
 *          data is assumed column major for FORTRAN, row major for C
 * Remarks: dims may be larger than size of slice.  In that event, the actual
 *          data may not be contiguous in the array "data".
 *          User sets dims before call.
 *--------------------------------------------------------------------------*/

/*****************************************************************************/
/* DESIGN DECISIONS                                                          */
/*****************************************************************************/
/* 
   A. All stride/index/offset value will, when this is done -- refer to
      element counts rather than byte counts in the name of consistency.

   B. The conversion buffers/allcated areas... will all be char buffers --
      providing that the Cray-2 is cooperative.
									     */	
/*****************************************************************************/

/*****************************************************************************/
/* CHANGE LOG                                                                */
/*****************************************************************************/
/*
  A.
									     */
/*****************************************************************************/

#ifdef PROTOTYPE
intn DFSDIgetslice(char *filename, int32 winst[], int32 windims[],
	           VOIDP data, int32 dims[], intn isfortran)
#else
intn DFSDIgetslice(filename, winst, windims, data, dims, isfortran)
    char    *filename;  /* HDF file containing the dataset */
    int32 winst[];      /* array containing the coordinates of the start */
                        /*  of the slice in the HDF file */
    int32 windims[];	/* array containing the size of the slice */
    int32 dims[];	/* array containing the dimensions of data[] */
    VOID  *data;	/* array to hold the floating point data read*/
    intn  isfortran;	/* true if called from Fortran */
#endif /* PROTOTYPE */
{
    intn  rank;           /* number of dimensions in data[] */
    int32 leastsig;       /* fastest varying subscript in the array */
    int32 error;          /* flag if an error occurred, */
                          /*  used by DFconvert macro */
    int32 convert;        /* true if machine NT != NT to be read */
    int32 transposed;     /* true if we must transpose the data before writing */
    int32 done;           /* true if we are at the end of the slice */
    int32 aid;
    int32 i, j;           /* temporary loop index */
    int32 issdg;          /* 1 -- pure sdg. do what HDF3.1 does   */
    int32 *wstart;        /* tmp array containing starting slice dims */
    int32 *wdims;         /* tmp array containing the slice size */
    int32 *adims;         /* tmp array containing the dimensions of data[] */
    int32  *fdims;        /* tmp array containing the dimensions */
                          /*  of the dataset in the file */
    int32 numtype;        /* current number type  */
    int32 fileNTsize;     /* size of this NT in the file  */
    int32 localNTsize;    /* size of this NT as it occurs in this machine */
    int32 numelements;    /* number of floats to read at once */
    int32 readsize;       /* number of bytes to read at once */
    int32 datastride;     /* number of floats in one row of data[] */
    int32 *offset;        /* array for accessing the next element in data[] */
    int32 *foffset;       /* array for accessing the next element in the file */
    int32 *dimsleft;      /* array for tracking the current position in data[] */
    int32 isnative;
    int32 fileoffset;     /* offset into the current dataset in the file */
    int32 machinetype;    /* assigned DF_MT.  used for debugging */
    uint8 platnumsubclass; /* class of this NT for this platform */
    uint8 fileNT;         /* file number subclass */
    uint8 *scatterbuf;    /* buffer to hold the current row contiguously */
    uint8 *sp;            /* ptr into scatterbuf      */
    uint8 *datap;         /* ptr into data[] at starting offset */
                            /* of current block */
    uint8 *dp;            /* ptr into data[] at an element of the current row */
    uint8 *buf;           /* buffer containing the converted current row */
    int32 file_id;        /* HDF file pointer */
    char *FUNC="DFSDIgetslice";

    HEclear();

    if (data == NULL)
        HRETURN_ERROR(DFE_BADPTR, FAIL);

    file_id = DFSDIopen(filename, DFACC_READ);
    if (file_id == DF_NOFILE)
      return FAIL;

    if (Newdata != 1)
      {		/* if Readsdg not fresh */
        if (DFSDIsdginfo(file_id) < 0)
          { /* reads next SDG from file */
            Hclose(file_id);
            return FAIL;
          }
      }
    rank        = Readsdg.rank;
    numtype     = Readsdg.numbertype;
    fileNT      = Readsdg.filenumsubclass;
    issdg       = Readsdg.isndg? 0: 1; 
    isnative    = DFNT_NATIVE;
    machinetype = DF_MT;
    localNTsize = DFKNTsize((numtype | isnative) & (~DFNT_LITEND));
    fileNTsize  = DFKNTsize(numtype);
    platnumsubclass = DFKgetPNSC(numtype & (~DFNT_LITEND), DF_MT);

    /* get dimensions of slice to extract, set nwindims. also err check */
    for (i = 0; i < (int32)rank; i++)
      {
    	/* check validity for the dimension ranges */
        if ((windims[i] < 1) || (winst[i] < 1)
            || (winst[i]+windims[i]-1 > Readsdg.dimsizes[i])) 
          {
            HCLOSE_RETURN_ERROR(file_id, DFE_BADDIM, FAIL);
          }
        /* check if space allocated is sufficient */
        if (dims[i] < windims[i])
          {
            HCLOSE_RETURN_ERROR(file_id, DFE_NOTENOUGH, FAIL);
          }
      }
    /* allocate buffers */
    wstart = (int32 *) HDgetspace((uint32) 4 * rank * sizeof(int32));
    if (wstart == NULL)
      {
        HCLOSE_RETURN_ERROR(file_id, DFE_NOSPACE, FAIL);
      }
    wdims = wstart + rank;
    adims = wdims + rank;
    fdims = adims + rank;

    /* copy arrays to private workspace (so that they are in row major order)*/
    for (i = 0; i < (int32)rank; i++)
      {
        int32 ii = (issdg && isfortran)? rank-i-1 : i;

        adims[i]  = dims[ii];
        ii        = (issdg && FileTranspose)? rank-i-1 : i;
        wstart[i] = winst[ii]-1; /* translate to 0 origin */
        wdims[i]  = windims[ii];
        fdims[i]  = Readsdg.dimsizes[ii];
      }

    convert    = (fileNT != platnumsubclass); /* is conversion necessary */
    transposed = issdg && (isfortran ^ FileTranspose); /* is transposition needed */

    /*
     * Note that if the data is transposed we must work on a row by row
     * basis and cannot collapse dimensions.
     */
    if (!transposed)
      {
        /* collapse dimensions if contiguous both in the file and in memory */
        for (i =(int32)rank - 1; i > 0; i--) 
          { /* stop before most sig dim */
            if (adims[i] > wdims[i] /* not all of data[] will be filled */
                || wstart[i] != 0 /* reading only part of the dataset */
                || wdims[i] < fdims[i]) 
              {
                break;
              }
            wstart[i-1] *= fdims[i];
            wdims[i-1]  *= wdims[i];
            adims[i-1]  *= adims[i];
            fdims[i-1]  *= fdims[i];
            rank--;
          }
      }
    leastsig = (int32)rank - 1;		/* which is least sig dim */

    /* position at start of data set */
    aid = Hstartread(file_id, Readsdg.data.tag, Readsdg.data.ref);
    if (aid == FAIL)
      {
        HDfreespace((VOIDP)wstart);
        Hclose(file_id);
        return FAIL;
      }

    error = 0;
    if (rank == 1 && !convert) 
      {
        /* all data is contiguous with no conversions */
        readsize = adims[0] * fileNTsize;
        if ((Hseek(aid, wstart[0]*fileNTsize, 0) == FAIL) 
            || (readsize != Hread(aid, readsize, (uint8 *)data)) ) 
          {
            error = 1;
          }
      } 
    else 
      {
	/*
	 * The data must be further manipulated.
	 * It may be transposed, may need conversion, may not be contiguous, or
	 * any combination of these.
	 */
        numelements  = wdims[leastsig];
        readsize     = numelements * fileNTsize;

        /* allocate 1 row buffers */
        if (convert) 
          {
            if ((buf = (uint8 *) HDgetspace((uint32) readsize)) == NULL) 
              {
                HDfreespace((VOIDP)wstart);
                Hendaccess(aid);
                HCLOSE_RETURN_ERROR(file_id, DFE_NOSPACE, FAIL);
              }
          } 
        else 
          buf = NULL;

        if (transposed) 
          {
            scatterbuf = 
              (uint8 *)HDgetspace((uint32) numelements * localNTsize);

            if (scatterbuf == NULL) 
              {
                HDfreespace((VOIDP)wstart);
                HDfreespace((VOIDP)buf);
                Hendaccess(aid);
                HCLOSE_RETURN_ERROR(file_id, DFE_NOSPACE, FAIL);
              }
          } 
        else 
          scatterbuf = NULL;

        offset = (int32 *) HDgetspace((uint32)3 * rank * sizeof(int32));
        if (offset == NULL) 
          {
            HDfreespace((VOIDP)wstart);
            HDfreespace((VOIDP)buf);
            HDfreespace((VOIDP)scatterbuf);
            Hendaccess(aid);
            HCLOSE_RETURN_ERROR(file_id, DFE_NOSPACE, FAIL);
          }
        foffset  = offset + rank;
        dimsleft = foffset + rank;

        /* compute initial position in the data */
        for (i = leastsig; i >= 0; i--)
            dimsleft[i] = wdims[i];

        /* compute offsets in the source array */
        if (transposed) 
          {
            offset[0] = 1*localNTsize;
            for (i = 0; i < leastsig; i++)
              offset[i+1] = offset[i] * adims[leastsig - i];
          } 
        else 
          {
            offset[leastsig] = 1*localNTsize;
            for (i = leastsig; i > 0; i--)
              offset[i-1] = offset[i] * adims[i];
          }
        datastride = offset[leastsig];

        /* compute offsets in the file */
        for (i = leastsig, foffset[i] = 1 * fileNTsize; i > 0; i--)
            foffset[i-1] = foffset[i] * fdims[i];

        /*
         * Compute starting position in file
         * All file reads are done relative to this starting offset.
         * Cumulative offset is from most sig to next to least sig dim.
         */
        for (i = 0, fileoffset = 0; i < leastsig; i++)
            fileoffset = (fileoffset+wstart[i]) * fdims[i+1];
        fileoffset += wstart[leastsig]; /* adjust for last dim */
        fileoffset *= fileNTsize; /* convert to bytes */

        datap = (uint8 *)data;
        done = 0;
        /* -- now read in the data */
        do {
            /* move to the next data element in the file */
            if (Hseek(aid, fileoffset, 0) == FAIL) 
              {
                error=1;
                break;
              }

            /* read and convert one contiguous block of data */
            if (convert) 
              {
                if (readsize != Hread(aid, readsize, buf)) {
                    error=1;
                    break;
                }
                DFKconvert((VOIDP)buf, transposed ? (VOIDP)scatterbuf :
                    (VOIDP)datap, numtype, numelements, DFACC_READ, 0, 0);
              }
            else 
              {
                if (readsize != Hread(aid, readsize,
                                      transposed ? scatterbuf : datap)) 
                  {
                    error=1;
                    break;
                  }
              }
            if (transposed) 
              {
            /* scatter out the elements of one row */
#ifdef UNICOS
#pragma ivdep
#endif
                for (dp = datap, sp = scatterbuf, i = 0; i < numelements; i++)
                  {                
                    for (j = 0; j < localNTsize; j++)
                        *(dp +j) = *(sp +j);
                    sp += localNTsize;
                    dp += datastride;
                  }
              }

            /*
             * Find starting place of the next row/block.
             * Note that all moves are relative:
             *   this preserves the starting offsets of each dimension
             */
            for (i = leastsig - 1; i >= 0; i--) 
              {
                if (--dimsleft[i] > 0) 
                  {
                    /* move to next element in the current dimension */
                    datap      += offset[i];
                    fileoffset += foffset[i];
                    break;
                  } 
                else 
                  {
                    dimsleft[i] = wdims[i];
                    /*
                     * Note that we are still positioned at the beginning of
                     * the last element in the current dimension
                     */
                    /* move back to the beginning of dimension i */
                    datap -= offset[i] * (wdims[i] - 1);
                    /* move back to beginning read position of dimension i */
                    fileoffset -= foffset[i] * (wdims[i] - 1);
                    if (i == 0)
                      done = 1;
                  }
              }
        } while (!done && leastsig > 0);

        HDfreespace((VOIDP)buf);
        HDfreespace((VOIDP)scatterbuf);
        HDfreespace((VOIDP)offset);
      }

    Hendaccess(aid);
    HDfreespace((VOIDP)wstart);
    if (error)
      {
        Hclose(file_id);
        return FAIL;
      }
    else
      return (Hclose(file_id));
}

/*----------------------------------------------------------------------------
 * Name:    DFSDIputslice
 * Purpose: Put slice of data to SDG.
 * Inputs:  windims: array of size rank, containing size of slice
 *          data: array containing slice
 *          dims: dimensions of array data
 *	    	isfortran: 0 for C, 1 for Fortran
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDputslice
 * Invokes: DFwrite, HDgetspace, HDfreespace,DFKnumout(if conversion
	    required)
 * Method:  check dimensions for contiguity, convert types if necessary
 *          write to file
 * Remarks: dims may be larger than size of slice.  In that event, the actual
 *          data may not be contiguous in the array "data".
 *          DFSDstartslice must have been called first
 *          If DFKnumout is called, DFSDsetNT may need to have been
 *	    called
 *	    Note, writes must be contiguous - successive calls to putslice
 *          must write out array consecutively, according to the setting
 *          of the Fortorder variable - row major if 0, column major if 1
 *--------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDIputslice(int32 windims[], VOIDP data, int32 dims[], intn isfortran)
#else
intn DFSDIputslice(windims, data, dims, isfortran)
    int32 windims[];  /* array containing dimensions of the slice */
    int32 dims[];     /* array containing the dimensions of data[] */
    VOID  *data;      /* array of the floating point data to write */
    intn  isfortran;  /* true if called from Fortran */
#endif /* PROTOTYPE */
{
    intn rank;           /* number of dimensions in data[] */
    int32 leastsig;      /* fastest varying subscript in the array */
    int32 convert;       /* true if machine NT = NT to be written */
    int32 contiguous;    /* true if there are no gaps in the data to be written */
    int32 numtype;       /* current number type */
    int32 platnumsubclass; /* class of this NT for this platform */
    int32 fileNTsize;    /* size of this NT as it will be in the file */
    int32 fileNT;        /* class of NT for the data to write */
    int32 isnative;
    int32 localNTsize;   /* size of this NT as it occurs in theis machine */
    int32 ret;           /* return code from DFwrite */
    int32 i, j;          /* temporaries */
    int32 numelements;   /* number of elements to write out per row */
    int32 writesize;     /* number of bytes to write out per row */
    int32 datastride;    /* number of bytes in one row of data[] */
    uint8 *datap;        /* pointer into data[] at */
                         /*  the start of the current row */
    uint8 *buf;          /* buffer containing converted current row */
    char *FUNC="DFSDIputslice";

    HEclear();

    if (!data) 
        HRETURN_ERROR(DFE_BADPTR, FAIL);

    if (Sfile_id == DF_NOFILE) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    rank = Writesdg.rank;

    for (i = 0; i < (int32)rank; i++) 
      {
    	/* check validity for the dimension ranges */
        if ((windims[i]<=0) || (windims[i]>Writesdg.dimsizes[i])) 
            HRETURN_ERROR(DFE_BADDIM, FAIL);

        /* check if space allocated is sufficient */
        if (dims[i] < windims[i]) 
            HRETURN_ERROR(DFE_NOTENOUGH, FAIL);
      }

    /* check to see if the slices fit together */
    /* Same for Fortran or C    */
    /* find the first significant dimension */
    for (i = 0; windims[i] == 1 && i < (int32)rank - 1; i++)
        /* empty */;
    /* check that all 'lesser' dims match */
    for (j = i + 1; j < (int32)rank; j++)
      {
        if (Writesdg.dimsizes[j] != windims[j]) 
            HRETURN_ERROR(DFE_BADDIM, FAIL);
      }

    /* update Sddims to reflect new write */
    Sddims[i] += windims[i];
    for (;i > 0 && Sddims[i] >= Writesdg.dimsizes[i]; i--) 
      {
        Sddims[i-1] += Sddims[i] / Writesdg.dimsizes[i];
	 /* promote the unit */
        Sddims[i] %= Writesdg.dimsizes[i];
      } 

    leastsig =  (int32)rank - 1; /* which is least sig dim */
    numtype  = Writesdg.numbertype;
   
   /* get class of this num type for this platform */
    fileNT      = Writesdg.filenumsubclass;
    isnative    = DFNT_NATIVE;
    fileNTsize  = DFKNTsize(numtype);
    localNTsize = DFKNTsize((numtype | isnative) & (~DFNT_LITEND));
    platnumsubclass = DFKgetPNSC(numtype & (~DFNT_LITEND), DF_MT);
    convert         = (platnumsubclass != fileNT);

    contiguous = 1;
    for (i = 0; contiguous && i < (int32)rank; i++) 
      {
        /* check if data at the end of the users array will be contiguous */
        if (dims[i] > Writesdg.dimsizes[i])
            contiguous = 0;
	/* Note: if a winstart[] array is ever added, will also need */
	/*	to check if data at start of users array will be */
	/*	contig			*/
      }

    /*
     *  2 Factors that determine how we write (in order of importance)
     *  conversion and contiguous
     */
    datap = (uint8 *)data;
    if (!convert && contiguous) 
      {
        /* compute total number of elements to write */
        for (i = 0, numelements = 1; i <(int32)rank; i++) 
            numelements *= windims[i];
        writesize = numelements * fileNTsize;

        ret = Hwrite(Writesdg.aid, writesize, (uint8 *)data); /* done */
        if (ret == FAIL)  
          {
            Hclose(Sfile_id); 
            return FAIL;
          } 
      }
    else 
      {          /* must step through the data */
        /* compute number of occurrences of the least sig dim */
        if (Fortorder) 
          {
            for (i = (int32)rank - 1, j = 1; i > 0; i--) 
              j *= windims[i];
          }
        else 
          {
            for (i = 0, j = 1; i < (int32)rank - 1; i++) 
              j *= windims[i];
          }

        numelements = windims[leastsig];
        writesize   = numelements * fileNTsize;
        datastride  = dims[leastsig] * localNTsize;
        if (convert) 
          {
            buf = (uint8 *)HDgetspace((uint32) writesize);
            if (buf == NULL)  
              {
                Hclose(Sfile_id); 
                return FAIL;
              }
            for (i = 0; i < j; i++, datap += datastride) 
              {
                DFKconvert((VOIDP)datap, (VOIDP)buf, numtype,
                           numelements, DFACC_WRITE, 0, 0);
                ret = Hwrite(Writesdg.aid, writesize, buf);  /* done */
                if (ret == FAIL) 
                  {
                    HDfreespace((VOIDP)buf);
                    Hclose(Sfile_id); 
                    return FAIL;
                  }
              }
            HDfreespace((VOIDP)buf);
          }
        else 
          { /* !contiguous	*/
            for (i = 0; i < j; i++, datap += datastride)
                ret = Hwrite(Writesdg.aid, writesize, datap);
            if (ret == FAIL) 
              {
                Hclose(Sfile_id); 
                return FAIL;
              }
          }
      }

    return(ret>=0 ? 0 : -1);
}

/* ------------------------------ DFSDgetcal ------------------------------ */
/*-----------------------------------------------------------------------------
 * Name:    DFSDgetcal()
 * Purpose: Get calibration and uncalibrated offsets for data values
 * Inputs:  pcal:  Pointer to float64 to return calibration value
 *          pcal_err:  Pointer to float64 to return calibration error value
 *          pioff:  Pointer to float64 to return uncalibrated offset value 
 *          pioff_err:  Pointer to float64 to return uncalibrated offset error value
 *          cal_nt : Pointer to int32 to return what the data's real NT is
 * Globals: IsCal
 * Returns: 0 on success, -1 if no values or if error, with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Retrieves values from Readsdg
 * Remarks: none
 *---------------------------------------------------------------------------*/

#if defined __STDC__ || defined PC
intn DFSDgetcal(float64 *pcal, float64 *pcal_err, float64 *pioff, 
                float64 *pioff_err, int32 *cal_nt)
#else
intn DFSDgetcal(pcal, pcal_err, pioff, pioff_err, cal_nt)
     float64 *pcal;
     float64 *pcal_err;
     float64 *pioff;
     float64 *pioff_err;
     int32   *cal_nt;
#endif /* __STDC__ || PC */
{
    PRIVATE char *FUNC = "DFSDgetcal";

    HEclear();
    
    if (Newdata < 0) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    if (IsCal) 
      {
        *pcal      = Readsdg.cal;
        *pcal_err  = Readsdg.cal_err;
        *pioff     = Readsdg.ioff;
        *pioff_err = Readsdg.ioff_err;
        *cal_nt    = Readsdg.cal_type;
        return(SUCCEED);
      } 
    else 
        HRETURN_ERROR(DFE_NOVALS, FAIL);

} /* DFSDgetcal */

/* ------------------------------ DFSDsetcal ------------------------------ */
/*-----------------------------------------------------------------------------
 * Name:    DFSDsetcal()
 * Purpose: Set calibration and offset (before calibration) of data
 * Inputs:  cal:  Calibration
 *          cal_err:  Calibration error
 *          ioff:  Uncalibrated offset
 *          ioff_err:  Uncalibrated offset error
 *          cal_nt:  Numbertype of uncalibrated data
 * Globals: Ref
 * Returns: SUCCEED on success, FAIL if no calibration values or if error
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  Modify Writesdg, set Ref
 * Remarks: Automatically cleared after call to DFSDputdata or DFSDadddata
 *---------------------------------------------------------------------------*/
#if defined __STDC__ || defined PC
intn DFSDsetcal(float64 cal, float64 cal_err, float64 ioff, float64 ioff_err,
                int32 cal_nt)
#else
intn DFSDsetcal(cal, cal_err, ioff, ioff_err, cal_nt)
     float64 cal;
     float64 cal_err;
     float64 ioff;
     float64 ioff_err;
     int32 cal_nt;
#endif /* __STDC__ || PC */
{
    HEclear();

    Writesdg.cal      = (float64) cal;
    Writesdg.cal_err  = (float64) cal_err;
    Writesdg.ioff     = (float64) ioff;
    Writesdg.ioff_err = (float64) ioff_err;
    Writesdg.cal_type = (int32)   cal_nt;

    Ref.cal = 0;

    return(SUCCEED);
}

/* ------------------------------ Slab Write ----------------------------- */
/*-----------------------------------------------------------------------------
 * Name:    DFSDwriteref
 * Purpose: Set ref of SDS to write next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next write
 * Returns: 0 on success, FAIL on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: HEclear, DFSDIopen, Hstartread, DFSDIgetndg, Hendacces, Hclose
 * Remarks:
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn
DFSDwriteref(char *filename, uint16 ref)
#else
intn
DFSDwriteref(filename, ref)
    char *filename;
    uint16 ref;
#endif /* PROTOTYPE */
{
    int32 file_id; 
    int32 aid;
    char *FUNC="DFSDwriteref";

    /* Clear error stack */
    HEclear();

    /* Open file for read access */
    file_id = DFSDIopen(filename, DFACC_READ);
    if (file_id == DF_NOFILE)
        return FAIL;

    /* Check for existence of SDG */
    if((aid = Hstartread(file_id, DFTAG_SDG, ref)) == FAIL
       && (aid = Hstartread(file_id, DFTAG_NDG, ref)) == FAIL)
      {
        HCLOSE_RETURN_ERROR(file_id, DFE_NOMATCH, FAIL);
      }

    /*
    ** Probably need to call DFSDgetndg to intialize Writesdg struct
    ** This is so that we use the information of an SDG that has
    ** already been written out. Once a SDG has been written out,
    ** a user should not be able to change attributes such as
    ** numbertype, dimensions or fill value.
    */
    if ((DFSDIgetndg(file_id, DFTAG_SDG, ref, &Writesdg) < 0)
        && (DFSDIgetndg(file_id, DFTAG_NDG, ref, &Writesdg) < 0 ))
      {
        Hendaccess(aid);
        Hclose(file_id);
        return FAIL;
      }

    /* Close access to file, set Writeref */
    Hendaccess(aid);
    Writeref = ref;
    Lastref  = ref;

    return Hclose(file_id);
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDsetfillvalue
 * Purpose: Set fill value of SDS
 * Inputs:  fill_value: fill value
 * Returns: 0 on success, FAIL on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: HEclear, DFKNTsize, HDfreespace, HDgetspace, HDmemcpy
 * Remarks: Memory bug on SGI's if you try to free allocated space for
 *          fill values.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn
DFSDsetfillvalue(VOIDP fill_value)
#else
intn
DFSDsetfillvalue(fill_value)
    VOID *fill_value;
#endif /* PROTOTYPE */
{
    int32 numtype;      /* current number type  */
    int32 localNTsize;  /* size of this NT on as it is on this machine  */
    char *FUNC="DFSDsetfillvalue";

    /* Clear error stack  */
    HEclear();

    /* Check to see if fill value written out already */
    if (Ref.fill_value == -1 && Writesdg.fill_fixed==TRUE)
       return FAIL;
    else
     {
       /* Get local and file number type sizes  */
       numtype     = Writesdg.numbertype;
       localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));

       /* Set fill value in Writesdg struct, and set fill value flag  */
       Ref.fill_value = 0;
       if (HDmemcpy(Writesdg.fill_value, fill_value, localNTsize) != NULL)
         return SUCCEED;
       else
         return FAIL;
     }
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetfillvalue
 * Purpose: Get fill value of SDS
 * Inputs:  fill_value: fill value is returned
 * Returns: 0 on success, FAIL on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: HEclear, DFKNT, HDmemcpy
 * Remarks: 
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn
DFSDgetfillvalue(VOIDP fill_value)
#else
intn
DFSDgetfillvalue(fill_value)
    VOID *fill_value;
#endif /* PROTOTYPE */
{
    int32 numtype;      /* current number type  */
    int32 localNTsize;  /* size of this NT on as it is on this machine  */
    char *FUNC="DFSDgetfillvalue";

    /* Clear error stack  */
    HEclear();

    /* Check if Readsdg is fresh  */
    if (Newdata < 0) 
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    /* Get local number type size  */
    numtype     = Readsdg.numbertype;
    localNTsize = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));

    /* Set return fill value  */
    if (HDmemcpy(fill_value, Readsdg.fill_value, localNTsize) != NULL)
      return SUCCEED;
    else
      return FAIL;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDgetslab
 * Purpose: Get slab of data from SDG.  Will sequence to next SDG if
 *          DFSDgetdims, DFSDgetdata or DFSDgetslab not called earlier.
 * Inputs:  filename: name of HDF file to use
 *          start: array of size = rank of data, containing start of slice
 *          slab_size: array of size rank, containing end of slice
 *          stride:
 *          buffer: array for returning slice
 *          buffer_size: dimensions of array data
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: slab of data in data
 * Users:   
 * Invokes: DFSDgetslice
 * Method:  call DFSDgetslice
 * Remarks: buffer may be larger than size of slab.  In that event, the actual
 *          data may not be contiguous in the array "buffer".
 *          User sets buffer_size before call.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFSDgetslab(char *filename, int32 start[], int32 slab_size[], 
                 int32 stride[], VOIDP buffer, int32 buffer_size[])
#else
intn DFSDgetslab(filename, start, slab_size, stride, buffer, buffer_size)
     char *filename;
     int32 start[];
     int32 slab_size[];
     int32 stride[];
     VOID *buffer;
     int32 buffer_size[];
#endif /* PROTOTYPE */
{
    return (DFSDgetslice(filename, start, slab_size, buffer, buffer_size));
}


/*-----------------------------------------------------------------------------
 * Name:    DFSDstartslab
 * Purpose: Set up to write slab of data to SD.
 * Inputs:  filename: name of HDF file to write to
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   None
 * Invokes: 
 * Method:
 * Remarks: DFSDsetdims must have been called first
 *          No call which needs a file open may be made after this
 *          till DFSDendslab is called. This routine will write out the fill 
 *          values if DFSDsetfillvalue() is called before this routine.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn
DFSDstartslab(char *filename)
#else
intn
DFSDstartslab(filename)
     char *filename;
#endif /* PROTOTYPE */
{
    int32 i;
    int32 sdg_size;
    int32 localNTsize;
    int32 fileNTsize;
    int32 fill_bufsize = 16384; /* Chosen for the PC */
    int32 odd_size;
    uint8 *fill_buf;

    char *FUNC="DFSDstartslab";

    /* Clear errors */
    HEclear();

    /* Check rank set i.e. DFSDsetdims()  */
    if (!Writesdg.rank)
        HRETURN_ERROR(DFE_BADDIM, FAIL);

    /* If NT not set(i.e. DFSDsetNT() not called), default to float32  */
    if (Writesdg.numbertype == DFNT_NONE)
        DFSDsetNT(DFNT_FLOAT32);

    /* Open file */
    Sfile_id = DFSDIopen(filename, DFACC_WRITE);
    if (Sfile_id == DF_NOFILE)
      return FAIL;

    /*
    ** Check for Writeref set i.e. DFSDwriteref() called?
    ** If not Writeref then we create a new Writeref i.e new SDG
    ** Else use existing one.
    */
    if (!Writeref)
      Writeref = Hnewref(Sfile_id);
    if (!Writeref)
      return FAIL;

    /* Set tag, ref of SDG to write */
    Writesdg.data.tag = DFTAG_SD;
    Writesdg.data.ref = Writeref;

    /* Intialize a few local variables */
    localNTsize = DFKNTsize((Writesdg.numbertype | DFNT_NATIVE) & (~DFNT_LITEND));
    fileNTsize  = DFKNTsize(Writesdg.numbertype);

    /* Calculate size of of dataset */
    sdg_size = fileNTsize;
    for (i = 0; i < Writesdg.rank; i++)
        sdg_size *= Writesdg.dimsizes[i];

    /* set up to write data */
    Writesdg.aid = Hstartwrite(Sfile_id, DFTAG_SD, Writeref, sdg_size);
    if (Writesdg.aid == FAIL)
      {
        Hclose(Sfile_id);
        return FAIL;
      }

    /*
    ** Check if fill value is set
    */
    if (!Ref.fill_value)
      {
        /* make the fill buffer smaller if possible */
        if(fill_bufsize>sdg_size && localNTsize==fileNTsize)
            fill_bufsize=sdg_size;

        /* Allocate space for fill buffer */
        if ((fill_buf =(uint8 *)HDgetspace((uint32) fill_bufsize)) == NULL)
          {
            Hendaccess(Writesdg.aid);
            Hclose(Sfile_id);
            HRETURN_ERROR(DFE_NOSPACE, FAIL);
          }

        /* Intialize buffer to fill value */
       for (i = 0; i < fill_bufsize; i = i + localNTsize)
           HDmemcpy((uint8 *)&(fill_buf[i]),Writesdg.fill_value,localNTsize);

       if (sdg_size <= fill_bufsize)
          odd_size = sdg_size;
       else
         {
           odd_size = sdg_size % fill_bufsize;
           for (i = 0; i < (sdg_size/fill_bufsize); i++)
             {  /* Write out fill buffer X times */
               if (Hwrite(Writesdg.aid, fill_bufsize, fill_buf) == FAIL)
                 {
                    Hendaccess(Writesdg.aid);
                    Hclose(Sfile_id);
                    HDfreespace((VOIDP)fill_buf);
                    HRETURN_ERROR(DFE_WRITEERROR, FAIL);
                  }
             }
         }
         /* Write fill values for odd size piece */
         if (Hwrite(Writesdg.aid, odd_size, fill_buf) == FAIL)
           {
            Hendaccess(Writesdg.aid);
            Hclose(Sfile_id);
            HDfreespace((VOIDP)fill_buf);
            HRETURN_ERROR(DFE_WRITEERROR, FAIL);
           }

        Writesdg.fill_fixed=TRUE;       /* fill value cannot be changed */
         /* Free up space */
         HDfreespace((VOIDP)fill_buf);
      }

    return SUCCEED;
}

/*----------------------------------------------------------------------------
 * Name:    DFSDwriteslab
 * Purpose: Write slab of data to SD.
 * Inputs:  start: array of size = rank of data, containing start of slab
 *          stride: array for subsampling
 *          count: array of size rank, containing size of slab
 *          data: array of data to be written
 * Returns: 0 on success, FAIL on failure with error set
 * Outputs: None
 * Users:   HDF programmers
 * Invokes:
 * Method:  Open file,  convert types if necessary, write data to file
 * Remarks: 
 *--------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn
DFSDwriteslab(int32 start[], int32 stride[],
              int32 count[], VOIDP data)
#else
intn
DFSDwriteslab(start, stride, count, data)
    int32 start[];        /* array containing the coordinates of the start */
                          /*   of the slab in the HDF file */
    int32 stride[];       /* array containing the dimensions of data[] */
    int32 count[];        /* array containing the size of the slab */
    VOID  *data;          /* array to hold the floating point data to write*/
#endif /* PROTOTYPE */
{
    intn  rank;           /* number of dimensions in data[] */
    int32 i;              /* temporary loop index */

    int32 leastsig;       /* fastest varying subscript in the array */
    int32 convert;        /* true if machine NT != NT to be read */
    int32 done;           /* true if we are at the end of the slab */
    int32 issdg;          /* 1 -- pure sdg. do what HDF3.1 does   */
    int32 numtype;        /* current number type  */
    int32 fileNTsize;     /* size of this NT in the file  */
    int32 localNTsize;    /* size of this NT as it occurs in this machine */
    int32 numelements;    /* number of floats to read at once */
    int32 sdgsize;        /* number of bytes to be written in the SDG */
    int32 rowsize;        /* number of bytes to be written at once */
                          /*   in the hyperslab */
    int32 datastride;     /* number of floats in one row of data[] */
    int32 fileoffset;     /* offset into the current dataset in the file */
    int32 *doffset;       /* array for accessing the next element in data[] */
    int32 *foffset;       /* array for accessing  next element in the file */
    int32 *dimsleft;      /* array for tracking current position in data[]*/
    int32 *startdims;     /* tmp array containing starting slab dims */
    int32 *sizedims;      /* tmp array containing the slab size */
    int32 *filedims;      /* tmp array containing the dimensions */
                          /*   of the dataset in the file */
    int32 r_error;        /* flag if an error occurred, */
                          /*   used by DFconvert macro */
    int32 aid;

    uint8 platnumsubclass; /* class of this NT for this platform */
    uint8 fileNT;          /* file number subclass  */
    uint8 *buf;            /* buffer containing the converted current row */
    uint8 *datap;          /* ptr into data[] at starting offset */
                           /*   of current block */

    char *FUNC="DFSDwriteslab";

    /* Clear error stack  */
    HEclear();

    /* Sanity checking of input data  */
    if (!data)
        HRETURN_ERROR(DFE_BADPTR, FAIL);

    /* Set rank */
    rank = Writesdg.rank;

    /* Do sanity checking of starting and size dimension arrays  */
    for (i = 0; i < (int32)rank; i++)
      {
        /*
        ** Check validity for the dimension ranges by
        **  checking lower bound of slab sizes
        **  checking lower bound of starting dimensions
        **  checking upper bound on writing dimensions
        */
        if ((count[i] < 1) || (start[i] < 1)
            || (start[i]+count[i]-1 > Writesdg.dimsizes[i]))
          {
            Hclose(Sfile_id);
            HRETURN_ERROR(DFE_BADDIM, FAIL);
          }
      }

    /* Intialize a few local variables */
    numtype         = Writesdg.numbertype;
    platnumsubclass = DFKgetPNSC(numtype & (~DFNT_LITEND), DF_MT);
    localNTsize     = DFKNTsize((numtype | DFNT_NATIVE) & (~DFNT_LITEND));
    fileNTsize      = DFKNTsize(numtype);
    fileNT          = Writesdg.filenumsubclass;
    issdg           = Writesdg.isndg? 0: 1;

    /* Calculate total bytes in SDS that can be written */
    sdgsize = fileNTsize;
    for (i=0; i < Writesdg.rank; i++)
        sdgsize *= Writesdg.dimsizes[i];

    /* Set Access Id */
    aid = Writesdg.aid;

    /*
    ** Get dimensions of hyperslab to write out
    ** Allocate temporary buffers(3) to hold starting, size,
    **  and file(SDG in file) dimensions
    */
    startdims = (int32 *) HDgetspace((uint32) 3 * rank * sizeof(int32));
    if (startdims == NULL)
      {
        Hclose(Sfile_id);
        HRETURN_ERROR(DFE_NOSPACE, FAIL);
      }
    sizedims = startdims + rank;
    filedims = sizedims  + rank;

    /* Copy arrays to private workspace (row major order) */
   for (i = 0; i < (int32)rank; i++)
     {
       startdims[i] = start[i] - 1;
       sizedims[i]  = count[i];            /* dimensions of just slab */
       filedims[i]  = Writesdg.dimsizes[i]; /* dimensions of whole SDG */
     }

    /* Is conversion necessary */
    convert = (fileNT != platnumsubclass);

    /* Collapse dimensions if contiguous both in the file and in memory */
    for (i =(int32)rank - 1; i > 0; i--) /* stop before most sig dim */
      {
        /* read only part of dataset */
        if (startdims[i] != 0 || sizedims[i] < filedims[i])
            break;
        startdims[i-1] *= filedims[i];
        sizedims[i-1] *= sizedims[i];
        filedims[i-1] *= filedims[i];
        rank--;
      }

    /*
    ** Which is least sig dim i.e fastest varying.
    ** In C usually the last.
    */
    leastsig = (int32)rank - 1;

    r_error = 0;
    if (rank == 1 && !convert)
      {
        /* all data is contiguous with no conversions */
        rowsize = sizedims[0] * fileNTsize;
        if ( (Hseek(aid, startdims[0]*fileNTsize, 0) == FAIL)
            || (rowsize != Hwrite(aid, rowsize, (uint8 *)data)) )
          {
            r_error = 1;
          }
      }
    else
      {
        /*
        * The data must be further manipulated.
        * It may need conversion, may not be contiguous, or
        * any combination of these.
        */
        numelements = sizedims[leastsig];      /* # of elmenents in a row */
        rowsize     = numelements * fileNTsize;/* # of bytes in a row */

        /* If conversion, allocate 1 row buffers */
        if (convert)
          {
            if ((buf = (uint8 *) HDgetspace((uint32) rowsize)) == NULL) {
                HDfreespace((VOIDP)startdims);
                Hendaccess(aid);
                Hclose(Sfile_id);
                HRETURN_ERROR(DFE_NOSPACE, FAIL);
              }
          }
        else
            buf = NULL;

        /* Allocate space for file and data offsets and dimsleft */
        foffset = (int32 *) HDgetspace((uint32)3 * rank * sizeof(int32));
        if (foffset==NULL)
          {
            HDfreespace((VOIDP)startdims);
            HDfreespace((VOIDP)buf);
            Hendaccess(aid);
            Hclose(Sfile_id);
            HRETURN_ERROR(DFE_NOSPACE, FAIL);
          }
        dimsleft = foffset + rank;
        doffset  = dimsleft + rank;

        /* Set number of dimensions left */
        for (i = leastsig; i >= 0; i--)
            dimsleft[i] = sizedims[i];

        /* compute offsets in the source array */
        doffset[leastsig] = 1*localNTsize;
        for (i = leastsig; i > 0; i--)
            doffset[i-1] = doffset[i] * sizedims[i];

        /* Set data stride along leastsig */
        datastride = sizedims[leastsig];

        /*
        ** Compute offsets in the file for dimension, according to the
        ** possible length for each dimension. Depends on numbertype.
        */
        for (i = leastsig, foffset[i] = 1*fileNTsize; i > 0; i--)
            foffset[i-1] = foffset[i] * filedims[i];

        /*
        ** Compute starting position in file
        ** All file writes are done relative to this starting offset.
        ** Cumulative offset is from most sig to next to least sig dim.
        */
        for (i = 0, fileoffset = 0; i < leastsig; i++)
            fileoffset = fileoffset + (startdims[i] * foffset[i]);

        /* Dont forget about last dimension */
        fileoffset = fileoffset + startdims[leastsig] * fileNTsize;

        datap = (uint8 *)data;
        done = 0;

        /* -- now write the data */
        do {
            /* move to the next data element in the file */
            if (Hseek(aid, fileoffset, 0) == FAIL)
              {
                r_error = 1;
                break;
              }

            /*  If convert and write one contiguous block of data */
            /*  Else write one contiguous block of data */
            if (convert)
              {
                DFKconvert((VOIDP)datap, (VOIDP)buf, numtype,
                            numelements, DFACC_WRITE, 0, 0);
                if (rowsize != Hwrite(aid, rowsize, buf))
                  {
                    r_error = 1;
                    break;
                  }
              }
            else
              {
                if (rowsize != Hwrite(aid, rowsize, datap))
                  {
                    r_error = 1;
                    break;
                  }
              }

            /*
             * Find starting place of the next row/block.
             * Note that all moves are relative:
             *   this preserves the starting offsets of each dimension
             */
            for (i = leastsig - 1; i >= 0; i--)
              {
                if (--dimsleft[i] > 0)
                  {
                    /* Move to next element in the current dimension */
                    datap += doffset[i];
                    fileoffset += foffset[i];
                    break;
                  }
                else
                  {
                    dimsleft[i] = sizedims[i];
                    /*
                     * Note that we are still positioned at the beginning of
                     * the last element in the current dimension
                     */
                    /* move back to the beginning of dimension i */
                    datap -= doffset[i] * (sizedims[i]-1);

                    /* move back to beginning read position of dimension i */
                    fileoffset -= foffset[i] * (sizedims[i]-1);
                    if (i==0)
                        done = 1;
                  }
              }
        } while (!done && leastsig > 0);

        HDfreespace((VOIDP)buf);
        HDfreespace((VOIDP)foffset);
      }

    /* Clean up time....*/
    HDfreespace((VOIDP)startdims);

    if (r_error)
     return FAIL;
    else
     return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFSDendslab
 * Purpose: Write of data to SDG completed, write NDG and close file
 * Inputs:  None
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   DFSDwriteslab
 * Invokes:
 * Method:  call DFSDputndg, close Sfile_id
 * Remarks: Writes NDG to file on very fist write to SDS.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn
DFSDendslab(void)
#else
intn
DFSDendslab()
#endif /* PROTOTYPE */
{
    intn ret;
    char *FUNC="DFSDendslab";

    /* Clear error stack */
    HEclear();

    /* Valid file id */
    if (Sfile_id == DF_NOFILE)
        HRETURN_ERROR(DFE_BADCALL, FAIL);

    /* Check to see if we have written out the SDG info */
    if (!Ref.new_ndg)
      {
        if (DFSDIputndg(Sfile_id, Writeref, &Writesdg)<0)
          {
            Hclose(Sfile_id);
            return FAIL;
          }

        /* old nsdg table should be reset next time  */
        if (nsdghdr != NULL)
          {
            if (nsdghdr->nsdg_t != NULL)
              {
                DFnsdgle *rear, *front;

                rear = nsdghdr->nsdg_t;
                front = rear->next;
                while (rear != NULL)
                  {
                   if ((rear=(DFnsdgle *)HDfreespace((VOIDP) rear)) != NULL)
                      return FAIL;
                   rear = front;
                   if (rear != NULL)
                        front = rear->next;
                  }
                nsdghdr->size=0;
                nsdghdr->nsdg_t = NULL;
                lastnsdg.tag = DFTAG_NULL;
                lastnsdg.ref = 0;
              }

            if ((nsdghdr=(DFnsdg_t_hdr *)HDfreespace((VOIDP)nsdghdr))
                 != NULL)
              {
                return FAIL;
              }
          }

        Ref.new_ndg = -1;
      }

    /* Slab clean up */
    Hendaccess(Writesdg.aid);
    ret = Hclose(Sfile_id);
    Sfile_id = 0;
    Lastref = (uint16)Writeref;     /* remember ref written */
    Writeref = 0;                   /* Reset Write ref */

    return ret;
}
