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
 * File:    dfp.c
 * Purpose: read and write palettes
 * Invokes: df.c
 * Contents:
 *  DFPgetpal: retrieve next palette
 *  DFPputpal: write palette to file
 *  DFPaddpal: add palette to file
 *  DFPnpals: number of palettes in HDF file
 *  DFPreadref: get palette with this reference number next
 *  DFPwriteref: put palette with this reference number next
 *  DFPrestart: forget info about last file accessed - restart from beginning
 *  DFPlastref: return reference number of last element read or written
 *  DFPIopen: open/reopen file
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "herr.h"
#include "hfile.h"

static uint16 Readref=0;
static uint16 Writeref=0;
static uint16 Refset=0;                /* Ref of palette to get next */
static uint16 Lastref = 0;     /* Last ref read/written */

static char Lastfile[DF_MAXFNLEN]; /* last file opened */

#ifdef VMS
int32 _DFPIopen();
#endif


/*-----------------------------------------------------------------------------
 * Name:    DFPgetpal
 * Purpose: get next palette from file
 * Inputs:  filename: name of HDF file
 *          palette: 768 byte space to read palette into
 * Returns: 0 on success, -1 on failure with DFerror set
 *          palette in pal
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFPIopen, DFIerr, DFclose, DFgetelement
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFPgetpal(char *filename, VOIDP palette)
#else
intn DFPgetpal(filename, palette)
    char *filename;
    VOIDP palette;
#endif
{
    char *FUNC="DFPgetpal";
    int32 file_id;
    int32 aid;
    int32 length;

    HEclear();

    if (!palette) {
        HERROR(DFE_ARGS);
        return FAIL;
    }
    file_id = DFPIopen(filename, DFACC_READ);
    if (file_id == FAIL) {
       return FAIL;
    }

    if (Refset) {
        aid = Hstartread(file_id, DFTAG_IP8, Refset);
        if (aid == FAIL)
            aid = Hstartread(file_id, DFTAG_LUT, Refset);
    } else if (Readref) {
        aid = Hstartread(file_id, DFTAG_IP8, Readref);
        if(aid == FAIL)
            aid = Hstartread(file_id, DFTAG_LUT, Readref);
        if (aid != FAIL &&
              (Hnextread(aid, DFTAG_IP8, DFREF_WILDCARD, DF_CURRENT) == FAIL)) {
            if(Hnextread(aid, DFTAG_LUT, DFREF_WILDCARD, DF_CURRENT) == FAIL){
                Hendaccess(aid);
               aid = FAIL;
            }
        }
    } else {
        aid = Hstartread(file_id, DFTAG_IP8, DFREF_WILDCARD);
        if(aid == FAIL)
            aid = Hstartread(file_id, DFTAG_LUT, DFREF_WILDCARD);
    }

    Refset = 0;
    if (aid == FAIL) {
       return(HDerr(file_id));
    }
    /* on error, close file and return -1 */

    if (Hinquire(aid, (int32*)NULL, (uint16*)NULL, &Readref, &length,
            (int32*)NULL, (int32*)NULL, (int16*)NULL, (int16*)NULL) == FAIL) {
       Hendaccess(aid);
       return HDerr(file_id);
    }

        /* read palette */
    if (Hread(aid, length, (uint8 *)palette) == FAIL) {
       Hendaccess(aid);
       return(HDerr(file_id));
    }

    Hendaccess(aid);

    Lastref = Readref;

    return(Hclose(file_id));
}


/*-----------------------------------------------------------------------------
 * Name:    DFPputpal
 * Purpose: Write palette to file
 * Inputs:  filename: name of HDF file
 *          palette: palette to be written to file
 *          overwrite: if 1, overwrite last palette read or written
 *                     if 0, write it as a fresh palette
 *          filemode: if "a", append palette to file
 *                    if "w", create new file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF users, programmers, utilities
 * Invokes: DFPIopen, DFclose, DFputelement, DFIerr
 * Remarks: To overwrite, the filename must be the same as for the previous
 *          call
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFPputpal(char *filename, VOIDP palette, int overwrite, char *filemode)
#else
intn DFPputpal(filename, palette, overwrite, filemode)
    char *filename;
    VOIDP palette;
    int overwrite;
    char *filemode;
#endif
{
    char *FUNC="DFPputpal";
    int32 file_id;

    HEclear();

    if (!palette) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    if (overwrite && HDstrcmp(filename, Lastfile)) {
        HERROR(DFE_BADCALL);
        return FAIL;
    }

    file_id = DFPIopen(filename,
                      (*filemode=='w') ? DFACC_CREATE : DFACC_WRITE);
    if (file_id==FAIL)
        return FAIL;

    /* if we want to overwrite, Lastref is the ref to write.  If not, if
        Writeref is set, we use that ref.  If not we get a fresh ref. The
        ref to write is placed in Lastref */

    if (!overwrite)
        Lastref = (uint16)(Writeref ? Writeref : Hnewref(file_id));
    if (Lastref == 0)
        return FAIL;

    Writeref = 0;           /* don't know ref to write after this */

        /* write out palette */
    if (Hputelement(file_id, DFTAG_IP8, Lastref, 
                             (uint8 *)palette, (int32) 768)<0)
            return(HDerr(file_id));

    Hdupdd(file_id, DFTAG_LUT, Lastref, DFTAG_IP8, Lastref);

    return(Hclose(file_id));
}


/*-----------------------------------------------------------------------------
 * Name:    DFPaddpal
 * Purpose: Add palette to file
 * Inputs:  filename: name of HDF file
 *          palette: palette to be written to file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF users, programmers, utilities
 * Invokes: DFPputpal
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFPaddpal(char *filename, VOIDP palette)
#else
int DFPaddpal(filename, palette)
    char *filename;
    VOIDP palette;
#endif
{
    return(DFPputpal(filename, palette, 0, "a"));
}


/*-----------------------------------------------------------------------------
 * Name:    DFPnpals
 * Purpose: How many palettes are present in this file?
 * Inputs:  filename: name of HDF file
 * Returns: number of palettes on success, -1 on failure with DFerror set
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFPIopen, Hclose, Hnumber, Hfind
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFPnpals(char *filename)
#else
int DFPnpals(filename)
    char *filename;
#endif
{
    char *FUNC="DFPnpals";
    int32 file_id;
    intn curr_pal;          /* current palette count */
    intn nip8, nlut,        /* number of IP8s & number of LUTs */
        npals;              /* total number of palettes */
    uint16 find_tag,find_ref;   /* storage for tag/ref pairs found */
    int32 find_off,find_len;    /* storage for offset/lengths of tag/refs found */
    int32 *pal_off;         /* storage for an array of palette offsets */
    intn i,j;               /* local counting variable */

    HEclear();

    /* should use reopen if same file as last time - more efficient */
    file_id = DFPIopen(filename, DFACC_READ);
    if (file_id==FAIL)
        return FAIL;

    nip8 = Hnumber(file_id, DFTAG_IP8);       /* count number of IPs */
    if (nip8 == FAIL)
       return(HDerr(file_id));
    nlut = Hnumber(file_id, DFTAG_LUT);       /* count number of LUTs */
    if (nlut == FAIL)
       return(HDerr(file_id));
    npals = nip8 + nlut;

    /* Get space to store the palette offsets */
    if((pal_off=(int32 *)HDgetspace(npals*sizeof(int32)))==NULL) {
        HERROR(DFE_NOSPACE);
        return(FAIL);
      } /* end if */

    /* go through the IP8s */
    curr_pal=0;
    find_tag=find_ref=0;
    while(Hfind(file_id,DFTAG_IP8,DFREF_WILDCARD,&find_tag,&find_ref,&find_off,&find_len,DF_FORWARD)==SUCCEED) {
        pal_off[curr_pal]=find_off;  /* store offset */
        curr_pal++;
      } /* end while */

    /* go through the LUTs */
    find_tag=find_ref=0;
    while(Hfind(file_id,DFTAG_LUT,DFREF_WILDCARD,&find_tag,&find_ref,&find_off,&find_len,DF_FORWARD)==SUCCEED) {
        pal_off[curr_pal]=find_off;  /* store offset */
        curr_pal++;
      } /* end while */

    npals=curr_pal;     /* reset the number of palettes we really have */
    for(i=1; i<curr_pal; i++) {   /* go through the palettes looking for duplicates */
        for(j=0; j<i; j++) {
            if(pal_off[i]==pal_off[j])
                npals--;  /* if duplicate found, decrement the number of palettes */
          } /* end for */
      } /* end for */

    HDfreespace((VOIDP)pal_off);       /* free offsets */

    if (Hclose(file_id) == FAIL)
        return FAIL;

    return(npals);
}

/*-----------------------------------------------------------------------------
 * Name:    DFPreadref
 * Purpose: Set ref of palette to get next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next get
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFPIopen, DFIfind, DFclose
 * Remarks: checks if palette with this ref exists
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFPreadref(char *filename, uint16 ref)
#else
intn DFPreadref(filename, ref)
    char *filename;
    uint16 ref;
#endif
{
    int32 file_id;
    int32 aid;

    HEclear();

    file_id = DFPIopen(filename, DFACC_READ);
    if (file_id == FAIL) return FAIL;

    aid = Hstartread(file_id, DFTAG_IP8, ref);
    if (aid == FAIL) {
      aid = Hstartread(file_id, DFTAG_LUT, ref);
      if(aid == FAIL) 
        return(HDerr(file_id));
    }

    Hendaccess(aid);
    Refset = ref;

    return(Hclose(file_id));
}


/*-----------------------------------------------------------------------------
 * Name:    DFPwriteref
 * Purpose: Set ref of palette to put next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next put
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: none
 *---------------------------------------------------------------------------*/

/* shut lint up */
/* ARGSUSED */
#ifdef PROTOTYPE
int DFPwriteref(char *filename, uint16 ref)
#else
int DFPwriteref(filename, ref)
    char *filename;
    uint16 ref;
#endif
{
    Writeref = ref;
    return SUCCEED;
}



/*-----------------------------------------------------------------------------
 * Name:    DFPrestart
 * Purpose: Do not remember info about file - get again from first palette
 * Inputs:  none
 * Returns: 0 on success
 * Users:   HDF programmers
 * Remarks: Just reset Lastfile to NULL
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFPrestart(void)
#else
int DFPrestart()
#endif
{

    Lastfile[0] = '\0';
    return SUCCEED;
}


/*-----------------------------------------------------------------------------
 * Name:    DFPlastref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Lastref
 * Returns: ref on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  return Lastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
uint16 DFPlastref(void)
#else
uint16 DFPlastref()
#endif
{

    return(Lastref);
}



/**************************************************************************/
/*----------------------- Internal routines ------------------------------*/
/**************************************************************************/


/*-----------------------------------------------------------------------------
 * Name:    DFPIopen
 * Purpose: open or reopen a file
 * Inputs:  filename: name of file to open
 *          access : access mode
 * Returns: file pointer on success, NULL on failure with DFerror set
 * Users:   HDF systems programmers, other DFP routines
 * Invokes: DFopen
 * Remarks: This is a hook for someday providing more efficient ways to
 *          reopen a file, to avoid re-reading all the headers
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFPIopen(char *filename, int access)
#else
int32 DFPIopen(filename, access)
    char *filename;
    int access;
#endif
{

    int32 file_id;

        /* use reopen if same file as last time - more efficient */
    if (HDstrncmp(Lastfile,filename,DF_MAXFNLEN) || (access==DFACC_CREATE)) {
                                    /* treat create as different file */
        if ((file_id = Hopen(filename, access, 0)) == FAIL)
           return FAIL;
        Refset = 0;         /* no ref to get set for this file */
        Readref = 0;
    } else
        if ((file_id = Hopen(filename, access, 0)) == FAIL)
           return FAIL;

    HDstrncpy(Lastfile, filename, DF_MAXFNLEN);
    /* remember filename, so reopen may be used next time if same file */

    return(file_id);
}
