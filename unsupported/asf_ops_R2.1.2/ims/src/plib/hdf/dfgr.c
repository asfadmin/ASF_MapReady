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
 * File:    dfgr.c
 * Purpose: read and write general raster images
 * Invokes: df.c, dfkit.c, dfcomp.c, dfgroup.c, dfgr.h
 * Contents:
 *  DFGRgetlutdims: get dimensions of lookup table
 *  DFGRreqlutil: use this interlace when returning lookup table
 *  DFGRgetlut: read in lookup table
 *  DFGRgetimdims: get dimensions of image
 *  DFGRreqimil: use this interlace when returning image
 *  DFGRgetimage: read in image
 *  DFGRsetcompress: specify compression scheme to be used
 *  DFGRsetlutdims: set dimensions of lookup table
 *  DFGRsetlut: set lookup table to write out with subsequent images
 *  DFGRaddlut: write out lookup table
 *  DFGRsetimdims: set dimensions of image
 *  DFGRaddimage: write out image
 *
 *  DFGRgetrig: read in raster image group
 *  DFGRaddrig: write out raster image group
 *
 *  DFGRIopen: open/reopen file
 *  DFGRIriginfo: obtain info about next RIG
 *  DFGRIgetdims: get dimensions of lut/iamge
 *  DFGRIreqil: get lut/image with this interlace
 *  DFGRIgetimlut: get image/lut
 *  DFGRIsetdims: set image/lut dimensions
 *  DFGRIaddimlut: write out image/lut
 * Remarks: A RIG specifies attributes associated with an image - lookup table,

 *          dimension, compression, color compensation etc.
 *---------------------------------------------------------------------------*/

#include "dfgr.h"
#include "herr.h"

static char Grlastfile[DF_MAXFNLEN];
static DFGRrig Grread;         /* information about RIG being read */
static DFGRrig Grwrite;        /* information about RIG being written */
static int Grnewdata = 0;      /* does Grread contain fresh data? */
static int Grcompr = 0;        /* compression scheme to use */
static comp_info Grcinfo;      /* Compression information for each scheme */
uint8  *Grlutdata=NULL;        /* points to lut, if in memory */
static uint16 Grrefset=0;      /* Ref of image to get next */
static uint16 Grlastref = 0;   /* Last ref read/written */
static int Grreqil[2]= {0, 0}; /* requested lut/image il */
static struct {                        /* track refs of set vals written before
 */
    int lut;                   /* -1: no vals set */
    int16 dims[2];             /* 0: vals set, not written */
    int nt;                    /* non-zero: ref of val in file */
} Ref = {-1, {-1, -1}, -1 };

static DFGRrig Grzrig = {      /* empty RIG for initialization */
    { {0, 0}, {0, 0}, {0, 0}, },
    { {0, 0, {0, 0}, 0, 0, {0, 0}},
      {0, 0, {0, 0}, 0, 0, {0, 0}},
      {0, 0, {0, 0}, 0, 0, {0, 0}}, },
    0, 0, (float32)0.0, (float32)0.0,
    {(float32)0.0, (float32)0.0, (float32)0.0},
    {(float32)0.0, (float32)0.0, (float32)0.0},
    {(float32)0.0, (float32)0.0, (float32)0.0},
    {(float32)0.0, (float32)0.0, (float32)0.0}, NULL
};

#ifdef QAK
uint8 GRtbuf[512];
#endif

#define LUT     0
#define IMAGE   1

/* private functions */
PRIVATE int DFGRIriginfo
    PROTO((int32 file_id));
PRIVATE int DFGRgetrig
    PROTO((int32 file_id, uint16 ref, DFGRrig *rig));
PRIVATE int DFGRaddrig
    PROTO((int32 file_id, uint16 ref, DFGRrig *rig));

/*-----------------------------------------------------------------------------
 * Name:    DFGRgetlutdims
 * Purpose: get dimensions of lut from next RIG
 * Inputs:  filename: name of HDF file
 *          pxdim, pydim: pointer to locations for returning x,y dimensions
 *          pncomps: location for returning no of components
 *          pil: location for returning interlace of lut in file
 * Returns: 0 on success, -1 on failure with DFerror set
 *          *pxdim, *pydim, *pncomps, *pil set on success
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRgetlutdims(char *filename, int32 *pxdim, int32 *pydim, int *pncomps,
                  int *pil)
#else
int DFGRgetlutdims(filename, pxdim, pydim, pncomps, pil)
char *filename;
int32 *pxdim, *pydim;
int *pncomps, *pil;
#endif
{
    return(DFGRIgetdims(filename, pxdim, pydim, pncomps, pil, LUT));
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRreqlutil
 * Purpose: get next lut with specified interlace
 * Inputs:  il: interlace to get next lut with
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIreqil
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRreqlutil(int il)
#else
int DFGRreqlutil(il)
int il;
#endif
{
    return(DFGRIreqil(il, LUT));
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRgetlut
 * Purpose: get lut from next RIG
 * Inputs:  filename: name of HDF file
 *          lut: pointer to space to return lookup table
 *          xdim, ydim: dimensions of space to return lut
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetimlut
 * Remarks: space is assumed to be xdim * ydim * ncomps bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRgetlut(char *filename, VOIDP lut, int32 xdim, int32 ydim)
#else
int DFGRgetlut(filename, lut, xdim, ydim)
char *filename;
VOIDP lut;
int32 xdim, ydim;
#endif
{
    /* 0 == C */
    return(DFGRIgetimlut(filename, lut, xdim, ydim, LUT, 0));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRgetimdims
 * Purpose: get dimensions of next image RIG
 * Inputs:  filename: name of HDF file
 *          pxdim, pydim: pointer to locations for returning x,y dimensions
 *          pncomps: location for returning no of components
 *          pil: location for returning interlace of image in file
 * Returns: 0 on success, -1 on failure with DFerror set
 *          *pxdim, *pydim, *pncomps, *pil set on success
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRgetimdims(char *filename, int32 *pxdim, int32 *pydim, int *pncomps,
                 int *pil)
#else
int DFGRgetimdims(filename, pxdim, pydim, pncomps, pil)
char *filename;
int32 *pxdim, *pydim;
int *pncomps, *pil;
#endif
{
    return(DFGRIgetdims(filename, pxdim, pydim, pncomps, pil, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRreqimil
 * Purpose: get next image with specified interlace
 * Inputs:  il: interlace to get next image with
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIreqil
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRreqimil(int il)
#else
int DFGRreqimil(il)
int il;
#endif
{
    return(DFGRIreqil(il, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRgetimage
 * Purpose: get image from next RIG
 * Inputs:  filename: name of HDF file
 *          image: pointer to space to return image
 *          xdim, ydim: dimensions of space to return lut
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIgetimlut
 * Remarks: space is assumed to be xdim * ydim * ncomps bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRgetimage(char *filename, VOIDP image, int32 xdim, int32 ydim)
#else
int DFGRgetimage(filename, image, xdim, ydim)
char *filename;
VOIDP image;
int32 xdim, ydim;
#endif
{
    /* 0 == C */
    return(DFGRIgetimlut(filename, image, xdim, ydim, IMAGE, 0));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRsetcompress
 * Purpose: set compression scheme to use
 * Inputs:
 *      scheme - compression scheme
 *      cinfo - compression information structure
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRsetcompress(int32 scheme,comp_info *cinfo)
#else
int DFGRsetcompress(scheme,cinfo)
int32 scheme;
comp_info *cinfo;
#endif
{
    char *FUNC="DFGRsetcompress";

    if(scheme<0 || scheme>COMP_MAX_COMP || compress_map[scheme]==0)
        HRETURN_ERROR(DFE_BADSCHEME, FAIL);

    /* map JPEG compression into correct type of JPEG compression */
    if(scheme==COMP_JPEG)
        Grcompr = DFTAG_JPEG;       /* Set the compression scheme */
    else    /* otherwise, just use mapped tag */
        Grcompr = compress_map[scheme];
    Grcinfo = (*cinfo);       /* Set the compression parameters */
    return SUCCEED;
}   /* end DFGRsetcompress() */

/*-----------------------------------------------------------------------------
 * Name:    DFGRsetlutdims
 * Purpose: set dimensions of lut to write next
 * Inputs:  xdim, ydim: dimensions of lut
 *          ncomps: no of components
 *          il: interlace of lut
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRsetlutdims(int32 xdim, int32 ydim, int ncomps, int il)
#else
int DFGRsetlutdims(xdim, ydim, ncomps, il)
int32 xdim, ydim;
int ncomps, il;
#endif
{
    if (DFGRIsetil(il, LUT) < 0)
        return FAIL;
    return(DFGRIsetdims(xdim, ydim, ncomps, LUT));
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRsetlut
 * Purpose: set lut for subsequent RIGs
 * Inputs:  lut: lookup table to write
 *          xdim, ydim: dimensions of array lut
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIaddimlut
 * Remarks: array lut is assumed to be xdim * ydim * ncomps bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRsetlut(VOIDP lut, int32 xdim, int32 ydim)
#else
int DFGRsetlut(lut, xdim, ydim)
VOIDP lut;
int32 xdim, ydim;
#endif
{
    /* 0 == C, 0 == no newfile */
    return(DFGRIaddimlut((char*)NULL, lut, xdim, ydim, LUT, 0, 0));
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRaddlut
 * Purpose: write lut to file, associate it with subsequent RIGs
 * Inputs:  filename: name of HDF file
 *          lut: lookup table to write
 *          xdim, ydim: dimensions of array lut
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIaddimlut
 * Remarks: array lut is assumed to be xdim * ydim * ncomps bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRaddlut(char *filename, VOIDP lut, int32 xdim, int32 ydim)
#else
int DFGRaddlut(filename, lut, xdim, ydim)
char *filename;
VOIDP lut;
int32 xdim, ydim;
#endif
{
    /* 0 == C, 0 == no new file */
    return(DFGRIaddimlut(filename, lut, xdim, ydim, LUT, 0, 0));
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRsetimdims
 * Purpose: set dimensions of image to write next
 * Inputs:  xdim, ydim: dimensions of image
 *          ncomps: no of components
 *          il: interlace of image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIsetdims
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRsetimdims(int32 xdim, int32 ydim, int ncomps, int il)
#else
int DFGRsetimdims(xdim, ydim, ncomps, il)
int32 xdim, ydim;
int ncomps, il;
#endif
{
    if (DFGRIsetil(il, IMAGE) < 0)
        return FAIL;
    return(DFGRIsetdims(xdim, ydim, ncomps, IMAGE));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRaddimage
 * Purpose: Write out image
 * Inputs:  filename: name of HDF file
 *          image: image to write
 *          xdim, ydim: dimensions of array image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIaddimlut
 * Remarks: array image is assumed to be xdim * ydim * ncomps bytes
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRaddimage(char *filename, VOIDP image, int32 xdim, int32 ydim)
#else
int DFGRaddimage(filename, image, xdim, ydim)
char *filename;
VOIDP image;
int32 xdim, ydim;
#endif
{
    /* 0 == C, 0 == not new file */
    return(DFGRIaddimlut(filename, image, xdim, ydim, IMAGE, 0, 0));
}

#ifdef PROTOTYPE
int DFGRputimage(char *filename, VOIDP image, int32 xdim, int32 ydim)
#else
int DFGRputimage(filename, image, xdim, ydim)
char *filename;
VOIDP image;
int32 xdim, ydim;
#endif
{
    /* 0 == C, 1 == new file */
    return(DFGRIaddimlut(filename, image, xdim, ydim, IMAGE, 0, 1));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRreadref
 * Purpose: Set ref of rig to get next
 * Inputs:  filename: file to which this applies
 *          ref: reference number of next get
 * Returns: 0 on success, -1 on failure
 * Users:   HDF programmers, other routines and utilities
 * Invokes: DFGRIopen, DFIfind, DFclose
 * Remarks: checks if rig with this ref exists
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRreadref(char *filename, uint16 ref)
#else
int DFGRreadref(filename, ref)
    char *filename;
    uint16 ref;
#endif
{
    int32 file_id;
    int32 aid;

    HEclear();

    file_id = DFGRIopen(filename, DFACC_READ);
    if (file_id == FAIL)
       return FAIL;

    aid = Hstartread(file_id, DFTAG_RIG, ref);
    if (aid == FAIL)
        return(HDerr(file_id));
    Hendaccess(aid);

    Grrefset = ref;
    return(Hclose(file_id));
}


/*****************************************************************************/
/* This is the next lower layer - procedures to read in and write out a RIG. */
/*****************************************************************************/

/*-----------------------------------------------------------------------------
 * Name:    DFGRgetrig
 * Purpose: Read a RIG into memory
 * Inputs:  file_id: pointer to HDF file containing RIG
 *          ref: reference number of RIG to get
 *          rig: struct in which to place info obtained
 * Returns: 0 on success, -1 on failure with DFerror set
 *          contents of RIG in the struct rig
 * Users:   HDF programmers, utilities, DFGRIgetdims,DFGRIgetimlut,
 *          other routines
 * Invokes: DFdiget, DFdinext, DFIcheck, DFgetelement
 * Remarks: incomplete - does not support DFTAG_MA etc.
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
PRIVATE int DFGRgetrig(int32 file_id, uint16 ref, DFGRrig *rig)
#else
PRIVATE int DFGRgetrig(file_id, ref, rig)
    int32 file_id;
    uint16 ref;
    DFGRrig *rig;
#endif
{
    char *FUNC="DFGRgetrig";
    uint16 elt_tag, elt_ref;
    uint8 ntstring[4];
    int type;
    int32 GroupID;
    uint8 GRtbuf[64];       /* local buffer for reading RIG info */

    HEclear();
    if (!HDvalidfid(file_id) || !ref) {
       HERROR(DFE_ARGS);
        return FAIL;
    }
    
    /* read RIG into memory */
    if ((GroupID = DFdiread(file_id, DFTAG_RIG, ref)) == FAIL) 
        return FAIL;

    *rig = Grzrig;             /* fill rig with zeroes */
    while (!DFdiget(GroupID, &elt_tag, &elt_ref)) {  /* get next tag/ref */
        switch (elt_tag) {     /* process tag/ref */
            case DFTAG_CI:
            case DFTAG_RI:
            case DFTAG_LUT:
                type = (elt_tag==DFTAG_LUT) ? LUT : IMAGE;
                rig->data[type].tag = elt_tag;
                rig->data[type].ref = elt_ref;
                break;
            case DFTAG_ID:          /* read description info */
            case DFTAG_LD:
                type = (elt_tag==DFTAG_LD) ? LUT : IMAGE;
               if (Hgetelement(file_id, elt_tag, elt_ref, GRtbuf) != FAIL) {
                    uint8 *p;
                    p = GRtbuf;
                    INT32DECODE(p, rig->datadesc[type].xdim);
                    INT32DECODE(p, rig->datadesc[type].ydim);
                    UINT16DECODE(p, rig->datadesc[type].nt.tag);
                    UINT16DECODE(p, rig->datadesc[type].nt.ref);
                    INT16DECODE(p, rig->datadesc[type].ncomponents);
                    INT16DECODE(p, rig->datadesc[type].interlace);
                    UINT16DECODE(p, rig->datadesc[type].compr.tag);
                    UINT16DECODE(p, rig->datadesc[type].compr.ref);
                } else
                   return FAIL;

                if (rig->datadesc[type].nt.tag==0)
                   break; /* old RIGs */

               /* read NT */
                if (Hgetelement(file_id, rig->datadesc[type].nt.tag,
                               rig->datadesc[type].nt.ref, ntstring) == FAIL)
                    return FAIL;
                if ((ntstring[2]!=8) || (ntstring[1]!=DFNT_UCHAR)) {
                    HERROR(DFE_BADCALL);
                    return FAIL;
                }
                break;
            default:           /* ignore unknown tags */
                break;
        }
    }
    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRaddrig
 * Purpose: Write RIG struct out to HDF file
 * Inputs:  file_id: HDF file pointer
 *          ref: ref to write RIG with
 *          rig: struct containing RIG info to write
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF programmers, utilities, DFGRIaddimlut, other routines
 * Invokes: DFIcheck, DFdistart, DFdiadd, DFdiend, DFputelement
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
PRIVATE int DFGRaddrig(int32 file_id, uint16 ref, DFGRrig *rig)
#else
PRIVATE int DFGRaddrig(file_id, ref, rig)
    int32 file_id;
    uint16 ref;
    DFGRrig *rig;
#endif
{
    char *FUNC="DFGRaddrig";
    uint8 ntstring[4];
    int32 lutsize;
    int32 GroupID;
    uint8 GRtbuf[64];       /* local buffer for reading RIG info */

    HEclear();

    if (!HDvalidfid(file_id) || !ref) {
       HERROR(DFE_ARGS);
        return FAIL;
    }

    if (Ref.nt<=0) {           /* if nt not previously written to file */
                               /* construct and write out NT */
        ntstring[0] = DFNT_VERSION; /* version */
        ntstring[1] = DFNT_UCHAR; /* type */
        ntstring[2] = 8;       /* width: RIG data is 8-bit chars */
        ntstring[3] = DFNTC_BYTE; /* class: data are numeric values */
        if (Hputelement(file_id, DFTAG_NT, ref,
                       (uint8 *)ntstring, (int32) 4)  == FAIL)
            return FAIL;
        rig->datadesc[IMAGE].nt.tag = DFTAG_NT;
        rig->datadesc[IMAGE].nt.ref = ref;
        Ref.nt = ref;
    }

    if (Ref.dims[IMAGE]==0) {
        uint8 *p;
        p = GRtbuf;
        INT32ENCODE(p, rig->datadesc[IMAGE].xdim);
        INT32ENCODE(p, rig->datadesc[IMAGE].ydim);
        UINT16ENCODE(p, rig->datadesc[IMAGE].nt.tag);
        UINT16ENCODE(p, rig->datadesc[IMAGE].nt.ref);
        INT16ENCODE(p, rig->datadesc[IMAGE].ncomponents);
        INT16ENCODE(p, rig->datadesc[IMAGE].interlace);
        UINT16ENCODE(p, rig->datadesc[IMAGE].compr.tag);
        UINT16ENCODE(p, rig->datadesc[IMAGE].compr.ref);
        if (Hputelement(file_id, DFTAG_ID, ref,
                       GRtbuf, (int32)(p-GRtbuf)) == FAIL)
            return FAIL;

        Ref.dims[IMAGE] = ref;
    }
    if (!Ref.lut) {            /* associated lut not written to this file */
        if (Grlutdata==NULL) { /* no lut associated */
            HERROR(DFE_ARGS);
            return FAIL;
        }
        lutsize = Grwrite.datadesc[LUT].xdim * Grwrite.datadesc[LUT].ydim *
           Grwrite.datadesc[LUT].ncomponents;
        if (Hputelement(file_id, DFTAG_LUT, ref,
                       Grlutdata, (int32)lutsize) == FAIL)
           return FAIL;
        rig->data[LUT].tag = DFTAG_LUT;
        rig->data[LUT].ref = ref;
        Ref.lut = ref;
    }

    if (Ref.dims[LUT]==0) {
        uint8 *p;
        p = GRtbuf;
        INT32ENCODE(p, rig->datadesc[LUT].xdim);
        INT32ENCODE(p, rig->datadesc[LUT].ydim);
        UINT16ENCODE(p, rig->datadesc[LUT].nt.tag);
        UINT16ENCODE(p, rig->datadesc[LUT].nt.ref);
        INT16ENCODE(p, rig->datadesc[LUT].ncomponents);
        INT16ENCODE(p, rig->datadesc[LUT].interlace);
        UINT16ENCODE(p, rig->datadesc[LUT].compr.tag);
        UINT16ENCODE(p, rig->datadesc[LUT].compr.ref);
        if (Hputelement(file_id, DFTAG_LD, ref,
                       GRtbuf, (int32)(p-GRtbuf)) == FAIL)
            return FAIL;
        Ref.dims[LUT] = ref;
    }

    /* prepare to start writing rig */
    /* ### NOTE: the parameter to this call may go away */
    if ((GroupID = DFdisetup(10)) == FAIL)
       return FAIL; /* max 10 tag/refs in set */
    /* add tag/ref to RIG - image description, image and lookup table */
    if (DFdiput(GroupID, DFTAG_ID,(uint16)Ref.dims[IMAGE]) == FAIL)
       return FAIL;

    if (DFdiput(GroupID, rig->data[IMAGE].tag, rig->data[IMAGE].ref) == FAIL)
       return FAIL;

    if ((Ref.dims[LUT]>0)
       && (DFdiput(GroupID, DFTAG_LD, (uint16)Ref.dims[LUT]) == FAIL))
       return FAIL;

    if ((Ref.lut>0)
       && (DFdiput(GroupID, rig->data[LUT].tag, rig->data[LUT].ref) == FAIL))
       return FAIL;

    /* write out RIG */
    return(DFdiwrite(file_id, GroupID, DFTAG_RIG, ref));
}

/*****************************************************************************/
/*----------------------- Internal routines ---------------------------------*/
/*****************************************************************************/


/*-----------------------------------------------------------------------------
 * Name:    DFGRIopen
 * Purpose: open or reopen a file
 * Inputs:  filename: name of file to open
 *          access : access mode
 * Returns: file pointer on success, NULL on failure with DFerror set
 * Users:   HDF systems programmers, all the RIG routines
 * Invokes: DFopen
 * Remarks: This is a hook for someday providing more efficient ways to
 *          reopen a file, to avoid re-reading all the headers
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFGRIopen(char *filename, int access)
#else
int32 DFGRIopen(filename, access)
    char *filename;
    int access;
#endif
{
    int32 file_id;

    file_id = Hopen(filename, access, 0);
    if (file_id == FAIL) return FAIL;

    /* use reopen if same file as last time - more efficient */
    if (HDstrncmp(Grlastfile,filename,DF_MAXFNLEN) || (access==DFACC_CREATE)) {
       /* treat create as different file */
        Grrefset = 0;          /* no ref to get set for this file */
        Grnewdata = 0;
        if (Ref.lut>0) Ref.lut = 0;
        if (Grlutdata==NULL)
           Ref.lut = (-1);     /* no LUT if not a "set" call */
        if (Ref.dims[IMAGE]>0)
           Ref.dims[IMAGE] = 0;
        if (Ref.dims[LUT]>0)
           Ref.dims[LUT] = 0;
        if (Ref.nt>0)
           Ref.nt = 0;
        Grread = Grzrig;        /* no rigs read yet */
    }

    HDstrncpy(Grlastfile, filename, DF_MAXFNLEN);
    /* remember filename, so reopen may be used next time if same file */
    return(file_id);
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRIriginfo
 * Purpose: Get information about next RIG in file
 * Inputs:  file_id: pointer to DF file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF systems programmers
 * Invokes: DFIfind, DFgetelement, DFGRgetrig
 * Remarks: if Grrefset set, gets image with that ref, if any
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
PRIVATE int DFGRIriginfo(int32 file_id)
#else
PRIVATE int DFGRIriginfo(file_id)
int32 file_id;
#endif
{
    int i, isfirst;
    uint16 newref=0, newtag, gettag, getref, ref, dummy;
    struct {
        uint16 xdim;
        uint16 ydim;
    } r8dims;
    char *p;
    int32 aid;

    isfirst = (Grrefset!=0) || (Grread.data[IMAGE].ref==0);
    getref = Grrefset;         /* ref if specified, else 0 */
    Grrefset = 0;              /* no longer need to remember specified ref */
    gettag = DFTAG_RIG;
    for (i=0; i<4; i++) {      /* repeat for RIG, RI8, CI8, II8 */
       if (isfirst) {
           aid = Hstartread(file_id, gettag, getref);
       } else {
           aid = Hstartread(file_id, gettag, Grread.data[IMAGE].ref);
           if ((aid != FAIL) &&
               Hnextread(aid, gettag, getref, DF_CURRENT) == FAIL) {
               Hendaccess(aid);
               aid = FAIL;
           }
       }
       if (aid == FAIL) {
           /* not found */
            if (gettag==DFTAG_RIG) { /* were looking for RIGs */
                if ((Grread.data[IMAGE].tag==DFTAG_RI) /* file has Rigs */
                    || (Grread.data[IMAGE].tag==DFTAG_CI)) {
                    return FAIL;       /* no more to return */
                }
                gettag = DFTAG_RI8; /* if no RIGs in file, look for RI8s */
            }
            else if ((gettag==DFTAG_II8) && (!newref)) { /* no RI8/CI8/II8 */
                return FAIL;
            }
            continue;          /* continue checking */
        }
       /* found */
       HQuerytagref(aid, &dummy, &ref);
       Hendaccess(aid);
        if (!newref || (ref < newref)) { /* is it next one? */
            newref = ref;      /* remember tag, ref */
            newtag = gettag;
        }
        if (gettag==DFTAG_RI8)
           gettag = DFTAG_CI8; /* check next */
        else if (gettag==DFTAG_CI8)
           gettag = DFTAG_II8;
        else
           break;              /* all checked, quit */
    }

    if (newtag==DFTAG_RIG) {
        if (DFGRgetrig(file_id, newref, &Grread) == FAIL)
            return FAIL;
    } else {
        Grread.data[IMAGE].ref = newref;
        Grread.data[IMAGE].tag = newtag;
        if (newtag==DFTAG_CI8)
           Grread.datadesc[IMAGE].compr.tag = DFTAG_RLE;
        else if (newtag==DFTAG_II8)
            Grread.datadesc[IMAGE].compr.tag = DFTAG_IMC;

        if (Hgetelement(file_id, DFTAG_ID8, newref, (uint8 *)&r8dims) == FAIL)
            return FAIL;
        p = (char *) &r8dims;
        UINT16DECODE(p, Grread.datadesc[IMAGE].xdim);
        UINT16DECODE(p, Grread.datadesc[IMAGE].ydim);

       aid = Hstartread(file_id, DFTAG_IP8, newref);
        if (aid != FAIL) {
            Grread.data[LUT].tag = DFTAG_IP8;
            Grread.data[LUT].ref = newref;
           Hendaccess(aid);
        }
        HEclear();     /* reset it, just in case! */
    }

    /* if LUT dimensions not set, set default dimensions */
    if (Grread.data[LUT].tag && Grread.datadesc[LUT].xdim==0) {
        Grread.datadesc[LUT].xdim = 256;
        Grread.datadesc[LUT].ydim = 1;
        Grread.datadesc[LUT].ncomponents = 3;
    }

    Grlastref = Grread.data[IMAGE].ref;        /* remember ref read */

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRIgetdims
 * Purpose: get dimensions of next image/lut from RIG
 * Inputs:  filename: name of HDF file
 *          pxdim, pxdim: pointer to locations for returning x,y dimensions
 *          pncomps: location for returning no of components
 *          pil: location for returning interlace of image/lut in file
 *          type: LUT to get lut dims, IMAGE to get image dims
 * Returns: 0 on success, -1 on failure with DFerror set
 *          *pxdim, *pydim are set to dimensions of the next image on success
 *          *pncomps, *pil set on success
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: DFGRIopen, DFclose, DFGRIriginfo, DFIerr
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRIgetdims(char *filename, int32 *pxdim, int32 *pydim,
                        int *pncomps, int *pil, int type)
#else
int DFGRIgetdims(filename, pxdim, pydim, pncomps, pil, type)
    char *filename;
    int32 *pxdim, *pydim;
    int *pncomps, *pil, type;
#endif
{
    char *FUNC="DFGRIgetdims";
    int32 file_id;

    HEclear();

    file_id = DFGRIopen(filename, DFACC_READ);
    if (file_id == FAIL)
       return FAIL;

    if (type==IMAGE) {         /* getimdims sequences, getlutdims does not */
        if (DFGRIriginfo(file_id)
           == FAIL)            /* reads next RIG or RI8 from file */
            return(HDerr(file_id)); /* on error, close file and return -1 */
        Grnewdata = 1;
    }

    if (type==LUT && Grread.data[LUT].ref==0) {
            HERROR(DFE_NOMATCH);
            return HDerr(file_id);             /* no LUT */
    }
    if (pxdim)
       *pxdim = Grread.datadesc[type].xdim;
    if (pydim)
       *pydim = Grread.datadesc[type].ydim;
    if (pncomps)
       *pncomps = Grread.datadesc[type].ncomponents;
    if (pil)
       *pil = Grread.datadesc[type].interlace;
    return(Hclose(file_id));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRIreqil
 * Purpose: set interlace with which to get subsequent images/luts
 * Inputs:  il: interlace to get image/lut with
 *          type: LUT for luts, IMAGE for images
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRIreqil(intn il, intn type)
#else
int DFGRIreqil(il, type)
    intn il, type;
#endif
{
    HEclear();

    Grreqil[type] = il;

    return SUCCEED;
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRIgetimlut
 * Purpose: get next image/lut from a RIG
 * Inputs:  filename: name of HDF file 
 *          imlut: space to read image/lut into
 *          xdim, ydim: dimensions of space allocated by user for image/lut
 *          type: LUT for luts, IMAGE for images
 *          isfortran: 0 if called from C, 1 if called from Fortran
 * Returns: 0 on success, -1 on failure with DFerror set
 *          image/lut in imlut
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFGRIopen, DFGRIriginfo, DFIerr, DFclose, DFgetelement, DFgetcomp
 * Remarks: Will also get RI8s and CI8s if no RIGs in file
 *          Normally, DFGRgetimdims is called first and it moves to next image
 *          But if that is not called, DFGRgetimlut will itself move to next
 *          image (but not next lut!).
 *          Automatically decompresses images/luts
 *---------------------------------------------------------------------------*/

/* shut lint up */
/* ARGSUSED */
#ifdef PROTOTYPE
int DFGRIgetimlut(char *filename, VOIDP imlut, int32 xdim, int32 ydim,
                         int type, int isfortran)
#else
int DFGRIgetimlut(filename, imlut, xdim, ydim, type, isfortran)
    char *filename;
    int32 xdim, ydim;
    VOIDP imlut;
    int type, isfortran;
#endif
{
    char *FUNC="DFGRIgetimlut";
    int32 file_id;
    int32 currpos[3], currmax[3], destsize[3], bufsize, i, j;
    uint8 *buf, *destp;
    int32 aid;

    HEclear();

    file_id = DFGRIopen(filename, DFACC_READ);
    if (file_id == FAIL)
       return FAIL;

    if ((type==IMAGE) && (Grnewdata!=1)) { /* if Grread not fresh */
        if (DFGRIriginfo(file_id)
           == FAIL) /* reads next RIG or RI8 from file */
            return(HDerr(file_id)); /* on error, close file and return -1 */
    }
    if (Grnewdata==0) {
        HERROR(DFE_BADCALL);
        return FAIL;
    }
    Grnewdata = 0;             /* read new RIG next time */

    if ((xdim!=Grread.datadesc[type].xdim)
            || (ydim!=Grread.datadesc[type].ydim)) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* read image/lut */
    if (Grread.datadesc[type].compr.tag) { /* compressed image/lut */
        if ((Grreqil[type] >= 0) &&
                (Grreqil[type] != Grread.datadesc[type].interlace)) {
            HERROR(DFE_UNSUPPORTED);
            return FAIL;
        }
        if (DFgetcomp(file_id, Grread.data[type].tag, Grread.data[type].ref,
                     (uint8*)imlut, Grread.datadesc[type].xdim,
                     Grread.datadesc[type].ydim,
                     Grread.datadesc[type].compr.tag) == FAIL)
            return(HDerr(file_id));
    } else {                   /* non-compressed raster image/lut */
        if (Grreqil[type]>=0) {
            if (Grreqil[type]>=Grread.datadesc[type].ncomponents) {
                HERROR(DFE_ARGS);
                return FAIL;
            }
            else if (Grreqil[type]!=Grread.datadesc[type].interlace) {
               aid = Hstartread(file_id, Grread.data[type].tag,
                                Grread.data[type].ref);
                if (aid == FAIL)
                    return(HDerr(file_id));
               /* current position in data */
                currpos[0] = currpos[1] = currpos[2] = 0;
                currmax[0] = Grread.datadesc[type].ncomponents;
                currmax[1] = Grread.datadesc[type].xdim;
                currmax[2] = Grread.datadesc[type].ydim;

               /* compute size of each dim of dest array */
                destsize[0] = destsize[1] = 1;
                destsize[2] = currmax[1]; /* xdim is more sig than ydim */
                if (Grreqil[type]==0) {
                    destsize[1] *= currmax[0];
                    destsize[2] *= currmax[0];
                }
                else if (Grreqil[type]==1) {
                    destsize[0] *= currmax[1];
                    destsize[2] *= currmax[0];
                }
                else if (Grreqil[type]==2) {
                    destsize[0] *= currmax[1] * currmax[2];
                }

                bufsize = Grread.datadesc[type].ydim *
                   Grread.datadesc[type].ncomponents;
                buf = (uint8 *)HDgetspace((uint32)bufsize);
                if (buf==NULL) {
                   Hendaccess(aid);
                   return(HDerr(file_id));
                }

               /* read byte by byte and copy */
                for (i=0; i<Grread.datadesc[type].xdim; i++) {
                    if (Hread(aid, bufsize, buf) == FAIL) {
                       Hendaccess(aid);
                       return(HDerr(file_id));
                    }
                    for (j=0; j<bufsize; j++) {
                        destp =  (uint8 *) imlut + destsize[0] * currpos[0] +
                                                   destsize[1] * currpos[1] +
                                                   destsize[2] * currpos[2];
                        *destp = buf[j];
                        if (Grread.datadesc[type].interlace==0) {
                            if (++currpos[0] == currmax[0]) {
                                currpos[0] = 0;
                                if (++currpos[1] == currmax[1]) {
                                    currpos[1] = 0;
                                    if (++currpos[2] == currmax[2]) break;
                                }
                            }
                        } else if (++currpos[1]==currmax[1]) {
                            currpos[1] = 0;
                            if (Grread.datadesc[type].interlace==1) {
                                if (++currpos[0] == currmax[0]) {
                                    currpos[0] = 0;
                                    if (++currpos[2] == currmax[2]) break;
                                }
                            } else {
                                if (++currpos[2] == currmax[2]) {
                                    currpos[2] = 0;
                                    if (++currpos[0] == currmax[0]) break;
                                }
                            }
                        }
                    }
                }
               Hendaccess(aid);
	       HDfreespace((VOIDP)buf);
                return(Hclose(file_id));
            }
        }
        if (Hgetelement(file_id, Grread.data[type].tag, Grread.data[type].ref,
                       (uint8 *)imlut) == FAIL)
            return(HDerr(file_id));
    }

    return(Hclose(file_id));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRIsetdims
 * Purpose: set dimensions of image/lut
 * Inputs:  xdim, ydim: dimensions of lut
 *          ncomps: no of components
 *          il: interlace of lut
 *          type: LUT if lut, IMAGE if image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRIsetdims(int32 xdim, int32 ydim, intn ncomps, int type)
#else
int DFGRIsetdims(xdim, ydim, ncomps, type)
    int32 xdim, ydim;
    intn ncomps;
    int type;
#endif
{
    char *FUNC="DFGRIsetdims";
    if (ncomps == FAIL || (xdim<=0) || (ydim<=0)) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    Grwrite.datadesc[type].xdim = xdim;
    Grwrite.datadesc[type].ydim = ydim;
    Grwrite.datadesc[type].ncomponents = ncomps;

    Ref.dims[type] = 0;

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRIsetil
 * Purpose: set interlace of image/lut
 * Inputs:  il: interlace of lut
 *          type: LUT if lut, IMAGE if image
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFGRIsetil(int il, int type)
#else
int DFGRIsetil(il, type)
    int il;
    int type;
#endif
{
    char *FUNC="DFGRIsetil";
    if (il == FAIL) {
        HERROR(DFE_ARGS);
        return FAIL;
    }
    Grwrite.datadesc[type].interlace = il;

    return SUCCEED;
}
/*-----------------------------------------------------------------------------
 * Name:    DFGRIrestart
 * Purpose: restart file
 * Inputs:
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL (high-level library) users, utilities, other routines
 * Invokes: none
 * Remarks: none
 *---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int DFGRIrestart(void)
#else
int DFGRIrestart()
#endif
{
    Grlastfile[0] = '\0';
    Grrefset = 0;
    return SUCCEED;
}


/*-----------------------------------------------------------------------------
 * Name:    DFGRIaddimlut
 * Purpose: Internal routine to write RIG to file
 * Inputs:  filename: name of HDF file
 *          imlut: image/lut to be written to file
 *          xdim, ydim: dimensions of image/lut
 *          type: LUT if lut, IMAGE if image
 *          isfortran: 0 if called from C, 1 if called from Fortran
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF systems programmers, DFGRaddimage, DFGRaddlut, DFGRsetlut
 * Invokes: DFGRIopen, DFclose, DFputelement, DFdup, DFGRaddrig, DFputcomp,
 *          DFIerr
 * Remarks: Creates both RIG and RI8/CI8 tags, to accomodate older programs
 *          LUT will be associated with image if set previously
 *---------------------------------------------------------------------------*/

/* shut lint up */
/* ARGSUSED */
#ifdef PROTOTYPE
int DFGRIaddimlut(char *filename, VOIDP imlut, int32 xdim, int32 ydim,
                 int type, int isfortran, int newfile)
#else
int DFGRIaddimlut(filename, imlut, xdim, ydim, type, isfortran, newfile)
    char *filename;
    int32 xdim, ydim;
    VOIDP imlut;
    int type, isfortran, newfile;
#endif
{
    char *FUNC="DFGRIaddimlut";
    int32 file_id;
    uint16 wtag, wref;         /* tag of image/lut being written */
    uint8 *newlut;
    int32 lutsize;
    int is8bit;
    struct {
        uint16 xdim;
        uint16 ydim;
    } r8dims;
    uint8 *p;

    HEclear();

    if (0 != HDstrcmp(Grlastfile,filename)) {  /* if new file, reset dims */
      Grwrite.datadesc[type].xdim = xdim;
      Grwrite.datadesc[type].ydim = ydim;
      Ref.dims[type] = 0;                  /* indicate set & not written */
    }
    
    if ((Ref.dims[type] == 0 && (xdim != Grwrite.datadesc[type].xdim
            || ydim != Grwrite.datadesc[type].ydim)) || !imlut) {
        HERROR(DFE_ARGS);
        return FAIL;
    }
    
    /* if dims not set, set dimensions */
    if (Ref.dims[type] == FAIL)        
        if (DFGRIsetdims(xdim, ydim, 1, type) == FAIL)
            return FAIL;

    /* default: ncomps=1, il=0 */
    
    if ((type==LUT) && (filename==NULL)) { /* set call */
        if (Grlutdata) {
            HDfreespace((VOIDP) Grlutdata);
            Grlutdata = NULL;
        }
        Ref.lut = -1;
        if (imlut==NULL)
            return SUCCEED;
        lutsize = Grwrite.datadesc[LUT].xdim * Grwrite.datadesc[LUT].ydim
                * Grwrite.datadesc[LUT].ncomponents;
        Grlutdata = (uint8 *) HDgetspace((uint32)lutsize);
        if(Grlutdata==NULL)
            return FAIL;
        HDmemcpy(Grlutdata, imlut, (int)lutsize);
        Ref.lut = 0;
        return SUCCEED;
    }
    
    file_id = DFGRIopen(filename, newfile ? DFACC_CREATE : DFACC_RDWR);
    if (file_id==(int32)NULL)
       return FAIL;

    wref = Hnewref(file_id);
    if (!wref)
       return(HDerr(file_id));

#ifdef OLD_WAY
    is8bit = (Grwrite.datadesc[IMAGE].ncomponents == 1);
#else
    /* make 8-bit compatibility only for older 8-bit stuff, not JPEG */
    is8bit = ((Grwrite.datadesc[IMAGE].ncomponents == 1) &&
            (Grcompr!=DFTAG_GREYJPEG && Grcompr!=DFTAG_JPEG));
#endif

    wtag = (uint16)((type==LUT) ? DFTAG_LUT : (Grcompr ? DFTAG_CI : DFTAG_RI));
    Grwrite.data[type].tag = wtag;


    /* write out image/lut */
    if ((type==IMAGE) && Grcompr) {
        lutsize = Grwrite.datadesc[LUT].xdim * Grwrite.datadesc[LUT].ydim *
           Grwrite.datadesc[LUT].ncomponents;
        if (Grcompr==DFTAG_IMC) {
            if (Grlutdata==NULL) {
                HERROR(DFE_BADCALL);
                return(HDerr(file_id));
            }
            newlut = (uint8 *) HDgetspace((uint32)lutsize);
        }
        if (DFputcomp(file_id, wtag, wref, (uint8*)imlut, xdim, ydim,
                    (uint8*)Grlutdata, (uint8*)newlut, (int16)Grcompr, &Grcinfo)
                     == FAIL)
            return(HDerr(file_id));
    } else {                   /* image need not be compressed */
        if (Hputelement(file_id, (uint16)wtag, (uint16)wref, (uint8 *)imlut,
                       xdim*ydim*Grwrite.datadesc[type].ncomponents) == FAIL)
            return(HDerr(file_id));
    }
    Grwrite.data[type].ref = wref;
    Grwrite.aspectratio = (float32)1.0;

    /* Write out Raster-8 tags for those who want it */
    if (is8bit) {
        wtag = (uint16)((type==LUT) ? DFTAG_IP8 : Grcompr ?
            ((Grcompr==DFTAG_RLE) ? DFTAG_CI8 :
                    DFTAG_II8) : DFTAG_RI8);

        if(Hdupdd(file_id, wtag, wref, Grwrite.data[type].tag, wref) == FAIL)
            return(HDerr(file_id));
      } /* end if */

    if (type==IMAGE)
       Grwrite.datadesc[IMAGE].compr.tag = (uint16)Grcompr;

    if (Grcompr==DFTAG_IMC) {
        if (Hputelement(file_id, DFTAG_LUT, wref, newlut, lutsize) == FAIL)
            return(HDerr(file_id));
        Ref.lut = wref;
    }

    if (DFGRaddrig(file_id, wref, &Grwrite) == FAIL) /* writes ID, NT */
        return(HDerr(file_id));

    if (is8bit) {
       /* put in Raster-8 stuff also, for those who want it */
        if ((Ref.lut>=0)
           && Hdupdd(file_id, DFTAG_IP8, wref, DFTAG_LUT, wref) == FAIL)
            return(HDerr(file_id));
        p = (uint8 *) &r8dims.xdim;
        UINT16ENCODE(p, Grwrite.datadesc[IMAGE].xdim);
        UINT16ENCODE(p, Grwrite.datadesc[IMAGE].ydim);
        if (Hputelement(file_id, DFTAG_ID8, wref, (uint8 *)&r8dims, (int32) 4)
           == FAIL)
            return(HDerr(file_id));
    }

    if (Grcompr==DFTAG_IMC) {
        Ref.lut = 0;
        HDfreespace((VOIDP) newlut);
        newlut = NULL;
    }

    Grlastref = wref;          /* remember the last ref */

    wref = 0;                  /* don't know ref to write next */

    return(Hclose(file_id));
}

/*-----------------------------------------------------------------------------
 * Name:    DFGRlastref
 * Purpose: Return last ref written or read
 * Inputs:  none
 * Globals: Grlastref
 * Returns: ref on success
 * Users:   HDF users, utilities, other routines
 * Invokes: none
 * Method:  return Grlastref
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
uint16 DFGRIlastref(void)
#else
uint16 DFGRIlastref()
#endif
{
    return((uint16) Grlastref);
}
