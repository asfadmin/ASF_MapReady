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

/*+ hextelt.c
 Routines for external elements, i.e., data elements that reside on
 some other file.  These elements have no limits on their length.
 While users are prevented from reading beyond what is written, a 
 user can write an unlimited amount of data.

 17-Mar-93
 Adding offset and "starting length" to elements so that a user can
 take an existing file with some data in it and create an HDF file
 which has a pointer to that data.
+*/

#include "hdf.h"
#include "herr.h"
#include "hfile.h"

/* extinfo_t -- external elt information structure */

typedef struct {
    int attached;              /* number of access records attached
                                  to this information structure */
    int32 extern_offset;
    int32 length;              /* length of this element */
    int32 length_file_name;    /* length of the external file name */
    hdf_file_t file_external;      /* external file descriptor */
    char *extern_file_name;    /* name of the external file */
} extinfo_t;

/* forward declaration of the functions provided in this module */

PRIVATE int32 HXIstaccess
    PROTO((accrec_t *access_rec, int16 access));
PRIVATE int32 HXIstread
    PROTO((accrec_t *rec));
PRIVATE int32 HXIstwrite
    PROTO((accrec_t *rec));
PRIVATE int32 HXIseek
    PROTO((accrec_t *access_rec, int32 offset, int origin));
PRIVATE int32 HXIread
    PROTO((accrec_t *access_rec, int32 length, VOIDP data));
PRIVATE int32 HXIwrite
    PROTO((accrec_t *access_rec, int32 length, VOIDP data));
PRIVATE int32 HXIinquire
    PROTO((accrec_t *access_rec, int32 *pfile_id, uint16 *ptag, uint16 *pref,
            int32 *plength, int32 *poffset,int32 *pposn, int16 *paccess,
            int16 *pspecial));
PRIVATE int32 HXIendaccess
    PROTO((accrec_t *access_rec));

/* ext_funcs -- table of the accessing functions of the external
   data element function modules.  The position of each function in
   the table is standard */

int32 (*ext_funcs[])() = {
    HXIstread,
    HXIstwrite,
    HXIseek,
    HXIinquire,
    HXIread,
    HXIwrite,
    HXIendaccess,
};

/* ------------------------------- HXcreate ------------------------------- */
/*

 Create a data element in an external file.  If that file already
 exists, we will simply *modify* that file, not delete it and
 start over.  Offset and start_len are for encapsulating data
 that already exists in a seperate file so that it can be referenced
 from the HDF file.

 If the objext we are writing out already exists in an HDF file and 
 is "promoted" then the start_len is ignored.

 Return an AID to the newly created external element, FAIL on error.

*/
#ifdef PROTOTYPE
int32 HXcreate(int32 file_id, uint16 tag, uint16 ref, char *extern_file_name, int32 f_offset, int32 start_len)
#else
int32 HXcreate(file_id, tag, ref, extern_file_name, f_offset, start_len)
    int32 file_id;             /* file record id */
    uint16 tag, ref;           /* tag/ref of the special data element
                                  to create */
    char *extern_file_name;    /* name of external file to use as
                                  data element */
    int32 f_offset,start_len;
#endif
{
    char *FUNC="HXcreate";     /* for HERROR */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access element record */
    int slot;
    dd_t *dd;
    ddblock_t *data_block;     /* dd block ptr to exist data element */
    int32 data_idx;            /* dd list index to existing data element */
    hdf_file_t file_external;      /* external file descriptor */
    extinfo_t *info;           /* special element information */
#ifndef oldspecial
    dd_t *data_dd;             /* dd of existing regular element */
    uint16 special_tag;                /* special version of tag */
#endif

    /* clear error stack and validate args */

    HEclear();
    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0 || !extern_file_name || (f_offset<0)
#ifndef oldspecial
       || SPECIALTAG(tag) || (special_tag = MKSPECIALTAG(tag)) == DFTAG_NULL
#endif
       ) {
       HERROR(DFE_ARGS);
       return FAIL;
    }
    if (!(file_rec->access & DFACC_WRITE)) {
       HERROR(DFE_DENIED);
       return FAIL;
    }

    /* get a slot in the access records table */

    slot = HIget_access_slot();
    if (slot == FAIL) {
       HERROR(DFE_TOOMANY);
       return FAIL;
    }
    access_rec = &access_records[slot];

    /* look for existing data element of the same tag/ref */
    if (FAIL != HIlookup_dd(file_rec, tag, ref, &data_block, &data_idx)) {
#ifndef oldspecial
       data_dd = &(data_block->ddlist[data_idx]);
       if (SPECIALTAG(data_dd->tag)) {

           /* abort since we cannot convert the data element to an external
              data element */

           HERROR(DFE_CANTMOD);
           access_rec->used = FALSE;
           return FAIL;
       }
    } else {
       data_dd = (dd_t *) NULL;
#else
       /* abort since we cannot convert the data element to an external
          data element */

       HERROR(DFE_CANTMOD);
       access_rec->used = FALSE;
       return FAIL;
#endif
    }

    /* look for empty dd to use */
    if (FAIL == HIlookup_dd(file_rec, DFTAG_NULL, DFREF_WILDCARD,
                         &file_rec->null_block, &file_rec->null_idx)) {
       if (FAIL == HInew_dd_block(file_rec, FILE_NDDS(file_rec), FUNC)) {
           HERROR(DFE_NOFREEDD);
           access_rec->used = FALSE;
           return FAIL;
       } else {
           access_rec->block = file_rec->ddlast;
           access_rec->idx   = 0;
       }
    } else {
      access_rec->block = file_rec->null_block;
      access_rec->idx   = file_rec->null_idx;
    }
    dd = &access_rec->block->ddlist[access_rec->idx];

    /* create the external file */

    file_external = HI_OPEN(extern_file_name, DFACC_WRITE);
    if (OPENERR(file_external)) {
        file_external = HI_CREATE(extern_file_name);
        if(OPENERR(file_external)) {
            HERROR(DFE_BADOPEN);
            access_rec->used = FALSE;
            return FAIL;
        }
    }
    
    /* set up the special element information and write it to file */

    access_rec->special_info = (VOIDP) HDgetspace((uint32)sizeof(extinfo_t));
    info = (extinfo_t *) access_rec->special_info;
    if (!info) {
       HERROR(DFE_NOSPACE);
       access_rec->used = FALSE;
       return FAIL;
    }

#ifndef oldspecial
    if (data_dd) {
       VOIDP buf;              /* temporary buffer */
       buf = (VOIDP)HDgetspace((uint32) data_dd->length);
       if (!buf) {
           HERROR(DFE_NOSPACE);
           HDfreespace((VOIDP)info);
           return FAIL;
       }
       if (HI_SEEK(file_rec->file, data_dd->offset) == FAIL) {
           HERROR(DFE_SEEKERROR);
           HDfreespace((VOIDP)info);
           HDfreespace((VOIDP)buf);
           return FAIL;
       }
       if (HI_READ(file_rec->file, buf, data_dd->length) == FAIL) {
           HERROR(DFE_READERROR);
           HDfreespace((VOIDP)info);
           HDfreespace((VOIDP)buf);
           return FAIL;
       }
       if (HI_SEEK(file_external, f_offset) == FAIL) {
           HERROR(DFE_SEEKERROR);
           HDfreespace((VOIDP)info);
           HDfreespace((VOIDP)buf);
           return FAIL;
       }
       if (HI_WRITE(file_external, buf, data_dd->length) == FAIL) {
           HERROR(DFE_WRITEERROR);
           HDfreespace((VOIDP)info);
           HDfreespace((VOIDP)buf);
           return FAIL;
       }
       HDfreespace((VOIDP)buf);
       info->length = data_dd->length;
    } else {
       info->length = start_len;
    }
#endif

    info->attached = 1;
    info->file_external = file_external;
    info->extern_offset = f_offset;
    info->extern_file_name = (char *)HDstrdup(extern_file_name);
    if (!info->extern_file_name) {
       HERROR(DFE_NOSPACE);
       access_rec->used = FALSE;
       return FAIL;
    }

    info->length_file_name = HDstrlen(extern_file_name);
    {
       uint8 *p = tbuf;
       INT16ENCODE(p, SPECIAL_EXT);
       INT32ENCODE(p, info->length);
       INT32ENCODE(p, info->extern_offset);
       INT32ENCODE(p, info->length_file_name);
       HDstrcpy((char *) p, (char *)extern_file_name);
    }
    if (HI_SEEKEND(file_rec->file) == FAIL) {
       HERROR(DFE_SEEKERROR);
       access_rec->used = FALSE;
       return FAIL;
    }
    dd->offset = HI_TELL(file_rec->file);
    dd->length = 14 + info->length_file_name;
    dd->tag = special_tag;
    dd->ref = ref;
    if (HI_WRITE(file_rec->file, tbuf, dd->length) == FAIL) {
       HERROR(DFE_WRITEERROR);
       access_rec->used = FALSE;
       return FAIL;
    }

    if (FAIL == HIupdate_dd(file_rec, access_rec->block,
                           access_rec->idx, FUNC)) {
        access_rec->used = FALSE;
        return FAIL;
    }

    /* add new DD to hash table */
    if (FAIL == HIadd_hash_dd(file_rec, dd->tag, dd->ref, access_rec->block,
                           access_rec->idx)) {
        access_rec->used = FALSE;
        return FAIL;
    }

#ifndef oldspecial
    if (data_dd) {
       Hdeldd(file_id, data_dd->tag, data_dd->ref);
       HIdel_hash_dd(file_rec, data_dd->tag, data_dd->ref);
    }
#endif

    /* update access record and file record */

    access_rec->special_func = ext_funcs;
    access_rec->special = SPECIAL_EXT;
    access_rec->posn = 0;
    access_rec->access = DFACC_WRITE;
    access_rec->file_id = file_id;

    file_rec->attach++;
    if (ref > file_rec->maxref) file_rec->maxref = ref;

    return ASLOT2ID(slot);
}


/* ----------------------------- HXIstaccess ------------------------------ */
/*

  start accessing a data element
  called by HXIstread and HXIstwrite
  
  Return FAIL on error

-*/
#ifdef PROTOTYPE
PRIVATE int32 HXIstaccess(accrec_t *access_rec, int16 access)
#else
PRIVATE int32 HXIstaccess(access_rec, access)
    accrec_t *access_rec;      /* access record */
    int16 access;                        /* access mode */
#endif
{
    char *FUNC="HXIstaccess";  /* for HERROR */
    dd_t *info_dd;             /* dd of the special information element */
    extinfo_t *info;           /* special element information */
    filerec_t *file_rec;       /* file record */

    /* get file record and validate */

    file_rec = FID2REC(access_rec->file_id);
    if (!file_rec || file_rec->refcount == 0 || !(file_rec->access & access)) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* intialize the access record */

    access_rec->special = SPECIAL_EXT;
    access_rec->posn = 0;
    access_rec->access = access;

    /* get the dd for information */

    info_dd = &access_rec->block->ddlist[access_rec->idx];

    /* get the special info record */

    access_rec->special_info = HIgetspinfo(access_rec,
                                          info_dd->tag, info_dd->ref);
    if (access_rec->special_info) {

       /* found it from other access records */

       info = (extinfo_t *)access_rec->special_info;
       info->attached++;

    } else {

       /* look for information in the file */

       if (HI_SEEK(file_rec->file, info_dd->offset+2) == FAIL) {
           HERROR(DFE_SEEKERROR);
           access_rec->used = FALSE;
           return FAIL;
       }
       if (HI_READ(file_rec->file, tbuf, 12) == FAIL) {
           HERROR(DFE_READERROR);
           access_rec->used = FALSE;
           return FAIL;
       }
       access_rec->special_info = (VOIDP) HDgetspace((uint32)sizeof(extinfo_t));
       info = (extinfo_t *) access_rec->special_info;
       if (!info) {
           HERROR(DFE_NOSPACE);
           access_rec->used = FALSE;
           return FAIL;
       }
       {
           uint8 *p = tbuf;
           INT32DECODE(p, info->length);
           INT32DECODE(p, info->extern_offset);
           INT32DECODE(p, info->length_file_name);
       }
       info->extern_file_name = (char *)HDgetspace((uint32)
                                               info->length_file_name + 1);
       if (!info->extern_file_name) {
           HERROR(DFE_NOSPACE);
           access_rec->used = FALSE;
           return FAIL;
       }
       if (HI_READ(file_rec->file, info->extern_file_name,
                  info->length_file_name) == FAIL) {
           HERROR(DFE_READERROR);
           access_rec->used = FALSE;
           return FAIL;
       }
       info->extern_file_name[info->length_file_name] = '\0';
       info->file_external = HI_OPEN(info->extern_file_name, access);
       if (OPENERR(info->file_external)) {
           HERROR(DFE_BADOPEN);
           access_rec->used = FALSE;
           return FAIL;
       }
       info->attached = 1;
    }

    file_rec->attach++;

    return ASLOT2ID(access_rec-access_records);
}

/*- HXIstread
 start reading an external data element
-*/
#ifdef PROTOTYPE
PRIVATE int32 HXIstread(accrec_t *rec)
#else
PRIVATE int32 HXIstread(rec)
    accrec_t *rec;
#endif
{
    return HXIstaccess(rec, DFACC_READ);
}

/*- HXIstwrite
 start writing an external data element
-*/
#ifdef PROTOTYPE
PRIVATE int32 HXIstwrite(accrec_t *rec)
#else
PRIVATE int32 HXIstwrite(rec)
    accrec_t *rec;
#endif
{
    return HXIstaccess(rec, DFACC_WRITE);
}

/*- HXIseek
 seek to offset with the data element
-*/
#ifdef PROTOTYPE
PRIVATE int32 HXIseek(accrec_t *access_rec, int32 offset, int origin)
#else
PRIVATE int32 HXIseek(access_rec, offset, origin)
    accrec_t *access_rec;
    int32 offset;
    int origin;
#endif
{
    char *FUNC="HXIseek";      /* for HERROR */

    /* Adjust offset according to origin.
       there is no upper bound to posn */

    if (origin == DF_CURRENT) offset += access_rec->posn;
    if (origin == DF_END)
       offset += ((extinfo_t *)(access_rec->special_info))->length;
    if (offset < 0) {
       HERROR(DFE_RANGE);
       return FAIL;
    }

    /* set the offset */

    access_rec->posn = offset;
    return SUCCEED;
}

/*- HXIread
 read in a portion of data from the external element
-*/
#ifdef PROTOTYPE
PRIVATE int32 HXIread(accrec_t *access_rec, int32 length, VOIDP data)
#else
PRIVATE int32 HXIread(access_rec, length, data)
    accrec_t *access_rec;      /* access record */
    int32 length;              /* length of data to read in */
    VOIDP data;                        /* data buffer */
#endif
{
    char *FUNC="HXIread";      /* for HERROR */
    extinfo_t *info =          /* information on the special element */
       (extinfo_t *)access_rec->special_info;

    /* validate length */
    if (length < 0) {
       HERROR(DFE_RANGE);
       return FAIL;
    }

    /* adjust length if it falls off the end of the element */


    if (length == 0) length = info->length - access_rec->posn;
    else
        if (length < 0 || access_rec->posn + length > info->length) {
           HERROR(DFE_RANGE);
           return FAIL;
        }

    /* read it in from the file */

    if (HI_SEEK(info->file_external, 
	       access_rec->posn + info->extern_offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    if (HI_READ(info->file_external, data, length) == FAIL) {
       HERROR(DFE_READERROR);
       return FAIL;
    }

    /* adjust access position */

    access_rec->posn += length;

    return length;
}

/*- HXIwrite
 write a length of data to the element
-*/
#ifdef PROTOTYPE
PRIVATE int32 HXIwrite(accrec_t *access_rec, int32 length, VOIDP data)
#else
PRIVATE int32 HXIwrite(access_rec, length, data)
    accrec_t *access_rec;      /* access record */
    int32 length;              /* length of data to write */
    VOIDP data;                        /* data buffer */
#endif
{
    char *FUNC="HXIwrite";     /* for HERROR */
    extinfo_t *info =          /* information on the special element */
       (extinfo_t*)(access_rec->special_info);

    /* validate length */

    if (length < 0) {
       HERROR(DFE_RANGE);
       return FAIL;
    }

    /* write the data onto file */

    if (HI_SEEK(info->file_external, 
		access_rec->posn + info->extern_offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    if (HI_WRITE(info->file_external, data, length) == FAIL) {

       /* this external file might not be opened with write permission,
          reopen the file and try again */

       hdf_file_t f = HI_OPEN(info->extern_file_name, DFACC_WRITE);
       if (OPENERR(f) || HI_SEEK(f, 
		   access_rec->posn + info->extern_offset) == FAIL ||
           HI_WRITE(f, data, length) == FAIL) {
           HERROR(DFE_DENIED);
           HI_CLOSE(f);
           return FAIL;
       }
       HI_CLOSE(info->file_external);

       /* if okay, substitute the file descriptor */

       info->file_external = f;
    }

    /* update access record, and information about special elelemt */

    access_rec->posn += length;
    if (access_rec->posn > info->length) {
       uint8 *p =      /* temp buffer ptr */
           tbuf;
       dd_t *info_dd =         /* dd of infromation element */
           &access_rec->block->ddlist[access_rec->idx];
       filerec_t *file_rec =   /* file record */
           FID2REC(access_rec->file_id);

       info->length = access_rec->posn;
       INT32ENCODE(p, info->length);
       if (HI_SEEK(file_rec->file, info_dd->offset+2) == FAIL) {
           HERROR(DFE_SEEKERROR);
           return FAIL;
       }
       if (HI_WRITE(file_rec->file, tbuf, 4) == FAIL) {
           HERROR(DFE_WRITEERROR);
           return FAIL;
       }
    }

    return length;
}


/* ------------------------------ HXIinquire ------------------------------ */
/*

 inquire information about the access record and data element

*/
#ifdef PROTOTYPE
PRIVATE int32 HXIinquire(accrec_t *access_rec, int32 *pfile_id, uint16 *ptag,
                        uint16 *pref, int32 *plength, int32 *poffset,
                        int32 *pposn, int16 *paccess, int16 *pspecial)
#else
PRIVATE int32 HXIinquire(access_rec, pfile_id, ptag, pref, plength, poffset,
                        pposn, paccess, pspecial)
     accrec_t *access_rec;     /* access record */
     int32 *pfile_id;          /* ptr to file id, OUT */
     uint16 *ptag;             /* ptr to tag of information, OUT */
     uint16 *pref;             /* ptr to ref of information, OUT */
     int32 *plength;           /* ptr to length of data element, OUT */
     int32 *poffset;           /* ptr to offset of data element, OUT */
     int32 *pposn;             /* ptr to position of access in element, OUT */
     int16 *paccess;           /* ptr to access mode, OUT */
     int16 *pspecial;          /* ptr to special code */
#endif
{
    dd_t *info_dd =            /* dd of special information */
       &(access_rec->block->ddlist[access_rec->idx]);
    extinfo_t *info =          /* special information record */
       (extinfo_t *)access_rec->special_info;

    /* fill in the variables if they are present */

    if (pfile_id) *pfile_id = access_rec->file_id;
    if (ptag) *ptag = info_dd->tag;
    if (pref) *pref = info_dd->ref;
    if (plength) *plength = info->length;
    if (poffset) *poffset = 0; /* meaningless */
    if (pposn) *pposn = access_rec->posn;
    if (paccess) *paccess = access_rec->access;
    if (pspecial) *pspecial = access_rec->special;

    return SUCCEED;
}

/* ----------------------------- HXIendaccess ----------------------------- */
/*

  Close the file pointed to by the current AID and free the AID

*/
#ifdef PROTOTYPE
PRIVATE int32 HXIendaccess(accrec_t *access_rec)
#else
PRIVATE int32 HXIendaccess(access_rec)
    accrec_t *access_rec;      /* access record to dispose of */
#endif
{
    char *FUNC="HXIendaccess"; /* for HERROR */
    filerec_t *file_rec =      /* file record */
       FID2REC(access_rec->file_id);

    /* close the file pointed to by this access rec */
    HXIcloseAID(access_rec);

    /* validate file record */

    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }

    /* detach from the file */

    file_rec->attach--;

    /* free the access record */

    access_rec->used = FALSE;

    return SUCCEED;
}

/* ----------------------------- HXIcloseAID ------------------------------ */
/*

  close the file currently being pointed to by this AID but do *NOT* 
  free the AID.
  
  This is called by Hnextread() which reuses an AID to point to
  the 'next' object as requested.  If the current object was an
  external object, the external file needs to be closed before all
  reference to it is lost.

*/
#ifdef PROTOTYPE
int32 HXIcloseAID(accrec_t *access_rec)
#else
int32 HXIcloseAID(access_rec)
accrec_t *access_rec;
#endif
{

    char *FUNC="HXIcloseAID"; /* for HERROR */
    extinfo_t *info =          /* special information record */
       (extinfo_t *)access_rec->special_info;

    /* detach the special information record.
       If no more references to that, free the record */

    if (--(info->attached) == 0) {
       HI_CLOSE(info->file_external);
       HDfreespace((VOIDP) info->extern_file_name);
       HDfreespace((VOIDP) info);
    }

    return SUCCEED;


} /* HXIcloseAID */

