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

/*+ hblocks .c
 routines to implement linked-block elements
+*/
/*
[[ (since we no longer have a way to generate special tags from
    normal tags, we no longer provide the conversion. LCC 7jun91)
  Linked element in HDF files created in two ways
  -- created from the start or
  -- converted from a normal data element
]]

  A linked-block element is a special element.
[[ (This part is not true any more, LCC 7jun91)
  Special elements are
  flagged with a set high-bit in their tag.  Thus, a tag t has
  BASETAG == t & 0x7f and is a special tag if t & 0x80 != 0x00
]]
  The first 16 bits of the meta-element that this tag/ref points to
  tells us what type of special element this is.  If these 16 bits is
  SPECIAL_LINKED, then it contains information about the linked blocks.
  After this 16 bits, 32 bit which is the length of each block, after
  which is the information header:

  -----------------------------------------------------------------------
  | # blocks in | tag/ref of | tag/ref of blocks list .......          |
  | this header | next header|                                         |
  -----------------------------------------------------------------------
*/

#include "hdf.h"
#include "hfile.h"
#include "herr.h"

/* block_t - record of a linked block. contains the tag and ref of the
   data elt that forms the linked block */

typedef struct block_t {
    uint16 ref;                        /* ref of the linked block */
} block_t;

/* link_t - a linked list block table.
   Very similar to the dd block structure */

typedef struct link_t {
    uint16 nextref;            /* ref of the next block table */
    struct link_t *next;       /* ptr to the next block table */
    struct block_t *block_list;     /* ptr to the block list for this table */
} link_t;

/* information on this special linked block data elt */

typedef struct linkinfo_t {
    int attached;               /* how many access records refer to this elt */
    int32 length;               /* the actual length of the data elt */
#ifndef oldspecial
    int32 first_length;         /* length of first block */
#endif
    int32 block_length;         /* the length of the remaining blocks */
    int32 number_blocks;        /* total number of blocks in each link/block
                                  table */
    uint16 link_ref;            /* ref of the first block table structure */
    link_t *link;               /* pointer to the first block table */
    link_t *last_link;          /* pointer to the last block table */
} linkinfo_t;

/* private functions */

PRIVATE int32 HLIstaccess PROTO((accrec_t *access_rec, int16 access));
PRIVATE int32 HLIstread PROTO((accrec_t *access_rec));
PRIVATE int32 HLIstwrite PROTO((accrec_t *access_rec));
PRIVATE link_t *HLIgetlink PROTO((int32 file_id, uint16 ref, \
                        int32 number_blocks));
PRIVATE int32 HLIseek PROTO((accrec_t *access_rec, int32 offset, int origin));
PRIVATE int32 HLIread PROTO((accrec_t *access_rec, int32 length, uint8 *data));
PRIVATE int32 HLIwrite PROTO((accrec_t *access_rec, int32 length, uint8 *data));
PRIVATE link_t *HLInewlink PROTO((int32 file_id, int32 number_blocks,
                          uint16 link_ref, uint16 first_block_ref));
PRIVATE int32 HLIinquire PROTO((accrec_t *access_rec, int32 *pfile_id, \
                uint16 *ptag, uint16 *pref, int32 *plength, int32 *poffset, \
                int32 *pposn, int16 *paccess, int16 *pspecial));
PRIVATE int32 HLIendaccess PROTO((accrec_t *access_rec));

/* the accessing function table for linked blocks */

int32 (*linked_funcs[])() = {
    HLIstread,
    HLIstwrite,
    HLIseek,
    HLIinquire,
    HLIread,
    HLIwrite,
    HLIendaccess,
};

/*- HLcreate
 creates a linked block special data element
 Took out all portions for converting a normal data elt to
 a linked block data elt.  LCC 11jun91
-*/
#ifdef PROTOTYPE
int32 HLcreate(int32 file_id, uint16 tag, uint16 ref, int32 block_length,
              int32 number_blocks)
#else
int32 HLcreate(file_id, tag, ref, block_length, number_blocks)
    int32 file_id;
    uint16 tag;
    uint16 ref;
    int32 block_length;
    int32 number_blocks;
#endif
{
    char *FUNC="HLcreate";     /* for HERROR */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */
    int slot;                  /* slot in access records */
    dd_t *dd;                  /* ptr to created dd */
    ddblock_t *data_block;     /* dd lbock of the data dd */
    int32 data_idx;            /* dd list index of data dd */
    linkinfo_t *info;          /* information for the linked blocks elt */
    uint16 link_ref;           /* the ref of the link structure
                                  (block table) */
#ifndef oldspecial
    dd_t *data_dd;             /* dd of existing regular element */
    uint16 special_tag;                /* special version of this tag */
#endif

    /* clear error stack and validate file record id */

    HEclear();
    file_rec = FID2REC(file_id);
    if (!file_rec ||
       file_rec->refcount == 0 ||
       block_length < 0 ||
       number_blocks < 0
#ifndef oldspecial
       || SPECIALTAG(tag)
       || (special_tag = MKSPECIALTAG(tag)) == DFTAG_NULL
#endif
       ) {
       HERROR(DFE_ARGS);
       return FAIL;
    }
    if (!(file_rec->access & DFACC_WRITE)) {
       HERROR(DFE_DENIED);
       return FAIL;
    }

    /* get empty slot in access records */

    slot = HIget_access_slot();
    if (slot == FAIL) {
       HERROR(DFE_TOOMANY);
       return FAIL;
    }
    access_rec = &access_records[slot];

    /* update maxref if necessary */

    if (ref > file_rec->maxref)
       file_rec->maxref = ref;

    /* search for identical dd */
    if (HIlookup_dd(file_rec, tag, ref, &data_block, &data_idx) != FAIL) {
#ifndef oldspecial
       data_dd = &(data_block->ddlist[data_idx]);
       if (SPECIALTAG(data_dd->tag)) {
           HERROR(DFE_CANTMOD);
           access_rec->used = FALSE;
           return FAIL;
       }
    } else {
       data_dd = (dd_t *) NULL;
#else
       HERROR(DFE_CANTMOD);
       access_rec->used = FALSE;
        return FAIL;
#endif
    }

    /* search for the empty dd to put new dd */

    if (HIlookup_dd(file_rec, DFTAG_NULL, DFREF_WILDCARD, &file_rec->null_block,
                 &file_rec->null_idx) == FAIL) {
       if (HInew_dd_block(file_rec, FILE_NDDS(file_rec), FUNC) == FAIL) {
           HERROR(DFE_NOFREEDD);
           access_rec->used = FALSE;
           return FAIL;
       } else {
           access_rec->block = file_rec->ddlast;
           access_rec->idx = 0;
       }
    } else {
      access_rec->block = file_rec->null_block;
      access_rec->idx   = file_rec->null_idx;
    }
    dd = &access_rec->block->ddlist[access_rec->idx];

#ifndef oldspecial
    if (data_dd) {

       /* remove old tag from hash table */
       if(HIdel_hash_dd(file_rec, data_dd->tag, data_dd->ref) == FAIL) 
         return FAIL;

       data_dd->tag = DFTAG_LINKED;
       if (HIupdate_dd(file_rec, data_block, data_idx, FUNC) == FAIL) {
           access_rec->used = FALSE;
           return FAIL;
       }

       /* update hash table */
       if(HIadd_hash_dd(file_rec, data_dd->tag, data_dd->ref, data_block, 
                        data_idx) == FAIL) 
         return FAIL;

    }
#endif

    /* write the special info structure to fill */

    link_ref = Hnewref(file_id);
    dd->length = 16;

    /* write new dd at the end of file */

    if (HI_SEEKEND(file_rec->file) == FAIL) {
       HERROR(DFE_SEEKERROR);
       access_rec->used = FALSE;
       return FAIL;
    }
    dd->offset = HI_TELL(file_rec->file);
    access_rec->special_info = (VOIDP) HDgetspace((uint32)sizeof(linkinfo_t));
    if (!access_rec->special_info) {
       HERROR(DFE_NOSPACE);
       access_rec->used = FALSE;
       return FAIL;
    }

    info = (linkinfo_t *) access_rec->special_info;
    info->attached = 1;
#ifndef oldspecial
    info->length = data_dd ? data_dd->length : 0;
    info->first_length = data_dd ? data_dd->length : block_length;
#else
    info->length = 0;
#endif
    info->block_length = block_length;
    info->number_blocks = number_blocks;

    {
       uint8 *p;
       p = tbuf;
       UINT16ENCODE(p, SPECIAL_LINKED);
       INT32ENCODE(p, info->length);
       INT32ENCODE(p, block_length);
       INT32ENCODE(p, number_blocks);
       UINT16ENCODE(p, link_ref); /* link_ref */
    }
    if (HI_WRITE(file_rec->file, tbuf, dd->length) == FAIL) {
       HERROR(DFE_WRITEERROR);
       access_rec->used = FALSE;
       return FAIL;
    }
    dd->tag = special_tag;
    dd->ref = ref;

    /* allocate info structure and file it in */
    info->link = HLInewlink(file_id, number_blocks, link_ref,
#ifndef oldspecial
                           (uint16)(data_dd ? data_dd->ref : 0)
#else
                           (uint16)0
#endif
                           );
    if (!info->link) {
       access_rec->used = FALSE;
       return FAIL;
    }
    info->link_ref = link_ref;

    /* update dd list in file */
    if (HIupdate_dd(file_rec, access_rec->block, access_rec->idx, FUNC) ==
       FAIL) {
       access_rec->used = FALSE;
        return FAIL;
    }

    /* update hash table */
    if(HIadd_hash_dd(file_rec, dd->tag, dd->ref, access_rec->block, 
                     access_rec->idx) == FAIL) 
      return FAIL;

    /* update access record and file record */

    access_rec->special_func = linked_funcs;
    access_rec->special = SPECIAL_LINKED;
    access_rec->posn = 0;
    access_rec->access = DFACC_WRITE;
    access_rec->file_id = file_id;
    file_rec->attach++;

    return ASLOT2ID(slot);
}


/*---------------------------------------------------------------------------
 HDinqblockinfo - Given an aid, return special info for linked-block
		  element in space provided.  This function works like
		  HDinquire().
---------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int HDinqblockinfo(int32 aid, int32 *length, int32 *first_length,
                int32 *block_length, int32 *number_blocks)
#else
int HDinqblockinfo(aid, length, first_length, block_length, number_blocks)
int32 aid;
int32 *length, *first_length, *block_length, *number_blocks;
#endif /* PROTOTYPE */
{
    accrec_t *arec;
    char *FUNC="HDinqblockinfo";


    HEclear();
    if ((arec = AID2REC(aid)) == (accrec_t *)NULL) {
        HERROR(DFE_BADAID);
        return (FAIL);
    }

    if (arec->special != SPECIAL_LINKED) {
        HERROR(DFE_ARGS);
        return (FAIL);
    }

    if(length) *length = ((linkinfo_t *)(arec->special_info))->length;
    if(first_length)
        *first_length = ((linkinfo_t *)(arec->special_info))->first_length;
    if(block_length)
        *block_length = ((linkinfo_t *)(arec->special_info))->block_length;
    if(number_blocks)
        *number_blocks = ((linkinfo_t *)(arec->special_info))->number_blocks;

    return(SUCCEED);
}



/*- HLIstaccess
 start access to the special linked block data elt
-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIstaccess(accrec_t *access_rec, int16 access)
#else
PRIVATE int32 HLIstaccess(access_rec, access)
    accrec_t *access_rec;      /* access record */
    int16 access;               /* access mode */
#endif
{
    char *FUNC="HLIstaccess";  /* for HERROR */
    filerec_t *file_rec;       /* file record */
    linkinfo_t *info;          /* information about data elt */
    dd_t *dd;                  /* dd for the special elt */

    /* validate file record id */

    file_rec = FID2REC(access_rec->file_id);
    if (!file_rec || file_rec->refcount == 0 || !(file_rec->access & access)) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* set up some data in access record */

    access_rec->special = SPECIAL_LINKED;
    access_rec->posn = 0;
    access_rec->access = access;
    dd = &access_rec->block->ddlist[access_rec->idx];

    /* if the special information are already in some other acc elt,
       copy it */

    access_rec->special_info = HIgetspinfo(access_rec, dd->tag, dd->ref);
    if (access_rec->special_info) {
       ((linkinfo_t *)access_rec->special_info)->attached++;
        file_rec->attach++;
       return ASLOT2ID(access_rec-access_records);
    }

    /* read in the information from file */

    if (HI_SEEK(file_rec->file, dd->offset+2) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    if (HI_READ(file_rec->file, tbuf, 14) == FAIL) {
       HERROR(DFE_READERROR);
       return FAIL;
    }
    access_rec->special_info = (VOIDP) HDgetspace((uint32)sizeof(linkinfo_t));
    info = (linkinfo_t *) access_rec->special_info;
    if (!info) {
       HERROR(DFE_NOSPACE);
       return FAIL;
    }
    {
    uint8 *p = tbuf;
    INT32DECODE(p, info->length);
    INT32DECODE(p, info->block_length);
    INT32DECODE(p, info->number_blocks);
    UINT16DECODE(p, info->link_ref);
    }

    /* set up the block tables of the information */

    info->link = HLIgetlink(access_rec->file_id,
                           info->link_ref, info->number_blocks);
    if (!info->link) {
       HDfreespace((VOIDP) info);
       return FAIL;
    }
#ifndef oldspecial
    if (info->link->block_list[0].ref) {
       info->first_length = Hlength(access_rec->file_id,
                                    DFTAG_LINKED,
                                    info->link->block_list[0].ref);
       if (info->first_length == FAIL) {
           HERROR(DFE_INTERNAL);
           HDfreespace((VOIDP) info->link);
           HDfreespace((VOIDP) info);
           return FAIL;
       }
    } else {
       info->first_length = info->block_length;
    }
#endif

    info->last_link = info->link;
    while (info->last_link->nextref != 0) {
       info->last_link->next = HLIgetlink(access_rec->file_id,
                                          info->last_link->nextref,
                                          info->number_blocks);
       if (!info->last_link->next) {
            link_t *l, *next;
           for (l=info->link; l; l=next) {
               next = l->next;
               if (l->block_list) HDfreespace((VOIDP) l->block_list);
               HDfreespace((VOIDP)l);
           }
           HDfreespace((VOIDP) info);
           return FAIL;
       }
       info->last_link = info->last_link->next;
    }
    /* update data */

    info->attached = 1;
    file_rec->attach++;

    return ASLOT2ID(access_rec - access_records);
}

/*- HLIstread
 start read on special linked block data element
 just calls HLIstaccess
-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIstread(accrec_t *access_rec)
#else
PRIVATE int32 HLIstread(access_rec)
    accrec_t *access_rec;      /* access record */
#endif
{
    return HLIstaccess(access_rec, DFACC_READ);
}

/*- HLKIstwrite
 start write on a special linked block data element
 calls HLIstaccess
-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIstwrite(accrec_t *access_rec)
#else
PRIVATE int32 HLIstwrite(access_rec)
    accrec_t *access_rec;
#endif
{
    return HLIstaccess(access_rec, DFACC_WRITE);
}

/*- HLIgetlink
 get link information
-*/
#ifdef PROTOTYPE
PRIVATE link_t *HLIgetlink(int32 file_id, uint16 ref, int32 number_blocks)
#else
PRIVATE link_t * HLIgetlink(file_id, ref, number_blocks)
    int32 file_id;             /* file record id */
    uint16 ref;                        /* tag ref of block table elt */
    int32 number_blocks;       /* number of blocks in this link elt */
#endif
{
    char *FUNC="HLIgetlink";   /* for HERROR */
    int32 access_id;           /* access record id */
    uint8 *buffer;
    uint16 tag = DFTAG_LINKED;

    /* allocate necessary memory for block table */

    link_t *link = (link_t *) HDgetspace((uint32)sizeof(link_t));
    if (!link) {
       HERROR(DFE_NOSPACE);
       return (link_t *) NULL;
    }
    link->block_list = (block_t*) HDgetspace((uint32)number_blocks
                                        * sizeof(block_t));
    if (!link) {
       HDfreespace((VOIDP) link);
       HERROR(DFE_NOSPACE);
       return (link_t *)NULL;
    }
    link->next = (link_t *) NULL;

    /* read block table into buffer */

    buffer = (uint8 *) HDgetspace((uint32)(2 + 2*number_blocks));
    if (!buffer) {
       HERROR(DFE_NOSPACE);
       return (link_t *) NULL;
    }
    access_id = Hstartread(file_id, tag, ref);
    if (access_id == FAIL ||
       Hread(access_id, 2+2*number_blocks, buffer) == FAIL) {
       HDfreespace((VOIDP)buffer);
       HERROR(DFE_READERROR);
       return (link_t *) NULL;
    }

{
    register int32 i;
    uint8 *p = buffer;

    UINT16DECODE(p, link->nextref);
    for (i=0; i<number_blocks; i++)
       UINT16DECODE(p, link->block_list[i].ref);
}
    Hendaccess(access_id);
    HDfreespace((VOIDP)buffer);

    return link;
}

/*- HLIseek
 set position of next access into data elt
-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIseek(accrec_t *access_rec, int32 offset, int origin)
#else
PRIVATE int32 HLIseek(access_rec, offset, origin)
    accrec_t *access_rec;
    int32 offset;
    int origin;
#endif
{
    char *FUNC="HLIseek";      /* for HERROR */

    /* validate access record */

    if (access_rec->special != SPECIAL_LINKED) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }

    /* adjust the offset according to origin and validate */

    /* there is no upper bound to posn */

    if (origin == DF_CURRENT) offset += access_rec->posn;
    if (origin == DF_END)
       offset += ((linkinfo_t *)(access_rec->special_info))->length;
    if (offset < 0) {
       HERROR(DFE_RANGE);
       return FAIL;
    }

    /* set position */
    access_rec->posn = offset;

    return SUCCEED;
}

/*- HLIread
 read data from elt into data buffer
-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIread(accrec_t *access_rec, int32 length, uint8 *data)
#else
PRIVATE int32 HLIread(access_rec, length, data)
    accrec_t *access_rec;      /* access record */
    int32 length;              /* length of data to read */
    uint8 *data;               /* buffer to read data into */
#endif
{
    char *FUNC="HLIread";      /* for HERROR */

    /* information record for this special data elt */

    linkinfo_t *info = (linkinfo_t*)(access_rec->special_info);

    /* block table record */

    link_t *link = info->link;

    /* relative position in linked block of data elt */

    int32 relative_posn = access_rec->posn;

    int32 block_idx;             /* block table index of current block */
#ifndef oldspecial
    int32 current_length;      /* length of current block */
#endif
    int32 nbytes = 0;           /* # bytes read on any single Hread() */
    int32 bytes_read = 0;       /* total # bytes read for this call of HLIread*/

    /* validate length */

    if (length == 0) length = info->length - access_rec->posn;
    else
/*      if (length < 0 || access_rec->posn + length > info->length) {*/
        if (length < 0) {
           HERROR(DFE_RANGE);
           return FAIL;
        }
    if (access_rec->posn + length > info->length)
        length = info->length - access_rec->posn;

    /* search for linked block to start reading from */

#ifndef oldspecial
    if (relative_posn < info->first_length) {
       block_idx = 0;
       current_length = info->first_length;
    } else {
       relative_posn -= info->first_length;
       block_idx = relative_posn / info->block_length + 1;
       relative_posn %= info->block_length;
       current_length = info->block_length;
    }
#else
    block_idx = relative_posn / info->block_length + 1;
    relative_posn %= info->block_length;
#endif

    {
        register int32 i;
       for (i=0; i<block_idx/info->number_blocks; i++) {
           if (!link) {
               HERROR(DFE_INTERNAL);
               return FAIL;
           }
           link = link->next;
       }
    }
    block_idx %= info->number_blocks;

    /* found the starting block, now read in the data */

    do {
       register int32 remaining = /* remaining data in current block */
#ifndef oldspecial
           current_length
#else
           info->block_length
#endif
               - relative_posn;

       /* read in the data in this block */

       if (remaining > length) remaining = length;
       if (link->block_list[block_idx].ref != 0) {
           int32 access_id;    /* access record id for this block */
           block_t *current_block = /* record on the current block */
               &(link->block_list[block_idx]);

           access_id = Hstartread(access_rec->file_id,
                                  DFTAG_LINKED,
                                  current_block->ref);
           if (access_id == (int32) FAIL ||
                   (relative_posn &&
                   (int32)FAIL == Hseek(access_id, relative_posn, DF_START)) ||
                   (int32)FAIL == (nbytes = Hread(access_id, remaining, data))) {
               HERROR(DFE_READERROR);
               return FAIL;
           }
           bytes_read += nbytes;
           Hendaccess(access_id);

       } else {

           /*if block is missing, fill this part of buffer with zero's */

            register int32 i;
           for (i=0; i<remaining; i++)
               data[i] = '\0';
            bytes_read += nbytes;
       }

       /* move variables for the next block */

       data +=remaining;
       length -= remaining;
       if (length > 0 && ++block_idx >= info->number_blocks) {
           block_idx = 0;
           link = link->next;
           if (!link) {
               HERROR(DFE_INTERNAL);
               return FAIL;
           }
       }
       relative_posn = 0;
#ifndef oldspecial
       current_length = info->block_length;
#endif
    } while (length > 0);      /* if still somemore to read in, repeat */

    access_rec->posn += bytes_read;
    return bytes_read;
}

/*- HLIwrite
 write out data into linked block data elt
-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIwrite(accrec_t *access_rec, int32 length, uint8 *data)
#else
PRIVATE int32 HLIwrite(access_rec, length, data)
    accrec_t *access_rec;      /* access record */
    int32 length;              /* length of data */
    uint8 *data;                        /* data buffer to write from */
#endif
{
    char *FUNC="HLIwrite";     /* for HERROR */
    filerec_t *file_rec =      /* file record */
       FID2REC(access_rec->file_id);
    dd_t *info_dd =                    /* dd of the special info record */
        &(access_rec->block->ddlist[access_rec->idx]);
    linkinfo_t *info =         /* linked blocks information record */
       (linkinfo_t*)(access_rec->special_info);
    link_t *link =             /* ptr to link block table */
       info->link;
    int32 relative_posn =      /* relative position in linked block */
       access_rec->posn;
    int32 block_idx;           /* block table index of current block */
    link_t *prev_link =        /* ptr to block table before
                                  current block table.
                                  for groking the offset of
                                  current block table */
       NULL;
#ifndef oldspecial
    int32 current_length;      /* length of current block */
#endif
    int32 nbytes = 0;           /* #bytes written by any single Hwrite */
    int32 bytes_written = 0;    /* total #bytes written by HLIwrite */

    /* validate length and file records */

    if (length < 0) {
       HERROR(DFE_RANGE);
       return FAIL;
    }
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }

    /* determine linked block and position to start writing into */

    /* determine where to start.  Setup missing block tables
       along the way. */

#ifndef oldspecial
    if (relative_posn < info->first_length) {
       block_idx = 0;
       current_length = info->first_length;
    } else {
       relative_posn -= info->first_length;
       block_idx = relative_posn / info->block_length + 1;
       relative_posn %= info->block_length;
       current_length = info->block_length;
    }
#else
    block_idx = relative_posn / info->block_length + 1;
    relative_posn %= info->block_length;
#endif
    {
       /* follow the links of block tables and create missing
          block tabels along the way */

        register int32 num_links;        /* number of links to follow */

       for (num_links=block_idx/info->number_blocks;
            num_links>0;
            num_links--) {
           if (!link->next) {

               /* create missing link (block table) */

                link->nextref = Hnewref(access_rec->file_id);
               link->next =
                   HLInewlink(access_rec->file_id, info->number_blocks,
                              link->nextref, 0);
               if (!link->next)
                   return FAIL;
               {               /* AA */
                   /* update previous link with information about new link */

                   uint16 link_tag = DFTAG_LINKED;
                   uint16 link_ref = /* ref of current link */
                            (uint16)(prev_link!=NULL ?
                                prev_link->nextref : info->link_ref);

                   uint8 *p = tbuf;   /* temp buf ptr */

                   /* write file the updated portion of current link */

                   int32 link_id = /* access id for current link */
                       Hstartwrite(access_rec->file_id, link_tag,
                                   link_ref, 0);
                   if (link_id == FAIL) {
                       HERROR(DFE_WRITEERROR);
                       return FAIL;
                   }
                   UINT16ENCODE(p, link->nextref);
                   if (Hwrite(link_id, 2, tbuf) == FAIL) {
                       HERROR(DFE_WRITEERROR);
                       return FAIL;
                   }
                   Hendaccess(link_id);
               }               /* AA */
           }                   /* if not link->next */

           /* move to the next link */

            prev_link = link;
           link = link->next;
       }                       /* for */
    }                          /*  */

    block_idx %= info->number_blocks;

    /* start writing in that block */

    do {
       int32 access_id;        /* access record id */
       int32 remaining =       /* remaining data length in this block */
#ifndef oldspecial
           current_length
#else
           info->block_length
#endif
               - relative_posn;
       uint16 new_ref =        /* ref of newly created block */
           0;

       /* determine length and write this block */

       if (remaining > length) remaining = length;
       if (link->block_list[block_idx].ref != 0) {

           /* this block already exist, so just set up access to it */

           block_t *current_block = /* ptr to current block record */
               &(link->block_list[block_idx]);

           access_id= Hstartwrite(access_rec->file_id,
                                  DFTAG_LINKED,
                                  current_block->ref,
#ifndef oldspecial
                                  current_length
#else
                                  info->block_length
#endif
                                  );

       } else {

           /* block is missing, set up a new block */

           new_ref = Hnewref(access_rec->file_id);
           access_id = Hstartwrite(access_rec->file_id, DFTAG_LINKED,
                                   new_ref,
#ifndef oldspecial
                                   current_length
#else
                                   info->block_length
#endif
                                   );

       }

       if (access_id == (int32) FAIL) {
           HERROR(DFE_WRITEERROR);
           return FAIL;
       }
       if ((relative_posn &&
           (int32)FAIL == Hseek(access_id, relative_posn, DF_START)) ||
           (int32)FAIL == (nbytes = Hwrite(access_id, remaining, data))) {
           HERROR(DFE_WRITEERROR);
           return FAIL;
       }
       Hendaccess(access_id);
       bytes_written += nbytes;

       if (new_ref) {

           /* created a new block, so update the link/block table */

           uint16 link_tag = DFTAG_LINKED;
           uint16 link_ref =   /* ref of the current link/block table */
               (uint16)(prev_link ? prev_link->nextref : info->link_ref);

           uint8 *p =  /* temp buffer ptr */
               tbuf;

           int32 link_id =     /* access record id of the current
                                  link/block table */
               Hstartwrite(access_rec->file_id, link_tag, link_ref, 0);

           if (link_id == FAIL) {
               HERROR(DFE_WRITEERROR);
               return FAIL;
           }
           UINT16ENCODE(p, new_ref);
           if (Hseek(link_id, 2+2*block_idx, DF_START) == FAIL) {
               HERROR(DFE_SEEKERROR);
               return FAIL;
           }
           if (Hwrite(link_id, 2, tbuf) == FAIL) {
               HERROR(DFE_WRITEERROR);
               return FAIL;
           }
           Hendaccess(link_id);

           /* update memory structure */

           link->block_list[block_idx].ref = new_ref;
       }                       /* if new_ref */

       /* move ptrs and counters for next phase */

       data +=remaining;
       length -= remaining;

       if (length>0 &&
           ++block_idx >= info->number_blocks) {

           /* move to the next link/block table */

           block_idx = 0;

           if (!link->next) {

               /* create missing link/block table */

                link->nextref = Hnewref(access_rec->file_id);
               link->next = HLInewlink(access_rec->file_id,
                                       info->number_blocks,
                                        link->nextref, 0);
               if (!link->next)
                   return FAIL;

               {               /* BB */
                   uint16 link_tag = DFTAG_LINKED;
                   uint16 link_ref = /* ref of current link/block table */
                       (uint16)(prev_link ? prev_link->nextref : info->link_ref);

                   uint8 *p = /* temp buffer ptr */
                       tbuf;

                   int32 link_id = /* access record id of
                                      current link/block table */
                       Hstartwrite(access_rec->file_id, link_tag,
                                   link_ref, 0);

                   if (link_id == FAIL) {
                       HERROR(DFE_WRITEERROR);
                       return FAIL;
                   }
                   UINT16ENCODE(p, link->nextref);
                   if (Hwrite(link_id, 2, tbuf) == FAIL) {
                       HERROR(DFE_WRITEERROR);
                       return FAIL;
                   }
                   Hendaccess(link_id);
               }               /* BB */
           }                   /* if not link->next  */

           /* move to the next link/block table */

            prev_link = link;
           link = link->next;

       }

       /* update vars for next phase */

       relative_posn = 0;
#ifndef oldspecial
       current_length = info->block_length;
#endif

    } while (length > 0);

    if (HI_SEEK(file_rec->file, info_dd->offset+2) == FAIL) {
        HERROR(DFE_SEEKERROR);
       return FAIL;
    }
        {
            int32 tmp;

            tmp = bytes_written + access_rec->posn;
            if (tmp > info->length)
                info->length = tmp;
        }
    {
        uint8 *p = tbuf;
       INT32ENCODE(p, info->length);
    }
    if (HI_WRITE(file_rec->file, tbuf, 4) == FAIL) {
        HERROR(DFE_WRITEERROR);
       return(FAIL);
    }

    access_rec->posn += bytes_written;
    /* return SUCCEED; */
    /* if wrong # bytes written, FAIL has already been returned */
    return bytes_written;
}

/*- HLInewlink
 create a new link/block table in memory and in file
 returns ptr to the new link/block table
-*/
#ifdef PROTOTYPE
PRIVATE link_t *HLInewlink(int32 file_id, int32 number_blocks,
                          uint16 link_ref, uint16 first_block_ref)
#else
PRIVATE link_t *HLInewlink(file_id, number_blocks, link_ref, first_block_ref)
    int32 file_id;              /* file record id */
    int32 number_blocks;        /* number os block to put in this link */
    uint16 link_ref;            /* ref of link to create */
    uint16 first_block_ref;     /* ref of first block in this link */
#endif
{
    char *FUNC="HLInewlink";   /* for HERROR */
    int32 link_id;             /* access record id of new link */
    uint8 *buf;                 /* temp buffer */

    /* set up new link record in memory */

    link_t *link =             /* new link record */
       (link_t *)HDgetspace((uint32)sizeof(link_t));

    if (!link) {
       HERROR(DFE_NOSPACE);
       return NULL;
    }
    link->block_list = (block_t *) HDgetspace((uint32)number_blocks
                                         * sizeof(block_t));
    if (!link->block_list) {
       HDfreespace((VOIDP) link);
       HERROR(DFE_NOSPACE);
       return NULL;
    }
    link->next = NULL;

    /* write the new link to file */

    link_id = Hstartwrite(file_id, DFTAG_LINKED, link_ref, 2+2*number_blocks);
    if (link_id==FAIL) {
       HDfreespace((VOIDP) link->block_list);
       HDfreespace((VOIDP) link);
        HERROR(DFE_WRITEERROR);
        return NULL;
    }
    {                          /* CC */
       register int32 i;       /* temp int index */
       uint8 *p;       /* temp buffer ptr */

       p = buf = (uint8 *)HDgetspace((uint32) (2 + 2*number_blocks));
       if (!buf) {
           HERROR(DFE_NOSPACE);
           return NULL;
       }

       /* set up the record and write to file */

       link->nextref = 0;
       UINT16ENCODE(p, 0);
       link->block_list[0].ref = first_block_ref;
       UINT16ENCODE(p, first_block_ref);

       for (i=1; i<number_blocks; i++) {

           /* set up each block in this link */

           link->block_list[i].ref = 0;
           UINT16ENCODE(p, 0);
       }
    }                          /* CC */

    /* write the link */

    if (Hwrite(link_id, 2+2*number_blocks, buf) == FAIL) {
        HDfreespace((VOIDP)buf);
       HDfreespace((VOIDP) link->block_list);
       HDfreespace((VOIDP) link);
        HERROR(DFE_WRITEERROR);
        return NULL;
    }
    HDfreespace((VOIDP)buf);
    Hendaccess(link_id);

    return link;
}

/*- HLIinquire

-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIinquire(accrec_t *access_rec, int32 *pfile_id, uint16 *ptag,
                uint16 *pref, int32 *plength, int32 *poffset, int32 *pposn,
                int16 *paccess, int16 *pspecial)
#else
PRIVATE int32 HLIinquire(access_rec, pfile_id, ptag, pref, plength, poffset,
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
    linkinfo_t *info =         /* special information record */
       (linkinfo_t *)access_rec->special_info;

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

/*- HLIendaccess

-*/
#ifdef PROTOTYPE
PRIVATE int32 HLIendaccess(accrec_t *access_rec)
#else
PRIVATE int32 HLIendaccess(access_rec)
    accrec_t *access_rec;      /* access record to dispose of */
#endif
{
    char *FUNC="HLIendaccess"; /* for HERROR */
    linkinfo_t *info =         /* special information record */
       (linkinfo_t *)access_rec->special_info;
    filerec_t *file_rec =      /* file record */
       FID2REC(access_rec->file_id);

    /* detach the special information record.
       If no more references to that, free the record */

    if (--(info->attached) == 0) {
       link_t *link;           /* current link to free */
       link_t *next;           /* next link to free */

       /* free the linked list of links/block tables */

       for (link = info->link; link; link = next) {
           next = link->next;
           HDfreespace((VOIDP) link->block_list);
           HDfreespace((VOIDP) link);
       }

       HDfreespace((VOIDP) info);
    }

    /* detach from the file */

    file_rec->attach--;

    /* free the access record */

    access_rec->used = FALSE;

    return SUCCEED;
}
