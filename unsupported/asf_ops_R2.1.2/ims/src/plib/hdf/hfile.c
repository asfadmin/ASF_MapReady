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

/*+
 FILE
       hfile.c
       HDF low level file I/O routines
 EXPORTED ROUTINES
       Hopen -- open or create a HDF file
       Hclose -- close HDF file
       Hstartread -- locate and position a read access elt on a tag/ref
       Hnextread -- locate and position a read access elt on next tag/ref.
       Hinquire -- inquire stats of an access elt
       Hstartwrite -- set up a WRITE access elt for a write
       Happendable -- attempt make a dataset appendable
       Hseek -- position an access element to an offset in data element
       Hread -- read the next segment from data element
       Hwrite -- write next data segment to data element
       Hendaccess -- to dispose of an access element
       Hgetelement -- read in a data element
       Hlength -- returns length of a data element
       Htrunc -- truncate a dataset to a length
       Hoffset -- get offset of data element in the file
       Hputelement -- writes a data element
       Hdupdd -- duplicate a data descriptor
       Hdeldd -- delete a data descriptor
       Hnewref -- returns a ref that is guaranteed to be unique in the file
       Hishdf -- tells if a file is an HDF file
       Hsync -- sync file with memory
       Hnumber -- count number of occurrances of tag/ref in file
       Hgetlibversion -- return version info on current HDF library
       Hgetfileversion -- return version info on HDF file
 AUTHOR
       Chin_Chau Low
 MODIFICATION HISTORY
	12/12/91 Doug Ilg  Changed implementation of version tags.  Added
			   Hgetlibversion() and Hgetfileversion() (public) and
			   HIread_version() and HIupdate_version() (PRIVATE).
+*/
/*
 SOME EXAMPLES
       Example 1.  To create a HDF file and write a data element into it in
       one go:

       int fileid;
       int tag=100, ref;
       char data[DATA_SIZE];

       ..create a file with clobber, use default ndds..
       fileid = Hopen("myfile.hdf", DF_CREATE, 0);
       ..get a new ref..
       ref = Hnewref(fileid);
       Hputelement(fileid, tag, ref, data, DATA_SIZE);

       Hclose(fileid);

       Example 2.  To read a data element in one go:

       int fileid;
       int tag=100, ref=3;
       long length;
       char data[DATA_SIZE];

       ..open the file with read only..
       fileid = Hopen("myfile.hdf", DFACC_READ, 0);
       ..check length of data element..
       length = Hlength(fileid, tag, ref);
       if (length > DATA_SIZE) routine_with_larger_buffer();
       else
           Hgetelement(fileid, tag, ref, data);

       Example 3.  To write 2 data elements piece by piece

       int fileid;
       int acc1, acc2;
       int tag1=100, tag2=200;
       int ref1=1, ref2=3;
       char buf1[1000], buf2[500];

       fileid = Hopen("myfile.hdf", DF_CREATE, 0)
       acc1 = Hstartwrite(fileid, tag1, ref1, ELT1_SIZE);
       acc2 = Hstartwrite(fileid, tag2, ref2, ELT2_SIZE);

       Hwrite(acc1, 1000L, buf1);
       Hwrite(acc2, 500L, buf2);
       ..fill buffers again..
       Hwrite(acc1, 1000L, buf1);
       ..etc..
       Hendaccess(acc1);
       Hendaccess(acc2);

       Example 4.  To read 2 data element piece by piece

       int fileid;
       int acc1, acc2;
       int tag1=100, tag2=200;
       int ref1=1, ref2=3;
       char buf1[1000], buf2[500];

       fileid = Hopen("myfile.hdf", DFACC_READ, 0);
       acc1 = Hstartread(fileid, tag1, ref1);
       acc2 = Hstartread(fileid, tag2, ref2);

       Hread(acc1, 1000L, buf1);
       Hread(acc2, 500L, buf2);
       ..do something with data read..
       Hread(acc1, 1000L, buf1);
       ..etc..
       Hendaccess(acc1);
       Hendaccess(acc2);
*/

#define HMASTER
#include "hdf.h"
#undef HMASTER
#include "herr.h"
#include "hfile.h"

/*
** Prototypes for local functions
*/
static int HIlock
  PROTO((int32 file_id));

static int HIunlock
  PROTO((int32 file_id));

static int HIchangedd
  PROTO((dd_t *datadd, ddblock_t *block, int idx, int16 special,
	 VOIDP special_info, int32 (**special_func)()));

/* Array of file records that contains all relevant
   information on an opened HDF file.
   See hfile.h for structure and members definition of filerec_t. */

#if defined(macintosh) | defined(THINK_C)
struct filerec_t *file_records = NULL;
#else /* !macintosh */
struct filerec_t file_records[MAX_FILE];
#endif /* !macintosh */

/* Array of records of information on each access elements.
   These will contain information like how to access the data element,
   where in the data element the current access should start from, etc.
   Allocated dynamically.
   See hfile.h for definition. */

struct accrec_t *access_records = NULL;

/* Temporary memory space for doing some general stuff so we don't
   have to allocate and deallocate memory all the time.  This space should
   be "sufficiently" large, or at least 64 bytes long.  Routines using
   tbuf should not assume that the buffer is longer than that. */

int32 int_tbuf[TBUF_SZ];
uint8 *tbuf = (uint8 *)int_tbuf;

/* Function tables declarations.  These function tables contain pointers
   to functions that help access each type of special element. */

/* Functions for accessing the linked block special
   data element.  For definition of the linked block, see hblocks.c. */

extern int32 (*linked_funcs[])();

/* Functions for accessing external data elements, or data
   elements that are in some other files.  For definition of the external
   data element, see hext.c. */

extern int32 (*ext_funcs[])();

/* Table of these function tables for accessing special elements.  The first
   member of each record is the speical code for that type of data element. */

functab_t functab[] = {
    {SPECIAL_LINKED, linked_funcs},
    {SPECIAL_EXT, ext_funcs},
    {0, NULL}                  /* terminating record; add new record */
                               /* before this line */
};

/*
** Declaration of private functions. 
*/
PRIVATE int HIget_file_slot
  PROTO((char *path, char *FUNC));

PRIVATE bool HIvalid_magic
  PROTO((hdf_file_t file, char *FUNC));

PRIVATE int HIfill_file_rec
  PROTO((filerec_t *file_rec, char *FUNC));

PRIVATE int HIinit_file_dds
  PROTO((filerec_t *file_rec, int16 ndds, char *FUNC));

PRIVATE int32 (**HIget_function_table PROTO((accrec_t *access_rec, char *FUNC)))();

PRIVATE int HIupdate_version
  PROTO((int32));

PRIVATE int HIread_version
  PROTO((int32));

/*--------------------------------------------------------------------------
 NAME
       Hopen -- Opens a HDF file.
 USAGE
       int32 Hopen(path, access, ndds)
       char *path;             IN: Name of file to be opened.
       int access;             IN: DFACC_READ, DFACC_WRITE, DFACC_CREATE
                                   or any bitwise-or of the above.
       int16 ndds;             IN: Number of dds in a block if this
                                   file needs to be created.
 RETURNS
       On success returns file id, on failure returns -1.
 DESCRIPTION
       Opens a HDF file.  Returns the the file ID on success, or -1
       on failure.

       Access equals DFACC_CREATE means discard existing file and
       create new file.  If access is a bitwise-or of DFACC_CREATE
       and anything else, the file is only created is it does not
       exist.  DFACC_WRITE set in access also means that if the file
       does not exist, it is created.  DFACC_READ is assumed to be
       implied even if it is not set.  DFACC_CREATE implies
       DFACC_WRITE.

       If the file is already opened and access is DFACC_CREATE:
       error DFE_ALROPEN.
       If the file is already opened, the requested access contains
       DFACC_WRITE, and previous open does not allow write: attempt
       to reopen the file with write permission.

       On successful exit,
       * file_rec members are filled in correctly.
       * file is opened with the relevant permission.
       * information about dd's are set up in memory.
       For new file, in addition,
       * the file headers and initial information are set up properly.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hopen(char *path, intn access, int16 ndds)
#else
int32 Hopen(path, access, ndds)
    char *path;                        /* Path of file to open */
    intn access;                       /* Access mode */
    int16 ndds;                        /* Number of dd's in each ddblock
                                  if file is created */
#endif
{
    char *FUNC="Hopen";                /* For HERROR */
    int slot;                  /* File record slot */
    filerec_t *file_rec;       /* File record */
    int vtag = 0;		/* write version tag? */

    /* Clear errors and check args and all the boring stuff. */

    HEclear();
    if (!path || ((access & DFACC_ALL) != access)) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* Get a space to put the file information.
       HIget_file_slot() also copies path into the record. */

    slot = HIget_file_slot(path, FUNC);
    if (slot == FAIL) {
        /* The slots are full. */
       HERROR(DFE_TOOMANY);
       return FAIL;
    }
    file_rec = &(file_records[slot]);

    if (file_rec->refcount) {
       /* File is already opened, check that permission is okay. */

        /* If this request is to create a new file and file is still
          in use, return error. */
       if (access == DFACC_CREATE) {
           HERROR(DFE_ALROPEN);
           return FAIL;
       }

       if ((access & DFACC_WRITE) && !(file_rec->access & DFACC_WRITE)) {
           /* If the request includes writing, and if original open does not
              provide for write, then try to reopen file for writing.
              This cannot be done on OS (such as the SXOS) where only one
              open is allowed per file at any time. */
#ifndef NO_MULTI_OPEN
           hdf_file_t f;

           f = HI_OPEN(file_rec->path, access);
           if (OPENERR(f)) {
               HERROR(DFE_DENIED);
               return FAIL;
           }

           /* Replace file_rec->file with new file pointer and
              close old one. */

           if (HI_CLOSE(file_rec->file) == FAIL) {
               HI_CLOSE(f);
               HERROR(DFE_CANTCLOSE);
               return FAIL;
           }
           file_rec->file = f;
#else /* NO_MULTI_OPEN */
           HERROR(DFE_DENIED);
           return FAIL;
#endif /* NO_MULTI_OPEN */
       }

       /* There is now one more open to this file. */

       file_rec->refcount++;

    } else {

        /* Flag to see if file is new and needs to be set up. */
       bool new_file=FALSE;

       /* Open the file, fill in the blanks and all the good stuff. */
       if (access != DFACC_CREATE) {
           /* try to open existing file */

           file_rec->file = HI_OPEN(file_rec->path, access);
           if (OPENERR(file_rec->file)) {
               if (access & DFACC_WRITE) {
                   /* Seems like the file is not there, try to create it. */
                   new_file = TRUE;
               } else {
                   HERROR(DFE_BADOPEN);
                   return FAIL;
               }
           } else {
               /* Open existing file successfully. */

               file_rec->access = access | DFACC_READ;

               /* Check to see if file is a HDF file. */

               if (!HIvalid_magic(file_rec->file, FUNC)) {
                   HERROR(DFE_NOTDFFILE);
                   HI_CLOSE(file_rec->file);
                   return FAIL;
               }

               /* Read in all the relevant data descriptor records. */

               if (HIfill_file_rec(file_rec, FUNC) == FAIL) {
                   HERROR(DFE_BADOPEN);
                   HI_CLOSE(file_rec->file);
                   return FAIL;
               }
           }
       }

       /* do *not* use else here */

       if (access == DFACC_CREATE || new_file) {
           /* create the file */

        /* version tags */
            vtag = 1;
        /* end version tags */

           file_rec->file = HI_CREATE(path);
           if (OPENERR(file_rec->file)) {
               HERROR(DFE_BADOPEN);
               return FAIL;
           }

           /* set up the newly created (and empty) file with
              the magic cookie and initial data descriptor records */

           if (HI_WRITE(file_rec->file, HDFMAGIC, MAGICLEN) == FAIL) {
               HERROR(DFE_WRITEERROR);
               return FAIL;
           }
           if (HI_FLUSH(file_rec->file) == FAIL) {  /* flush the cookie */
               HERROR(DFE_WRITEERROR);
               return FAIL;
           }
           if (HIinit_file_dds(file_rec, ndds, FUNC) == FAIL) {
               HERROR(DFE_WRITEERROR);
               return FAIL;
           }
           file_rec->maxref = 0;
           file_rec->access = new_file ? access | DFACC_READ : DFACC_ALL;
       }
       file_rec->refcount = 1;
       file_rec->attach = 0;

       /* Set up the new pointers for empty space */
       file_rec->null_block = file_rec->ddhead;
       file_rec->null_idx   = -1;
       
       /* version tags */
       if (vtag == 1)
         HIupdate_version(FSLOT2ID(slot));
     }

    file_rec->version_set = FALSE;

    if(vtag==0)
        HIread_version(FSLOT2ID(slot));
    /* end version tags */

    return FSLOT2ID(slot);
  }

/*--------------------------------------------------------------------------
 NAME
       Hclose -- close HDF file
 USAGE
       intn Hclose(id)
       int id;                 IN: the file id to be closed
 RETURNS
       returns SUCCEED (0) if successful and FAIL (-1) if failed.
 DESCRIPTION
       closes an HDF file given the file id.  Id is first validated.  If
       there are still access objects attached to the file, an error is
       returned and the file is not closed.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hclose(int32 file_id)
#else
intn Hclose(file_id)
    int32 file_id;             /* id of closing file */
#endif
{
  register intn i;
  char *FUNC="Hclose";       /* for HERROR */
  filerec_t *file_rec;       /* file record pointer */
  register tag_ref_list_ptr p, q;
  
  /* convert file id to file rec and check for validity */
  
  file_rec = FID2REC(file_id);
  if (!file_rec || file_rec->refcount == 0) {
    HERROR(DFE_ARGS);
    return FAIL;
  }
  
  /* version tags */
  if ((file_rec->refcount > 0) && (file_rec->version.modified == 1))
    HIupdate_version(file_id);
  /* end version tags */
  
  /* decrease the reference count */
  if (--file_rec->refcount == 0) {
    ddblock_t *bl, *next;  /* current ddblock and next ddblock pointers.
                                  for freeing ddblock linked list*/
    
    /* if file reference count is zero but there are still attached
       access elts, reject this close. */
    
       if (file_rec->attach > 0) {
         file_rec->refcount++;
         HERROR(DFE_OPENAID);
         HEreport("There are still %d active aids attached",file_rec->attach);
         return FAIL;
       }
    
    /* otherwise, nothing should still be using this file, close it */
#if 0
       if (HI_CLOSE(file_rec->file) == FAIL) {
         HERROR(DFE_CLOSE);
         return FAIL;
       }
#else
    /* ignore any close error */
    HI_CLOSE(file_rec->file);
#endif
    
    /* free the ddblock linked list of this file record, the path name;
       and reset some variables */
    
    for (bl = file_rec->ddhead; bl; bl = next) {
      next = bl->next;
      if (bl->ddlist) HDfreespace((VOIDP) bl->ddlist);
      if (bl) HDfreespace((VOIDP)bl);
    }
    
    for(i = 0; i < HASH_MASK + 1; i++) {
      for(p = file_rec->hash[i]; p; p = q) {
        q = p->next;
        HDfreespace((VOIDP)p);
      }
      file_rec->hash[i] = NULL;
    }
    
    file_rec->ddhead = (ddblock_t *) NULL;
    if (file_rec->path) HDfreespace(file_rec->path);
    file_rec->path = (char *)NULL;
  }
  
  return SUCCEED;
} /* Hclose */

/*--------------------------------------------------------------------------

 NAME
       Hstartread -- locate and position a read access elt on a tag/ref
 USAGE
       int32 Hstartread(fileid, tag, ref)
       int fileid;             IN: id of file to attach access element to
       int tag;                IN: tag to search for
       int ref;                IN: ref to search for
 RETURNS
       returns id of access element if successful, otherwise FAIL (-1)
 DESCRIPTION
       Searches the DD's for a particular tag/ref combination.  The
       searching starts from the head of the DD list.  Wildcards can be
       used for tag or ref (DFTAG_WILDCARD, DFREF_WILDCARD) and they match
       any values.  If the search is successful, the access elt is
       positioned to the start of that tag/ref, otherwise it is an error.
       An access element is created and attached to the file.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hstartread(int32 file_id, uint16 tag, uint16 ref)
#else
int32 Hstartread(file_id, tag, ref)
    int32 file_id;             /* file id to read from */
    uint16 tag;                        /* tag of elt to read */
    uint16 ref;                        /* ref of elt to read */
#endif
{
    char *FUNC="Hstartread";   /* for HERROR */
    int slot;                  /* slot in access record array */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */

    /* clear error stack */

    HEclear();

    /* convert file id to file record and check for validity */

    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* get a slot in the access record array */

    slot = HIget_access_slot();
    if (slot == FAIL) {
       HERROR(DFE_TOOMANY);
       return FAIL;
    }
    access_rec = &(access_records[slot]);

#ifndef oldspecial
    /* convert tag to base form */

    tag = BASETAG(tag);
#endif

    /* search for the data element in the dd lists of the file */

    access_rec->file_id = file_id;
    access_rec->block = file_rec->ddhead;
    access_rec->idx = -1;
    if (HIlookup_dd(file_rec, tag, ref, &access_rec->block, &access_rec->idx) == FAIL) {
       HERROR(DFE_NOMATCH);
       access_rec->used = FALSE;
       return FAIL;
    }

    /* if special data element, get the relevant special function table
       and run the START READ function on this element */

    if (SPECIALTAG(access_rec->block->ddlist[access_rec->idx].tag)) {
       access_rec->special_func = HIget_function_table(access_rec, FUNC);
       if (!access_rec->special_func) {
           HERROR(DFE_INTERNAL);
           access_rec->used = FALSE;
           return FAIL;
       }
       return (*access_rec->special_func[SP_STREAD])(access_rec);
    }

    /* reset the data element and update file record */
    access_rec->posn = 0;
    access_rec->access = DFACC_READ;
    access_rec->special = 0;
    access_rec->appendable=FALSE;   /* start data as non-appendable */
    access_rec->flush=FALSE;        /* start data as not needing flushing */
    file_rec->attach++;

    return ASLOT2ID(slot);
}   /* Hstartread() */

/*--------------------------------------------------------------------------

 NAME
       Hnextread -- locate and position a read access elt on tag/ref.
 USAGE
       intn Hnextread(access_id, tag, ref, origin)
       int32 access_id;         IN: id of a READ access elt
       uint16 tag;              IN: the tag to search for
       uint16 ref;              IN: ref to search for
       int origin;              IN: from where to start searching
 RETURNS
       returns SUCCEED (0) if successful and FAIL (-1) otherwise
 DESCRIPTION
       Searches for the `next' DD that fits the tag/ref.  Wildcards
       apply.  If origin is DF_START, search from start of DD list,
       if origin is DF_CURRENT, search from current position, otherwise
       origin should be DF_END which searches from end of file.
       If the search is successful, then the access elt
       is positioned at the start of that tag/ref, otherwise, it is not
       modified.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DF_END _not_ supported yet!
 EXAMPLES
 REVISION LOG
    Moved file_rec initialization in front of if statement instead
    of inside it.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hnextread(int32 access_id, uint16 tag, uint16 ref, int origin)
#else
intn Hnextread(access_id, tag, ref, origin)
    int32 access_id;           /* id of the read access record to modify */
    uint16 tag;                        /* the tag to look for */
    uint16 ref;                        /* the ref to look for */
    int origin;                        /* where to start searching from */
#endif
{
    char *FUNC="Hnextread";    /* for HERROR */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */
    ddblock_t *block;
    int32 idx;

    /* clear error stack and check validity of the access id */

    HEclear();
    access_rec = AID2REC(access_id);
    if (access_rec == (accrec_t *) NULL || !access_rec->used ||
       access_rec->access != DFACC_READ ||
       (origin != DF_START && origin != DF_CURRENT)) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* DF_END is NOT supported yet !!!! */

    file_rec = FID2REC(access_rec->file_id);
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
        HERROR(DFE_INTERNAL);
        return FAIL;
    }

    /*
     * if access record used to point to an external element we 
     * need to close the file before moving on
     */
    if(access_rec->special == SPECIAL_EXT) {
        if(HXIcloseAID(access_rec) == FAIL) {
            HERROR(DFE_CANTCLOSE);
            return FAIL;
        }
    }

    if (origin == DF_START) {
       /* set up variables to start searching from beginning of file */
       block = file_rec->ddhead;
       idx = -1;

    } else {                   /* origin == CURRENT */
       /* set up variables to start searching from the current position */

       block = access_rec->block;
       idx = access_rec->idx;
    }

    /* go look for the dd */

    if (HIlookup_dd(file_rec, tag, ref, &block, &idx) == FAIL) {
       HERROR(DFE_NOMATCH);
       return FAIL;
    }

    /* found, so update the access record */

    access_rec->block = block;
    access_rec->idx = idx;
    if (SPECIALTAG(access_rec->block->ddlist[access_rec->idx].tag)) {

       /* special element, call special function to handle */

       access_rec->special_func = HIget_function_table(access_rec, FUNC);
       if (!access_rec->special_func) {
           HERROR(DFE_INTERNAL);
           return FAIL;
       }
       HIunlock(access_rec->file_id); /* remove old attach to the file_rec */
       return (int)(*access_rec->special_func[SP_STREAD])(access_rec);
    }

    access_rec->special = 0;
    access_rec->posn = 0;
    access_rec->appendable=FALSE;   /* start data as non-appendable */
    access_rec->flush=FALSE;        /* start data as not needing flushing */

    return SUCCEED;
}   /* end Hnextread() */

/*--------------------------------------------------------------------------

 NAME
       Hfind -- locate the next object of a search in an HDF file
 USAGE
       intn Hfind(file_id ,search_tag, search_ref, find_tag, find_ref,
            find_offset, find_length, position)
       int32 file_id;           IN: file ID to search in
       uint16 search_tag;       IN: the tag to search for
                                    (can be DFTAG_WILDCARD)
       uint16 search_ref;       IN: ref to search for
                                    (can be DFREF_WILDCARD)
       uint16 *find_tag;        IN: if (*find_tag==0) and (*find_ref==0)
                                    then start search
                                OUT: tag matching the search tag
       uint16 *find_ref;        IN: if (*find_tag==0) and (*find_ref==0)
                                    then start search
                                OUT: ref matching the search ref
       int32 *find_offset;      OUT: offset of the data element found
       int32 *find_length;      OUT: length of the data element found
       intn direction;          IN: Direction to search in - DF_FORWARD
                                    searches forward from the current location,
                                    DF_BACKWARD searches backward from the
                                    current location.
 RETURNS
       returns SUCCEED (0) if successful and FAIL (-1) otherwise
 DESCRIPTION
       Searches for the `next' DD that fits the search tag/ref.  Wildcards
       apply.  If origin is DF_FORWARD, search from current position forwards
       in the file, otherwise DF_BACKWARD searches backward from the current
       position in the file.  If *find_tag and *find_ref are both set to
       0, this indicates the beginning of a search, and the search will
       start from the beginning of the file if the direction is DF_FORWARD
       and from the and of the file if the direction is DF_BACKWARD.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hfind(int32 file_id, uint16 search_tag, uint16 search_ref,
    uint16 *find_tag,uint16 *find_ref,int32 *find_offset,int32 *find_length,
    intn direction)
#else
intn Hfind(file_id, search_tag, search_ref, find_tag, find_ref, find_offset,
    find_length, direction)
int32 file_id;
uint16 search_tag, search_ref;
uint16 *find_tag,*find_ref;
int32 *find_offset,*find_length;
intn direction;
#endif
{
    char *FUNC="Hfind";         /* for HERROR */
    filerec_t *file_rec;        /* file record */
    ddblock_t *block;
    int32 idx;
    dd_t *list;                 /* ptr to current ddlist searched */

    /* clear error stack and check validity of the access id */
    HEclear();
    if(file_id == FAIL || search_ref>MAX_REF || find_tag==NULL ||
            find_ref==NULL || find_offset==NULL || find_length==NULL ||
            (direction != DF_FORWARD && direction != DF_BACKWARD)) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */

    file_rec = FID2REC(file_id);
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
        HERROR(DFE_INTERNAL);
        return(FAIL);
      } /* end if */

    if(*find_ref==0 && *find_tag==0) {  /* starting search */
        if(direction==DF_FORWARD) {     /* start at beginning of the DD list */
            block = file_rec->ddhead;
            idx = -1;
          } /* end if */
        else {      /* start the very end of the DD list */
            block = file_rec->ddlast;
            idx = block->ndds;
          } /* end else */
      } /* end if */
    else {      /* continue a search */
        /* get the block and index of the last tag/ref found, to continue */
        if(HIlookup_dd(file_rec, *find_tag, *find_ref, &block, &idx)==FAIL) {
            HERROR(DFE_NOMATCH);
            return(FAIL);
          } /* end if */
      } /* end else */

    /* Go get the next match in the given direction */
    if(HIfind_dd(search_tag,search_ref,&block,&idx,direction)==FAIL) {
        HERROR(DFE_NOMATCH);
        return(FAIL);
      } /* end if */

    list=block->ddlist;         /* get a pointer to the DD list to look in */
    *find_tag=list[idx].tag;
    *find_ref=list[idx].ref;
    *find_offset=list[idx].offset;
    *find_length=list[idx].length;

    return(SUCCEED);
}   /* end Hfind() */

/*--------------------------------------------------------------------------

 NAME
       Hinquire -- inquire stats of an access elt
 USAGE
       intn Hinquire(access_id, pfile_id, ptag, pref, plength,
                       poffset, pposn, paccess, pspecial)
       int access_id;          IN: id of an access elt
       int32 *pfile_id;        OUT: file id
       uint16 *ptag;           OUT: tag of the element pointed to
       uint16 *pref;           OUT: ref of the element pointed to
       int32 *plength;         OUT: length of the element pointed to
       int32 *poffset;         OUT: offset of elt in the file
       int32 *pposn;           OUT: position pointed to within the data elt
       int *paccess;           OUT: the access type of this access elt
     int *pspecial;            OUT: special code
 RETURNS
       returns SUCCEED (0) if the access elt points to some data element,
       otherwise FAIL (-1)
 DESCRIPTION
       Inquire statistics of the data element pointed to by access elt and
       the access elt.  The access type is set if the access_id is valid even
       if FAIL is returned.  If access_id is not valid then access is set to
       zero (0).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hinquire(int32 access_id, int32 *pfile_id, uint16 *ptag, uint16 *pref,
              int32 *plength, int32 *poffset, int32 *pposn, int16 *paccess,
              int16 *pspecial)
#else
intn Hinquire(access_id, pfile_id, ptag, pref, plength, poffset, pposn,
              paccess, pspecial)
    int32 access_id;           /* access id */
    int32 *pfile_id;           /* file id */
    uint16 *ptag;              /* elt tag */
    uint16 *pref;              /* elt ref */
    int32 *plength;            /* length of element */
    int32 *poffset;            /* offset of elt in the file */
    int32 *pposn;              /* position in the data elt we are accessing */
    int16 *paccess;              /* access mode */
    int16 *pspecial;             /* special code */
#endif
{
    char *FUNC="Hinquire";     /* for HERROR */
    register accrec_t *access_rec;      /* access record */
    register dd_t *dd;                  /* dd of access record */

    /* clear error stack and check validity of access id */

    HEclear();
    access_rec = AID2REC(access_id);
    if (access_rec == (accrec_t *) NULL || !access_rec->used) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* if special elt, let special functions handle it */

    if (access_rec->special) {
       return (int)(*access_rec->special_func[SP_INQUIRE])
           (access_rec, pfile_id, ptag, pref, plength,
            poffset, pposn, paccess, pspecial);
    }

    /* get dd and fill in variables asked for (i.e. not NULL) */

    dd = &(access_rec->block->ddlist[access_rec->idx]);
    if (pfile_id) *pfile_id = access_rec->file_id;
    if (ptag) *ptag = dd->tag;
    if (pref) *pref = dd->ref;
    if (plength) *plength = dd->length;
    if (poffset) *poffset = dd->offset;
    if (pposn) *pposn = access_rec->posn;
    if (paccess) *paccess = access_rec->access;
    if (pspecial) *pspecial = 0;

    return SUCCEED;
}   /* end Hinquire() */

/*--------------------------------------------------------------------------

 NAME
       Hstartwrite -- set up a WRITE access elt for a write
 USAGE
       int32 Hstartwrite(fileid, tag, ref, len)
       int fileid;             IN: id of file to write to
       int tag;                IN: tag to write to
       int ref;                IN: ref to write to
       long length;            IN: the length of the data element
 RETURNS
       returns id of access element if successful and FAIL otherwise
 DESCRIPTION
       Set up a WRITE access elt to write out a data element.  The DD list
       of the file is searched first.  If the tag/ref is found, it is
       NOT replaced - the seek position is presumably at 0.
                If it does not exist, it is created.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hstartwrite(int32 file_id, uint16 tag, uint16 ref, int32 length)
#else
int32 Hstartwrite(file_id, tag, ref, length)
    int32 file_id;             /* file id */
    uint16 tag;                        /* tag of elt to write */
    uint16 ref;                        /* ref of elt to write */
    int32 length;              /* length of elt to write */
#endif
{
    char *FUNC="Hstartwrite";  /* for HERROR */
    int slot;                  /* free access records array slot */
    bool ddnew = FALSE;                /* is the dd a new one? */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */
    /* version tags */
    uint32 lmajorv, lminorv, lrelease;
    uint32 fmajorv, fminorv, frelease;
    char string[81];
    int newver;
    /* end version tags */

    /* clear error stack and check validity of file id */
    HEclear();
    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* can write in this file? */
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

#ifndef oldspecial
    /* convert tag to base form */

    tag = BASETAG(tag);
#endif

    /* set up access record to look for the dd */
    access_rec = &(access_records[slot]);
    access_rec->file_id = file_id;
    access_rec->block = file_rec->ddhead;
    access_rec->idx = -1;
    if (HIlookup_dd(file_rec, tag, ref, &access_rec->block, &access_rec->idx)
            == FAIL) {  /* dd not found, so have to create new element */

        /* look for empty dd slot */
        if (HIfind_dd((uint16)DFTAG_NULL, (uint16)DFREF_WILDCARD,
                &file_rec->null_block, &file_rec->null_idx, DF_FORWARD) != FAIL) {
            access_rec->block = file_rec->null_block;
            access_rec->idx   = file_rec->null_idx;
        } else {     /* cannot find empty dd slot, so create new dd block */
           if (HInew_dd_block(file_rec, FILE_NDDS(file_rec), FUNC) == FAIL) {
               HERROR(DFE_NOFREEDD);
               access_rec->used = FALSE;
               return FAIL;
           } else {     /* use dd slot in new dd block */
               access_rec->block = file_rec->ddlast;
               access_rec->idx = 0;
           }
        }

        ddnew = TRUE;
        if(HIadd_hash_dd(file_rec, tag, ref, access_rec->block, access_rec->idx)
                == FAIL)
            return(FAIL);

    } else if (SPECIALTAG(access_rec->block->ddlist[access_rec->idx].tag)) {

       /* found, if this elt is special, let special function handle it */
       access_rec->special_func = HIget_function_table(access_rec, FUNC);
       if (!access_rec->special_func) {
           HERROR(DFE_INTERNAL);
           access_rec->used = FALSE;
           return FAIL;
       }
       return (*access_rec->special_func[SP_STWRITE])(access_rec);
    }

    /* the dd is pointed to by access_rec->block and access_rec->idx */

    /* cannot write more bytes than are allocated for element */
    if (!ddnew && (access_rec->block->ddlist[access_rec->idx].length < length)) {
        HERROR(DFE_BADLEN);
        HEreport("Values: old length %d   new length%d",
                access_rec->block->ddlist[access_rec->idx].length, length);
        access_rec->used = FALSE;
        return FAIL;
    }

    if (ddnew) {    /* have to allocate new space in the file for the data */
       int32 offset;           /* offset of this data element in file */

       /* place the data element at the end of the file and record its offset */
       if (HI_SEEKEND(file_rec->file) == FAIL) {
           HERROR(DFE_SEEKERROR);
           access_rec->used = FALSE;
           return FAIL;
       }
        offset = access_rec->block->ddlist[access_rec->idx].offset
            = HI_TELL(file_rec->file);

       /* reserve the space by marking the end of the element */

       if (HI_SEEK(file_rec->file, length-1+offset) == FAIL) {
           HERROR(DFE_SEEKERROR);
           access_rec->used = FALSE;
           return FAIL;
       }
       if (HI_WRITE(file_rec->file, tbuf, 1) == FAIL) {
           HERROR(DFE_WRITEERROR);
           access_rec->used = FALSE;
           return FAIL;
       }

       /* fill in dd record */
       access_rec->block->ddlist[access_rec->idx].tag = tag;
       access_rec->block->ddlist[access_rec->idx].ref = ref;
       access_rec->appendable=TRUE;     /* mark data as appendable */
    }

    /* update dd in the file */
    if (ddnew && length > 0)
        access_rec->block->ddlist[access_rec->idx].length = length;
    if (HIupdate_dd(file_rec, access_rec->block,
                   access_rec->idx, FUNC) == FAIL) {
        access_rec->used = FALSE;
        return FAIL;
    }

    /* update the access record, and the file record */

    access_rec->posn = 0;
    access_rec->access = DFACC_WRITE;
    access_rec->file_id = file_id;
    access_rec->special = 0;
    access_rec->appendable=FALSE;   /* start data as non-appendable */
    access_rec->flush=FALSE;        /* start data as not needing flushing */
    file_rec->attach++;
    if (ref > file_rec->maxref)
        file_rec->maxref = ref;

    /*
     *  If this is the first time we are writting to this file
     *    update the version tags as needed
     */
    if(!file_rec->version_set) {
      /* version tags */
      /* get file version and set newver condition */
      newver = 0;
      
      if (Hgetfileversion(file_id, &fmajorv, &fminorv, &frelease,string)!=SUCCEED) {
        newver = 1;
        HEclear();
      }
      
      /* get library version */
      Hgetlibversion(&lmajorv, &lminorv, &lrelease, string);

        /* check whether we need to update the file version tag */
      if (newver == 1 ||
              (lmajorv > fmajorv || lminorv > fminorv || lrelease > frelease)) {
        file_rec->version.majorv = lmajorv;
        file_rec->version.minorv = lminorv;
        file_rec->version.release = lrelease;
        HIstrncpy(file_rec->version.string, string, 81);
        file_rec->version.modified = 1;
      }
      
      file_rec->version_set = TRUE;

    } /* test to set version tags */
      
    return ASLOT2ID(slot);
}   /* end Hstartwrite() */

/*--------------------------------------------------------------------------

 NAME
       Happendable -- Allow a data set to be appended to without the
        use of linked blocks
 USAGE
       intn Happendable(aid)
       int32 aid;              IN: aid of the dataset to make appendable
 RETURNS
       returns 0 if dataset is allowed to be appendable, FAIL otherwise
 DESCRIPTION
       If a dataset is at the end of a file, allow Hwrite()s to write
       past the end of a file.  Allows expanding datasets without the use
       of linked blocks.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Happendable(int32 aid)
#else
intn Happendable(aid)
    int32 aid;              /* Access ID (from Hstartwrite, etc.) */
#endif
{
    char *FUNC="Happendable";   /* for HERROR */
    int32 file_id;              /* file id the AID is attached to */
    filerec_t *file_rec;        /* file record */
    accrec_t *access_rec;       /* access record */
    int32 file_off;             /* offset in the file we are at currently */
    int32 data_len;             /* length of the data we are checking */
    int32 data_off;             /* offset of the data we are checking */

    /* clear error stack and check validity of file id */
    HEclear();

    if((access_rec=AID2REC(aid))==NULL) {   /* get the access_rec pointer */
        HERROR(DFE_ARGS);
        return FAIL;
      } /* end if */

    file_id=access_rec->file_id;    /* get the file ID the AID is attached */

    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* get the offset and length of the dataset */
    data_len=access_rec->block->ddlist[access_rec->idx].length; 
    data_off=access_rec->block->ddlist[access_rec->idx].offset;


    file_off=HI_TELL(file_rec->file);   /* get the current offset */
    HI_SEEKEND(file_rec->file);
    if(data_len+data_off==HI_TELL(file_rec->file))    /* dataset at end? */
        access_rec->appendable=TRUE;
    else
        access_rec->appendable=FALSE;

    HI_SEEK(file_rec->file,file_off);   /* restore the previous position */


    if(access_rec->appendable)      /* return an appropriate status */
        return(SUCCEED);
    else
        return(FAIL);
}   /* end Happendable() */

/*--------------------------------------------------------------------------

 NAME
       Hseek -- position an access element to an offset in data element
 USAGE
       intn Hseek(access_id, offset, origin)
       int32 access_id;        IN: id of access element
       long offset;            IN: offset to seek to
       int origin;             IN: position to seek from by offset, 0: from
                               beginning; 1: current position; 2: end of
                               data element
 RETURNS
       returns FAIL (-1) if fail, SUCCEED (0) otherwise.
 DESCRIPTION
       Sets the position of an access element in a data element so that the
       next Hread or Hwrite will start from that position.  origin
       determines the position from which the offset should be added.  This
       routine fails if the access elt is not associated with any data
       element and if the seeked position is outside of the data element.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hseek(int32 access_id, int32 offset, int origin)
#else
intn Hseek(access_id, offset, origin)
    int32 access_id;           /* access id */
    int32 offset;              /* offset in this element to seek to */
    int origin;                        /* origin in this elt to seek from */
#endif
{
    char *FUNC="Hseek";                /* for HERROR */
    accrec_t *access_rec;      /* access record */

    /* clear error stack and check validity of this access id */

    HEclear();
    access_rec = AID2REC(access_id);
    if (access_rec == (accrec_t *) NULL || !access_rec->used ||
       (origin != DF_START && origin != DF_CURRENT && origin != DF_END)) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* if special elt, use special function */

    if (access_rec->special) {
       return (*access_rec->special_func[SP_SEEK])(access_rec, offset, origin);
    }

    /* calculate real offset based on the origin and check for range */

    if (origin == DF_CURRENT) offset += access_rec->posn;
    if (origin == DF_END) offset +=
       access_rec->block->ddlist[access_rec->idx].length;
    if (offset < 0 ||
            (!access_rec->appendable &&
            offset >= access_rec->block->ddlist[access_rec->idx].length)) {
      HERROR(DFE_BADSEEK);
      HEreport("Tried to seek to %d (object length:  %d)", offset,
               access_rec->block->ddlist[access_rec->idx].length);

      fprintf(stderr, "TAG/REF was %d %d\n", 
              access_rec->block->ddlist[access_rec->idx].tag, 
              access_rec->block->ddlist[access_rec->idx].ref);

      return FAIL;
    }

    if(access_rec->appendable &&
            offset >= access_rec->block->ddlist[access_rec->idx].length) {
        filerec_t *file_rec;    /* file record */
        int32 file_off;         /* offset in the file we are at currently */
        int32 file_end;         /* length of the file */
        int32 data_len;         /* length of the data we are checking */
        int32 data_off;         /* offset of the data we are checking */

        /* get the offset and length of the dataset */
        data_len=access_rec->block->ddlist[access_rec->idx].length;
        data_off=access_rec->block->ddlist[access_rec->idx].offset;

        file_rec = FID2REC(access_rec->file_id);
        file_off=HI_TELL(file_rec->file);   /* get the current offset */
        HI_SEEKEND(file_rec->file);
        file_end=HI_TELL(file_rec->file);
        HI_SEEK(file_rec->file,file_off);   /* restore the previous position */
        if(data_len+data_off!=file_end) {    /* dataset at end? */
            access_rec->appendable=FALSE;
              HERROR(DFE_BADSEEK);
              HEreport("Tried to seek to %d (object length:  %d)", offset,
                       access_rec->block->ddlist[access_rec->idx].length);
              return FAIL;
          } /* end if */
      } /* end if */

    /* set the new position */

    access_rec->posn = offset;

    return SUCCEED;
}   /* Hseek() */

/*--------------------------------------------------------------------------

 NAME
       Hread -- read the next segment from data element
 USAGE
       int32 Hread(access_id, length, data)
       int access_id;          IN: id of READ access element
       long length;            IN: length of segment to read in
       char *data;             OUT: pointer to data array to read to
 RETURNS
       returns length of segment actually read in if successful and FAIL
       (-1) otherwise
 DESCRIPTION
       Read in the next segment in the data element pointed to by the
       access elt.  If the data element is too short then it only reads
       to end of the data element.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hread(int32 access_id, int32 length, uint8 *data)
#else
int32 Hread(access_id, length, data)
    int32 access_id;           /* access id */
    int32 length;              /* length of data to read */
    uint8 *data;               /* data buffer to read into */
#endif
{
    char *FUNC="Hread";                /* for HERROR */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */
    dd_t *dd;                  /* current dd pointer */

    /* clear error stack and check validity of access id */

    HEclear();
    access_rec = AID2REC(access_id);
    if (access_rec == (accrec_t *) NULL || !access_rec->used ||
/*
       access_rec->access != DFACC_READ || 
*/
        !data) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* special elt, so call special function */

    if (access_rec->special) {
       return (*access_rec->special_func[SP_READ])(access_rec, length, data);
    }

    /* check validity of file record */
    file_rec = FID2REC(access_rec->file_id);
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }

    /* get the dd of this data elt */

    dd = &(access_rec->block->ddlist[access_rec->idx]);
    if (length < 0){
       HERROR(DFE_BADSEEK);
       return FAIL;
    }

    /* seek to position to start reading and read in data */

    if (HI_SEEK(file_rec->file, access_rec->posn + dd->offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }

    /* length == 0 means to read to end of element,
       if read length exceeds length of elt, read till end of elt */

    if (length == 0 || length + access_rec->posn > dd->length)
       length = dd->length - access_rec->posn;

    if (HI_READ(file_rec->file, data, length) == FAIL) {
       HERROR(DFE_READERROR);
       return FAIL;
    }

    /* move the position of the access record */

    access_rec->posn += length;

    return length;
}   /* Hread() */

/*--------------------------------------------------------------------------

 NAME
       Hwrite -- write next data segment to data element
 USAGE
       int32 Hwrite(access_id, len, data)
       int access_id;          IN: id of WRITE access element
       long len;               IN: length of segment to write
       char *data;             IN: pointer to data to write
 RETURNS
       returns length of segment successfully written, FAIL (-1) otherwise
 DESCRIPTION
       Write the data to data element where the last write or Hseek()
       stopped.  If the space reserved is less than the length to write,
       then only as much as can fit is written.  It is the responsibility
       of the user to insure that no two access elements are writing to the
       same data element.  It is possible to interlace writes to more than
       one data elements in the same file though.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hwrite(int32 access_id, int32 length, uint8 *data)
#else
int32 Hwrite(access_id, length, data)
    int32 access_id;           /* access id */
    int32 length;              /* length of data to write */
    uint8 *data;               /* data buffer */
#endif
{
    char *FUNC="Hwrite";       /* for HERROR */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */
    dd_t *dd;                  /* ptr to dd of current elt */

    /* clear error stack and check validity of access id */

    HEclear();
    access_rec = AID2REC(access_id);
    if (access_rec == (accrec_t *) NULL || !access_rec->used ||
            access_rec->access != DFACC_WRITE || !data) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* if special elt, call special function */
    if (access_rec->special)
       return (*access_rec->special_func[SP_WRITE])(access_rec, length, data);

    /* check validity of file record and get dd ptr */
    file_rec = FID2REC(access_rec->file_id);
    if (!file_rec || file_rec->refcount == 0) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }
    dd = &(access_rec->block->ddlist[access_rec->idx]);

    /* check validity of length and write data.
       NOTE: it is an error to attempt write past the end of the elt */
    if (length <= 0 ||
            (!access_rec->appendable && length + access_rec->posn > dd->length)) {
       HERROR(DFE_BADSEEK);
       return FAIL;
    }

    if(access_rec->appendable && length + access_rec->posn > dd->length) {
        int32 file_off;         /* offset in the file we are at currently */
        int32 file_end;         /* length of the file */
        int32 data_len;         /* length of the data we are checking */
        int32 data_off;         /* offset of the data we are checking */

        /* get the offset and length of the dataset */
        data_len=access_rec->block->ddlist[access_rec->idx].length;
        data_off=access_rec->block->ddlist[access_rec->idx].offset;

        file_off=HI_TELL(file_rec->file);   /* get the current offset */
        HI_SEEKEND(file_rec->file);
        file_end=HI_TELL(file_rec->file);
        HI_SEEK(file_rec->file,file_off);   /* restore the previous position */
        if(data_len+data_off!=file_end) {    /* dataset at end? */
            access_rec->appendable=FALSE;
            HERROR(DFE_BADSEEK);
            return FAIL;
          } /* end if */
        dd->length=access_rec->posn+length;   /* update the DD length */
        access_rec->flush=TRUE; /* make certain the DD gets updated on disk */
      } /* end if */

    if (HI_SEEK(file_rec->file, access_rec->posn + dd->offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    if (HI_WRITE(file_rec->file, data, length) == FAIL) {
       HERROR(DFE_WRITEERROR);
       return FAIL;
    }

    /* update position of access in elt */

    access_rec->posn += length;

    return length;
}   /* end Hwrite() */

/*--------------------------------------------------------------------------

 NAME
       Hendaccess -- to dispose of an access element
 USAGE
       int32 Hendaccess(access_id)
       int access_id;          IN: id of access element to dispose of
 RETURNS
       returns SUCCEED (0) if successful, FAIL (-1) otherwise
 DESCRIPTION
       Used to dispose of an access element.  If access elements are not
       disposed it will eventually become impossible to allocate new
       ones.

       If there are active aids Hclose will *NOT* close the file.  This
       is a very common problem when developing new code.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hendaccess(int32 access_id)
#else
int32 Hendaccess(access_id)
    int32 access_id;           /* access id */
#endif
{
    char *FUNC="Hendaccess";   /* for HERROR */
    filerec_t *file_rec;       /* file record */
    accrec_t *access_rec;      /* access record */

    /* check validity of access id */
    access_rec = AID2REC(access_id);
    if (!access_rec || !access_rec->used) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* if special elt, call special function */

    if (access_rec->special) {
       return (*access_rec->special_func[SP_END])(access_rec);
    }

    /* check validity of file record */

    file_rec = FID2REC(access_rec->file_id);
    if (!file_rec || file_rec->refcount == 0) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }

    /* update file and access records */
    if(access_rec->flush) { /* check whether to flush the assoc. DD */
        if(HIupdate_dd(file_rec, access_rec->block, access_rec->idx, FUNC)==FAIL) {
            HERROR(DFE_CANTFLUSH);
            return FAIL;
          } /* end if */
      } /* end if */

    file_rec->attach--;
    access_rec->used = FALSE;

    return SUCCEED;
}   /* end Hendaccess() */

/*--------------------------------------------------------------------------

 NAME
       Hgetelement -- read in a data element
 USAGE
       int Hgetelement(file_id, tag, ref, data)
       int file_id;            IN: id of the file to read from
       int tag;                IN: tag of data element to read
       int ref;                IN: ref of data element to read
       char *data;             OUT: buffer to read into
 RETURNS
       returns SUCCEED (0) if successful, FAIL (-1)
       otherwise
 DESCRIPTION
       Read in a data element from a HDF file and puts it into buffer
       pointed to by data.  The space allocated for buffer is assumed to be
       large enough.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hgetelement(int32 file_id, uint16 tag, uint16 ref, uint8 *data)
#else
int32 Hgetelement(file_id, tag, ref, data)
    int32 file_id;             /* id of file to read from */
    uint16 tag;                        /* tag of elt to read */
    uint16 ref;                        /* ref of elt to read */
    uint8 *data;                       /* data buffer to read into */
#endif
{
    char *FUNC="Hgetelement";  /* for HERROR */
    int32 access_id;           /* access record id */
    int32 length;              /* length of this elt */
    int32 ret;                 /* return code */

    /* clear error stack */

    HEclear();

    /* get the access record, get the length of the elt, read in data,
       and dispose of access record */

    access_id = Hstartread(file_id, tag, ref);
    if (access_id == FAIL) {
       HERROR(DFE_NOMATCH);
       return FAIL;
    }
    Hinquire(access_id, (int32 *)NULL, (uint16 *)NULL, (uint16 *)NULL, &length,
            (int32 *)NULL, (int32 *)NULL, (int16 *)NULL, (int16 *)NULL);
    if ((ret = Hread(access_id, 0, data)) == FAIL) {
       HERROR(DFE_READERROR);
    }
    (void) Hendaccess(access_id);

    return (ret == FAIL) ? ret : length;
}   /* Hgetelement() */

/*--------------------------------------------------------------------------

 NAME
       Hputelement -- writes a data element
 USAGE
       int Hputelement(fileid, tag, ref, data, length)
       int fileid;             IN: id of file
       int tag;                IN: tag of data element to put
       int ref;                IN: ref of data element to put
       char *data;             IN: pointer to buffer
       long length;            IN: length of data
 RETURNS
       returns SUCCEED (0) if successful and FAIL (-1) otherwise
 DESCRIPTION
       Writes a data element or replace an existing data element in a HDF
       file.  Uses Hwrite and its associated routines.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int Hputelement(int32 file_id, uint16 tag, uint16 ref, uint8 *data,
               int32 length)
#else
int Hputelement(file_id, tag, ref, data, length)
    int32 file_id;             /* file id to write to */
    uint16 tag;                /* tag of elt to write */
    uint16 ref;                /* ref of elt to write */
    uint8 *data;               /* data buffer to write */
    int32 length;              /* length of data to write */
#endif
{
    char *FUNC="Hputelement";  /* for HERROR */
    int32 access_id;           /* access record id */
    int32 ret;                  /* return code */

    /* clear error stack */

    HEclear();

    /* get access record, write out data and dispose of access record */

    access_id = Hstartwrite(file_id, (uint16)tag, (uint16)ref, length);
    if (access_id == FAIL) {
       HERROR(DFE_NOMATCH);
       return FAIL;
    }
    if ((ret = Hwrite(access_id, length, data)) == FAIL) {
       HERROR(DFE_WRITEERROR);
    }
    (void) Hendaccess(access_id);

    return (ret == length ? SUCCEED : FAIL);
}   /* end Hputelement() */

/*--------------------------------------------------------------------------

 NAME
       Hlength -- returns length of a data element
 USAGE
       int32 Hlength(fileid, tag, ref)
       int fileid;             IN: id of file
       int tag;                IN: tag of data element
       int ref;                IN: ref of data element
 RETURNS
       return the length of a data element
 DESCRIPTION
       returns length of data element if it is present in the file.  Return
       FAIL (-1) if it is not in the file or an error occurs.

       The current implementation is probably less efficient than it could be.
       However, because of special elements the code is much cleaner this way.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hlength(int32 file_id, uint16 tag, uint16 ref)
#else
int32 Hlength(file_id, tag, ref)
    int32 file_id;             /* file id of elt to inquire */
    uint16 tag;                        /* tag of id to inquire */
    uint16 ref;                        /* ref of id to inquire */
#endif
{
    char *FUNC="Hlength";      /* for HERROR */
    int32 access_id;           /* access record id */
    int32 length;              /* length of elt inquired */
    int ret;                   /* return code */

    /* clear error stack */
    HEclear();

    /* get access record, inquire about lebngth and then dispose of
       access record */
    access_id = Hstartread(file_id, tag, ref);
    if (access_id == FAIL) {
       HERROR(DFE_ARGS);
       return FAIL;
    }
    if ((ret = (int)Hinquire(access_id, (int32*)NULL, (uint16*)NULL,
                  (uint16*)NULL,&length, (int32*)NULL, (int32*)NULL,
                  (int16*)NULL, (int16*) NULL)) == FAIL) {
       HERROR(DFE_INTERNAL);
    }
    (void) Hendaccess(access_id);

    return (ret == FAIL) ? (int32) FAIL : length;
}   /* end Hlength() */

/*--------------------------------------------------------------------------

 NAME
       Hoffset -- get offset of data element in the file
 USAGE
       int32 Hoffset(fileid, tag, ref)
       int32 fileid;           IN: id of file
       uint16 tag;             IN: tag of data element
       uint16 ref;             IN: ref of data element
 RETURNS
       returns offset of data element if it is present in the file or FAIL (-1)
       if it is not.  This should be used for debugging purposes only since 
       the user should not have to know the actual offset of a data element in a file.

       Like Hlength().  This could be sped up by not going through Hstartread()
       but because of special elements it is easier this way
 DESCRIPTION
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hoffset(int32 file_id, uint16 tag, uint16 ref)
#else
int32 Hoffset(file_id, tag, ref)
    int32 file_id;             /* file id of elt to inquire */
    uint16 tag;                        /* tag of elt to inquire */
    uint16 ref;                        /* ref of elt to inquire */
#endif
{
    char *FUNC="Hoffset";      /* for HERROR */
    int32 access_id;           /* access record id */
    int32 offset;              /* offset of elt inquired */
    int ret;                   /* return code */

    /* clear error stack */

    HEclear();

    /* get access record, inquire offset, and dispose of access record */

    access_id = Hstartread(file_id, tag, ref);
    if (access_id == FAIL) {
       HERROR(DFE_ARGS);
       return FAIL;
    }
    if ((ret = (int)Hinquire(access_id, (int32*)NULL, (uint16*)NULL,
                       (uint16*)NULL, (int32*)NULL, &offset, (int32*)NULL,
                       (int16*)NULL, (int16*) NULL)) == FAIL) {
       HERROR(DFE_INTERNAL);
    }
    (void) Hendaccess(access_id);

    return (ret == FAIL) ? (int32) FAIL : offset;
}   /* end Hoffset() */

/*--------------------------------------------------------------------------

 NAME
       Hdupdd -- duplicate a data descriptor
 USAGE
       int Hdupdd(file_id, tag, ref, old_tag, old_ref)
       int32 file_id;          IN: id of file
       uint16 tag;             IN: tag of new data descriptor
       uint16 ref;             IN: ref of new data descriptor
       uint16 old_tag;         IN: tag of data descriptor to duplicate
       uint16 old_ref;         IN: ref of data descriptor to duplicate
 RETURNS
       returns SUCCEED (0) if successful, FAIL (-1) otherwise
 DESCRIPTION
       Duplicates a data descriptor so that the new tag/ref points to the
       same data element pointed to by the old tag/ref.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int Hdupdd(int32 file_id, uint16 tag, uint16 ref,
          uint16 old_tag, uint16 old_ref)
#else
int Hdupdd(file_id, tag, ref, old_tag, old_ref)
    int32 file_id;             /* file id of dd's to duplicate */
    uint16 tag;                        /* tag of new duplicate dd */
    uint16 ref;                        /* ref of new duplicate dd */
    uint16 old_tag;            /* tag of old dd to duplicate */
    uint16 old_ref;            /* ref of old dd to duplicate */
#endif
{
    char *FUNC="Hdupdd";       /* for HERROR */
    filerec_t *file_rec;       /* file record */
    ddblock_t *block;          /* dd block fo old dd */
    ddblock_t *new_block;      /* dd block of new dd */
    int32 idx;                 /* index into dd list for old dd */
    int32 new_idx;             /* index into dd list for new dd */

    /* clear error stack and check validity of file id */

    HEclear();
    file_rec = FID2REC(file_id);
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* look for old dd and new dd in file record */

    new_block = block = file_rec->ddhead;
    new_idx = idx = -1;
    if (HIlookup_dd(file_rec, old_tag, old_ref, &block, &idx) == FAIL) {
       HERROR(DFE_NOMATCH);
       return FAIL;
    }
    if (FAIL != HIlookup_dd(file_rec, tag, ref, &new_block, &new_idx)) {

       /* dd already exist, cannot modify */

       HERROR(DFE_DUPDD);
       return FAIL;
    }

    /* look for empty dd to put new dd */

    new_block = file_rec->ddhead;
    new_idx = -1;
    if (HIlookup_dd(file_rec, (uint16)DFTAG_NULL, (uint16)DFTAG_WILDCARD,
                 &file_rec->null_block, &file_rec->null_idx) == FAIL) {
       if (HInew_dd_block(file_rec, FILE_NDDS(file_rec), FUNC) == FAIL) {
           HERROR(DFE_NOFREEDD);
           return FAIL;
       } else {
           new_block = file_rec->ddlast;
           new_idx = 0;
       }
    } else {
      new_block = file_rec->null_block;
      new_idx   = file_rec->null_idx;
    }

    /* fill in the new dd with details from old dd and update file with
       new dd */

    new_block->ddlist[new_idx].tag = tag;
    new_block->ddlist[new_idx].ref = ref;
    new_block->ddlist[new_idx].offset = block->ddlist[idx].offset;
    new_block->ddlist[new_idx].length = block->ddlist[idx].length;

    /* add the new thing to the hash table */
    if(HIadd_hash_dd(file_rec, tag, ref,  new_block, new_idx))
      return FAIL;                     

    return HIupdate_dd(file_rec, new_block, new_idx, FUNC);
}   /* end Hdupdd() */

/*--------------------------------------------------------------------------
 HIupdate_dd

 write an updated dd (in memory) to the file.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int HIupdate_dd(filerec_t *file_rec, ddblock_t *block, int32 idx, char *FUNC)
#else
int HIupdate_dd(file_rec, block, idx, FUNC)
    filerec_t *file_rec;       /* file record */
    ddblock_t *block;          /* dd block of updated dd */
    int32 idx;                 /* dd list index of updated dd */
    char *FUNC;                /* for HERROR */
#endif
{
    int32 offset;              /* offset of updated dd in file */
    uint8 *p;                  /* temp buffer ptr */

    /* look for offset of updated dd block in the file */
    if (block == file_rec->ddhead)      /* updated ddblock is the first one */
       offset = MAGICLEN + NDDS_SZ + OFFSET_SZ + (idx * DD_SZ);
    else
       offset = block->prev->nextoffset + NDDS_SZ + OFFSET_SZ + (idx * DD_SZ);

    /* write in the updated dd */
    if (HI_SEEK(file_rec->file, offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    p = tbuf;
    UINT16ENCODE(p, block->ddlist[idx].tag);
    UINT16ENCODE(p, block->ddlist[idx].ref);
    INT32ENCODE(p, block->ddlist[idx].offset);
    INT32ENCODE(p, block->ddlist[idx].length);
    if (HI_WRITE(file_rec->file, tbuf, DD_SZ) == FAIL) {
       HERROR(DFE_WRITEERROR);
       return FAIL;
    }

    return SUCCEED;
}

/*--------------------------------------------------------------------------

 NAME
       Hdeldd -- delete a data descriptor
 USAGE
       int Hdeldd(file_id, tag, ref)
       int file_id;            IN: id of file
       int tag;                IN: tag of data descriptor to delete
       int ref;                IN: ref of data descriptor to delete
 RETURNS
       returns SUCCEED (0) if successful, FAIL (-1) otherwise
 DESCRIPTION
       Deletes a data descriptor of tag/ref from the dd list of the file.
       This routine is unsafe and may leave a file in a condition that is
       not usable by some routines.  Use with care.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int Hdeldd(int32 file_id, uint16 tag, uint16 ref)
#else
int Hdeldd(file_id, tag, ref)
    int32 file_id;             /* file record id */
    uint16 tag;                        /* tag of dd to delete */
    uint16 ref;                        /* ref of dd to delete */
#endif
{
    char *FUNC="Hdeldd";       /* for HERROR */
    filerec_t *file_rec;       /* file record */
    ddblock_t *block;          /* dd block of deleted dd */
    int32 idx;                 /* dd list index of deleted dd */

    /* clear error stack and check validity of file record id */

    HEclear();
    file_rec = FID2REC(file_id);
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0 ||
       tag == DFTAG_WILDCARD || ref == DFREF_WILDCARD) {
       HERROR(DFE_ARGS);
       return FAIL;
    }

    /* look for the deleted dd */
    if (HIlookup_dd(file_rec, tag, ref, &block, &idx) == FAIL) {
       HERROR(DFE_NOMATCH);
       return FAIL;
    }

    /* this may have thrown off our ending markers, reset to beginning */

    file_rec->null_block = file_rec->ddhead;
    file_rec->null_idx   = -1;

    /* mark the dd as empty and then update the file */

    block->ddlist[idx].tag = DFTAG_NULL;

    /* remove it from the hash table */
    if(HIdel_hash_dd(file_rec, tag, ref) == FAIL)
      return FAIL;

    return HIupdate_dd(file_rec, block, idx, FUNC);
}   /* end Hdeldd() */

/*--------------------------------------------------------------------------

 NAME
       Hnewref -- returns a ref that is guaranteed to be unique in the file
 USAGE
       uint16 Hnewref(file_id)
       int32 file_id;          IN: id of file
 RETURNS
       returns the ref number, 0 otherwise
 DESCRIPTION
       Returns a ref number that can be used with any tag to produce a
       unique tag/ref.  Successive calls to Hnewref will generate a
       strictly increasing sequence until the highest possible ref had been
       returned, then Hnewref will return unused ref's starting from 1.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
uint16 Hnewref(int32 file_id)
#else
uint16 Hnewref(file_id)
    int32 file_id;             /* file record id */
#endif
{
    char *FUNC="Hnewref";      /* for HERROR */
    filerec_t *file_rec;       /* file record */
    uint16 ref;                        /* the new ref */

    /* clear error stack and check validity of file record id */

    HEclear();
    file_rec = FID2REC(file_id);
    if (file_rec == (filerec_t *) NULL || file_rec->refcount == 0) {
       HERROR(DFE_ARGS);
       return 0;
    }

    /* if maxref of this file is still below the maximum,
       just return next number */
    if (file_rec->maxref < MAX_REF) {
       return ++(file_rec->maxref);
    }

    /* otherwise, search for an empty ref */

    /* incredibly slow but unlikely situation */

    for (ref = 1; ref < MAX_REF; ref++) {
       ddblock_t *bl;
       int32 idx;
       bl = file_rec->ddhead;
       idx = -1;
       if (HIfind_dd((uint16)DFTAG_WILDCARD, ref, &bl, &idx, DF_FORWARD) == FAIL)
           return ref;
    }

    return 0;
}

/*--------------------------------------------------------------------------

 NAME
       Hishdf -- tells if a file is an HDF file
 USAGE
       int32 Hishdf(path)
       char *path;             IN: name of file
 RETURNS
       returns TRUE (non-zero) if file is HDF, FALSE (0) otherwise
 DESCRIPTION
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/

int32
#ifdef PROTOTYPE
Hishdf(char *filename)
#else
Hishdf(filename)
    char *filename;
#endif /* PROTOTYPE */
{
    char *FUNC = "Hishdf";

#if defined(VMS) || defined(MAC) || defined(PC)
  
    int32 fid;

    fid = Hopen(filename, DFACC_READ, 0);
    if(fid == FAIL)
        return FALSE;

    Hclose(fid);

    return TRUE;

#else

    bool ret;
    hdf_file_t fp;
    char b[MAGICLEN];
  
    fp = HI_OPEN(filename, DFACC_READ);
    if (OPENERR(fp))
        return(FALSE);
    else {
        if(HI_SEEK(fp, 0) == FAIL) {
            HERROR(DFE_SEEKERROR);
            return FALSE;
          }

        if(HI_READ(fp, b, MAGICLEN) == FAIL) {
            HERROR(DFE_READERROR);
            return FALSE;
          }
        if(NSTREQ(b, HDFMAGIC, MAGICLEN)) ret = TRUE;
        else ret = FALSE;

        HI_CLOSE(fp);
        return(ret);
    }
#endif
} /* Hishdf */

/*--------------------------------------------------------------------------

 NAME
       Htrunc -- truncate a data element to a length
 USAGE
       int32 Htrunc(aid, len)
       int32 aid;             IN: id of file
       int32 len;             IN: ref of data element
 RETURNS
       return the length of a data element
 DESCRIPTION
       truncates a data element in the file.  Return
       FAIL (-1) if it is not in the file or an error occurs.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Htrunc(int32 aid, int32 trunc_len)
#else
int32 Htrunc(aid, trunc_len)
    int32 aid;             /* access id of elt to truncate */
    int32 trunc_len;       /* length to truncate element to */
#endif
{
    char *FUNC="Htrunc";       /* for HERROR */
    accrec_t *access_rec;      /* access record */

    /* clear error stack and check validity of access id */

    HEclear();
    access_rec = AID2REC(aid);
    if (access_rec == (accrec_t *) NULL || !access_rec->used ||
            access_rec->access != DFACC_WRITE) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* Dunno about truncating special elements... */
#ifdef OLD_WAY
    /* if special elt, call special function */
    if (access_rec->special)
       return (*access_rec->special_func[SP_WRITE])(access_rec, length, data);
#endif

    /* check for actually being able to truncate the data */
    if(access_rec->block->ddlist[access_rec->idx].length>trunc_len) {
        access_rec->block->ddlist[access_rec->idx].length=trunc_len;
        if(access_rec->posn>trunc_len)  /* move the seek position back */
            access_rec->posn=trunc_len;
        access_rec->flush=TRUE; /* make certain the DD gets updated on disk */
        return(trunc_len);
      } /* end if */
    else {
        HERROR(DFE_BADLEN);
        return FAIL;
      } /* end else */
}   /* end Htrunc() */

/*--------------------------------------------------------------------------
 NAME
       Hsync -- sync file with memory
 USAGE
       int Hsync(file_id)
       int file_id;            IN: id of file
 RETURNS
       returns SUCCEED (0) if sucessful, FAIL (-1) otherwise
 DESCRIPTION
       Currently, the on-disk and in-memory representations are always
       the same.  Thus there is no real use for Hsync().  In the future,
       things may be buffered before being written out at which time
       Hsync() will be useful to sync up the on-disk representation.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
/* ARGSUSED */
#ifdef PROTOTYPE
int Hsync(int32 file_id)
#else
int Hsync(file_id)
    int32 file_id;
#endif
{
#ifdef QAK
    accrec_t *access_rec;      /* access record */
    filerec_t *file_rec;       /* file record */

    access_rec = AID2REC(access_id);
    if (access_rec == (accrec_t *) NULL || !access_rec->used ||
            access_rec->access != DFACC_WRITE || !data) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* check validity of file record and get dd ptr */
    file_rec = FID2REC(access_rec->file_id);
    if (!file_rec || file_rec->refcount == 0) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }
  
    if(access_rec->flush) { /* check whether to flush the assoc. DD */
        if(HIupdate_dd(file_rec, access_rec->block, access_rec->idx, FUNC)==FAIL) {
            HERROR(DFE_CANTFLUSH);
            return FAIL;
          } /* end if */
      } /* end if */
#endif
    return SUCCEED;
}


/*--------------------------------------------------------------------------
 HDvalidfid

 Check to see if a file id is valid.  Used by external functions.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
bool HDvalidfid(int32 file_id)
#else
bool HDvalidfid(file_id)
    int32 file_id;
#endif
{
    filerec_t *file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0)
       return FALSE;
    else
       return TRUE;
}

/*--------------------------------------------------------------------------
 HDerr

 Closes a file and return FAIL.  Replacement for DFIerr in HDF3.1 and before
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int HDerr(int32 file_id)
#else
int HDerr(file_id)
    int32 file_id;
#endif
{
    Hclose(file_id);
    return FAIL;
}


/*==========================================================================

  Internal Routines 

==========================================================================*/




/*--------------------------------------------------------------------------
 HIchangedd

  function for setting access elements record if that record had been
   made special.  It actually just fills in the appropriate info
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
static int HIchangedd(dd_t *datadd, ddblock_t *block, int idx, int16 special,
              VOIDP special_info, int32 (**special_func)())
#else
static int HIchangedd(datadd, block, idx, special, special_info, special_func)
    dd_t *datadd;               /* dd that had been converted to special */
    ddblock_t *block;           /* new dd block of converted dd */
    int idx;                    /* next dd list index of converted dd */
    int16 special;              /* special code of converted dd */
    VOIDP special_info;         /* special info of converted dd */
    int32 (**special_func)();   /* special function table of converted dd */
#endif
{
    int i;                     /* temp index */
    int attached = 0;          /* number of accesses attached to this dd */

    /* go through access records to look for converted dd,
       and then update the matching records */

    for (i=0; i<MAX_FILE; i++)
       if (access_records[i].used) {
           dd_t *tdd =         /* ptr to dd of current access record */
               &access_records[i].block->ddlist[access_records[i].idx];
           if (tdd == datadd) {
               access_records[i].block = block;
               access_records[i].idx = idx;
               access_records[i].special = special;
               access_records[i].special_func = special_func;
               access_records[i].special_info = special_info;
               attached++;
           }
       }

    return attached;
}

/*--------------------------------------------------------------------------
 HIinit_file_dds

 Initialize the first dd block in memory and in new file
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE int HIinit_file_dds(filerec_t *file_rec, int16 ndds, char *FUNC)
#else
PRIVATE int HIinit_file_dds(file_rec, ndds, FUNC)
    filerec_t *file_rec;       /* file record */
    int16 ndds;        /* number of dd's to put in this block */
    char *FUNC;                        /* for HERROR */
#endif
{
    ddblock_t *block;          /* dd block to intialize */
    uint8 *p;                  /* temp buffer ptr */
    dd_t *list;                /* list of dd */
    register int i;            /* temp ints */
    register int16 n;

    /* 'reasonablize' the value of ndds.  0 means use default */

    if (0 == ndds)
       ndds = DEF_NDDS;
    else if (ndds < MIN_NDDS)
       ndds = MIN_NDDS;

    /* allocate the dd block in memory and initialize it */

    file_rec->ddhead = (ddblock_t *) HDgetspace(sizeof(ddblock_t));
    if (file_rec->ddhead == (ddblock_t *) NULL) {
       HERROR(DFE_NOSPACE);
       return FAIL;
    }
    block = file_rec->ddlast = file_rec->ddhead;
    block->prev = (ddblock_t *) NULL;
    block->ndds = ndds;
    block->next = (ddblock_t *) NULL;
    block->nextoffset = 0;

    /* write first dd block to file */

    p = tbuf;
    INT16ENCODE(p, block->ndds);
    INT32ENCODE(p, (int32) 0);
    if (HI_WRITE(file_rec->file, tbuf, NDDS_SZ+OFFSET_SZ) == FAIL) {
       HERROR(DFE_WRITEERROR);
       return FAIL;
    }

    /* allocate and initialize dd list */

    list = block->ddlist = (dd_t *) HDgetspace((uint32) ndds * sizeof(dd_t));
    if (list == (dd_t *) NULL) {
       HERROR(DFE_NOSPACE);
       return FAIL;
    }
    for (i = 0; i < ndds; i++) {
       list[i].tag = DFTAG_NULL;
       list[i].ref = 0;
       list[i].length = list[i].offset = 0;
    }

    /* write dd list to file */

    /* n is the maximum number of dd's in tbuf */

    n = sizeof(int_tbuf) / DD_SZ;
    if (n > ndds) n = ndds;
    p = tbuf;

    for (i = 0; i < n; i++) {
       UINT16ENCODE(p, (uint16)DFTAG_NULL);
       UINT16ENCODE(p, (uint16)0);
       INT32ENCODE(p, (int32)0);
       INT32ENCODE(p, (int32)0);
    }
    while (ndds > 0) {
       if (HI_WRITE(file_rec->file, tbuf, n*DD_SZ) == FAIL) {
           HERROR(DFE_WRITEERROR);
           return FAIL;
       }
       ndds -= n;
       if (n > ndds) n = ndds;
    }

    /* write the version string */

    /* commented out for version tags
    if (HI_WRITE(file_rec->file, HDF_VERSION, HDstrlen(HDF_VERSION)) == FAIL) {
        HERROR(DFE_WRITEERROR);
       return FAIL;
    }
    end of commenting out for version tags */

    /* no dd's yet, so maximum ref is 0 */

    file_rec->maxref = 0;

    HDmemset(file_rec->hash, 0, sizeof(tag_ref_list_ptr) * (HASH_MASK+1));

    return SUCCEED;
}

/*--------------------------------------------------------------------------
 HIget_func_table

 get the function table for special elt of an access record
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE int32 (**HIget_function_table(accrec_t *access_rec, char *FUNC))()
#else
PRIVATE int32 (** HIget_function_table(access_rec, FUNC))()
    accrec_t *access_rec;      /* access record */
    char *FUNC;                        /* for HERROR */
#endif
{
    dd_t *dd;                  /* ptr to current dd */
    filerec_t *file_rec;       /* file record */

    /* read in the special code in the special elt */

    dd = &access_rec->block->ddlist[access_rec->idx];
    file_rec = FID2REC(access_rec->file_id);
    if (HI_SEEK(file_rec->file, dd->offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return NULL;
    }
    if (HI_READ(file_rec->file, tbuf, 2) == FAIL) {
       HERROR(DFE_READERROR);
       return NULL;
    }

    /* using special code, look up function table in associative table */

    {
       register int i;
       uint8 *p;
       p = tbuf;
       INT16DECODE(p, access_rec->special);
       for (i=0; functab[i].key != 0; i++) {
           if (access_rec->special == functab[i].key)
               return functab[i].tab;
       }
    }

    return NULL;
}

/*--------------------------------------------------------------------------
 HIgetspinfo

 used by routines in special elt modules
 get information for a special elt
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
VOIDP HIgetspinfo(accrec_t *access_rec, uint16 tag, uint16 ref)
#else
VOIDP HIgetspinfo(access_rec, tag, ref)
    accrec_t *access_rec;      /* file record id */
    uint16 tag;                        /* tag of special elt */
    uint16 ref;                        /* ref of special elt */
#endif
{
    register int i;            /* temp index */

    /* search access records for the matching dd,
       and return special information */

    for (i=0; i< MAX_ACC; i++)
       if (access_records + i != access_rec &&
           access_records[i].used &&
           access_records[i].file_id == access_rec->file_id &&
           access_records[i].block->ddlist[access_records[i].idx].tag==tag &&
           access_records[i].block->ddlist[access_records[i].idx].ref == ref)

           return (VOIDP) access_records[i].special_info;

    return NULL;
}

/*--------------------------------------------------------------------------
 HIlock

 lock a file record.  This is used by special functions to prevent
 losing files taht are still accessed
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
static int HIlock(int32 file_id)
#else
static int HIlock(file_id)
    int32 file_id;             /* file record id to lock */
#endif
{
    char *FUNC="HIlock";       /* for HERROR */

    /* get file record and check validity */

    filerec_t *file_rec=FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* lock the file record */
    file_rec->attach++;

    return SUCCEED;
}

/*--------------------------------------------------------------------------
 HIunlock

 unlock a previously locked file record
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
static int HIunlock(int32 file_id)
#else
static int HIunlock(file_id)
    int32 file_id;             /* file record to unlock */
#endif
{
    char *FUNC="HIunlock";     /* for HERROR */

    /* get file record and validate */

    filerec_t *file_rec=FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return FAIL;
    }

    /* unlock the file record */

    file_rec->attach--;

    return SUCCEED;
}


/*--------------------------------------------------------------------------
 Hnumber

 Returns the number of instances of a tag in a file.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int Hnumber(int32 file_id, uint16 tag)
#else
int Hnumber(file_id, tag)
    int32 file_id;
    uint16 tag;
#endif
{
    char *FUNC="Hnumber";
    int n = 0;
    ddblock_t *block;
    int32 idx;
    filerec_t *file_rec = FID2REC(file_id);

    HEclear();
    if (!file_rec || file_rec->refcount == 0) {
       HERROR(DFE_ARGS);
       return FAIL;
    }
    block = file_rec->ddhead;
    idx = -1;
    for (;;) {
        if (HIfind_dd(tag, DFREF_WILDCARD, &block, &idx, DF_FORWARD) == FAIL)
            break;
        n++;
    }
    return n;
}

/* "Special tag" routines */

/* The HDF tag space is divided as follows based on the 2 highest bits:
   00: NCSA reserved ordinary tags
   01: NCSA reserved special tags
   10, 11: User tags.

   It is relatively cheap to operate with special tags within the NCSA
   reserved tags range.  For users to specify special tags and their
   corresponding ordinary tag, the pair has to be added to the
   special_table. */

/* The special_table contains pairs of each tag and its corrsponding
   special tag.  The same table is also used to determine if a tag is
   special.  Add to this table any additional tag/special_tag pairs
   that might be necessary.  */

typedef struct special_table_t {
    uint16 tag;
    uint16 special_tag;
} special_table_t;

static special_table_t special_table[] = {
{0x8010, 0x4000 | 0x8010},             /* dummy */
};

#define SP_TAB_SZ (sizeof(special_table) / sizeof(special_table[0]))

/*--------------------------------------------------------------------------
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
uint16 HDmake_special_tag(uint16 tag)
#else
uint16 HDmake_special_tag(tag)
    uint16 tag;                        /* tag to convert */
#endif
{
    register int i;

    if (~tag & 0x8000)
       return ((uint16)(tag | 0x4000));

    for (i=0; i<SP_TAB_SZ; i++)
       if (special_table[i].tag == tag)
           return special_table[i].special_tag;

    return DFTAG_NULL;
}

/*--------------------------------------------------------------------------
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
bool HDis_special_tag(uint16 tag)
#else
bool HDis_special_tag(tag)
    uint16 tag;                        /* tag to check */
#endif
{
    register int i;

    if (~tag & 0x8000)
       return (tag & 0x4000) ? TRUE : FALSE;

    for (i=0; i<SP_TAB_SZ; i++)
       if (special_table[i].special_tag == tag)
           return TRUE;

    return FALSE;
}

/*--------------------------------------------------------------------------
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
uint16 HDbase_tag(uint16 tag)
#else
uint16 HDbase_tag(tag)
    uint16 tag;                        /* tag to convert */
#endif
{
    register int i;

    if (~tag & 0x8000)
       return ((uint16)(tag & ~0x4000));

    for (i=0; i<SP_TAB_SZ; i++)
       if (special_table[i].special_tag == tag)
           return special_table[i].special_tag;

    return tag;                        /* return itself */
}

/*--------------------------------------------------------------------------
**
** NAME
**	Hgetlibversion -- return version info for current HDF library
** USAGE
**	int Hgetlibversion(majorv, minorv, release, string)
**	uint32 *majorv;		OUT: majorv version number
**	uint32 *minorv;		OUT: minorv versoin number
**	uint32 *release;	OUT: release number
**	char   string[];	OUT: informational text string (80 chars)
** RETURNS
**	returns SUCCEED (0).
** DESCRIPTION
**	Copies values from #defines in hfile.h to provided buffers.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int Hgetlibversion(uint32 *majorv, uint32 *minorv, uint32 *release, char string[])
#else
int Hgetlibversion(majorv, minorv, release, string)
uint32 *majorv, *minorv, *release;
char string[];
#endif
{
    char *FUNC="Hgetlibversion";

    HEclear();

    *majorv = LIBVER_MAJOR;
    *minorv = LIBVER_MINOR;
    *release = LIBVER_RELEASE;
    HIstrncpy(string, LIBVER_STRING, 81);

    return(SUCCEED);
}

/*--------------------------------------------------------------------------
**
** NAME
**	Hgetfileversion -- return version info for HDF file
** USAGE
**	int Hgetfileversion(file_id, majorv, minorv, release, string)
**	int32 file_id;		IN: handle of file
**	uint32 *majorv;		OUT: majorv version number
**	uint32 *minorv;		OUT: minorv versoin number
**	uint32 *release;	OUT: release number
**	char *string;		OUT: informational text string (80 chars)
** RETURNS
**	returns SUCCEED (0) if successful and FAIL (-1) if failed.
** DESCRIPTION
**	Copies values from file_records[] for given file to provided buffers.
** GLOBAL VARIABLES
**	Reads file_records[]
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int Hgetfileversion(int32 file_id, uint32 *majorv, uint32 *minorv,
		    uint32 *release, char string[])
#else
int Hgetfileversion(file_id, majorv, minorv, release, string)
int32 file_id;
uint32 *majorv, *minorv, *release;
char string[];
#endif
{
    filerec_t *file_rec;
    char *FUNC="Hgetfileversion";


    HEclear();

    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }

    *majorv = file_rec->version.majorv;
    *minorv = file_rec->version.minorv;
    *release = file_rec->version.release;
    HIstrncpy(string, file_rec->version.string, 81);

    if (majorv == 0) {
        HERROR(DFE_NOMATCH);
        return(FAIL);
    } else
        return(SUCCEED);
}

#ifdef PC
/*--------------------------------------------------------------------------
**
** NAME
**  HDfreadbig -- function specific to the PC to read in 32-bit sized buffers
** USAGE
**  int32 HDfreadbig(buffer,size,fp)
**  VOIDP buffer;       IN: the buffer to put bytes into
**  int32 size;         IN: the number of bytes to read
**  FILE *fp;           IN: the file pointer for the file to read
** RETURNS
**  returns the number of bytes read
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for fread, this function
**  blocks that up into 64kb blocks to read from a file.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef WIN3
#ifdef PROTOTYPE
int32 HDfreadbig(VOIDP buffer,int32 size,HFILE fp)
#else
int32 HDfreadbig(buffer,size,fp)
VOIDP buffer;
int32 size;
HFILE fp;
#endif
#else /* !WIN3 */
#ifdef PROTOTYPE
int32 HDfreadbig(VOIDP buffer,int32 size,FILE *fp)
#else
int32 HDfreadbig(buffer,size,fp)
VOIDP buffer;
int32 size;
FILE *fp;
#endif
#endif /* WIN3 */
{
    uint8 *b;           /* alias for the buffer */
    int32 bytes_read;   /* variable to accumulate the number of bytes read in */

    if(size<=UINT_MAX)   /* if the size is small enough read it in all at once */
#ifdef WIN3
            bytes_read+=_lread(fp,b,UINT_MAX);
#else
        bytes_read=fread(buffer,1,(uint16)size,fp);
#endif
    else {  /* number of bytes to read */
        bytes_read=0;
        b=buffer;
        while(size>UINT_MAX) {
#ifdef WIN3
        bytes_read=_lread(fp,buffer,(uint16)size);
#else
            bytes_read+=fread(b,1,UINT_MAX,fp);
#endif
            b+=UINT_MAX;
            size-=UINT_MAX;
          } /* end while */
        if(size>0)
#ifdef WIN3
            bytes_read+=_lread(fp,b,(uint16)size);
#else
            bytes_read+=fread(b,1,(uint16)size,fp);
#endif
      } /* end else */
    return(bytes_read);
}   /* end HDfreadbig() */

/*--------------------------------------------------------------------------
**
** NAME
**  HDfwritebig -- function specific to the PC to write out 32-bit sized buffers
** USAGE
**  int32 HDfwritebig(buffer,size,fp)
**  VOIDP buffer;       IN: the buffer to get bytes from
**  int32 size;         IN: the number of bytes to write
**  FILE *fp;           IN: the file pointer for the file to write
** RETURNS
**  returns the number of bytes written
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for fwrite, this function
**  blocks that up into 64kb blocks to write to a file.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef WIN3
#ifdef PROTOTYPE
int32 HDfwritebig(VOIDP buffer,int32 size,HFILE fp)
#else
int32 HDfwritebig(buffer,size,fp)
VOIDP buffer;
int32 size;
HFILE fp;
#endif
#else /* !WIN3 */
#ifdef PROTOTYPE
int32 HDfwritebig(VOIDP buffer,int32 size,FILE *fp)
#else
int32 HDfwritebig(buffer,size,fp)
VOIDP buffer;
int32 size;
FILE *fp;
#endif
#endif /* WIN3 */
{
    uint8 *b;              /* alias for the buffer */
    int32 bytes_written;   /* variable to accum. the number of bytes written */

    if(size<=UINT_MAX)  /* if the size is small enough read it in all at once */
#ifdef WIN3
        bytes_written=_lwrite(fp,buffer,(uint16)size);
#else
        bytes_written=fwrite(buffer,1,(uint16)size,fp);
#endif
    else {  /* number of bytes to write */
        bytes_written=0;
        b=buffer;
        while(size>UINT_MAX) {
#ifdef WIN3
            bytes_written+=_lwrite(fp,b,UINT_MAX);
#else
            bytes_written+=fwrite(b,1,UINT_MAX,fp);
#endif
            b+=UINT_MAX;
            size-=UINT_MAX;
          } /* end while */
        if(size>0)
#ifdef WIN3
            bytes_written+=_lwrite(fp,b,(uint16)size);
#else
            bytes_written+=fwrite(b,1,(uint16)size,fp);
#endif
      } /* end else */
    return(bytes_written);
}   /* end HDfwritebig() */
#endif

/*--------------------------------------------------------------------------
 HIget_file_slot

 Searches the file record array for a matching record, or an empty slot.
 The file is considered the same if the path matches exactly.  This
 routine is unable to detect aliases, or how to compare relative and
 absolute paths.

 Error occurred is charged to the calling function.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE int HIget_file_slot(char *path, char *FUNC)
#else
PRIVATE int HIget_file_slot(path, FUNC)
    char *path;                /* file path */
    char *FUNC;                /* Error is charged to calling function */
#endif
{
    int i;
    int slot;

#if defined(macintosh) | defined(THINK_C)

    if (!file_records) {
        /* The array has not been allocated.  Allocating file records
          dynamically. */

       file_records = (filerec_t *) HDgetspace((uint32)MAX_FILE * sizeof(filerec_t));
       if (!file_records) {
           HERROR(DFE_NOSPACE);
           return FAIL;
       }

       /* Initialize file records. */

       for (i = 0; i < MAX_FILE; i++) {
           file_records[i].path = (char *) NULL;
           file_records[i].ddhead = (ddblock_t *) NULL;
           file_records[i].refcount = 0;
       }

       /* Use the first slot. */

       file_records[0].version_set = FALSE;

       file_records[0].path = HDgetspace(HDstrlen(path)+1);
       if(file_records[0].path)
           HDstrcpy(file_records[0].path,path);
       return file_records[0].path ? 0 : FAIL;
    }

#endif /* macintosh or THINK_C */

    /* Search for a matching or free slot. */

    slot = FAIL;
    for (i = 0; i < MAX_FILE; i++) {

        /* If there already is an active FID for this file return it
           thus, there will only ever be one FID per open file.
           This is a BUG if you want to open the file twice with
           different access privs each time. */

       if (file_records[i].path && STREQ(file_records[i].path, path))
           return i;

       /* Otherwise, record first free slot. */

       if (!file_records[i].refcount && slot == FAIL) {
           slot = i;
           file_records[i].path = (char *) NULL;
           file_records[i].ddhead = (ddblock_t *) NULL;
       }

    }

    if (slot == FAIL) {     /* No matching or free slot. */
        HERROR(DFE_FNF);
        return FAIL;
      } /* end if */

    /* Fill empty slot with data. */

    file_records[slot].version_set = FALSE;

    if (file_records[slot].path) HDfreespace(file_records[slot].path);
    file_records[slot].path = (char *)HDstrdup(path);
    return file_records[slot].path ? slot : FAIL;
}

/*--------------------------------------------------------------------------
 HIvalid_magic

 Checks the magic cookie at the beginning of an opened file to see
 if the file is a valid HDF file.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE bool HIvalid_magic(hdf_file_t file, char *FUNC)
#else
PRIVATE bool HIvalid_magic(file, FUNC)
    hdf_file_t file;               /* File handle. */
    char *FUNC;                        /* Charge error to calling function. */
#endif
{
    char b[MAGICLEN];          /* Temporary buffer */

    /* Seek to beginning of the file. */

    if (HI_SEEK(file, 0) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FALSE;
    }

    /* Read in magic cookie and compare. */

    if (HI_READ(file, b, MAGICLEN) == FAIL) {
       HERROR(DFE_READERROR);
       return FALSE;
    }
    if (NSTREQ(b, HDFMAGIC, MAGICLEN)) return TRUE;
    else return FALSE;
}


/*--------------------------------------------------------------------------
 HIget_access_slot

 get a free access record slot
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int HIget_access_slot(void)
#else
int HIget_access_slot()
#endif
{
    int i;                     /* temp index */

    /* access records not allocated yet.
       Allocate dynamically and initialize*/

    if (!access_records) {
       access_records = (accrec_t *) HDgetspace(MAX_ACC * sizeof(accrec_t));
       if (!access_records)    return FAIL;
       for (i = 0; i < MAX_ACC; i++)
           access_records[i].used = FALSE;

       /* use the first record */

       access_records[0].used = TRUE;
       return 0;
    }

    /* return the first unused record */

    for (i = 0; i < MAX_ACC; i++)
       if (!access_records[i].used) {
           access_records[i].used = TRUE;
           return i;
       }

    return FAIL;
} /* HIget_access_slot */

/*--------------------------------------------------------------------------
 HInew_dd_block

 create new ddblock
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int HInew_dd_block(filerec_t *file_rec, int16 ndds, char *FUNC)
#else
int HInew_dd_block(file_rec, ndds, FUNC)
    filerec_t *file_rec;       /* file record */
    int16 ndds;                /* number of dd's to create in this ddblock */
    char *FUNC;                        /* function that this was called from */
#endif
{
    ddblock_t *block;          /* current dd block */
    int32 nextoffset;          /* offset of new ddblock */
    int32 offset;              /* offset to the offset of new ddblock */
    uint8 *p;                  /* temp buffer ptr */
    dd_t *list;                /* dd list array of new dd block */
    int i;                     /* temp integers */
    int16 n;

    /* check integrity of file record */

    if (!file_rec->ddhead || !file_rec->ddlast) {
       HERROR(DFE_INTERNAL);
       return FAIL;
    }

    /* allocate new dd block record and fill in data */

    block = (ddblock_t *) HDgetspace(sizeof(ddblock_t));
    if (block == (ddblock_t *) NULL) {
       HERROR(DFE_NOSPACE);
       return FAIL;
    }
    block->ndds = ndds;
    block->next = (ddblock_t *) NULL;
    block->nextoffset = 0;

    /* put the new dd block at the end of the file */

    if (HI_SEEKEND(file_rec->file) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    nextoffset = HI_TELL(file_rec->file);
    p = tbuf;
    INT16ENCODE(p, block->ndds);
    INT32ENCODE(p, (int32)0);
    if (HI_WRITE(file_rec->file, tbuf, NDDS_SZ+OFFSET_SZ) == FAIL) {
       HERROR(DFE_WRITEERROR);
       return FAIL;
    }

    /* set up the dd list of this dd block and put it in the file
       after the dd block header */

    p = tbuf;
    list = block->ddlist = (dd_t *) HDgetspace((uint32) ndds * sizeof(dd_t));
    if (list == (dd_t *) NULL) {
       HERROR(DFE_NOSPACE);
       return FAIL;
    }
    for (i = 0; i < ndds; i++) {
       list[i].tag = DFTAG_NULL;
       list[i].ref = 0;
       list[i].length = list[i].offset = 0;
    }

    /* n is the number of dds that could fit into tbuf at one time */

    n = sizeof(int_tbuf) / DD_SZ;
    if (n > ndds) n = ndds;
    for (i = 0; i < n; i++) {
       UINT16ENCODE(p, (uint16)DFTAG_NULL);
       UINT16ENCODE(p, (uint16)0);
       INT32ENCODE(p, (int32)0);
       INT32ENCODE(p, (int32)0);
    }
    while (ndds > 0) {
       if (HI_WRITE(file_rec->file, tbuf, n*DD_SZ) == FAIL) {
           HERROR(DFE_WRITEERROR);
           return FAIL;
       }
       ndds -= n;
       if (n > ndds) n = ndds;
    }

    /* update previously last ddblock to point to this new dd block */

    file_rec->ddlast->nextoffset = nextoffset;
    block->prev = file_rec->ddlast;
    file_rec->ddlast->next = block;
    if (file_rec->ddhead == file_rec->ddlast) {
       offset = MAGICLEN + NDDS_SZ;
    } else {
       offset = file_rec->ddlast->prev->nextoffset + NDDS_SZ;
    }
    p = tbuf;
    INT32ENCODE(p, nextoffset);
    if (HI_SEEK(file_rec->file, offset) == FAIL) {
       HERROR(DFE_SEEKERROR);
       return FAIL;
    }
    if (HI_WRITE(file_rec->file, tbuf, OFFSET_SZ) == FAIL) {
       HERROR(DFE_WRITEERROR);
       return FAIL;
    }

    /* update file record */
    file_rec->ddlast = file_rec->ddlast->next;

    return SUCCEED;
} /* HInew_dd_block */

/*--------------------------------------------------------------------------
 HIfill_file_rec

 Fill in a file record with data from the file, especially
 the data descriptors.
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE int HIfill_file_rec(filerec_t *file_rec, char *FUNC)
#else
PRIVATE int HIfill_file_rec(file_rec, FUNC)
    filerec_t *file_rec;       /* File record */
    char *FUNC;                        /* Charge error to calling function. */
#endif
{
  uint8 *p;               /* Temporary pointer. */
  int32 n;
  register intn ndds, i, idx;     /* Temporary integers. */

  /* Alloc start of linked list of ddblocks. */
  
  file_rec->ddhead = (ddblock_t *) HDgetspace(sizeof(ddblock_t));
  if (file_rec->ddhead == (ddblock_t *) NULL) {
    HERROR(DFE_NOSPACE);
    return FAIL;
  }
  
  /* Only one elt in linked list so head is also last. */
  
  file_rec->ddlast = file_rec->ddhead;
  file_rec->ddlast->next = (ddblock_t *) NULL;
  file_rec->ddlast->prev = (ddblock_t *) NULL;
  
  /* The first ddblock always starts after the magic number.
     Set it up so that we start reading from there. */
  
  if (HI_SEEK(file_rec->file, MAGICLEN) == FAIL) {
    HERROR(DFE_SEEKERROR);
    return FAIL;
  }

  /* Blank out hash table */
  HDmemset(file_rec->hash, 0, sizeof(tag_ref_list_ptr) * (HASH_MASK+1));

  /* Read in the dd's one at a time and determine the max ref in the file
     at the same time. */
  
  file_rec->maxref = 0;
  for (;;) {
    
    /* Read in the start of this dd block.
       Read data consists of ndds (number of dd's in this block) and
       offset (offset to the next ddblock). */
    
    if (HI_READ(file_rec->file, tbuf, NDDS_SZ+OFFSET_SZ) == FAIL) {
      HERROR(DFE_READERROR);
      return FAIL;
    }
    
    /* Decode the numbers. */
    
    p = tbuf;
    INT16DECODE(p, FILE_NDDS(file_rec));
    if (FILE_NDDS(file_rec) <= 0) {
      /* validity check */
      
      HERROR(DFE_CORRUPT);
      return FAIL;
    }
    INT32DECODE(p, file_rec->ddlast->nextoffset);
    
    /* Now that we know how many dd's are in this block,
       alloc memory for the records. */
    
    file_rec->ddlast->ddlist =
      (dd_t *) HDgetspace((uint32)FILE_NDDS(file_rec) * sizeof(dd_t));
    if (!file_rec->ddlast->ddlist) {
      HERROR(DFE_NOSPACE);
      return FAIL;
    }
    
    /* Read in dd's. */
    
    ndds = FILE_NDDS(file_rec);
    
    /* Since the tbuf might not be large enough to read in all the dd's
       at once, we try to read in chunks as large as possible. */
    
    /* n is number of dd's that could fit into tbuf at one time */
    
    n = sizeof(int_tbuf) / DD_SZ;
    if (n > ndds)
      n = ndds;
    
    /* Index of current dd in ddlist of this ddblock is 0. */
    
    idx = 0;
    
    while (ndds > 0) {
      /* ndds is the remaining number of dd's
         to be read in this block. */
      
      /* Read in a chunk of dd's from the file. */
      
      if (HI_READ(file_rec->file, tbuf, n * DD_SZ) == FAIL)
        HRETURN_ERROR(DFE_READERROR, FAIL);

      /* decode the dd's */
      
      p = tbuf;
      for (i = 0; i < n; i++, idx++) {
        UINT16DECODE(p, file_rec->ddlast->ddlist[idx].tag);
        UINT16DECODE(p, file_rec->ddlast->ddlist[idx].ref);
        INT32DECODE(p, file_rec->ddlast->ddlist[idx].offset);
        INT32DECODE(p, file_rec->ddlast->ddlist[idx].length);
        if (file_rec->maxref < file_rec->ddlast->ddlist[idx].ref)
          file_rec->maxref = file_rec->ddlast->ddlist[idx].ref;
        
        if(HIadd_hash_dd(file_rec, 
                         file_rec->ddlast->ddlist[idx].tag, 
                         file_rec->ddlast->ddlist[idx].ref, 
                         file_rec->ddlast, 
                         idx) == FAIL)
          return FAIL;

      }
      
      /* number of remaining dd's in this ddblock */
      
      ndds -= (intn)n;
      if (n > ndds)
        n = ndds;
    }
    
    if (file_rec->ddlast->nextoffset != 0) {
      /* More ddblocks in the file */
      
      /* extend the linked list */
      
      file_rec->ddlast->next = (ddblock_t *) HDgetspace((uint32)sizeof(ddblock_t));
      if (file_rec->ddlast->next == (ddblock_t *) NULL)
        HRETURN_ERROR(DFE_NOSPACE, FAIL);
      
      /* set up the file so next read will be at the next ddblock */
      
      if (HI_SEEK(file_rec->file, file_rec->ddlast->nextoffset) == FAIL)
        HRETURN_ERROR(DFE_SEEKERROR, FAIL);
      
      file_rec->ddlast->next->prev = file_rec->ddlast;
      file_rec->ddlast = file_rec->ddlast->next;
      file_rec->ddlast->next = (ddblock_t *) NULL;
      file_rec->ddlast->ddlist = (dd_t *) NULL;
      
    } else
      break;
  }

  return SUCCEED;
}


/*--------------------------------------------------------------------------
** PRIVATE	PRIVATE		PRIVATE		PRIVATE		PRIVATE
** NAME
**	HIupdate_version -- determine whether new version tag should be written
** USAGE
**	int HIupdate_version(file_id)
**	int32 file_id;		IN: handle of file
** RETURNS
**	returns SUCCEED (0) if successful and FAIL (-1) if failed.
** DESCRIPTION
**	Writes out version numbers of current library as file version.
** GLOBAL VARIABLES
**	Resets modified field of version field of appropriate file_records[]
**	entry.
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE int HIupdate_version(int32 file_id)
#else
PRIVATE int HIupdate_version(file_id)
int32 file_id;
#endif
{
    /*
    uint32 lmajorv, lminorv, lrelease;
    */
    uint8 /*lstring[81],*/ lversion[LIBVER_LEN];
    filerec_t *file_rec;
    int ret;
    char *FUNC="Hupdate_version";


    HEclear();

    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }

    /* copy in-memory version to file */
    Hgetlibversion(&(file_rec->version.majorv), &(file_rec->version.minorv),
    		   &(file_rec->version.release), file_rec->version.string);

    {
	uint8 *p;

	p = lversion;
	UINT32ENCODE(p, file_rec->version.majorv);
	UINT32ENCODE(p, file_rec->version.minorv);
	UINT32ENCODE(p, file_rec->version.release);
        HIstrncpy((char*) p, file_rec->version.string, 80);
    }

    ret = Hputelement(file_id, (uint16)DFTAG_VERSION, (uint16)1, lversion,
		      (uint32)sizeof(lversion));

    if (ret == SUCCEED) {
        file_rec->version.modified = 0;
        return(SUCCEED);
    } else {
        HERROR(DFE_INTERNAL);
        return(FAIL);
    }
}

/*--------------------------------------------------------------------------
** PRIVATE	PRIVATE		PRIVATE		PRIVATE		PRIVATE
** NAME
**	HIread_version -- reads a version tag from a file
** USAGE
**	int HIread_version(file_id)
**	int32 file_id;		IN: handle of file
** RETURNS
**	returns SUCCEED (0) if successful and FAIL (-1) if failed.
** DESCRIPTION
**	Reads a version tag from the specified file into the version fields
**	of the appropriate filerec_t.  On failure, zeros are put in the version
**	number fields and NULLS in the string.
** GLOBAL VARIABLES
**	Writes to version fields of appropriate file_records[] entry.
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
PRIVATE int HIread_version(int32 file_id)
#else
PRIVATE int HIread_version(file_id)
int32 file_id;
#endif
{
    filerec_t *file_rec;
    uint8 fversion[LIBVER_LEN];
    char *FUNC="Hread_version";


    HEclear();

    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }

    if (Hgetelement(file_id, (uint16)DFTAG_VERSION,
                              (uint16)1, fversion) == FAIL) {
        file_rec->version.majorv = 0;
        file_rec->version.minorv = 0;
        file_rec->version.release = 0;
        HDstrcpy(file_rec->version.string, "");
        file_rec->version.modified = 0;
        HERROR(DFE_INTERNAL);
        return(FAIL);
    } else {
        uint8 *p;

        p = fversion;
        UINT32DECODE(p, file_rec->version.majorv);
        UINT32DECODE(p, file_rec->version.minorv);
        UINT32DECODE(p, file_rec->version.release);
        HIstrncpy(file_rec->version.string, (char*) p, 80);
    }
    file_rec->version.modified = 0;

    return(SUCCEED);
}

/* end version tags */

/* ----------------------------- Hfidinquire ----------------------------- */
/*
** NAME
**	Hfidinquire --- Inquire about a file ID
** USAGE
**	int Hfidinquire(file_id)
**	int32 file_id;		IN: handle of file
**      char  *path;            OUT: path of file
**      int32 mode;             OUT: mode file is opened with 
** RETURNS
**	returns SUCCEED (0) if successful and FAIL (-1) if failed.
** DESCRIPTION
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hfidinquire(int32 file_id, char **fname, intn *access, intn *attach)
#else
intn Hfidinquire(file_id, fname, access, attach)
int32 file_id;
char  **fname;
intn  *access;
intn  *attach;
#endif
{
    filerec_t *file_rec;
    char *FUNC="Hfidinquire";

    HEclear();

    file_rec = FID2REC(file_id);
    if (!file_rec || file_rec->refcount == 0) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }

    *fname  = file_rec->path;
    *access = file_rec->access;
    *attach = file_rec->attach;

    return SUCCEED;

} /* Hfidinquire */

#if defined(MAC) & !defined(THINK_C)
/*
*  Macintosh file stubs for HDF
*
*  Implement a subset of the C unbuffered file I/O stubs in the
*  Mac toolbox.
*/

#include "Errors.h"
#ifdef MPW
#include "Strings.h"
#endif

static int32 hdfc = 1061109567L;    /* equal to '????' in ascii */
static int32 hdft = 1600085855L;    /* equal to '_HDF' in ascii */

#ifdef MPW
hdf_file_t
mopen(char *name, intn flags)
{
    hdf_file_t volref,rn;
    OSErr result;
    FInfo fndrInfo;

    GetVol(NULL,&volref);
    
    if (flags == DFACC_CREATE)   { /* we need to create it */
        
        result = getfinfo(name, volref, &fndrInfo);
        if (result != fnfErr)
            if( noErr != (result = fsdelete(name, volref)))
                return FAIL;
        
        if (noErr != (result = create(name, volref, hdfc, hdft)))
            return FAIL;
        
    }
    
    if (noErr != (result = fsopen(name, volref, &rn)))
        return FAIL;
    
    if (flags & O_CREAT)    /* and truncate it */
        SetEOF(rn, 0);
    
    return(rn);  
}
#else

static Str255 pname;

hdf_file_t
mopen(char *name, intn flags)
{
    hdf_file_t volref,rn;
    OSErr result;
    FInfo fndrInfo;

    strcpy((char *) pname, (char *) name);
    CtoPstr(pname);

    result = GetVol(NULL,&volref);
    
    if (flags == DFACC_CREATE)   { /* we need to create it */
        
        result = GetFInfo(name, volref, &fndrInfo);
        if (result != fnfErr)
            if( noErr != (result = FSDelete(pname, volref)))
                return FAIL;
        
        if (noErr != (result = Create(pname, volref, hdfc, hdft)))
            return FAIL;
        
    }
    
    if (noErr != (result = FSOpen(pname, volref, &rn)))
        return FAIL;
    
    if (flags & O_CREAT)    /* and truncate it */
        SetEOF(rn, 0L);
    
    return(rn);  
}

#endif

int32
mclose(hdf_file_t rn)
{
        return(FSClose(rn));
}

int32 
mread(hdf_file_t rn, char *buf, int32 n)
{
	OSErr result;

        if (noErr != (result = FSRead( rn, &n, buf)))
                return(FAIL);

        return(n);
}

int32
mwrite(hdf_file_t rn, char *buf, int32 n)
{
    OSErr result;

    if (noErr != (result = FSWrite( rn, &n, buf)))
        return(FAIL);
    
    return(n);
}

int32
mlseek(hdf_file_t rn, int32 n, intn m)
{
    OSErr result;
    int32 newEOF;

#ifdef OLD_EXTD
    long pos, oldpos, logEOF;
    Ptr buffy;
#endif
    
    switch(m) {
    case 0:
    default:
        m = fsFromStart;
        break;
    case 1:
        m = fsFromMark;
        break;
    case 2:
        m = fsFromLEOF;
        break;
    }
    
    if (noErr != (result = SetFPos(rn, m, n)))
        {
            if(result == eofErr)
                {
#ifdef OLD_EXTD
                    if(noErr != (result = GetEOF(rn, &logEOF)))
                        return FAIL;

                    oldpos = pos = n - logEOF;
                    if(NULL == (buffy = NewPtr((Size) pos)))
                        return FAIL;
                    if (noErr != (result = FSWrite(rn, &pos, buffy)))
                        {
                            DisposPtr(buffy);
                            return FAIL;
                        }
                    DisposPtr(buffy);
#else


                    if(m != fsFromStart)
                        return FAIL;

                    newEOF = n;
                    if(noErr != (result = SetEOF(rn, newEOF)))
                        return FAIL;    
                    
#endif                     
                    
                    if (noErr != (result = SetFPos(rn, fsFromStart, n)))
                        return FAIL;
                }
            else return FAIL;
        }
    
    if (noErr != (result = GetFPos(rn, &n)))
        return FAIL;
    
    if (m == fsFromMark) {
        return(n);
    } else {
        return(SUCCEED);
    }

}


#endif /* MAC */

