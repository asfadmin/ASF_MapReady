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

/*+ hfile.h
*** Header for hfile.c, routines for low level data element I/O
+*/

#ifndef HFILE_H
#define HFILE_H

/* maximum number of files (number of slots for file records) */

#ifndef MAX_FILE
#ifdef PC
#   define MAX_FILE 8
#else
#   define MAX_FILE 16
#endif
#endif

/* maximum number of access elements */

#ifndef MAX_ACC
#   define MAX_ACC 256
#endif

/* Magic cookie for HDF data files */

#define MAGICLEN 4             /* length */
#define HDFMAGIC "\016\003\023\001" /* ^N^C^S^A */

/* version tags */
/* Library version numbers */

#define LIBVER_MAJOR	3
#define LIBVER_MINOR    3
#define LIBVER_RELEASE	0
#define LIBVER_STRING   "NCSA HDF Version 3.3 Release 1, September 1993"
#define LIBVER_LEN	92	/* 4+4+4+80 = 92 */
/* end of version tags */

/* FILELIB -- file library to use for file access: 1 stdio, 2 fcntl
   default to stdio library */

#ifndef FILELIB
#   define FILELIB UNIXBUFIO /* UNIX buffered I/O is the default */
#endif

#if (FILELIB == UNIXBUFIO)
/* using C buffered file I/O routines to access files */
typedef FILE *hdf_file_t;
#ifdef VMS
/* For VMS, use "mbc=64" to improve performance     */
#   define HI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                 fopen((p), "r+", "mbc=64") : fopen((p), "r", "mbc=64"))
#else  /*  !VMS  */
#   define HI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                        fopen((p), "r+") : fopen((p), "r"))
#endif
#ifdef PC386
#   define HI_CREATE(p)        (fopen((p), "wb+"))
#else /* PC386 */
#   define HI_CREATE(p)        (fopen((p), "w+"))
#endif  /* PC386 */
#   define HI_READ(f, b, n)    (((n) == fread((b), 1, (n), (f))) ? \
                                SUCCEED : FAIL)
#   define HI_WRITE(f, b, n)   (((n) == fwrite((b), 1, (n), (f))) ? \
                                SUCCEED : FAIL)
#   define HI_CLOSE(f) (fclose(f))
#   define HI_FLUSH(f) (fflush(f)==0 ? SUCCEED : FAIL)
#   define HI_SEEK(f, o)       (fseek((f), (long)(o), SEEK_SET))
#   define HI_SEEKEND(f) (fseek((f), (long)0, SEEK_END))
#   define HI_TELL(f)  (ftell(f))
#   define OPENERR(f)  ((f) == (FILE *)NULL)
#endif /* FILELIB == UNIXBUFIO */

#if (FILELIB == UNIXUNBUFIO)
/* using UNIX unbuffered file I/O routines to access files */
typedef int hdf_file_t;
#   define HI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                        open((p), O_RDWR) : open((p), O_RDONLY))
#   define HI_CREATE(p)        (open((p), O_RDWR | O_CREAT | O_TRUNC))
#   define HI_CLOSE(f) (close(f))
#   define HI_FLUSH(f) (SUCCEED)
#   define HI_READ(f, b, n)    (read((f), (char *)(b), (n)))
#   define HI_WRITE(f, b, n)   (write((f), (char *)(b), (n)))
#   define HI_SEEK(f, o)       (lseek((f), (off_t)(o), SEEK_SET))
#   define HI_SEEKEND(f) (lseek((f), (off_t)0, SEEK_END))
#   define HI_TELL(f)  (lseek((f), (off_t)0, SEEK_CUR))
#   define OPENERR(f)  (f < 0)
#endif /* FILELIB == UNIXUNBUFIO */

#if (FILELIB == MACIO)
/* using special routines to redirect to Mac Toolkit I/O */
typedef short hdf_file_t;
#   define HI_OPEN(x,y) mopen(x,y)
#   define HI_CREATE(name) mopen(name, DFACC_CREATE)
#   define HI_CLOSE(x) mclose(x)
#   define HI_FLUSH(a) (SUCCEED)
#   define HI_READ(a,b,c) mread(a, (char *) b, (int32) c)
#   define HI_WRITE(a,b,c) mwrite(a, (char *) b, (int32) c)
#   define HI_SEEK(x,y) mlseek(x, (int32 )y, 0)
#   define HI_SEEKEND(x) mlseek(x, 0L, 2)
#   define HI_TELL(x) mlseek(x,0L,1)
#   define DF_OPENERR(f)	((f) == -1)
#   define OPENERR(f)  (f < 0)
#endif /* FILELIB == MACIO */

#if (FILELIB == PCIO)
/* using special PC functions to enable reading/writing large chunks */
typedef FILE *hdf_file_t;
#   define HI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                        fopen((p), "rb+") : fopen((p), "rb"))
#   define HI_CREATE(p)        (fopen((p), "wb+"))
/* Alias the HI_READ and HI_WRITE macros to functions which can handle */
/*  32-bits of data to read/write */
#   define HI_READ(f, b, n)    (((int32)(n) == HDfreadbig((b), (n), (f))) ? \
                                SUCCEED : FAIL)
#   define HI_WRITE(f, b, n)   (((int32)(n) == HDfwritebig((b), (n), (f))) ? \
                                SUCCEED : FAIL)
#   define HI_CLOSE(f) (fclose(f))
#   define HI_FLUSH(f) (fflush(f)==0 ? SUCCEED : FAIL)
#   define HI_SEEK(f, o)       (fseek((f), (long)(o), SEEK_SET))
#   define HI_SEEKEND(f) (fseek((f), (long)0, SEEK_END))
#   define HI_TELL(f)  (ftell(f))
#   define OPENERR(f)  ((f) == (FILE *)NULL)
#endif /* FILELIB == PCIO */

#if (FILELIB == WINIO)
/* using special MS Windows functions to enable reading/writing large chunks */
typedef HFILE hdf_file_t;
#   define HI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                        _lopen((p), READ_WRITE) : _lopen((p), READ))
#   define HI_CREATE(p)        (_lcreat((p), 0))
/* Alias the HI_READ and HI_WRITE macros to functions which can handle */
/*  32-bits of data to read/write */
#   define HI_READ(f, b, n)    (((int32)(n) == HDfreadbig((b), (n), (f))) ? \
                                SUCCEED : FAIL)
#   define HI_WRITE(f, b, n)   (((int32)(n) == HDfwritebig((b), (n), (f))) ? \
                                SUCCEED : FAIL)
#   define HI_CLOSE(f) (_lclose(f))
#   define HI_FLUSH(f) (0)
#   define HI_SEEK(f, o)       (_llseek((f), (long)(o), SEEK_SET))
#   define HI_SEEKEND(f) (_llseek((f), (long)0, SEEK_END))
#   define HI_TELL(f)  (_llseek((f),0l,SEEK_CUR))
#   define OPENERR(f)  ((f) == (HFILE)HFILE_ERROR)
#endif /* FILELIB == WINIO */

/* The internal structure used to keep track of the files opened: an
   array of filerec_t structures, each has a linked list of ddblock_t.
   Each ddblock_t struct points to an array of dd_t structs. */

/* record of each data decsriptor */

typedef struct dd_t {
    uint16 tag;
    uint16 ref;
    int32 length;
    int32 offset;
} dd_t;

/* version tags */
typedef struct version_t {
    uint32 majorv;		/* major version number */
    uint32 minorv;		/* minor version number */
    uint32 release;		/* release number */
    char string[81];		/* optional text description */
    int16 modified;		/* indicates file was modified */
} version_t;

/* record of a block of data descriptors, mirrors structure of a HDF file.  */
typedef struct ddblock_t {
    int16 ndds;                /* number of dd's in this block */
    int32 nextoffset;          /* offset to the next ddblock in the file */
    struct ddblock_t *next;    /* pointer to the next ddblock in memory */
    struct ddblock_t *prev;    /* Pointer to previous ddblock. */
    struct dd_t *ddlist;       /* pointer to array of dd's */
} ddblock_t;

/* hashing information */
#define HASH_MASK       0xff
#define HASH_BLOCK_SIZE 100
typedef struct tag_ref_str {
  intn        tag;              /* tag for this element */
  intn        ref;              /* ref for this element */
  ddblock_t   *pblock;          /* ddblock this object is in */
  int32       pidx;             /* this object's index into dd block */
} tag_ref, *tag_ref_ptr;

typedef struct tag_ref_list_str {
    int         count;                      /* number of objects */
    tag_ref     objects[HASH_BLOCK_SIZE];   /* DDs */
    struct tag_ref_list_str *next;  /* next one in the list */
} tag_ref_list, *tag_ref_list_ptr;


typedef struct filerec_t {
    char *path;                 /* name of file */
    hdf_file_t file;            /* either file descriptor or pointer */
    intn access;                /* access mode */
    intn refcount;              /* reference count / times opened */
    struct ddblock_t *ddhead;   /* head of ddblock list */
    struct ddblock_t *ddlast;   /* end of ddblock list */
    uint16 maxref;              /* highest ref in this file */
    intn attach;                /* number of access elts attached */

    /* version tag stuff */
    intn version_set;
    version_t version;		/* file version info */
    
    /* fast lookup of empty dd stuff */
    struct ddblock_t *null_block; /* last block a NULL entry was found in */
    int32             null_idx;   /* index into null_block of NULL entry */

    /* hash table stuff */
    tag_ref_list_ptr hash[HASH_MASK + 1];  /* hashed table of tag / refs */

} filerec_t;

/* Each access element is associated with a tag/ref to keep track of
   the dd it is pointing at.  To facilitate searching for next dd's,
   instead of pointing directly to the dd, we point to the ddblock and
   index into the ddlist of that ddblock. */

typedef struct accrec_t {
    int32 file_id;              /* id of attached file */
    struct ddblock_t *block;    /* ptr to ddblock that contains dd */
    int32 idx;                  /* index of dd into *block */
    int32 posn;                 /* seek position with respect to */
                                /* start of element */
    int16 access;               /* access codes */
    intn used;                  /* whether the access record is used */
    bool appendable;            /* whether appends to the data are allowed */
    bool flush;                 /* whether the DD for this data should be flushed */
                                /* when Hendaccess() is called */
    int16 special;
    VOIDP special_info;
    int32 (**special_func)();
} accrec_t;

/* a function table record for accessing special data elements.
   special data elements of a key could be accessed through the list
   of functions in array pointed to by tab. */

typedef struct functab_t {
    int16 key;                 /* the key for this type of special elt */
    int32 (**tab)();           /* table of accessing functions */
} functab_t;

/* sizes of elements in a file.  This is necessary because
   the size of variables need not be the same as in the file
   (cannot use sizeof) */

#define DD_SZ 12               /* 2+2+4+4 */
#define NDDS_SZ 2
#define OFFSET_SZ 4

/* ndds (number of dd's in a block) default,
   so user need not specify */

#ifndef DEF_NDDS
#   define DEF_NDDS 16
#endif

/* ndds minimum, to prevent excessive overhead of very small dd-blocks */

#ifndef MIN_NDDS
#   define MIN_NDDS 4
#endif

/* largest number that will fit into 16-bit word ref variable */

#define MAX_REF ((uint16)32767)

/* indices for special function table: the list of accessing functions
   are ordered so that the functions are always in the same position
   in a table */

#define SP_STREAD 0
#define SP_STWRITE 1
#define SP_SEEK 2
#define SP_INQUIRE 3
#define SP_READ 4
#define SP_WRITE 5
#define SP_END 6

/* macros */

/* each id, what ever the type, will be represented with a 32-bit word,
   the most-significant 16 bits is a number unique for type.  The less-
   significant 16 bits is an id unique to each type; in this, we use the
   internal slot number. */

#define FIDTYPE   1
#define AIDTYPE   2
#define GROUPTYPE 3
#define SDSTYPE   4
#define DIMTYPE   5
#define CDFTYPE   6
#define VGIDTYPE  8         /* also defined in vg.h for Vgroups */
#define VSIDTYPE  9         /* also defined in vg.h for Vsets */
#define BITTYPE   10        /* For bit-accesses */
#define FSLOT2ID(s) ((((uint32)FIDTYPE & 0xffff) << 16) | ((s) & 0xffff))
#define VALIDFID(i) (((((uint32)(i) >> 16) & 0xffff) == FIDTYPE) && \
                    (((uint32)(i) & 0xffff) < MAX_FILE))
#define FID2SLOT(i) (VALIDFID(i) ? (uint32)(i) & 0xffff : -1)
#define FID2REC(i) ((VALIDFID(i) ? &(file_records[(uint32)(i) & 0xffff]) : NULL))
#define ASLOT2ID(s) ((((uint32)AIDTYPE & 0xffff) << 16) | ((s) & 0xffff))
#define VALIDAID(i) (((((uint32)(i) >> 16) & 0xffff) == AIDTYPE) && \
                    (((uint32)(i) & 0xffff) < MAX_ACC) && \
                    (access_records))
#define AID2SLOT(i) (VALIDAID(i) ? (uint32)(i) & 0xffff : -1)
#define AID2REC(i) ((VALIDAID(i) ? &(access_records[(uint32)(i) & 0xffff]) : NULL))

#define NO_ID     (uint32) 0

/* a tag is special if its tag belongs to a special set.  This test can be
   just ((t)==SPECIAL1 || (t)==SPECIAL2), or a full blown function.
   right now, no special tags yet */

#define BASETAG(t) (HDbase_tag(t))
#define SPECIALTAG(t) (HDis_special_tag(t))
#define MKSPECIALTAG(t) (HDmake_special_tag(t))

/* access records array.  defined in hfile.c */

extern accrec_t *access_records;

/* file records array.  defined in hfile.c */

#if defined(macintosh) | defined(THINK_C)
extern filerec_t *file_records;
#else /* !macintosh */
extern filerec_t file_records[];
#endif /* !macintosh */

/* */
#define FILE_NDDS(file_rec) ((file_rec)->ddlast->ndds)


/* 
** Functions to get information of special elt from other access records.
**   defined in hfile.c 
** These should really be HD... routines, but they are only used within
**   the H-layer...
*/

extern int HIget_access_slot
  PROTO((void));

extern int HIfind_dd
  PROTO((uint16 look_tag, uint16 look_ref, ddblock_t **pblock, int32 *pidx,
        intn direction));

extern int HInew_dd_block
  PROTO((filerec_t *file_rec, int16 ndds, char *FUNC));

extern int HIupdate_dd
  PROTO((filerec_t *file_rec, ddblock_t *block, int32 idx, char *FUNC));

extern VOIDP HIgetspinfo
  PROTO((accrec_t *access_rec, uint16 tag, uint16 ref));

extern int HIlookup_dd
  PROTO((filerec_t *file_rec, uint16 look_tag, uint16 look_ref,
         ddblock_t **pblock, int32 *pidx));

extern int HIadd_hash_dd
  PROTO((filerec_t *file_rec, uint16 look_tag, uint16 look_ref,
         ddblock_t *pblock, int32 pidx));

extern int HIdel_hash_dd
  PROTO((filerec_t *file_rec, uint16 look_tag, uint16 look_ref));

extern int32 HXIcloseAID
    PROTO((accrec_t *access_rec));
	
#ifdef MAC
extern hdf_file_t mopen
	PROTO((char * filename, intn access));
	
extern int32 mclose
	PROTO((hdf_file_t rn));
	
extern int32 mlseek
	PROTO((hdf_file_t rn, int32 n, intn m));

extern int32 mread
    PROTO((hdf_file_t rn, char *buf, int32 n));

extern int32 mwrite
    PROTO((hdf_file_t rn, char *buf, int32 n));

#endif

#endif /* HFILE_H */

