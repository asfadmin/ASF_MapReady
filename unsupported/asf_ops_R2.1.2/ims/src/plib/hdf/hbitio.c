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
       hbitio.c
       HDF bit level I/O routines
 REMARKS
        These functions operate on top of the "H" layer routines
        (i.e. they call Hstartread, Hstartwrite, Hread, Hseek, Hwrite, etc.)
        and depend on them for all actual I/O to data elements in the
        file.  This may be somewhat slow, but it prevents having
        to duplicate code for that access.
 EXPORTED ROUTINES
        Hstartbitread - open a dataset for bitfile dataset reading
        Hstartbitwrite - open a dataset for bitfile dataset writing
        Hbitread - read bits from a bitfile dataset
        Hbitwrite - write bits to a bitfile dataset
        Hendbitaccess - close off access to a bitfile dataset
 AUTHOR
       Quincey Koziol
 MODIFICATION HISTORY
    3/15/92     Starting writing
+*/

#define BITMASTER
#include "hdf.h"
#include "herr.h"
#include "hfile.h"
#include "hbitio.h"

/* Local Variables */

/* Array of records of information on each bitfile element.
   These will contain information like how to access the data element,
   where in the data element the current access should start from, etc.
   Allocated dynamically.
   See bitfile.h for definition. */

static struct bitrec_t *bitfile_records = NULL;

/* Local Function Declarations */
#ifdef CONVEX
PRIVATE int HIget_bitfile_slot
    PROTO(());
#else
PRIVATE int HIget_bitfile_slot
    PROTO((VOID));
#endif

/* Actualy Function Definitions */

/*--------------------------------------------------------------------------

 NAME
       Hstartbitread -- locate and position a bit-read access elt on a tag/ref
 USAGE
       int32 Hstartbitread(fileid, tag, ref)
       int fileid;             IN: id of file to attach access element to
       int tag;                IN: tag to search for
       int ref;                IN: ref to search for
 RETURNS
       returns id of bit-access element if successful, otherwise FAIL (-1)
 DESCRIPTION
        Calls Hstartread and initializes bit-level structures.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hstartbitread(int32 file_id, uint16 tag, uint16 ref)
#else
int32 Hstartbitread(file_id, tag, ref)
int32 file_id;          /* file id to read from */
uint16 tag;             /* tag of elt to read */
uint16 ref;             /* ref of elt to read */
#endif
{
    char *FUNC="Hstartbitread";   /* for HERROR */
    int bitslot;        /* slot in bit-access record array */
    int32 aid;          /* Access ID for the bit-level routines to use */
    struct bitrec_t *bitfile_rec;   /* Pointer to the bitfile record */

    /* clear error stack */
    HEclear();

    /* Try to get an AID */
    if((aid=Hstartread(file_id,tag,ref))==FAIL) {
        HERROR(DFE_BADAID);
        return(FAIL);
      } /* end if */

    /* get a slot in the access record array */
    bitslot=HIget_bitfile_slot();
    if (bitslot == FAIL) {
       HERROR(DFE_TOOMANY);
       return FAIL;
    }
    bitfile_rec = &(bitfile_records[bitslot]);

    bitfile_rec->acc_id=aid;
    bitfile_rec->mode='r';
    bitfile_rec->bytez=bitfile_rec->bytea+BITBUF_SIZE;
    bitfile_rec->bytep=bitfile_rec->bytez;  /* set to the end of the buffer to force read */
    bitfile_rec->count=0;

    return SLOT2BITID(bitslot);
}   /* Hstartbitread() */

/*--------------------------------------------------------------------------

 NAME
       Hstartbitwrite -- set up a bit access elt for a write
 USAGE
       int32 Hstartbitwrite(fileid, tag, ref, len)
       int fileid;             IN: id of file to write to
       int tag;                IN: tag to write to
       int ref;                IN: ref to write to
       long length;            IN: the length of the data element (in bytes)
 RETURNS
       returns id of bit access element if successful and FAIL otherwise
 DESCRIPTION
       Set up a bit-write access elt to write out a data element.  Calls
       Hstartwrite for most initialization and just initializes the bit-
       level stuff here.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hstartbitwrite(int32 file_id, uint16 tag, uint16 ref, int32 length)
#else
int32 Hstartbitwrite(file_id, tag, ref, length)
int32 file_id;          /* file id */
uint16 tag;             /* tag of elt to write */
uint16 ref;             /* ref of elt to write */
int32 length;           /* length of elt to write */
#endif
{
    char *FUNC="Hstartbitwrite";  /* for HERROR */
    int bitslot;                  /* free access records array slot */
    bitrec_t *bitfile_rec;      /* access record */
    int32 aid;          /* Access ID for the bit-level routines to use */

    /* clear error stack and check validity of file id */
    HEclear();

    /* Try to get an AID */
    if((aid=Hstartwrite(file_id,tag,ref,length))==FAIL) {
        HERROR(DFE_BADAID);
        return(FAIL);
      } /* end if */

    /* get empty slot in bit-access records */
    if((bitslot = HIget_bitfile_slot())==FAIL) {
        HERROR(DFE_TOOMANY);
        return(FAIL);
      } /* end if */

    bitfile_rec = &(bitfile_records[bitslot]);

    bitfile_rec->acc_id=aid;
    bitfile_rec->mode='w';
    bitfile_rec->bytez=bitfile_rec->bytea+BITBUF_SIZE;
    bitfile_rec->bytep=bitfile_rec->bytea;  /* set to the beginning of the buffer */
    bitfile_rec->count=BITNUM;
    bitfile_rec->bits=0;

    return SLOT2BITID(bitslot);
}   /* end Hstartbitwrite() */

/*--------------------------------------------------------------------------

 NAME
       Hbitwrite -- write a number of bits out to a bit-element
 USAGE
       intn Hbitwrite(bitid, count, data)
       int32 bitid;         IN: id of bit-element to write to
       intn count;          IN: number of bits to write
       uint32 data;         IN: actual data bits to output
                            (bits to output must be in the low bits)
 RETURNS
       the number of bits written for successful write,
       FAIL to indicate failure
 DESCRIPTION
       Write a number of bits out to a bit-element.  This function
       buffers the bits and then writes them out when appropriate
       with Hwrite().
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hbitwrite(int32 bitid, intn count, uint32 data)
#else
intn Hbitwrite(bitid, count, data)
int32 bitid;            /* Bit ID to use when writing out data */
intn count;             /* Number of bits to write */
uint32 data;            /* Actual bits to output */
#endif
{
    char *FUNC="Hbitwrite"; /* for HERROR */
    bitrec_t *bitfile_rec;  /* access record */
    intn orig_count=count;  /* keep track of orig, number of bits to output */

    /* clear error stack and check validity of file id */
    HEclear();

    if(count<=0 || (bitfile_rec = BITID2REC(bitid))==NULL) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */

    /* Check for write access */
    if(bitfile_rec->mode!='w') {
        HERROR(DFE_BADACC);
        return(FAIL);
      } /* end if */

    if(count>32)
        count=32;

    data&=maskl[count];

    /* if the new bits will not fill up a byte, then just */
    /* merge the new bits into the current bits buffer */
    if(count<bitfile_rec->count) {
        bitfile_rec->bits|=data<<(bitfile_rec->count-=count);
        return(orig_count);
      } /* end if */

    /* fill up the current bits buffer and output the byte */
    *(bitfile_rec->bytep)=bitfile_rec->bits|data>>(count-=bitfile_rec->count);
    if(++bitfile_rec->bytep==bitfile_rec->bytez) {
        bitfile_rec->bytep=bitfile_rec->bytea;
        if(Hwrite(bitfile_rec->acc_id,BITBUF_SIZE,bitfile_rec->bytea)==FAIL) {
            HERROR(DFE_WRITEERROR);
            return(FAIL);
          } /* end if */
      } /* end if */

    /* output any and all remaining whole bytes */
    while(count>=BITNUM) {
        *(bitfile_rec->bytep)=data>>(count-=BITNUM);
        if(++bitfile_rec->bytep==bitfile_rec->bytez) {
            bitfile_rec->bytep=bitfile_rec->bytea;
            if(Hwrite(bitfile_rec->acc_id,BITBUF_SIZE,bitfile_rec->bytea)==FAIL) {
                HERROR(DFE_WRITEERROR);
                return(FAIL);
              } /* end if */
          } /* end if */
      } /* end while */

    /* put any remaining bytes into the bits buffer */
    if((bitfile_rec->count=BITNUM-count)>0)
        bitfile_rec->bits=data<<bitfile_rec->count;

    return(orig_count);
}   /* end Hbitwrite() */

/*--------------------------------------------------------------------------

 NAME
       Hbitread -- read a number of bits from a bit-element
 USAGE
       intn Hbitread(bitid, count, data)
       int32 bitid;         IN: id of bit-element to write to
       intn count;          IN: number of bits to write
       uint32 *data;        IN: pointer to the bits to read
                            OUT: points to the bits read in
                            (bits input will be in the low bits)
 RETURNS
       the number of bits read for successful write,
       FAIL to indicate failure
 DESCRIPTION
       Read a number of bits from a bit-element.  This function
       buffers the bits and then reads them when appropriate
       with Hread().
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn Hbitread(int32 bitid, intn count, uint32 *data)
#else
intn Hbitread(bitid, count, data)
int32 bitid;            /* Bit ID to use when writing out data */
intn count;             /* Number of bits to write */
uint32 *data;            /* Actual bits to output */
#endif
{
    char *FUNC="Hbitread"; /* for HERROR */
    bitrec_t *bitfile_rec;  /* access record */
    register uint32 l;
    uint32 b=0;         /* bits to return */
    uint16 orig_count;  /* the original number of bits to read in */
    int32 n;

    /* clear error stack and check validity of file id */
    HEclear();

    if(count<=0 || (bitfile_rec = BITID2REC(bitid))==NULL) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */

    /* Check for write access */
    if(bitfile_rec->mode!='r') {
        HERROR(DFE_BADACC);
        return(FAIL);
      } /* end if */

    if(count>32)    /* truncate the count if it's too large */
        count=32;

    /* if the request can be satisfied with just the */
    /* buffered bits then do the shift and return */
    if(count<=bitfile_rec->count) {
        *data=(bitfile_rec->bits>>(bitfile_rec->count-=count)&maskc[count]);
        return(count);
      } /* end if */

    /* keep track of the original number of bits to read in */
    orig_count=count;

    /* get all the buffered bits into the correct position first */
    if(bitfile_rec->count>0) {
        b=bitfile_rec->bits&maskc[bitfile_rec->count];
        b<<=(count-=bitfile_rec->count);
      } /* end if */
	
    /* bring in as many whole bytes as the request allows */
    while(count>=BITNUM) {
        if(bitfile_rec->bytep==bitfile_rec->bytez) {
            n=Hread(bitfile_rec->acc_id,BITBUF_SIZE,bitfile_rec->bytea);
            if(n==FAIL) {       /* EOF */
                bitfile_rec->count=0;  /* make certain that we don't try to access the file->bits information */
                *data=b;         /* assign the bits read in */
                return(orig_count-count);   /* break out now */
              } /* end if */
            bitfile_rec->bytez=n+(bitfile_rec->bytep=bitfile_rec->bytea);
          } /* end if */
        l = *(bitfile_rec->bytep++);
        b |= l << (count-=BITNUM);
      } /* end while */

    /* split any partial request with the bits buffer */
    if(count>0) {
        if(bitfile_rec->bytep==bitfile_rec->bytez) {
            n=Hread(bitfile_rec->acc_id,BITBUF_SIZE,bitfile_rec->bytea);
            if(n==FAIL) {          /* EOF */
                bitfile_rec->count=0;  /* make certain that we don't try to access the file->bits information */
                *data=b;         /* assign the bits read in */
                return(orig_count-count);      /* return now */
              } /* end if */
            bitfile_rec->bytez=n+(bitfile_rec->bytep=bitfile_rec->bytea);
          } /* end if */
        bitfile_rec->count=(BITNUM-count);
        l=bitfile_rec->bits = *(bitfile_rec->bytep++);
        b|=l>>bitfile_rec->count;
      } /* end if */
    else
        bitfile_rec->count=0;

    *data=b;
    return(orig_count);
}   /* end Hbitread() */

/*--------------------------------------------------------------------------

 NAME
       Hendbitaccess -- to dispose of a bitfile element
 USAGE
       int32 Hendbitaccess(bitfile_id,flushbit)
       int32 bitfile_id;        IN: id of bitfile element to dispose of
       intn flushbit;           IN: determines how to flush leftover bits
                                   (leftover bits are bits that have been
                                    buffered, but are less than the
                                    BITNUM (usually set to 8) number of
                                    bits)
                                    0 - flush with zeros
                                    1 - flush with ones
                                   -1 - throw away any leftover bits
 RETURNS
       returns SUCCEED (0) if successful, FAIL (-1) otherwise
 DESCRIPTION
       Used to dispose of a bitfile element.  Flushes any buffered bits
       to the dataset (if writing), and then calls Hendaccess.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
int32 Hendbitaccess(int32 bitfile_id,intn flushbit)
#else
int32 Hendbitaccess(bitfile_id,flushbit)
int32 bitfile_id;           /* access id */
intn flushbit;              /* how to flush the bits */
#endif
{
    char *FUNC="Hendbitaccess";   /* for HERROR */
    bitrec_t *bitfile_rec;      /* bitfile record */

    /* check validity of access id */
    bitfile_rec = BITID2REC(bitfile_id);
    if (bitfile_rec==NULL || !bitfile_rec->used) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */

    if(bitfile_rec->mode=='w') {
        if(flushbit!=(-1) && bitfile_rec->count<8)  /* only flush bits if asked and there are bits to flush */
            Hbitwrite(bitfile_id,bitfile_rec->count,(uint32)(flushbit ? 0xFF : 0));
        Hwrite(bitfile_rec->acc_id,bitfile_rec->bytep-bitfile_rec->bytea,bitfile_rec->bytea);
      } /* end if */
    HDfreespace((VOIDP)bitfile_rec->bytea);    /* free the space for the buffer */
    bitfile_rec->used = FALSE;

    return(Hendaccess(bitfile_rec->acc_id));
}   /* end Hendbitaccess() */

/*--------------------------------------------------------------------------
 HIget_bitfile_slot

 get a free bitfile record slot
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
#ifdef CONVEX
PRIVATE int HIget_bitfile_slot()
#else
PRIVATE int HIget_bitfile_slot(VOID)
#endif
#else
PRIVATE int HIget_bitfile_slot()
#endif
{
    int i;                     /* temp index */
    char *FUNC="HIget_bitfile_slot";

    /* Access records not allocated yet, allocate dynamically and initialize */
    if (!bitfile_records) {
        bitfile_records=(bitrec_t *)HDgetspace(MAX_BITFILE * sizeof(bitrec_t));
        if (!bitfile_records)
            return FAIL;
        for(i=0; i<MAX_BITFILE; i++)
            bitfile_records[i].used=FALSE;

        if((bitfile_records[0].bytea = (uint8 *) HDgetspace(BITBUF_SIZE)) == NULL) {
            HERROR(DFE_NOSPACE);
            return(FAIL);
          } /* end if */
        bitfile_records[0].used=TRUE;  /* use the first record */
        return(0);
      } /* end if */

    /* return the first unused record */
    for(i=0; i<MAX_BITFILE; i++)
        if(!bitfile_records[i].used) {
            if((bitfile_records[i].bytea = (uint8 *) HDgetspace(BITBUF_SIZE)) == NULL) {
                HERROR(DFE_NOSPACE);
                return(FAIL);
              } /* end if */
            bitfile_records[i].used=TRUE;
            return(i);
          } /* end if */

    return FAIL;
} /* HIget_bitfile_slot */

