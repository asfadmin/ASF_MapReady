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

#include <ctype.h>
#ifdef MALDEBUG
#define __MALDEBUG__
#endif
#include "hdf.h"
#include "herr.h"
#include "hfile.h"

/*-----------------------------------------------------------------------------
 * Name:    HIstrncpy
 * Purpose: This function creates a string in dest that is at most
 *          'len' characters long.  The 'len' characters *include* the
 *          NULL terminatior which must be added for historical reasons.
 *          So if you have the string "Foo\0" you must call this copy
 *          function with len == 4
 * Inputs:  dest, source: destination and source for copy
 *          len: max length of the outgoing string
 * Returns: Address of dest.
 * Users:   HDF systems programmers
 * Remarks:
 *---------------------------------------------------------------------------*/

#if defined PROTOTYPE
char _HUGE *HIstrncpy(register char *dest,register char *source,int32 len)
#else
char _HUGE *HIstrncpy(dest, source, len)
register char *source, *dest;
int32 len;
#endif /* PROTOTYPE */
{
    char *destp;

    destp = dest;
    if (len == 0) return(destp);
    for(; (len > 1) && (*source != '\0');len--)
        *dest++ = *source++;
    *dest = '\0';       /* Force the last byte be '\0'   */
    return(destp);
}

#if defined PC && !defined PC386
#ifdef WIN3
int32 HDspaceleft(void)
{
/* return the largest amount of memory Windows can give us */
   return(GlobalCompact(0));
}
#else /* WIN3 */
int32 HDspaceleft(void)
{
    struct _heapinfo h_info;        /* structure for heap information, defined in <malloc.h> */
    int heap_status;                /* the current condition of the heap */
    int32 total_free,total_used;    /* variables to store the amount used and the amount free in the heap */

    total_free=0;
    total_used=0;
    h_info._pentry=NULL;
    while((heap_status=_heapwalk(&h_info))==_HEAPOK) {
        if(h_info._useflag==_USEDENTRY)
            total_used+=h_info._size;
        else
            total_free+=h_info._size;
    } /* end for */
    switch(heap_status) {
        case _HEAPEND:
        case _HEAPEMPTY:
            break;

        case _HEAPBADBEGIN:
            printf("%s block at %Fp of size %4.4X\n",(h_info._useflag==_USEDENTRY ? "USED" : "FREE"),h_info._pentry,h_info._size);
            printf("ERROR - heap is damaged\n\n");
            return((int32)-1);

        case _HEAPBADPTR:
            printf("%s block at %Fp of size %4.4X\n",(h_info._useflag==_USEDENTRY ? "USED" : "FREE"),h_info._pentry,h_info._size);
            printf("ERROR - bad pointer to heap\n\n");
            return((int32)-1);

        case _HEAPBADNODE:
            printf("%s block at %Fp of size %4.4X\n",(h_info._useflag==_USEDENTRY ? "USED" : "FREE"),h_info._pentry,h_info._size);
            printf("ERROR - bad node in heap\n\n");
            return((int32)-1);
    } /* end switch */
    return((int32)total_free);
} /* end HDspaceleft() */
#endif /* WIN3 */
#endif /* PC */


#if defined PC && !defined PC386
#ifdef WIN3
VOIDP HDgetspace(uint32 qty)
{
    char *FUNC="HDgetspace";

    HGLOBAL hTmp;
    HGLOBAL far *wfpTmp;

    hTmp=GlobalAlloc(GMEM_MOVEABLE|GMEM_DISCARDABLE|GMEM_ZEROINIT,qty+sizeof(HGLOBAL));
    if (!hTmp) {
        HERROR(DFE_NOSPACE);
        return(NULL);
      } /* end if */
    wfpTmp=(HGLOBAL far *) GlobalLock(hTmp);
    if (!wfpTmp) {
        GlobalFree(hTmp);
        HERROR(DFE_NOSPACE);
        return(NULL);
    }
    *wfpTmp=hTmp;
    wfpTmp++;
    return((void _HUGE *)wfpTmp);
}

VOIDP HDregetspace(VOIDP vfp, uint32 new_size)
{
    char *FUNC="HDregetspace";
    HGLOBAL new_handle;         /* handle of the new memory block */
    HGLOBAL hTmp;
    WORD far *wfpTmp;
    
    if (!vfp)
        return(NULL);
    hTmp = (HGLOBAL)(*(--((WORD far *) vfp)));
    if (!hTmp)
        return(NULL);
    if (GlobalUnlock(hTmp)) {
        HERROR(DFE_NOSPACE);
        return(NULL);
      } /* end if */
    if((new_handle=GlobalReAlloc(hTmp,new_size,GMEM_MOVEABLE|GMEM_DISCARDABLE|GMEM_ZEROINIT))!=NULL) {
        wfpTmp=(WORD far *) GlobalLock(new_handle);
        if (!wfpTmp) {
            GlobalFree(new_handle);
            HERROR(DFE_NOSPACE);
            return(NULL);
        }
        *wfpTmp=(WORD)hTmp;
        wfpTmp++;
        return(wfpTmp);
    } else
        return(NULL);
}

void _HUGE *HDfreespace(void *vfp)
{
    HGLOBAL hTmp;

    if (!vfp)
        return(NULL);
    hTmp=(HGLOBAL)(*(--((WORD far *) vfp)));
    if (!hTmp)
        return(NULL);
    GlobalUnlock(hTmp);
    GlobalFree(hTmp);

    return(NULL);
}
#else /* !WIN3 */

VOIDP HDgetspace(uint32 qty)
{
    char *FUNC="HDgetspace";
    char huge *p;

#ifndef TEST_PC
    qty+=sizeof(char)+sizeof(uint32);   /* increment the quantity to allocate */

    if(qty>=(int32)64000) {   /* see if we have to use halloc() to get a really large chunk of memory */
        p = halloc((int32)qty,(size_t)1);
        if (p==NULL) {
            HERROR(DFE_NOSPACE);
            return(NULL);
          } /* end if */
        *p++=1;     /* indicate that halloc() was used to acquire this memory */
      } /* end if */
    else {      /* we can get away with just malloc() */
        p = malloc((size_t)qty);
        if (p==NULL) {
            HERROR(DFE_NOSPACE);
            return(NULL);
          } /* end if */
        *p++=0;     /* indicate that malloc() was used to acquire this memory */
      } /* end else */
    *(uint32 *)p=qty;   /* save the size of the block */
    p+=sizeof(uint32);
#else
    p = malloc((size_t)qty);
    if (p==NULL)
        HERROR(DFE_NOSPACE);
#endif
    return(p);
}

#ifndef MIN
#define MIN(a,b)    (((a)<(b)) ? (a) : (b))
#endif

VOIDP HDregetspace(VOIDP ptr, uint32 qty)
{
    char *FUNC="HDregetspace";
    uint32 old_size;
    char *p=ptr;
    char *p2;

    qty+=sizeof(char)+sizeof(uint32);   /* increment the quantity to allocate */

    p-=sizeof(uint32);    /* decrement the pointer to free */
    old_size=*(uint32 *)p;
    p-=sizeof(char);
    if(qty>=(int32)64000) {   /* see if we have to use halloc() to get a really large chunk of memory */
        p2=halloc((int32)qty,(size_t)1);
        if(p2==NULL) {
            HERROR(DFE_NOSPACE);
            return(NULL);
          } /* end if */
        *p2++=1;     /* indicate that halloc() was used to acquire this memory */
        *(uint32 *)p2=qty;   /* save the size of the block */
      } /* end if */
    else {      /* we can get away with just malloc() */
        p2=malloc((size_t)qty);
        if(p2==NULL) {
            HERROR(DFE_NOSPACE);
            return(NULL);
          } /* end if */
        *p2++=0;     /* indicate that malloc() was used to acquire this memory */
      } /* end else */
    *(uint32 *)p2=qty;   /* save the size of the block */
    p2+=sizeof(uint32);
    HDmemcpy(p2,p+(sizeof(char)+sizeof(uint32)),
            MIN(UINT_MAX,(uint16)MIN(old_size,qty))-(sizeof(char)+sizeof(uint32)));
    if(*p)    /* check whether block of memory was allocated with halloc() */
        hfree(p);
    else       /* memory was allocated through malloc() */
        free(p);
    return(p2);
}

VOIDP HDfreespace(void *ptr)
{
    char *p=ptr;

    if(ptr==NULL)
        return(NULL);

#ifndef TEST_PC
    p-=(sizeof(char)+sizeof(uint32));    /* decrement the pointer to free */
    if(*p) {   /* check whether block of memory was allocated with halloc() */
printf("HDfreespace(): hfree() called\n");
        hfree(p);
      } /* end if */
    else       /* memory was allocated through malloc() */
#endif
        free(p);
    return(NULL);
}

#endif /* WIN3 */

#else /* !PC | PC386 */


#if defined PROTOTYPE
VOIDP HDgetspace(uint32 qty)
#else
VOIDP HDgetspace(qty)
uint32 qty;
#endif /* PROTOTYPE */
{
    char *FUNC="HDgetspace";
    char *p;

    p = (char *) malloc(qty);
    if (p== (char *) NULL) {
        HERROR(DFE_NOSPACE);
        HEreport("Attempted to allocate %d bytes", qty);
        return(NULL);
    }
    return(p);
}

#if defined PROTOTYPE
VOIDP HDregetspace(VOIDP where, uint32 qty)
#else
VOIDP HDregetspace(where, qty)
VOIDP where;
uint32 qty;
#endif /* PROTOTYPE */
{
    char *FUNC="HDregetspace";
    char *p;

    p = (char *) realloc(where, qty);
    if (p== (char *) NULL) {
        HERROR(DFE_NOSPACE);
        HEreport("Attempted to re-allocate %d bytes", qty);
        return(NULL);
    }
    return(p);
}

#if defined PROTOTYPE
VOIDP HDfreespace(VOIDP ptr)
#else
VOIDP HDfreespace(ptr)
VOIDP ptr;
#endif /* PROTOTYPE */
{
    if (ptr!=NULL) free(ptr);
    return(NULL);
}

#endif /* !PC | PC386 */

#if defined VMS | (defined PC & !defined PC386) | defined macintosh
/* HDstrdup replacement for strdup() on VMS and PCs under MS-DOS and Windows. */
/* Also replacement on Macintosh.*/
/* This is needed because of the way memory is allocated */
char *HDstrdup(const char *s)
{
    char *ret;

    ret=HDgetspace((uint32)HDstrlen(s)+1);
    if(ret==NULL)
        return(NULL);
    HDstrcpy(ret,s);
    return(ret);
} /* end HDstrdup() */
#endif /* VMS & (PC & !PC386) & Macintosh */

#if defined WIN3 || defined PC
#ifdef WIN3
/*--------------------------------------------------------------------------
**
** NAME
**  fmemcpy_big -- function specific to the PC to copy 32-bits of data
** USAGE
**  VOIDP fmemcpy_big (dst,src,len)
**  VOIDP dst;          IN: the buffer to put bytes into
**  VOIDP src;          IN: the source buffer to copy from
**  uint32 len;         IN: the number of bytes to copy
** RETURNS
**  returns the original pointer to dst
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for memcpy, this function
**  blocks that up into 64kb blocks to copy at a time.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
VOIDP fmemcpy_big(VOIDP dst,VOIDP src,uint32 len)
#else
VOIDP fmemcpy_big(dst,src,len)
VOIDP dst;
VOIDP src;
uint32 len;
#endif
{
    uint8 *s,d;             /* alias for the buffers */

    if(len<=UINT_MAX)   /* if the size is small enough copy all at once */
        return(_fmemcpy(dst,src,(size_t)len));
    else {  /* number of bytes to read */
        s=(uint8 *)src;
        d=(uint8 *)dst;
        while(len>UINT_MAX) {
            _fmemcpy(d,s,UINT_MAX);
            s+=UINT_MAX;
            d+=UINT_MAX;
            len-=UINT_MAX;
          } /* end while */
        if(len>0)
            _fmemcpy(d,s,(size_t)len);
      } /* end else */
    return(dst);
}   /* end fmemcpy_big() */

/*--------------------------------------------------------------------------
**
** NAME
**  fmemset_big -- function specific to the PC to set 32-bits of data
** USAGE
**  VOIDP fmemset_big (src, c, len)
**  VOIDP src;          IN: the buffer to set to a value
**  intn c;             IN: the value to use to set
**  uint32 len;         IN: the number of bytes to set
** RETURNS
**  returns the original pointer to s
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for memcpy, this function
**  blocks that up into 64kb blocks to set at a time.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
VOIDP fmemset_big(VOIDP src,intn c,uint32 len)
#else
VOIDP fmemset_big(src,c,len)
VOIDP src;
intn c;
uint32 len;
#endif
{
    uint8 *s;               /* alias for the buffers */

    if(len<=UINT_MAX)   /* if the size is small enough copy all at once */
        return(_fmemset(src,c,(size_t)len));
    else {  /* number of bytes to read */
        s=(uint8 *)src;
        while(len>UINT_MAX) {
            _fmemset(s,c,UINT_MAX);
            s+=UINT_MAX;
            len-=UINT_MAX;
          } /* end while */
        if(len>0)
            _fmemset(s,c,(size_t)len);
      } /* end else */
    return(src);
}   /* end fmemset_big() */

/*--------------------------------------------------------------------------
**
** NAME
**  fmemcmp_big -- function specific to the PC to compare 32-bits of data
** USAGE
**  VOIDP fmemcmp_big (s1,s2,len)
**  VOIDP s1;           IN: the first buffer
**  VOIDP s2;           IN: the second buffer
**  uint32 len;         IN: the number of bytes to copy
** RETURNS
**  returns a value less than, equal to, or greater than 0 indicating
**      that the object pointed to by s1 is less than, equal to, or greater
**      than the object pointed to by s2
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for memcpy, this function
**  blocks that up into 64kb blocks to compare at a time.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn fmemcmp_big(VOIDP s1,VOIDP s2,uint32 len)
#else
intn fmemcmp_big(s1,s2,len)
VOIDP s1;
VOIDP s2;
uint32 len;
#endif
{
    intn ret_val;

    if(len<=UINT_MAX)   /* if the size is small enough copy all at once */
        return(_fmemcmp(s1,s2,(size_t)len));
    else {  /* number of bytes to read */
        while(len>UINT_MAX) {
            ret_val=_fmemcmp(s1,s2,UINT_MAX);
            if(ret_val!=0)
                return(ret_val);
            ((uint8 huge *)s1)+=UINT_MAX;
            ((uint8 huge *)s2)+=UINT_MAX;
            len-=UINT_MAX;
          } /* end while */
        if(len>0)
            return(_fmemcmp(s1,s2,(size_t)len));
      } /* end else */
    return(0);
}   /* end fmemcmp_big() */
#else   /* !WIN3 */
/*--------------------------------------------------------------------------
**
** NAME
**  memcpy_big -- function specific to the PC to copy 32-bits of data
** USAGE
**  VOIDP memcpy_big (dst,src,len)
**  VOIDP dst;          IN: the buffer to put bytes into
**  VOIDP src;          IN: the source buffer to copy from
**  uint32 len;         IN: the number of bytes to copy
** RETURNS
**  returns the original pointer to dst
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for memcpy, this function
**  blocks that up into 64kb blocks to copy at a time.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
VOIDP memcpy_big(VOIDP dst,VOIDP src,uint32 len)
#else
VOIDP memcpy_big(dst,src,len)
VOIDP dst;
VOIDP src;
uint32 len;
#endif
{
    uint8 *s,*d;             /* alias for the buffers */

    if(len<=UINT_MAX)   /* if the size is small enough copy all at once */
        return(memcpy(dst,src,(size_t)len));
    else {  /* number of bytes to read */
        s=(uint8 *)src;
        d=(uint8 *)dst;
        while(len>UINT_MAX) {
            memcpy(d,s,UINT_MAX);
            s+=UINT_MAX;
            d+=UINT_MAX;
            len-=UINT_MAX;
          } /* end while */
        if(len>0)
            memcpy(d,s,(size_t)len);
      } /* end else */
    return(dst);
}   /* end memcpy_big() */

/*--------------------------------------------------------------------------
**
** NAME
**  memset_big -- function specific to the PC to set 32-bits of data
** USAGE
**  VOIDP memset_big (src, c, len)
**  VOIDP src;          IN: the buffer to set to a value
**  intn c;             IN: the value to use to set
**  uint32 len;         IN: the number of bytes to set
** RETURNS
**  returns the original pointer to s
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for memcpy, this function
**  blocks that up into 64kb blocks to set at a time.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
VOIDP memset_big(VOIDP src,intn c,uint32 len)
#else
VOIDP memset_big(src,c,len)
VOIDP src;
intn c;
uint32 len;
#endif
{
    uint8 *s;               /* alias for the buffers */

    if(len<=UINT_MAX)   /* if the size is small enough copy all at once */
        return(memset(src,c,(size_t)len));
    else {  /* number of bytes to read */
        s=(uint8 *)src;
        while(len>UINT_MAX) {
            memset(s,c,UINT_MAX);
            s+=UINT_MAX;
            len-=UINT_MAX;
          } /* end while */
        if(len>0)
            memset(s,c,(size_t)len);
      } /* end else */
    return(src);
}   /* end memset_big() */

/*--------------------------------------------------------------------------
**
** NAME
**  memcmp_big -- function specific to the PC to compare 32-bits of data
** USAGE
**  VOIDP memcmp_big (s1,s2,len)
**  VOIDP s1;           IN: the first buffer
**  VOIDP s2;           IN: the second buffer
**  uint32 len;         IN: the number of bytes to copy
** RETURNS
**  returns a value less than, equal to, or greater than 0 indicating
**      that the object pointed to by s1 is less than, equal to, or greater
**      than the object pointed to by s2
** DESCRIPTION
**  Because the IBM PC compilers use 16-bit number for memcpy, this function
**  blocks that up into 64kb blocks to compare at a time.
** GLOBAL VARIABLES
**  None
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
--------------------------------------------------------------------------*/
#ifdef PROTOTYPE
intn memcmp_big(VOIDP s1,VOIDP s2,uint32 len)
#else
intn memcmp_big(s1,s2,len)
VOIDP s1;
VOIDP s2;
uint32 len;
#endif
{
    uint8 *t1=(uint8 *)s1,
        *t2=(uint8 *)s2;
    intn ret_val;

    if(len<=UINT_MAX)   /* if the size is small enough copy all at once */
        return(memcmp(t1,t2,(size_t)len));
    else {  /* number of bytes to read */
        while(len>UINT_MAX) {
            ret_val=memcmp(t1,t2,UINT_MAX);
            if(ret_val!=0)
                return(ret_val);
            t1+=UINT_MAX;
            t2+=UINT_MAX;
            len-=UINT_MAX;
          } /* end while */
        if(len>0)
            return(memcmp(t1,t2,(size_t)len));
      } /* end else */
    return(0);
}   /* end memcmp_big() */
#endif  /* WIN3 */

#endif  /* WIN3 | PC */

