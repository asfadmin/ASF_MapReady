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
 * File:    dfgroup.c
 * Purpose: Low level functions for implementing groups
 * Invokes: df.c df.h
 * Contents:
 *  DFdiread: read in the data identifier list from the group
 *  DFdiget: get next data identifier from list
 *  DFdisetup: get ready to store a list of data identifiers to write out
 *  DFdiput: add a data identifier to the list to be written out
 *  DFdiwrite: write out the list of data identifiers
 * Remarks: A group is a way of associating data elements with each other.
 *          It is a tag whose data is a list of tag/refs
 *          Each tag/ref combination is called a data identifier (DI).
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "herr.h"
#include "hfile.h"

#define MAX_GROUPS 8

typedef struct DIlist_struct {
    uint8    *DIlist;
    int32    num;
    int32    current;
} DIlist, *DIlist_ptr;

static DIlist_ptr *Group_list = NULL;

#define GSLOT2ID(s) ((((uint32)GROUPTYPE & 0xffff) << 16) | ((s) & 0xffff))
#define VALIDGID(i) (((((uint32)(i) >> 16) & 0xffff) == GROUPTYPE) && \
                    (((uint32)(i) & 0xffff) < MAX_GROUPS))
#define GID2REC(i)  ((VALIDGID(i) ? (Group_list[(uint32)(i) & 0xffff]) : NULL))

/*-----------------------------------------------------------------------------
 * Name:    setgroupREC
 * Purpose: Add a group list into the internal structure and return an ID
 * Inputs:  list_rec: list to remember
 * Returns: FAIL on failure else a group ID to the list
 * Users:   other group routines
 * Invokes: 
 * Remarks: Allocates internal storeage if necessary
 *---------------------------------------------------------------------------*/
PRIVATE int32
#ifdef PROTOTYPE
setgroupREC(DIlist_ptr list_rec)
#else
setgroupREC(list_rec)
     DIlist_ptr list_rec;
#endif
{
    char *FUNC="setgroupREC";
    int32 i;

    if (!Group_list) {
        Group_list = (DIlist_ptr *) HDgetspace((uint32) MAX_GROUPS *
                                               sizeof(DIlist_ptr));
        if (!Group_list)
            HRETURN_ERROR(DFE_NOSPACE, FAIL);

#ifndef OLD_WAY
        for (i = 0; i < MAX_GROUPS; i++)
            Group_list[i] = NULL;
#else
        HDmemset(Group_list,0,MAX_GROUPS*sizeof(DIlist_ptr));
#endif

    }

    for (i = 0; i < MAX_GROUPS; i++)
        if (!Group_list[i]) {
            Group_list[i] = list_rec;
            return GSLOT2ID(i);
        }

    HRETURN_ERROR(DFE_INTERNAL, FAIL);
} /* setgroupREC */

/*-----------------------------------------------------------------------------
 * Name:    DFdiread
 * Purpose: Read a list of DIs into memory
 * Inputs:  file_id: HDF file pointer
 *          tag, ref: id of group which is to be read in
 * Returns: FAIL on failure else a group ID to the list
 * Users:   HDF systems programmers, DF8getrig, other routines
 * Invokes: HDvalidfid, DFIfind, DFgetelement
 * Remarks: assumes tag is a group
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFdiread(int32 file_id, uint16 tag, uint16 ref)
#else
int32 DFdiread(file_id, tag, ref)
     int32 file_id;
     uint16 tag, ref;           /* tag, ref of group */
#endif
{
    DIlist_ptr new_list;
    char *FUNC="DFdiread";
    int32 length;

    HEclear();

    if (!HDvalidfid(file_id))
        HRETURN_ERROR(DFE_ARGS, FAIL);

    /* Find the group. */
    length = Hlength(file_id, tag, ref);
    if (length == FAIL)
        HRETURN_ERROR(DFE_INTERNAL, FAIL);

    /* allocate a new structure to hold the group */
    new_list = (DIlist_ptr) HDgetspace((uint32) sizeof(DIlist));
    if(!new_list)
        HRETURN_ERROR(DFE_NOSPACE, FAIL);


    new_list->DIlist = (uint8 *) HDgetspace((uint32)length);
    if (!new_list->DIlist) {
        HDfreespace((VOIDP)new_list);
        HRETURN_ERROR(DFE_NOSPACE, FAIL);
    }

    new_list->num = length / 4;
    new_list->current = 0;           /* no DIs returned so far */

    /* read in group */
    if (Hgetelement(file_id, tag, ref, (uint8 *)new_list->DIlist)<0) {
        HDfreespace((VOIDP)new_list->DIlist);
        HDfreespace((VOIDP)new_list);
        HRETURN_ERROR(DFE_READERROR, FAIL);
    }
    return (int32) setgroupREC(new_list);
}

/*-----------------------------------------------------------------------------
 * Name:    DFdiget
 * Purpose: return next DI from the list of DIs in a group
 * Inputs:  list: handle to group (which is list of DIs)
 * Outputs: ptag: pointer to tag part of DI to be returned
 *          pref: pointer to ref part of DI to be returned
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF systems programmers, DF8getrig, other routines
 * Invokes: none
 * Remarks: frees Dilist space when all DIs returned
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFdiget(int32 list, uint16 *ptag, uint16 *pref)
#else
int DFdiget(list, ptag, pref)
     int32  list;
     uint16 *ptag;
     uint16 *pref;
#endif
{
    char       *FUNC="DFdiget";
    uint8      *p;
    DIlist_ptr list_rec;

    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);
    if (list_rec->current >= list_rec->num)
        HRETURN_ERROR(DFE_INTERNAL, FAIL);

    /* compute address of Ndi'th di */
    p = (uint8 *) list_rec->DIlist + 4 * list_rec->current++;
    UINT16DECODE(p, *ptag);
    UINT16DECODE(p, *pref);

    if (list_rec->current == list_rec->num) {
        HDfreespace((VOIDP)list_rec->DIlist);/*if all returned, free storage */
        HDfreespace((VOIDP)list_rec);
        Group_list[list & 0xffff] = NULL;  /* YUCK! BUG! */
    }
    return SUCCEED;
}


/*-----------------------------------------------------------------------------
 * Name:    DFdisetup
 * Purpose: setup space for storing a list of DIs to be written out
 * Inputs:  maxsize: maximum number of DIs expected in the list
 * Returns: FAIL on failure with error set
 *          else a group ID
 * Users:   HDF systems programmers, DF8putrig, other routines
 * Invokes: none
 * Remarks: This call should go away sometime.  Need better way to allocate
 *          space, possibly just use a big block of static space
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFdisetup(int maxsize)
#else
int32 DFdisetup(maxsize)
    int maxsize;
#endif
{
    char *FUNC="DFdisetup";
    DIlist_ptr new_list;

    new_list = (DIlist_ptr) HDgetspace((uint32) sizeof(DIlist));

    if (!new_list)
        HRETURN_ERROR(DFE_NOSPACE, FAIL);

    new_list->DIlist = (uint8 *) HDgetspace((uint32)(maxsize * 4));
    if (!new_list->DIlist) {
        HDfreespace((VOIDP)new_list);
        HRETURN_ERROR(DFE_NOSPACE, FAIL);
    }

    new_list->num     = maxsize;
    new_list->current = 0;

    return setgroupREC(new_list);
}

/*-----------------------------------------------------------------------------
 * Name:    DFdiput
 * Purpose: add a DI to the list to be written out
 * Inputs:  tag, ref: DI to add
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF systems programmers, DF8putrig, other routines
 * Invokes: none
 * Remarks: arg is tag/ref rather than DI for convenience
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFdiput(int32 list, uint16 tag, uint16 ref)
#else
int DFdiput(list, tag, ref)
     int32 list;
     uint16 tag, ref;
#endif
{
    char *FUNC="DFdiput";
    uint8 *p;
    DIlist_ptr list_rec;
    
    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);
    if (list_rec->current >= list_rec->num)
        HRETURN_ERROR(DFE_INTERNAL, FAIL);

    /* compute address of Ndi'th di to put tag/ref in */
    p = (uint8 *) list_rec->DIlist + 4 * list_rec->current++;
    UINT16ENCODE(p, tag);
    UINT16ENCODE(p, ref);

    return SUCCEED;
}

/*-----------------------------------------------------------------------------
 * Name:    DFdiwrite
 * Purpose: Write DI list out to HDF file
 * Inputs:  file_id: HDF file pointer
 *          tag, ref: tag and ref of group whose contents is the list
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF systems programmers, DF8putrig, other routines
 * Invokes: none
 * Remarks: frees storage for Dilist
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFdiwrite(int32 file_id, int32 list, uint16 tag, uint16 ref)
#else
int DFdiwrite(file_id, list, tag, ref)
     int32 file_id;
     int32 list;
     uint16 tag, ref;
#endif
{
    char *FUNC="DFdiwrite";
    int ret;                   /* return value */
    DIlist_ptr list_rec;

    if (!HDvalidfid(file_id))
        HRETURN_ERROR(DFE_ARGS, FAIL);

    list_rec = GID2REC(list);

    if (!list_rec)
        HRETURN_ERROR(DFE_ARGS, FAIL);

    ret = Hputelement(file_id, tag, ref, list_rec->DIlist,
                      (int32)list_rec->current * 4);
    HDfreespace((VOIDP)list_rec->DIlist);
    HDfreespace((VOIDP)list_rec);
#ifdef QAK
printf("DFdiwrite(): list=%ld, list&0xFFFF=%ld\n",list,(list & 0xffff));
#endif
    Group_list[list & 0xffff] = NULL;  /* YUCK! BUG! */
    return ret;
}
