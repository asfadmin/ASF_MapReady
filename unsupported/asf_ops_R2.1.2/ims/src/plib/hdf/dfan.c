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
 * File:    dfan.c
 * Purpose: read and write annotations: labels and descriptions of data items
 * Invokes: df.c
 * Contents: 
 *
 *  DFANgetlablen: get length of label of tag/ref
 *  DFANgetlabel:  get label of tag/ref
 *  DFANgetdesclen: get length of description of tag/ref
 *  DFANgetdesc:   get description of tag/ref
 *
 *  DFANgetfidlen: get length of file ID
 *  DFANgetfid:    get file ID
 *  DFANgetfdslen: get length of file description
 *  DFANgetfds:    get file description
 *
 *  DFANputlabel:  put label of tag/ref
 *  DFANputdesc:   put description of tag/ref
 *
 *  DFANaddfid:    add file ID
 *  DFANaddfds:    add file description
 *
 *  DFANlastref:   return ref of last annotation read or written
 *  DFANlablist:   get list of labels for a particular tag
 *
 *  DFANIopen:     open/reopen file
 *  DFANIlocate:   return ref of label/desc of tag/ref
 *  DFANIaddentry: add entry in annotation directory
 *  DFANIgetannlen: get length of annotation of tag/ref
 *  DFANIgetann:   get annotation of tag/ref
 *  DFANIputann:   put annotation of tag/ref
 *  DFANIlablist:  get list of labels for a particular tag
 *
 *  DFANIaddfann:  add file annotation (ID or description)
 *  DFANIgetfannlen: get length of file annotation
 *  DFANIgetfann:  get file annotation
 *---------------------------------------------------------------------------*/

#include "hdf.h"
#include "herr.h"
#include "hfile.h"
#include "dfan.h"

static uint16 Lastref = 0;             /* Last ref read/written */
static uint16 Next_label_ref = 0;      /* Next file label ref to read/write */
static uint16 Next_desc_ref = 0;       /* Next file desc ref to read/write */

static char Lastfile[DF_MAXFNLEN];          /* last file opened */

/* pointers to directories of object annotations */
static DFANdirhead *DFANdir[2] = { NULL,          /* object labels       */
                                   NULL           /* object descriptions */
                                 };
/*
** Prototypes for local functions
*/

#ifndef VMS
PRIVATE int32 DFANIopen
    PROTO((char *filename, intn access));
#else /*VMS*/
PRIVATE int32 _DFANIopen();
#endif

#ifdef OLD_WAY
uint16 DFANIlocate
  PROTO((int32 file_id, int type, uint16 tag, uint16 ref));

int DFANIaddentry
  PROTO((int type, uint16 annref, uint16 datatag, uint16 dataref));


int32 DFANIgetannlen
  PROTO((char *filename, uint16 tag, uint16 ref, int type));

int DFANIgetann
  PROTO((char *filename, uint16 tag, uint16 ref, uint8 *ann,
                int32 maxlen, int type));

int DFANIputann
  PROTO((char *filename, uint16 tag, uint16 ref, uint8 *ann, 
	 int32 annlen, int type));

int DFANIlablist
  PROTO((char *filename, uint16 tag, uint16 reflist[], uint8 *labellist,
	 int listsize, int maxlen, int startpos, int isfortran));

int DFANIaddfann
  PROTO((int32 file_id, char *ann, int32 annlen, int type));

int32 DFANIgetfannlen
  PROTO((int32 file_id, int type, int isfirst));

int32 DFANIgetfann
  PROTO((int32 file_id, char *ann, int32 maxlen, int type, int isfirst));
#endif  /* OLD_WAY */


/*-----------------------------------------------------------------------------
 * HDF object (i.e. tag/ref) label and description input routines
 *---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    DFANgetlablen
 * Purpose: get length of label of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want label
 * Returns: length of label on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetannlen
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANgetlablen(char *filename, uint16 tag, uint16 ref)
#else
int32 DFANgetlablen(filename, tag, ref)
    char *filename;
    uint16 tag, ref;
#endif
{
    return(DFANIgetannlen(filename, tag, ref, DFAN_LABEL));
}


/*-----------------------------------------------------------------------------
 * Name:    DFANgetlabel
 * Purpose: get label of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want label
 *          label: space to return label in
 *          maxlen: size of space to return label in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetann
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANgetlabel(char *filename, uint16 tag, uint16 ref, char *label,
                 int32 maxlen)
#else
int DFANgetlabel(filename, tag, ref, label, maxlen)
char *filename;
uint16 tag, ref;
char *label;
int32 maxlen;
#endif
{
    return(DFANIgetann(filename, tag, ref, (uint8 *)label, maxlen, DFAN_LABEL));
}


/*-----------------------------------------------------------------------------
 * Name:    DFANgetdesclen
 * Purpose: get length of description of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want description
 * Returns: length of description on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetannlen
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANgetdesclen(char *filename, uint16 tag, uint16 ref)
#else
int32 DFANgetdesclen(filename, tag, ref)
char *filename;
uint16 tag, ref;
#endif 
{
    return(DFANIgetannlen(filename, tag, ref, DFAN_DESC));
}


/*-----------------------------------------------------------------------------
 * Name:    DFANgetdesc
 * Purpose: get description of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want description
 *          desc: space to return description in
 *          maxlen: size of space to return description in
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetann
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANgetdesc(char *filename, uint16 tag, uint16 ref, char *desc,
                int32 maxlen)
#else
int DFANgetdesc(filename, tag, ref, desc, maxlen)
char *filename;
uint16 tag, ref;
char *desc;
int32 maxlen;
#endif 
{
    return(DFANIgetann(filename, tag, ref, (uint8 *)desc, maxlen, DFAN_DESC));
}


/*-----------------------------------------------------------------------------
 * File ID and description input routines
 *---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    DFANgetfidlen
 * Purpose: get length of next file ID
 * Inputs:  file_id: pointer to HDF file
 *          isfirst: 1: start with first one; 0: get length of next one
 * Returns: On success: length of next file ID; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfannlen
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANgetfidlen(int32 file_id, int isfirst)
#else
int32 DFANgetfidlen(file_id, isfirst)
int32 file_id;
int isfirst;
#endif 
{
     return ( DFANIgetfannlen(file_id, DFAN_LABEL, isfirst) );
}


/*-----------------------------------------------------------------------------
 * Name:    DFANgetfid
 * Purpose: get next file ID
 * Inputs:  file_id: pointer to HDF file
 *          id: label
 *          maxlen: max allowable length for label
 *          isfirst: 1: start with first one; 0: get next one
 * Returns: On success: length of label; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfann
 * Remarks: If maxlen not great enough, label is truncated to maxlen-1 chars
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANgetfid(int32 file_id, char *id, int32 maxlen, int isfirst)
#else
int32 DFANgetfid(file_id, id, maxlen, isfirst)
int32 file_id;
char *id;
int32 maxlen;
int isfirst;
#endif 
{
     return ( DFANIgetfann(file_id, id, maxlen, DFAN_LABEL, isfirst) );
}



/*-----------------------------------------------------------------------------
 * Name:    DFANgetfdslen
 * Purpose: get length of next file description
 * Inputs:  file_id: pointer to HDF file
 *          isfirst: 1: start with first one; 0: get length of next one
 * Returns: On success: length of next file ID; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfannlen
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANgetfdslen(int32 file_id, int isfirst)
#else
int32 DFANgetfdslen(file_id, isfirst)
    int32 file_id;
    int isfirst;
#endif 
{
     return ( DFANIgetfannlen(file_id, DFAN_DESC, isfirst) );
}


/*-----------------------------------------------------------------------------
 * Name:    DFANgetfds
 * Purpose: get next file description
 * Inputs:  file_id: pointer to HDF file
 *          desc: description
 *          maxlen: max allowable length for description
 *          isfirst: 1: start with first one; 0: get next one
 * Returns: On success: length of description;
 *          On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIgetfann
 * Remarks: If maxlen not great enough, description is truncated to
 *          maxlen-1 chars
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANgetfds(int32 file_id, char *desc, int32 maxlen, int isfirst)
#else
int32 DFANgetfds(file_id, desc, maxlen, isfirst)
int32 file_id;
char *desc;
int32 maxlen;
int isfirst;
#endif 
{
    return ( DFANIgetfann(file_id, desc, maxlen, DFAN_DESC, isfirst) );
}



/*-----------------------------------------------------------------------------
 * HDF object (i.e. tag/ref) label and description output routines
 *---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    DFANputlabel
 * Purpose: put label of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which this is the label
 *          label: label to write to file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIputann
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANputlabel(char *filename, uint16 tag, uint16 ref, char *label)
#else
int DFANputlabel(filename, tag, ref, label)
char *filename;
uint16 tag, ref;
char *label;
#endif 
{
    return(DFANIputann(filename, tag, ref, (uint8 *)label,
                (int32)HDstrlen(label), DFAN_LABEL));
}


/*-----------------------------------------------------------------------------
 * Name:    DFANputdesc
 * Purpose: put description of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which this is the description
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIputann
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANputdesc(char *filename, uint16 tag, uint16 ref, char *desc,
                int32 desclen)
#else
int DFANputdesc(filename, tag, ref, desc, desclen)
char *filename;
uint16 tag, ref;
char *desc;
int32 desclen;
#endif 
{
    return(DFANIputann(filename, tag, ref, (uint8 *)desc, desclen, DFAN_DESC));
}


/*-----------------------------------------------------------------------------
 * File ID and description output routines
 *---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    DFANaddfid
 * Purpose: add file file ID
 * Inputs:  file_id: pointer to HDF file
 *          id: ID to write to file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIadfile_idann
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANaddfid(int32 file_id, char *id)
#else
int DFANaddfid(file_id, id)
int32 file_id;
char *id;
#endif 
{
    return ( DFANIaddfann(file_id, id, (int32)HDstrlen(id), DFAN_LABEL) );
}


/*-----------------------------------------------------------------------------
 * Name:    DFANaddfds
 * Purpose: add file file ID
 * Inputs:  file_id: pointer to HDF file
 *          desc: description to write to file
 *          desclen: length of description
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIadfile_idann
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANaddfds(int32 file_id, char *desc, int32 desclen)
#else
int DFANaddfds(file_id, desc, desclen)
int32 file_id;
char *desc;
int32 desclen;
#endif 
{
    return ( DFANIaddfann(file_id, desc, desclen, DFAN_DESC) );
}


/*-----------------------------------------------------------------------------
 * Miscellaneous other routines
 *---------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
 * Name:    DFANlastref
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
uint16 DFANlastref(void)
#else
uint16 DFANlastref()
#endif 
{
    return(Lastref);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANlablist
 * Purpose: Return list of refs and labels for a given tag
 * Inputs:  filename: name of HDF file
 *          tag: tag to get list of refs and labels for
 *          reflist: array to place refs in
 *          labellist: array of strings to place labels in
 *          listsize: size of ref and label lists
 *          maxlen: maximum length allowed for label
 *          startpos: beginning from the startpos'th entry, upto listsize
 *              entries will be returned.
 * Returns: number of entries on success, -1 on error with DFerror set
 * Users:   HDF users, utilities, other routines
 * Invokes: DFANIlablist
 * Method:  call DFANIlablist
 * Remarks: Revised 04/17/90 so that it returns all ref numbers for
 *          the given tag, rather than just those that have labels.
 *          Where there is no corresponding label, the position in
 *          labellist is zero filled (C) or blank filled (Fortran).
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANlablist(char *filename, uint16 tag, uint16 reflist[], char *labellist,
                int listsize, int maxlen, int startpos)
#else
int DFANlablist(filename, tag, reflist, labellist, listsize, maxlen, startpos)
char *filename;
uint16 tag, reflist[];
char *labellist;
int listsize;
int maxlen, startpos;
#endif 
{
    return(DFANIlablist(filename, tag, reflist, (uint8 *)labellist, 
                                            listsize, maxlen, startpos, 0));
}


/******************************************************************************/
/*----------------------- Internal routines ---------------------------------*/
/******************************************************************************/


/*-----------------------------------------------------------------------------
 * Name:    DFANIopen
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
PRIVATE int32 DFANIopen(char *filename, intn access)
#else
PRIVATE int32 DFANIopen(filename, access)
char *filename;
intn access;
#endif 
{

    int32 file_id;
    DFANdirhead *p, *q;

        /* use reopen if same file as last time - more efficient */
    if (HDstrncmp(Lastfile,filename,DF_MAXFNLEN) || (access==DFACC_CREATE)) {
                                    /* treat create as different file */
        file_id = Hopen(filename, access, 0); 
        if (file_id == FAIL) 
            return FAIL;

        for (p=DFANdir[0]; p!=NULL; p=q) {  /* free linked list space */
            q = p->next;
            HDfreespace((VOIDP) p);
        }
        for (p=DFANdir[1]; p!=NULL; p=q) {
            q = p->next;
            HDfreespace((VOIDP) p);
        }
        DFANdir[0] = DFANdir[1] = NULL;
    }
    else {
        file_id = Hopen(filename, access, 0);
        if (file_id == FAIL)
            return FAIL;
    }

    HIstrncpy(Lastfile, filename, DF_MAXFNLEN);
        /* remember filename, so reopen may be used next time if same file */
    return(file_id);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIlocate
 * Purpose: get ref of annotation of given data tag/ref
 * Inputs:  file_id: pointer to HDF file
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 *          tag, ref: tag/ref of item of which we want ref of annotation
 * Returns: ref of annotation on success, 0 on failure with DFerror set
 * Users:   DFANIgetann, DFANIputann, DFANIgetannlen
 * Invokes: DFaccess, DFnumber, DFread, DFIfind
 * Bugs:    When FORTRAN calls this routine with type "label", the string
 *          returned is incorrect in length by one character
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
uint16 DFANIlocate(int32 file_id, int type, uint16 tag, uint16 ref)
#else
uint16 DFANIlocate(file_id, type, tag, ref)
int32 file_id;
int type;
uint16 tag, ref;
#endif 
{
    char *FUNC="DFANIlocate";
    uint8 datadi[4];
    int32 more_anns;
    int32 aid;
    int32 nanns, i;
    uint16 anntag, annref=0;
    DFANdirhead *p;
    uint8 *ptr;
    
    HEclear();

    anntag = (uint16)((type==DFAN_LABEL) ? DFTAG_DIL : DFTAG_DIA);

        /* if no directory for this type of annotation, make one */
    if (DFANdir[type]==NULL) { 
        nanns = Hnumber(file_id, anntag); 
#ifdef QAK
        if (nanns < 0) return(0);
#else
        if (nanns == 0) return(0);
#endif

           /* allocate directory space.  Note head struct includes 1 entry */
        DFANdir[type] = (DFANdirhead *)
                          HDgetspace(((uint32)sizeof(DFANdirhead) +
                                      (nanns-1) * sizeof(DFANdirentry)));
        DFANdir[type]->next = NULL;
        DFANdir[type]->nentries = nanns;

            /* fill directory table */
        aid = Hstartread(file_id, anntag, DFREF_WILDCARD);
        if (aid == FAIL) {
            Hendaccess(aid); return 0;
        }
        else
            more_anns = SUCCEED;

        for (i=0; (i< nanns) && (more_anns != FAIL); i++) {

            if (FAIL == Hinquire(aid, (int32*)NULL, (uint16*)NULL, &annref,
                                (int32*)NULL, (int32*)NULL, (int32*)NULL, 
                                (int16*)NULL, (int16*)NULL) )
                return 0;
            if ((int32)FAIL == Hread(aid, (int32) 4, datadi) )
                return 0;

                /* get data tag/ref */
            DFANdir[type]->entries[i].annref = annref;
            ptr = (uint8 *)&(datadi[0]);
            UINT16DECODE(ptr, DFANdir[type]->entries[i].datatag);
            UINT16DECODE(ptr, DFANdir[type]->entries[i].dataref);

            more_anns = Hnextread(aid, anntag, DFREF_WILDCARD, DF_CURRENT);
        }
        Hendaccess(aid);
    }
    if (!tag) return(1);      /* used to generate directory without search */

        /* find annotation that goes with this tag/ref */
    for (p=(DFANdirhead *)DFANdir[type]; p!=NULL; p=p->next)
        for (i=0; i<p->nentries; i++)
            if(p->entries[i].annref != 0)
                if ((p->entries[i].dataref==ref) && (p->entries[i].datatag==tag))
                    return(p->entries[i].annref);
    HERROR(DFE_NOMATCH);
    return(0);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIaddentry
 * Purpose: add entry to annotation directory
 * Inputs:  type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 *          annref: tag/ref of annotation
 *          datatag, dataref: tag/ref of item of which this is annotation
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   DFANIputann
 * Invokes: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANIaddentry(int type, uint16 annref, uint16 datatag, uint16 dataref)
#else
int DFANIaddentry(type, annref, datatag, dataref)
uint16 annref, datatag, dataref;
int type;
#endif 
{
    int32 i;
    DFANdirhead *p, *q;
    
        /* move to last entry in list */
    for (p=DFANdir[type]; (p!=NULL) && (p->next!=NULL); p=p->next)
        ;

    if (p) {                                    /* not new list */
        for (i=0; i<p->nentries; i++)                   /* check all entries */
            if (p->entries[i].annref==0) {              /* empty slot */
                p->entries[i].annref = annref;          /* store entry */
                p->entries[i].datatag = datatag;
                p->entries[i].dataref = dataref;
                return(0);
            }
    }

        /* need new list or new node in list */
        /* allocate directory space.  Note head struct includes 1 entry */
    q = (DFANdirhead *) HDgetspace((uint32)sizeof(DFANdirhead) +
                                (DFAN_DEFENTRIES-1) * sizeof(DFANdirentry));
    q->next = NULL;
    q->nentries = DFAN_DEFENTRIES;
    if (!p) DFANdir[type] = q;          /* set pointer to this new node */
    else p->next = q;

        /* store entry */
    q->entries[0].annref = annref;
    q->entries[0].datatag = datatag;
    q->entries[0].dataref = dataref;

    for (i=1; i<DFAN_DEFENTRIES; i++)       /* mark rest unused */
        q->entries[i].annref = 0;

    return(0);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIgetannlen
 * Purpose: get length of annotation of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want annotation
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 * Returns: length of annotation on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIopen, DFANIlocate, DFIerr, DFclose, DFIfind
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANIgetannlen(char *filename, uint16 tag, uint16 ref, int type)
#else
int32 DFANIgetannlen(filename, tag, ref, type)
char *filename;
uint16 tag, ref;
int type;
#endif 
{
    char *FUNC="DFANIgetannlen";
    int32 file_id, annlength;
    uint16 anntag, annref;

    HEclear();
    if (!tag) { HERROR(DFE_BADTAG); return FAIL; }
    if (!ref) { HERROR(DFE_BADREF); return FAIL; }

    file_id = DFANIopen(filename, DFACC_READ);
    if (file_id == FAIL) return FAIL; 

        /* get ref of annotation of tag/ref */
    annref = DFANIlocate(file_id, type, tag, ref);
    if (annref==0) {
        Hclose(file_id); return FAIL;
    }
    anntag=(uint16)((type==DFAN_LABEL) ? DFTAG_DIL : DFTAG_DIA);  /* set type tag */

    annlength = Hlength(file_id,anntag,annref) - 4;  /* 4=len of data tag/ref */
    if (annlength == FAIL) { 
        Hclose(file_id); return FAIL; 
    }
    Lastref = annref;                       /* remember ref last accessed */
    if (Hclose(file_id) == FAIL)            /* close file */
        return FAIL;

    return(annlength);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIgetann
 * Purpose: get annotation of tag/ref
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which we want annotation
 *          ann: space to return annotation in
 *          maxlen: size of space to return annotation in
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIopen, DFANIlocate, DFIerr, DFclose, DFaccess, DFIfind, DFread
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFANIgetann(char *filename, uint16 tag, uint16 ref, uint8 *ann,
                int32 maxlen, int type)
#else
intn DFANIgetann(filename, tag, ref, ann, maxlen, type)
char *filename;
uint16 tag, ref;
uint8 *ann;
int32 maxlen;
int type;
#endif 
{
    char *FUNC="DFANIgetann";
    int32 file_id, aid;
    int32 annlen;
    uint16 anntag, annref;
    uint8 datadi[4];        /* to read in and discard data/ref! */

    HEclear();
    if (!ann) { HERROR(DFE_BADPTR); return FAIL; }
    if (!tag) { HERROR(DFE_BADTAG); return FAIL; }
    if (!ref) { HERROR(DFE_BADREF); return FAIL; }

    file_id = DFANIopen(filename, DFACC_READ);
    if (file_id == FAIL) return FAIL; 

        /* get annref and anntag of annotation of tag/ref */
    annref = DFANIlocate(file_id, type, tag, ref);
    if (annref==0) {
        Hclose(file_id); return FAIL;
    }
    anntag = (uint16)((type==DFAN_LABEL) ? DFTAG_DIL : DFTAG_DIA);

        /* find DD for that annotation */
    aid = Hstartread(file_id, anntag, annref);
    if (aid == FAIL) { 
        Hendaccess(aid); Hclose(file_id); return FAIL;
    }
    if (FAIL == Hinquire(aid,(int32*)NULL,(uint16*)NULL,(uint16*)NULL, &annlen,
                        (int32*)NULL, (int32*)NULL, (int16*)NULL, (int16*)NULL) ) { 
        Hendaccess(aid); Hclose(file_id); return FAIL;
    }
    annlen -= 4;  /* first four bytes were tag/ref, so they don't count */

        /* check length, if not enough space, truncate annotation */
        /* labels need space for null terminator, descriptions don't */
    if (type == DFAN_LABEL) {
        if (annlen > maxlen-1) annlen = maxlen-1;
    } else {
        if (annlen > maxlen) annlen = maxlen;
    }

        /* read annotation */
    if ((int32)FAIL == Hread(aid, (int32) 4, datadi)) { /* go past tag/ref */
        Hendaccess(aid); Hclose(file_id); return FAIL;
    }
    if ((int32)FAIL == Hread(aid, annlen, ann)) {  /* read the annotation */
        Hendaccess(aid); Hclose (file_id); return FAIL;
    }
    if (type == DFAN_LABEL)
        ann[annlen] = '\0';                 /* terminate string properly */

    Lastref = annref;                       /* remember ref last accessed */
    Hendaccess(aid);
    return(Hclose(file_id));
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIputann
 * Purpose: put annotation of tag/ref into file
 * Inputs:  filename: name of HDF file
 *          tag, ref: tag/ref of item of which this is the annotation
 *          ann: annotation to write to file
 *          annlen: length of annotation
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: DFANIopen, DFANIlocate, DFANIaddentry, DFIerr, DFclose, DFnewref,
 *          DFaccess, DFwrite
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
intn DFANIputann(char *filename, uint16 tag, uint16 ref, uint8 *ann,
                int32 annlen, int type)
#else
intn DFANIputann(filename, tag, ref, ann, annlen, type)
char *filename;
uint16 tag, ref;
uint8 *ann;
int32 annlen;
int type;
#endif 
{

    char *FUNC="DFANIputann";
    int32 file_id, aid;
    int newflag=0;
    uint16 anntag, annref;
    uint8 datadi[4];        /* to hold data tag/ref for writing */
    uint8 *ptr;

    HEclear();
    if (!ann) { HERROR(DFE_BADPTR); return FAIL; }
    if (!tag) { HERROR(DFE_BADTAG); return FAIL; }
    if (!ref) { HERROR(DFE_BADREF); return FAIL; }

    file_id = DFANIopen(filename, DFACC_RDWR);
    if (file_id == 0) return FAIL;

    anntag = (uint16)((type==DFAN_LABEL) ? DFTAG_DIL : DFTAG_DIA);

        /* check if this tag/ref already has this type of annotation */
    annref = DFANIlocate(file_id, type, tag, ref);
    if (annref==0) {
        annref = Hnewref(file_id);
        if (annref==0) { 
            Hclose(file_id); return FAIL;
        }
        newflag = 1;            /* remember to add ann tag/ref to directory */
    }
    
      /*
       * if annotation exists, delete it and rewrite new annotation
       */
    if (newflag == 0) {           /* does prev annotation exist? */
        if (Hdeldd(file_id, anntag, annref) == FAIL) {
            Hclose(file_id);
            HEreport("Unable to replace old annotation");
            return FAIL;
        }
    }

        /* put annotation */
        /* Note: cannot use DFputelement because need to write data tag/ref */
    aid = Hstartwrite(file_id, anntag, annref, annlen+4);
    if (aid == FAIL) {
        Hendaccess(aid); Hclose(file_id); return FAIL;
    }

        /* write annotation */
    ptr = (uint8 *)&(datadi[0]);    /* first, write the object's tag/ref */
    UINT16ENCODE(ptr, tag);
    UINT16ENCODE(ptr, ref);
    if ((int32)FAIL == Hwrite(aid, (int32) 4, datadi)) {
        Hclose(file_id); return FAIL; 
    }
    if ((int32)FAIL == Hwrite(aid, annlen, ann)) {  /* then write the annotation */
        Hendaccess(aid); Hclose(file_id); return FAIL;
    }

        /* put annotation tag/ref into directory if new */
    if (newflag) {
        if (FAIL == DFANIaddentry(type, annref, tag, ref)){
            Hendaccess(aid); Hclose(file_id); return FAIL;
        }
    }

    Lastref = annref;                       /* remember ref last accessed */
    Hendaccess(aid);
    return (Hclose(file_id));
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIlablist
 * Purpose: Return list of refs and labels for a given tag
 * Inputs:  filename: name of HDF file
 *          tag: tag to get list of refs and labels for
 *          reflist: array to place refs in
 *          labellist: array of strings to place labels in
 *          listsize: size of ref and label lists
 *          maxlen: maximum length allowed for label
 *          startpos: position in list from which on to return listsize entries
 *          isfortran: 0 if C, 1 if Fortran
 * Returns: number of entries on success, -1 on error with DFerror set
 * Users:   DFANlablist
 * Invokes: DFANIopen, DFIerr, DFclose, DFANIlocate, DFaccess, DFread
 * Method:  search directory
 * Remarks: none
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANIlablist(char *filename, uint16 tag, uint16 reflist[], 
		 uint8 *labellist, int listsize, int maxlen, int startpos, 
		 int isfortran)
#else
int DFANIlablist(filename, tag, reflist, labellist, listsize, maxlen,
                                                        startpos, isfortran)
char *filename;
uint16 tag, reflist[];
uint8 *labellist;                       /* actually an array of strings */
int listsize, maxlen, startpos, isfortran;
#endif 
{
    char *FUNC="DFANIlablist";
    int32 i;
    int j, k;
    int32 file_id, aid, len;
    uint16 ref;
    DFANdirhead *p;
    uint8 *lp;                    /* pointer to label */
    int nrefs,nlabs;
    uint8 labeldi[4];             /* to read in and discard data/ref */
    
    HEclear();

    if (!reflist || !labellist)  { 
        HERROR(DFE_BADPTR); return FAIL; }
    if (!tag) { 
        HERROR(DFE_BADTAG); return FAIL; }

    file_id = DFANIopen(filename, DFACC_READ);
    if (file_id == 0) return FAIL;

    /* clear labellist.  pad with blanks for Fortran; add null for C  */
    if (isfortran)
        HDmemset(labellist, ' ', (int32)maxlen * (int32)listsize);
    else
        HDmemset(labellist, '\0', (int32)maxlen * (int32)listsize);

    /* find all refs for this tag; store them in reflist */
    nrefs = Hnumber(file_id, tag);         /* how many times is tag in file? */
    if (nrefs == FAIL) {
        Hclose(file_id); return FAIL;
    }

    aid = Hstartread(file_id, tag, DFREF_WILDCARD);   /* set search for refs */
    if (aid == FAIL) {
        Hendaccess(aid); Hclose(file_id); return FAIL;
    }

    for ( i=0, j=0; i<nrefs && j<listsize; i++) {
        if (HQuerytagref(aid, (uint16*)NULL, &ref) == FAIL) {
            Hendaccess(aid); Hclose(file_id); return FAIL;
        }
        if (i >= startpos-1)
            reflist[j++] = ref;         /* store next ref in reflist */
        Hnextread(aid, tag, DFREF_WILDCARD, DF_CURRENT);
    }
    nrefs = j;
    Hendaccess(aid);

        /* get directory of all labels */

    nlabs = Hnumber(file_id, DFTAG_DIL);
    if (nlabs != 0)  {
       if (DFANdir[DFAN_LABEL]==NULL) {          /* if no dir info create dir */
          if (0== DFANIlocate(file_id, DFAN_LABEL, 0, 0)){
             Hendaccess(aid); Hclose(file_id); return FAIL;
          }
       }

        lp = labellist;

       /* Look through all labels. Get those that correspond to the tag,
           and match them with corresponding tag/refs in the reflist.      */

    for (p = DFANdir[DFAN_LABEL]; p!=NULL; p=p->next) { /* for each ann dir */
        for (i=0; i<p->nentries; i++) {              /* for each tag in dir */
            if (p->entries[i].datatag==tag) {        /* if this tag==our tag */

                aid = Hstartread(file_id, DFTAG_DIL, p->entries[i].annref);
                if (aid == FAIL) {
                    Hendaccess(aid); Hclose(file_id); return FAIL;
                }
                if ((int32)FAIL == Hread(aid, (int32) 4, labeldi)) { /* data tag/ref */
                    Hendaccess(aid); Hclose(file_id); return FAIL;
                }
                    /* look for corresponding ref in reflist */
                for (k=0; k<nrefs && p->entries[i].dataref != reflist[k];k++)
                     ;
                if (k < nrefs) {               /* if ref found */

                    lp = labellist + k*maxlen;      /* get pos to copy to */

                        /* note len on read may be too big, but OK for DFread */
                    len = Hread(aid, (int32) (maxlen-1), lp);
                    if (len == FAIL) { 
                        Hendaccess(aid); Hclose(file_id); return FAIL;
                    }
                        /* ret now contains actual length read */
                        /* pad with blanks for Fortran; add null for C */
                    if (isfortran) 
                        while (len++ < maxlen) lp[len] = ' ';
                    else 
                        lp[len] = '\0';
                }
                Hendaccess(aid);
               }  /* tag == our tag  */
           }      /* for each tag in dir  */
       } /* for each ann dir  */
    }   /* nlabs != 0  */
    if (FAIL == Hclose(file_id))       /* close file */
        return FAIL; 
    return(nrefs);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIaddfann
 * Purpose: add file annotation (file ID or file description)
 * Inputs:  file_id: pointer to HDF file
 *          ann: annotation to write to file
 *          annlen: length of annotation
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: HDF general purpose routines
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFANIaddfann(int32 file_id, char *ann, int32 annlen, int type)
#else
int DFANIaddfann(file_id, ann, annlen, type)
    int32 file_id;
    char *ann;
    int32 annlen;
    int type;
#endif 
{
    char *FUNC="DFANIaddfann";
    uint16 anntag, annref;

    HEclear();
    if (!ann) { HERROR(DFE_BADPTR);  return FAIL; }

    anntag = (uint16)((type==DFAN_LABEL) ? DFTAG_FID : DFTAG_FD);

    annref = Hnewref(file_id);
    if (annref==0) return FAIL;

        /* write out annotation */
    if (FAIL == Hputelement(file_id, anntag, annref, (uint8 *) ann, annlen) )
        return FAIL;

    Lastref = annref;         /* remember ref last accessed */
    return(0);
}


/*-----------------------------------------------------------------------------
 * Name:    DFANIgetfannlen
 * Purpose: get length of next file annotation (file ID or file description)
 * Inputs:  file_id: pointer to HDF file
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 *          isfirst: 1: start with first one; 0: get next one
 * Returns: On success: length of annotation; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: HDF general purpose routines
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANIgetfannlen(int32 file_id, int type, int isfirst)
#else
int32 DFANIgetfannlen(file_id, type, isfirst)
int32 file_id;
int type;
int isfirst;
#endif 
{
    char *FUNC="DFANIgetfannlen";
    uint16 anntag, annref;
    int32 aid;
    int32 length;

    HEclear();

    /* Identify tag for this "type" of access; determine which ref to key on. */
    if (type == DFAN_LABEL) {
        anntag = DFTAG_FID;
        annref = (uint16)((isfirst == 1) ? DFREF_WILDCARD : Next_label_ref);
    } else {
        anntag = DFTAG_FD;
        annref = (uint16)((isfirst == 1) ? DFREF_WILDCARD : Next_desc_ref);
    }
    aid = Hstartread(file_id, anntag, annref);
    if (aid == FAIL) {
        Hendaccess(aid); return FAIL;
    }
    if (FAIL == Hinquire(aid, (int32*)NULL, (uint16*)NULL, &annref, &length ,
                         (int32*)NULL, (int32*)NULL,  (int16*)NULL, (int16*)NULL)){
        Hendaccess(aid); return FAIL;
    }
    if (type == DFAN_LABEL)         /* prepare for next call */
        Next_label_ref = annref;
    else
        Next_desc_ref = annref;

    Hendaccess(aid);
    Lastref = annref;             /* remember ref last accessed */ 

    if (length >= 0)         /* (length == 0) => no length found */
        return length;
    else {
        HERROR(DFE_NOMATCH); 
        return FAIL;
    }
}

/*-----------------------------------------------------------------------------
 * Name:    DFANIgetfann
 * Purpose: get next file annotation (file ID or file description)
 * Inputs:  file_id: pointer to HDF file
 *          ann: annotation to write to file
 *          annlen: length of annotation
 *          maxlen: max allowable length for annotation
 *          type: DFAN_LABEL for labels, DFAN_DESC for descriptions
 *          isfirst: 1: start with first one; 0: get next one
 * Returns: On success: length of annotation; On failure: -1, with DFerror set
 * Users:   HDF HLL users, utilities, other routines
 * Invokes: HDF general purpose routines
 * Remarks: If maxlen not great enough, ann is truncated to maxlen-1 chars
 * Bugs:    If ref is high possible ref value, setting of Next_label_ref or
            Next_desc_ref behave unpredictably. 
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int32 DFANIgetfann(int32 file_id, char *ann, int32 maxlen, int type,
                          int isfirst)
#else
int32 DFANIgetfann(file_id, ann, maxlen, type, isfirst)
int32 file_id;
char *ann;
int32 maxlen;
int type;
int isfirst;
#endif 
{
    char *FUNC="DFANIgetfann";
    uint16 anntag, annref;
    int32 length, aid;

    HEclear();

    if (!ann) {
        HERROR(DFE_BADPTR); return FAIL;
    }
    /* Identify tag for this "type" of access; determine which ref to key on. */
    if (type == DFAN_LABEL) {
        anntag = DFTAG_FID;
        annref = (uint16)((isfirst == 1) ? DFREF_WILDCARD : Next_label_ref);
    } else {
        anntag = DFTAG_FD;
        annref = (uint16)((isfirst == 1) ? DFREF_WILDCARD : Next_desc_ref);
    }

    aid = Hstartread(file_id, anntag, annref);
    if (aid == FAIL) {
        Hendaccess(aid); return FAIL;
    }
    if (FAIL == Hinquire(aid, (int32*)NULL, (uint16*)NULL, &annref, &length ,
                         (int32*)NULL, (int32*)NULL, (int16*)NULL, (int16*)NULL) ){
        Hendaccess(aid); return FAIL;
    }
    length = (length > maxlen) ?maxlen : length; /* truncate if too long */

    if ((int32)FAIL == Hread(aid, length, (uint8 *) ann) )    /* get the annotation */
        return FAIL;

    if(length > maxlen - 1) length = maxlen - 1;

    ann[length] = '\0';

    Lastref = annref;                          /* remember ref last accessed */ 

        /* prepare for next call */
    if (FAIL == Hnextread(aid, anntag, DFREF_WILDCARD, DF_CURRENT) )
    {                           /* If no more of them, set Next_ ???_ref */
        if (type == DFAN_LABEL) /*    to one higher than current value   */
            Next_label_ref++;   /*    so that next call will fail.       */ 
        else
            Next_desc_ref++;
    } else {                    /* Otherwise save the next ref */
        if (FAIL == Hinquire(aid, (int32*)NULL,(uint16*)NULL, &annref, 
                                  (int32*)NULL, (int32*)NULL, (int32*)NULL,  
                                  (int16*)NULL, (int16*)NULL) )  {
            Hendaccess(aid);
            return FAIL;
        }
        if (type == DFAN_LABEL)
            Next_label_ref = annref;
        else
            Next_desc_ref = annref;
    }

    Hendaccess(aid);
    return(length);                /* return length of label */
}


