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

/*
** FILE
**	dfstubs.c
**	V3.X stubs for V4.0 "H-level" I/O routines.  First implemented: V3.2
** EXPORTED ROUTINES
**	*DFopen -- open HDF file
**	*DFclose -- close HDF file
**	*DFdescriptors -- return a list of the data descriptors in the HDF file
**	*DFnumber -- count the number of occurrances of a given tag in HDF file
**	*DFsetfind -- set up a search
**	*DFfind -- search for tag/ref combination
**	DFaccess -- set up a read/write on a data element
**	DFstart -- set up a read/write on a data element
**	DFread -- read a portion of a data element
**	DFseek -- seek a new position within a data element
**	DFwrite -- write a portion of a data element
**	DFupdate -- write out updated DDs to HDF file
**	*DFstat -- provide status information about HDF file
**	*DFgetelement -- read an entire data element
**	*DFputelement -- write an entire data element
**	*DFdup -- create an additional descriptor for a data element
**	*DFdel -- delete a data element
**	*DFnewref -- get an unused reference number
**	*DFishdf -- is this an HDF file?
**	*DFerrno -- return value of DFerror
** AUTHOR
**	Doug Ilg
*/
#include "dfstubs.h"
#include "df.h"

#define CKMALLOC( x, ret) { if (!x) { DFerror = DFE_NOSPACE; return(ret); } }

#define CKSEEK(x,y,z, ret)  {  if (DF_SEEK( x,(long)y,z) <0) \
                {DFerror = DFE_SEEKERROR; return(ret); } }

#define CKSEEKEND(x,y,z, ret)   {  if (DF_SKEND( x,(long)y,z) <0) \
                {DFerror = DFE_SEEKERROR; return(ret); } }

#ifdef VMS
#define CKREAD(x,y,z,f, ret)    { \
                int32 currfileposn; \
                currfileposn = DF_TELL(f); \
                if (DF_READ( (char*)x, (int)(y), (int)(z), (f))<0) \
                { DFerror = DFE_READERROR; return(ret); } \
                DF_SEEK(f, (long) (currfileposn + y*z), 0); \
                }
#else /*VMS*/
#define CKREAD(x,y,z,f, ret)    { \
                if (DF_READ( (char*)x, (int)(y), (int)(z), (f))<0) \
                { DFerror = DFE_READERROR; return(ret); } \
                }
#endif /*VMS*/

#define CKWRITE(x,y,z,f, ret)   { if (DF_WRITE( (char*)x, (int)y, (int)z,f)<0) \
                {DFerror = DFE_WRITEERROR; return(ret); } }

/*
 *  Important Internal Variables
 */
static DF *DFlist=NULL;         /* pointer to list of open DFs */
#ifdef PERM_OUT
static int DFinuse=0;           /* How many are currently in use */
static uint16 DFmaxref;         /* which is the largest ref used? */
static unsigned char *DFreflist=NULL; /* list of refs in use */
static char patterns[] = {0x80, 0x40, 0x20, 0x10, 0x08,
                                       0x04, 0x02, 0x01};
#endif /* PERM_OUT */

/*
** NAME
**	DFopen -- open HDF file
** USAGE
**	DF *DFopen(name, access, ndds)
**	char* name;		IN: name of file to open
**	int access;		IN: DFACC_READ, DFACC_WRITE, DFACC_CREATE,
**				    DFACC_ALL
**	int ndds;		IN: number of DDs in a block
** RETURNS
**	DF ptr to open file on success, NULL on failure with DFerror set
** DESCRIPTION
**	Open an HDF file, if it exists.  If file does not exist and write
**	access requested, create file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	The pointer returned by DFopen is NOT a reference to a DF.  It is
**	just a place keeper for the new type of file handle.  Any program that
**	relies on the contents of a DF returned by DFopen must be re-written.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
DF *DFopen(char *name, int access, int ndds)
#else
	DF *
DFopen(name, access, ndds)
    char *name;
    int access;
    int ndds;
#endif /* PROTOTYPE */
{
    if (DFIcheck(DFlist) == 0) {
        DFerror = DFE_TOOMANY;
        return(NULL);
    }
    else
        DFerror = DFE_NONE;

    DFaccmode = access | DFACC_READ;
    DFid = Hopen(name, DFaccmode, (int16)ndds);

    if(DFid == -1) {
        DFerror = HEvalue(1);
        return(NULL);
    }
    else {
    	/*
        DFlist = makedf(DFid);
	*/
        DFlist = (DF *)&DFid;
        return(DFlist);
    }
}

/*
** NAME
**	DFclose -- close HDF file
** USAGE
**	int DFclose(dfile)
**	DF *dfile;		IN: pointer to an open DF file
** RETURNS
**	0 on success, -1 on failure with DFerror set
** DESCRIPTION
**	Write out updated DDs; close DF file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFclose(DF *dfile)
#else
	int
DFclose(dfile)
    DF *dfile;
#endif /* PROTOTYPE */
{
    int stat;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(FAIL);
    }
    else
        DFerror = DFE_NONE;

    if (DFelstat == DFEL_RESIDENT) {
        Hputelement(DFid, acc_tag, acc_ref, (unsigned char *)DFelement, DFelsize);
        HDfreespace(DFelement);
    }
    else
        Hendaccess(DFaid);

    if (search_stat == DFSRCH_OLD) {
        Hendaccess(search_aid);
        search_aid = 0;
    }

    stat = Hclose(DFid);
    if(stat == 0) {
        dfile = 0;
        DFlist = (DF *)NULL;
        DFid = 0;
        DFaccmode = 0;
    } else {
        DFerror = HEvalue(1);
    }

    return(stat);
}

/*
** NAME
**	DFdescriptors -- return a list of the data descriptors in the file
** USAGE
**	int DFdescriptors(dfile, ptr, begin, num)
**	DF *dfile;		IN: pointer to an open DF file
**	DFdesc ptr[];		IN: pointer to space for the list of DDs
**	int begin;		IN: starting DD number
**	int num;		IN: number of entries
** RETURNS
**	number of DDs returned in the list
** DESCRIPTION
**	Fills in a list of all DDs in the file beginning with DD begin and
**	including a maximum of num entries.  The number of DDs actually entered
**	into the list is returned.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFdescriptors(DF *dfile, DFdesc ptr[], int begin, int num)
#else
	int
DFdescriptors(dfile, ptr, begin, num)
    DF *dfile;
    DFdesc ptr[];
    int begin;
    int num;
#endif /* PROTOTYPE */
{
    int i, stat;
    int32 aid;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;
    
    aid = Hstartread(DFid, DFTAG_WILDCARD, DFREF_WILDCARD);

    if (aid == FAIL) {
        DFerror = HEvalue(1);
        return(-1);
    }
    
    for (i = 2; i <= begin; i++) {
        stat = Hnextread(aid, DFTAG_WILDCARD, DFREF_WILDCARD, DF_CURRENT);
        if (stat == FAIL) {
            DFerror = HEvalue(1);
            return(-1);
        }
    }
    
    Hinquire(aid, NULL, &ptr[0].tag, &ptr[0].ref, &ptr[0].length,
	     &ptr[0].offset, NULL, NULL, NULL);

    for (i = 1; i < num; i++) {
        stat = Hnextread(aid, DFTAG_WILDCARD, DFREF_WILDCARD,  DF_CURRENT);
        if (stat == FAIL)
            return (i);
        Hinquire(aid, NULL, &ptr[i].tag, &ptr[i].ref, &ptr[i].length,
             &ptr[i].offset, NULL, NULL, NULL);
    }
    Hendaccess(aid);

    return(i);
}



/*
** NAME
**	DFnumber -- return number of occurrences of given tag in the HDF file
** USAGE
**	DFnumber(dfile, tag)
**	DF *dfile;		IN: pointer to open DF file
**	uint16 tag;		IN: tag to count occurrences of
** RETURNS
**	Number of occurrences on success, -1 on failure with DFerror set.
** DESCRIPTION
**	Returns the number of occurrences of the specified tag in the HDF file.
**	If tag is DFTAG_WILDCARD, all tags are counted.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFnumber(DF *dfile, uint16 tag)
#else
	int
DFnumber(dfile, tag)
    DF *dfile;
    uint16 tag;
#endif /* PROTOTYPE */
{
    int32 aid;
    int num, stat;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;
    
    aid = Hstartread(DFid, tag, DFREF_WILDCARD);
    if (aid == FAIL)
        return(0);
    
    num = 0;
    for (stat = 0; stat == 0; num++)
        stat = Hnextread(aid, tag, DFREF_WILDCARD, DF_CURRENT);
    Hendaccess(aid);
    return(num);
}


/*
** NAME
**	DFsetfind -- set up parameters for a wildcard find
** USAGE
**	int DFsetfind(dfile, tag, ref)
**	DF *dfile;		IN: pointer to open DF file
**	uint16 tag;		IN: tag of element to search for (0 is wild)
**	uint16 ref;		IN: ref of element to search for (0 is wild)
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	Sets up parameters for a wildcard find on a tag/ref pair.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFsetfind(DF *dfile, uint16 tag, uint16 ref)
#else
	int
DFsetfind(dfile, tag, ref)
    DF *dfile;
    uint16 tag;
    uint16 ref;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    search_tag = tag;
    search_ref = ref;

    search_stat = DFSRCH_NEW;

    return(0);
}


/*
** NAME
**	DFfind -- perform wildcard searches
** USAGE
**	int DFfind(dfile, ptr)
**	DF *dfile;		IN: pointer to an open DF file
**	DFdesc *ptr;		IN: pointer to put in DD when found
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	If desired tag/ref is found, its DD is copied to *ptr.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFfind(DF *dfile, DFdesc *ptr)
#else
	int
DFfind(dfile, ptr)
    DF *dfile;
    DFdesc *ptr;
#endif /* PROTOTYPE */
{
    int stat;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    if (search_stat == DFSRCH_NEW) {
        search_aid = Hstartread(DFid, search_tag, search_ref);
        search_stat = DFSRCH_OLD;
        stat = 0;
    } else {
        stat = Hnextread(search_aid, search_tag, search_ref, DF_CURRENT);
    }

    if ((search_aid == FAIL) || (stat == FAIL)) {
        DFerror = DFE_NOMATCH;
        ptr->tag = 0;
        ptr->ref = 0;
        return(-1);
    }

    Hinquire(search_aid, NULL, &ptr->tag, &ptr->ref, &ptr->length, &ptr->offset,
	     NULL, NULL, NULL);

    return(0);
}


/*
** NAME
**	DFaccess -- set up a read/write on a data element
** USAGE
**	int DFaccess(dfile, tag, ref, access)
**	DF *dfile;		IN: pointer to open HDF file
**	uint16 tag;		IN: tag of element
**	uint16 ref;		IN: ref number of element
**	char *access;		IN: "r", "w", or "a" (read, write, append)
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	Set up read or write access on data element.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	This function needs to call DFupdate and Hendaccess if there is already
**	an active access element with a different tag/ref.
**	Also, set up globals "acc_tag" and "acc_ref" to keep tabs on the data
**	being referenced, and "in_mem" to keep track of whether the data for
**	an element to be appended to has been read into memory.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFaccess(DF *dfile, uint16 tag, uint16 ref, char *access)
#else
	int
DFaccess(dfile, tag, ref, access)
    DF *dfile;
    uint16 tag;
    uint16 ref;
    char *access;
#endif /* PROTOTYPE */
{
    int accmode;
    /*
    DFdle *ptr;
    int dle_num, index, i;
    */

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    switch(*access) {
	case 'r': accmode = DFACC_READ;
		  break;
	case 'w': accmode = DFACC_WRITE;
		  if (((DFaccmode & DFACC_WRITE) == 0) &&
		      ((DFaccmode & DFACC_CREATE) == 0)) {
		      DFerror = DFE_BADACC;
		      return(-1);
		  }
		  break;
	case 'a': accmode = DFACC_APPEND;
		  if (((DFaccmode & DFACC_WRITE) == 0) &&
		      ((DFaccmode & DFACC_CREATE) == 0)) {
		      DFerror = DFE_BADACC;
		      return(-1);
		  }
		  break;
	default:  DFerror = DFE_BADACC;
		  return(-1);
    }

/* test
    if (((tag != acc_tag) || (ref != acc_ref)) || (accmode != DFelaccmode))
        if (DFelstat == DFEL_RESIDENT) {
	    Hputelement(DFid, acc_tag, acc_ref, DFelement, DFelsize);
	    free(DFelement);
        }
	else
	    Hendaccess(DFaid);
test */

    acc_tag = tag;
    acc_ref = ref;
    DFelaccmode = accmode;
    DFelstat = DFEL_ABSENT;
    DFelseekpos = 0;
    DFelsize = 0;

    switch(*access) {
	case 'r': 
		  DFelsize = Hlength(DFid, acc_tag, acc_ref);
		  if (DFelsize <= 0) {
		      DFIclearacc();
                      DFerror = HEvalue(1);
		      return(-1);
		  }
		  /* test
		  DFaid = Hstartread(DFid, acc_tag, acc_ref);
		  if (DFaid != FAIL) {
		      Hinquire(DFaid, (int32*)NULL, (uint16*)NULL, (uint16*)NULL,
			       &DFelsize, (int32*)NULL, (int32*)NULL,
			       (int32*)NULL, (int32*)NULL);
		      inq_accid(DFaid, &dle_num, &index, &(dfile->up_access));
		      Hendaccess(DFaid);
		      ptr = dfile->list;
		      for (i=0; i<dle_num; i++)
			  ptr = ptr->next;
		      dfile->up_dd = &(ptr->dd[index]);
		  } else {
		      DFIclearacc();
                      DFerror = HEvalue(1);
		      return(-1);
		  }
		  test */
		  break;
	/* _maybe_ treat 'w' and 'a' in the same general 'a'-way */
	case 'w':
		  DFelsize = Hlength(DFid, acc_tag, acc_ref);
		  if (DFelsize == FAIL) {
		      DFelsize = 0;
		  } else
		      DFelstat = DFEL_RESIDENT;
		  break;
	case 'a': 
		  DFelsize = Hlength(DFid, acc_tag, acc_ref);
		  if (DFelsize == FAIL) {
		      DFIclearacc();
                      DFerror = HEvalue(1);
		      return(-1);
		  }
		  DFelseekpos = DFelsize;
		  break;
    }

    return(0);
}



#ifdef PROTOTYPE
PRIVATE int DFIclearacc(void)
#else
	PRIVATE int
DFIclearacc()
#endif /* PROTOTYPE */
{
    Hendaccess(DFaid);
    DFaid = 0;
    acc_tag = 0;
    acc_ref = 0;
    DFelsize = 0;
    DFelseekpos = 0;
    DFelstat = DFEL_ABSENT;
    DFelement = NULL;

    return(0);
}


/*
** NAME
**	DFstart -- set up a read/write on an access element
** USAGE
**	int DFstart(dfile, tag, ref, access)
**	DF *dfile;		IN: pointer to open  DF file
**	uint16 tag;		IN: tag of element
**	uint16 ref;		IN: ref number of element
**	char *access;		IN: "r", "w", ro "a" (read, write, append)
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	Set up a read or write access on data element.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFstart(DF *dfile, uint16 tag, uint16 ref, char *access)
#else
	int
DFstart(dfile, tag, ref, access)
    DF *dfile;
    uint16 tag;
    uint16 ref;
    char *access;
#endif /* PROTOTYPE */
{
    return(DFaccess(dfile, tag, ref, access));
}
    


/*
** NAME
**	DFread -- read a portion of a data element
** USAGE
**	int32 DFread(dfile, ptr, len)
**	DF *dfile;		IN: pointer to open DF file
**	char *ptr;		IN: pointer to space to read data into
**	int32 len;		IN: number of bytes to read
** RETURNS
**	number of bytes read on success, -1 on failure
** DESCRIPTION
**	Read bytes from a DF file (part of element specified by DFaccess)
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	Space for data is assumed to be pre-allocated.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int32 DFread(DF *dfile, char *ptr, int32 len)
#else
	int32
DFread(dfile, ptr, len)
    DF *dfile;
    char *ptr;
    int32 len;
#endif /* PROTOTYPE */
{
    int32 stat;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    DFaid = Hstartread(DFid, acc_tag, acc_ref);
    stat = Hseek(DFaid, DFelseekpos, 0);
    if (stat == FAIL) {
        DFerror = HEvalue(1);
        return(-1);
    }

    stat = Hread(DFaid, len, (unsigned char *)ptr);
    Hendaccess(DFaid);

    if (stat == FAIL) {
        DFerror = HEvalue(1);
        return(-1);
    }
    else {
        DFelseekpos += stat;
        return(stat);
    }
}



/*
** NAME
**	DFseek -- seek a new position within a data element
** USAGE
**	int32 DFseek(dfile, offset)
**	DF *dfile;		IN: pointer to open DF file
**	int32 offset;		IN: offset from beginning of element
** RETURNS
**	offset of actual position seek'ed to from beginning of element
** DESCRIPTION
**	Seek position within element specified by DFaccess.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int32 DFseek(DF *dfile, int32 offset)
#else
	int32
DFseek(dfile, offset)
    DF *dfile;
    int32 offset;
#endif /* PROTOTYPE */
{
    int stat;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;
    
    /* assuming no blank space can be forced by seeking past end of element
       and writing more data */
    if (offset > DFelsize) {
        DFerror = DFE_BADSEEK;
        return(-1);
    } else {
        stat = Hseek(DFaid, offset, DF_START);
        if (stat == FAIL) {
            DFerror = HEvalue(1);
            return(-1);
        }
        DFelseekpos = offset;
    }

    return(0);
}


/*
** NAME
**	DFwrite -- write a portion of a data element
** USAGE
**	int32 DFwrite(dfile, ptr, len)
**	DF *dfile;		IN: pointer to open DF file
**	char *ptr;		IN: pointer to data to be written
**	int32 len;		IN: number of bytes to be written
** RETURNS
**	number of bytes written on success, -1 on failure
** DESCRIPTION
**	Write bytes to DF file (part of element specified by DFaccess)
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	This function should check the access mode in DFaccmode.  On write
**	access, if(!in_mem) Hstartwrite, Hwrite, and set in_mem, otherwise just
**	Hwrite.  On append access, if(!in_mem) Hstartread, Hinquire(oldsize),
**	malloc(oldsize+writesize), Hread to malloc'd area, copy write request
**	to end of malloc'd area, set in_mem, otherwise, realloc(area+writesize)
**	copy write request to end of area.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int32 DFwrite(DF *dfile, char *ptr, int32 len)
#else
	int32
DFwrite(dfile, ptr, len)
    DF *dfile;
    char *ptr;
    int32 len;
#endif /* PROTOTYPE */
{
    int32 size, ret, newlen;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    if ((DFelaccmode != DFACC_WRITE) && (DFelaccmode != DFACC_APPEND)) {
        DFerror = DFE_BADACC;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    size = DFelseekpos + len;
    if (DFelaccmode == DFACC_WRITE) {
        if (DFelstat == DFEL_ABSENT) {
            Hendaccess(DFaid);
            DFaid = Hstartwrite(DFid, acc_tag, acc_ref, len);
            Hseek(DFaid, DFelseekpos, DF_START);
            ret = Hwrite(DFaid, len, (unsigned char *)ptr);
        } else {
            if (size <= DFelsize) {
                Hendaccess(DFaid);
                DFaid = Hstartwrite(DFid, acc_tag, acc_ref, len);
                Hseek(DFaid, DFelseekpos, DF_START);
                ret = Hwrite(DFaid, len, (unsigned char *)ptr);
            } else {
                DFerror = DFE_NOTENOUGH;
                return(-1);
            }
        }
    } else {
        newlen = size - Hlength(DFid, acc_tag, acc_ref);
        Hendaccess(DFaid);
        DFaid = HLcreate(DFid, acc_tag, acc_ref, newlen, (int32)4);
        Hseek(DFaid, DFelseekpos, DF_START);
        ret = Hwrite(DFaid, len, (unsigned char *)ptr);
    }

    Hendaccess(DFaid);
    DFelseekpos += len;
    DFelsize = size;
    DFelstat = DFEL_RESIDENT;

    return(ret);
}


/*
** NAME
**	DFupdate -- write out updated DDs to HDF file
** USAGE
**	int DFupdate(dfile)
**	DF *dfile;		IN: pointer to open DF file
** RETURNS
**	0 on success, -1 on failure with DFerror set.
** DESCRIPTION
**	This function only checks for valid input and returns.  It is included
**	solely for compatibility with older programs.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	This function does nothing but check for valid input.
**	However, this function should check to see if an appended-to data
**	element is in memory and, if it is, write it out.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFupdate(DF *dfile)
#else
	int
DFupdate(dfile)
    DF *dfile;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    /* test
    if (DFelstat == DFEL_RESIDENT) {
	Hputelement(DFid, acc_tag, acc_ref, DFelement, DFelsize);
	free(DFelement);
	DFIclearacc();
    }
    test */

    return(0);
}


/*
** NAME
**	DFstat -- provide status information about HDF file
** USAGE
**	int DFstat(dfile, dfinfo)
**	DF *dfile;		IN: pointer to open DF file
**	struct DFdata *dfinfo;	IN: pointer to space for info
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	Fill dfinfo with status information about the HDF file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	Currently, only the HDF version number is returned in dfinfo.
**	Actually, nothing happens here now.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFstat(DF *dfile, DFdata *dfinfo)
#else
	int
DFstat(dfile, dfinfo)
    DF *dfile;
    DFdata *dfinfo;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    dfinfo = dfinfo;

    return(0);
}




/*
** NAME
**	DFgetelement -- read an entire data element
** USAGE
**  int32 DFgetelement(dfile, tag, ref, ptr)
**	DF *dfile;		IN: pointer to open DF file
**	uint16 tag;		IN: tag of element
**	uint16 ref;		IN: ref number of element
**	char *ptr;		IN: pointer to space for data element
** RETURNS
**	number of bytes read on success, -1 on failure
** DESCRIPTION
**	Reads in a data element from an HDF file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	Currently, this function returns 0 on success, not #bytes read.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int32 DFgetelement(DF *dfile, uint16 tag, uint16 ref, char *ptr)
#else
int32 DFgetelement(dfile, tag, ref, ptr)
    DF *dfile;
    uint16 tag;
    uint16 ref;
    char *ptr;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    /* test
    if (DFelstat == DFEL_RESIDENT) {
	Hputelement(DFid, acc_tag, acc_ref, DFelement, DFelsize);
	free(DFelement);
	DFIclearacc();
    }
    test */

    if (Hgetelement(DFid, tag, ref, (unsigned char *)ptr) == -1) {
        DFerror = HEvalue(1);
        return(-1);
    }
    else
	return(Hlength(DFid, tag, ref));
}


/*
** NAME
**	DFputelement -- write an entire data element
** USAGE
**	int DFputelement(dfile, tag, ref, ptr, len)
**	DF *dfile;		IN: pointer to open DF file
**	uint16 tag;		IN: tag of data element
**	uint16 ref;		IN: ref number of data element
**	char *ptr;		IN: pointer to data element
**	int32 len;		IN: length of data element
** RETURNS
**	Number of bytes written on success, -1 on failure
** DESCRIPTION
**	Write an entire data element to HDF file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int32 DFputelement(DF *dfile, uint16 tag, uint16 ref, char *ptr, int32 len)
#else
    int32
DFputelement(dfile, tag, ref, ptr, len)
    DF *dfile;
    uint16 tag;
    uint16 ref;
    char *ptr;
    int32 len;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    /* test
    if (DFelstat == DFEL_RESIDENT) {
	Hputelement(DFid, acc_tag, acc_ref, DFelement, DFelsize);
	free(DFelement);
	DFIclearacc();
    }
    test */

    if (Hputelement(DFid, tag, ref, (unsigned char *)ptr, len) == FAIL) {
        DFerror = HEvalue(1);
        return(-1);
    }
    else
        return(Hlength(DFid, tag, ref));
}


/*
** NAME
**	DFdup -- create an additional descriptor for a data element
** USAGE
**	int DFdup(dfile, itag, iref, otag, oref)
**	DF *dfile;		IN: pointer to open DF file
**	uint16 itag;		IN: new tag of data element
**	uint16 iref;		IN: new ref number of data element
**	uint16 otag;		IN: current tag of data element
**	uint16 oref;		IN: current ref number of data element
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	Add a new tag/ref for existing data.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFdup(DF *dfile, uint16 itag, uint16 iref, uint16 otag, uint16 oref)
#else
	int
DFdup(dfile, itag, iref, otag, oref)
    DF *dfile;
    uint16 itag;
    uint16 iref;
    uint16 otag;
    uint16 oref;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    if (Hdupdd(DFid, itag, iref, otag, oref) != 0) {
        DFerror = HEvalue(1);
        return(-1);
    }
    else
        return(0);
}


/*
** NAME
**	DFdel -- delete a data element
** USAGE
**	int DFdel(dfile, tag, ref)
**	DF *dfile;		IN: pointer to open DF file
**	uint16 tag;		IN: tag of element
**	uint16 ref;		IN: ref number of element
** RETURNS
**	0 on success, -1 on failure
** DESCRIPTION
**	Delete a data element from HDF file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
**	The data element is not actually deleted; it simply loses its DD.
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFdel(DF *dfile, uint16 tag, uint16 ref)
#else
	int
DFdel(dfile, tag, ref)
    DF *dfile;
    uint16 tag;
    uint16 ref;
#endif /* PROTOTYPE */
{
    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(-1);
    }
    else
        DFerror = DFE_NONE;

    if (Hdeldd(DFid, tag, ref) != 0) {
        DFerror = HEvalue(1);
        return(-1);
    }
    else
        return(0);
}


/*
** NAME
**	DFnewref -- get an unused reference number
** USAGE
**	uint16 DFnewref(dfile)
**	DF *dfile;		IN: pointer to open DF file
** RETURNS
**	unused reference number, or 0 if no reference numbers are free
** DESCRIPTION
**	Get an unused reference number.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
uint16 DFnewref(DF *dfile)
#else
	uint16
DFnewref(dfile)
    DF *dfile;
#endif /* PROTOTYPE */
{
    uint16 stat;

    if (DFIcheck(dfile) != 0) {
        DFerror = DFE_NOTOPEN;
        return(0);
    }
    else
        DFerror = DFE_NONE;

    stat = Hnewref(DFid);
    if (stat == 0xffff) {
        DFerror = HEvalue(1);
        return(0);
    }

    return(stat);
}


/*
** NAME
**	DFishdf -- is this an HDF file?
** USAGE
**	int DFishdf(filename)
**	char *filename;		IN: name of file to check
** RETURNS
**	0 if it is an HDF file, -1 if it is not.
** DESCRIPTION
**	Determine whether file is an HDF file.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFishdf(char *filename)
#else
	int
DFishdf(filename)
    char *filename;
#endif /* PROTOTYPE */
{
    int32 dummy;

    DFerror = DFE_NONE;

    dummy = Hopen(filename, DFACC_READ, 0);
    if (dummy == -1) {
        DFerror = HEvalue(1);
        return(-1);
    }
    else {
        Hclose(dummy);
        return(0);
    }
}


/*
** NAME
**	DFerrno -- return value of DFerror
** USAGE
**	int DFerrno()
** RETURNS
**	Value of DFerror.
** DESCRIPTION
**	Return value of DFerror.
** GLOBAL VARIABLES
** COMMENTS, BUGS, ASSUMPTIONS
** EXAMPLES
** REVISION LOG
*/
#ifdef PROTOTYPE
int DFerrno(void)
#else
	int
DFerrno()
#endif /* PROTOTYPE */
{
    return(DFerror);
}

#ifdef PERM_OUT
/*-----------------------------------------------------------------------------
 * Name:    DFIseedDDs
 * Purpose: read DDs in file into memory
 * Inputs:  dfile: pointer to open DF file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF systems programmers, DFopen
 *---------------------------------------------------------------------------*/

int DFIseedDDs(dfile)
DF *dfile;
{
    DFdle *list;
    DFddh ddh;
    int i,n;                        /* n = no. of DDs in block */

    DFerror = DFE_NONE;

    if (dfile->list) {
        DFerror = DFE_SEEDTWICE;    /* ### NOTE: Internal error! */
        return(-1);
    }

    dfile->list= (DFdle *) DFIgetspace(sizeof(DFdle));
    /* includes one DD - unused */
    CKMALLOC( dfile->list, -1);

    list=dfile->list;
    list->next=NULL;                /* No other lists (yet) */
    list->ddh.next= (int32)4L;      /* next is at 4 in file */
    list->ddh.dds= -1;              /* flag so this is not written */

    DFmaxref = 0;                   /* largest ref found till now is 0 */

    while (list->ddh.next) {        /* while more headers to read */
        CKSEEK( dfile->file, list->ddh.next, 0, -1);

                            /* read headers */
#ifdef DF_STRUCTOK
        CKREAD( &ddh, sizeof(DFddh), 1, dfile->file, -1);
#else /*DF_STRUCTOK*/
        {
            register  char *p;
            p = DFtbuf;
            CKREAD( DFtbuf, 6, 1, dfile->file, -1);     /* 6 = size of header */
            INT16READ( p, ddh.dds);
            INT32READ( p, ddh.next);
        }
#endif /*DF_STRUCTOK*/
        n   =ddh.dds;

        /* read in DDs */
        list->next= (DFdle *)
            DFIgetspace((unsigned)
                        (sizeof(DFdle)+ (n-1)* sizeof(DFdd)));
                                /* note space for 1 DD included in DLE */
        CKMALLOC( list->next, -1);
        list=list->next;
        list->next=NULL;

        HDmemcpy((char*)&(list->ddh), (char*)&ddh, sizeof(DFddh) ); /* Copy ddh */

        if (n) {
#ifdef DF_STRUCTOK
            CKREAD( &list->dd[0], sizeof(DFdd), n, dfile->file, -1);
            /* load DD's */
#else /*DF_STRUCTOK*/
            {
                register  char *p;
                p = DFtbuf;
                CKREAD( DFtbuf, n*12, 1, dfile->file, -1);  /* 12=size of DD */
                for (i=0; i<n; i++) {
                    UINT16READ( p, list->dd[i].tag);
                    UINT16READ( p, list->dd[i].ref);
                    INT32READ( p, list->dd[i].offset);
                    INT32READ( p, list->dd[i].length);
                }
            }
#endif /*DF_STRUCTOK*/
        }
                /* Remember highest ref found - ignore MTs */
        for (i=0; i<n; i++)
            if ((list->dd[i].ref > DFmaxref) && (list->dd[i].tag != DFTAG_MT))
                                     DFmaxref = list->dd[i].ref;
    }
    return(0);
}
#endif /* PERM_OUT */

/*-----------------------------------------------------------------------------
 * Name:    DFIcheck
 * Purpose: check if dfile argument represents a valid DF file
 * Inputs:  dfile: pointer to open DF file
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   HDF systems programmers, several routines in this file
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
PRIVATE int DFIcheck(DF *dfile)
#else
PRIVATE int DFIcheck( dfile)
DF *dfile;
#endif  /* PROTOTYPE */
{
    DFerror = DFE_NONE;

    if ((dfile != (DF *)&DFid) || (DFid == 0)) {
        DFerror = DFE_DFNULL;
        return(-1);
    }

    if ((DFaccmode & DFACC_ALL) != DFaccmode) {
        DFerror = DFE_BADACC;
        return(-1);
    }
    else
        return(0);

    /* test
    if (!dfile) {
        DFerror = DFE_DFNULL;
        return(-1);
    }

    if ((dfile->access & DFACC_ALL) != dfile->access)
        DFerror = DFE_BADACC;

    if ((dfile->type >1) || (dfile->type <-1))
        DFerror = DFE_ILLTYPE;

    if (!dfile->list)
        DFerror= DFE_BADDDLIST;

    if (DFerror)
        return(-1);
    else
        return(0);
    test */

}

#ifdef PERM_OUT
/*-----------------------------------------------------------------------------
 * Name:    DFIfind
 * Purpose: perform wildcard searches
 * Inputs:  dfile: pointer to open DF file
 *          tag, ref: tag, ref (possibly wildcard) being searched for
 *          isfirst: 1 if first call to DFIfind for this tag/ref, else 0
 *          ltag, lref: last tag and ref returned for this search,
 *              don't care if isfirst set
 *          cDLEp, cddp: pointers to DLE and DD number to return matched DD in
 * Returns: 0 on success, -1 on failure
 *          if success, cDLEp and cddp are set to point to matched DD
 * Users:   HDF system programmers, DFfind, HDF utilities, many routines
 * Remarks: The searching algorithm is a little complex.  It returns entries
 *          in the sorting order of refs, then tags.  Even after a candidate
 *          is found, searching continues till best candidate found.  Best way
 *          to check if conditions: work it out independently for yourself!
 *---------------------------------------------------------------------------*/

int DFIfind( dfile, tag, ref, isfirst, ltag, lref, cDLEp, cddp)
DF *dfile;
DFdle **cDLEp;
int *cddp;
int isfirst;                            /* 1 if no prev search, 0 otherwise */
uint16 tag, ref, ltag, lref;
{
    DFdle *DLEp;
    int i, found=0;
    uint16 ctag=0, cref=0, wtag,wref; /* ctag, cref: tag, ref found so far */
                                      /* wtag, wref: tag, ref being checked */


    if (isfirst) {
        search_tag = tag;
        search_ref = ref;
    }

    DLEp=dfile->list;               /* start of DLE list */

    if (tag && ref) {               /* No wildcards */
        if (isfirst) {              /* if not already found */
            while (DLEp) {          /* go through list */
                for (i=0; i<DLEp->ddh.dds; i++) {       /* for all DDs */
                    if (DLEp->dd[i].tag==tag &&
                            DLEp->dd[i].ref==ref)
                        {*cDLEp=DLEp; *cddp=i; return(0);}
                    }
                DLEp=DLEp->next;
                }
            }
        }
    else if (tag && !ref)           /* wildcard ref */
        while (DLEp) {
            for (i=0; i<DLEp->ddh.dds; i++) {
                wtag=DLEp->dd[i].tag;
                wref=DLEp->dd[i].ref;
        /* condition = tag match, better than found so far (if any),
            follows what was returned last time (if any) */
                if ( (wtag==tag) && (!found || (wref<cref)) &&
                    (isfirst || (wref>lref)))
                    { ctag=wtag; cref=wref; *cDLEp=DLEp; *cddp=i;found=1;}
                }
            DLEp=DLEp->next;
            }
    else if (!tag && ref)           /* wildcard tag */
        while (DLEp) {
            for (i=0; i<DLEp->ddh.dds; i++) {
                wtag=DLEp->dd[i].tag;
                wref=DLEp->dd[i].ref;
                if ((wref==ref) && (isfirst || (wtag>ltag)) &&
                    (!found || (wtag<ctag)) )
                    { ctag=wtag; cref=wref; *cDLEp=DLEp; *cddp=i;found=1;}
                }
            DLEp=DLEp->next;
            }
    else if (!tag && !ref)          /* wildcard tag & ref */
        while (DLEp) {
            for (i=0; i<DLEp->ddh.dds; i++) {
                wtag=DLEp->dd[i].tag;
                wref=DLEp->dd[i].ref;
                if ((isfirst || (wref>lref) || (wref==lref && wtag>ltag)) &&
                    (!found || (wref<cref) || (wref==cref && wtag<ctag)) &&
                    (wtag!=DFTAG_NULL))         /* empty DDs are invisible */
                    { ctag=wtag; cref=wref; *cDLEp=DLEp; *cddp=i;found=1;}
                }
            DLEp=DLEp->next;
            }
    return(found-1);            /* 0 or -1 */
}


/*-----------------------------------------------------------------------------
 * Name:    DFIemptyDD
 * Purpose: find an empty DD to use, or create a block of DDs if necessary
 * Inputs:  dfile: pointer to open DF file
 * Returns: pointer to an empty DD
 * Invokes: DFIfind
 * Users:   HDF system programmers, DFaccess, DFdup
 *---------------------------------------------------------------------------*/


DFdd *DFIemptyDD(dfile)
DF *dfile;
{
    DFdle *cDLEp;
    int cdd;

    if (!DFIfind( dfile, DFTAG_NULL, DFREF_WILDCARD, 1, 0, 0, &cDLEp, &cdd))
        return(&(cDLEp->dd[cdd]));      /* there is an empty DD */

    else {          /* add new DDH block */
        int32 fpos;
        DFdle *p, *dle;
        DFddh ddh;
        DFdd dd;
        int j;
        char MYtbuf[12];                /* My own tbuf so that the content
                                           of DFtbuf will be preserved */

        CKSEEKEND( dfile->file, (long) 0, 2, NULL); /* go to end of df */
        fpos= (int32) DF_TELL(dfile->file);
        ddh.dds= dfile->defdds;             /* Initialize ddh */
        ddh.next= 0;
        dd.tag=DFTAG_NULL;                  /* and all DD's */
        dd.ref=0;
#ifdef DF_STRUCTOK
        CKWRITE( &ddh, sizeof(DFddh), 1, dfile->file, NULL);
#else /*DF_STRUCTOK*/
        {
            register  char *p;
            p = MYtbuf;
            INT16WRITE( p, ddh.dds);
            INT32WRITE( p, ddh.next);
            CKWRITE( MYtbuf, 6, 1, dfile->file, NULL);  /* 6 = size of header */
        }
#endif /*DF_STRUCTOK*/
        for (j=0; j<ddh.dds; j++) {
#ifdef DF_STRUCTOK
            CKWRITE( &dd, sizeof(DFdd),1, dfile->file, NULL);
#else /*DF_STRUCTOK*/
            {
                register  char *p;
                p = MYtbuf;
                UINT16WRITE( p, dd.tag);
                UINT16WRITE( p, dd.tag);
                INT32WRITE( p, dd.offset);
                INT32WRITE( p, dd.length);
                CKWRITE( MYtbuf, 12, 1, dfile->file, NULL); /* 12=size of dd */
            }
#endif /*DF_STRUCTOK*/
        }

        p=dfile->list;                      /* find end of list */
        while (p->next) p= p->next;

        p->ddh.next=fpos;                   /* new dd goes at end of file */
        dle=(DFdle *)
            DFIgetspace((unsigned)
                        (sizeof(DFdle)+(ddh.dds-1)*sizeof(DFdd)));
                            /* one dd included in dle */
        CKMALLOC(dle, NULL);
        p->next=dle;                        /* insert dle at end of list */
        dle->next=NULL;
        HDmemcpy((char*)&dle->ddh, (char*)&ddh, sizeof(DFddh));
        for (j=0; j<ddh.dds; j++)
            HDmemcpy((char*)&dle->dd[j], (char*)&dd, sizeof(DFdd));
        return(&(dle->dd[0]));
    }
#ifdef PC
    return(NULL);           /* dummy, for return value checking */
#endif /*PC*/
}


/* Simplified version without the overhead.  This is useful if you */
 /* know that the args are okay, and if you need to read many time */
 /* (like in a loop in DFSDIgetslice()) */
int32 DFIread( dfile, ptr, len)
DF *dfile;
char *ptr;
int32 len;
{
    int32 maxlen;
#ifdef VMS
    int32 totalread;
    int32 readsize;
#endif /*VMS*/
    maxlen = dfile->up_dd->length -
                ((int32) DF_TELL(dfile->file) - dfile->up_dd->offset);
    if (len>maxlen) len = maxlen;
    if (len<0) {            /* will also catch reads from beyond element */
        DFerror = DFE_BADLEN;
        return(-1);
    }

#ifdef VMS
    totalread = 0;
    while (totalread<len) {
        readsize = len - totalread;
        if (readsize>512) readsize = 512;
        CKREAD( &ptr[totalread], (int)readsize, 1, dfile->file, -1);
        totalread += readsize;
    }

#else /*VMS*/
    if (len) {      /* NOTE: cast to (int) will limit to 64K on 16 bit m/cs */
        CKREAD( ptr, (int) len, 1, dfile->file, -1);
    }
#endif /*VMS*/

    return(len);
}

/* Simplified version without the overhead.  This is useful if you */
 /* know that the args are okay, and if you need to seek many time */
 /* (like in a loop in DFSDIgetslice()) */
int32 DFIseek( dfile, offset)
DF *dfile;
int32 offset;
{
    CKSEEK( dfile->file, (long) dfile->up_dd->offset + offset, 0, -1);
    return(offset);
}
#endif /* PERM_OUT */

/*-----------------------------------------------------------------------------
 * Name:    DFIerr
 * Purpose: Close a file and return on error. save DFerror
 * Inputs:  dfile: pointer to HDF file to close
 * Returns: -1
 * Users:   HDF systems programmers, for error handling
 * Invokes: DFclose
 * Remarks: Used to centralize some error handling
 *---------------------------------------------------------------------------*/

#ifdef PROTOTYPE
int DFIerr(DF *dfile)
#else
int DFIerr(dfile)
DF *dfile;
#endif  /* PROTOTYPE */
{
    int saveerror;

    saveerror = DFerror;
    if (dfile!=NULL) (void) DFclose(dfile);
    DFerror = saveerror;
    return(-1);
}


/*-----------------------------------------------------------------------------
 * The following functions are stubs for the old routines from "dfkit.c".
 *---------------------------------------------------------------------------*/

#ifndef IBM6000
#include <ctype.h>
#endif

#ifdef PC
#ifdef WIN3
int32 DFIspaceleft(void)
{
/* return the largest amount of memory Windows can give us */
   return(GlobalCompact(0));
}
#else /* WIN3 */
int32 DFIspaceleft(void)
{
    return(HDspaceleft());
}
#endif /* WIN3 */
#endif /* PC */


#if defined PROTOTYPE
void *DFIgetspace(uint32 qty)
#else
void *DFIgetspace(qty)
uint32 qty;
#endif /* PROTOTYPE */
{
    void *ret;

    ret = HDgetspace(qty);
    DFerror = HEvalue(1);
    return(ret);
}

#if defined PROTOTYPE
void *DFIfreespace(void *ptr)
#else
void *DFIfreespace(ptr)
char *ptr;
#endif /* PROTOTYPE */
{
    return(HDfreespace(ptr));
}


#if defined PROTOTYPE
intn DFIc2fstr(char *str, int len)
#else
intn DFIc2fstr(str, len)
char* str;
int len;
#endif /* PROTOTYPE */
{
    return(HDc2fstr(str, len));
}

#if defined PROTOTYPE
char *DFIf2cstring(_fcd fdesc, intn len)
#else
char *DFIf2cstring(fdesc, len)
    _fcd fdesc;
    intn len;
#endif /* PROTOTYPE */
{
    return(HDf2cstring(fdesc, len));
}
