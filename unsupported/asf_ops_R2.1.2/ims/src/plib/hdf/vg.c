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

#include "vg.h"

/* Private functions */
PRIVATE int32 matchnocase
  PROTO((char *strx, char *stry));

/* ------------------------------------------------------------------
*	Vnewref
*	utility routine. returns a unique reference number.
*
*	RETURNS a unique ref (+ve unsigned 16-bit integer) ,
*	RETURNS 0 if error
*
*  undocumented
*
*/
#ifdef PROTOTYPE
uint16 vnewref (HFILEID f)		
#else
uint16 vnewref (f)		
HFILEID f;
#endif
{
	uint16 r;
	char * FUNC = "vnewref";

    r = (uint16) Hnewref (f);
    if (r == 0)
        HERROR(DFE_NOFREEDD);

	return (r);
} /* vnewref */

/* ================================================================== */

/* matchnocase -  (PRIVATE) compares 2 strings, ignoring case 
*	               returns TRUE if match, else FALSE
*/

#ifdef PROTOTYPE
PRIVATE int32 matchnocase (char *strx, char *stry)
#else
PRIVATE int32 matchnocase (strx, stry)
char *strx,*stry;
#endif
{
	int32 	i,nx,ny;

    nx = HDstrlen(strx);
    ny = HDstrlen(stry);
    if (nx != ny)
        return(FALSE);  /* different lengths */

    for(i=0; i<nx; i++,strx++,stry++)
        if(toupper(*strx)!=toupper(*stry))
            return(FALSE);

	return (TRUE);
} /* matchnocase */


/* ------------------------------------------------------------------
* VSelts
* returns the number of elements in the VDATA vs 
* returns FAIL  on error.
*
* undocumented
* 28-MAR-91 Jason NG NCSA
*
*/
#ifdef PROTOTYPE
PUBLIC int32 VSelts (int32 vkey)
#else
PUBLIC int32 VSelts (vkey)
int32 vkey;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSelts";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vs=w->vs;
    if ((vs == NULL) || (vs->otag != DFTAG_VH)) {
          HERROR(DFE_ARGS);
          HEprint(stderr,0);
          return(FAIL);
    }

    return(vs->nvertices);
} /* VSelts */


/* ------------------------------------------------------------------
*	VSgetinterlace 
*  returns the interlace (in the file) of the vdata vs.
*  returns FAIL on error.
*
*  undocumented
*
*/

#ifdef PROTOTYPE
PUBLIC int32 VSgetinterlace (int32 vkey)
#else
PUBLIC int32 VSgetinterlace (vkey)
int32 vkey;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSgetinterlace";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vs=w->vs;
    if (vs == NULL) {
        HERROR(DFE_ARGS);
        HEprint(stderr,0);
        return(FAIL);
    }

    return((int32)(vs->interlace));
} /* VSgetinterlace */

/* ------------------------------------------------------------------
*	VSsetinterlace 
* 	sets the vdata's interlace to full or none.
*	returns FAIL on error.
*/

#ifdef PROTOTYPE
PUBLIC int32 VSsetinterlace(int32 vkey, int32 interlace)
#else
PUBLIC int32 VSsetinterlace(vkey, interlace)
int32 vkey;
int32 interlace;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSsetinterlace";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vs=w->vs;
    if(vs == NULL)  {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }

    if(vs->access == 'r') {
        HERROR(DFE_RDONLY);
        return(FAIL);
    }
    if(vs->nvertices > 0) {
        HERROR(DFE_NORESET);
        return(FAIL);
    }

	/* currently only 2 kinds allowed */
    if ( interlace == FULL_INTERLACE || interlace == NO_INTERLACE ) {
        vs->interlace = (int16)interlace;
        return (TRUE); /* ok */
	} 
    else
        return (FAIL);
} /* VSsetinterlace */


/* ------------------------------------------------------------------
*	VSgetfields 
*  returns the fieldnames in a vdata.
*  RETURNS  -1 on error, else the no of fields in the vdata.
*
*	undocumented
*
*/

#ifdef PROTOTYPE
PUBLIC int32 VSgetfields (int32 vkey, char *fields)
#else
PUBLIC int32 VSgetfields (vkey, fields)
int32 vkey;
char  * fields;         /* fieldnames are returned in this var */
#endif
{
	int32 i;
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSgetfields";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vs=w->vs;
    if(vs == NULL)  {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }

	fields[0] = '\0';
    for (i=0;i<vs->wlist.n;i++) {   /* build the comma-separated string */
        HDstrcat(fields,vs->wlist.name[i]);
		if ( i < vs->wlist.n - 1 )
            HDstrcat(fields,",");
	}

	return ((int32) vs->wlist.n);
} /* VSgetfields */

/* ------------------------------------------------------------------
*	VSfexist
* 	tests for existence of 1 or more fields in a vdata.
*
*	RETURNS -1 if false, or error
*	RETURNS 1 if true
*/

#ifdef PROTOTYPE
PUBLIC int32 VSfexist (int32 vkey, char *fields)
#else
PUBLIC int32 VSfexist (vkey, fields)
int32 vkey;
char *fields;
#endif
{
	char   		**av, *s;
	int32			ac,i,j,found;
	VWRITELIST	*w;
    vsinstance_t    *wi;
    VDATA           *vs;
    char * FUNC = "VSfexist";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(wi=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vs=wi->vs;

	/* call scanattrs to parse the string */
	if (scanattrs(fields,&ac,&av) < 0) {
        HERROR(DFE_BADFIELDS);
        return (FAIL);
    }

	if ((vs == NULL) || (ac<1)) {
        HERROR(DFE_ARGS);
        return (FAIL);
    }

	/* now check in vs's field table */
    w = &vs->wlist;
    for (i=0; i<ac; i++) {
        for (found=0,s=av[i],j=0; j<w->n; j++) {
			if ( matchnocase(s,w->name[j]) ) {
				found = 1;
				break;
			}
		}
        if (!found)
            return (FAIL);
	}

    return(TRUE);
} /* VSfexist */


/* ================================================================== */
/*
*	VSsizeof - computes the byte size of the field(s) of a vdata.
*	         - Note that the size is the actual size for the local machine.
*
*		 - RETURNS FAIL on error, else the field(s) size (+ve integer).
*/

#ifdef PROTOTYPE
PUBLIC int32 VSsizeof (int32 vkey, char *fields)
#else
PUBLIC int32 VSsizeof (vkey, fields)
int32 vkey;
char  *fields;
#endif
{
    int32 	totalsize, ac, i,j,found;
    char   	**av;
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSsizeof";
    
    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
    
    /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }
    
    vs=w->vs;
    if((vs==NULL) || (scanattrs(fields,&ac,&av) < 0) || (ac<1)) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }
    
    totalsize=0;
    for (i=0;i<ac;i++) {
        for (found=0,j=0;j<vs->wlist.n;j++) /* check fields in vs */
            if (!HDstrcmp(av[i], vs->wlist.name[j])) {
                totalsize += vs->wlist.esize[j];
                found=1;
                break;
            }
        
        if (!found) {
            HERROR(DFE_ARGS);
            HEreport("VSsizeof:[%s] not in vs", av[i]);
            return(FAIL);
        }
    }
    return(totalsize);
} /* VSsizeof */

/* ================================================================== */

/*
*	VSdump - prints contents of a vdata (for debugging) 
*				no return codes.
*/

#ifdef PROTOTYPE
VOID VSdump (int32 vkey)
#else
VOID VSdump (vkey)
int32 vkey;
#endif
{
#ifdef OLD_WAY
    VWRITELIST  *w;
	int32 		i;
	char * FUNC = "VSdump";

	if (!vs) { 
          sprintf(sjs,"@VSdump: vs is null\n"); zj; 
          return;
        }

	sprintf(sjs,"@tag=%d ref=%d i=%d ",vs->otag, vs->oref,vs->interlace); zj;
    sprintf(sjs,"@nv=%ld\n ",vs->nvertices); zj;

	w = (VWRITELIST*) &vs->wlist;
	sprintf(sjs,"@vsize(hdf)=%d fields=%d [%s]\n",w->ivsize,w->n,vs->vsname); zj;

	for (i=0;i<w->n;i++) { 
		sprintf(sjs,"@<%s>      type:%d esize=%d isize=%d off=%d\n",
		    w->name[i], w->type[i], w->esize[i],w->isize[i],w->off[i]);
		zj;
	}

	sprintf(sjs,"\n"); zj;
#else
    vkey=vkey;      /* suppress warning */
#endif  /* OLD_WAY */
} /* VSdump */


/* ======================================================= */
/*
*	VSsetname - give a name to a vdata.
*	          - NO RETURN VALUES
*	          - truncates name to max length of VSNAMELENMAX
*      
*/

#ifdef PROTOTYPE
PUBLIC void VSsetname (int32 vkey, char *vsname)
#else
PUBLIC void VSsetname (vkey, vsname)
int32 vkey;
char    *vsname;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSsetname";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
    }

    vs=w->vs;
    if(vs==NULL) {
        HERROR(DFE_BADPTR);
        return;
    }

    if ( HDstrlen(vsname) > VSNAMELENMAX) {
        HDstrncpy(vs->vsname, vsname,VSNAMELENMAX);
		vs->vsname[VSNAMELENMAX]='\0';
	}
	else 
        HDstrcpy(vs->vsname, vsname);
	vs->marked = TRUE;
} /* VSsetname */

/* ======================================================= */
/*
*	VSsetclass- assigns a class name to a vdata.
*	          - NO RETURN VALUES
*	          - truncates to max length of VSNAMELENMAX
*      
*/

#ifdef PROTOTYPE
PUBLIC void VSsetclass (int32 vkey, char *vsclass)
#else
PUBLIC void VSsetclass (vkey, vsclass)
int32 vkey;
char    *vsclass;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSsetclass";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
    }

    vs=w->vs;
    if(vs==NULL) {
        HERROR(DFE_BADPTR);
        return;
    }

    if ( HDstrlen(vsclass) > VSNAMELENMAX) {
        HDstrncpy(vs->vsclass, vsclass,VSNAMELENMAX);
		vs->vsclass[VSNAMELENMAX]='\0';
	}
	else 
        HDstrcpy(vs->vsclass, vsclass);
	vs->marked = TRUE;
} /* VSsetclass*/

/* ======================================================= */

/*
*	VSgetname - gets the vdata's name.
*				 - NO RETURN VALUES
*/

#ifdef PROTOTYPE
PUBLIC void VSgetname (int32 vkey, char *vsname)
#else
PUBLIC void VSgetname (vkey, vsname)
int32 vkey;
char    *vsname;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSgetname";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
    }

    vs=w->vs;
    if(vs==NULL) {
        HERROR(DFE_BADPTR);
        return;
    }

    HDstrcpy(vsname, vs->vsname);
} /* VSgetname */

/* ======================================================= */

/*
*	VSgetclass - gets the vdata's class name.
*				 - NO RETURN VALUES
*/

#ifdef PROTOTYPE
PUBLIC void VSgetclass (int32 vkey, char *vsclass)
#else
PUBLIC void VSgetclass (vkey, vsclass)
int32 vkey;
char    *vsclass;
#endif
{
    vsinstance_t    *w;
    VDATA           *vs;
    char * FUNC = "VSgetclass";

    if (!VALIDVSID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(vkey),(uint16)VSID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
    }

    vs=w->vs;
    if(vs==NULL) {
        HERROR(DFE_BADPTR);
        return;
    }

    HDstrcpy(vsclass, vs->vsclass);
} /* VSgetclass */


/* ================================================================== */
/*
*	VSinquire - gets info about a vdata vs:
*
*		  nvertices: 	no of vertices in it.
*		  interlace: 	its interlcae
*		  fields : 	a comma separated string listing the field(s). 
*                                                 (eg "PX,PY")
*		  eltsize: 	size of elmt (all field(s)) on local machine.
*		  vsname: 	vdata's name, if any.
*
*	RETURNS FAIL if error
*	RETURNS 1 if ok
*
*/
#ifdef PROTOTYPE
PUBLIC int32 VSinquire (int32 vkey, int32 *nelt, int32 *interlace,
        char *fields, int32 *eltsize, char *vsname)
#else
PUBLIC int32 VSinquire (vkey, nelt, interlace, fields, eltsize, vsname)
int32 vkey;
char    *fields, *vsname;
int32   *nelt, *interlace, *eltsize;
#endif
{
    char * FUNC = "VSinquire";

	if(fields)
        VSgetfields(vkey,fields);
	if(nelt) 
        *nelt=VSelts(vkey);
	if(interlace)
        *interlace=VSgetinterlace(vkey);
	if(eltsize)
        *eltsize=VSsizeof(vkey,fields);
	if(vsname)
        VSgetname(vkey,vsname);

	return (SUCCEED); /* ok */
} /* VSinquire */

/* ================================================================== */
/*
* VSlone - returns an array of refs of all lone vdatas in the file.
*      - returns -1 if error
*      - otherwise returns the total number of lone vdatas in the file 
*
*		If idarray is too small, routine will only fill idarray with up
*		 to asize worth of refs.
*
*		INPUT idarray: user supplies  an int array.
*	   INPUT asize: integer specifying how many ints in idarray[];
*		INPUT f: HDF file pointer.
*
*/

#ifdef PROTOTYPE
PUBLIC int32 VSlone(HFILEID f, int32 idarray[], int32 asize) 
#else
PUBLIC int32 VSlone(f, idarray, asize)
HFILEID f;
int32   idarray[];       /* array to contain the refs */
int32   asize;            /* input: size of idarray */
#endif
{
    uint8  *lonevdata; /* lcl wrk area: stores flags of vdatas */
    int32  i, vgid, vsid, vstag;
    int32  vkey;
    int32  nlone; /* total number of lone vdatas */
    char * FUNC = "VSlone";
    
    
/* -- allocate space for vdata refs, init to zeroes -- */
    if (NULL == (lonevdata = (uint8 *) HDgetspace( 65000L * sizeof(uint8)))) {
        HERROR(DFE_NOSPACE);
        return(FAIL);
    }
    HDmemset(lonevdata,0,65000L*sizeof(uint8));
    
    /* -- look for all vdatas in the file, and flag (1) each -- */
    vsid = -1;
    while( -1L != (vsid = VSgetid (f, vsid)))   /* until no more vdatas */
        lonevdata[vsid ] = 1;
    
/* -- Look through all vgs, searching for vdatas -- */
    /* -- increment its index in lonevdata if found -- */
    vgid = -1;
    while( -1L != (vgid = Vgetid (f, vgid))) { /* until no more vgroups */
#ifdef OLD_WAY
        vg = (VGROUP*) Vattach(f,vgid,"r");
        for (i=0; i < Vntagrefs(vg); i++) {
            Vgettagref (vg, i, &vstag, &vsid);
            if (vstag == (int32) DFTAG_VH)
                lonevdata[vsid]++;
        }
        Vdetach(vg);
#else
        vkey = Vattach(f,vgid,"r");
        for (i=0; i< Vntagrefs(vkey); i++) {
            Vgettagref (vkey, i, &vstag, &vsid);
            if (vstag == (int32) DFTAG_VH)
                lonevdata[vsid]=0;
        }
        Vdetach(vkey);
#endif
    }

/* -- check in lonevdata: it's a lone vdata if its flag is still 1 -- */
    nlone = 0;
    for(i=0; i<65000; i++) {
        if (lonevdata[i]) {
            if (nlone < asize)      /* insert into idarray up till asize */
                idarray[nlone] = i;
            nlone ++;
        }
    }
    HDfreespace((VOIDP)lonevdata);

    return (nlone); /* return the TOTAL # of lone vdatas */
} /* VSlone */

/* ================================================================== */
/*
* Vlone  - returns an array of refs of all lone vgroups in the file.
* 	      - returns -1 if error
*	      - otherwise returns the total number of lone vgroups in the file 
*
*			If idarray is too small, routine will only fill idarray with up
*			 to asize worth of refs.
*
*			INPUT idarray: user supplies  an int array.
*		   INPUT asize: integer specifying how many ints in idarray[];
*			INPUT f: HDF file pointer.
*
*/

#ifdef PROTOTYPE
PUBLIC int32 Vlone (HFILEID f, int32 idarray[], int32 asize) 
#else
PUBLIC int32 Vlone (f, idarray, asize)
HFILEID f;
int32   idarray[];       /* array to contain the refs */
int32   asize;            /* input: size of idarray */
#endif
{
    uint8       *lonevg; /* local wrk area: stores flags of vgroups */
	int32		i;
	int32 	vgid, vstag, id;
#ifdef OLD_WAY
	VGROUP 	* vg;
#else
    int32 vkey;
#endif
	int32 	nlone; /* total number of lone vgroups */
	char * FUNC = "Vlone";

/* -- allocate space for vgroup refs, init to zeroes -- */
    if (NULL == (lonevg = (uint8 *) HDgetspace ( 65000L * sizeof(uint8)))) {
        HERROR(DFE_NOSPACE);
        return(FAIL);
    }
    HDmemset(lonevg,0,65000L*sizeof(uint8));

/* -- look for all vgroups in the file, and flag (1) each -- */
    id = -1;
    while( -1L != (id = Vgetid (f, id)))   /* until no more vgroups */
        lonevg[ id ] = 1;

/* -- Look through all vgs, searching for vgroups -- */
/* -- increment its index in lonevg if found -- */
    vgid = -1;
    while( -1L != (vgid = Vgetid (f, vgid))) {  /* until no more vgroups */
#ifdef OLD_WAY
        vg = (VGROUP*) Vattach(f,vgid,"r");
        id = -1;
        for (i=0; i< Vntagrefs(vg); i++) {
            Vgettagref (vg, i, &vstag, &id);
            if (vstag == DFTAG_VG)
                lonevg[id]++;
        }
        Vdetach(vg);
#else
        vkey = Vattach(f,vgid,"r");
        id = -1;
        for (i=0; i< Vntagrefs(vkey); i++) {
            Vgettagref (vkey, i, &vstag, &id);
            if (vstag == DFTAG_VG)
                lonevg[id]=0;
        }
        Vdetach(vkey);
#endif
    }

    /* -- check in lonevg: it's a lone vgroup if its flag is still 1 -- */
    nlone = 0;
    for(i=0; i<65000; i++) {
        if (lonevg[i]) {
            if (nlone < asize)  /* insert into idarray up till asize */
                idarray[nlone] = i;
            nlone ++;
        }
    }
    HDfreespace ((VOIDP)lonevg);

    return (nlone); /* return the TOTAL # of lone vgroups */
} /* Vlone */


/* ================================================================== */
/* new jan 3 1991 */
/* looks in the file and returns the ref of the vgroup with name vgname */
/* 
* returns 0 if not found, or error.
* otherwise, returns the vgroup's ref (a +ve integer).
*/

#ifdef PROTOTYPE
int32 Vfind (HFILEID f, char *vgname)
#else
int32 Vfind (f, vgname)
HFILEID f;
char    * vgname;
#endif
{
  	int32 	vgid = -1;
    int32   ret_ref;
#ifdef OLD_WAY
 	VGROUP* 	vg;
#else
    int32 vkey;
#endif
  	char 		name[512];
	char * 	FUNC = "Vfind";

    while ( -1L != (vgid=Vgetid(f, vgid)) ) {
#ifdef OLD_WAY
		vg = (VGROUP*) Vattach(f,vgid,"r");
		if (vg==NULL) return(0); 			/* error */
		Vgetname(vg, name);
		Vdetach (vg);
#else
        vkey = Vattach(f,vgid,"r");
        if (vkey==FAIL)
            return(0);            /* error */
        Vgetname(vkey, name);
        Vdetach (vkey);
#endif
        if (!HDstrcmp(vgname,name)) {
            ret_ref = VQueryref(vkey);
            return (ret_ref);   /* found the vgroup */
          } /* end if */
  	}
  	return(0); /* not found */

} /* Vfind */

/* ================================================================== */
/* new jan 3 1991 */
/* looks in the file and returns the ref of the vdata with name vsname */
/* 
* returns 0 if not found, or error.
* otherwise, returns the vdata's ref (a +ve integer).
*/
#ifdef PROTOTYPE
int32 VSfind (HFILEID f, char *vsname)
#else
int32 VSfind (f, vsname)
HFILEID f;
char * vsname;
#endif
{
  	int32 	vsid = -1;
    int32   ret_ref;
#ifdef OLD_WAY
  	VDATA * 	vs;
#else
    int32 vkey;
#endif
  	char 		name[512];
	char * 	FUNC = "VSfind";

    while ( -1L != (vsid=VSgetid(f, vsid)) ) {
#ifdef OLD_WAY
		vs = (VDATA*) VSattach(f,vsid,"r");
		if (vs==NULL) return(0); 			/* error */
		VSgetname(vs, name);
		VSdetach (vs);
#else
        vkey = VSattach(f,vsid,"r");
        if (vkey==FAIL)
            return(0);            /* error */
        VSgetname(vkey, name);
        VSdetach (vkey);
#endif
        if (!HDstrcmp(vsname, name)) {
            ret_ref = VSQueryref(vkey);
            return(ret_ref);  /* found the vdata */
          } /* end if */
  	}
  	return(0); /* not found */

} /* VSfind */

/* ================================================================== */

/*
* Vsetzap: Useless now. Maintained for back compatibility.
*/

#ifdef PROTOTYPE
#ifdef CONVEX
PUBLIC VOID Vsetzap()
#else
PUBLIC VOID Vsetzap(void)
#endif
#else
PUBLIC VOID Vsetzap()
#endif
{
}
/* ================================================================== */
