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
#include "hfile.h"

PRIVATE int32 Load_vfile
  PROTO((HFILEID f));

PRIVATE VOID Remove_vfile
  PROTO((HFILEID f));

PUBLIC intn vcompare
    PROTO((VOIDP k1,VOIDP k2,intn cmparg));

PRIVATE vginstance_t *vginstance
  PROTO((HFILEID f, uint16 vgid));

PRIVATE void vunpackvg
    PROTO((VGROUP *vg, uint8 buf[]));

/*
* -------------------------------------------------------------------- 
* PRIVATE  data structure and routines.
* 
* Info about all vgroups in the file are loaded into vgtab  at start;
* and the vg field set to NULL until that vgroup is attached,
* and reset back to NULL when that vgroup is detached. 
* Info about all vdatas in the file are loaded into vstab  at start;
* and the vs field set to NULL until that vdata is attached,
* and reset back to NULL when that vdata is detached. 
* -------------------------------------------------------------------- 
*/


PUBLIC vfile_t  vfile [MAX_VFILE] = {0};


/* -------------------------- Load_vfile ------------------------ */
/*
  *** Only called by Vinitialize()  ***   

loads vgtab table with info of all vgroups in file f.
Will allocate a new vfile_t, then proceed to load vg instances.
RETURNS FAIL if error or no more file slots available.
RETURNS SUCCEED if ok.
*/

#ifdef PROTOTYPE
PRIVATE int32 Load_vfile (HFILEID f)    
#else
PRIVATE int32 Load_vfile (f)
HFILEID f;
#endif
{
    vfile_t		  * vf;
    vginstance_t  * v;
    vsinstance_t  * w;
    int32 			aid, stat;
    uint16			tag, ref;
    char * FUNC = "Load_vfile";
    
    /* allocate a new vfile_t structure */
    vf = Get_vfile(f);
    if(!vf)
        return FAIL;

    /* the file is already loaded (opened twice) do nothing */
    if(vf->access++) {
        return SUCCEED;
    }

    /* load all the vg's  tag/refs from file */
#ifdef OLD_WAY
    vf->vgtabn    = -1;
    vf->vgtabtail = &(vf->vgtab);
    
    vf->vgtab.ref      = -1;
    vf->vgtab.nattach  = -1;
    vf->vgtab.nentries = -1;
    vf->vgtab.vg       = NULL;
    vf->vgtab.next     = NULL;
#else
    vf->vgtabn = 0;
    vf->vgtree = tbbtdmake(vcompare, sizeof(int32));
    if(vf->vgtree == NULL)
        return(FAIL);
#endif
        
    stat = aid = Hstartread(f, DFTAG_VG,  DFREF_WILDCARD);
    while (stat != FAIL) {
        HQuerytagref (aid, &tag, &ref);
        if (NULL== (v = (vginstance_t*) HDgetspace (sizeof(vginstance_t)))) {
            HERROR(DFE_NOSPACE);
            tbbtdfree(vf->vgtree, vdestroynode, NULL);
            return(FAIL);
          }
          
#ifdef OLD_WAY
        vf->vgtabtail->next  = v;
        vf->vgtabtail      = v;
        v->next            = NULL;
#else
        vf->vgtabn++;
        v->key      = (int32) VGSLOT2ID(f,ref); /* set the key for the node */
        v->ref      = (intn) ref;
        v->vg       = (VGROUP*) NULL; /* ie not attached yet */
        v->nattach  = 0;
        v->nentries = 0;
        tbbtdins(vf->vgtree,(VOIDP)v,NULL);    /* insert the vg instance in B-tree */
#endif
        stat = Hnextread (aid, DFTAG_VG, DFREF_WILDCARD, DF_CURRENT);
	}
    Hendaccess (aid);

    /* load all the vs's  tag/refs from file */
#ifdef OLD_WAY
	vf->vstabn    = -1;
	vf->vstabtail = &(vf->vstab);

	vf->vstab.ref      = -1;
	vf->vstab.nattach  = -1;
	vf->vstab.nvertices= -1;
	vf->vstab.vs       = NULL;
	vf->vstab.next     = NULL;
#else
    vf->vstabn = 0;
    vf->vstree = tbbtdmake(vcompare, sizeof(int32));
    if(vf->vstree==NULL) {
        tbbtdfree(vf->vgtree, vdestroynode, NULL);
        return(FAIL);
      } /* end if */
#endif

    stat = aid = Hstartread(f, VSDESCTAG,  DFREF_WILDCARD);
    while (stat != FAIL) {
        HQuerytagref (aid, &tag, &ref);
        if (NULL == (w = (vsinstance_t*) HDgetspace (sizeof(vsinstance_t)))) {
            HERROR(DFE_NOSPACE);
            tbbtdfree(vf->vgtree, vdestroynode, NULL);
            tbbtdfree(vf->vstree, vsdestroynode, NULL);
            return(FAIL);
          }
          
#ifdef OLD_WAY
        vf->vstabtail->next  = w;
        vf->vstabtail        = w;
        w->next     = NULL;
#else
        vf->vstabn++;
        w->key      = (int32) VSSLOT2ID(f,ref); /* set the key for the node */
        w->ref      = (intn) ref;
        w->vs       = (VDATA*) NULL; /* ie not attached yet */
        w->nattach  = 0;
        w->nvertices= 0;
        tbbtdins(vf->vstree,(VOIDP)w,NULL);    /* insert the vg instance in B-tree */
#endif
        stat = Hnextread (aid, VSDESCTAG, DFREF_WILDCARD, DF_CURRENT);
	}
    Hendaccess (aid);

	/* file may be incompatible with vset version 2.x. Need to check it */
    if ( ((int32)0 == vf->vgtabn) && ((int32)0 == vf->vstabn) )
        if ( (int32)0 == vicheckcompat (f) ) { /* not compatible */
#if 0
            nvfile--; 	/* delete the structure for that file */
#endif
            HERROR(DFE_BADOPEN);
            HEreport("This file is incompatible with the current release");
            tbbtdfree(vf->vgtree, vdestroynode, NULL);
            tbbtdfree(vf->vstree, vsdestroynode, NULL);
            return(FAIL);
          }
        
	/* otherwise, success */
	return (SUCCEED);
} /* Load_vfile */

/* ---------------------------- Remove_vfile ------------------------- */
/*
  removes the file ptr from the vfile[] table. 
  *** Only called by Vfinish() ***
*/
#ifdef PROTOTYPE
PRIVATE VOID Remove_vfile (HFILEID f)
#else
PRIVATE VOID Remove_vfile (f)
HFILEID f;
#endif
{
#ifdef OLD_WAY
    vginstance_t *vginst, *vg1;
    vsinstance_t *vsinst, *vs1;
#endif
    vfile_t      *vf=NULL;
    char * FUNC = "Remove_vfile";
    
    /* Figure out what file to work on */
    vf = Get_vfile(f);
    
    if(vf == NULL)
        return;
    
    /* someone still has an active pointer to this file */
    if(--vf->access) {
        return;
    }

#ifdef OLD_WAY
	/* free vstab and vgtab link-list entries */
    vginst = vf->vgtab.next;
    while (vginst) {
        vg1 = vginst->next;
        if (vginst->vg)  {
            HDfreespace ((VOIDP)vginst->vg);
            HDfreespace ((VOIDP)vginst);
        }
        vginst = vg1;
    }

    vsinst = vf->vstab.next;
    while (vsinst) {
        vs1 = vsinst->next; 
        if (vsinst->vs) {
            HDfreespace ((VOIDP)vsinst->vs);
            HDfreespace ((VOIDP)vsinst);
        }
        vsinst = vs1; 
    }

    vf->vgtab.next = NULL;
    vf->vstab.next = NULL;
#else
    tbbtdfree(vf->vgtree, vdestroynode, NULL);
    tbbtdfree(vf->vstree, vsdestroynode, NULL);
#endif
}  /* Remove_vfile */

/* ---------------------------- vcompare ------------------------- */
/*
  Compares two B-tree keys for equality.  Similar to memcmp.

  *** Only called by B-tree routines, should _not_ be called externally ***
*/
#ifdef PROTOTYPE
PUBLIC intn vcompare(VOIDP k1,VOIDP k2,intn cmparg)
#else
PUBLIC intn vcompare(k1,k2,cmparg)
VOIDP k1;
VOIDP k2;
intn cmparg;
#endif
{
#ifdef QAK
printf("vcompare: k1=%d, k2=%d\n",(int)k1,(int)k2);
printf("vcompare: *k1=%d, *k2=%d\n",*(int32 *)k1,*(int32 *)k2);
#endif
    return((intn)((*(int32 *)k1) - (*(int32 *)k2)));  /* valid for integer keys */
}  /* vcompare */

/* ---------------------------- vprint ------------------------- */
/*
  Prints out the key and reference number of VDatas and Vgroups

  *** Only called by B-tree routines, should _not_ be called externally ***
*/
#ifdef PROTOTYPE
PUBLIC VOID vprint(VOIDP k1)
#else
PUBLIC VOID vprint(k1)
VOIDP k1;
#endif
{
    printf("Ptr=%p, key=%d, ref=%d\n",k1,((vginstance_t *)k1)->key,((vginstance_t *)k1)->ref);
}  /* vprint */

/* ---------------------------- vdestroynode ------------------------- */
/*
  Frees B-Tree nodes

  *** Only called by B-tree routines, should _not_ be called externally ***
*/
#ifdef PROTOTYPE
PUBLIC VOID vdestroynode(VOIDP n)
#else
PUBLIC VOID vdestroynode(n)
VOIDP n;
#endif
{
    VGROUP       * vg;

    vg = ((vginstance_t *)n)->vg;
    if(vg != NULL) {
        HDfreespace((VOIDP)vg->tag);
        HDfreespace((VOIDP)vg->ref);
        HDfreespace((VOIDP)vg);
    }

    HDfreespace((VOIDP)n);

}  /* vdestroynode */

#ifdef NOTNEEDED
/* ---------------------------- vtfreekey ------------------------- */
/*
  Frees B-Tree index (actually doesn't anything at all)

  *** Only called by B-tree routines, should _not_ be called externally ***
*/
#ifdef PROTOTYPE
PUBLIC VOID vtfreekey(VOIDP k)
#else
PUBLIC VOID vtfreekey(k)
VOIDP k;
#endif
{
    k=k;    /* i.e. do nothing */
}  /* vtfreekey */
#endif

/* ---------------------------- Vinitialize ------------------------- */

#ifdef PROTOTYPE
PUBLIC VOID Vinitialize(HFILEID f)
#else
PUBLIC VOID Vinitialize(f)
HFILEID f;
#endif
{
    char * FUNC = "Vinitialize";
    
    Load_vfile (f);
}

/* ---------------------------- Vfinish ------------------------- */

#ifdef PROTOTYPE
PUBLIC intn Vfinish (HFILEID f)
#else
PUBLIC intn Vfinish (f)
HFILEID f;
#endif
{
    char * FUNC = "Vfinish";
    
    Remove_vfile (f);
    return(SUCCEED);
}


/* ---------------------------- vginstance ----------------------------- */
/*
* Looks thru vgtab for vgid and return the addr of the vg instance
* where vgid is found.
* RETURNS NULL if error or not found.
* RETURNS vginstance_t pointer if ok.
*
*/
#ifdef PROTOTYPE
PRIVATE vginstance_t * vginstance (HFILEID f, uint16 vgid)
#else
PRIVATE vginstance_t * vginstance (f, vgid)
HFILEID     f;
uint16  vgid;
#endif
{
    VOIDP *t;
    vfile_t      * vf;
    int32 key;
    char *FUNC = "vginstance";
  
#ifdef QAK
printf("vginstance(): f=%d vf=%p\n",(int)f,Get_vfile(f));
#endif
    if (NULL== (vf = Get_vfile(f)))
        HRETURN_ERROR(DFE_FNF, NULL);

    /* tbbtdfind returns a pointer to the vginstance_t pointer */
    key=VGSLOT2ID(f,vgid);
#ifdef QAK
printf("vginstance(): key=%d\n",(int)key);
#endif
    t=(VOIDP *)tbbtdfind(vf->vgtree,(VOIDP)&key,NULL);
    if(t!=NULL)
        return((vginstance_t *)*t);     /* return the actual vginstance_t ptr */

    HERROR(DFE_NOMATCH);
    return(NULL);
} /* vginstance */

/* ------------------------ vexistvg --------------------------- */
/* 
* Tests if a vgroup with id vgid is in the file's vgtab.
* returns FAIL if not found,
* returns TRUE if found.
*/
#ifdef PROTOTYPE
int32 vexistvg (HFILEID f, uint16 vgid)
#else
int32 vexistvg (f, vgid)
HFILEID     f;
uint16  vgid;
#endif
{
    char * FUNC = "vexistvg";
  
    if (NULL== (vginstance_t *) vginstance(f,vgid))
        return(FAIL);
    else
        return(TRUE);
} /* vexistvg */

/* ==================================================================== */
/*
* vpackvg() and vunpackvg() : Packing and unpacking routines.
* For use in retrieving and storing vgroups to/from the HDF file.
*
*	Fields of VGROUP  that gets stored in HDF as a DFTAG_VG data object:
*		int16		nvelt (no of entries )
*		char		vgname[MAXVGNAMELEN]
*     char     vgclass[MAXVGNAMELEN]
*		int16		tag[1..nvelt]		
*		int16		ref[1..nvelt]		
*/
/* ==================================================================== */

#ifdef QAK
#define INT16SIZE 2
#define UINT16SIZE 2
#endif

/* ==================================================================== */
/* 
*	vpackvg
*	extracts fields from  a VGROUP struct vg and pack the fields
*  into array buf in preparation for storage in the HDF file.
*
*  NO RETURN VALUES.
*/

#ifdef PROTOTYPE
void vpackvg (VGROUP *vg, uint8 buf[], int32 *size)
#else
void vpackvg (vg, buf, size)
VGROUP          *vg;    /* vgroup to be saved to file */
uint8           buf[];  /* buffer to receive the packed fields */
int32           *size;  /* the size of buf is returned here */
#endif
{
	register uint16 	i;
    register uint8      *bb;
	char * FUNC = "vpackvg";

	bb = &buf[0];

	/* save nvelt */
    UINT16ENCODE(bb,vg->nvelt);

	/* save all tags */
    for(i=0; i<vg->nvelt; i++)
        UINT16ENCODE(bb,vg->tag[i]);

	/* save all refs */
    for(i=0; i<vg->nvelt; i++)
        UINT16ENCODE(bb,vg->ref[i]);

	/* save the vgnamelen and vgname - omit the null */
    UINT16ENCODE(bb,HDstrlen(vg->vgname));

    HDstrcpy((char*) bb,vg->vgname);
    bb +=  HDstrlen(vg->vgname) ;

	/* save the vgclasslen and vgclass- omit the null */
    UINT16ENCODE(bb,HDstrlen(vg->vgclass));

    HDstrcpy((char*) bb,vg->vgclass);
    bb +=  HDstrlen(vg->vgclass) ;

	/* save the expansion tag/ref pair */
    UINT16ENCODE(bb,vg->extag);   /* the vg's expansion tag */
    UINT16ENCODE(bb,vg->exref);   /* the vg's expansion ref */

	/*  save the vg's version field */
    UINT16ENCODE(bb,vg->version);

	/* save the vg's more field */
    UINT16ENCODE(bb,vg->more);

	/* returns the size of total fields saved */
    *size = (int32) (bb - buf) + 1;
} /* vpackvg */

/* ==================================================================== */
/*
*	vunpackvg:
*	Unpacks the fields from a buf (ie a DFTAG_VG data object just 
*	read in from the HDF file), into a VGROUP structure vg.
*
* 	Will first zero out vg, unpack fields, then inits as much of 
*  vg as it can.
*
*	NO RETURN VALUES
*
*/

#ifdef PROTOTYPE
PRIVATE void vunpackvg (VGROUP *vg, uint8 buf[])
#else
PRIVATE void vunpackvg (vg, buf)
VGROUP *vg; /* vgroup to be loaded with file data */
uint8  buf[];  /* must contain a DFTAG_VG data object from file */
#endif
{
    register uint8   *bb;
    register uintn   u;
    register uint16  uint16var;
    char * FUNC = "vunpackvg";
    
    bb = &buf[0];

    /* retrieve nvelt */
    UINT16DECODE(bb,vg->nvelt);
    
    vg->msize = (vg->nvelt > MAXNVELT ? vg->nvelt : MAXNVELT);
    vg->tag  = (uint16 *) HDgetspace(vg->msize * sizeof(uint16));
    vg->ref  = (uint16 *) HDgetspace(vg->msize * sizeof(uint16));
    
    if((vg->tag == NULL) || (vg->ref == NULL))
        return;
    
    /* retrieve the tags */
    for (u = 0; u < vg->nvelt; u++)
        UINT16DECODE(bb,vg->tag[u]);
    
    /* retrieve the refs */
    for (u = 0; u < vg->nvelt; u++)
        UINT16DECODE(bb,vg->ref[u]);

    /* retrieve vgname (and its len)  */
    UINT16DECODE(bb,uint16var);
    
    HIstrncpy(vg->vgname, (char*) bb, (int32) uint16var + 1);
    bb += uint16var;
    
    /* retrieve vgclass (and its len)  */
    UINT16DECODE(bb,uint16var);
    
    HIstrncpy(vg->vgclass, (char*) bb, (int32) uint16var + 1);
    bb += uint16var;
    
    UINT16DECODE(bb,vg->extag); /* retrieve the vg's expansion tag */
    UINT16DECODE(bb,vg->exref); /* retrieve the vg's expansion ref */
    
    UINT16DECODE(bb,vg->version); /* retrieve the vg's version field */
    
    UINT16DECODE(bb,vg->more); /* retrieve the vg's more field */
} /* vunpackvg */


/* ----------------------------- Vattach --------------------------- */

/*
*	 Vattach:
*
*   attaches to an existing vgroup or creates a new vgroup.
*	 returns NULL if  error, else ptr to vgroup.
*
*	IGNORE accesstype. (but save it)  
*  if vgid == -1,
*	  create a NEW vg if vgdir is not full.
*	  Also set nattach =1, nentries=0.
*  if vgid +ve, 
*	  look in vgdir to see if already attached,
*	  if yes, incr nattach 
*	  if not, fetch from file. attach, set nattach=1, netries= val from file 
*
*	In any case, set marked flag to 0.
*/

#ifdef PROTOTYPE
PUBLIC int32 Vattach (HFILEID f, int32 vgid, char *accesstype)
#else
PUBLIC int32 Vattach (f, vgid, accesstype)
HFILEID f;      /* HDF file handle */
int32   vgid;       /* actual vgroup's vgid or -1 for new vgroup */
char    *accesstype;    /* access mode */
#endif
{
	VGROUP			*vg;
    int16           access;
    uint8           * vgpack;
    vginstance_t    * v;
	vfile_t			* vf;
	char * FUNC = "Vattach";

    if (f == FAIL) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }
    if (NULL==(vf = Get_vfile(f))) {
        HERROR(DFE_FNF);
        return(FAIL);
    }

    if(tolower(accesstype[0])=='r')
        access = 'r';
    else if(tolower(accesstype[0])=='w')
        access = 'w';
    else
        HRETURN_ERROR(DFE_BADACC, FAIL);

    if (vgid == -1) {           /******* create a NEW vg in vgdir ******/
#ifdef QAK
printf("Vattach, creating new VG in file\n");
#endif
        if (access=='r') {
            HERROR(DFE_ARGS);
            return(FAIL);
        }

      /* allocate space for vg, & zero it out */
        if ( (vg = (VGROUP*) HDgetspace (sizeof(VGROUP)) ) == NULL) {
            HERROR(DFE_NOSPACE);
            return(FAIL);
        }

      /* initialize new vg */
        vg->msize = MAXNVELT;
        vg->tag   = (uint16 *) HDgetspace(vg->msize * sizeof(uint16));
        vg->ref   = (uint16 *) HDgetspace(vg->msize * sizeof(uint16));

        if((vg->tag == NULL) || (vg->ref == NULL)) {
            HERROR(DFE_NOSPACE);
            return(FAIL);
        }

        vg->nvelt   = 0;
        vg->vgname[0]   = '\0';
        vg->f           = f;
        vg->otag        = DFTAG_VG;
        vg->oref    = vnewref(f);  /* create a new unique ref for it */
        if( vg->oref == 0 ) {
            HERROR(DFE_NOREF);
            return(FAIL);
        }

        vg->access    = access;

        vg->marked        = 0;
        vg->vgclass[0]    = '\0';
        vg->extag     = 0;
        vg->exref     = 0;
        vg->more          = 0;
        vg->version       = VSET_VERSION;

      /* attach new vg to file's vgtab  */
        if ( NULL == (v = (vginstance_t*) HDgetspace (sizeof(vginstance_t)))) {
            HERROR(DFE_NOSPACE);
            return(FAIL);
        }

#ifdef OLD_WAY
        vf->vgtabtail->next = v;
        vf->vgtabtail       = v;
        v->next   = NULL;
#else
        vf->vgtabn++;
        v->key      = (int32) VGSLOT2ID(f,vg->oref); /* set the key for the node */
        v->ref      = (intn) vg->oref;
        v->vg       = vg;
        v->nattach  = 1;
        v->nentries = 0;
        tbbtdins(vf->vgtree,(VOIDP)v,NULL);    /* insert the vg instance in B-tree */
#endif

#ifdef OLD_WAY
        return(vg);
#else
#ifdef QAK
tbbtdump(vf->vgtree,0);
#endif
        return(v->key);     /* return key instead of VGROUP ptr */
#endif
	}
	else { 		
          /******* access an EXISTING vg *********/
        uint32 len;
          
        if (NULL == (v= vginstance (f,(uint16)vgid))) {
            HERROR(DFE_NOMATCH);
            HEreport("Vgid (%d) is not in vgtab[]", vgid);
            return(FAIL);
        }
          
          /*
           * vg already attached.  inc nattach and return existing ptr
           */
        if (v->vg != NULL) {
            v->nattach++;

#ifdef OLD_WAY
            return(v->vg);
#else
            return(v->key);     /* return key instead of VGROUP ptr */
#endif
        }
          
          /* else vg not attached, must fetch vg from file */
          
        len = Hlength(f, DFTAG_VG, (uint16) vgid);
        if (len == FAIL)
            return(FAIL);

        vgpack = (uint8 *) HDgetspace(len);
        if(vgpack == NULL)
            return(FAIL);


        if (Hgetelement(f, DFTAG_VG, (uint16)vgid, vgpack) == (int32)FAIL) {
            HERROR(DFE_NOMATCH);
            return(FAIL);
        }
          
          /* allocate space for vg, & zero it out */
          
        if (NULL == (vg =(VGROUP*) HDgetspace (sizeof(VGROUP))) ) {
            HERROR(DFE_NOSPACE);
            return(FAIL);
        }
          
        /* unpack vgpack into structure vg, and init  */
        vunpackvg(vg,vgpack);
        vg->f             = f;
        vg->oref            = (uint16)vgid;
        vg->otag      = DFTAG_VG;
        vg->access        = access;
        vg->marked        = 0;
          
        /* attach vg to file's vgtab at the vg instance v */
        v->vg             = vg;
        v->nattach        = 1;
        v->nentries    = vg->nvelt;
        HDfreespace((VOIDP)vgpack);
          
#ifdef OLD_WAY
        return(vg);
#else
#ifdef QAK
tbbtdump(vf->vgtree,0);
#endif
        return(v->key);     /* return key instead of VGROUP ptr */
#endif
	}
} /* Vattach */

/* ---------------------------- Vdetach ---------------------------- */
/* 
*	Vdetach
*	Detaches access to vg.    
*	NO RETURN VALUES
*
*  if marked flag is 1, write out vg to file.
*	if vg still has velts attached to it, cannot detach vg.
*	decr  nattach. if (nattach is 0), free vg from vg instance.
*	(check that no velts are still attached to vg before freeing)
*
*  if attached with read access, just return.
*
* after detach, set marked flag to 0.	
*
*/
#ifdef PROTOTYPE
PUBLIC void Vdetach (int32 vkey)
#else
PUBLIC void Vdetach (vkey)
int32 vkey;
#endif
{
    VGROUP       *vg;
    int32         vgpacksize;
    uint8         * vgpack;
    vginstance_t  * v;
    char * FUNC = "Vdetach";
  
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
    }

    vg=v->vg;
    if ((vg == NULL) || (vg->otag != DFTAG_VG)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
    }
  
  /* update vgroup to file if it has write-access */
  
  /* if its marked flag is 1 */
  /* - OR - */
  /* if that vgroup is empty */
  if (vg->access == 'w') {
    if ((vg->nvelt==0) || (vg->marked == 1)) {
      vgpack = (uint8 *) HDgetspace((int32) sizeof(VGROUP) + vg->nvelt * 4);
      vpackvg(vg,vgpack,&vgpacksize);

      /* 
       *  For now attempt to blow away the old one.  This is a total HACK
       *    but the H-level needs to stabilize first
       */
      Hdeldd(vg->f, DFTAG_VG, vg->oref);

      if(Hputelement(vg->f, DFTAG_VG, vg->oref, vgpack, vgpacksize) == FAIL) {
        HERROR(DFE_WRITEERROR);
        HEprint(stderr, 0);
      }
      HDfreespace((VOIDP)vgpack);
      vg->marked = 0;
/*    return; */
    }
  }
  
  v->nattach--;
  
  if (v->nattach > 0)
    return;    /* ok */
  
  
#ifdef OLD_WAY
  v->vg = NULL;             /* detach vg from vgdir */
  
  HDfreespace((VOIDP)vg->tag);
  HDfreespace((VOIDP)vg->ref);
  HDfreespace((VOIDP)vg);
#endif
  
  return; /* ok */
} /* Vdetach */


/* ------------------------------ Vinsert ----------------------------- */
/*
*	Vinsert
*  inserts a velt (vs or vg)  into a vg 
*	RETURNS entry position within vg (0 or +ve) or FAIL on error.
*
*	checks and prevents duplicate links.
*
* Since multiple files are now possible, check that both vg and velt
* are from the same file. else error.
*/

#ifdef PROTOTYPE
PUBLIC int32 Vinsert (int32 vkey, int32 insertkey)
#else
PUBLIC int32 Vinsert (vkey, insertkey)
int32 vkey;
int32 insertkey;          /* (VGROUP*) or (VDATA*), doesn't matter */
#endif
{
    VGROUP *vg;
    vginstance_t  * v;
    VDATA *velt;
    vsinstance_t  * w;
    vginstance_t  * x;
    register uintn u;
    char * FUNC = "Vinsert";
    int32 newtag, newref, newfid;
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
    }
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    
    if (vg->otag != DFTAG_VG) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }
    
    newfid = FAIL;
    if (VALIDVSID(insertkey)) {
  
        /* locate vs's index in vstab */
        if(NULL==(w=(vsinstance_t*)vsinstance(VSID2VFILE(insertkey),(uint16)VSID2SLOT(insertkey)))) {
            HERROR(DFE_NOVS);
            HEprint(stderr, 0);
            return(FAIL);
        }
        
        if (w->vs == NULL) {
            HERROR(DFE_ARGS);
            HEprint(stderr,0);
            return(FAIL);
        }
     
        newtag = (int32) DFTAG_VH;
        newref = (int32) w->vs->oref;
        newfid = w->vs->f;

    } else {
        
        if(VALIDVGID(insertkey)) {
            
            /* locate vs's index in vgtab */
            if(NULL==(x=(vginstance_t*)vginstance(VGID2VFILE(insertkey),(uint16)VGID2SLOT(insertkey)))) {
                HERROR(DFE_NOVS);
                HEprint(stderr, 0);
                return(FAIL);
            }
        
            if (x->vg == NULL) {
                HERROR(DFE_ARGS);
                HEprint(stderr,0);
                return(FAIL);
            }
            
            newtag = (int32) DFTAG_VG;
            newref = (int32) x->vg->oref;
            newfid = x->vg->f;
            
        }

    }
    
    /* make sure we found something */
    if(newfid == FAIL) {
        HERROR(DFE_ARGS);
        return(FAIL);
    }

    if (vg->f != newfid) {
        HERROR(DFE_DIFFFILES);
        return(FAIL);
    }
    
    /* check and prevent duplicate links */
    for(u = 0; u < vg->nvelt; u++)
        if((vg->ref[u] == newref) && (vg->tag[u] == newtag)) {
            HERROR(DFE_DUPDD);
            HEreport("Vinsert: duplicate link <%d/%d>", newtag, newref);
            return(FAIL);
        }
    
    /* Finally, ok to insert */
    vinsertpair(vg, (uint16) newtag, (uint16) newref);

    return(vg->nvelt - 1);

} /* Vinsert */

/* ----------------------------- Vflocate -------------------------------- */
/*
Checks to see if the given field exists in a vdata belonging to this vgroup.
If found, returns the ref of the vdata.
If not found, or error, returns FAIL
28-MAR-91 Jason Ng NCSA
*/
#ifdef PROTOTYPE
PUBLIC int32 Vflocate (int32 vkey, char *field)
#else
PUBLIC int32 Vflocate (vkey, field)
int32 vkey;
char * field;
#endif
{
    int32   s;
    register uintn  u;
    vginstance_t  * v;
    VGROUP *vg;
#ifdef OLD_WAY
    VDATA   *vs;
#else
    int32 vskey;
#endif
    char * FUNC = "Vflocate";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    
    for (u = 0; u < vg->nvelt; u++)  {
        if(vg->tag[u]!=VSDESCTAG)
            continue;
#ifdef OLD_WAY
        vs=(VDATA*)VSattach(vg->f,vg->ref[u],"r");
        if(vs==NULL)
            return(FAIL);
        s=VSfexist(vs, field);
        VSdetach(vs);
#else
        vskey=VSattach(vg->f,vg->ref[u],"r");
        if(vskey==FAIL)
            return(FAIL);
        s=VSfexist(vskey, field);
        VSdetach(vskey);
#endif
        if(s==1)
            return (vg->ref[u]); /* found. return vdata's ref */
    }
    
    return (FAIL); /* field not found */
} /* Vflocate */

/* ----------------------- Vinqtagref ------------------------------------- */
/*
* Checks whether the given tag/ref pair already exists in the vgroup.
* RETURNS TRUE if exist
* RETURNS FALSE if not.
* 28-MAR-91 Jason Ng NCSA
*/
#ifdef PROTOTYPE
PUBLIC int32 Vinqtagref (int32 vkey, int32 tag, int32 ref)
#else
PUBLIC int32 Vinqtagref (vkey, tag, ref)
int32 vkey;
int32   tag, ref;
#endif
{
    register uintn   u;
    register uint16     ttag, rref;
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vinqtagref";

    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    ttag = (uint16) tag;
    rref = (uint16) ref;

    for (u=0; u<vg->nvelt; u++)
        if ((ttag==vg->tag[u]) && (rref==vg->ref[u]))
            return (TRUE); /* exist */
    
    return (FALSE); /* does not exist */
} /* Vinqtagref */

/* ------------------------- Vntagrefs ------------------------------- */
/*
* Returns the number (0 or +ve integer) of tag/ref pairs in a vgroup.
* If error, returns FAIL
* 28-MAR-91 Jason Ng NCSA.
*/
#ifdef PROTOTYPE
PUBLIC int32 Vntagrefs (int32 vkey)
#else
PUBLIC int32 Vntagrefs (vkey)
int32 vkey;
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vntagrefs";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    return ( (vg->otag == DFTAG_VG) ? (int32) vg->nvelt : FAIL);
} /* Vntagrefs */

/* -------------------------- Vgettagrefs ----------------------------- */
/*
* Returns n tag/ref pairs from the vgroup into the caller-supplied arrays
* tagrarray and refarray.
* n can be any +ve number, but arrays must be this big.
* RETURNS the total number of (0 or +ve #)  tag/ref pairs returned.
* 28-MAR-91 Jason Ng NCSA.
*
* NOTE: Do not confuse with Vgettagref().
*
*/
#ifdef PROTOTYPE
PUBLIC int32 Vgettagrefs (int32 vkey, int32 tagarray[], int32 refarray[], int32 n)
#else
PUBLIC int32 Vgettagrefs (vkey, tagarray, refarray, n)
int32 vkey;
int32   n;
int32 tagarray[], refarray[];
#endif
{
    int32 i;
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgettagrefs";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }

    if (n > (int32)vg->nvelt)
        n = vg->nvelt;

    for (i=0; i<n; i++) {
        tagarray[i] = (int32)vg->tag[i];
        refarray[i] = (int32)vg->ref[i];
	}
    return (n);
} /* Vgettagrefs */

/* -------------------------- Vgettagref -------------------------------- */
/*
* Returns a specified tag/ref pair from the vgroup.
* User specifies an index. 
* RETURNS FAIL if OK.
* RETURNS SUCCEED if error.
* 12-MAY-91 Jason Ng NCSA.
*
* NOTE: Do not confuse with Vgettagrefs().
*
*/
#ifdef PROTOTYPE
PUBLIC int32 Vgettagref (int32 vkey, int32 which, int32 *tag, int32 *ref)
#else
PUBLIC int32 Vgettagref (vkey, which, tag, ref)
int32 vkey;
int32   which;
int32   *tag, *ref; /* these are returned */
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgettagref";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    
    if (which < 0 || which > (int32)(vg->nvelt-1))
        return (FAIL); /* range err */
    
    *tag  = (int32) vg->tag[which];
    *ref  = (int32) vg->ref[which];
    return (SUCCEED); /* ok */
} /* Vgettagref */


/* -------------------------- VQuerytag -------------------------------- */
/*
 * Return the tag of this Vgroup.
 * Return 0 on failure
 */
#ifdef PROTOTYPE
PUBLIC int32 VQuerytag(int32 vkey)
#else
PUBLIC int32 VQuerytag(vkey)
int32 vkey;
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgettagref";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        return(FAIL);
    } 
  
    /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    
    return ((int32) vg->otag);

} /* VQuerytag */


/* -------------------------- VQueryref -------------------------------- */
/*
  Return the ref of this Vgroup.
  Return FAIL on failure
*/
#ifdef PROTOTYPE
PUBLIC int32 VQueryref(int32 vkey)
#else
PUBLIC int32 VQueryref(vkey)
int32 vkey;
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgettagref";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        return(FAIL);
    } 
  
    /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
    
    return ((int32) vg->oref);

} /* VQueryref */


/* ------------------------ Vaddtagref ---------------------------------- */
/*
 * Inserts a tag/ref pair into the attached vgroup vg.
 * First checks that the tag/ref is unique.
 * If error, returns FAIL or tag/ref is not inserted.
 * If OK, returns the total number of tag/refs in the vgroup (a +ve integer).
 * 28-MAR-91 Jason Ng NCSA.
 */

#ifdef PROTOTYPE
PUBLIC int32 Vaddtagref (int32 vkey, int32 tag, int32 ref)
#else
PUBLIC int32 Vaddtagref ( vkey, tag, ref)
int32 vkey;
int32  tag, ref;
#endif
{
    int32  n, i;
    vginstance_t  * v;
    VGROUP *vg;
    uint16 ttag, rref;
    char * FUNC = "Vaddtagref";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }

#ifdef NO_DUPLICATES
    /* make sure doesn't already exist in the Vgroup */
    ttag = (uint16) tag;
    rref = (uint16) ref;
    for (i = 0; i < vg->nvelt; i++)
        if ((ttag == vg->tag[i]) && (rref == vg->ref[i]))
            return(FAIL); /* exists */
#endif /* NO_DUPLICATES */

    n = vinsertpair(vg, (uint16) tag, (uint16) ref);

    return (n);

} /* Vaddtagref */

/* ------------------------ vinsertpair --------------------------------- */
/*
* Inserts a tag/ref pair into the attached vgroup vg.
* Expand the tag/ref space if necessary
* Returns the total number of tag/refs in the vgroup.
*/

#ifdef PROTOTYPE
int32 vinsertpair (VGROUP *vg, uint16 tag, uint16 ref)
#else
int32 vinsertpair ( vg, tag, ref)
VGROUP      * vg;
uint16      tag, ref;   /* this MUST be uint16 -  private routine */
#endif
{
    char * FUNC = "vinsertpair";
    
    if(vg->nvelt >= (uintn)vg->msize) {
        vg->msize *= 2;
        vg->tag  = (uint16 *) 
            HDregetspace((VOIDP)vg->tag, vg->msize * sizeof(uint16));
        vg->ref  = (uint16 *) 
            HDregetspace((VOIDP)vg->ref, vg->msize * sizeof(uint16));
        
        if((vg->tag == NULL) || (vg->ref == NULL)) {
            HERROR(DFE_NOSPACE);
            return(FAIL);
        }  
    }
    vg->tag[vg->nvelt]   = tag;
    vg->ref[vg->nvelt]   = ref;
    vg->nvelt ++;
    
    vg->marked = TRUE;
    return ((int32) vg->nvelt);
}

/* ==================================================================== */
/* 
* 	Ventries
*	returns the no of entries (+ve integer) in the vgroup vgid.
*  vgid must be an actual id
*  RETURNS FAIL if error
*
*  undocumented
*
*/
#ifdef PROTOTYPE
int32 Ventries (HFILEID f, int32 vgid)    
#else
int32 Ventries (f, vgid)
HFILEID f;
int32   vgid;
#endif
{
    uint8   * vgpack;
	VGROUP 	vg;
    int32   len;
    char * FUNC = "Ventries";

	if (vgid < 1) {
        HERROR(DFE_ARGS);
        return(FAIL);
      }

    len = Hlength(f, DFTAG_VG, (uint16) vgid);
    if(len == FAIL)
        return FAIL;

    vgpack = (uint8 *) HDgetspace(len);
    if(vgpack == NULL)
        return FAIL;

    if ( Hgetelement(f, DFTAG_VG, (uint16)vgid, vgpack) == FAIL) {
        HERROR(DFE_NOVS);
        return (FAIL);
    }

    vunpackvg(&vg,vgpack);

    HDfreespace((VOIDP)vg.tag);
    HDfreespace((VOIDP)vg.ref);

    return( (int32) vg.nvelt);
} /* Ventries */

/* ==================================================================== */
/*
*	Vsetname
* 	gives a name to the VGROUP vg.
*
* RETURN VALUES: SUCCEED for success, FAIL for failure (big suprise, eh?)
*
*	truncates to max length of VGNAMELENMAX 
*/
#ifdef PROTOTYPE
PUBLIC int32 Vsetname (int32 vkey, char *vgname)
#else
PUBLIC int32 Vsetname (vkey, vgname)
int32 vkey;
char        *vgname;
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vsetname";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
#ifdef QAK
printf("Vsetname(): vkey=%d, VFILE=%d, VSLOT=%d\n",vkey,VGID2VFILE(vkey),VGID2SLOT(vkey));
#endif
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }
  
    HIstrncpy(vg->vgname, vgname, VGNAMELENMAX);
    vg->marked = TRUE;
    return(SUCCEED);
} /* Vsetname */

/* ==================================================================== */
/*
*	Vsetclass
* 	assigns a class name to the VGROUP vg.
*
* RETURN VALUES: SUCCEED for success, FAIL for failure (big suprise, eh?)
*
*	truncates to max length of VGNAMELENMAX 
*/

#ifdef PROTOTYPE
PUBLIC int32 Vsetclass (int32 vkey, char *vgclass)
#else
PUBLIC int32 Vsetclass (vkey, vgclass)
int32 vkey;
char *vgclass;
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vsetclass";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
    }

    HIstrncpy(vg->vgclass, vgclass,VGNAMELENMAX);
    vg->marked = TRUE;
    return(SUCCEED);
} /* Vsetclass*/


/* -------------------------------- Visvg --------------------------------- */
/*
* 	Visvg
*	tests if an entry in the vgroup vg is a VGROUP, given the entry's id. 
*
*	RETURNS TRUE if so
*	RETURNS FALSE if not, or if error
*
*/
#ifdef PROTOTYPE
PUBLIC int32 Visvg (int32 vkey, int32 id)
#else
PUBLIC int32 Visvg (vkey, id)
int32 vkey;
int32   id;     /* valid id of the entry in question */
#endif
{
    register uintn u;
    register uint16 ID;
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Visvg";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FALSE);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FALSE);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FALSE);
    }
  
    ID = (uint16) id;

    for(u = 0; u < vg->nvelt; u++)
        if (vg->ref[u] == ID   &&   /* if the ids match, */
                vg->tag[u] == DFTAG_VG)     /* and it is a vgroup */
            return (TRUE);
  
    return (FALSE);
} /* Visvg */

/* -------------------------- Visvs -------------------------------- */

/* Visvs
*  checks if an id in a vgroup refers to a VDATA
*  RETURNS 1 if so
*  RETURNS 0 if not, or if error.
*/

#ifdef PROTOTYPE
PUBLIC int32 Visvs (int32 vkey, int32 id)
#else
PUBLIC int32 Visvs (vkey, id)
int32 vkey;
int32   id;
#endif
{
    register intn i;
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "VSisvs";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FALSE);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FALSE);
    }

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FALSE);
    }

    i = vg->nvelt;
    while(i)
        if (vg->ref[--i] == (uint16)id && vg->tag[i]==VSDESCTAG)
            return(TRUE);
  
  return(FALSE);
} /* Visvs */

/* ======================================================= */
/* 
*	Vgetid
*	
*	Given a vgroup's id, returns the next vgroup's id in the file f .
*	The call Vgetid(f,-1) returns the id of the FIRST vgroup in the file. 
*
*	RETURNS -1 if error
*	RETURNS the next vgroup's id (0 or +ve integer).
*
*	This id is actually the "ref" of the vgroup "tag/ref".
*/

#ifdef PROTOTYPE
PUBLIC int32 Vgetid (HFILEID f,int32 vgid)         
#else
PUBLIC int32 Vgetid (f, vgid)
HFILEID     f;                      /* HDF file handle */
int32   vgid;                   /* current vgid */
#endif
{
	vginstance_t * v;
	vfile_t		* vf;
    VOIDP *t;
	int32 key;
	char * FUNC = "Vgetid";

    if(vgid < -1 ) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */

    if (NULL==(vf = Get_vfile(f))) {
        HERROR(DFE_FNF);
        return(FAIL);
      } /* end if */

    if (vgid == (-1)) { /* check for magic value to return the first group */
        if (NULL == (t=(VOIDP *)tbbtfirst((TBBT_NODE *)*(vf->vgtree))))
            return (FAIL);
        else {
            v=(vginstance_t *)*t;   /* get actual pointer to the vginstance_t */
            return( v->ref); /* rets 1st vgroup's ref */
          } /* end else */
      } /* end if */

	/* look in vgtab for vgid */
#ifdef OLD_WAY
	v = (vf->vgtab).next;
	while(NULL != v) {
        if(v->ref == (uint16)vgid)
            break;
		v = v->next;
	}
        if (v==NULL)
            return (FAIL); /* none found */
	else
            if( v->next ==NULL)
                return (FAIL); /* this is the last vg, no more after it */
            else
                return((v->next)->ref); /* success, return the next vg's ref */
#else

    /* tbbtdfind returns a pointer to the vginstance_t pointer */
    key=VGSLOT2ID(f,vgid);
    t=(VOIDP *)tbbtdfind(vf->vgtree,(VOIDP)&key,NULL);
    if(t == NULL || 
       t == (VOIDP *) tbbtlast((TBBT_NODE *)*(vf->vgtree))) /* couldn't find the old vgid */
        return(FAIL);                                       /* or at the end */
    else
        if (NULL==(t=(VOIDP *)tbbtnext((TBBT_NODE *)t)))      /* get the next node in the tree */
            return (FAIL);
        else {
            v=(vginstance_t *)*t;   /* get actual pointer to the vginstance_t */
            return(v->ref); /* rets 1st vgroup's ref */
          } /* end else */
#endif
} /* Vgetid */


/* ================================================================= */
/*
*	Vgetnext
*
*	Given the id of an entry from a vgroup vg, looks in vg for the next
*	entry after it, and returns its id.
*	The call Vgetnext (vg,-1) returns the id of the FIRST entry in the vgroup.
*
*  Vgetnext will look at only VSET elements in the vgroup.
*  To look at all links in a vgroup, use Vgettagrefs instead.
*
*	RETURNS -1 if error
*	RETURNS the id of the next entry( 0 or +ve integer)  in the vgroup.
*
*	This id is actually the "ref" of the entry's "tag/ref".
*
*/

#ifdef PROTOTYPE
PUBLIC int32 Vgetnext (int32 vkey, int32 id)
#else
PUBLIC int32 Vgetnext (vkey, id)
int32 vkey;
int32 id;     /* actual id of an entry in the vgroup vg */
#endif
{
    register uintn  u;
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgetnext";
    
    if (!VALIDVGID(vkey) || id<(-1)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
    }

    vg=v->vg;
  
    if ((vg == NULL) || (vg->otag != DFTAG_VG)) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */
  
    if (vg->nvelt == 0)
        return(FAIL);             /* nothing in vg */
  
    if (id == -1) {
        if ((vg->tag[0] == DFTAG_VG) || (vg->tag[0]==VSDESCTAG))
            return(vg->ref[0]);       /* id of first entry */
      } /* end if */
  
    /* look in vg for id */
    for(u=0; u<vg->nvelt; u++)
        if ((vg->tag[u]==DFTAG_VG) || (vg->tag[u]==VSDESCTAG)) {
            if(vg->ref[u] == (uint16)id) {
                if (u == (vg->nvelt - 1) )
                    return(FAIL);
                else  {
                    if ((vg->tag[u+1] == DFTAG_VG) || (vg->tag[u+1]==VSDESCTAG))
                        return(vg->ref[u+1]);     /* return the id of next entry */
                    else
                        return (FAIL);
                  } /* end else */
              } /* end if */
          } /* end if */
    
    return (FAIL);
} /* Vgetnext  */

/* ================================================================= */
/*
*	Vgetname
*	returns the vgroup's name
*   ASSUME that vgname has been allocated large enough to hold
*   the name
*
*/

#ifdef PROTOTYPE
PUBLIC void Vgetname (int32 vkey, char *vgname)
#else
PUBLIC void Vgetname (vkey, vgname)
int32 vkey;
char *vgname;            /* its name is returned in this var */
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgetname";
    
    if (!VALIDVGID(vkey) || vgname==NULL) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
      } /* end if */

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return;
      } /* end if */

    HDstrcpy(vgname, vg->vgname);
} /* Vgetname */

/* ================================================================= */
/*
*	Vgetclass
*	returns the vgroup's class name 
*   ASSUME that vgclass has been allocated large enough to hold
*   the name
*
*/

#ifdef PROTOTYPE
PUBLIC void Vgetclass (int32 vkey, char *vgclass)
#else
PUBLIC void Vgetclass (vkey, vgclass)
int32 vkey;
char    *vgclass;   /* its class name is returned in this var */
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vgetclass";
    
    if (!VALIDVGID(vkey) || vgclass==NULL) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return;
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return;
      } /* end if */

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return;
      } /* end if */
  
    HDstrcpy(vgclass, vg->vgclass);
} /* Vgetclass*/

/* ================================================================= */
/*
*	Vinquire
*
*	General inquiry routine for VGROUP. 
*
*	output parameters:
*			nentries - no of entries in the vgroup
*			vgname	- the vgroup's name
*
*	RETURNS FAIL if error
*	RETURNS SUCCEED if ok
*
*/

#ifdef PROTOTYPE
PUBLIC int32 Vinquire (int32 vkey, int32 *nentries, char *vgname)
#else
PUBLIC int32 Vinquire (vkey, nentries, vgname)
int32 vkey;
int32   *nentries;
char        *vgname;
#endif
{
    vginstance_t  * v;
    VGROUP *vg;
    char * FUNC = "Vinquire";
    
    if (!VALIDVGID(vkey)) {
        HERROR(DFE_ARGS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */
  
  /* locate vg's index in vgtab */
    if(NULL==(v=(vginstance_t*)vginstance(VGID2VFILE(vkey),(uint16)VGID2SLOT(vkey)))) {
        HERROR(DFE_NOVS);
        HEprint(stderr, 0);
        return(FAIL);
      } /* end if */

    vg=v->vg;
    if (vg == NULL) {
        HERROR(DFE_BADPTR);
        return(FAIL);
      } /* end if */
  
    if(vg->otag != DFTAG_VG) {
        HERROR(DFE_ARGS);
        return(FAIL);
      } /* end if */

    HDstrcpy(vgname, vg->vgname);
    *nentries = vg->nvelt;
  
    return(SUCCEED);
} /* Vinquire */

/* ================================================================= */

/* ---------------------------- Vopen ------------------------- */
/* 
* 
* This routine will replace the code segment " Hopen(); Vinitialize(f)".
* Thus, if Vopen is used, do not call Vinitialize after that.
* 
* Similar to Hopen().
* INPUTS: 
*		char * path     - file name.
*     int n  access   - type of access. See Hopen().
*     int16  ndds     - no. of dd blocks. See Hopen().
*
* This routine opens the HDF file and initializes it for Vset operations.
*
* RETURN VALUE:
*  if error:  -1 (FAIL).
*  if successful: the id of the file (>0).
*
* See also Vclose().
*
* By: Jason Ng 10 Aug 92
* 
*/

#ifdef PROTOTYPE
PUBLIC HFILEID Vopen( char *path, intn access, int16 ndds)
#else
PUBLIC HFILEID Vopen (path, access, ndds)
char 		*path;
intn 		access;
int16       ndds;
#endif
{
	char * FUNC = "Vopen";
	HFILEID  f;

  	f = Hopen(path, access, ndds);
    if (f==FAIL)
        return(FAIL);

	Vinitialize(f);
    return (f);
}

/* ---------------------------- Vclose ------------------------- */
/* 
* 
* This routine will replace the code segment " Vfinish(f); Hclose(f);".
* Thus, if Vclose is used, do not call Vfinish before that.
*
* This routine closes the HDF file, after it has freed all memory and 
* updated the file.
* 
* INPUTS: 
*     int32   f       - if of HDF file to be closed.
*
* RETURN VALUE:  intn status - result of Hopen().
*
* See also Vopen().
*
* By: Jason Ng 10 Aug 92
* 
*/

#ifdef PROTOTYPE
PUBLIC intn Vclose (HFILEID f)
#else
PUBLIC intn Vclose (f)
HFILEID f;
#endif
{
	char * FUNC = "Vclose";

	Vfinish (f);
    return(Hclose (f));
}

/* ------------------------------- Vdelete -------------------------------- */
/*

  Remove a Vgroup from its file.  This function will both remove the Vgoup
  from the internal Vset data structures as well as from the file.

  (i.e. it calls tbbt_delete() and Hdeldd())

  Return FAIL / SUCCEED

*/
int32
#ifdef PROTOTYPE
Vdelete(int32 f, int32 vgid)
#else
Vdelete(f, vgid)
int32 f;
int32 vgid;
#endif
{

    VOIDP	   v;
    vfile_t      * vf;
    VOIDP        * t;
    int32          key;
    char         * FUNC = "Vdelete";

    if(vgid < 0) {
        HERROR(DFE_ARGS);
        return(FAIL);
    } 

    if (NULL==(vf = Get_vfile(f))) {
        HERROR(DFE_FNF);
        return(FAIL);
    } 

    key=VGSLOT2ID(f,vgid);

    t = (VOIDP *)tbbtdfind(vf->vgtree,(VOIDP)&key,NULL);

    if(t == NULL)
        return FAIL;

    v = tbbtrem((TBBT_NODE **)vf->vgtree, (TBBT_NODE *)t, NULL);
    if(v) 
        vdestroynode((VOIDP)v);

    Hdeldd(f, DFTAG_VG, (uint16) vgid);

    return SUCCEED;
       
} /* Vdelete */
