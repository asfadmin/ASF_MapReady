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

/*
**  remove trailing blanks from a string. input argument is a  string
**  and *MUST* be a variable and not a constant!! For internal use only!!
**  Used only on Crays where the Fortran compiler will pad strings to the 
**  nearest 8-byte boundary.
*/

void
#ifdef PROTOTYPE
trimendblanks(char *ss) 
#else
trimendblanks(ss) 
    char *ss;
#endif
{
    int32 i,n;

    n = HDstrlen(ss);
	for(i=n-1;i>=0;i--) {
		if(ss[i]!=' ') {
			ss[i+1]='\0';
			break;
		}
	}
}

/* ================================================== */
/*  VGROUP routines                                   */
/* ================================================== */

/*-----------------------------------------------------------------------------
 * Name:    dfivopn
 * Purpose: Fortran stub for dfvopen to call DFvsetopen to open HDF file
 * Inputs:  name: name of file to open
 *          access: access mode - integer with value DFACC_READ etc.
 *          defdds: default number of DDs per header block
 *          namelen: length of name
 * Returns: 0 on success, -1 on failure with error set
 * Users:   HDF Fortran programmers
 * Invokes: Hopen
 * Method:  Convert filename to C string, call Hopen
 *---------------------------------------------------------------------------*/

   FRETVAL(intf)
#ifdef PROTOTYPE
ndfivopn(_fcd name, intf *access, intf *defdds, intf *namelen)
#else
ndfivopn(name, access, defdds, namelen)
_fcd name;
intf *access;
intf *defdds;
intf *namelen;
#endif /* PROTOTYPE */
{
   char *fn;
   intf ret;

   fn = HDf2cstring(name, (intn)*namelen);
   ret = (intf)Vopen(fn, (intn)*access, (int16)*defdds);
   HDfreespace(fn);
   return(ret);
}	/* end ndfivopn() */

/*-----------------------------------------------------------------------------
 * Name:    dfvclos
 * Purpose: Call DFvsetclose to close HDF file
 * Inputs:  file_id: handle to HDF file to close
 * Returns: 0 on success, FAIL on failure with error set
 * Users:   HDF Fortran programmers
 * Invokes: Hclose
 *---------------------------------------------------------------------------*/

FRETVAL(intf)
#ifdef PROTOTYPE
ndfvclos(intf *file_id)
#else
ndfvclos(file_id)
intf *file_id;
#endif /* PROTOTYPE */
{
   return(Vclose(*file_id));
}	/* ndfvclos() */

/* 
**  attach to a vgroup and returns its ptr
**  related: Vattach--vatchc--VFATCH
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvatchc(HFILEID *f, intf *vgid, _fcd accesstype)
#else
nvatchc(f, vgid, accesstype)
    HFILEID *f;
    intf       *vgid;
    _fcd        accesstype;                     /* assume one character only */
#endif
{
    int32 vkey;
	char	 *acc;

    acc = HDf2cstring (accesstype, 1);

    vkey = Vattach(*f, *vgid, acc);
    HDfreespace(acc);

    return(vkey);
}

/* ------------------------------------------------------------------ */

/* 
**  detach from a vgroup
**  related: Vdetach--vdtchc--VFDTCH
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvdtchc(intf *vkey)
#else
nvdtchc(vkey)
intf *vkey;
#endif
{
    Vdetach(*vkey);
}
/* ------------------------------------------------------------------ */

/* 
**  get the name of a vgroup
**  related: Vgetname--vgnamc--VFGNAM
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvgnamc(intf *vkey, _fcd vgname)
#else
nvgnamc(vkey, vgname)
intf *vkey;
_fcd    vgname;             /* output */
#endif
{
    Vgetname (*vkey, vgname);
}   /* VGNAMC */

/* ------------------------------------------------------------------ */
/* 
**  get the class name of a vgroup
**  related: Vgetclass--vgclsc--VFGCLS
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvgclsc(intf *vkey, _fcd vgclass)
#else
nvgclsc(vkey, vgclass)
intf *vkey;
_fcd        vgclass;                /* output */
#endif
{
     Vgetclass (*vkey, vgclass);
}   /* VGCLSC */

/* ------------------------------------------------------------------ */
/* 
**  general inquiry on a vgroup 
**  related: Vinquire--vinqc--VFINQ
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvinqc(intf *vkey, intf *nentries, _fcd vgname)
#else
nvinqc(vkey, nentries, vgname)
intf *vkey;
intf    *nentries;          /* output */
_fcd    vgname;             /* output */
#endif
{
    return( (intf) Vinquire(*vkey, (int32 *)nentries, vgname) );
} /* VINQC */


/* ------------------------------------------------------------------ */
/* 
**  gets the id of the next vgroup in the file
**  related: Vgetid--vgidc--VFGID
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvgidc(HFILEID *f, intf *vgid)
#else
nvgidc(f,vgid)
HFILEID     *f;
intf        *vgid;
#endif
{
    return( (intf) Vgetid (*f, *vgid) );
}

/* ------------------------------------------------------------------ */
/* 
**  gets the id of the next entry in the vgroup
**  related: Vgetnext--vgnxtc--VFGNXT
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvgnxtc(intf *vkey, intf *id)
#else
nvgnxtc(vkey,id)
intf *vkey;
intf    *id;
#endif
{
    return( Vgetnext(*vkey, *id) );
}

/* ------------------------------------------------------------------ */
/* 
**  sets the name of the vgroup
**  related: Vsetname--vsnamc--VFSNAM
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvsnamc(intf *vkey, _fcd vgname, intf *vgnamelen)
#else
nvsnamc(vkey, vgname, vgnamelen)
intf *vkey;
_fcd    vgname;
intf    *vgnamelen;
#endif
{
	char *name;

    name = HDf2cstring (vgname, (intn)*vgnamelen);
	/* trimendblanks(name); */
    Vsetname (*vkey, name);
    HDfreespace (name);
}

/* ------------------------------------------------------------------ */
/* 
**  sets the class name of the vgroup
**  related: Vsetclass--vsclsc--VFSCLS
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvsclsc(intf *vkey, _fcd vgclass, intf *vgclasslen)
#else
nvsclsc(vkey, vgclass, vgclasslen)
intf *vkey;
_fcd    vgclass;
intf    *vgclasslen;
#endif
{
	char *class;

    class = HDf2cstring (vgclass, (intn)*vgclasslen);
	/* trimendblanks(class); */
    Vsetclass (*vkey, class);
    HDfreespace (class);
}

/* ------------------------------------------------------------------ */
/* 
**  inserts a vset object (ie a vgroup or vdata ptr) into the given vgroup
**  related: Vinsert--vinsrtc--VFINSRT
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvinsrtc(intf *vkey, intf *vobjptr)
#else
nvinsrtc(vkey, vobjptr)
intf *vkey;
intf *vobjptr;
#endif
{
    return( (intf) Vinsert(*vkey, *vobjptr) );
}

/* ------------------------------------------------------------------ */
/* 
**  tests if a vset object (having id id) in a vgroup refers to a vgroup
**  related: Visvg--visvgc--VFISVG
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvisvgc(intf *vkey, intf *id)
#else
nvisvgc(vkey, id)
intf *vkey;
intf    *id;
#endif
{
    return( (intf) Visvg(*vkey, *id) );
}

/* ------------------------------------------------------------------ */
/* 
**  wrapper for Vstart
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvfstart(HFILEID *f)
#else
nvfstart(f)
HFILEID *f;
#endif
{
    Vstart(*f);
} /* nvfstart */

/* ------------------------------------------------------------------ */
/* 
**  wrapper for Vend
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvfend(HFILEID *f)
#else
nvfend(f)
HFILEID *f;
#endif
{
    return( (intf) Vend(*f));
} /* nvfend */

/* ------------------------------------------------------------------ */
/* 
**  tests if an id in a vgroup refers to a vdata
**  related: Visvs--visvsc--VFISVS
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvisvsc(intf *vkey, intf *id)
#else
nvisvsc(vkey, id)
intf *vkey;
intf    *id;
#endif
{
    return( (intf) Visvs(*vkey, *id) );
}

/* ================================================== */
/*  VDATA routines                                    */
/* ================================================== */

/* 
**  attach to a vdata and returns its ptr
**  related: VSattach--vsatchc--VFATCH
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsatchc(HFILEID *f, intf *vsid, _fcd accesstype)
#else
nvsatchc(f, vsid, accesstype)
HFILEID     *f;
intf        *vsid;
_fcd        accesstype;
#endif
{
    intf    vkey;
	char 	*acc;

    acc = HDf2cstring (accesstype, 1); /* 'r' or 'w' only */
    vkey =  VSattach(*f, *vsid, acc);
    HDfreespace(acc);

    return(vkey);
}

/* ------------------------------------------------------------------ */
/*  
**  detach from a vdata
**  related: VSdetach--vsdtchc--VFDTCH
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvsdtchc(intf *vkey)
#else
nvsdtchc(vkey)
intf *vkey;
#endif
{
    VSdetach (*vkey);
}

/* ------------------------------------------------------------------ */
/* 
**  seeks to a given element position in a vadata
**  related: VSseek--vsseekc--VSFSEEK
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsseekc(intf *vkey, intf *eltpos)
#else
nvsseekc(vkey, eltpos)
intf *vkey;
intf *eltpos;
#endif
{
    return( (intf) VSseek(*vkey, *eltpos) );
}

/* ------------------------------------------------------------------ */
/* 
**  gets the name of a vdata
**  related: VSgetname--vsgnamc--VSFGNAM
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvsgnamc(intf *vkey, _fcd vsname)
#else
nvsgnamc(vkey, vsname)
intf *vkey;
_fcd    vsname;
#endif
{
    VSgetname (*vkey, vsname);
}	/* VSGNAMC */

/* ------------------------------------------------------------------ */
/* 
**  get the class name of a vdata
**  related: VSgetclass--vsgclsc--VSFGCLS
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvsgclsc(intf *vkey, _fcd vsclass)
#else
nvsgclsc(vkey, vsclass)
intf *vkey;
_fcd    vsclass;                    /* output */
#endif
{
    VSgetclass(*vkey, vsclass);
}	/* VSGCLSC */

/* ------------------------------------------------------------------ */
/*
**  general inquiry on a vdata
**  related: VSinquire--vsinqc--VSFINQ
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsinqc(intf *vkey, intf *nelt ,intf *interlace, _fcd fields, intf *eltsize,
    _fcd vsname)
#else
nvsinqc(vkey, nelt ,interlace, fields, eltsize, vsname)
intf *vkey;
intf    *nelt, *interlace, *eltsize;            /* outputs */
_fcd    fields, vsname;                         /* outputs */
#endif
{
    return( (intf) VSinquire (*vkey, (int32 *)nelt, (int32 *)interlace,
            fields, (int32 *)eltsize, vsname) );
} 	/* VSINQC */


/* ------------------------------------------------------------------ */
/* 
**  tests if given fields exist in the vdata
**  related: VSfexist--vsfexc--VSFEX
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsfexc(intf *vkey, _fcd fields, intf *fieldslen)
#else
nvsfexc(vkey, fields, fieldslen)
intf *vkey;
_fcd    fields;
intf    *fieldslen;
#endif
{
    intf    stat;
	char		*flds;

    flds = HDf2cstring (fields, (intn)*fieldslen );
	/* trimendblanks(flds); */
    stat =  (int32) VSfexist(*vkey, flds);
    HDfreespace (flds);

	return (stat);
}

/* ------------------------------------------------------------------ */
/* 
**  gets the id of the next vdata from the file
**  related: VSgetid--vsgidc--VSFGID
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsgidc(HFILEID *f, intf *vsid)
#else
nvsgidc(f, vsid)
    HFILEID *f;
    intf    *vsid;
#endif
{
    return( (intf) VSgetid(*f, *vsid) );
}

/* ------------------------------------------------------------------ */
/* 
**  sets the name of a vdata
**  related: VSsetname--vssnamc--VSFSNAM
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvssnamc(intf *vkey, _fcd vsname,intf *vsnamelen)
#else
nvssnamc(vkey, vsname, vsnamelen)
intf *vkey;
_fcd   vsname;
intf   *vsnamelen;
#endif
{
	char   *name;

    name = HDf2cstring(vsname, (intn)*vsnamelen);
	/* trimendblanks (name); */
    VSsetname (*vkey, name);
    HDfreespace (name);
}

/* ------------------------------------------------------------------ */
/* 
**  sets the class name of the vdata
**  related: VSsetclass--vssclsc--VSFSCLS
*/

    FRETVAL(void)
#ifdef PROTOTYPE
nvssclsc(intf *vkey, _fcd vsclass, intf *vsclasslen)
#else
nvssclsc(vkey, vsclass, vsclasslen)
intf *vkey;
_fcd    vsclass;
intf    *vsclasslen;
#endif
{
	char 	*class;

    class = HDf2cstring (vsclass, (intn)*vsclasslen);
	/* trimendblanks(class); */
    VSsetclass (*vkey, class);
    HDfreespace (class);
}

/* ------------------------------------------------------------------ */
/* 
**  sets the fields in a vdata for reading or writing
**  related: VSsetfields--vssfldc--VSFSFLD
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvssfldc(intf *vkey, _fcd fields, intf *fieldslen)
#else
nvssfldc(vkey, fields, fieldslen)
intf *vkey;
_fcd    fields;
intf    *fieldslen;
#endif
{
	char 	*flds;
    intf    stat;

    flds = HDf2cstring (fields, (intn)*fieldslen);
	/* trimendblanks(flds); */
    stat =  (int32) VSsetfields (*vkey, flds);
    HDfreespace (flds);

	return(stat);
}

/* ------------------------------------------------------------------ */
/* 
**  sets the file interlace of a vdata
**  related: VSsetinterlace--vssintc--VSFSINT
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvssintc(intf *vkey, intf *interlace)
#else
nvssintc(vkey, interlace)
intf *vkey;
intf    *interlace;
#endif
{
    return( (intf) VSsetinterlace (*vkey, *interlace) );
}

/* ------------------------------------------------------------------ */
/* 
**  defines a new field to be used in the vdata
**  related: VSfdefine--vsfdefc--VSFFDEF
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsfdefc(intf *vkey, _fcd field, intf *localtype, intf *order, intf *fieldlen)
#else
nvsfdefc(vkey, field, localtype, order, fieldlen)
intf *vkey;
_fcd    field;
intf    *localtype, *order;
intf    *fieldlen;
#endif
{
    intf    stat;
    char    *fld;

    fld  = HDf2cstring (field, (intn)*fieldlen);
	/* trimendblanks(fld); */
    stat =  (int32) VSfdefine(*vkey, fld, *localtype, *order );
    HDfreespace(fld);
	return (stat);
}

/* ------------------------------------------------------------------ */
/* 
**  reads from a vdata
**  related: VSread--vsreadc--VSFREAD
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsreadc(intf *vkey, uint8 *buf, intf *nelt, intf *interlace)
#else
nvsreadc(vkey, buf, nelt, interlace)
intf *vkey;
uint8        *buf;
intf        *nelt, *interlace;
#endif
{
    return( (intf) VSread(*vkey, buf, *nelt, *interlace) );
}

/* ------------------------------------------------------------------ */
/* 
**  writes to a vdata
**  related: VSwrite--vswritc--VSFWRIT
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvswritc(intf *vkey, uint8 *buf, intf *nelt, intf *interlace)
#else
nvswritc(vkey, buf, nelt, interlace)
intf *vkey;
uint8        *buf;
intf        *nelt, *interlace;
#endif
{
    return( (intf) VSwrite(*vkey, buf, *nelt, *interlace) );
}

/* ======================================== */
/* miscellaneous VDATA inquiry routines */
/* ======================================== */
/* undocumented */

/* ------------------------------------------------------------------ */
/* 
**  gets the interlace of the vdata
**  related: VSgetinterlace--vsgintc--VSFGINT
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsgintc(intf *vkey)
#else
nvsgintc(vkey)
intf *vkey;
#endif
{
    return( (intf) VSgetinterlace(*vkey) );
}

/* ------------------------------------------------------------------ */
/* 
**  gets the number of elements in the vdata
**  related: VSelts--vseltsc--VSFELTS
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvseltsc(intf *vkey)
#else
nvseltsc(vkey)
intf *vkey;
#endif
{
    return( (intf) VSelts (*vkey) );
}

/* ------------------------------------------------------------------ */
/* 
**  gets the fields in the vdata
**  related: VSgetfields--vsgfldc--VSFGFLD
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvsgfldc(intf *vkey, _fcd fields)
#else
nvsgfldc(vkey, fields)
intf *vkey;
_fcd    fields;         /* output */
#endif
{
    return ( (intf) VSgetfields (*vkey, fields) );
}   /* VSGFLDC */

/* ------------------------------------------------------------------ */
/* 
**  determines the (machine) size of the given fields
**  related: VSsizeof--vssizc--VSFSIZ
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvssizc(intf *vkey, _fcd fields, intf *fieldslen)
#else
nvssizc(vkey, fields, fieldslen)
intf *vkey;
_fcd    fields;
intf    *fieldslen;
#endif
{
	char 	*flds;
    intf    stat;

    flds = HDf2cstring (fields, (intn)*fieldslen);
	/* trimendblanks(flds); */
    stat =  VSsizeof(*vkey, flds);
    HDfreespace(flds);
	return (stat);
}

/* ------------------------------------------------------------------ */
/*
**  determines the no of entries in a vgroup
**  related: Ventries--ventsc--VFENTS
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nventsc(HFILEID *f,intf *vgid)
#else
nventsc(f,vgid)
HFILEID *f;
intf    *vgid;
#endif
{
    return( (intf) Ventries (*f,*vgid) );
}

/* ------------------------------------------------------------------ */
/* 
**  gets the refs of all lone vgroups in the file
**  related: Vlone--vlonec--VFLONE
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvlonec(HFILEID *f, intf **idarray, intf *asize)
#else
nvlonec(f, idarray, asize)
HFILEID     *f;
intf    **idarray;          /* output -- an integer array */
intf    *asize;
#endif
{
    return( (intf) Vlone( *f, (int32 *)*idarray, (int32)*asize) );
}

/* ------------------------------------------------------------------ */
/*
**  gets the refs of all lone vdatas in the file
**  related: VSlone--vslonec--VSFLONE
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvslonec(HFILEID *f, intf **idarray, intf *asize)
#else
nvslonec(f, idarray, asize)
HFILEID     *f;
intf    **idarray;      /* output -- an integer array */
intf    *asize;
#endif
{
    return( VSlone( *f, (int32 *)*idarray, (int32)*asize) );
}

/*
** ==================================================================
** HIGH-LEVEL VSET ROUTINES --- VHxxxxx()
** ==================================================================
*/

/*
**  store a simple dataset in a vdata 
**  related: VHstoredata--vhsdc--vhfsd
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvhsdc(HFILEID *f, _fcd field, uint8 *buf, intf *n, intf *datatype, _fcd vsname,
    _fcd vsclass, intf *fieldlen, intf *vsnamelen, intf *vsclasslen)
#else
nvhsdc(f, field, buf, n, datatype, vsname, vsclass, fieldlen, vsnamelen,
        vsclasslen)
HFILEID     *f;
_fcd    field, vsname, vsclass;
intf    *n, *datatype;
uint8    *buf;
intf    *fieldlen, *vsnamelen, *vsclasslen;
#endif
{
    char *fld, *name, *class;
    intf ret_val;

    fld = HDf2cstring(field, (intn)*fieldlen);
    name = HDf2cstring(vsname, (intn)*vsnamelen);
    class = HDf2cstring(vsclass, (intn)*vsclasslen);

    ret_val=(intf) VHstoredata (*f, fld, buf, *n, *datatype, name, class);
    HDfreespace(fld);
    HDfreespace(name);
    HDfreespace(class);

    return(ret_val);
}

/* ------------------------------------------------------------------ */
/*
**  store an aggregate dataset in a vdata
**  related: VHstoredatam--vhsdmc--vhfsdm
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvhsdmc(HFILEID *f, _fcd field, uint8 *buf, intf *n, intf *datatype,
        _fcd vsname, _fcd vsclass, intf *order, intf *fieldlen,
        intf *vsnamelen, intf *vsclasslen)
#else
nvhsdmc(f, field, buf, n, datatype, vsname, vsclass, order,
           fieldlen, vsnamelen, vsclasslen)

HFILEID *f;
_fcd    field, vsname, vsclass;
intf    *n, *datatype, *order;
uint8    *buf;
intf    *fieldlen, *vsnamelen, *vsclasslen;
#endif
{
    char *fld, *name, *class;
    intf ret_val;

    fld = HDf2cstring(field, (intn)*fieldlen);
    name = HDf2cstring(vsname, (intn)*vsnamelen);
    class = HDf2cstring(vsclass, (intn)*vsclasslen);

    ret_val=(intf)VHstoredatam(*f,fld,buf,*n,*datatype,name,class,*order);
    HDfreespace(fld);
    HDfreespace(name);
    HDfreespace(class);

    return(ret_val);
}

/* ------------------------------------------------------------------ */
/*
**  make a new vgroup given several tag/ref pairs 
**  related: VHmakegroup--vhmkgpc--vhfmkgp
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvhmkgpc(HFILEID *f, intf *tagarray, intf *refarray, intf *n, _fcd vgname,
    _fcd vgclass, intf *vgnamelen, intf *vgclasslen)
#else
nvhmkgpc(f, tagarray, refarray , n, vgname, vgclass, vgnamelen, vgclasslen)
HFILEID *f;
_fcd    vgname, vgclass;
intf    *n, *tagarray, *refarray;
intf    *vgnamelen, *vgclasslen;
#endif
{
    char *gname, *gclass;
    intf ret_val;

    gname = HDf2cstring(vgname, (intn)*vgnamelen);
    gclass = HDf2cstring(vgclass, (intn)*vgclasslen);

    ret_val=(intf)VHmakegroup (*f, (int32 *)tagarray, (int32 *)refarray,
            *n, gname, gclass);
    HDfreespace(gname);
    HDfreespace(gclass);

    return(ret_val);
}

/* ================================================================== */
/*
**  locate a field in a vdata that belongs to this VGROUP
**  related: Vflocate--vffloc--vflocc
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvflocc(intf *vkey, _fcd field, intf *fieldlen)
#else
nvflocc(vkey, field, fieldlen)
intf *vkey;
_fcd    field;
intf    *fieldlen;
#endif
{
    char  *fld;
    intf  stat;

    fld = HDf2cstring (field, (intn)*fieldlen);
	/* trimendblanks(fld); */
    stat = (int32) Vflocate (*vkey, fld);
    HDfreespace(fld);

	return(stat);
}

/* ------------------------------------------------------------------ */
/* 
**  tests if a tag/ref pair is in a vgroup.
**  related: Vinqtagref--vinqtrc--vfinqtr
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvinqtrc(intf *vkey, intf *tag, intf *ref)
#else
nvinqtrc(vkey, tag, ref)
intf *vkey;
intf    *tag, *ref;
#endif
{
    return ( (intf) Vinqtagref ( *vkey, *tag, *ref) );
}
/* ------------------------------------------------------------------ */
/* 
**  gets the number of tag/refs stored in a vgroup
**  related: Vntagrefs--vntrc--VFNTR
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvntrc(intf *vkey)
#else
nvntrc(vkey)
intf *vkey;
#endif
{
    return ( (intf) Vntagrefs (*vkey) );
}
/* ------------------------------------------------------------------ */

/*
**  returns all the tag/ref pairs in a vgroup 
**  related: Vgettagrefs--vgttrsc--vfgttrs
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvgttrsc(intf *vkey, intf *tagarray, intf *refarray, intf *n)
#else
nvgttrsc(vkey, tagarray, refarray, n)
intf *vkey;
intf *tagarray, *refarray;       /* outputs - integer arrays */
intf *n;
#endif
{
    return ( (intf)  Vgettagrefs (*vkey, (int32 *)tagarray, (int32 *)refarray, *n) );
}

/* ------------------------------------------------------------------ */
/*
**  returns a specified tag/ref pair in a vgroup 
**  related: Vgettagref--vgttrc--vfgttr
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvgttrc(intf *vkey, intf *which, intf *tag, intf *ref)
#else
nvgttrc(vkey, which, tag, ref)
intf *vkey;
intf    *which;
intf    *tag, *ref;                     /* outputs */
#endif
{
    return ( (intf) Vgettagref (*vkey, *which, (int32 *)tag, (int32 *)ref) );
}
/* ------------------------------------------------------------------ */

/* 
**  tests if a tag/ref pair is in a vgroup.
**  related: Vinqtagref--vinqtrc--vfinqtr
*/

    FRETVAL(intf)
#ifdef PROTOTYPE
nvadtrc(intf *vkey, intf *tag, intf *ref)
#else
nvadtrc(vkey, tag, ref)
intf *vkey;
intf    *tag, *ref;
#endif
{
    return ( (intf) Vaddtagref ( *vkey, *tag, *ref) );
}
/* ------------------------------------------------------------------ */
