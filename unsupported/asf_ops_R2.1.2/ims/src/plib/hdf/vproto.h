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
** from vconv.c
*/
#ifdef __cplusplus
extern "C" {
#endif

extern int32 vicheckcompat
  PROTO((HFILEID f));

extern int32 vimakecompat
  PROTO((HFILEID f));

extern int32 vcheckcompat
  PROTO((char _HUGE *fs));

extern int32 vmakecompat
  PROTO((char _HUGE *fs));

/*
** from vg.c
*/
extern uint16 vnewref
  PROTO((HFILEID f));

extern int32 VSelts
  PROTO((int32 vkey));

extern int32 VSgetinterlace
  PROTO((int32 vkey));

extern int32 VSsetinterlace
  PROTO((int32 vkey, int32 interlace));

extern int32 VSgetfields
  PROTO((int32 vkey, char _HUGE *fields));

extern int32 VSfexist
  PROTO((int32 vkey, char _HUGE *fields));

extern int32 VSsizeof
  PROTO((int32 vkey, char _HUGE *fields));

extern VOID VSdump
  PROTO((int32 vkey));

extern void VSsetname
  PROTO((int32 vkey, char _HUGE *vsname));

extern void VSsetclass
  PROTO((int32 vkey, char _HUGE *vsclass));

extern void VSgetname
  PROTO((int32 vkey, char _HUGE *vsname));

extern void VSgetclass
  PROTO((int32 vkey, char _HUGE *vsclass));

extern int32 VSinquire
  PROTO((int32 vkey,int32 _HUGE *nelt, int32 _HUGE *interlace,
    char _HUGE *fields, int32 _HUGE *eltsize, char _HUGE *vsname));

extern int32 VSlone
  PROTO((HFILEID f, int32 _HUGE idarray[], int32 asize));

extern int32 Vlone
  PROTO((HFILEID f, int32 _HUGE idarray[], int32 asize));

extern int32 Vfind
  PROTO((HFILEID f, char _HUGE *vgname));

extern int32 VSfind
  PROTO((HFILEID f, char _HUGE *vsname));

extern VOID Vsetzap
    PROTO((void));

/*
** from vgp.c
*/
extern intn vcompare
    PROTO((VOIDP k1,VOIDP k2,intn cmparg));

extern intn vcompareref
    PROTO((VOIDP k1,VOIDP k2,intn cmparg));

extern VOID vdestroynode
    PROTO((VOIDP n));

extern VOID vtfreekey
    PROTO((VOIDP k));

extern VOID Vinitialize
  PROTO((HFILEID f));

extern intn Vfinish
  PROTO((HFILEID f));

extern HFILEID Vopen
    PROTO(( char *path, intn access, int16 ndds));

extern intn Vclose
    PROTO((HFILEID f));

extern int32 vexistvg
  PROTO((HFILEID f, uint16 vgid));

extern int32 Vattach
  PROTO((HFILEID f, int32 vgid, char _HUGE *accesstype));

extern void Vdetach
  PROTO((int32 vkey));

extern int32 Vinsert
  PROTO((int32 vkey, int32 vskey));
  /* note: 2nd arg of Vinsert can also be (VGROUP *) */

extern int32 Vflocate
  PROTO((int32 vkey, char _HUGE *field));

extern int32 Vinqtagref
  PROTO((int32 vkey, int32 tag, int32 ref));

extern int32 Vntagrefs
  PROTO((int32 vkey));

extern int32 Vgettagrefs
  PROTO((int32 vkey, int32 _HUGE tagarray[], int32 _HUGE refarray[], int32 n));

extern int32 Vgettagref
  PROTO((int32 vkey, int32 which, int32 _HUGE *tag, int32 _HUGE *ref));

extern int32 VQueryref
  PROTO((int32 vkey));

extern int32 VQuerytag
  PROTO((int32 vkey));

extern int32 Vaddtagref
  PROTO((int32 vkey, int32 tag, int32 ref));

extern int32 Ventries
  PROTO((HFILEID f, int32 vgid));

extern int32 Vsetname
  PROTO((int32 vkey, char _HUGE *vgname));

extern int32 Vsetclass
  PROTO((int32 vkey, char _HUGE *vgclass));

extern int32 Visvg
  PROTO((int32 vkey, int32 id));

extern int32 Visvs
    PROTO((int32 vkey, int32 id));

extern int32 Vgetid
  PROTO((HFILEID f, int32 vgid));

extern int32 Vgetnext
  PROTO((int32 vkey, int32 id));

extern void Vgetname
  PROTO((int32 vkey, char _HUGE *vgname));

extern void Vgetclass
  PROTO((int32 vkey, char _HUGE *vgclass));

extern int32 Vinquire
  PROTO((int32 vkey, int32 _HUGE *nentries, char _HUGE *vgname));

extern int32 Vdelete
    PROTO((int32 f, int32 ref));

/*
** from vparse.c
*/
extern int32 scanattrs
  PROTO((char _HUGE *attrs, int32 _HUGE *attrc, char _HUGE ***attrv));


/*
** from vhi.c
*/
extern int32 VHstoredata
  PROTO((HFILEID f, char _HUGE *field, uint8 _HUGE buf[], int32 n, int32 datatype,
    char _HUGE *vsname, char _HUGE *vsclass));

extern int32 VHstoredatam
  PROTO((HFILEID f, char _HUGE *field, uint8 _HUGE buf[], int32 n, int32 datatype,
    char _HUGE *vsname, char _HUGE *vsclass, int32 order));

extern int32 VHmakegroup
  PROTO((HFILEID  f, int32 _HUGE tagarray[], int32 _HUGE refarray[], int32 n, char _HUGE *vgname,
    char _HUGE *vgclass));


/*
** from vio.c
*/

extern int32 vexistvs
    PROTO((HFILEID f, uint16 vsid));

extern VOID vsdestroynode
    PROTO((VOIDP n));

extern int32 VSattach
    PROTO((HFILEID f, int32 vsid, char _HUGE *accesstype));

extern void VSdetach
    PROTO((int32 vkey));

extern int32 VSQuerytag
  PROTO((int32 vkey));

extern int32 VSQueryref
  PROTO((int32 vkey));

extern int32 VSgetid
    PROTO((HFILEID f, int32 vsid));

extern int32 VSgetversion
    PROTO((int32 vkey));

extern int32 VSdelete
    PROTO((int32 f, int32 ref));

extern int32 VSappendable 
    PROTO((int32 vkey, int32 blk));


/*
** from vsfld.c
*/

extern int16 VSIZEOF
    PROTO((int16 x));

extern int16 HDFSIZEOF
    PROTO((int16 x));

extern int32 VSsetfields
  PROTO((int32 vkey, char _HUGE *fields));

extern int32 VSfdefine
  PROTO((int32 vkey, char _HUGE *field, int32 localtype, int32 order));

extern int32 VFnfields 
    PROTO((int32 vkey));

extern char * VFfieldname
    PROTO((int32 vkey, int32 index));

extern int32 VFfieldtype
    PROTO((int32 vkey, int32 index));

extern int32 VFfieldisize
    PROTO((int32 vkey, int32 index));

extern int32 VFfieldesize
    PROTO((int32 vkey, int32 index));

extern int32 VFfieldorder
    PROTO((int32 vkey, int32 index));

/*
** from vrw.c
*/
extern int32 VSseek
  PROTO((int32 vkey, int32 eltpos));

extern int32 VSread
  PROTO((int32 vkey, uint8 _HUGE buf[], int32 nelt, int32 interlace));

extern int32 VSwrite
  PROTO((int32 vkey, uint8 _HUGE buf[], int32 nelt, int32 interlace));

/*
** from vgF.c
*/
#ifndef VG_FNAMES
#   define VG_FNAMES
#ifdef DF_CAPFNAMES
#	define  ndfivopn FNAME(DFIVOPN)
#	define  ndfvclos FNAME(DFVCLOS)
#   define  nvatchc  FNAME(VATCHC)
#   define  nvdtchc  FNAME(VDTCHC)
#   define  nvgnamc  FNAME(VGNAMC)
#   define  nvgclsc  FNAME(VGCLSC)
#   define  nvinqc   FNAME(VINQC)
#   define  nvgidc   FNAME(VGIDC)
#   define  nvgnxtc  FNAME(VGNXTC)
#   define  nvsnamc  FNAME(VSNAMC)
#   define  nvsclsc  FNAME(VSCLSC)
#   define  nvinsrtc FNAME(VINSRTC)
#   define  nvisvgc  FNAME(VISVGC)
#   define  nvisvsc  FNAME(VISVSC)
#   define  nvsatchc FNAME(VSATCHC)
#   define  nvsdtchc FNAME(VSDTCHC)
#   define  nvsseekc FNAME(VSSEEKC)
#   define  nvsgnamc FNAME(VSGNAMC)
#   define  nvsgclsc FNAME(VSGCLSC)
#   define  nvsinqc  FNAME(VSINQC)
#   define  nvsfexc  FNAME(VSFEXC)
#   define  nvsgidc  FNAME(VSGIDC)
#   define  nvssnamc FNAME(VSSNAMC)
#   define  nvssclsc FNAME(VSSCLSC)
#   define  nvssfldc FNAME(VSSFLDC)
#   define  nvssintc FNAME(VSSINTC)
#   define  nvsfdefc FNAME(VSFDEFC)
#   define  nvsreadc FNAME(VSREADC)
#   define  nvswritc FNAME(VSWRITC)
#   define  nvsgintc FNAME(VSGINTC)
#   define  nvseltsc FNAME(VSELTSC)
#   define  nvsgfldc FNAME(VSGFLDC)
#   define  nvssizc  FNAME(VSSIZC)
#   define  nventsc  FNAME(VENTSC)
#   define  nvlonec  FNAME(VLONEC)
#   define  nvslonec FNAME(VSLONEC)
#   define  nvhsdc   FNAME(VHSDC)
#   define  nvhsdmc  FNAME(VHSDMC)
#   define  nvhmkgpc FNAME(VHMKGPC)
#   define  nvflocc  FNAME(VFLOCC)
#   define  nvinqtrc FNAME(VINQTRC)
#   define  nvntrc   FNAME(VNTRC)
#   define  nvgttrsc FNAME(VGTTRSC)
#   define  nvgttrc  FNAME(VGTTRC)
#   define  nvadtrc  FNAME(VADTRC)
#   define  nvfstart FNAME(VFSTART)
#   define  nvfend   FNAME(VFEND)
#else   /* !DF_CAPFNAMES */
#	define  ndfivopn FNAME(dfivopn)
#	define  ndfvclos FNAME(dfvclos)
#   define  nvatchc  FNAME(vatchc)
#   define  nvdtchc  FNAME(vdtchc)
#   define  nvgnamc  FNAME(vgnamc)
#   define  nvgclsc  FNAME(vgclsc)
#   define  nvinqc   FNAME(vinqc)
#   define  nvgidc   FNAME(vgidc)
#   define  nvgnxtc  FNAME(vgnxtc)
#   define  nvsnamc  FNAME(vsnamc)
#   define  nvsclsc  FNAME(vsclsc)
#   define  nvinsrtc FNAME(vinsrtc)
#   define  nvisvgc  FNAME(visvgc)
#   define  nvisvsc  FNAME(visvsc)
#   define  nvsatchc FNAME(vsatchc)
#   define  nvsdtchc FNAME(vsdtchc)
#   define  nvsseekc FNAME(vsseekc)
#   define  nvsgnamc FNAME(vsgnamc)
#   define  nvsgclsc FNAME(vsgclsc)
#   define  nvsinqc  FNAME(vsinqc)
#   define  nvsfexc  FNAME(vsfexc)
#   define  nvsgidc  FNAME(vsgidc)
#   define  nvssnamc FNAME(vssnamc)
#   define  nvssclsc FNAME(vssclsc)
#   define  nvssfldc FNAME(vssfldc)
#   define  nvssintc FNAME(vssintc)
#   define  nvsfdefc FNAME(vsfdefc)
#   define  nvsreadc FNAME(vsreadc)
#   define  nvswritc FNAME(vswritc)
#   define  nvsgintc FNAME(vsgintc)
#   define  nvseltsc FNAME(vseltsc)
#   define  nvsgfldc FNAME(vsgfldc)
#   define  nvssizc  FNAME(vssizc)
#   define  nventsc  FNAME(ventsc)
#   define  nvlonec  FNAME(vlonec)
#   define  nvslonec FNAME(vslonec)
#   define  nvhsdc   FNAME(vhsdc)
#   define  nvhsdmc  FNAME(vhsdmc)
#   define  nvhmkgpc FNAME(vhmkgpc)
#   define  nvflocc  FNAME(vflocc)
#   define  nvinqtrc FNAME(vinqtrc)
#   define  nvntrc   FNAME(vntrc)
#   define  nvgttrsc FNAME(vgttrsc)
#   define  nvgttrc  FNAME(vgttrc)
#   define  nvadtrc  FNAME(vadtrc)
#   define  nvfstart FNAME(vfstart)
#   define  nvfend   FNAME(vfend)
#endif  /* DF_CAPFNAMES */
#endif  /* VG_FNAMES */

extern FRETVAL(intf) ndfivopn
    PROTO((_fcd filename, intf _HUGE *access, intf _HUGE *defdds, intf _HUGE *namelen));

extern FRETVAL(intf) ndfvclos
    PROTO((intf _HUGE *file_id));

extern FRETVAL(intf) nvatchc
    PROTO((HFILEID _HUGE *f, intf _HUGE *vgid, _fcd accesstype));

extern FRETVAL(void) nvdtchc
    PROTO((intf _HUGE *vkey));

extern FRETVAL(void) nvgnamc
    PROTO((intf _HUGE *vkey, _fcd vgname));

extern FRETVAL(void) nvgclsc
    PROTO((intf _HUGE *vkey, _fcd vgclass));

extern FRETVAL(intf) nvinqc
    PROTO((intf _HUGE *vkey, intf _HUGE *nentries, _fcd vgname));

extern FRETVAL(intf) nvgidc
    PROTO((HFILEID _HUGE *f, intf _HUGE *vgid));

extern FRETVAL(intf) nvgnxtc
    PROTO((intf _HUGE *vkey, intf _HUGE *id));

extern FRETVAL(void) nvsnamc
    PROTO((intf _HUGE *vkey, _fcd vgname, intf _HUGE *vgnamelen));

extern FRETVAL(void) nvsclsc
    PROTO((intf _HUGE *vkey, _fcd vgclass, intf _HUGE *vgclasslen));

extern FRETVAL(intf) nvinsrtc
    PROTO((intf _HUGE *vkey, intf _HUGE *vobjptr));

extern FRETVAL(intf) nvisvgc
    PROTO((intf _HUGE *vkey, intf _HUGE *id));

extern FRETVAL(intf) nvisvsc
    PROTO((intf _HUGE *vkey, intf _HUGE *id));

extern FRETVAL(intf) nvsatchc
    PROTO((HFILEID _HUGE *f, intf _HUGE *vsid, _fcd accesstype));

extern FRETVAL(void) nvsdtchc
    PROTO((intf _HUGE *vkey));

extern FRETVAL(intf) nvsseekc
    PROTO((intf _HUGE *vkey, intf _HUGE *eltpos));

extern FRETVAL(void) nvsgnamc
    PROTO((intf _HUGE *vkey, _fcd vsname));

extern FRETVAL(void) nvsgclsc
    PROTO((intf _HUGE *vkey, _fcd vsclass));

extern FRETVAL(intf) nvsinqc
    PROTO((intf _HUGE *vkey, intf _HUGE *nelt ,intf _HUGE *interlace,
        _fcd fields, intf _HUGE *eltsize, _fcd vsname));

extern FRETVAL(intf) nvsfexc
    PROTO((intf _HUGE *vkey, _fcd fields, intf _HUGE *fieldslen));

extern FRETVAL(intf) nvsgidc
    PROTO((HFILEID _HUGE *f, intf _HUGE *vsid));

extern FRETVAL(void) nvssnamc
    PROTO((intf _HUGE *vkey, _fcd vsname,intf _HUGE *vsnamelen));

extern FRETVAL(void) nvssclsc
    PROTO((intf _HUGE *vkey, _fcd vsclass, intf _HUGE *vsclasslen));

extern FRETVAL(intf) nvssfldc
    PROTO((intf _HUGE *vkey, _fcd fields, intf _HUGE *fieldslen));

extern FRETVAL(intf) nvssintc
    PROTO((intf _HUGE *vkey, intf _HUGE *interlace));

extern FRETVAL(intf) nvsfdefc
    PROTO((intf _HUGE *vkey, _fcd field, intf _HUGE *localtype,
            intf _HUGE *order, intf _HUGE *fieldlen));

extern FRETVAL(intf) nvsreadc
    PROTO((intf _HUGE *vkey, uint8 _HUGE *buf, intf _HUGE *nelt,
            intf _HUGE *interlace));

extern FRETVAL(intf) nvswritc
    PROTO((intf _HUGE *vkey, uint8 _HUGE *buf, intf _HUGE *nelt,
            intf _HUGE *interlace));

extern FRETVAL(intf) nvsgintc
    PROTO((intf _HUGE *vkey));

extern FRETVAL(intf) nvseltsc
    PROTO((intf _HUGE *vkey));

extern FRETVAL(intf) nvsgfldc
    PROTO((intf _HUGE *vkey, _fcd fields));

extern FRETVAL(intf) nvssizc
    PROTO((intf _HUGE *vkey, _fcd fields, intf _HUGE *fieldslen));

extern FRETVAL(intf) nventsc
    PROTO((HFILEID _HUGE *f,intf _HUGE *vgid));

extern FRETVAL(intf) nvlonec
    PROTO((HFILEID _HUGE *f, intf _HUGE **idarray, intf _HUGE *asize));

extern FRETVAL(intf) nvslonec
    PROTO((HFILEID _HUGE *f, intf _HUGE **idarray, intf _HUGE *asize));

extern FRETVAL(intf) nvhsdc
    PROTO((HFILEID _HUGE *f, _fcd field, uint8 _HUGE *buf, intf _HUGE *n, intf _HUGE *datatype,
        _fcd vsname, _fcd vsclass, intf _HUGE *fieldlen, intf _HUGE *vsnamelen,
        intf _HUGE *vsclasslen));

extern FRETVAL(intf) nvhsdmc
    PROTO((HFILEID _HUGE *f, _fcd field, uint8 _HUGE *buf, intf _HUGE *n, intf _HUGE *datatype,
        _fcd vsname, _fcd vsclass, intf _HUGE *order, intf _HUGE *fieldlen,
        intf _HUGE *vsnamelen, intf _HUGE *vsclasslen));

extern FRETVAL(intf) nvhmkgpc
    PROTO((HFILEID _HUGE *f, intf _HUGE *tagarray, intf _HUGE *refarray, intf _HUGE *n,
        _fcd vgname, _fcd vgclass,  intf _HUGE *vgnamelen, intf _HUGE *vgclasslen));

extern FRETVAL(intf) nvflocc
    PROTO((intf _HUGE *vkey, _fcd field, intf _HUGE *fieldlen));

extern FRETVAL(intf) nvinqtrc
    PROTO((intf _HUGE *vkey, intf _HUGE *tag, intf _HUGE *ref));

extern FRETVAL(intf) nvntrc
    PROTO((intf _HUGE *vkey));

extern FRETVAL(intf) nvgttrsc
    PROTO((intf _HUGE *vkey, intf _HUGE *tagarray, intf _HUGE *refarray, intf _HUGE *n));

extern FRETVAL(intf) nvgttrc
    PROTO((intf _HUGE *vkey, intf _HUGE *which, intf _HUGE *tag, intf _HUGE *ref));

extern FRETVAL(intf) nvadtrc
    PROTO((intf _HUGE *vkey, intf _HUGE *tag, intf _HUGE *ref));

#ifdef __cplusplus
}
#endif


