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

/* --------------------------------------------------------------------- */
/*                           !!!! NOTE !!!!
 *  The Convex does not like prototypes of the form:
 *
 *  extern foo PROTO((VOID));
 *
 *  If the only parameter is of type VOID you *MUST* declare the function
 *  as:
 *
 *  extern foo PROTO((void));
 */
/* --------------------------------------------------------------------- */

#ifndef _H_PROTO
#define _H_PROTO

#ifdef __cplusplus
extern "C" {
#endif

/*
** from hfile.c 
*/
extern int32 Hopen 
  PROTO((char _HUGE *path, intn access, int16 ndds));

extern intn Hclose
  PROTO((int32 file_id));

extern int32 Hstartread
  PROTO((int32 file_id, uint16 tag, uint16 ref));

extern intn Hnextread
  PROTO((int32 access_id, uint16 tag, uint16 ref, int origin));

extern intn Hfind
    PROTO((int32 file_id, uint16 search_tag, uint16 search_ref,
        uint16 *find_tag,uint16 *find_ref,int32 *find_offset,
        int32 *find_length,intn direction));

extern intn Hinquire
  PROTO((int32 access_id, int32 _HUGE *pfile_id, uint16 _HUGE *ptag,
        uint16 _HUGE *pref,int32 _HUGE *plength, int32 _HUGE *poffset,
        int32 _HUGE *pposn, int16 _HUGE *paccess, int16 _HUGE *pspecial));

extern int32 Hstartwrite
  PROTO((int32 file_id, uint16 tag, uint16 ref, int32 length));

extern intn Happendable
  PROTO((int32 aid));

extern intn Hseek
  PROTO((int32 access_id, int32 offset, int origin));

extern int32 Hread
  PROTO((int32 access_id, int32 length, uint8 _HUGE *data));

extern int32 Hwrite
  PROTO((int32 access_id, int32 length, uint8 _HUGE *data));

extern int32 Htrunc
  PROTO((int32 access_id, int32 trunc_len));

extern int32 Hendaccess
  PROTO((int32 access_id));

extern int32 Hgetelement
  PROTO((int32 file_id, uint16 tag, uint16 ref, uint8 _HUGE *data));

extern int Hputelement
  PROTO((int32 file_id, uint16 tag, uint16 ref, uint8 _HUGE *data, int32 length));

extern int32 Hlength
  PROTO((int32 file_id, uint16 tag, uint16 ref));

extern int32 Hoffset
  PROTO((int32 file_id, uint16 tag, uint16 ref));

extern int Hdupdd
  PROTO((int32 file_id, uint16 tag, uint16 ref, uint16 old_tag, uint16 old_ref));

extern int Hdeldd
  PROTO((int32 file_id, uint16 tag, uint16 ref));

extern uint16 Hnewref
  PROTO((int32 file_id));

extern int Hsync
  PROTO((int32 file_id));

extern int Hnumber
  PROTO((int32 file_id, uint16 tag));

extern int Hgetlibversion
  PROTO((uint32 _HUGE *majorv, uint32 _HUGE *minorv,
        uint32 _HUGE *release, char _HUGE *string));

extern int Hgetfileversion
  PROTO((int32 file_id, uint32 _HUGE *majorv, uint32 _HUGE *minorv,
        uint32 _HUGE *release, char _HUGE *string));

#if defined WIN3
extern int32 HDfreadbig
  PROTO((VOIDP buffer, int32 size,HFILE fp));

extern int32 HDfwritebig
  PROTO((VOIDP buffer, int32 size,HFILE fp));

#else  /* !WIN3 */
#if defined PC
extern int32 HDfreadbig
  PROTO((VOIDP buffer, int32 size,FILE _HUGE *fp));

extern int32 HDfwritebig
  PROTO((VOIDP buffer, int32 size,FILE _HUGE *fp));
#endif /* PC */
#endif /* !WIN3 */

extern uint16 HDmake_special_tag
  PROTO((uint16 tag));

extern bool HDis_special_tag
  PROTO((uint16 tag));

extern uint16 HDbase_tag
  PROTO((uint16 tag));

extern int HDerr
  PROTO((int32 file_id));

extern bool HDvalidfid
  PROTO((int32 file_id));

extern char _HUGE *HDgettagname
  PROTO((uint16 tag));

extern int32 Hishdf
  PROTO((char _HUGE *filename));

extern intn Hfidinquire
    PROTO((int32 file_id, char _HUGE **fname, intn _HUGE *access,
        intn _HUGE *attach));

/*
** from hkit.c 
*/
extern char _HUGE *HIstrncpy
  PROTO((register char _HUGE *dest, register char _HUGE *source, int32 len));

extern int32 HDspaceleft
  PROTO((void));

extern VOIDP HDgetspace
  PROTO((uint32 qty));

extern VOIDP HDregetspace
  PROTO((VOIDP where, uint32 qty));

extern VOIDP HDfreespace
  PROTO((VOIDP ptr));

#if defined PC & !defined PC386
extern VOIDP fmemcpy_big
  PROTO((VOIDP dest, VOIDP source, uint32 len));

extern VOIDP fmemset_big
  PROTO((VOIDP dest, intn c, uint32 len));

extern intn fmemcmp_big
  PROTO((VOIDP s1, VOIDP s2, uint32 len));

extern VOIDP memcpy_big
  PROTO((VOIDP dest, VOIDP source, uint32 len));

extern VOIDP memset_big
  PROTO((VOIDP dest, intn c, uint32 len));

extern intn memcmp_big
  PROTO((VOIDP s1, VOIDP s2, uint32 len));

#endif  /* WIN3 | PC */

#if defined VMS | (defined PC & !defined PC386)
extern char *HDstrdup
  PROTO((const char *s));

#endif

extern intn HDc2fstr
  PROTO((char _HUGE *str, intn len));

extern char _HUGE *HDf2cstring
  PROTO((_fcd fdesc, intn len));

extern intn HDflush
    PROTO((int32 file_id));

extern intn HDpackFstring
    PROTO((char _HUGE *src, char _HUGE *dest, intn len));

/* 
** from hblocks.c 
*/
extern int32 HLcreate
  PROTO((int32 file_id, uint16 tag, uint16 ref, int32 block_length, 
        int32 number_blocks));

extern int HDinqblockinfo
    PROTO((int32 aid, int32 *length, int32 *first_length, int32 *block_length,
        int32 *number_blocks));

/*
** from hextelt.c 
*/
extern int32 HXcreate
  PROTO((int32 file_id, uint16 tag, uint16 ref, char _HUGE *extern_file_name, int32 f_offset, int32 start_len));

/*
** from herr.c
*/
extern char _HUGE *HEstring
  PROTO((int16 error_code));

extern VOID HEpush
  PROTO((int16 error_code, char _HUGE *function_name, char _HUGE *file_name,
        int line));

#ifndef _H_ERR_MASTER_
extern VOID HEreport
  PROTO((char _HUGE *, ...));
#endif /* _H_ERR_MASTER_ */

extern VOID HEprint
  PROTO((FILE _HUGE *stream, int32 print_level));

extern int16 HEvalue
  PROTO((int32 level));

extern VOID HEclear
  PROTO((void));

/*
** from dfcomp.c
*/
extern int DFputcomp
  PROTO((int32 file_id, uint16 tag, uint16 ref, uint8 _HUGE *image, int32 xdim,
     int32 ydim, uint8 _HUGE *palette, uint8 _HUGE *newpal, int16 scheme,
     comp_info *cinfo));

extern int DFgetcomp
  PROTO((int32 file_id, uint16 tag, uint16 ref, uint8 _HUGE *image, int32 xdim,
	 int32 ydim, uint16 scheme));

/*
** from dfrle.c
*/
extern int32 DFCIrle
  PROTO((VOIDP buf, VOIDP bufto, int32 len));

extern int32 DFCIunrle
  PROTO((uint8 _HUGE *buf, uint8 *bufto, int32 outlen, int resetsave));

/*
** from dfimcomp.c 
*/
extern VOID DFCIimcomp
  PROTO((int32 xdim, int32 ydim, uint8 _HUGE in[], uint8 _HUGE out[],
        uint8 _HUGE in_pal[], uint8 _HUGE out_pal[], int mode));

extern VOID DFCIunimcomp
  PROTO((int32 xdim, int32 ydim, uint8 _HUGE in[], uint8 _HUGE out[]));

/*
** from dfjpeg.c
*/

extern intn DFCIjpeg
    PROTO((int32 file_id,uint16 tag,uint16 ref,int32 xdim, int32 ydim,
            VOIDP image, int16 scheme, comp_info *scheme_info));

/*
** from dfunjpeg.c
*/

extern intn DFCIunjpeg
    PROTO((int32 file_id, uint16 tag, uint16 ref, VOIDP image, int32 xdim,
            int32 ydim, int16 scheme));

/* 
** from dfgroup.c 
*/
extern int32 DFdiread
  PROTO((int32 file_id, uint16 tag, uint16 ref));

extern int DFdiget
  PROTO((int32 list, uint16 _HUGE *ptag, uint16 _HUGE *pref));

extern int32 DFdisetup
  PROTO((int maxsize));

extern int DFdiput
  PROTO((int32 list, uint16 tag, uint16 ref));

extern int DFdiwrite
  PROTO((int32 file_id, int32 list, uint16 tag, uint16 ref));

/*
** from dfp.c 
*/
extern intn DFPgetpal
  PROTO((char _HUGE *filename, VOIDP palette));

extern intn DFPputpal
  PROTO((char _HUGE *filename, VOIDP palette, int overwrite, char _HUGE *filemode));

extern int DFPaddpal
  PROTO((char _HUGE *filename, VOIDP palette));

extern int DFPnpals
  PROTO((char _HUGE *filename));

extern intn DFPreadref
  PROTO((char _HUGE *filename, uint16 ref));

extern int DFPwriteref
  PROTO((char _HUGE *filename, uint16 ref));

extern int DFPrestart
  PROTO((void));

extern uint16 DFPlastref
  PROTO((void));

extern int32 DFPIopen
  PROTO((char _HUGE *filename, int access));

/*
** from dfr8.c 
*/
extern int DFR8setcompress
  PROTO((int32 scheme,comp_info *cinfo));

extern intn DFR8getdims
  PROTO((char _HUGE *filename, int32 _HUGE *pxdim, int32 _HUGE *pydim,
        int _HUGE *pispal));

extern intn DFR8getimage
  PROTO((char _HUGE *filename, uint8 _HUGE *image, int32 xdim, int32 ydim,
        uint8 _HUGE *pal));

extern int DFR8setpalette
  PROTO((uint8 _HUGE *pal));

extern int DFR8putimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim, uint16 compress));

extern int DFR8addimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim, uint16 compress));

extern int DFR8nimages
  PROTO((char _HUGE *filename));

extern intn DFR8readref
  PROTO((char _HUGE *filename, uint16 ref));

extern int DFR8writeref
  PROTO((char _HUGE *filename, uint16 ref));

extern int DFR8restart
  PROTO((void));

extern uint16 DFR8lastref
  PROTO((void));

/*
** from dfgr.c 
*/
extern int DFGRgetlutdims
  PROTO((char _HUGE *filename, int32 _HUGE *pxdim, int32 _HUGE *pydim,
        int _HUGE *pncomps, int _HUGE *pil));

extern int DFGRreqlutil
  PROTO((int il));

extern int DFGRgetlut
  PROTO((char _HUGE *filename, VOIDP lut, int32 xdim, int32 ydim));

extern int DFGRgetimdims
  PROTO((char _HUGE *filename, int32 _HUGE *pxdim, int32 _HUGE *pydim,
        int _HUGE *pncomps, int _HUGE *pil));

extern int DFGRreqimil
  PROTO((int il));

extern int DFGRgetimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim));

extern int DFGRsetcompress
  PROTO((int32 scheme,comp_info *cinfo));

extern int DFGRsetlutdims
  PROTO((int32 xdim, int32 ydim, int ncomps, int il));

extern int DFGRsetlut
  PROTO((VOIDP lut, int32 xdim, int32 ydim));

extern int DFGRaddlut
  PROTO((char _HUGE *filename, VOIDP lut, int32 xdim, int32 ydim));

extern int DFGRsetimdims
  PROTO((int32 xdim, int32 ydim, int ncomps, int il));

extern int DFGRaddimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim));

extern int DFGRputimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim));

extern int DFGRreadref
  PROTO((char _HUGE *filename, uint16 ref));

extern uint16 DFGRIlastref
    PROTO((void));

extern int DFGRIgetdims
  PROTO((char _HUGE *filename, int32 _HUGE *pxdim, int32 _HUGE *pydim,
        int _HUGE *pncomps, int _HUGE *pil, int type));

extern int DFGRIreqil
  PROTO((intn il, intn type));

extern int DFGRIgetimlut
  PROTO((char _HUGE *filename, VOIDP imlut, int32 xdim, int32 ydim, int type,
	 int isfortran));

extern int DFGRIsetdims
  PROTO((int32 xdim, int32 ydim, intn ncomps, int type));

extern int DFGRIsetil
  PROTO((int il, int type));

extern int DFGRIrestart
  PROTO((void));

extern int DFGRIaddimlut
  PROTO((char _HUGE *filename, VOIDP imlut, int32 xdim, int32 ydim, int type,
	 int isfortran, int newfile));

/*
** from df24.c 
*/
extern int DF24getdims
  PROTO((char _HUGE *filename, int32 _HUGE *pxdim, int32 _HUGE *pydim,
        int _HUGE *pil));

extern int DF24reqil
  PROTO((int il));

extern int DF24getimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim));

extern int DF24setdims
  PROTO((int32 xdim, int32 ydim));

extern int DF24setil
  PROTO((int il));

extern int DF24setcompress
  PROTO((int32 type,comp_info *cinfo));

extern int DF24restart
  PROTO((void));

extern int DF24addimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim));

extern int DF24putimage
  PROTO((char _HUGE *filename, VOIDP image, int32 xdim, int32 ydim));

extern int DF24nimages
  PROTO((char _HUGE *filename));

extern int DF24readref
  PROTO((char _HUGE *filename, uint16 ref));

extern uint16 DF24lastref
    PROTO((void));

/*
** from dfan.c
*/

extern int32 DFANgetlablen
    PROTO((char _HUGE *filename, uint16 tag, uint16 ref));

extern int DFANgetlabel
    PROTO((char _HUGE *filename, uint16 tag, uint16 ref, char _HUGE *label,
            int32 maxlen));

extern int32 DFANgetdesclen
    PROTO((char _HUGE *filename, uint16 tag, uint16 ref));

extern int DFANgetdesc
    PROTO((char _HUGE *filename, uint16 tag, uint16 ref, char _HUGE *desc,
            int32 maxlen));

extern int32 DFANgetfidlen
    PROTO((int32 file_id, int isfirst));

extern int32 DFANgetfid
    PROTO((int32 file_id, char _HUGE *id, int32 maxlen, int isfirst));

extern int32 DFANgetfdslen
    PROTO((int32 file_id, int isfirst));

extern int32 DFANgetfds
    PROTO((int32 file_id, char _HUGE *desc, int32 maxlen, int isfirst));

extern int DFANputlabel
    PROTO((char _HUGE *filename, uint16 tag, uint16 ref, char _HUGE *label));

extern int DFANputdesc
    PROTO((char _HUGE *filename, uint16 tag, uint16 ref, char _HUGE *desc,
            int32 desclen));

extern int DFANaddfid
    PROTO((int32 file_id, char _HUGE *id));

extern int DFANaddfds
    PROTO((int32 file_id, char _HUGE *desc, int32 desclen));

extern uint16 DFANlastref
    PROTO((void));

extern int DFANlablist
    PROTO((char _HUGE *filename, uint16 tag, uint16 _HUGE reflist[],
            char _HUGE *labellist, int listsize, int maxlen, int startpos));

extern uint16 DFANIlocate
  PROTO((int32 file_id, int type, uint16 tag, uint16 ref));

extern int DFANIaddentry
  PROTO((int type, uint16 annref, uint16 datatag, uint16 dataref));

extern int32 DFANIgetannlen
  PROTO((char _HUGE *filename, uint16 tag, uint16 ref, int type));

extern intn DFANIgetann
  PROTO((char _HUGE *filename, uint16 tag, uint16 ref, uint8 _HUGE *ann,
            int32 maxlen, int type));

extern intn DFANIputann
  PROTO((char _HUGE *filename, uint16 tag, uint16 ref, uint8 _HUGE *ann,
            int32 annlen, int type));

extern int DFANIlablist
  PROTO((char _HUGE *filename, uint16 tag, uint16 _HUGE reflist[],
            uint8 _HUGE *labellist, int listsize, int maxlen, int startpos,
            int isfortran));

extern int DFANIaddfann
  PROTO((int32 file_id, char _HUGE *ann, int32 annlen, int type));

extern int32 DFANIgetfannlen
  PROTO((int32 file_id, int type, int isfirst));

extern int32 DFANIgetfann
  PROTO((int32 file_id, char _HUGE *ann, int32 maxlen, int type, int isfirst));

/*
** from dfsd.c
*/

extern int DFSDgetdims
    PROTO((char _HUGE *filename, intn _HUGE *prank, int32 _HUGE sizes[], intn maxrank));

extern int DFSDgetdatastrs
    PROTO((char _HUGE *label, char _HUGE *unit, char _HUGE *format,char _HUGE *coordsys));

extern int DFSDgetdimstrs
    PROTO((int dim, char _HUGE *label, char _HUGE *unit, char _HUGE *format));

extern int DFSDgetdatalen
    PROTO((int _HUGE *llabel, int _HUGE *lunit, int _HUGE *lformat,int _HUGE *lcoordsys));

extern int DFSDgetdimlen
    PROTO((int dim, int _HUGE *llabel, int _HUGE *lunit, int _HUGE *lformat));

extern int DFSDgetdimscale
    PROTO((intn dim, int32 maxsize, VOIDP scale));

extern int DFSDgetrange
    PROTO((VOIDP pmax, VOIDP pmin));

extern int DFSDgetdata
    PROTO((char _HUGE *filename, intn rank, int32 _HUGE maxsizes[], VOIDP data));

extern int DFSDsetlengths
    PROTO((int maxlen_label, int maxlen_unit, int maxlen_format,
                int maxlen_coordsys));

extern int DFSDsetdims
    PROTO((intn rank, int32 _HUGE dimsizes[]));

extern int DFSDsetdatastrs
    PROTO((char _HUGE *label, char _HUGE *unit, char _HUGE *format,char _HUGE *coordsys));

extern int DFSDsetdimstrs
    PROTO((int dim, char _HUGE *label, char _HUGE *unit, char _HUGE *format));

extern int DFSDsetdimscale
    PROTO((intn dim, int32 dimsize, VOIDP scale));

extern int DFSDsetrange
    PROTO((VOIDP maxi, VOIDP mini));

extern int DFSDputdata
    PROTO((char _HUGE *filename, intn rank, int32 _HUGE dimsizes[], VOIDP data));

extern int DFSDadddata
    PROTO((char _HUGE *filename, intn rank, int32 _HUGE dimsizes[], VOIDP data));

extern int DFSDrestart
    PROTO((void));

extern int32 DFSDndatasets
    PROTO((char _HUGE *filename));

extern int DFSDclear
    PROTO((void));

extern uint16 DFSDlastref
    PROTO((void));

extern int DFSDreadref
    PROTO((char _HUGE *filename, uint16 ref));

extern int DFSDgetslice
    PROTO((char _HUGE *filename, int32 _HUGE winst[], int32 _HUGE windims[],VOIDP data,
                int32 _HUGE dims[]));

extern int DFSDstartslice
    PROTO((char _HUGE *filename));

extern int DFSDputslice
    PROTO((int32 _HUGE winend[], VOIDP data, int32 _HUGE dims[]));

extern int DFSDendslice
    PROTO((void));

extern int DFSDsetNT
    PROTO((int32 numbertype));

extern int DFSDsetorder
    PROTO((int arrayorder));

extern int DFSDgetNT
    PROTO((int32 _HUGE *pnumbertype));

extern intn DFSDpre32sdg
    PROTO((char _HUGE *filename,uint16 ref, intn _HUGE *ispre32));

extern int DFSDsetcal
    PROTO((float64 cal, float64 cal_err, float64 ioff, 
           float64 ioff_err, int32 cal_nt));

extern int DFSDgetcal
    PROTO((float64 _HUGE *pcal, float64 _HUGE *pcal_err, float64 _HUGE *pioff,
            float64 _HUGE *pioff_err, int32 _HUGE *cal_nt));

extern int DFSDwriteref
    PROTO((char _HUGE *filename, uint16 ref));

extern int DFSDsetfillvalue
    PROTO((VOIDP fill_value));

extern int DFSDgetfillvalue
    PROTO((VOIDP fill_value));

extern int DFSDstartslab
    PROTO((char _HUGE *filename));

extern int DFSDwriteslab
    PROTO((int32 _HUGE start[], int32 _HUGE stride[], int32 _HUGE count[],
        VOIDP data));

extern int DFSDendslab
    PROTO((void));

/*
** from dfconv.c
*/

extern int DFKNTsize
    PROTO((int32 number_type));

extern int32 DFKisnativeNT
    PROTO((int32 numbertype));

extern int32 DFKislitendNT
    PROTO((int32 numbertype));

extern int8 DFKgetPNSC
    PROTO((int32 numbertype, int32 machinetype));

extern intn DFKsetNT
    PROTO((int32 ntype));

extern int32 DFKconvert
    PROTO((VOIDP source, VOIDP dest, int32 ntype, int32 num_elm,
            int16 access, int32 source_stride, int32 dest_stride));

/*
** from dfknat.c
*/

extern intn DFKnb1b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKnb2b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKnb4b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKnb8b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

/*
** from dfkswap.c
*/

extern intn DFKsb2b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKsb4b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKsb8b
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

/*
** from dfkcray.c
*/

extern intn DFKui2i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKui2s
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKuo2i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKui4i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKui4s
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKuo4i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKui4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKuo4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKui8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKuo8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlui2i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlui2s
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKluo2i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlui4i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlui4s
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKluo4i
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlui4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKluo4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlui8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKluo8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

/*
** from dfkvms.c
*/

extern intn DFKvi4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKvo4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKvi8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKvo8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlvi4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlvo4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlvi8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlvo8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

/*
** from dfkconv.c
*/

extern intn DFKci4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKco4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKci8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKco8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlci4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlco4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlci8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlco8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

/*
** from dfkfuji.c
*/

extern intn DFKpi4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKpo4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKpi8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKpo8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlpi4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlpo4f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlpi8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

extern intn DFKlpo8f
    PROTO((VOIDP s,VOIDP d,uint32 num_elm,uint32 source_stride,uint32 dest_stride));

/*
** from dfanF.c
*/
#ifndef DFAN_FNAMES
#   define  DFAN_FNAMES
#ifdef DF_CAPFNAMES
#   define ndaiganl  FNAME(DAIGANL)
#   define ndaigann  FNAME(DAIGANN)
#   define ndaipann  FNAME(DAIPANN)
#   define ndailist  FNAME(DAILIST)
#   define ndalref   FNAME(DALREF)
#   define ndfanlastref     FNAME(DFANLASTREF)

#   define ndfanaddfds      FNAME(DFANADDFDS)
#   define ndfangetfidlen   FNAME(DFANGETFIDLEN)
#   define ndfangetfdslen   FNAME(DFANGETFDSLEN)
#   define ndfangetfid      FNAME(DFANGETFID)
#   define ndfangetfds      FNAME(DFANGETFDS)
#   define ndaafds          FNAME(DAAFDS)
#   define ndagfidl         FNAME(DAGFIDL)
#   define ndagfdsl         FNAME(DAGFDSL)
#   define ndagfid          FNAME(DAGFID)
#   define ndagfds          FNAME(DAGFDS)
#   define ndaiafid         FNAME(DAIAFID)
#else   /* DF_CAPFNAMES */
#   define ndaiganl  FNAME(daiganl)
#   define ndaigann  FNAME(daigann)
#   define ndaipann  FNAME(daipann)
#   define ndailist  FNAME(dailist)
#   define ndalref   FNAME(dalref)
#   define ndfanlastref     FNAME(dfanlastref)

#   define ndfanaddfds      FNAME(dfanaddfds)
#   define ndfangetfidlen   FNAME(dfangetfidlen)
#   define ndfangetfdslen   FNAME(dfangetfdslen)
#   define ndfangetfid      FNAME(dfangetfid)
#   define ndfangetfds      FNAME(dfangetfds)
#   define ndaafds          FNAME(daafds)
#   define ndagfidl         FNAME(dagfidl)
#   define ndagfdsl         FNAME(dagfdsl)
#   define ndagfid          FNAME(dagfid)
#   define ndagfds          FNAME(dagfds)
#   define ndaiafid         FNAME(daiafid)
#endif /* DF_CAPFNAMES */
#endif /* DFAN_FNAMES */

extern FRETVAL(intf) ndaiganl
    PROTO((_fcd filename, intf _HUGE *tag, intf _HUGE *ref, intf _HUGE *type,
            intf _HUGE *fnlen));

extern FRETVAL(intf) ndaigann
    PROTO((_fcd filename, intf _HUGE *tag, intf _HUGE *ref, _fcd annotation,
        intf _HUGE *maxlen, intf _HUGE *type, intf _HUGE *fnlen));

extern FRETVAL(intf) ndaipann
    PROTO((_fcd filename, intf _HUGE *tag, intf _HUGE *ref, _fcd annotation,
            intf _HUGE *annlen, intf _HUGE *type, intf _HUGE *fnlen));

extern FRETVAL(intf) ndailist
    PROTO((_fcd filename, intf _HUGE *tag, intf _HUGE reflist[], _fcd labellist,
        intf _HUGE *listsize, intf _HUGE *maxlen, intf _HUGE *startpos,
        intf _HUGE *fnlen));

extern FRETVAL(intf) ndalref
    PROTO((void));

extern FRETVAL(intf) ndfanlastref
    PROTO((void));

extern FRETVAL(intf) ndfanaddfds
    PROTO((intf _HUGE *dfile, _fcd desc, intf _HUGE *desclen));

extern FRETVAL(intf) ndfangetfidlen
    PROTO((intf _HUGE *dfile, intf _HUGE *isfirst));

extern FRETVAL(intf) ndfangetfdslen
    PROTO((intf _HUGE *dfile, intf _HUGE *isfirst));

extern FRETVAL(intf) ndfangetfid
    PROTO((intf _HUGE *dfile, _fcd id, intf _HUGE *maxlen, intf _HUGE *isfirst));

extern FRETVAL(intf) ndfangetfds
    PROTO((intf _HUGE *dfile, _fcd id, intf _HUGE *maxlen, intf _HUGE *isfirst));

extern FRETVAL(intf) ndaafds
    PROTO((intf _HUGE *dfile, _fcd desc, intf _HUGE *desclen));

extern FRETVAL(intf) ndagfidl
    PROTO((intf _HUGE *dfile, intf _HUGE *isfirst));

extern FRETVAL(intf) ndagfdsl
    PROTO((intf _HUGE *dfile, intf _HUGE *isfirst));

extern FRETVAL(intf) ndagfid
  PROTO((intf _HUGE *dfile, _fcd id, intf _HUGE *maxlen, intf _HUGE *isfirst));

extern FRETVAL(intf) ndagfds
  PROTO((intf _HUGE *dfile, _fcd id, intf _HUGE *maxlen, intf _HUGE *isfirst));

extern FRETVAL(intf) ndaiafid
  PROTO((intf _HUGE *dfile, _fcd id, intf _HUGE *idlen));

/*
** from dfr8F.c
*/
#ifndef DFR8_FNAMES
#   define DFR8_FNAMES
#ifdef DF_CAPFNAMES
#   define nd8spal   FNAME(D8SPAL)
#   define nd8first  FNAME(D8FIRST)
#   define nd8igdim  FNAME(D8IGDIM)
#   define nd8igimg  FNAME(D8IGIMG)
#   define nd8ipimg  FNAME(D8IPIMG)
#   define nd8iaimg  FNAME(D8IAIMG)
#   define nd8irref  FNAME(D8IRREF)
#   define nd8iwref  FNAME(D8IWREF)
#   define nd8inims  FNAME(D8INIMS)
#   define nd8lref   FNAME(D8LREF)
#   define ndfr8lastref      FNAME(DFR8LASTREF)
#   define ndfr8setpalette   FNAME(DFR8SETPALETTE)
#   define ndfr8restart  FNAME(DFR8RESTART)
#   define nd8scomp  FNAME(D8SCOMP)
#   define ndfr8scompress FNAME(DFR8SCOMPRESS)
#   define nd8sjpeg  FNAME(D8SJPEG)
#   define ndfr8sjpeg FNAME(DFR8SJPEG)
#else   /* !DF_CAPFNAMES */
#   define nd8spal   FNAME(d8spal)
#   define nd8first  FNAME(d8first)
#   define nd8igdim  FNAME(d8igdim)
#   define nd8igimg  FNAME(d8igimg)
#   define nd8ipimg  FNAME(d8ipimg)
#   define nd8iaimg  FNAME(d8iaimg)
#   define nd8irref  FNAME(d8irref)
#   define nd8iwref  FNAME(d8iwref)
#   define nd8inims  FNAME(d8inims)
#   define nd8lref   FNAME(d8lref)
#   define ndfr8lastref      FNAME(dfr8lastref)
#   define ndfr8setpalette   FNAME(dfr8setpalette)
#   define ndfr8restart  FNAME(dfr8restart)
#   define nd8scomp  FNAME(d8scomp)
#   define ndfr8scompress FNAME(dfr8scompress)
#   define nd8sjpeg  FNAME(d8sjpeg)
#   define ndfr8sjpeg FNAME(dfr8sjpeg)
#endif /* DF_CAPFNAMES */
#endif /* DFR8_FNAMES */

extern  FRETVAL(intf) nd8spal
    PROTO((_fcd pal));

extern  FRETVAL(intf) nd8first
    PROTO((void));

extern  FRETVAL(intf) nd8igdim
    PROTO((_fcd filename, intf _HUGE *xdim, intf _HUGE *ydim, intf _HUGE *ispal,
            intf _HUGE *lenfn));

extern  FRETVAL(intf) nd8igimg
    PROTO((_fcd filename, _fcd image, intf _HUGE *xdim, intf _HUGE *ydim,
            _fcd pal, intf _HUGE *lenfn));

extern  FRETVAL(intf) nd8ipimg
    PROTO((_fcd filename, _fcd image, intf _HUGE *xdim, intf _HUGE *ydim,
            intf _HUGE *compress, intf _HUGE *lenfn));

extern  FRETVAL(intf) nd8iaimg
    PROTO((_fcd filename, _fcd image, intf _HUGE *xdim, intf _HUGE *ydim,
            intf _HUGE *compress, intf _HUGE *lenfn));

extern  FRETVAL(intf) nd8irref
    PROTO((_fcd filename, intf _HUGE *ref, intf _HUGE *fnlen));

extern FRETVAL(intf) nd8iwref
    PROTO((_fcd filename, intf _HUGE *ref, intf _HUGE *fnlen));

extern FRETVAL(intf) nd8inims
    PROTO((_fcd filename, intf _HUGE *fnlen));

extern FRETVAL(intf) nd8lref
    PROTO((void));

extern FRETVAL(intf) ndfr8lastref
    PROTO((void));

extern  FRETVAL(intf) ndfr8setpalette
    PROTO((_fcd pal));

extern  FRETVAL(intf) ndfr8restart
    PROTO((void));

extern FRETVAL(intf) nd8scomp
    PROTO((intf *scheme));

extern FRETVAL(intf) ndfr8scompress
    PROTO((intf *scheme));

extern FRETVAL(intf) nd8sjpeg
    PROTO((intf *quality,intf *force_baseline));

extern FRETVAL(intf) ndfr8sjpeg
    PROTO((intf *quality,intf *force_baseline));

/*
** from dfsdF.c
*/
#ifndef DFSD_FNAMES
#   define DFSD_FNAMES
#ifdef DF_CAPFNAMES
#   define ndsgdast  FNAME(DSGDAST)
#   define ndsgdisc  FNAME(DSGDISC)
#   define ndsgrang  FNAME(DSGRANG)
#   define ndssdims  FNAME(DSSDIMS)
#   define ndssdisc  FNAME(DSSDISC)
#   define ndssrang  FNAME(DSSRANG)
#   define ndsclear  FNAME(DSCLEAR)
#   define ndsslens  FNAME(DSSLENS)
#   define ndsgdiln  FNAME(DSGDILN)
#   define ndsgdaln  FNAME(DSGDALN)
#   define ndsfirst  FNAME(DSFIRST)
#   define ndspslc   FNAME(DSPSLC)
#   define ndseslc   FNAME(DSESLC)
#   define ndsgnt    FNAME(DSGNT)
#   define ndssnt    FNAME(DSSNT)
#   define ndsigdim  FNAME(DSIGDIM)
#   define ndsigdat  FNAME(DSIGDAT)
#   define ndsipdat  FNAME(DSIPDAT)
#   define ndsiadat  FNAME(DSIADAT)
#   define ndsigdas  FNAME(DSIGDAS)
#   define ndsigslc  FNAME(DSIGSLC)
#   define ndsigdis  FNAME(DSIGDIS)
#   define ndsisslc  FNAME(DSISSLC)
#   define ndsisdas  FNAME(DSISDAS)
#   define ndsisdis  FNAME(DSISDIS)
#   define ndsirref  FNAME(DSIRREF)
#   define ndslref   FNAME(DSLREF)
#   define ndsinum   FNAME(DSINUM)
#   define ndsip32s  FNAME(DSIP32S)
#   define ndsscal   FNAME(DSSCAL)
#   define ndsgcal   FNAME(DSGCAL)
#   define ndfsdgetdatastrs  FNAME(DFSDGETDATASTRS)
#   define ndfsdgetdimscale  FNAME(DFSDGETDIMSCALE)
#   define ndfsdgetrange     FNAME(DFSDGETRANGE)
#   define ndfsdsetdims      FNAME(DFSDSETDIMS)
#   define ndfsdsetdimscale  FNAME(DFSDSETDIMSCALE)
#   define ndfsdsetrange     FNAME(DFSDSETRANGE)
#   define ndfsdclear        FNAME(DFSDCLEAR)
#   define ndfsdsetlengths   FNAME(DFSDSETLENGTHS)
#   define ndfsdgetdimlen    FNAME(DFSDGETDIMLEN)
#   define ndfsdgetdatalen   FNAME(DFSDGETDATALEN)
#   define ndfsdrestart      FNAME(DFSDRESTART)
#   define ndfsdputslice     FNAME(DFSDPUTSLICE)
#   define ndfsdendslice     FNAME(DFSDENDSLICE)
#   define ndfsdsetnt        FNAME(DFSDSETNT)
#   define ndfsdgetnt        FNAME(DFSDGETNT)
#   define ndfsdlastref      FNAME(DFSDLASTREF)
#   define ndsiwref          FNAME(DSIWREF)
#   define ndssfill          FNAME(DSSFILL)
#   define ndsgfill          FNAME(DSGFILL)
#   define ndsisslab         FNAME(DSISSLAB)
#   define ndswslab          FNAME(DSWSLAB)
#   define ndseslab          FNAME(DSESLAB)
#else
#   define ndsgdast  FNAME(dsgdast)
#   define ndsgdisc  FNAME(dsgdisc)
#   define ndsgrang  FNAME(dsgrang)
#   define ndssdims  FNAME(dssdims)
#   define ndssdisc  FNAME(dssdisc)
#   define ndssrang  FNAME(dssrang)
#   define ndsclear  FNAME(dsclear)
#   define ndsslens  FNAME(dsslens)
#   define ndsgdiln  FNAME(dsgdiln)
#   define ndsgdaln  FNAME(dsgdaln)
#   define ndsfirst  FNAME(dsfirst)
#   define ndspslc   FNAME(dspslc)
#   define ndseslc   FNAME(dseslc)
#   define ndsgnt    FNAME(dsgnt)
#   define ndssnt    FNAME(dssnt)
#   define ndsigdim  FNAME(dsigdim)
#   define ndsigdat  FNAME(dsigdat)
#   define ndsipdat  FNAME(dsipdat)
#   define ndsiadat  FNAME(dsiadat)
#   define ndsigdas  FNAME(dsigdas)
#   define ndsigslc  FNAME(dsigslc)
#   define ndsigdis  FNAME(dsigdis)
#   define ndsisslc  FNAME(dsisslc)
#   define ndsisdas  FNAME(dsisdas)
#   define ndsisdis  FNAME(dsisdis)
#   define ndsirref  FNAME(dsirref)
#   define ndslref   FNAME(dslref)
#   define ndsinum   FNAME(dsinum)
#   define ndsip32s  FNAME(dsip32s)
#   define ndsscal   FNAME(dsscal)
#   define ndsgcal   FNAME(dsgcal)
#   define ndfsdgetdatastrs  FNAME(dfsdgetdatastrs)
#   define ndfsdgetdimscale  FNAME(dfsdgetdimscale)
#   define ndfsdgetrange     FNAME(dfsdgetrange)
#   define ndfsdsetdims      FNAME(dfsdsetdims)
#   define ndfsdsetdimscale  FNAME(dfsdsetdimscale)
#   define ndfsdsetrange     FNAME(dfsdsetrange)
#   define ndfsdclear        FNAME(dfsdclear)
#   define ndfsdsetlengths   FNAME(dfsdsetlengths)
#   define ndfsdgetdimlen    FNAME(dfsdgetdimlen)
#   define ndfsdgetdatalen   FNAME(dfsdgetdatalen)
#   define ndfsdrestart      FNAME(dfsdrestart)
#   define ndfsdputslice     FNAME(dfsdputslice)
#   define ndfsdendslice     FNAME(dfsdendslice)
#   define ndfsdsetnt        FNAME(dfsdsetnt)
#   define ndfsdgetnt        FNAME(dfsdgetnt)
#   define ndfsdlastref      FNAME(dfsdlastref)
#   define ndsiwref          FNAME(dsiwref)
#   define ndssfill          FNAME(dssfill)
#   define ndsgfill          FNAME(dsgfill)
#   define ndsisslab         FNAME(dsisslab)
#   define ndswslab          FNAME(dswslab)
#   define ndseslab          FNAME(dseslab)
#endif /* DF_CAPFNAMES */
#endif  /* DFSD_FNAMES */

extern FRETVAL(intf) ndsgdisc
    PROTO((intf _HUGE *dim, intf _HUGE *maxsize, VOIDP scale));

extern FRETVAL(intf) ndsgrang
    PROTO((VOIDP pmax, VOIDP pmin));

extern FRETVAL(intf) ndssdims
    PROTO((intf _HUGE *rank, intf _HUGE dimsizes[]));

extern FRETVAL(intf) ndssdisc
    PROTO((intf _HUGE *dim, intf _HUGE *dimsize, VOIDP scale));

extern FRETVAL(intf) ndssrang
    PROTO((VOIDP max, VOIDP min));

extern FRETVAL(intf) ndsclear
    PROTO((void));

extern FRETVAL(intf) ndsslens
    PROTO((intf _HUGE *maxlen_label, intf _HUGE *maxlen_unit, 
            intf _HUGE *maxlen_format, intf _HUGE *maxlen_coordsys));

extern FRETVAL(intf) ndsgdiln
    PROTO((intf _HUGE *dim, intf _HUGE *llabel, intf _HUGE *lunit,
            intf _HUGE *lformat));

extern FRETVAL(intf) ndsgdaln
    PROTO((intf _HUGE *llabel, intf _HUGE *lunit, intf _HUGE *lformat,
            intf _HUGE *lcoordsys));

extern FRETVAL(intf) ndsfirst
    PROTO((void));

extern FRETVAL(intf) ndspslc
    PROTO((intf _HUGE windims[], VOIDP data, intf _HUGE dims[]));

extern FRETVAL(intf) ndseslc
    PROTO((void));

extern FRETVAL(intf) ndssnt
    PROTO((intf _HUGE *numbertype));

extern FRETVAL(intf) ndsgnt
    PROTO((intf _HUGE *pnumbertype));

extern FRETVAL(intf) ndsigdim
    PROTO((_fcd filename, intf _HUGE *prank, intf _HUGE sizes[],
            intf _HUGE *maxrank, intf _HUGE *lenfn));

extern FRETVAL(intf) ndsigdat
    PROTO((_fcd filename, intf _HUGE *rank, intf _HUGE maxsizes[],
            VOIDP data, intf _HUGE *fnlen));

extern FRETVAL(intf) ndsipdat
    PROTO((_fcd filename, intf _HUGE *rank, intf _HUGE dimsizes[],
            VOIDP data, intf _HUGE *fnlen));

extern FRETVAL(intf) ndsiadat
    PROTO((_fcd filename, intf _HUGE *rank, intf _HUGE dimsizes[],
            VOIDP data, intf _HUGE *fnlen));

extern FRETVAL(intf) ndsigslc
    PROTO((_fcd filename, intf _HUGE winst[], intf _HUGE windims[],
            VOIDP data, intf _HUGE dims[], intf _HUGE *fnlen));

extern FRETVAL(intf) ndsisslc
    PROTO((_fcd filename, intf _HUGE *fnlen));

extern FRETVAL(intf) ndsirref
    PROTO((_fcd filename, intf _HUGE *ref, intf _HUGE *fnlen));

extern FRETVAL(intf) ndslref
    PROTO((void));

extern FRETVAL(intf) ndsinum
    PROTO((_fcd filename, intf _HUGE *len));

extern FRETVAL(intf) ndsip32s
    PROTO((_fcd filename, intf _HUGE *ref, intf _HUGE *ispre32, intf _HUGE *len));

extern FRETVAL(intf) ndfsdgetdatastrs
    PROTO((_fcd label, _fcd unit, _fcd format, _fcd coordsys));

extern FRETVAL(intf) ndfsdgetdimstrs
    PROTO((intf _HUGE *dim, _fcd label, _fcd unit, _fcd format));

extern FRETVAL(intf) ndfsdgetdimscale
    PROTO((intf _HUGE *dim, intf _HUGE *maxsize, VOIDP scale));

extern FRETVAL(intf) ndfsdgetrange
    PROTO((VOIDP pmax, VOIDP pmin));

extern FRETVAL(intf) ndfsdsetdims
    PROTO((intf _HUGE *rank, intf _HUGE dimsizes[]));

extern FRETVAL(intf) ndfsdsetdimscale
    PROTO((intf _HUGE *dim, intf _HUGE *dimsize, VOIDP scale));

extern FRETVAL(intf) ndfsdsetrange
    PROTO((VOIDP max, VOIDP min));

extern FRETVAL(intf) ndfsdclear
    PROTO((void));

extern FRETVAL(intf) ndfsdsetlengths
    PROTO((intf _HUGE *maxlen_label, intf _HUGE *maxlen_unit,
            intf _HUGE *maxlen_format, intf _HUGE *maxlen_coordsys));

extern FRETVAL(intf) ndfsdgetdimlen
    PROTO((intf _HUGE *dim, intf _HUGE *llabel, intf _HUGE *lunit,
            intf _HUGE *lformat));

extern FRETVAL(intf) ndfsdgetdatalen
    PROTO((intf _HUGE *llabel, intf _HUGE *lunit, intf _HUGE *lformat,
            intf _HUGE *lcoordsys));

extern FRETVAL(intf) ndfsdrestart
    PROTO((void));

extern FRETVAL(intf) ndfsdputslice
    PROTO((intf _HUGE windims[], VOIDP data, intf _HUGE dims[]));

extern FRETVAL(intf) ndfsdendslice
    PROTO((void));

extern FRETVAL(intf) ndfsdsetnt
    PROTO((intf _HUGE *numbertype));

extern FRETVAL(intf) ndfsdgetnt
    PROTO((intf _HUGE *pnumbertype));

extern FRETVAL(intf) ndfsdlastref
    PROTO((void));

extern FRETVAL(intf) ndsisdis
    PROTO((intf _HUGE *dim, _fcd flabel, _fcd funit, _fcd fformat,
            intf _HUGE *llabel, intf _HUGE *lunit, intf _HUGE *lformat));

extern FRETVAL(intf) ndsigdis
    PROTO((intf _HUGE *dim, _fcd label, _fcd unit, _fcd format,
            intf _HUGE *llabel, intf _HUGE *lunit, intf _HUGE *lformat));

extern FRETVAL(intf) ndsisdas
    PROTO((_fcd flabel, _fcd funit, _fcd fformat, _fcd fcoordsys,
        intf _HUGE *isfortran, intf _HUGE *llabel, intf _HUGE *lunit,
        intf _HUGE *lformat, intf _HUGE *lcoordsys));

extern FRETVAL(intf) ndsigdas
    PROTO((_fcd label, _fcd unit, _fcd format, _fcd coordsys, intf _HUGE *llabel,
        intf _HUGE *lunit, intf _HUGE *lformat, intf _HUGE *lcoord));

extern FRETVAL(intf) ndsscal
    PROTO((float64 _HUGE *cal, float64 _HUGE *cal_err, float64 _HUGE *ioff,
        float64 _HUGE *ioff_err, intf _HUGE *cal_type));

extern FRETVAL(intf) ndsgcal
    PROTO((float64 _HUGE *cal, float64 _HUGE *cal_err, float64 _HUGE *ioff,
        float64 _HUGE *ioff_err, intf _HUGE *cal_type));

extern FRETVAL(intf) ndswref
     PROTO((_fcd filename, intf _HUGE *fnlen, intf _HUGE *ref));

extern FRETVAL(intf) ndssfill
     PROTO((VOIDP fill_value));

extern FRETVAL(intf) ndsgfill
     PROTO((VOIDP fill_value));

extern FRETVAL(intf) ndssslab
     PROTO((_fcd filename, intf _HUGE *fnlen));

extern FRETVAL(intf) ndswslab
     PROTO((intf _HUGE start[], intf _HUGE stride[],
            intf _HUGE cont[], VOIDP data));

extern FRETVAL(intf) ndseslab
     PROTO((void));

/*
** from dfpF.c
*/

#ifndef DFP_FNAMES
#   define DFP_FNAMES
#ifdef DF_CAPFNAMES
#   define ndpigpal  FNAME(DPIGPAL)
#   define ndpippal  FNAME(DPIPPAL)
#   define ndpinpal  FNAME(DPINPAL)
#   define ndpiwref  FNAME(DPIWREF)
#   define ndpirref  FNAME(DPIRREF)
#   define ndprest   FNAME(DPREST)
#   define ndplref   FNAME(DPLREF)
#   define ndfprestart   FNAME(DFPRESTART)
#   define ndfplastref   FNAME(DFPLASTREF)
#else   /* !DF_CAPNAMES */
#   define ndpigpal  FNAME(dpigpal)
#   define ndpippal  FNAME(dpippal)
#   define ndpinpal  FNAME(dpinpal)
#   define ndpiwref  FNAME(dpiwref)
#   define ndpirref  FNAME(dpirref)
#   define ndprest   FNAME(dprest)
#   define ndplref   FNAME(dplref)
#   define ndfprestart   FNAME(dfprestart)
#   define ndfplastref   FNAME(dfplastref)
#endif /* DF_CAPFNAMES */
#endif /* DFP_FNAMES */

extern  FRETVAL(intf) ndpigpal
    PROTO((_fcd filename, _fcd pal, intf _HUGE *fnlen));

extern FRETVAL(intf) ndpippal
    PROTO((_fcd filename, _fcd pal, intf _HUGE *overwrite, _fcd filemode,
            intf _HUGE *fnlen));

extern FRETVAL(intf) ndpinpal
    PROTO((_fcd filename, intf _HUGE *fnlen));

extern FRETVAL(intf) ndpirref
    PROTO((_fcd filename, uint16 _HUGE *ref, intf _HUGE *fnlen));

extern FRETVAL(intf) ndpiwref
    PROTO((_fcd filename, uint16 _HUGE *ref, intf _HUGE *fnlen));

extern FRETVAL(intf) ndprest
    PROTO((void));

extern FRETVAL(intf) ndplref
    PROTO((void));

extern FRETVAL(intf) ndfprestart
    PROTO((void));

extern FRETVAL(intf) ndfplastref
    PROTO((void));

/*
** from df24F.c
*/
#ifndef DF24_FNAMES
#   define DF24_FNAMES
#ifdef DF_CAPFNAMES
#   define nd2reqil  FNAME(D2REQIL)
#   define ndf24reqil    FNAME(DF24REQIL)
#   define nd2sdims  FNAME(D2SDIMS)
#   define ndf24setdims  FNAME(DF24SETDIMS)
#   define nd2setil  FNAME(D2SETIL)
#   define ndf24setil    FNAME(DF24SETIL)
#   define nd2first  FNAME(D2FIRST)
#   define ndf24restart  FNAME(DF24RESTART)
#   define nd2igdim  FNAME(D2IGDIM)
#   define nd2igimg  FNAME(D2IGIMG)
#   define nd2iaimg  FNAME(D2IAIMG)
#   define nd2irref  FNAME(D2IRREF)
#   define nd24lref  FNAME(D24LREF)
#   define nd2scomp  FNAME(D2SCOMP)
#   define ndf24scompress FNAME(DF24SCOMPRESS)
#   define nd2sjpeg  FNAME(D2SJPEG)
#   define ndf24sjpeg FNAME(DF24SJPEG)
#else
#   define nd2reqil  FNAME(d2reqil)
#   define ndf24reqil    FNAME(df24reqil)
#   define nd2sdims  FNAME(d2sdims)
#   define ndf24setdims  FNAME(df24setdims)
#   define nd2setil  FNAME(d2setil)
#   define ndf24setil    FNAME(df24setil)
#   define nd2first  FNAME(d2first)
#   define ndf24restart  FNAME(df24restart)
#   define nd2igdim  FNAME(d2igdim)
#   define nd2igimg  FNAME(d2igimg)
#   define nd2iaimg  FNAME(d2iaimg)
#   define nd2irref  FNAME(d2irref)
#   define nd24lref  FNAME(d24lref)
#   define nd2scomp  FNAME(d2scomp)
#   define ndf24scompress FNAME(df24scompress)
#   define nd2sjpeg  FNAME(d2sjpeg)
#   define ndf24sjpeg FNAME(df24sjpeg)
#endif /* DF_CAPFNAMES */
#endif /* DF24_FNAMES */

extern FRETVAL(intf) nd2reqil
    PROTO((intf _HUGE *il));

extern FRETVAL(intf) nd2sdims
    PROTO((intf _HUGE *xdim, intf _HUGE *ydim));

extern FRETVAL(intf) nd2igdim
    PROTO((_fcd filename, intf _HUGE *pxdim, intf _HUGE *pydim, intf _HUGE *pil,
            intf _HUGE *fnlen));

extern FRETVAL(intf) nd2igimg
    PROTO((_fcd filename, _fcd image, intf _HUGE *xdim, intf _HUGE *ydim,
            intf _HUGE *fnlen));

extern FRETVAL(intf) nd2iaimg
    PROTO((_fcd filename, _fcd image, intf _HUGE *xdim, intf _HUGE *ydim,
            intf _HUGE *fnlen, intf _HUGE *newfile));

extern FRETVAL(intf) nd2setil
    PROTO((intf _HUGE *il));

extern FRETVAL(intf) nd2first
    PROTO((void));

extern FRETVAL(intf) ndf24reqil
    PROTO((intf _HUGE *il));

extern FRETVAL(intf) ndf24setdims
    PROTO((intf _HUGE *xdim, intf _HUGE *ydim));

extern FRETVAL(intf) ndf24setil
    PROTO((intf _HUGE *il));

extern FRETVAL(intf) ndf24restart
    PROTO((void));

extern FRETVAL(intf) nd2irref
    PROTO((_fcd filename, intf _HUGE *ref, intf _HUGE *fnlen));

extern FRETVAL(intf) nd24lref
    PROTO((void));

extern FRETVAL(intf) nd2scomp
    PROTO((intf *scheme));

extern FRETVAL(intf) ndf24scompress
    PROTO((intf *scheme));

extern FRETVAL(intf) nd2sjpeg
    PROTO((intf *quality,intf *force_baseline));

extern FRETVAL(intf) ndf24sjpeg
    PROTO((intf *quality,intf *force_baseline));

/*
** from dfF.c
*/
#ifndef DF_FNAMES
#   define DF_FNAMES
#ifdef DF_CAPFNAMES
#   define ndfiaccess FNAME(DFIACCESS)
#   define ndfiopen  FNAME(DFIOPEN)
#   define ndfclose  FNAME(DFCLOSE)
#   define ndfdesc   FNAME(DFDESC)
#   define ndfdup    FNAME(DFDUP)
#   define ndfdel    FNAME(DFDEL)
#   define ndfstart  FNAME(DFSTART)
#   define ndfread   FNAME(DFREAD)
#   define ndfseek   FNAME(DFSEEK)
#   define ndfwrite  FNAME(DFWRITE)
#   define ndfupdate FNAME(DFUPDATE)
#   define ndfget    FNAME(DFGET)
#   define ndfput    FNAME(DFPUT)
#   define ndfsfind  FNAME(DFSFIND)
#   define ndffind   FNAME(DFFIND)
#   define ndferrno  FNAME(DFERRNO)
#   define ndfnewref FNAME(DFNEWREF)
#   define ndfnumber FNAME(DFNUMBER)
#   define ndfstat   FNAME(DFSTAT)
#   define ndfiishdf FNAME(DFIISHDF)
#else   /* !DF_CAPFNAMES */
#   define ndfiaccess FNAME(dfiaccess)
#   define ndfiopen  FNAME(dfiopen)
#   define ndfclose  FNAME(dfclose)
#   define ndfdesc   FNAME(dfdesc)
#   define ndfdup    FNAME(dfdup)
#   define ndfdel    FNAME(dfdel)
#   define ndfstart  FNAME(dfstart)
#   define ndfread   FNAME(dfread)
#   define ndfseek   FNAME(dfseek)
#   define ndfwrite  FNAME(dfwrite)
#   define ndfupdate FNAME(dfupdate)
#   define ndfget    FNAME(dfget)
#   define ndfput    FNAME(dfput)
#   define ndfsfind  FNAME(dfsfind)
#   define ndffind   FNAME(dffind)
#   define ndferrno  FNAME(dferrno)
#   define ndfnewref FNAME(dfnewref)
#   define ndfnumber FNAME(dfnumber)
#   define ndfstat   FNAME(dfstat)
#   define ndfiishdf FNAME(dfiishdf)
#endif /* DF_CAPFNAMES */
#endif /* DF_FNAMES */

extern FRETVAL(intf) ndfiopen
    PROTO((_fcd name, intf _HUGE *access, intf _HUGE *defdds, intf _HUGE *namelen));

extern FRETVAL(intf) ndfclose
    PROTO((intf _HUGE *dfile));

extern FRETVAL(intf) ndfdesc
    PROTO((intf _HUGE *dfile, intf _HUGE ptr[][4], intf _HUGE *begin,
            intf _HUGE *num));

extern FRETVAL(intf) ndfdup
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref, intf _HUGE *otag,
            intf _HUGE *oref));

extern FRETVAL(intf) ndfdel
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref));

extern FRETVAL(intf) ndfiaccess
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref, _fcd access, intf _HUGE *acclen));

extern FRETVAL(intf) ndfstart
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref, char _HUGE *access));

extern FRETVAL(intf) ndfread
    PROTO((intf _HUGE *dfile, _fcd ptr, intf _HUGE *len));

extern FRETVAL(intf) ndfseek
    PROTO((intf _HUGE *dfile, intf _HUGE *offset));

extern FRETVAL(intf) ndfwrite
    PROTO((intf _HUGE *dfile, _fcd ptr, intf _HUGE *len));

extern FRETVAL(intf) ndfupdate
    PROTO((intf _HUGE *dfile));

extern FRETVAL(intf) ndfget
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref, _fcd ptr));

extern FRETVAL(intf) ndfput
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref, _fcd ptr, intf _HUGE *len));

extern FRETVAL(intf) ndfsfind
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *ref));

extern FRETVAL(intf) ndffind
    PROTO((intf _HUGE *dfile, intf _HUGE *itag, intf _HUGE *iref, intf _HUGE *len));

extern FRETVAL(intf) ndferrno
    PROTO((void));

extern FRETVAL(intf) ndfnewref
    PROTO((intf _HUGE *dfile));

extern FRETVAL(intf) ndfnumber
    PROTO((intf _HUGE *dfile, intf _HUGE *tag));

#ifdef TEMP_OUT
extern FRETVAL(intf) ndfstat
    PROTO((intf _HUGE *dfile, DFdata _HUGE *dfinfo));
#endif

extern FRETVAL(intf) ndfiishdf
    PROTO((_fcd name, intf _HUGE *namelen));

/*
** from dfutil.c
*/
extern uint16 DFfindnextref
  PROTO((int32 file_id, uint16 tag, uint16 lref));

/*
** from dfutilF.c
*/
#ifndef DFUTIL_FNAMES
#   define DFUTIL_FNAMES
#ifdef DF_CAPFNAMES
#   define ndfindnr          FNAME(DFINDNR)
#   define ndffindnextref    FNAME(DFFINDNEXTREF)
#else
#   define ndfindnr          FNAME(dfindnr)
#   define ndffindnextref    FNAME(dffindnextref)
#endif /* DF_CAPFNAMES */
#endif /* DFUTIL_FNAMES */

extern FRETVAL(intf) ndfindnr
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *lref));

extern FRETVAL(intf) ndffindnextref
    PROTO((intf _HUGE *dfile, intf _HUGE *tag, intf _HUGE *lref));

/*
** from herrF.c
*/
#ifndef HERR_FNAMES
#   define HERR_FNAMES
#ifdef DF_CAPFNAMES
#   define nheprnt   FNAME(HEPRNT)
#else
#   define nheprnt   FNAME(heprnt)
#endif /* DF_CAPFNAMES */
#endif  /* HERR_FNAMES */

extern FRETVAL(VOID) nheprnt
    PROTO((intf _HUGE *print_levels));

/*
** from hfileF.c
*/
#ifndef HFILE_FNAMES
#   define HFILE_FNAMES
#ifdef DF_CAPFNAMES
#   define nhiopen   FNAME(HIOPEN)
#   define nhclose   FNAME(HCLOSE)
#   define nhnumber  FNAME(HNUMBR)
#else
#   define nhiopen   FNAME(hiopen)
#   define nhclose  FNAME(hclose)
#   define nhnumber FNAME(hnumbr)
#endif /* DF_CAPFNAMES */
#endif /* HFILE_FNAMES */

extern FRETVAL(intf) nhiopen
    PROTO((_fcd name, intf _HUGE *access, intf _HUGE *defdds, intf _HUGE *namelen));

extern FRETVAL(intf) nhclose
    PROTO((intf _HUGE *file_id));

/*
** from dfufp2im.c
*/
#ifndef DFUFP2I_FNAMES
#   define DFUFP2I_FNAMES
#ifdef DF_CAPFNAMES
#   define nduif2i       FNAME(DUIF2I)
#else
#   define nduif2i       FNAME(duif2i)
#endif /* DF_CAPFNAMES */
#endif /* DFUFP2I_FNAMES */

extern FRETVAL(int) nduif2i
    PROTO((int32 _HUGE *hdim, int32 _HUGE *vdim, float32 _HUGE *max,
            float32 _HUGE *min, float32 _HUGE hscale[], float32 _HUGE vscale[],
            float32 _HUGE data[], uint8 _HUGE *palette, _fcd outfile,
            int _HUGE *ct_method, int32 _HUGE *hres, int32 _HUGE *vres,
            int _HUGE *compress, int _HUGE *lenfn));

extern int DFUfptoimage
    PROTO((int32 hdim, int32 vdim, float32 max, float32 min,
            float32 _HUGE *hscale, float32 _HUGE *vscale, float32 _HUGE *data,
            uint8 _HUGE *palette, char _HUGE *outfile, int ct_method,
            int32 hres, int32 vres, int compress));

/* temp. fix for circular includes with vsets */
#ifdef PERM_OUT
#include "vproto.h"
#endif /* PERM_OUT */

#ifdef __cplusplus
}
#endif

#endif /* _H_PROTO */
