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

/* In order to speed the conversion process, eliminate a layer of function
 * calls by making DFconvert into a macro.
 * Peter Webb, Oct 11, 1989
 */
#ifndef DFCONVRT_H
#define DFCONVRT_H

#ifndef FUNC_CONV
/* This is the default */
/* using the DFconvert macro instead of function */
#define DFconvert(src,dest,ntype,stype,dtype,size,status)\
{ char *s=(src), *d=(dest);\
  int nt=(ntype), st=(stype), dt=(dtype);\
  int sz=((int)size);\
  if (nt==DFNT_FLOAT) {\
    if ((st==DFNTF_IEEE && dt==DFNTF_PC) ||\
        (st==DFNTF_PC && dt==DFNTF_IEEE)) {\
      register int32 i;\
      for (i=0;i<sz*4;i+=4) {\
        d[i] = s[i+3];\
        d[i+1] = s[i+2];\
        d[i+2] = s[i+1];\
        d[i+3] = s[i];\
      }\
      status=0;\
    } else {\
      if (st==DFNTF_PC) {\
        register int i;\
        register char t;\
        for (i=0;i<sz*4;i+=4) {\
          t = s[i];\
          s[i] = s[i+3];\
          s[i+3] = t;\
          t = s[i+1];\
          s[i+1] = s[i+2];\
          s[i+2] = t;\
        }\
        st=DFNTF_IEEE;\
      }\
      if (st==DFNTF_IEEE && dt==DFNTF_CRAY) {\
	int i=1;\
        SCUP32(s,d,&sz,&i);\
      } else if (st==DFNTF_CRAY && (dt==DFNTF_IEEE || dt==DFNTF_PC)) {\
	int i=1;\
        CSPK32(s,d,&sz,&i);\
      } else if (st==DFNTF_IEEE && dt==DFNTF_VAX) {\
        status = DFCVieeeF2vaxF((union float_uint_uchar *)s,(union float_uint_uchar *)d,sz);\
      } else if (st==DFNTF_VAX && (dt==DFNTF_IEEE || dt==DFNTF_PC)) {\
        status = DFCVvaxF2ieeeF((union float_uint_uchar *)s,(union float_uint_uchar *)d,sz);\
      } else {\
        status = -1;\
      }\
      if (dt==DFNTF_PC) {\
        register int i;\
        register char t;\
        for (i=0;i<sz*4;i+=4) {\
          t = d[i];\
          d[i] = d[i+3];\
          d[i+3] = t;\
          t = d[i+1];\
          d[i+1] = d[i+2];\
          d[i+2] = t;\
        }\
      }\
      if ((stype)==DFNTF_PC) {\
        register int i;\
        register char t;\
        for (i=0;i<sz*4;i+=4) {\
          t = s[i];\
          s[i] = s[i+3];\
          s[i+3] = t;\
          t = s[i+1];\
          s[i+1] = s[i+2];\
          s[i+2] = t;\
        }\
        st=DFNTF_IEEE;\
      }\
    }\
  } else {\
  status = -1;\
  }\
  if (status == -1) HERROR(DFE_BADCONV);\
}
#endif /* !FUNC_CONV */

#endif /* !DFCONVRT_H */
