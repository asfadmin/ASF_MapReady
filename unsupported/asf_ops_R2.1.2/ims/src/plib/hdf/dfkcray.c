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

/************************************************************************/
/* DFKCRAY.C                                                            */
/************************************************************************/

/*------------------------------------------------------------------
 File:  dfkcray.c

 Purpose:
    Routines to support Cray conversion to and from HDF format

 Invokes:
    
 PRIVATE conversion functions:
    DFKui2i -  Unicos routine for importing 16 bit unsigned integers
    DFKui2s -  Unicos routine for importing 16 bit signed integers
    DFKuo2i -  Unicos routine for exporting 16 bit integers (both)
    DFKui4i -  Unicos routine for importing unsigned 32bit integers
    DFKui4s -  Unicos routine for importing signed 32bit integers
    DFKuo4i -  Unicos routine for exporting 4 byte integers (both)
    DFKui4f -  Unicos routine for importing 32 bit floats   
    DFKuo4f -  Unicos routine for exporting 32 bit floats  
    DFKui8f -  Unicos routine for importing 64 bit floats 
    DFKuo8f -  Unicos routine for exporting 64 bit floats
    DFKlui2i-  Unicos routine for importing little-endian 16 bit unsigned ints
    DFKlui2s-  Unicos routine for importing little-endian 16 bit signed ints
    DFKluo2i-  Unicos routine for exporting little-endian 16 bit ints (both)
    DFKlui4i-  Unicos routine for importing little-endian unsigned 32bit ints
    DFKlui4s-  Unicos routine for importing little-endian signed 32bit ints
    DFKluo4i-  Unicos routine for exporting little-endian 4 byte ints (both)
    DFKlui4f-  Unicos routine for importing little-endian 32 bit floats
    DFKluo4f-  Unicos routine for exporting little-endian 32 bit floats
    DFKlui8f-  Unicos routine for importing little-endian 64 bit floats
    DFKluo8f-  Unicos routine for exporting little-endian 64 bit floats

 Remarks:
    These files used to be in dfconv.c, but it got a little too huge,
    so I broke them out into seperate files. - Q

 *------------------------------------------------------------------*/

/*****************************************************************************/
/*                                                                           */
/*    All the routines in this file marked as PRIVATE have been marked so    */
/*  for a reason.  *ANY* of these routines may or may nor be supported in    */
/*  the next version of HDF (4.00).  Furthurmore, the names, paramters, or   */
/*  functionality is *NOT* guaranteed to remain the same.                    */
/*    The *ONLY* guarantee possible is that DFKnumin(), and DFKnumout()      */
/*  will not change.  They are *NOT* guaranteed to be implemented in the     */
/*  next version of HDF as function pointers.  They are guaranteed to take   */
/*  the same arguments and produce the same results.                         */
/*    If your programs call any routines in this file except for             */
/*  DFKnumin(), DFKnumout, and/or DFKsetntype(), your code may not work      */
/*  with future versions of HDF and your code will *NOT* be portable.        */
/*                                                                           */
/*****************************************************************************/

#include "hdf.h"
#include "herr.h"
#include "hconv.h"

/*****************************************************************************/
/* NUMBER CONVERSION ROUTINES FOR THE UNICOS OPERATING SYSTEM                */
/* Parameter dest_stride is used because source buffer and dest buffer will  */
/* be different sizes for all data types except char.                        */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*   For all you CRAY lovers out there, I just thought I would tell you      */
/* how my routines work.  If you know of a better, or more efficient         */
/* method for converting any of these, please tell me.                       */
/*                                                                           */
/*   Mark Straka provided the routine to convert the 32 bit and 64 bit       */
/* Cray floats into the IEEE format and back.  In the previous release of    */
/* HDF, his routines were incorporated from Fortran source.  In this         */
/* release, the original C source has been incorporated.  Please note,       */
/* the Public Domain notice included at the top of this file pertains to     */
/* these routines as well.                                                   */
/*                                                                           */
/*****************************************************************************/

#if defined(UNICOS)

#define UI2I_MASKA  0xffff000000000000
#define UI2I_MASKB  0x0000ffff00000000
#define UI2I_MASKC  0x00000000ffff0000
#define UI2I_MASKD  0x000000000000ffff

/************************************************************/
/* DFKui2i()                                                */
/* -->Unicos routine for importing 2 byte data items        */ 
/* (**) This routine converts two byte IEEE to eight byte   */
/*      Cray big endian integer.                            */
/************************************************************/
#ifdef PROTOTYPE
int DFKui2i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKui2i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  register uint32 i;
  int fast_processing=0;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
    long *lp_dest;
    unsigned long *lp_src;
  char *FUNC="DFKui2i";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);                /* No elements is an error */
    return FAIL;
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0) 
      fast_processing = 1;            

    if(fast_processing) {
#ifndef DUFF_ui2i
#if defined TEST2_ui2i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
            lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
            lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
            lp_dest[3]=lp_src[0]&UI2I_MASKD;
            lp_dest+=4;
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                break;

            case 2:
                lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                break;

            case 1:
                lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                break;

            default:
                break;
          } /* end switch */
#elif defined TEST1_ui2i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
            *lp_dest++=(lp_src[0]&UI2I_MASKB)>>32;
            *lp_dest++=(lp_src[0]&UI2I_MASKC)>>16;
            *lp_dest++=lp_src[0]&UI2I_MASKD;
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
                *lp_dest++=(lp_src[0]&UI2I_MASKB)>>32;
                *lp_dest++=(lp_src[0]&UI2I_MASKC)>>16;
                break;

            case 2:
                *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
                *lp_dest++=(lp_src[0]&UI2I_MASKB)>>32;
                break;

            case 1:
                *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
                break;

            default:
                break;
          } /* end switch */
#else
    for(i = 0; i < num_elm; i++) {
      lptr_dest[0] = 0x0000000000000000;
      dest[6] = source[0];
      dest[7] = source[1];
      source += 2;
      lptr_dest++;
      dest = (uint8*)lptr_dest;
    }
#endif
#else   /* DUFF_ui2i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));

        odd_man_out = num_elm%4;

        num_elm/=4;
        n=(num_elm+7)/8;
		if(orig_num_elm>3)
        switch(num_elm%8) {
            case 0:
                do{
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
#ifdef QAK
            case 15:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 14:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 13:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 12:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 11:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 10:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 9:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 8:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
#endif
            case 7:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 6:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 5:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 4:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 3:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 2:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 1:
                    lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                    lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                    lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                    lp_dest[3]=lp_src[0]&UI2I_MASKD;
                    lp_dest+=4;
                    lp_src++;
                } while(--n>0);
		}
        switch(odd_man_out) {
            case 3:
                lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                lp_dest[2]=(lp_src[0]&UI2I_MASKC)>>16;
                break;

            case 2:
                lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                lp_dest[1]=(lp_src[0]&UI2I_MASKB)>>32;
                break;

            case 1:
                lp_dest[0]=(lp_src[0]&UI2I_MASKA)>>48;
                break;

            default:
                break;
          } /* end switch */
#endif  /* DUFF_ui2i */
  }
  else { /* Generic stride processing */
    for(i = 0; i < num_elm; i++) {
      dest[0] = 0x00;
      dest[1] = 0x00;
      dest[2] = 0x00;
      dest[3] = 0x00;
      dest[4] = 0x00;
      dest[5] = 0x00;
      dest[6] = source[0];
      dest[7] = source[1];
      source += source_stride;
      dest += dest_stride;
    }
  }
  return 0;
}

#define UI2S_MASKA  0xffff000000000000
#define UI2S_MASKB  0x0000ffff00000000
#define UI2S_MASKC  0x00000000ffff0000
#define UI2S_MASKD  0x000000000000ffff
#define UI2S_MASKE  0x8000000000000000
#define UI2S_MASKF  0x0000800000000000
#define UI2S_MASKG  0x0000000080000000
#define UI2S_MASKH  0x0000000000008000
#define UI2S_MASKI  0xffffffffffff0000

/************************************************************/
/* DFKui2s()                                                */
/* -->Unicos routine for importing 2 byte signed ints       */
/* (**) This routine converts two byte IEEE to eight byte   */
/*      Cray.                                               */
/************************************************************/
#ifdef PROTOTYPE
int DFKui2s(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKui2s(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  register uint32 i;
  int fast_processing=0;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
    long *lp_dest;
    unsigned long *lp_src;
  char *FUNC="DFKui2s";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0) 
      fast_processing = 1;            

  if(fast_processing) {
#ifndef DUFF_ui2s
#if defined TEST2_ui2s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
            lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
            if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
            lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
            if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
            lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
            if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
            lp_dest[3]|=lp_src[0]&UI2S_MASKD;
            lp_dest+=4;
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                    lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                /* falls through */

            case 2:
                if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                    lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                /* falls through */

            case 1:
                if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                    lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                break;

            case 0:
                break;
          } /* end switch */
#elif defined TEST1_ui2s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
            if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&UI2I_MASKB)>>32;
            if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&UI2I_MASKC)>>16;
            if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=lp_src[0]&UI2I_MASKD;
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
                if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=(lp_src[0]&UI2I_MASKB)>>32;
                if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=(lp_src[0]&UI2I_MASKC)>>16;
                break;

            case 2:
                if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
                if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=(lp_src[0]&UI2I_MASKB)>>32;
                break;

            case 1:
                if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=(lp_src[0]&UI2I_MASKA)>>48;
                break;

            case 0:
                break;
          } /* end switch */
#else
    for(i = 0; i < num_elm; i++) {
      if((source[0] & 0x80))           /* Can't forget to extend sign */
	lptr_dest[0] = 0xffffffffffffffff;
      else
	lptr_dest[0] = 0x0000000000000000;
      dest[6] = source[0];
      dest[7] = source[1];
      source += 2;
      lptr_dest++;
      dest = (uint8*)lptr_dest;
    }
#endif
#else   /* DUFF_ui2s */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));

        odd_man_out = num_elm%4;

        num_elm/=4;
        n=(num_elm+7)/8;
		if(orig_num_elm>3)
        switch(num_elm%8) {
            case 0:
                do{
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
#ifdef QAK
            case 15:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 14:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 13:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 12:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 11:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 10:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 9:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 8:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
#endif
            case 7:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 6:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 5:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 4:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 3:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 2:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
            case 1:
                    if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                        lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                    lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                    if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                        lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                    lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                    if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                        lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                    lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                    if(lp_src[0] & UI2S_MASKH)      /* Can't forget to extend sign */
                        lp_dest[3] = UI2S_MASKI|(lp_src[0]&UI2S_MASKD);
                    lp_dest[3]|=lp_src[0]&UI2S_MASKD;
                    lp_dest+=4;
                    lp_src++;
                } while(--n>0);
		}
        switch(odd_man_out) {
            case 3:
                if(lp_src[0] & UI2S_MASKG)      /* Can't forget to extend sign */
                    lp_dest[2] = UI2S_MASKI|((lp_src[0]&UI2S_MASKC)>>16);
                lp_dest[2]|=(lp_src[0]&UI2S_MASKC)>>16;
                /* falls through */

            case 2:
                if(lp_src[0] & UI2S_MASKF)      /* Can't forget to extend sign */
                    lp_dest[1] = UI2S_MASKI|((lp_src[0]&UI2S_MASKB)>>32);
                lp_dest[1]|=(lp_src[0]&UI2S_MASKB)>>32;
                /* falls through */

            case 1:
                if(lp_src[0] & UI2S_MASKE)      /* Can't forget to extend sign */
                    lp_dest[0] = UI2S_MASKI|((lp_src[0]&UI2S_MASKA)>>48);
                lp_dest[0]|=(lp_src[0]&UI2S_MASKA)>>48;
                break;

            case 0:
                break;
          } /* end switch */
#endif  /* DUFF_ui2s */
  }
  else { /* Generic stride processing */
    for(i = 0; i < num_elm; i++) {
      if((source[0] & 0x80)) {          /* Can't forget to extend sign */
	dest[0] = 0xff;
	dest[1] = 0xff;
	dest[2] = 0xff;
	dest[3] = 0xff;
	dest[4] = 0xff;
	dest[5] = 0xff;
      }
      else {
	dest[0] = 0x00;
	dest[1] = 0x00;
	dest[2] = 0x00;
	dest[3] = 0x00;
	dest[4] = 0x00;
	dest[5] = 0x00;
      }
      dest[6] = source[0];
      dest[7] = source[1];
      source += source_stride;
      dest += dest_stride;
    }
  }
  return 0;
}

#define UO2I_MASK 0x000000000000ffff

/************************************************************/
/* DFKuo2i()                                                */
/* -->Unicos routine for exporting 2 byte data items        */ 
/************************************************************/
#ifdef PROTOTYPE
int DFKuo2i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKuo2i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  register uintn i;
  int fast_processing=0;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
    long *lp_dest;
    long *lp_src;
    char *FUNC="DFKuo2i";

    HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0)
      fast_processing = 1;            

  if(fast_processing) {
#ifndef DUFF_uo2i
#if defined TEST1_uo2i
    int odd_man_out;        /* By default there are even num_elm */
    intn n;

    odd_man_out = num_elm%4;

    n=num_elm/4;
    lp_dest=(long *)dest;
    lp_src=(long *)source;
    for(i = 0; i < n; i++) {
        *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                    ((lp_src[1]&UO2I_MASK)<<32) |
                    ((lp_src[2]&UO2I_MASK)<<16) |
                    (lp_src[3]&UO2I_MASK);
        lp_src+=4;
    }
    switch(odd_man_out) {   /* clean up leftovers */
        case 3:
            *lp_dest=((lp_src[0]&UO2I_MASK)<<48) |
                        ((lp_src[1]&UO2I_MASK)<<32) |
                        ((lp_src[2]&UO2I_MASK)<<16);
            break;

        case 2:
            *lp_dest=((lp_src[0]&UO2I_MASK)<<48) |
                        ((lp_src[1]&UO2I_MASK)<<32);
            break;

        case 1:
            *lp_dest=(lp_src[0]&UO2I_MASK)<<48;
            break;

        default:
            break;
      } /* end switch */
#else
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[6];
      dest[1] = source[7];
      dest += 2;
      source += 8;
    }
#endif
#else   /* DUFF_uo2i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm%4;

        num_elm/=4;
        n=(num_elm+7)/8;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
		if(orig_num_elm>3)
        switch(num_elm%8) {
            case 0:
                do{
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
#ifdef QAK
            case 15:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 14:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 13:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 12:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 11:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 10:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 9:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 8:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
#endif
            case 7:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 6:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 5:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 4:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 3:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 2:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
            case 1:
                    *lp_dest++=((lp_src[0]&UO2I_MASK)<<48) |
                                ((lp_src[1]&UO2I_MASK)<<32) |
                                ((lp_src[2]&UO2I_MASK)<<16) |
                                (lp_src[3]&UO2I_MASK);
                    lp_src+=4;
                } while(--n>0);
		}

        switch(odd_man_out) {   /* clean up leftovers */
            case 3:
                *lp_dest=((lp_src[0]&UO2I_MASK)<<48) |
                            ((lp_src[1]&UO2I_MASK)<<32) |
                            ((lp_src[2]&UO2I_MASK)<<16);
                break;

            case 2:
                *lp_dest=((lp_src[0]&UO2I_MASK)<<48) |
                            ((lp_src[1]&UO2I_MASK)<<32);
                break;

            case 1:
                *lp_dest=(lp_src[0]&UO2I_MASK)<<48;
                break;

            default:
                break;
          } /* end switch */
#endif  /* DUFF_uo2i */
  }
  else { /* Generic Stride processing */
    for(i = 0; i < num_elm; i++){
      dest[0] = source[6];
      dest[1] = source[7];
      source += source_stride;
      dest += dest_stride;
    }
  }
  return 0;
}

#define UI4I_MASKA 0xffffffff00000000
#define UI4I_MASKB 0x00000000ffffffff

/************************************************************/
/* DFKui4i()                                                */
/* -->Unicos routine for importing 4 byte unsigned ints     */
/************************************************************/
#ifdef PROTOTYPE
int DFKui4i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKui4i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing=0;
  register uint32 i;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
    long *lp_dest;
    unsigned long *lp_src;
  char *FUNC="DFKui4i";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

    if(source_stride == 0 && dest_stride == 0)
        fast_processing = 1;
  
    if(fast_processing) {
#ifndef DUFF_ui4i
#if defined TEST2_ui4i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
            lp_dest[1]=lp_src[0]&UI4I_MASKB;
            lp_dest+=2;
            lp_src++;
          } /* end for */
        if(odd_man_out)
            *lp_dest=(lp_src[0]&UI4I_MASKA)>>32;
#elif defined TEST1_ui4i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            *lp_dest++=(lp_src[0]&UI4I_MASKA)>>32;
            *lp_dest++=lp_src[0]&UI4I_MASKB;
            lp_src++;
          } /* end for */
        if(odd_man_out)
            *lp_dest++=(lp_src[0]&UI4I_MASKA)>>32;
#else
        for(i = 0; i < num_elm; i++) {
            lptr_dest[0]=0;
            dest[4] = source[0];
            dest[5] = source[1];
            dest[6] = source[2];
            dest[7] = source[3];
            source += 4;
            lptr_dest ++;
            dest = (uint8 *)lptr_dest;
          } /* end for */
#endif
#else   /* DUFF_ui4i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(unsigned long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
#ifdef QAK
            case 15:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 14:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 13:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 12:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 11:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 10:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 9:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 8:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
#endif
            case 7:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 6:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 5:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 4:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 3:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 2:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 1:
                    lp_dest[0]=(lp_src[0]&UI4I_MASKA)>>32;
                    lp_dest[1]=lp_src[0]&UI4I_MASKB;
                    lp_dest+=2;
                    lp_src++;
                } while(--n>0);
		}
        if(odd_man_out)
            *lp_dest=(lp_src[0]&UI4I_MASKA)>>32;
#endif  /* DUFF_ui4i */
      } /* end if */
    else {
        for(i = 0; i < num_elm; i++) {
            dest[0] = 0;
            dest[1] = 0;
            dest[2] = 0;
            dest[3] = 0;
            dest[4] = source[0];
            dest[5] = source[1];
            dest[6] = source[2];
            dest[7] = source[3];
            dest += dest_stride;
            source += source_stride;
          } /* end for */
      } /* end else */
  return 0;
}

#define UI4S_MASKA 0xffffffff00000000
#define UI4S_MASKB 0x00000000ffffffff
#define UI4S_MASKC 0x8000000000000000
#define UI4S_MASKD 0x0000000080000000
#define UI4S_MASKE 0xffffffff00000000

/************************************************************/
/* DFKui4s()                                                */
/* -->Unicos routine for importing 4 signed ints            */ 
/************************************************************/
#ifdef PROTOTYPE
int DFKui4s(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKui4s(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing=0;
  register uint32 i;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
    long *lp_dest;
    long *lp_src;
  char *FUNC="DFKui4s";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  if(source_stride == 0 && dest_stride == 0)
    fast_processing = 1;
  
    if(fast_processing) {
#ifndef DUFF_ui4s
#if defined TEST2_ui4s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
            lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
            if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
            lp_dest[1]|=lp_src[0]&UI4S_MASKB;
            lp_dest+=2;
            lp_src++;
          } /* end for */
        if(odd_man_out) {
            if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
            lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
          } /* end if */
#elif defined TEST1_ui4s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&UI4S_MASKA)>>32;
            if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=lp_src[0]&UI4S_MASKB;
            lp_src++;
          } /* end for */
        if(odd_man_out) {
            if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&UI4S_MASKA)>>32;
          } /* end if */
#else
        for(i = 0; i < num_elm; i++) {
            if((source[0] & 0x80))            /* Can't forget to sign extend */
                lptr_dest[0] = 0xffffffffffffffff;
            else
                lptr_dest[0] = 0x0000000000000000;
            dest[4] = source[0];
            dest[5] = source[1];
            dest[6] = source[2];
            dest[7] = source[3];
            source += 4;
            lptr_dest ++;
            dest = (uint8 *)lptr_dest;
        }
#endif
#else   /* DUFF_ui4s */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
#ifdef QAK
            case 15:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 14:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 13:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 12:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 11:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 10:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 9:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 8:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
#endif
            case 7:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 6:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 5:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 4:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 3:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 2:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
            case 1:
                    if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                        lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
                    lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
                    if(lp_src[0] & UI4S_MASKD)            /* Can't forget to sign extend */
                        lp_dest[1] = UI4S_MASKE|(lp_src[0]&UI4S_MASKA);
                    lp_dest[1]|=lp_src[0]&UI4S_MASKB;
                    lp_dest+=2;
                    lp_src++;
                } while(--n>0);
		}
        if(odd_man_out) {
            if(lp_src[0] & UI4S_MASKC)            /* Can't forget to sign extend */
                lp_dest[0] = UI4S_MASKE|((lp_src[0]&UI4S_MASKA)>>32);
            lp_dest[0]|=(lp_src[0]&UI4S_MASKA)>>32;
          } /* end if */
#endif  /* DUFF_ui4s */
      } /* end if */
  else 
    for(i = 0; i < num_elm; i++) {
      if(source[0] & 0x80) {          /* Can't forget to sign extend */
        dest[0] = 0xff;
        dest[1] = 0xff;
        dest[2] = 0xff;
        dest[3] = 0xff;
      }
      else {
        dest[0] = 0;
        dest[1] = 0;
        dest[2] = 0;
        dest[3] = 0;
      }
      dest[4] = source[0];
      dest[5] = source[1];
      dest[6] = source[2];
      dest[7] = source[3];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

#define UO4I_MASK 0x00000000ffffffff

/************************************************************/
/* DFKuo4i()                                                */
/* -->Unicos routine for exporting 4 byte data items        */ 
/************************************************************/

#ifdef PROTOTYPE
int DFKuo4i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKuo4i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing=0;
  register uintn i;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
    long *lp_dest;
    long *lp_src;
  char *FUNC="DFKuo4i";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  if(source_stride == 0 && dest_stride == 0) {
    fast_processing = 1;
  }
  
  if(fast_processing) {

#ifndef DUFF_uo4i
#if defined TEST1_uo4i
    int odd_man_out;        /* By default there are even num_elm */
    intn n;

    odd_man_out = num_elm % 2;

    n=num_elm/2;
    lp_dest=(long *)dest;
    lp_src=(long *)source;
    for(i = 0; i < n; i++) {
        *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
        lp_src+=2;
    }
    if(odd_man_out)
        *lp_dest=(lp_src[0]&UO4I_MASK)<<32;
#else
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[4];
      dest[1] = source[5];
      dest[2] = source[6];
      dest[3] = source[7];
      dest += 4;
      source += 8;
    }
#endif
#else   /* DUFF_uo4i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
        lp_dest=(long *)dest;
        lp_src=(long *)source;

		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
#ifdef QAK
            case 15:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 14:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 13:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 12:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 11:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 10:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 9:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 8:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
#endif
            case 7:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 6:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 5:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 4:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 3:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 2:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
            case 1:
                    *lp_dest++=((lp_src[0]&UO4I_MASK)<<32)|(lp_src[1]&UO4I_MASK);
                    lp_src+=2;
                } while(--n>0);
		}
        if(odd_man_out)
            *lp_dest++=(lp_src[0]&UO4I_MASK)<<32;

#endif  /* DUFF_uo4i */
  }
  else 
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[4];
      dest[1] = source[5];
      dest[2] = source[6];
      dest[3] = source[7];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

#define UI4F_MASKA 0x8000000000000000
#define UI4F_MASKB 0x7f80000000000000
#define UI4F_MASKC 0x007fffff00000000
#define UI4F_MASKD 0x0000800000000000
#define UI4F_MASKE 0x0000000080000000
#define UI4F_MASKF 0x000000007f800000
#define UI4F_MASKG 0x00000000007fffff
#define UI4F_MASKH 0x0000000000008000
#define UI4F_MASKI 0x7fffffff00000000
#define UI4F_MASKJ 0x000000007fffffff

/************************************************************/
/* DFKui4f()                                                */
/* -->Unicos routine for importing 32 bit floats            */
/************************************************************/

/************************************************************

                     <<<< WARNING >>>>

    The nature of converting between 64 bit floating point
  numbers and 32 bit floating point numbers LOSES PRECISION.
  Taking a number in 64bit cray format, converting to IEEE
  (internal HDF format) and back will round the number at
  about the 7th decimal place.

 ************************************************************/

#ifdef PROTOTYPE
int DFKui4f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKui4f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source;
uint8 * dest;
uint32 num_elm;
uint32 source_stride;
uint32 dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int odd_man_out = 0;                  /* By default there are even num_elm */
  int i,j,n;
  long buf1;                            /* This is a temporary stride buf */
  long buf2;                            /* This is a temporary stride buf */
  uint8 * dud1 = (uint8*)&buf1;         /* Dummy pointer to buf1 for strides */
  uint8 * dud2 = (uint8*)&buf2;         /* Dummy pointer to buf2 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;            /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;              /* void and void* constructions */
#endif /* PROTOTYPE */
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKui4f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0)
      fast_processing = 1;

    if(fast_processing) {
#ifndef DUFF_ui4f
#if defined TEST1_ui4f
        odd_man_out = num_elm%2 ;
        n = num_elm / 2;
        for(i = 0; i < n; i++) {
            if((*lptr_src & UI4F_MASKI)
                lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                    ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                    (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
            else
                lptr_dest[0]=0;
            if((*lptr_src & UI4F_MASKJ)
                lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                    ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                    (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
            else
                lptr_dest[1]=0;
            lptr_dest+=2;
            lptr_src++;
          } /* end for */
        if(odd_man_out) {
            if((*lptr_src & UI4F_MASKI)
                lptr_dest[0] = (((lptr_src[0]) & UI4F_MASKA) |
                    (((lptr_src[0]) & UI4F_MASKB) >> 7) + (16258 << 48)) |
                    ((((lptr_src[0]) & UI4F_MASKC) >> 8) | (UI4F_MASKD));
            else
                lptr_dest[0]=0;
          } /* end if */
#else
        if((num_elm % 2))                   /* If this is true, we have odd num */
          odd_man_out = 1;
        num_elm = num_elm / 2;
        for(i = 0; i < num_elm; i++) {
            if(*(float*)lptr_src != 0) {
                *lptr_dest = (((*lptr_src & UI4F_MASKA) |
                        ((*lptr_src & UI4F_MASKB) >> 7) +
                        (16258 << 48)) |
                        (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD)));
                if(((*lptr_src & 0xffffffff00000000) << 1)== 0)
                    *lptr_dest = 0;
              } /* end if */
            else
                *lptr_dest = *lptr_src;
            lptr_dest++;
            if(*(float*)lptr_src != 0) {
                *lptr_dest = ((((((*lptr_src << 32) & UI4F_MASKA)) |
                        (((*lptr_src <<32) & UI4F_MASKB) >> 7) +
                        (16258 << 48)) |
                        ((((*lptr_src << 32) & UI4F_MASKC) >> 8) |(UI4F_MASKD))));
                if(((*lptr_src & 0x00000000ffffffff) << 33)== 0)
                    *lptr_dest = 0;
              } /* end if */
            else
                *lptr_dest = *lptr_src;
            lptr_src++;
            lptr_dest++;
          } /* end for */
        if(odd_man_out) {
            if(((float*)lptr_src)[0] != 0) {
                lptr_dest[0] = ((((lptr_src[0]) & UI4F_MASKA) |
                    (((lptr_src[0]) & UI4F_MASKB) >> 7) + (16258 << 48)) |
                    ((((lptr_src[0]) & UI4F_MASKC) >> 8) | (UI4F_MASKD)));
                if(((lptr_src[0] & 0xffffffff00000000) << 1)== 0)
                    lptr_dest[0] = 0;
              } /* end if */
            else
                *lptr_dest = *lptr_src;
          } /* end if */
#endif
#else   /* DUFF_ui4f */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
					if(*lptr_src & UI4F_MASKI)
                    	lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
					else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                    	lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
					else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
#ifdef QAK
            case 15:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 14:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 13:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 12:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 11:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 10:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 9:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 8:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
#endif
            case 7:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 6:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 5:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 4:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 3:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 2:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
            case 1:
                    if(*lptr_src & UI4F_MASKI)
                        lptr_dest[0] = ((*lptr_src & UI4F_MASKA) |
                            ((*lptr_src & UI4F_MASKB) >> 7) + (16258 << 48)) |
                            (((*lptr_src & UI4F_MASKC) >> 8) | (UI4F_MASKD));
                    else
						lptr_dest[0]=0;
					if(*lptr_src & UI4F_MASKJ)
                        lptr_dest[1] = ((((*lptr_src & UI4F_MASKE)) |
                            ((*lptr_src & UI4F_MASKF) >> 7) + (16258 << 16)) |
                            (((*lptr_src & UI4F_MASKG) >> 8) |(UI4F_MASKH))) << 32;
                    else
						lptr_dest[1]=0;
                    lptr_dest+=2;
                    lptr_src++;
                } while(--n>0);
		}
        if(odd_man_out) {
            if(*lptr_src & UI4F_MASKI)
                lptr_dest[0] = (((lptr_src[0]) & UI4F_MASKA) |
                    (((lptr_src[0]) & UI4F_MASKB) >> 7) + (16258 << 48)) |
                    ((((lptr_src[0]) & UI4F_MASKC) >> 8) | (UI4F_MASKD));
            else
                lptr_dest[0]=0;
          } /* end if */
#endif  /* DUFF_ui4f */
      } /* end if */
    else { /* We end up here if we are doing stride based processing */
        buf1 = 0;
        for(i = 0; i < num_elm; i++) {
            dud1[0] = source[0];               /* Loop would be less efficient */
            dud1[1] = source[1];
            dud1[2] = source[2];
            dud1[3] = source[3];

            if((float)buf1 != 0) {
                buf2 = (((buf1 & UI4F_MASKA) |
                        ((buf1 & UI4F_MASKB) >> 7) +
                        (16258 << 48)) |
                        (((buf1 & UI4F_MASKC) >> 8) | (UI4F_MASKD)));
                if((buf1 << 1)== 0)
                    buf2 = 0;
              } /* end if */
            else
                buf2 = buf1;

            dest[0] = dud2[0];            /* Loop would be less efficient */
            dest[1] = dud2[1];
            dest[2] = dud2[2];
            dest[3] = dud2[3];
            dest[4] = dud2[4];
            dest[5] = dud2[5];
            dest[6] = dud2[6];
            dest[7] = dud2[7];

            source += source_stride;
            dest += dest_stride;
          } /* end for */
      } /* end else */
  return;
}


#define UO4F_MASKA 0x8000000000000000
#define UO4F_MASKB 0x7fff000000000000
#define UO4F_MASKC 0x00007fffff000000
#define UO4F_MASKD 0x0000000000800000
#define UO4F_MASKE 0xffffffff00000000

/************************************************************/
/* DFKuo4f()                                                */
/* -->Unicos routine for exporting 32 bit floats            */
/************************************************************/

/************************************************************

                     <<<< WARNING >>>>

    The nature of converting between 64 bit floating point
  numbers and 32 bit floating point numbers LOSES PRECISION.
  Taking a number in 64bit cray format, converting to IEEE
  (internal HDF format) and back will round the number at
  about the 7th decimal place.

 ************************************************************/

#ifdef PROTOTYPE
int DFKuo4f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKuo4f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int odd_man_out = 0;                  /* By default there are even num_elm */
  int i,j,n;
  long buf1;                            /* This is a temporary stride buf */
  long buf2;                            /* This is a temporary stride buf */
  uint8 * dud1 = (uint8*)&buf1;         /* Dummy pointer to buf1 for strides */
  uint8 * dud2 = (uint8*)&buf2;         /* Dummy pointer to buf2 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;           /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;             /* void and void* constructions */
#endif /* PROTOTYPE */
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKuo4f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0)
      fast_processing = 1;            

    if(fast_processing) {
#ifndef DUFF_uo4f
#if defined TEST1_uo4f
        odd_man_out = num_elm%2;
        n=num_elm/2;
        for(i = 0; i < n; i++) {
            if(lptr_src[0] && lptr_src[1])  /* both not zero */
                lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                    ((((lptr_src[0] & UO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[0] & UO4F_MASKC) +
                    ((lptr_src[0] & UO4F_MASKD) << 1)) << 8)) & UO4F_MASKE) |
                    (((((lptr_src[1] & UO4F_MASKA) |
                    ((((lptr_src[1] & UO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[1] & UO4F_MASKC) +
                    ((lptr_src[1] & UO4F_MASKD) << 1)) << 8))&UO4F_MASKE)>>32);
            else if(lptr_src[0])    /* src[0] not zero */
                lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                    ((((lptr_src[0] & UO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[0] & UO4F_MASKC) +
                    ((lptr_src[0] & UO4F_MASKD) << 1)) << 8)) & UO4F_MASKE);
            else if(lptr_src[1])    /* src[1] not zero */
                lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                    ((((lptr_src[1] & UO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[1] & UO4F_MASKC) +
                    ((lptr_src[1] & UO4F_MASKD) << 1)) << 8))&UO4F_MASKE)>>32);
            else                    /* both zero */
                lptr_dest[0]=0;
            lptr_src+=2;
            lptr_dest++;
          } /* end for */
        if(odd_man_out)
            if(lptr_src[0])    /* src[0] not zero */
                lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                    ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                    (((lptr_src[0] & UO4F_MASKC) +
                    ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
            else                    /* both zero */
                lptr_dest[0]=0;
#else
        if((num_elm % 2))                   /* If this is true, we have odd num */
          odd_man_out = 1;
        for(i = 0; i < (int)(num_elm/2); i++) {
            buf1 = lptr_src[0];
            buf2 = lptr_src[1];
            if(buf1 != 0)
                buf1 = (((buf1 & UO4F_MASKA) |
                        ((((buf1 & UO4F_MASKB) >> 48) - 16258) << 55)) +
                        (((buf1 & UO4F_MASKC) +
                        ((buf1 & UO4F_MASKD) << 1)) << 8));
            else
                buf1 = 0;
            if(buf2 != 0)
                buf2 = ((((buf2 & UO4F_MASKA) |
                        ((((buf2 & UO4F_MASKB) >> 48) - 16258) << 55)) +
                        (((buf2 & UO4F_MASKC) +
                        ((buf2 & UO4F_MASKD) << 1)) << 8)) >> 32);
            else
                buf2 = 0;
            lptr_dest[0]=((buf1&0xffffffff00000000)|(buf2&0x00000000ffffffff));
            lptr_src ++;
            lptr_src ++;
            lptr_dest ++;
          } /* end for */
        if(odd_man_out) {
            if(lptr_src[0] != 0)
                lptr_dest[0] = (((lptr_src[0] & UO4F_MASKA) |
                        ((((lptr_src[0] & UO4F_MASKB) >> 48) - 16258) << 55)) +
                        (((lptr_src[0] & UO4F_MASKC) +
                        ((lptr_src[0] & UO4F_MASKD) << 1)) << 8));
            else
                lptr_dest[0] = 0;
          } /* end if */
#endif
#else   /* DUFF_uo4f */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;

		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
#ifdef QAK
            case 15:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 14:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 13:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 12:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 11:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 10:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 9:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 8:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
#endif
            case 7:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 6:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 5:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 4:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 3:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 2:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
            case 1:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB)>>48)-16258) << 55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0] & UO4F_MASKD)<<1))<<8))&UO4F_MASKE) |
                            (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                            ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[0] & UO4F_MASKC) +
                            ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
                    else if(lptr_src[1])    /* src[1] not zero */
                        lptr_dest[0] = (((((lptr_src[1] & UO4F_MASKA) |
                            ((((lptr_src[1] & UO4F_MASKB) >> 48)-16258)<<55)) +
                            (((lptr_src[1] & UO4F_MASKC) +
                            ((lptr_src[1]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE)>>32);
                    else                    /* both zero */
                        lptr_dest[0]=0;
                    lptr_src+=2;
                    lptr_dest++;
                } while(--n>0);
		}
        if(odd_man_out)
            if(lptr_src[0])    /* src[0] not zero */
                lptr_dest[0] = ((((lptr_src[0] & UO4F_MASKA) |
                    ((((lptr_src[0] & UO4F_MASKB) >> 48)-16258)<<55)) +
                    (((lptr_src[0] & UO4F_MASKC) +
                    ((lptr_src[0]&UO4F_MASKD)<<1))<<8))&UO4F_MASKE);
            else                    /* both zero */
                lptr_dest[0]=0;
#endif  /* DUFF_uo4f */
      } /* end if */
    else { /* We end up here if we are doing stride based processing */
        buf1 = 0;
        for(i = 0; i < num_elm; i++) {
            dud1[0] = source[0];               /* Loop would be less efficient */
            dud1[1] = source[1];
            dud1[2] = source[2];
            dud1[3] = source[3];
            dud1[4] = source[4];
            dud1[5] = source[5];
            dud1[6] = source[6];
            dud1[7] = source[7];

            if((float)buf1 != 0)
                buf2 = (((buf1 & UO4F_MASKA) |
                        ((((buf1 & UO4F_MASKB) >> 48) -16258) << 55)) +
                        (((buf1 & UO4F_MASKC) + ((buf1 & UO4F_MASKD) << 1)) << 8));
            else
                buf2 = buf1;

            dest[0] = dud2[0];            /* Loop would be less efficient */
            dest[1] = dud2[1];
            dest[2] = dud2[2];
            dest[3] = dud2[3];

            source += source_stride;
            dest += dest_stride;
          } /* end for */
      } /* end else */
  return;
}

#define UI8F_MASKA 0x8000000000000000
#define UI8F_MASKB 0x7ff0000000000000
#define UI8F_MASKC 0x000fffffffffffff
#define UI8F_MASKD 0x0000000000000008
#define UI8F_MASKE 0x0000800000000000
#define UI8F_MASKG 0x7fffffffffffffff

/************************************************************/
/* DFKui8f()                                                */
/* -->Unicos routine for importing 64 bit floats            */
/************************************************************/

#ifdef PROTOTYPE
int DFKui8f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKui8f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int i,j,n;
  long buf;                             /* This is a temporary stride buf */
  uint8 * dud = (uint8*)&buf;           /* Dummy pointer to buf1 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;           /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;             /* void and void* constructions */
#endif /* PROTOTYPE*/
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKui8f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0) 
    fast_processing = 1;            

    if(fast_processing) {
#ifndef DUFF_ui8f
#if defined TEST2_ui8f
        n=num_elm;
        for(i = 0; i < n; i ++) {
            buf=lptr_src[0];
            if(buf&UI8F_MASKG)
                lptr_dest[0] = ((buf & UI8F_MASKA) |
                    ((buf & UI8F_MASKB) >> 4) + (15362 << 48)) |
                    ((((buf & UI8F_MASKC) +
                    ((buf & UI8F_MASKD) << 1)) >> 5) | UI8F_MASKE);
            else
                lptr_dest[0]=0;
            lptr_src++;
            lptr_dest++;
          } /* end for */
#elif defined TEST1_ui8f
        n=num_elm;
        for(i = 0; i < n; i ++) {
            if(lptr_src[0]&UI8F_MASKG)
                lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                    ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362 << 48)) |
                    ((((lptr_src[0] & UI8F_MASKC) +
                    ((lptr_src[0] & UI8F_MASKD) << 1)) >> 5) | UI8F_MASKE);
            else
                lptr_dest[0]=0;
            lptr_src++;
            lptr_dest++;
          } /* end for */
#else
        for(i = 0; i < num_elm; i ++) {
            if (lptr_src[0] != 0) {
                lptr_dest[0] = (((lptr_src[0] & UI8F_MASKA) |
                        ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362 << 48)) |
                        ((((lptr_src[0] & UI8F_MASKC) +
                        ((lptr_src[0] & UI8F_MASKD) << 1)) >> 5) |
                        (UI8F_MASKE)) );
                if((lptr_dest[0] << 1) == 0)
                    lptr_dest[0] = 0;
              } /* end if */
            else
                lptr_dest[0] = 0;
            lptr_src++;
            lptr_dest++;
          } /* end for */
#endif
#else   /* DUFF_ui8f */
        n=(num_elm+7)/8;
        switch(num_elm%8) {
            case 0:
                do{
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
#ifdef QAK
            case 15:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 14:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 13:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 12:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 11:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 10:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 9:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 8:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
#endif
            case 7:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 6:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 5:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 4:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 3:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 2:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 1:
                    if(lptr_src[0]&UI8F_MASKG)
                        lptr_dest[0] = ((lptr_src[0] & UI8F_MASKA) |
                            ((lptr_src[0] & UI8F_MASKB) >> 4) + (15362<<48)) |
                            ((((lptr_src[0] & UI8F_MASKC) +
                            ((lptr_src[0] & UI8F_MASKD)<<1))>>5)|UI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
                } while(--n>0);
		}
#endif  /* DUFF_ui8f */
      } /* end if */
    else
        for(i = 0; i < num_elm; i ++) {
            dud[0] = source[0];
            dud[1] = source[1];
            dud[2] = source[2];
            dud[3] = source[3];
            dud[4] = source[4];
            dud[5] = source[5];
            dud[6] = source[6];
            dud[7] = source[7];

            if (buf != 0) {
                buf = (((buf & UI8F_MASKA) |
                        ((buf & UI8F_MASKB) >> 4) + (15362 << 48)) |
                        ((((buf & UI8F_MASKC) + ((buf & UI8F_MASKD) << 1)) >> 5) |
                        (UI8F_MASKE)) );
                if ((buf << 1) == 0)
                    buf = 0;
              } /* end if */
            else
                buf = 0;

            dest[0] = dud[0];
            dest[1] = dud[1];
            dest[2] = dud[2];
            dest[3] = dud[3];
            dest[4] = dud[4];
            dest[5] = dud[5];
            dest[6] = dud[6];
            dest[7] = dud[7];

            source += source_stride;
            dest += dest_stride;
          } /* end for */
  return;
}

#define UO8F_MASKA 0x8000000000000000
#define UO8F_MASKB 0x7fff000000000000
#define UO8F_MASKC 0x00007fffffffffff

/************************************************************/
/* DFKuo8f()                                                */
/* -->Unicos routine for exporting 64 bit floats            */
/************************************************************/

#ifdef PROTOTYPE
int DFKuo8f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKuo8f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int odd_man_out = 0;                  /* By default there are even num_elm */
  int i,j,n;
  long buf;                             /* This is a temporary stride buf */
  uint8 * dud = (uint8*)&buf;           /* Dummy pointer to buf1 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;           /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;             /* void and void* constructions */
#endif /* PROTOTYPE */
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKuo8f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0)
      fast_processing = 1;            

    if(fast_processing) {
#ifndef DUFF_uo8f
#if defined TEST1_uo8f
        n=num_elm;
        for(i = 0; i < n; i ++) {
            if(lptr_src[0])
                lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                    (((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 53) >> 1)) +
                    ((lptr_src[0] & UO8F_MASKC) << 5);
            else
                lptr_dest[0]=0;
            lptr_src++;
            lptr_dest++;
          } /* end for */
#else
        for(i = 0; i < num_elm; i ++) {
            if (lptr_src[0] != 0)
                lptr_dest[0] = (((lptr_src[0] & UO8F_MASKA) |
                        (((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 53) >> 1)) +
                        ((lptr_src[0] & UO8F_MASKC) << 5));
            else
                lptr_dest[0] = 0;
            lptr_src++;
            lptr_dest++;
          } /* end for */
#endif
#else   /* DUFF_uo8f */
        n=(num_elm+7)/8;
        switch(num_elm%8) {
            case 0:
                do{
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
#ifdef QAK
            case 15:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 14:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 13:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 12:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 11:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 10:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 9:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 8:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
#endif
            case 7:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 6:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 5:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 4:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 3:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 2:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 1:
                    if(lptr_src[0])
                        lptr_dest[0] = ((lptr_src[0] & UO8F_MASKA) |
                            ((((lptr_src[0] & UO8F_MASKB) >> 48) - 15362) << 52)) +
                            ((lptr_src[0] & UO8F_MASKC) << 5);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
                } while(--n>0);
		}
#endif  /* DUFF_uo8f */
      } /* end if */
    else
        for(i = 0; i < num_elm; i ++) {
            dud[0] = source[0];
            dud[1] = source[1];
            dud[2] = source[2];
            dud[3] = source[3];
            dud[4] = source[4];
            dud[5] = source[5];
            dud[6] = source[6];
            dud[7] = source[7];

            if (buf != 0) {
                buf = (((buf & UO8F_MASKA) |                             /* sign bit */
                        (((((buf & UO8F_MASKB) >> 48) - 15362) << 53) >> 1)) |   /* exp */
                        ((buf & UO8F_MASKC) << 5));                       /* mantissa */
              } /* end if */
            else
                buf = 0;

            dest[0] = dud[0];
            dest[1] = dud[1];
            dest[2] = dud[2];
            dest[3] = dud[3];
            dest[4] = dud[4];
            dest[5] = dud[5];
            dest[6] = dud[6];
            dest[7] = dud[7];

            source += source_stride;
            dest += dest_stride;
          } /* end for */
  return;
}

#define LUI2I_MASKA1 0xff00000000000000
#define LUI2I_MASKA2 0x00ff000000000000
#define LUI2I_MASKB1 0x0000ff0000000000
#define LUI2I_MASKB2 0x000000ff00000000
#define LUI2I_MASKC1 0x00000000ff000000
#define LUI2I_MASKC2 0x0000000000ff0000
#define LUI2I_MASKD1 0x000000000000ff00
#define LUI2I_MASKD2 0x00000000000000ff

/* QAK */
/************************************************************/
/* DFKlui2i()                                               */
/* -->Unicos routine for importing 2 byte data items        */ 
/* (**) This routine converts two byte little-endian IEEE   */
/*      to eight byte Cray big endian integer.              */
/************************************************************/
#ifdef PROTOTYPE
int DFKlui2i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKlui2i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  register uint32 i;
  int fast_processing=0;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
    long *lp_dest;
    long *lp_src;
  char *FUNC="DFKui2i";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);                /* No elements is an error */
    return FAIL;
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0) 
      fast_processing = 1;            

  if(fast_processing) {
#ifndef DUFF_lui2i
#if defined TEST2_lui2i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                    ((lp_src[0]&LUI2I_MASKA2)>>40);
            lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                    ((lp_src[0]&LUI2I_MASKB2)>>24);
            lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                    ((lp_src[0]&LUI2I_MASKC2)>>8);
            lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                    ((lp_src[0]&LUI2I_MASKD2)<<8);
            lp_dest+=4;
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                        ((lp_src[0]&LUI2I_MASKB2)>>24);
                lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                        ((lp_src[0]&LUI2I_MASKC2)>>8);
                break;

            case 2:
                lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                        ((lp_src[0]&LUI2I_MASKB2)>>24);
                break;

            case 1:
                lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                break;

            default:
                break;
          } /* end switch */
#elif defined TEST1_lui2i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            *lp_dest++=((lp_src[0]&LUI2I_MASKA1)>>56) |
                    ((lp_src[0]&LUI2I_MASKA2)>>40);
            *lp_dest++=((lp_src[0]&LUI2I_MASKB1)>>40) |
                    ((lp_src[0]&LUI2I_MASKB2)>>24);
            *lp_dest++=((lp_src[0]&LUI2I_MASKC1)>>24) |
                    ((lp_src[0]&LUI2I_MASKC2)>>8);
            *lp_dest++=((lp_src[0]&LUI2I_MASKD1)>>8) |
                    ((lp_src[0]&LUI2I_MASKD2)<<8);
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                *lp_dest++=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                *lp_dest++=((lp_src[0]&LUI2I_MASKB1)>>40) |
                        ((lp_src[0]&LUI2I_MASKB2)>>24);
                *lp_dest++=((lp_src[0]&LUI2I_MASKC1)>>24) |
                        ((lp_src[0]&LUI2I_MASKC2)>>8);
                break;

            case 2:
                *lp_dest++=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                *lp_dest++=((lp_src[0]&LUI2I_MASKB1)>>40) |
                        ((lp_src[0]&LUI2I_MASKB2)>>24);
                break;

            case 1:
                *lp_dest++=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                break;

            default:
                break;
          } /* end switch */
#else
    for(i = 0; i < num_elm; i++) {
      lptr_dest[0] = 0x0000000000000000;
      dest[6] = source[1];
      dest[7] = source[0];
      source += 2;
      lptr_dest++;
      dest = (uint8*)lptr_dest;
    }
#endif
#else   /* DUFF_lui2i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));

        odd_man_out = num_elm % 4;

        num_elm/=4;
        n=(num_elm+7)/8;
		if(orig_num_elm>3)
        switch(num_elm%8) {
            case 0:
                do{
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
#ifdef QAK
            case 15:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 14:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 13:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 12:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 11:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 10:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 9:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 8:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
#endif
            case 7:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 6:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 5:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 4:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 3:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 2:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 1:
                    lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                            ((lp_src[0]&LUI2I_MASKA2)>>40);
                    lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                            ((lp_src[0]&LUI2I_MASKB2)>>24);
                    lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                            ((lp_src[0]&LUI2I_MASKC2)>>8);
                    lp_dest[3]=((lp_src[0]&LUI2I_MASKD1)>>8) |
                            ((lp_src[0]&LUI2I_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
                } while(--n>0);
		}
        switch(odd_man_out) {
            case 3:
                lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                        ((lp_src[0]&LUI2I_MASKB2)>>24);
                lp_dest[2]=((lp_src[0]&LUI2I_MASKC1)>>24) |
                        ((lp_src[0]&LUI2I_MASKC2)>>8);
                break;

            case 2:
                lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                lp_dest[1]=((lp_src[0]&LUI2I_MASKB1)>>40) |
                        ((lp_src[0]&LUI2I_MASKB2)>>24);
                break;

            case 1:
                lp_dest[0]=((lp_src[0]&LUI2I_MASKA1)>>56) |
                        ((lp_src[0]&LUI2I_MASKA2)>>40);
                break;

            default:
                break;
          } /* end switch */
#endif  /* DUFF_lui2i */
  }
  else { /* Generic stride processing */
    for(i = 0; i < num_elm; i++) {
      dest[0] = 0x00;
      dest[1] = 0x00;
      dest[2] = 0x00;
      dest[3] = 0x00;
      dest[4] = 0x00;
      dest[5] = 0x00;
      dest[6] = source[1];
      dest[7] = source[0];
      source += source_stride;
      dest += dest_stride;
    }
  }
  return 0;
}

#define LUI2S_MASKA1 0xff00000000000000
#define LUI2S_MASKA2 0x00ff000000000000
#define LUI2S_MASKB1 0x0000ff0000000000
#define LUI2S_MASKB2 0x000000ff00000000
#define LUI2S_MASKC1 0x00000000ff000000
#define LUI2S_MASKC2 0x0000000000ff0000
#define LUI2S_MASKD1 0x000000000000ff00
#define LUI2S_MASKD2 0x00000000000000ff
#define LUI2S_MASKE  0x0080000000000000
#define LUI2S_MASKF  0x0000008000000000
#define LUI2S_MASKG  0x0000000000800000
#define LUI2S_MASKH  0x0000000000000080
#define LUI2S_MASKI  0xffffffffffff0000

/************************************************************/
/* DFKlui2s()                                                */
/* -->Unicos routine for importing 2 byte signed ints       */
/* (**) This routine converts two byte IEEE to eight byte   */
/*      Cray.                                               */
/************************************************************/
#ifdef PROTOTYPE
int DFKlui2s(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKlui2s(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  register uint32 i;
  int fast_processing=0;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
    long *lp_dest;
    long *lp_src;
  char *FUNC="DFKui2s";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0) 
      fast_processing = 1;            

  if(fast_processing) {
#ifndef DUFF_lui2s
#if defined TEST2_lui2s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                    ((lp_src[0]&LUI2S_MASKA2)>>40));
            lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                    ((lp_src[0]&LUI2S_MASKA2)>>40);
            if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                    ((lp_src[0]&LUI2S_MASKB2)>>24));
            lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                    ((lp_src[0]&LUI2S_MASKB2)>>24);
            if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                    ((lp_src[0]&LUI2S_MASKC2)>>8));
            lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                    ((lp_src[0]&LUI2S_MASKC2)>>8);
            if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                    ((lp_src[0]&LUI2S_MASKC2)<<8));
            lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                    ((lp_src[0]&LUI2S_MASKD2)<<8);
            lp_dest+=4;
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                    lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                        ((lp_src[0]&LUI2S_MASKC2)>>8));
                lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                        ((lp_src[0]&LUI2S_MASKC2)>>8);
                /* falls through */

            case 2:
                if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                    lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                        ((lp_src[0]&LUI2S_MASKB2)>>24));
                lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                        ((lp_src[0]&LUI2S_MASKB2)>>24);
                /* falls through */

            case 1:
                if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                    lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40));
                lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40);
                break;

            default:
                break;
          } /* end switch */
#elif defined TEST1_lui2s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm%4;

        n=num_elm/4;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));
        for(i = 0; i < n; i++) {
            if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=((lp_src[0]&LUI2S_MASKA1)>>56) |
                    ((lp_src[0]&LUI2S_MASKA2)>>40);
            if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=((lp_src[0]&LUI2S_MASKB1)>>40) |
                    ((lp_src[0]&LUI2S_MASKB2)>>24);
            if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=((lp_src[0]&LUI2S_MASKC1)>>24) |
                    ((lp_src[0]&LUI2S_MASKC2)>>8);
            if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                *lp_dest = 0xffffffffffffffff;
            *lp_dest++=((lp_src[0]&LUI2S_MASKD1)>>8) |
                    ((lp_src[0]&LUI2S_MASKD2)<<8);
            lp_src++;
          } /* end for */
        switch(odd_man_out) {
            case 3:
                if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40);
                if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=((lp_src[0]&LUI2S_MASKB1)>>40) |
                        ((lp_src[0]&LUI2S_MASKB2)>>24);
                if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=((lp_src[0]&LUI2S_MASKC1)>>24) |
                        ((lp_src[0]&LUI2S_MASKC2)>>8);
                break;

            case 2:
                if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40);
                if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=((lp_src[0]&LUI2S_MASKB1)>>40) |
                        ((lp_src[0]&LUI2S_MASKB2)>>24);
                break;

            case 1:
                if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                    *lp_dest = 0xffffffffffffffff;
                *lp_dest++=((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40);
                break;

            default:
                break;
          } /* end switch */
#else
    for(i = 0; i < num_elm; i++) {
      if((source[1] & 0x80))           /* Can't forget to extend sign */
	lptr_dest[0] = 0xffffffffffffffff;
      else
	lptr_dest[0] = 0x0000000000000000;
      dest[6] = source[1];
      dest[7] = source[0];
      source += 2;
      lptr_dest++;
      dest = (uint8*)lptr_dest;
    }
#endif
#else   /* DUFF_lui2s */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long));

        odd_man_out = num_elm % 4;

        num_elm/=4;
        n=(num_elm+7)/8;
		if(orig_num_elm>3)
        switch(num_elm%8) {
            case 0:
                do{
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
#ifdef QAK
            case 15:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 14:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 13:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 12:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 11:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 10:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 9:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 8:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
#endif
            case 7:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 6:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 5:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 4:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 3:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 2:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
            case 1:
                    if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                        lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40));
                    lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                            ((lp_src[0]&LUI2S_MASKA2)>>40);
                    if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                        lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24));
                    lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                            ((lp_src[0]&LUI2S_MASKB2)>>24);
                    if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                        lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8));
                    lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                            ((lp_src[0]&LUI2S_MASKC2)>>8);
                    if(lp_src[0] & LUI2S_MASKH)     /* Can't forget to extend sign */
                        lp_dest[3] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKC2)<<8));
                    lp_dest[3]|=((lp_src[0]&LUI2S_MASKD1)>>8) |
                            ((lp_src[0]&LUI2S_MASKD2)<<8);
                    lp_dest+=4;
                    lp_src++;
                } while(--n>0);
		}
        switch(odd_man_out) {
            case 3:
                if(lp_src[0] & LUI2S_MASKG)     /* Can't forget to extend sign */
                    lp_dest[2] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKC1)>>24) |
                        ((lp_src[0]&LUI2S_MASKC2)>>8));
                lp_dest[2]|=((lp_src[0]&LUI2S_MASKC1)>>24) |
                        ((lp_src[0]&LUI2S_MASKC2)>>8);
                /* falls through */

            case 2:
                if(lp_src[0] & LUI2S_MASKF)     /* Can't forget to extend sign */
                    lp_dest[1] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKB1)>>40) |
                        ((lp_src[0]&LUI2S_MASKB2)>>24));
                lp_dest[1]|=((lp_src[0]&LUI2S_MASKB1)>>40) |
                        ((lp_src[0]&LUI2S_MASKB2)>>24);
                /* falls through */

            case 1:
                if(lp_src[0] & LUI2S_MASKE)     /* Can't forget to extend sign */
                    lp_dest[0] = LUI2S_MASKI|(((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40));
                lp_dest[0]|=((lp_src[0]&LUI2S_MASKA1)>>56) |
                        ((lp_src[0]&LUI2S_MASKA2)>>40);
                break;

            default:
                break;
          } /* end switch */
#endif  /* DUFF_lui2s */
  }
  else { /* Generic stride processing */
    for(i = 0; i < num_elm; i++) {
      if((source[1] & 0x80)) {          /* Can't forget to extend sign */
	dest[0] = 0xff;
	dest[1] = 0xff;
	dest[2] = 0xff;
	dest[3] = 0xff;
	dest[4] = 0xff;
	dest[5] = 0xff;
      }
      else {
	dest[0] = 0x00;
	dest[1] = 0x00;
	dest[2] = 0x00;
	dest[3] = 0x00;
	dest[4] = 0x00;
	dest[5] = 0x00;
      }
      dest[6] = source[1];
      dest[7] = source[0];
      source += source_stride;
      dest += dest_stride;
    }
  }
  return 0;
}

#define LUO2I_MASKA 0x00000000000000ff
#define LUO2I_MASKB 0x000000000000ff00

/************************************************************/
/* DFKluo2i()                                               */
/* -->Unicos routine for exporting 2 byte little-endian     */
/*      data items                                          */
/************************************************************/
#ifdef PROTOTYPE
int DFKluo2i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKluo2i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  register uint32 i;
  int fast_processing=0;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
    long *lp_dest;
    long *lp_src;
    char *FUNC="DFKuo2i";

    HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0)
      fast_processing = 1;            

  if(fast_processing) {
#ifndef DUFF_luo2i
#if defined TEST1_luo2i
    int odd_man_out;        /* By default there are even num_elm */
    intn n;

    odd_man_out = num_elm%4;

    n=num_elm/4;
    lp_dest=(long *)dest;
    lp_src=(long *)source;
    for(i = 0; i < n; i++) {
        *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                    ((lp_src[0]&LUO2I_MASKB)<<40) |
                    ((lp_src[1]&LUO2I_MASKA)<<40) |
                    ((lp_src[1]&LUO2I_MASKB)<<24) |
                    ((lp_src[2]&LUO2I_MASKA)<<24) |
                    ((lp_src[2]&LUO2I_MASKB)<<8) |
                    ((lp_src[3]&LUO2I_MASKA)<<8) |
                    ((lp_src[3]&LUO2I_MASKB)>>8);
        lp_src+=4;
    }
    switch(odd_man_out) {   /* clean up leftovers */
        case 3:
            *lp_dest=((lp_src[0]&LUO2I_MASKA)<<56) |
                        ((lp_src[0]&LUO2I_MASKB)<<40) |
                        ((lp_src[1]&LUO2I_MASKA)<<40) |
                        ((lp_src[1]&LUO2I_MASKB)<<24) |
                        ((lp_src[2]&LUO2I_MASKA)<<24) |
                        ((lp_src[2]&LUO2I_MASKB)<<8);
            break;

        case 2:
            *lp_dest=((lp_src[0]&LUO2I_MASKA)<<56) |
                        ((lp_src[0]&LUO2I_MASKB)<<40) |
                        ((lp_src[1]&LUO2I_MASKA)<<40) |
                        ((lp_src[1]&LUO2I_MASKB)<<24);
            break;

        case 1:
            *lp_dest=((lp_src[0]&LUO2I_MASKA)<<56) |
                        ((lp_src[0]&LUO2I_MASKB)<<40);
            break;

        case 0:
            break;
      } /* end switch */
#else
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[7];
      dest[1] = source[6];
      dest += 2;
      source += 8;
    }
#endif
#else   /* DUFF_luo2i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm % 4;

        num_elm/=4;
        n=(num_elm+7)/8;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
		if(orig_num_elm>3)
        switch(num_elm%8) {
            case 0:
                do{
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
#ifdef QAK
            case 15:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 14:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 13:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 12:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 11:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 10:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 9:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 8:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
#endif
            case 7:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 6:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 5:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 4:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 3:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 2:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
            case 1:
                    *lp_dest++=((lp_src[0]&LUO2I_MASKA)<<56) |
                                ((lp_src[0]&LUO2I_MASKB)<<40) |
                                ((lp_src[1]&LUO2I_MASKA)<<40) |
                                ((lp_src[1]&LUO2I_MASKB)<<24) |
                                ((lp_src[2]&LUO2I_MASKA)<<24) |
                                ((lp_src[2]&LUO2I_MASKB)<<8) |
                                ((lp_src[3]&LUO2I_MASKA)<<8) |
                                ((lp_src[3]&LUO2I_MASKB)>>8);
                    lp_src+=4;
                } while(--n>0);
		}
        switch(odd_man_out) {   /* clean up leftovers */
            case 3:
                *lp_dest=((lp_src[0]&LUO2I_MASKA)<<56) |
                            ((lp_src[0]&LUO2I_MASKB)<<40) |
                            ((lp_src[1]&LUO2I_MASKA)<<40) |
                            ((lp_src[1]&LUO2I_MASKB)<<24) |
                            ((lp_src[2]&LUO2I_MASKA)<<24) |
                            ((lp_src[2]&LUO2I_MASKB)<<8);
                break;

            case 2:
                *lp_dest=((lp_src[0]&LUO2I_MASKA)<<56) |
                            ((lp_src[0]&LUO2I_MASKB)<<40) |
                            ((lp_src[1]&LUO2I_MASKA)<<40) |
                            ((lp_src[1]&LUO2I_MASKB)<<24);
                break;

            case 1:
                *lp_dest=((lp_src[0]&LUO2I_MASKA)<<56) |
                            ((lp_src[0]&LUO2I_MASKB)<<40);
                break;

            default:
                break;
          } /* end switch */
#endif  /* DUFF_luo2i */
  }
  else { /* Generic Stride processing */
    for(i = 0; i < num_elm; i++){
      dest[0] = source[7];
      dest[1] = source[6];
      source += source_stride;
      dest += dest_stride;
    }
  }
  return 0;
}

#define LUI4I_MASKA 0xff00000000000000
#define LUI4I_MASKB 0x00ff000000000000
#define LUI4I_MASKC 0x0000ff0000000000
#define LUI4I_MASKD 0x000000ff00000000
#define LUI4I_MASKE 0x00000000ff000000
#define LUI4I_MASKF 0x0000000000ff0000
#define LUI4I_MASKG 0x000000000000ff00
#define LUI4I_MASKH 0x00000000000000ff

/************************************************************/
/* DFKlui4i()                                               */
/* -->Unicos routine for importing 4 byte little-endian     */
/*      unsigned ints                                       */
/************************************************************/
#ifdef PROTOTYPE
int DFKlui4i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKlui4i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing=0;
  register uint32 i;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
  long *lp_dest;
  long *lp_src;
  char *FUNC="DFKui4i";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  if(source_stride == 0 && dest_stride == 0) {
    fast_processing = 1;
  }
  
    if(fast_processing) {
#ifndef DUFF_lui4i
#if defined TEST2_lui4i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                        (lp_src[0]&LUI4I_MASKB)>>40 |
                        (lp_src[0]&LUI4I_MASKC)>>24 |
                        (lp_src[0]&LUI4I_MASKD)>>8;
            lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                        (lp_src[0]&LUI4I_MASKF)>>8 |
                        (lp_src[0]&LUI4I_MASKG)<<8 |
                        (lp_src[0]&LUI4I_MASKH)<<24;
            lp_dest+=2;
            lp_src++;
          } /* end for */
        if(odd_man_out)
            *lp_dest=(lp_src[0]&LUI4I_MASKA)>>56 |
                        (lp_src[0]&LUI4I_MASKB)>>40 |
                        (lp_src[0]&LUI4I_MASKC)>>24 |
                        (lp_src[0]&LUI4I_MASKD)>>8;
#elif defined TEST1_lui4i
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            *lp_dest++=(lp_src[0]&LUI4I_MASKA)>>56 |
                        (lp_src[0]&LUI4I_MASKB)>>40 |
                        (lp_src[0]&LUI4I_MASKC)>>24 |
                        (lp_src[0]&LUI4I_MASKD)>>8;
            *lp_dest++=(lp_src[0]&LUI4I_MASKE)>>24 |
                        (lp_src[0]&LUI4I_MASKF)>>8 |
                        (lp_src[0]&LUI4I_MASKG)<<8 |
                        (lp_src[0]&LUI4I_MASKH)<<24;
            lp_src++;
          } /* end for */
        if(odd_man_out)
            *lp_dest++=(lp_src[0]&LUI4I_MASKA)>>56 |
                        (lp_src[0]&LUI4I_MASKB)>>40 |
                        (lp_src[0]&LUI4I_MASKC)>>24 |
                        (lp_src[0]&LUI4I_MASKD)>>8;
#else
    for(i = 0; i < num_elm; i++) {
      lptr_dest[0] = 0;
      dest[4] = source[3];
      dest[5] = source[2];
      dest[6] = source[1];
      dest[7] = source[0];
      source += 4;
      lptr_dest ++;
      dest = (uint8 *)lptr_dest;
    }
#endif
#else   /* DUFF_lui4i */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
#ifdef QAK
            case 15:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 14:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 13:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 12:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 11:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 10:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 9:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 8:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
#endif
            case 7:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 6:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 5:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 4:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 3:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 2:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 1:
                    lp_dest[0]=(lp_src[0]&LUI4I_MASKA)>>56 |
                                (lp_src[0]&LUI4I_MASKB)>>40 |
                                (lp_src[0]&LUI4I_MASKC)>>24 |
                                (lp_src[0]&LUI4I_MASKD)>>8;
                    lp_dest[1]=(lp_src[0]&LUI4I_MASKE)>>24 |
                                (lp_src[0]&LUI4I_MASKF)>>8 |
                                (lp_src[0]&LUI4I_MASKG)<<8 |
                                (lp_src[0]&LUI4I_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
                } while(--n>0);
		}
        if(odd_man_out)
            *lp_dest=(lp_src[0]&LUI4I_MASKA)>>56 |
                        (lp_src[0]&LUI4I_MASKB)>>40 |
                        (lp_src[0]&LUI4I_MASKC)>>24 |
                        (lp_src[0]&LUI4I_MASKD)>>8;
#endif  /* DUFF_lui4i */
      } /* end if */
  else 
    for(i = 0; i < num_elm; i++) {
      dest[0] = 0;
      dest[1] = 0;
      dest[2] = 0;
      dest[3] = 0;
      dest[4] = source[3];
      dest[5] = source[2];
      dest[6] = source[1];
      dest[7] = source[0];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

#define LUI4S_MASKA 0xff00000000000000
#define LUI4S_MASKB 0x00ff000000000000
#define LUI4S_MASKC 0x0000ff0000000000
#define LUI4S_MASKD 0x000000ff00000000
#define LUI4S_MASKE 0x00000000ff000000
#define LUI4S_MASKF 0x0000000000ff0000
#define LUI4S_MASKG 0x000000000000ff00
#define LUI4S_MASKH 0x00000000000000ff
#define LUI4S_MASKI 0x0000008000000000
#define LUI4S_MASKJ 0x0000000000000080
#define LUI4S_MASKK 0xffffffff00000000

/************************************************************/
/* DFKlui4s()                                                */
/* -->Unicos routine for importing 4 signed ints            */ 
/************************************************************/
#ifdef PROTOTYPE
int DFKlui4s(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKlui4s(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing=0;
  register uint32 i;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  long * lptr_dest = (long*)dest;
  long *lp_dest;
  long *lp_src;
  char *FUNC="DFKui4s";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  if(source_stride == 0 && dest_stride == 0) {
    fast_processing = 1;
  }
  
  if(fast_processing) {
#ifndef DUFF_lui4s
#if defined TEST2_lui4s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8);
            lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8;
            if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                        (lp_src[0]&LUI4S_MASKF)>>8 |
                        (lp_src[0]&LUI4S_MASKG)<<8 |
                        (lp_src[0]&LUI4S_MASKH)<<24);
            lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                        (lp_src[0]&LUI4S_MASKF)>>8 |
                        (lp_src[0]&LUI4S_MASKG)<<8 |
                        (lp_src[0]&LUI4S_MASKH)<<24;
            lp_dest+=2;
            lp_src++;
          } /* end for */
        if(odd_man_out) {
            if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8);
            lp_dest[0]=(lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8;
          } /* end if */
#elif defined TEST1_lui4s
        int odd_man_out;        /* By default there are even num_elm */
        intn n;

        odd_man_out = num_elm % 2;

        n=num_elm/2;
        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */
        for(i = 0; i < n; i++) {
            if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                lp_dest[0] = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8;
            if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                lp_dest[0] = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&LUI4S_MASKE)>>24 |
                        (lp_src[0]&LUI4S_MASKF)>>8 |
                        (lp_src[0]&LUI4S_MASKG)<<8 |
                        (lp_src[0]&LUI4S_MASKH)<<24;
            lp_src++;
          } /* end for */
        if(odd_man_out) {
            if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                lp_dest[0] = 0xffffffffffffffff;
            *lp_dest++=(lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8;
          } /* end if */
#else
    for(i = 0; i < num_elm; i++) {
      if((source[3] & 0x80))            /* Can't forget to sign extend */
	lptr_dest[0] = 0xffffffffffffffff;
      else
	lptr_dest[0] = 0x0000000000000000;
      dest[4] = source[3];
      dest[5] = source[2];
      dest[6] = source[1];
      dest[7] = source[0];
      source += 4;
      lptr_dest ++;
      dest = (uint8 *)lptr_dest;
    }
#endif
#else   /* DUFF_lui4s */
        uintn n;
        int odd_man_out;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        lp_dest=(long *)dest;
        lp_src=(long *)source;
        HDmemset(lp_dest,0,num_elm*sizeof(long)); /* initialize to zeros */

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
#ifdef QAK
            case 15:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 14:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 13:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 12:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 11:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 10:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 9:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 8:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
#endif
            case 7:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 6:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 5:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 4:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 3:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 2:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
            case 1:
                    if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                        lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8);
                    lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                                (lp_src[0]&LUI4S_MASKB)>>40 |
                                (lp_src[0]&LUI4S_MASKC)>>24 |
                                (lp_src[0]&LUI4S_MASKD)>>8;
                    if(lp_src[0] & LUI4S_MASKJ)     /* Can't forget to sign extend */
                        lp_dest[1] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24);
                    lp_dest[1]|=(lp_src[0]&LUI4S_MASKE)>>24 |
                                (lp_src[0]&LUI4S_MASKF)>>8 |
                                (lp_src[0]&LUI4S_MASKG)<<8 |
                                (lp_src[0]&LUI4S_MASKH)<<24;
                    lp_dest+=2;
                    lp_src++;
                } while(--n>0);
		}
        if(odd_man_out) {
            if(lp_src[0] & LUI4S_MASKI)     /* Can't forget to sign extend */
                lp_dest[0] = LUI4S_MASKK|((lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8);
            lp_dest[0]|=(lp_src[0]&LUI4S_MASKA)>>56 |
                        (lp_src[0]&LUI4S_MASKB)>>40 |
                        (lp_src[0]&LUI4S_MASKC)>>24 |
                        (lp_src[0]&LUI4S_MASKD)>>8;
          } /* end if */
#endif  /* DUFF_lui4s */
      } /* end if */
  else 
    for(i = 0; i < num_elm; i++) {
      if((source[3] & 0x80)) {          /* Can't forget to sign extend */
	dest[0] = 0xff;
	dest[1] = 0xff;
	dest[2] = 0xff;
	dest[3] = 0xff;
      }
      else {
	dest[0] = 0;
	dest[1] = 0;
	dest[2] = 0;
	dest[3] = 0;
      }
      dest[4] = source[3];
      dest[5] = source[2];
      dest[6] = source[1];
      dest[7] = source[0];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

#define LUO4I_MASKA 0x00000000ff00ff00
#define LUO4I_MASKB 0x0000000000ff00ff
#define LUO4I_MASKC 0xffff0000ffff0000
#define LUO4I_MASKD 0x0000ffff0000ffff

/************************************************************/
/* DFKluo4i()                                                */
/* -->Unicos routine for exporting 4 byte data items        */ 
/************************************************************/

#ifdef PROTOTYPE
int DFKluo4i(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKluo4i(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing=0;
  register uintn i;
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  unsigned long *lp_dest;
  unsigned long *lp_src;
  char *FUNC="DFKuo4i";

  HEclear();

  if(source == dest || num_elm == 0) {  /* Inplace conversions  not permitted */
    HERROR(DFE_BADCONV);                /* No elements to convert is an error */
    return FAIL;
  }

  if(source_stride == 0 && dest_stride == 0) {
    fast_processing = 1;
  }
  
  if(fast_processing) {
#ifndef DUFF_luo4i
#if defined TEST2_luo4i
    int odd_man_out = 0;        /* By default there are even num_elm */
    intn n;

    if(num_elm % 2)             /* If this is true, we have odd num */
      odd_man_out = 1;

    n=num_elm/2;
    lp_dest=(long *)dest;
    lp_src=(long *)source;
    for(i = 0; i < n; i++) {
        *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) | ((lp_src[0]&LUO4I_MASKB)<<40) |
                   ((lp_src[1]&LUO4I_MASKA)>>8) | ((lp_src[1]&LUO4I_MASKB)<<8);
        *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) | ((*lp_dest&LUO4I_MASKD)<<16);
		lp_dest++;
        lp_src+=2;
    }
    if(odd_man_out) {
        *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) | ((lp_src[0]&LUO4I_MASKB)<<40);
        *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) | ((*lp_dest&LUO4I_MASKD)<<16);
      } /* end if */
#else
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[7];
      dest[1] = source[6];
      dest[2] = source[5];
      dest[3] = source[4];
      dest += 4;
      source += 8;
    }
#endif
#else   /* DUFF_luo4i */
        register uintn n;
        int odd_man_out = 0;        /* By default there are even num_elm */
		uintn orig_num_elm=num_elm;

        if(num_elm % 2)             /* If this is true, we have odd num */
          odd_man_out = 1;

        num_elm/=2;
        n=(num_elm+7)/8;
        lp_dest=(unsigned long *)dest;
        lp_src=(unsigned long *)source;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
#ifdef QAK
            case 15:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 14:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 13:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 12:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 11:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 10:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 9:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 8:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
#endif
            case 7:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 6:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 5:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 4:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 3:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 2:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
            case 1:
                    *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                                ((lp_src[0]&LUO4I_MASKB)<<40) |
                                ((lp_src[1]&LUO4I_MASKA)>>8) |
                                ((lp_src[1]&LUO4I_MASKB)<<8);
                    *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                                ((*lp_dest&LUO4I_MASKD)<<16);
					lp_dest++;
                    lp_src+=2;
                } while(--n>0);
		}
        if(odd_man_out) {
            *lp_dest=((lp_src[0]&LUO4I_MASKA)<<24) |
                    ((lp_src[0]&LUO4I_MASKB)<<40);
            *lp_dest=((*lp_dest&LUO4I_MASKC)>>16) |
                    ((*lp_dest&LUO4I_MASKD)<<16);
          } /* end if */
#endif  /* DUFF_luo4i */
  }
  else 
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[7];
      dest[1] = source[6];
      dest[2] = source[5];
      dest[3] = source[4];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

#define LUI4F_MASKA  0x0000008000000000
#define LUI4F_MASKB1 0x0000007f00000000
#define LUI4F_MASKB2 0x0000800000000000
#define LUI4F_MASKC1 0x00007f0000000000
#define LUI4F_MASKC2 0x00ff000000000000
#define LUI4F_MASKC3 0xff00000000000000
#define LUI4F_MASKD  0x0000800000000000

#define LUI4F_MASKE  0x0000000000000080
#define LUI4F_MASKF1 0x000000000000007f
#define LUI4F_MASKF2 0x0000000000008000
#define LUI4F_MASKG1 0x0000000000007f00
#define LUI4F_MASKG2 0x0000000000ff0000
#define LUI4F_MASKG3 0x00000000ff000000

#define LUI4F_MASKH  0xffffff7f00000000
#define LUI4F_MASKI  0x00000000ffffff7f

/***************************************************************/
/* DFKlui4f()                                                  */
/* -->Unicos routine for importing little-endian 32 bit floats */
/***************************************************************/

/************************************************************

                     <<<< WARNING >>>>

    The nature of converting between 64 bit floating point
  numbers and 32 bit floating point numbers LOSES PRECISION.
  Taking a number in 64bit cray format, converting to IEEE
  (internal HDF format) and back will round the number at
  about the 7th decimal place.

 ************************************************************/

#ifdef PROTOTYPE
int DFKlui4f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKlui4f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source;
uint8 * dest;
uint32 num_elm;
uint32 source_stride;
uint32 dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int odd_man_out = 0;                  /* By default there are even num_elm */
  int i,j,n;
  long buf1;                            /* This is a temporary stride buf */
  long buf2;                            /* This is a temporary stride buf */
  uint8 * dud1 = (uint8*)&buf1;         /* Dummy pointer to buf1 for strides */
  uint8 * dud2 = (uint8*)&buf2;         /* Dummy pointer to buf2 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;            /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;              /* void and void* constructions */
#endif /* PROTOTYPE */
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKui4f";

    HEclear();

    /* Check for conversion errors */
    if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
        HERROR(DFE_BADCONV);             /* under UNICOS */
        return FAIL;                     /* No elements convert is an error   */
    }

    /* Find out if it is OK to use faster array processing */
    if(source_stride == 0 && dest_stride == 0)
        fast_processing = 1;

    if(fast_processing) {
#ifndef DUFF_lui4f
#if defined TEST1_lui4f
        odd_man_out = num_elm%2;
        n = num_elm / 2;
        for(i = 0; i < n; i++) {
            if(*lptr_src & LUI4F_MASKH)
                lptr_dest[0] = (((*lptr_src & LUI4F_MASKA)<<24) |/* Sign bit */
                    (((*lptr_src & LUI4F_MASKB1) << 17) |       /* Exp. */
                    ((*lptr_src & LUI4F_MASKB2) << 1)) +
                    (16258 << 48)) |
                    (((*lptr_src & LUI4F_MASKC1) |              /* Mantissa */
                    ((*lptr_src & LUI4F_MASKC2) >>16) |
                    ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
            else
                lptr_dest[0]=0;
            if(*lptr_src & LUI4F_MASKI)
                lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                    (((*lptr_src & LUI4F_MASKF1) << 49) |       /* Exp. */
                    ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                    (16258 << 48)) |
                    ((((*lptr_src & LUI4F_MASKG1) <<32) |       /* Mantissa */
                    ((*lptr_src & LUI4F_MASKG2) <<16)|
                    (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
            else
                lptr_dest[1]=0;
            lptr_src++;
            lptr_dest+=2;
          } /* end for */
        if(odd_man_out)
            if(*lptr_src & LUI4F_MASKH)
                lptr_dest[0] =(((lptr_src[0] & LUI4F_MASKA)<<24) |/* Sign bit */
                    (((lptr_src[0] & LUI4F_MASKB1) << 17) |  /* Exp. */
                    ((lptr_src[0] & LUI4F_MASKB2) << 1)) +
                    (16258 << 48)) |
                    (((lptr_src[0] & LUI4F_MASKC1) |         /* Mantissa */
                    ((lptr_src[0] & LUI4F_MASKC2) >>16) |
                    ((lptr_src[0] & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
            else
                lptr_dest[0]=0;
#else
        if((num_elm % 2))           /* If this is true, we have odd num */
            odd_man_out = 1;
        num_elm = num_elm / 2;
        for(i = 0; i < num_elm; i++) {
            if(*(float*)lptr_src != 0) {
                if(((*lptr_src & 0xffffffff00000000) << 1)== 0)
                    *lptr_dest = 0;
                else 
                    *lptr_dest = (((*lptr_src & LUI4F_MASKA)<<24) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |  /* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |         /* Mantissa */
                             ((*lptr_src & LUI4F_MASKC2) >>16) |
                             ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
            }
            else
                *lptr_dest = *lptr_src;
            lptr_dest++;
            if(*(float*)lptr_src != 0) {
                if(((*lptr_src & 0x00000000ffffffff) << 33)== 0)
                    *lptr_dest = 0;
                else
                    *lptr_dest = ((((*lptr_src << 32) & LUI4F_MASKA)<<24) |/* Sign bit */
                            ((((*lptr_src << 32) & LUI4F_MASKB1) << 17) |  /* Exp. */
                            (((*lptr_src <<32 ) & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            ((((*lptr_src << 32) & LUI4F_MASKC1) |         /* Mantissa */
                             (((*lptr_src << 32) & LUI4F_MASKC2) >>16) |
                             (((*lptr_src << 32) & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
            }
            else
                *lptr_dest = *lptr_src;
            lptr_src++;
            lptr_dest++;
        }
        if(odd_man_out) {
            if(((float*)lptr_src)[0] != 0) {
                if(((lptr_src[0] & 0xffffffff00000000) << 1)== 0)
                    lptr_dest[0] = 0;
                else
                    lptr_dest[0] = (((lptr_src[0] & LUI4F_MASKA)<<24) |/* Sign bit */
                            (((lptr_src[0] & LUI4F_MASKB1) << 17) |  /* Exp. */
                            ((lptr_src[0] & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((lptr_src[0] & LUI4F_MASKC1) |         /* Mantissa */
                             ((lptr_src[0] & LUI4F_MASKC2) >>16) |
                             ((lptr_src[0] & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
            }
            else
                *lptr_dest = *lptr_src;
        }
#endif
#else   /* DUFF_lui4f */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
#ifdef QAK
            case 15:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 14:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 13:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 12:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 11:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 10:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 9:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 8:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
#endif
            case 7:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 6:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 5:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 4:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 3:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 2:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
            case 1:
                    if(*lptr_src & LUI4F_MASKH)
                        lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                            (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                            ((*lptr_src & LUI4F_MASKB2) << 1)) +
                            (16258 << 48)) |
                            (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                            ((*lptr_src & LUI4F_MASKC2) >>16) |
                            ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
                    else
                        lptr_dest[0]=0;
                    if(*lptr_src & LUI4F_MASKI)
                        lptr_dest[1] = (((*lptr_src & LUI4F_MASKE) <<56) |/* Sign bit */
                            (((*lptr_src & LUI4F_MASKF1) << 49) |/* Exp. */
                            ((*lptr_src  & LUI4F_MASKF2) << 33)) +
                            (16258 << 48)) |
                            ((((*lptr_src & LUI4F_MASKG1) <<32) |/* Mantissa */
                            ((*lptr_src & LUI4F_MASKG2) <<16)|
                            (*lptr_src & LUI4F_MASKG3)) | LUI4F_MASKD);
                    else
                        lptr_dest[1]=0;
                    lptr_src++;
                    lptr_dest+=2;
                } while(--n>0);
		}
        if(odd_man_out)
            if(*lptr_src & LUI4F_MASKH)
                lptr_dest[0]=(((*lptr_src & LUI4F_MASKA)<<24) | /* Sign bit */
                    (((*lptr_src & LUI4F_MASKB1) << 17) |/* Exp. */
                    ((*lptr_src & LUI4F_MASKB2) << 1)) +
                    (16258 << 48)) |
                    (((*lptr_src & LUI4F_MASKC1) |       /* Mantissa */
                    ((*lptr_src & LUI4F_MASKC2) >>16) |
                    ((*lptr_src & LUI4F_MASKC3) >>32)) | LUI4F_MASKD);
            else
                lptr_dest[0]=0;
#endif  /* DUFF_lui4f */
    }
    else { /* We end up here if we are doing stride based processing */
        buf1 = 0;
        for(i = 0; i < num_elm; i++) {
            dud1[0] = source[3];        /* Loop would be less efficient */
            dud1[1] = source[2];
            dud1[2] = source[1];
            dud1[3] = source[0];

            if((float)buf1 != 0) {
                buf2 = (((buf1 & UI4F_MASKA) |
                        ((buf1 & UI4F_MASKB) >> 7) +
                        (16258 << 48)) |
                        (((buf1 & UI4F_MASKC) >> 8) | (UI4F_MASKD)));
                if((buf1 << 1)== 0)
                    buf2 = 0;
            }
            else
                buf2 = buf1;

            dest[0] = dud2[0];            /* Loop would be less efficient */
            dest[1] = dud2[1];
            dest[2] = dud2[2];
            dest[3] = dud2[3];
            dest[4] = dud2[4];
            dest[5] = dud2[5];
            dest[6] = dud2[6];
            dest[7] = dud2[7];

            source += source_stride;
            dest += dest_stride;
        }
    }
  return;
}


#define LUO4F_MASKA 0x8000000000000000
#define LUO4F_MASKB 0x7fff000000000000
#define LUO4F_MASKC 0x00007fffff000000
#define LUO4F_MASKD 0x0000000000800000
#define LUO4F_MASKE 0xff00ff00ff00ff00
#define LUO4F_MASKF 0x00ff00ff00ff00ff
#define LUO4F_MASKG 0xffff0000ffff0000
#define LUO4F_MASKH 0x0000ffff0000ffff

/***************************************************************/
/* DFKluo4f()                                                  */
/* -->Unicos routine for exporting little-endian 32 bit floats */
/***************************************************************/

/************************************************************

                     <<<< WARNING >>>>

    The nature of converting between 64 bit floating point
  numbers and 32 bit floating point numbers LOSES PRECISION.
  Taking a number in 64bit cray format, converting to IEEE
  (internal HDF format) and back will round the number at
  about the 7th decimal place.

 ************************************************************/

#ifdef PROTOTYPE
int DFKluo4f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKluo4f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int odd_man_out = 0;                  /* By default there are even num_elm */
  int i,j,n;
  long buf1;                            /* This is a temporary stride buf */
  long buf2;                            /* This is a temporary stride buf */
  uint8 * dud1 = (uint8*)&buf1;         /* Dummy pointer to buf1 for strides */
  uint8 * dud2 = (uint8*)&buf2;         /* Dummy pointer to buf2 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;           /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;             /* void and void* constructions */
#endif /* PROTOTYPE */
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKuo4f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
  if(source_stride == 0 && dest_stride == 0)
      fast_processing = 1;            

    if(fast_processing) {
#ifndef DUFF_luo4f
#if defined TEST1_luo4f
        odd_man_out = num_elm%2;
        n=num_elm/2;
        for(i = 0; i < n; i++) {
            if(lptr_src[0] && lptr_src[1])  /* both not zero */
                buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                    ((((lptr_src[0] & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[0] & LUO4F_MASKC) +
                    ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                    ((((lptr_src[1] & LUO4F_MASKA) |
                    ((((lptr_src[1] & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[1] & LUO4F_MASKC) +
                    ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
            else if(lptr_src[0])    /* src[0] not zero */
                buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                    ((((lptr_src[0] & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[0] & LUO4F_MASKC) +
                    ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
            else if(lptr_src[1])    /* src[1] not zero */
                buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                    ((((lptr_src[1] & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[1] & LUO4F_MASKC) +
                    ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
            else                    /* both zero */
                buf1=0;
            buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
            lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
            lptr_src+=2;
            lptr_dest ++;
        }
        if(odd_man_out) {
            if(lptr_src[0])    /* src[0] not zero */
                buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                    ((((lptr_src[0] & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                    (((lptr_src[0] & LUO4F_MASKC) +
                    ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
            else                    /* both zero */
                buf1=0;
            buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
            lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
        }
#else
        if((num_elm % 2))                   /* If this is true, we have odd num */
          odd_man_out = 1;
        for(i = 0; i < (int)(num_elm/2); i++) {
            buf1 = lptr_src[0];
            buf2 = lptr_src[1];
            if(buf1 != 0)
                buf1 = (((buf1 & LUO4F_MASKA) |
                        ((((buf1 & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                        (((buf1 & LUO4F_MASKC) +
                        ((buf1 & LUO4F_MASKD) << 1)) << 8));
            else
                buf1 = 0;
            if(buf2 != 0)
                buf2 = ((((buf2 & LUO4F_MASKA) |
                        ((((buf2 & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                        (((buf2 & LUO4F_MASKC) +
                        ((buf2 & LUO4F_MASKD) << 1)) << 8)) >> 32);
            else
                buf2 = 0;
            lptr_dest[0] = (((buf1 & 0xff00000000000000) >>24) |
                            ((buf1 & 0x00ff000000000000) >>8) |
                            ((buf1 & 0x0000ff0000000000) <<8) |
                            ((buf1 & 0x000000ff00000000) <<24) ) |
                           (((buf2 & 0x00000000000000ff) <<24) |
                            ((buf2 & 0x000000000000ff00) <<8) |
                            ((buf2 & 0x0000000000ff0000) >>8) |
                            ((buf2 & 0x00000000ff000000) >>24));
            lptr_src ++;
            lptr_src ++;
            lptr_dest ++;
        }
        if(odd_man_out) {
            buf1 = lptr_src[0];
            if(buf1 != 0) {
                buf1 = ((buf1 & LUO4F_MASKA) |
                        ((((buf1 & LUO4F_MASKB) >> 48) - 16258) << 55)) +
                        (((buf1 & LUO4F_MASKC) +
                        ((buf1 & LUO4F_MASKD) << 1)) << 8);
                lptr_dest[0] = ((buf1 & 0xff00000000000000) >>24) |
                               ((buf1 & 0x00ff000000000000) >>8) |
                               ((buf1 & 0x0000ff0000000000) <<8) |
                               ((buf1 & 0x000000ff00000000) <<24) ;
              } /* end if */
            else
                lptr_dest[0] = 0;
        }
#endif
#else   /* DUFF_luo4f */
		uintn orig_num_elm=num_elm;

        odd_man_out = num_elm % 2;

        num_elm/=2;
        n=(num_elm+7)/8;
		if(orig_num_elm>1)
        switch(num_elm%8) {
            case 0:
                do{
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
#ifdef QAK
            case 15:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 14:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 13:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 12:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 11:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 10:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 9:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 8:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
#endif
            case 7:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 6:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 5:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 4:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 3:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 2:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
            case 1:
                    if(lptr_src[0] && lptr_src[1])  /* both not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8)) |
                            ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else if(lptr_src[0])    /* src[0] not zero */
                        buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                            ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[0] & LUO4F_MASKC) +
                            ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
                    else if(lptr_src[1])    /* src[1] not zero */
                        buf1 = ((((lptr_src[1] & LUO4F_MASKA) |
                            ((((lptr_src[1] & LUO4F_MASKB) >> 48)-16258)<<55))+
                            (((lptr_src[1] & LUO4F_MASKC) +
                            ((lptr_src[1] & LUO4F_MASKD) << 1)) << 8)) >> 32);
                    else                    /* both zero */
                        buf1=0;
                    buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
                    lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
                    lptr_src+=2;
                    lptr_dest ++;
                } while(--n>0);
		}
        if(odd_man_out) {
            if(lptr_src[0])    /* src[0] not zero */
                buf1 = (((lptr_src[0] & LUO4F_MASKA) |
                    ((((lptr_src[0] & LUO4F_MASKB) >> 48)-16258)<<55))+
                    (((lptr_src[0] & LUO4F_MASKC) +
                    ((lptr_src[0] & LUO4F_MASKD) << 1)) << 8));
            else                    /* both zero */
                buf1=0;
            buf1 = ((buf1 & LUO4F_MASKE)>>8) | ((buf1 & LUO4F_MASKF)<<8);
            lptr_dest[0] = ((buf1 & LUO4F_MASKG)>>16) | ((buf1 & LUO4F_MASKH)<<16);
        }
#endif  /* DUFF_luo4f */
    }
    else { /* We end up here if we are doing stride based processing */
        buf1 = 0;
        for(i = 0; i < num_elm; i++) {
            dud1[0] = source[0];        /* Loop would be less efficient */
            dud1[1] = source[1];
            dud1[2] = source[2];
            dud1[3] = source[3];
            dud1[4] = source[4];
            dud1[5] = source[5];
            dud1[6] = source[6];
            dud1[7] = source[7];
      
            if((float)buf1 != 0)
                buf2 = (((buf1 & LUO4F_MASKA) |
                        ((((buf1 & LUO4F_MASKB) >> 48) -16258) << 55)) +
                        (((buf1 & LUO4F_MASKC) + ((buf1 & LUO4F_MASKD) << 1)) << 8));
            else
                buf2 = buf1;

            dest[3] = dud2[0];            /* Loop would be less efficient */
            dest[2] = dud2[1];
            dest[1] = dud2[2];
            dest[0] = dud2[3];

            source += source_stride;
            dest += dest_stride;
        }
    }
    return;
}

#define LUI8F_MASKA  0x0000000000000080
#define LUI8F_MASKB1 0x000000000000007f
#define LUI8F_MASKB2 0x000000000000f000
#define LUI8F_MASKC1 0x0000000000000f00
#define LUI8F_MASKC2 0x0000000000ff0000
#define LUI8F_MASKC3 0x00000000ff000000
#define LUI8F_MASKC4 0x000000ff00000000
#define LUI8F_MASKC5 0x0000ff0000000000
#define LUI8F_MASKC6 0x00ff000000000000
#define LUI8F_MASKC7 0xff00000000000000
#define LUI8F_MASKD  0x0800000000000000
#define LUI8F_MASKE  0x0000800000000000
#define LUI8F_MASKF 0xff00ff00ff00ff00
#define LUI8F_MASKG 0x00ff00ff00ff00ff
#define LUI8F_MASKH 0xffff0000ffff0000
#define LUI8F_MASKI 0x0000ffff0000ffff
#define LUI8F_MASKJ 0xffffffff00000000
#define LUI8F_MASKK 0x00000000ffffffff
#define LUI8F_MASKL 0xffffffffffffff7f

/************************************************************/
/* DFKlui8f()                                               */
/* -->Unicos routine for importing 64 bit floats            */
/************************************************************/

#ifdef PROTOTYPE
int DFKlui8f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKlui8f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int i,j,n;
  long buf;                             /* This is a temporary stride buf */
  uint8 * dud = (uint8*)&buf;           /* Dummy pointer to buf1 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;           /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;             /* void and void* constructions */
#endif /* PROTOTYPE*/
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKui8f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
    if(source_stride == 0 && dest_stride == 0)
        fast_processing = 1;

    if(fast_processing) {
#ifndef DUFF_lui8f
#if defined TEST1_lui8f
        n=num_elm;
        for(i = 0; i < n; i ++) {
            if(lptr_src[0]&LUI8F_MASKL)
                lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                    (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                    ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                    ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                        ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                        ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                        ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                        ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                        ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                        ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                    ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
            else
                lptr_dest[0];
            lptr_src++;
            lptr_dest++;
        }
#else
        for(i = 0; i < num_elm; i ++) {
            if (lptr_src[0] != 0) {
                if ((lptr_src[0] & (~LUI8F_MASKA)) == 0)
                    lptr_dest[0] = 0;
                else
                    lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
            }
            else
                lptr_dest[0] = 0;
            lptr_src++;
            lptr_dest++;
        }
#endif
#else   /* DUFF_lui8f */
        n=(num_elm+7)/8;
        switch(num_elm%8) {
            case 0:
                do{
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
#ifdef QAK
            case 15:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 14:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 13:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 12:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 11:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 10:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 9:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 8:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
#endif
            case 7:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 6:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 5:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 4:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 3:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 2:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
            case 1:
                    if(lptr_src[0]&LUI8F_MASKL)
                        lptr_dest[0] = (((lptr_src[0] & LUI8F_MASKA) << 56) | /* Sign */
                            (((lptr_src[0] & LUI8F_MASKB1) << 52) |   /* Exp. */
                            ((lptr_src[0] & LUI8F_MASKB2) << 36)) + (15362 << 48)) |
                            ((((((lptr_src[0] & LUI8F_MASKC1) << 40) | /* Mantissa */
                                ((lptr_src[0] & LUI8F_MASKC2) << 24) |
                                ((lptr_src[0] & LUI8F_MASKC3) << 8) |
                                ((lptr_src[0] & LUI8F_MASKC4) >> 8) |
                                ((lptr_src[0] & LUI8F_MASKC5) >> 24) |
                                ((lptr_src[0] & LUI8F_MASKC6) >> 40) |
                                ((lptr_src[0] & LUI8F_MASKC7) >> 56)) +
                            ((lptr_src[0] & LUI8F_MASKD) >> 55)) >>5) | LUI8F_MASKE);
                    else
                        lptr_dest[0]=0;
                    lptr_src++;
                    lptr_dest++;
                } while(--n>0);
        }
#endif  /* DUFF_lui8f */
      } /* end if */
    else
        for(i = 0; i < num_elm; i ++) {
            dud[0] = source[7];
            dud[1] = source[6];
            dud[2] = source[5];
            dud[3] = source[4];
            dud[4] = source[3];
            dud[5] = source[2];
            dud[6] = source[1];
            dud[7] = source[0];

            if (buf != 0) {
                buf = (((buf & UI8F_MASKA) | ((buf & UI8F_MASKB) >> 4) +
                        (15362 << 48)) |
                        ((((buf & UI8F_MASKC) + ((buf & UI8F_MASKD) << 1)) >> 5) |
                        (UI8F_MASKE)) );
                if ((buf << 1) == 0)
                    buf = 0;
            }
            else
                buf = 0;

            dest[0] = dud[0];
            dest[1] = dud[1];
            dest[2] = dud[2];
            dest[3] = dud[3];
            dest[4] = dud[4];
            dest[5] = dud[5];
            dest[6] = dud[6];
            dest[7] = dud[7];

            source += source_stride;
            dest += dest_stride;
        }
  return;
}

#define LUO8F_MASKA 0x8000000000000000
#define LUO8F_MASKB 0x7fff000000000000
#define LUO8F_MASKC 0x00007fffffffffff
#define LUO8F_MASKD 0xff00ff00ff00ff00
#define LUO8F_MASKE 0x00ff00ff00ff00ff
#define LUO8F_MASKF 0xffff0000ffff0000
#define LUO8F_MASKG 0x0000ffff0000ffff
#define LUO8F_MASKH 0xffffffff00000000
#define LUO8F_MASKI 0x00000000ffffffff

/************************************************************/
/* DFKluo8f()                                               */
/* -->Unicos routine for exporting 64 bit floats            */
/************************************************************/

#ifdef PROTOTYPE
int DFKluo8f(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKluo8f(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* By default not array processed */
  int odd_man_out = 0;                  /* By default there are even num_elm */
  int i,j,n;
  long buf;                             /* This is a temporary stride buf */
  uint8 * dud = (uint8*)&buf;           /* Dummy pointer to buf1 for strides */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;           /* Cray does not like certain   */
  uint8 * dest = (uint8*)d;             /* void and void* constructions */
#endif /* PROTOTYPE */
  long * lptr_src = (long*)source;
  long * lptr_dest = (long*)dest;
  char *FUNC="DFKuo8f";

  HEclear();

  /* Check for conversion errors */
  if(source == dest || num_elm == 0) { /* Inplace conversions not permitted */
    HERROR(DFE_BADCONV);               /* under UNICOS */
    return FAIL;                       /* No elements convert is an error   */
  }

  /* Find out if it is OK to use faster array processing */
    if(source_stride == 0 && dest_stride == 0)
        fast_processing = 1;

    if(fast_processing) {
#ifndef DUFF_luo8f
#if defined TEST1_luo8f
        n=num_elm;
        for(i = 0; i < n; i ++) {
            if(lptr_src[0])
                buf = (((lptr_src[0] & LUO8F_MASKA) |
                    (((((lptr_src[0] & LUO8F_MASKB) >> 48) - 15362) << 53) >> 1)) +
                    ((lptr_src[0] & LUO8F_MASKC) << 5));
            else
                buf=0;
            buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
            buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
            lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
            lptr_src++;
            lptr_dest++;
        }
#else
        for(i = 0; i < num_elm; i ++) {
            if (lptr_src[0] != 0) {
                buf = (((lptr_src[0] & LUO8F_MASKA) |
                        (((((lptr_src[0] & LUO8F_MASKB) >> 48) - 15362) << 53) >> 1)) +
                        ((lptr_src[0] & LUO8F_MASKC) << 5));
                lptr_dest[0] = ((buf & 0xff00000000000000) >>56) |
                               ((buf & 0x00ff000000000000) >>40) |
                               ((buf & 0x0000ff0000000000) >>24) |
                               ((buf & 0x000000ff00000000) >>8) |
                               ((buf & 0x00000000ff000000) <<8) |
                               ((buf & 0x0000000000ff0000) <<24) |
                               ((buf & 0x000000000000ff00) <<40) |
                               ((buf & 0x00000000000000ff) <<56);
              } /* end if */
            else
                lptr_dest[0] = 0;
            lptr_src++;
            lptr_dest++;
        }
#endif
#else   /* DUFF_luo8f */
        n=(num_elm+7)/8;
        switch(num_elm%8) {
            case 0:
                do{
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
#ifdef QAK
            case 15:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 14:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 13:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 12:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 11:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 10:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 9:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 8:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
#endif
            case 7:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 6:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 5:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 4:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 3:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 2:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
            case 1:
                    if(lptr_src[0])
                        buf = ((lptr_src[0] & LUO8F_MASKA) |
                            ((((lptr_src[0] & LUO8F_MASKB) >> 48)-15362)<<52))+
                            ((lptr_src[0] & LUO8F_MASKC) << 5);
                    else
                        buf=0;
                    buf = ((buf & LUO8F_MASKD)>>8) | ((buf & LUO8F_MASKE)<<8);
                    buf = ((buf & LUO8F_MASKF)>>16) | ((buf & LUO8F_MASKG)<<16);
                    lptr_dest[0] = ((buf & LUO8F_MASKH)>>32) | ((buf & LUO8F_MASKI)<<32);
                    lptr_src++;
                    lptr_dest++;
                } while(--n>0);
		}
#endif  /* DUFF_luo8f */
      } /* end if */
    else
        for(i = 0; i < num_elm; i ++) {
            dud[0] = source[0];
            dud[1] = source[1];
            dud[2] = source[2];
            dud[3] = source[3];
            dud[4] = source[4];
            dud[5] = source[5];
            dud[6] = source[6];
            dud[7] = source[7];

            if (buf != 0) {
                buf = (((buf & LUO8F_MASKA) |             /* sign bit */
                        (((((buf & LUO8F_MASKB) >> 48) - 15362) << 53) >> 1)) |/* exp */
                        ((buf & LUO8F_MASKC) << 5));      /* mantissa */
            }
            else
                buf = 0;

            dest[7] = dud[0];
            dest[6] = dud[1];
            dest[5] = dud[2];
            dest[4] = dud[3];
            dest[3] = dud[4];
            dest[2] = dud[5];
            dest[1] = dud[6];
            dest[0] = dud[7];

            source += source_stride;
            dest += dest_stride;
        }
  return;
}

#else /* i.e. not on a cray */

int cray_dummy; /* prevent empty symbol table messages */

#endif /* UNICOS */

