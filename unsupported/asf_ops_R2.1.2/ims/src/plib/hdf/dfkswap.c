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
/* DFKSWAP.C                                                            */
/************************************************************************/

/*------------------------------------------------------------------
 File:  dfkswap.c

 Purpose:
    Routines to support little-endian conversion to and from HDF format

 Invokes:
    
 PRIVATE conversion functions:
    DFKsb2b -  Byte swapping for 16 bit integers
    DFKsb4b -  Byte swapping for 32 bit integers
    DFKsb8b -  Byte swapping for 64 bit floats 

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
/* NUMBER CONVERSION ROUTINES FOR BYTE SWAPPING                              */
/*****************************************************************************/

/************************************************************/
/* DFKsb2b()                                                */
/* -->Byte swapping for 2 byte data items                   */
/************************************************************/
#ifdef PROTOTYPE
int DFKsb2b(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKsb2b(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* Default is not fast processing */
  int in_place = 0;                     /* Inplace must be detected */
  register uint32 i;            
  uint8 buf[2];                          /* Inplace processing buffer */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  char *FUNC="DFKsb2b";

  HEclear();

  if(num_elm == 0){                          /* No elements is an error. */
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if(source_stride == 0 && dest_stride == 0)
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) 
    if(!in_place) {
      for(i = 0; i < num_elm; i++) {
        dest[0] = source[1];
        dest[1] = source[0];
        dest += 2;
        source += 2;
      }
      return 0;
    }
    else { 
      for(i = 0; i < num_elm; i++) {
        buf[0] = source[1];
        buf[1] = source[0];
        dest[0] = buf[0];
        dest[1] = buf[1];
        dest += 2;
        source += 2;
      }
      return 0;
    }
  
  /* Generic stride processing */
  if(!in_place)
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[1];
      dest[1] = source[0];
      dest += dest_stride;
      source += source_stride;
    }
  else
    for(i = 0; i < num_elm; i++) {
      buf[0] = source[1];
      buf[1] = source[0];
      dest[0] = buf[0];
      dest[1] = buf[1];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

/************************************************************/
/* DFKsb4b()                                                */
/* -->Byte swapping for 4 byte data items                   */
/************************************************************/
#ifdef PROTOTYPE
int DFKsb4b(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKsb4b(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* Default is not fast processing */
  int in_place = 0;                     /* Inplace must be detected */
  register uint32 i;            
  uint8 buf[4];                          /* Inplace processing buffer */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
  char *FUNC="DFKsb4b";
#ifdef TEST3_sb4b
  uint32 *lp_dest;
  uint32 *lp_src;
#endif

  HEclear();

  if(num_elm == 0){                         /* No elements is an error. */
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if(source_stride == 0 && dest_stride == 0)
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) 
    if(!in_place) {
#ifndef DUFF_sb4b
#ifdef TEST1_sb4b
    source+=3;
#endif
#ifdef TEST3_sb4b
    lp_dest=(uint32 *)dest;
    lp_src=(uint32 *)source;
#endif
      for(i = 0; i < num_elm; i++) {
#if defined TEST3_sb4b
        *lp_dest++ = ((lp_src[0]&0x000000ff)<<24) |
                    ((lp_src[0]&0x0000ff00)<<8) |
                    ((lp_src[0]&0x00ff0000)>>8) |
                    ((lp_src[0]&0xff000000)>>24);
        lp_src++;
#else
        dest[0] = source[3];
        dest[1] = source[2];
        dest[2] = source[1];
        dest[3] = source[0];
        dest += 4;
        source += 4;
#endif
      }
#else   /* DUFF_sb4b */
        register uint32 n=(num_elm+7)/8;

        switch(num_elm%8) {
            case 0:
                do{
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 7:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 6:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 5:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 4:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 3:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 2:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
            case 1:
                    dest[0] = source[3];
                    dest[1] = source[2];
                    dest[2] = source[1];
                    dest[3] = source[0];
                    dest += 4;
                    source += 4;
                } while(--n>0);
		}
#endif  /* DUFF_sb4b */
      return 0;
    }
    else {
      for(i = 0; i < num_elm; i++) {
        buf[0] = source[3];
        buf[1] = source[2];
        buf[2] = source[1];
        buf[3] = source[0];
        dest[0] = buf[0];
        dest[1] = buf[1];
        dest[2] = buf[2];
        dest[3] = buf[3];
        dest += 4;
        source += 4;
      }
      return 0;
    }
  
  /* Generic stride processing */
  if(!in_place)
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[3];
      dest[1] = source[2];
      dest[2] = source[1];
      dest[3] = source[0];
      dest += dest_stride;
      source += source_stride;
    }
  else
    for(i = 0; i < num_elm; i++) {
      buf[0] = source[3];
      buf[1] = source[2];
      buf[2] = source[1];
      buf[3] = source[0];
      dest[0] = buf[0];
      dest[1] = buf[1];
      dest[2] = buf[2];
      dest[3] = buf[3];
      dest += dest_stride;
      source += source_stride;
    }
  return 0;
}

/************************************************************/
/* DFKsb8b()                                                */
/* -->Byte swapping for 8 byte data items                   */
/************************************************************/
#ifdef PROTOTYPE
int DFKsb8b(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKsb8b(source, dest, num_elm, source_stride, dest_stride)
uint8 * source, * dest;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* Default is not fast processing */
  int in_place = 0;                     /* Inplace must be detected */
  register uint32 i;            
  uint8 buf[8];                          /* Inplace processing buffer */
#ifdef PROTOTYPE
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
#endif /* PROTOTYPE */
    char *FUNC="DFKsb8b";

    HEclear();

  if(num_elm == 0){                         /* No elements is an error. */
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if(source_stride == 0 && dest_stride == 0)
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) 
    if(!in_place) {
      for(i = 0; i < num_elm; i++) {
        dest[0] = source[7];
        dest[1] = source[6];
        dest[2] = source[5];
        dest[3] = source[4];
        dest[4] = source[3];
        dest[5] = source[2];
        dest[6] = source[1];
        dest[7] = source[0];
        dest += 8;
        source += 8;
      }
      return 0;
    }
    else {
      for(i = 0; i < num_elm; i++) {
        buf[0] = source[7];
        buf[1] = source[6];
        buf[2] = source[5];
        buf[3] = source[4];
        buf[4] = source[3];
        buf[5] = source[2];
        buf[6] = source[1];
        buf[7] = source[0];
        dest[0] = buf[0];
        dest[1] = buf[1];
        dest[2] = buf[2];
        dest[3] = buf[3];
        dest[4] = buf[4];
        dest[5] = buf[5];
        dest[6] = buf[6];
        dest[7] = buf[7];
        dest += 8;
        source += 8;
      }
      return 0;
    }
  
  /* Generic stride processing */
  if(!in_place)
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[7];
      dest[1] = source[6];
      dest[2] = source[5];
      dest[3] = source[4];
      dest[4] = source[3];
      dest[5] = source[2];
      dest[6] = source[1];
      dest[7] = source[0];
      dest += dest_stride;
      source += source_stride;
    }
  else
    for(i = 0; i < num_elm; i++) {
      buf[0] = source[7];
      buf[1] = source[6];
      buf[2] = source[5];
      buf[3] = source[4];
      buf[4] = source[3];
      buf[5] = source[2];
      buf[6] = source[1];
      buf[7] = source[0];
      dest[0] = buf[0];
      dest[1] = buf[1];
      dest[2] = buf[2];
      dest[3] = buf[3];
      dest[4] = buf[4];
      dest[5] = buf[5];
      dest[6] = buf[6];
      dest[7] = buf[7];
      dest += dest_stride;
      source += source_stride;
    }

  return 0;
}


