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
/* DFCNAT.C                                                             */
/************************************************************************/

/*------------------------------------------------------------------
 File:  dfcnat.c

 Purpose:
    Routines to support "native mode" conversion to and from HDF format

 Invokes:
    
 PRIVATE conversion functions:
    DFKnb1b -  Native mode for 8 bit integers
    DFKnb2b -  Native mode for 16 bit integers
    DFKnb4b -  Native mode for 32 bit integers and floats 
    DFKnb8b -  Native mode for 64 bit floats 

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
/* NATIVE MODE NUMBER "CONVERSION" ROUTINES                                  */
/*****************************************************************************/

/************************************************************/
/* DFKnb1b()                                                */
/*   Native mode for 1 byte data items                      */
/************************************************************/
#ifdef PROTOTYPE
int DFKnb1b(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKnb1b(s, d, num_elm, source_stride, dest_stride)
VOIDP s, d;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;
  int in_place = 0;
  register uint32 i;
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
  char *FUNC="DFKnb1b";

  HEclear();

  if(num_elm == 0){
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if((source_stride == 0 && dest_stride == 0) ||
     (source_stride == 1 && dest_stride == 1))
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) {
    if(!in_place) {
      HDmemcpy(dest, source, num_elm);
      return 0;
    }
    else
      return 0;                         /* Nothing to do */
  } 
  else
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[0];
      dest += dest_stride;
      source += source_stride;
    }
  
  return 0;
}

#if !defined(UNICOS)  /* UNICOS does not need these routines */

/************************************************************/
/* DFKnb2b()                                                */
/* -->Native mode for 2 byte data items                     */
/************************************************************/
#ifdef PROTOTYPE
int DFKnb2b(VOIDP s, VOIDP d, uint32 num_elm, uint32 source_stride,
		   uint32 dest_stride)
#else
int DFKnb2b(s, d, num_elm, source_stride, dest_stride)
VOIDP s, d;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* Default is not fast processing */
  int in_place = 0;                     /* Inplace must be detected */
  register uint32 i;            
  uint8 buf[2];                          /* Inplace processing buffer */
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
  char *FUNC="DFKnb2b";

  HEclear();

  if(num_elm == 0){
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if((source_stride == 0 && dest_stride == 0) ||
     (source_stride == 2 && dest_stride == 2))
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) 
    if(!in_place) {
      HDmemcpy(dest, source, num_elm*2);
      return 0;
    }
    else {    /* Nothing to do */
      return 0;
    }
  
  /* Generic stride processing */
  if(!in_place)
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[0];
      dest[1] = source[1];
      dest += dest_stride;
      source += source_stride;
    }
  else
    for(i = 0; i < num_elm; i++) {
      buf[0] = source[0];
      buf[1] = source[1];
      dest[0] = buf[0];
      dest[1] = buf[1];
      dest += dest_stride;
      source += source_stride;
    }
    
  return 0;
}

/************************************************************/
/* DFKnb4b()                                                */
/* -->Native mode for 4 byte items                          */
/************************************************************/
#ifdef PROTOTYPE
int DFKnb4b(VOIDP s, VOIDP d, uint32 num_elm,
		   uint32 source_stride, uint32 dest_stride)
#else
int DFKnb4b(s, d, num_elm, source_stride, dest_stride)
uint8 * s, * d;
uint32 num_elm, source_stride, dest_stride;
#endif /* PROTOTYPE */
{
  int fast_processing = 0;              /* Default is not fast processing */
  int in_place = 0;                     /* Inplace must be detected */
  register uint32 i;            
  uint8 buf[4];                          /* Inplace processing buffer */
  uint8 * source = (uint8*)s;
  uint8 * dest = (uint8*)d;
  char *FUNC="DFKnb4b";

  HEclear();

  if(num_elm == 0){
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if((source_stride == 0 && dest_stride == 0) ||
     (source_stride == 4 && dest_stride == 4))
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) 
    if(!in_place) {
      HDmemcpy(dest, source, num_elm*4);
      return 0;
    }
    else {  /* Nothing to do */
      return 0;
    }
 
  /* Generic stride processing */
  if(!in_place)
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[0];
      dest[1] = source[1];
      dest[2] = source[2];
      dest[3] = source[3];
      dest += dest_stride;
      source += source_stride;
    }
  else
    for(i = 0; i < num_elm; i++) {
      buf[0] = source[0];
      buf[1] = source[1];
      buf[2] = source[2];
      buf[3] = source[3];
      dest[0] = buf[0];
      dest[1] = buf[1];
      dest[2] = buf[2];
      dest[3] = buf[3];
      dest += dest_stride;
      source += source_stride;
    }
    
  return 0;
}

#endif /* UNICOS */

/************************************************************/
/* DFKnb8b()                                                */
/* -->Native mode for 8 byte items                          */
/************************************************************/
#ifdef PROTOTYPE
int DFKnb8b(VOIDP s, VOIDP d, uint32 num_elm,
		   uint32 source_stride, uint32 dest_stride)
#else
int DFKnb8b(source, dest, num_elm, source_stride, dest_stride)
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

    char *FUNC="DFKnb8b";

    HEclear();

  if(num_elm == 0){
    HERROR(DFE_BADCONV);
    return FAIL;
  }

  /* Determine if faster array processing is appropriate */
  if((source_stride == 0 && dest_stride == 0) ||
     (source_stride == 8 && dest_stride == 8))
    fast_processing = 1;

  /* Determine if the conversion should be inplace */
  if(source == dest)
    in_place = 1;

  if(fast_processing) 
    if(!in_place) {
      HDmemcpy(dest, source, num_elm*8);
      return 0;
    }
    else {  
      return 0;     /* No work to do ! */
      }
 
  /* Generic stride processing */
  if(!in_place)
    for(i = 0; i < num_elm; i++) {
      dest[0] = source[0];
      dest[1] = source[1];
      dest[2] = source[2];
      dest[3] = source[3];
      dest[4] = source[4];
      dest[5] = source[5];
      dest[6] = source[6];
      dest[7] = source[7];
      dest += dest_stride;
      source += source_stride;
    }
  else
    for(i = 0; i < num_elm; i++) {
      buf[0] = source[0];
      buf[1] = source[1];
      buf[2] = source[2];
      buf[3] = source[3];
      buf[4] = source[4];
      buf[5] = source[5];
      buf[6] = source[6];
      buf[7] = source[7];
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



