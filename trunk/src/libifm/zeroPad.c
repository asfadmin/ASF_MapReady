/****************************************************************
FUNCTION NAME: zeroPad

SYNTAX: void zeroPad(vIn,vOut,dim,os);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    vIn         Complex *       in vector
    vOut        Complex *       out vector
    dim         int             dimension of square matrix
    os          int             (* unknown *)

DESCRIPTION:
  Frequency domain oversampling; we need to zero pad matrix 'v' by
  copying 'v' into 'vo' with extra zeros; note that ambiguity 
  splitting (1/2 and 1/4 values) are taken care of after the main copying 
  code.  
 
  As an example of how a zero pad copy proceeds, we have:
 
   a b c d
   e f g h
   i j k l
   m n p q
 
   with dim = 4 and os = 2 becomes
  
   a    b    c/2  0    0    0    c/2  d
   e    f    g/2  0    0    0    g/2  h
   i/2  j/2  k/4  0    0    0    k/4  l/2 
   0    0    0    0    0    0    0    0 
   0    0    0    0    0    0    0    0 
   0    0    0    0    0    0    0    0 
   i/2  j/2  k/4  0    0    0    k/4  l/2
   m    n    p/2  0    0    0    p/2  q
 
RETURN VALUE:
  A zero padded output array
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - Rob Fatland & Mike Shindle - Original Development
****************************************************************/
#include "ifm.h"

void zeroPad(complexFloat *vIn, complexFloat *vOut, int dim, int os)
{
  int i, j, ii, jj, indx, oindx, big;
  complexFloat a, b;

  big = dim*os;

  /*
   *  First step:  copy the unchanged elements (a, b, d, e, f, 
   *  h, m, n, and q in the example above) from the corners. 
   */

  /* 'ulh' corner */
  for (i = 0; i < dim/2; i++) { 
    for (j = 0; j < dim/2; j++) {
      ii = i;
      jj = j;
      indx  = j*dim + i;
      oindx = jj*big + ii;
      *(vOut + oindx) = *(vIn + indx);
    } 
  }

  /* 'llh' corner */
  for (i = 0; i < dim/2; i++) { 
    for (j = dim/2 + 1; j < dim; j++) {
      ii = i;
      jj = j - (dim/2 + 1) + big - (dim/2 - 1);
      indx  = j*dim + i;
      oindx = jj*big + ii;
      *(vOut + oindx) = *(vIn + indx);
    }
  }

  /* 'urh' corner */
  for (i = dim/2 + 1; i < dim; i++) { 
    for (j = 0; j < dim/2; j++) {
      ii = i - (dim/2 + 1) + big - (dim/2 - 1);
      jj = j;
      indx  = j*dim + i;
      oindx = jj*big + ii;
      *(vOut + oindx) = *(vIn + indx);
    }
  }

  /* 'lrh' corner */
  for (i = dim/2 + 1; i < dim; i++) { 
    for (j = dim/2 + 1; j < dim; j++) {
      ii = i - (dim/2 + 1) + big - (dim/2 - 1);
      jj = j - (dim/2 + 1) + big - (dim/2 - 1);
      indx  = j*dim + i;
      oindx = jj*big + ii;
      *(vOut + oindx) = *(vIn + indx);
    }
  }

  /*
   *  Second step:  copy the halved middle row elements (i, j, l in 
   *  the example above) across. 
   */

  /* copy ambiguous high frequency elements:  row 'dim/2' */
  j = dim/2;
  for (i = 0; i < dim; i++) {
    indx = j*dim + i;
    a = *(vIn + indx);
    b = Csmul(.5, a);
      
    /* two regions handled separately:  i < dim/2, i > dim/2 */
    /* The case i == dim/2 is handled later */ 
    if (i < dim/2) {
      /* top part */
      ii = i;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
      /* bottom part */
      ii = i;
      jj = dim*os - dim/2;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
    } 
    else if (i > dim/2) {
      /* top part */
      ii = dim*(os - 1) + i;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
      /* bottom part */
      ii = dim*(os-1) + i;
      jj = dim*os - dim/2;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
    }
  } /* end row 'dim/2' */

  /*
   *  Third step:  copy the halved middle column elements (c, g, p in 
   *  the example above) across. 
   */
  /* copy ambiguous high frequency elements:  column 'dim/2' */
  i = dim/2;
  for (j = 0; j < dim; j++) {
    indx = j*dim + i;
    a = *(vIn + indx);
    b = Csmul (.5, a);
      
    /* two regions handled separately:  j < dim/2, j > dim/2 */
    /* The case j == dim/2 is handled later */ 
    if (j < dim/2) {
      /* left part */
      ii = i;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
      /* right part */
      ii = dim*os - dim/2;
      jj = j;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
    }
    else if (j > dim/2) {
      /* left part */
      ii = i;
      jj = dim*(os - 1) + j;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
      /* right part */
      ii = dim*os - dim/2;
      jj = dim*(os-1) + j;
      oindx = ii + jj*dim*os;
      *(vOut + oindx) = b;
    }
  } /* end column 'dim/2' */

  /*
   *  Fourth step:  copy the quartered middle column middle column
   *  elements (k the example above) to 4 locations. 
   */
    
  /* copy ambiguous high frequency elements:  element (dim/2, dim/2) */
  /* The dim/2 case is handled here */ 
  i = dim/2;
  j = dim/2;
  indx = j*dim + i;
  a = *(vIn + indx);
  b = Csmul (.25, a);

  /* ulh */
  ii = dim/2;
  jj = dim/2;
  oindx = ii + jj*dim*os;
  *(vOut + oindx) = b;

  /* llh */
  ii = dim/2;
  jj = dim*os - dim/2;
  oindx = ii + jj*dim*os;
  *(vOut + oindx) = b;

  /* urh */
  ii = dim*os - dim/2;
  jj = dim/2;
  oindx = ii + jj*dim*os;
  *(vOut + oindx) = b;

  /* lrh */
  ii = dim*os - dim/2;
  jj = dim*os - dim/2;
  oindx = ii + jj*dim*os;
  *(vOut + oindx) = b;

  return;
}
