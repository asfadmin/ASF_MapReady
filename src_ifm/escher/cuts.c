/*Escher:
Routines to create/maintain branch cut tree.
*/
#include "escher.h"


void 
cutMask(void)
{
  int i, j;

  /*
   * This function contains two loops over (wid-3)x(len-3) potential 
   *   residue sites.
   * The first loop is a 'pre-processor' binary clobber.  
   * The second loop leads to a Goldstein style cutting function if a 
   *   site contains a residue.  
   */

#if 0
  printf("starting binary filter preprocessor [cutMask()]...\n");
  for (j = 1; j < len-2; j++) {     

    if (!(j%(len/6))) printf ("  ...binary clobber at %d of %d\n", j, len);

    for (i = 1; i < wid-2; i++) { 

      if (mask[j*wid+i) & POSITIVE_CHARGE) {
        if      (mask[(j-1)*wid+i-1] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i-1] |= IN_CUT; }
        else if (mask[(j-1)*wid+i  ] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i  ] |= IN_CUT; }
        else if (mask[(j-1)*wid+i+1] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i+1] |= IN_CUT; }
        else if (mask[(j  )*wid+i-1] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i-1] |= IN_CUT; }
        else if (mask[(j  )*wid+i+1] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i+1] |= IN_CUT; }
        else if (mask[(j+1)*wid+i-1] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i-1] |= IN_CUT; }
        else if (mask[(j+1)*wid+i  ] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i  ] |= IN_CUT; }
        else if (mask[(j+1)*wid+i+1] & NEGATIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i+1] |= IN_CUT; }
      }
      else if (mask[j*wid+i) & NEGATIVE_CHARGE) {
        if      (mask[(j-1)*wid+i-1] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i-1] |= IN_CUT; }
        else if (mask[(j-1)*wid+i  ] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i  ] |= IN_CUT; }
        else if (mask[(j-1)*wid+i+1] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i+1] |= IN_CUT; }
        else if (mask[(j  )*wid+i-1] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i-1] |= IN_CUT; }
        else if (mask[(j  )*wid+i+1] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i+1] |= IN_CUT; }
        else if (mask[(j+1)*wid+i-1] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i-1] |= IN_CUT; }
        else if (mask[(j+1)*wid+i  ] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i  ] |= IN_CUT; }
        else if (mask[(j+1)*wid+i+1] & POSITIVE_CHARGE) { mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i+1] |= IN_CUT; }
      }

    }
  }     /* end of sites loop */
#endif

  /* initialize the number of points in 'list' to zero */
  list.n = 0;

  /* loop over (wid-3)x(len-3) residue sites */
  printf("starting branch cut scan [cutMask()]...\n");
  for (j = 1; j < len-2; j++) {
    register Uchar *maskLineStart=mask+wid*j;
    if (!(j%(len/8)))  printf("  ...at %d of %d\n", j, len); 
    for (i = 1; i < wid-2; i++) { 
      /* 
       * this will be a point to cut if it has some charge 
       * and is not already in a cut 
       */
      if (*(maskLineStart+i) & SOME_CHARGE && !(*(maskLineStart+i) & IN_CUT)) { 
	generateCut(i, j); 
      }
    }
  }   

  printf(" - finished branch cutting\n");
  return;
}

/*
 * generateCut(int i, int j) is passed a coordinate within the image 
 * which contains a charge, either + or -.  The job of generateCut() is to 
 * install a branch cut in the mask array which includes the point (i, j) 
 * and which has a total charge of zero. Furthermore, we want the 
 * number of points involved in the branch cut to be minimized.
 */
void 
generateCut(int i, int j)   
{
  Uchar tV;                        /* test value */
  int point, point_i, point_j;
  int subR, subRmo;
  int r, n, k, l;
  int p, p_i, p_j, p_ii, p_jj, cIdx;
  int rmo;                       /* r minus one */
  int maxR;
  int tC=0;                        /* total charge */
  /*int count;*/                     /* debug test */
  int scram;

  /* calculate the total charge of the tree */
  if (mask[ j*wid + i] & POSITIVE_CHARGE) 
    tC = +1; 
  else if (mask[ j*wid + i] & NEGATIVE_CHARGE) 
    tC = -1; 
  else  
    Exit("generateCut() called with no charge at (%d,%d)",i,j); 

  /* calculate the maximum possible radius box around this point */
  maxR = min(i + 1, j + 1);
  maxR = min(maxR, wid - i);
  maxR = min(maxR, len - j);

  /* set the number of points in the list to 1, and point 0 to (i, j) */
  list.n      = 1;
  list.p[0].i = i;
  list.p[0].j = j;
  list.c[0]   = 0;   /* point 0 connects to itself */

  /* set this point in the mask to IN_TREE */
  mask[ j*wid + i] |= IN_TREE;

  /* set initial radius r = 2 and also set rmo = r - 1, a utility variable */
  r   = 2; 
  rmo = r-1;

  /* set a halting flag to FALSE */
  scram = FALSE;

  /* do-loop over radii */
  do {

    /* loop over the charge points in the current tree      */
    /*   (These may include cut charges from earlier trees) */
    for (point = 0; point < list.n; point++) {
 
      point_i = list.p[point].i;
      point_j = list.p[point].j;

      /* loop over ALL the pixels in the box of radius r around this point */ 
      /* do this by starting with a box of radius 2 and working out */
      /* to radius r */
      for (subR = 2; subR <= r; subR++) {
        subRmo = subR - 1;
        for (n = 0; n < 8*subRmo; n++) {

          /*
           calculate the coordinates (k, l) in the loop around the box 
	   of radius 'subR' centered on the current location 
	   (point_i, point_j), which is one of the  points in the point 
	   list for this branch cut 
          */
          if (n < 2*subRmo) { 
	    k = point_i + n -   subRmo; 
	    l = point_j     -   subRmo; 
	  }
          else if (n < 4*subRmo) { 
	    k = point_i     +   subRmo; 
	    l = point_j + n - 3*subRmo; 
	  }
          else if (n < 6*subRmo) { 
	    k = point_i - n + 5*subRmo; 
	    l = point_j     +   subRmo; 
	  }
          else { 
	    k = point_i     -   subRmo; 
	    l = point_j - n + 7*subRmo; 
	  }

          /* make sure (k, l) is within the image boundary */
          if (k >= 0 && k < wid && l >= 0 && l < len) {

            /* establish a test value 'tV', the value of the mask at (k, l) */
            tV = mask[l*wid+k];

            /* test to see if the test value is grounded */
            if (tV & GROUNDED) {
              /* logical error check */
              if (tV & IN_TREE) Exit("tV is both GROUNDED && IN_TREE");
              /* new total charge is zero automatically */
              tC    =    0;
              /* 
	       * get ready to scram; 
	       * don't mark point as on current tree since we're done. 
	       */
              scram = TRUE;

              /* increment number of points in the list, set last location */
              list.n++;
              if (list.n > 1000000) 
		 Exit("list exceeded 1 million points");
              list.p[(list.n)-1].i = k;
              list.p[(list.n)-1].j = l;
	      /* connect this guy to point number 'point' */
              list.c[(list.n)-1]   = point;  

              /* 
	       * connect all points with GROUNDED lines 
	       * to their source connections 
	       */
	      /* start at the second point on the list */
	      /* loop to the last point on the list    */
              for (p = 1; p <= (list.n)-1; p++) {
		/* set (p_i, p_j) to 'p-th' point in the list */
                p_i  = list.p[p].i;  
                p_j  = list.p[p].j;
		/* connection index is carried in c[] array   */
                cIdx = list.c[p];   
                p_ii = list.p[cIdx].i;
                p_jj = list.p[cIdx].j;
                makeBranchCut(p_i, p_j, p_ii, p_jj, (IN_CUT | GROUNDED));
              }

            }  /* end if test value tV is GROUNDED */

            /* 
	     * else check to see if the test value is charged 
	     * AND not on the current tree
	     */
            else if (tV & SOME_CHARGE && !(tV & IN_TREE)) {

              /* 
	       * calculate a new total charge only if (k, l) 
	       * is not already part of a cut */
              if (!(tV & IN_CUT)) { tC += 3 - 2*((int)(tV & SOME_CHARGE)); }
              /* label all points from (point_i, _j) to (k, l) as IN_CUT */
              makeBranchCut(point_i, point_j, k, l, IN_CUT);
              /* increment number of points in the list, set last location */
              list.n++;
              if (list.n > 1000000) Exit("list exceeded 1 million points");
              list.p[(list.n)-1].i = k;
              list.p[(list.n)-1].j = l;
	      /* connect this guy to point number 'point' */
              list.c[(list.n)-1]   = point;  

              /* mark this point as being on the current tree */
              mask[ l*wid + k] |= IN_TREE;

              /* scram if this cut has neutralized the tree */
              if (!tC) { scram = TRUE; }

            }  
	    /* 
	     * end else if test value tV is a charge no already 
	     * in the current tree
	     */
          } /* end if (k, l) in box around a point 
	     * in the tree is within image boundary 
	     */

          /* get out of everything if either total charge = 0 
	   * or we hit a ground */
          if (scram) break;

        } /* end of loop over a box around a point in the tree */

        /* get out of everything if either total charge = 0 
	 * or we hit a ground */
        if (scram) break;
  
      } /* end loop over sub-Radii to span box of radius r */

      /* get out of everything if either total charge = 0 or we hit a ground */
      if (scram) break;

    } /* end of loop over the points in the tree */

    /* get out of everything if either total charge = 0 or we hit a ground */
    if (scram) break;

    r++; 
    rmo++;

  } while (r <= maxR);
  
#if DO_DEBUG_CHECKS
  /* a logic check; scram should be TRUE */
  /* I think we can just about take this out pretty soon */
  if (!scram) {
    printf("(%d, %d), maxR = %d, r = %d, rmo = %d, list.n = %d\n",
      i, j, maxR, r, rmo, list.n);
    Exit("Error in generateCut()");
  }
#endif
    

  /* 
   * Having escaped from this do-while loop, 
   * there is a list of points on the current
   * tree which should be marked 'NOT_IN_TREE'.
   */
  for (point = 0; point < list.n; point++) {
    point_i = list.p[point].i;
    point_j = list.p[point].j;
    mask[ point_j*wid + point_i] &= NOT_IN_TREE;
  }

#if DEBUG_TEST
  /* debug test... */
  /* I think we can just about take this out pretty soon */
  count = 0;
  for (point = 0; point < list.n; point++) {
    point_i = list.p[point].i;
    point_j = list.p[point].j;
    if (!(mask[ point_j*wid + point_i] & IN_CUT)) { count++; }
  }
  if (count) {
    printf("at point (%d, %d), the debug test for IN_CUT returned:\n",i,j);
    printf("\t%d bad of %d in the list\n", count, list.n);
    for (point = 0; point < list.n; point++) {
      point_i = list.p[point].i;
      point_j = list.p[point].j;
      if (!(mask[ point_j*wid + point_i] & IN_CUT)) { count++; }
      printf("%d: (%d, %d)\n\tmask %d\n\tmask & IN_CUT %d\n",
        point, point_i, point_j, (int)(mask[point_j*wid+point_i)),
        (int)(mask[point_j*wid+point_i] & IN_CUT));
      printf("\tmask & GROUNDED %d\n\tmask & SOME_CHARGE %d\n",
        (int)(mask[point_j*wid+point_i] & GROUNDED),
        (int)(mask[point_j*wid+point_i] & SOME_CHARGE));
    }
    Exit("generateCut() failed logical test");
  }
#endif

  /* reset number of points on list to zero */
  list.n = 0;

  return;
}



/* 
 * makeBranchCut uses the 'one coord diff >= the other; use it for a loop' idea.
 * The purpose of this function is to do a logical or of 'orVal' with every 
 *   pixel in the mask array from (i, j) to (ii, jj) inclusive.
 */
void makeBranchCut(int i, int j, int ii, int jj, Uchar orVal)
{
  int   dx, dy;        /* differences in coord values               */
  int   adx, ady;      /* absolute values of diffs                  */
  int   lc, sc;        /* int and short coords                     */
  int   order;         /* boolean to indicate coordinate order      */
  int   lcd;           /* int coordinate difference (adx or ady)   */ 
  int   c1;            /* coord one (the int coordinate)           */
  int   c2;            /* coord two (the short coordinate)          */
  int   dc1=1;         /* delta in coord 1 (+1 or -1)               */
  float slope;         /* slope of line between (i, j) and (ii, jj) */

  /* cut sizes */
  dx  = i - ii;
  dy  = j - jj;
  adx = abs(dx);
  ady = abs(dy);

  /* initialize looping parameters */
  if (adx > ady) {
    lc    = i; 
    sc    = j; 
    order = 1;
    lcd   = dx;
    if      (dx < 0) dc1 =  1;
    else if (dx > 0) dc1 = -1;
    else             Exit("makeBranchCut():  logic error 1");
    slope = (float)(dy)/(float)(dx);
  }
  else           {
    lc    = j; 
    sc    = i; 
    order = 0;
    lcd   = dy;
    if      (dy < 0) dc1 =  1;
    else if (dy > 0) dc1 = -1;
    else             Exit("makeBranchCut():  logic error 2");
    slope = (float)(dx)/(float)(dy);
  }

  /* loop over the points and set the IN_CUT bit */
  if (order) {
    for (c1 = lc; c1 != lc - lcd + dc1; c1 += dc1) {
      c2 = sc + (int)(slope*(float)(c1 - lc));
      mask[c2*wid+c1] |= orVal;
    }
  }
  else {
    for (c1 = lc; c1 != lc - lcd + dc1; c1 += dc1) {
      c2 = sc + (int)(slope*(float)(c1 - lc));
      mask[c1*wid+c2] |= orVal;
    }
  }

  /* also label the last point as IN_CUT */
  mask[ jj*wid + ii] |= orVal;

  return;
}
