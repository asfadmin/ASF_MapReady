#include "asf.h"
#include "ifm.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "asf_insar.h"

// General constants
#define MAXNAME         256
#define DO_DEBUG_CHECKS 0

/*
 * bits are number from 0 (lsb) to 7 (msb)
 *
 * status array 'mask' uses 5 bits:
 *
 * bit 0 TRUE => +residue
 * bit 1 TRUE => -residue
 * bit 2 TRUE => in a branch cut
 * bit 3 TRUE => grounded
 * bit 4 TRUE => in current search tree
 * bit 5 TRUE => integrated
 *
 * Note IICG means INTEGRTED | IN_CUT | GROUNDED
 *
 * integration array 'im' uses 8 bits:
 *
 * bit 0 TRUE => tried to move up
 * bit 1 TRUE => tried to move right
 * bit 2 TRUE => tried to move down
 * bit 3 TRUE => tried to move left
 * bit 4 TRUE => arrived at from below
 * bit 5 TRUE => arrived at from left
 * bit 6 TRUE => arrived at from above
 * bit 7 TRUE => arrived at from right
 *
 * These translate as:
 */

#define ZERO                  (0x00)        /* 0 0 0 0  0 0 0 0 */
#define NEUTRAL               (0xfc)        /* 1 1 1 1  1 1 0 0 */
#define POSITIVE_CHARGE       (0x01)        /* 0 0 0 0  0 0 0 1 */
#define NEGATIVE_CHARGE       (0x02)        /* 0 0 0 0  0 0 1 0 */
#define SOME_CHARGE           (0x03)        /* 0 0 0 0  0 0 1 1 */
#define NOT_IN_CUT            (0xfb)        /* 1 1 1 1  1 0 1 1 */
#define IN_CUT                (0x04)        /* 0 0 0 0  0 1 0 0 */
#define NOT_GROUNDED          (0xf7)        /* 1 1 1 1  0 1 1 1 */
#define GROUNDED              (0x08)        /* 0 0 0 0  1 0 0 0 */
#define NOT_INTEGRATED        (0xef)        /* 1 1 1 0  1 1 1 1 */
#define INTEGRATED            (0x10)        /* 0 0 0 1  0 0 0 0 */
#define IICG                  (0x1c)        /* 0 0 0 1  1 1 0 0 */
#define NOT_IN_TREE           (0xdf)        /* 1 1 0 1  1 1 1 1 */
#define IN_TREE               (0x20)        /* 0 0 1 0  0 0 0 0 */
#define NO_TRIES              (0x00)        /* 0 0 0 0  0 0 0 0 */
#define TRIED_U               (0x01)        /* 0 0 0 0  0 0 0 1 */
#define TRIED_R               (0x02)        /* 0 0 0 0  0 0 1 0 */
#define TRIED_UR              (0x03)        /* 0 0 0 0  0 0 1 1 */
#define TRIED_D               (0x04)        /* 0 0 0 0  0 1 0 0 */
#define TRIED_URD             (0x07)        /* 0 0 0 0  0 1 1 1 */
#define TRIED_L               (0x08)        /* 0 0 0 0  1 0 0 0 */
#define TRIED_URDL            (0x0f)        /* 0 0 0 0  1 1 1 1 */
#define SOURCE_B              (0x10)        /* 0 0 0 1  0 0 0 0 */
#define SOURCE_L              (0x20)        /* 0 0 1 0  0 0 0 0 */
#define SOURCE_A              (0x40)        /* 0 1 0 0  0 0 0 0 */
#define SOURCE_R              (0x80)        /* 1 0 0 0  0 0 0 0 */
#define SOURCE_X              (0xf0)        /* 1 1 1 1  0 0 0 0 */

// New Data types
typedef struct _Point {
  int i;
  int j;
} Point;

#define MAX_PLIST 1048576
typedef struct _PList {
  int n;
  Point p[1048576];
  int c[1048576];
} PList;

// Globals
PList list;
Uchar *mask;     /* phase-state mask  */
Uchar *im;       /* integration mask  */
float *phase;   /* input phase       */
float *coh;     /* coherence 'rho'   */
int wid;
int len;
int size;

// Function declarations
void loadWrappedPhase(char *phaseName);
void groundBorder(void);
void makeMask(void);
void verifyCuts(void);
Uchar chargeCalc(float ul, float ll, float lr, float ur);
float phaseRemap(float in);
void installCordon(char *cordonName);
void cutMask(void);
void generateCut(int x, int y);
void makeBranchCut(int x1, int y1, int x2, int y2, Uchar orBy);
void saveMask(Uchar *mask, char *maskName);
void readMask(Uchar *mask, char *maskName);
void finishUwp(void);
void checkSeed(int *new_seedX, int *new_seedY);
void integratePhase(int x, int y);
void saveUwp(char *uwpName);
void doStats();

void loadWrappedPhase(char *f)
{
  FILE * fd;
  meta_parameters *meta;

  fd = FOPEN(f, "rb");
  meta = meta_read(f);
  get_float_lines(fd, meta, 0, len, phase);
  FCLOSE(fd);

  return;
}

void groundBorder()
{
  int i, j;

  /* ground the left edge once and ground the right edge twice */
  for (j = 0; j < len; j++) {
    mask[j*wid+0]     |= GROUNDED;
    mask[j*wid+wid-2] |= GROUNDED;
    mask[j*wid+wid-1] |= GROUNDED;
  }

  /* ground the top edge once and ground the bottom edge twice */
  for (i = 0; i < wid; i++) {
    mask[(0)*wid+i]     |= GROUNDED;
    mask[(len-2)*wid+i] |= GROUNDED;
    mask[(len-1)*wid+i] |= GROUNDED;
  }

  return;
}


float phaseRemap(float p)
{
  p = (double)fmod((double)p,(double)TWOPI);
  if (p>PI) p-=TWOPI;
  if (p<-PI) p+=TWOPI;
  return p;
}


int isGoodSeed(int x,int y)
{
#define check_span 10 /*Make sure no cuts occur within this many pixels of seed*/
  int dx,dy;
  if ((x<check_span)||(x>=wid-check_span)||
      (y<check_span)||(y>=len-check_span))
    return 0;/*out-of-bounds*/
  dy=0;
  for (dx=-check_span;dx<=check_span;dx++)
    if (mask[(y+dy)*wid+(x+dx)]!=0)
      return 0;/*Some cut is near this point*/
  dx=0;
  for (dy=-check_span;dy<=check_span;dy++)
    if (mask[(y+dy)*wid+(x+dx)]!=0)
      return 0;/*Some cut is near this point*/
  return 1;/*If no cut is nearby, this is a good point*/
}

void checkSeed(int *x, int *y)
{
  /* adjust seed point to reside on a usable (mask == ZERO) pixel */
  while (!isGoodSeed(*x,*y))
  {
    asfPrintStatus("\n   seed point (%d, %d) is not ZERO.\n", *x, *y);
    /*Pick a new, random seed point.*/
    *x=(rand()&0x7fff)*wid/0x7fff;
    *y=(rand()&0x7fff)*len/0x7fff;
    asfPrintStatus("\n   auto-adjusted seed point to (%d, %d).\n", *x, *y);
  }
  asfPrintStatus("\n   checkSeed() finished\n\n");
  return;
}

/*
 * This function is a cursory test to check for 'residual' residues.
 *   If 'escher' is working properly it should give a null result.
 */
void verifyCuts(void)
{
  int i, j, nSites = 0, nResidues = 0;
  float p0, p1, p2, p3;

  asfPrintStatus("\nStarting the verification ...\n\n");

  for (j = 1; j < len - 2; j++) {
    for (i = 1; i < wid - 2; i++) {
      /* check only mask points which have 0-valued local loops */
      if (!mask[j*wid+i] && !mask[j*wid+i+1] &&
          !mask[(j+1)*wid+i+1] && !mask[(j+1)*wid+i]) {
        p0 = phase[wid*(j  )+i  ];
        p1 = phase[wid*(j+1)+i  ];
        p2 = phase[wid*(j+1)+i+1];
        p3 = phase[wid*(j  )+i+1];
        nSites++;

        /*
         * This if() is a quick way of utilizing the fact that a return
         *   charge value is nonzero, whereas no charge returs zero.
         */
        if (chargeCalc(p0, p1, p2, p3)) nResidues++;

      }
    }
  }

  asfPrintStatus("  Found %d add'l residues out of %d sites, %f percent\n",
    nResidues, nSites, 100.0*(float)(nResidues)/(float)(nSites));
  return;
}

void makeMask()
{
  int i, j;
  float p0, p1, p2, p3;

  for (j = 1; j < len - 2; j++) {
    for (i = 1; i < wid - 2; i++) {
      if (0.0==phase[wid*(j  )+i])
        /*Ground out zero-phases (e.g., layover regions)*/
	mask[j*wid+i] |= GROUNDED;
      p0 = phase[wid*(j  )+i  ];
      p1 = phase[wid*(j+1)+i  ];
      p2 = phase[wid*(j+1)+i+1];
      p3 = phase[wid*(j  )+i+1];
      mask[j*wid+i] |= chargeCalc(p0,p1,p2,p3);
    }
  }
  return;
}

Uchar chargeCalc(float p0, float p1, float p2, float p3)
{
  register float d0, d1, d2, d3, od0, od1, od2, od3, sum;

  /* compute differences in phase on [-2pi, 2pi] */
  od0 = p0 - p1;
  od1 = p1 - p2;
  od2 = p2 - p3;
  od3 = p3 - p0;

  /* remap [-2pi, -pi] to [0, pi] and [pi, 2pi] to [-pi, 0]  */
  d0 = phaseRemap(od0);
  d1 = phaseRemap(od1);
  d2 = phaseRemap(od2);
  d3 = phaseRemap(od3);

  /* add up sum of 'near-rectified' phase differences */
  sum = d0 + d1 + d2 + d3;

  /* compute charge, return if legit */
  if (fabs(sum)         < 0.00001) return ZERO;
  else if (fabs(sum - TWOPI) < 0.00001) return POSITIVE_CHARGE;
#if !DO_DEBUG_CHECKS
  else return NEGATIVE_CHARGE;
#else
  else if (fabs(sum + TWOPI) < 0.00001)
        return NEGATIVE_CHARGE;

  /* if we reach this point, computed charge is way bad */
  asfPrintStatus("\n\n");
  asfPrintStatus("    p0 = %f\n", p0);
  asfPrintStatus("    p1 = %f\n", p1);
  asfPrintStatus("    p2 = %f\n", p2);
  asfPrintStatus("    p3 = %f\n", p3);
  asfPrintStatus("\n");
  asfPrintStatus("    od0 = %f\n", od0);
  asfPrintStatus("    od1 = %f\n", od1);
  asfPrintStatus("    od2 = %f\n", od2);
  asfPrintStatus("    od3 = %f\n", od3);
  asfPrintStatus("\n");
  asfPrintStatus("    d0 = %f\n", d0);
  asfPrintStatus("    d1 = %f\n", d1);
  asfPrintStatus("    d2 = %f\n", d2);
  asfPrintStatus("    d3 = %f\n", d3);
  asfPrintStatus("\n");
  asfPrintStatus("   sum = %f\n", sum);
  asfPrintStatus("\n\n");
  asfPrintError("chargeCalc(): bad residue calculation");
  return 0;/*<-- for whining compilers.*/
#endif
}

void installCordon(char *cordonFnm)
{
  int i, n, x, y;
  FILE *fp;

  if (fileExist(cordonFnm)) {
    n = fileNumLines(cordonFnm);
    fp = FOPEN(cordonFnm,"r");
    for (i = 0; i < n; i++) {
      fscanf(fp,"%d", &x);
      fscanf(fp,"%d", &y);
      mask[ y*wid + x] |= GROUNDED;
    }
    fclose(fp);
    //printf("\ngrounded out %d points from cordon file '%s'\n\n",
    //  n, cordonFnm);
    doStats();
  }
  else {
    /*The "cordon" file almost never exists; so this shouldn't be an error!
    fprintf(stderr,
            " ** cordon file '%s' does not exist; skipped installation **\n",
            cordonFnm);*/
  }

  return;
}

void saveMask(unsigned char *m, char *f)
{
  char fnm[256];

  create_name(fnm,f,"_mask.img");
  writeVector(m, fnm, CHAR, size);
  return;
}

void readMask(unsigned char *m, char *fnm)
{
  if (!size)
    Exit("   escher:  readMask():  zero mask size");
  readVector(m, fnm, CHAR, size);
  return;
}

void cutMask(void)
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
  for (j = 1; j < len-2; j++) {

    if (!(j%(len/6))) 
      asfPrintStatus ("    ...binary clobber at %d of %d\n", j, len);

    for (i = 1; i < wid-2; i++) {

      if (mask[j*wid+i) & POSITIVE_CHARGE) {
        if      (mask[(j-1)*wid+i-1] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i-1] |= IN_CUT; 
	}
        else if (mask[(j-1)*wid+i  ] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i  ] |= IN_CUT; 
	}
        else if (mask[(j-1)*wid+i+1] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i+1] |= IN_CUT; 
	}
        else if (mask[(j  )*wid+i-1] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i-1] |= IN_CUT; 
	}
        else if (mask[(j  )*wid+i+1] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i+1] |= IN_CUT; 
	}
        else if (mask[(j+1)*wid+i-1] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i-1] |= IN_CUT; 
	}
        else if (mask[(j+1)*wid+i  ] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i  ] |= IN_CUT; 
	}
        else if (mask[(j+1)*wid+i+1] & NEGATIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i+1] |= IN_CUT; 
	}
      }
      else if (mask[j*wid+i) & NEGATIVE_CHARGE) {
        if      (mask[(j-1)*wid+i-1] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i-1] |= IN_CUT; 
	}
        else if (mask[(j-1)*wid+i  ] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i  ] |= IN_CUT; 
	}
        else if (mask[(j-1)*wid+i+1] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j-1)*wid+i+1] |= IN_CUT; 
	}
        else if (mask[(j  )*wid+i-1] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i-1] |= IN_CUT; 
	}
        else if (mask[(j  )*wid+i+1] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j  )*wid+i+1] |= IN_CUT; 
	}
        else if (mask[(j+1)*wid+i-1] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i-1] |= IN_CUT; 
	}
        else if (mask[(j+1)*wid+i  ] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i  ] |= IN_CUT; 
	}
        else if (mask[(j+1)*wid+i+1] & POSITIVE_CHARGE) { 
	  mask[j*wid+i] |= IN_CUT; mask[(j+1)*wid+i+1] |= IN_CUT; 
	}
      }

    }
  }     /* end of sites loop */
#endif

  /* initialize the number of points in 'list' to zero */
  list.n = 0;

  /* loop over (wid-3)x(len-3) residue sites */
  for (j = 1; j < len-2; j++) {
    register Uchar *maskLineStart=mask+wid*j;
    if (!(j%(len/8)))  
      asfPrintStatus("     ...at %d of %d\n", j, len);
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

  return;
}

/*
 * generateCut(int i, int j) is passed a coordinate within the image
 * which contains a charge, either + or -.  The job of generateCut() is to
 * install a branch cut in the mask array which includes the point (i, j)
 * and which has a total charge of zero. Furthermore, we want the
 * number of points involved in the branch cut to be minimized.
 */
void generateCut(int i, int j)
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
    Exit("   generateCut() called with no charge at (%d,%d)",i,j);

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
    asfPrintStatus("(%d, %d), maxR = %d, r = %d, rmo = %d, list.n = %d\n",
      i, j, maxR, r, rmo, list.n);
    asfPrintError("Error in generateCut()");
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
    asfPrintStatus("   at point (%d, %d), the debug test for IN_CUT returned:"
		   "\n",i,j);
    asfPrintStatus("   \t%d bad of %d in the list\n", count, list.n);
    for (point = 0; point < list.n; point++) {
      point_i = list.p[point].i;
      point_j = list.p[point].j;
      if (!(mask[ point_j*wid + point_i] & IN_CUT)) { count++; }
      asfPrintStatus("   %d: (%d, %d)\n\tmask %d\n\tmask & IN_CUT %d\n",
        point, point_i, point_j, (int)(mask[point_j*wid+point_i)),
        (int)(mask[point_j*wid+point_i] & IN_CUT));
      asfPrintStatus("   \tmask & GROUNDED %d\n\tmask & SOME_CHARGE %d\n",
        (int)(mask[point_j*wid+point_i] & GROUNDED),
        (int)(mask[point_j*wid+point_i] & SOME_CHARGE));
    }
    Exit("   generateCut() failed logical test");
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

void finishUwp(void)
{
  int i, j;

  for (j = 0; j < len; j++) {
    register float *lineStart=&phase[wid*j];
    for (i = 0; i < wid; i++)
      if (!(mask[j*wid+i]&INTEGRATED))
       *(lineStart+i) = 0.0;/*Set non-integrated phases to zero*/
  }

  return;
}

void saveUwp(char *f)
{
  meta_parameters *meta;

  FILE *fd = FOPEN(f, "wb");
  meta = meta_read(f);
  put_float_lines(fd, meta, 0, len, phase);
  FCLOSE(fd);

  return;
}


void integratePhase(int i, int j)
{
  int    u, v;      /* starting point coordinates                         */
  Uchar  s;         /* temp status value                                  */
  int    t = 0;     /* total number of pixels integrated for this region  */
  int    madeJump;  /* indicates an integration jump has been made        */

  /* initialize things */
  /* set 'source' bits for seed point in 'im' to 0; no source! */
  im[j*wid+i]   &= 0x0f;

  /* set unwrapped phase to seed value p0                      */
  /*  *(uwp+j*wid+i)   = p0; <--IMPLICIT*/

  /* label seed point as integrated                            */
  mask[j*wid+i] |= INTEGRATED;

  /* increment integration counter                             */
  t++;

  /* start out (u, v) = (i, j), a fall-through the while loop condition */
  u = i; v = j;

  /*
   * this section tries to initialize (u, v) to a 4-nbr starting point.
   */

  /* try 1:  go up one pixel to (i, j-1) */
  if (!(mask[(j-1)*wid+i] & IICG)){
    v--; t++;
    im[ j*wid + i] |= TRIED_U;
    im[ v*wid + u] |= SOURCE_B;
    mask[ v*wid + u] |= INTEGRATED;
    phase[v*wid + u]  = phase[j*wid+i] +
                           phaseRemap((phase[v*wid+u]) -
                           (phase[j*wid+i]));
    asfPrintStatus("   from seed point, started out by going up...");
  }

  /* try 2:  go right one pixel to (i + 1, j) */
  else if (!(mask[j*wid+(i+1)] & IICG)){
    u++; t++;
    im[ j*wid + i] |= TRIED_UR;
    im[ v*wid + u] |= SOURCE_L;
    mask[ v*wid + u] |= INTEGRATED;
    phase[ v*wid + u]  = phase[j*wid+i] +
                           phaseRemap((phase[v*wid+u]) -
                           (phase[j*wid+i]));
    asfPrintStatus("   from seed point, started out by going right...\n");
  }

  /* try 3:  go down one pixel to (i, j + 1) */
  else if (!(mask[(j+1)*wid+i] & IICG)){
    v++; t++;
    im[ j*wid + i] |= TRIED_URD;
    im[ v*wid + u] |= SOURCE_A;
    mask[ v*wid + u] |= INTEGRATED;
    phase[ v*wid + u]  = phase[j*wid+i] +
                           phaseRemap((phase[v*wid+u]) -
                           (phase[j*wid+i]));
    asfPrintStatus("   from seed point, started out by going down...\n");
  }

  /* try 4:  go left one pixel to (i - 1, j) */
  else if (!(mask[j*wid+(i-1)] & IICG)){
    u--; t++;
    im[ j*wid + i] |= TRIED_URDL;
    im[ v*wid + u] |= SOURCE_R;
    mask[ v*wid + u] |= INTEGRATED;
    phase[ v*wid + u]  = phase[j*wid+i] +
                           phaseRemap((phase[v*wid+u]) -
                           (phase[j*wid+i]));
    asfPrintStatus("\n   from seed point, started out by going left...\n");
  }

  /* fall through:  No good 4-nbrs found */
  else {
    im[ j*wid + i] |= TRIED_URDL;
  }

  /*
   * this section is a big 'while not done'
   *   Keep on going inside the while() as long as:
   *     u is not back to i, and
   *     v is not back to j, and
   *     the TRIED_L bit is not set *at the seed point (i, j)*.
   *  This way we only fall out when we get back to (i, j) and all search
   *    directions are exhausted.
   */
  while (u != i || v != j || !(im[j*wid+i] & TRIED_L)){

    if (!(t%100000)) asfPrintStatus ("\r   total integrated = %d", t);

    /* s = temp value of 'im' at pixel (u, v) */
    s        = im[v*wid + u];
    madeJump = FALSE;

    /*
     * The following series of ifs assume we want to test successively
     *   for jump directions until a good one is found.  When this happens,
     *   the jump is made and the remaining ifs are skipped by virtue of
     *   a boolean 'madeJump' set to TRUE.
     */

    /*
     * Start at (u, v); no jumps tried from here yet?
     */
    if (!(s & TRIED_URDL)) {

      /* if this is the case, then set TRIED_U since we're about to try 'up' */
      im[ v*wid + u] |= TRIED_U;

      if (!(mask[(v-1)*wid+u] & IICG)) {
        v--; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_B;
        mask[ v*wid + u] |= INTEGRATED;
        phase[ v*wid + u]  = phase[(v+1)*wid + u] +
                               phaseRemap(phase[v*wid+u] -
                               phase[(v+1)*wid+u]);
      }

    }

    /* Have we tried 'up' (and no others)? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_U)) {

      /* if this is the case,
         then set TRIED_R since we're about to try 'right' */
      im[ v*wid + u] |= TRIED_R;

      /* check if potential destination is not integrated,
         not cut, and not grounded */
      if (!(mask[v*wid+(u+1)] & IICG)) {
        u++; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_L;
        mask[ v*wid + u] |= INTEGRATED;
        phase[ v*wid + u]  = phase[ v*wid + (u-1)] +
                               phaseRemap(phase[v*wid+u] -
                                 phase[v*wid+(u-1)]);
      }

    }

    /* Have we tried 'up' and 'right'? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_UR)) {

      /* if this is the case, then set TRIED_D since
         we're about to try 'down' */
      im[ v*wid + u] |= TRIED_D;

      /* check if potential destination is
         not integrated, not cut, and not grounded */
      if (!(mask[(v+1)*wid+u] & IICG)) {
        v++; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_A;
        mask[ v*wid + u] |= INTEGRATED;
        phase[ v*wid + u]  = phase[ (v-1)*wid + u] +
                               phaseRemap(phase[v*wid+u] -
                                 phase[(v-1)*wid+u]);
      }

    }

    /* Have we tried 'up', 'right', and 'down'? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_URD)) {

      /* if this is the case,
         then set TRIED_L since we're about to try 'left' */
      im[ v*wid + u] |= TRIED_L;

      /* check if potential destination is
         not integrated, not cut, and not grounded */
      if (!(mask[v*wid+(u-1)] & IICG)) {
        u--; t++;
        madeJump = TRUE;
        im[ v*wid + u] |= SOURCE_R;
        mask[ v*wid + u] |= INTEGRATED;
        phase[ v*wid + u]  = phase[v*wid + (u+1)] +
                               phaseRemap(phase[v*wid+u] -
                                 phase[v*wid+(u+1)]);
      }

    }

    /* Have we tried 'up', 'right', 'down', and 'left'? */
    if (!madeJump && ((s & TRIED_URDL) == TRIED_URDL)) {

      /* if so, then all we can do is back up */
      if      (s & SOURCE_B) { v++; }
      else if (s & SOURCE_L) { u--; }
      else if (s & SOURCE_A) { v--; }
      else if (s & SOURCE_R) { u++; }
      else {
         /* Don't change (u, v) if all source bits are
          *   0, as this means that we are back at the
          *   seed point (i, j).
          */
      }
    }

  }  /* end of the big 'while-not-done' loop */

  asfPrintStatus("\nUnwrapped %d pixels...\n", t);
  return;
}


void doStats()
{
  int    i, j, k;
  int    nZero     = 0;
  int    nPlus     = 0;
  int    nMinus    = 0;
  int    nGround   = 0;
  int    nCut      = 0;
  int    nInteg    = 0;
  int    nInTree   = 0;
  float total     = (float)(len*wid);

  for (j = 0; j < len; j++) {
    register Uchar *lineStart=mask+wid*j;
    for (i = 0; i < wid; i++) {

      k = (int)(*(lineStart+i));

      if (!k)                  { nZero++;   }
      if (k & POSITIVE_CHARGE) { nPlus++;   }
      if (k & NEGATIVE_CHARGE) { nMinus++;  }
      if (k & IN_CUT)          { nCut++;   }
      if (k & GROUNDED)        { nGround++; }
      if (k & INTEGRATED)      { nInteg++; }
      if (k & IN_TREE)         { nInTree++; }

    }
  }

  asfPrintStatus ("   %9d pixels                         \n", (int)(total));
  asfPrintStatus ("   %9d unknown     %7.3f %%\n", 
		  nZero, 100.0*(float)(nZero)/total);
  asfPrintStatus ("   %9d unwrapped   %7.3f %%\n", 
		  nInteg, 100.0*(float)(nInteg)/total);
  asfPrintStatus ("   %9d residues    %7.3f %%\n", 
		  nPlus + nMinus, 100.0*(float)(nPlus + nMinus)/total);
  asfPrintStatus ("->  %9d +residues   %7.3f %%\n", 
		  nPlus, 100.0*(float)(nPlus)/total);
  asfPrintStatus ("->  %9d -residues   %7.3f %%\n", 
		  nMinus, 100.0*(float)(nMinus)/total);
  asfPrintStatus ("    %9d grounds     %7.3f %%\n", 
		  nGround, 100.0*(float)(nGround)/total);
  asfPrintStatus ("    %9d in tree     %7.3f %%\n", 
		  nInTree, 100.0*(float)(nInTree)/total);
  asfPrintStatus ("    %9d cuts        %7.3f %%\n", 
		  nCut, 100.0*(float)(nCut)/total);
  asfPrintStatus ("\n");

  if (nInTree) { 
    asfPrintStatus ("\n\nnote that number in tree != 0.\n\n"); 
  }

  return;
}

int escher(char *inFile, char *outFile)
{
  int seedX=-1,seedY=-1; 
  char szWrap[MAXNAME], szUnwrap[MAXNAME];
  meta_parameters *meta;

  create_name(szWrap, inFile, ".img");
  create_name(szUnwrap, outFile, ".img");

  meta = meta_read(szWrap);
  wid = meta->general->sample_count;
  len = meta->general->line_count;
  if ((seedX == -1)&&(seedY == -1))
  {
    seedX = wid/2;
    seedY = len/2;
  }
  
  meta_write(meta, szUnwrap);

  size = wid*len;
  mask = (Uchar *)calloc(size, sizeof(Uchar));
  im = (Uchar *)calloc(size, sizeof(Uchar));
  phase = (float *)MALLOC(sizeof(float)*size);

  /* perform steps*/
  asfPrintStatus("\nGenerating phase unwrapping mask ...\n\n");
  loadWrappedPhase(szWrap);
  groundBorder();
  makeMask();
  doStats();
  asfPrintStatus("\n\nGrounding remaining residues ...\n\n");
  installCordon("cordon");
  asfPrintStatus("\n\nDefining branch cuts ...\n\n");
  cutMask();
  doStats();

#if DO_DEBUG_CHECKS
  saveMask((char *)mask, "test");

  verifyCuts();                                           
#endif

  asfPrintStatus("\n\nIntegrating the phase ...\n\n");
  checkSeed(&seedX, &seedY);
  integratePhase(seedX, seedY);
  doStats();
  finishUwp();
  saveMask(mask, szUnwrap);
  saveUwp(szUnwrap);
  
  // Clean up
  FREE(mask);
  FREE(im);
  FREE(phase);

  return(0);
}
