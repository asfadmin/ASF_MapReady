/*Escher:
Utilities for loading/saving arrays to files,
etc.*/
#include "escher.h"
#include "asf_endian.h"


void 
loadWrappedPhase(char *f)
{
  FILE * fd;
  meta_parameters *meta;

  printf ("   loading wrapped phase...\n");

  fd = FOPEN(f, "rb");
  meta = meta_read(f);
  get_float_lines(fd, meta, 0, len, phase);
  FCLOSE(fd);

  printf("   wrapped phase data loaded...\n");
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

  printf("    - grounded border\n");
  return;
}


float 
phaseRemap(float p)
{
  p=(double)fmod((double)p,(double)TWOPI);
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
    printf("\n   seed point (%d, %d) is not ZERO.\n", *x, *y);
    /*Pick a new, random seed point.*/
    *x=(rand()&0x7fff)*wid/0x7fff;
    *y=(rand()&0x7fff)*len/0x7fff;
    printf("\n   auto-adjusted seed point to (%d, %d).\n", *x, *y);
  }
  printf("\n   checkSeed() finished\n\n");
  return;
}


void doStats(char *msg)
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

  printf ("                           \n");
  printf ("   doStats():  %s          \n", msg);
  printf ("                           \n");

  printf ("       %9d pixels                         \n", (int)(total));
  printf ("       %9d unknown     %7.3f %%\n", nZero, 100.0*(float)(nZero)/total);
  printf ("       %9d unwrapped   %7.3f %%\n", nInteg, 100.0*(float)(nInteg)/total);
  printf ("       %9d residues    %7.3f %%\n", nPlus + nMinus, 
	  100.0*(float)(nPlus + nMinus)/total);
  printf ("   ->  %9d +residues   %7.3f %%\n", nPlus, 100.0*(float)(nPlus)/total);
  printf ("   ->  %9d -residues   %7.3f %%\n", nMinus, 100.0*(float)(nMinus)/total);
  printf ("       %9d grounds     %7.3f %%\n", nGround, 100.0*(float)(nGround)/total);
  printf ("       %9d in tree     %7.3f %%\n", nInTree, 100.0*(float)(nInTree)/total);
  printf ("       %9d cuts        %7.3f %%\n", nCut, 100.0*(float)(nCut)/total);
  printf ("                                      \n");

  if (nInTree) { printf ("\n\n   note that number in tree != 0.\n\n"); }

  return;
}
