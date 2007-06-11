/*Escher:
Routines dealing with the charge mask.
*/
#include "escher.h"

void 
makeMask()
{
  int i, j;
  float p0, p1, p2, p3;

  printf("   starting to make mask...\n");
  for (j = 1; j < len - 2; j++) {
    for (i = 1; i < wid - 2; i++) {
      if (0.0==phase[wid*(j  )+i])
         mask[j*wid+i] |= GROUNDED;/*Ground out zero-phases (e.g., layover regions)*/
      p0 = phase[wid*(j  )+i  ];
      p1 = phase[wid*(j+1)+i  ];
      p2 = phase[wid*(j+1)+i+1];
      p3 = phase[wid*(j  )+i+1];
      mask[j*wid+i] |= chargeCalc(p0,p1,p2,p3);
    } 
  }
  printf ("    - made mask\n");
  return;
}

Uchar 
chargeCalc(float p0, float p1, float p2, float p3)
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
  printf("\n\n");
  printf("    p0 = %f\n", p0);
  printf("    p1 = %f\n", p1);
  printf("    p2 = %f\n", p2);
  printf("    p3 = %f\n", p3);
  printf("\n");
  printf("    od0 = %f\n", od0);
  printf("    od1 = %f\n", od1);
  printf("    od2 = %f\n", od2);
  printf("    od3 = %f\n", od3);
  printf("\n");
  printf("    d0 = %f\n", d0);
  printf("    d1 = %f\n", d1);
  printf("    d2 = %f\n", d2);
  printf("    d3 = %f\n", d3);
  printf("\n");
  printf("   sum = %f\n", sum);
  printf("\n\n");
  Exit("chargeCalc(): bad residue calculation");
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
    printf("\ngrounded out %d points from cordon file '%s'\n\n",
      n, cordonFnm);
    doStats("after cordon installed:");
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
  printf("   saved a mask array to the file %s ...", fnm);
  return;
}

void readMask(unsigned char *m, char *fnm)
{
  if (!size) 
    Exit("   escher:  readMask():  zero mask size");
  readVector(m, fnm, CHAR, size);
  printf("   read mask from file %s ...\n", fnm);
  return;
}

