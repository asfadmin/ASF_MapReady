/*Escher:
Debugging tools, utilities, etc.*/
#include "escher.h"


/* 
 * This function is a cursory test to check for 'residual' residues.
 *   If 'escher' is working properly it should give a null result.
 */
void 
verifyCuts(void)
{
  int i, j, nSites = 0, nResidues = 0;
  float p0, p1, p2, p3;

  printf("starting cut verification...\n");

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

  printf("  Found %d add'l residues out of %d sites, %f percent\n",
    nResidues, nSites, 100.0*(float)(nResidues)/(float)(nSites));
  printf(" - verified cuts\n");
  return;
}

