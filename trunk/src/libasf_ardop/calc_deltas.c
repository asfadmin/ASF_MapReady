#include "asf.h"

int calc_deltas(char *inFile1, char *inFile2, int nLines, char *outFile)
{
  FILE *fp;
  float mx1, bx1, my1, by1;
  float mx2, bx2, my2, by2;
  float delt_m_x, delt_b_x, delt_m_y, delt_b_y;
  
  // Read first input file
  fp = FOPEN(inFile1, "r");
  fscanf(fp, "%f %f\n", &mx1, &bx1);
  fscanf(fp, "%f %f\n", &my1, &by1);
  FCLOSE(fp);
  
  // Read second input file
  fp = FOPEN(inFile2, "r");
  fscanf(fp, "%f %f\n", &mx2, &bx2);
  fscanf(fp, "%f %f\n", &my2, &by2);
  FCLOSE(fp);
  
  delt_m_x = (mx2 - mx1) / nLines;
  delt_b_x = (bx2 - bx1) / nLines;
  delt_m_y = (my2 - my1) / nLines;
  delt_b_y = (by2 - by1) / nLines;
  
  // Write the output file
  fp = FOPEN(outFile, "w");
  fprintf(fp, "%f %f %f %f\n", mx1, bx1, my1, by1);
  fprintf(fp, "%g %g %g %g\n", delt_m_x, delt_b_x, delt_m_y, delt_b_y);
  FCLOSE(fp);

  // Keep the user informed
  asfPrintStatus("   u1 = %f x + %f\n", mx1, bx1);
  asfPrintStatus("   v1 = %f y + %f\n", my1, by1);
  asfPrintStatus("   u8 = %f x + %f\n", mx2, bx2);
  asfPrintStatus("   v8 = %f y + %f\n\n", my2, by2);
  asfPrintStatus("   delu = %f x + %f\n", delt_m_x, delt_b_x);
  asfPrintStatus("   delv = %f y + %f\n\n", delt_m_y, delt_b_y);

}
