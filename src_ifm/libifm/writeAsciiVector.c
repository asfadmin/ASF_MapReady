/****************************************************************
FUNCTION NAME:  writeAsciiVector

SYNTAX:  void writeAsciiVector(v,fnm,type,n);

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    v		void *		source vector
    fnm		char *		destination filename
    type	data_t		data type
    n		int		number of elements

DESCRIPTION:
    Saves a vector to the specified file name. If it cannot open the file for
    whatever reason, it will not save the data.

    The file is saved as an ASCII file.

RETURN VALUE:
    None.

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
   1.0 - Rob Fatland & Mike Shindle - original development
****************************************************************/
#include "asf.h"
#include "ifm.h"

void writeAsciiVector(void *v, char *fnm, data_t type, int n)
{
  int  i;
  FILE *fp;
  char *c1;
  Uchar *u1;
  int *i1;
  short int *s1;
  float *f1;
  double *d1;
  complexFloat *C1;
  complexDouble *D1;


  fp = FOPEN (fnm, "a");
  /************************************************************
  ****************fopen was replaced by FOPEN above.***********
  ******error message below is not needed anymore**************
  if (fp == NULL) 
    Alert("writeAsciiVector(): cannot open file '%s'", fnm);
  *************************************************************/  
  /* simple integer types */
  if (type == INT || type == INTEGER) {
    i1 = (int *)(v);
    for (i = 0; i < n; i++)  
      fprintf(fp, "%d\n", (int)(*(i1+i))); 
  }
  else if (type == SHORT_INT) {
    s1 = (short int *)(v);
    for (i = 0; i < n; i++)  
      fprintf(fp, "%d\n", (int)(*(s1+i))); 
  }
  else if (type == CHAR) {
    c1 = (char *)(v);
    for (i = 0; i < n; i++) 
      fprintf(fp, "%d\n", (int)(*(c1+i))); 
  }
  else if (type == UCHAR) {
    u1 = (unsigned char *)(v);
    for (i = 0; i < n; i++) 
      fprintf(fp, "%d\n", (int)(*(u1+i))); 
  }
  else if (type == FLOAT) {
    f1 = (float *)(v);
    for (i = 0; i < n; i++) 
      fprintf(fp, "%f\n", (float)(*(f1+i))); 
  }
  else if (type == DOUBLE) {
    d1 = (double *)(v);
    for (i = 0; i < n; i++) 
      fprintf(fp, "%f\n", (double)(*(d1+i))); 
  }
  else if (type == FLOAT_COMPLEX) {
    C1 = (complexFloat *)(v);
    for (i = 0; i < n; i++)  
      fprintf(fp, "%f %f\n", (C1+i)->real, (C1+i)->imag); 
  }
  else if (type == COMPLEX || type == DOUBLE_COMPLEX) {
    D1 = (complexDouble *)(v);
    for (i = 0; i < n; i++)  
      fprintf(fp, "%f %f\n", (D1+i)->real, (D1+i)->imag); 
  }

  fclose(fp);
  return;
}
