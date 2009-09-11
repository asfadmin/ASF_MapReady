#include "asf.h"
#include "ceos.h"

void prn_raddr(char *file, struct VRADDR *dr)
{
  FILE *fp;
  int i;
  
  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Radiometric Data record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Radiometric Data record</h2>\n");
  fprintf(fp, "<strong>Radiometric Data Record Sequence Number: </strong>%i<br>\n", dr->seqnum );
  fprintf(fp, "<strong>Number of radiometric data fields: </strong>%i<br>\n", dr->datfield );
  fprintf(fp, "<strong>Radiometric data set size in bytes: </strong>%i<br>\n", dr->setsize );
  fprintf(fp, "<strong>SAR channel indicator: </strong>%s<br>\n", dr->sarchan );
  fprintf(fp, "<strong>Look Up Table Designator: </strong>%s<br>\n", dr->luttype );
  fprintf(fp, "<strong>Number of samples in Look Up Table: </strong>%i<br>\n", dr->nosample );
  fprintf(fp, "<strong>Sample Type Designator: </strong>%s<br>\n", dr->samptype );
  fprintf(fp, "<strong>Calibration coefficient a1: </strong>%e<br>\n", dr->a[0] );
  fprintf(fp, "<strong>Calibration coefficient a2: </strong>%e<br>\n", dr->a[1] );
  fprintf(fp, "<strong>Calibration coefficient a3: </strong>%e<br>\n", dr->a[2] );
  for (i = 0; i < dr->nosample; i +=4)
    {
      fprintf(fp, "<strong>Noise Values %3d - %3d: </strong>",i+1,i+4);
      fprintf(fp, "%12.7f",dr->noise[i]);
      fprintf(fp, "%12.7f",dr->noise[i+1]);
      fprintf(fp, "%12.7f",dr->noise[i+2]);
      fprintf(fp, "%12.7f<br>\n",dr->noise[i+3]);
    }
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);

  return;
}

