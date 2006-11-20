/********************************************************************
NAME:     print_value_raddr.c --  print radiometric data value record

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           10/93   T. Logan (ASF)
  1.1           1/97    M. Shindle (ASF) - fixed error with dr->nosample
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

void prn_raddr(FILE *fp, struct VRADDR *dr)
{
 int i;

 fprintf(fp, "\n********** begin of Radiometric Data record *****************\n\n");
 fprintf(fp, "Radiometric Data Record Sequence Number : %i\n", dr->seqnum );
 fprintf(fp, "Number of radiometric data fields       : %i\n", dr->datfield );
 fprintf(fp, "Radiometric data set size in bytes      : %i\n", dr->setsize );
 fprintf(fp, "SAR channel indicator                   : %s\n", dr->sarchan );
 fprintf(fp, "Look Up Table Designator                : %s\n", dr->luttype );
 fprintf(fp, "Number of samples in Look Up Table      : %i\n", dr->nosample );
 fprintf(fp, "Sample Type Designator                  : %s\n", dr->samptype );
 fprintf(fp, "Calibration coefficient a1              : %e\n", dr->a[0] );
 fprintf(fp, "Calibration coefficient a2              : %e\n", dr->a[1] );
 fprintf(fp, "Calibration coefficient a3              : %e\n", dr->a[2] );
 for (i = 0; i < dr->nosample; i +=4)
  {
    fprintf(fp, "Noise Values %3d - %3d :",i+1,i+4);
    fprintf(fp, "%12.7f",dr->noise[i]);
    fprintf(fp, "%12.7f",dr->noise[i+1]);
    fprintf(fp, "%12.7f",dr->noise[i+2]);
    fprintf(fp, "%12.7f\n",dr->noise[i+3]);
  }
 fprintf(fp, "*********** end of Radiometric Data record ******************\n\n");
 return;
}

