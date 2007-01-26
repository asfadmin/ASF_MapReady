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

char *sprn_raddr(struct VRADDR *dr)
{
 int i;

 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 add(&ret, "\n********** begin of Radiometric Data record *****************\n\n");
 add(&ret, "Radiometric Data Record Sequence Number : %i\n", dr->seqnum );
 add(&ret, "Number of radiometric data fields       : %i\n", dr->datfield );
 add(&ret, "Radiometric data set size in bytes      : %i\n", dr->setsize );
 add(&ret, "SAR channel indicator                   : %s\n", dr->sarchan );
 add(&ret, "Look Up Table Designator                : %s\n", dr->luttype );
 add(&ret, "Number of samples in Look Up Table      : %i\n", dr->nosample );
 add(&ret, "Sample Type Designator                  : %s\n", dr->samptype );
 add(&ret, "Calibration coefficient a1              : %e\n", dr->a[0] );
 add(&ret, "Calibration coefficient a2              : %e\n", dr->a[1] );
 add(&ret, "Calibration coefficient a3              : %e\n", dr->a[2] );
 for (i = 0; i < dr->nosample; i +=4)
  {
    add(&ret, "Noise Values %3d - %3d :",i+1,i+4);
    add(&ret, "%12.7f",dr->noise[i]);
    add(&ret, "%12.7f",dr->noise[i+1]);
    add(&ret, "%12.7f",dr->noise[i+2]);
    add(&ret, "%12.7f\n",dr->noise[i+3]);
  }
 add(&ret, "*********** end of Radiometric Data record ******************\n\n");
 return ret;
}

void prn_raddr(FILE *fp, struct VRADDR *dr)
{
    char *rec = sprn_raddr(dr);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

