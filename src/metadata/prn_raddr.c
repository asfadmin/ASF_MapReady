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

void prn_raddr(struct VRADDR *dr)
{
 int i;

 printf("\n********** begin of Radiometric Data record *****************\n\n");
 printf("Radiometric Data Record Sequence Number : %i\n", dr->seqnum );
 printf("Number of radiometric data fields       : %i\n", dr->datfield );
 printf("Radiometric data set size in bytes      : %i\n", dr->setsize );
 printf("SAR channel indicator                   : %s\n", dr->sarchan );
 printf("Look Up Table Designator                : %s\n", dr->luttype );
 printf("Number of samples in Look Up Table      : %i\n", dr->nosample );
 printf("Sample Type Designator                  : %s\n", dr->samptype );
 printf("Calibration coefficient a1              : %e\n", dr->a[0] );
 printf("Calibration coefficient a2              : %e\n", dr->a[1] );
 printf("Calibration coefficient a3              : %e\n", dr->a[2] );
 for (i = 0; i < dr->nosample; i +=4)
  {
    printf("Noise Values %3d - %3d :",i+1,i+4);
    printf("%12.7f",dr->noise[i]);
    printf("%12.7f",dr->noise[i+1]);
    printf("%12.7f",dr->noise[i+2]);
    printf("%12.7f\n",dr->noise[i+3]);
  }
 printf("*********** end of Radiometric Data record ******************\n\n");
 return;
}

