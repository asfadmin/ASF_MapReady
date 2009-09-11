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

char *sprn_rsi_raddr(struct RSI_VRADDR *dr)
{
 int i;

 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 add(&ret, "\n********** begin of Radiometric Data record (RSI/CDPF/CSTARS) **********\n\n");
 add(&ret, "Radiometric Data Record Sequence Number : %i\n", dr->seq_num);
 add(&ret, "Number of radiometric data sets         : %i\n", dr->n_data);
 add(&ret, "Radiometric data set size in bytes      : %i\n", dr->field_size);
 add(&ret, "SAR channel indicator                   : %s\n", dr->chan_ind);
 add(&ret, "Look Up Table Designator                : %s\n", dr->table_desig);
 add(&ret, "Number of samples in Look Up Table      : %i\n", dr->n_samp);
 add(&ret, "Sample Type Designator                  : %s\n", dr->samp_type);
 add(&ret, "Increment between table entries         : %i\n", dr->samp_inc);
 for (i = 0; i < dr->n_samp; i +=4) {
   add(&ret, "Values %3d - %3d :",i+1,i+4);
   add(&ret, "%15.3f",dr->lookup_tab[i]);
   add(&ret, "%15.3f",dr->lookup_tab[i+1]);
   add(&ret, "%15.3f",dr->lookup_tab[i+2]);
   add(&ret, "%15.3f\n",dr->lookup_tab[i+3]);
 }
 add(&ret, "Thermal noise reference level (dB)      : %lf\n", dr->noise_scale);
 add(&ret, "Scaling offset                          : %e\n", dr->offset);
 add(&ret, "Calibration constant                    : %e\n", dr->calib_const);
 add(&ret, "*********** end of Radiometric Data record ******************\n\n");
 return ret;
}

void prn_rsi_raddr(FILE *fp, struct RSI_VRADDR *dr)
{
    char *rec = sprn_rsi_raddr(dr);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

