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

char *sprn_ardr(struct alos_rad_data_rec *dr)
{
 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 add(&ret, "\n********** begin of Radiometric Data record (ALOS) *************\n\n");
 add(&ret, "Radiometric Data Record Sequence Number : %i\n", dr->sequence_num );
 add(&ret, "Number of radiometric data fields       : %i\n", dr->number_rec );
 add(&ret, "Calibration factor                      : %lf\n", 
     dr->calibration_factor );
 add(&ret, "Transmission distortion matrix real(1,1): %lf\n", 
     dr->delta_trans_real[0] );
 add(&ret, "Transmission distortion matrix imag(1,1): %lf\n", 
     dr->delta_trans_imag[0] );
 add(&ret, "Transmission distortion matrix real(1,2): %lf\n", 
     dr->delta_trans_real[1] );
 add(&ret, "Transmission distortion matrix imag(1,2): %lf\n", 
     dr->delta_trans_imag[1] );
 add(&ret, "Transmission distortion matrix real(2,1): %lf\n", 
     dr->delta_trans_real[2] );
 add(&ret, "Transmission distortion matrix imag(2,1): %lf\n", 
     dr->delta_trans_imag[2] );
 add(&ret, "Transmission distortion matrix real(2,2): %lf\n", 
     dr->delta_trans_real[3] );
 add(&ret, "Transmission distortion matrix imag(2,2): %lf\n", 
     dr->delta_trans_imag[3] );
 add(&ret, "Reception distortion matrix real(1,1)   : %lf\n", 
     dr->delta_trans_real[0] );
 add(&ret, "Reception distortion matrix imag(1,1)   : %lf\n", 
     dr->delta_trans_imag[0] );
 add(&ret, "Reception distortion matrix real(1,2)   : %lf\n", 
     dr->delta_trans_real[1] );
 add(&ret, "Reception distortion matrix imag(1,2)   : %lf\n", 
     dr->delta_trans_imag[1] );
 add(&ret, "Reception distortion matrix real(2,1)   : %lf\n", 
     dr->delta_trans_real[2] );
 add(&ret, "Reception distortion matrix imag(2,1)   : %lf\n", 
     dr->delta_trans_imag[2] );
 add(&ret, "Reception distortion matrix real(2,2)   : %lf\n", 
     dr->delta_trans_real[3] );
 add(&ret, "Reception distortion matrix imag(2,2)   : %lf\n", 
     dr->delta_trans_imag[3] );
 add(&ret, "*********** end of Radiometric Data record ******************\n\n");
 return ret;
}

void prn_ardr(FILE *fp, struct alos_rad_data_rec *dr)
{
    char *rec = sprn_ardr(dr);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

