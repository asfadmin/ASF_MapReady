/********************************************************************
NAME: check_cal

SYNOPSIS: check_cal(char *filename)

DESCRIPTION:
  Checks calibration status field of the data quality summary record.
  If the data is not calibrated, a message is displayed and 0 is returned.
  If the data has inferred calibration, a message is displayed and 1 is
  returned.  Otherwise 1 is returned.

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           10/96   T. Logan (ASF) 
*********************************************************************/
#include <stdio.h>
#include "ceos.h"
#include "calibrate.h"
#define VERSION 1.00

int check_cal(char *filename)
{
 struct qual_sum_rec    *dqsr;
 char   file[256];
 int    era;

 era = set_era(filename,file,-1);
 if (!era) return(1);
 else
 {
  dqsr=(struct qual_sum_rec*)malloc(sizeof(struct qual_sum_rec));
  if (get_dqsr(file,dqsr) == -1) return(1);

  if (strncmp(dqsr->cal_status,"UNCALIB",7)==0)
   {
     printf("\7\7\n\n**********  UNCALIBRATED DATA  **********  \7\7\n");
     printf("Calibration Comments: %s\n",dqsr->cal_comment);
     free(dqsr);
     return(0);
   }
  else if (strncmp(dqsr->cal_status,"INFERRE",7)==0)
   {
     printf("INFERRED CALIBRATION DATA\n");
     printf("Calibration Comments: %s\n",dqsr->cal_comment);
     free(dqsr);
     return(1);
   }
  else if (strncmp(dqsr->cal_status,"CALIBRA",7)==0)
   {
     printf("Calibration Comments: %s\n",dqsr->cal_comment);
     free(dqsr);
     return(1);
   }
  else 
   {
     printf("\7\7\n\n****** UNABLE TO DETERMINE CALIBRATION OF DATA ******\n"); 
     printf("Calibration Comments: %s\n",dqsr->cal_comment);
     free(dqsr);
     return(0);
   }
 }
}
