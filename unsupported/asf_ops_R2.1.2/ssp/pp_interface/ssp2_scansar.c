/* SccsId[]= @(#)ssp2_scansar.c	2.41 3/24/98 */
static char sccsid_ssp2_scansar[]= "@(#)PPssp2_scansar.c:2.41";

#include <stdio.h>
#include <time.h>
#include "error.h"

int ssp2_scansar(file_path,file_config,subsystem_name,time_file)
char *file_config;
char *file_path;
char *subsystem_name;
char *time_file;
{
int icode;
char *cmd1,*cmd2,*cmd3,*cmd4,*cmd5;
double before_t,after_t;
char buff[300];
FILE   *TIME_FILE_FP;








/*****************/
cmd1=(char *)malloc(sizeof(char)*(strlen(file_config)+256));
cmd2=(char *)malloc(sizeof(char)*(strlen(file_config)+256));
cmd3=(char *)malloc(sizeof(char)*(strlen(file_config)+256));
cmd4=(char *)malloc(sizeof(char)*(strlen(file_config)+256));
cmd5=(char *)malloc(sizeof(char)*(strlen(file_config)+256));
strcpy(cmd1,file_path);
strcpy(cmd2,file_path);
strcpy(cmd3,file_path);
strcpy(cmd4,file_path);
strcpy(cmd5,file_path);
strcat(cmd1,"/run.transfer  ");
strcat(cmd2,"/run.pre_processor  ");
strcat(cmd3,"/run.correlator  ");
strcat(cmd4,"/run.post_processor  ");
strcat(cmd5,"/run.ceos  ");
strcat(cmd1,file_config);
strcat(cmd2,file_config);
strcat(cmd3,file_config);
strcat(cmd4,file_config);
strcat(cmd5,file_config);
strcat(cmd1," ");
strcat(cmd2," ");
strcat(cmd3," ");
strcat(cmd4," ");
strcat(cmd5," ");
strcat(cmd1,subsystem_name);
strcat(cmd2,subsystem_name);
strcat(cmd3,subsystem_name);
strcat(cmd4,subsystem_name);
strcat(cmd5,subsystem_name);

TIME_FILE_FP=fopen(time_file,"a");
if (TIME_FILE_FP == NULL)
 {
  printf("cant open the %s\n",time_file);
  sprintf(buff,"cannot open %s\n",time_file);
  printclog(1,buff);
  printerr(time_file);
  return(ierr_2);
 }
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[data transfer    ] %s","IN PROGRESS");
printclog(2,buff);
icode=system(cmd1);
after_t=rtc();
fprintf(TIME_FILE_FP,"%lf\n",after_t-before_t);
icode=icode/256;
if ((icode < 0) || (icode >0))
 {
  if (icode<0)
   {
    sprintf(buff,"[data transfer    ] %s","SYSTEM INTERRUPT");
    printclog(2,buff);
   }
  if (icode>0)
   {
    sprintf(buff,"[data transfer    ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
   }
  free(cmd1);
  return(icode);
 }
else
 {
    sprintf(buff,"[data transfer    ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
free(cmd1);
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[pre-processing   ] %s","IN PROGRESS");
printclog(2,buff);
icode=system(cmd2);
after_t=rtc();
fprintf(TIME_FILE_FP,"%lf\n",after_t-before_t);
icode=icode/256;
if ((icode < 0) || (icode >0))
 {
  if (icode<0)
   {
    sprintf(buff,"[pre-processing   ] %s","SYSTEM INTERRUPT");
    printclog(2,buff);
   }
  if (icode>0)
   {
    sprintf(buff,"[pre-processing   ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
   }
  free(cmd2);
  return(icode);
 }
else
 {
    sprintf(buff,"[pre-processing   ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
free(cmd2);
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[main-processing  ] %s","IN PROGRESS");
printclog(2,buff);
icode=system(cmd3);
after_t=rtc();
fprintf(TIME_FILE_FP,"%lf\n",after_t-before_t);
icode=icode/256;
if ((icode < 0) || (icode >0))
 {
  if (icode<0)
   {
    sprintf(buff,"[main-processing  ] %s","SYSTEM INTERRUPT");
    printclog(2,buff);
   }
  if (icode>0)
   {
    sprintf(buff,"[main-processing  ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
   }
  free(cmd3);
  return(icode);
 }
else
 {
    sprintf(buff,"[main-processing  ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
free(cmd3);
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[post-processing  ] %s","IN PROGRESS");
printclog(2,buff);
icode=system(cmd4);
after_t=rtc();
fprintf(TIME_FILE_FP,"%lf\n",after_t-before_t);
icode=icode/256;
if ((icode < 0) || (icode >0))
 {
  if (icode<0)
   {
    sprintf(buff,"[post-processing  ] %s","SYSTEM INTERRUPT");
    printclog(2,buff);
   }
  if (icode>0)
   {
    sprintf(buff,"[post-processing  ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
   }
  free(cmd4);
  return(icode);
 }
else
 {
    sprintf(buff,"[post-processing  ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
free(cmd4);
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[ceos output    ] %s","IN PROGRESS");
printclog(2,buff);
icode=system(cmd5);
after_t=rtc();
fprintf(TIME_FILE_FP,"%lf\n",after_t-before_t);
icode=icode/256;
if ((icode < 0) || (icode >0))
 {
  if (icode<0)
   {
    sprintf(buff,"[ceos output    ] %s","SYSTEM INTERRUPT");
    printclog(2,buff);
   }
  if (icode>0)
   {
    sprintf(buff,"[ceos output    ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
   }
  free(cmd5);
  return(icode);
 }
else
 {
    sprintf(buff,"[ceos output    ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
free(cmd5);
fclose(TIME_FILE_FP);

 return(icode);
}
