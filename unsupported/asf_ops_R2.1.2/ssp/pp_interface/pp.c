/* SccsId[]= @(#)pp.c	2.41 3/24/98 */
static char sccsid_pp[]= "@(#)PPpp.c:2.41";

#include <stdio.h>
#include <time.h>
#include "error.h"

int pp(file_path,file_config,subsystem_name,time_file)
char *file_config;
char *file_path;
char *subsystem_name;
char *time_file;
{
int icode;
double before_t,after_t;
char buff[300];
FILE   *TIME_FILE_FP;
char  cmd[500];


TIME_FILE_FP=fopen(time_file,"a");
if (TIME_FILE_FP == NULL)
 {
  printf("cannot open the %s\n",time_file);
  sprintf(buff,"cannot open %s\n",time_file);
  printclog(1,buff);
  printerr(time_file);
  return(ierr_2);
 }
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[data transfer    ] %s","IN PROGRESS");
printclog(2,buff);
sprintf(cmd, "%s/run.pp_transfer %s %s", file_path, file_config, subsystem_name); 
icode=system(cmd);
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
  return(icode);
 }
else
 {
    sprintf(buff,"[data transfer    ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[main-processing  ] %s","IN PROGRESS");
printclog(2,buff);
sprintf(cmd, "%s/run.pp_correlator %s %s", file_path, file_config, subsystem_name); 
icode=system(cmd); 
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
  return(icode);
 }
else
 {
    sprintf(buff,"[main-processing  ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
/*******************************************************************************/
before_t=rtc();
sprintf(buff,"[ceos output    ] %s","IN PROGRESS");
printclog(2,buff);
sprintf(cmd, "%s/run.pp_ceos %s %s", file_path, file_config, subsystem_name);
icode=system(cmd); 
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
  return(icode);
 }
else
 {
    sprintf(buff,"[ceos output    ] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
fclose(TIME_FILE_FP);

 return(icode);
}
