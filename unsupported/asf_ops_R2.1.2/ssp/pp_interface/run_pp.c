/* SccsId[]= @(#)run_pp.c	2.41 3/24/98 */
static char sccsid_run_pp[]= "@(#)PPrun_pp.c:2.41";

#include <stdio.h>
#include "error.h"

int run_pp(file_path,file_config,job_id,subsystem_name,time_file)
char *file_path;
char *file_config;
int job_id;
char *subsystem_name;
char *time_file;
{
int icode;
char buff[300],name[300];
char *status_str(),*description_str();
char *dummy;


sprintf(buff,"%s",subsystem_name);
engage_log(buff);
icode=pp(file_path,file_config,subsystem_name,time_file);
if ((icode < 0) || (icode >0))
 {
  if (icode<0)
   {
/*    sprintf(buff,"[Job Status RETURN] %s","SYSTEM INTERRUPT");           */
/*    printclog(1,buff);                                                   */
/*    sprintf(buff,"JOB FAILED DUE TO  %s","CP request of reset or stop"); */
/*    printclog(2,buff);                                                   */
   }
  if (icode>0)
   {
    sprintf(buff,"[Job Status RETURN] %s",status_str(ssp2_msg[icode]));
    readerr(icode,name);
    strcat(buff,name);
    printclog(1,buff);
    sprintf(buff,"JOB FAILED DUE TO  %s",description_str(ssp2_msg[icode]));
    printclog(2,buff);
   }
 }
else
 {
    sprintf(buff,"[Job Status RETURN] %s",status_str(ssp2_msg[icode]));
    printclog(2,buff);
 }
disengage_log();
printf("CODE PROPERLY RETURN\n");

 return(icode);

}
