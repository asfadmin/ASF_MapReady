/* SccsId[]= @(#)write_log.c	2.41 3/24/98 */
static char sccsid_write_log[]= "@(#)PPwrite_log.c:2.41";

#include <stdio.h>
#include <stdlib.h>
#include "/usr/include/sys/syslog.h"
#include "error.h"

int read_1st_auxline(filename,RDS_ver)
char *filename;
char *RDS_ver;
{
 char filename_c[60];
 FILE* dummy_fp;
 char buff[300];
 char *sss;
 char str_c[60];

 str_f_to_c(filename_c,filename,60);

 if ((dummy_fp=fopen(filename_c,"r"))== NULL)
    {
     printf("cannot open %s\n",filename_c);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",filename_c);
     printclog(1,buff);
     printerr(filename_c);
     return(ierr_2);
    }

 fgets(buff,300,dummy_fp);
 sss=(char *)strstr(buff,"RDS");
 sscanf(&sss[3],"%s",str_c);
 str_c_to_f(RDS_ver,str_c,60);





 return(0);


}

int printflog(level,msg)
int *level;
char *msg;
{
 int i;
char tmsg[500];

 for (i = 0; i < strlen(msg); i++)
    if (*(msg + i) == '&') *(msg + i) = '\0';


 strcpy(tmsg,"(asf_ssp2) ");
 strcat(tmsg,msg);

 if (*level==1) syslog(LOG_ERR     ,tmsg);
 if (*level==2) syslog(LOG_INFO   ,tmsg);
 if (*level==3) syslog(LOG_DEBUG  ,tmsg);



 return(1);
}
int printclog(level,msg)
int level;
char *msg;
{
char tmsg[500];

 strcpy(tmsg,"(asf_ssp2) ");
 strcat(tmsg,msg);


/*if  (level==1) setlogmask (LOG_MASK (LOG_ERR));
if  (level==2) setlogmask (LOG_MASK (LOG_INFO));
if  (level==3) setlogmask (LOG_MASK (LOG_DEBUG));*/

 if (level==1) syslog(LOG_ERR    ,tmsg);
 if (level==2) syslog(LOG_INFO   ,tmsg);
 if (level==3) syslog(LOG_DEBUG  ,tmsg);
 


 return(1);
}

int engage_log(job_msg)
char *job_msg;
{
static char job_msg_copy[300];

 strcpy(job_msg_copy,job_msg);

 
openlog(job_msg_copy,LOG_NOWAIT | LOG_NDELAY | LOG_PID,LOG_LOCAL4);

 return(1);
}

int disengage_log()
{
 closelog();

 return(1);

}
int printerr(msg)
char *msg;
{
 char FILE_ENG_CONFIG[256];
 FILE *fp;
 char dummy_s[256];
 int dummy_i,i;
 char file_err[256];
 char buff[300];


    strcpy(file_err,getenv("LOCAL"));
    strcat(file_err,"/ssp/error.dat");



 if ((fp=fopen(file_err,"w"))== NULL)
    {
     printf("cannot open %s\n",file_err);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",file_err);
     printclog(3,buff);
     exit(-1);
    }
 fprintf(fp,"%s\n",msg);
 fclose(fp);

 return(0);
}
int readerr(ierr,name)
int ierr;
char *name;
{
 char FILE_ENG_CONFIG[256];
 FILE *fp;
 char dummy_s[256];
 int dummy_i,i;
 char file_err[256];
 char buff[300];

    strcpy(file_err,getenv("LOCAL"));
    strcat(file_err,"/ssp/error.dat");



 if ((ierr==2) ||
     (ierr==3) ||
     (ierr==4) ||
     (ierr==8)    )
  {
    if ((fp=fopen(file_err,"r"))== NULL)
    {
     printf("cannot open %s\n",file_err);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",file_err);
     printclog(3,buff);
     exit(-1);
    }
    fscanf(fp,"%s",name);
    fclose(fp);
   }
 else
   {
    strcpy(name,"");
   }
 return(0);
}
