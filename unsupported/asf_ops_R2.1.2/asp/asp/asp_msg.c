/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <syslog.h>
#include <asp_msg.h>

extern int aspsyslog;
extern int asp_job_id, asp_rev_id;
extern char job_name[100];

void asp_msg( int sysloglevel, char *fmt, ... )
{
        va_list args;
        char msg[132],string[132];

        va_start(args,fmt);
	strcpy(msg,"*");
	vsprintf(&msg[1],fmt,args);
	if (sysloglevel == LOG_ERR){
	   sprintf(string,"%s: %s",job_name,msg);
	 /* sprintf(string,"REV %d, JOB_ID %d: %s",asp_rev_id,asp_job_id,msg);*/
	   strcpy(msg,string);
	}
	printf("%s\n",msg);
        if (aspsyslog) syslog(sysloglevel,msg);
	if ( sysloglevel == LOG_ERR ) strcpy( asp_error_msg, msg ); 
        va_end(args);
}
