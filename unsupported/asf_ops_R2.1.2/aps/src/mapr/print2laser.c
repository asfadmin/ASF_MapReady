#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		print2laser.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)print2laser.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.print2laser.c"

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

extern char window_name_[80];
extern char printer_name[80];

print2laser()
{
	char	string[200];
	int	pid, ret;

	pid = (int)getpid();

#ifdef DONTUSE

	char	printer_name[200];
	printer_name[0] = '\0';
	fprintf(stdout,"Enter the name of your color laser printer_name: ");
	fgets( printer_name, 80, stdin);
	/* have to remove '\n' from input buffer */
	printer_name[strlen(printer_name) - 1] = '\0';

#endif
	sprintf(string,"%s\n",window_name_); printf(string);
	sprintf(string, "import -window \"%s\" %d.miff", 
		window_name_, pid);
	ret = system (string) ;
	if (ret > 0)
	{
		printf("Command <%s> failed.\n",string);
		return 0;
	}
	else
		printf("Command <%s> was successful.\n",string);

	sprintf(string, "convert %d.miff %d.ras", pid, pid);
	ret = system (string) ;
	if (ret > 0)
	{
		printf("Command <%s> failed.\n",string);
		return 0;
	}
	else
		printf("Command <%s> was successful.\n",string);
	sprintf(string, "lpr -P%s %d.ras", printer_name, pid);
	ret = system (string) ;
	if (ret > 0)
	{
		printf("Command <%s> failed.\n",string);
		return 0;
	}
	else
		printf("Command <%s> was successful.\n",string);
	printf("Printer Job Queue: \n\n",string);
	sprintf(string, "lpq -P%s", printer_name);
	ret = system (string) ;
	if (ret > 0)
	{
		printf("Command <%s> failed.\n",string);
		return 0;
	}
	sprintf(string, "rm -f %d.miff %d.ras", pid, pid);
	ret = system (string) ;
	if (ret > 0)
	{
		printf("Command <%s> failed.\n",string);
		return 0;
	}
}
