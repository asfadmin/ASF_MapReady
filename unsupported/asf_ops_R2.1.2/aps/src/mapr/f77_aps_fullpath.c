#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_aps_fullpath.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)f77_aps_fullpath.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.f77_aps_fullpath.c"

#include "stdio.h"
#include "apspath.h"

f77_aps_fullpath(char *location, char *filename, char *buf, int *bufsize)
{
	int loc;
	char *fullpath;

	if (!location)
		return NULL;
	if (!strcmp(location,"APS_TEMP"))
		loc = APS_TEMP;
	else
	if (!strcmp(location,"APS_EPHMIN"))
		loc = APS_EPHMIN;
	else
	if (!strcmp(location,"APS_EPHMOUT"))
		loc = APS_EPHMOUT;
	else
	if (!strcmp(location,"APS_ORBGIN"))
		loc = APS_ORBGIN;
	else
	if (!strcmp(location,"APS_ORBGOUT"))
		loc = APS_ORBGOUT;
	else
	if (!strcmp(location,"APS_MAPPER"))
		loc = APS_MAPPER;

	fullpath = aps_fullpath(loc, filename);
#ifdef DEBUG
	printf("f77_aps_fullpath(%s,%s) -> %s\n",location,filename,fullpath);
#endif

	if (fullpath && (*bufsize > strlen(fullpath)))
	{
		strncpy(buf,fullpath,strlen(fullpath));
		buf[strlen(fullpath)] = '\0';
		*bufsize = strlen(fullpath);
		free(fullpath);
	} else
		*bufsize = 0;

}

f77_free_fullpath(char *fullpath)
{
	if (fullpath)
		free(fullpath);
}
