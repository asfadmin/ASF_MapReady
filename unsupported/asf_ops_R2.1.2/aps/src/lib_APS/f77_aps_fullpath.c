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
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.f77_aps_fullpath.c"

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
	else if (!strcmp(location,"APS_EPHMIN"))
		loc = APS_EPHMIN;
	else if (!strcmp(location,"APS_EPHMOUT"))
		loc = APS_EPHMOUT;
	else if (!strcmp(location,"APS_ORBGIN"))
		loc = APS_ORBGIN;
	else if (!strcmp(location,"APS_ORBGOUT"))
		loc = APS_ORBGOUT;
	else if (!strcmp(location,"APS_CVRG"))  /* cvrg same as robgout */
		loc = APS_ORBGOUT;
	else if (!strcmp(location,"APS_MAPPER"))
		loc = APS_MAPPER;
	else if (!strcmp(location,"APS_STOICFILES"))
		loc = APS_STOICFILES;
	else if (!strcmp(location,"APS_UPW"))
		loc = APS_UPW;
	else if (!strcmp(location,"APS_STMP"))
		loc = APS_STMP;
	else if (!strcmp(location,"APS_GTMP"))
		loc = APS_GTMP;
	else if (!strcmp(location,"APS_ASF_FILES"))
		loc = APS_ASF_FILES;
	else if (!strcmp(location,"APS_NASDA_FILES"))
		loc = APS_NASDA_FILES;
	else if (!strcmp(location,"APS_CSA_FILES"))
		loc = APS_CSA_FILES;
	else if (!strcmp(location,"APS_ESA_FILES"))
		loc = APS_ESA_FILES;
	else if (!strcmp(location,"APS_LOGS"))
		loc = APS_LOGS;
	else if (!strcmp(location,"APS_REPORTS"))
		loc = APS_REPORTS;
	else if (!strcmp(location,"APS_RADARSAT_SVF"))
		loc = APS_RADARSAT_SVF;
	else if (!strcmp(location,"APS_RADARSAT_RAR"))
		loc = APS_RADARSAT_RAR;
	else if (!strcmp(location,"APS_RADARSAT_RECEPTION"))
		loc = APS_RADARSAT_RECEPTION;
	else if (!strcmp(location,"APS_RADARSAT_ARCHIVE"))
		loc = APS_RADARSAT_ARCHIVE;
 
	else  /* default to tmp in case of error */
		loc = APS_TEMP ;

	fullpath = aps_fullpath(loc, filename);

	/*  printf("f77_aps_fullpath(%s) -> %s\n",filename,fullpath); */

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
