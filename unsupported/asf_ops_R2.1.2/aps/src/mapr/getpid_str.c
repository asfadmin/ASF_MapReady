#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		getpid_str.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)getpid_str.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.getpid_str.c"

#include <sys/types.h>
#include <unistd.h>

getpid_str(char str[18])
{
	int	i = getpid();
	sprintf(str,"%d",i);
	printf("mapr_getpid -> %s\n", str);
}
