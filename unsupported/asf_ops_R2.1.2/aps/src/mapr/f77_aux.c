#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_aux.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)f77_aux.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/mapr/SCCS/s.f77_aux.c"

#include <stdlib.h>

int f77_getenv( env_var, buf , sizeofbuf )
char	*env_var,*buf;
int	*sizeofbuf;
{
	char	*cp = 0;

	if (!(cp = (char *)getenv (env_var)))
        {
                printf("f77_getenv : evironment variable not defined '%s'\n",env_var);
                return 1;
        }
	if (strlen(cp) < *sizeofbuf)
	{
		sprintf(buf,"%s",cp);
		printf("f77_getenv : %s -> %s\n",env_var,cp);
		return 0;
	}
	else 
	{
		printf("f77_getenv : passin buffer not big enough\n");
		return 1;
		
	}
}
