#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#pragma ident	"@(#)aps_access.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.aps_access.c"
/*********************************************************************
*  Name: aps_access
*
*  Module Type: PROCEDURE     Language: C
*
*                                                                   
*  Purpose: checks protections on a file to see if permissions are allowed.  
*		returns a 0 in the error code if they are allowed.  
*		also can check for file existence.  
*
*  Input Parameters:                                                
*                                                                   
*  Name          Type        Description                           
*
*  *filename	char *		name of the file including directory path.
*  *perms	char *		desired permissions to give.  the permissions
*				go to user, group and other.  
*				use R, W, X, or F in any order or case, as:
*				"W", "wr", F, or "rWx" etc.  
*				R = ask if read permission is there.  
*				W = ask if write permission is there.  
*				X = ask if execute (search) permission is there.
*				F = ask if the file is there.  
*
* USAGE:  when calling from a FORTRAN program, terminate the input strings 
*        with a CHAR(0).  When calling from a c program, well, you c programmers
*        don't have to be reminded!  
*	example from a FORTRAN program:  
*
*	external aps_access !$pragma C(aps_access)
*	call aps_access(filename, 'rw', ier)
*                                                                   
*  Output Parameters:                                               
*                                                                   
*  Name          Type        Description                            
*  ier     	*int       Pointer to status code:  0 means the perms are 
*				permitted.   
*************************************************************************/
#include <unistd.h>
void aps_access(char *filename, char *perms, int *ier)
{
int istat, leng, j;
mode_t mode, fmode;
mode = 0;
leng = strlen(perms);
/*
printf("aps_access:  filename = %s--\n      perms = %s--\n", filename, perms);
 */
for (j=0;j<leng;j++)
{
	if(*(perms+j) == 'r' || *(perms+j) == 'R') 
	{
/*
	 printf("aps_access:  read \n"); 
 */
		mode = mode | R_OK ;
	}
	else if(*(perms+j) == 'w' || *(perms+j) == 'W') 
	{
/*
	  printf("aps_access:  write \n");
 */
		mode = mode | W_OK ;
	}
	else if(*(perms+j) == 'x' || *(perms+j) == 'X') 
	{
/*
	  printf("aps_access:  execute \n");  
 */
		mode = mode | X_OK ;
	}
	else if(*(perms+j) == 'f' || *(perms+j) == 'F') 
	{
/*
	  printf("aps_access:  existence \n");  
 */
		fmode = F_OK;
		istat = access(filename, fmode);
		if(istat != 0) 
		{
			*ier = istat;
			return;
		}
	}
/*
	printf("aps_access:  mode = %o\n", mode);  
 */
}
istat = access(filename, mode);
*ier = istat;
return;
}
