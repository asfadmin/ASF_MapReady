#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#pragma ident	"@(#)aps_chmod.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.aps_chmod.c"
/*********************************************************************
*  Name: aps_chmod
*
*  Module Type: PROCEDURE     Language: C
*
*                                                                   
*  Purpose: change protections on a file so everyone can work with a file.
*          
*
*  Input Parameters:                                                
*                                                                   
*  Name          Type        Description                           
*
*  *filename	char *		name of the file including directory path.
*  *perms	char *		desired permissions to give.  the permissions
*				go to user, group and other.  
*				use R, W, X in any order or case, as:
*				"W", "wr", or "rWx" etc.  
*
* NOTE:  when calling from a FORTRAN program, terminate the input strings 
*        with a '\0'.  When calling from a c program, well, you c programmers
*        don't have to be reminded!  
*                                                                   
*  Output Parameters:                                               
*                                                                   
*  Name          Type        Description                            
*  ier     	*int       Pointer to status code.  
*************************************************************************/
#include <sys/types.h>
#include <sys/stat.h>
void aps_chmod(char *filename, char *perms, int *ier)
{
/* usage:  apf_chmod(filename, perms, ier) 				*/ 
/*                   filename = path and file name. 			*/ 
/*                   perms = R, W, X, or any combo.  			*/
int istat, leng, j;
mode_t mode;
mode = 0;
leng = strlen(perms);
/* 
printf("aps_chmod:  filename = %s--\n      perms = %s--\n", filename, perms);
 */
for (j=0;j<leng;j++)
{
if(*(perms+j) == 'r' || *(perms+j) == 'R') 
{
/* 
 printf("aps_chmod:  read \n"); 
 */
	mode = mode | S_IROTH ;
	mode = mode | S_IRGRP ;
	mode = mode | S_IRUSR ;
}
else if(*(perms+j) == 'w' || *(perms+j) == 'W') 
{
/* 
  printf("aps_chmod:  write \n");
 */
	mode = mode | S_IWOTH ;
	mode = mode | S_IWGRP ;
	mode = mode | S_IWUSR ;
}
else if(*(perms+j) == 'x' || *(perms+j) == 'X') 
{
/* 
  printf("aps_chmod:  execute \n");  
 */
	mode = mode | S_IXOTH ;
	mode = mode | S_IXGRP ;
	mode = mode | S_IXUSR ;
}
/* 
  printf("aps_chmod:  mode = %o\n", mode);  
 */

}
istat = chmod(filename, mode);
if(istat) 
{
	printf("aps_chmod:  ERROR ##### change mod operation failed.\n");
	printf("            filename = %s\n", filename);
}
*ier = istat;
return;
}
