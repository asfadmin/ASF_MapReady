#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	banner_exit.c	

Description:	
	a routine that prints out an ERROR or NORMAL END banner and then exits 
	with the provided exit code.  NORMAL END is printed for the exit code 
	of 0, while ERROR is printed for any other exit code.  

External Functions Defined:
	void banner_exit(int exit_code) 
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:

Notes:
	Currently, this routine calls system() to print the banner.  
	This method may later be changed if standardization is needed.

==============================================================================*/
#pragma ident	"@(#)banner_exit.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.banner_exit.c"


/*==============================================================================
Function:		banner_exit

Description:	
	a routine that prints out an ERROR or NORMAL END banner and then exits 
	with the provided exit code.  NORMAL END is printed for the exit code 
	of 0, while ERROR is printed for any other exit code.  

Parameters:		
	int exit_code	this is the code used in the exit statement.

Returns:     	
	void; does not return.  

Creator:		Lawrence Stevens

Creation Date:	01/04/1995

Notes:		
	Currently, this routine calls system() to print the banner.  
	This method may later be changed if standardization is needed.
==============================================================================*/
void banner_exit(int exit_code)
{
	int rcode, system();

	if(exit_code == 0)
		rcode = system("banner 'NORMAL END'");
	else
		rcode = system("banner ERROR");

	exit(exit_code);
}
