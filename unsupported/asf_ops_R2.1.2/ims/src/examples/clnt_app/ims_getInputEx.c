static char *sccs = "@(#)ims_getInputEx.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_getInputEx.c
**
** Function:    IMS example program for getting terminal input.
**
** Author:      S. Hardman
**
** Date:        5/12/95
**
** The following command will make the executable on SunOS Release 5.x:
**
** cc -I/asf/include/imsdads -L/asf/lib ims_getInputEx.c \
** -lims -o ims_getInputEx
**
******************************************************************************/
  
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
   
#include <ims_const.h>
#include <ims_getInput.h>

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (void)
{
	char buffer[IMS_INPUT_BUF_LEN+1];
	char *username;
	char *password;

	if (ims_getString (IMS_FALSE, buffer, IMS_INPUT_BUF_LEN,
		"Username: ") == (char *) NULL)
	{
		(void) printf ("\nError detected while reading input string.\n\n");
		exit (1);
	}
										  
	username = malloc (strlen (buffer) + 1);
	(void) strcpy (username, buffer);

	if (ims_getPassword (buffer) == (char *) NULL)
	{
		(void) printf ("\nError detected while reading input string.\n\n");
		exit (1);
	}
																		
	password = malloc (strlen (buffer) + 1);
	(void) strcpy (password, buffer);

	(void) printf ("\n%s\n", username);
	(void) printf ("%s\n\n", password);

	free (username);
	free (password);

	exit (0);
}
