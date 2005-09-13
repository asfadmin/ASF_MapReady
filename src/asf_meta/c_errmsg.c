/******************************************************************************
FUNCTION:	c_errmsg

PURPOSE:

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   02/89    B. Davis		fixed bug in for loop of print_mes that
					searches for white space to continue
					message on next line.  Also, to help
					accommodate int file names, if no
					white space is found in a line-widths-
					worth of message, a line wrap is
					allowed to occur at a / for unix host
					file names, at a . for vms host file
					names, and at a ] for tae file names. 
  5.1	   03/89    B. Davis		Refrained from changing the value of
					vrity before passing it to z_exit.  It
					is possible for a fortran routine to
					pass 0, but the address being passed by
					fortran recieved by C is not necessarily
					an address that is allowed to be written
					to.  We will therefore assign a value to
					a local variable for passing to exit.
  5.2	  04/89	   B.Ailts		Made sure that -1 is passed to exit
					when an fatal error code is passed in.
  5.3	  08/95    M.Shindle		Updated to allow System V & ANSI
  					compilation.
  6.0      6/97    O. Lawlor  Function Prototypes!  Whee!

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 

ALGORITHM REFERENCES:
	c_errmsg hacked for the I hate TAE crowd
*******************************************************************************/
#include "asf.h"


#include <ctype.h>
#include "worgen.h"

#define MSG_PART_LEN 80

void c_errmsg(char	* message,char	* key, int	severity )

{
int	vflag;			/* int for changing the value of vrity to be */
				/* pass to exit			      */
char	os_mes[81];	/* message for OS dependent error */

/* Resolve the type of key. */
if ( key[0] == '\0' )			/* null string check */
	if ( severity <= 0 )			   /* fatal error */
		print_mes("Fatal error encountered.","ERROR-FATAL");
	else
		print_mes("Non-fatal error encountered.","ERROR-NONFATAL");

else if ( key[0] == ' ')
	if ( severity <= 0 )			   /* fatal error */
		print_mes(message,"general-error");
	else
		print_mes(message,"informational");

else if ( (strcmp(key,"unix")==0) || (strcmp(key,"UNIX")==0) )
	{
	sprintf(os_mes,"%s%s%d."," UNIX "," error code ",severity); 
  	print_mes(os_mes,"ERROR-UNIX");
	}

else if ( (strcmp(key,"vms")==0) || (strcmp(key,"VMS")==0) )
	{
	sprintf(os_mes,"%s%s%d."," VMS "," error code ",severity); 
  	print_mes(os_mes,"ERROR-VMS");
	}

else
	print_mes( message, key );

/* Decide whether or not to continue processing. 
------------------------------------------------ */
if ( severity <= 0 )
	{
	vflag = -1;
	exit(vflag);
	}

return;
}

void print_mes( char	*ms,char	* ky )
{
short	back;	/* counters to break up string MS */
char	*newms;		/* pointer to remaining part of ms */
			/* if it is too int for a one line */
char	restms[CMLEN];	/* string to pass recursively to print_mes */

/* If the length of MS is not greater than MSG_PART_LEN - the length of 
   the key then print it 
-----------------------------------------------------------------------*/
if ( strlen(ms) <= (MSG_PART_LEN-3)-strlen(ky) )
	{
	printf("\r[%s] %s\n",ky ,ms);
	}
else
	{
	for ( back = (MSG_PART_LEN-3)-strlen(ky);
	   ((back>=0) && (!isspace(ms[back])));
		back--)
		;		/* find w. space to def substring */
	if ( back <= 0 )
	   {
	   for ( back = (MSG_PART_LEN-3)-strlen(ky);
	      ((back>=0) && (ms[back] != '/') &&
			     (ms[back] != '.') &&
			      (ms[back] != ']'));
	   	   back--)
		   ;		/* no w. space, find host path separator */
	   if ( back <= 0 ) back = (MSG_PART_LEN-3)-strlen(ky);
	   }			/*no path separator, just wrap after 80th char*/
	newms = (ms + back + 1);
	strcpy(restms,newms);
	ms[back + 1] = '\0';	/* define substr with null */
	printf("\r[%s] %s\n",ky ,ms);
	print_mes( restms, ky );		/* print rest of string */
	}

return;
}
