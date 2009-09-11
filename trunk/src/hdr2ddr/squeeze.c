/******************************************************************************
NAME:	SQUEEZE

FUNCTION:
	SQUEEZE returns a pointer to a null terminated character string.  It
	searches for the first non-blank character of the input parameter
	"str" from the right.  If the first non-blank character is not
	the null value (0), then the next character is assigned to the null
	value.  This routine is used to null terminate a fortran character
	string.

PROGRAM HISTORY:
  Version	Date       Author       Request
  -------	----	   ------       -------
    1.0         5/86       K. Gacke     initial development
    1.1        10/86       K. Gacke     if a " " is inputted, the returned
				        string is null
    1.2	       12/87	   B.Ailts      change include directory specifications
					Use raw 'C' types
    1.3        12/92       J.Hemmer	Correct for blank strings
					
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	
		none

PROJECT:	LAS

ALGORITHM:
	Search from the right of a character string for the first non blank
	character.  If this character is not the null value, then it is 
	assigned to the null value.  This routine is used to null terminate
	fortran character strings.

******************************************************************************/

#include "asf.h"


#include "worgen.h"

FUNCTION char *squeeze(register const char *str,register int  len)
{
register char *newstr;
register char *ptr;

newstr = MALLOC(len+1);

strncpy(newstr,str,len);
for(ptr = newstr + len - 1; ((ptr >= newstr) && (*ptr == ' ')); ptr--)
	;
*(ptr + 1) = '\0';
return(newstr);
}
