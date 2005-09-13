/****************************************************************************
NAME:				c_ckptid

PURPOSE:  Checks point id to see if it has been used before

PROGRAM HISTORY:
VERSION	 DATE	AUTHOR	   CODE/CONT   REASON
-------	 ----	------	   ---------   -----------------------------
  5.0	 4/89	D. Steinwand  CSB      LAS 5.0 Original development
	 8/90   L. Huewe      CSB      Modified to remove all blanks from
				       the point id string.

ALGORITHM DESCRIPTION:
	Convert pt_id to lower case
	Remove any white space from the pt_id
	If the resulting string is a null, return FALSE
	Check the static point id array to see if the current id is there
	If there, return FALSE
	Put the current point id in the point id array
	Increment the counter
	Return TRUE
*****************************************************************************/
#include "asf.h"
#include <ctype.h>
#include "worgen.h"
#include "geompak.h"

char *rem_blanks(register char *str,register int  *len);

int c_ckptid(pt_id)

char *pt_id;			/* Tie point identifier */
{
static char id[MAX_TIE_POINTS][20];	/* All point id's entered so far */
static int count = 0;		/* Point id count */
int i;				/* Loop counter */
int twenty = 20;		/* Constant of 20 */
char *string;			/* Input string with blanks removed */

c_up2low(pt_id, &twenty);
string = rem_blanks(pt_id,&twenty);
if (*string == '\0')
    return(FALSE);
for (i = 0; i < count; i++)
   if (strcmp(&(id[i][0]), string) == 0) return(FALSE);
strcpy(&(id[count][0]), string);
strcpy(pt_id,string);
count++;
return(TRUE);
}

/******************************************************************************
NAME:	REM_BLANKS

PURPOSE:
	REM_BLANKS returns the input character with all white space removed.
	This includes leading, trailing and internal white sapce.

PROGRAM HISTORY:
	By L. Huewe, USGS/EROS Data Center, 8/90

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	
		Run under TAE

PROJECT:	LAS

ALGORITHM:

	Allocate buffer space for the output string.
	For each character in the input string
	    If the character is not a blank
		Copy it to the output string.
	Null terminate the output string.
	Return to the calling routine.

******************************************************************************/
char *rem_blanks(register char *str,register int  *len)
{
register char *newstr, *ptr, *newptr;

newstr = MALLOC(*len+1);

/*
   Copy all non-blank characters from the input string to the output string.
*/
for (newptr = newstr, ptr = str; ((ptr < str + *len) && (*ptr != '\0')); ptr++)
    {
    if (isspace(*ptr) == 0)
	{
	*newptr = *ptr;
	newptr++;
	}
    }
*(newptr) = '\0';
return(newstr);
}
