/*==============================================================================
Filename:	real.c

Description:
	This module contains the function to check if a string conforms
to the ISO NR3 format. The conversion of valid strings to
floating-point values which may be done by the C-library function
'atof' is not performed.

External Functions:
	is_real_number
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <string.h>
#include <ctype.h>
#include "faifdefs.h"   /* Primary header file */

#ifdef __STDC__
int is_real_number(char *) ;
#else
int is_real_number() ;
#endif



/*==============================================================================
Function:	int is_real_number(char *string) 
Description:	
	Check string if it is a valid real number string

	This function examines the ISO NR3 string to make sure it
contains no illegal characters.  This function returns ACCEPT if string
is in the correct format and REJECT otherwise.

Parameters:
	string - input string to validate as real number string

Returns:	
	ACCEPT - string is a valid real number string
	REJECT - string is not a valid real number string

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
	Assert string is not a NULL string.
==============================================================================*/
#ifdef __STDC__
int 
is_real_number(char *string)
#else
int 
is_real_number(string)
   char * string;
#endif
{
   char *p, *start;
   int state;

   if (string == (char *)NULL)
      return(REJECT) ;

   /* Skip through blank characters 
   */
   start = string ;
   while (isspace(*start))
      start++ ;

   /* Return REJECT for blank lines 
   */ 
   if (*start == '\0')
      return(REJECT) ;

   for (p=string, state = 0; *p ; p++)
   {
      switch (state)
      {
         /* initial case or E 
	 */
         case 0 :
            if (*p == '.')
            {
               /* In case string is "." , REJECT 
	       */
               if (strlen(string) == 1)
                  return(REJECT);

               state = 3;
               break;
            }
            /* FALL THROUGH */

         case 7 :
            if (isdigit(*p))
            {
               state+=2;
               break;
            }
            if (*p == '+' || *p == '-')
            {
               state++;
               break;
            }
            return(REJECT);

         /* sign case 
	 */
         case 1 :
            if (*p == '.')
            {
               state = 3;
               break;
            }
            /* FALL THROUGH */

         case 8 :
            if (isdigit(*p))
            {
               state++;
               break;
            }
            return(REJECT);

         /* states 2,4,6,9 are digits 
	 */
         case 2 : 
            if (*p == '.')
            {
               state = 5;
               break;
            }
            /* FALL THROUGH */

         case 4 :
         case 6 :
            if (*p == 'E' || *p == 'e')
            {
            state = 7;
            break;
         }
         /* FALL THROUGH */

         case 9 :
            if (isdigit(*p)) /* eat up digits */
               break;
            return(REJECT);

         /* cases 5&3 on decimal point 
	 */
         case 5 :
            if (*p == 'E' || *p == 'e')
            {
               state = 7;
               break;
            }
            /* FALL THROUGH */

         case 3 :
            if (isdigit(*p))
            {
               state++;
               break;
            }
            return(REJECT);

         default:
            return(REJECT);
      }
   }

   return(ACCEPT);

} /* is_real_number */

/* End of File */
