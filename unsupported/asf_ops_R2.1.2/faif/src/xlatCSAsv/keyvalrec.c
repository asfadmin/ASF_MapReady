/*==============================================================================
Filename:	keyvalrec.c

Description:	
	This module contains the keyword value statement storage and
manipulation functions.

External Functions:
	alloc_keyval_record
	parse_keyval_stmt
	
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "faifdefs.h"
#include "keyvalrec.h"

#ifdef __STDC__
Keyword_Value_Stmt * alloc_keyval_record(void) ;
int                  parse_keyval_stmt(char *, Keyword_Value_Stmt *) ;
#else
Keyword_Value_Stmt * alloc_keyval_record() ;
int                  parse_keyval_stmt() ;
#endif

extern void *util_do_malloc() ;



/*==============================================================================
Function:	Keyword_Value_Stmt *alloc_keyval_record(void)

Description:
	Allocate and initialize a keyword value record data structure

	This function allocates a keyword value record and then
initializes its fields.  If the allocation fails, NULL is returned.

Parameters:	None
Returns:	pointer to newly allocated record or NULL
Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:	
==============================================================================*/
#ifdef __STDC__
Keyword_Value_Stmt *
alloc_keyval_record(void)
#else
Keyword_Value_Stmt *
alloc_keyval_record()
#endif
{
   Keyword_Value_Stmt *keyval = NULL ;

   keyval = (Keyword_Value_Stmt *)util_do_malloc(sizeof(Keyword_Value_Stmt)) ;
   if (keyval != (Keyword_Value_Stmt *)NULL)
   {
      keyval->keyword = NULL ;
      keyval->value_string = NULL ;
   }

   return(keyval) ;

} /* alloc_keyval_record */
 




/*==============================================================================
Function:       int parse_keyval_stmt(char *dataline,
			Keyword_Value_Stmt *keyval)
Description:
	Parse a string as a keyword value statement and store the parts
obtained into a keyword value statement record

	This function parses a character string as a keyword value
statement.  If the parse is successful, the keyword and value string
components are copied in the keyword value statement record.  If
parsing does not succeed, REJECT is returned.  ERROR is returned if
there are errors in the input and in parsing.  ACCEPT is returned if
the keyword and value strings are obtained and stored successfully.

Parameters:
	char *dataline - input data line
	Keyword_Value_Stmt *keyval - keyword value statement record

Returns:
	ACCEPT - keyword and value strings successfully obtained
	REJECT - unable to parse out keyword and value strings
	ERROR - error encountered while parsing

Creator:	Norbert Piega	
Creation Date:	03/03/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_keyval_stmt(char *dataline, Keyword_Value_Stmt *keyval)
#else
int
parse_keyval_stmt(dataline, keyval)
   char *dataline ;
   Keyword_Value_Stmt *keyval ;
#endif
{
   char *ptr, *kptr, *vptr, *equalsptr ;

   if (keyval == (Keyword_Value_Stmt *)NULL || dataline == (char *)NULL)
      return(ERROR) ;

   /* Assign pointers to the start of the data line
   */
   kptr = ptr = dataline ;

   /* Try to parse the data line as a keyword-value
   -- statement and obtain the keyword portion
   */
   while (!isspace(*ptr) && (*ptr != '='))
      ptr++ ;
   *ptr++ = '\0' ;
   if ((int)strlen(kptr) <= 0)
      return(REJECT) ;

   /* Assign obtained keyword in the keyval record's keyword field
   */
   keyval->keyword  = (char *)util_do_malloc(sizeof(char)*(strlen(kptr)+1)) ;
   if (keyval->keyword == (char *)NULL)
      return(ERROR) ;
   strcpy(keyval->keyword, kptr) ;

   /* Look for the value portion of the keyword
   -- value statement by finding the equal sign
   -- in the data line. Skip spaces in between.
   */
   if ((equalsptr = strchr(ptr, '=')) != NULL)
      ptr = ++equalsptr ;
   while (isspace(*ptr))
      ptr++ ;
   vptr = ptr ;
   while (!isspace(*ptr) && *ptr != '\0')
      ptr++ ;
   *ptr = '\0' ;
   if ((int)strlen(vptr) <= 0)
      return(REJECT) ;

   /* Assign obtained value in the keyval record's value field
   */
   keyval->value_string =
      (char *)util_do_malloc(sizeof(char)*(strlen(vptr)+1)) ;
   if (keyval->value_string == (char *)NULL)
      return(ERROR) ;
   strcpy(keyval->value_string, vptr) ;

   return(ACCEPT) ;

} /* parse_keyval_stmt */

/* End of File */
