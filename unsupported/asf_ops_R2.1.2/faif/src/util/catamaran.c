/*==============================================================================
Filename:	multi_strcat.c

Description:	
	This module contains the multiple strcat function.

External Functions:
	
Static Functions:
        	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:
==============================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void *multi_strcat(int num_input_strings, char *strings, ...)

/*==============================================================================
Function:	void *multi_strcat(num_input_strings, strings, ...)

Description:
        Form an output string by catenating an arbitrary number of
        input strings. When successful, a pointer to the newly allocated string
        is returned. If malloc fails, a NULL string is returned.

Parameters:
	# of strings to catenate, followed by each string.

Returns:        Pointer to the newly allocated string
Creator:	Rich Norman
Creation Date:	960218
==============================================================================*/

{
   va_list arg_pointer;
   char *next_string;
   int i;
   int output_size;
   char *return_string;

/* Check for degenerate case ... no input strings supplied. */
   if (num_input_strings == 0)
   {
      return_string = (char *)NULL;
      return(return_string);
   }

/* Initialize counter for size of output string. */
   output_size = 0;

/* Get first argument. */
   va_start(arg_pointer, strings);
   output_size = output_size + strlen(strings);

/* Loop through remaining arguments. */
   for (i = 1; i <= num_input_strings -1 ; i++)
   {
       next_string = va_arg(arg_pointer, char *);
       output_size = output_size + strlen(next_string);
   }

   output_size = output_size + 1; /* For trailing null */

   va_end(arg_pointer);

/*
   Use the output string length calculated above to allocate enough memory
   for the transfer of the input strings.
*/

   return_string = (char *)malloc(output_size);
   if (return_string == (char *)NULL) return(return_string);

/*
   Following is a repeat of the code above, except now we actually move the
   source strings to the destination, instead of merely getting their lengths. 
*/

   va_start(arg_pointer, strings);
   strcpy(return_string, strings);

   for (i = 1; i <= num_input_strings -1 ; i++)
   {
       next_string = va_arg(arg_pointer, char *);
       strcat(return_string, next_string);
   }

   va_end(arg_pointer);

   return(return_string) ;

} /* multi_strcat */

/* End of File */

