/*****************************************************************************

  Description: Routines for attaching comments to aggregate and parameter
               nodes on an ODL tree and for removing such comments.
 
  Author:  Randy Davis, University of Colorado LASP

  Creation Date:  25 August 1990
  Last Modified:  18 May 1991

  History:

    Creation - This set of routines was introduced in Version 2.1 of the
    ODLC library.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h.

*****************************************************************************/

#include "odldef.h"




/*****************************************************************************

  Routine: CommentAggregate
 
  Description: Attach a comment to or detach one from an aggregate node.
               The previous comment, if any, is discarded.
 
  Input:
          aggregate - Pointer to the aggregate node.
          comment   - Character string with comment to be attached. If
                      this argument is NULL, then the aggregate node
                      will have no comment.
 
  Output: A value of 1 is returned as the function value if processing
          is successful.  If the operation fails, typically due to a
          lack of memory in which to store the comment, a value of 0
          is returned.  A value of 0 will also be returned if the
          input base node argument is NULL.

*****************************************************************************/


int CommentAggregate (aggregate,comment)

     AGGREGATE    aggregate;
     char        *comment;

{
  char  *string;            /* Pointer to storage space for comment        */


  if (aggregate == NULL)
    {
      return (0);
    }

  /* Detach the current comment, if any, from this aggregate */

  free (aggregate->comment);

  /* If a comment was supplied, copy it and attach it to the node */

  if (comment == NULL)
    {
      aggregate->comment = NULL;
    }
  else
    {
      string = (char *) malloc (strlen (comment)+1);
      if (string == NULL)
        {
          /* Error: couldn't get memory for the copy of the string */

          return (0);
        }

      aggregate->comment = strcpy (string, comment);
     }

return (1);
}


/*****************************************************************************

  Routine: CommentParameter
 
  Description: Attach a comment to or detach one from a parameter node.
               The previous comment, if any, is discarded.
 
  Input:
          parameter - Pointer to the parameter node.
          comment   - Character string with comment to be attached. If
                      this argument is NULL, then the parameter node
                      will have no comment.
 
  Output: A value of 1 is returned as the function value if processing
          is successful.  If the operation fails, typically due to a
          lack of memory in which to store the comment, a value of 0
          is returned.  A value of 0 will also be returned if the
          input parameter argument is NULL.

*****************************************************************************/


int CommentParameter (parameter,comment)

     PARAMETER     parameter;
     char         *comment;

{
  char  *string;            /* Pointer to storage space for comment        */


  if (parameter == NULL)
    {
      return (0);
    }

  /* Detach the current comment, if any, from this parameter */

  free (parameter->comment);

  /* If a comment was supplied, copy it and attach it to the node */

  if (comment == NULL)
    {
      parameter->comment = NULL;
    }
  else
    {
      string = (char *) malloc (strlen (comment)+1);
      if (string == NULL)
        {
          /* Error: couldn't get memory for the copy of the string */

          return (0);
        }

       parameter->comment = strcpy (string, comment);
     }

return (1);
}
