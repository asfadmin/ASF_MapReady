/*****************************************************************************

  Description: This file contains C routines to that create and manipulate
               the parameters -- attributes and pointers -- of an object
               or group.
 
  Author:  Randy Davis, University of Colorado LASP
 
  Creation Date:  12 March 1989
  Last Modified:  18 May 1991

  History:

    Creation - This module was introduced in a somewhat different form in
    the Version 1 release of the ODLC library.

    Version 2.0 - 30 August 1990 - R. Davis, U. of Colorado LASP
      a) This set of routines has been adapted from the Version 1 set of
         routines for manipulating attributes.  The routines NewParameter,
         RemoveParameter, FindParameter and NextParameter have been con-
         structed from the Version 1 routines NewAttribute, RemoveAttribute,
         FindAttribute and NextAttribute, respectively.  The function of the
         Version 1 routine FirstAttribute is now part of FindParameter.
         Routines CopyParameter, CutParameter and PasteParameter are new
         for this version.

    Version 2.1 - 13 March 1991 - R. Davis, U. of Colorado LASP
      a) Implemented cut, copy and paste routines and changed some of the
         other routines to take advantage of them.
      b) Added support for comments attached to parameters.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Removed include statements that were Unix specific and placed them
       in odldef.h.
    b) Added intialization of two application specific fields to the
       NewParameter routine.
    c) Removed unused variables from several routines.
*****************************************************************************/
#include <ctype.h>
#include <errno.h>
#include <math.h>

#include  "odldef.h"




/*****************************************************************************

  Routine: NewParameter
 
  Description: Create a new parameter node and attach it to an
               object or group node.
 
  Input:
          aggregate - Pointer to the aggregate node to which the parameter
                      is to be attached.  If this value is NULL, the
                      parameter node is created but not attached to
                      any aggregate node.
          kind      - Kind of parameter: attribute (KP_ATTRIBUTE) or
                      pointer (KP_POINTER).
          name      - A character string containing the name to be given
                      to the parameter.
 
  Output: A pointer to the new parameter node is returned as the function
          result.  A NULL value indicates that the parameter could not be
          added, probably due to lack of dynamic memory. The NULL value is
          also returned if the name input name argument is NULL.
 
*****************************************************************************/


PARAMETER NewParameter (aggregate,kind,name)

     AGGREGATE       aggregate;
     PARAMETER_KIND  kind;
     char           *name;

{
  PARAMETER  parameter;        /* Pointer to the new parameter node         */
  char *string;

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/18/91  Removed unused variable old_last             >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>> */


  if (name == NULL)
    {
      return (NULL);
    }

  /* Allocate memory for the new parameter node */

  parameter = (PARAMETER) malloc (PARAMETER_NODE_SIZE);
  if (parameter != NULL)
    {
      /* Copy the parameters's kind and name */

      parameter->node_kind = kind;

      string = (char *) malloc (strlen(name)+1);
      if (string == NULL)
        {
          /* Error: Allocation failed, probably due to lack of memory.
             Free up all space and return */

          free (parameter);
          return (NULL);
        }
     
      parameter->name = strcpy (string, name);

      /* Initialize the other data fields in the parameter node */

      parameter->comment        = NULL;
      parameter->value_kind     = KV_UNKNOWN;
      parameter->value_count    = 0;
      parameter->columns        = 0;
      parameter->rows           = 0;

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/18/91  Added intialization of two application       >>>>> */
/* >>>>>              defined fields in the parameter structure.   >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

#ifdef PDS_TOOLBOX
      parameter->appl1         = 0;
      parameter->appl2         = 0;
#endif

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>> */

      /* Indicate that there are no values for this parameter yet */

      parameter->first_value = NULL;
      parameter->last_value  = NULL;

      /* Attach the parameter node to its aggregate node, if any */

      if (aggregate != NULL)
        {
          PasteParameter (aggregate, parameter);
        }
      else
        {
          /* There is no aggregate node to attach to: this parameter
             node will stand alone */

          parameter->owner         = NULL;
          parameter->left_sibling  = NULL;
          parameter->right_sibling = NULL;
        }
    }

  return (parameter);
}




/*****************************************************************************

  Routine: RemoveParameter
 
  Description: Removes a parameter node and all of its values.
 
  Input:
         parameter   - A pointer to the parameter node to be removed.
 
  Output: A pointer to the next parameter of the aggregate.  The NULL
          value is returned if there is no next parameter. The NULL
          value is also returned if input argument is NULL.

*****************************************************************************/
 

PARAMETER RemoveParameter (parameter)

     PARAMETER   parameter;

{
  PARAMETER  r_parameter;    /* Pointer to parameter's right sibling        */
  VALUE      value;          /* Pointer to parameter's value nodes          */


  if (parameter == NULL)
    {
      return (NULL);
    }

  /* Remove all the values associated with the parameter */

  value = FirstValue (parameter);

  while (value != NULL)
    {
      value = RemoveValue (value);
    }

  /* Remember the location of the next parameter node for this aggregate */

  r_parameter = parameter->right_sibling;

  /* Detach the parameter node from its aggregate node */

  CutParameter (parameter);

  /* Free up the memory space used to hold the parameter node, its
     name string and any comment attached to the node */

  free (parameter->comment);
  free (parameter->name);
  free (parameter);

  /* Point to the next available parameter, if any */

  return (r_parameter);
}




/*****************************************************************************

  Routine: FirstParameter
 
  Description: Returns the first parameter node of an object or group node.
 
  Input:
          parameter   - A pointer to the aggregate node.
 
  Output: A pointer to the first parameter node is returned as the function
          result. The value NULL is returned if there are no parameters for
          the aggregate.  The NULL value is also returned if the input
          argument is NULL.

*****************************************************************************/
 

PARAMETER FirstParameter (aggregate)

     AGGREGATE   aggregate;

{

  if (aggregate == NULL)
    {
      return (NULL);
    }
  else
    {
      return (aggregate->first_parameter);
    }

}




/*****************************************************************************

  Routine: NextParameter
 
  Description: Returns the next parameter of an object or group node.
 
  Input:
          parameter   - A pointer to the current parameter node.
 
  Output: A pointer to the next parameter node is returned as the function
          result. The value NULL is returned if there are no more parameters
          for the object or group.  The NULL value is also returned if the
          input argument is NULL.

*****************************************************************************/
 

PARAMETER NextParameter (parameter)

     PARAMETER   parameter;

{

  if (parameter == NULL)
    {
      return (NULL);
    }
  else
    {
      return (parameter->right_sibling);
    }

}




/*****************************************************************************

  Routine: FindParameter
 
  Description: Finds the first parameter of an object or group node with a
               specified name.
 
  Input:
         base_node   - A pointer to the aggregate node for which the
                       parameter node search is to be performed. 
         name        - Character string containing parameter name to be
                       located.
 
  Output: A pointer to the parameter node with the specified name.  If
          no match is found, then the NULL value is returned.  The NULL
          value will also be returned if either of the input arguments
          is NULL.

*****************************************************************************/
 

PARAMETER FindParameter (aggregate,name)

     AGGREGATE   aggregate;
     char       *name;

{
  PARAMETER  parameter;         /* Pointer to parameter node                */


  if (aggregate == NULL || name == NULL)
    {
      return (NULL);
    }

  /* Start searching with the first parameter of the specified aggregate */

  parameter = FirstParameter (aggregate);

  while (parameter != NULL && strcmp (parameter->name, name) != 0)
    {
      parameter = NextParameter (parameter);
    }

  return (parameter);
}




/*****************************************************************************

  Routine: CutParameter
 
  Description: Cuts a parameter node and its values from an ODL tree.
 
  Input:
          parameter   - A pointer to the parameter node that is to be
                        cut from the tree.
 
  Output: A pointer to the cut parameter node is returned as the function
          value. The NULL value is returned if the input argument is NULL.

*****************************************************************************/
 

PARAMETER CutParameter (parameter)

     PARAMETER   parameter;

{
  AGGREGATE  aggregate;      /* Pointer to parameter's aggregate node       */
  PARAMETER  l_parameter;    /* Pointer to parameter's left sibling         */
  PARAMETER  r_parameter;    /* Pointer to parameter's right sibling        */

  
  if (parameter == NULL)
    {
      return (NULL);
    }

  /* Detach the parameter node from its aggregate node */

  aggregate   = parameter->owner;
  l_parameter = parameter->left_sibling;
  r_parameter = parameter->right_sibling;
  
  if (l_parameter != NULL)
    {
      l_parameter->right_sibling = r_parameter;
    }
  else if (aggregate != NULL)
    {
      aggregate->first_parameter = r_parameter;
    }

  if (r_parameter != NULL)
    {
      r_parameter->left_sibling = l_parameter;
    }
  else if (aggregate != NULL)
    {
      aggregate->last_parameter = l_parameter;
    }

  /* Reset the parent pointer and the sibling pointers in the parameter
     node, since this node now stands alone */

  parameter->owner         = NULL;
  parameter->left_sibling  = NULL;
  parameter->right_sibling = NULL;

  /* Return a pointer to the cut parameter node */

  return (parameter);
}




/*****************************************************************************

  Routine: CopyParameter
 
  Description: Copies a parameter node and all of its values.
 
  Input:
          parameter   - A pointer to the parameter node to be copied.  This
                        parameter node remains in the ODL tree.
 
  Output: A pointer to the copied parameter node. The value NULL is returned
          if the input parameter pointer is NULL or if storage couldn't be
          obtained for the copy.

*****************************************************************************/
 

PARAMETER CopyParameter (parameter)

     PARAMETER  parameter;

{
  PARAMETER        parameter_copy; /* Copy of the specified parameter       */
  VALUE            value;          /* Value node currently being copied     */
  VALUE            value_copy;     /* Copy of the specified value           */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/18/91  Removed unused variable string               >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>> */


  if (parameter == NULL)
    {
      return (NULL);
    }

  /* Create a new parameter node */

  parameter_copy = NewParameter (NULL, parameter->node_kind, parameter->name);
  if (parameter_copy == NULL)
    {
      return (NULL);
    }

  /* Copy the comment attached to the parameter node, if any */

  CommentParameter (parameter_copy, parameter->comment);

  /* Copy the other data fields in the parameter node */

  parameter_copy->value_kind  = parameter->value_kind;
  parameter_copy->value_count = parameter->value_count;
  parameter_copy->columns     = parameter->columns;
  parameter_copy->rows        = parameter->rows;
    
  /* Copy each value node attached to the parameter.  For error values there
     may be none; for scalar values there will be one; for set and sequence
     values there may be zero, one or more  */
  
  value = FirstValue (parameter);

  while (value != NULL)
   {
     value_copy = CopyValue (value);
     NewValue (parameter_copy, &value_copy->item);
     free (value_copy);
     value = NextValue (value);
   }

  return (parameter_copy);
}




/*****************************************************************************

  Routine: PasteParameter
 
  Description: Inserts a parameter node and all its values into an ODL tree.
               The parameter node becomes the current last parameter node
               of the specified aggregate node.

  Input:
          aggregate   - A pointer to the aggregate node to which the
                        parameter node is to be added.
          parameter   - A pointer to the parameter node to be pasted.

  Output: A pointer to the inserted parameter node is returned as the
          function result. The value NULL is returned if either the
          target aggregate node or the input parameter node are NULL.

*****************************************************************************/
 

PARAMETER PasteParameter (aggregate,parameter)

     AGGREGATE   aggregate;
     PARAMETER   parameter;

{
  PARAMETER        old_last;       /* Previously last parameter node        */


  if (aggregate == NULL || parameter == NULL)
    {
      return (NULL);
    }

  /* Place the new parameter node at the end of the list of parameters
     for the specified aggregate */

  old_last = aggregate->last_parameter;
  if (old_last != NULL)
    {
      old_last->right_sibling = parameter;
    }

  aggregate->last_parameter = parameter;
  if (aggregate->first_parameter == NULL)
    {
      aggregate->first_parameter = parameter;
    }

  parameter->owner         = aggregate;
  parameter->left_sibling  = old_last;
  parameter->right_sibling = NULL;

  return (parameter);
}
