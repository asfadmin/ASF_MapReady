/*****************************************************************************

  Description: This file contains C routines that navigate among the object
               nodes of an ODL tree.  These routines augment the general
               purpose routines for manipulating aggregate nodes.  The main
               differences between these routines and the general routines
               for handling aggregates are the following:

                 - These routines ignore group nodes when navigating through
                   an ODL tree;

                 - The search routines FindObject and FindNextObject are
                   able to search for a specific class of object as well
                   as for an object node with a specific name.
 
  Author:  Randy Davis, University of Colorado LASP
 
  Creation Date:  13 March 1991
  Last Modified:  18 May 1991

  History:

    Creation - This set of routines was introduced in the Version 2.1
    release of the ODLC library.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h.

*****************************************************************************/
#include <ctype.h>
#include <errno.h>
#include <math.h>

#include  "odldef.h"





/*****************************************************************************

  Routine: ParentObject
 
  Description: Returns a pointer to an aggregate's parent object.  
 
  Input:
          base_node - A pointer to an aggregate node.
 
  Output: A pointer to the parent object node.  If there is no parent object
          (if, for example, the input base node is the root node of an ODL
          tree) then the NULL value is returned.  The NULL value is also
          returned if the input argument is NULL.

*****************************************************************************/
 

OBJECT ParentObject (base_node)

     AGGREGATE    base_node;

{
  AGGREGATE  node;               /* Pointer to current aggregate node       */


  node = base_node;

  do
    {
      node = ParentAggregate (node);
    } while (node != NULL && node->kind != KA_OBJECT);

  return (node);
}




/*****************************************************************************

  Routine: NextObject
 
  Description: Returns the next object node after the one pointed to by the
               input aggregate node, according to a pre-order traversal of
               the ODL tree.
 
  Input:
          base_node  - A pointer to the starting aggregate node.
 
  Output: A pointer to the next object node is returned as the function
          value.  A NULL value indicates that there is no next object
          node.  The NULL value is also returned if the input argument
          is NULL.

*****************************************************************************/
 

OBJECT NextObject (base_node)

     AGGREGATE base_node;

{
  AGGREGATE  node;                    /* Pointer to the next node           */


  node = base_node;

  do
    {
      node = NextAggregate (node);
    } while (node != NULL && node->kind != KA_OBJECT);

  return node;
}




/*****************************************************************************

  Routine: NextSubObject
 
  Description: Returns the next sub-object of the base node after the one
               pointed to by the start node.
 
  Input:
          base_node  - A pointer to the base aggregate node.
          start_node - A pointer to the starting node.
 
  Output: A pointer to the next object sub-node of the base node is
          returned as the function value.  A NULL value indicates that
          there is no next object sub-node.  The NULL value is also
          returned if the input argument is NULL.

*****************************************************************************/
 

OBJECT NextSubObject (base_node,start_node)

     AGGREGATE    base_node;
     AGGREGATE    start_node;

{
  AGGREGATE  node;                    /* Pointer to the next node           */


  node = start_node;

  do
    {
      node = NextSubAggregate (base_node, node);
    } while (node != NULL && node->kind != KA_OBJECT);

  return node;
}




/*****************************************************************************

  Routine: FindObject
 
  Description: Finds an object node with a specified name and/or class.
               The search starts with the base node and continues as
               necessary with all of the base nodes's children, all their
               children, and so on using a preorder search.
 
  Input:
          base_node - A pointer to the starting aggregate node.
          name      - Pointer to a character string containing the object
                      name to be located. This may be NULL; if it is, the
                      search will be for the first object node of the
                      specified class.
          class     - Pointer to a character string with the object class
                      to be located. This may be NULL; if it is, the search
                      will be for the first object node with the specified
                      name, regardless of its class.
 
  Output: A pointer to the first object node that has the specified name
          and/or class is returned as the function value. If no match is
          found, then the NULL value is returned.  The NULL value is also
          returned if the input pointer to the base node is NULL or if
          both the input name and class are NULL.

*****************************************************************************/
 

OBJECT FindObject (base_node,name,class)

     AGGREGATE    base_node;
     char        *name;
     char        *class;

{
  AGGREGATE  node;                    /* Pointer to current node           */


  if (base_node == NULL || (name == NULL && class == NULL))
    { 
      return (NULL);
    }

  /* Start searching with the base node and stop searching when we
     find a node with the specified name and/or class or when we
     have visited all of the progeny of the base node  */

  node = base_node;

  while (node != NULL)
    {
      if (node->kind == KA_OBJECT &&
          (name  == NULL || strcmp (node->name,  name)  == 0) &&
          (class == NULL || strcmp (node->class, class) == 0))
        {
          break;
        }

      node = NextSubObject (base_node, node);
    }

  return (node);
}



/*****************************************************************************

  Routine: FindNextObject
 
  Description: This routine is similar to routine FindObject in that it
               searches the progeny of the specified base node, except that
               the search begins with the node specified by the second
               argument. This is useful when there is more than one object
               node with the same name or class in the search path.

  Input:
          base_node  - A pointer to the base node for the search.
          start_node - A pointer to the starting node for the search.  The
                       search does not include the start node.
          name       - Pointer to a character string containing the name of
                       the object to be located.  This pointer can be NULL:
                       if it is, the first object node of the specified
                       class is returned, regardless of its name.
          class      - Pointer to a character string containing the class of
                       object to be located. If this pointer is NULL, the
                       first object node located with the specified name
                       is returned, regardless of its class.
 
  Output: A pointer to the next object node that has the specified name
          and/or class is returned as function value. If no match is
          found, then the NULL value is returned.  The NULL value is
          also returned if either the input base node or start node
          pointers are NULL, or if both the name and class arguments
          are NULL.

*****************************************************************************/
 

OBJECT FindNextObject (base_node,start_node,name,class)

     AGGREGATE   base_node;
     AGGREGATE   start_node;
     char       *name;
     char       *class;

{
  AGGREGATE  node;                    /* Pointer to current node           */


  if (base_node == NULL || start_node == NULL ||
      (name == NULL && class == NULL))
    {
      return (NULL);
    }

  /* Start searching from the start node and stop searching when we
     find an object node with the specified name and/or class, or
     when we have visited all of the progeny of the base node  */

  node = NextSubObject (base_node, start_node);

  while (node != NULL)
    { 
      if ((name  == NULL || strcmp (node->name,  name)  == 0) &&
          (class == NULL || strcmp (node->class, class) == 0))
        {
          break;
        }

      node = NextSubObject (base_node, node);
    }

  return (node);
}
