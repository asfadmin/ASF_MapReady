/*****************************************************************************

  Description: This file contains C routines for navigating among the group
               nodes in an ODL tree. These routines augment the general
               purpose routines for manipulating aggregate nodes by
               providing mechanisms to locate group nodes, while
               ignoring object nodes. 

  Author:  Randy Davis, University of Colorado LASP
 
  Creation Date:  13 March 1991
  last Modified:  18 May 1991

  History:

    Creation - This set of routines was introduced in the Version 2.1
    release of the ODLC library.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h.

*****************************************************************************/

#include  "odldef.h"



/*****************************************************************************

  Routine: ParentGroup
 
  Description: Returns a pointer to the group containing an aggregate node.
 
  Input:
          base_node - A pointer to an aggregate node.
 
  Output: If the input aggregate node is within a group, a pointer to that
          group node is returned as the function value.  If the input node
          is not within a group, the NULL value is returned.  The NULL
          value is also returned if the input argument is NULL.

*****************************************************************************/
 

GROUP ParentGroup (base_node)

     AGGREGATE   base_node;

{
  AGGREGATE  node;               /* Pointer to current aggregate node       */


  node = base_node;

  do
    {
      node = ParentAggregate (node);
    } while (node != NULL && node->kind != KA_GROUP);

  return (node);
}




/*****************************************************************************

  Routine: NextGroup
 
  Description: Returns the next group node after the input aggregate node,
               according to a pre-order traversal of the ODL tree.
 
  Input:
          base_node  - A pointer to the starting aggregate node.
 
  Output: A pointer to the next group node is returned as the function value.
          The NULL value is returned if there is no next group node.  The
          NULL value is also returned if the input argument is NULL.

*****************************************************************************/
 

GROUP NextGroup (base_node)

     AGGREGATE   base_node;

{
  AGGREGATE  node;                    /* Pointer to the next node           */


  node = base_node;

  do
    {
      node = NextAggregate (node);
    } while (node != NULL && node->kind != KA_GROUP);

  return node;
}



/*****************************************************************************

  Routine: NextSubGroup

  Description: Returns the next group of the base node after the one
               pointed to by the start node.

  Input:
          base_node  - A pointer to the base aggregate node.
          start_node - A pointer to the starting node.

  Output: A pointer to the next group sub-node of the base node is
          returned as the function value.  A NULL value indicates that
          there is no next group sub-node.  The NULL value is also
          returned if the input argument is NULL.

*****************************************************************************/


GROUP NextSubGroup (base_node,start_node)

     AGGREGATE   base_node;
     AGGREGATE   start_node;

{
  AGGREGATE  node;                    /* Pointer to the next node           */


  node = start_node;

  do
    {
      node = NextSubAggregate (base_node, node);
    } while (node != NULL && node->kind != KA_GROUP);

  return node;
}




/*****************************************************************************

  Routine: FindGroup
 
  Description: Finds a group node with a specified name.  The search starts
               with the base node and continues as necessary with all
               of the base nodes's children, all their children, and
               so on using a preorder search.
 
  Input:
          base_node - A pointer to the starting aggregate node.
          name      - Pointer to a character string with the name of the
                      group to be located. 
 
  Output: A pointer to the first group node that has the specified name.
          If no match is found, then the NULL value is returned.  The
          NULL value is also returned if either of the input arguments
          is NULL.

*****************************************************************************/
 

GROUP FindGroup (base_node,name)

     AGGREGATE   base_node;
     char       *name;

{
  AGGREGATE  node;                    /* Pointer to current node           */


  if (base_node == NULL || name == NULL)
    {
      return (NULL);
    }

  /* Start searching with the base node and stop searching when we
     find a group node with the specified name or when we have
     visited all of the progeny of the base node  */

  node = base_node;

  while (node != NULL)
    {
      if (node->kind == KA_GROUP && strcmp (node->name, name) == 0)
        {
          break;
        }

      node = NextSubGroup (base_node, node);
    }

  return (node);
}




/*****************************************************************************

  Routine: FindNextGroup
 
  Description: This routine is similar to routine FindGroup in that it
               searches the progeny of the specified base node, except
               that the search begins with the node specified by the second
               argument. This is useful when there is more than one group
               node with the same name in the search path.

  Input:
          base_node  - A pointer to the base aggregate node for the search.
          start_node - A pointer to the starting node for the search.  The
                       search does not include the start node.
          name       - Pointer to a character string with the name of the
                       group to be located. 
 
  Output: A pointer to the next group node that has the specified name is
          returned as the function value. If no match is found, then the
          NULL value is returned. The NULL value is also returned if any
          of the three input arguments are NULL.

*****************************************************************************/
 

GROUP FindNextGroup (base_node,start_node,name)

     AGGREGATE   base_node;
     AGGREGATE   start_node;
     char       *name;

{
  AGGREGATE  node;                    /* Pointer to current node           */


  if (base_node == NULL || start_node == NULL || name == NULL)
    {
      return (NULL);
    }

  /* Start searching from the start node and stop searching when we find
     a group node with the specified name or when we have visited all of
     the progeny of the base node  */

  node = NextSubGroup (base_node, start_node);

  while (node != NULL && strcmp (node->name, name) != 0)
    { 
      node = NextSubGroup (base_node, node);
    } 

  return (node);
}
