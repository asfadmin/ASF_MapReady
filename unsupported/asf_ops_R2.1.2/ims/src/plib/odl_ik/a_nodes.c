/*****************************************************************************

  Description: This file contains C routines that create and manipulate
               aggregate nodes on an ODL tree.
 
  Author:  Randy Davis, University of Colorado LASP
 
  Creation Date:  30 August 1990
  Last Modified:  18 May 1991

  History:

    Creation - This set of routines was introduced in Version 2.0 of the
    ODLC library, replacing the similar set of routines available in
    Version 1 for manipulating object nodes.
      a) The routines NewAggregate, RemoveAggregate, FindAggregate,
         NextAggregate and ParentAggregate replace the routines NewObject,
         RemoveObject, FindObject, NextObject and ParentObject that were
         part of Version 1 of the ODLC library.
      b) The function performed by the Version 1 routine NewODLTree
         has been folded into routine NewAggregate.
      c) New routines FindNextAggregate, CopyAggregate, CutAggregate and
         PasteAggregate have been added to provide services useful for
         label editors and similar programs.

    Version 2.1 - 13 March 1991 - Randy Davis, U. of Colorado LASP
      a) Updated routines for cutting, copying and pasting aggregate nodes
         and modified some of the existing routines to take advantage of
         them.
      b) Added handling for the comment and application-specific fields
         that were added to aggregate nodes in Version 2.1.
      c) Added routine NextSubAggregate to improve navigation capabilities.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h.

*****************************************************************************/

#include  "odldef.h"




/*****************************************************************************

  Routine: NewAggregate
 
  Description: Inserts a new aggregate node into an ODL tree.                 
 
  Input:
          base_node   - A pointer to the node that will become the parent
                        of the new aggregate we are creating.  If this pointer
                        is NULL, then the new node will be a root node for
                        a new ODL tree.
          kind        - Kind of node: object (KA_OBJECT) or group (KA_GROUP).
          name        - Pointer to a character string containing the new
                        node's name.  A non-null name must be provided.
          class       - If the new aggregate node is an object node, this
                        character string contains the object's class.  It
                        may be NULL or a null string ("") if the class in
                        not specified. This argument is ignored for group
                        nodes.
 
  Output: A pointer to the new aggregate node is returned as the function
          value. A value of NULL indicates that the new node could not be
          created, probably due to a lack of dynamic memory.  A NULL value
          will also be returned if the name argument is NULL.

*****************************************************************************/
 

AGGREGATE NewAggregate (base_node,kind,name,class)

     AGGREGATE       base_node;
     AGGREGATE_KIND  kind;
     char           *name;
     char           *class;

{
  AGGREGATE  node;             /* Pointer to the new aggregate node         */
  char      *string;           /* Pointer to created character string       */


  if (name == NULL)
    {
      return (NULL);
    }

  /* Allocate memory for the new aggregate node */

  node = (AGGREGATE) malloc (AGGREGATE_NODE_SIZE);
  if (node == NULL)
    {
      /* Error: couldn't get memory to hold this node */

      return (NULL);
    }

  /* Set the node kind */

  node->kind = kind;

  /* Copy the node name */

  string = (char *) malloc (strlen (name)+1);
  if (string == NULL)
    {
      /* Error: probably no memory available for name string. Free up
         all space and return */

      free (node);
      return (NULL);
    }

  node->name = strcpy (string, name);

  /* Copy the class name.  If this is a group node, or if it is an
     object node but with no class name specified, then set the
     class name to a null string */

  if (kind == KA_OBJECT && class != NULL)
    {
      string = (char *) malloc (strlen (class)+1);
      if (string != NULL)
        {
          node->class = strcpy (string, class);
        }
    }
  else
    {
      string = (char *) malloc (1);
      if (string != NULL)
        {
          node->class = strcpy (string, "");
        }
    }

  if (string == NULL)
    {
      /* Error: couldn't store class name, probably because of lack
         of memory.  Free up all space and return */

      free (node->name);
      free (node);
      return (NULL);
    }
       
  /* Indicate no comment present as yet and initialize the
     application-specific fields  */

  node->comment = NULL;
  node->appl1   = 0;
  node->appl2   = 0;

  /* Indicate that there are no children of this node yet */

  node->first_child    = NULL;
  node->last_child     = NULL;

  /* Indicate that there are no parameters for this node yet */

  node->first_parameter = NULL;
  node->last_parameter  = NULL;

  /* If there is a base node specified, attach the new node to it */

  if (base_node == NULL)
    {
      /* Make the new node a root node, with no parent or siblings */
  
      node->parent        = NULL;
      node->left_sibling  = NULL;
      node->right_sibling = NULL;
    }
  else
    {
      /* Attach the new aggregate node to the base node */

      PasteAggregate (base_node, node);
    }

  return (node);
}




/*****************************************************************************

  Routine: RemoveAggregate
 
  Description: Removes an aggregate node and all its progeny.
 
  Input:
          base_node - A pointer to the aggregate node to be removed.
 
  Output: A pointer to the next aggregate node belonging to the same parent
          (the removed node's right sibling) is returned as function value.
          The NULL value is returned if there is no right sibling. The NULL
          value is also returned if the input argument is NULL.

*****************************************************************************/
 

AGGREGATE  RemoveAggregate (base_node)

     AGGREGATE   base_node;

{
  AGGREGATE node;          /* Pointer to current node                       */
  AGGREGATE r_node;        /* Pointer to base nodes's right sibling         */
  PARAMETER parameter;     /* Pointer to nodes's attributes and pointers    */


  if (base_node == NULL)
    {
      return (NULL);
    }

  /* Remove all the progeny of the base node through recursive calls to
     this function */

  node = base_node->first_child;

  while (node != NULL)
    {
      node = RemoveAggregate (node);
    }

  /* Remove all the parameters and values attached to the base node */

  parameter = FirstParameter (base_node);

  while (parameter != NULL)
    {
      parameter = RemoveParameter (parameter);
    }

  /* Save the location to the next aggregate in sequence, if any */

  r_node = base_node->right_sibling;

  /* Cut the base node from the tree */

  CutAggregate (base_node);

  /* Free up the memory space used to hold the node and the character
     strings containing the node's name, class and comment */

  free (base_node->comment);
  free (base_node->class);
  free (base_node->name);
  free (base_node);

  /* Return a pointer to the next aggregate node in sequence, if any */

  return (r_node);
}




/*****************************************************************************

  Routine: ParentAggregate
 
  Description: Returns a pointer to an aggregate node's parent.  
 
  Input:
          base_node - A pointer to an aggregate node.
 
  Output: A pointer to the parent node.  If the base node is the root node
          of an ODL tree, then NULL is returned.  The NULL value is also
          returned if the input argument is NULL.

*****************************************************************************/
 

AGGREGATE ParentAggregate (base_node)

     AGGREGATE  base_node;
     
{

  if (base_node == NULL)
    {
      return (NULL);
    }
  else
    {
      return (base_node->parent);
    }

}




/*****************************************************************************

  Routine: NextAggregate
 
  Description: Returns the aggregate node after the one pointed to by the
               input parameter.  The next node is determined using a preorder
               traversal of the tree.  Basically the scheme is as follows:
                 1) If the base node has children then its first child
                    is returned.
                 2) If a node has no children and is not the last child
                    of its parent, then the sibling to the immediate
                    right is returned.
                 3) Otherwise move to the node's parent and go back to
                    step 2.
                 4) If there is no next node then the value NULL will
                    be returned.
 
  Input:
          base_node  - A pointer to the starting aggregate node.
 
  Output: A pointer to the next node is returned as the function value.
          A NULL value indicates that there is no next node.  The NULL
          value is also returned if the input argument is NULL.

*****************************************************************************/
 

AGGREGATE NextAggregate (base_node)

     AGGREGATE    base_node;

{
  AGGREGATE  node;                    /* Pointer to the next node           */


  if (base_node == NULL)
    {
      return (NULL);
    }

  node = base_node;

  /* Check to see if the base node has children. If it does then
     return the base nodes's first child  */

  if (node->first_child != NULL)
    {
      node = node->first_child;
    }
  else
    {
      /* Search for a node in the ancestral path that has a
         right sibling and return that sibling */

      while (node != NULL)
        {
          if (node->right_sibling != NULL)
            {
              node = node->right_sibling;
              break;
            }

          node = node->parent;
        }
    }

  return (node);
}




/*****************************************************************************

  Routine: NextSubAggregate
 
  Description: Returns the next aggregate node after the starting node that
               is a sub-node of the base node.  The nodes are visited in
               pre-order.  If there are no more sub-nodes of the base
               node, the NULL value is returned.
 
  Input:
          base_node  - A pointer to the base aggregate node.
          start_node - A pointer to the aggregate node for which the
                       next node is desired.  The start node must
                       be the base node or a sub-node of the base node.
 
  Output: A pointer to the next node is returned as the function value.
          A NULL value indicates that there is no next sub-node for the
          base node.  The NULL value is also returned if either of the
          input arguments are NULL.

*****************************************************************************/
 

AGGREGATE NextSubAggregate (base_node,start_node)

     AGGREGATE   base_node;
     AGGREGATE   start_node;

{
  AGGREGATE  node;                    /* Pointer to the next node           */


  if (base_node == NULL || start_node == NULL)
    {
      return (NULL);
    }

  node = start_node;

  /* Check to see if the start node has children. If it does then
     return the start nodes's first child  */

  if (node->first_child != NULL)
    {
      node = node->first_child;
    }
  else
    {
      /* Search for a sub-node of the base node that has a
         right sibling and return that sibling */

      while (node != NULL)
        {
          if (node == base_node)
            {
              node = NULL;
              break;
            }
          else if (node->right_sibling != NULL)
            {
              node = node->right_sibling;
              break;
            }

          node = node->parent;
        }
    }

  return (node);
}




/*****************************************************************************

  Routine: FindAggregate
 
  Description: Finds a node with a specified name.  The search starts
               with the base node and continues as necessary with all
               of the base nodes's children, all their children, and
               so on using a preorder search.
 
  Input:
          base_node - A pointer to the starting aggregate node.
          name      - Pointer to a character string with the name of
                      the node to be located.
 
  Output: A pointer to the first node that has the specified name.  If no
          match is found, then the NULL value is returned. The NULL value
          is also returned if either of the input arguments is NULL.

*****************************************************************************/
 

AGGREGATE FindAggregate (base_node,name)

     AGGREGATE   base_node;
     char       *name;

{
  AGGREGATE  node;                    /* Pointer to current node           */


  if (base_node == NULL || name == NULL)
    {
      return (NULL);
    }

  /* Start searching with the base node and stop searching when we
     find a node with the specified name or when we have visited
     all of the sub-nodes of the base node  */

  node = base_node;

  while (node != NULL && strcmp (node->name, name) != 0)
    {
      node = NextSubAggregate (base_node, node);
    }

  return (node);
}




/*****************************************************************************

  Routine: FindNextAggregate
 
  Description: This routine is similar to routine FindAggregate in that it
               searches the progeny of the specified base node, except that
               the search begins with the node specified by the second
               argument. This is useful when there is more than one node
               with the same name in the search path.

  Input:
          base_node  - A pointer to the base node for the search.
          start_node - A pointer to the starting node for the search. The
                       search does not include the start node.
          name       - Pointer to a character string with the name of the
                       node to be located. 
 
  Output: A pointer to the next node that has the specified name. If no
          match is found, then the NULL value is returned.  The NULL
          value is also returned if any of the input arguments are NULL.

*****************************************************************************/
 

AGGREGATE FindNextAggregate (base_node,start_node,name)

     AGGREGATE    base_node;
     AGGREGATE    start_node;
     char        *name;

{
  AGGREGATE  node;                    /* Pointer to current node           */


  if (base_node == NULL || start_node == NULL || name == NULL)
    {
      return (NULL);
    }

  /* Start searching with the first node after the start node and stop
     searching when we find a node with the specified name or when we
     have visited all of the sub-nodes of the base node  */

  node = NextSubAggregate (base_node, start_node);

  while (node != NULL && strcmp (node->name, name) != 0)
    {
      node = NextSubAggregate (base_node, node);
    }

  return (node);
}




/*****************************************************************************

  Routine: CutAggregate
 
  Description: Detaches an aggregate node, along with its parameters and all
               of it progeny, from an ODL tree.
 
  Input:
          base_node - A pointer to the aggregate node to be cut.
 
  Output: A pointer to the cut node is returned as the function value.  The
          value will be NULL if the input argument is NULL.

*****************************************************************************/
 

AGGREGATE CutAggregate (base_node)

     AGGREGATE   base_node;

{
  AGGREGATE parent;        /* Pointer to base nodes's parent node           */
  AGGREGATE l_node;        /* Pointer to base nodes's left sibling          */
  AGGREGATE r_node;        /* Pointer to base nodes's right sibling         */


  if (base_node == NULL)
    {
      return (NULL);
    }

  /* Remove the base node from the tree and patch pointers in other 
     nodes, as needed */

  parent = base_node->parent;
  l_node = base_node->left_sibling;
  r_node = base_node->right_sibling;
  
  if (l_node != NULL)
    {
      l_node->right_sibling = r_node;
    }
  else if (parent != NULL)
    {
      parent->first_child = r_node;
    }

  if (r_node != NULL)
    {
      r_node->left_sibling = l_node;
    }
  else if (parent != NULL)
    {
      parent->last_child = l_node;
    }

  /* Reset the parent and sibling pointers since this aggregate now
     stands alone */

  base_node->parent        = NULL;
  base_node->left_sibling  = NULL;
  base_node->right_sibling = NULL;

  return (base_node);
}



/*****************************************************************************

  Routine: CopyAggregate
 
  Description: Copy an aggregate node with all of its parameters and
               all of the node's progeny (and their parameters).
 
  Input:
          base_node - A pointer to the starting node to be copied.  This
                      node remains in the ODL tree.
 
  Output: A pointer to the copy of the aggregate node is returned as the
          function result. The result will be NULL if the input aggregate
          node is NULL or if there is insufficient memory available to
          complete the copy operation.

*****************************************************************************/

 
AGGREGATE CopyAggregate (base_node)

     AGGREGATE   base_node;

{
  AGGREGATE  node_copy;      /* Pointer to copy of base node                */
  AGGREGATE  child_node;     /* Pointer to current child aggregate node     */
  AGGREGATE  child_copy;     /* Pointer to copy of current child aggregate  */
  PARAMETER  parameter;      /* Pointer to current parameter of base node   */
  PARAMETER  parameter_copy; /* Pointer to copy of current parameter        */


  if (base_node == NULL)
    {
      return (NULL);
    }

  /* Create a new aggregate node for the copy */

  node_copy = NewAggregate (NULL, base_node->kind,
                            base_node->name, base_node->class);
  if (node_copy == NULL)
    {
      /* Error: couldn't make a copy of the base node, probably
         because we ran out of memory */

      return (NULL);
    }

  /* Copy the comment attached to the node, if any */

  CommentAggregate (node_copy, base_node->comment);

  /* Copy all the aggregates's parameter nodes and their values and
     attach them to the aggregate node */

  parameter = FirstParameter (base_node);

  while (parameter != NULL)
    {
      parameter_copy = CopyParameter (parameter);
      PasteParameter (node_copy, parameter_copy);

      parameter = NextParameter (parameter);
    }

  /* Call this routine recursively to copy all of the children
     aggregate nodes */

  child_node = base_node->first_child;

  while (child_node != NULL)
    {
      /* Make a copy of the current child node */

      child_copy = CopyAggregate (child_node);
      if (child_copy == NULL)
        {
          /* Error: we apparently ran out of memory while copying.
             Rather than leave the copy operation only partially
             complete, we release all space used and return */

          RemoveAggregate (node_copy);
          return (NULL);
        }

      /* Attach the copied child node to the copy of the base node */

      PasteAggregate (node_copy, child_copy);

      child_node = child_node->right_sibling;
    }

  return (node_copy);
}



/*****************************************************************************

  Routine: PasteAggregate
 
  Description: Attach an aggregate node (along with its parameters and
               all of its progeny aggregate nodes) to the specified
               aggregate node.
 
  Input:
          base_node - A pointer to the node that will become the parent
                      to the inserted node.
   
          aggregate - A pointer to the aggregrate node to be attached.
 
  Output: A pointer to the attached aggregate node is returned as the
          function value.  If either of the input arguments is NULL,
          the NULL value will be returned.

*****************************************************************************/
 

AGGREGATE PasteAggregate (base_node,aggregate)

     AGGREGATE   base_node;
     AGGREGATE   aggregate;

{
  AGGREGATE  old_last;         /* Pointer to previously last child node     */


  if (base_node == NULL || aggregate == NULL)
    {
      return (NULL);
    }

  /* Attach the new node to the base node, updating the pointers
     in the previous and next aggregate nodes as necessary  */

  old_last = base_node->last_child;
  if (old_last != NULL)
    {
      old_last->right_sibling = aggregate;
    }

  base_node->last_child = aggregate;
  if (base_node->first_child == NULL)
    {
      base_node->first_child = aggregate;
    }

  aggregate->parent         = base_node;
  aggregate->left_sibling   = old_last;
  aggregate->right_sibling  = NULL;

  return (aggregate);
}
