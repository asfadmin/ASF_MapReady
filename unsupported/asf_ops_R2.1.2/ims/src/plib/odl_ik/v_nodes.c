/*****************************************************************************

  Description:  This file contains C routines to store and manipulate the
                values assigned to an attribute or pointer.
 
  Author:  Randy Davis, University of Colorado LASP
 
  Creation Date:  12 March 1989
  Last Modified:  18 May 1991

  History:

    Creation - This set of routines was part of the Version 1 release of
    the ODLC library.

    Version 2.0 - 30 August 1990 - R. Davis, U. of Colorado LASP
      a) Changed to work with Version 2 parameter routines rather than
         the older Version attribute routines.

    Version 2.1 - 13 March 1991 - R. Davis, U. of Colorado LASP
      a) Added routines for cutting, copying and pasting values and
         recoded NewValue and RemoveValue to use these routines.
         The paste and cut routines increment and decrement the count
         of value nodes attached to a parameter.  Since NewValue and
         RemoveValue now use the paste and cut routines, they too
         update the value node counter.  Previously this updating had
         to be done outside of these routines.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Removed include statements that were Unix specific and placed them
       in odldef.h.
    b) Added code to NewValue to allocate space for string values rather
       than using the space passed in.

*****************************************************************************/

#include  "odldef.h"




/*****************************************************************************

  Routine: NewValue
 
  Description: Adds a new value to a parameter.
 
  Input:
          parameter  - Pointer to the parameter node to which the value
                       is to be attached.
          value_data - Pointer to the data value and its format.

  Output: A pointer to the value node is returned as the function value.  A
          NULL value indicates that the value could not be added, probably
          due to lack of dynamic memory.  The NULL value is also returned
          if the value data argument is NULL.
 
*****************************************************************************/


VALUE NewValue (parameter,value_data)

     PARAMETER   parameter;
     VALUE_DATA *value_data;

{
  VALUE  value;                /* Pointer to the new value node             */


  if (value_data == NULL)
    {
      return (NULL);
    }

  /* Allocate memory for the new value node */

  value = (VALUE) malloc (VALUE_NODE_SIZE);
  if (value == NULL)
    {
      /* Error: couldn't allocate memory to store value node */

      return (NULL);
    }

  /* Copy the value data into the new value node */

  value->item = *value_data;

  if (parameter != NULL)
    {
      /* Attach the new value node to its parameter */

      PasteValue (parameter, value);
    }
  else
    {
      /* There is no parameter to attach this value node to, so
         set the parent and sibling pointers to NULL */

      value->parameter     = NULL;
      value->left_sibling  = NULL;
      value->right_sibling = NULL;
    }
  return (value);
}




/*****************************************************************************

  Routine: RemoveValue
 
  Description: Removes a value of a parameter.  The memory space used to
               hold the value node plus any additional memory used by
               the value itself is returned to the system.
 
  Input:
         value      - A pointer to the value node to be removed.
 
  Output: A pointer to the next value node is returned as the function 
          value.  The NULL value is returned if there is no next value
          for the parameter.  The NULL value is also returned if the
          input argument is NULL.

*****************************************************************************/
 

VALUE RemoveValue (value)

     VALUE     value;

{
  VALUE      r_value;           /* Pointer to value's right sibling         */
  struct ODLUnits *units_field; /* Pointer to current units field descriptor*/
  struct ODLUnits *next_field;  /* Pointer to next units field descriptor   */


  /* Return immediately if there is nothing to remove */

  if (value == NULL)
    {
      return (NULL);
    }

  /* Free up any memory space used for storing the value.  This is the case
     for string and symbol values and for the units of a real or integer
     number */

  switch (value->item.type)
   {
    case TV_STRING:  case TV_SYMBOL:

      /* Free up the space used to hold the text of the string or symbol */

      free (value->item.value.string);
      break;

    case TV_INTEGER: case TV_REAL:

      /* Free up the space used to hold any units fields */

      if (value->item.type == TV_INTEGER)
        {
          units_field = value->item.value.integer.units;
        }
      else
        {
          units_field = value->item.value.real.units;
        }

      while (units_field != NULL)
        {
          next_field = units_field->next_field;
          free (units_field->designator);
          free (units_field);
          units_field = next_field;
        }

      break;

    default:

      break;
   }

  /* Save a pointer to the next value node in the list of values, if any */

  r_value = value->right_sibling;

  /* Detach the value from its parameter node */

  CutValue (value);

  /* Free up the memory space used to hold the value node */

  free (value);

  /* Return a pointer to the next value attached to the parameter, if any */

  return (r_value);
}




/*****************************************************************************

  Routine: FirstValue
 
  Description: Returns the first value associated with a parameter.
 
  Input:
          parameter - A pointer to the parameter.
 
  Output: A pointer to the first value node is returned as the function.
          The value NULL is returned if there are no values for this
          parameter.  The NULL value is also returned if the input
          argument is NULL.

*****************************************************************************/
 

VALUE FirstValue (parameter)

     PARAMETER   parameter;

{

  if (parameter == NULL)
    {
      return (NULL);
    }
  else
    {
      return (parameter->first_value);
    }

}




/*****************************************************************************

  Routine: NextValue
 
  Description: Returns the next value of an attribute or pointer.
 
  Input:
          value      - A pointer to the current value node.
 
  Output: A pointer to the next value node is returned as the function
          value.  The value NULL is returned if there are no more 
          values for the parameter.  The NULL value is also returned
          if the input argument is NULL.

*****************************************************************************/
 

VALUE NextValue (value)

     VALUE     value;

{

  if (value == NULL)
    {
      return (NULL);
    }
  else
    {
      return (value->right_sibling);
    }

}




/*****************************************************************************

  Routine: CopyValue
 
  Description: Copies a value node.
 
  Input:
          value   - A pointer to the value node to be copied.  The
                    input value node remains in the ODL tree after
                    the copy is made.
 
  Output: A pointer to the copied value node is returned as the function
          result. The NULL value is returned if the input argument is
          NULL or if storage couldn't be obtained for the copy.

*****************************************************************************/
 

VALUE CopyValue (value)

     VALUE     value;

{
  VALUE            value_copy;       /* Copy of the specified value         */
  VALUE_DATA       item_copy;        /* Copy of the value data structure    */
  struct ODLUnits *units;            /* Pointer to units field being copied */
  struct ODLUnits *units_copy;       /* Copy of the units structure         */
  struct ODLUnits *next_field;       /* Pointer to next units field, if any */
  struct ODLUnits *first_units_copy; /* Pointer to first units field copy   */
  struct ODLUnits *last_units_copy;  /* Pointer to last units field copy    */
  char            *string;           /* Copy of name string                 */


  if (value == NULL)
    {
      return (NULL);
    }

  /* Fill in the value type-independent fields of the value data structure */

  item_copy.type      = value->item.type;
  item_copy.valid     = value->item.valid;
  item_copy.format    = value->item.format;
  item_copy.precision = value->item.precision;
  item_copy.length    = value->item.length;

  /* Fill in the value type-dependent part of the value data structure */

  switch (item_copy.type)
    {
      case TV_STRING:  case TV_SYMBOL:

        /* Copy character string and attach it to the value node */

        string = (char *) malloc (item_copy.length+1);
        if (string == NULL)
          {
            /* Error: couldn't get memory for the copy of the string */

            return (NULL);
          }

        item_copy.value.string = strcpy (string, value->item.value.string);

        break;

      case TV_INTEGER:  case TV_REAL:

        /* Copy the number value and get a pointer to first units, if any */

        if (item_copy.type == TV_INTEGER)
          {
            item_copy.value.integer.number = value->item.value.integer.number;
            units = value->item.value.integer.units;
          }
        else
          {
            item_copy.value.real.number = value->item.value.real.number;
            units = value->item.value.real.units;
          }

        first_units_copy = NULL;

        while (units != NULL)
          {
            /* Allocate space for the copy of the units field */

            units_copy = (struct ODLUnits *) malloc (sizeof(struct ODLUnits));
            string = (char *) malloc (strlen (units->designator)+1);

            if (units_copy == NULL || string == NULL)
              {
                /* Error: couldn't get enough storage space to copy
                   the units.  Rather than leave the copy operation
                   in an unknown state, we release all storage space
                   and return */

                units_copy = first_units_copy;

                while (units_copy != NULL)
                  {
                    next_field = units_copy->next_field;
                    free (units_copy->designator);
                    free (units_copy);

                    units_copy = next_field;
                  }

                return (NULL);
              }

            /* Copy the units information */

            units_copy->designator = strcpy (string, units->designator);
            units_copy->exponent   = units->exponent;
            units_copy->next_field = NULL;

            /* Attach the units information */

            if (first_units_copy == NULL)
              {
                first_units_copy = units_copy;
              }
            else
              {
                last_units_copy->next_field = units_copy;
              }

            last_units_copy = units_copy;
            units = units->next_field;
          }

        /* Attach the units field(s) to the value node */

        if (item_copy.type == TV_INTEGER)
          {
            item_copy.value.integer.units = first_units_copy;
          }
        else
          {
            item_copy.value.real.units = first_units_copy;
          }

        break;

      case TV_DATE:  case TV_TIME:   case TV_DATE_TIME:

        /* Copy the date and time fields */

        item_copy.value.date_time = value->item.value.date_time;

        break;

      default:

        break;
     }

  /* Create a new stand-alone value node containing the copied data */

  value_copy = NewValue (NULL, &item_copy);
 
  return (value_copy);
}




/*****************************************************************************

  Routine: CutValue
 
  Description: Detaches a value node from its parameter node.
 
  Input:
         value      - A pointer to the value node to be cut.
 
  Output: A pointer to the cut value node is returned as the function 
          result.  The NULL value is returned if the input argument
          is NULL.

*****************************************************************************/
 

VALUE CutValue (value)

     VALUE     value;

{
  PARAMETER  parameter;         /* Pointer to parameter node                */
  VALUE      l_value;           /* Pointer to value's left sibling          */
  VALUE      r_value;           /* Pointer to value's right sibling         */

 /* Remove the value from the list of values for its parameter node */

  if (value == NULL)
     return (NULL);

  parameter = value->parameter;
  l_value   = value->left_sibling;
  r_value   = value->right_sibling;

  if (l_value != NULL)
   {
     l_value->right_sibling = r_value;
   }
  else if (parameter != NULL)
   {
     parameter->first_value = r_value;
   }

  if (r_value != NULL)
   {
     r_value->left_sibling = l_value;
   }
  else if (parameter != NULL)
   {
     parameter->last_value = l_value;
   }

  /* Decrement the count of value nodes attached to the parameter */

  if (parameter != NULL)
    {
      parameter->value_count--;
    }

  /* Reset the parent and sibling pointers to indicate this node now
     stands by itself */

  value->parameter     = NULL;
  value->left_sibling  = NULL;
  value->right_sibling = NULL;
  
  /* Return a pointer to the detached node */

  return (value);
}




/*****************************************************************************

  Routine: PasteValue
 
  Description: Attaches a value node to a parameter node.
 
  Input:
          parameter  - Pointer to the parameter node to which the value
                       node is to be attached.
          value      - Pointer to the value node to be attached.

  Output: A pointer to the pasted value node is returned as the function
          result.  A NULL value will be returned if either of the input
          arguments is NULL.
 
*****************************************************************************/


VALUE PasteValue (parameter,value)

     PARAMETER  parameter;
     VALUE      value;

{
  VALUE  old_last;             /* Pointer to previous last value            */


  if (parameter == NULL || value == NULL)
    {
      return (NULL);
    }

  /* Place the value node at the end of the list of values for the
     parameter */

  old_last = parameter->last_value;
  if (old_last != NULL)
    {
      old_last->right_sibling = value;
    }

  parameter->last_value = value;
  if (parameter->first_value == NULL)
    {
      parameter->first_value = value;
    }

  value->parameter     = parameter;
  value->left_sibling  = old_last;
  value->right_sibling = NULL;

  /* Increment the count of value nodes attached to the parameter */

  parameter->value_count++;

  /* Return a pointer to the pasted value node */

  return (value);
}
