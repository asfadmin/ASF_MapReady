/*****************************************************************************

  Description:  This file contains action routines for the Object
                Description Language parser.  The routines are
                called by the parser when specific production
                rules are triggered.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date:  20 March 1989
  Last Modified:  18 May 1991

  History:

    Creation - This set of routines was introduced in Version 1 of
    the ODLC library.

    Version 2.0 - 26 November 1990 - R. Davis, U. of Colorado LASP
      a) Upgraded routines to be compatible with Version 2 of ODL.  Some
         of the routine names and calling sequences have changes as a
         result of this upgrade.
      b) Converted to use new warning and error reporting scheme.
      c) Created routine ODLCheckSequence to perform checks on
         sequences previously performed as part of ODLMarkAttribute.
      d) Defined global variables in this module rather than in ReadLabel.

    Version 2.1 - 13 March 1991 - R. Davis, U. of Colorado LASP
      a) Modified the calling sequence to these routines so that only
         pointers to value data structures are passed as arguments,
         rather than having the entire structure copied in.
      b) Added a new global variable ODLcurrent_comment to hold a pointer to
         the most recently encountered comment.  Also added code to 
         ODLBeginAggregate and ODLBeginParameter to attach the current
         comment, if any, to the new aggregate or parameter node.
      c) Moved the definition of global variables ODLerror_count and
         ODLwarning_count out of this module.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Removed include statements that were Unix specific and placed them
       in odldef.h. Removed function prototypes that are now in an
       include file and added include file odlinter.h.
    b) Added PDS_TOOLBOX options to store a line number in each parameter
       and aggregate structure.

*****************************************************************************/

#include "odldef.h"
#include "odlinter.h"

/* The following variables are defined in this module and referenced
   in other modules.  Note that their presence makes ODL parsing
   non-re-entrant */

AGGREGATE   ODLroot_node;              /* Pointer to root node of ODL tree  */
AGGREGATE   ODLcurrent_aggregate;      /* Pointer to current aggregate node */
PARAMETER   ODLcurrent_parameter;      /* Pointer to current parameter node */
VALUE       ODLcurrent_value;          /* Pointer to current value node     */
char       *ODLcurrent_comment;        /* Pointer to current comment        */

/* The following variables are defined in the module containing the output
   routines ODLPrintError and ODLPrintWarning */

extern int  ODLerror_count;            /* Cumulative count of errors        */
extern int  ODLwarning_count;          /* Cumulative count of warnings      */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/14/91  Added external definition of yylineno so     >>>>> */
/* >>>>>              line numbers could be placed in aggregates   >>>>> */
/* >>>>>              and parameters.                              >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

#ifdef PDS_TOOLBOX

extern int yylineno;

#endif

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */


/*****************************************************************************

  Routine: ODLBeginAggregate

  Description:  Creates a new aggregate node on the ODL tree in response to
                the processing of an OBJECT = name or GROUP = name line.

  Input:   kind - Aggregate kind: object (KA_OBJECT) or group (KA_GROUP).
           item - Pointer to a value data structure containing a string
                  with the name of the new object or group.
          
  Output:  None.  A new aggregate node is attached to the ODL tree.
           
*****************************************************************************/


void ODLBeginAggregate (kind,item)
     
     AGGREGATE_KIND    kind;
     VALUE_DATA       *item;

{

  /* Create a new aggregate node */

  ODLcurrent_aggregate = NewAggregate (ODLcurrent_aggregate, kind,
                                       item->value.string, "");

  /* Free up the memory space used to hold the aggregate name */

  free (item->value.string);

  /* Attach the current comment, if any, to the new aggregate  */

  ODLcurrent_aggregate->comment = ODLcurrent_comment;
  ODLcurrent_comment = NULL;

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/14/91  Added code to set the appl1 field in the     >>>>> */
/* >>>>>              aggregate to the current line number         >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

#ifdef PDS_TOOLBOX

  ODLcurrent_aggregate -> appl1 = yylineno;

#endif

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */

  return;
}




/*****************************************************************************

  Routine: ODLEndAggregate
 
  Description:  Closes out processing for the current aggregate node
                and reverts to its parent.
 
  Input:   kind - Aggregate kind: indicates whether an END_OBJECT or
                  END_GROUP statement was seen by the parser.
           item - Pointer to a value data structure containing a string
                  with the object/group name found in the END_OBJECT or
                  END_GROUP (if any).
           
  Output:  None.
           
*****************************************************************************/


void ODLEndAggregate (kind,item)

     AGGREGATE_KIND   kind;
     VALUE_DATA      *item;

{
  char  errmsg[120];                      /* Error message text          */


  if (ODLcurrent_aggregate != ODLroot_node)
    {
      if (ODLcurrent_aggregate->kind == kind)
        {
          /* Make sure that the name in the END_OBJECT or END_GROUP part
             matches the name that was given previously in the OBJECT
             or GROUP part */

          if (item->value.string != NULL)
            {
              if (strcmp (item->value.string, ODLcurrent_aggregate->name) != 0)
                {
                  sprintf (errmsg,
                           "END_%s = %s doesn't match %s = %s",
                           (kind==KA_OBJECT) ? "OBJECT" : "GROUP",
                           item->value.string,
                           (kind==KA_OBJECT) ? "OBJECT" : "GROUP",
                           ODLcurrent_aggregate->name);
                  ODLPrintWarning (errmsg);
                }

              /* Free up the memory space used to hold the name */

              free (item->value.string);
            }

          /* Revert to the parent object or group */

          ODLcurrent_aggregate = ParentAggregate (ODLcurrent_aggregate);
        }
      else
        {
          /* Found an END_GROUP when expecting END_OBJECT, or vice versa */

          sprintf (errmsg,
               "Found END_%s when expecting END_%s - Ignored",
               (kind==KA_OBJECT) ? "OBJECT" : "GROUP",
               (ODLcurrent_aggregate->kind==KA_OBJECT) ? "OBJECT" : "GROUP");
          ODLPrintError (errmsg);
        }
    }
  else
    {
      /* Found an extra END_GROUP or END_OBJECT */

      sprintf (errmsg,
               "Encountered an extra END_%s - Ignored",
               (kind==KA_OBJECT) ? "OBJECT" : "GROUP");
      ODLPrintError (errmsg);
    }

  /* Get rid of any 'dangling' comment, since we don't have any parameter
     to attach it to */

  if (ODLcurrent_comment != NULL)
    {
      free (ODLcurrent_comment);
      ODLcurrent_comment = NULL;
    }

  return;
}




/*****************************************************************************

  Routine: ODLBeginParameter
 
  Description:  Creates a new parameter node for the current object or group.
 
  Input:   kind - Parameter kind: attribute (KP_ATTRIBUTE) or
                  pointer (KP_POINTER).
           item - Pointer to a value data structure containing a string
                  with the name of the new parameter.
           
  Output:  None.  A new parameter node is attached to the current object
                  or group node.
           
*****************************************************************************/


void ODLBeginParameter (kind,item)

     PARAMETER_KIND   kind;
     VALUE_DATA      *item;

{
  char  warning[120];                      /* Warning message text          */


  /* Look to see if a parameter with this name already exists for the
     current object or group */

  if (FindParameter (ODLcurrent_aggregate, item->value.string) != NULL)
    {
      /* It does: issue a warning */

      sprintf (warning,
               "A parameter named %s already exists for %s %s.",
               item->value.string,
               (ODLcurrent_aggregate->kind==KA_OBJECT) ? "object" : "group",
               ODLcurrent_aggregate->name);
      ODLPrintWarning (warning);
    }

  /* Create the new parameter node */

  ODLcurrent_parameter = NewParameter (ODLcurrent_aggregate, kind,
                                       item->value.string);

  /* Free up the memory space used to hold the parameter name */

  free (item->value.string);

  /* Attach the current comment, if any, to the new parameter  */

  ODLcurrent_parameter->comment = ODLcurrent_comment;
  ODLcurrent_comment = NULL;

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/14/91  Added code to set the appl1 field in the     >>>>> */
/* >>>>>              parameter to the current line number         >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

#ifdef PDS_TOOLBOX

  ODLcurrent_parameter -> appl1 = yylineno;

#endif

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */

  return;
}




/*****************************************************************************

  Routine: ODLMarkParameter
 
  Description:  Puts information about the kind of parameter value into
                the current parameter node.
 
  Input:   kind - Kind of parameter value: scalar (KV_SCALAR),
                  sequence (KV_SEQUENCE) or set (KV_SET)
           
  Output:  None. The current parameter node is modified.
           
*****************************************************************************/


void ODLMarkParameter (kind)

     VALUE_KIND    kind;

{

  /* Save the parameter value kind */

  ODLcurrent_parameter->value_kind = kind; 

  return;
}




/*****************************************************************************

  Routine: ODLStoreValue
 
  Description:  Adds a value to the current parameter and records
                information about the value.
 
  Input:   item - An value data structure containing the value to be stored.
           
  Output:  None.  A new value is attached to the current parameter node.
           
*****************************************************************************/


void ODLStoreValue (item)

     VALUE_DATA    *item;

{

  /* Attach the new value node to the current parameter */

  ODLcurrent_value = NewValue (ODLcurrent_parameter, item);

  return;
}



    
/*****************************************************************************

  Routine: ODLStoreUnits1
 
  Description:  Store a units value without an exponent.
 
  Input:
           name     - Pointer to a value data structure with a string
                      specifying the units name.

  Output:  None.  A units field descriptor structure is created.
           
*****************************************************************************/


void ODLStoreUnits1 (name)

     VALUE_DATA     *name;

{
  VALUE_DATA    exponent;   /* A value data structure for default exponent  */

  /* Since no exponent was specified we will set it to the default value +1 */

  exponent.type = TV_INTEGER;
  exponent.value.integer.number = 1;

  /* Now we can call our sister routine to create and store the new units
     field descriptor */

  ODLStoreUnits2 (name, &exponent);

  return;
}




/*****************************************************************************

  Routine: ODLStoreUnits2
 
  Description:  Store a units value with an exponent.
 
  Input:
           name     - Pointer to a value data structure containing a
                      string with the units name.
           exponent - Pointer to a value data structure containing the
                      integer exponent value.

  Output:  None.  A units field descriptor structure is created.
           
*****************************************************************************/


void ODLStoreUnits2 (name,exponent)

     VALUE_DATA    *name;
     VALUE_DATA    *exponent;

{
  struct ODLUnits *new_units;      /* Pointer to new units field descriptor */
  struct ODLUnits *current_units;  /* Pointer to current field descriptor   */


  /* Allocate memory in which to store the units field descriptor */

  new_units = (struct ODLUnits *) malloc (sizeof(struct ODLUnits));
  if (new_units == NULL)
    {
      ODLPrintWarning ("Memory allocation failure while storing units");
    }
  else
    {
      /* Copy the pointer to the units designator */
 
      new_units->designator = name->value.string;

      /* Set the units exponent */

      new_units->exponent = exponent->value.integer.number;

      /* Indicate that this is the last units field descriptor in the chain */

      new_units->next_field = NULL;

      /* Store the units field descriptor */

      switch (ODLcurrent_value->item.type)
       {
        case TV_REAL:

          current_units = ODLcurrent_value->item.value.real.units;
          if (current_units != NULL)
            {
              break;
            }
   
          /* This is the first units field for a real value */

          ODLcurrent_value->item.value.real.units = new_units;
          return;
   
        case TV_INTEGER:

          current_units = ODLcurrent_value->item.value.integer.units;
          if (current_units != NULL)
            {
              break;
            }

          /* This is the first units field for an integer value */

          ODLcurrent_value->item.value.integer.units = new_units;
          return;

        default:
          /* This should never happen */

          free (new_units->designator);
          free (new_units);
          return;
       }

      /* There is already at least one units field: find the end of the
         chain and tack this units field onto the end */
 
      while (current_units->next_field != NULL)
        {
          current_units = current_units->next_field;
        }

      current_units->next_field = new_units;
    }

  return;
}

    

    
/*****************************************************************************

  Routine: ODLMarkUnits
 
  Description:  Modifies the sign of the exponent for the most recently
                recorded units field to indicate whether the units field
                is a multiplier or a divisor relative to the previous
                units expression.  For example, if a units expression of
                <KM/SEC> is encountered, this routine will be called with
                a value of -1 to indicate that the exponent associated
                with the units field for seconds must be negated.  The
                expression will then be stored internally as KM^1, SEC^-1.
 
  Input:
           exponent_sign - Set to +1 if the units field is a multiplier (for
                           which the units exponent is given a positive value)
                           or -1 if the units field is to be a divisor (for
                           which the units exponent needs to be negative).

  Output:  None.  The last units field descriptor structure may be modified.
           
*****************************************************************************/


void ODLMarkUnits (exponent_sign)

     int     exponent_sign;

{
  struct ODLUnits *current_units;  /* Pointer to current field descriptor   */


  /* If the units field is acting as a multiplier then the exponent sign
     is already set properly and we don't have to do anything.  Otherwise,
     we need to negate the sign of the last units field exponent. */

  if (exponent_sign < 0)
    {
      /* We need to negate the exponent sign: find the last
         units field descriptor */

      if (ODLcurrent_value->item.type == TV_REAL)
        {
          current_units = ODLcurrent_value->item.value.real.units;
        }
      else if (ODLcurrent_value->item.type == TV_INTEGER)
        {
          current_units = ODLcurrent_value->item.value.integer.units;
        }
      else
        {
          current_units = NULL;
        }

      if (current_units != NULL)
        {
          while (current_units->next_field != NULL)
            {
              current_units = current_units->next_field;
            }

          /* Negate the exponent */

          current_units->exponent = -current_units->exponent;
        }
    }
  
  return;
}




/*****************************************************************************

  Routine: ODLCheckSequence
 
  Description:  Keeps track of number of rows and columns in a sequence.
 
  Input:   None.
           
  Output:  Warning message printed if problem detected.
           
*****************************************************************************/


void ODLCheckSequence ()
{
  char  warning[120];           /* Character string to hold warning message */


  /* Increment the count of rows.  If this is the first row, establish the
     number of columns per row.  All further rows should have the same
     number of columns. */

  ODLcurrent_parameter->rows++;
  if (ODLcurrent_parameter->rows == 1)
    {
      ODLcurrent_parameter->columns = ODLcurrent_parameter->value_count;
    }
  else if (ODLcurrent_parameter->rows*ODLcurrent_parameter->columns !=
           ODLcurrent_parameter->value_count)
    { 
      sprintf (warning,
          "Row %d of sequence has different number of columns than first row",
          ODLcurrent_parameter->rows);
      ODLPrintWarning (warning);
    }

  return;
}




/*****************************************************************************

  Routine: ODLCheckRange
 
  Description:  Checks the low and high range values.
 
  Input:
           low  - Value data structure containing the low value of the range.
           high - Value data structure containing the high value of the range.

  Output:  Warning message printed if a problem is detected.
           
*****************************************************************************/


void ODLCheckRange (low,high)

     VALUE_DATA    *low;
     VALUE_DATA    *high;

{

  /* Check the high and low range values */

  if (high->value.integer.number < low->value.integer.number)
    {
      ODLPrintWarning ("Low value of range is greater than high value");
    }

  /* Since there is no special range value kind in ODL Version 2, establish
     that the range value will be represented as a sequence. */

  ODLCheckSequence ();

  return;
}




/*****************************************************************************

  Routine: ODLEndLabel
 
  Description:  Finishes processing when the end of a label is found.
 
  Input:   None.
           
  Output:  The function value is set to 1 if no errors occurred during
           parsing; otherwise the value is set to 0.
           
*****************************************************************************/


int ODLEndLabel ()
{
  char  error_msg[120];                 /* Error message text               */


  while (ODLcurrent_aggregate != ODLroot_node)
    {
      /* Put out an error message for each missing END_OBJECT or END_GROUP  */

      sprintf (error_msg,
               "Missing END_%s for %s %s.",
               (ODLcurrent_aggregate->kind==KA_OBJECT) ? "OBJECT" : "GROUP",
               (ODLcurrent_aggregate->kind==KA_OBJECT) ? "object" : "group",
               ODLcurrent_aggregate->name);
      ODLPrintError(error_msg);

      /* Point to the current aggregate node's parent */
    
      ODLcurrent_aggregate = ParentAggregate (ODLcurrent_aggregate);
    }

  return ((ODLerror_count+ODLwarning_count) == 0);
}
