/*****************************************************************************
                                                                           
  WriteLabel:  Writes a PDS label in the Object Description Language
               to the specified file.

  Input:
         output_file  - File pointer to output file.

         base_node    - Pointer to aggregate node of the ODL tree with which
                        writing is to begin.  This node, and all aggregates
                        beneath it will be written.
         
  Output:  There are no output parameters and no function value is returned.
           The label is written to the file using output routine ODLWriteStmt.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date: 11 October 1990
  Last Modified: 18 May 1991

  History:

  Creation - This routine was introduced in Version 1.0 of the ODLC library
  and upgraded significantly in Version 2.0.
  
  Version 2.1 - 13 March 1991 - Randy Davis, U. of Colorado LASP
  a) Now prints out comments associated with an aggregate or parameter.
  b) Eliminated use of the 'level' field, which has been deleted
     from aggregate nodes.
  c) Modified so that if both a name and class are specified for an
     object, they are printed out in the PDS standard format, ie:

        OBJECT = class
          NAME = name
          ... other parameters ...
        END_OBJECT = class

     If an object class is not given, then the old-style is used:

        OBJECT = name
          ... parameters ...
        END_OBJECT

     The latter style is always used for groups, since they don't have
     a class.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h. Removed ODL function prototypes and included file
    odlinter.h.

  Version 2.3 - 13 October 1991 - M. DeMore, Jet Propulsion Laboratory
    Modified to handle new argument list to ODLFormatString, in order
    to handle backslashes in DOS file names. Added check of pds_finish_label
    for Toolbox use.

 ****************************************************************************/

#include "odldef.h"
#include "odlinter.h"

/* The following define the maximum number of characters in a label line, the
   number of characters to indent on the left margin for each step up in
   aggregate nesting level, and the minimum spacing for a parameter name */

#define  MAXLABLINE 70
#define  INDENTSIZE  2
#define  MINAMESIZE 20

#ifdef PDS_TOOLBOX

extern int pds_finish_label;

#endif

/* The following macro is used to space in properly from the left margin */

#define indentf(X) for (i=0 ; i < X ; i++) stmt[len++] = ' '


void WriteLabel (output_file, base_node)
       FILE      *output_file;
       AGGREGATE  base_node;
{

  AGGREGATE  node;              /* Pointer to current node                  */
  AGGREGATE  old_node;          /* Pointer to previous node                 */
  PARAMETER  parameter;         /* Pointer to current parameter node        */
  VALUE      data;              /* Pointer to current parameter value       */
  int        i;                 /* Loop index                               */
  int        ip;                /* Number of parens to print for sequence   */
  int        ncol;              /* Current output line column number        */
  int        nindent;           /* Indentation for parameter names          */
  int        nl;                /* Length of current parameter name         */
  int        nlv;               /* Length of current value                  */
  int        no;                /* Alignment on OBJECT/END_OBJECT lines     */
  int        ns;                /* Alignment for parameter assignments      */
  int        nv;                /* Alignment for parameter values           */
  int        vindent;           /* Indentation for values                   */
  int        col_count;         /* Count of array columns processed         */
  char       stmt[ODLMAXSTMT];  /* Buffer to hold each statement            */
  int        len;               /* Number of characters in output statement */

  if (base_node == NULL)
    {
      return;
    }

  /* Process the base node first and then all the nodes beneath it */

  old_node = base_node->parent;
  node = base_node;
  nindent = 0;

  while (node != NULL)
    {
     /* Put out any END_OBJECTs or END_GROUPs that are needed */

     if (node != base_node && node->parent == old_node)
       {
         /* The new object or group belongs to the old node.  We defer the
            END_OBJECT or END_GROUP for the old node until we're done
            processing all of its dependent nodes, so we don't do anything
            here except put a blank line before the OBJECT or GROUP line
            if we're not going to put out a comment first */

         if (node->comment == NULL)
           {
             ODLWriteStmt (output_file, "\n");
           }
       }
     else
       {
         /* The new object or group is at the same or lower level than the old
            object or group. Put out the END_OBJECT or END_GROUP for the
            old node and all of its ancestors, up to and including the node
            at the same level as the new node */

         while (node->parent != old_node)
           {
             len = 0;
             nindent -= INDENTSIZE;
             indentf (nindent);
             no = MINAMESIZE + INDENTSIZE;

             if (old_node->kind  == KA_OBJECT && 
                 old_node->class != NULL && old_node->class[0] != '\0')
               {
                 sprintf (&stmt[len], "%-*s = %s\n\n",
                          no,
                          "END_OBJECT",
                          old_node->class);
               }
             else
               {
                 sprintf (&stmt[len], "%-*s = %s\n\n",
                          no,
                          (old_node->kind==KA_OBJECT)?"END_OBJECT":"END_GROUP",
                          old_node->name);
               }

             ODLWriteStmt (output_file, stmt);
             old_node = ParentAggregate (old_node);
           }
       }

     /* Put out the comment, if any associated with the new aggregate node */

     if (node->comment != NULL)
       {
         ODLFormatComment (stmt, node->comment, nindent+1, MAXLABLINE);
         ODLWriteStmt (output_file, stmt);
       }
 
     /* Put in the OBJECT = name or GROUP = name line for the new node,
        unless this is a root node */

     if (node->parent != NULL)
       {
         len = 0;
         indentf (nindent);
         nindent += INDENTSIZE;
         no = MINAMESIZE + INDENTSIZE;

         if (node->kind  == KA_OBJECT && 
             node->class != NULL && node->class[0] != '\0')
           {
             sprintf (&stmt[len], "%-*s = %s\n",
                      no,
                      "OBJECT",
                      node->class);
             len = strlen (stmt);

             indentf (nindent);
             sprintf (&stmt[len], "%-*s = %s\n",
                      MINAMESIZE,
                      "NAME",
                      node->name);
           }
         else
           {
             sprintf (&stmt[len], "%-*s = %s\n",
                      no,
                      (node->kind == KA_OBJECT) ? "OBJECT" : "GROUP",
                      node->name);
           }

         ODLWriteStmt (output_file, stmt);
       }

     /* Generate the attribute and pointer assignment statements for
        the current object or group */

     parameter = FirstParameter (node);

     while (parameter != NULL)
       {
         /* Print out the comment associated with the parameter, if any */

         if (parameter->comment != NULL)
           {
             ODLFormatComment (stmt, parameter->comment, nindent+1, MAXLABLINE);
             ODLWriteStmt (output_file, stmt);
           }
         
         len = 0;

         /* Determine the padding necessary to make the equal sign and
            values for this assignment line up nicely */

         ns = (parameter->node_kind==KP_POINTER) ? MINAMESIZE-1 : MINAMESIZE;

         /* Calculate the indentation to the start of the value for this
            assignment.  This amount of indentation will be applied if
            the value continues onto second and subsequent lines so that
            values will line up better */

         nl = strlen (parameter->name);
         nv = ((nl > ns)? nl : ns) + 3;
         vindent = nindent + nv;

         /* Write out the parameter name with appropriate indentation and
            padding */

         indentf (nindent);

         if (parameter->node_kind == KP_POINTER)
           {
             stmt[len++] = '^';
           }

         sprintf (&stmt[len], "%-*s = ",
                  ns,
                  parameter->name);
         len += nv;

         /* Perform value-kind dependent formatting */

         switch (parameter->value_kind)
           {
             case KV_SEQUENCE:
	       ip = (parameter->rows > 0)? parameter->rows : 1;
	       if (ip > 2) ip = 2;
               for (i=0; (i < ip); i++)
                 {
                   stmt[len++] = '(';
                   vindent++;
                 }
               break;

             case KV_SET:
               stmt[len++] = '{';
               vindent++;
               break;

             default:
               break;
           }

         /* Process every value for this assignment */

         col_count = 0;
         ncol = vindent;

         data = FirstValue (parameter);

         while (data != NULL)
           {
             if (data->item.type != TV_STRING)
               {
                 /* Determine the number of character spaces the new value
                    will occupy on the current line */

                 nlv = data->item.length;
                 if (data->item.type == TV_SYMBOL && data->item.format == 0)
                   {
                     /* This is a quoted symbolic literal: account for the
                        space occupied by the delimiting apostrophes */

                     nlv += 2;
                   }

                 ncol += nlv;

                 if (ncol >= MAXLABLINE)
                   {
                     /* Adding the new value would overflow the current line,
                        so start a new line */

                     stmt[len++] = '\n';
                     indentf (vindent);
                     ncol = vindent + nlv;
                   }
               }

             /* Format the data value, according to its data type */

             switch (data->item.type)
               {
                 case TV_INTEGER:
                   len += ODLFormatInteger (&stmt[len], &data->item);
                   break;

                 case TV_REAL:
                   len += ODLFormatReal (&stmt[len], &data->item);
                   break;

                 case TV_SYMBOL:
                   len += ODLFormatSymbol (&stmt[len], &data->item);
                   break;

                 case TV_STRING:
                   if (parameter->node_kind == KP_POINTER)
		      len += ODLFormatString (&stmt[len], &data->item, &ncol,
                                         nindent+2*INDENTSIZE, MAXLABLINE-2, 1, 1);
                   else
		      len += ODLFormatString (&stmt[len], &data->item, &ncol,
                                         nindent+2*INDENTSIZE, MAXLABLINE-2, 1, 0);
                   break;

                 case TV_DATE:
                   len += ODLFormatDate (&stmt[len], &data->item);
                   break;

                 case TV_TIME:
                   len += ODLFormatTime (&stmt[len], &data->item);
                   break;

                 case TV_DATE_TIME:
                   len += ODLFormatDateTime (&stmt[len], &data->item);
                   break;
               }
           
             /* Get the next data value for this assignment, if any */

             data = NextValue (data);

             /* Add any formatting that might be necessary to separate
                values */

            if (data != NULL)
              {
                switch (parameter->value_kind)
                  {
                    case KV_SEQUENCE:
                      col_count++;
                      if (col_count >= parameter->columns)
                        {
                          stmt[len++] = ')';
                          stmt[len++] = '\n';
                          indentf (vindent-1);
                          stmt[len++] = '(';
                          ncol = vindent;
                          col_count = 0;
                        }
                      else
                        {
                          stmt[len++] = ',';
                          stmt[len++] = ' ';
                          ncol += 2;
                        }
                      break;

                    default:
                      stmt[len++] = ',';
                      stmt[len++] = ' ';
                      ncol += 2;
                      break;
                  }
               }   
            }

          /* We've processed all the values for this assignment.  Put
             out any necessary end-of-assignment formatting */

          switch (parameter->value_kind)
            {
              case KV_SEQUENCE:
	       for (i=0; (i < ip); i++)
                  {
                    stmt[len++] = ')';
                  }
               break;

              case KV_SET:
               stmt[len++] = '}';
               break;

              default:
               break;
            }

          stmt[len++] = '\n';
          stmt[len] = '\0';
          ODLWriteStmt (output_file, stmt);
          
          /* Get the next parameter for this object, if any */

          parameter = NextParameter (parameter);
        }

      /* Get the next object or group node, if any */

      old_node = node;
      node = NextSubAggregate (base_node, node);
    }

  /* Print out any END_OBJECTs or END_GROUPs that we have pending */

  while (old_node != base_node)
    {
      len = 0;
      nindent -= INDENTSIZE;
      indentf (nindent);
      sprintf (&stmt[len], "%-*s = %s\n\n",
               no,
               (old_node->kind == KA_OBJECT) ? "END_OBJECT" : "END_GROUP",
               old_node->name);
      ODLWriteStmt (output_file, stmt);
      old_node = ParentAggregate (old_node);
    }

  /* Print out the terminating END statement */

#ifdef PDS_TOOLBOX
   if (pds_finish_label)
      ODLWriteStmt (output_file, "END\n");
#else
   ODLWriteStmt (output_file, "END\n");
#endif

  return;
}
