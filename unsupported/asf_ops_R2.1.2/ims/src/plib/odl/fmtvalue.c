/*****************************************************************************

  Description:  This file contains routines that are used to format
                ODL values for output.  Each routine converts a
                specific type of data value from its internal ODLC
                format to an ASCII text string.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date: 22 March 1989
  Last Modified: 18 May 1991

  History:

    Creation - This module was introduced in the Version 1 release of the
    ODLC library.

    Version 2.0 - 03 November 1990 - R. Davis, U. of Colorado LASP
      a) Modified to be compatible with Version 2 data structures.
      b) Direct writing of output is eliminated: formatted values now
         returned in text strings.
      c) Changed names of routines from ODLPutx (for example, ODLPutString)
         to ODLFormatx (as in ODLFormatString) to reflect the fact that
         these routines no longer directly write their output.
      d) Cleaned up the code for routine ODLFormatString.

    Version 2.1 - 13 March 1991 - R. Davis, U. of Colorado LASP
      a) Modified the calling sequence to ODLFormatString so that the
         pointer to the current column is passed as argument column,
         and an updated value is returned.  This makes the routine
         format output correctly for arrays of strings and other
         situations that previously caused problems with excessively
         long lines of output.
      a) Added routine ODLFormatComment to format comments in labels.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h. Added include file odlinter.h.

  Version 2.3 - 13 October 1991 - M. DeMore, Jet Propulsion Laboratory.
    Added is_pointer input to ODLFormatString in order to cope with
    backslashes in DOS file names.
  
*****************************************************************************/
    

#include <ctype.h>
#include <math.h>

#include "odldef.h"
#include "odlinter.h"





/*****************************************************************************

  Routine: ODLFormatInteger
 
  Description:  Formats an integer number for output, in decimal or based
                format, and with or without units.
 
  Input:
           stmt  - Character string to receive formatted value.
           value - Pointer to a value data structure that contains the
                   value and associated formatting information.

  Output:  The value is formatted and returned in the string pointed
           to by the first parameter.  The count of characters
           placed into the output string is returned as the
           function value.
           
*****************************************************************************/


int ODLFormatInteger (stmt,item)

     char        stmt[];
     VALUE_DATA *item;

{
  char             digits[64];  /* Array to hold  string of digits          */
  int              base;        /* Number base                              */
  int              dnum;        /* Current digit of number                  */
  int              i;           /* Index into array                         */
  int              len;         /* Number of characters in output string    */
  unsigned long    num;         /* Used to hold number during conversion    */


  /* If the format is  0 then we are to print in standard signed decimal
     format. If format > 0, the value will be printed as an unsigned number
     using based notation.  A format value < 0 indicates an error and the
     value cannot be printed. */

  if (item->valid == 0)
    {
      sprintf (stmt, "**Error**");
      len = 9;
    }
  else
    {
      num = item->value.integer.number;
      if (item->format == 0)
        {
          sprintf (stmt, "%ld", num);
        }
      else
        {
          /* The output is to be in based notation. Convert the output a digit
             at a time, according to the specified base. We always put out at
             least one digit.  */

          i = 64;
          digits[--i] = '\0';

          if (item->format > 0)
            {
              /* The number is positive */

              base = item->format;
            }
          else
            {
              /* The number is negative: negate the value to be output */

              base = -item->format;
              num  = -num;
            } 

          do
            {
              dnum = num%base;
              digits[--i] = (dnum < 10) ? dnum + '0' : (dnum-10) + 'A';
              num = num/base; 
            } while (num > 0);

          if (item->format < 0)
            {
              digits[--i] = '-';
            }

          sprintf (stmt, "%d#%s#",
                   base,
                   &digits[i]);
        }

      /* Determine the length of the number string */

      len = strlen (stmt);

      /* Put in the units associated with the number, if any */

      len += ODLFormatUnits (&stmt[len], item->value.integer.units);
    }

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatReal
 
  Description:  Formats a real number for output, with or without units.
 
  Input:
           output_file - A pointer to the file to which the output is to go.
           value       - Pointer to a value data structure that contains the
                         value and associated formatting information.

  Output:  The value is formatted and returned in the string pointed
           to by the first parameter.  The count of characters
           placed into the output string is returned as the
           function value.
           
*****************************************************************************/


int ODLFormatReal (stmt,item)

     char        stmt[];
     VALUE_DATA *item;
{
  int              len;         /* Number of characters in output string    */


  /* If the format value is < 0 then the value is in error and can't
      be output.  Otherwise format the value for output */

  if (item->valid == 0)
    {
      sprintf (stmt, "**Error**");
      len = 9;
    }
  else
    {
      if (item->format == 0)
        {
          /* Format the number using the general format */

          sprintf (stmt, "%g",
                   item->value.real.number);
        }
      else if (item->format == 1)
        {
          /* Format the number using the decimal format.  We
             always put in at least one decimal place */

          sprintf (stmt, "%.*f",
                   (item->precision <= 0)? 1 : item->precision,
                   item->value.real.number);
        }
      else
        {
          /* Format the number using the exponential format.  We
             always put in at least one decimal place */

          sprintf (stmt, "%.*e",
                   (item->precision <= 0)? 1 : item->precision,
                    item->value.real.number);
        }

      /* Determine the length of the number string */

      len = strlen (stmt);

      /* Put in the units associated with the number, if any */

      len += ODLFormatUnits (&stmt[len], item->value.real.units);
    }

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatUnits
 
  Description:  Formats the units of an integer or real number for output.
 
  Input:
           stmt  - Character string to receive formatted value.
           units - Pointer to the first units field.  If this value is
                   NULL then no units are present and the routine
                   does nothing.

  Output:  The units are formatted and returned in the string pointed
           to by the first parameter.  The count of characters
           placed into the output string is returned as the
           function value.
           
*****************************************************************************/


int ODLFormatUnits (stmt,units)

     char               stmt[];
     struct ODLUnits   *units;

{
  struct ODLUnits *current_units;/* Pointer to current units field          */
  int              first_field; /* =1 if first units field; 0 otherwise     */
  int              uexp;        /* Units exponent value                     */
  int              len;         /* Number of characters in output string    */


  len = 0;

  if (units != NULL)
    {
     current_units = units;

     /* Start with the opening units expression delimiter */

     stmt[len++] = '<';

     /* Add in each field of the units expression */

     first_field = 1;

     while (current_units != NULL)
       {
         /* If this is not the first field put in a '*' if the field is
            being multiplied by the previous units or '/' if the field
            is being divided into the previous units */

         uexp = current_units->exponent;

         if (!first_field)
           {
             if (uexp > 0)
               {
                 stmt[len++] = '*';
               }      

             else
               {
                 stmt[len++] = '/';
                 uexp = -uexp;
               }
           }
         else
           {
             first_field = 0;
           }

         /* Put in the units */

         sprintf (&stmt[len], "%s",
                  current_units->designator);
         len += strlen (&stmt[len]);

         /* Put in the exponent, if appropriate */

         if (uexp != 1)
           {
             sprintf (&stmt[len], "**%d",
                      uexp);
             len += strlen (&stmt[len]);
           }

         /* Get the next units field, if any */

         current_units = current_units->next_field;
       }

     /* Put in the terminating delimiter for the units expression */

     stmt[len++] = '>';
     stmt[len] = '\0';
   }

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatSymbol
 
  Description:  Formats a symbol, with or without delimiters.
 
  Input:
           stmt   - Character string to receive the formatted output.
           value  - Pointer to a value data structure that contains the
                    value and associated formatting information.

  Output:  The symbol is placed into the statement string and the number
           of characters put in the string is returned as the function
           value.
           
*****************************************************************************/


int ODLFormatSymbol (stmt,item)

     char         stmt[];
     VALUE_DATA  *item;

{
  int              len;         /* Number of characters in output string    */


  /* Format the symbol.  If the format code is less than zero then the value
     has an error and can't be output.  If the format is greater than
     zero then the symbol is a name that doesn't require delimiters.
     Else, if format is zero, put the symbol between delimiters */

  if (item->valid == 0)
    {
      sprintf (stmt, "**Error**");
    }
  else if (item->format > 0)
    {
      sprintf (stmt, "%s",
               item->value.string);
    }
  else
    {
      sprintf (stmt, "\'%s\'",
               item->value.string);
    }

  /* Determine the length of the formatted string */

  len = strlen (stmt);

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatDate
 
  Description:  Formats a date for output, either as year-month-day or
                year-doy.
 
  Input:
          stmt   - Character string to receive formatted output.
          value  - Pointer to a value data structure that contains the
                   value and associated formatting information.

  Output:  The formatted value is placed into the statement string and the
           number of characters put in the string is returned as the
           function value.

*****************************************************************************/


int ODLFormatDate (stmt,item)

     char           stmt[];
     VALUE_DATA    *item;

{
/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/26/91  Removed declarations of unused variables     >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

  int     day;                  /* Day of month (1-31)                      */
  int     doy;                  /* Day of year (1-366)                      */
  int     month;                /* Month (1-12)                             */
  int     year;                 /* Year                                     */
  int     len;                  /* Number of characters in output string    */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */


  /* If the format flag is less than zero then there is an error in the
     value and we can't format it for output */

  if (item->valid == 0)
    {
      sprintf (stmt, "**Error**");
    }
  else
    {
      /* Get the components of the date */

      year  = (int) item->value.date_time.year;
      doy   = (int) item->value.date_time.doy;
      month = (int) item->value.date_time.month;
      day   = (int) item->value.date_time.day;

      if (item->format/10 == 3)
        {
          /* Format the date as year, month and day-of-month */

          sprintf (stmt, "%d-%02d-%02d",
                   year, month, day);
        }
      else
        {

          /* Format the date as year and day-of-year */

          sprintf (stmt, "%d-%03d",
                   year, doy);
        } 
    }

  /* Determine the length of the formatted string */

  len = strlen (stmt);

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatTime
 
  Description:  Formats a time and time zone indicator for output.
 
  Input:
           stmt   - Character string to receive formatted output.
           value  - Pointer to a value data structure that contains the
                    value and associated formatting information.

  Output:  The formatted value is placed into the statement string.  The
           function returns the number of characters put in the string.
           
*****************************************************************************/


int ODLFormatTime (stmt,item)

     char           stmt[];
     VALUE_DATA    *item;

{
  int     hours;                /* Hours of day (0-23) and time zone hours  */
  int     minutes;              /* Minutes of hour (0-59) and time zone mins*/
  double  seconds;              /* Seconds of minute (<60.0)                */
  int     len;                  /* Number of characters in output string    */


  if (item->valid == 0)
    {
      sprintf (stmt, "**Error**");
      len = 9;
    }
  else
    {
      /* Get the components of the time */

      hours   = (int) item->value.date_time.hours;
      minutes = (int) item->value.date_time.minutes;
      seconds = (double) item->value.date_time.seconds +
                (double) item->value.date_time.nanoseconds*1.0E-9;

      /* Put out the hours, minutes and seconds (always two
         digits apiece) */
     
      sprintf (stmt, "%02d:%02d",
               hours, minutes);
      len = 5;

      if (item->format % 10 == 3)
        {
          /* Put out seconds part of time as well */

	  stmt[len++] = ':';

          if (seconds < 10.0)
            {
              stmt[len++] = '0';
            }
          sprintf (&stmt[len], "%.*f",
                   item->precision,
                   seconds);
          len += strlen (&stmt[len]);
        }

      /* Put out the time zone.  If UTC, output a 'Z'; otherwise
         output the number of hours from Greenwich */

      hours   = (int) item->value.date_time.zone_hours;
      minutes = (int) item->value.date_time.zone_minutes;

      if (hours == 0)
        {
          stmt[len++] = 'Z';
          stmt[len] = '\0';
        }
      else
        {
          /* Put out hours part of time zone offset */

          sprintf (&stmt[len], "%+03d",
                   hours);
          len += 3;

          if (minutes != 0)
            {
              /* Put out minutes part of time zone offset also */

              sprintf (&stmt[len], ":%02d",
                       minutes);
              len += 3;
            }
        }
    }

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatDateTime
 
  Description:  Formats a date and time for output.
 
  Input:
           stmt    - Character string to received formatted result.
           value   - Pointer to a value data structure that contains the
                     value and associated formatting information.

  Output:  The formatted value is placed into the statement string.  The
           function returns the number of characters put in the string.
           
*****************************************************************************/


int ODLFormatDateTime (stmt,item)

     char            stmt[];
     VALUE_DATA     *item;

{
  int     len;                  /* Number of characters in output string    */


  /* If the format is less than zero then the value has an error and can't
     be printed.  Else print the date and time with a 'T' in between as
     a separator. */

  if (item->valid == 0)
    {
      sprintf (stmt, "**Error**");
      len = 9;
    }
  else
    {
      len = ODLFormatDate (stmt, item);
      stmt[len++] = 'T';
      len += ODLFormatTime (&stmt[len], item);
    }

  return (len);
}




/*****************************************************************************
                                                                           
  ODLFormatString:  Format an ODL string value for output.

  Input:
         stmt         - Character string to receive formatted output.
         value        - Pointer to a value data structure that contains
                        the value and associated formatting information.
         column       - Pointer to current column number.  Upon return,
                        the value will be updated to point to the next
                        column after the terminating  quotation mark.
         left_margin  - Starting column number for lines 2 and beyond,
                        counting from 1.
         right_margin - Last column for text on all lines.
         format_flag  - Set to TRUE if the '\t's and '\n's in the string
                        are not be be expanded on output (PDS label format).
         is_pointer   - Set to TRUE if the string being processed was part
                        of a pointer attribute, in which case '\t's and '\n's 
                        in the string aren't touched.

  Output:  The formatted string is placed into the statement string and the
           number of characters put in the string is returned as the
           function value.
                                                       
 ****************************************************************************/


int ODLFormatString (stmt,item,column,left_margin,right_margin,format_flag,
                     is_pointer)

     char        stmt[];
     VALUE_DATA *item;
     int        *column;
     int         left_margin;
     int         right_margin;
     int         format_flag;
     int         is_pointer;

{
  char   *text;                 /* Pointer to input string                  */
  char    c;                    /* Current input character                  */
  int     i;                    /* Loop index                               */
  int     l;                    /* Current column number                    */
  int     m;                    /* Index into input string                  */
  int     n;                    /* Length of input string                   */
  int     lmargin;              /* Adjusted left margin for line 2 & beyond */
  int     rmargin;              /* Adjusted right margin                    */
  int     endline;              /* TRUE if at end-of-line                   */
  int     len;                  /* Number of characters in output string    */

#define indentf(X) for (i=0 ; i < X ; i++) stmt[len++] = ' '

#define TAB_SIZE 8
#define MIN_LINE_LENGTH 40
#define MAX_LEFT_MARGIN 32
#define MAX_RIGHT_MARGIN 132


  /* Check the line margin values and adjust them if they are out of range */
     
  if (left_margin < 1)
    {
      lmargin = 1;
    }
  else if (left_margin > MAX_LEFT_MARGIN)
    {
      lmargin = MAX_LEFT_MARGIN;
    }
  else
    {
      lmargin = left_margin;
    }

  if (right_margin > MAX_RIGHT_MARGIN)
    {
      rmargin = MAX_RIGHT_MARGIN;
    }
  else if (right_margin < lmargin+MIN_LINE_LENGTH)
    {
      rmargin = lmargin + MIN_LINE_LENGTH;
    }
  else
    {
      rmargin = right_margin;
    }

  /* Initialize counters and flags */

  l = (*column > 0)? *column : 1;
  endline = 0;
  len = 0;

  if (l > rmargin)
    {
      /* We're beyond the right margin already, so start a new one before
         starting the string */
      
      stmt[len++] = '\n';
      indentf (lmargin);
      l = lmargin;
    }

  /* Put out the opening quotation mark string delimiter */

  stmt[len++] =  '\"';
  l++;

  /* Scan through all of the input string, formatting as we go */

  text = item->value.string;
  n = item->length;

  for ( m = 0 ; m < n ; )
    {
      c = text[m++];
      if (!is_pointer && c == '\\')
      {
         c = text[m++];
         switch (c)
         {
           case 'n':

            /* We encountered an explicit newline: go put out the line */

            if (format_flag)
              {
                stmt[len++] = '\\';
                stmt[len++] = 'n';
                l += 2;
              }

            endline = 1;
            break;

          case 't':

            /* Character is a horizontal tab */

            if (format_flag)
              {
                /* The output is being formatted in PDS label format, so
                   indicate the tab explicitly */

                stmt[len++] = '\\';
                stmt[len++] = 't';
                l += 2;
              }
            else
              {
                /* The output is being formatted as normal text so expand the
                   tab if there is room on the line; if the tab comes at the
                   end of the line it can be discarded */

                if (l+TAB_SIZE < rmargin)
                  {
                    indentf (TAB_SIZE);
                    l += TAB_SIZE;
                  }
                else
                  {
                    endline = 1;
                  }
              }
             break;

	    default:
               stmt[len++] = '\\';
               stmt[len++] = c;
               l+=2;
               break;
            }
          } 
          else
          {
            /* Add the character to the line.  */

            stmt[len++] = c;
            l++;

            if (l > rmargin)
              {
                /* The current output line is full.  See if the next
                   character is a blank or tab */

                if (m < n && isspace (text[m]))
                  {
                    /* It is: skip forward to the next non-blank character */

                    for ( ; m < n && isspace (text[m]) ; m++);
                  }
                else
                  {
                    /* It is not: move back to a previous space character
                       to perform word wrapping. */

                    while (l > lmargin && len > 1 && !isspace (stmt[len-1]))
                      {
                        len--;
                        l--;
                        m--;
                      }
                  }

                endline = 1;
              }
	  }

      /* If we're at the end of a line, print it out and start a new line
         at the left margin */
    
      if (endline)  
        {
          stmt[len++] = '\n';
          indentf (lmargin);
          l = lmargin;
          endline = 0;
        }            
    }

  /* Put out the terminating quotation mark string delimiter and return
     the location of the next available print column */

  stmt[len++] = '\"';
  stmt[len] = '\0';
  *column = l+2;

  return (len);
}




/*****************************************************************************

  Routine: ODLFormatComment
 
  Description:  Formats a comment for insertion into a label
 
  Input:
           stmt    - Character string to receive the formatted output.
           comment - Character string containing the comment.

  Output:  The comment is placed into the statement string and the number
           of characters put in the string is returned as the function
           value.
           
*****************************************************************************/


int ODLFormatComment (stmt,comment,left_margin,right_margin)

     char         stmt[];
     char         comment[];
     int          left_margin;
     int          right_margin;
     
{
  char    c;                    /* Current input character                  */
  int     i;                    /* Loop index                               */
  int     l;                    /* Index into output line                   */
  int     ic;                   /* Index into comment string                */
  int     len;                  /* Number of characters in output string    */
  int     lmargin;              /* Adjusted left margin                     */
  int     rmargin;              /* Adjusted right margin                    */

#define indentf(X) for (i=0 ; i < X ; i++) stmt[len++] = ' '

#define MIN_LINE_LENGTH 40
#define MAX_LEFT_MARGIN 32
#define MAX_RIGHT_MARGIN 132


  if (comment == NULL)
    {
      return (0);
    }

  /* Check the line margin values and adjust them if they are out of range */
     
  if (left_margin < 1)
    {
      lmargin = 1;
    }
  else if (left_margin > MAX_LEFT_MARGIN)
    {
      lmargin = MAX_LEFT_MARGIN;
    }
  else
    {
      lmargin = left_margin;
    }

  if (right_margin > MAX_RIGHT_MARGIN)
    {
      rmargin = MAX_RIGHT_MARGIN;
    }
  else if (right_margin < lmargin+MIN_LINE_LENGTH)
    {
      rmargin = lmargin + MIN_LINE_LENGTH;
    }
  else
    {
      rmargin = right_margin;
    }

  rmargin = rmargin - 3;

  /* Put out the slash and asterisk to start the first comment line */

  len = 0;
  stmt[len++] = '\n';
  indentf (lmargin-1);
  stmt[len++] = '/';
  stmt[len++] = '*';
  l = lmargin+2;

  for (ic = 0 ; (c = comment[ic]) != '\0' ; ic++)
    {
      if (c == '\n' || l > rmargin)
        {
          /* This is the end of a line: put out the asterisk and slash
             that terminate the current line */

          stmt[len++] = ' ';
          stmt[len++] = '*';
          stmt[len++] = '/';
          stmt[len++] = '\n';

          /* Put out the slash and asterisk that start a new comment line */

          indentf (lmargin-1);
          stmt[len++] = '/';
          stmt[len++] = '*';
          l = lmargin+2;
        }

      if (c != '\n')
        {
          /* Put the current character into the output line */

          stmt[len++] = c;
          l++;
        }
    }

  /* Terminate the last line of the comment */

  stmt[len++] = ' ';
  stmt[len++] = '*';
  stmt[len++] = '/';
  stmt[len++] = '\n';
  stmt[len++] = '\n';
  stmt[len] = '\0';

  return (len);
}
