/*****************************************************************************

  Description:  This file contains routines for converting ODL values from
                ASCII text representation into values that can be attached
                to an ODL tree.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date: 13 March 1991
  Last Modified: 18 May 1991

  History:

    Creation - Beginning with Version 2.1 of the ODLC library, the routines
    in this file supplant the routines previously found in the LEXACT.C
    module. Previously these routines were only for processing tokens from
    the lexical analyzer. They have now been generalized and can be called
    directly by a user's program to convert text representations of values
    into data values for an ODL tree. The mapping between new and old
    routines is as follows:
      - ODLConvertInteger combines the previous routines ODLIntegerDToken
        and ODLIntegerBToken
      - ODLConvertSymbol combines ODLSymbolToken and ODLNameToken
      - ODLConvertReal is from ODLRealToken
      - ODLConvertString implements part of ODLStringToken (the rest of that
        earlier routine became routine yyGetStringToken in the lexical
        analyzer)
      - ODLConvertDate, ODLConvertTime and ODLConvertDateTime are from
        ODLDateToken, ODLTimeToken and ODLDateTimeToken, respectively.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed include statements that were Unix specific and placed them
    in odldef.h. Removed prototypes of ODL functions.

*****************************************************************************/

#include <ctype.h>
#include <errno.h>
#include <math.h>

#include "odldef.h"
#include "odlinter.h"




/*****************************************************************************

  Routine: ODLConvertInteger
 
  Description: Convert ASCII representation of an integer value into an
               ODLC integer data value.  The input integer value can be
               represented in either decimal or based notation.
 
  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
          vlength - Number of characters in value string.
          
  Output: A value data structure containing the converted value is returned
          as the function value.
           
*****************************************************************************/


VALUE_DATA ODLConvertInteger (vtext,vlength)

     char     vtext[];
     int      vlength;

{
  VALUE_DATA  item;               /* Structure to hold integer value        */
  char        warning[120];       /* Warning message text                   */
  int         base;               /* Number base                            */
  char       *sp;                 /* Pointer to substring within input      */


  /* Set the value type and length.  The format indicator is set to show
     whether the input value was in decimal notation (format = 0) or based
     notation (format = base). The precision indicator has no meaning
     for integer numbers */

  item.type      = TV_INTEGER;
  item.length    = vlength;
  item.valid     = 1;
  item.precision = 0;

  /* Initialize the units to NONE */

  item.value.integer.units  = NULL;

  /* See if there is a pound sign, denoting a based number */

  sp = strchr (vtext,'#');
  if (sp == NULL)
    {
      /* No pound sign found, so this value is in decimal integer format */

      item.format = 0;
      item.value.integer.number = atol (vtext);
    }
  else
    {
      /* The number is in based format. Get the number base and make sure
         it is in the range 2 .. 16 */

      base  = atoi (vtext);
      if (base < 2 || base > 16)
        {
          ODLPrintError ("The number base must be in range 2..16");
          item.value.integer.number = 0;
          item.valid = 0;
        }
      else
        {
          /* See if the next character is a number sign */

          if (*++sp == '-')
            {
              /* There is a negative number sign: set the format word to
                 a negative value to indicate that the number is negative */

              item.format = -base;
            }
          else
            {
              /* There is a positive number sign or no number sign: set
                 the format word to a positive value to indicate this */

              item.format = base;
            }

          /* Convert the number according to the specified number base */

          item.value.integer.number = strtol (sp, &sp, base);

          /* Check to make sure that there weren't any illegal digits */

          if (*sp != '#')
            {
              ODLPrintError ("Based integer contains illegal digit(s)");
              item.value.integer.number = 0;
              item.valid = 0;
            }
        }
    }

  /* See if the value was too large to be represented.  Note: this check
     does not work for all implementations of C */

  if (errno == ERANGE)
    {
      sprintf (warning,
               "Magnitude of integer number is too large.\n  %s %s value.",
               "The number has been set to the maximum",
               (item.value.integer.number < 0 ? "negative" : "positive"));
      ODLPrintWarning (warning);
      errno = 0;
    }

  return (item);
}




/*****************************************************************************

  Routine: ODLConvertReal
 
  Description: Convert ASCII representation of a real value into an ODLC
               real data value.

  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
          vlength - Number of characters in value string.

  Output: A value data structure containing the converted value is
          returned as the function value.
 
*****************************************************************************/


VALUE_DATA  ODLConvertReal (vtext,vlength)

     char     vtext[];
     int      vlength;

{
  VALUE_DATA  item;                 /* Structure to hold real value         */
  char        warning[120];         /* Warning message text                 */
  char       *sp;                   /* Pointer to substring within input    */

  double  atof ();                  /* Convert string to double floating    */


  /* Set the value type and length and initialize the validity indicator */

  item.type      = TV_REAL;
  item.length    = vlength;
  item.valid     = 1;

  /* Determine whether or not the number has an exponent.  We record
     this in the format field so the number can be output the same
     way later */

  sp = strpbrk (vtext, "Ee");
  if (sp == NULL)
    {
      /* Set the format code to indicate the value does not include an
         exponent */

      item.format = 1;
    }
  else
    {
      /* Set the format code to indicate the value includes an exponent */

      item.format = 2;
    }

  /* Determine the number of decimal places by counting the digits between
     the decimal point and the exponent or end of number, whichever occurs
     first */

  sp = strchr (vtext, '.');
  item.precision = strcspn (++sp, "Ee");

  /* Convert the input string and store the value.  Initialize the
     units to NONE */

  item.value.real.number = atof (vtext);
  item.value.real.units  = NULL;
  
  /* Determine whether overflow occurred.  Note: this check does not work
     for all implementations of C */

  if (errno == ERANGE)
    {
      sprintf (warning,
               "Magnitude of real number is too large\n  %s %s value.",
               "The number has been set to the maximum",
               (item.value.real.number < 0.0 ? "negative" : "positive"));
      ODLPrintWarning (warning);
      errno = 0;
    }

  return (item);
}




/*****************************************************************************

  Routine: ODLConvertSymbol
 
  Description: Convert a symbol value string into an ODLC data value.

  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
          vlength - Number of characters in value string.
          vflag   - Flag indicating whether value is a name:
                    1 - Symbol is a valid name and can be represented
                        without surrounding apostrophes;
                    2 - Symbol must be delimited by apostrophes.
                    Any other value for vflag will result in the symbol
                    being delimited by apostrophes.

  Output: A value data structure containing the converted value is
          returned as the function value.
 
*****************************************************************************/


VALUE_DATA  ODLConvertSymbol (vtext,vlength,vflag)

     char  vtext[];
     int   vlength;
     int   vflag;

{
  VALUE_DATA  item;                /* Structure to hold symbol value        */
  char       *ntext;               /* Pointer to storage for symbol value   */
  char        c;                   /* Current character                     */
  int         i;                   /* Index into text strings               */


  /* Set the type and length of the data value and the format value to
     indicate whether or not the symbol must be printed within
     apostrophes.  The precision field is not used for symbols */

  item.type      = TV_SYMBOL;
  item.length    = vlength;
  item.format    = (vflag == 1)? 1 : 0;
  item.precision = 0;

  /* Allocate memory to hold the name */

  ntext = (char *) malloc (vlength+1);
  if (ntext == NULL)
    {
      /* Couldn't allocate memory to hold the symbol */

      ODLPrintWarning ("Storage allocation failed -- couldn't store symbol");
      item.valid = 0;
    }
  else
    {
      /* Copy the symbol, converting alphabetic characters to upper case */

     for (i = 0; i < vlength; i++)
       {
         c = vtext[i];
         ntext[i] = (islower (c))? toupper (c) : c;
       }

     ntext[vlength] = '\0';
     item.valid = 1;
    }

  item.value.string = ntext;

  return (item);
}




/*****************************************************************************

  Routine: ODLConvertString
 
  Description: Convert a text string value into an ODLC data value.

  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
                    This does not include the opening quotation mark.
          vlength - Number of characters in value string, not including
                    the ending quotation mark.

  Output: A value data structure containing the converted value is
          returned as the function value.
 
*****************************************************************************/


VALUE_DATA  ODLConvertString (vtext,vlength)

     char    vtext[];
     int     vlength;

{
  VALUE_DATA  item;                /* Structure to hold string value        */
  char       *ntext;               /* Pointer to storage for string value   */


  /* Fill in the value data structure. The format and precision indicators
     aren't used for text strings */

  item.type      = TV_STRING;
  item.length    = vlength;
  item.format    = 0;
  item.precision = 0;

  /* Allocate memory to hold the output string value */

  ntext = (char *) malloc (vlength+1);
  if (ntext != NULL)
    {
      /* Copy the string */

      strncpy (ntext, vtext, vlength);
      ntext[vlength] = '\0';
      item.valid = 1;
    }
  else
    {
      /* Memory couldn't be allocated to store the string */

      ODLPrintWarning ("Storage allocation failed -- couldn't store string");
      item.valid = 0;
    }

  item.value.string = ntext;

  return (item);
}




/*****************************************************************************

  Routine: ODLConvertDate
 
  Description: Convert a date value in ASCII into an ODLC data value.

  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
          vlength - Number of characters in value string.

  Output: A value data structure containing the converted value is
          returned as the function value.
 
*****************************************************************************/


VALUE_DATA  ODLConvertDate (vtext,vlength)

     char     vtext[];
     int      vlength;

{
  VALUE_DATA  item;               /* Structure to hold string value        */

  /* Set the value type and length */

  item.type   = TV_DATE;
  item.length = vlength;

  /* Initialize the format, validity and precision indicators. They may be
     reset by the extraction routine */

  item.valid     = 1;
  item.format    = 0;
  item.precision = 0;

  /* Since this is a date only, initialize the time components of the
     value data structure to zero */

  item.value.date_time.hours        = 0;
  item.value.date_time.minutes      = 0;
  item.value.date_time.seconds      = 0;
  item.value.date_time.nanoseconds  = 0;

  item.value.date_time.zone_hours   = 0;
  item.value.date_time.zone_minutes = 0;

  /* Extract the date from the value string.  If there is an error, a
     message will be printed out by the extraction routine */

  ODLExtractDate (vtext, &item);

  return (item);
}




/*****************************************************************************

  Routine: ODLConvertTime
 
  Description: Convert a time value in ASCII into an ODLC data value.

  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
          vlength - Number of characters in value string.

  Output: A value data structure containing the converted value is
          returned as the function value.
 
*****************************************************************************/


VALUE_DATA  ODLConvertTime (vtext,vlength)

     char     vtext[];
     int      vlength;

{
  VALUE_DATA  item;                /* Structure to hold string value        */

  /* Set the value type and length */

  item.type   = TV_TIME;
  item.length = vlength;

  /* Initialize the format, validity and precision indicators. They may be
     reset by the extraction routine */

  item.valid     = 1;
  item.format    = 0;
  item.precision = 0;

  /* Since this is a time only, initialize the date components of the
     value data structure to zero */

  item.value.date_time.year  = 0;
  item.value.date_time.doy   = 0;
  item.value.date_time.month = 0;
  item.value.date_time.day   = 0;

  /* Extract the time from the value string.  If there is an error, a
     message will be printed out by the extraction routine */

  ODLExtractTime (vtext, &item);

  return (item);
}




/*****************************************************************************

  Routine: ODLConvertDateTime
 
  Description: Convert a date/time value text string into an ODLC data value.

  Input:  
          vtext   - Pointer to string containing the value as ASCII text.
          vlength - Number of characters in value string.

  Output: A value data structure containing the converted value is
          returned as the function value.
 
*****************************************************************************/


VALUE_DATA  ODLConvertDateTime (vtext,vlength)

     char     vtext[];
     int      vlength;

{
  VALUE_DATA  item;                /* Structure to hold date/time value     */
  char       *sp;                  /* Pointer to time part of input string  */

  /* Set the value type and length */

  item.type   = TV_DATE_TIME;
  item.length = vlength;

  /* Initialize the format, validity and precision indicators. They may be
     reset by the extraction routines */

  item.valid     = 1;
  item.format    = 0;
  item.precision = 0;

  /* Locate the separator between the date part and the time part, and
     replace it with a null character to terminate the date string. First
     we assume that the date/time is in the newer ODL format where the
     separator between date and time is the letter 'T' */

  sp = strchr (vtext, 'T'); 
  if (sp == NULL)
    {
      /* The date/time must be in the old ODL Version 0 format, where the
         date is separated from the time part by a dash character */

      sp = strchr (vtext, '-');
    }

  *sp++ = '\0';

  /* Extract the date from the value string.  If there is an error, a
     message will be printed out by the extraction routine */

  ODLExtractDate (vtext, &item);

  /* Now extract the time from the value string.  If there is an error,
     a message will be printed out by the extraction routine */

  ODLExtractTime (sp, &item);

  return (item);
}



/*****************************************************************************

  Routine: ODLExtractDate
 
  Description:  Extracts the date from a date/time string.

 
  Input:  text - Character string containing date in ASCII text format.
          item - Pointer to structure to contain date/time value.
           
  Output: The date fields are set in the date/time value data structure
          along with the validity and format indicators.
 
*****************************************************************************/


void  ODLExtractDate (text,item)

     char         *text;
     VALUE_DATA   *item;

{
/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/26/91  Removed declarations of unused variables     >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

  char   error_msg[80];    /* Error message text                            */
  long   day;              /* Day of month                                  */
  long   doy;              /* Day of year                                   */
  long   month;            /* Month number                                  */
  long   year;             /* Year number                                   */
  int    leap_year;        /* Set to one if leap year; otherwise zero       */
  int    nparts;           /* Number of parts to the date (2 or 3)          */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */

  static short day_table[2][13] =      /* Number of days in each month      */
           {{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
            {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}};
                                                            
  static short doy_table[13] =         /* Day of year for end of each month */
           {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};


  /* Extract the components of the date from the input string */

  nparts = sscanf (text, "%ld%*[-/]%ld%*[-/]%ld", &year, &month, &day);

  /* Check the year number for reasonableness */

  if (year < 100)
    {
      year = year + 1900;
    }
  else if (year < 1700 || year > 2200)
    {
      ODLPrintWarning ("Year is outside expected range - please check");
    }

  /* Determine whether or not the specified year is a leap year */

  leap_year = year%4 == 0 && year%100 != 0 || year%400 == 0;

  /* If we only got two input fields, they are the year and day of year; if
     we got three fields then we have year, month and day of month */

  if (nparts == 2)
    {
      /* The date was specified as year-doy: Check the doy value and
         if it is OK, convert it to month and day of month */

      doy = month;
      if (doy < 1 || (!leap_year && doy > 365) || (leap_year && doy > 366))
        {
          ODLPrintError
             ("Day-of-year must be in range 1..365 (or 366 in a leap year)");
          item->valid = 0;
        }
      else
        {
          day = doy;
          for (month=1 ; day > day_table[leap_year][month] ; month++)
            {
              day -= day_table[leap_year][month];
            }
        }
    }
  else
    {
      /* The date was specified as year, month and day.  Check the
         month and day values for validity */

      if (month >= 1 && month <= 12)
        {
          /* The month is valid. Make sure the day is valid for the
             specified month */

          if (day >= 1 && day <= day_table[leap_year][month])
            {
              /* The day is valid. Calculate the corresponding day
                 of year */

              doy = doy_table[month-1] + day;
              if (leap_year && month > 2)
                {
                  doy++;
                }
            }
          else
            {
              /* The day value is not valid */

              sprintf (error_msg,
                       "Day number should be in range 1..%d",
                       day_table[leap_year][month]);
              ODLPrintError (error_msg);
              item->valid = 0;
            } 
        }
      else
        {
          /* The month value is not valid */

          ODLPrintError ("Month number must be in range 1..12");
          item->valid = 0;
        }
    }

  item->format += nparts*10;
  if (item->valid)
    {
      /* Store the completed date */

      item->value.date_time.year  = (short) year;
      item->value.date_time.doy   = (short) doy;
      item->value.date_time.month = (char) month;
      item->value.date_time.day   = (char) day;
    }
  else
    {
      /* Since the date is in error, set all of the fields to zero */

      item->value.date_time.year  = 0;
      item->value.date_time.doy   = 0;
      item->value.date_time.month = 0;
      item->value.date_time.day   = 0;
    }

  return;
}




/*****************************************************************************

  Routine: ODLExtractTime
 
  Description:  Extracts the time from a date/time text string.

 
  Input:  text - String containing the time value as ASCII text.
          item - Pointer to structure that is to contain date/time value.
           
  Output: The time fields are placed into the date/time value data
          structure and the validity, format and precision fields are
          updated as appropriate.
 
*****************************************************************************/


void  ODLExtractTime (text,item)

     char       *text;
     VALUE_DATA *item;

{
/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/26/91  Removed declarations of unused variables     >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

  int     nparts;          /* Number of fields in time or time zone         */
  long    hours;           /* Hours, from input                             */
  long    minutes;         /* Minutes, from input                           */
  double  seconds;         /* Seconds, from input                           */
  char   *sp;              /* Pointer to substrings in input string         */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */

  /* Extract the components of the time from the input string.  We read
     them into temporary long integer variables so that we can reduce
     the potential for overflow problems if the user enters bad values. */

  nparts = sscanf (text, "%ld:%ld:%lf", &hours, &minutes, &seconds);
  
  /* Check the hours field to make sure it is valid */

  if (hours >= 24)
    {
      ODLPrintError ("Hours must be in range 0..23");
      item->valid = 0;
    }

  /* Check the minutes field */

  if (minutes > 60)
    {
      ODLPrintError ("Minutes must be in range 0..59");
      item->valid = 0;
    }

  if (nparts == 2)
    {
      /* No seconds parts present in the input time string.  Set the
         format code to indicate this and set the seconds field to zero */

      item->format += 2;
      seconds = 0.0;
    }
  else
    {

      /* Set the format to indicate that the time field has three parts */

      item->format += 3;

      /* Check the seconds value to make sure it is valid */

      if (seconds >= 60.0)
        {
          ODLPrintError ("Seconds must be less than 60");
          item->valid = 0;
        }
      else
        {
          /* Determine the number of decimal places specified in the seconds
             field.  This will be the number of characters between a decimal
             point, if any, and either the time zone indicator or the end of
             string, whichever occurs first */
    
          sp = strchr (text, '.');
          if (sp != NULL)
            {
              item->precision = strcspn (++sp, "Z+-");
            }
        }
    }

  if (item->valid)
    {
      /* Store the completed time value */
    
      item->value.date_time.hours       = (char) hours;
      item->value.date_time.minutes     = (char) minutes;
      item->value.date_time.seconds     = (char) seconds;
      item->value.date_time.nanoseconds =        
                              (long) (modf(seconds, &seconds)*1.0E9);
    }
  else
    {
      /* The time is invalid, so set all the fields to zero */
    
      item->value.date_time.hours       = 0;
      item->value.date_time.minutes     = 0;
      item->value.date_time.seconds     = 0;
      item->value.date_time.nanoseconds = 0;
    }

  /* Find the time zone indication in the input text string */

  sp = strpbrk (text, "+-");
  if (sp == NULL)
    {
      /* Either a time zone was not specified or the letter Z was specified
         to indicate UTC.  Either way, set the time zone offsets to zero */

      hours = 0;
      minutes = 0;
    }
  else
    {
      /* A delta from Greenwich was specified.  Get the zone offset in
         hours and (optionally) minutes from UTC */

      nparts = sscanf (sp, "%ld:%ld", &hours, &minutes);

      if (hours < -12 || hours > 12)
        {
          ODLPrintError ("Time zone hours value must be in range -12..+12");
          item->valid = 0;
        }

      if (nparts == 1)
        {
          /* There were no minutes specified in the time zone offset */

          minutes = 0;
        }
      else
        {
          /* The minutes part of the zone offset was specified: check it */
 
          if (minutes >= 60)
            {
              ODLPrintError
                  ("Time zone minutes value must be in range 0..59");
              item->valid = 0;
            }
        }

    }

  if (item->valid)
    {
      /* Store the time zone information */

      item->value.date_time.zone_hours   = (short) hours;
      item->value.date_time.zone_minutes = (char) minutes;
    }
  else
    {
      item->value.date_time.zone_hours   = 0;
      item->value.date_time.zone_minutes = 0;
    }

  return;
}
