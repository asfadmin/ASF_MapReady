%{

/*****************************************************************************

 Description: This file contains the parser for the Object Description
              Langauge (ODL).  The parser is produced using Yacc and
              all changes to the parsing scheme should be made by
              modifying the Yacc input file rather than the
              C-language code produced from by Yacc.

 Author:  Randy Davis, University of Colorado LASP

 Creation Date: 17 April 1990
 Last Modified: 18 May 1991

 History:

   Creation - This module was introduced in the Version 1 ODLC library.

   Version 2.0 - 30 August 1990 - R. Davis, U. of Colorado LASP
     a) Modified to comply with ODL Version 2.  This includes adding
        support for GROUP statements.

   Version 2.0.1 - 26 November 1990 - R. Davis, U. of Colorado LASP
     a) Changed parsing scheme to provide better error reporting and
        recovery.

   Version 2.1 - 13 March 1991
     a) Modified calls to parser action routines to pass pointers to
        value structures rather than copying in the entire structure.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Added include file odlinter.h.

*****************************************************************************/

#include "odldef.h"
#include "odlinter.h"

#ifdef PDS_TOOLBOX
    extern int pds_watch_ends;
#else
    int pds_watch_ends = TRUE;
#endif

%}


%start        label

%union        {
               struct Value_Data  item;
               int                flag;
	      }

%token        _OBJECT
%token        _END_OBJECT
%token        _GROUP
%token        _END_GROUP
%token        _END

%token        _sequence_opening
%token        _sequence_closing
%token        _set_opening
%token        _set_closing
%token        _units_opening
%token        _units_closing
%token        _list_separator
%token        _point_operator
%token        _assignment_operator
%token        _multiply_operator
%token        _divide_operator
%token        _exponentiate_operator
%token        _range_operator

%token <item> _date
%token <item> _date_time
%token <item> _date_timeV0
%token <item> _integer
%token <item> _name
%token <item> _real
%token <item> _symbol
%token <item> _text_string
%token <item> _time

%type  <item> integer_value
%type  <item> group_closing
%type  <item> object_closing
%type  <item> units
%type  <item> units_exponent
%type  <flag> units_mult_op

%%


label                  : statement_list
                          {
                            /* End-of-file hit before END statement found */
                            ODLEndLabel ();
                            if (pds_watch_ends)
                            {
                               yyerror ("END statement is missing");
                               YYABORT;
			    }
                            else
                               YYACCEPT;
                          }

 statement_list        : statement
                       | statement_list  statement

  statement            : aggregation_stmt
                       | assignment_stmt
                       | end_statement
                       | error statement

   aggregation_stmt    : object_stmt
                       | group_stmt
                       ;

   assignment_stmt     : attribute_stmt
                       | pointer_stmt
                       ;

   end_statement       : _END
                          {
                            /* This is the normal termination of parsing */
                            if (ODLEndLabel ())
                              {
                                YYACCEPT;
                              }
                            else
                              {
                                YYABORT;
                              }
                          }                  


    object_stmt        : object_opening
                       | object_closing
                       ;

    object_opening     : _OBJECT
                           { yyerror ("Missing '=' operator after OBJECT"); }
                       | _OBJECT  _assignment_operator  _name
                           { ODLBeginAggregate (KA_OBJECT, &$3); }

    object_closing     : _END_OBJECT
                           { 
                             $$.value.string = NULL;
                             ODLEndAggregate (KA_OBJECT, &$$);
                           }
                       | _END_OBJECT  _assignment_operator  _name
                           { ODLEndAggregate (KA_OBJECT, &$3); }


   group_stmt          : group_opening
                       | group_closing
                       ;

    group_opening      : _GROUP
                           { yyerror ("Missing '=' operator after GROUP"); }
                       | _GROUP  _assignment_operator  _name
                           { ODLBeginAggregate (KA_GROUP, &$3); }

    group_closing      : _END_GROUP
                           { 
                             $$.value.string = NULL;
                             ODLEndAggregate (KA_GROUP, &$$);
                           }
                       | _END_GROUP  _assignment_operator  _name
                           { ODLEndAggregate (KA_GROUP, &$3); }
               

  attribute_stmt       : _name  _assignment_operator
                           { ODLBeginParameter (KP_ATTRIBUTE, &$1); }
                         value
                       | _name  _assignment_operator  error
                           { 
                             yyerror ("Bad value in assignment statement");
                             yyclearin;
                           }
                       | _name error
                           { yyerror ("Expected '=' after name"); }


  pointer_stmt         : _point_operator  _name  _assignment_operator
                           { ODLBeginParameter (KP_POINTER, &$2); }
                         value
                       ;

   value               : scalar_value
                            { ODLMarkParameter (KV_SCALAR); }
                       | sequence_value
                            { ODLMarkParameter (KV_SEQUENCE); }
                       | set_value
                            { ODLMarkParameter (KV_SET); }
                       | range_value
                            { ODLMarkParameter (KV_SEQUENCE); }
                       ;

    scalar_value       : integer_value
                       | real_value
                       | date_time_value
                       | symbolic_value
                       | text_string_value
                       ;
                       
     integer_value     : _integer
                            { ODLStoreValue (&$1); }
                         units_part
                       ;

     real_value        : _real
                            { ODLStoreValue (&$1); }
                         units_part
                       ;

      units_part       :       
                       | units_expression
                       ;

      units_expression : _units_opening units_expr _units_closing
                       ;

       units_expr      : units_factor
                       | units_expr  units_mult_op  units_factor
                           { ODLMarkUnits ($2); }

        units_factor   : units
                           { ODLStoreUnits1 (&$1); }
                       | units  units_exp_op  units_exponent
                           { ODLStoreUnits2 (&$1, &$3); }

         units         : _name
                       | error  units_expression
                           { yyerror ("Units designator must be a name"); }

         units_mult_op : _multiply_operator
                           { $$ = 1; }
                       | _divide_operator
                           { $$ = -1; }
                       | error  units_expression
                           { yyerror ("Expected a '*', '/' or '**' operator");}
  
         units_exp_op  : _exponentiate_operator
                       ;

         units_exponent: _integer
                       | error  units_expression
                         { yyerror("Exponent in units expr must be integer");}

     date_time_value   : _date
                            { ODLStoreValue (&$1); } 
                       | _time
                            { ODLStoreValue (&$1); } 
                       | _date_time
                            { ODLStoreValue (&$1); } 
                       | _date_timeV0 time_zoneV0
                            { ODLStoreValue (&$1); }

      time_zoneV0      :
                       | _units_opening _name _units_closing
                       ;

     symbolic_value    : _name
                            { ODLStoreValue (&$1); }
                       | _symbol
                            { ODLStoreValue (&$1); }

     text_string_value : _text_string
                            { ODLStoreValue (&$1); }
 
    sequence_value     : sequence_1D
                       | sequence_2D
                       ;

     sequence_1D       : _sequence_opening              _sequence_closing
                           { yyerror("Sequences with no values not allowed"); }
                       | _sequence_opening  value_list  _sequence_closing
                           { ODLCheckSequence (); }
                       | _sequence_opening  value_list  error
                           { 
                             yyerror("')' at end of a sequence is missing");
                             ODLCheckSequence ();
                           }

      value_list       : scalar_value
                       | value_list  _list_separator  scalar_value
                       | error
                          { yyerror ("Error in value list"); }

     sequence_2D       : _sequence_opening  sequence_1D_list  _sequence_closing
                       ;

      sequence_1D_list : sequence_1D
                       | sequence_1D_list  sequence_1D
                       ;

   set_value           : _set_opening              _set_closing
                       | _set_opening  value_list  _set_closing
                       | _set_opening  value_list  error
                           { yyerror ("The '}' is missing at end of set"); }


   range_value         : integer_value _range_operator integer_value
                            { ODLCheckRange (&$1, &$3); }
                       ;

%%


/* Error handling routine.  This routine is called, explicitly or
   implicitly, whenever a syntax error is detected.                         */

yyerror (error_msg)
  char *error_msg;                 /* Error message text                    */

{
  ODLPrintError (error_msg);

  return;
}
