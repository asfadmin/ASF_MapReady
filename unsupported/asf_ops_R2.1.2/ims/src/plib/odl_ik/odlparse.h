/*****************************************************************************

 Module:  odlparse.h

 Description: This C-language include file contains the definitions for the
              tokens returned to the Object Description Language (ODL)
              parser by the lexical analyzer.  This include file is
              produced directly by Yacc by specifying the -d option.
              Changes to these definitions should be made by modifying the
              Yacc input file for the ODL rather than by changing the code
              below.

 Author:  Randy Davis, University of Colorado LASP

 Creation Date: 15 March 1989
 Last Modified: 30 August 1990

 History:

 Creation - This file was included in the Version 1 release of the ODLC
 library.

 Version 2 - 30 August 1990 - R. Davis, University of Colorado LASP
 a) Upgraded to work with the ODL Version 2 parser.

*****************************************************************************/

typedef union         {
               struct Value_Data  item;
               int                flag;
	      } YYSTYPE;
extern YYSTYPE yylval;
# define _OBJECT 257
# define _END_OBJECT 258
# define _GROUP 259
# define _END_GROUP 260
# define _END 261
# define _sequence_opening 262
# define _sequence_closing 263
# define _set_opening 264
# define _set_closing 265
# define _units_opening 266
# define _units_closing 267
# define _list_separator 268
# define _point_operator 269
# define _assignment_operator 270
# define _multiply_operator 271
# define _divide_operator 272
# define _exponentiate_operator 273
# define _range_operator 274
# define _date 275
# define _date_time 276
# define _date_timeV0 277
# define _integer 278
# define _name 279
# define _real 280
# define _symbol 281
# define _text_string 282
# define _time 283
