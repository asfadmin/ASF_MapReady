
%{

#include <stdlib.h>

#include "lex_yacc.h"   /* Must precede y.tab.h include.  */
#include "y.tab.h"

%}

%option nomain
%option noyywrap

INT_STRING         -?([0-9]+)
DOUBLE_STRING      -?(([0-9]+)|([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?)
ID_STRING          [a-zA-Z]("["|"]"|[-a-zA-Z0-9'>=(),;/_ \t])*
FIELD_STRING       [^#\n]*

%x FIELD_STATE

%%

 /* Eat up shell style comments.  */
\#.*\n ;

 /* Eat up blank lines.  */
\n ;

 /* Name tokens name blocks or fields.  */
{ID_STRING}[ \t]*/[:{]         { int last_nonspace = yyleng - 1;
                                 while ( isspace(yytext[last_nonspace]) ) 
                                   last_nonspace--;
                                 strncpy(yylval.string_val, yytext,
                                         last_nonspace + 1);
                                 yylval.string_val[last_nonspace + 1] = '\0'; 
                                 return NAME; }

 /* Eat up spaces around field seperator , go to field state and return 
    seperator.  */
[ \t]*:[ \t]*                 { BEGIN FIELD_STATE; return ':'; }
 /* Eat up spaces around block opening character and return it.  */
[ \t]*\{[ \t]*                { return '{'; }
 /* Eat up spaces around block closing character and return it.  */
[ \t]*\}[ \t]*                { return '}'; }

 /* Return a numeric value.  FIXME: needs errno ERANGE check added.  */
<FIELD_STATE>{INT_STRING}/[#\n]   { BEGIN INITIAL;
                                    yylval.int_val 
			              = strtol(yytext, NULL, 10);
                                    return INT; }
<FIELD_STATE>{DOUBLE_STRING}/[#\n]   { BEGIN INITIAL;
                                       yylval.double_val 
				         = strtod(yytext, NULL); 
                                       return DOUBLE; }

 /* Return a pointer to a string.  FIXME: length check string.  */
<FIELD_STATE>{FIELD_STRING}/[#\n]   { BEGIN INITIAL;
                                      strcpy(yylval.string_val, yytext);
                                      return STRING; }

