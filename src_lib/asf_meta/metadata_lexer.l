
%{

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "lex_yacc.h"   /* Must precede y.tab.h include.  */
#include "y.tab.h"

int line_number = 1;		/* Position in source file.  */

/* Appropriately prefixed version of yywrap.  */
int meta_yywrap(void)
{
  return 1;
}

%}

%option nomain

DOUBLE_STRING      (-?([0-9]+)|(([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?)?)
ID_STRING          [a-zA-Z]("["|"]"|[-a-zA-Z0-9'>=(),;/_ \t])*[^ \t\n]*
FIELD_STRING       [^#\n]*[^#\n \t]

%x FIELD_STATE

%%

 /* Eat up shell style comments.  */
\#.*        { ; }

 /* Eat up trailing blanks and new lines.  */
[ \t]*\n    { line_number++; }

 /* Name tokens name blocks or fields.  */
{ID_STRING}[ \t]*/[:{]   { int last_nonspace = meta_yyleng - 1;

                           while ( isspace(meta_yytext[last_nonspace]) ) 
                             last_nonspace--;
                           strncpy(meta_yylval.string_val, meta_yytext, 
                                   last_nonspace + 1);
                           meta_yylval.string_val[last_nonspace + 1] = '\0'; 
                           return NAME; }

 /* Gobble up spaces that are not already part of field or block id */
 /* strings (recall rules are tried in order.  */
[ \t]*      { ; } 

 /* Eat up spaces after field seperator , go to field state and return 
    seperator.  */
:[ \t]*                  { BEGIN FIELD_STATE; return ':'; }
 /* Eat up spaces after block opening character and return it.  */
\{[ \t]*                 { return '{'; }
 /* Eat up spaces around block closing character and return it.  */
[ \t]*\}[ \t]*           { return '}'; }

 /* Return a numeric value.  FIXME: needs errno ERANGE check added.  */
<FIELD_STATE>{DOUBLE_STRING}/[ \t]*[#\n]   { BEGIN INITIAL;
                                             meta_yylval.double_val 
				               = strtod(meta_yytext, NULL); 
                                             return DOUBLE; }

 /* Return a pointer to a string.  FIXME: length check string.  */
<FIELD_STATE>{FIELD_STRING}/[ \t]*[#\n]   { BEGIN INITIAL;
                                            strcpy(meta_yylval.string_val, 
                                                   meta_yytext);
                                            return STRING; }

%%

