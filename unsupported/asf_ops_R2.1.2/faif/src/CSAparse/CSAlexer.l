%{
#include "y.tab.h"
extern char *yylval;
int EQ = 0, AA = 0;
%}
%a 2500
%%

begin 
{

return (Begin);
}

end {

return (END);
}

array 
{

return (ARRAY);
}

END_OF_FILE 
{
return (0);
}

\;### ;

= 
{
EQ = 1;
return ('=');
}

: 
{
EQ = 1;
return (':');
}

[0-9]+[-][0-9]+[-][0-9]+[:][0-9]+[:][0-9]+[\.][0-9]+ 
{
yylval = yytext;
return(DATE);
}

[a-zA-Z0-9]+(_[a-zA-Z0-9]+)*/([ ]*[=:]) 
{
yylval = yytext;
return (KEYWORD);
}
[-a-zA-Z0-9\\\+:_\.]+ 
{
yylval = yytext;
return (VALUE);
}

\;[^\n^#]* 
{
yylval = yytext;
return(COMMENT);
}

[ \t]* ;

[\n]* ;


