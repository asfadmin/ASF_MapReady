#include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	int yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng;
char yytext[YYLMAX];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
static char *sccs = "@(#)ims_askLexer.l	5.1  16 Mar 1996";

# line 4 "ims_askLexer.l"
/**************************************************************************
**
** File:      ims_askLexer.l
**  
** Creator:   H. Sayah
**
** Date:      August 5, 1991
**
** Function:  Lexical scanner to identify tokens for the ims_parseKwBuffer()
**            routine. 
**            This file is an input to the UNIX's lex program.
**
** Updated:   H. Sayah   March 1995
**            ASF adoption of AMMOS-CDB inherited code.
**
**************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <ims_const.h>
#include <ims_keyword.h>
#include <ims_dbms.h>
#include <ims_askLexer.h>
#include <ims_askParser.h>


# line 31 "ims_askLexer.l"
/*
** These variables are flags indicating where input is comming from
** It can either come from memory of from file(fp).  The program that
** will call the parser will have to set the flag before a call to the
** parser is made.  If lexinput is set to FILE_PTR then, input will be
** from fp else from memory.  The variables lexinput,lexinput_fp,and
** lexinput_memptr must be set appropriately by the calling program
*/
enum LEX_INPUT_SOURCE lexinput;
FILE		*lexinput_fp;
char		*lexinput_memptr;


#undef getc
#define	getc(x) ((lexinput == FILE_PTR) ? \
	(fgetc(lexinput_fp)):(*(lexinput_memptr++)))

#define	yywrap()	1


# line 50 "ims_askLexer.l"
/*
** Function to save a string 
*/
char	*saveString(string)
char	*string;	/*passed in string*/
{
	char	*new;
	if (!string)
	{
		return((char *)NULL);
	}
	if ( new = malloc((unsigned)strlen(string)+1))
	{
		(void) strcpy(new, string);
		return(new);
	}
	else
	{
		return((char *)NULL);
	}
}
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 80 "ims_askLexer.l"
	;
break;
case 2:

# line 82 "ims_askLexer.l"
			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 3:

# line 83 "ims_askLexer.l"
			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 4:

# line 84 "ims_askLexer.l"
			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 5:

# line 85 "ims_askLexer.l"
			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 6:

# line 86 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 7:

# line 87 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 8:

# line 88 "ims_askLexer.l"
	{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
break;
case 9:

# line 89 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(UN_OPERATOR); }
break;
case 10:

# line 90 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(UN_OPERATOR); }
break;
case 11:

# line 91 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(BL_OPERATOR); }
break;
case 12:

# line 92 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(BL_OPERATOR); }
break;
case 13:

# line 93 "ims_askLexer.l"
	{ yylval.string = saveString(yytext); return(WHERE); }
break;
case 14:

# line 94 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(OPEN_PARAN); }
break;
case 15:

# line 95 "ims_askLexer.l"
		{ yylval.string = saveString(yytext); return(CLOSE_PARAN); }
break;
case 16:

# line 97 "ims_askLexer.l"
	;
break;
case 17:

# line 99 "ims_askLexer.l"
	{
				yylval.string = saveString(yytext);
				return(T_STRING);
			}
break;
case 18:

# line 103 "ims_askLexer.l"
{
				yylval.string = saveString(yytext);
				return(T_QSTRING);
			}
break;
case 19:

# line 107 "ims_askLexer.l"
{
				yylval.string = saveString(yytext);
				return(T_QSTRING);
			}
break;
case 20:

# line 111 "ims_askLexer.l"
		{ return(yytext[0]); }
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

17,
20,
0,

16,
20,
0,

16,
0,

17,
20,
0,

20,
0,

20,
0,

14,
20,
0,

15,
20,
0,

17,
20,
0,

6,
17,
20,
0,

7,
17,
20,
0,

5,
17,
20,
0,

20,
0,

17,
20,
0,

17,
20,
0,

17,
20,
0,

17,
20,
0,

17,
20,
0,

17,
0,

4,
17,
0,

18,
0,

19,
0,

17,
0,

2,
17,
0,

3,
17,
0,

17,
0,

17,
0,

17,
0,

17,
0,

12,
17,
0,

17,
0,

17,
0,

11,
17,
0,

17,
0,

9,
17,
0,

10,
17,
0,

17,
0,

1,
17,
0,

8,
17,
0,

17,
0,

1,
0,

13,
17,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	36,36,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
3,21,	0,0,	0,0,	47,36,	
0,0,	0,0,	0,0,	43,36,	
3,0,	3,0,	0,0,	0,0,	
3,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	1,7,	
25,0,	0,0,	0,0,	2,6,	
1,8,	1,9,	1,10,	3,0,	
36,43,	3,0,	6,21,	2,10,	
1,11,	44,37,	3,0,	3,0,	
3,0,	2,11,	6,0,	6,0,	
47,43,	0,0,	6,0,	37,37,	
43,43,	1,12,	1,13,	1,14,	
37,44,	43,47,	2,12,	2,13,	
2,14,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	6,0,	0,0,	6,0,	
7,23,	0,0,	0,0,	0,0,	
6,0,	6,0,	6,0,	0,0,	
7,23,	7,23,	0,0,	0,0,	
1,15,	0,0,	1,15,	0,0,	
0,0,	2,15,	1,16,	2,15,	
0,0,	0,0,	3,0,	2,16,	
3,0,	0,0,	6,22,	0,0,	
0,0,	1,17,	1,18,	0,0,	
1,19,	7,24,	2,17,	2,18,	
0,0,	2,19,	7,23,	7,23,	
1,20,	8,25,	0,0,	0,0,	
0,0,	2,20,	0,0,	0,0,	
0,0,	8,25,	8,25,	11,21,	
0,0,	0,0,	0,0,	0,0,	
6,0,	0,0,	6,0,	11,0,	
11,0,	0,0,	0,0,	11,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	12,0,	
12,0,	0,0,	8,0,	12,0,	
0,0,	0,0,	27,27,	8,26,	
8,25,	0,0,	11,0,	0,0,	
11,0,	0,0,	27,36,	27,36,	
0,0,	11,0,	11,0,	11,0,	
11,27,	0,0,	12,0,	0,0,	
12,0,	0,0,	0,0,	0,0,	
0,0,	12,0,	12,0,	12,0,	
13,0,	13,0,	0,0,	0,0,	
13,0,	0,0,	0,0,	27,36,	
0,0,	0,0,	14,0,	14,0,	
27,36,	27,36,	14,0,	27,37,	
0,0,	0,0,	0,0,	12,28,	
0,0,	0,0,	0,0,	13,0,	
0,0,	13,0,	0,0,	0,0,	
0,0,	0,0,	13,0,	13,0,	
13,0,	14,0,	0,0,	14,0,	
16,21,	11,0,	0,0,	11,0,	
14,0,	14,0,	14,0,	0,0,	
16,0,	16,0,	0,0,	17,21,	
16,0,	12,0,	0,0,	12,0,	
0,0,	0,0,	0,0,	17,0,	
17,0,	0,0,	0,0,	17,0,	
0,0,	0,0,	14,29,	0,0,	
0,0,	0,0,	0,0,	16,0,	
0,0,	16,0,	0,0,	0,0,	
0,0,	0,0,	16,0,	16,0,	
16,0,	0,0,	17,0,	0,0,	
17,0,	18,21,	13,0,	0,0,	
13,0,	17,0,	17,0,	17,0,	
0,0,	18,0,	18,0,	19,21,	
14,0,	18,0,	14,0,	0,0,	
0,0,	0,0,	0,0,	19,0,	
19,0,	0,0,	0,0,	19,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
18,0,	0,0,	18,0,	0,0,	
0,0,	0,0,	0,0,	18,0,	
18,0,	18,0,	19,0,	0,0,	
19,0,	0,0,	16,0,	0,0,	
16,0,	19,0,	19,0,	19,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	17,0,	0,0,	17,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	16,30,	0,0,	0,0,	
0,0,	20,21,	0,0,	17,31,	
0,0,	0,0,	0,0,	0,0,	
0,0,	20,0,	20,0,	0,0,	
0,0,	20,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,0,	
0,0,	18,0,	0,0,	21,0,	
21,0,	18,32,	0,0,	21,0,	
20,0,	19,0,	20,0,	19,0,	
0,0,	18,33,	0,0,	20,0,	
20,0,	20,0,	22,0,	22,0,	
0,0,	0,0,	22,0,	0,0,	
0,0,	0,0,	21,0,	0,0,	
21,0,	0,0,	0,0,	0,0,	
19,34,	21,0,	21,0,	21,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	22,0,	0,0,	22,0,	
0,0,	0,0,	0,0,	0,0,	
22,0,	22,0,	22,0,	28,0,	
28,0,	0,0,	0,0,	28,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	29,0,	29,0,	0,0,	
0,0,	29,0,	0,0,	20,0,	
0,0,	20,0,	0,0,	0,0,	
0,0,	0,0,	28,0,	0,0,	
28,0,	0,0,	0,0,	0,0,	
20,35,	28,0,	28,0,	28,0,	
29,0,	21,0,	29,0,	21,0,	
0,0,	30,21,	0,0,	29,0,	
29,0,	29,0,	0,0,	0,0,	
0,0,	30,0,	30,0,	31,21,	
22,0,	30,0,	22,0,	0,0,	
0,0,	0,0,	0,0,	31,0,	
31,0,	32,21,	0,0,	31,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	32,0,	32,0,	0,0,	
30,0,	32,0,	30,0,	0,0,	
0,0,	0,0,	0,0,	30,0,	
30,0,	30,0,	31,0,	0,0,	
31,0,	28,0,	0,0,	28,0,	
0,0,	31,0,	31,0,	31,0,	
32,0,	0,0,	32,0,	29,0,	
0,0,	29,0,	0,0,	32,0,	
32,0,	32,0,	0,0,	0,0,	
33,0,	33,0,	0,0,	0,0,	
33,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	34,0,	34,0,	
35,21,	0,0,	34,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
35,0,	35,0,	0,0,	33,0,	
35,0,	33,0,	0,0,	30,0,	
0,0,	30,0,	33,0,	33,0,	
33,0,	34,0,	0,0,	34,0,	
30,38,	31,0,	0,0,	31,0,	
34,0,	34,0,	34,0,	35,0,	
0,0,	35,0,	0,0,	32,0,	
0,0,	32,0,	35,0,	35,0,	
35,0,	31,39,	0,0,	38,0,	
38,0,	0,0,	0,0,	38,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	39,0,	39,0,	0,0,	
0,0,	39,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
32,40,	0,0,	38,0,	0,0,	
38,0,	0,0,	33,0,	0,0,	
33,0,	38,0,	38,0,	38,0,	
39,0,	0,0,	39,0,	0,0,	
34,0,	0,0,	34,0,	39,0,	
39,0,	39,0,	0,0,	40,0,	
40,0,	33,41,	35,0,	40,0,	
35,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	41,0,	41,0,	
35,42,	0,0,	41,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	40,0,	0,0,	
40,0,	0,0,	0,0,	0,0,	
0,0,	40,0,	40,0,	40,0,	
0,0,	41,0,	0,0,	41,0,	
0,0,	38,0,	0,0,	38,0,	
41,0,	41,0,	41,0,	0,0,	
42,0,	42,0,	0,0,	39,0,	
42,0,	39,0,	0,0,	0,0,	
0,0,	0,0,	45,0,	45,0,	
0,0,	39,45,	45,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	42,0,	
0,0,	42,0,	0,0,	0,0,	
0,0,	0,0,	42,0,	42,0,	
42,0,	45,0,	0,0,	45,0,	
0,0,	40,0,	0,0,	40,0,	
45,0,	45,0,	45,0,	46,0,	
46,0,	0,0,	0,0,	46,0,	
41,0,	0,0,	41,0,	0,0,	
0,0,	0,0,	0,0,	48,0,	
48,0,	0,0,	0,0,	48,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	46,0,	0,0,	
46,0,	0,0,	0,0,	0,0,	
0,0,	46,0,	46,0,	46,0,	
0,0,	0,0,	48,0,	0,0,	
48,0,	0,0,	42,0,	0,0,	
42,0,	48,0,	48,0,	48,0,	
0,0,	0,0,	0,0,	0,0,	
45,0,	0,0,	45,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	42,46,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	46,0,	0,0,	46,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	46,48,	
0,0,	48,0,	0,0,	48,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-6,	yysvec+1,	0,	
yycrank+-11,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+4,
yycrank+0,	0,		yyvstop+7,
yycrank+-45,	0,		yyvstop+9,
yycrank+-79,	0,		yyvstop+12,
yycrank+-120,	0,		yyvstop+14,
yycrank+0,	0,		yyvstop+16,
yycrank+0,	0,		yyvstop+19,
yycrank+-130,	0,		yyvstop+22,
yycrank+-142,	yysvec+6,	yyvstop+25,
yycrank+-175,	yysvec+3,	yyvstop+29,
yycrank+-185,	yysvec+6,	yyvstop+33,
yycrank+0,	0,		yyvstop+37,
yycrank+-219,	0,		yyvstop+39,
yycrank+-230,	0,		yyvstop+42,
yycrank+-264,	0,		yyvstop+45,
yycrank+-274,	0,		yyvstop+48,
yycrank+-332,	0,		yyvstop+51,
yycrank+-350,	yysvec+3,	yyvstop+54,
yycrank+-365,	yysvec+3,	yyvstop+56,
yycrank+0,	yysvec+7,	0,	
yycrank+0,	0,		yyvstop+59,
yycrank+-2,	yysvec+8,	0,	
yycrank+0,	0,		yyvstop+61,
yycrank+-157,	0,		yyvstop+63,
yycrank+-398,	yysvec+3,	yyvstop+65,
yycrank+-408,	yysvec+3,	yyvstop+68,
yycrank+-444,	0,		yyvstop+71,
yycrank+-454,	0,		yyvstop+73,
yycrank+-464,	0,		yyvstop+75,
yycrank+-499,	yysvec+16,	yyvstop+77,
yycrank+-509,	yysvec+3,	yyvstop+79,
yycrank+-519,	0,		yyvstop+82,
yycrank+-2,	yysvec+27,	0,	
yycrank+-17,	yysvec+27,	yyvstop+84,
yycrank+-554,	yysvec+3,	yyvstop+86,
yycrank+-564,	yysvec+35,	yyvstop+89,
yycrank+-598,	yysvec+3,	yyvstop+91,
yycrank+-609,	yysvec+3,	yyvstop+94,
yycrank+-643,	yysvec+19,	yyvstop+97,
yycrank+-18,	yysvec+27,	0,	
yycrank+-7,	yysvec+27,	yyvstop+99,
yycrank+-653,	yysvec+3,	yyvstop+102,
yycrank+-686,	yysvec+35,	yyvstop+105,
yycrank+-14,	yysvec+27,	yyvstop+107,
yycrank+-698,	yysvec+3,	yyvstop+109,
0,	0,	0};
struct yywork *yytop = yycrank+791;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   9,  10,   1,   1,   9,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  9,   1,  34,   1,   1,   1,   1,  39, 
 40,  40,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,  40,   1,  40,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.8	95/02/11 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
