#include "asf.h"
#include <ctype.h>
#include "expression.h"




/*Check the number of parenthesis and other validity of expression.*/
int expressionMalformed(const char *expr,int nvars)
{

	int i,strln=strlen(expr);
	int nParens;
/*Check for empty expressions.*/
	if (strln==0||(strln==1&&expr[0]=='\n'))
	{
		printf("The empty string is not an expression.\n");
		return 1;
	}
	for (i=1;i<strln;i++)
		if (expr[i-1]=='('&&expr[i]==')')
		{
			printf("() means nothing to me.\n");
			return 1;
		}
/*Check for the correct number of parenthesis.*/
	nParens=0;
	for (i=0;i<strln;i++)
	{
		if (expr[i]=='(')
			nParens++;
		else if (expr[i]==')')
			nParens--;
		if (nParens<0)
		{
			printf("The parenthesis do not match properly.\n");
			return 1;
		}
	}
	if (nParens!=0)
	{
		printf("Your parenthesis do not match.\n");
		return 1;
	}
/*Check to make sure all our variables are the right ones.*/
	for (i=0;i<strln;i++)
		if (isalpha(expr[i]))
			if (tolower(expr[i])-'a'>=nvars&&
				tolower(expr[i])!='x'&&
				tolower(expr[i])!='y')
			{
				printf("The variable '%c' is undefined.\n",expr[i]);
				return 1;
			}
/*Check to make sure we never have two operators in a row.*/
	for (i=1;i<strln;i++)
		if (ispunct(expr[i-1])&&ispunct(expr[i])&&
			expr[i-1]!='('&&expr[i-1]!=')'&&
			expr[i]!='('&&expr[i]!=')')
		{
			printf("The characters %c and %c don't look right together.\n",
				expr[i-1],expr[i]);
			return 1;
		}
/*Check to make sure we never have two operands in a row.*/
	for (i=1;i<strln;i++)
		if (isalpha(expr[i-1])&&isalpha(expr[i]))
		{
			printf("The characters %c and %c don't look right together.\n",
				expr[i-1],expr[i]);
			return 1;
		}
	return 0;
}

/* Evaluation functions:*/
#define EVALFUNC(name) double name(const void *tok,const double *vars,double a, double b);\
double name(const void *tok,const double *vars,double a, double b)
EVALFUNC(addOp) { return a+b;}
EVALFUNC(subOp) { return a-b;}
EVALFUNC(mulOp) { return a*b;}
EVALFUNC(divOp) { if (b==0) return a; return a/b;}
EVALFUNC(modOp)
{
	double mod;
	if (b==0) 
		return a;
	mod=fmod(a,b);
	if (mod<0)
		mod+=b;
	return mod;
}
EVALFUNC(powOp){return pow(a,b);}
EVALFUNC(valOp)
{
	return ((token *)tok)->val;
}
EVALFUNC(varOp)
{
	return vars[((token *)tok)->index];
}

/*Tokenizer Interface: Hacks up a string into
	parts I call tokens-- these can be operators,
	variables, or constants.*/

char *currExpression;
int expressionIndex,expressionLength; 

void setTokenExpression(const char *expr)
{
	expressionLength=strlen(expr);
	currExpression=(char *)MALLOC(sizeof(char)*expressionLength);
	strcpy(currExpression,expr);
	expressionIndex=0;
}

token *nextToken(void)
{
	int index=expressionIndex;
	token *t;
	int c;
	/*Skip over white space.*/
	while (index<expressionLength&&
		((currExpression[index]==' ')||
		(currExpression[index]=='\t')||
		(currExpression[index]=='\n')))
			index++;
	/*Parse the next token, if any:*/
	if (!(index<expressionLength))
	{
		expressionIndex=index;
		return NULL;
	}
	t=(token *)MALLOC(sizeof(token));
	c=currExpression[index++];
	t->type=tokOperator;
	t->op=c;
	switch (c)
	{
		case '+':
			t->eval=addOp;t->precedence=1;break;
		case '-':
			t->eval=subOp;t->precedence=1;break;
		case '*':
			t->eval=mulOp;t->precedence=2;break;
		case '/':
			t->eval=divOp;t->precedence=2;break;
		case '%':
			t->eval=modOp;t->precedence=2;break;
		case '^':
			t->eval=powOp;t->precedence=3;break;
		case '(':
			t->precedence=10;break;
		case ')':
			t->precedence=10;break;
		default:
			if (isdigit(c))
			{/*This is a constant.*/
				t->type=tokConstant;
				t->eval=valOp;
				sscanf(&currExpression[--index],"%lf",&t->val);
				while (isdigit(currExpression[index])||currExpression[index]=='.')
					index++;
			}
			else if (isalpha(c))
			{/*This is a variable.*/
				t->type=tokVariable;
				t->eval=varOp;
				t->index=tolower(c)-'a';
			} else {
				printf("Unrecognized symbol '%c'\n",c);
				free(t);
				return NULL;
			}
	}
	expressionIndex=index;
	return t;
}

