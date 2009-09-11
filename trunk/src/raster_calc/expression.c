#include "asf.h"
#include "expression.h"

char *expression2cookie(const char *expr,int nvars)
{
	token **outputStack=(token **)MALLOC(200*sizeof(token));
	int outputPtr=0;
	token **operandStack=(token **)MALLOC(200*sizeof(token));
	int operandPtr=0;
	
	token *next;
	if (expressionMalformed(expr,nvars))
		return NULL;
	setTokenExpression(expr);
	while (NULL!=(next=nextToken()))
	{
		if (next->type==tokOperator)
		{
			if (next->op==')')
			{
				while (operandStack[operandPtr-1]->op!='(')
					outputStack[outputPtr++]=operandStack[--operandPtr];
				operandPtr--;/*Pop off (*/
			}
			else 
			{
				int precedence=0;
				if (operandPtr-1>=0&&operandStack[operandPtr-1]->op!='(')
					precedence=operandStack[operandPtr-1]->precedence;
				if (next->precedence<=precedence)
					outputStack[outputPtr++]=operandStack[--operandPtr]; 
				operandStack[operandPtr++]=next;
			}
		} else /*Just push variables and constants.*/
			outputStack[outputPtr++]=next;
	}
	while (operandPtr!=0)
		outputStack[outputPtr++]=operandStack[--operandPtr];
	outputStack[outputPtr++]=NULL;
	return (char *)outputStack;
}
double evaluate(char *cookie,const double *variables)
{
	token **tokenStack=(token **)cookie;
	double evalStack[100]={0.0,0.0,0.0};
	int evalPtr=2;
	while (*tokenStack)
	{
		evalStack[evalPtr]=(*tokenStack)->eval(
			(void *)*tokenStack,variables,evalStack[evalPtr-2],evalStack[evalPtr-1]);
		if ((*tokenStack++)->type==tokOperator)
		{
			evalStack[evalPtr-2]=evalStack[evalPtr];
			evalPtr--;
		} else evalPtr++;
	}
	return evalStack[evalPtr-1];
}
