/* Expression functions.*/

#ifndef _EXPRESSION_H_
#define _EXPRESSION_H_

/********EXTERNAL VARIABLES FROM ASF.H******************/
extern FILE *fLog;		
extern int logflag;		
extern char logFile[255];	
extern int quietflag;		
extern char logbuf[255];
extern char errbuf[255];
/********************************************************/


char *expression2cookie(const char *expr,int nvars);
double evaluate(char *cookie,const double *variables);

/* Internal Variables.*/
typedef enum {
	tokOperator,tokConstant,tokVariable
}tokenType;

typedef double (*evalFunc)(const void *tok,const double *vars,double a,double b);

typedef struct token {
	tokenType type;
	char op; /*For operators, the operator (+,-,/,*).*/
	int precedence; /*For operators, the precedence.*/
	int index;/*For variables, the variable's index.*/
	evalFunc eval; /*Evaluation function.*/
	double val; /*For constants, the value of the constant.*/
} token;


/*Tokenizer functions.*/
int expressionMalformed(const char *expr,int nvars);
void setTokenExpression(const char *expr);
token *nextToken(void);
#endif
