/*-------------------------------*
 |  ODL Interface:  R1A Release.
 |	@(#)odl.h	2.9 95/11/02 13:00:47
 *-------------------------------*/

#ifndef _ODL_R1A_H_
#define _ODL_R1A_H_

#include <string.h>
#include <time.h>
#include "Msg.h"			/* R1B Interface */

static  char sccsid_odl_h[] =
        "@(#)odl.h	2.9 95/11/02 13:00:47";

typedef	void*	ODL;

#ifndef	_ODL_TIME_H_
/*----------------------------*
 |  Augmented 'tm' structure
 *----------------------------*/
typedef struct {
    struct tm   tm;                     /* Standard 'tm' structure */
    long        tv_usec;                /* and microseconds */

} GMT_t;
#endif

#ifdef	_NO_PROTO

int	ODLinit();
void	ODLFree();
char*	ODLToStr();
ODL	ODLcopy();
ODL	StrToODL();
ODL	ODLparse();

int	ODLGetVal();
int	ODLGetInt();
int*	ODLGetArrayInt();
double	ODLGetDouble();
double* ODLGetArrayDouble();
char*	ODLGetString();
char*	ODLGetStr();
char*	ODLGetTime();

int	ODLSetInt();
int	ODLSetDouble();
int	ODLSetString();

int	m_init();
void*	m_alloc();
void	m_free();

#else	/*!_NO_PROTO */

int	ODLinit(void);
void	ODLFree(void*);
char*	ODLToStr(ODL, char* name);
ODL	ODLcopy(ODL);
ODL	StrToODL(char* str, size_t len);
ODL	ODLparse(char* str_or_file, size_t strLen_or_0_if_file, char* err);


int	ODLGetVal(ODL, char* name, void* val);
int	ODLGetInt(ODL, char* name, int* err);
int*	ODLGetArrayInt(ODL, char* name, int* val, int* n_row, int* n_col);
double	ODLGetDouble(ODL, char* name, int* err);
double*	ODLGetArrayDouble(ODL, char* name, double* val, int* n_row, int* n_col);
char*	ODLGetString(ODL, char* name, int* err);
char*	ODLGetStr(ODL, char* name);	/* non-malloced version */
char*	ODLGetTime(ODL, char* name, int* err);

int	ODLSetInt(ODL, char* name, int val, char* unit);
int	ODLSetDouble(ODL,char* name, double val, char* unit);
int	ODLSetString(ODL, char* name, char* val);

int	m_init(void);
void*	m_alloc(size_t size, void *ptr);
void	m_free(void* ptr);

#endif	/*!_NO_PROTO */

#ifdef	NOSTDARG
int	ODLSetVal();
#else
int	ODLSetVal(ODL, char* name, ...);
#endif

#endif	/*!_ODL_R1A_H_ */
