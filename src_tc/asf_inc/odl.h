/*-------------------------------*
 |  ODL Interface:  R1A Release.
 |	@(#)odl.h	2.9 95/11/02 13:00:47
 |  This is the main header file for asf_odl.a,
 |  ASF's Object Description Language reading
 |  library.  Private include files are in
 |  asf_odl/include.
 *-------------------------------*/

#ifndef _ODL_R1A_H_
#define _ODL_R1A_H_


#include <time.h>
#include "Msg.h"			/* R1B Interface */


typedef	void*	ODL;

#ifndef	_ODL_TIME_H_
/*----------------------------*
 |  Augmented 'tm' structure
 *----------------------------*/
typedef struct {
    struct tm   tm;                     /* Standard 'tm' structure */
    int        tv_usec;                /* and microseconds */

} GMT_t;
#endif

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

#ifdef	NOSTDARG
int	ODLSetVal();
#else
int	ODLSetVal(ODL, char* name, ...);
#endif

#endif	/*!_ODL_R1A_H_ */
