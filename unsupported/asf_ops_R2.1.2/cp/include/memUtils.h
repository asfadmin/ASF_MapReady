#ifndef _MEMUTILS_H
#define _MEMUTILS_H
/*--------------------------------
 * sccs header
 * @(#)memUtils.h	2.4 95/06/28 17:55:45
 *------------------------------*/
/*******************************
* memUtils.h
********************************/

static char sccsid_memutils_h[] =
	"@(#)memUtils.h	2.4 95/06/28 17:55:45";

#ifdef sgi
#include <ulocks.h>
#endif 

void  *doMalloc(int size);
void  *doRealloc(void *ptr, int size);
void  doFree(void *ptr);

#endif   /* !_MEMUTILS_H */
