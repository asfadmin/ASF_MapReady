/*
 * @(#)memUtils.c	2.3 95/11/03 15:04:39
 *
 */

#include <stdio.h>
#include <malloc.h>
#include "memUtils.h"

static char sccsid_memutils_c[] =
	"@(#)memUtils.c	2.3 95/11/03 15:04:39";

/*
 * NAME:
 * doMalloc
 *
 * DESCRIPTION:
 * provide a cover routine for malloc so that any issues
 * involving its allocation and release can be managed
 * and under threads
 *
 * NOTES:
 *
 *
 */

void *doMalloc(int size)
{
    void  *retvalPtr;

    /*
    retvalPtr = usmalloc(size, (usptr_t *)getusPtrHandle() );
    */

    retvalPtr = (void *) malloc(size);
    return(retvalPtr);

}


/*
 * NAME:
 * doRealloc
 *
 * DESCRIPTION:
 * provide a cover routine for malloc so that any issues
 * involving its allocation and release can be managed
 * and under threads
 *
 * NOTES:
 *
 *
 */

void *doRealloc(void *ptr, int size)
{
 
    ptr = (void *) realloc(ptr, size);

    /*
    retvalPtr = realloc(ptr, size);
    retvalPtr = usrealloc(ptr, size, (usptr_t *)getusPtrHandle() );
    */

    return(ptr);
}


/*
 * NAME:
 * doFree
 *
 * DESCRIPTION:
 * provide a cover routine for malloc so that any issues
 * involving its allocation and release can be managed
 * and under threads
 *
 * NOTES:
 *
 *
 */

void doFree(void *ptr)
{
    /*
    usfree(ptr, (usptr_t *)getusPtrHandle());
    */

    free(ptr); 
    ptr = NULL;
}
