/*
 * Name: IK_Patch.c
 *
 *
 * Description: This file contains code which is used to patch bugs in
 * system libraries and any other required non-IMS fixes.
 *
 * Notes:
 *
 *-------------------------------------------------------------------------
 *
 * RCS information:
 *
 * $RCSfile$
 *
 * $Id$
 *
 * $Log$
 * Revision 1.1  2004/02/03 03:32:54  pdenny
 * Initial revision
 *
 * Revision 5.0  1995/11/06  13:04:06  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.2  1995/09/15  19:49:58  shiva
 * memcmp is implemented by using the most natural character
 * comparison on the machine.  Thus the sign of the value returned
 * when one of the characters has its high order bit set is not
 * the same in all platform implementations and should not be
 * relied upon. Therefore IK_memcmp was written to replace memcmp
 * function. The first 2 arguments passed to IK_memcmp are
 * converted to (unsigned char *) to avoid the problems encountered
 * on some platforms with the high order bit.  Specifically, memcmp
 * was causing the sort routines not to sort correctly when more than
 * 15 hex numbers were being compared.
 * This problem existed on the SGI Irix4 but not on the Sunos4.1.3.
 * Therefore for portability reasons, IK_memcmp will be replaced
 * throughout the IMS V0 client.
 *
 * Revision 4.5.1.1  1995/09/15  19:37:39  shiva
 * Place holder for the 4.5.1 branch.
 *
 * Revision 4.5  1995/07/27  18:43:06  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:21:04  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:50:05  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.5  1994/06/23  16:14:40  winter
 * Added conditional definition of strdup().
 *
 * Revision 4.0.1.4  1994/06/22  12:45:03  winter
 * Removed stdlib.h, added sys/types.h.
 *
 * Revision 4.0.1.3  1994/06/21  16:06:51  winter
 * Added ims_realloc().
 *
 * Revision 4.0.1.2  1994/06/20  19:56:03  winter
 * Changed return code for ims_free() to always be 1, since older versions of
 * free() return 1 on success and 0 on failure.
 *
 * Revision 4.0.1.1  1994/06/20  19:34:52  winter
 * Added code for ims_free()/
 *
 * Revision 4.0  1994/06/20  16:40:04  ims
 * Initial checkin of stub code.
 *
 * */

/*****************************************************************************/

/* Define the RCS identifier string. */
static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* Standard headers */
#include <sys/types.h>

/* Third-party library headers */

/* IMS headers */
#include "IK_Patch.h"

/*****************************************************************************/

/* External variable declarations */

/*****************************************************************************/

/* Local #define directives */

/* Define NULL if it hasn't been defined yet. */
#ifndef NULL
#define NULL ((void *) 0)
#endif

/*****************************************************************************/

/* Local variable definitions */

/*****************************************************************************/

/* Local function prototypes */

/*****************************************************************************/

/*
 * Name: ims_free()
 *
 * Description: This function is used to patch non-ANSI behavior in
 * the free() standard library routine in some implementations (most
 * noticeably Sun's acc compiler). This function checks to see if its
 * argument is NULL, and returns immediately if it is. If non-NULL,
 * the argument is freed using a call to the library free()
 * routine. In either case, the return value from this function is
 * always 1, since older versions of free() return 1 on success and 0
 * on failure.
 *
 * Parameters:
 * void *ptr - Pointer to memory to be freed.
 *
 * Return Values:
 * int 1 - At all times (for compatibility with older code)
 *
 * Warnings:
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions
 *
 * Notes:
 *
 * Revision History:
 *
 * Monday 20 June 1994 (Eric Winter) - Inserted and modified original
 * code from Brett Kerbel.
 *
 * */

/* N.B. If free is defined as a preprocessor macro, undefine it before
   proceeding. */
#ifdef free
#undef free

/* Include an external prototype for the library free() function. */
extern void free(void *ptr);

int ims_free(void *ptr)
{

  /* If the pointer is non-NULL, deallocate it using the library
     free() routine. Some older free() implementations have a return
     value, so explicitly discard it. */
  if (ptr != NULL) {
    (void) free(ptr);
  }

  /* Always return 1 (for compatibility with older code which expects
     a return value from free()). */
  return(1);

}

#endif   /* free */

/*****************************************************************************/

/*
 * Name: ims_realloc()
 *
 * Description: This function is used to patch non-ANSI behavior in
 * the realloc() standard library routine in some implementations
 * (most noticeably Sun's acc compiler). This function checks to see
 * if its argument is NULL, and returns newly-allocated space of the
 * specified size if it is. If non-NULL, the argument is reallocated
 * using a call to the library realloc() routine. In either case, the
 * return value from this function is always the pointer to the new
 * memory.
 *
 * Parameters:
 * void *ptr - Pointer to memory to be reallocated
 * size_t size - Size of memory to allocate
 *
 * Return Values:
 * void *ptr - Pointer to newly reallocated space; this is the argument, ptr,
 * but changed to point (possibly) to the new memory space.
 *
 * Warnings:
 *
 * Global Variables Used:
 * None
 *
 * Pre- and Post-Conditions
 *
 * Notes:
 *
 * Revision History:
 *
 * Tuesday 21 June 1994 (Eric Winter) - Inserted and modified original
 * code from Brett Kerbel.
 *
 * */

/* N.B. If realloc is defined as a preprocessor macro, undefine it
   before proceeding. */
#ifdef realloc
#undef realloc

/* Include an external prototype for the library realloc() and
   malloc() functions. */
extern void *realloc(void *ptr, size_t size);
extern void *malloc(size_t size);

void *ims_realloc(void *ptr, size_t size)
{

  /* If the pointer is NULL, allocate it using the library malloc()
     function. */
  if (ptr == (void *) NULL) {
    ptr = malloc(size);
  } else {
    ptr = realloc(ptr, size);
  }

  /* Always return a pointer to the new space. */
  return(ptr);

}

#endif   /* realloc */

/*****************************************************************************/

/*
 * Name: strdup()
 *
 * Description: This function is used only if the strdup() function is
 * not available on the host system, i.e. if the preprocessor constant
 * HAVE_STRDUP is _not_ defined. This function takes a pointer to a
 * null-terminated string, allocates memory for a copy, and copies the
 * string into the new space. A pointer to the copy is returned.
 *
 * Parameters:
 * const char *s - Pointer to null-terminated string to copy
 *
 * Return Values:
 * char *copy - Pointer to new copy of s
 * NULL - If an error occurs
 *
 * Warnings:
 *
 * The caller is responsible for freeing the memory allocated by this
 * function.
 *
 * If the argument is a NULL pointer, results of this function are
 * undefined.
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions
 *
 * Notes: This function should be conditionally compiled only if
 * HAVE_STRDUP is not defined.
 *
 * Revision History:
 *
 * Wednesday 18 May 1994 (Eric Winter) - Initial version.
 *
 * Thursday 23 June 1994 (Eric Winter) - Transferred here from
 * IK_Stats.c.
 * */

#ifndef HAVE_STRDUP

char *strdup(const char *s)
{

  /* copy is a pointer to the buffer allocated to hold the copy of
     string s. */
  char *copy;

  /*-------------------------------------------------------------------------*/

  /* Allocate memory for the copy. */
  if ((copy = malloc(strlen(s) + 1)) == NULL) {

    /* Couldn't allocate space for the copy. */

  } else {

    /* Copy the string. */
    (void) strcpy(copy, s);

  }

  /* Return the copy (if good) or NULL (if bad). */
  return(copy);

}

#endif   /* !HAVE_STRDUP */


/*
 * memcmp is implemented by using the most natural character comparison on
 * the machine.  But, the sign of the value returned when one of the
 * characters has its high order bit set is not implemented the same way
 * on different platforms and the implementations and should not be relied 
 * upon. Therefore the following function was written to replace memcmp
 * function throughout the IMS V0 client for portability reasons. The first
 * 2 arguments passed to IK_memcmp are converted to (unsigned char *) to
 * avoid the problems encountered on some platforms with the high order bit.
 */

int IK_memcmp(void *st1, void *st2, size_t n)
{
  unsigned char *cst1 = (unsigned char *)st1;
  unsigned char *cst2 = (unsigned char *)st2;

  while(n > 0) {
    if((*cst1) != (*cst2)) 
      return ((*cst1) - (*cst2));
    cst1++; 
    cst2++; 
    n--;
  }
  return 0;
}

