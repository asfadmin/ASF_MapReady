/*
 * Name: IK_Auth.c
 *
 * Description: This file contains the code to generate a user
 * authentication key.
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
 * Revision 5.0.1.2  1995/12/04  20:36:09  katen
 * Added to_ulower macro and changed auth_key_tmp to point to
 * to_lower macro.  Correction for new build.
 *
 * Revision 5.0.1.1  1995/11/06  13:12:02  ims
 * COPIED FROM 4.5.1.3 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.3  1995/10/26  12:17:02  winter
 * Removed SUNOS_413 conditionals.
 *
 * Revision 4.5.1.2  1995/10/17  11:12:57  winter
 * Added conditional include for <crypt.h>, and a prototype for the crypt()
 * function when the header is not available. The primary offender is
 * SunOS 4.1.3.
 *
 * Revision 4.5.1.1  1995/10/09  16:22:01  winter
 * Cleaned up and removed superfluous header files.
 *
 * Revision 4.5  1995/07/27  18:41:46  ims
 * PROMOTION FROM GUI_4_4_8_950501 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4  1995/02/13  22:19:34  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3.1.2  1994/11/17  17:57:56  ryan
 * SMR 1734
 * Removed function IK_AddAuthenticator().  Moved function to IK_Nmfn.c
 * IK_AddAuthenticator is not necessary for the IK library.
 *
 * Revision 4.3.1.1  1994/11/17  15:47:58  ryan
 * new branch
 *
 * Revision 4.3  1994/08/26  10:49:03  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.5  1994/06/20  15:37:53  kerbel
 * beefed up comment for resetting errno to 0
 *
 * Revision 4.0.1.4  1994/06/13  18:04:07  kerbel
 * fix stupid syntax bug
 *
 * Revision 4.0.1.3  1994/06/13  18:02:13  kerbel
 * Add #include <errno.h>
 *
 * Revision 4.0.1.2  1994/06/13  17:58:31  kerbel
 * In function IK_AddAuthenticator: if length of authenticator is 0
 * then reset errno to 0 after calling RemoveParameter, because
 * on some systems including rhine.stx.com running SunOS 4.1.3
 * errno is incorrectly set to EINVAL upon return from RemoveParameter.
 *
 * Revision 4.0.1.1  1994/06/13  17:55:54  kerbel
 * new branch
 *
 * Revision 4.0  1994/06/08  14:08:28  ims
 * PROMOTION FROM REV4
 *
 * Revision 1.4.1.5  1994/05/18  20:20:27  ryan
 * overhauled IK_AddAuthenticator code.  removed reference to global
 * AGGREGATE profile.  added call to IK_GetProfileValues().
 *
 * Revision 1.4.1.4  1994/05/02  15:38:33  ryan
 * IK_MakeAuthenticator will now ignore non-alphanumeric characters
 * and will ignore case in FIRST_NAME, LAST_NAME, and AUTHENTICATION_KEY.
 *
 * Revision 1.4.1.3  1994/04/28  18:47:45  ryan
 * Fixed up code which attaches AUTHENTICATOR to outgoing messages.
 * There was a danger that the ODL tree could be corrupted.
 *
 * Revision 1.4.1.2  1994/04/28  17:48:01  ryan
 * Removed logic which used system time to perturb encryption.
 *
 * Revision 1.4.1.1  1994/04/28  16:39:08  ryan
 * new branch
 *
 * Revision 1.4  1994/04/25  14:22:31  ryan
 * modified IK_MakeAuthenticator so that it only pays attention to
 * alphanumeric characters.  blanks are ignored and case is not
 * significant.
 * completed IK_AddAuthenticator.
 *
 * Revision 1.3  1994/04/21  17:58:56  ryan
 * continued development of authentication code.
 *
 * Revision 1.2  1994/04/19  14:36:00  ryan
 * added functionality and comments.
 *
 * Revision 1.1  1994/03/30  16:45:24  ryan
 * Initial revision
 *
 *
 */

/*****************************************************************************/

static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

/* This header must be #included first in order to use
   autoconf-generated information. */
#include "IK_Ims.h"

/* Standard headers */
#include <ctype.h>
#include <string.h>

/* IMS headers */
#include "IK_Auth.h"
#include "IK_DataDic.h"

/*****************************************************************************/

/* External function declarations */

extern char *crypt(const char *, const char *);

/*****************************************************************************/

/* Local #define directives */

/* safe definition of toupper */
#define to_upper(c) ((isalpha(c) && islower(c)) ? toupper(c) : c)

/* safe definition of tolower */
#define to_lower(c) ((isalpha(c) && isupper(c)) ? tolower(c) : c)

#define IK_SALT_SZ (64)

/*****************************************************************************/

/* Local variable definitions */

char *IK_salt =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789/.";

/*****************************************************************************/

/*
 * Name:
 *   IK_MakeAuthenticator
 *
 * Description:
 *   generate an authentication string for outgoing ODL messages   
 *
 * Parameters:
 *   char *string1, *string2, *authentication_key
 *     These are 3 arbitrary strings.  Usually, however, string1 is
 *     the user's first name, string2 is the last name, and
 *     authentication_key is the user key.
 *
 * Return Values:
 *   type: int
 *     If any of the ODL library calls fails, -1 will be returned.
 *     If no error occurs, 0 will be returned.
 *
 * Warnings:
 *   If any of the ODL calls fail, the memory allocated by the prior calls
 *   may go unused.
 *
 * Global Variables Used:
 *   none
 *
 * Pre- and Post-Conditions
 *
 * Notes:
 *   
 *   The algorithm for creating the authenticator is very simple.  It
 *   makes use of the Unix crypt(3) function.  The string1 and string2
 *   parameters are used to pick the two salt characters.  This is
 *   achieved via a checksum of sorts. The salt characters and the
 *   authentication_key parameters are passed to crypt() and the
 *   result from crypt is returned by the function.  When string1 and
 *   string2 are traversed to create the salt characters, only
 *   alphanumeric characters are used.  Additionally, it changes all
 *   alphabetic characters to uppercase.  This is done in order to
 *   avoid problems with people typing in their names in different
 *   ways.
 * 
 * Revision History:
 *   940329 PMR authored.
 */

char *
IK_MakeAuthenticator(char *string1, char *string2, char *authentication_key)
{
    char salt[2], auth_key_tmp[IK_AUTHENTICATION_KEY_SZ + 1], *p;
    static char	authenticator[IK_AUTHENTICATOR_SZ + 1];
    int	i;

    /*------------------------------------------------------------------------*/

    /* If the user does not have an authentication key, then simply
     * zero out the IK_authenticator string.  This will result in a
     * zero-length string being sent out as the AUTHENTICATOR
     * parameter in ODL messages.
     *
     * First, we want to strip out any non-alphanumerics in the
     * authentication key and change everything to the same case.
     */
    for (i = 0, p = authentication_key; *p; ++p) {
	if (isalnum(*p)) {
	    auth_key_tmp[i] = to_lower(*p);
	    ++i;
	}
    }
    auth_key_tmp[i] = '\0';

    if (!*auth_key_tmp) { /* no key present */
	(void) memset(authenticator, 0, IK_AUTHENTICATOR_SZ + 1);
	return(authenticator);
    }

    /* calculate the first salt character */
    for (i = 0, p = string1; *p; ++p) {
	if (!isalnum(*p)) {
	    continue;
	}
	i += (int) to_upper(*p);
    }
    salt[0] = IK_salt[i % IK_SALT_SZ];

    /* calculate the second salt character */
    for (i = 0, p = string2; *p; ++p) {
	if (!isalnum(*p)) {
	    continue;
	}
	i += (int) to_upper(*p);
    }

    salt[1] = IK_salt[i % IK_SALT_SZ];

    /* encrypt the key */
    p = crypt(auth_key_tmp, salt);

    /* put the result in a global variable */
    if (p) {
	(void) strcpy(authenticator, p);
    } else {
	/* error */
    }

    return(authenticator);
}
