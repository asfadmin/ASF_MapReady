/*==============================================================================
Filename:	getupass.c

Description:	
	This module contains the function for decrypting a file to obtain
username and password information.  The username and password obtained is
used by the FAIF for transferring files to remote flight agency hosts or 
systems in ASF on remote hosts (ex. ACS).

External Functions:
	get_FA_upass
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <des_crypt.h>
#include "getupass.h"

#ifdef __STDC__
char *get_FA_upass(char *) ;
#else
char *get_FA_upass() ;
#endif

extern void *util_do_malloc() ;



/*==============================================================================
Function:	get_FA_upass

Description:	
	This function decrypts an FAIF encrypted password file.  The
information obtained is a string containing a username and password which
FAIF uses to transfer files for a particular interface. The format of the
string is simply the unencoded username followed by the password separated
by a single comma.  It is assumed that the username does not contain a
comma.

Parameters:
	char *upassfile - name of password file to decrypt

Returns:	pointer to unencoded username and password string or NULL
Creator:	Norbert Piega	
Creation Date:	11/28/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
char *
get_FA_upass(char *upassfile)
#else
char *
get_FA_upass(upassfile)
   char *upassfile ;
#endif
{
   char  key[KEYSIZE] ;
   char  iv[IVSIZE] ;
   char  buf[BUFSIZE] ;
   FILE *pass_fp ;
   char *decoded = NULL ;
   int estatus ;

   memset(key, 0, (size_t)sizeof(key)) ;
   memset(iv, 0, (size_t)sizeof(iv)) ;
   strcpy(key, FAIF_ENC_KEY) ;
   des_setparity(key) ;

   if ((pass_fp = fopen(upassfile,"r")) == (FILE *)NULL)
   {
      syslog(LOG_ERR, "WARNING, Unable to open password file\n") ;
      return(NULL) ;
   }

   fread(buf, sizeof(char), BUFSIZE, pass_fp) ;
   fclose(pass_fp) ;
   estatus = cbc_crypt(key, buf, BUFSIZE, DES_DECRYPT | DES_SW, iv);
   switch(estatus)
   {
      case DESERR_NONE:
/*         syslog(LOG_DEBUG, "NOTICE: No error in call to cbc_crypt\n") ; */
         break ;

      case DESERR_NOHWDEVICE:
         syslog(LOG_DEBUG, "NOTICE: cbc_crypt status DESERR_NOHWDEVICE\n") ;
         break ;

      case DESERR_BADPARAM:
         syslog(LOG_DEBUG, "WARNING, Bad parameters passed to cbc_crypt\n") ;
         syslog(LOG_ERR, "WARNING, Unexpected password decryption behavior\n") ;
	 return(NULL) ;
         break ;

      default:
         syslog(LOG_DEBUG, "WARNING, Unknown status retured by cbc_crypt\n") ;
         syslog(LOG_ERR, "WARNING, Unexpected password decryption behavior\n") ;
	 return(NULL) ;
   }

   if ((decoded = (char *)util_do_malloc(sizeof(char)*(strlen(buf)+1)))
       != (char *)NULL)
      strcpy(decoded, buf) ;

   return(decoded) ;

} /* get_FA_upass */


/* End of File */
