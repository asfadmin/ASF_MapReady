/*==============================================================================
Filename:	par.c
==============================================================================*/

#include <stdio.h>
#include <string.h>
#include <des_crypt.h>
#include <stdlib.h>
#include <unistd.h>
#include "faifdefs.h"
#include "sendfile.h"
#include "getupass.h"

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
      printf ("WARNING, Unable to open password file\n") ;
      return(NULL) ;
   }

   fread(buf, sizeof(char), BUFSIZE, pass_fp) ;
   fclose(pass_fp) ;
   estatus = cbc_crypt(key, buf, BUFSIZE, DES_DECRYPT | DES_SW, iv);
   switch(estatus)
   {
      case DESERR_NONE:
         break ;

      case DESERR_NOHWDEVICE:
         printf("NOTICE: cbc_crypt status DESERR_NOHWDEVICE\n") ;
         break ;

      case DESERR_BADPARAM:
         printf("WARNING, Bad parameters passed to cbc_crypt\n") ;
         printf("WARNING, Unexpected password decryption behavior\n") ;
	 return(NULL) ;
         break ;

      default:
         printf("WARNING, Unknown status retured by cbc_crypt\n") ;
         printf("WARNING, Unexpected password decryption behavior\n") ;
	 return(NULL) ;
   }

   if ((decoded = (char *)util_do_malloc(sizeof(char)*(strlen(buf)+1)))
       != (char *)NULL)
      strcpy(decoded, buf) ;

   return(decoded) ;

} /* get_FA_upass */

/*===========================================================================*/

main(int argc, char *argv[])
{
   char *comma, *ups ;
   char *upassfile = NULL ;
   char *username, *password ;

   upassfile = argv[FTAM] ;

   if (access(upassfile, R_OK) != 0)
   {
      printf("WARNING, No read permission for password file %s\n",
	  upassfile);
      return(ERROR) ;
   }

   if ((ups = get_FA_upass(upassfile)) == (char *)NULL) 
   {
      printf("WARNING, Unable to decrypt password file %s\n", upassfile);
      return(ERROR) ;
   }

   comma = strchr(ups, ',') ;
   if (comma == (char *)NULL)
   {
      printf("WARNING, Invalid decrypted data from password file %s\n",
	  upassfile);
      return(ERROR) ;
   }
   *comma = '\0' ;
   username = ups ;
   password = comma+1 ;
   printf ("%s %s \n", username, password);
   free(ups);
} 




