/*==============================================================================
Filename:	getpass.c

Description:	
	This module contains a version of the getpass function.  The
getpass function is used to prompt for a string (usually a password)
without echoing back user input.  The getpass function supplied in the
Solaris function library limits the size of the user string to 8 
characters.  This implementation allows us to increase the allowed 
limit.

External Functions:
	getpass
	
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

#include <sys/siginfo.h>
#include <sys/signal.h>
#include <stdio.h>
#include <termios.h>

#define	MAX_PASS_LEN	100		/* max #chars for user to enter */

#ifdef __STDC__
char * getpass(const char *) ;
#else
char * getpass() ;
#endif



/*==============================================================================
Function:	char *getpass(const char *prompt)	

Description:		
	This function prompts for user input without echoing back the
inputted data.  This function is usually used for obtaining passwords.

Parameters:
	const char *prompt - the string printed to prompt the user for
input

Returns:	char * (points to buffer containing inputted data)

Creator:	Norbert Piega
Creation Date:	01/05/1994
Notes:		
	This code was taken from Richard Stevens's book "Advanced Programming
in the Unix environment", Program 11.8, page 350.
==============================================================================*/
#ifdef __STDC__
char *
getpass(const char *prompt)
#else
char *
getpass(prompt)
   const char *prompt ;
#endif
{
   static char buf[MAX_PASS_LEN + 1] ; /* null byte at end */
   char *ptr ;
   sigset_t sig, sigsave ;
   struct termios term, termsave;
   FILE *fp;
   int c;

   if ( (fp = fopen(ctermid(NULL), "r+")) == NULL)
      return(NULL) ;
   setbuf(fp, NULL) ;

   sigemptyset(&sig) ;   /* block SIGINT & SIGTSTP, save signal mask */
   sigaddset(&sig, SIGINT) ;
   sigaddset(&sig, SIGTSTP) ;
   sigprocmask(SIG_BLOCK, &sig, &sigsave) ;

   tcgetattr(fileno(fp), &termsave) ; /* save tty state */
   term = termsave ;                  /* structure copy */
   term.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL) ;
   tcsetattr(fileno(fp), TCSAFLUSH, &term) ;

   fputs(prompt, fp) ;

   ptr = buf ;
   while ( (c = getc(fp)) != EOF && c != '\n') 
   {
      if (ptr < &buf[MAX_PASS_LEN])
         *ptr++ = c ;
   }
   *ptr = 0 ;   /* null terminate */
   putc('\n', fp) ;   /* we echo a newline */

   /* restore tty state */
   tcsetattr(fileno(fp), TCSAFLUSH, &termsave) ;

   /* restore signal mask */
   sigprocmask(SIG_SETMASK, &sigsave, NULL) ;
   fclose(fp) ;   /* done with /dev/tty */

   return(buf);

} /* getpass */


/* End of File */
