/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:       ADEOS_msg.c

Description:    Contains the functions to parse the ADEOS mail message.

External Functions:
	None
	
Static Functions:
	usage
	main
	
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

#include <unistd.h>
#include <stdio.h>
#include <syslog.h>
#include <sys/wait.h>
#include <sys/types.h>
#include "faifdefs.h"
#include "ADEOS_msg.h"

extern int   create_ADEOS_msg() ;
extern MESSAGEPTR parse_ADEOS_msg() ;

static void usage(char *) ;



/*==============================================================================
Function:	static void usage(char *progname)
Description:	Usage banner for ADEOS_msg program	
Parameters:	name of program
Returns:	None
Creator:	Cameron Cooper
Creation Date:	Thu Sep  7 11:44:19 PDT 1995
Notes:		
==============================================================================*/
static 
void usage(char *progname)
{
   printf(" Usage:\n") ;
   printf("    %s -c configfile\n\n", progname) ;
} /* usage */




/*==============================================================================
Function:       main
Description:    Pulls out the subject of the ADEOS mail message, and the 
                individual messages, and passes the messages to parse_ADEOS_msg.  
		Then creates a linked list, and passes it to create_ADEOS_msg. 
Parameters:    	argv, argc 
Returns:       	None 
Creator:        Philip Yurchuk
Creation Date:  05/30/1995
Notes:
==============================================================================*/
void
main(int argc, char *argv[])
{
   extern char *optarg ;
   int opt;                             /* Option letter */
   char *optlist = "c:" ;               /* Valid options */
   char *configfile = NULL ; 
   char *rootpath = NULL ;
   char temp[80];
   char temp2[80];
   char temp3[80];
   char temp4[80];
   char temp5[80];
   char temp6[80];
   char subject[80];
   char address[80];
   char message[256];
   MESSAGEPTR head;
   MESSAGEPTR node1;
   MESSAGEPTR node2;
   int i = 1;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Open the system error log 
   */
   openlog(argv[0], LOG_PID|LOG_CONS|LOG_NDELAY, SYSLOG_FAIF) ;
   setlogmask(LOG_UPTO(LOG_DEBUG)) ;
   syslog(LOG_INFO, "Starting adeosmsg program");

   /* Parse command line options
   */
   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt) {
         case 'c' :
            configfile = (char *) strdup(optarg) ;
            break ;
         default :
            usage(argv[0]) ;
            exit(1) ;
      } /* endswitch */

   if (configfile == (char *)NULL)
   {
      fprintf(stderr, 
	 "Notice:  No config file specified in %s execution.\n",
	 argv[0]) ;
      syslog(LOG_NOTICE,
	 "Notice: No config file specified in %s execution.\n",
	 argv[0]) ;
   }

   /* Check if FAIF rootpath is specified
   */
   rootpath = (char *)get_config_val(configfile, FAIF_ROOTPATH_EV) ;
   if (rootpath == (char *)NULL)
   {
      fprintf(stderr, "FAIF_ROOTPATH unspecified.  Exiting.\n") ;
      syslog(LOG_ERR, "FAIF_ROOTPATH unspecified.  Exiting.\n") ;
      exit(1) ;
   }

   /* get the return address of the message 
   */
   while (i == 1)
   {
      if (!(gets(temp3)))
	  exit(0);
      if (strstr(temp3, "From:"))
	i = 0;
   }
 
   strcpy(address, &temp3[6]);

   i = 1;

   /* get the subject of the message 
   */
   while (i == 1)
   {
      if (!(gets(temp2)))
	  exit(0);
      if (strstr(temp2, "Subject:"))
	i = 0;
   }
  
   strcpy(temp4, &temp2[9]);
   temp4[6] = NULL;
   strncpy(temp5, &temp2[15], 10);
   temp5[10] = NULL;
   strcpy(temp6, &temp2[25]);
   strcpy(subject, temp4);
   strcat(subject, temp6);
   strcat(subject, temp5);

   i = 1;

   /* go through the mail until you find the "Beginning of Message" indicator 
   */
   while (i == 1)
   {
      if (!(gets(temp)))
	  exit(0);
      if (strstr(temp, "<BOM>"))
	i = 0;
   }

   i = 1;

   /* after <BOM> has been found, start reading in the messages 
   */

   if (!gets(message))
   {
      syslog(LOG_DEBUG, "File ends prematurely\n") ;
      exit(0);
   }
   if (strstr(message, "<EOM>"))
   {
      syslog(LOG_DEBUG, "No files to get - message empty\n") ;
      exit(0);
   }
  
   head = node1 = NULL ;

   /* send messages to parse_ADEOS_msg 
   */
   if (message[0])
      head = node1 = parse_ADEOS_msg(message) ;    
   while(gets(message))                   /* create a linked list of    */
   {                                      /* nodes containing the msg.  */
      if (message[0])                     /* contents                   */
      {
         if (strstr(message, "<EOM>"))
	    break;
	 if (!node1)
	    head = node1 = parse_ADEOS_msg(message) ;
	 else
	 {
	    node1->next = parse_ADEOS_msg(message) ;
	    node1 = node1->next ;
	 }
      }
   }

   create_ADEOS_msg(head, address, subject, 1, configfile, 'R') ; 
  
} /* main */

/* End of File */
