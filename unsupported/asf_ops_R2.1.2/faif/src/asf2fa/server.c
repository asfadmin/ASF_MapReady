/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	server.c

Description:
	This module contains the FAxmitserver main function, which
processes command line options and calls the DCE RPC server 
initialization routine. It also contains the server initialization
function, and other functions having to do with the basic server itself.

External Functions:
	None
	
Static Functions:
	usage
	main
        verbose
        server_init
        server_shutdown
        set_sig_trap
	
External Variables Defined:
	FAxmitserver_configfile
        binding vector
        Entry_Name
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFile[] = "server_new.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "12 Sep 1996";
static char SccsLastChanger[] = "@(#)server_new.c	1.3";
static char SccsState[] = "1.3";

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <syslog.h>
#include <dce/rpcexc.h>
#include <signal.h>
#include "sendtoFA.h"      /* header created by the idl compiler */
#include "faifdefs.h"
#include "server_send_data.h"
#include "server_send.h"
#include "check_status.h"  /* header with the CHECK_STATUS macro */

#ifdef __STDC__
static void usage(char *) ;
#else
static void usage() ;
#endif

#ifdef __STDC__
static void verbose(char *);
#else
static void verbose();
#endif

#ifdef __STDC__
void set_sig_trap() ;
#else
void set_sig_trap() ;
#endif

#ifdef __STDC__
int  server_init() ;
#else
int  server_init() ;
#endif

#ifdef __STDC__
void server_shutdown(int) ;
#else
void server_shutdown() ;
#endif

extern Names_Table_Entry Xmit_HostVar_Table[] ;
extern Names_Table_Entry ESA_ExtDir_Table[] ;
extern Names_Table_Entry NASDA_ExtDir_Table[] ;
extern Names_Table_Entry CSA_ExtDir_Table[] ;
extern Names_Table_Entry WALPS_ExtDir_Table[] ;
extern Names_Table_Entry FA_TransferCmd_Table[] ;

char *FAxmitserver_configfile = NULL; /*name of Config file*/
rpc_binding_vector_t *binding_vector; /*set of binding handles(rpcbase.h)*/
unsigned_char_t *Entry_Name; /*CDS entry name*/

extern char *get_config_val() ;
extern void *util_do_malloc() ;


/*==============================================================================
Function:	static void usage(char *progname)

Description:	
        This is the program usage banner that gets executed when
the client program is executed with missing or invalid options.
This banner would like below:
	 
	 Usage:
	    FAxmitserver -c config_file_name -e CDS_Entry_Name

Parameters:
	progname - the name of the executable program

Returns:	None
Creator:	Norbert Piega	
Creation Date:	08/01/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
static void
usage(char *progname)
#else
static void
usage(progname)
   char *progname ;
#endif
{
   printf(" Usage:\n") ;
   printf("    %s [-h] [-v] -c config_file_name -e CDS_Entry_Name\n", 
      progname) ;
   printf("\n") ;
   syslog(LOG_NOTICE, "ERROR, Improper arguements in call to FAxmitserver\n");
} /* usage */
   


/*==============================================================================
Function:       static void verbose (error checking)

Description:
        This function does error checking and gives a print out for each file
type.

Parameters:
       rootpath - the rootpath of the configfile

Returns: None
Creator: Samah Sohrab
Creation Date: 9/11/96
Notes
==============================================================================*/
#ifdef __STDC__
static void verbose(char *rootpath)
#else
static void verbose(rootpath)
    char *rootpath;
#endif
{
   char *configval = NULL ;       /* config value to print (verbose option) */
   int i ;

   printf("FAxmitserver configuration:\n") ;
   printf("   CDS server entry : %s\n", Entry_Name) ;
   printf("   Config file: %s\n", FAxmitserver_configfile) ;
   printf("   Rootpath : %s\n", rootpath) ;

   for (i=0; Xmit_HostVar_Table[i].name_id != SENTINEL; i++)
      if ((configval = (char *)get_config_val(FAxmitserver_configfile,
              Xmit_HostVar_Table[i].name_identifier)) != (char *)NULL)
      {
         printf("   %s : %s\n", 
                Xmit_HostVar_Table[i].name_identifier, configval) ;
         free(configval) ;
      }
      else
         printf("   %s : %s\n", 
                Xmit_HostVar_Table[i].name_identifier,
                Xmit_HostVar_Table[i].default_value) ;

   for (i=0; ESA_ExtDir_Table[i].name_id != SENTINEL; i++)
      if ((configval = (char *)get_config_val(FAxmitserver_configfile,
              ESA_ExtDir_Table[i].name_identifier)) != (char *)NULL)
      {
         printf("   %s : %s\n", 
                ESA_ExtDir_Table[i].name_identifier, configval) ;
         free(configval) ;
      }
      else
         printf("   %s : %s\n", 
                ESA_ExtDir_Table[i].name_identifier,
                ESA_ExtDir_Table[i].default_value) ;

   for (i=0; NASDA_ExtDir_Table[i].name_id != SENTINEL; i++)
      if ((configval = (char *)get_config_val(FAxmitserver_configfile,
              NASDA_ExtDir_Table[i].name_identifier)) != (char *)NULL)
      {
         printf("   %s : %s\n", 
                NASDA_ExtDir_Table[i].name_identifier, configval) ;
         free(configval) ;
      }
      else
         printf("   %s : %s\n", 
                NASDA_ExtDir_Table[i].name_identifier,
                NASDA_ExtDir_Table[i].default_value) ;

   for (i=0; CSA_ExtDir_Table[i].name_id != SENTINEL; i++)
      if ((configval = (char *)get_config_val(FAxmitserver_configfile,
              CSA_ExtDir_Table[i].name_identifier)) != (char *)NULL)
      {
         printf("   %s : %s\n", 
                 CSA_ExtDir_Table[i].name_identifier, configval) ;
         free(configval) ;
      }
      else
         printf("   %s : %s\n", 
                CSA_ExtDir_Table[i].name_identifier,
                CSA_ExtDir_Table[i].default_value) ;

   for (i=0; WALPS_ExtDir_Table[i].name_id != SENTINEL; i++)
      if ((configval = (char *)get_config_val(FAxmitserver_configfile,
              WALPS_ExtDir_Table[i].name_identifier)) != (char *)NULL)
      {
         printf("   %s : %s\n", 
                WALPS_ExtDir_Table[i].name_identifier, configval) ;
         free(configval) ;
      }
      else
         printf("   %s : %s\n", 
                WALPS_ExtDir_Table[i].name_identifier,
                WALPS_ExtDir_Table[i].default_value) ;

   for (i=0; FA_TransferCmd_Table[i].name_id != SENTINEL; i++)
      if ((configval = (char *)get_config_val(FAxmitserver_configfile,
              FA_TransferCmd_Table[i].name_identifier)) != (char *)NULL)
      {
         printf("   %s : %s\n", 
                FA_TransferCmd_Table[i].name_identifier, configval) ;
         free(configval) ;
      }
      else
         printf("   %s : %s\n", 
                FA_TransferCmd_Table[i].name_identifier,
                FA_TransferCmd_Table[i].default_value) ;
}



/*==============================================================================
Function:	main (server initialization driver)

Description:
	This function represents the main driver for the ASF to FA send
transmit server.  The processing of server activation options and the
call to the DCE RPC server initialization function server_init is
included.  The option processing includes checking if a configfile and
the server's CDS entry name were specified in the command line and
determining whether to use default values.

Parameters:
	argc - number of command line arguments
	argv - array of command line argument strings

Returns:	exit status 0 (success) or 1 (error)
Creator:	Samah Sohrab	
Creation Date:	09/11/1996
Notes:		
==============================================================================*/
#ifdef __STDC__
int
main(int argc, char *argv[])
#else
int
main(argc, argv)
  int argc ;
  char *argv[] ;
#endif
{
   extern char *optarg ;
   int opt ;
   char *optlist = "hvc:e:" ;
   unsigned32      status;                                  /* error status */
   char *configpath = NULL ;                    /* part of config file path */
   char *rootpath = NULL ;                                /* FAIF root path */
   LOGICAL verbose_flag = FALSE ;                    /* verbose option flag */

   /* Open system error log
   */
   openlog(argv[0], LOG_PID | LOG_CONS | LOG_NDELAY, SYSLOG_FAIF) ;
   setlogmask(LOG_UPTO(LOG_DEBUG)) ;

   /* Checking to see if first option is the help option
   */
   if((strcmp(argv[1], "-h")) == 0)
   {
      usage(argv[0]) ;
      exit(1) ;
   }

   /* Invalid invocation
   */
   if (argc < 5)
   {
      usage(argv[0]) ;
      exit(1);
   }

   /* If options specified in command line, override previous values
   */
   while ((opt = getopt(argc, argv, optlist)) != EOF)
      switch (opt)
      {
         case 'c' :
            FAxmitserver_configfile = (char *) strdup(optarg) ;
            break ;
         case 'e' :
            Entry_Name = (unsigned_char_t *) strdup(optarg) ;
            break ;
         case 'v' :
	    verbose_flag = TRUE ;
	    break ;
         default :
            usage(argv[0]) ;
            exit(1) ;
      } /* endswitch */

   /* Get rootpath from env. var. If not specified does not work.
   */
   rootpath = getenv(FAIF_ROOTPATH_EV) ;
   if (rootpath == (char *)NULL)
   {
      syslog(LOG_NOTICE, "ERROR, Rootpath not set in environment\n");
      exit(1);
   }

   /* If no config file specified will not work.
   */
   if (FAxmitserver_configfile == (char *)NULL)
   {
       syslog(LOG_NOTICE, "ERROR, No config file specified.\n");
       exit(1);
   }

   /* Check if config file exists and is readable, use it.
   */
   if (access(FAxmitserver_configfile, F_OK) != 0 ||
       access(FAxmitserver_configfile, R_OK) != 0)
   {
      syslog(LOG_ERR, "ERROR, Can't access config file %s. \n\n",
	 FAxmitserver_configfile) ;
      exit(1);
   }

   if (Entry_Name == (unsigned_char_t *)NULL)
   {
      syslog(LOG_NOTICE, "ERROR, CDS entry name not specified\n");
      exit(1);
   } 

   if(verbose_flag == TRUE)
      verbose(rootpath);

   if ((status = server_init()) == 0)
   {
     syslog(LOG_ERR, 
	"ERROR, Unable to complete server initialization. Exiting\n") ;
     free(FAxmitserver_configfile) ;
     free(Entry_Name);
     exit(1);
   }

   free(FAxmitserver_configfile) ;
   free(Entry_Name) ;
   return(0);

} /* main */



/*==============================================================================
Function:	void server_shutdown(int signal)

Description:	
         This function shuts down the server gracefully, making it clear
the bindings, and unexport the CDS entry_name.

Parameters:
         signal - the signal that is sent by the operator to cancel the
program and which is caught by the signal function.

Returns:	An error status from DCE if unable to quit
Creator:	Samah Sohrab
Creation Date:	09/11/1996
Notes:		
==============================================================================*/
#ifdef __STDC__
void server_shutdown(int signal)
#else
void server_shutdown(signal)
       int signal;
#endif
{
   error_status_t        status;                  /* error status (nbase.h) */

   syslog(LOG_NOTICE, "FAxmitserver: signal %d trapped\n", signal);

   rpc_ep_unregister(     /* remove endpoints from local endpoint map    */
      sendtoFA_v1_0_s_ifspec,      /* handle to interface specification  */
      binding_vector,             /* vector of server binding handles    */
      NULL,                       /* no object UUIDs to unregister       */
      &status 
   ) ;
   CHECK_STATUS(status, "Can't remove endpoints from endpoint map:", RESUME) ;

   /* Unexport entry to name service database */

   rpc_ns_binding_unexport(
           rpc_c_ns_syntax_default,
           Entry_Name,
           sendtoFA_v1_0_s_ifspec,
           NULL,
           &status
   );
   CHECK_STATUS(status, "Can't unexport to name service database\n", RESUME);

   rpc_binding_vector_free(               /* free set of binding handles */
      &binding_vector,
      &status
   ) ; 
   CHECK_STATUS(status, "Can't free binding handles and vector\n", ABORT) ;

   syslog(LOG_NOTICE, "Now exiting: FAxmitserver\n");
   exit(0);
   
} /*server_shutdown*/


/*==============================================================================
Function:	void set_sig_trap()

Description:	
        This function is called by the server to allow it to catch certain
signals sent to it by ther operator, and act accordingly. It calls server
shutdown, which shuts down the server.

Parameters:     None

Calls:          server_shutdown

Returns:	None
Creator:	Samah Sohrab
Creation Date:	09/11/1996
Notes:		
==============================================================================*/
#ifdef __STDC__
void set_sig_trap()
#else
void set_sig_trap()
#endif
{
   signal(SIGHUP, server_shutdown); /*kill -1 (hangup)*/
   signal(SIGINT, server_shutdown); /*kill -2 (interrupt)*/
   signal(SIGQUIT, server_shutdown); /*kill -3 (quit)*/
   signal(SIGTERM, server_shutdown); /*kill -15 (software termination)*/
}



/*==============================================================================
Function:	int server_init() ;

Description:
	This module represents the DCE RPC server initialization
procedures.  The initialization procedures include the registration of
DCE client/server interfaces, specification of binding information,
etc. These procedures are performed by the RPC runtime library
functions called.  The macro CHECK_STATUS  which prints through syslog
the appropriate message for a DCE errror status is called as well.  

Parameters:     None

Calls:          set_sig_trap

Returns:	status returned from RPC runtime library calls
Creator:	Samah Sohrab
Creation Date:	08/17/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int server_init()
#else
int server_init()
#endif
{
   unsigned32 status;                  /* error status (nbase.h) */

   set_sig_trap();

   /**************************************************************************
                       DCE server initialization 
   **************************************************************************/

   rpc_server_register_if(       /* register interface with the RPC runtime */
      sendtoFA_v1_0_s_ifspec,       /* interface specification (sendtoFA.h) */
      NULL, NULL,                     
      &status) ;                                            /* error status */
   CHECK_STATUS(status, "Can't register interface\n", ABORT) ;

   rpc_server_use_protseq(               /* create binding information */
      (unsigned_char_t *)("ncacn_ip_tcp"),
      rpc_c_protseq_max_reqs_default,   /* queue size for calls (rpcbase.h) */
      &status) ;
   CHECK_STATUS(status, "Can't establish protocol sequences\n", ABORT) ;

   rpc_server_inq_bindings(     /* obtain this server's binding information */
      &binding_vector,
      &status) ; 
   CHECK_STATUS(status, "Can't get binding information\n", ABORT) ;

   rpc_ep_register(             /* register endpoints in local endpoint map */
      sendtoFA_v1_0_s_ifspec,       /* interface specification (sendtoFA.h) */
      binding_vector,             /* the set of server binding handles      */
      NULL, NULL, 
      &status ) ;
   CHECK_STATUS(status, "Can't add address to the endpoint map\n", ABORT) ;

   rpc_ns_binding_export(         /* export entry to name service database  */
      rpc_c_ns_syntax_default,     /* syntax of the entry name  (rpcbase.h) */
      (unsigned_char_t *)Entry_Name,     /* CDS entry name for name service */
      sendtoFA_v1_0_s_ifspec,       /* interface specification (sendtoFA.h) */
      binding_vector,              /* the set of server binding handles     */
      NULL,
      &status ) ;
   CHECK_STATUS(status, "Can't export to name service database\n", ABORT) ;
   
   syslog(LOG_NOTICE, 
      "Server entry %s listening for remote procedure calls...",
       Entry_Name) ;

   rpc_server_listen(                   /* listen for remote calls       */
      rpc_c_listen_max_calls_default,   /* number of threads             */
   			            /*concurrent calls to server (rpcbase.h)*/
      &status
   ) ;
   CHECK_STATUS(status, "RPC listen failed:", RESUME) ;

   if(status < 0)
      status = 0;
   return(status) ;

} /* server_init */

/* End of file */
