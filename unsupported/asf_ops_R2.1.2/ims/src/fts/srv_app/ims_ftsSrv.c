static char *sccs = "@(#)ims_ftsSrv.c	6.2 03/26/98";
/******************************************************************************
**
** File:        ims_ftsSrv.c
**
** Function:    File transfer server program.
**
** Author:      Hoshyar Sayah
**
** Date:        9/20/90
**
** Modified:    2/2/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Removed all references to srvprocAttn because of its non
**              multi-threaded nature. Removed the include files strings.h and
**              malloc.h. Added the include files stdlib.h and sys/unistd.h.
**              Changed the datatype of ftSrvPid from int to pid_t.
**              Replaced a call to getwd() with getcwd().
**              Replaced the use of sys_errlist[] with strerror().
**              Modified the getArgs() function to remove dbauth functionality.
**              Removed the include file ims_authrec.h. Replaced a call to
**              gethostname() with uname().
**
**              8/17/94 - H. Sayah, S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-IMS.
**
**              12/9/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              12/20/94 - S. Hardman - R1B
**              Modified rpc_stop() to disconnect client threads and
**              then wait for their completion before stopping the server.
**              Also added rpc_who() which will display a list of active
**              client threads. Cleaned up exit_remote().
**       
**              02/23/95 - D. Crichton - R1B
**              Added an account id parameter to the add, delete, replace, 
**              get events.
**
**              4/4/95 - D. Crichton - R1B
**              Process new RPC for checking file types.
**
**              11/10/95 - D. Crichton - R1B'
**              Remove opening of syslog file and issolate to each 
**              individual thread.  
**
**				10/2/96 - D. Crichton - R2.1
**				Change transfer mechanism to use bulk copy. The send
**				file fragment is still supported as well.  
**				
** Notes:       This is the server program for File Transfer System.  It
**              accepts client requests to add/get/delete and replace files
**              from the storage area based on policy information loaded
**              in the database.  
**
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <syberror.h>
#include <ospublic.h>
#include <oserror.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_hash.h>
#include <ims_archive.h>
#include <ims_ftsSrvCat.h>
#include <ims_ftsSrv.h>
#include <ims_auxCat.h>
#include <ims_version.h>
#include <ims_util.h>
#include <ims_signal.h>
#include <ims_keyword.h>

#include <IK_Syslog.h>

/*
** Local Functions
*/
static int srvInit (IMS_MSG_STRUCT *);
static int checkServerHost (IMS_MSG_STRUCT *);
static int cachePolicy (IMS_MSG_STRUCT *);
/* static int addProcessInfo (IMS_MSG_STRUCT *); */
/* static int addProcessEndTime (IMS_MSG_STRUCT *); */
static int init_server (SRV_SERVER *);
static int init_remote (SRV_PROC *);
static int exit_remote (SRV_PROC *);
static int rpc_execute (SRV_PROC *);
static CS_INT chk_err (SRV_SERVER *, SRV_PROC *, CS_INT, CS_BYTE, CS_BYTE,
	CS_INT, CS_CHAR *, CS_INT, CS_CHAR *, CS_INT);
static int remotemsgs (DBPROCESS *, DBINT, int, int, char *, char *,
	char *, int);
static int remoteerr (DBPROCESS *, int, int, int, char *, char *);
static int getArgs (int, char *[], IMS_MSG_STRUCT *);
static int attn_handler (SRV_PROC *);
static int checkDbLogin (IMS_MSG_STRUCT *);
static int checkDatasetPolicy (SRV_PROC *);
static int addSrvProcToGlbList (SRV_PROC *);
static int removeSrvProcFromGlbList (SRV_PROC *);
static void termHandler (int);
static void pipeHandler (int);
static void childHandler (int);
static int sybToCdbSeverity (int);
static CS_RETCODE ims_bulkHandler(SRV_PROC *srvproc);
static CS_RETCODE ims_langHandler(SRV_PROC *spp);

/* static int validateKrbTicket (IMS_SERVER_KRB *); */

/*
** Local RPC Functions
*/
static int rpc_list (SRV_PROC *);
static int rpc_who (SRV_PROC *);
static int rpc_stop (SRV_PROC *);
static int rpc_validateLogin (SRV_PROC *);
static int rpc_getEventBegin (SRV_PROC *);
static int rpc_getEventEnd (SRV_PROC *);
static int rpc_addEventBegin (SRV_PROC *);
static int rpc_addEventEnd (SRV_PROC *);
static int rpc_replaceEventBegin (SRV_PROC *);
static int rpc_replaceEventEnd (SRV_PROC *);
static int rpc_deleteEvent (SRV_PROC *);
static int rpc_openFile (SRV_PROC *);
static int rpc_fileFragment (SRV_PROC *);
static int rpc_closeFile (SRV_PROC *);
static int rpc_transferFileName (SRV_PROC *);
static int rpc_transferFile (SRV_PROC *);
static int rpc_validateMetadata (SRV_PROC *);
static int rpc_checkFileTypes (SRV_PROC *);
static int rpc_getVersionNumber (SRV_PROC *);

/* static int rpc_krbFragment (SRV_PROC *); */

#define IMS_MIN(x, y) ((x < y) ? x : y)

/*
** FOR DEBUG ONLY.
*/

/* #define IMS_DEBUG */


/* #define MALLOCMAPCHECK */

/*
** Defines.
*/
#define SERVICE "sfoc_ims"
#define MAXCONNECTIONS "10"
#define DEFAULT_TIMEOUT "60"

/*
** Global variable definitions
*/
static int maxConn;                    /* Connection max count */
static int stackSize;                  /* thread stack size. */
static int timeout;                    /* Login timeout period to SQL server. */
static char logFileName[IMS_COL60_LEN+1]; /* default log file. */
static char logFileDir[IMS_COL255_LEN+1]; /* default log file dir. */
static char logFileFullPath[IMS_COL255_LEN+IMS_COL60_LEN+1];
static SRV_PROC *newSrvProc = (SRV_PROC *)NULL; /* Latest SRV_PROC allocated. */
static SRV_OBJID glbAuxMutex;    /* auxliary mutex structure. */
static SRV_OBJID glbThreadMutex; /* thread mutex structure. */
static CS_CONTEXT *glbContext = (CS_CONTEXT *) NULL; /* Global Context. */

/*
** Global platform/sensor policy. Points to the start of list.
*/
static FTS_SENSOR_POLICY *glb_sensorPolicy = (FTS_SENSOR_POLICY *) NULL;

/*
** Global pointer to readFile hash structure.
*/
static IMS_HASH_STRUCT *glb_readHashPtr = (IMS_HASH_STRUCT *) NULL;

/*
** Global pointer to client srvproc connections.
*/
static FTS_SRVPROC_LIST *glb_srvProcList = (FTS_SRVPROC_LIST *) NULL;

/*
** Global pointer to status description table.
*/
static FTS_STATUS_TABLE *glb_statusTable = (FTS_STATUS_TABLE *) NULL;

/*
** Global pointer to rpc hash structure.
*/
static IMS_HASH_STRUCT *glb_rpcHashPtr = (IMS_HASH_STRUCT *) NULL;

/*
** The following structure contains the information about RPC's.  It includes
** the RPC name, usage name, and pointer to a function to handle the RPC.
*/
typedef struct rpc
{
	CS_CHAR rpcName[SRV_MAXNAME+1];
	CS_CHAR rpcUsage[128];
	int (*rpcHandler)(SRV_PROC *);
} RPC;

/*
** The list of all RPC's handled by the server.
*/
static RPC Rpcs[] = 
{
	{"list", "list", rpc_list},
	{"who", "who", rpc_who},
	{"stop", "stop", rpc_stop},
	{"validateLogin", "validateLogin", rpc_validateLogin},
	{"getEventBegin", 
		"getEventBegin @platform, @sensor, @dataset, \
		@name, @format, @version, @requestType, @accountId", 
		rpc_getEventBegin},
	{"getEventEnd", "getEventEnd", rpc_getEventEnd},
	{"addEventBegin", 
		"addEventBegin @platform, @sensor, @dataset, \
		@name, @format, @accountId",
		rpc_addEventBegin},
	{"addEventEnd", "addEventEnd", rpc_addEventEnd},
	{"replaceEventBegin", 
		"replaceEventBegin @platform, @sensor, @dataset, \
		@name, @format, @version, @accountId", 
		rpc_replaceEventBegin},
	{"replaceEventEnd", "replaceEventEnd", rpc_replaceEventEnd},
	{"deleteEvent", 
		"deleteEvent @platform, @sensor, @dataset, \
		@name, @version, @accountId",
		rpc_deleteEvent},
	{"openFile",
		"openFile @name, @extension",
		rpc_openFile},
	{"fileFragment",
		"fileFragment @byteCount, @index, @buffer", rpc_fileFragment},
	{"closeFile",
		"closeFile @name, @extension",
		rpc_closeFile},
	{"transferFileName", "transferFileName", rpc_transferFileName},
	{"checkFileTypes", "checkFileTypes", rpc_checkFileTypes},
	{"transferFile", "transferFile", rpc_transferFile},
	{"validateMetadata", "validateMetadata", rpc_validateMetadata},
	{"getVersionNumber", "getVersionNumber", rpc_getVersionNumber}
/*
**	{"krbFragment", 
**		"krbFragment @byteCount, @index, @buffer", 
**		rpc_krbFragment},
*/
};

#define rpcCount  sizeof(Rpcs) / sizeof(RPC)

/*
** Command line pointers 
*/
static struct cmdLine 
{
	char *login;
	char *password;
	char *ftsSrvName;
	char *logFile;
	char *logFileDir;
	char *maxConn;
	char *stackSize;
	char *recordMsgs;
	char *timeout;
	char *commandFile;
	char *catSrvName;
	char *catDbName;
	/*
	** TRACE FLAGS
	*/
	char *attn;
	char *defqueue;
	char *event;
	char *msgq;
	char *netdriver;
	char *netreq;
	char *netwake;
	char *tdsdata;
	char *tdshdr;
} cmdLine;

/*
** Command line arguments
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",           &cmdLine.login},
	{"+username",    &cmdLine.login},
	{"-P",           &cmdLine.password},
	{"+password",    &cmdLine.password},
	{"-N",           &cmdLine.ftsSrvName},
	{"+name",        &cmdLine.ftsSrvName},
	{"-L",           &cmdLine.logFile},
	{"+logFile",     &cmdLine.logFile},
	{"-D",           &cmdLine.logFileDir},
	{"+directory",   &cmdLine.logFileDir},
	{"-M",           &cmdLine.maxConn},
	{"+maxConn",     &cmdLine.maxConn},
	{"-S",           &cmdLine.stackSize},
	{"+stackSize",   &cmdLine.stackSize},
	{"-R",           &cmdLine.recordMsgs},
	{"+recordMsgs",  &cmdLine.recordMsgs},
	{"-T",           &cmdLine.timeout},
	{"+timeout",     &cmdLine.timeout},
	{"-C",           &cmdLine.commandFile},
	{"+commandFile", &cmdLine.commandFile},
	{"-X",           &cmdLine.catSrvName},
	{"+catServer",   &cmdLine.catSrvName},
	{"-Y",           &cmdLine.catDbName},
	{"+catDatabase", &cmdLine.catDbName},
	/*
	** TRACE FLAGS
	*/
	{"+attn",        &cmdLine.attn},
	{"+defqueue",    &cmdLine.defqueue},
	{"+event",       &cmdLine.event},
	{"+msgq",        &cmdLine.msgq},
	{"+netdriver",   &cmdLine.netdriver},
	{"+netreq",      &cmdLine.netreq},
	{"+netwake",     &cmdLine.netwake},
	{"+tdsdata",     &cmdLine.tdsdata},
	{"+tdshdr",      &cmdLine.tdshdr},
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username", &cmdLine.login},
	{"password", &cmdLine.password},
	{"name", &cmdLine.ftsSrvName},
	{"logFile", &cmdLine.logFile},
	{"directory", &cmdLine.logFileDir},
	{"maxConn", &cmdLine.maxConn},
	{"timeout", &cmdLine.timeout},
	{"stackSize", &cmdLine.stackSize},
	{"recordMsgs", &cmdLine.recordMsgs},
	{"catServer", &cmdLine.catSrvName},
	{"catDatabase", &cmdLine.catDbName},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global variables for holding all of the important information regarding
** server logins.
*/
static char *glb_ftsUsername;
static char *glb_ftsPassword;
static char *glb_catSrvName;
static char *glb_catDbName;
static char *glb_ftsSrvName;
static char *glb_ftsProgramName;
static char glb_ftsHostName[IMS_HOST_LEN+1];
static pid_t glb_ftsPid;

static char Msg[CS_MAX_MSG + 256];

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	CS_CONTEXT *context;        /* Context structure */
	SRV_SERVER *server;         /* Open server service process */
	IMS_MSG_STRUCT *msgDesc;    /* Message facility structure */
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	struct utsname uname_info;	/* Structure for uname() */
	char srvVersion[1024];      /* Used to store the version info. */

#ifdef MALLOCMAPCHECK
	malloc_debug (2);
	if (0)
	{
		malloc_verify ();
		mallocmap ();
	}
#endif	/* MALLOCMAPCHECK */

	/*
	** Get hostname, pid and program name.
	*/
	glb_ftsPid = getpid ();
	(void) uname (&uname_info);
	(void) strncpy (glb_ftsHostName, uname_info.nodename,
		IMS_HOST_LEN);

	glb_ftsHostName[IMS_HOST_LEN] = '\0';

	glb_ftsProgramName = ims_extractFileName (argv[0]);

	/*
	** Allocate message facility structure.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"IMS_MSG_STRUCT structure allocation error.");
		exit (ERREXIT);
	}

	/*
	** Initialize the message facility options.
	*/
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_ftsProgramName);
	(void) sprintf (banner, "%s::%s", glb_ftsHostName, glb_ftsProgramName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/*
	** Read cmdLine arguments. 
	*/
	if (getArgs (argc, argv, msgDesc) < IMS_OK)
	{
		exit (ERREXIT);
	}

	/*
	** Create full path names for the server error log and the
	** IK syslog.
	*/
	(void) sprintf (logFileFullPath, "%s/%s", 
		cmdLine.logFileDir, cmdLine.logFile);

	/*
	** We are ready to initialize the server.
	*/
	(void) fprintf (stderr,
		"\nFTS server initialization in progress.  Please wait...\n\n");

	/*
	** Allocate a CS-Library context structure to define the
	** default localization information. Open Server will also
	** store global state information in this structure during
	** initialization.
	*/
	if (cs_ctx_alloc (CS_VERSION_100, &context) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the CS-Library context structure.");
		exit (ERREXIT);
	}
	glbContext = context;

	/*
	** Perform initialization before invoking server.
	*/
	if (srvInit (msgDesc) < IMS_OK)
	{
		goto SERVER_ERROR;
	}

	/*
	** Set the Open Server version and context information.
	*/
	if (srv_version (context, CS_VERSION_100) != CS_SUCCEED)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not set server version and context information.");
		goto SERVER_ERROR;
	}

	/*
	** Install server error handler.
	*/
	if (srv_props (context, CS_SET, SRV_S_ERRHANDLE, 
		(CS_VOID *) chk_err, (CS_INT) sizeof (CS_VOID *),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Setting server configuration 'SRV_S_ERRHANDLE' failed.");
		goto SERVER_ERROR;
	}

	/*
	** Set max server connection as specified in the cmdLine.
	*/
	if (srv_props (context, CS_SET, SRV_S_NUMCONNECTIONS, 
		(CS_VOID *) &maxConn, (CS_INT) sizeof (maxConn),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Setting server configuration 'SRV_S_NUMCONNECTIONS' failed.");
		goto SERVER_ERROR;
	}

	/*
	** Set maximum number of DBPROCESSes to maxConn.
	*/
	if (maxConn > dbgetmaxprocs ())
	{
		if (dbsetmaxprocs (maxConn) == CS_FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Setting maximum number of DBPROCESSes to '%d' failed.",
				maxConn);
			goto SERVER_ERROR;
		}
	}

	/*
	** Set stack size
	*/
	if (cmdLine.stackSize != (char *) NULL)
	{
		if (srv_props (context, CS_SET, SRV_S_STACKSIZE,
			(CS_VOID *) &stackSize, (CS_INT) sizeof (stackSize),
			(CS_INT *) NULL) == CS_FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Setting server configuration 'SRV_S_STACKSIZE' failed.");
			goto SERVER_ERROR;
		}
	}

	/*
	** Set SQL Server timeout value in seconds. 
	*/
	if (cmdLine.timeout != (char *)NULL)
	{
		if (dbsetlogintime (timeout) == CS_FAIL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Setting login timeout value to '%d' failed.",
				timeout);
			goto SERVER_ERROR;
		}
	}

	/*
	** Set the log files as specified in the cmdLine.
	*/
	if (srv_props (context, CS_SET, SRV_S_LOGFILE,
		(CS_VOID *) logFileFullPath, (CS_INT) strlen (logFileFullPath),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Setting server configuration 'SRV_LOGFILE' failed.");
		goto SERVER_ERROR;
	}

	/*
	** Initialize the File Transfer Server.
	*/
	if ((server = srv_init ((SRV_CONFIG *) NULL, glb_ftsSrvName,
		CS_NULLTERM)) == (SRV_SERVER *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Could not initialize File Transfer Open Server.");
		goto SERVER_ERROR;
	}

	/*
	** Check the server node against the catalog policy.
	** Server must be started only on the specified node in the catalog 
	** policy.
	*/
	if ((status = checkServerHost (msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Checking FTS server host name failed.");
		goto SERVER_ERROR;
	}

	/*
	** Print the startup message in the server log file and to the screen.
	*/
	(void) sprintf (Msg,
		"\n\n>>>>>>>>>>>>>>>>>>>>  Server Startup  <<<<<<<<<<<<<<<<<<<<\n\n");
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(void) sprintf (Msg,
		"Started IMS File Transfer Server '%s'.\n",
		glb_ftsSrvName);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

	(void) sprintf (Msg,
		"%s %s Version %s %s\n\n%s\n%s\n\n",
		IMS_PROJECT, IMS_GROUP, IMS_VERSION, IMS_VERSION_DATE,
		COPYRIGHT1, COPYRIGHT2);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

	/*
	** Load catalog policies into local cache memory.  This will reduce
	** the number of catalog accesses during file transfer operations.
	** All policy acceses will be done through the cache memory.
	*/
	if ((status = cachePolicy (msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Loading dataset policies into cache memory failed.");
		goto SERVER_ERROR;
	}

	/*
	** Install the SRV_START event handler.  All other event handlers 
	** are defined in the SRV_START hander, init_server().
	*/
	(CS_VOID) srv_handle ((SRV_SERVER *) NULL, SRV_START,
		(INTFUNCPTR) init_server);


	/* MOVED to init_server handler routine 
	**
	** Install a handler for bulk transfers of data.
	**

	(void) srv_handle ((SRV_SERVER *) NULL, SRV_BULK,
				ims_bulkHandler);

	**
	** Install a language handler for bulk transfers.  This is 
	** required in order to use the bulk transfer event.
	**

	(void) srv_handle ((SRV_SERVER *) NULL, SRV_LANGUAGE,
				ims_langHandler);
	*/
					 

#ifdef PROCESS
	/*
	** Log server startup information in process table.
	*/
	if (addProcessInfo (msgDesc) < IMS_OK)
	{
		(void) sprintf (Msg, 
		 "Adding process information to processes table failed.\n");

		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
		goto SERVER_ERROR;
	}
#endif	/* PROCESS */

	/*
	** Print server settings and Sybase copyright to the screen
	** and to the log file.
	*/
	(void) sprintf (Msg,
		"\nMaximum client connections: %d\n", maxConn);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(void) sprintf (Msg,
		"SQL Server login timeout (Seconds): %d\n", timeout);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(void) sprintf (Msg,
		"Client stack size (Bytes): %d\n", stackSize);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(void) sprintf (Msg,
		"Log File: %s\n", logFileFullPath);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(CS_VOID) srv_props (context, CS_GET, SRV_S_VERSION, 
		(CS_VOID *) srvVersion, (CS_INT) sizeof (srvVersion),
		(CS_INT *) NULL);

	(void) sprintf (Msg, "\n%s\n", srvVersion);
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(void) sprintf (Msg, "\n%s\n", dbversion ());
	(void) fprintf (stderr, Msg);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg, CS_NULLTERM);

	(void) fflush (stderr);

	/*
	** Shutdown the message facility for the server startup portion.
	*/
	(void) ims_msgStructFree (msgDesc);
	msgDesc = (IMS_MSG_STRUCT *) NULL;

	/*
	** Start the File Transfer Server.
	*/
	if (srv_run ((SRV_SERVER *) NULL) == CS_FAIL)
	{
		(void) fprintf (stderr,
			"Could not run server.");
		goto SERVER_ERROR;
	}

	exit (STDEXIT);


	SERVER_ERROR:

	/*
	** Release all allocated control structures and exit.
	*/
	if (server != (SRV_SERVER *) NULL)
	{
		(CS_VOID) srv_free (server);
	}
	(CS_VOID) cs_ctx_drop (context);

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);

	exit (ERREXIT);
}

/******************************************************************************
**
** srvInit ()
**
** Initialize the server based on the cmdLine structure.
**
******************************************************************************/

static int srvInit (
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashPtr;
	int i;

#ifdef IMS_DEBUG
	/*
	** Make sure we can login to the database as the surrogate.
	*/
	if (checkDbLogin (msgDesc) < IMS_OK)
	{
		return (ims_msgGetSeverity (msgDesc));
	}
#endif	/* IMS_DEBUG */

	/*
	** Initialize server attn structure and max connection count.
	*/
	if (cmdLine.maxConn != (char *) NULL)
	{
		if (!ims_isInteger (cmdLine.maxConn))
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"maxConn parameter value is not a valid integer.");
			return (IMS_FATAL);
		}
		if (((maxConn = atoi (cmdLine.maxConn)) > SRV_MAX_CONNECTIONS)
			|| (maxConn < 1))
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"maxConn parameter value cannot be > '%d' or < '%d'.",
				SRV_MAX_CONNECTIONS, 1);
			return (IMS_FATAL);
		}
	}
	else
	{
		cmdLine.maxConn = MAXCONNECTIONS;
		maxConn = atoi (MAXCONNECTIONS);
	}

	if (cmdLine.stackSize != (char *) NULL)
	{
		if (!ims_isInteger (cmdLine.stackSize))
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"stackSize parameter value is not a valid integer.");
			return (IMS_FATAL);
		}
		if ((stackSize = atoi (cmdLine.stackSize)) < SRV_DEF_STACKSIZE)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"stackSize parameter value cannot be < '%d'.",
				SRV_DEF_STACKSIZE);
			return (IMS_FATAL);
		}
	}
	else
	{
		stackSize = SRV_DEF_STACKSIZE;
	}

	if (cmdLine.timeout != (char *) NULL)
	{
		if (!ims_isInteger (cmdLine.timeout))
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Login timeout parameter value is not a valid integer.");
			return (IMS_FATAL);
		}
		if (((timeout = atoi (cmdLine.timeout)) > 900) ||
			(timeout < 30))
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Timeout parameter value cannot be > '900' Seconds or < '30' Seconds.");
			return (IMS_FATAL);
		}
	}
	else
	{
		cmdLine.timeout = DEFAULT_TIMEOUT;
		timeout = atoi (cmdLine.timeout);
	}
		
	/*
	** Now set the trace flags in the server process structure.
	*/

	/*
	** Display attention signals from the client.
	*/
	if (cmdLine.attn != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_ATTN, (CS_INT) sizeof (SRV_TR_ATTN),
			(CS_INT *) NULL);
	}

	/*
	** Trace event queue activity.
	*/
	if (cmdLine.defqueue != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_DEFQUEUE, (CS_INT) sizeof (SRV_TR_DEFQUEUE),
			(CS_INT *) NULL);
	}

	/*
	** Display information about events.
	*/
	if (cmdLine.event != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_EVENT, (CS_INT) sizeof (SRV_TR_EVENT),
			(CS_INT *) NULL);
	}

	/*
	** Trace the message queue activity.
	*/
	if (cmdLine.msgq != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_MSGQ, (CS_INT) sizeof (SRV_TR_MSGQ),
			(CS_INT *) NULL);
	}

	/*
	** Trace TCL Net-Lib driver requests.
	*/
	if (cmdLine.netdriver != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_NETDRIVER, (CS_INT) sizeof (SRV_TR_NETDRIVER),
			(CS_INT *) NULL);
	}

	/*
	** Trace TCL requests.
	*/
	if (cmdLine.netreq != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_NETREQ, (CS_INT) sizeof (SRV_TR_NETREQ),
			(CS_INT *) NULL);
	}

	/*
	** Trace TCL wakeup request.
	*/
	if (cmdLine.netwake != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_NETWAKE, (CS_INT) sizeof (SRV_TR_NETWAKE),
			(CS_INT *) NULL);
	}

	/*
	** TDS traffic display between client/ftSrv in ASCII.
	*/
	if (cmdLine.tdsdata != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_TDSDATA, (CS_INT) sizeof (SRV_TR_TDSDATA),
			(CS_INT *) NULL);
	}

	/*
	** TDS packet header information trace.
	*/
	if (cmdLine.tdshdr != (char *) NULL)
	{
		(CS_VOID) srv_props (glbContext, CS_SET, SRV_S_TRACEFLAG, 
			(CS_VOID *) SRV_TR_TDSHDR, (CS_INT) sizeof (SRV_TR_TDSHDR),
			(CS_INT *) NULL);
	}

	/*
	** Setup the readFile hash table structure.  This hash table is empty
	** at startup time.  Entries into this table is inserted and extracted
	** when file read operations are initiated and terminated.
	*/
	if ((glb_readHashPtr = ims_hashCreate (maxConn, IMS_INTEGER, msgDesc)) ==
		(IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, ims_msgGetSeverity (msgDesc),
			"Creating readFile hash table structure failed.");
		return (ims_msgGetSeverity (msgDesc));
	}

	/*
	** Setup rpc hash table structure.
	** This hash structure provides fast search for rpc names
	** in the rpc_execute routine.
	*/
	if ((glb_rpcHashPtr = ims_hashCreate (rpcCount + 3, IMS_STRING, msgDesc))
		== (IMS_HASH_STRUCT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Creating rpc hash table structure failed.");
		return (IMS_FATAL);
	}

	/*
	** Insert rpc names into the hash structure.
	*/
	for (i=0; i < rpcCount; i++)
	{
		item.key = (void *) Rpcs[i].rpcName;
		item.data = (void *) &Rpcs[i];

		/*
		** Place the item in the hash structure.
		*/
		if ((hashPtr = ims_hashSearch (glb_rpcHashPtr, &item, IMS_ENTER,
			msgDesc)) == (IMS_HASH_ENTRY *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Inserting rpc name in hash structure failed.");
			return (IMS_FATAL);
		}
	}
	return (IMS_OK);
}

/******************************************************************************
**
** checkServerHost ()
** 
** Check the server host name by matching it against the catalog policy.
** This function uses the static server variables for its input.
**
******************************************************************************/

static int checkServerHost (
	IMS_MSG_STRUCT *msgDesc)
{
	AUX_CAT_REQUEST auxReq;
	AUX_INFO_SPEC auxSpec;
	int status;

	auxSpec.msgDesc = msgDesc;

	/*
	** Setup the auxReq structure for database login and 
	** query through ims_auxCat routine.
	*/
	(void) strcpy (auxSpec.username, glb_ftsUsername);
	(void) strcpy (auxSpec.password, glb_ftsPassword);
	(void) strcpy (auxSpec.ftsSrvName, glb_ftsSrvName);
	(void) strcpy (auxSpec.programName, glb_ftsProgramName);
	(void) strcpy (auxSpec.catSrvName, glb_catSrvName);
	(void) strcpy (auxSpec.catDbName, glb_catDbName);
	auxReq.auxSpec = &auxSpec;

	/*
	** Make a connection to the database.
	*/
	if (ims_auxCat (&auxReq, AUX_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Opening the catalog database connection failed.");
		return (IMS_FATAL);
	}
	
	/*
	** Get the host name for this FTS server.
	*/
	if ((status = ims_auxCat (&auxReq, AUX_GET_SERVER_HOST)) < IMS_OK)
	{
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		return (status);
	}

	if (strcmp (auxSpec.ftsHostName, glb_ftsHostName) != 0)
	{
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"FTS server '%s' cannot be started on host '%s'. Check catalog policy.",
			glb_ftsSrvName, glb_ftsHostName);
		return (IMS_FATAL);
	}

	if (ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Closing the catalog database connection failed.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** cachePolicy ()
** 
** Load needed catalog policies into local cache memeory for on-line use.
** This function uses the static server variables for its input.
**
******************************************************************************/

static int cachePolicy (
	IMS_MSG_STRUCT *msgDesc)
{
	AUX_CAT_REQUEST auxReq;
	AUX_INFO_SPEC auxSpec;
	FTS_SENSOR_POLICY *currSensor;
	FTS_DATASET_POLICY *currDataset;
	FTS_DATASET_POLICY *tempDataset;
	FTS_FILE_POLICY *tempFilePolicy;
	int currDatasetLen;
	IMS_HASH_ENTRY item;
	int status;
	FTS_KEYWORD_POLICY *keywordPolicy;

	auxSpec.msgDesc = msgDesc;

	/*
	** Setup the auxReq structure for database login and 
	** query through ims_auxCat routine.
	*/
	(void) strcpy (auxSpec.username, glb_ftsUsername);
	(void) strcpy (auxSpec.password, glb_ftsPassword);
	(void) strcpy (auxSpec.ftsSrvName, glb_ftsSrvName);
	(void) strcpy (auxSpec.programName, glb_ftsProgramName);
	(void) strcpy (auxSpec.catSrvName, glb_catSrvName);
	(void) strcpy (auxSpec.catDbName, glb_catDbName);
	auxReq.auxSpec = &auxSpec;

	/*
	** Make a connection to the database.
	*/
	if (ims_auxCat (&auxReq, AUX_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Opening the catalog database connection failed.");
		return (IMS_FATAL);
	}
	
	/*
	** Get the status description table.
	*/
	if ((status = ims_auxCat (&auxReq, AUX_GET_STATUS_TABLE)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not populate the status table.");
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		return (IMS_FATAL);
	}
	glb_statusTable = (FTS_STATUS_TABLE *) auxReq.itemList1;

	/*
	** Get the list of platforms/sensors for this server.
	*/
	if ((status = ims_auxCat (&auxReq, AUX_GET_SENSOR_POLICY)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not get the platform/sensor policy list.");
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		return (IMS_FATAL);
	}

	glb_sensorPolicy = (FTS_SENSOR_POLICY *) auxReq.itemList1;
	currSensor = glb_sensorPolicy;
	currDataset = tempDataset = (FTS_DATASET_POLICY *) NULL;
	currDatasetLen = 0;

	/*
	** Now that we have the sensor list, then we must get the dataset
	** information from the database and form a hash table on the policies
	** for every platform/sensor in the sensor list.
	*/
	while (currSensor != (FTS_SENSOR_POLICY *)NULL)
	{
		(void) strcpy (auxSpec.platform, currSensor->platform);
		(void) strcpy (auxSpec.sensor, currSensor->sensor);

		auxReq.itemList2 = (void *) &currDatasetLen;

		if ((status = ims_auxCat (&auxReq, AUX_GET_DATASET_POLICY)) <
			IMS_OK)
		{
			(void) ims_msg (msgDesc, status, 
				"Could not get the dataset policy list for platform '%s' and sensor '%s'.",
				auxSpec.platform,
				auxSpec.sensor);
			(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
			return (IMS_FATAL);
		}

		currDataset = tempDataset =
			(FTS_DATASET_POLICY *) auxReq.itemList1;
		tempFilePolicy = (FTS_FILE_POLICY *) NULL;

		/*
		** Go through the policies returned for this platform and
		** sensor and extract the file policies from the catalog.
		*/
		while (tempDataset != (FTS_DATASET_POLICY *) NULL)
		{
			/*
			** Get the current dataset information.
			*/
			auxSpec.dataset_idx = tempDataset->dataset_idx;
			(void) strcpy (auxSpec.dataset, tempDataset->dataset);
			
			if ((status = ims_auxCat (&auxReq,
				AUX_GET_FILE_POLICY)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, status,
					"Could not get the file policy list for dataset '%s'.",
					auxSpec.dataset);
				(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
				return (IMS_FATAL);
			}

			tempDataset->filePolicy = tempFilePolicy =
				(FTS_FILE_POLICY *) auxReq.itemList1;

			/*
			** Now get the format specific information.
			*/
			while (tempFilePolicy != (FTS_FILE_POLICY *) NULL)
			{
				auxReq.itemList2 = (void *) &tempFilePolicy->fileCount;
				(void) strcpy (auxSpec.format, tempFilePolicy->format);

				if ((status = ims_auxCat (&auxReq,
					AUX_GET_FORMAT_POLICY)) < IMS_OK)
				{
					(void) ims_msg (msgDesc, status,
						"Could not get the format policy list for dataset '%s' and format '%s'.",
						auxSpec.dataset,
						auxSpec.format);
					(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
					return (IMS_FATAL);
				}

				tempFilePolicy->formatPolicy =
					(FTS_FORMAT_POLICY *) auxReq.itemList1;
				tempFilePolicy = tempFilePolicy->next;
			}

			tempDataset = tempDataset->next;
		}



		/*
		** Go through the policies returned for this platform and
		** sensor and extract the path policies from the catalog.
		*/
		tempDataset = currDataset;

		while (tempDataset != (FTS_DATASET_POLICY *) NULL)
		{
			/* 
			** Get the current dataset information.
			*/
			auxSpec.dataset_idx = tempDataset->dataset_idx;
			(void) strcpy (auxSpec.dataset, tempDataset->dataset);

		 	if ((status = ims_auxCat (&auxReq,
		 		AUX_GET_PATH_POLICY)) < IMS_OK)
		 	{
		 		(void) ims_msg (msgDesc, status, 
		 			"Could not get the path policy list for dataset '%s'.",
		 			auxSpec.dataset);
		 		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		 		return (IMS_FATAL);
		 	}

			/* Set pointer in dataset struct to front of list returned */
			tempDataset->pathPolicy =
				(FTS_PATH_POLICY *) auxReq.itemList1;

		 	tempDataset = tempDataset->next;
		}


		/*
		** Go through the policies returned for this platform and
		** sensor and extract the keyword policies from the catalog.
		*/
		tempDataset = currDataset;

		while (tempDataset != (FTS_DATASET_POLICY *) NULL)
		{
			/* 
			** Get the current dataset information.
			*/
			auxSpec.dataset_idx = tempDataset->dataset_idx;
			(void) strcpy (auxSpec.dataset, tempDataset->dataset);
		 	auxReq.itemList2 =
				(void *) &(tempDataset->keywordPolicyLen);

		 	if ((status = ims_auxCat (&auxReq,
		 		AUX_GET_KEYWORD_POLICY)) < IMS_OK)
		 	{
		 		(void) ims_msg (msgDesc, status, 
		 			"Could not get the keyword policy list for dataset '%s'.",
		 			auxSpec.dataset);
		 		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		 		return (IMS_FATAL);
		 	}
			tempDataset->keywordPolicy =
				(FTS_KEYWORD_POLICY *) auxReq.itemList1;

			/*
			**	Get the keyword value list for each keyword policy 
			*/
			keywordPolicy = tempDataset->keywordPolicy;

			while (keywordPolicy != NULL)
			{
				/*
				** If the datatype is string, character or symbol
				** then cache the keyword list for the given index.
				*/

				switch (keywordPolicy->data_type)
				{
					case IMS_CHAR_TYPE:
					case IMS_SYMBOL_TYPE:
					case IMS_STRING_TYPE:
						/*
						** Get the keyword values for the index
						*/
						auxSpec.keyword_idx = keywordPolicy->keyword_idx;

		 				if ((status = ims_auxCat (&auxReq,
		 					AUX_GET_KEYWORD_VALUE)) < IMS_OK)
		 				{
		 					(void) ims_msg (msgDesc, status, 
		 					"Could not get keyword values for dataset '%s'.", 
		 						auxSpec.dataset);
		 					(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		 					return (IMS_FATAL);
						}

						keywordPolicy->keywordValue = 
							(FTS_KEYWORD_VALUE *) auxReq.itemList1;
						break;

					default:
						/*
						** No keyword values for the datatype
						*/

						keywordPolicy->keywordValue = NULL;
						break;
				}
				keywordPolicy = keywordPolicy->next;
			}

			/*
			** Set the local archive path to null.
			*/

			tempDataset->localArchivePath = NULL;

			/*
			** Get next dataset 
			*/

		 	tempDataset = tempDataset->next;
		}

		/*
		** We must now create the hash table. Only create the 
		** hash table for platforms and sensors with non-empty dataset lists.
		*/
		if ((currDatasetLen > 0) && (currDataset !=
			(FTS_DATASET_POLICY *) NULL))
		{
			if ((currSensor->hashPtr = (IMS_HASH_STRUCT *) 
				ims_hashCreate (currDatasetLen, IMS_STRING,
				msgDesc)) == (IMS_HASH_STRUCT *) NULL)
			{
				(void) ims_msg (msgDesc,
					ims_msgGetSeverity (msgDesc),
					"Creating hash policy table failed.");
				(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
				return (IMS_FATAL);
			}
			tempDataset = currDataset;

			while (tempDataset != (FTS_DATASET_POLICY *) NULL)
			{

				item.data = (void *) tempDataset;
				item.key  = (void *) tempDataset->dataset;

				if (ims_hashSearch (currSensor->hashPtr,
					&item, IMS_ENTER, msgDesc) ==
					(IMS_HASH_ENTRY *) NULL)
				{
					(void) ims_msg (msgDesc,
						ims_msgGetSeverity (msgDesc),
						"ims_hashSearch failed at ENTER.");
					(void) ims_auxCat (&auxReq,
						AUX_CLOSE_CONNECTION);
					return (IMS_FATAL);
				}

				(void) sprintf (Msg, 
					"Loaded policy for '%s, %s, %s'.\n",
					currSensor->platform,
					currSensor->sensor,
					tempDataset->dataset);
				(void) fprintf (stderr, Msg);

				(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_FALSE, Msg,
					CS_NULLTERM);

				tempDataset = tempDataset->next;
			}
		}

		/*
		** Increment currSensor pointer.
		*/
		currSensor = currSensor->next;
	}

	/*
	** Policies are loaded into the cache memory.
	*/
	if ((status = ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Closing the catalog database connection failed.");
		return (status);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** addProcessInfo ()
** 
** Add process information into process table.
** This function uses the static server variables for its input.
**
******************************************************************************/

#ifdef PROCESS
static int addProcessInfo (
	IMS_MSG_STRUCT *msgDesc)
{
	AUX_CAT_REQUEST auxReq;
	AUX_INFO_SPEC auxSpec;

	auxSpec.msgDesc = msgDesc;

	/*
	** Setup the auxReq structure for database login and 
	** query through ims_auxCat routine.
	*/
	(void) strcpy (auxSpec.username, glb_ftsUsername);
	(void) strcpy (auxSpec.password, glb_ftsPassword);
	(void) strcpy (auxSpec.ftsSrvName, glb_ftsSrvName);
	(void) strcpy (auxSpec.programName, glb_ftsProgramName);
	(void) strcpy (auxSpec.catSrvName, glb_catSrvName);
	(void) strcpy (auxSpec.catDbName, glb_catDbName);
	auxReq.auxSpec = &auxSpec;

	/*
	** Make a connection to the database.
	*/
	if (ims_auxCat (&auxReq, AUX_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Connection to the database failed.");
		return (IMS_FATAL);
	}

	/*
	** Add process information into process table
	*/
	auxReq.itemList1 = (void *) glb_ftsHostName;
	auxReq.itemList2 = (void *) &glb_ftsPid;
	auxReq.itemList3 = (void *) glb_ftsSrvName;

	if (ims_auxCat (&auxReq, AUX_ADD_PROCESS_INFO) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Adding process information to processes table failed.");
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		return (IMS_FATAL);
	}

	/*
	** Close database connection.
	*/
	if (ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Closing connection to the database failed.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}
#endif	/* PROCESS */

/******************************************************************************
**
** addProcessEndTime ()
** 
** Set process end time in the process table.
** This function uses the static server variables for its input.
**
******************************************************************************/

#ifdef PROCESS
static int addProcessEndTime (
	IMS_MSG_STRUCT *msgDesc)
{
	AUX_CAT_REQUEST auxReq;
	AUX_INFO_SPEC auxSpec;

	auxSpec.msgDesc = msgDesc;

	/*
	** Setup the auxReq structure for database login and 
	** query through ims_auxCat routine.
	*/
	(void) strcpy (auxSpec.username, glb_ftsUsername);
	(void) strcpy (auxSpec.password, glb_ftsPassword);
	(void) strcpy (auxSpec.ftsSrvName, glb_ftsSrvName);
	(void) strcpy (auxSpec.programName, glb_ftsProgramName);
	(void) strcpy (auxSpec.catSrvName, glb_catSrvName);
	(void) strcpy (auxSpec.catDbName, glb_catDbName);
	auxReq.auxSpec = &auxSpec;

	/*
	** Make a connection to the database.
	*/
	if (ims_auxCat (&auxReq, AUX_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		(void) ims_msg (msgDesc,IMS_FATAL,
			"Connection to the database failed.");
		return (IMS_FATAL);
	}

	/*
	** Add process information into process table
	*/
	auxReq.itemList1 = (void *) glb_ftsHostName;
	auxReq.itemList2 = (void *) &glb_ftsPid;

	if (ims_auxCat (&auxReq, AUX_ADD_PROCESS_ENDTIME) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Setting process end time in processes table failed.");
		(void) ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION);
		return (IMS_FATAL);
	}

	/*
	** Close database connection.
	*/
	if (ims_auxCat (&auxReq, AUX_CLOSE_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Closing connection to the database failed.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}
#endif	/* PROCESS */

/******************************************************************************
**
** init_server ()
**
** Initialize the server on a SRV_START event.
** Event handlers for the server are installed.
**
** Returns:	CS_SUCCEED
**
******************************************************************************/

static int init_server (
	SRV_SERVER *server)
{
	/*
	** When a request comes in from a client, then verify the
	** query request (including permissions, and set up such
	** things.
	*/
	(CS_VOID) srv_handle ((SRV_SERVER *) NULL, SRV_CONNECT,
		(INTFUNCPTR) init_remote);

	/*
	** When a rpc request from a client is recieved, call
	** rpc-execute () to handle the request.  When a language
	** request comes in, the defualt language request handler
	** will take care of it by sending a rejection message to 
	** the client.
	*/
	(CS_VOID) srv_handle ((SRV_SERVER *) NULL, SRV_RPC,
		(INTFUNCPTR) rpc_execute);

	/*
	** When a disconnect request is received, call exit_remote ()
	** to close the connection to the remote DBMS.  Databases
	** reservations are cancelled and queries terminited (end
	** times added).
	*/
	(CS_VOID) srv_handle ((SRV_SERVER *) NULL, SRV_DISCONNECT,
		(INTFUNCPTR) exit_remote);

	/*
	** Install the handler that is called when the Open Server
	** receives an attention from one of its clients. An
	** attention is received when a client calls dbcancel ().
	** I wonder if an attention is occurs on an exit?
	*/	
	(CS_VOID) srv_handle ((SRV_SERVER *) NULL, SRV_ATTENTION,
		(INTFUNCPTR) attn_handler);

	/*
	** Install a handler for bulk transfers of data.
	*/
	(void) srv_handle ((SRV_SERVER *) NULL, SRV_BULK,
		(INTFUNCPTR) ims_bulkHandler);

	/*
	** Install a language handler for bulk transfers.  This is 
	** required in order to use the bulk transfer event.
	*/
	(void) srv_handle ((SRV_SERVER *) NULL, SRV_LANGUAGE,
		(INTFUNCPTR) ims_langHandler);
					 
	/*
	** Create auxiliary process mutex.
	*/
	if (srv_createmutex (FTS_AUX_MUTEX, CS_NULLTERM, &glbAuxMutex)
		!= CS_SUCCEED)
	{
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE,
			"Auxiliary mutex creation failure.\n", CS_NULLTERM);
		return (SRV_STOP);
	}

	/*
	** Create thread mutex for the critical section problem.
	*/
	if (srv_createmutex (FTS_THREAD_MUTEX, CS_NULLTERM, &glbThreadMutex)
		!= CS_SUCCEED)
	{
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE,
			"Replace mutex creation failure.\n", CS_NULLTERM);
		return (SRV_STOP);
	}

	/*
	** Install the message and error handlers for any
	** messages from the remote DBMS or errors from DB-Library.
	*/
	(void) dberrhandle (remoteerr);
	(void) dbmsghandle (remotemsgs);

	/*
	** Install SIGTERM signal handler.
	*/
	if (signal (SIGTERM, termHandler) == (void (*)())-1)
	{
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE,
			"SIGTERM installation failure.\n", CS_NULLTERM);
		return (SRV_STOP);
	}

	/*
	** Now, install our own signal handler for PIPE, to keep the
	** query server from dying when a client is simply killed.
	*/
	if (signal (SIGPIPE, pipeHandler) == (void (*)())-1)
	{
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE,
			"SIGPIPE installation failure.\n", CS_NULLTERM);
		return (SRV_STOP);
	}

	/*
	** Install SIGCHLD signal handler.
	*/

#ifndef WAIT_FOR_CHILD

	if (signal (SIGCLD, childHandler) == (void (*)())-1)
	{
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE,
			"SIGCHLD installation failure.\n", CS_NULLTERM);
		return (SRV_STOP);
	}
#endif




	return (CS_SUCCEED);
}

/******************************************************************************
**
** init_remote ()
**
** Initialize server process on dbopen.
** Event handler for a SRV_CONNECT event.
**
** Arguments:	srvproc - the handle to the client connection that got the
**		SRV_CONNECT.
**
** Returns:	CS_SUCCEED
**
******************************************************************************/

static int init_remote (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;	
	CS_SERVERMSG srvMsg;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];

	/*
	** Our private data area to keep
	** track of our remote DBMS 
	** connection.
	*/ 
	char user[IMS_COL30_LEN+1];
	char password[IMS_COL30_LEN+1];
	int len;

#ifdef IMS_DEBUG
	int threadId;

	(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
		(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
		(CS_INT *) NULL);

	(void) fprintf (stderr, "\nID = %d: CLIENT CONNECT.\n", threadId);
	(void) fflush (stderr);
#endif	/* IMS_DEBUG */

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Set newSrvProc.  This is used if we get an error on the dbopen ()
	** from DB-Library.  Since we won't have a DBPROCESS, we'll
	** know which SRV_PROC to send the message back on when the
	** DB-Library error-handler is called.
	*/
	newSrvProc = srvproc;


	/*
	** Add srvproc to the glb_srvProcList.
	*/
	(void) addSrvProcToGlbList (srvproc);

	/*
	** Allocate a FTS_PROC_DESC information structure.
	*/
	if ((procDesc = (FTS_PROC_DESC *) srv_alloc (sizeof (FTS_PROC_DESC))) ==
		(FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Memory allocation for the process descriptor failed.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}
	(CS_VOID) srv_bzero (procDesc, sizeof (FTS_PROC_DESC));

	/*
	** Initialize message facility for this srvproc.
	*/
	if ((procDesc->msgStruct = ims_msgStructAlloc ())
		== (IMS_MSG_STRUCT *) NULL)
	{
		(void) sprintf (Msg,
			"Memory allocation for the message descriptor failed.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}

	/* Initialize message facility flags. */
	(void) ims_msgSubSystem (procDesc->msgStruct, "IMS");
	(void) sprintf (banner, "%s::%s", glb_ftsHostName, glb_ftsProgramName);
	(void) ims_msgBanner (procDesc->msgStruct, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgStderrFlag (procDesc->msgStruct, IMS_OFF);
	(void) ims_msgQueueFlag (procDesc->msgStruct, IMS_ON);

	/*
	** Open syslog...
	*/
	(void) ims_msgProgramName (procDesc->msgStruct, glb_ftsProgramName);
	(void) ims_msgOpenSyslog (procDesc->msgStruct, "IMS/DADS:", LOG_LOCAL5);

	/*
	** Initialize kerberos data structure.
	*/
#ifdef KRB
	procDesc->sKrb.service = SERVICE;
	procDesc->sKrb.host = glb_ftsHostName;
	procDesc->sKrb.ktxt.length = 0;
#endif	/* KRB */

	/*
	** Initialize catReq structure.
	*/
	procDesc->catReq.granuleSpec = (FTS_CAT_GRANULESPEC *) NULL;
	procDesc->catReq.keyValue = (FTS_CAT_KEYVALUE *) NULL;

	/*
	** Set the readFile hash table structure.
	*/
	procDesc->readHashPtr = glb_readHashPtr;

	/*
	** Initialize the granule descriptor structure.
	*/
	procDesc->granuleDesc.granule_idx = -1;
	procDesc->granuleDesc.fileCount = 0;
	procDesc->granuleDesc.fileList = (FTS_FILE_LIST *) NULL;
	procDesc->granuleDesc.currFile = (FTS_FILE_LIST *) NULL;

	/*
	** Setup the logfile path.
	*/

	procDesc->logFileDir = cmdLine.logFileDir;

	/*
	** Initialize the status table pointer.
	*/
	procDesc->statusTable = glb_statusTable;

	/*
	** Initialize auxiliary mutex structure pointer.
	*/
	procDesc->auxMutex = &glbAuxMutex;
	procDesc->threadMutex = &glbThreadMutex;

	/*
	** Initialize the rest of the procDesc pointers.
	*/
	procDesc->keywordList = (FTS_KEYWORD_LIST *) NULL;
	procDesc->sensorPolicy = (FTS_SENSOR_POLICY *) NULL;
	procDesc->datasetPolicy  = (FTS_DATASET_POLICY *) NULL;
	procDesc->filePolicy = (FTS_FILE_POLICY *) NULL;
	procDesc->readGranule = (FTS_READ_GRANULE *) NULL;

	/*
	** Initialize the process descriptor flags.
	*/
	procDesc->rollbackOnExit = 0;
	procDesc->cancelFlag = 0;
	procDesc->validUser = 0;

	if (cmdLine.recordMsgs != (char *) NULL)
	{
		procDesc->recordMsgs = 1;
	}
	else
	{
		procDesc->recordMsgs = 0;
	}

	/*
	** Get a query descriptor for this process.
	*/
	if ((procDesc->qDesc = ims_qiDescAlloc (procDesc->msgStruct)) ==
		(IMS_QI_DESC_OBJ *) NULL)
	{
		(void) sprintf (Msg,
			"Memory allocation for the query descriptor failed.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	/*
	** Allocate command buffer memory space for qDesc.
	*/
	if ((procDesc->qDesc->cmd = (char *) malloc (IMS_COL512_LEN * 20)) 
		== (char *) NULL)
	{
		(void) sprintf (Msg,
			"Memory allocation for the query command buffer failed.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	/*
	** Hostname and pid were setup at initialization.  The pid and
	** a pointer to the hostname are included here for catalog
	** query reporting.
	*/
	procDesc->ftsHostName = glb_ftsHostName;
	procDesc->ftsPid = glb_ftsPid;
	procDesc->ftsSrvName = glb_ftsSrvName;
	procDesc->ftsProgramName = glb_ftsProgramName;
	procDesc->catSrvName = glb_catSrvName;
	procDesc->catDbName = glb_catDbName;

	/*
	** Update this process login and password.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USER,
		(CS_VOID *) user, (CS_INT) sizeof (user),
		(CS_INT *) &len) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get SRV_T_USER in init_remote.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	if (len > IMS_COL15_LEN)
	{
		(void) sprintf (Msg,
			"User login exceeds maximum length of '%d'.",
			IMS_COL15_LEN);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}
	user[len] = '\0';
	(void) strcpy (procDesc->client.login, user);

	if (srv_thread_props (srvproc, CS_GET, SRV_T_PWD,
		(CS_VOID *) password, (CS_INT) sizeof (password),
		(CS_INT *) &len) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get SRV_T_PWD in init_remote.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	if (len > IMS_COL15_LEN)
	{
		(void) sprintf (Msg,
			"User password exceeds maximum length of '%d'.",
			IMS_COL15_LEN);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INIT_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}
	password[len] = '\0';
	(void) strcpy (procDesc->client.password, password);

	/*
	** The length of glb_ftsUsername and glb_ftsPassword has 
	** already been checked.
	*/
	(void) strcpy (procDesc->surrogate.login, glb_ftsUsername);
	(void) strcpy (procDesc->surrogate.password, glb_ftsPassword);

	/*
	** Now, set this remote-user information into the srvproc structure for
	** this connection. It will then be available to other events ...
	** some of which may require this information to perform connections
	** to other dataservers.  
	*/
	if (srv_thread_props (srvproc, CS_SET, SRV_T_USERDATA,
		(CS_VOID *) procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not set SRV_T_USERDATA in init_remote.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	/*
	** Send the client a done, to acknowledge the
	** connect request.
	*/
	(CS_VOID) srv_senddone (srvproc, SRV_DONE_FINAL,
		 CS_TRAN_UNDEFINED, (CS_INT) 0 );
		
	return (CS_SUCCEED);
}

/******************************************************************************
**
** exit_remote ()
**
** Handler for SRV_DISCONNECT events. Closes the remote DBMS connection
** if appropriate.
**
** Note that the code to disconnect from the remote DBMS is
** SQL Server specific. If disconnecting from a foreign DBMS,
** the disconnect code would be different, but would still
** occur here.
**
** Arguments:	srvproc - the handle to the client connection
**
** Returns:	    CS_SUCCEED
**
******************************************************************************/

static int exit_remote (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_GRANULE_DESC *granuleDesc;
	IMS_MSG_STRUCT *msgStruct;
	FTS_KEYWORD_LIST *tptr1;         /* Temporary pointers. */
	FTS_KEYWORD_LIST *tptr2;
	FTS_CAT_GRANULESPEC *tPtr3;
	FTS_CAT_GRANULESPEC *tPtr4;
	FTS_CAT_KEYVALUE *tPtr5;
	FTS_CAT_KEYVALUE *tPtr6;
	FTS_FILE_LIST *tPtr7;
	FTS_FILE_LIST *tPtr8;
	char *event = NULL;
	int status;

#ifdef IMS_DEBUG
	int threadId;
	(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
		(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
		(CS_INT *) NULL);

	(void) fprintf (stderr, "\nID = %d: CLIENT EXIT.\n", threadId);
	(void) fflush (stderr);
#endif	/* IMS_DEBUG */

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
			(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
			(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get SRV_T_USERDATA in exit_remote.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		(void) ims_srvLogMsgs (srvproc);
		procDesc = (FTS_PROC_DESC *)NULL;
	}

	/*
	** Is there a FTS_PROC_DESC structure to clean up?
	*/
	if (procDesc != (FTS_PROC_DESC *) NULL)
	{
		/* Initialize pointers. */
		catReq = &(procDesc->catReq);
		stateSpec = &(catReq->stateSpec);
		granuleSpec = catReq->granuleSpec;
		granuleDesc = &(procDesc->granuleDesc);
		msgStruct = procDesc->msgStruct;

		/*
		** Go through the linked fileList and clean up the files
		** accordingly for the current event.
		*/
		tPtr7 = granuleDesc->fileList;
		while (tPtr7 != (FTS_FILE_LIST *) NULL)
		{
			if (tPtr7->isFileOpen)
			{
				(void) close (tPtr7->fd);
				tPtr7->isFileOpen = IMS_FALSE;
			}

			if (procDesc->rollbackOnExit == IMS_TRUE)
			{
				if ((procDesc->clientReq.requestType == IMS_ADD) ||
					(procDesc->clientReq.requestType == IMS_REPLACE))
				{
					if (tPtr7->hasFileBeenOpen)
					{
						if (unlink (tPtr7->fileToWrite) == -1)
						{
							(void) ims_msg (msgStruct, IMS_FATAL,
								"Failed to unlink file '%s' at rollback. %s.",
								tPtr7->fileToWrite, strerror (errno));
							(void) ims_srvLogMsgs (srvproc);
						}
					}
				}
			}

			/* Free the current fileList structure. */
			tPtr8 = tPtr7->next;
			(void) free ((char *) tPtr7);
			tPtr7 = tPtr8;
		}

		/*
		** If the rollback flag is on then we need to delete the
		** granule record for add and replace events and we need
		** to decrement the read counter for the get event. Also
		** for the replace event we need to reset the status of
		** the original record.
		*/
		if (procDesc->rollbackOnExit == IMS_TRUE)
		{
#ifdef IMS_DEBUG
			(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
				(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
				(CS_INT *) NULL);

			(void) fprintf (stderr,
				"\n\tID = %d: RollbackOnExit detected.\n", threadId);
			(void) fflush (stderr);
#endif	/* IMS_DEBUG */

			switch (procDesc->clientReq.requestType)
			{
			case IMS_GET:
			case IMS_GET_LATEST:
			case IMS_GET_MAX:
				if (ims_decrReadGranule (procDesc->readHashPtr,
					procDesc->readGranule, msgStruct) < IMS_OK)
				{
					(void) ims_srvLogMsgs (srvproc);
				}
				break;	

			case IMS_ADD:
				/* Lock mutex and raise priority. */
				(void) ims_lockMutexWait (procDesc->threadMutex);
				(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

				if (ims_srvCat (srvproc, FTS_DELETE_GRANULE_RECORD)
					< IMS_OK)
				{
					/* Unlock mutex and lower priority. */
					(void) ims_unlockMutex (procDesc->threadMutex);
					(void) ims_changePriority (srvproc,
						-1 * FTS_PRIORITY_DELTA);

					(void) ims_srvLogMsgs (srvproc);
					break;
				}

				/* Unlock mutex and lower priority. */
				(void) ims_unlockMutex (procDesc->threadMutex);
				(void) ims_changePriority (srvproc,
					-1 * FTS_PRIORITY_DELTA);
				break;

			case IMS_REPLACE:
				/* Lock mutex and raise priority. */
				(void) ims_lockMutexWait (procDesc->threadMutex);
				(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

				if (ims_srvCat (srvproc, FTS_DELETE_GRANULE_RECORD)
					< IMS_OK)
				{
					/* Unlock mutex and lower priority. */
					(void) ims_unlockMutex (procDesc->threadMutex);
					(void) ims_changePriority (srvproc,
						-1 * FTS_PRIORITY_DELTA);

					(void) ims_srvLogMsgs (srvproc);
					break;
				}

				/*
				** Set the status to available for the original record.
				** The original records granule_idx is in oldGranule_idx.
				*/
				stateSpec->status = IMS_AVAILABLE;
				granuleSpec->granule_idx = granuleSpec->oldGranule_idx;
				if (ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)
					< IMS_OK)
				{
					/* Unlock mutex and lower priority. */
					(void) ims_unlockMutex (procDesc->threadMutex);
					(void) ims_changePriority (srvproc,
						-1 * FTS_PRIORITY_DELTA);

					(void) ims_srvLogMsgs (srvproc);
					break;
				}

				/* Unlock mutex and lower priority. */
				(void) ims_unlockMutex (procDesc->threadMutex);
				(void) ims_changePriority (srvproc,
					-1 * FTS_PRIORITY_DELTA);
				break;

			default:
				break;
			}
		}

		/*
		** Free keyword list extracted.
		*/
		tptr1 = procDesc->keywordList;
		while (tptr1 != (FTS_KEYWORD_LIST *) NULL)
		{
			tptr2 = tptr1->next;
			(void) free ((char *) tptr1);
			tptr1 = tptr2;
		}
		
		/*
		** Free the allocated granuleSpec structure.
		*/
		tPtr3 = procDesc->catReq.granuleSpec;
		while (tPtr3 != (FTS_CAT_GRANULESPEC *) NULL)
		{
			tPtr4 = tPtr3->next;
			(void) free ((char *) tPtr3);
			tPtr3 = tPtr4;
		}

		/*
		** Free the allocated keyValue structure.
		*/
		tPtr5 = procDesc->catReq.keyValue;
		while (tPtr5 != (FTS_CAT_KEYVALUE *) NULL)
		{
			tPtr6 = tPtr5->next;
			(void) free ((char *) tPtr5);
			tPtr5 = tPtr6;
		}


		/*
		** Logoff and free the query descriptor.
		*/
		if (procDesc->qDesc != (IMS_QI_DESC_OBJ *) NULL)
		{
			(void) free (procDesc->qDesc->cmd);
			(void) ims_qiFreeDesc (procDesc->qDesc);
		}

		/*
		** Send a status log to syslog here.
		*/
		switch (procDesc->clientReq.requestType)
		{
			case IMS_ADD:
				event = "ADD";
				break;

			case IMS_GET:
				event = "GET";
				break;

			case IMS_DELETE:
				event = "DELETE";
				break;

			case IMS_REPLACE:
				event = "REPLACE";
				break;
		}
		
		/*
		** If this is an event we want to track, then log the message
		** here.
		*/

		if (event)
		{
			if ((status = ims_msgGetSeverity(msgStruct)) < IMS_WARNING)
			{
				(void) ims_msg(msgStruct, status,
					"Event %s by user '%s' for granule '%s' in dataset '%s' failed.",
					event, procDesc->userName, 
					procDesc->clientReq.name, procDesc->clientReq.dataset);
			}
			else
			{
				(void) ims_msg(msgStruct, IMS_INFO,
					"Event %s by user '%s' for granule '%s' in dataset '%s' succeeded.",
					event, procDesc->userName, 
					procDesc->clientReq.name, procDesc->clientReq.dataset);
			}
		}

		/*
		** Log any messages in the message queue.
		*/
		(void) ims_srvLogMsgs (srvproc);

		/*
		** Free the message descriptor.
		*/
		(void) ims_msgStructFree (msgStruct);
		msgStruct = (IMS_MSG_STRUCT *) NULL;

		/*
		** Free the FTS process descriptor.
		*/	
		if (srv_free ((char *) procDesc) == CS_FAIL)
		{
			(void) sprintf (Msg,
				"Memory de-allocation for procDesc failed.\n");
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg,
				CS_NULLTERM);
		}

	}

	/*
	** Remove srvproc from global srvproc list.
	*/
	(void) removeSrvProcFromGlbList (srvproc);

	return (CS_SUCCEED);
}

/******************************************************************************
**
** rpc_execute ()
**
** Execute client rpc request.
**
** If we get an rpc that we do not recognize, we'll send an error message
** to the client.  To find the RPC from our RPC list, a simple linear scan
** is made. 
**
******************************************************************************/

static int rpc_execute (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;
	RPC *Rpc;
	CS_SERVERMSG srvMsg;
	int (*function)(SRV_PROC *);
	int status;

#ifdef IMS_DEBUG
	int threadId;

	(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
		(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
		(CS_INT *) NULL);

	(void) fprintf (stderr,
		"\nID = %d: RPC EXECUTE.\n", threadId);
	(void) fflush (stderr);
#endif	/* IMS_DEBUG */

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get SRV_T_USERDATA in rpc_execute.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg, 
			"Process descriptor is NULL in rpc_execute.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT) 0);
		return (CS_SUCCEED);
	}

	/*
	** Get the current rpc name.
	*/
	item.key = (char *) srv_rpcname (srvproc, (CS_INT *) NULL);

	if ((hashEntry = ims_hashSearch (glb_rpcHashPtr, &item, IMS_FIND,
		procDesc->msgStruct)) != (IMS_HASH_ENTRY *) NULL)
	{
		Rpc = (RPC *) hashEntry->data;
		function = Rpc->rpcHandler;

		/*
		** Call the rpc handler.
		*/
#ifdef IMS_DEBUG
		(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
			(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
			(CS_INT *) NULL);

		(void) fprintf (stderr,
			"\n\tID = %d: Processing remote procedure '%s'.\n",
			threadId, Rpc->rpcName);
		(void) fflush (stderr);
#endif	/* IMS_DEBUG */

		if ((status = (*function)(srvproc)) < IMS_OK)
		{
			if (procDesc->cancelFlag)
			{
#ifdef IMS_DEBUG
				(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
					(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
					(CS_INT *) NULL);

			 	(void) fprintf (stderr,
					"\n\tID = %d: Client connection terminated.\n", threadId);
				(void) fflush (stderr);
#endif	/* IMS_DEBUG */

				if (ims_srvShipMsgs (srvproc) < IMS_OK)
				{
					(void) sprintf (Msg,
						"Sending messages to client failed.\n");
					(CS_VOID) srv_log ((SRV_SERVER *) NULL,
						CS_TRUE, Msg, CS_NULLTERM);
				}
				(CS_VOID) srv_sendstatus (srvproc, (CS_INT) status);
				(CS_VOID) srv_senddone (srvproc,
					SRV_DONE_ERROR | SRV_DONE_FINAL,
					CS_TRAN_UNDEFINED, (CS_INT) 0);

			 	if (srv_event (srvproc, SRV_DISCONNECT,
					(CS_VOID *) NULL) == CS_FAIL)
			 	{
					(void) sprintf (Msg,
						"SRV_DISCONNECT event failed.\n");
					(CS_VOID) srv_log ((SRV_SERVER *) NULL,
						CS_TRUE, Msg, CS_NULLTERM);
			 	}
			}
			else
			{
				if (ims_srvShipMsgs (srvproc) < IMS_OK)
				{
					(void) sprintf (Msg,
						"Sending messages to client failed.\n");
					(CS_VOID) srv_log ((SRV_SERVER *) NULL,
						CS_TRUE, Msg, CS_NULLTERM);
				}
				(CS_VOID) srv_sendstatus (srvproc, (CS_INT) status);
				(CS_VOID) srv_senddone (srvproc, SRV_DONE_FINAL,
					CS_TRAN_UNDEFINED, (CS_INT) 0);
			}
		}
		else
		{

			/*
			** Send messages generated so far to the client.
			*/
			if (ims_srvShipMsgs (srvproc) < IMS_OK)
			{
				(void) sprintf (Msg,
					"Sending messages to client failed.\n");
				(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE,
					Msg, CS_NULLTERM);
			}
			(CS_VOID) srv_sendstatus (srvproc, (CS_INT) IMS_OK);
			(CS_VOID) srv_senddone (srvproc, SRV_DONE_FINAL,
				CS_TRAN_UNDEFINED, (CS_INT) 0); 
		}
		return (CS_SUCCEED);
	}

	/*
	** RPC not recognized.
	*/

	if (ims_srvShipMsgs (srvproc) < IMS_OK)
	{
		(void) sprintf (Msg, "Could not ship messages to client.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg,
			CS_NULLTERM);
	}

	(CS_VOID) srv_sendstatus (srvproc, (CS_INT) IMS_FATAL);

	/*
	** Make sure that RPC name is not NULL!
	*/

	if (srv_rpcname(srvproc, (CS_INT *) NULL) != NULL)
	{
		(void) sprintf (Msg, "Unknown RPC received: %s\n", 
			srv_rpcname (srvproc, (CS_INT *) NULL));
	}
	else
	{
		(void) sprintf (Msg, "Server received NULL RPC name.");
	}

	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

	/* Populate the CS_SERVERMSG structure. */
	srvMsg.msgnumber = FTS_RPC_ERROR;
	(void) strcpy (srvMsg.text, Msg);
	srvMsg.textlen = (CS_INT) strlen (Msg);

	(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
	(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
		CS_TRAN_UNDEFINED, (CS_INT) 0);

	return (CS_SUCCEED);
}

/******************************************************************************
**
** chk_err ()
**
** Print out Open Server errors.
**
** Arguments:	server - the Open Server server running.
**		srvproc	- the Open Server connection that wants to login.
**		errornum - the Open Server error number.
**		severity - error severity.
**		state - error state.
**		oserrnum - operating system error number, if any.
**		errtext	- the text of the error message.
**		errtextlen - length of the errtext message
**		oserrtext - the text of the operating system error message.
**		oserrtextlen - length of the errtext message
**
**  Returns:	CS_SUCCEED, SRV_CANCEL, or SRV_EXIT_PROGRAM
**
******************************************************************************/

static CS_INT chk_err (
	SRV_SERVER *server,
	SRV_PROC *srvproc,
	CS_INT errornum,
	CS_BYTE severity,
	CS_BYTE state,
	CS_INT oserrnum,
	CS_CHAR *errtext,
	CS_INT errtextlen,
	CS_CHAR *oserrtext,
	CS_INT oserrtextlen)
{
	CS_SERVERMSG srvMsg;
	CS_CHAR errtextBuff[CS_MAX_MSG];
	CS_CHAR oserrtextBuff[CS_MAX_MSG];
	CS_INT errtextBuffLen, oserrtextBuffLen;
#ifdef IMS_DEBUG
	int threadId;

	threadId = 0;
	(void) fprintf (stderr, "\nID = %d: SERVER CHK_ERR.\n", threadId);
	(void) fflush (stderr);
#endif

	errtextBuffLen = IMS_MIN (errtextlen + 1, CS_MAX_MSG - 1);
	srv_bmove(errtext, errtextBuff, errtextBuffLen);
	errtextBuff[errtextBuffLen] = '\0';

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));

	/*
	** Is this an operating system error?
	*/
	if (oserrnum != SRV_ENO_OS_ERR)
	{
		oserrtextBuffLen = IMS_MIN (oserrtextlen + 1, CS_MAX_MSG - 1);
		srv_bmove(oserrtext, oserrtextBuff, oserrtextBuffLen);
		oserrtextBuff[oserrtextBuffLen] = '\0';
		(void) sprintf (Msg, "SERVER OS ERROR: %d/%d/%d/%d: %s.",
			errornum, severity, state, oserrnum, oserrtextBuff);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
#ifdef IMS_DEBUG
		(void) fprintf (stderr, Msg);
		(void) fprintf (stderr, "\n");
		(void) fflush (stderr);
#endif
	}

	/*
	** Is this a fatal error for the Open Server?
	*/
	if (severity == SRV_FATAL_SERVER)
	{
		(void) sprintf (Msg,
			"SERVER FATAL SERVER ERROR: %d/%d/%d: %s.",
			errornum, severity, state, errtextBuff);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
#ifdef IMS_DEBUG
		(void) fprintf (stderr, Msg);
		(void) fprintf (stderr, "\n");
		(void) fflush (stderr);
#endif
		return (SRV_CANCEL);
	}

	/*
	** Is this a fatal error for the srvproc?
	*/
	if (severity == SRV_FATAL_PROCESS)
	{
		(void) sprintf (Msg,
			"SERVER FATAL PROCESS ERROR: %d/%d/%d: %s.",
			errornum, severity, state, errtextBuff);
		(void) fprintf (stderr, Msg);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
#ifdef IMS_DEBUG
		(void) fprintf (stderr, Msg);
		(void) fprintf (stderr, "\n");
		(void) fflush (stderr);
#endif
		
		/*
		** Make a disconnect request, to be executed if possible.
		*/
		if (srvproc != (SRV_PROC *) NULL)
		{
			(CS_VOID) srv_event (srvproc, SRV_DISCONNECT,
				(CS_VOID *) NULL);
		}

		return (SRV_CANCEL);
	}

	/*
	** This is a non-fatal error or an informational message.
	** Pass it through to the client.
	*/
	(void) sprintf (Msg, "SERVER NON-FATAL ERROR: %d/%d/%d: %s",
		errornum, severity, state, errtextBuff);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
#ifdef IMS_DEBUG
	(void) fprintf (stderr, Msg);
	(void) fprintf (stderr, "\n");
	(void) fflush (stderr);
#endif

	if (srvproc != (SRV_PROC *) NULL && (server != NULL))
	{
		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = errornum;
		srvMsg.state = state;
		srvMsg.severity = (CS_INT) severity;
		(void) strcpy (srvMsg.text, errtextBuff);
		srvMsg.textlen = errtextBuffLen;
		srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;
	
		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
	}

	return (CS_SUCCEED);
}

/******************************************************************************
**
** remotemsgs ()
**
** This routine is called by DB-Library when any messages are received
** from the remote DBMS. It gets the remote message information and
** sends it back to the Open Server client as a message.
**
** Arguments:	dbproc - The handler to the remote DBMS process that sent the
**			message.
**		msgno - The message number.
**		msgstate - The message state.
**		severity - The message severity.
**		msgtext - The text of the message.
**		srvname - The name of the server that sent the message.
**		procname - The procedure name, if any, of the remote dbms
**			command.
**		line - The remote dbms command buffer line to which the msg
**			applies.
**
**  Returns:	0
**
******************************************************************************/

static int remotemsgs (
	DBPROCESS *dbproc,
	DBINT msgno,
	int msgstate,
	int severity,
	char *msgtext,
	char *srvname,
	char *procname,
	int line)
{
	SRV_PROC *srvproc;
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];
	int sigFlag;
#ifdef IMS_DEBUG
	int threadId;

	threadId = 0;
	(void) fprintf (stderr, "\nID = %d: REMOTEMSGS.\n", threadId);
	(void) fflush (stderr);
#endif

	srvproc = (SRV_PROC *) NULL;
	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));

	/*
	** If this is msg #5701, skip it.  It's just a database context
	** change.
	*/
	if (msgno == 5701)
	{
		return (0);
	}

	/* 
	** There may have been a remote DBMS error during the remote open.
	** In this case, the DBPROCESS will be NULL, and we'll send 
	** the message back on the most recent SRV_PROC. 
	*/
	if (dbproc == (DBPROCESS *) NULL)
	{
		srvproc = newSrvProc;
	}
	else
	{
		/*
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		srvproc = (SRV_PROC *) dbgetuserdata (dbproc);
	}

	/*
	** Create and then process the message
	*/
	(void) sprintf (msg,
		"Sybase Message (%s)", srvname[0] == '\0'? "unnamed": srvname);

	if (procname[0] != '\0')
	{
		(void) sprintf (msg + strlen (msg), " (proc = %s)", procname);
	}

	if (line != 0)
	{
		(void) sprintf (msg + strlen (msg), " (line = %d)", line);
	}

	(void) sprintf (msg + strlen (msg), ": ");
	(void) sprintf (msg + strlen (msg), msgtext);

	if (srvproc != (SRV_PROC *) NULL)
	{
		/*
		** Get the process descriptor for this thread.
		*/
		if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
			(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
			(CS_INT *) NULL) == CS_FAIL)
		{
			procDesc = (FTS_PROC_DESC *)NULL;
		}

		if (procDesc != (FTS_PROC_DESC *) NULL)
		{
			/*
			** To avoid a deadlock loop we must not log messages
			** into sigEvents database  generated from executing 
			** addSigEvent stored procedure.
			**
			** ???? I think this chunk of code needs to be deleted or
			** finished. If this does happen we need a graceful exit.
			*/
			sigFlag = IMS_TRUE;
			if (strcmp (procname , "addSigEvent") == 0)
			{
				sigFlag = IMS_FALSE;
			}

			/* Queue the message. */
			(void) ims_msg (procDesc->msgStruct,
				sybToCdbSeverity (severity), msg);
		}
		else
		{
			/* Send the message to the client. */

			/* Populate the CS_SERVERMSG structure. */
			srvMsg.msgnumber = msgno;
			srvMsg.state = msgstate;
			srvMsg.severity = severity;
			(void) strcpy (srvMsg.text, msg);
			srvMsg.textlen = (CS_INT) strlen (msg);
			(void) strcpy (srvMsg.svrname, srvname);
			srvMsg.svrnlen = (CS_INT) strlen (srvname);
			(void) strcpy (srvMsg.proc, procname);
			srvMsg.proclen = (CS_INT) strlen (procname);
			srvMsg.line = line;
			srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

#ifdef IMS_DEBUG
			(void) fprintf (stderr, "\n\tID = %d: REMOTEMSGS(NULL), %s.\n", threadId, msg);
			(void) fflush (stderr);
#endif	/* IMS_DEBUG */

			(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		}
	}

	return (0);
}

/******************************************************************************
**
** remoteerr ()
**
** This is the handler for error messages from the remote DBMS, in 
** this case, DB-Library. It is called whenever a DB-Library error
** occurs. It queues the created message in the message queue.
**
** Arguments:	dbproc   - The process handle for the remote dbms.
**		severity - The severity of the error.
**		dberr	 - The DB-Library error number.
**		oserr	 - The operating system error, if any.
**		dberrstr - The text of the DB-Library error.
**		oserrstr - The text of operating system error, if any.
**
**  Returns:	INT_EXIT to exit the program.
**		INT_CANCEL to cause a CS_FAIL return from the DB-Library routine
**			that got the error.
**
******************************************************************************/

static int remoteerr (
	DBPROCESS *dbproc,
	int severity,
	int dberr,
	int oserr,
	char *dberrstr,
	char *oserrstr)
{
	SRV_PROC *srvproc;
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];
#ifdef IMS_DEBUG
	int threadId;

	threadId = 0;
	(void) fprintf (stderr, "\nID = %d: REMOTEERR.\n", threadId);
	(void) fflush (stderr);
#endif

	srvproc = (SRV_PROC *) NULL;
	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = severity;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/* 
	**  There may have been a remote DBMS error during the remote 
	**  open. In this case, the DBPROCESS will be NULL and
	**  we'll send the message to the client on the most recent
	**  SRV_PROC. 
	*/
	if (dbproc == (DBPROCESS *) NULL)
	{
		srvproc = newSrvProc;
	}
	else
	{
		/*
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		srvproc = (SRV_PROC *) dbgetuserdata (dbproc);
	}

	/*
	**  Format the message string and send it to the Open Server client.
	*/
	if (srvproc != (SRV_PROC *) NULL)
	{
		(void) sprintf (msg, "Sybase Error: %s\n", dberrstr);

		/*
		** Get the process descriptor for this thread.
		*/
		if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
				(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
				(CS_INT *) NULL) == CS_FAIL)
		{
			procDesc = (FTS_PROC_DESC *)NULL;
		}

		if (procDesc != (FTS_PROC_DESC *) NULL)
		{
			(void) ims_msg (procDesc->msgStruct,
				sybToCdbSeverity (severity), msg);
		}
		else
		{
			/* Populate the CS_SERVERMSG structure. */
			srvMsg.msgnumber = dberr;
			(void) strcpy (srvMsg.text, msg);
			srvMsg.textlen = (CS_INT) strlen (msg);

#ifdef IMS_DEBUG
			(void) fprintf (stderr, "\n\tID = %d: REMOTEERR(NULL), %s.\n", threadId, msg);
			(void) fflush (stderr);
#endif	/* IMS_DEBUG */

			(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		}

		if (oserr != DBNOERR)
		{
			(void) sprintf (msg, "Sybase OS Error: %s\n", oserrstr);
			if (procDesc != (FTS_PROC_DESC *)NULL)
			{
				(void) ims_msg (procDesc->msgStruct,
					sybToCdbSeverity (severity), msg);
			}
			else
			{
				/* Populate the CS_SERVERMSG structure. */
				srvMsg.msgnumber = oserr;
				(void) strcpy (srvMsg.text, msg);
				srvMsg.textlen = (CS_INT) strlen (msg);

#ifdef IMS_DEBUG
				(void) fprintf (stderr, "\n\tID = %d: REMOTEERR(NULL), %s.\n", threadId, msg);
				(void) fflush (stderr);
#endif	/* IMS_DEBUG */

				(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
			}
		}
	}
	return (INT_CANCEL);
}

/******************************************************************************
**
** getArgs ()
**
** Read and set program arguments from the command line.
**
** Parameters:  argc - int
**              argv - pointer to array of char pointers
**              msgDesc - The message descriptor.
**
** Returns:	IMS_OK, IMS_FATAL
**
** 2/2/94 - Removed the call to ims_dbAuth() because we eliminated the
** authorization table capability.
**
******************************************************************************/

static int getArgs (
	int argc,
	char *argv[],
	IMS_MSG_STRUCT *msgDesc)
{
	int status;

	/*
	** Get new command-line arguments.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if we got everything off of the command line.
	*/
	if (status < argc)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Only %d out of the %d command line arguments were processed.",
			status, argc);
	}

	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the commandline, except
	** password, which will be gone by this point.
	*/
	if (cmdLine.commandFile != (char *) NULL)
	{ 
		if ((status = ims_getFileParms (cmdLine.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{  
			return (status);	
		}  

		/*
		** Now, get command line args again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			return (status);
		}
	}

	if (cmdLine.ftsSrvName == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Must provide server name.");
		return (IMS_FATAL);
	}
	glb_ftsSrvName = cmdLine.ftsSrvName;

	/*
	** Login provided on commandline?
	**
	** 2/2/94 - Removed the ability to check the dbAuth.tbl file
	** for authorization.
	*/
	if (cmdLine.login == (char *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Must provide login name.");
		return (IMS_FATAL);
	}
	else
	{
		glb_ftsUsername = cmdLine.login;

		if (cmdLine.password == (char *) NULL)
		{
			glb_ftsPassword = "";
		}
		else
		{
			glb_ftsPassword = cmdLine.password;
		}
	}

	if (((int) strlen (glb_ftsUsername) > IMS_COL30_LEN) ||
		((int) strlen (glb_ftsPassword) > IMS_COL30_LEN))
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Server login or password exceeds maximum length of '%d'.",
			IMS_COL30_LEN);
		return (IMS_FATAL);
	}

	/*
	** Check for the catalog server name.
	*/
	if (cmdLine.catSrvName == (char *) NULL)
	{
		glb_catSrvName = "";
	}
	else
	{
		glb_catSrvName = cmdLine.catSrvName;
	}

	/*
	** Check for the catalog database name.
	*/
	if (cmdLine.catDbName == (char *) NULL)
	{
		glb_catDbName = "";
	}
	else
	{
		glb_catDbName = cmdLine.catDbName;
	}

	/*
	** Setup logFile
	*/
	if (cmdLine.logFile == (char *) NULL)
	{
		/*
		** Set default for logFile
		*/
		(void) sprintf (logFileName, "%s_%s.log",
			glb_ftsProgramName, ims_timeStamp ());
		cmdLine.logFile = logFileName;
	}

	if (cmdLine.logFileDir == (char *) NULL)
	{
		/*
		** Set default for logFileDir
		*/
		if ((char *) getcwd (logFileDir, IMS_COL255_LEN-1)
			== (char *)NULL)
		{
			(void) strcpy (logFileDir, "."); 
		}
		cmdLine.logFileDir = logFileDir;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** attn_handler ()
**
** This event handler is called when the Open Server receives an
** attention from one of its clients.
**
** Arguments:	srvproc - the Open Server connection received the attention.
**
** Returns:	CS_SUCCEED
**
******************************************************************************/

static int attn_handler (
	SRV_PROC *srvproc)
{

#ifdef IMS_DEBUG
	int threadId;

	(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
		(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
		(CS_INT *) NULL);

	(void) fprintf (stderr, "\nID = %d: ATTN RECEIVED.\n", threadId);
	(void) fflush (stderr);
#endif	/* IMS_DEBUG */

	return (CS_SUCCEED);
}

/******************************************************************************
**
** checkDbLogin ()
**
** Verify dataserver login information.
** This function uses the static server variables for its input.
**
******************************************************************************/

static int checkDbLogin (
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_QI_DESC_OBJ *catDesc;

	/*
	** Obtain descriptor for a catalog login.
	*/
	if ((catDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Internal error trying to verify database login information.");
		return (IMS_FATAL);
	}

	/*
	** Set login information into the descriptor.
	*/
	IMS_SETUSER (catDesc, glb_ftsUsername);

	IMS_SETPSWD (catDesc, glb_ftsPassword);

	if ((int) strlen (glb_catSrvName) > 0)
	{
		IMS_SETSERVER (catDesc, glb_catSrvName);
	}

	if ((int) strlen (glb_catDbName) > 0)
	{
		IMS_SETDBNAME (catDesc, glb_catDbName);
	}

	IMS_SETPROG (catDesc, glb_ftsProgramName);

	/*
	** Turn Sybase error and message handling on.
	*/
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/*
	** Login to the catalog database.  If the login fails, return
	** an error.
	*/
	if (ims_qiLogin (catDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"User '%s' is not able to log in to the catalog server.",
			glb_ftsUsername);

		return (IMS_FATAL);
	}

	/*
	** Logoff from the catalog database, and return IMS_OK.
	*/
	(void) ims_qiLogoff (catDesc);

	/*
	** Turn Sybase error and message handling off.
	*/
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_OFF);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_OFF);

	(void) fprintf (stderr, "\nSurrogate Login Successful.\n");
	(void) fflush (stderr);

	return (IMS_OK);
}

/******************************************************************************
**
** rpc_list ()
**
** List the procedures that exist on this server.
**
** This routine runs through the array of known RPC's and sends back
** the RPC name and usage message.
**
** Arguments:	srvproc - Pointer to the SRV_PROC that got the rpc.
**
** Returns:	CS_SUCCEED or CS_FAIL
**
******************************************************************************/

static int rpc_list (
	SRV_PROC *srvproc)
{
	CS_DATAFMT format;
	char colname[IMS_COL80_LEN];
	int colLen;
	register int i;

	/*
	** Since there are no parameters, we won't do any RPC parameter
	** checking.  We'll just describe the row and then send the data
	** back to the client.
	*/

	/*
	** Build the column name.
	*/
	(void) sprintf (colname, "RPC list for %s", glb_ftsSrvName);

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &format, (CS_INT) sizeof (format));
	(void) strcpy (format.name, colname);
	format.namelen = (CS_INT) strlen (colname);
	format.datatype = CS_CHAR_TYPE;
	format.maxlength = IMS_COL128_LEN;

	/*
	** Describe the column.
	*/
	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &format);

	/*
	** Go through the list of procedures
	*/
	for (i = 0; i < rpcCount; i++)
	{
		/*
		** Bind the location of the row data and its length
		** for each cmd (row).
		*/
		colLen = strlen (Rpcs[i].rpcUsage);

		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 1, &format, (CS_BYTE *) Rpcs[i].rpcUsage,
			(CS_INT *) &colLen, CS_GOODDATA);

		/*
		** Send the row.
		*/
		(CS_VOID) srv_xferdata (srvproc, CS_SET, SRV_ROWDATA);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** rpc_getEventBegin ()
**
******************************************************************************/

static int rpc_getEventBegin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_DATAFMT paramFormat;
	CS_SERVERMSG srvMsg;
	char tempFormat[IMS_COL10_LEN+1];
	CS_TINYINT requestType;
	int status;
	int length1;
	int length2;
	int length3;
	int length4;
	int length5;
	int length6;
	int length7;
	int length8;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = length3 = length4 = length5 = length6 = length7 = length8 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_getEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg, 
			"Process descriptor is NULL in rpc_getEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf(Msg,
			"Client login not valid in rpc_getEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* platform */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) procDesc->clientReq.platform,
		(CS_INT *) &length1, CS_GOODDATA);

	/* sensor */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) procDesc->clientReq.sensor,
		(CS_INT *) &length2, CS_GOODDATA);

	/* dataset */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) procDesc->clientReq.dataset,
		(CS_INT *) &length3, CS_GOODDATA);

	/* name */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat, (CS_BYTE *) procDesc->clientReq.name,
		(CS_INT *) &length4, CS_GOODDATA);

	/* format */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat, (CS_BYTE *) tempFormat,
		(CS_INT *) &length5, CS_GOODDATA);

	/* version */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat, (CS_BYTE *) &(procDesc->clientReq.version),
		(CS_INT *) &length6, CS_GOODDATA);

	/* requestType */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 7, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 7, &paramFormat, (CS_BYTE *) &requestType,
		(CS_INT *) &length7, CS_GOODDATA);


	/* accountId */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 8, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 8, &paramFormat, (CS_BYTE *) procDesc->clientReq.accountId,
		(CS_INT *) &length8, CS_GOODDATA);

	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get parameter data in rpc_getEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 8) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL30_LEN) ||
		(length3 == 0) || (length3 > IMS_COL80_LEN) ||
		(length4 == 0) || (length4 > IMS_COL30_LEN) ||
		(length5 == 0) || (length5 > IMS_COL10_LEN) ||
		(length6 != sizeof (procDesc->clientReq.version)) ||
		(length7 != sizeof (requestType)) ||
		(length8 == 0) || (length8 > IMS_COL15_LEN))
	{
		(void) sprintf (Msg,
			"Error in parameter list in rpc_getEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character fields.
	*/
	procDesc->clientReq.platform[length1] = '\0';
	procDesc->clientReq.sensor[length2] = '\0';
	procDesc->clientReq.dataset[length3] = '\0';
	procDesc->clientReq.name[length4] = '\0';
	procDesc->clientReq.accountId[length8] = '\0';

	/* This is different to allow for a blank format for the get event. */
	tempFormat[length5] = '\0';
	(void) strcpy (procDesc->clientReq.format, ims_truncStr (tempFormat));

	/*
	** Determine the GET request type.
	*/
	switch (requestType)
	{
		case 0:
			procDesc->clientReq.requestType = IMS_GET;
			break;

		case 1:
			procDesc->clientReq.requestType = IMS_GET_MAX;
			break;

		case 2:
			procDesc->clientReq.requestType = IMS_GET_LATEST;
			break;

		default:
			(void) sprintf (Msg, 
				"Invalid requestType in rpc_getEventBegin.");
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

			/* Populate the CS_SERVERMSG structure. */
			srvMsg.msgnumber = FTS_RPC_ERROR;
			(void) strcpy (srvMsg.text, Msg);
			srvMsg.textlen = (CS_INT) strlen (Msg);

			(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
			return (IMS_FATAL);
	}

	/*
	** Search for the dataset information in the supported 
	** dataset list.
	*/
	if ((status = checkDatasetPolicy (srvproc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Process the granule transfer request.
	*/
	status = ims_getEventBegin (srvproc); 

	return (status);
}

/******************************************************************************
**
** rpc_getEventEnd ()
**
******************************************************************************/

static int rpc_getEventEnd (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_getEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg, 
			"Process descriptor is NULL in rpc_getEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf(Msg,
			"Client login not valid in rpc_getEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	status = ims_getEventEnd (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_addEventBegin ()
**
** Handler for the addEventBegin remote procedure call.
**
** rpc_addEventBegin @platform, @sensor, @dataset, @name, @format
**
******************************************************************************/

static int rpc_addEventBegin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_DATAFMT paramFormat;
	CS_SERVERMSG srvMsg;
	int status;
	int length1;
	int length2;
	int length3;
	int length4;
	int length5;
	int length6;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = length3 = length4 = length5 = length6 =  0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_addEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_addEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_addEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* platform */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) procDesc->clientReq.platform,
		(CS_INT *) &length1, CS_GOODDATA);

	/* sensor */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) procDesc->clientReq.sensor,
		(CS_INT *) &length2, CS_GOODDATA);

	/* dataset */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) procDesc->clientReq.dataset,
		(CS_INT *) &length3, CS_GOODDATA);

	/* name */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat, (CS_BYTE *) procDesc->clientReq.name,
		(CS_INT *) &length4, CS_GOODDATA);

	/* format */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat, (CS_BYTE *) procDesc->clientReq.format,
		(CS_INT *) &length5, CS_GOODDATA);

	/* account id */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat, (CS_BYTE *) procDesc->clientReq.accountId,
		(CS_INT *) &length6, CS_GOODDATA);

	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get parameter data in rpc_addEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 6) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL30_LEN) ||
		(length3 == 0) || (length3 > IMS_COL80_LEN) ||
		(length4 == 0) || (length4 > IMS_COL30_LEN) ||
		(length5 == 0) || (length5 > IMS_COL10_LEN) ||
		(length6 == 0) || (length6 > IMS_COL15_LEN))

	{
		(void) sprintf (Msg,
			"Error in parameter list in rpc_addEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character fields.
	*/
	procDesc->clientReq.platform[length1] = '\0';
	procDesc->clientReq.sensor[length2] = '\0';
	procDesc->clientReq.dataset[length3] = '\0';
	procDesc->clientReq.name[length4] = '\0';
	procDesc->clientReq.format[length5] = '\0';
	procDesc->clientReq.accountId[length6] = '\0';

	/*
	** Fill in the known values.
	*/
	procDesc->clientReq.requestType = IMS_ADD;
	procDesc->clientReq.version = (CS_SMALLINT) -1;

	/*
	** Search for the dataset information in the supported 
	** dataset list.
	*/
	if ((status = checkDatasetPolicy (srvproc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Process the granule addition request.
	*/
	status = ims_addEventBegin (srvproc); 

	return (status);
}

/******************************************************************************
**
** rpc_replaceEventBegin ()
**
******************************************************************************/

static int rpc_replaceEventBegin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_DATAFMT paramFormat;
	CS_SERVERMSG srvMsg;
	int status;
	int length1;
	int length2;
	int length3;
	int length4;
	int length5;
	int length6;
	int length7;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = length3 = length4 = length5 = length6 = length7 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_replaceEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_replaceEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_replaceEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* platform */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) procDesc->clientReq.platform,
		(CS_INT *) &length1, CS_GOODDATA);

	/* sensor */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) procDesc->clientReq.sensor,
		(CS_INT *) &length2, CS_GOODDATA);

	/* dataset */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) procDesc->clientReq.dataset,
		(CS_INT *) &length3, CS_GOODDATA);

	/* name */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat, (CS_BYTE *) procDesc->clientReq.name,
		(CS_INT *) &length4, CS_GOODDATA);

	/* format */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat, (CS_BYTE *) procDesc->clientReq.format,
		(CS_INT *) &length5, CS_GOODDATA);

	/* version */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat, (CS_BYTE *) &(procDesc->clientReq.version),
		(CS_INT *) &length6, CS_GOODDATA);

	/* account id */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 7, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 7, &paramFormat, (CS_BYTE *) procDesc->clientReq.accountId,
		(CS_INT *) &length7, CS_GOODDATA);

	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get parameter data in rpc_replaceEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 7) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL30_LEN) ||
		(length3 == 0) || (length3 > IMS_COL80_LEN) ||
		(length4 == 0) || (length4 > IMS_COL30_LEN) ||
		(length5 == 0) || (length5 > IMS_COL10_LEN) ||
		(length6 != sizeof (procDesc->clientReq.version)) ||
		(length7 == 0) || (length6 > IMS_COL15_LEN))

	{
		(void) sprintf (Msg,
			"Error in parameter list in rpc_replaceEventBegin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character fields.
	*/
	procDesc->clientReq.platform[length1] = '\0';
	procDesc->clientReq.sensor[length2] = '\0';
	procDesc->clientReq.dataset[length3] = '\0';
	procDesc->clientReq.name[length4] = '\0';
	procDesc->clientReq.format[length5] = '\0';
	procDesc->clientReq.accountId[length7] = '\0';

	/*
	** Fill in the known values.
	*/
	procDesc->clientReq.requestType = IMS_REPLACE;

	/*
	** Search for the dataset information in the supported 
	** dataset list.
	*/
	if ((status = checkDatasetPolicy (srvproc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Process the granule replacement request.
	*/
	status = ims_replaceEventBegin (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_fileFragment ()
**
** This rpc handler is responsible for accepting 
** file fragments transmitted by the client.
**
** Note: The fileFragment handler does not open or close the file.
**
******************************************************************************/

static int rpc_fileFragment (
	SRV_PROC *srvproc)
{
	int status;

	if ((status = ims_srvAcceptFile (srvproc)) < IMS_OK)
	{
		return (status);
	}

	return (status);
}

/******************************************************************************
**
** rpc_krbFragment ()
**
** This rpc handler is responsible for accepting 
** Kerberos encrypted ticket from the client.
**
******************************************************************************/

#ifdef KRB
static int rpc_krbFragment (SRV_PROC *srvproc)
{
	int status;

	status = ims_srvAcceptKrbTicket (srvproc);

	return (status);
}
#endif	/* KRB */

/******************************************************************************
**
** rpc_validateLogin ()
**
** This rpc handler is responsible for validating client login connection.
**
******************************************************************************/

static int rpc_validateLogin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_DATAFMT paramFormat;
	CS_DATAFMT paramFormat2;
	CS_DATAFMT paramFormat3;
	CS_DATAFMT paramFormat4;
	CS_SERVERMSG srvMsg;
/*	IMS_SERVER_KRB *sKrb; */
	int status;
	int length1, length2, length3, length4;
	int paramCount;
	int clientPid;
	char clientMachine[IMS_COL30_LEN+1];
	char clientUser[IMS_COL30_LEN+1];

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_validateLogin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_validateLogin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/* 
	** Bind the parameters for the RPC call */


	/* account id */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) procDesc->clientReq.accountId,
		(CS_INT *) &length1, CS_GOODDATA);

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if (paramCount > 1)
	{
		/*
		** Foreign Client User Name.
		*/

		(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat2);

		(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) 2, &paramFormat2, (CS_BYTE *) clientUser,
			(CS_INT *) &length2, CS_GOODDATA);

		/*
		** Foreign Client Machine Name.
		*/

		(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) 3, &paramFormat3);

		(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) 3, &paramFormat3, (CS_BYTE *) clientMachine,
			(CS_INT *) &length3, CS_GOODDATA);

		/*   
		** Foreign Client Process Id.
		*/

		(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) 4, &paramFormat4);
	
		(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
			(CS_INT) 4, &paramFormat4, (CS_BYTE *) &clientPid,
			(CS_INT *) &length4, CS_GOODDATA);
	}

	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get parameter data in rpc_validateLogin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (paramCount < 1) 
	{
		(void) sprintf (Msg,
			"Error in parameter list in rpc_validateLogin.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);

	}

	procDesc->clientReq.accountId[length1] = '\0';

	if (paramCount > 1)
	{
		/*
		** Populate the client connection information in procDesc.
		*/

		clientUser[length2] = '\0';
		clientMachine[length3] = '\0';
		
		strcpy(procDesc->clientUser, clientUser);
		strcpy(procDesc->clientMachine, clientMachine);
		procDesc->clientPid = clientPid;
	}

/*	sKrb = &(procDesc->sKrb); */

	/* 
	** Check whether there is a Kerberos ticket.
	*/
#ifdef KRB
	if (sKrb->ktxt.length > 0)
	{
		if ((status = validateKrbTicket (sKrb)) < IMS_OK)
		{
			if (strcmp (procDesc->client.login, " ") == 0)
			{
				(void) sprintf (Msg,
					"Kerberos Error, %s",
					krb_err_txt[sKrb->msgIndex]);
				(void) ims_msg (procDesc->msgStruct,
					status, Msg);
				return (status);
			}
		}
		else /* we have a valid ticket. */
		{
			if ((strcmp (procDesc->client.login, sKrb->ad.pname) ==
				0) ||
				(strcmp (procDesc->client.login, " ") == 0))
			{
				procDesc->userName = sKrb->ad.pname;
				procDesc->validUser = 1;

				return (IMS_OK);
			}
		}
	}
#endif	/* KRB */

	if ((status = ims_validateClientLogin (srvproc)) < IMS_OK)
	{
		return (status);
	}

	procDesc->userName = procDesc->client.login;
	procDesc->validUser = 1;

	return (IMS_OK);
}

/******************************************************************************
**
** rpc_addEventEnd ()
**
** This rpc handler is in charge of the remaining 
** tasks required for the file tranfer operation
** into IMS storage.  At this point a copy of the 
** transferred file is available in IMS storage.  
**
******************************************************************************/

static int rpc_addEventEnd (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_addEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_addEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_addEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	status = ims_addEventEnd (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_replaceEventEnd ()
**
** This rpc handler is in charge of the remaining 
** tasks required for the file replace operation
** in IMS storage.  At this point a copy of the 
** transferred file is available in IMS storage.  
**
******************************************************************************/

static int rpc_replaceEventEnd (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_replaceEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_replaceEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_replaceEventEnd.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	status = ims_replaceEventEnd (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_deleteEvent ()
**
******************************************************************************/

static int rpc_deleteEvent (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_DATAFMT paramFormat;
	CS_SERVERMSG srvMsg;
	int status;
	int length1;
	int length2;
	int length3;
	int length4;
	int length5;
	int length6;
	int paramCount;

	procDesc = (FTS_PROC_DESC *) NULL;
	length1 = length2 = length3 = length4 = length5 = length6 = 0;
	paramCount = 0;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_deleteEvent.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_deleteEvent.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_deleteEvent.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* platform */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) procDesc->clientReq.platform,
		(CS_INT *) &length1, CS_GOODDATA);

	/* sensor */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) procDesc->clientReq.sensor,
		(CS_INT *) &length2, CS_GOODDATA);

	/* dataset */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) procDesc->clientReq.dataset,
		(CS_INT *) &length3, CS_GOODDATA);

	/* name */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat, (CS_BYTE *) procDesc->clientReq.name,
		(CS_INT *) &length4, CS_GOODDATA);

	/* version */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 5, &paramFormat, (CS_BYTE *) &(procDesc->clientReq.version),
		(CS_INT *) &length5, CS_GOODDATA);

	/* account id */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 6, &paramFormat, (CS_BYTE *) procDesc->clientReq.accountId,
		(CS_INT *) &length6, CS_GOODDATA);

	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get parameter data in rpc_deleteEvent.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 6) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL30_LEN) ||
		(length3 == 0) || (length3 > IMS_COL80_LEN) ||
		(length4 == 0) || (length4 > IMS_COL30_LEN) ||
		(length5 != sizeof (procDesc->clientReq.version)) ||
		(length6 == 0) || (length6 > IMS_COL15_LEN))

	{
		(void) sprintf (Msg,
			"Error in parameter list in rpc_deleteEvent.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character fields.
	*/
	procDesc->clientReq.platform[length1] = '\0';
	procDesc->clientReq.sensor[length2] = '\0';
	procDesc->clientReq.dataset[length3] = '\0';
	procDesc->clientReq.name[length4] = '\0';

	/*
	** Fill in the known values.
	*/
	procDesc->clientReq.requestType = IMS_DELETE;

	/*
	** Search for the dataset information in the supported 
	** dataset list.
	*/
	if ((status = checkDatasetPolicy (srvproc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Process the granule deletion request.
	*/
	status = ims_deleteEvent (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_validateMetadata ()
**
******************************************************************************/

static int rpc_validateMetadata (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_validateMetadata.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_validatMetadata.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_validatMetadata.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Process the validation request.
	*/
	status = ims_validateMetadata (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_openFile ()
**
******************************************************************************/

static int rpc_openFile (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_openFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_openFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_openFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Process the open file request.
	*/
	status = ims_srvOpenFile (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_closeFile ()
**
******************************************************************************/

static int rpc_closeFile (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;


	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_closeFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_closeFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_closeFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Process the close file request.
	*/
	status = ims_srvCloseFile (srvproc);

	return (status);
}

/******************************************************************************
**
** rpc_transferFileName ()
**
******************************************************************************/

static int rpc_transferFileName (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_transferFileName.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_transferFileName.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_transferFileName.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Process the transfer file name request.
	*/
	status = ims_srvSendFileName (srvproc);

	return (status);
}
/******************************************************************************
**
** rpc_transferFile ()
**
******************************************************************************/

static int rpc_transferFile (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_GRANULE_DESC *granuleDesc;
	CS_SERVERMSG srvMsg;
	int status;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_transferFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_transferFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	granuleDesc = &(procDesc->granuleDesc);

	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_transferFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Process the close file request.
	*/
	status = ims_srvSendFile (srvproc);

	/*
	** Increment currFile pointer 
	*/
	granuleDesc->currFile = granuleDesc->currFile->next;

	return (status);
}

/******************************************************************************
**
** rpc_checkFileTypes ()
**
******************************************************************************/

static int rpc_checkFileTypes (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	IMS_FILE_TYPES *fileTypes;
	IMS_FILE_TYPES *ptr;
	FTS_GRANULE_DESC *granuleDesc;
	CS_SERVERMSG srvMsg;
	AUX_CAT_REQUEST auxReq;
	AUX_INFO_SPEC auxSpec;
	CS_DATAFMT paramFormat;
	int paramCount;
	int status;
	int length1;
	int length2;
	int length3;
	int length4;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_transferFile.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	/*
	** Unpack the RPC client parameters.
	*/

	srv_bzero ((CS_VOID *) &paramFormat, (CS_INT) sizeof (paramFormat));

	/*
	** Bind the parameters with their descriptions.
	*/

	/* platform */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 1, &paramFormat, (CS_BYTE *) procDesc->clientReq.platform,
		(CS_INT *) &length1, CS_GOODDATA);

	/* sensor */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 2, &paramFormat, (CS_BYTE *) procDesc->clientReq.sensor,
		(CS_INT *) &length2, CS_GOODDATA);

	/* dataset */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 3, &paramFormat, (CS_BYTE *) procDesc->clientReq.dataset,
		(CS_INT *) &length3, CS_GOODDATA);

	/* format */
	(CS_VOID) srv_descfmt (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat);

	(CS_VOID) srv_bind (srvproc, CS_GET, SRV_RPCDATA,
		(CS_INT) 4, &paramFormat, (CS_BYTE *) procDesc->clientReq.format,
		(CS_INT *) &length4, CS_GOODDATA);
	/*
	** Get the parameters from the client.
	*/
	if (srv_xferdata (srvproc, CS_GET, SRV_RPCDATA) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Could not get parameter data in rpc_checkFileTypes.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Determine the number of parameters passed in and whether they
	** exceed their maximum lengths.
	*/
	(CS_VOID) srv_numparams (srvproc, (CS_INT *) &paramCount);

	if ((paramCount < 4) ||
		(length1 == 0) || (length1 > IMS_COL30_LEN) ||
		(length2 == 0) || (length2 > IMS_COL30_LEN) ||
		(length3 == 0) || (length3 > IMS_COL80_LEN) ||
		(length4 == 0) || (length4 > IMS_COL10_LEN))
	{
		(void) sprintf (Msg,
			"Error in parameter list in rpc_checkFileTypes.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Null terminate the character fields.
	*/
	procDesc->clientReq.platform[length1] = '\0';
	procDesc->clientReq.sensor[length2] = '\0';
	procDesc->clientReq.dataset[length3] = '\0';
	procDesc->clientReq.format[length4] = '\0';

	fileTypes = (IMS_FILE_TYPES *) ims_checkServerFileTypes(srvproc);

	/*
	** Send file types list to client.
	*/

	status = ims_sendFileTypes(srvproc, fileTypes);

	/*
	** Free File Types 
	*/

	while (procDesc->fileTypes != NULL)
	{
		ptr = procDesc->fileTypes->next;
		free(procDesc->fileTypes);	
		procDesc->fileTypes = ptr;
	}

	return(status);
}

/******************************************************************************
**
** rpc_getVersionNumber ()
**
******************************************************************************/

static int rpc_getVersionNumber (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	CS_DATAFMT paramFormat;
	int paramCount;
	int status;
	int length1;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_getVersionNumber.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_getVersionNumber.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}


	/*
	** Make sure that the client login is validated.
	*/
	if (procDesc->validUser <= 0)
	{
		(void) sprintf (Msg,
			"Client login not valid in rpc_getVersionNumber.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	status = ims_sendVersionNumber(srvproc);
	return(status);
}

/******************************************************************************
**
** rpc_who ()
**
******************************************************************************/

static int rpc_who (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *tempProc;
	FTS_SRVPROC_LIST *listPtr;
	FTS_CLIENT_REQUEST *clientReq;
	CS_DATAFMT format1;
	CS_DATAFMT format2;
	CS_DATAFMT format3;
	CS_DATAFMT format4;
	CS_DATAFMT format5;
	CS_DATAFMT format6;
	int spid;
	char username[IMS_COL30_LEN+1];
	char clientEvent[IMS_COL30_LEN+1];
	char hostname[IMS_COL128_LEN+1];
	char useracct[IMS_COL30_LEN+1];
	char host_proc_id[IMS_COL10_LEN+1];
	int process_id;
	int length1;
	int length2;
	int length3;
	int length4;
	int length5;
	int length6;

	listPtr = glb_srvProcList;

	/*
	** Initialize the CS_DATAFMT structure.
	*/
	srv_bzero ((CS_VOID *) &format1, (CS_INT) sizeof (format1));
	srv_bzero ((CS_VOID *) &format2, (CS_INT) sizeof (format2));
	srv_bzero ((CS_VOID *) &format3, (CS_INT) sizeof (format3));
	srv_bzero ((CS_VOID *) &format4, (CS_INT) sizeof (format4));
	srv_bzero ((CS_VOID *) &format5, (CS_INT) sizeof (format5));
	srv_bzero ((CS_VOID *) &format6, (CS_INT) sizeof (format6));

	/*
	** Describe the columns.
	*/

	/* Process ID */
	(void) strcpy (format1.name, "SPID");
	format1.namelen = (CS_INT) strlen (format1.name);
	format1.datatype = CS_INT_TYPE;
	format1.maxlength = (CS_INT) sizeof (int);

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 1, &format1);

	/* Username */
	(void) strcpy (format2.name, "Username");
	format2.namelen = (CS_INT) strlen (format2.name);
	format2.datatype = CS_CHAR_TYPE;
	format2.maxlength = IMS_COL30_LEN;

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 2, &format2);

	/* Client Event */
	(void) strcpy (format3.name, "Client Event");
	format3.namelen = (CS_INT) strlen (format3.name);
	format3.datatype = CS_CHAR_TYPE;
	format3.maxlength = IMS_COL30_LEN;

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 3, &format3);

	/*
	** Host
	*/ 
	(void) strcpy (format4.name, "Host Name");
	format4.namelen = (CS_INT) strlen (format4.name);
	format4.datatype = CS_CHAR_TYPE;
	format4.maxlength = IMS_COL64_LEN;

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 4, &format4);

	/*
	** User Account
	*/
	(void) strcpy (format5.name, "Host Account");
	format5.namelen = (CS_INT) strlen (format5.name);
	format5.datatype = CS_CHAR_TYPE;
	format5.maxlength = IMS_COL30_LEN;

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 5, &format5);

	/*
	** Client Process ID
	*/

	(void) strcpy (format6.name, "Process ID");
	format6.namelen = (CS_INT) strlen (format6.name);
	format6.datatype = CS_CHAR_TYPE;
	format6.maxlength = IMS_COL10_LEN;

	(CS_VOID) srv_descfmt (srvproc, CS_SET, SRV_ROWDATA,
		(CS_INT) 6, &format6);

	/*
	** Send a row for each thread recorded in the srvproc list.
	*/
	while (listPtr != (FTS_SRVPROC_LIST *) NULL)
	{
		/* Clean out the buffers. */
		username[0] = '\0';
		clientEvent[0] = '\0';

		/* Get the thread identifier. */
		(CS_VOID) srv_thread_props (listPtr->srvproc, CS_GET, SRV_T_SPID,
			(CS_VOID *) &spid, (CS_INT) sizeof (spid),
			(CS_INT *) &length1);

		/* Get the client username for the current thread. */
		(CS_VOID) srv_thread_props (listPtr->srvproc, CS_GET, SRV_T_USER,
			(CS_VOID *) username, (CS_INT) sizeof (username),
			(CS_INT *) &length2);
		
		if (length2 == 0)
		{
			(void) strcpy (username, "NULL");
			length2 = 4;
		}

		/* Get the process descriptor for the current thread. */
		(CS_VOID) srv_thread_props (listPtr->srvproc, CS_GET, SRV_T_USERDATA,
			(CS_VOID *) &tempProc, (CS_INT) sizeof (tempProc),
			(CS_INT *) NULL);

		/* Determine the user event if any. */
		if (listPtr->srvproc != srvproc)
		{
			if (tempProc != (FTS_PROC_DESC *) NULL)
			{
				switch (tempProc->clientReq.requestType)
				{
				case IMS_ADD:
					(void) strcpy (clientEvent, "Add Event");
					break;

				case IMS_GET:
				case IMS_GET_MAX:
				case IMS_GET_LATEST:
					(void) strcpy (clientEvent, "Get Event");
					break;

				case IMS_DELETE:
					(void) strcpy (clientEvent, "Delete Event");
					break;

				case IMS_REPLACE:
					(void) strcpy (clientEvent, "Replace Event");
					break;

				default:
					(void) strcpy (clientEvent, "NULL");
					break;
				}
			}

			/* 
			** Client's hostname.
			*/


			length4 = strlen(tempProc->clientMachine);
			(void) strcpy(hostname, tempProc->clientMachine);
		
			if (length4 == 0)
			{
				(void) strcpy (hostname, "NULL");
				length4 = 4;
			}

			/* 
			** Client User Account
			*/

			length5 = strlen(tempProc->clientUser);
			(void) strcpy(useracct, tempProc->clientUser);
		
		
			if (length5 == 0)
			{
				(void) strcpy (useracct, "NULL");
				length5 = 4;
			}

			/*
			** Client Process ID.
			*/

			sprintf(host_proc_id, "%d", tempProc->clientPid);
			length6 = strlen(host_proc_id);

		}

		else
		{
			/* This is the current process. */
			(void) strcpy (clientEvent, "Current Process");
			(void) strcpy(hostname, "UNAVAILABLE");
			(void) strcpy(useracct, "UNAVAILABLE");
			(void) strcpy(host_proc_id, "NULL");
		}


		/* Bind the columns. */
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 1, &format1, (CS_BYTE *) &spid,
			(CS_INT *) &length1, CS_GOODDATA);

		length2 = strlen (username);
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 2, &format2, (CS_BYTE *) username,
			(CS_INT *) &length2, CS_GOODDATA);

		length3 = strlen (clientEvent);
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 3, &format3, (CS_BYTE *) clientEvent,
			(CS_INT *) &length3, CS_GOODDATA);

		length4 = strlen (hostname);
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 4, &format4, (CS_BYTE *) hostname,
			(CS_INT *) &length4, CS_GOODDATA);

		length5 = strlen (useracct);
		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 5, &format5, (CS_BYTE *) useracct,
			(CS_INT *) &length5, CS_GOODDATA);


		length6 = strlen(host_proc_id);

		(CS_VOID) srv_bind (srvproc, CS_SET, SRV_ROWDATA,
			(CS_INT) 6, &format6, (CS_BYTE *) host_proc_id,
			(CS_INT *) &length6, CS_GOODDATA);

		/* Send the row. */
		(CS_VOID) srv_xferdata (srvproc, CS_SET, SRV_ROWDATA);


		listPtr = listPtr->next;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** rpc_stop ()
**
******************************************************************************/

static int rpc_stop (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_PROC_DESC *tempProc;
	FTS_SRVPROC_LIST *listPtr;
	CS_SERVERMSG srvMsg;
	char username[IMS_COL30_LEN+1];
	int nameLength;
	int cleanUpDone;
	int threadCount;
	int status;
	int threadId;

	procDesc = (FTS_PROC_DESC *) NULL;
	status = IMS_OK;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Only the 'sa' and 'ims_srv' accounts can perform this RPC.
	*/
	(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_USER,
		(CS_VOID *) username, (CS_INT) sizeof (username),
		(CS_INT *) &nameLength);

	username[nameLength] = '\0';

	if ((strcmp (username, "sa") != 0) &&
		(strcmp (username, "ims_srv") != 0))
	{
		(void) sprintf (Msg, 
			"Invalid user '%s' attempting to shutdown server.\n",
			username);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

#ifdef PROCESS
	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in rpc_stop.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in rpc_stop.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA + 1) < IMS_OK)
	{
		(void) sprintf (Msg, 
			"Changing priority for the rpc_stop srvproc failed.\n");
	 	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
		status = IMS_FATAL;
	}

	if (addProcessEndTime (procDesc->msgStruct) < IMS_OK)
	{
		(void) sprintf (Msg, 
			"Setting process end time in process table failed.\n");
	 	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
		status = IMS_FATAL;
	}

	if (ims_changePriority (srvproc, -1*(FTS_PRIORITY_DELTA+1)) < IMS_OK)
	{
		(void) sprintf (Msg, 
			"Changing priority for the rpc_stop srvproc failed.\n");
	 	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
		status = IMS_FATAL;
	}
#endif	/* PROCESS */

	/*
	** Terminate all client event processes before shutdown
	** that have an FTS event associated with them.
	*/
	listPtr = glb_srvProcList;
	while (listPtr != (FTS_SRVPROC_LIST *) NULL)
	{
		/* Get the process descriptor for the current thread. */
		(CS_VOID) srv_thread_props (listPtr->srvproc, CS_GET, SRV_T_USERDATA,
			(CS_VOID *) &tempProc, (CS_INT) sizeof (tempProc),
			(CS_INT *) NULL);

		/* We only care about threads with a process descriptor. */
		if (tempProc != (FTS_PROC_DESC *) NULL)
		{
			/* Only disconnect threads which have an FTS event. */
			if (tempProc->clientReq.requestType != IMS_NO_REQUEST)
			{
				/* Raise the priority for these threads. */
				(CS_VOID) srv_setpri (listPtr->srvproc, SRV_C_NEWPRI,
					SRV_C_MAXPRIORITY);

				if (srv_event (listPtr->srvproc, SRV_DISCONNECT,
					(CS_VOID *) NULL) == CS_FAIL)
				{
					(void) sprintf (Msg, "SRV_DISCONNECT event failed.\n");
					(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg,
						CS_NULLTERM);
					status = IMS_FATAL;
				}
			}
		}

		listPtr = listPtr->next;
	}

	/*
	** Lower our priority to let other processes finish up.
	*/
	if (srv_setpri (srvproc, SRV_C_NEWPRI, SRV_C_LOWPRIORITY) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Lowering priority for the rpc_stop thread failed.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
	}

	/*
	** Wait for the disconnected processes to clean themselves up.
	*/
	cleanUpDone = IMS_FALSE;
	while (cleanUpDone != IMS_TRUE)
	{
		threadCount = 0;
		listPtr = glb_srvProcList;
		while (listPtr != (FTS_SRVPROC_LIST *) NULL)
		{
			/* Get the process descriptor for the current thread. */
			(CS_VOID) srv_thread_props (listPtr->srvproc, CS_GET,
				SRV_T_USERDATA, (CS_VOID *) &tempProc,
				(CS_INT) sizeof (tempProc), (CS_INT *) NULL);

			/* We only care about threads with a process descriptor. */
			if (tempProc != (FTS_PROC_DESC *) NULL)
			{
				/*
				** We don't count NO_REQUEST and any GET events as
				** active threads.  If there is still a GET event
				** running when we kill the server, it is OK because
				** it won't effect the catalog.
				*/
				if ((tempProc->clientReq.requestType != IMS_NO_REQUEST) &&
					(tempProc->clientReq.requestType != IMS_GET) &&
					(tempProc->clientReq.requestType != IMS_GET_MAX) &&
					(tempProc->clientReq.requestType != IMS_GET_LATEST))
				{
					threadCount++;
				}
			}

			listPtr = listPtr->next;
		}

		if (threadCount == 0)
		{
			cleanUpDone = IMS_TRUE;
		}
		else
		{
#ifdef IMS_DEBUG
			(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
				(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
				(CS_INT *) NULL);

			(void) fprintf (stderr,
				"\nID = %d: Waiting for %d threads to clean-up.\n",
				threadId, threadCount);
			(void) fflush (stderr);
#endif	/* IMS_DEBUG */

			/* Yield to other threads. */
			(CS_VOID) srv_yield();
		}
	}

	/*
	** It is now time to shutdown the server.
	*/
	if (srv_event_deferred (srvproc, SRV_STOP, (CS_VOID *) NULL) != -1)
	{
		(void) sprintf (Msg,
			"SHUTTING DOWN File Transfer Server '%s'.\n",
			glb_ftsSrvName);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_INFO;
		srvMsg.severity = SRV_INFO;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
	}
	else
	{
		(void) sprintf (Msg,
			"Could not SHUT DOWN File Transfer Server '%s'.\n",
			glb_ftsSrvName);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_RPC_ERROR;
		srvMsg.severity = SRV_FATAL_PROCESS;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		status = IMS_FATAL;
	}

	return (status);
}

/******************************************************************************
**
** checkDatasetPolicy ()
**
** Search the cache memory for the client requested
** information based on platform, sensor,
** and dataset.  This routine sets pointers to
** procDesc->sensor and procDesc->dataset.
**
** Note: This routine must not return a IMS_FATAL value because mission
** information is not yet found and adding a sigEvent event to the
** sigEvents table requires mission information.
**
******************************************************************************/

static int checkDatasetPolicy (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;
	CS_SERVERMSG srvMsg;

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in checkDatasetPolicy.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in checkDatasetPolicy.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	procDesc->sensorPolicy = (FTS_SENSOR_POLICY *) glb_sensorPolicy;
	procDesc->datasetPolicy  = (FTS_DATASET_POLICY *) NULL;

	while (procDesc->sensorPolicy != (FTS_SENSOR_POLICY *) NULL)
	{
		if ((strcmp (procDesc->sensorPolicy->platform,
			procDesc->clientReq.platform) == 0) &&
			(strcmp (procDesc->sensorPolicy->sensor,
			procDesc->clientReq.sensor) == 0)) 
		{
			break;
		}
		procDesc->sensorPolicy = procDesc->sensorPolicy->next;
	}

	if (procDesc->sensorPolicy == (FTS_SENSOR_POLICY *) NULL)
	{
		(void) sprintf (Msg,
			"Policy for '%s, %s' is not supported by FTS server '%s' . Possible installation failure.  Contact the DBA.",
			procDesc->clientReq.platform,
			procDesc->clientReq.sensor,
			procDesc->ftsSrvName);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_POLICY_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Platform/sensor information is found in the above search and now
	** search for the dataset policy information from the hash table
	** created in cache memory.
	*/
	item.key = procDesc->clientReq.dataset;

	if ((hashEntry = ims_hashSearch (procDesc->sensorPolicy->hashPtr,
		&item, IMS_FIND, procDesc->msgStruct)) ==
		(IMS_HASH_ENTRY *) NULL)
	{
		(void) sprintf (Msg,
			"Policy for '%s, %s, %s' is not supported by FTS server '%s'. Possible installation failure. Contact the DBA.",
			procDesc->clientReq.platform,
			procDesc->clientReq.sensor,
			procDesc->clientReq.dataset,
			procDesc->ftsSrvName);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_POLICY_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Dataset policy information returned.
	*/
	procDesc->datasetPolicy = (FTS_DATASET_POLICY *) hashEntry->data;

	/*
	** Given the current dataset, set up a pointer to the file policy
	** for the format specified.
	*/
	procDesc->filePolicy = procDesc->datasetPolicy->filePolicy;

	/*
	** For the delete and get events we will just point to the
	** first format. We will determine file policy later based on
	** the format returned from the catalog query.
	*/
	if ((procDesc->clientReq.requestType == IMS_DELETE) ||
		(procDesc->clientReq.requestType == IMS_GET) ||
		(procDesc->clientReq.requestType == IMS_GET_MAX) ||
		(procDesc->clientReq.requestType == IMS_GET_LATEST))
	{
		return (IMS_OK);
	}

	/* For all other events we will point to the requested format. */
	while (procDesc->filePolicy != (FTS_FILE_POLICY *) NULL)
	{
		if (strcmp (procDesc->filePolicy->format,
			procDesc->clientReq.format) == 0)
		{
			break;
		}
		procDesc->filePolicy = procDesc->filePolicy->next;
	}

	if (procDesc->filePolicy == (FTS_FILE_POLICY *) NULL)
	{
		(void) sprintf (Msg,
			"Policy for '%s, %s, %s, %s' is not supported by FTS server '%s' . Possible installation failure.  Contact the DBA.",
			procDesc->clientReq.platform,
			procDesc->clientReq.sensor,
			procDesc->clientReq.dataset,
			procDesc->clientReq.format,
			procDesc->ftsSrvName);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_POLICY_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	return (IMS_OK);
}

/******************************************************************************
**
** addSrvProcToGlbList ()
**
******************************************************************************/

static int addSrvProcToGlbList (
	SRV_PROC *srvproc)
{
	FTS_SRVPROC_LIST *listPtr;

	/*
	** Allocate space for the FTS_SRVPROC_LIST structure.
	**
	** lint: pointer cast may result in improper alignment
	** ???? Review this.
	*/
	if ((listPtr = (FTS_SRVPROC_LIST *) malloc (sizeof (FTS_SRVPROC_LIST)))
		== (FTS_SRVPROC_LIST *) NULL)
	{
		(void) sprintf (Msg, 
			"Memory allocation failed for FTS_SRVPROC_LIST structure. Could not add srvproc to glb_srvProcList.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg,
			CS_NULLTERM);
		return (IMS_FATAL);
	}

	if (glb_srvProcList == (FTS_SRVPROC_LIST *) NULL)
	{
		listPtr->next = (FTS_SRVPROC_LIST *) NULL;
		listPtr->srvproc = srvproc;
		glb_srvProcList = listPtr;
	}
	else
	{
		listPtr->next = glb_srvProcList;
		listPtr->srvproc = srvproc;
		glb_srvProcList = listPtr;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** removeSrvProcFromGlbList ()
**
******************************************************************************/

static int removeSrvProcFromGlbList (
	SRV_PROC *srvproc)
{
	FTS_SRVPROC_LIST *currPtr;
	FTS_SRVPROC_LIST *prevPtr;
	int found;

	currPtr = prevPtr = glb_srvProcList;
	found = 0;

	while (currPtr != (FTS_SRVPROC_LIST *) NULL)
	{
		if (currPtr->srvproc == srvproc)
		{
			found = 1;
			break;
		}
		prevPtr = currPtr;
		currPtr = currPtr->next;
	}

	if (found)
	{
		if (currPtr == glb_srvProcList)
		{
			glb_srvProcList = currPtr->next;
		}

		prevPtr->next = currPtr->next;
		(void) free (currPtr);
		return (IMS_OK);
	}
	else
	{
		(void) sprintf (Msg, 
			"Did not find srvproc in glb_srvProcList.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg,
			CS_NULLTERM);
		return (IMS_FATAL);
	}
}

/******************************************************************************
**
** termHandler ()
**
******************************************************************************/

static void termHandler (
	int sig)
{
	SRV_PROC *srvproc;

	(void) sprintf (Msg,
		"Termination of %s due to signal: %s (%d).\n",
		glb_ftsProgramName, ims_sigMsg (sig), sig);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

	/*
	** Get the address of the current thread.
	*/
	if (srv_props (glbContext, CS_GET, SRV_S_CURTHREAD, 
		(CS_VOID *) &srvproc, (CS_INT) sizeof (srvproc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Getting server configuration 'SRV_S_CURTHREAD' failed.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
	}

	if (srvproc != (SRV_PROC *) NULL)
	{
		(void) sprintf (Msg,
			"SHUTTING DOWN File Transfer Server: '%s'.\n", 
			glb_ftsSrvName);
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

#ifdef PROCESS
		(void) sprintf (Msg,
			"Has not set 'endTime' in the processes table.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
#endif	/* PROCESS */

		/*
		** Raise the event to stop the server.
		*/
		if (srv_event_deferred (srvproc, SRV_STOP, (CS_VOID *) NULL) == -1)
		{
			(void) sprintf (Msg, "Normal SHUTDOWN failed.\n");
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
			exit (0);
		}
		return;
	}
	else
	{
		exit (0);
	}
}

/******************************************************************************
**
** pipeHandler ()
**
******************************************************************************/

static void pipeHandler (
	int sig)
{
	(void) sprintf (Msg,
		"Termination of client connection due to signal: %s (%d).\n",
		ims_sigMsg (sig), sig);
	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

#ifdef IMS_DEBUG
	(void) fprintf (stderr, Msg);
	(void) fflush (stderr);
#endif	/* IMS_DEBUG */

	return;
}

/******************************************************************************
**
** childHandler ()
**
******************************************************************************/

static void childHandler (
	int sig)
{
	int retPid;
	int status;

    if ((retPid = wait(&status)) == -1)
	{
		(void) sprintf (Msg,
			"Wait for Child process Failed.");
		return;

	}

	(void) sprintf (Msg,
		"Termination of child process %d due to signal: %s (%d).\n",
		retPid, ims_sigMsg (sig), sig);

	(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

	/*
	** Re-instate the signal handler.
	*/

	if (signal(SIGCLD, childHandler) == (void (*)())-1)
	{
		(void) sprintf(Msg, "Could not setup signal handler for child processes.");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);
	}


	return;
}


/******************************************************************************
**
** sybToCdbSeverity () 
**
******************************************************************************/

static int sybToCdbSeverity (
	int sybSeverity)
{
	switch (sybSeverity)
	{
		case EXINFO:
			return (IMS_INFO);

		case EXUSER: 
		case EXNONFATAL:
			return (IMS_ERROR);

		default:
			return (IMS_FATAL);
	}
}

/******************************************************************************
**
** validateKrbTicket ()
**
** Validate a received Kerberos ticket.  Variables service, host,
** and ktxt.dat must be provided by the caller.
**
******************************************************************************/

#ifdef KRB
static int validateKrbTicket (IMS_SERVER_KRB *sKrb)
{
	char msg[1024];

	if ((sKrb->msgIndex = krb_rd_req (&(sKrb->ktxt), sKrb->service,
			sKrb->host, 0, &(sKrb->ad), "")) != KSUCCESS)	
	{
		return (IMS_ERROR);
	}
	return (IMS_OK);
}
#endif	/* KRB */


/******************************************************************************
**
** ims_bulkHandler
**
******************************************************************************/

CS_RETCODE ims_bulkHandler(SRV_PROC *srvproc)
{

	FTS_PROC_DESC *procDesc;
	CS_DATAFMT paramFormat;
	CS_SERVERMSG srvMsg;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	int status;
	int i;
	CS_INT iotype;
	int outlen;
	CS_IODESC iodesc;
	int ret;
	int count;
	char *buffer;
	int failCount = 0;

#ifdef IMS_DEBUG
	int threadId;

	(CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
		(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
		(CS_INT *) NULL);

	(void) fprintf (stderr, "\nID = %d: BULK DATA TRANSFER.\n", threadId);
	(void) fflush (stderr);
#endif	/* IMS_DEBUG */

	procDesc = (FTS_PROC_DESC *) NULL;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;
	
	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (Msg, 
			"Could not get SRV_T_USERDATA in ims_bulkHandler");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (Msg,
			"Process descriptor is NULL in ims_bulkHandler");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}

	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;

	/* get the current bulk type being sent by the client.
	** we only handle one type, but we need to make sure
	** its the right one...
	*/
	if (srv_thread_props(srvproc, CS_GET, SRV_T_BULKTYPE, (CS_VOID *)&iotype,
		CS_SIZEOF(iotype), (CS_INT *)NULL) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Not able to get the bulk type in ims_bulkHandler");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}

	if (iotype != SRV_IMAGELOAD)
	{
		(void) sprintf (Msg,
			"The bulk type is invalid in ims_bulkHandler");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}

	memset(&iodesc, 0, sizeof(iodesc));

	if (srv_text_info(srvproc, CS_GET, 1, &iodesc) == CS_FAIL)
	{
		(void) sprintf (Msg,
			"Getting text description failed in ims_bulkHandler");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}

	count = 0;


	if ((buffer = (void *) malloc(16000)) == NULL)
	{
		(void) sprintf (Msg,
			"Could not allocate buffer space to read bulk data");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, Msg);
		srvMsg.textlen = (CS_INT) strlen (Msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
			CS_TRAN_UNDEFINED, (CS_INT)0);
		return (CS_SUCCEED);
	}


	while (ret != CS_END_DATA)
	{
		if ((ret = srv_get_text(srvproc, (CS_VOID *) buffer, 16000,
				(CS_INT *) &outlen)) 
			== CS_FAIL)
		{
			free(buffer);
			(void) sprintf (Msg,
				"Not able to extract all data in ims_bulkHandler");
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

			/* Populate the CS_SERVERMSG structure. */
			srvMsg.msgnumber = FTS_THREAD_ERROR;
			(void) strcpy (srvMsg.text, Msg);
			srvMsg.textlen = (CS_INT) strlen (Msg);

			(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
			(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
				CS_TRAN_UNDEFINED, (CS_INT)0);
			return (CS_SUCCEED);
		}

		if (write(currFile->fd, buffer, 
					outlen) != outlen)
		{
		    /* we are out of disk space but we still have to empty out the 
		       socket's data streaming from the client */ 
		    while (ret != CS_END_DATA) { 
			ret = srv_get_text(srvproc, (CS_VOID *)buffer, 16000,
					   (CS_INT *)&outlen);
			/* protection from infinite loops */
			if (ret == CS_FAIL) {
			    if(++failCount > 25) exit(ERREXIT);
			}
		    }

			free(buffer);
			(void) sprintf (Msg,
				"Could not write to file in ims_bulkHandler");
			(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, Msg, CS_NULLTERM);

			/* Populate the CS_SERVERMSG structure. */
			srvMsg.msgnumber = FTS_THREAD_ERROR;
			(void) strcpy (srvMsg.text, Msg);
			srvMsg.textlen = (CS_INT) strlen (Msg);

			(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
			(CS_VOID) srv_senddone (srvproc, SRV_DONE_ERROR | SRV_DONE_FINAL,
				CS_TRAN_UNDEFINED, (CS_INT)0);
			return (CS_SUCCEED);
		}
		count += outlen;

	}


	free(buffer);
	(CS_VOID) srv_senddone(srvproc, SRV_DONE_FINAL, CS_TRAN_COMPLETED, 0);
	return(CS_SUCCEED);

}

/******************************************************************************
**
** ims_langHandler
**
** This is required by Sybase in order to handle the bulk transfer event.
******************************************************************************/
CS_RETCODE ims_langHandler(SRV_PROC *spp)
{
	CS_INT  iotype = SRV_IMAGELOAD;
	 
	/* set flag to tell server to expect bulk event */
	(void) srv_thread_props(spp, CS_SET, SRV_T_BULKTYPE,
		 &iotype, CS_SIZEOF(iotype), (CS_INT *)NULL);
			  
	(void) srv_senddone(spp, SRV_DONE_FINAL,
		  CS_TRAN_UNDEFINED, (CS_INT)0);
		  return (CS_SUCCEED);
}

