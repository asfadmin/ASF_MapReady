/*=============================================================================
 |  @(#)asf.c	2.24 97/01/24 17:32:39
 |
 |  Alaska SAR Facility (ASF) Application Toolkit.
 |  Copyright (C) Jet Propulsion Laboratory
 |
 *============================================================================*/
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <fcntl.h>
#include <syslog.h>

#define  _ASFLIB_
#include "asf.h"
#include "pthread_wrapper.h"
#include "version.h"

static char sccsid_asf_c[] = "@(#)asf.c	2.24 97/01/24 17:32:39";

static char CP[]		= "CP";
static char RESET[]		= "SUBSYSTEM_HALT";
static char STOP[]		= "SUBSYSTEM_STOP";
static char READY[]		= "SUBSYSTEM_READY";
static char HEARTBEAT[]		= "SUBSYSTEM_HEARTBEAT";
static char TIME[]		= "COMMON_HEADER.TIME";
static char MSG_TYPE[]		= "COMMON_HEADER.MSG_TYPE";
static char SOURCE[]		= "COMMON_HEADER.SOURCE";
static char DESTINATION[]	= "COMMON_HEADER.DESTINATION";
static char JOB_ID[]		= "BODY.JOB_ID";
static char HEARTBEAT_ACK[]	= "SUBSYSTEM_STATUS";
static char STATUS[]		= "SUBSYSTEM_STATUS.BODY.STATUS";
static char MESSAGE[]		= "SUBSYSTEM_STATUS.BODY.COMMENT";
static char SUB_TYPE[]		= "SUBSYSTEM_STATUS.BODY.SUB_TYPE";

/*  For interface testing only  */

static char *heartbeat_file	= NULL;
static char *ready_file		= NULL;

/*  End of interface testing.	*/

static char *MSGtemplate	= "$LOCAL/sps/templates/msgTemplate.odl";
static ODL   ODLtemplate	= NULL;

static AsfState	ASFstate	= IDLE;

extern char *convertEnvFile();

/*------------------------------*
 |  ASF Callback Message Queue
 *------------------------------*/
typedef struct AsfMsg {
    struct AsfMsg  *insert;	/* Point to last element inserted */
    struct AsfMsg  *remove;	/* Point to element on next remove */
    void (*callback)();		/* Callback routine */
    void  *callbackArg;         /* User argument for the callback routine */
    char  *msgType;		/* Message type */
    ODL	   msg;			/* The received message in ODL format */
    void  *app;			/* Application context */

} *AsfMsg;

#define Q_insert(q,e)		\
    (q)->insert = ((e)->insert=((e)->remove=(q))->insert)->remove = (e)

#define Q_remove(q,e)		\
    ((*(e)) = (q)->remove, (*(e))->remove->insert = (q), \
    (q)->remove = (*(e))->remove)


/*---------------------------*
 |  ASF Application Context
 *---------------------------*/
typedef struct AsfApp {
    struct AsfMsg *insert;	/* Point to last element inserted */
    struct AsfMsg *remove;	/* Point to element on next remove */

    struct AsfMsg *prev;	/* Point to last installed callback */
    struct AsfMsg *next;	/* Point to first installed callback */

    char	*name;		/* ASF subsystem name */
    int		req_active;	/* True if processing request is active */
    pthread_t	req_thread;	/* Only one processing request at a time */

} *AsfApp;


/*-----------------------------------------------------------------------------
 |  NAME
 |      callback - main entry point of the callback thread.
 |
 |  SYPNOSIS
 |	void callback (AsfApp app_context);
 |
 |  DESCRIPTION
 |      This function is the main entry point of the callback thread which is
 |	is distinct and separate from the socket communication receive thread.
 |	This constitutes the main loop of the callback thread, and as such,
 |	it does not return.  It loops forever waiting for callback events
 |	and dispatch them to the appropriate callback procedure.  The callback
 |	events are enqueued by the socket communication receive thread upon
 |	arrival of a message.
 *----------------------------------------------------------------------------*/
static
#ifdef	_NO_PROTO
void callback (m)
AsfMsg m;
#else

void callback (AsfMsg m)
#endif
{
    pthread_t id;
    if (!strcasecmp(m->msgType, RESET) || !strcasecmp(m->msgType, HEARTBEAT))
	(*m->callback)(m->msg, m->callbackArg);
    else {
	AsfApp app = (AsfApp) m->app;
	app->req_thread = pthread_self();
	app->req_active = 1;
	(*m->callback)(m->msg, m->callbackArg);
	app->req_active = 0;
    }
    ODLFree(m->msg);
    id = pthread_self();
    pthread_detach(&id);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfSetProcessState - note the current process state
 |
 |  SYPNOSIS
 |	void AsfSetProcessState (AsfState state)
 |
 |  DESCRIPTION
 |      This call indicates the state of the current processing request.
 |	The state set by this call will be used by the ASF library to
 |	respond to an asynchronous SUBSYSTEM_HEARTBEAT inquiry message.
 |
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
void AsfSetProcessState (state)
AsfState state;
#else

void AsfSetProcessState (AsfState state)
#endif
{
    ASFstate = state;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      asfsetprocessstate_ - Fortran callable version of AsfSetProcessState.
 |
 |  SYPNOSIS
 |	integer current_state
 |
 |	CALL AsfSetProcessState (current_state)
 |
 |  DESCRIPTION
 |      This function is callable from a Fortran subprogram.  It is
 |	the Fortran equivalent of the C function 'AsfSetProcessState'.
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
void asfsetprocessstate_(state)
AsfState *state;
#else

void asfsetprocessstate_(AsfState *state)
#endif
{
    AsfSetProcessState(*state);
}

/*-----------------------------------------------------------------------------
 |  NAME
 |      InitODLtemplate - read in the ODL message template file.
 |
 |  SYPNOSIS
 |	ODL InitODLtemplate (void);
 |
 |  DESCRIPTION
 |      This function reads in the ODL message template file.  It returns a
 |	pointer to the parsed ODL tree structure on success, and NULL on error.
 *----------------------------------------------------------------------------*/
/* static */
#ifdef	_NO_PROTO
ODL InitODLtemplate(msgTemplateName)
char *msgTemplateName;
#else

ODL InitODLtemplate (char *msgTemplateName)
#endif
{
    static char* ID = "InitODLtemplate";
    struct stat fs;
    char error[256], *buf;

    if (ODLtemplate != NULL)
	ODLFree(ODLtemplate);

    if (msgTemplateName != NULL) /* allow default to be set by caller */
        MSGtemplate = msgTemplateName;
    else
        MSGtemplate = convertEnvFile(MSGtemplate);

    if ((ODLtemplate = ODLparse(MSGtemplate, 0, error)) == NULL) {
	printfLLog(LOG_ERR, "%s: %s, %s\n", ID, MSGtemplate, error);
        return NULL;
    }
    return ODLtemplate;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      getargv - extract ASF specific command line arguments
 |
 |  SYPNOSIS
 |	int getargv (int *argc, char *argv[], AsfApp app_context);
 |
 |  DESCRIPTION
 |      This function extracts ASF-specific command line arguments and
 |	initializes the application accordingly.
 *----------------------------------------------------------------------------*/
static 
#ifdef	_NO_PROTO
int getargv (argc, argv, p)
int* argc;
char* argv[];
AsfApp p;
#else

int getargv (int* argc, char* argv[], AsfApp p)
#endif
{
    register int i;
    for (i=1; argv[i] != NULL; ++i) {

	if (! strcmp(argv[i], "-asfn")) {
	    if (argv[++i] == NULL) {
		printfLLog (LOG_ERR, "Missing name of subsystem -- Goodbye!\n");
		exit (-1);
	    }
	    p->name = argv[i];
	}
	else if (! strcmp(argv[i], "-iftheartbeat")) {
	    if (argv[++i] == NULL) {
		printfLLog (LOG_ERR, "Missing SUBSYSTEM_HEARTBEAT file -- Goodbye!\n");
		exit (-1);
	    }
	    heartbeat_file = argv[i];
	}
	else if (! strcmp(argv[i], "-iftready")) {
	    if (argv[++i] == NULL) {
		printfLLog (LOG_ERR, "Missing SUBSYSTEM_READY file -- Goodbye!\n");
		exit (-1);
	    }
	    ready_file = argv[i];
	}
	else if (! strcmp(argv[i], "-config")) {
	    struct stat buf;
	    if (argv[++i] == NULL) {
		printfLLog (LOG_ERR, "Missing name of message template -- Goodbye!\n");
		exit (-1);
	    }
	    if (stat(argv[i], &buf) == -1) {
		printfLLog (LOG_ERR, "Can't open message template %s -- Goodbye!\n", argv[i]);
		exit (-1);
	    }
	    MSGtemplate = argv[i];
	}
    }
    return 1;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfFindMsg - look up a given message type in specified message list.
 |
 |  SYPNOSIS
 |	AsfMsg AsfFindMsg (char *msgType, AsfMsg list);
 |
 |  DESCRIPTION
 |      This function returns a pointer to an AsfMsg structure associated 
 |	with the given message type 'msgType' in the message type list 'list'.
 |	The function returns NULL if it does not find such type in the list.
 *----------------------------------------------------------------------------*/
static
#ifdef	_NO_PROTO
AsfMsg AsfFindMsg (msgType, list)
char*  msgType;
AsfMsg list;
#else

AsfMsg AsfFindMsg (char* msgType, AsfMsg list)
#endif
{
    register AsfMsg m;
    for (m = list->remove; m != list; m = m->remove)
	if (!strcmp(m->msgType, msgType))
	    return m;
    return NULL;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfAddCallback - find a given message type in specified message list.
 |
 |  SYPNOSIS
 |	int AsfAddCallback(
 |	    AsfApp app_context, 
 |	    char *msgType, void (*callback)(ODL msg, void *arg), void *arg,
 |	    ...);
 |
 |	Sample Codes:
 |	    AsfAddCallback(app, "RDS_PROC_REQ", ProcessRequest, NULL, 
 |			        "RDS_PROD_REQ", ProductRequest, NULL,
 |			   0);
 |  DESCRIPTION
 |      This function adss the specified callback procedures to the specified
 |	application's callback list.  The function takes a variable number of
 |	arguments and thus must be terminated with NULL as its last argument.
 *----------------------------------------------------------------------------*/
#ifdef	NOSTDARG
int AsfAddCallback (va_alist)
va_dcl
{
    int  n = 0;
    char* type;
    AsfMsg m;
    AsfApp p;
    va_list  args;
    va_start(args);
    p = va_arg(args, AsfApp);
#else
int AsfAddCallback (AsfApp p, ...)
{
    int n = 0;
    char* type;
    AsfMsg m;
    va_list  args;
    va_start(args, p);
#endif
    if (p == NULL) return 0;

    while (type = va_arg(args, char*)) {
	if ((m = AsfFindMsg(type, (AsfMsg) &p->prev)) != NULL) {
	    m->app = (void*)p;
	    m->callback = (void (*)()) va_arg(args, void*);
	    m->callbackArg = va_arg(args, void*);
	    ++n;
	    continue;
	}
	if ((m = (AsfMsg) malloc(sizeof(*m))) == NULL) {
	    printfLLog (LOG_ERR, "Out of memory... callback dropped!\n");
	    return n;
	}
	m->app = (void*)p;
	m->msgType = type;
	m->callback = (void (*)()) va_arg(args, void*);
	m->callbackArg = va_arg(args, void*);
	Q_insert ((AsfMsg) &p->prev, m);
	++n;
    }
    va_end(args);
    return n;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      GetNewMsg - allocate and return a new ODL message of specified type.
 |
 |  SYPNOSIS
 |	ODL GetNewMsg (char *msgType);
 |
 |  DESCRIPTION
 |      This function allocates and returns a new message of type 'msgType'.
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
ODL GetNewMsg (msgType)
char* msgType;
#else

ODL GetNewMsg (char* msgType)
#endif
{
    struct timeval tv;
    struct timezone tz;
    static char* ID = "GetNewMsg";
    char timebuf[256], *str;
    ODL msg;

    if (ODLtemplate == NULL && InitODLtemplate(NULL) == NULL)
	return NULL;

    if ((str = ODLToStr(ODLtemplate, msgType)) == NULL) {
	printfLLog (LOG_ERR, "%s: can't find %s in template\n", ID, msgType);
	return NULL;
    }
    if ((msg = StrToODL(str, strlen(str))) == NULL) {
	printfLLog (LOG_ERR, "%s: can't convert %s to ODL\n", ID, msgType);
	ODLFree(str);
	return NULL;
    }
    ODLFree(str);

    if (gettimeofday(&tv, &tz) != -1 &&
	strlen(msgType) + strlen(TIME) < sizeof(timebuf)-1) {
	sprintf(timebuf, "%s.%s", msgType, TIME);
	ODLSetVal(msg, timebuf, &tv);
    }
    return msg;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      GetAckMsg - allocate return a new ODL message of specified type
 |
 |  SYPNOSIS
 |	 ODL GetAckMsg (ODL msg, char *ackType);
 |
 |  DESCRIPTION
 |      This function allocates and returns a new message of type 'ackType'
 |	with a header pre-initialized to value similar to that of 'msg'.
 |
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
ODL GetAckMsg (msg, ackType)
ODL msg;
char* ackType;
#else

ODL GetAckMsg (ODL msg, char* ackType)
#endif
{
    ODL ack;
    static char* ID = "GetAckMsg";

    if ((ack = GetNewMsg(ackType)) && msg) {
	int val;
	char src[256], dst[256];
	char* msgType = Name(msg);
	size_t msgLen = strlen(msgType);
	size_t ackLen = strlen(ackType);

	if (msgLen + strlen(SOURCE) < sizeof(src)-1 &&
	    ackLen + strlen(DESTINATION) < sizeof(dst)-1) {

	    sprintf(src, "%s.%s", msgType, SOURCE);
	    sprintf(dst, "%s.%s", ackType, DESTINATION);

	    if (ODLGetVal(msg, src, &val))
		ODLSetVal(ack, dst,  val);
	}
	if (ackLen + strlen(SOURCE) < sizeof(src)-1 &&
	    msgLen + strlen(DESTINATION) < sizeof(dst)-1) {

	    sprintf(src, "%s.%s", ackType, SOURCE);
	    sprintf(dst, "%s.%s", msgType, DESTINATION);

	    if (ODLGetVal(msg, dst, &val))
		ODLSetVal(ack, src,  val);
	}
	if (msgLen + strlen(JOB_ID) < sizeof(src)-1 &&
	    ackLen + strlen(JOB_ID) < sizeof(dst)-1) {

	    sprintf(src, "%s.%s", msgType, JOB_ID);
	    sprintf(dst, "%s.%s", ackType, JOB_ID);

	    if (ODLGetVal(msg, src, &val))
		ODLSetVal(ack, dst,  val);
	}
    }
    return ack;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
int SendTextToServer(filename)
char* filename;
#else

int SendTextToServer(char* filename)
#endif
{
    char* buf;
    struct stat stbuf;
    int  fd;
    if ((fd = open(filename, O_RDONLY)) == -1) {
	printfLLog(LOG_ERR, "can't open %s, %s\n", filename, strerror(errno));
	return 0;
    }
    if (fstat(fd, &stbuf) == -1) {
	printfLLog(LOG_ERR, "can't stat %s, %s\n", filename, strerror(errno));
	close(fd);
	return 0;
    }
    if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
	printfLLog(LOG_ERR, "can't malloc %d bytes\n", stbuf.st_size);
	close(fd);
	return 0;
    }
    if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
	printfLLog(LOG_ERR, "can't read %d bytes\n", stbuf.st_size);
	close(fd);
	free(buf);
	return 0;
    }
    close(fd);
    writeTextToServer(buf, stbuf.st_size);
    free(buf);
    return 1;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      SendReadyMsg - send a SUBSYSTEM_READY message to the CP process.
 |
 |  SYPNOSIS
 |	int SendReadyMsg (AsfApp app_context);
 |
 |  DESCRIPTION
 |      This function sends a SUBSYSTEM_READY message to the CP process to
 |	indicate that we are up and ready to receive /processing requests.
 *----------------------------------------------------------------------------*/
static
#ifdef	_NO_PROTO
int SendReadyMsg (p)
AsfApp p;
#else

int SendReadyMsg (AsfApp p)
#endif
{
    ODL msg;
    if (ready_file != NULL)
	return SendTextToServer(ready_file);

    if ((msg = GetNewMsg(READY)) == NULL) {
	printfLLog (LOG_ERR, "Unable to get a %s template\n", READY);
	return 0;
    }
    if (WriteMsgToServer(msg) == -1) {
	printfLLog (LOG_ERR, "Unable to send %s\n", READY);
	ODLFree(msg);
	return 0;
    }
    ODLFree(msg);
    return 1;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfEnableKill - enable the delivery of ASF application kill signal.
 |
 |  SYPNOSIS
 |	int AsfEnableKill (void);
 |
 |  DESCRIPTION
 |      This function unblocks the delivery of ASF application kill signal
 |	which was blocked by 'AsfDisableKill'.  This function is used in
 |	conjunction with AsfDisableKill to protect a critical region where
 |	asynchronous termination at such point may leave a device in an
 |	unpredictable state.
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
int AsfEnableKill()
#else

int AsfEnableKill(void)
#endif
{
   int status;
   status = pthread_setcancel(CANCEL_ON);
#ifdef _ALPHA
   pthread_testcancel();
#endif
   return status;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfDisableKill - disable the delivery of ASF application kill signal.
 |
 |  SYPNOSIS
 |	int AsfDisableKill (void);
 |
 |  DESCRIPTION
 |      This function blocks the delivery of ASF application kill signal
 |	which is implemented via system SIGHUP signal.  This call is used in
 |	conjuction with 'AsfEnableKill' to protect a critical region where
 |	asynchronous termination at such point may leave a device in an 
 |	unpredictable state.
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
int AsfDisableKill()
#else

int AsfDisableKill(void)
#endif
{
   return pthread_setcancel(CANCEL_OFF);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfAppMainLoop - read and process ASF events
 |
 |  SYPNOSIS
 |	void AsfAppMainLoop (AsfApp app_context);
 |
 |  DESCRIPTION
 |      This function reads next incoming ASF events and dispatches the event
 |	to the appropriate callback routine.  This constitutes the main loop
 |	of ASF Toolkit application, and, as such, it does not return.
 *----------------------------------------------------------------------------*/
#ifdef	_NO_PROTO
void AsfAppMainLoop (app)
AsfApp app;
#else

void AsfAppMainLoop (AsfApp app)
#endif
{
    ODL msg;
    AsfMsg type;
    char* msgType;
    int read_status;
    pthread_t tid;
    pthread_addr_t status;
    pthread_attr_t attr = pthread_attr_default;

#ifndef __sgi
    if (pthread_attr_create (&attr) == -1 ||
	pthread_attr_setstacksize (&attr, 1024*1024) == -1)
	printfLLog (LOG_ERR, "Can't adjust stack size\n");
#endif
    if (app == NULL || !SendReadyMsg(app))
	return;

    for (;;) {
	printfLLog (LOG_INFO, "Waiting for messages from CP...\n");

	if (! (msg = (ODL) ReadMsgFromServer (&read_status))) {
	    if (read_status == -1)
		SendReadyMsg(app);
	    continue;
	}
	if (! (msgType = Name(msg))) {
	    ODLFree(msg);
	    printfLLog (LOG_ERR, "Invalid message received!\n");
	    continue;
	}
	printfLLog (LOG_INFO, "Just received a %s message...\n", msgType);

	if (! (type = AsfFindMsg(msgType, (AsfMsg) &app->prev))) {
	    ODLFree(msg);
	    printfLLog (LOG_ERR, "%s callback not installed - msg ignored.\n", msgType);
	    continue;
	}
	if (! strcasecmp(msgType, RESET)) {
	    (*type->callback)(msg, type->callbackArg);
	    ODLFree(msg);
	    SendReadyMsg(app);
	    continue;
	}
	if (! strcasecmp(msgType, STOP)) {
	    (*type->callback)(msg, type->callbackArg);
	    ODLFree(msg);
	    exit(-1);
	}
	type->msg = msg;
	if (pthread_create(&tid, attr, (pthread_startroutine_t) callback,
			  (pthread_addr_t) type) != -1)
	    continue;
	printfLLog (LOG_ERR, "Can't start %s callback thread - msg ignored\n", msgType);
	ODLFree(msg);
    }
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
static
#ifdef	_NO_PROTO
void RESET_callback(msg, app)
ODL  msg;
AsfApp app;
#else

void RESET_callback(ODL msg, AsfApp app)
#endif
{
    pthread_addr_t status;
    if (app->req_active) {
	app->req_active = 0;

	pthread_cancel (app->req_thread);
	printfLLog (LOG_INFO, "Cancelling thread %d...\n", app->req_thread);

	pthread_join (app->req_thread, &status);
	printfLLog (LOG_INFO, "Thread now exited.\n");
    }
    AsfSetProcessState(IDLE);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
static
#ifdef	_NO_PROTO
void HEARTBEAT_callback(msg, app)
ODL  msg;
AsfApp app;
#else

void HEARTBEAT_callback(ODL msg, AsfApp app)
#endif
{
    ODL ack;
    static char *id = "HEARTBEAT_callback";
    static char *stateString[] = {
	"IDLE", "SCAN", "PREPROCESSING", "MAIN_PROCESSING"
    };
    int status = ASFstate;
    char *s, *message = (status < (int) IDLE || status > (int) MAIN_PROCESSING)
			? "UNKNOWN" : stateString[status];
    char *statusStr = (status < (int) IDLE || status > (int) MAIN_PROCESSING)
			? "CANCEL/FAIL" : "COMPLETED";


    if (heartbeat_file != NULL) {
	SendTextToServer (heartbeat_file);
	return;
    }
    if (! (ack = GetAckMsg(msg, s = HEARTBEAT_ACK))) {
	printfLLog (LOG_ERR, "%s: can't access %s - %s ignored\n", id, s, HEARTBEAT);
	return;
    }
    if (!ODLSetVal(ack, s = STATUS, statusStr))
	printfLLog(LOG_ERR, "%s: can't set %s - %s ignored\n", id, s, HEARTBEAT);

    else if (!ODLSetString(ack, s = MESSAGE, message))
	printfLLog(LOG_ERR, "%s: can't set %s - %s ignored\n", id, s, HEARTBEAT);

    else if (!ODLSetString(ack, s = SUB_TYPE, "HEARTBEAT"))
	printfLLog(LOG_ERR, "%s: can't set %s - %s ignored\n", id, s, HEARTBEAT);
    else
	WriteMsgToServer(ack);
    ODLFree(ack);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      AsfAppInitialize - initialize ASF toolkit
 |
 |  SYPNOSIS
 |	AsfApp AsfAppInitialize (char *asfName, int *argc, char *argv[])
 |
 |  DESCRIPTION
 |      This call initializes the ASF library toolkit, and must be made
 |	prior to calling any other ASF library.  The function returns an 
 |	application context which is needed by other ASF routines.
 *----------------------------------------------------------------------------*/

#ifdef	_NO_PROTO
AsfApp AsfAppInitialize (asfName, argc, argv)
char* asfName;
int*  argc;
char* argv[];
#else

AsfApp AsfAppInitialize (char* asfName, int* argc, char* argv[])
#endif
{
    int i;
    ODL *msg;
    AsfApp p;
    char id[256];
    struct sigaction sigact;  /* cws */
    sigset_t sigset;          /* cws */

    if (initASFlog(*argc, argv, asfName) == -1 ||
	InitClient(*argc, argv) == -1)
	return NULL;

    /* cws added the following to handle zombie processes */
    sigemptyset(&sigset);
    sigact.sa_handler = NULL;
    sigact.sa_mask    = sigset;

#ifdef __sgi
    sigact.sa_flags   = SA_NOCLDWAIT;
#else
    sigact.sa_flags   = SA_NOCLDSTOP;
#endif
    sigaction(SIGCHLD, &sigact, NULL);

    /* cws added the odl init call */
    if (m_init() == -1 || ODLinit() == -1)
	return NULL;

    if ((p = (AsfApp) malloc(sizeof(*p))) == NULL) {
	printfLLog (LOG_ERR, "can't malloc AsfApp structure!\n");
	return NULL;
    }
    p->remove = p->insert = (AsfMsg) p;
    p->next = p->prev = (AsfMsg) &p->prev;
    p->name = (asfName == NULL ? "XXX" : asfName);
    p->req_active = 0;
    p->req_thread = pthread_self();

    MSGtemplate	= convertEnvFile("$LOCAL/sps/templates/msgTemplate.odl");
    if (!getargv(argc, argv, p) || !InitODLtemplate(MSGtemplate)) {
	free(p);
	return NULL;
    }
    msg = Val(ODLtemplate);

    for (i = 0 ; msg[i] != NULL; ++i) {
	sprintf(id, "%s.%s", Name(msg[i]), SOURCE);
	ODLSetVal(ODLtemplate, id, p->name);

	sprintf(id, "%s.%s", Name(msg[i]), DESTINATION);
	ODLSetVal(ODLtemplate, id, CP);
    }
    AsfAddCallback(p, HEARTBEAT, HEARTBEAT_callback, p,
    		      RESET, RESET_callback, p,
    		      STOP, RESET_callback, p, 0);
    return p;
}
