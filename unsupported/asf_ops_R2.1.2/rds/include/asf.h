/*--------------------------------
 * sccs header
 * @(#)asf.h	2.5 95/06/28 17:55:42
 *------------------------------*/
/* ---------------------------------------------------------------------------
 *
 *  Copyright (C) Jet Propulsion Laboratory
 *
 *  Alaska SAR Facility (ASF) Application Toolkit Include.
 *
 * --------------------------------------------------------------------------*/

#ifndef _ASF_H
#define _ASF_H

static char sccsid_asf_h[] =
	"@(#)asf.h	2.5 95/06/28 17:55:42";

#include "odl.h"

typedef enum {
	IDLE = 0,
	SCAN,
        PREPROCESSING,
        MAIN_PROCESSING
} AsfState;

#ifndef _ASFLIB_

typedef void *AsfApp;

#define ASFApp			AsfApp
#define ASFAppMainLoop		AsfAppMainLoop
#define ASFAppInitialize	AsfAppInitialize
#define ASFAddCallback		AsfAddCallback
#define ASFEnableKill		AsfEnableKill
#define ASFDisableKill		AsfDisableKill
#define ASFSetProcessState	AsfSetProcessState

/* ----------------------------------------------------------------------------
 *  NAME
 *      AsfAppInitialize - initialize ASF toolkit
 *
 *  SYPNOSIS
 *      AsfApp AsfAppInitialize (char *asfName, int *argc, char *argv[])
 *
 *  DESCRIPTION
 *      This call initializes the ASF library toolkit, and must be made
 *      prior to calling any other ASF library.  The function returns an
 *      application context which is needed by other ASF routines.
 *
 */
AsfApp  AsfAppInitialize (char *asfName, int *argc, char *argv[]);


/* ----------------------------------------------------------------------------
 *  NAME
 *      AsfAppMainLoop - read and process ASF events
 *
 *  SYPNOSIS
 *      void AsfAppMainLoop (AsfApp app_context);
 *
 *  DESCRIPTION
 *      This function reads next incoming ASF events and dispatches the event
 *      to the appropriate callback routine.  This constitutes the main loop
 *      of ASF Toolkit application, and, as such, it does not return.
 *
 */
void	AsfAppMainLoop (AsfApp app_context);


/* ----------------------------------------------------------------------------
 *  NAME
 *      AsfSetProcessState - note the current process state
 *
 *  SYPNOSIS
 *      void AsfSetProcessState (AsfState state)
 *
 *  DESCRIPTION
 *      This call indicates the state of the current processing request.
 *      The state set by this call will be used by the ASF library to
 *      respond to an asynchronous SUBSYSTEM_HEARTBEAT inquiry message.
 *
 */
void	AsfSetProcessState (AsfState state);


/* ----------------------------------------------------------------------------
 *  NAME
 *      AsfAddCallback - find a given message type in specified message list.
 *
 *  SYPNOSIS
 *      int AsfAddCallback(
 *          AsfApp app_context,
 *          char *msgType, void (*callback)(ODL msg, void *arg), void *arg,
 *          ...);
 *
 *      Sample Codes:
 *          AsfAddCallback(app, "RDS_PROC_REQ", ProcessRequest, 0,
 *                              "RDS_PROD_REQ", ProductRequest, 0,
 *                         0);
 *  DESCRIPTION
 *      This function adss the specified callback procedures to the specified
 *      application's callback list.  The function takes a variable number of
 *      arguments and thus must be terminated with NULL as its last argument.
 *
 */
int     AsfAddCallback (AsfApp app_context,
			char *msgType, void (*callback)(), void *callback_arg,
			...);

/* ----------------------------------------------------------------------------
 *  NAME
 *      AsfDisableKill - disable the delivery of ASF application kill signal.
 *
 *  SYPNOSIS
 *      int AsfDisableKill (void);
 *
 *  DESCRIPTION
 *      This function blocks the delivery of ASF application kill signal
 *      which is implemented via system SIGHUP signal.  This call is used in
 *      conjuction with 'AsfEnableKill' to protect a critical region where
 *      asynchronous termination at such point may leave a device in an
 *      unpredictable state.
 *
 */
int	AsfDisableKill (void);


/* ----------------------------------------------------------------------------
 *  NAME
 *      AsfEnableKill - enable the delivery of ASF application kill signal.
 *
 *  SYPNOSIS
 *      int AsfEnableKill (void);
 *
 *  DESCRIPTION
 *      This function unblocks the delivery of ASF application kill signal
 *      which was blocked by 'AsfDisableKill'.  This function is used in
 *      conjunction with AsfDisableKill to protect a critical region where
 *      asynchronous termination at such point may leave a device in an
 *      unpredictable state.
 *
 */
int	AsfEnableKill (void);


/* ----------------------------------------------------------------------------
 *  NAME
 *      GetNewMsg - allocate and return a new ODL message of specified type.
 *
 *  SYPNOSIS
 *      ODL GetNewMsg (char *msgType);
 *
 *  DESCRIPTION
 *      This function allocates and returns a new message of type 'msgType'.
 *
 */
ODL	GetNewMsg (char *msgType);


/* ----------------------------------------------------------------------------
 *  NAME
 *      GetAckMsg - allocate return a new ODL message of specified type
 *
 *  SYPNOSIS
 *       ODL GetAckMsg (ODL msg, char *ackType);
 *
 *  DESCRIPTION
 *      This function allocates and returns a new message of type 'ackType'
 *      with a header pre-initialized to value similar to that of 'msg'.
 *      Namely, the DATE value in the COMMON_HEADER, and the PROD_REQ_ID
 *      and PROC_REQ_ID values, if applicable, are assigned appropriately.
 *
 */
ODL	GetAckMsg (ODL msg, char *ackType);

#endif	/* _ASFLIB_ */

#endif /* !_ASF_H */
