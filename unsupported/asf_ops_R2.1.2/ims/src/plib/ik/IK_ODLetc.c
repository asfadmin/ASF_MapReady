/*
 * Name: IK_ODLetc.c
 *
 * Synopsis: This file contains functions which are extensions or
 * workarounds for ODL "features".
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
 * Revision 5.0.1.1  1995/11/15  20:17:30  iredell
 * if string is longer than the size limit, copy only the size limit.
 * this will prevent a clobber of memory.
 *
 * Revision 5.0  1995/11/06  13:04:02  ims
 * PROMOTION FROM GUI_4_5_8_951030 FOR GUI_PROMO_5_0_951106
 *
 * Revision 4.5.1.7  1995/10/24  13:26:17  iredell
 * smr 3211, fix bug in IK_AddHiResTimestamp
 *
 * Revision 4.5.1.6  1995/10/19  15:36:18  iredell
 * SMR 3197
 * Added DATASET_ID and DATA_CENTER_ID to parameters to scrub.
 *
 * Revision 4.5.1.5  1995/10/17  15:32:10  iredell
 * add proc IK_ScrubODL to upcase strings in ODL
 * ,
 *
 * Revision 4.5.1.4  1995/10/12  19:14:44  winter
 * Changed IK_Stats.h to IK_Session.h. Made code which creates session UID
 * work only for client.
 *
 * Revision 4.5.1.3  1995/10/04  14:03:42  winter
 * Added code to add the SESSION_ID parameter to the MONITOR group.
 *
 * Revision 4.5.1.2  1995/10/04  12:33:11  winter
 * Cleaned up prior to adding code for session ID.
 *
 * Revision 4.5.1.1  1995/07/27  19:03:21  ims
 * COPIED FROM 4.4.1.1 FOR GUI_PROMO_4_5_950726
 *
 * Revision 4.4.1.1  1995/05/25  12:50:41  ims
 * Added the HPUX implementation of IK_Sequence2List written by J. Cleveland
 * Langley DAAC.
 *
 * Revision 4.4  1995/02/13  22:20:56  ims
 * PROMOTION FROM GUI_4_4_950106 FOR GUI_PROMO_4_4_950213
 *
 * Revision 4.3  1994/08/26  10:50:00  ims
 * COPIED FROM GUI_4_3_940824.
 *
 * Revision 4.0.1.5  1994/06/30  12:33:28  winter
 * Added setting of rows and columns members of timestamp parameter, and
 * changed ODLConvertInteger() to ODLConvertString() for timestamp parameter
 * value addition. This should allow correct operation with older servers
 * based on the 4/22 IK lilbrary (I hope...).
 *
 * Revision 4.0.1.4  1994/06/23  14:07:25  winter
 * Changed arg types for IK_AddHiResTimestamp(), and cleaned up code.
 *
 * Revision 4.0.1.3  1994/06/23  12:40:02  winter
 * Deleted prototype for and definition of IK_add2label(), since it is no
 * longer used.
 *
 * Revision 4.0.1.2  1994/06/08  15:04:16  ims
 * COPIED FROM REVISION 3.1.1.2.
 *
 * Revision 3.1.1.2  1994/06/04  13:19:02  winter
 * Deleted lines in IK_AddHiResTimeStamp() which look for message ID.
 *
 * Revision 3.1.1.1  1994/05/31  12:21:32  winter
 * Added code to use high-resolution timestamps, using gettimeofday()
 * function. The new timestamps are placed in the MONITOR ODL group, which
 * is attched to all messages.
 *
 * Revision 3.1  1994/04/05  09:49:52  ims
 * Promotion from MAR28_RELEASE
 *
 * Revision 3.0.1.2  1994/03/24  21:07:10  ryan
 * cleaned up.
 *
 * Revision 3.0.1.1  1994/01/21  20:49:37  ryan
 * phase 1 cleanup
 *
 * Revision 3.0  1993/12/29  15:18:49  winter
 * ALPHA-1
 *
 * Revision 1.8.1.1  1993/09/07  22:09:53  sylvain
 * IK_AddMonTS now logs the time in seconds instead of in ascii.
 *
 * Revision 1.8  1993/06/28  20:49:08  sylvain
 * Updated the function IK_Sequence2List so that it alligns the arrays on a
 * 	bytes boundery (IK_ALIGN).
 *
 * Revision 1.7  1993/06/02  12:31:33  sylvain
 * Added better performance logging.  When the client receives a message
 * 	from a server, it will now log the timestamps that were
 * 	added to the monitor group by the server.
 * Split the loging function into 2 parts so that we could log time
 * 	stamps for images that didn't have corresponding ODL labels.
 *
 * Revision 1.5  1993/01/14  19:04:24  sylvain
 * added better error/syslog messages, and moved the DebugLevels into the
 * correct range
 *
 * Revision 1.4  1992/12/16  21:42:52  sylvain
 * add IK_AddMonTS() from IK_Comn.c
 *
 * Revision 1.3  1992/10/02  20:58:44  honce
 * #include <malloc.h> deleted because of conflict with stddef.h
 *
 * Revision 1.2  1992/09/29  19:59:09  honce
 * Changed IK_Sequence2List() to strip quotes from strings.
 *
 * Revision 1.1  1992/09/19  00:25:28  honce
 * Initial revision
 *
*/

/*****************************************************************************/

static const char *rcsid = "$Id$";

/*****************************************************************************/

/* #include directives */

#include "IK_Ims.h"

/* Standard headers */
#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

/* Third-party library headers */
#include "odldef.h"
#include "odlinter.h"

/* IMS headers */
#include "IK_Errno.h"
#include "IK_Network.h"
#include "IK_Session.h"
#include "IK_Syslog.h"

#ifdef DAAC_SERVER
#define IK_ALIGN(n) n
#else
#include "IK_Ingest.h"
#endif

/*****************************************************************************/

/* Local #define directives */

/* TSGRP_NAME is the name of the group that holds the time stamp
   information. */
#define TSGRP_NAME "MONITOR"

/* These are the names to use for the various timestamp parameters
   within the MONITOR group. */
#define TX_SERVER_PARAM_NAME "TX_SERVER"
#define RX_SERVER_PARAM_NAME "RX_SERVER"
#define TX_CLIENT_PARAM_NAME "TX_CLIENT"
#define RX_CLIENT_PARAM_NAME "RX_CLIENT"

/* This is the name of the session ID parameter within the monitor
   group. */
#define SESSION_ID_PARAM_NAME "SESSION_ID"

/*****************************************************************************/

/* External variable declarations */

extern IK_BOOLEAN IK_Is_Server;

/*****************************************************************************/

/* Local function prototypes */

static int
IK_AddHiResTimestamp(AGGREGATE agg_root, IK_BOOLEAN bool_tx);

/*****************************************************************************/

/*
 * Name: IK_Sequence2List()
 *
 * Description: This function changes an ODL multi-value PARAMETER
 * into a C NULL-terminated list. This function takes values from
 * multi-value ODL PARAMETER and creates a C NULL- terminated list of
 * NULL terminated character arrays.
 *
 * Parameters:
 * int *length - length of buffer or -1 
 * PARAMETER parm - parameter to strip of values 
 * unsigned int entry_size - Max length of a value 
 *
 * Return Values:
 * type: char**
 * Return: upon success, sizeof(List). On failure, -1 and errno.
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions
 *
 * Precondition 1: ODL Parameter Sequence and sufficient dynamic resources
 * Postcondition 1: return in list a NULL terminated array of NULL
 * terminated character arrays and the sizeof the "List".
 *
 * Precondition 2: ODL Parameter Sequence and insufficient dynamic
 * resources
 * Postcondition 2: return -1 and set errno to ENOMEM
 *
 * Precondition 3: No ODL Parameter Sequence or no address for
 * finished List is given
 * Postcondition 3: return -1 and set errno to EINVAL
 *
 * Notes:
 *
 * 1) While values for entry_size can't be validated, unrealistic
 * values will cause very __BAD__ things to happen.
 *
 * The space for the new "list" is obtained using malloc(3).
 *
 * Revision History:
 *
 *
 * */

#ifndef HPUX
char **
IK_Sequence2List(int *length, PARAMETER parm, unsigned int entry_size)
{

    /* pointer to raw buffer: Max Sized */
    char *raw;

    /* address for list */
    char **list;

    /* start of pointers in raw buffer */
    char **ptrs;

    /* start of data in raw buffer */
    char *data;

    /* values of Parameter */
    VALUE value;

    /* bytes to copy into *data */
    int bytestocopy;

    /*------------------------------------------------------------------------*/

    /* Initialize the length of buffer on return to -1 (assuming an
       error). */
    *length = -1;

    /* Verify that the arguments are OK. */
    if (parm == NULL || length == NULL || entry_size == 0) {
	errno = EINVAL;
    } else {

	/* Reserve space for each entry and its pointer. */
	raw = calloc(parm->value_count + 1, sizeof(char *) + entry_size);
	if (raw == NULL) {
	    errno = ENOMEM;
	} else {

	    /* pointers to the front of the buffer */
	    ptrs = (char **) raw;

	    /* store data after pointers, leave space for NULL pointer */
	    data = (char *) &ptrs[parm->value_count + 1];

	    /* Get value of parameter, calc offset of data, and dump
	       into data buffer. */
	    for (value = FirstValue(parm); value != NULL;
		 value = NextValue(value)) {
		*(ptrs)++ = data - (unsigned int) raw;

                /* bytes to copy is the smaller of either the */ 
                /* declared constant or actual length so as to */
                /* not clobber any memory */
                bytestocopy = entry_size - 1;
                if (bytestocopy > value->item.length)  {
                  bytestocopy = value->item.length;
                }
		(void) strncpy(data, value->item.value.string,
			       bytestocopy);
		data += IK_ALIGN(bytestocopy + 1);
	    }

	    /* Free up any extra resources allocated but then not
               used. */
	    *length = data - raw;
	    if (list = (char **) malloc(*length)) {
		(void) memcpy((char *) list, raw, *length);
		free(raw);
	    } else {
		list = (char **) raw;	/* go ahead and be a Republican */
	    }

	    /* Normalize pointers to "start of heap" */
	    for (ptrs = (char **) list; *ptrs; *(ptrs)++) {
		*ptrs = (char *) list + (unsigned int) *ptrs;
	    }
	}
    }

    return(list);
}

#else

/* HPUX implementation of IK_Sequence2List by J. Cleveland, Langley DAAC */
/* Not quite as conservative on memory, but it works. */

char **
IK_Sequence2List(int *length, PARAMETER parm, unsigned int entry_size)
{
    char *stg, **ptrtable, **ptr;
    VALUE value;

    /* bytes to copy into *stg */
    int bytestocopy;

    /*------------------------------------------------------------------------*/

    *length = -1;

    if (parm == NULL || length = NULL || entry_size == 0) {
	errno = EINVAL;
    } else {
	ptrtable = calloc(parm->value->value_count + 1,
			  sizeof(char *) + entry_size);
	if (ptrtable == NULL) {
	    errno = ENOMEM;
	} else {

	    /* 1st string comes after pointer table */
	    stg = &(ptrtable[parm->value_count + 1]);

	    /* copy values into buffer,setting up pointer table as we
               go */
	    for (ptr = ptrtable, value = FirstValue(parm); value1 = NULL;
		 ptr++, value = NextValue(value)) {
		*ptr = stg;

                /* bytes to copy is the smaller of either the */ 
                /* declared constant or actual length so as to */
                /* not clobber any memory                      */
                /* *this needs to be tested on HP platform!!*  */
                bytestocopy = entry_size - 1;
                if (bytestocopy > value->item.length)  {
                  bytestocopy = value->item.length;
                }
		strncpy(stg, value->item.value.string, bytestocopy);
		stg += IK_ALIGN(bytestocopy + 1);
	    }
	    *length = (unsigned int) stg - (unsigned int) ptrtable;
	}
    }
    return(ptrtable);
}
#endif

/*****************************************************************************/

/*
 * Name: IK_AddMonTS()
 *
 * Description: This function adds the MONITOR time stamps ODL group
 * to the ODL tree and writes them to IK_Syslog.
 *
 * Parameters:
 * IK_BOOLEAN tx - Set to IK_TRUE if this function is invoked from
 * IK_TxODL(), and IK_FALSE otherwise (assumed called from
 * IK_RxODL()).
 * AGGREGATE *root - Pointer to the ODL AGGREGATE for the tree to
 * which the MONITOR group should be added
 * int size - The size of the message
 *
 * Return Values:
 * type: int
 * Returns: upon success, 0. On failure, -1 and errno, IK_ImsErrno.
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions
 * 	Precondition 1 : 
 *		an ODL tree was passed in without a Message_ID paramter
 *
 *	Postcondition 1 :
 *  		LOG an error to IK_Syslog, SET errno to EPROTO,
 *              IK_ImsErrno to IK_EPROTO, and RETURN (-1) as 
 *              the function value.
 *
 * 	Precondition 2 : 
 *		A valid ODL tree was passed in, but there
 *		wasn't a MONITOR group in the tree.
 * 
 *	Postcondition 2 : 
 *		Add the group and it's attributes. Add the time 
 *   		stamps. If we were called from the 
 *		client, than LOG an error to IK_Syslog, as well.
 *
 *	Precondition 3 :
 *		We couldn't add the time stamp value to the 'correct'
 *   		parameter.
 *
 *	Postcondition 3 :
 *		LOG an error to IK_Syslog and SET errno to ENOBUFS,
 *              IK_ImsErrno to IK_ENOMEM, and RETURN (-1) as the 
 *              value of the function.
 *
 *	Precondition 4 : 
 *		We couldn't get the 'correct' parameter from the ODL tree.
 *
 *	Postcondition 4 :
 *		LOG an error to IK_Syslog and SET errno to EPROTO,
 *		IK_ImsErrno to IK_EPROTO, and RETURN (-1) as the 
 *              value of the function.
 *
 *	Precondition 5 :
 *		A valid ODL tree was passed in, but we couldn't get the 
 *		system time.
 *
 *	Postcondition 5 : 
 *		Generate the tree normally, but set the system time to
 * 		Jan 01 1970.  
 *
 * 	Precondition 6 : 
 *		a valid ODL tree was passed in.
 * 
 *	Postcondition 6 : 
 *		Add the time stamps, RETURN 0 as the function value.
 *
 * Notes:
 * 
 * Revision History:
 *
 * 26 May 1994 (Eric Winter) - Began work to add high-resolution
 * timestamps, using the gettimeofday() library function.
 *
 * Thursday 23 June 1994 (Eric Winter) - Changed
 * IK_AddHiResTimestamp() to no longer use timestamp argument, and
 * changed AGGREGATE * argument to AGGREGATE.
 *
 * */

int
IK_AddMonTS(IK_BOOLEAN tx, AGGREGATE *root, int size)
{

    /* return_code holds the execution status of this function. */
    int return_code = 0;

    /*------------------------------------------------------------------------*/

    /* Add the timestamp to the ODL tree (if the message is one), or
       log the size of the incoming image (if the message is one). */
    if (root) {

	/* The message is an ODL tree.  */
	return_code = IK_AddHiResTimestamp(*root, tx);

    } else {

	/* The message was an image. */
	if (!IK_Is_Server && !tx) {
	    IK_vSyslog(LOG_NOTICE, "There were %d bytes of the image received",
		       size);
	    if (size < IK_NETBLK_SZ) {
		IK_Syslog(LOG_NOTICE, "The image has been totally received");
	    }
	} else {
	    if (IK_Is_Server) {
		IK_Syslog(LOG_ERR, ".IK_AddMonTS was called by the server "
			  "with no ODL label");
	    } else if (tx) {
		IK_Syslog(LOG_ERR, ".IK_AddMonTS was called by the client "
			  "transmiting to the server - with no ODL label");
	    }
	}
    }

    return(return_code);

}

/*****************************************************************************/

/*
 * Name: IK_AddHiResTimestamp()
 *
 * Description: This function adds the high-resolution time stamp to
 * the ODL tree. The resolution is determined by the resolution of the
 * gettimeofday() call, which is 10 milliseconds by default. The
 * MONITOR group also holds the session ID (obtained from the stats
 * module).
 *
 * Parameters:
 * AGGREGATE agg_root - Root AGGREGATE for the ODL the tree to add the
 * timestamp to
 * IK_BOOLEAN bool_tx - IK_TRUE if the tree is being transmitted,
 * IK_FALSE otherwise.
 *
 * Return Values:
 * int 0 - If the timestamp is added successfully
 * int -1 - If an error occurs
 * 
 * Warnings:
 *
 * Global Variables Used:
 * IK_ImsErrno
 *
 * Pre- and Post-Conditions:
 *
 * Notes:
 *
 * Revision History:
 *
 * Thursday 23 June 1994 (Eric Winter) - Changed argument types -
 * AGGREGATE * became AGGREGATE, timestamp argument removed since now
 * generated internally.
 *
 * Friday 29 September 1995 (Eric Winter) - Added session ID to
 * MONITOR group.
 *
 * */

static int
IK_AddHiResTimestamp(AGGREGATE agg_root, IK_BOOLEAN bool_tx)
{

    /* tv_timestamp holds the time to a higher (microsecond)
       resolution (the resolution of the gettimeofday() call). */
    struct timeval tv_timestamp;

    /* i_returnCode holds the execution status for this function. */
    int i_returnCode = 0;

    /* agg_workingRoot is the current tree being inspected. */
    AGGREGATE agg_workingRoot = NULL;

    /* agg_monitorGrp is a pointer to the monitor group. */
    AGGREGATE agg_monitorGrp = NULL;

    /* pc_timestampParamName is the string version of the name of the
       timestamp parameter to be added. */
    char *pc_timestampParamName = NULL;

    /* param_timestamp is a pointer to a paramter in the
       monitor group. */
    PARAMETER param_timestamp = NULL;

    /* valdat_seconds is the ODL-formatted version of the seconds part
       of the timestamp. */
    VALUE_DATA valdat_seconds;

    /* valdat_microseconds is the ODL-formatted version of the
       microseconds part of the timestamp. */
    VALUE_DATA valdat_microseconds;

    /* ac_timeBuf[] is the buffer to hold the string version of the
       time components. */
    char ac_timeBuf[IK_MAXBUFLEN];

    /* pc_sessionID points to the session ID string. */
    char *pc_sessionID = NULL;

    /* i_sessionIDLen is the length of the session ID string. */
    int i_sessionIDLen = 0;

    /* param_sessionID points to the session ID parameter (new or
       existing). */
    PARAMETER param_sessionID = NULL;

    /* valdat_sessionID is used to add the session ID string to the
       session ID parameter. */
    VALUE_DATA valdat_sessionID;

    /*-----------------------------------------------------------------------*/

    /* Validate input. */
    assert(agg_root != NULL);

    /*-----------------------------------------------------------------------*/

    /* Generate the timestamp to a higher precision. If an error
       occurs, create a timestamp of 0. */
    if (gettimeofday(&tv_timestamp, NULL) == -1) {
	IK_Syslog(LOG_ERR,
		  "IK_AddHiResTimestamp(): Error in call to gettimeofday()");
	(void) strcpy(ac_timeBuf, "0");
    }

    /*-----------------------------------------------------------------------*/

    /* Get the first sub-AGGREGATE of the root AGGREGATE. This
       sub-AGGREGATE contains the MESSAGE_ID PARAMETER and the MONITOR
       group (if it exists). If available, get them. */
    agg_workingRoot = NextAggregate(agg_root);
    if (agg_workingRoot == NULL) {

	/* There is no sub-AGGREGATE. */
	IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): No subaggregate of root");
	errno = EPROTO;
	IK_ImsErrno = IK_EPROTO;
	i_returnCode = -1;

    } else {

	/* Find the MONITOR group. If it does not exist, create it. */
	agg_monitorGrp = FindGroup(agg_workingRoot, TSGRP_NAME);
	if (agg_monitorGrp == NULL) {

	    /* The MONITOR group does not exist, so create it. */
	    agg_monitorGrp = NewAggregate(agg_workingRoot, KA_GROUP, TSGRP_NAME,
					  NULL);
	    if (agg_monitorGrp == NULL) {
		IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): Unable to create "
			  "the MONITOR group for message");
		errno = EPROTO;
		IK_ImsErrno = IK_EPROTO;
		i_returnCode = -1;

	    }

	}

    }

    /*-----------------------------------------------------------------------*/

    /* If this is a client, add the session ID if needed. */
    if (IK_Is_Server == IK_FALSE) {

	/* Check to see if the tree already has a session ID
	   parameter. If so, ignore it. If not, create one. */
	param_sessionID = FindParameter(agg_monitorGrp, SESSION_ID_PARAM_NAME);
	if (param_sessionID == NULL) {

	    /* No session ID yet, so add one. */
	    param_sessionID = NewParameter(agg_monitorGrp, KP_ATTRIBUTE,
					   SESSION_ID_PARAM_NAME);
	    if (param_sessionID == NULL) {
		IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): Can't allocate "
			  "new session ID parameter");
		errno = ENOBUFS;
		IK_ImsErrno = IK_ENOMEM;
		i_returnCode = -1;
	    }

	    /* Make sure the session ID parameter has a scalar type
	       (since it contains only one value). */
	    param_sessionID->value_kind = KV_SCALAR;

	    /* Get a pointer to the (static) session ID string. */
	    pc_sessionID = IK_GetSessionID();
	    if (pc_sessionID == NULL) {
		IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): No session ID!");
		errno = ENOBUFS;
		IK_ImsErrno = IK_ENOMEM;
		i_returnCode = -1;
	    }

	    /* Convert the session ID string to ODL format. */
	    i_sessionIDLen = strlen(pc_sessionID);
	    assert(i_sessionIDLen > 0);
	    valdat_sessionID = ODLConvertString(pc_sessionID, i_sessionIDLen);

	    /* Add the session ID value to the session ID parameter. */
	    if (NewValue(param_sessionID, &valdat_sessionID) == NULL) {
		IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): Unable to add the "
			  "session ID to MONITOR group.");
		errno = ENOBUFS;
		IK_ImsErrno = IK_ENOMEM;
		i_returnCode = -1;
	    }

	} else {

	    /* Session ID already exists, so do nothing. */

	}
    }

    /*-----------------------------------------------------------------------*/

    /* If there have been no errors so far... */
    if (i_returnCode == 0) {

	/* The timestamp needs to be processed differently if it is
	   being attached at the server end vs the client end... */
	if (IK_Is_Server) {

	    /* The timestamp is being added to a tree at the
               server. */
	    if (bool_tx) {

		/* The tree is being transmitted from the server. */
		pc_timestampParamName = TX_SERVER_PARAM_NAME;

	    } else {

		/* The tree is being received by the server. */
		pc_timestampParamName = RX_SERVER_PARAM_NAME;

	    }

	} else {

	    /* The timestamp is being added to a tree at the
               client. */
	    if (bool_tx) {

		/* The tree is being transmitted from the client. */
		pc_timestampParamName = TX_CLIENT_PARAM_NAME;

	    } else {

		/* The tree is being received by the client. */
		pc_timestampParamName = RX_CLIENT_PARAM_NAME;

	    }

	}

	/* If a value (or pair of values) exists for the current
	   timestamp parameter, remove it (or them). */
	param_timestamp = FindParameter(agg_monitorGrp, pc_timestampParamName);
	if (param_timestamp != NULL) {

	    /* The desired parameter exists, so remove any existing
	       values for it. */
	    while (FirstValue(param_timestamp) != NULL) {
		(void) RemoveValue(FirstValue(param_timestamp));
	    }

	} else {

	    /* The desired timestamp parameter does not exist, so
	       create it. */
	    param_timestamp = NewParameter(agg_monitorGrp, KP_ATTRIBUTE,
					   pc_timestampParamName);
	    if (param_timestamp == NULL) {
		IK_vSyslog(LOG_ERR, "IK_AddHiResTimestamp(): Unable to create "
			   "timestamp parameter, errno = %d - %s",
			   errno, strerror(errno));
		i_returnCode = -1;
		IK_ImsErrno = IK_ENOMEM;
	    }

	}

	/* Make sure the timestamp parameter has a sequence type
	   (since it contains a pair of values). */
	param_timestamp->value_kind = KV_SEQUENCE;

	/* Convert the high-resolution timestamp components to ODL
	   format. */
	(void) sprintf(ac_timeBuf, "%ld", tv_timestamp.tv_sec);
	valdat_seconds = ODLConvertString(ac_timeBuf, strlen(ac_timeBuf));
	(void) sprintf(ac_timeBuf, "%ld", tv_timestamp.tv_usec);
	valdat_microseconds = ODLConvertString(ac_timeBuf, strlen(ac_timeBuf));

	/* Add the seconds part of the timestamp to the timestamp
	   PARAMETER. */
	if (NewValue(param_timestamp, &valdat_seconds) == NULL) {
	    IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): Unable to add the "
		      "seconds timestamp to MONITOR group.");
	    errno = ENOBUFS;
	    IK_ImsErrno = IK_ENOMEM;
	    i_returnCode = -1;
	}

	/* Add the microseconds part of the timestamp to the timestamp
	   PARAMETER. */
	if (NewValue(param_timestamp, &valdat_microseconds) == NULL) {
	    IK_Syslog(LOG_ERR, "IK_AddHiResTimestamp(): Unable to add the "
		      "microseconds timestamp to MONITOR group.");
	    errno = ENOBUFS;
	    IK_ImsErrno = IK_ENOMEM;
	    i_returnCode = -1;
	}

	/* Set the rows and columns members of the parameter structure
	   appropriately. */
	param_timestamp->rows = 1;
	param_timestamp->columns = param_timestamp->value_count;

    }

    /*-----------------------------------------------------------------------*/

    /* Return the execution status. */
    return(i_returnCode);

}

/*****************************************************************************/
/*
 * Name: IK_ScrubODL()
 *
 * Description: This function upcases the contents of campaign,
 *              sensor_name, source_name, and parameter.
 *
 * Parameters:
 * AGGREGATE root - Root AGGREGATE of the ODL tree to scrub.
 *
 * Return Values: None
 *
 * Warnings:
 *
 * Global Variables Used:
 *
 * Pre- and Post-Conditions:
 *
 * Notes:
 *
 * Revision History:
 *
 * */

void IK_ScrubODL(AGGREGATE root)
{
   AGGREGATE agg=NULL;    /* root of aggregate to search */
   PARAMETER para=NULL;   /* parameter to upcase */
   VALUE val=NULL;        /* value of parameter to upcase */
   int string_i=0;        /* index into item.value.string */

   /* Validate input */
   assert (root != NULL);

   agg = root;

   /* search each aggregate starting at the root */
   while ((agg = NextAggregate(agg)) != NULL) {
     para = FirstParameter(agg);

     /* if no parameters, goto next aggregate */
     if (para == NULL)  {
       continue;
     }

     /* search each parameter starting at First parameter */
     do {

       /* check if this parameter is one to be upcased */
       if (!strcmp (para->name, "CAMPAIGN") ||
           !strcmp (para->name, "SOURCE_NAME") ||
           !strcmp (para->name, "PARAMETER") ||
           !strcmp (para->name, "SENSOR_NAME") || 
           !strcmp (para->name, "DATASET_ID") ||
           !strcmp (para->name, "DATA_CENTER_ID")) {

         /* yes it is... upcase all values */
         switch (para->value_kind) {

         case KV_SCALAR:

           val = FirstValue(para);

           /* for each character, upcase it with toupper */
           for (string_i=0; string_i<val->item.length; string_i++) {
             val->item.value.string[string_i] =
                              toupper(val->item.value.string[string_i]);
           }
           break;

         case KV_SEQUENCE:

           /* for each value of the sequence... */
           for (val=FirstValue(para); val!=NULL; val=NextValue(val)) {

             /* for each character, upcase it with toupper */
             for (string_i=0; string_i<val->item.length; string_i++) {
               val->item.value.string[string_i] =
                              toupper(val->item.value.string[string_i]);
             }
           }
           break;

         default:

           IK_vSyslog (LOG_ERR, "IK_ScrubODL(): unrecognized parameter"
                  " type (file \"%s\", line %d) \n", rcsid,__LINE__);

         }
       }
     } while ((para = NextParameter(para)) != NULL);
   }
}
