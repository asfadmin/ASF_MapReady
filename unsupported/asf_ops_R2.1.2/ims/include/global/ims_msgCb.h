/******************************************************************************
**
** File:        ims_msgCb.h
**
** Function:    Header file for message facility callback prototypes.
**
** Author:      S. Hardman
**
** Date:        1/7/97
**
******************************************************************************/

#ifndef _IMS_MSGCB_H
#define _IMS_MSGCB_H

static char *sccsMsgCb = "@(#)ims_msgCb.h	1.1  03/13/97";

/*
** Function prototypes for ims_msgCb.c.
*/
extern CS_RETCODE CS_PUBLIC ims_msgClientCb PROTOTYPE((
	CS_CONTEXT *context,
	CS_CONNECTION *connection,
	CS_CLIENTMSG *errmsg
	));

extern CS_RETCODE CS_PUBLIC ims_msgServerCb PROTOTYPE((
	CS_CONTEXT *context,
	CS_CONNECTION *connection,
	CS_SERVERMSG *srvmsg
	));

#endif  /* !_IMS_MSGCB_H */
