/******************************************************************************
**
** File:        ims_childHandler.h
**
** Function:    This file includes definitions and function
**              prototypes for the ims_childHandler.c file.
**
** Author:      S. Hardman
**
** Date:        9/6/95
**
******************************************************************************/

#ifndef _IMS_CHILDHANDLER_H
#define _IMS_CHILDHANDLER_H

static char *sccsChildHandler = "@(#)ims_childHandler.h	5.1  03/17/96";

/*
** Constant Definitions.
*/
#define MAX_ARGS 100

/*
** Function prototypes for the ims_childHandler.c module.
*/
pid_t ims_startChild (IMS_MSG_STRUCT *, char *, ...);
pid_t ims_startChild2 (IMS_MSG_STRUCT *, char *, ...);
int ims_waitForChild (IMS_MSG_STRUCT *, pid_t);

#endif	/* !_IMS_CHILDHANDLER_H */
