/******************************************************************************
**
** File:        ims_acctCat.h
**
** Function:    This is the header file for the ims_acctCat.c functions which
**              perform catalog queries and updates for the account processing.
**
** Author:      J. Armando Cardona
**
** Date:        6/26/95
**
******************************************************************************/

#ifndef _IMS_ACCTCAT_H
#define _IMS_ACCTCAT_H

static char *sccsAcctCat = "@(#)ims_acctCat.h	5.1  16 Mar 1996";

/*
** Enumerated types for account catalog events.
*/
typedef enum ImsAcctCatEvent
{
	IMS_ACCT_VALIDATE_USER,
	IMS_ACCT_VALIDATE_ACCOUNT,
	IMS_ACCT_ACCOUNT_TRANSACTION,
	IMS_ACCT_BEGINTRANSACTION,
	IMS_ACCT_ROLLBACKTRANSACTION,
	IMS_ACCT_COMMITTRANSACTION,
	IMS_ACCT_GETACCOUNTLOCK
} IMS_ACCT_CAT_EVENT;

/*
** Function Prototype for the ims_acctCat.c module.
*/
int ims_acctCat (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *,
	IMS_ACCT_CAT_EVENT, void *);

#endif	/* !_IMS_ACCTCAT_H */
