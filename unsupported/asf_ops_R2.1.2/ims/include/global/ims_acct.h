/*****************************************************************************
**
** File:        ims_acct.h
** 
** Function:    This header file defines enumerated types and the function
**              prototypes used in ims_acct.c.
**
** Author:      J. Armando Cardona
**
** Date:        06/02/95
**
*****************************************************************************/

#ifndef _IMS_ACCT_H
#define _IMS_ACCT_H

static char *sccsAcct = "@(#)ims_acct.h	5.1  16 Mar 1996";

/*
** Enumerated types for account transactions.
*/
typedef enum ims_acct_trans_type
{
	CREDIT = 1,
	DEBIT = 2,
	DEBIT_BEGIN = 3,
	DEBIT_END = 4,
	DEBIT_ROLLBACK = 5
}IMS_ACCT_TRANS_TYPE ;


/*
** Function Prototypes for the ims_acct.c module.
*/
int ims_validUser (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, char *,
	char *, int, char *);
int ims_validAcct (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, char *,
	int *, int,  unsigned char);
int ims_acctTran (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, char *,
	int, float, IMS_ACCT_TRANS_TYPE);                                                            
#endif	/* !_IMS_ACCT_H */
