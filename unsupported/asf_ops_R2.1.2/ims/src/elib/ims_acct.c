static char *sccs = "@(#)ims_acct.c	5.1  16 Mar 1996";
/*****************************************************************************
**
** File:        ims_acct.c
**
** Function:    Account processing routines.
**
** Author:      Armando Cardona
**
** Date:        6/1/95
**
** Modified:    1/24/96 - Alin Tilden
**              Added checkConnection subroutine.
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_util.h>
#include <ims_acctCat.h>
#include <ims_acct.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_acct.h.
** They are listed here for reference.
**
**	int ims_validUser (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, char *,
**		char *, int, char *);
**	int ims_validAcct (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, char *,
**		int *, int, unsigned char);
**	int ims_acctTran (IMS_QI_DESC_OBJ *, IMS_MSG_STRUCT *, char *,
**		int, float, IMS_ACCT_TRANS_TYPE);
*/


/*
** Local function prototypes
*/
static int checkConnection (IMS_QI_DESC_OBJ *);

/****************************************************************************
**
** FUNCTION: ims_validUser
**
** DESCRIPTION: Validates that the user(user_id) with password (password)
**		has assignment on the account(acc_id).
**
** INPUT:	qDesc; msgDesc; user_id; password; 
**		crypt_flag = 0 : password not crypted, 
**		crypt_flag > 0 : password is crypted;
**		acc_id
**		
**
** OUTPUT:	IMS_OK if succeed. != IMS_OK otherwise.
**
****************************************************************************/

int ims_validUser (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc,
	char *user_id,
	char *password,
	int crypt_flag,
	char *acc_id)
{
	char buffer [IMS_COL255_LEN+1] ;
	char pass [IMS_COL128_LEN+1] ;
	int count ;
	char *savePtr;

	/* 
	** Check for valid input data                                          
	*/
	if ( msgDesc == (IMS_MSG_STRUCT *)NULL )
	{
		(void) printf("msgDesc in ims_user_validation is NULL\n") ;
		return (IMS_FATAL) ;
	}

	/*
	**  Check for an active database connection.
	*/
	if ((checkConnection (qDesc)) < IMS_OK )
	{
	    (void) ims_msg (msgDesc, IMS_FATAL,
			    "The database server connection was not valid.");
	    return (IMS_FATAL);
	}

	/*
	** Check that user passed a user id and account id.
	*/
	if ( (user_id == (char *)NULL) || (acc_id == (char *)NULL))
	{
	    (void) ims_msg (msgDesc, IMS_FATAL,
		"user_id, or acc_id are NULL in ims_user_validation");
	    return(IMS_FATAL);
	}

	/* 
	** Prepare password
	*/

	if ( (password == NULL) || !(*password) )
		(void) strcpy (pass,"") ;
	else
		(void) strcpy (pass,password) ;

	(void) sprintf( buffer, "exec acc_validate_user '%s', '%s', %d, '%s'",
                                     user_id, pass, crypt_flag, acc_id ) ;
	savePtr = qDesc->cmd;

	qDesc->cmd = buffer;

	/*
	** Execute query
	*/

	if ( ims_acctCat (qDesc, msgDesc, IMS_ACCT_VALIDATE_USER, 
                    (void *)NULL) < IMS_OK )
	{
    	  (void) ims_msg (msgDesc, IMS_ERROR,"Invalid User");
		  qDesc->cmd = savePtr;
          return (IMS_ERROR) ;
	} 

	qDesc->cmd = savePtr;
	return (IMS_OK) ;
	

}/* end of ims_user_validation */

/****************************************************************************
**
** FUNCTION: ims_validAcct
**
** DESCRIPTION: Validates that the account(acc_id)
**		has assignment on the datasets(dataset_list) with the
**		given permissions(perm_mask).
**		Permissions are set by setting the bits 5, 4, 3, 2, and 1
**		of perm_mask where bit 1 correspond to the LSB. These five
**		bits correspond to O(order), A(add), G(get), D(delete), and
**		R(replace).
** INPUT:	Assumes that qDesc and msgDesc have valid values, and
**		dataset_list is at least dataset_cnt long.
**
** OUTPUT:	IMS_OK if succeed. != IMS_OK otherwise.
**
****************************************************************************/

int ims_validAcct (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc,
	char *acc_id,
	int *dataset_list,
	int dataset_cnt,
	unsigned char perm_mask)
{
    char buffer [IMS_COL255_LEN+1] ;
    char message [2*IMS_COL255_LEN+1] ;
    int i ;
    unsigned char oagdr ;
    char *savePtr;

    /* 
    ** Check for valid input data
    */
    if ( msgDesc == (IMS_MSG_STRUCT *)NULL )
    {
	(void) printf("msgDesc in ims_user_validation is NULL\n") ;
	return (IMS_FATAL) ;
    }

    /*
     **  Check for an active database connection.
     */
    if ((checkConnection (qDesc)) < IMS_OK )
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
              "The database server connection was not valid.");
	return (IMS_FATAL);
    }

    /*
    ** Verify that a valid account id and dataset_list was passed.
    */
    if ((acc_id == (char *)NULL) 
	|| (dataset_list == (int *)NULL) || ( dataset_cnt <= 0 ))
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
	    "acc_id, or dataset_list are NULL in ims_acc_validation");
	return(IMS_FATAL);
    }

    /*
    ** Query database for every dataset.
    */
    savePtr = qDesc->cmd;

    for ( i = 0 ; i < dataset_cnt ; i++ )
    {
	if ( dataset_list[i] == NULL )
	{
	    (void) ims_msg (msgDesc, IMS_FATAL,
	        "ims_acct_validation: dataset_list has a NULL value.");
            return (IMS_FATAL) ;
	} 

	(void) sprintf (buffer,
			"exec acc_validate_account '%s', %d", acc_id,
		        dataset_list[i]);
	qDesc->cmd = buffer;

	if ( ims_acctCat (qDesc, msgDesc, IMS_ACCT_VALIDATE_ACCOUNT, 
			  (void *) &oagdr ) < IMS_OK)
	{
	    (void) sprintf(message,
             "ims_acct_validation: dataset %d is not assigned to account %s",
	     dataset_list[i], acc_id ) ;
	    (void) ims_msg (msgDesc, IMS_ERROR, message);
	    qDesc->cmd = savePtr;
            return (IMS_ERROR) ;
	} 

	/*
	** Check the permissions now 
	*/
	if ( (perm_mask & oagdr) != perm_mask )
	{
	    (void) sprintf(message,
		"ims_acct_validation: permission %d \
		is not enough for account %s on dataset %d",
                (int)perm_mask, acc_id , dataset_list[i]) ;
	    (void) ims_msg (msgDesc, IMS_ERROR, message);
	    qDesc->cmd = savePtr;
	    return (IMS_ERROR) ;
	} 
          
    }/* end for */	

    qDesc->cmd = savePtr;
    return ( IMS_OK ) ;

}/* end of ims_account_validation */

/****************************************************************************
**
** FUNCTION: ims_acctTran
**
** DESCRIPTION: 
**	  - According to the values of "type"(input parameter) it updates
**	    the following fields in the account table:
**	     
**	      1(CREDIT) or 2(DEBIT)(if there are enough resources)
**	      updates begin and current balance.
**	      3(DEBIT-BEGIN) updates current balance (if there are enough
**	      resources) and hold balance.
**	      4(DEBIT-END) updates hold balance( if it doesn't go negative).
**	      5(DEBIT-ROLLBACK) updates current and hold balances
**					     ( if it doesn't go negative).
**	  - Creates an entry in the account_transaction table.
**
** INPUT:	Assumes that qDesc and msgDesc have valid values.
**		account_id, order_id(-1=NULL for operator),
**		amount(>0) for updating, and type ( as described above )
**
** OUTPUT:	IMS_OK if succeed. != IMS_OK otherwise.
**
****************************************************************************/

int ims_acctTran (
	IMS_QI_DESC_OBJ *qDesc,
	IMS_MSG_STRUCT *msgDesc,
	char *account_id,
	int order_id,
	float amount, 
	IMS_ACCT_TRANS_TYPE type)
{
    char buffer [IMS_COL255_LEN+1] ;
    char Msg [ IMS_COL255_LEN+1] ;
    char pass [IMS_COL128_LEN+1] ;
    char *savePtr;

    /*
    ** Check for valid input data
    */
    if ( msgDesc == (IMS_MSG_STRUCT *)NULL )
    {
	(void) printf("msgDesc in ims_account_transaction is NULL\n") ;
	return (IMS_FATAL) ;
    }

    /*
    **  Check for an active database connection.
    */
    if ((checkConnection (qDesc)) < IMS_OK )
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
              "The database server connection was not valid.");
	return (IMS_FATAL);
    }

    /*
    ** Verify that the account_id and amount parameters are not null
    */
    if ((account_id == (char *)NULL) || (amount <= 0.0))
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
	    "acc_id, or amount are NULL in ims_account_transaction");
	return(IMS_FATAL);
    }

    if (ims_acctCat(qDesc, msgDesc,IMS_ACCT_BEGINTRANSACTION,NULL) < IMS_OK)
    {
	/* 
	** Display error messages
	*/
	(void) sprintf (Msg, 
			"ims_account_transaction: IMS_ACCT_BEGINTRANSACTION"
			" failed for Account Id: %s",account_id);   
	(void) ims_msg (msgDesc,  IMS_FATAL, Msg); 
	return (IMS_FATAL); 
    }

    if ( ims_acctCat (qDesc, msgDesc, IMS_ACCT_GETACCOUNTLOCK, 
		      (void *)NULL) < IMS_OK )
    {
	/*
	** Rollback transaction 
	*/
	(void) ims_acctCat (qDesc, msgDesc, IMS_ACCT_ROLLBACKTRANSACTION,NULL);
	
       	/*
	** Display error messages 
	*/

	(void) sprintf (Msg, 
			"ims_account_transaction: IMS_ACCT_GETACCOUNTLOCK"
			" failed for Account Id: %s",account_id);   
	(void) ims_msg (msgDesc,IMS_FATAL, Msg); 
	return (IMS_FATAL);
    }

    /*
    ** Create query
    */
    if ( order_id < 0 )
	(void) strcpy (pass, "NULL") ;
    else
	(void) sprintf(pass,"%d", order_id ) ;
    
    (void) sprintf (buffer,"acc_update_balance '%s', %s, %f, %d",
		    account_id, pass, amount, type ) ;

    savePtr = qDesc->cmd;
    qDesc->cmd = buffer ;

    if (ims_acctCat (qDesc, msgDesc, IMS_ACCT_ACCOUNT_TRANSACTION, NULL)
	< IMS_OK)
    {
	/*
	** Rollback transaction
	*/
	(void) ims_acctCat ( qDesc, msgDesc, IMS_ACCT_ROLLBACKTRANSACTION,NULL);

	/*
	** Display error messages 
	*/
	(void) sprintf (Msg, 
		        "ims_account_transaction: IMS_ACCT_ACCOUNT_TRANSACTION"
			" failed for Account Id: %s",account_id);   
	(void) ims_msg (msgDesc,IMS_FATAL, Msg); 
	
	qDesc->cmd = savePtr;
	return (IMS_ERROR);
    }

    if ( ims_acctCat (qDesc,msgDesc,IMS_ACCT_COMMITTRANSACTION,NULL) < IMS_OK) 
    {
	/* 
	** Rollback transaction 
	*/
	(void) ims_acctCat (qDesc, msgDesc, IMS_ACCT_ROLLBACKTRANSACTION,NULL);

	/* 
	**Display error messages 
	*/
	(void) sprintf (Msg,
			"ims_account_transaction: IMS_ACCT_COMMITTRANSACTION"
			" failed for Account Id: %s",account_id);   
	(void) ims_msg (msgDesc, IMS_FATAL, Msg); 
	
	qDesc->cmd = savePtr;
	return (IMS_FATAL);
    }

    qDesc->cmd = savePtr;
    return ( IMS_OK ) ;

}/* end of ims_account_transaction */

/******************************************************************************
**
** checkConnection ()
**
** Check the qDesc pointer for a valid database server connection.
**
******************************************************************************/

static int checkConnection (
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;

	/*
	** If this in NULL we can't possibly have a connection.
	*/
	if (qDesc  == (IMS_QI_DESC_OBJ *) NULL)
	{
		return (IMS_ERROR);
	}
	else
	{
		/*
		** Resetting the query descriptor will validate it.
		*/
		if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
		{
			return (status);
		}
		else
		{
			/*
			** See if the DBPROCESS has been marked dead.
			*/
			if (DBDEAD (qDesc->dbproc) == TRUE)
			{
				return (IMS_ERROR);
			}
		}
	}
	return (IMS_OK);
}  /* checkConnection */

