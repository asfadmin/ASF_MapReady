/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	mutex_odllib.c

Description:	Provides a mutex protected interface for functions which access
		the ODL library.

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)mutex_odllib.c	1.1    11/21/96";

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include "odldef.h"
#include "odlinter.h"
#include "ODLcommonhdr.h"
#include "PPSdefs.h"
#include "PPSextern.h"

extern pthread_mutex_t  g_mutex_for_odllib;

/*==============================================================================
Function:	create_order_status_buf_mx
Description:	put a mutex wrapper around create_order_status_buf
Parameters: 	same as the original function
Returns:	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		use mutex to protect calls to ODL library
==============================================================================*/
#ifdef __STDC__
void    create_order_status_buf_mx(
        IMS_Order_Status        *status_rec,
        char                    *buffer,
        int                     sizeof_buffer)
#else
void    create_order_status_buf_mx(status_rec, buffer, sizeof_buffer)
        IMS_Order_Status        *status_rec;
        char                    *buffer;
        int                     sizeof_buffer;
#endif
{
        /* lock the mutex lock for call to odl lib */
        pthread_mutex_lock(& g_mutex_for_odllib);
 
        /* create ODL format message */
        create_order_status_buf(status_rec, buffer, sizeof_buffer);
 
        /* release the lock */
        pthread_mutex_unlock(& g_mutex_for_odllib);
}

/*==============================================================================
Function:	ingest_CP_msg_mx
Description:	put a mutex wrapper around ingest_CP_msg
Parameters: 	same as the original function
Returns:	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		use mutex to protect calls to ODL library
==============================================================================*/
#ifdef __STDC__
int ingest_CP_msg_mx(char *infile, char buffer[MSG_SIZE], int *msg_type,
    Common_Header_Record **p_hdr, void **p_rec)
#else
int ingest_CP_msg_mx(infile, buffer, msg_type, p_hdr, p_rec)
   char *infile ;
   char buffer[MSG_SIZE] ;
   int  *msg_type ;
   Common_Header_Record **p_hdr;
   void **p_rec)
#endif
{
	int ret;

	/* lock the mutex lock for call to odl lib */
	pthread_mutex_lock(& g_mutex_for_odllib);
 
        /* create ODL format message */
        ret = ingest_CP_msg(infile, buffer, msg_type, p_hdr, p_rec);

	/* release the lock */
	pthread_mutex_unlock(& g_mutex_for_odllib);

	return (ret);
}

/*==============================================================================
Function:	ingest_IMS_msg_mx
Description:	put a mutex wrapper around ingest_IMS_msg
Parameters: 	same as the original function
Returns:	void
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		use mutex to protect calls to ODL library
==============================================================================*/
#ifdef __STDC__
int ingest_IMS_msg_mx(char *infile, char buffer[MSG_SIZE], int *msg_type,
    Common_Header_Record **p_hdr, void **p_rec)
#else
int ingest_IMS_msg_mx(infile, buffer, msg_type, p_hdr, p_rec)
   char *infile ;
   char buffer[MSG_SIZE] ;
   int  *msg_type ;
   Common_Header_Record **p_hdr;
   void **p_rec)
#endif
{
	int ret;

	/* lock the mutex lock for call to odl lib */
	pthread_mutex_lock(& g_mutex_for_odllib);
 
        /* create ODL format message */
        ret = ingest_IMS_msg(infile, buffer, msg_type, p_hdr, p_rec);

	/* release the lock */
	pthread_mutex_unlock(& g_mutex_for_odllib);

	return (ret);
}

/*==============================================================================
Function:       create_CP_Framejob_buf_mx
Description:    put a mutex wrapper around create_CP_Framejob_buf
Parameters:     same as the original function
Returns:        void
Creator:        Nadia Adhami
Creation Date:  5/1/1995
Notes:          use mutex to protect calls to ODL library
==============================================================================*/
#ifdef __STDC__
void    create_CP_Framejob_buf_mx(
        IMS_L1PReq_Record	*rec,
        char                    *buffer,
        int                     sizeof_buffer)
#else
void    create_CP_Framejob_buf_mx(rec, buffer, sizeof_buffer)
        IMS_L1PReq_Record	*rec;
        char                    *buffer;
        int                     sizeof_buffer;
#endif
{
	/* lock the mutex lock for call to odl lib */
	pthread_mutex_lock(& g_mutex_for_odllib);
 
        /* create ODL format message */
	create_CP_Framejob_buf(rec, buffer, sizeof_buffer);

	/* release the lock */
	pthread_mutex_unlock(& g_mutex_for_odllib);
}



/*==============================================================================
Function:       create_CP_Scanjob_buf_mx
Description:    put a mutex wrapper around create_CP_Scanjob_buf
Parameters:     same as the original function
Returns:        void
Creator:        Nadia Adhami
Creation Date:  5/1/1995
Notes:          use mutex to protect calls to ODL library
==============================================================================*/
#ifdef __STDC__
void    create_CP_Scanjob_buf_mx(
        IMS_ScanReq_Record	*rec,
        char                    *buffer,
        int                     sizeof_buffer)
#else
void    create_CP_Scanjob_buf_mx(rec, buffer, sizeof_buffer)
        IMS_ScanReq_Record	*rec;
        char                    *buffer;
        int                     sizeof_buffer;
#endif
{
	/* lock the mutex lock for call to odl lib */
	pthread_mutex_lock(& g_mutex_for_odllib);
 
        /* create ODL format message */
	create_CP_Scanjob_buf(rec, buffer, sizeof_buffer);

	/* release the lock */
	pthread_mutex_unlock(& g_mutex_for_odllib);
}

/* End of File */
