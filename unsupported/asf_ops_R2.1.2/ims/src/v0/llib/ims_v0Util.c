static char *sccs = "@(#)ims_v0Util.c	1.2  06/09/97";
/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Util.c
**
** Purpose
**		Generic functions utilized in v0 server.
**
**	Creator   :   Julie Wang
**
**	Date      :   Aug 5, 1996 
**
************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <string.h>
#include <IK_Network.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>

/***********************************************************************
**      
** send_email - generic function to construct and sends e-mail 
**    
**    subject  is the subject of the message (optional)
**    message  is the body of the message (required)
**    address1 is the recepient's e-mail address (required)
**    address2 is the e-mail address(es) for the cc list (optional)
**
************************************************************************/           
int v0_util__send_email      (IMS_MSG_STRUCT *msgDesc,
                               char *subject,
                               char *message,
													     char *address1,
                               char *address2)

{

	FILE  *fp;                     /* temp file to store the message */
	char  *temp_name;
	char  command[IMS_COL512_LEN];
	char  *p;

	if (address1 == (char *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0_util__send_email: Missing recepient's name.");

		return (IMS_ERROR);
	}

	if (message == (char *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0_util__send_email: Missing message content.");

		return (IMS_ERROR);
	}

	/*
	** initialize a temporary file to store the message 
	*/

	if ( (temp_name = tmpnam(NULL)) == NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0_util__send_email: Failed to get a name for the temporary file.");
		free(temp_name);
		return (IMS_ERROR);
	}

	if ( (fp = fopen(temp_name, "w")) == NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0_util__send_email: Failed to create a temporary file.");
		free (temp_name); 
		return (IMS_ERROR);
		
	}

	/*
	** write message into a file
	*/
	fprintf (fp, message);
	fflush (fp);
	fclose (fp);

	/*
	** create the mail command
	*/

	command[0] = '\0';
	p = command;

	strcpy (p, "Mail");
	p = p + strlen(p);

	if (subject != (char *)NULL)
	{
		sprintf (p, " -s \"%s\"", subject);
		p = p + strlen(p);
	}

	if (address2 != (char *)NULL)
	{
		sprintf (p, " -c \"%s\"", address2);
		p = p + strlen(p);
	}

	sprintf (p, " %s < %s", address1, temp_name);
	p = p + strlen(p);

	if ( system(command) != 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0_util__send_email: Failed to send e-mail to %s for "
			"subject: %s.",
			address1, subject);

		unlink (temp_name);
		free (temp_name);
		return (IMS_ERROR);
	}

	unlink (temp_name);
	free (temp_name);

	return (IMS_OK); 

} /* end of v0_util__send_email */

/*************************************************************************
**
** set_status_code_comment
**
** Purpose: Attach the comment string to the result structure which will
**          be returned to the client.  The caller of this routine should
**	    make sure that comment length is <= IMS_COL255_LEN.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
int
v0_util__set_status_code_comment (V0_DESC_STRUCT *v0Desc, char *comment, 
			 char *statusCode)
{
	V0_ERR_LIST	*comment_p;

	/*
	** If there is already something in the list,
	** go to the end of the odl_status_code_comment list
	** and allocate a new node at the end where the comment is inserted.
	*/
	if (v0Desc->result.odl_status_code_comment != 
		(V0_ERR_LIST *) NULL)
	{
		for (comment_p = v0Desc->result.odl_status_code_comment;
			comment_p->next_p != (V0_ERR_LIST *) NULL;
			comment_p = comment_p->next_p);

		if ( (comment_p->next_p = (V0_ERR_LIST *)
			malloc(sizeof(V0_ERR_LIST))) == NULL)
		{
			(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
				"Memory allocation failed for "
				"odl_status_code_comment.");
			(void) strcpy (v0Desc->odl_status, "19");
			return (IMS_FATAL);
		}
		comment_p = comment_p->next_p;
	}
	/*
	** Allocate a new node for the comment
	*/
        else 
	{
		if ( (v0Desc->result.odl_status_code_comment = (V0_ERR_LIST *)
                          malloc(sizeof(V0_ERR_LIST))) == NULL)
        	{
	                (void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
                                "Memory allocation failed for "
                                "odl_status_code_comment.");
	                (void) strcpy (v0Desc->odl_status, "19");
	                return(IMS_FATAL);
		}
		comment_p = v0Desc->result.odl_status_code_comment;
        }
        /*
        ** Copy the input
        */
        (void)strcpy(v0Desc->odl_status, statusCode);
        (void)strcpy(comment_p->err_buf,comment);
        comment_p->next_p = (V0_ERR_LIST *)NULL;

        return (IMS_OK);
 
} /* end of set_status_code_comment */

