/*=============================================================================
File:		llist_err.h
Date:		11/29/1989
Author(s):	Tim Graham, Steve Wright, Steve Parr

Description:	This file contains the definitions for the values which will
		be given to the llist_errno global variable.
Notes:		If this file is updated be sure to update llist_err.c .
Sccs:		@(#)llist_err.h	1.1  11/21/96
=============================================================================*/

#ifndef		_LLIST_ERR_
#define		_LLIST_ERR_

extern	int	llist_errno;
extern	int	llist_nerr;
extern	char	*llist_errs[];

				/*
				 * Linked list errors.
				 */

#define ELISTINIT	1	/* Could not initialize a linked list 	   */
#define ELISTREINIT	2	/* Tried to reinitialize a linked list 	   */
#define EBADLIST	3	/* Bad ptr to list in linked list call 	   */
#define ELISTNOENTRY	4	/* Tried to insert a NULL entry in list    */
#define ELINKCREATE	5	/* Was unable to create a link in list 	   */
#define ENOCURSOR	6	/* NULL ptr to cursor in linked list call  */
#define ELISTNOCF	7	/* NULL cmp func ptr in linked list call   */
#define ELISTCFERR	8	/* user entry compare func returned ERROR  */
#define ELISTEMPTY	9	/* the referenced linked list is empty 	   */
#define EENDOFLIST	10	/* tried to retrieve past end of list 	   */
#define EBEGOFLIST	11	/* tried to retrieve before start of list  */
#define ELISTDESTROY	12	/* Tried to destroy a static list 	   */
#define ENULLCURSOR	13	/* Cursor pointed to by cursor ptr is NULL */
#define ELMALLOC	14	/* Couldn't allocate memory		   */
#define	ELNOSUCHKEY	15	/* No entry with the key was found in list */ 


#endif /* _LLIST_ERR_ */


/* End of File */
