#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	llist.h
Description:	This file contains the definition of the DAPPS generic unordered
		linked list object.

Creator:	Tim Graham
Notes:		All constructors and method definitions for this object are
		contained in the file dapps_list.c.  See also ord_llist.h and
		dapps_list.h.  The former contains the definition of the ordered
		linked list object.  The latter contains the constructors for
		this object.

==============================================================================*/
#pragma ident	"@(#)llist.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/global/SCCS/s.llist.h"

#ifndef _LLIST_H_
#define _LLIST_H_

				/*
				 *  Public part
				 */
void		*(*append)();		/* Append an entry to a linked list */
void		*(*prepend)();		/* Prepend an entry to a linked list */
void		*(*ins_by_cursor)();	/* Insert entry before cursor pos. */
void		*(*get_by_key)();	/* Retrieve entry with a given key */
void		*(*get_by_cursor)();	/* Retrieve entry at cursor */
void		*(*get_first)();	/* Retrieve first entry in list */
void		*(*get_last)();		/* Retrieve last entry in list */
void		*(*get_next)();		/* Retrieve next entry in list */
void		*(*get_prev)();		/* Retrieve previous entry in list */
int		(*del_by_key)();	/* Delete entry with given key */
int		(*del_by_cursor)();	/* Delete entry at cursor position */
void		*(*unlink_by_cursor)();	/* Unlink (no del) entry at cursor position */
int		(*rm_all_entries)();	/* Remove all entries from list */
int		(*destroy_list)();	/* Destroy list (if possible ) */


				/*
				 *  Private part
				 */

int		initialized;		/* (TRUE/FALSE) - set if init */
int		num_elts;		/* number of elements in the list */
cursor		(*create_link)();	/* Create link to point to entry */
cursor		head;			/* head of linked list */
cursor		tail;			/* tail of linked list */

#endif	/* _LLIST_H_ */
