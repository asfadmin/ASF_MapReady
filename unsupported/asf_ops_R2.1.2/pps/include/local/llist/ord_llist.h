/*==============================================================================
Filename:	ord_llist.h
Description:	This file contains the definition of the contents of the DAPPS 
		ordered linked list object.
Creator:	Tim Graham
Notes:		All constructors and method definitions for this object are
		contained in the file dapps_ord_list.c.  See also dapps_list.h
		and llist.h.  The former contains type and macro definitions
		needed for the use of linked lists in general, while the
		latter contains the contents of the DAPPS unordered linked list
		object.
SCCS Info:
		@(#)ord_llist.h	1.1  11/21/96
==============================================================================*/


				/*
				 *  Public part
				 */

void		*(*insert)();		/* Insert entry into ordered list */
void		*(*get_by_key)();	/* Retrieve entry with a given key */
void		*(*get_by_cursor)();	/* Retrieve entry at cursor */
void		*(*get_first)();	/* Retrieve first entry in list */
void		*(*get_last)();		/* Retrieve last entry in list */
void		*(*get_next)();		/* Retrieve next entry in list */
void		*(*get_prev)();		/* Retrieve previous entry in list */
int		(*del_by_key)();	/* Delete entry with given key */
int		(*del_by_cursor)();	/* Delete entry at cursor position */
int		(*rm_all_entries)();	/* Remove all entries from list */
int		(*destroy_list)();	/* Destroy list (if possible ) */



				/*
				 *  Private part
				 */

int		initialized;		/* (TRUE/FALSE) - set if init */
int		num_elts;		/* Number of elements in the list */
int		(*ins_cmp_func_p)();	/* Function for sorting entries in */
cursor		(*create_link)();	/* Create link to point to entry */
cursor		head;			/* head of linked list */
cursor		tail;			/* tail of linked list */
