/*==============================================================================
Filename:	dapps_list.h
Description:	This file contains the type and macro definitions necessary 
		for the use of DAPPS generic linked lists.
Creator:	Tim Graham
Notes:		See also llist.h, ord_llist.h, dapps_list.c, dapps_ord_list.c
		The constructor and initializers for the llist object are
		in dapps_list.c.  The constructor and initializer for the 
		ord_llist object are in dapps_ord_list.c.

		Note that cursors should be initialized to appropriate
		values (by calling get_first(), get_last(), or some
		other appropriate linked list method) before they are
		used in function calls.  See the function headers in
		dapps_list.c and dapps_ord_list.c for descriptions of
		the effect of the execution of various methods on
		cursors passed in as parameters.


SCCS Info:
		@(#)dapps_list.h	1.1  11/21/96
==============================================================================*/

#ifndef _DAPPSLLIST_
#define _DAPPSLLIST_

#include "llist_err.h"

typedef void *cursor;

/*
	-- constructor for dynamic llists --

extern llist *create_dyn_llist()

	-- initializer for static llists --

extern int   init_static_llist( llist *list_p )

	-- constructor for dynamic ord_llists --

extern ord_llist *create_dyn_ord_llist( int (*ins_cmp_func_p)() )

	-- initializer for static ord_llists --

extern int init_static_ord_llist( ord_llist *list_p, int (*ins_cmp_func_p)() )

*/

struct llist
{
/*
-- Note that this comment is intended for information only - under no
-- circumstances should the comment delimiters be removed.  All of the
-- contents of the llist structure are defined to the compiler in the
-- llist.h file.  A #include of llist.h follows this comment within this
-- structure definition.

				==
				==  Public part
				==

void		*(*append)( llist *list_p, void *entry_p, 
			void (*rm_func_p)(), void *rm_func_parm, 
				cursor *u_cursor_p )

void		*(*prepend)( llist *list_p, void *entry_p, 
			void (*rm_func_p)(), void *rm_func_parm, 
				cursor *u_cursor_p ) 

void		*(*ins_by_cursor)( llist *list_p, void *entry_p,
			void (*rm_func_p)(), void *rm_func_parm, 
				cursor *u_cursor_p ) 

void		*(*get_by_key)( llist *list_p, void *key, int (*cmp_func_p)(),
		cursor *u_cursor_p)

void		*(*get_by_cursor)( llist *list_p, cursor *u_cursor_p)
void		*(*get_first)( llist *list_p, cursor *u_cursor_p)
void		*(*get_last)( llist *list_p, cursor *u_cursor_p)
void		*(*get_next)( llist *list_p, cursor *u_cursor_p)
void		*(*get_prev)( llist *list_p, cursor *u_cursor_p)

int		(*del_by_key)( llist *list_p, void *key, int (*cmp_func_p)(),
			cursor *u_cursor_p )

int		(*del_by_cursor)( llist *list_p, cursor *u_cursor_p )
int		(*rm_all_entries)( llist *list_p, cursor *u_cursor_p )
int		(*destroy_list)( llist *list_p )


*/
#include "llist.h"
};





struct ord_llist
{
/*
-- Note that this comment is intended for information only - under no
-- circumstances should the comment delimiters be removed.  All of the
-- contents of the ord_llist structure are defined to the compiler in the
-- ord_llist.h file.  A #include of ord_llist.h follows this comment within 
-- this structure definition.

				==
				==  Public part
				==

void		*(*insert)( ord_llist *list_p, void *entry_p,
				void (*rm_func_p)(), void *rm_func_parm,
				cursor *u_cursor_p )

void		*(*get_by_key)( llist *list_p, void *key, int (*cmp_func_p)(),
				cursor *u_cursor_p)

void		*(*get_by_cursor)( llist *list_p, cursor *u_cursor_p)
void		*(*get_first)( llist *list_p, cursor *u_cursor_p)
void		*(*get_last)( llist *list_p, cursor *u_cursor_p)
void		*(*get_next)( llist *list_p, cursor *u_cursor_p)
void		*(*get_prev)( llist *list_p, cursor *u_cursor_p)

int		(*del_by_key)( llist *list_p, void *key, int (*cmp_func_p)(),
				cursor *u_cursor_p )

int		(*del_by_cursor)( llist *list_p, cursor *u_cursor_p )
int		(*rm_all_entries)( llist *list_p, cursor *u_cursor_p )
int		(*destroy_list)( llist *list_p )

*/
#include "ord_llist.h"
};

				/*
				 * Macros for easy use of the linked
				 * list functions.
				 */

#define FIRST( list_p, cursor )  ((list_p)->get_first( (list_p), &(cursor) ))
#define LAST( list_p, cursor )   ((list_p)->get_last( (list_p), &(cursor) ))
#define NEXT( list_p, cursor )   ((list_p)->get_next( (list_p), &(cursor) ))
#define PREV( list_p, cursor )   ((list_p)->get_prev( (list_p), &(cursor) ))
#define GET( list_p, cursor )    ((list_p)->get_by_cursor( (list_p), &(cursor)))

#define APPEND( list_p, entry_p, rm_func_p, rm_func_parm )  \
		((list_p)->append( (list_p), (entry_p), (rm_func_p), \
		(rm_func_parm), (cursor *)NULL ))

#define PREPEND( list_p, entry_p, rm_func_p, rm_func_parm )  \
		((list_p)->prepend( (list_p), (entry_p), (rm_func_p), \
		(rm_func_parm), (cursor *)NULL ))

#define INS_BY_KEY( list_p, entry_p, rm_func_p, rm_func_parm ) \
		((list_p)->insert( (list_p), (entry_p), (rm_func_p), \
		(rm_func_parm), (cursor *)NULL ))

#define DEL_BY_KEY( list_p, key, cmp_func_p ) \
		((list_p)->del_by_key( (list_p), (key), (cmp_func_p), \
		(cursor *)NULL ))

#define DEL_ALL( list_p )  \
		((list_p)->rm_all_entries( (list_p), (cursor *)NULL ))

#define NUMELTS( list_p )	 ((list_p)->num_elts)

typedef struct llist llist;
typedef struct ord_llist ord_llist;

extern llist *create_dyn_llist();
extern int init_static_llist();
extern ord_llist *create_dyn_ord_llist();
extern int init_static_ord_llist();

#endif /* _DAPPSLLIST_ */

/* End of File */
