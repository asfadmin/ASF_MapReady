#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	dapps_list.c

Description:	This file contains the constructors, member functions, and
		auxiliary functions which implement a generic
		unordered linked list class and its derived types.

External Functions:
		create_dyn_llist()	  - dynamic llist constructor
		init_static_llist()	  - static llist initializer
	
Static Functions:
	Public Member Functions:
		Append_to_List()
		Prepend_to_List()
		Insert_Entry_before_Cursor()
		Get_Entry_by_Key()
		Get_Entry_at_Cursor()
		Get_First_Entry()
		Get_Last_Entry()
		Get_Next_Entry()
		Get_Prev_Entry()
		Unlink_Entry_at_Cursor()
		Delete_Entry_by_Key()
		Delete_Entry_at_Cursor()
		Delete_All_Entries()
		Destroy_List()
		Cant_Destroy_List()

	Private Member Functions:
		Create_Link_Object()

	Private Non-Member Functions:
		init_dyn_llist()
		append_entry()
		prepend_entry()
		insert_entry()
		unlink_entry()

External Variables Defined:
	
File Scope Static Variables:
	
See Also:	dapps_list.h, llist.h, ord_llist.h, dapps_ord_list.c

Notes:

==============================================================================*/
#pragma ident	"@(#)dapps_list.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_llist/SCCS/s.dapps_list.c"


#include <stdio.h>
#include "llist_err.h"
#include "dapps_defs.h"
#include "nmalloc.h"
#include "dapps_list.h"

	/*
	 * The llink type is defined in this file 
	 * in order to prevent use of its internals 
	 * by objects other than linked lists.
	 * In order to implement the get_next() 
	 * function of the linked list in such a 
	 * manner that a pointer to the current link 
	 * object in the linked list is not contained 
	 * in the llist object, it is necessary that 
	 * the pointer be in user variable space.  In 
	 * order to minimize the chances of unauthorized
	 * access of private elements of another 
	 * structure, this pointer will be declared to
	 * be of type cursor (defined in dapps_list.h).
	 */

typedef struct llink llink;

struct llink
{
	void			*entry_p;	/* ptr to obj. in list */
	llink			*next;		/* ptr to next link */
	llink			*prev;		/* ptr to prev link */
	llist			*list_p;	/* ptr to "owner" list */
	void			(*rm_func_p)();	/* func to call at remove */
	void			*rm_func_parm;	/* parm for rm_func_p */
};


/*=============================================================================
Function:	create_dyn_llist()

Description:	This function dynamically allocates and initializes a new
		linked list object.

Inputs:		None

Returns:	new_list_p	-	Pointer to the new linked list object

			NULL		-	If this function fails.  The llist_errno
							variable will be set to one of the
							following values:

							ELMALLOC:     If no heap space is 
								available for a new list.

							ELISTINIT:   The initialization of the 
								fields in the list object 
								failed for some reason.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Non-member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	

See Also:	init_dyn_llist(), init_static_llist(), nmalloc.h, nmalloc.c,
			znew.c

Notes:		This function sets a flag in the linked list object which will
			ensure that the object is not subsequently initialized using
			the init_stat_llist() external function in this file.
=============================================================================*/
static int	init_dyn_llist();

extern llist *
create_dyn_llist()

{
	extern void zfree();
	llist		*new_list_p = (llist *)NULL;


	/*
	 * Allocate the memory for the new object,
	 * and initialize the memory to zero.
	 * Note: ZNEW() is a macro to allocate and
	 * initialize memory.  It is defined
	 * in "znew.c" and "nmalloc.h".
	 */

	if ( (new_list_p = (llist *)ZNEW( sizeof( llist ) )) == (llist *)NULL )
	{
		llist_errno = ELMALLOC;
		return( (llist *)NULL );
	}


	/*
	 * Initialize the new object.
	 */

	if ( init_dyn_llist( new_list_p ) == ERROR )
	{
		llist_errno = ELISTINIT;
		ZFREE( (char *)new_list_p, sizeof( llist ) );
		return( (llist *)NULL );
	}


	return( new_list_p );
}


/*=============================================================================
Function:	init_static_llist()

Description:	This function initializes a statically defined (that is,
				defined at compile time) generic linked list object.

Inputs:		list_p		-	A pointer to the statically allocated
							linked list object which is to be
							initialized.

Returns:	OK			-	if it succeeds

			ERROR		-	if it fails.  The llist_errno
							variable will be set to one of the
							following values:

							EBADLIST:    If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter.

							ELISTREINIT: If the linked list object
								pointed to by list_p has 
								already been initialized.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Non-member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	If the static llist structure is not bzeroed prior to calling
			this routine, it is possible that this routine will not allow
			it to be initialized, thinking that it has already been
			initialized.

See Also:	create_dyn_llist(), init_dyn_llist()

Notes:		Ensure that the static llist structure to be initialized has
			been bzeroed prior to calling this routine.
=============================================================================*/
static void		*Append_to_List();
static void		*Prepend_to_List();
static void		*Insert_Entry_before_Cursor();
static void		*Get_Entry_by_Key();
static void		*Get_Entry_at_Cursor();
static void		*Get_First_Entry();
static void		*Get_Last_Entry();
static void		*Get_Next_Entry();
static void		*Get_Prev_Entry();
static void		*Unlink_Entry_at_Cursor();
static int		Delete_Entry_by_Key();
static int		Delete_Entry_at_Cursor();
static int		Delete_All_Entries();
static int		Cant_Destroy_List();
static cursor		Create_Link_Object();

extern int
init_static_llist( list_p )

	llist *list_p;	/* Pointer to the statically allocated list to init. */

{


	/*
	 * First make sure that we have not
	 * been passed a NULL pointer for the
	 * list_p parameter.
	 */

	if ( list_p == (llist *)NULL )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/*
	 * If the list pointed at by list_p has
	 * already been initialized, then do not
	 * reinitialize it here.
	 */

	if ( list_p->initialized == TRUE )
	{
		llist_errno = ELISTREINIT;
		return( ERROR );
	}


	/*
	 * Let's start with an zeroed structure
	 */

	bzero( (char *)list_p, sizeof( llist ) );

	
	/*
	 * Initialize the public part of the
	 * llist object.
	 */

	list_p->append = Append_to_List;
	list_p->prepend = Prepend_to_List;
	list_p->ins_by_cursor = Insert_Entry_before_Cursor;
	list_p->get_by_key = Get_Entry_by_Key;
	list_p->get_by_cursor = Get_Entry_at_Cursor;
	list_p->get_first = Get_First_Entry;
	list_p->get_last = Get_Last_Entry;
	list_p->get_next = Get_Next_Entry;
	list_p->get_prev = Get_Prev_Entry;
	list_p->unlink_by_cursor = Unlink_Entry_at_Cursor;
	list_p->del_by_key = Delete_Entry_by_Key;
	list_p->del_by_cursor = Delete_Entry_at_Cursor;
	list_p->rm_all_entries = Delete_All_Entries;
	list_p->destroy_list = Cant_Destroy_List;


	/*
	 * Initialize the private part of the
	 * llist object.
	 */

	list_p->num_elts = 0;
	list_p->create_link = Create_Link_Object;


	/*
	 * Mark the fact that this object has
	 * been initialized by setting the
	 * initialized flag.  Note that this
	 * flag is used for checking in the
	 * static llist initializer that it
	 * is not being called for an llist
	 * which has already been created by
	 * this routine.
	 */

	list_p->initialized = TRUE;

	return( OK );
}


/*=============================================================================
Function:	Append_to_List()

Description:	This function is intended to be called externally via a
				function pointer in the llist object.  Its purpose is to
				attach a new entry to the end of a previously allocated and
				initialized linked list.

Inputs:		list_p		-	a pointer to the list which is to be
							appended to.

			entry_p		-	a pointer to the entry to be appended.

			rm_func_p	-	pointer to a function to be called when
							the entry is removed from the list.  If
							this parameter is left NULL, no action
							will be taken upon entry removal.

			rm_func_parm -	a pointer to a parameter to be passed
							to the function pointed to by
							rm_func_p.  This parameter may be
							NULL.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated
							by this function to indicate the end
							of the list.  This parameter may
							be NULL.

Returns:	entry_p		-	if the append succeeded.

			NULL		-	if the append failed.  The llist_errno
							variable will be set to one of the
							following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ELISTNOENTRY: If a NULL entry_p
								parameter was passed to
								this function.

							ELINKCREATE:  It was impossible to
								create a new link object
								for some reason.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Prepend_to_List(), Insert_Entry_before_Cursor()

Notes:		This function is pointed at by the llist.append field.
			Note that if this function fails, the cursor pointed to
			by the u_cursor_p parameter will not be changed.  If the
			append succeeds, then upon return, the cursor will be at 
			the end of the list (note that cursors do NOT point to entries 
			in the list - they point to the links of the list, which, in 
			turn, point to the entries.
=============================================================================*/
static void append_entry();

static void *
Append_to_List( list_p, entry_p, rm_func_p, rm_func_parm, u_cursor_p )

	llist *list_p;		/* Pointer to list to insert in */
	void *entry_p;		/* Pointer to entry to insert */
	void (*rm_func_p)(); /* Pointer to function to call when entry is removed */
	void *rm_func_parm;	/* Parameter to pass to rm_func_p */
	cursor *u_cursor_p;	/* Pointer to an (allocated) cursor in caller space */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/*
	 * Check that the entry pointer which was 
	 * passed in is non-NULL.
	 */

	if ( entry_p == (void *)NULL )
	{
		llist_errno = ELISTNOENTRY;
		return( (void *)NULL );
	}


	/*
	 * Allocate a link object to put into the
	 * list of links.
	 */

	if ( (link_p = (llink *)list_p->create_link( list_p, entry_p,
				rm_func_p, rm_func_parm )) == (llink *)NULL )
	{
		llist_errno = ELINKCREATE;
		return( (void *)NULL );
	}


	/*
	 * Put the link object at the tail of the
	 * list.
	 */

	append_entry( list_p, link_p );


	/*
	 * Set the user's cursor pointer
	 * to point to the new link if the
	 * user provided us with a pointer
	 * to a cursor.
	 */

	list_p->num_elts++;
	if ( u_cursor_p != (cursor *)NULL )
		*u_cursor_p = (cursor)link_p;

	return( link_p->entry_p );
}


/*=============================================================================
Function:	Prepend_to_List()

Description:	This function is intended to be called externally via a
				function pointer in the llist object.  Its purpose is to
				attach a new entry to the beginning of a previously 
				allocated and initialized linked list.

Inputs:		list_p		-	a pointer to the list which is to be
							prepended to.

			entry_p		-	a pointer to the entry to be prepended.

			rm_func_p	-	pointer to a function to be called when
							the entry is removed from the list.  If
							this parameter is left NULL, no action
							will be taken upon entry removal.

			rm_func_parm	-	a pointer to a parameter to be passed
							to the function pointed to by
							rm_func_p.  This parameter may be
							NULL.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated
							by this function to indicate the end
							of the list.  This parameter may be
							NULL.

Returns:	entry_p		-	if the prepend succeeded.

			NULL		-	if the prepend failed.  The llist_errno
							variable will be set to one of the
							following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ELISTNOENTRY: If a NULL entry_p
								parameter was passed to
								this function.

							ELINKCREATE:  If it was impossible to
								create a new link object
								for some reason.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Append_to_List(), Insert_Entry_before_Cursor()

Notes:		This function is pointed at by the llist.prepend field.
			Note that if this function fails, the cursor pointed to
			by the u_cursor_p parameter will not be changed.  If the
			prepend succeeds, then upon return, the cursor will be at 
			the beginning of the list (note that cursors do NOT point to 
			entries in the list - they point to the links of the list, 
			which, in turn, point to the entries.
=============================================================================*/
static void prepend_entry();

static void *
Prepend_to_List( list_p, entry_p, rm_func_p, rm_func_parm, u_cursor_p )

	llist *list_p;		/* Pointer to list to insert in */
	void *entry_p;		/* Pointer to entry to insert */
	void (*rm_func_p)(); /* Pointer to function to call when entry is removed */
	void *rm_func_parm;	/* Parameter to pass to rm_func_p */
	cursor *u_cursor_p;	/* Pointer to an (allocated) cursor in caller space */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/*
	 * Check that the entry pointer which was 
	 * passed in is non-NULL.
	 */

	if ( entry_p == (void *)NULL )
	{
		llist_errno = ELISTNOENTRY;
		return( (void *)NULL );
	}


	/*
	 * Allocate a link object to put into the
	 * list of links.
	 */

	if ( (link_p = (llink *)list_p->create_link( list_p, entry_p,
				rm_func_p, rm_func_parm )) == (llink *)NULL )
	{
		llist_errno = ELINKCREATE;
		return( (void *)NULL );
	}


	/*
	 * Put the link object at the head of the
	 * list.
	 */

	prepend_entry( list_p, link_p );


	/*
	 * Set the user's cursor pointer
	 * to point to the new link.
	 */

	list_p->num_elts++;
	if ( u_cursor_p != (cursor *)NULL )
		*u_cursor_p = (cursor)link_p;

	return( link_p->entry_p );
}


/*=============================================================================
Function:	Insert_Entry_before_Cursor()

Description:	This function inserts an entry before the entry pointed to by
				the cursor which is passed in as a parameter.  Note that after
				insertion, the cursor is changed to point at the link object
				for the entry which was inserted.

Inputs:		list_p		-	a pointer to the list which is to be
							inserted into.

			entry_p		-	a pointer to the entry to be inserted.

			rm_func_p	-	pointer to a function to be called when
							the entry is removed from the list.  If
							this parameter is left NULL, no action
							will be taken upon entry removal.

			rm_func_parm	-	a pointer to a parameter to be passed
							to the function pointed to by
							rm_func_p.  This parameter may be
							NULL.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which indicates the
							position in the list before which the
							new entry is to be inserted.  The
							cursor pointed at by u_cursor_p will
							be updated upon return from this
							function to point to the link which
							points to the entry that is inserted.

Returns:	entry_p		-	if the insert succeeded.

			NULL		-	if the insert failed.  The llist_errno
							variable will be set to one of the
							following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ELISTNOENTRY: If a NULL entry_p
								parameter was passed to
								this function.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ELINKCREATE:  If it was impossible to
								create a new link object
								for some reason.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Append_to_List(), Prepend_to_List()

Notes:		This function is pointed at by the llist.ins_by_cursor field.
			Note that if this function fails, the cursor pointed to
			by the u_cursor_p parameter will not be changed.  If the
			insert succeeds, then upon return, the cursor will be at 
			the link which points to the entry which was inserted (note 
			that cursors do NOT point to entries in the list - they point 
			to the links of the list, which, in turn, point to the entries).
=============================================================================*/
static void append_entry();
static void insert_entry();

static void *
Insert_Entry_before_Cursor( list_p, entry_p, rm_func_p, rm_func_parm, 
								u_cursor_p )

	llist *list_p;		/* Pointer to list to insert in */
	void *entry_p;		/* Pointer to entry to insert */
	void (*rm_func_p)(); /* Pointer to function to call when entry is removed */
	void *rm_func_parm;	/* Parameter to pass to rm_func_p */
	cursor *u_cursor_p;	/* Pointer to an (allocated) cursor in caller space */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}
	

	/*
	 * Check that the entry pointer which was 
	 * passed in is non-NULL.
	 */

	if ( entry_p == (void *)NULL )
	{
		llist_errno = ELISTNOENTRY;
		return( (void *)NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Allocate a link object to put into the
	 * list of links.
	 */

	if ( (link_p = (llink *)list_p->create_link( list_p, entry_p,
				rm_func_p, rm_func_parm )) == (llink *)NULL )
	{
		llist_errno = ELINKCREATE;
		return( (void *)NULL );
	}


	/*
	 * Insert the new entry into the
	 * linked list before the entry
	 * pointed to by the cursor.
	 */

	if( (cursor)*u_cursor_p == (cursor)NULL )
		append_entry( list_p, link_p );
	else insert_entry( list_p, link_p, (llink *)*u_cursor_p );


	/*
	 * Set the user's cursor pointer
	 * to point to the new link.
	 */

	list_p->num_elts++;
	*u_cursor_p = (cursor)link_p;

	return( link_p->entry_p );
}


/*=============================================================================
Function:	Get_Entry_by_Key()

Description:    The purpose of this function is to retrieve an entry from a
				particular linked list by some key in the entry.   A user-
				provided function will be required to perform this function
				(see the Notes section in this header).

Inputs:		list_p		-	a pointer to the list which is to be
							searched for the key.

			key			-	a pointer to a variable of type key
							(in the user space, the variable can
							be of any type - this function
							considers it to be a void, since
							conversions to and from type void
							are defined to be portable by ANSI)
							which contains a key which in some
							way identifies one of the elements
							in list_p.

			cmp_func_p	-	a pointer to a function which is to be
							called by this function in the
							following way:
							  cmp_func_p( key, entry ),
							and which will return TRUE if the entry
							and the key match, FALSE if they do
							not, and ERROR if some error occurs.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated upon 
							return from this function to point to 
							the link which points to the entry 
							that is found.

Returns:	entry_p		-	if the entry with the matching key
							is found.

			NULL		-	if the entry is not found.  The 
					llist_errno variable will be set to one 
					of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ELISTNOCF:    If the pointer to the
								comparison function 
								(cmp_func_p) in the
								parameter list is NULL.

							ELISTCFERR:   If the cmp_func_p returns
								an ERROR.

							ELNOSUCHKEY:  If no entry with the key
								is contained in the list
								(according to the
								cmp_func_p function).

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Get_Entry_at_Cursor(), Get_First_Entry(), Get_Last_Entry(), 
			Get_Next_Entry(), Get_Prev_Entry()

Notes:		Note that upon return, the cursor pointed to by u_cursor_p
			is updated to point to the link which points to the entry
			which, according to cmp_func_p(), matched the given key.

			Note that currently if the cmp_func_p returns an ERROR, the
			search of the list stops.  This behavior could be modified.
=============================================================================*/

static void *
Get_Entry_by_Key( list_p, key, cmp_func_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	void *key;		/* Ptr to the key to pass to cmp_func_p */
	int (*cmp_func_p)(); /* Ptr to a caller funct to compare two list entries */
	cursor *u_cursor_p;	/* Ptr to a (caller allocated) cursor into list_p */

{
	llink *link_p = (llink *)NULL;
	int cf_retval = 0;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/*
	 * Check that the function pointer we are
	 * passed in to compare the entries in our
	 * list is not NULL.  We cannot find an
	 * entry without it.
	 */

	if ( cmp_func_p == (int (*)())NULL )
	{
		llist_errno = ELISTNOCF;
		return( (void *)NULL );
	}


	/*
	 * Search the list for the entry,
	 * returning a pointer to the entry
	 * if we find it, and a NULL pointer
	 * if we do not.
	 */

	link_p = (llink *)list_p->head;

	while ( link_p != (llink *)NULL )
	{
		if ( (cf_retval = (*cmp_func_p)( key, link_p->entry_p )) 
			== TRUE )
		{
			if ( u_cursor_p )
				*u_cursor_p = (cursor)link_p;
			return( link_p->entry_p );
		}
		else if ( cf_retval == ERROR )
		{
			llist_errno = ELISTCFERR;
			return( (void *)NULL );
		}
		link_p = link_p->next;
	}

	llist_errno = ELNOSUCHKEY;

	return( (void *)NULL );
}


/*=============================================================================
Function:	Get_Entry_at_Cursor()

Description:	This function returns the entry which is at the current cursor
				position in the indicated linked list.

Inputs:		list_p		-	a pointer to the list from which the
					entry at the cursor position is to 
					be retrieved.

			u_cursor_p	-	A pointer to a variable (for which
					memory has already been allocated) of
					type cursor which indicates the
					position in the list from which the
					desired entry is to be retrieved.

Returns:	entry_p		-	if the function does not fail.

			NULL		-	if the function fails.  The llist_errno 
							variable will be set to one of the 
							following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ENULLCURSOR:  If the cursor pointer
								points to a NULL cursor.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Get_Entry_by_Key(), Get_First_Entry(), Get_Last_Entry(), 
			Get_Next_Entry(), Get_Prev_Entry()
Notes:		
=============================================================================*/

static void *
Get_Entry_at_Cursor( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Check that the cursor pointer points
	 * to a non-NULL cursor.
	 */

	if ( *u_cursor_p == (cursor)NULL )
	{
		llist_errno = ENULLCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Retrieve the entry in the list.
	 */

	link_p = (llink *)*u_cursor_p;

	return( link_p->entry_p );
}


/*=============================================================================
Function:	Get_First_Entry()

Description:	This function is externally called via a function pointer
				in the llist object.  Its purpose is to retrieve the first
				entry in a specified linked list.

Inputs:		list_p		-	a pointer to the list from which the
					first entry is to be retrieved.

			u_cursor_p	-	A pointer to a variable (for which
					memory has already been allocated) of
					type cursor which will be updated upon
					successful return to point to the
					link which points to the first entry
					in the list.

Returns:	entry_p		-	if the function does not fail.

			NULL		-	if the function fails.  The llist_errno 
							variable will be set in this case to 
							one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ELISTEMPTY:   If the indicated list
								contains no entries.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Get_Entry_by_Key(), Get_Entry_at_Cursor(), Get_Last_Entry(),
			Get_Next_Entry(), Get_Prev_Entry()

Notes:		If this function fails, the cursor pointed to by u_cursor_p
			will not be updated to point to the link at the beginning of
			the list.
=============================================================================*/

static void *
Get_First_Entry( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Retrieve the entry in the list.
	 */

	link_p = (llink *)list_p->head;
	if ( link_p != (llink *)NULL )
	{
		*u_cursor_p = list_p->head;
		return( link_p->entry_p );
	}
	else
	{
		*u_cursor_p = (cursor)NULL;
		llist_errno = ELISTEMPTY;
		return( (void *)NULL );
	}

	/* returned before this point: see if/else, imm. above */
}


/*=============================================================================
Function:	Get_Last_Entry()

Description:	This function is called externally via a function pointer in 
				llist object.  Its purpose is to retrieve the last entry in a
				specified linked list.

Inputs:		list_p		-	a pointer to the list from which the
							last entry is to be retrieved.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated upon
							successful return to point to the
							link which points to the last entry
							in the list.

Returns:	entry_p		-	if the function does not fail.

			NULL		-	if the function fails.  The llist_errno 
							variable will be set in this case to 
							one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ELISTEMPTY:   If the indicated list
								contains no entries.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Get_Entry_by_Key(), Get_Entry_at_Cursor(), Get_First_Entry(),
			Get_Next_Entry(), Get_Prev_Entry()

Notes:		If this function fails, the cursor pointed to by u_cursor_p
			will not be updated to point to the link at the end of
			the list.
=============================================================================*/

static void *
Get_Last_Entry( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Retrieve the entry in the list.
	 */

	link_p = (llink *)list_p->tail;
	if ( link_p != (llink *)NULL )
	{
		*u_cursor_p = list_p->tail;
		return( link_p->entry_p );
	}
	else
	{
		llist_errno =  ELISTEMPTY;
		*u_cursor_p = (cursor)NULL;
		return( (void *)NULL );
	}

	/* returned before this point: see if/else, imm. above */
}


/*=============================================================================
Function:	Get_Next_Entry()

Description:	This function is called externally via a function pointer in
				the llist object.  Its purpose is to retrieve the element
				following the caller's current position in the linked list (as
				referred to by the u_cursor_p parameter to this function).

Inputs:		list_p		-	a pointer to the list from which the
							next entry is to be retrieved.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated upon
							successful return to point to the
							link which points to the next entry
							in the list.

Returns:	entry_p		-	if the function does not fail.

			NULL		-	if the function fails or if we are
							unable to retrieve the next element
							in the list, since we already at the
							end of the list.  In the latter
							case, upon return, the cursor will be
							reset to NULL, and a subsequent call
							to this function will return the first
							element in the list.  In any case of 
							error, the llist_errno variable will be 
							set to one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ELISTEMPTY:   If the list pointed to
								by list_p is empty.

							EENDOFLIST:   If the cursor was
								pointing to the link at 
								the end of the list.
								Note that in this case,
								upon return, the cursor
								will be NULLed.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Get_Entry_by_Key(), Get_Entry_at_Cursor(), Get_First_Entry(),
			Get_Last_Entry(), Get_Prev_Entry()

Notes:		If this function fails, the cursor pointed to by u_cursor_p
			will not be updated to point to the next link in the list.
=============================================================================*/

static void *
Get_Next_Entry( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Retrieve the entry in the list.
	 */

	link_p = (llink *)*u_cursor_p;
	if ( link_p == (llink *)NULL )
	{
		if ( list_p->head == (cursor)NULL )
		{
			llist_errno = ELISTEMPTY;
			return( (void *)NULL );
		}
		else
		{
			*u_cursor_p = list_p->head;
			link_p = (llink *)*u_cursor_p;
			return( link_p->entry_p );
		}
	}
	else if ( link_p->next == (llink *)NULL )
	{
		/* at end of list - set cursor to NULL */
		llist_errno = EENDOFLIST;
		*u_cursor_p = (cursor)NULL;
		return( (void *)NULL );
	}
	else
	{
		*u_cursor_p = (cursor)link_p->next;
		return( link_p->next->entry_p );
	}

	/* returned before this point: see if/else, imm. above */
}


/*=============================================================================
Function:	Get_Prev_Entry()

Description:	This function is called externally via a function pointer in
				the llist object.  Its purpose is to retrieve the entry in a
				specified linked list which is previous to the entry indicated
				by the u_cursor_p position which is passed in from the caller.

Inputs:		list_p		-	a pointer to the list from which the
							previous entry is to be retrieved.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated upon
							successful return to point to the
							link which points to the previous entry
							in the list.

Returns:	entry_p		-	if the function does not fail.

			NULL		-	if the function fails or if we are
							unable to retrieve the next element
							in the list, since we already at the
							beginning of the list.  In the latter
							case, upon return, the cursor will be
							reset to NULL, and a subsequent call
							to this function will return the last
							element in the list.  In any case of 
							error, the llist_errno variable will be 
							set to one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ELISTEMPTY:   If the list pointed to
								by list_p is empty.

							EENDOFLIST:   If the cursor was
								pointing to the link at 
								the beginning of the list.
								Note that in this case,
								upon return, the cursor
								will be NULLed.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Get_Entry_by_Key(), Get_Entry_at_Cursor(), Get_First_Entry(),
			Get_Last_Entry(), Get_Next_Entry()

Notes:		If this function fails, the cursor pointed to by u_cursor_p
			will not be updated to point to the next link in the list.
=============================================================================*/

static void *
Get_Prev_Entry( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( (void *)NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( (void *)NULL );
	}


	/*
	 * Retrieve the entry in the list.
	 */

	link_p = (llink *)*u_cursor_p;
	if ( link_p == (llink *)NULL )
	{
		if ( list_p->tail == (cursor)NULL )
		{
			llist_errno = ELISTEMPTY;
			return( (void *)NULL );
		}
		else
		{
			*u_cursor_p = list_p->tail;
			link_p = (llink *)*u_cursor_p;
			return( link_p->entry_p );
		}
	}
	else if ( link_p->prev == (llink *)NULL )
	{
		/* at beginning of list - set cursor to NULL */
		llist_errno = EBEGOFLIST;
		*u_cursor_p = (cursor)NULL;
		return( (void *)NULL );
	}
	else
	{
		*u_cursor_p = (cursor)link_p->prev;
		return( link_p->prev->entry_p );
	}

	/* returned before this point: see if/else, imm. above */
}


/*=============================================================================
Function:	Delete_Entry_by_Key()

Description:	This function is called externally via a function pointer in
				the llist object.  Its purpose is to locate the entry in a
				specified linked list which has a particular key, and to delete
				the entry.  Note that the location of the appropriate entry to
				delete is done via a user-provided function.  For the interface
				required of this user-provided function, see the Notes section
				below.

Inputs:		list_p		-	a pointer to the list from which the
							entry with the given key is to be
							deleted.

			key			-	a pointer to a variable of type key
							(in the user space, the variable can
							be of any type - this function
							considers it to be a void, since
							conversions to and from type void
							are defined to be portable by ANSI)
							which contains a key which in some
							way identifies one of the elements
							in list_p.

			cmp_func_p	-	a pointer to a function which is to be
							called by this function in the
							following way:
							  cmp_func_p( key, entry ),
							and which will return TRUE if the entry
							and the key match, FALSE if they do
							not, and ERROR if some error occurs.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be updated upon 
							return to point to the link which points
							to the entry before the one that was 
							deleted if the entry which was deleted 
							was not at the head of the list, and
							NULL otherwise.

Returns:	OK			-	if the deletion succeeds.

			ERROR		-	if the deletion fails.  In this case,
							the llist_errno variable will be set upon
							return to one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ELISTNOCF:    If the pointer to the
								comparison function 
								(cmp_func_p) in the
								parameter list is NULL.

							ELISTCFERR:   If the cmp_func_p returns
								an ERROR.

							ELNOSUCHKEY:  If no entry with the key
								is contained in the list
								(according to the
								cmp_func_p function).

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Delete_Entry_at_Cursor(), Delete_All_Entries(), Destroy_List(),

Notes:		Note that if the function fails the cursor will not be
			updated.
=============================================================================*/
static void unlink_entry();

static int
Delete_Entry_by_Key( list_p, key, cmp_func_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	void *key;		/* Ptr to the key we are to compare with */
	int (*cmp_func_p)();	/* Ptr to the funct we use to compare */
	cursor *u_cursor_p;	/* Ptr to a cursor which points into list_p */

{
	extern void zfree();
	llink *link_p = (llink *)NULL;
	int cf_retval = 0;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/*
	 * Check that the function pointer we are
	 * passed in to compare the entries in our
	 * list is not NULL.  We cannot find an
	 * entry without it.
	 */

	if ( cmp_func_p == (int (*)())NULL )
	{
		llist_errno = ELISTNOCF;
		return( ERROR );
	}


	/*
	 * Search the list for the entry;
	 * if we find it, remove it from
	 * the list, call the function that
	 * the caller provided us with at
	 * insert time (if there was one),
	 * update the caller's cursor, and then 
	 * free the link object and return.  If we
	 * do not find the entry in question, return 
	 * an ERROR.
	 */

	link_p = (llink *)list_p->head;

	while ( link_p != (llink *)NULL )
	{
		if ( (cf_retval = (*cmp_func_p)( key, link_p->entry_p )) 
			== TRUE )
		{

				/* update the caller's cursor */
			if ( u_cursor_p != (cursor *)NULL )
				*u_cursor_p = (cursor) link_p->prev;	/* might be NULL */

				/* unlink it */
			unlink_entry( list_p, link_p );

				/* Call rm_func if there is one */
			if ( link_p->rm_func_p )
				(*link_p->rm_func_p)( link_p->rm_func_parm );

				/* zero and free it */
			ZFREE( (char *)link_p, sizeof( llink ) );

				/* decrement the num of elts in the list */
			if ( list_p->num_elts == 0 )
				return( ERROR );
			else list_p->num_elts--;

			return( OK );
		}
		else if ( cf_retval == ERROR )
		{
			llist_errno = ELISTCFERR;
			return( ERROR );
		}
		else 
		{
			link_p = link_p->next;
			continue;
		}
	}

	llist_errno = ELNOSUCHKEY;

	return( ERROR );
}



/*=============================================================================
Function:	Delete_Entry_at_Cursor()

Description:	This function will delete the entry which is pointed to by
				the cursor (llink object) which is passed in as a parameter.

Inputs:		list_p		-	a pointer to the list from which the
							entry at the cursor position is to 
							be deleted.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which indicates the
							position in the list from which the
							desired entry is to be deleted.
							Updated upon return to point to the link
							which points to the entry before the one
							that was deleted if the entry which was
							deleted was not at the head of the list,
							and NULL otherwise.

Returns:	OK			-	if the function does not fail.

			ERROR		-	if the function fails.  In that case,
							the llist_errno variable will be set to 
							one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ENULLCURSOR:  If the cursor pointer
								points to a NULL cursor.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	
Notes:		The cursor pointed to by u_cursor_p will not be updated if the
			Delete_Entry_at_Cursor() function fails.
=============================================================================*/
static void unlink_entry();

static int
Delete_Entry_at_Cursor( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor which points into list_p */

{
	extern void zfree();
	llink *link_p = (llink *)NULL;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( ERROR );
	}


	/*
	 * Unlink the entry from the list.  Then
	 * call the user function which was provided
	 * at insert time (if there was one), update
	 * the caller's cursor, and free the link
	 * object.
	 */

	link_p = (llink *)*u_cursor_p;

	if ( link_p == (llink *)NULL )
	{
		llist_errno = ENULLCURSOR;
		return( ERROR );
	}

	*u_cursor_p = (cursor) link_p->prev;	/* might be NULL */

	unlink_entry( list_p, link_p );  /* unlink it */

	if ( link_p->rm_func_p )
		(*link_p->rm_func_p)( link_p->rm_func_parm );

	ZFREE( (char *)link_p, sizeof( llink ) );

	if ( list_p->num_elts == 0 )
		return( ERROR );
	else list_p->num_elts--;

	return( OK );
}


/*=============================================================================
Function:	Unlink_Entry_at_Cursor()

Description:	This function will remove the entry which is pointed to by
				the cursor (llink object) which is passed in as a parameter.
				Note that the associated memory is not freed.

Inputs:		list_p		-	a pointer to the list from which the
							entry at the cursor position is to 
							be removed.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which indicates the
							position in the list from which the
							desired entry is to be deleted.

Returns:	entry_p		-	if the entry is successfully unlinked.

			NULL		-	if an error occurs and the entry is not unlinked.
							The llist_errno variable will be set to 
							one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

							ENOCURSOR:    If the u_cursor_p
								parameter is NULL.

							ENULLCURSOR:  If the cursor pointer
								points to a NULL cursor.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	
Notes:		The cursor pointed to by u_cursor_p will not be updated if the
			Delete_Entry_at_Cursor() function fails.
=============================================================================*/
static void unlink_entry();

static void *
Unlink_Entry_at_Cursor( list_p, u_cursor_p )

	llist *list_p;		/* Ptr to the list we are manipulating */
	cursor *u_cursor_p;	/* Ptr to a cursor which points into list_p */

{
	extern void zfree();
	llink *link_p = (llink *)NULL;
	void *entry_p ;


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( NULL );
	}


	/* 
	 * Check that we were passed a non-NULL
	 * cursor pointer.
	 */

	if ( u_cursor_p == (cursor *)NULL )
	{
		llist_errno = ENOCURSOR;
		return( NULL );
	}

	
	/*
	 * Unlink the entry from the list.  Then
	 * call the user function which was provided
	 * at insert time (if there was one), update
	 * the caller's cursor, and free the link
	 * object.
	 */

	link_p = (llink *)*u_cursor_p;

	if ( link_p == (llink *)NULL )
	{
		llist_errno = ENULLCURSOR;
		return( NULL );
	}

	/* update the callers cursor */

	*u_cursor_p = (cursor) link_p->prev;	/* might be NULL */

	/*
	 * save a copy of the entry (data) pointer 
	 * to return to the caller
	 */
	entry_p = link_p->entry_p ;

	unlink_entry( list_p, link_p );  /* unlink it */

	ZFREE( (char *)link_p, sizeof( llink ) );

	if ( list_p->num_elts == 0 )
		return( NULL );
	else list_p->num_elts--;

	return( entry_p );
}

/*=============================================================================
Function:	Delete_All_Entries()

Description:	This function will traverse a given linked list, and will
				remove all of the entries from it.  The list itself, however,
				will not be freed.

Inputs:		list_p		-	a pointer to the list from which all
							entries are to be deleted.

			u_cursor_p	-	A pointer to a variable (for which
							memory has already been allocated) of
							type cursor which will be reset to
							NULL upon return.  This parameter
							may be NULL.

Returns:	OK			-	if the function does not fail.

			ERROR		-	if the function fails.  In that case,
							the llist_errno variable will be set to 
							one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Delete_Entry_by_Key(), Delete_Entry_at_Cursor(), Destroy_List()
Notes:		
=============================================================================*/
static void del_all_entries();

static int
Delete_All_Entries( list_p, u_cursor_p )

	llist *list_p;		/* The list which we are to clean out */
	cursor *u_cursor_p;	/* Pointer to a cursor into list_p - we NULL it */

{
	extern void zfree();


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/*
	 * Remove all entries from the list.
	 */

	del_all_entries( list_p );


	/*
	 * Reset the count of the number of elements in
	 * the list to zero.
	 */

	list_p->num_elts = 0;


	/*
	 * If the input cursor is non-NULL, reNULL
	 * it.
	 */

	if ( u_cursor_p != (cursor *)NULL )
		*u_cursor_p = (cursor)NULL;

	return( OK );
}


/*=============================================================================
Function:	Destroy_List()

Description:	This function is called externally via a pointer to a function
				in the llist object.  Its purpose is to clean out a given
				linked list, and then to free all space which was used by the
				linked list.  Note that this function cannot be called for
				statically allocated linked lists.  See the Notes section below
				for a discussion of how this is prevented.

Inputs:		list_p		-	a pointer to the list which is to be
							destroyed.

Returns:	OK			-	if the function does not fail.

			ERROR		-	if the function fails.  In that case,
							the llist_errno variable will be set to 
							one of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Cant_Destroy_List(), create_dyn_llist(), init_static_llist(), 
			init_dyn_llist()

Notes:		This function is pointed to by the destroy_list method of the
			llist object when the object in question has been dynamically
			allocated.  If the llist object was statically allocated, and
			was initialized by using the init_static_llist() function, the
			destroy_list method was initialized to point at the
			Cant_Destroy_List method.
=============================================================================*/
static void del_all_entries();

static int
Destroy_List( list_p )

	llist *list_p;		/* Ptr to the list we are to destroy */

{
	extern void zfree();


	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/*
	 * Remove all of the entries in the list.
	 */

	del_all_entries( list_p );


	/*
	 * Zero and free the list object
	 * itself.
	 */

	ZFREE( (char *)list_p, sizeof( llist ) );

	return( OK );
}


/*=============================================================================
Function:	Cant_Destroy_List()

Description:	This function is pointed to by the destroy_list field of the
				llist object when the object was statically allocated.  Since
				the object was statically allocated, we are not allowed to
				free the storage that it uses.

Inputs:		list_p		-	a pointer to the list which is to be
							destroyed.

Returns:	ERROR		-	this function always fails.  This is
							because this function will only be
							called for a statically allocated
							linked list, which we are not allowed
							to destroy.  This function can fail
							for being called with an invalid linked
							list as a parameter however.  The
							llist_errno variable will be set to one
							of the following values:

							EBADLIST:     If a NULL pointer to
								a linked list object is
								passed in for the list_p
								parameter or if the list
								has not been initialized.
					
							ELISTDESTROY: This is the "correct"
								return value for this
								function.  If this
								function is called, the
								caller is trying to
								destroy a statically
								allocated linked list.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Destroy_List(), create_dyn_llist(), init_static_llist(), 
			init_dyn_llist()

Notes:		This function cannot be called for dynamic llist objects.
			In dynamic linked list objects, the destroy_list method
			is initialized to point to the Destroy_List() method, whereas
			in static linked list objects, the destroy_list method is
			initialized to point to the Cant_Destroy_List() method.
=============================================================================*/

static int
Cant_Destroy_List( list_p )

	llist *list_p;		/* Ptr to the list caller wants to destroy */

{
			
	/*
	 * First check that we are manipulating a
	 * valid list object.
	 */

	if ( ( list_p == (llist *)NULL ) || ( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/*
	 * The list we are dealing with is statically
	 * allocated, so cannot be destroyed.
	 */

	llist_errno = ELISTDESTROY;
	return( ERROR );
}


/*=============================================================================
Function:	Create_Link_Object()

Description:	The purpose of this function is to create an llink object and
				to initialize its fields.  It is called via a function pointer
				by other methods of the llist object.

Inputs:		list_p		-	a pointer to the list which is to be
							destroyed.

			entry_p		-	pointer to the entry for which the
							link is being created.

			rm_func_p	-	pointer to the function to be called
							when the entry is removed.

			rm_func_parm	-	pointer to a parameter to pass to the
							rm_func_p function when the entry is
							removed.

Returns:	link_p		-	pointer to a new link object, if the
							function succeeds.

			NULL		-	if the function fails.  In that case,
							the llist_errno variable will be set to
							one of the following values:

							ELMALLOC:     If no heap space is 
								available for a new link.


Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	Append_to_List(), Prepend_to_List(),
			Insert_Entry_before_Cursor()

Notes:		This function is defined to be a private member function, and 
			hence should never be called by functions outside of this file.
=============================================================================*/

static cursor
Create_Link_Object( list_p, entry_p, rm_func_p, rm_func_parm )

	llist *list_p;
	void *entry_p;
	void (*rm_func_p)();
	void *rm_func_parm;

{
	llink *link_p = (llink *)NULL;


	/*
	 * Allocate the memory for the new object,
	 * and initialize the memory to zero.
	 * Note: ZNEW() is a macro to allocate and
	 * initialize memory.  It is defined in
	 * "znew.c" and "nmalloc.h".
	 */

	if ( (link_p = (llink *)ZNEW( sizeof( llink ) )) == (llink *)NULL )
	{
		llist_errno = ELMALLOC;
		return( (cursor)NULL );
	}


	/*
	 * Initialize the new link object.
	 */

	link_p->list_p = list_p;
	link_p->entry_p = entry_p;
	link_p->rm_func_p = rm_func_p;
	link_p->rm_func_parm = rm_func_parm;

	return( (cursor)link_p );
}


/*=============================================================================
Function:	init_dyn_llist()

Description:	The purpose of this function is to initialize a dynamically
				allocated linked list object.

Inputs:		new_list_p	-	pointer to the new linked list,
							which is to be initialized.

Returns:	OK			-	if the function succeeds.

			ERROR		-	if the function fails.  In that case,
							the llist_errno variable will be set to 
							one of the following values:

							EBADLIST:    If a NULL pointer to
							   a linked list object is
							   passed in for the list_p
							   parameter.

							ELISTREINIT: If the linked list object
							   pointed to by list_p has 
							   already been initialized.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Non-member Function
Globals:
			Accessed:	
			Changed :	llist_errno
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	create_dyn_llist(), init_static_llist(), Destroy_List(),
			Cant_Destroy_List()
Notes:		
=============================================================================*/
static void		*Append_to_List();
static void		*Prepend_to_List();
static void		*Insert_Entry_before_Cursor();
static void		*Get_Entry_by_Key();
static void		*Get_Entry_at_Cursor();
static void		*Get_First_Entry();
static void		*Get_Last_Entry();
static void		*Get_Next_Entry();
static void		*Get_Prev_Entry();
static void		*Unlink_Entry_at_Cursor();
static int		Delete_Entry_by_Key();
static int		Delete_Entry_at_Cursor();
static int		Delete_All_Entries();
static int		Destroy_List();
static cursor		Create_Link_Object();

static int
init_dyn_llist( new_list_p )

	llist *new_list_p;

{


	/*
	 * If the passed in pointer is NULL,
	 * we had better not try to initialize
	 * what it points at.
	 */

	if ( new_list_p == (llist *)NULL )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


	/*
	 * If the list object pointed at has already
	 * been initialized, then do not reinitialize
	 * it here.
	 */

	if ( new_list_p->initialized == TRUE )
	{
		llist_errno = ELISTREINIT;
		return( ERROR );
	}


	/*
	 * Initialize the public part of the
	 * dynamically allocated linked list object.
	 */

	new_list_p->append = Append_to_List;
	new_list_p->prepend = Prepend_to_List;
	new_list_p->ins_by_cursor = Insert_Entry_before_Cursor;
	new_list_p->get_by_key = Get_Entry_by_Key;
	new_list_p->get_by_cursor = Get_Entry_at_Cursor;
	new_list_p->get_first = Get_First_Entry;
	new_list_p->get_last = Get_Last_Entry;
	new_list_p->get_next = Get_Next_Entry;
	new_list_p->get_prev = Get_Prev_Entry;
	new_list_p->unlink_by_cursor = Unlink_Entry_at_Cursor;
	new_list_p->del_by_key = Delete_Entry_by_Key;
	new_list_p->del_by_cursor = Delete_Entry_at_Cursor;
	new_list_p->rm_all_entries = Delete_All_Entries;
	new_list_p->destroy_list = Destroy_List;


	/*
	 * Initialize the private part of the
	 * dynamically allocated linked list object.
	 */

	new_list_p->num_elts = 0;
	new_list_p->create_link = Create_Link_Object;


	/*
	 * Mark the fact that this object has
	 * been initialized by setting the
	 * initialized flag.  Note that this
	 * flag is used for checking in the
	 * static llist initializer that it
	 * is not being called for an llist
	 * which has already been created by
	 * this routine.
	 */

	new_list_p->initialized = TRUE;
	
	return( OK );
}


/*=============================================================================
Function:	append_entry()

Description:	This function is called by other functions in this file to
				link a new entry at the end of a linked list (llist) object.

Inputs:		list_p		-	pointer to the linked list
							to which the link_p is to be
							appended.

			link_p		-	pointer to the link which is
							to be appended to the list
							pointed to by list_p

Returns:	None

Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Non-member Function
Globals:
			Accessed:	
			Changed :	
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	prepend_entry(), insert_entry(), unlink_entry(),
			del_all_entries()

Notes:		This function was intentionally written with no error checking. 
			All checking on the validity of parameters must be done
			by the calling function.  Note that this function cannot
			be called outside of this file.
=============================================================================*/

static void
append_entry( list_p, link_p )

	llist *list_p;
	llink *link_p;

{
	if ( list_p->head == (cursor)NULL )
	{
		link_p->next = (llink *)NULL;
		link_p->prev = (llink *)NULL;
		list_p->head = (cursor)link_p;
		list_p->tail = (cursor)link_p;
	}
	else
	{
		link_p->prev = (llink *)list_p->tail;
		link_p->next = (llink *)NULL;
		((llink *)(list_p->tail))->next = link_p;
		list_p->tail = (cursor)link_p;
	}
	return;
}


/*=============================================================================
Function:	prepend_entry()

Description:	This function is called by other functions within this file to
				link a new entry at the beginning of a linked list.

Inputs:		list_p		-	pointer to the linked list
							to which the link_p is to be
							prepended.

			link_p		-	pointer to the link which is
							to be prepended to the list
							pointed to by list_p

Returns:	None

Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Non-member Function
Globals:
			Accessed:	
			Changed :	
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	prepend_entry(), insert_entry(), unlink_entry(),
			del_all_entries()

Notes:		This function was intentionally written with no error checking. 
			All checking on the validity of parameters must be done
			by the calling function.  Note that this function cannot
			be called outside of this file.
=============================================================================*/

static void
prepend_entry( list_p, link_p )

	llist *list_p;
	llink *link_p;

{
	if ( list_p->head == (cursor)NULL )
	{
		link_p->next = (llink *)NULL;
		link_p->prev = (llink *)NULL;
		list_p->head = (cursor)link_p;
		list_p->tail = (cursor)link_p;
	}
	else
	{
		link_p->next = (llink *)list_p->head;
		link_p->prev = (llink *)NULL;
		((llink *)(list_p->head))->prev = link_p;
		list_p->head = (cursor)link_p;
	}
	return;
}


/*=============================================================================
Function:	insert_entry()

Description:	This function is called by other functions in this file to
				insert a link entry before a given link an a linked list.

Inputs:		list_p		-	pointer to the linked list
							to which the link_p is to be
							appended.

			new_link_p	-	pointer to the link which is
							to be appended to the list
							pointed to by list_p

			u_link_p	-	pointer to the link before which
							new_link_p is to be inserted.

Returns:	None

Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Non-member Function
Globals:
			Accessed:	
			Changed :	
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	append_entry(), prepend_entry(), unlink_entry(),
			del_all_entries()

Notes:		This function was intentionally written with no error checking. 
			All checking on the validity of parameters must be done
			by the calling function.  Note that this function cannot
			be called outside of this file.
=============================================================================*/

static void
insert_entry( list_p, new_link_p, u_link_p )

	llist *list_p;
	llink *new_link_p;
	llink *u_link_p;

{
	/*
	 * Check that there is something in
	 * the linked list - if there is not,
	 * then insert the new link object
	 * into the list.
	 */

	if ( list_p->head == NULL )
	{
		new_link_p->next = (llink *)NULL;
		new_link_p->prev = (llink *)NULL;
		list_p->head = (cursor)new_link_p;
		list_p->tail = (cursor)new_link_p;
	}


	/*
	 * Insert the link object into the
	 * list before the cursor.
	 */

	new_link_p->next = u_link_p;
	new_link_p->prev = u_link_p->prev;
	if ( u_link_p->prev != NULL )
	{
		u_link_p->prev->next = new_link_p;
		u_link_p->prev = new_link_p;
	}
	else
	{
		list_p->head = (cursor) new_link_p;
		u_link_p->prev = new_link_p;
	}
	return;
}


/*=============================================================================
Function:	unlink_entry()

Description:	This function is called by other functions within this file
				to remove an entry and its link from a specified linked list,
				making sure that the links remaining in the list are linked
				together correctly.

Inputs:		list_p		-	pointer to the linked list
							from which the link_p is to be
							unlinked.

			link_p		-	pointer to the link which is
							to be unlinked from the list
							pointed to by list_p

Returns:	None

Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Non-member Function
Globals:
			Accessed:	
			Changed :	
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	append_entry(), prepend_entry(), insert_entry(),
			del_all_entries()

Notes:		This function was intentionally written with no error checking. 
			All checking on the validity of parameters must be done
			by the calling function.  Note that this function cannot
			be called outside of this file.
=============================================================================*/

static void
unlink_entry( list_p, link_p )

	llist *list_p;
	llink *link_p;

{
	if ( link_p->prev == (llink *)NULL )
	{
		list_p->head = (cursor)link_p->next;
		if ( link_p->next == (llink *)NULL )
			list_p->tail = (cursor)NULL;
		else
			link_p->next->prev = (llink *)NULL;
	}
	else
	{
		link_p->prev->next = link_p->next;
		if ( link_p->next == (llink *)NULL )
			list_p->tail = (cursor)link_p->prev;
		else
			link_p->next->prev = link_p->prev;
	}

	/*
	 * Note: do not change the next and
	 * prev field pointers in the link
	 * object here.  It is possible that
	 * they may be used subsequently by
	 * callers of this function.
	 */

	return;
}


/*=============================================================================
Function:	del_all_entries()

Description:	The purpose of this function is to remove all of the entries
				from a given linked list.

Inputs:		list_p		-	pointer to the linked list
							which is to be cleaned out.

Returns:	None

Date:		11/27/1989
Author:		Tim Graham
Scope:		Private - Non-Member Function
Globals:
			Accessed:	
			Changed :	
Statics:
			Accessed:	
			Changed :	
Warnings:	
See Also:	append_entry(), prepend_entry(), insert_entry(),
			del_all_entries()

Notes:		This function was intentionally written with no error checking. 
			All checking on the validity of parameters must be done
			by the calling function.  Note that this function cannot
			be called outside of this file.
=============================================================================*/

static void
del_all_entries( list_p )

	llist *list_p;

{
	llink *link_p = (llink *)NULL;
	llink *temp_link_p = (llink *)NULL;


	/*
	 * Start at the tail of the list, and
	 * work back through the list, calling the
	 * remove functions of the entries and
	 * deleting the entries from the list.
	 */

	link_p = (llink *)list_p->tail;
	if ( link_p == (llink *)NULL )
		return;

	while( link_p->prev != (llink *)NULL )
	{
		temp_link_p = link_p->prev;
		unlink_entry( list_p, link_p );
		if ( link_p->rm_func_p )
			(*link_p->rm_func_p)( link_p->rm_func_parm );
		ZFREE( (char *)link_p, sizeof( llink ) );
		link_p = temp_link_p;
	}
	unlink_entry( list_p, link_p );
	if ( link_p->rm_func_p )
		(*link_p->rm_func_p)( link_p->rm_func_parm );
	ZFREE( (char *)link_p, sizeof( llink ) );
		
	return;
}
