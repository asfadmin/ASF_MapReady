/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*=============================================================================
Filename:	dapps_ord_list.c
Date:		11/27/1989
Author:		Tim Graham

Description:	This file contains the constructors, member functions, and
		auxiliary functions which implement a generic
		ordered linked list class and its derived types.

External Functions:
		create_dyn_ord_llist()	  - dynamic ord_llist constructor
		init_static_ord_llist()	  - static ord_llist initializer

Static Functions:
   Public Member Functions:
		Insert_Entry_by_Key()
		Get_Entry_by_Key()
		Get_Entry_at_Cursor()
		Get_First_Entry()
		Get_Last_Entry()
		Get_Next_Entry()
		Get_Prev_Entry()
		Delete_Entry_by_Key()
		Delete_Entry_at_Cursor()
		Delete_All_Entries()
		Destroy_List()
		Cant_Destroy_List()

   Private Member Functions:
		Create_Ord_Link_Object()

   Private Non-Member Functions:
		init_dyn_ord_llist()
		append_entry()
		prepend_entry()
		insert_entry()
		unlink_entry()

Globals:
   Accessed:	
   Changed:	llist_errno

Waivers:	
Warnings:	
See Also:	dapps_list.h, ord_llist.h, llist.h, dapps_list.c

Notes:	 	
	
	      -- SEE NEXT PAGE FOR CHANGE HISTORY OF THIS FILE --

Change History:
	Always add to the top of this list, so that the file creation notice 
	is last in the list.  Feel free to take multiple lines for change
	descriptions, but please keep things aligned on tabs nicely and
	keep the change numbers up to date.

   # 	DATE		AUTHOR		CHANGE REASON AND DESCRIPTION
  ---	----		------		-----------------------------
  2)	mm/dd/yyyy	Change author
  1)	11/27/1989	Tim Graham	File Creation

=============================================================================*/


static char SccsFileId[] = "@(#)dapps_ord_list.c	1.1  11/21/96";

#include <stdio.h>
#include "llist_err.h"
#include "dapps_defs.h"
#include "nmalloc.h"
#include "dapps_list.h"


				/*
				 * The ord_llink type is defined in this file 
				 * in order to prevent use of its internals 
				 * by objects other than linked lists.
				 * In order to implement the get_next() 
				 * function of the linked list in such a 
				 * manner that a pointer to the current link 
				 * object in the linked list is not contained 
				 * in the ord_llist object, it is necessary 
				 * that the pointer be in user variable space. 
				 * In order to minimize the chances of 
				 * unauthorized access (by users) of private 
				 * elements of the ord_llink structure, this 
				 * pointer will be declared to be of type 
				 * cursor (defined in dapps_types.h).
				 */

typedef struct ord_llink ord_llink;

struct ord_llink
{
	void			*entry_p;	/* ptr to obj. in list */
	ord_llink		*next;		/* ptr to next link */
	ord_llink		*prev;		/* ptr to prev link */
	ord_llist		*list_p;	/* ptr to "owner" list */
	void			(*rm_func_p)();	/* func to call at remove */
	void			*rm_func_parm;	/* parm for rm_func_p */
};


/*=============================================================================
Function:	create_dyn_ord_llist()

Description:	This function dynamically allocates and initializes a new
		ordered linked list object.

Inputs:		ins_cmp_func_p	-	A pointer to a function which will
					compare the kinds of entries which
					will be put into the ordered list.
					The pointer will be called as follows:
					 ins_cmp_func_p( entry_1, entry_2 )
					and will return a positive, zero, or
					negative integer, as entry_1 is 
					greater than, equal to, or less than 
					entry_2, respectively (as determined 
					by the order imposed by the caller).

Returns:	new_list_p	-	Pointer to the new ordered linked list 
					object.

		NULL		-	If this function fails.  In this case,
					the llist_errno variable will be set to 
					one of the following values:

					ELISTNOCF:   If a NULL pointer to a 
						     comparison function
						     was passed as a parameter.

					ELMALLOC:    If no heap space is 
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

See Also:	init_dyn_ord_llist(), init_static_ord_llist(), nmalloc.h, 
		nmalloc.c, znew.c

Notes:		This function sets a flag in the linked list object which will
		ensure that the object is not subsequently initialized using
		the init_static_ord_llist() external function in this file.
=============================================================================*/

extern ord_llist *
create_dyn_ord_llist( ins_cmp_func_p )

int (*ins_cmp_func_p)(); /* A pointer to an insert comparison function */

{
extern void zfree();
int	init_dyn_ord_llist();
ord_llist	*new_list_p = (ord_llist *)NULL;


				/*
				 * Check that the pointer to the
				 * insertion comparison function which
				 * was passed in the parameter list is
				 * non-NULL.
				 */

	if ( ins_cmp_func_p == (int (*)())NULL )
	{
		llist_errno = ELISTNOCF;
		return( (ord_llist *)NULL );
	}


				/*
				 * Allocate the memory for the new object,
				 * and initialize the memory to zero.
				 * Note: ZNEW() is a macro to allocate and
				 * initialize memory.  It is defined
				 * in "znew.c" and "nmalloc.h".
				 */

	if ( (new_list_p = (ord_llist *)ZNEW( sizeof( ord_llist ) )) 
		== (ord_llist *)NULL )
	{
		llist_errno = ELMALLOC;
		return( (ord_llist *)NULL );
	}


				/*
				 * Initialize the new object.
				 */

	if ( init_dyn_ord_llist( new_list_p, ins_cmp_func_p ) == ERROR )
	{
		llist_errno = ELISTINIT;
		ZFREE( (char *)new_list_p, sizeof( ord_llist ) );
		return( (ord_llist *)NULL );
	}


	return( new_list_p );
}


/*=============================================================================
Function:	init_static_ord_llist()

Description:	This function initializes a statically defined (that is,
		defined at compile time) ordered linked list object.

Inputs:		list_p		-	A pointer to the statically allocated
					ordered linked list object which is to 
					be initialized.

		ins_cmp_func_p	-	A pointer to a function which will
					compare the kinds of entries which
					will be put into the ordered list.
					The pointer will be called as follows:
					 ins_cmp_func_p( entry_1, entry_2 )
					and will return a positive, zero, or
					negative integer, as entry_1 is 
					greater than, equal to, or less than 
					entry_2, respectively (as determined 
					by the order imposed by the caller).

Returns:	OK		-	if it succeeds

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

					ELISTNOCF:   If a NULL pointer to a 
						     comparison function
						     was passed as a parameter.

Date:		11/27/1989
Author:		Tim Graham
Scope:		Public - Non-member Function
Globals:
   Accessed:	
   Changed :	llist_errno
Statics:
   Accessed:	
   Changed :	
Warnings:	If the static ord_llist structure is not bzeroed prior to 
		calling this routine, it is possible that this routine will 
		not allow it to be initialized, thinking that it has already 
		been initialized.

See Also:	create_dyn_ord_llist(), init_dyn_ord_llist()

Notes:		Ensure that the static ord_llist structure to be initialized has
		been bzeroed prior to calling this routine.
=============================================================================*/

extern int
init_static_ord_llist( list_p, ins_cmp_func_p )

ord_llist *list_p;	 /* Pointer to the statically allocated list to init. */
int (*ins_cmp_func_p)(); /* Pointer to an insert comparison function. */

{
void		*Insert_Entry_by_Key();
void		*Get_Entry_by_Key();
void		*Get_Entry_at_Cursor();
void		*Get_First_Entry();
void		*Get_Last_Entry();
void		*Get_Next_Entry();
void		*Get_Prev_Entry();
int		Delete_Entry_by_Key();
int		Delete_Entry_at_Cursor();
int		Delete_All_Entries();
int		Cant_Destroy_List();
cursor		Create_Link_Object();




				/*
				 * First make sure that we have not
				 * been passed a NULL pointer for the
				 * list_p parameter.
				 */

	if ( list_p == (ord_llist *)NULL )
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
				 * Check that the pointer to the
				 * insertion comparison function which
				 * was passed in the parameter list is
				 * non-NULL.
				 */

	if ( ins_cmp_func_p == (int (*)())NULL )
	{
		llist_errno = ELISTNOCF;
		return( ERROR );
	}


				/*
				 * Let's start with a zeroed structure
				 */

	bzero( (char *)list_p, sizeof( ord_llist ) );

	

				/*
				 * Initialize the public part of the 
				 * ord_llist object.
				 */

	list_p->insert = Insert_Entry_by_Key;
	list_p->get_by_key = Get_Entry_by_Key;
	list_p->get_by_cursor = Get_Entry_at_Cursor;
	list_p->get_first = Get_First_Entry;
	list_p->get_last = Get_Last_Entry;
	list_p->get_next = Get_Next_Entry;
	list_p->get_prev = Get_Prev_Entry;
	list_p->del_by_key = Delete_Entry_by_Key;
	list_p->del_by_cursor = Delete_Entry_at_Cursor;
	list_p->rm_all_entries = Delete_All_Entries;
	list_p->destroy_list = Cant_Destroy_List;


				/*
				 * Initialize the private part of the
				 * ord_llist object.
				 */

	list_p->num_elts = 0;
	list_p->create_link = Create_Link_Object;
	list_p->ins_cmp_func_p = ins_cmp_func_p;


				/*
				 * Mark the fact that this object has
				 * been initialized by setting the
				 * initialized flag.  Note that this
				 * flag is used for checking in the
				 * static ord_llist initializer that it
				 * is not being called for an ord_llist
				 * which has already been created by
				 * this routine.
				 */

	list_p->initialized = TRUE;

	return( OK );
}


/*=============================================================================
Function:	Insert_Entry_by_Key()

Description:	Sort an entry into an ord_llist in an increasing order (the 
		caller provides the ordering function at the time of creation 
		of the ord_llist).  Note that if the entry which we are
		inserting has a key which is equal in the given order to one
		that is already in the list, the new entry will be inserted
		before the one that is already there.

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
					type cursor which will be updated
					by this function to indicate the end
					of the list.  This parameter may
					be NULL.

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

					ELISTNOCF:    If there is no comparison
						      function pointed to by
						      the cmp_func_p field of
						      the list at list_p.

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
See Also:	
Notes:		
=============================================================================*/

void *
Insert_Entry_by_Key( list_p, entry_p, rm_func_p, rm_func_parm, u_cursor_p )

ord_llist *list_p;	/* List in which to insert */
void *entry_p;		/* Entry to insert in the list */
void (*rm_func_p)();	/* Function to call when entry is removed */
void *rm_func_parm;	/* Parameters to the rm_func */
cursor *u_cursor_p;	/* Ptr to (allocated) cursor in caller space */

{
void append_entry();
void prepend_entry();
void insert_entry();
int cf_retval = 0;
ord_llink *link_p = (ord_llink *)NULL;
ord_llink *new_link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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
				 * Make sure that there is an
				 * ins_cmp_func_p - if there isn't,
				 * the ord_llist object has been corrupted
				 * in some way.
				 */

	if ( list_p->ins_cmp_func_p == (int (*)())NULL )
	{
		llist_errno = ELISTNOCF;
		return( (void *)NULL );
	}


				/*
				 * Allocate an ord_llink object to put
				 * into the list of links.
				 */

	if ( (new_link_p = (ord_llink *)list_p->create_link( list_p, entry_p,
			rm_func_p, rm_func_parm )) == (ord_llink *)NULL )
	{
		llist_errno = ELINKCREATE;
		return( (void *)NULL );
	}


				/*
				 * If the list is empty, put the new
				 * entry at the beginning of the list.
				 */

	if ( (link_p = (ord_llink *)list_p->head) == (ord_llink *)NULL )
	{
		prepend_entry( list_p, new_link_p );
		if ( u_cursor_p != (cursor *)NULL )
			*u_cursor_p = (cursor)new_link_p;
		list_p->num_elts++;
		return( new_link_p->entry_p );
	}


				/*
				 * Start at the beginning of the list,
				 * and until we reach the appropriate
				 * insertion point, call the user provided 
				 * comparison function for each entry in
				 * turn to determine if the entry which is 
				 * to be inserted is greater than, equal to, 
				 * or less than the current entry.  As soon
				 * as an entry already in the list is greater
				 * than or equal to the new entry, insert
				 * the new entry before that entry.
				 */

	while ( link_p != (ord_llink *)NULL )
	{
		if ( list_p->ins_cmp_func_p( link_p->entry_p, 
						new_link_p->entry_p) >= 0 )
		{
			insert_entry( list_p, new_link_p, link_p );
			if ( u_cursor_p != (cursor *)NULL )
				*u_cursor_p = (cursor)new_link_p;
			list_p->num_elts++;
			return( new_link_p->entry_p );
		}
		link_p = link_p->next;
	}
	append_entry( list_p, new_link_p );
	if ( u_cursor_p != (cursor *)NULL )
		*u_cursor_p = (cursor)new_link_p;
	list_p->num_elts++;
	return( new_link_p->entry_p );
}


/*=============================================================================
Function:	Get_Entry_by_Key()

Description:    The purpose of this function is to retrieve an entry from a
		particular linked list by some key in the entry.   A user-
		provided function will be required to perform this function
		(see the Notes section in this header).

Inputs:		list_p		-	a pointer to the list which is to be
					searched for the key.

		key		-	a pointer to a variable of type key
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

void *
Get_Entry_by_Key( list_p, key, cmp_func_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
void *key;		/* Ptr to the key to pass to cmp_func_p */
int (*cmp_func_p)();	/* Ptr to a caller funct to compare two list entries */
cursor *u_cursor_p;	/* Ptr to a (caller allocated) cursor into list_p */

{
ord_llink *link_p = (ord_llink *)NULL;
int cf_retval = 0;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)list_p->head;

	while ( link_p != (ord_llink *)NULL )
	{
		if ( (cf_retval = (*cmp_func_p)( key, link_p->entry_p )) 
			== TRUE )
		{
			if ( u_cursor_p )
				*u_cursor_p = (cursor) link_p;
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

void *
Get_Entry_at_Cursor( list_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)*u_cursor_p;

	return( link_p->entry_p );
}


/*=============================================================================
Function:	Get_First_Entry()

Description:	This function is externally called via a function pointer
		in the ord_llist object.  Its purpose is to retrieve the first
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

void *
Get_First_Entry( list_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)list_p->head;
	if ( link_p != (ord_llink *)NULL )
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
}


/*=============================================================================
Function:	Get_Last_Entry()

Description:	This function is called externally via a function pointer in 
		ord_llist object.  Its purpose is to retrieve the last entry in a
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

void *
Get_Last_Entry( list_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)list_p->tail;
	if ( link_p != (ord_llink *)NULL )
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
}


/*=============================================================================
Function:	Get_Next_Entry()

Description:	This function is called externally via a function pointer in
		the ord_llist object.  Its purpose is to retrieve the element
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

					EENDOFLIST:   If the cursor already
						      points to the link at
						      the end of the list.

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

void *
Get_Next_Entry( list_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)*u_cursor_p;
	if ( link_p == (ord_llink *)NULL )
	{
		if ( list_p->head == (cursor)NULL )
		{
			llist_errno = ELISTEMPTY;
			return( (void *)NULL );
		}
		else
		{
			*u_cursor_p = list_p->head;
			link_p = (ord_llink *)*u_cursor_p;
			return( link_p->entry_p );
		}
	}
	else if ( link_p->next == (ord_llink *)NULL )
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
}


/*=============================================================================
Function:	Get_Prev_Entry()

Description:	This function is called externally via a function pointer in
		the ord_llist object.  Its purpose is to retrieve the entry in a
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

					EBEGOFLIST:   If the cursor already
						      points to the link at
						      the beginning of the list.

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

void *
Get_Prev_Entry( list_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
cursor *u_cursor_p;	/* Ptr to a cursor into list_p */

{
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)*u_cursor_p;
	if ( link_p == (ord_llink *)NULL )
	{
		if ( list_p->tail == (cursor)NULL )
		{
			llist_errno = ELISTEMPTY;
			return( (void *)NULL );
		}
		else
		{
			*u_cursor_p = list_p->tail;
			link_p = (ord_llink *)*u_cursor_p;
			return( link_p->entry_p );
		}
	}
	else if ( link_p->prev == (ord_llink *)NULL )
	{
		/* at end of list - set cursor to NULL */
		llist_errno = EBEGOFLIST;
		*u_cursor_p = (cursor)NULL;
		return( (void *)NULL );
	}
	else
	{
		*u_cursor_p = (cursor)link_p->prev;
		return( link_p->prev->entry_p );
	}
}


/*=============================================================================
Function:	Delete_Entry_by_Key()

Description:	This function is called externally via a function pointer in
		the ord_llist object.  Its purpose is to locate the entry in a
		specified linked list which has a particular key, and to delete
		the entry.  Note that the location of the appropriate entry to
		delete is done via a user-provided function.  For the interface
		required of this user-provided function, see the Notes section
		below.

Inputs:		list_p		-	a pointer to the list from which the
					entry with the given key is to be
					deleted.

		key		-	a pointer to a variable of type key
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
					to the link at the head of the list
					otherwise.  This parameter may be
					NULL.

Returns:	OK		-	if the deletion succeeds.

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

int
Delete_Entry_by_Key( list_p, key, cmp_func_p, u_cursor_p )

ord_llist *list_p;	/* Ptr to the list we are manipulating */
void *key;		/* Ptr to the key we are to compare with */
int (*cmp_func_p)();	/* Ptr to the funct we use to compare */
cursor *u_cursor_p;	/* Ptr to a cursor which points into list_p */

{
extern void zfree();
void unlink_entry();
ord_llink *link_p = (ord_llink *)NULL;
int cf_retval = 0;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)list_p->head;

	while ( link_p != (ord_llink *)NULL )
	{
		if ( (cf_retval = (*cmp_func_p)( key, link_p->entry_p )) 
			== TRUE )
		{
				/* update the caller's cursor */
			if ( link_p->prev != (ord_llink *)NULL )
			{
				if ( u_cursor_p )
					*u_cursor_p = (cursor)link_p->prev;
			}
			else 
			{
				if ( u_cursor_p )
					*u_cursor_p = (cursor)link_p->next;
			}

				/* unlink it */
			unlink_entry( list_p, link_p );

				/* Call rm_func if there is one */
			if ( link_p->rm_func_p )
				(*link_p->rm_func_p)( link_p->rm_func_parm );

				/* zero and free it */
			ZFREE( (char *)link_p, sizeof( ord_llink ) );

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
		the cursor (ord_llink object) which is passed in as a parameter.

Inputs:		list_p		-	a pointer to the list from which the
					entry at the cursor position is to 
					be deleted.

		u_cursor_p	-	A pointer to a variable (for which
					memory has already been allocated) of
					type cursor which indicates the
					position in the list from which the
					desired entry is to be deleted.

Returns:	OK		-	if the function does not fail.

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

int
Delete_Entry_at_Cursor( list_p, u_cursor_p )

ord_llist *list_p;		/* Ptr to the list we are manipulating */
cursor *u_cursor_p;	/* Ptr to a cursor which points into list_p */

{
extern void zfree();
void unlink_entry();
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

	link_p = (ord_llink *)*u_cursor_p;

	if ( link_p == (ord_llink *)NULL )
	{
		llist_errno = ENULLCURSOR;
		return( ERROR );
	}

	if ( link_p->prev != (ord_llink *)NULL )
		*u_cursor_p = (cursor) link_p->prev;
	else *u_cursor_p = (cursor) link_p->next;

	unlink_entry( list_p, link_p );  /* unlink it */

	if ( link_p->rm_func_p )
		(*link_p->rm_func_p)( link_p->rm_func_parm );

	ZFREE( (char *)link_p, sizeof( ord_llink ) );

	if ( list_p->num_elts == 0 )
		return( ERROR );
	else list_p->num_elts--;

	return( OK );
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

Returns:	OK		-	if the function does not fail.

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

int
Delete_All_Entries( list_p, u_cursor_p )

ord_llist *list_p;		/* The list which we are to clean out */
cursor *u_cursor_p;	/* Pointer to a cursor into list_p - we NULL it */

{
extern void zfree();
void del_all_entries();


				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}



				/*
				 * Remove all of the entries from the
				 * list.
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
		in the ord_llist object.  Its purpose is to clean out a given
		linked list, and then to free all space which was used by the
		linked list.  Note that this function cannot be called for
		statically allocated linked lists.  See the Notes section below
		for a discussion of how this is prevented.

Inputs:		list_p		-	a pointer to the list which is to be
					destroyed.

Returns:	OK		-	if the function does not fail.

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
See Also:	Cant_Destroy_List(), create_dyn_ord_llist(), init_static_ord_llist(), 
		init_dyn_ord_llist()

Notes:		This function is pointed to by the destroy_list method of the
		ord_llist object when the object in question has been 
		dynamically allocated.  If the ord_llist object was statically 
		allocated, and was initialized by using the 
		init_static_ord_llist() function, the destroy_list method was 
		initialized to point at the Cant_Destroy_List method.
=============================================================================*/

int
Destroy_List( list_p )

ord_llist *list_p;		/* Ptr to the list we are to destroy */

{
extern void zfree();
void del_all_entries();


				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
	{
		llist_errno = EBADLIST;
		return( ERROR );
	}


				/*
				 * Remove all of the entries from the
				 * list.
				 */

	del_all_entries( list_p );



				/*
				 * Zero and free the list object
				 * itself.
				 */

	ZFREE( (char *)list_p, sizeof( ord_llist ) );

	return( OK );
}


/*=============================================================================
Function:	Cant_Destroy_List()

Description:	This function is pointed to by the destroy_list field of the
		ord_llist object when the object was statically allocated.  
		Since the object was statically allocated, we are not allowed to
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
					llist_errnllist_errno variable will be set to one
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
See Also:	Destroy_List(), create_dyn_ord_llist(), init_static_ord_llist(), 
		init_dyn_ord_llist()

Notes:		This function cannot be called for dynamic ord_llist objects.
		In dynamic linked list objects, the destroy_list method
		is initialized to point to the Destroy_List() method, whereas
		in static linked list objects, the destroy_list method is
		initialized to point to the Cant_Destroy_List() method.
=============================================================================*/

int
Cant_Destroy_List( list_p )

ord_llist *list_p;		/* Ptr to the list caller wants to destroy */

{


				/*
				 * First check that we are manipulating a
				 * valid list object.
				 */

	if ( ( list_p == (ord_llist *)NULL ) || 
					( list_p->initialized != TRUE ) )
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

Description:	The purpose of this function is to create an ord_llink object 
		and to initialize its fields.  It is called via a function 
		pointer by other methods of the ord_llist object.

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

					ELMALLOC:    If no heap space is 
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

cursor
Create_Link_Object( list_p, entry_p, rm_func_p, rm_func_parm )

ord_llist *list_p;
void *entry_p;
void (*rm_func_p)();
void *rm_func_parm;

{
extern int llist_errno;
ord_llink *link_p = (ord_llink *)NULL;



				/*
				 * Allocate the memory for the new object,
				 * and initialize the memory to zero.
				 * Note: ZNEW() is a macro to allocate and
				 * initialize memory.  It is defined in
				 * "znew.c" and "nmalloc.h".
				 */

	if ( (link_p = (ord_llink *)ZNEW( sizeof( ord_llink ) )) == (ord_llink *)NULL )
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
Function:	init_dyn_ord_llist()

Description:	The purpose of this function is to initialize a dynamically
		allocated linked list object.

Inputs:		new_list_p	-	pointer to the new linked list,
					which is to be initialized.

Returns:	OK		-	if the function succeeds.

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
See Also:	create_dyn_ord_llist(), init_static_ord_llist(), Destroy_List(),
		Cant_Destroy_List()
Notes:		
=============================================================================*/

int
init_dyn_ord_llist( new_list_p, ins_cmp_func_p )

ord_llist *new_list_p;
int (*ins_cmp_func_p)();

{
void		*Append_to_List();
void		*Prepend_to_List();
void		*Insert_Entry_before_Cursor();
void		*Get_Entry_by_Key();
void		*Get_Entry_at_Cursor();
void		*Get_First_Entry();
void		*Get_Last_Entry();
void		*Get_Next_Entry();
void		*Get_Prev_Entry();
int		Delete_Entry_by_Key();
int		Delete_Entry_at_Cursor();
int		Delete_All_Entries();
int		Destroy_List();
cursor		Create_Link_Object();



				/*
				 * If the passed in pointer is NULL,
				 * we had better not try to initialize
				 * what it points at.
				 */

	if ( new_list_p == (ord_llist *)NULL )
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

	new_list_p->insert = Insert_Entry_by_Key;
	new_list_p->get_by_key = Get_Entry_by_Key;
	new_list_p->get_by_cursor = Get_Entry_at_Cursor;
	new_list_p->get_first = Get_First_Entry;
	new_list_p->get_last = Get_Last_Entry;
	new_list_p->get_next = Get_Next_Entry;
	new_list_p->get_prev = Get_Prev_Entry;
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
	new_list_p->ins_cmp_func_p = ins_cmp_func_p;



				/*
				 * Mark the fact that this object has
				 * been initialized by setting the
				 * initialized flag.  Note that this
				 * flag is used for checking in the
				 * static ord_llist initializer that it
				 * is not being called for an ord_llist
				 * which has already been created by
				 * this routine.
				 */

	new_list_p->initialized = TRUE;
	
	return( OK );
}


/*=============================================================================
Function:	append_entry()

Description:	This function is called by other functions in this file to
		link a new entry at the end of a linked list (ord_llist) object.

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

void
append_entry( list_p, link_p )

ord_llist *list_p;
ord_llink *link_p;

{
	if ( list_p->head == (cursor)NULL )
	{
		link_p->next = (ord_llink *)NULL;
		link_p->prev = (ord_llink *)NULL;
		list_p->head = (cursor)link_p;
		list_p->tail = (cursor)link_p;
	}
	else
	{
		link_p->prev = (ord_llink *)list_p->tail;
		link_p->next = (ord_llink *)NULL;
		((ord_llink *)(list_p->tail))->next = link_p;
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

void
prepend_entry( list_p, link_p )

ord_llist *list_p;
ord_llink *link_p;

{
	if ( list_p->head == (cursor)NULL )
	{
		link_p->next = (ord_llink *)NULL;
		link_p->prev = (ord_llink *)NULL;
		list_p->head = (cursor)link_p;
		list_p->tail = (cursor)link_p;
	}
	else
	{
		link_p->next = (ord_llink *)list_p->head;
		link_p->prev = (ord_llink *)NULL;
		((ord_llink *)(list_p->head))->prev = link_p;
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

void
insert_entry( list_p, new_link_p, u_link_p )

ord_llist *list_p;
ord_llink *new_link_p;
ord_llink *u_link_p;

{
				/*
				 * Check that there is something in
				 * the linked list - if there is not,
				 * then insert the new link object
				 * into the list.
				 */

	if ( list_p->head == NULL )
	{
		new_link_p->next = (ord_llink *)NULL;
		new_link_p->prev = (ord_llink *)NULL;
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

void
unlink_entry( list_p, link_p )

ord_llist *list_p;
ord_llink *link_p;

{
	if ( link_p->prev == (ord_llink *)NULL )
	{
		list_p->head = (cursor)link_p->next;
		if ( link_p->next == (ord_llink *)NULL )
			list_p->tail = (cursor)NULL;
		else
			link_p->next->prev = (ord_llink *)NULL;
	}
	else
	{
		link_p->prev->next = link_p->next;
		if ( link_p->next == (ord_llink *)NULL )
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
See Also:	append_entry(), prepend_entry(), insert_entry(), unlink_entry()

Notes:		This function was intentionally written with no error checking. 
		All checking on the validity of parameters must be done
		by the calling function.  Note that this function cannot
		be called outside of this file.
=============================================================================*/

void
del_all_entries( list_p )

ord_llist *list_p;

{
ord_llink *link_p = (ord_llink *)NULL;
ord_llink *temp_link_p = (ord_llink *)NULL;


				/*
				 * Start at the tail of the list, and
				 * work back through the list, calling the
				 * remove functions of the entries and
				 * deleting the entries from the list.
				 */

	link_p = (ord_llink *)list_p->tail;
	if ( link_p == (ord_llink *)NULL )
		return;

	while( link_p->prev != (ord_llink *)NULL )
	{
		temp_link_p = link_p->prev;
		unlink_entry( list_p, link_p );
		if ( link_p->rm_func_p )
			(*link_p->rm_func_p)( link_p->rm_func_parm );
		ZFREE( (char *)link_p, sizeof( ord_llink ) );
		link_p = temp_link_p;
	}
	unlink_entry( list_p, link_p );
	if ( link_p->rm_func_p )
		(*link_p->rm_func_p)( link_p->rm_func_parm );
	ZFREE( (char *)link_p, sizeof( ord_llink ) );
		
	return;
}
