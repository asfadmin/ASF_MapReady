/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	znew.c

Description:	This file contains a function which allocates a set of memory
		and initializes its contents to zero, and a function which
		zeroes and frees a set of previously allocated memory.

Creator:	Tim Graham

External Functions:
		zclr( cptr , size )
		znew( size )
		zfree( cptr )
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)znew.c	1.1  11/21/96";

#include	"dapps_defs.h"
#include	"nmalloc.h"


/*=============================================================================
Function:	zclr( ptr , size )
Description:	Clears a region of memory and returns the pointer to it.
Inputs:		ptr	Pointer to the memory.
		size	size of region to allocate.
Returns:	(void *)
		NULL	if an error occures
Date:		11/28/1989
Author:		S.Wright
Scope:		External
Globals:	none
   Accessed:	
   Changed :	
Statics:	none
   Accessed:	
   Changed :	
Warnings:	
See Also:	
Notes:		
=============================================================================*/
void *
zclr( ptr , size )
void	*ptr;
int	size;
{
    if( !ptr )
	return( (void *)NULL );

    bzero( ptr , size );
    return( ptr );
}



/*=============================================================================
Function:	znew( size )
Description:	Uses NEW to allocate memory and zeros the region allocated.
Inputs:		size	size of region to allocate.
Returns:	(void *)
		NULL	if an error occures
Date:		10/31/1989
Author:		S.Wright
Scope:		External
Globals:	none
   Accessed:	
   Changed :	
Statics:	none
   Accessed:	
   Changed :	
Warnings:	
See Also:	
Notes:		
=============================================================================*/

void *
znew( size )
int	size;
{
void	*mem;

    if( (mem = (void *)NEW( size )) == (void *)NULL )
    {
	return( (void *) NULL );
    }

    bzero( (char *)mem , size );
    return( (void *)mem );
}


/*=============================================================================
Function:	zfree( cptr , nbytes );
Description:	Frees memory and clears it.  Should be used when deallocating
		data structures.  Forces some types of bugs to the surface
		when accessing deallocated memory.
Inputs:		
Returns:	
Date:		11/07/1989
Author:		S.Wright
Scope:		Extern
Globals:	None
   Accessed:	
   Changed :	
Statics:	None
   Accessed:	
   Changed :	
Warnings:	Note that for the current time, I will clear the heap
		space after it is free'd.  This will cause some grief among 
		our puritanic but the FREE function writes 0xf8 on free'd
		memory and that causes other problems.
See Also:	
Notes:		
=============================================================================*/
void
zfree( cptr , nbytes )

char	*cptr;
int	nbytes;

{
int	retval;

    bzero( cptr , nbytes );
    FREE( cptr );
}
