#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*=============================================================================
File:		nmalloc.h
Date:		09/20/1989
Author:		S.Wright
Description:	Memory sub-system, written by Bjorn Benson, captured off
				the net.
Change History:
			T. McKillop		01/15/96	changed ZNEW to use calloc instead
										of: a call to malloc, then a call to
										zclr which then has to do still another
										function call.
=============================================================================*/
#pragma ident	"@(#)nmalloc.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/global/SCCS/s.nmalloc.h"

#ifndef		_NMALLOC_
#define		_NMALLOC_

#ifdef		DEBUG_HEAP

extern	int	_free();
extern 	char 	*_new();
extern	void	*zclr();
extern	void	zfree();

#define	NEW(SZ)		_new( SZ, __FILE__, __LINE__ )
#define	FREE(PTR)	_free( PTR, __FILE__, __LINE__ )
#define	ZNEW(SZ)	zclr( (void *)_new( SZ , __FILE__ , __LINE__ ) , SZ )
#define	ZFREE(PTR,SZ)	zfree( PTR , SZ )

#else

extern	void	*zclr();
extern	void	zfree();

#define NEW(SZ)         (char *)malloc( SZ )
#define FREE(PTR)       (void)free( PTR )
/* ORIG DEF'N (replaced 01/15/96:
--	#define ZNEW(SZ)        zclr( (void *)malloc( SZ ) , SZ )
*/
#define ZNEW(SZ)		calloc( (size_t) 1, (size_t) (SZ) )
#define ZFREE(PTR,SZ)   zfree( PTR , SZ )

#endif /* 		DEBUG_HEAP      */

#endif /* _NMALLOC_ */

