/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*=============================================================================
File:		nmalloc.h
Date:		09/20/1989
Author:		S.Wright
Description:	Memory sub-system, written by Bjorn Benson, captured off
		the net.
=============================================================================*/

#ifndef		_NMALLOC_
#define		_NMALLOC_

#pragma ident "@(#)nmalloc.h	1.1  11/21/96"

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
#define ZNEW(SZ)        zclr( (void *)malloc( SZ ) , SZ )
#define ZFREE(PTR,SZ)   zfree( PTR , SZ )

#endif /* 		DEBUG_HEAP      */

#endif

