/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*=============================================================================
Filename:	nmalloc.c
Date:		09/20/1989
Author:		S.Wright
Description:	Replacement for malloc() and free().  It keeps track of memory
		allocated and freeed to allow tracking of memory leaks.
Externals:
		
Statics:
		
Globals:
   Accessed:	
   Changed:	
Waivers:	
Warnings:	
See Also:	
Notes:		

	Memory sub-system, written by Bjorn Benson.  This file was captured off
	of the net.
	
	      -- SEE NEXT PAGE FOR CHANGE HISTORY OF THIS FILE --

Change History:
	Always add to the top of this list, so that the file creation notice 
	is last in the list.  Feel free to take multiple lines for change
	descriptions, but please keep things aligned on tabs nicely and
	keep the change numbers up to date.

   # 	DATE		AUTHOR		CHANGE REASON AND DESCRIPTION
  ---	----		------		-----------------------------
  2)	mm/dd/yyyy	Change author
  1)	09/20/1989	S.Wright	File Creation

=============================================================================*/

static char SccsFileId[] = "@(#)nmalloc.c	1.1  11/21/96";


#include <stdio.h>

typedef	unsigned uns;	/* Shorthand */

struct REMEMBER_INTERNAL 
{
	struct REMEMBER *_pNext;	/* Linked list of structures	   */
	struct REMEMBER *_pPrev;	/* Other link			   */
	char	*_sFileName;		/* File in which memory was new'ed */
	uns	 _uLineNum;		/* Line number is above file	   */
	uns	 _uDataSize;		/* Size requested		   */
	uns	 _lSpecialValue;	/* Underrun marker value	   */
};

struct REMEMBER 
{
	struct REMEMBER_INTERNAL tInt;
	char	 aData[1];
};

#define	pNext		tInt._pNext
#define	pPrev		tInt._pPrev
#define	sFileName	tInt._sFileName
#define	uLineNum	tInt._uLineNum
#define	uDataSize	tInt._uDataSize
#define	lSpecialValue	tInt._lSpecialValue

static long lCurMemory = 0;	/* Current memory usage			*/
static long lMaxMemory = 0;	/* Maximum memory usage			*/
				/* Note: both these refer to the NEW'ed	*/
				/*       data only.  They do not include*/
				/*       malloc() roundoff or the extra	*/
				/*       space required by the REMEMBER	*/
				/*       structures.			*/
static uns cNewCount = 0;	/* Number of times NEW() was called	*/

static struct REMEMBER *pRememberRoot = NULL;
			/* Root of the linked list of REMEMBERs	*/

#define	ALLOC_VAL	0xA5	/* NEW'ed memory is filled with this	*/
				/* value so that references to it will	*/
				/* end up being very strange.		*/
#define	FREE_VAL	0x8F	/* FREE'ed memory is filled with this	*/
				/* value so that references to it will	*/
				/* also end up being strange.		*/

#define	MAGICKEY	0x14235296	/* A magic value for underrun key */
#define	MAGICEND0	0x68		/* Magic values for overrun keys  */
#define	MAGICEND1	0x34		/*		"		  */
#define	MAGICEND2	0x7A		/*              "		  */
#define	MAGICEND3	0x15		/*		"		  */
			/* Warning: do not change the MAGICEND? values to */
			/* something with the high bit set.  Various C    */
			/* compilers (like the 4.2bsd one) do not do the  */
			/* sign extension right later on in this code and */
			/* you will get erroneous errors.		  */




/*=============================================================================
Function:	_new()
Date:		09/20/1989
Author:		
Scope:		Global
Description:	Allocate some memory.
Inputs:		
 		uns uSize
		char *sFile
		uns uLine
Returns:	
Globals:
   Accessed:	
   Changed :	
Statics:
   Accessed:	
   Changed :	
Warnings:	This routine writes to standard error if one is detected.
See Also:	
Notes:		
=============================================================================*/


char *_new( uSize, sFile, uLine )
uns uSize,uLine;
char *sFile;
{
extern char *malloc();
struct REMEMBER *pTmp;
char *pPtr;

				/* 
				 *  Allocate the physical memory 
				 */

	pTmp = (struct REMEMBER *) malloc(
			  sizeof(struct REMEMBER_INTERNAL) /* REMEMBER data  */
			+ uSize				   /* size requested */
			+ 4				   /* overrun mark   */
		);

				/* 
				 *  Check if there isn't anymore memory 
				 *  avaiable 
				 */

	if( pTmp == NULL )
	{
		fprintf(stderr,"Out of memory at line %d, \"%s\"\n",
			uLine, sFile );
		fprintf(stderr,"\t(memory in use: %ld bytes (%ldk))\n",
			lMaxMemory, (lMaxMemory + 1023L)/1024L );
		return( NULL );
	}

				/* 
				 *  Fill up the structure 
				 */

	pTmp->lSpecialValue	= MAGICKEY;
	pTmp->aData[uSize+0]	= MAGICEND0;
	pTmp->aData[uSize+1]	= MAGICEND1;
	pTmp->aData[uSize+2]	= MAGICEND2;
	pTmp->aData[uSize+3]	= MAGICEND3;
	pTmp->sFileName		= sFile;
	pTmp->uLineNum  	= uLine;
	pTmp->uDataSize		= uSize;
	pTmp->pNext		= pRememberRoot;
	pTmp->pPrev		= NULL;

				/* 
				 *  Add this REMEMBER structure to the linked 
				 *  list 
				 */


	if( pRememberRoot )
		pRememberRoot->pPrev = pTmp;
	pRememberRoot = pTmp;

				/* 
				 *  Keep the statistics 
				 */

	lCurMemory += uSize;
	if( lCurMemory > lMaxMemory )
		lMaxMemory = lCurMemory;
	cNewCount++;

				/* 
				 *  Set the memory to the aribtrary wierd 
				 *  value 
				 */

	for( pPtr = &pTmp->aData[uSize]; pPtr > &pTmp->aData[0]; )
		*(--pPtr) = ALLOC_VAL;

				/* 
				 *  Return a pointer to the real data 
				 */

	return( &(pTmp->aData[0]) );
}


/*=============================================================================
Function:	_free()
Date:		09/20/1989
Author:		
Scope:		Global
Description:	Deallocate some memory.
Inputs:		
 		char *pPtr
		char *sFile
		uns uLine 
Returns:	NULL if an error is detected, pPtr otherwise.
Globals:
   Accessed:	
   Changed :	
Statics:
   Accessed:	
   Changed :	
Warnings:	This routine writes to standard err when an error is detected.	
See Also:	
Notes:		
=============================================================================*/

_free( pPtr, sFile, uLine )
char *pPtr,*sFile;
uns uLine;
{
struct REMEMBER *pRec;
char *pTmp;
uns uSize;

				/* 
				 *  Check if we have a non-null pointer 
				 */

	if( pPtr == NULL )
	{
		fprintf(stderr,"Freeing NULL pointer at line %d, \"%s\"\n",
			uLine, sFile );
		return( NULL );
	}

				/* 
				 *  Calculate the address of the REMEMBER 
				 *  structure 
				 */

	pRec = (struct REMEMBER *)( pPtr - 
			(sizeof(struct REMEMBER_INTERNAL)) );

				/* 
				 *  Check to make sure that we have a real 
				 *  REMEMBER structure 
				 *  Note: this test could fail for four reasons:
				 *   (1) The memory was already free'ed	
				 *   (2) The memory was never new'ed
				 *   (3) There was an underrun
				 *   (4) A stray pointer hit this location
				 */

	if( pRec->lSpecialValue != MAGICKEY )
	{
		fprintf(stderr,"Freeing unallocated data at line %d, \"%s\"\n",
			uLine, sFile );
		return( NULL );  /* NadiaAdhami 11/9/95, add (NULL) */
	}

					/* 
					 *  Check for an overrun 
					 */

	uSize = pRec->uDataSize;
	if( pRec->aData[uSize+0] != MAGICEND0
	 || pRec->aData[uSize+1] != MAGICEND1
	 || pRec->aData[uSize+2] != MAGICEND2
	 || pRec->aData[uSize+3] != MAGICEND3 )
	{
		fprintf(stderr,"Memory being free'ed at line %d, \"%s\" \
was overrun\n", uLine, sFile );
		fprintf(stderr,"\t(allocated at line %d, \"%s\")\n",
			pRec->uLineNum, pRec->sFileName );
		return( NULL );
	}

				/* 
				 *  Remove this structure from the linked 
				 *  list 
				 */

	if( pRec->pPrev )
		pRec->pPrev->pNext = pRec->pNext;
	else
		pRememberRoot = pRec->pNext;
	if( pRec->pNext )
		pRec->pNext->pPrev = pRec->pPrev;

				/* 
				 *  Mark this data as free'ed 
				 */

	for (pTmp = &pRec->aData[pRec->uDataSize]; pTmp > &pRec->aData[0] ; )
	    *(--pTmp) = FREE_VAL;
	pRec->lSpecialValue = ~MAGICKEY;

				/* 
				 *  Handle the statistics 
				 */

	lCurMemory -= pRec->uDataSize;
	cNewCount--;

				/* 
				 *  Actually free the memory 
				 */

	return( free( (char*)pRec ) );
}


/*=============================================================================
Function:	TERMINATE()
Date:		09/20/1989
Author:		
Scope:		Global
Description:	Report on all the memory pieces that have not been free'ed as 
		well as the statistics.
Inputs:	
Returns:	
Globals:
   Accessed:	
   Changed :	
Statics:
   Accessed:	
   Changed :	
Warnings:	
See Also:	
Notes:		
=============================================================================*/


TERMINATE()
{
struct REMEMBER *pPtr;

				/* 
				 *  Report the difference between number of 
				 *  calls to NEW and the number of calls to 
				 *  FREE.  >0 means more	NEWs than 
				 *  FREEs.  <0, etc.
				 */

	if( cNewCount )
		fprintf(stderr,"cNewCount: %d\n", cNewCount );

				/* 
				 *  Report on all the memory that was allocated
				 *  with NEW but not free'ed with FREE.
				 */

	if( pRememberRoot != NULL )
		fprintf(stderr, "Memory that was not free'ed:\n");
	pPtr = pRememberRoot;
	while( pPtr )
	{
		fprintf(stderr,"\t%5d bytes at 0x%06x, \
allocated at line %3d in \"%s\"\n",
			pPtr->uDataSize, &(pPtr->aData[0]),
			pPtr->uLineNum,    pPtr->sFileName
		);
		pPtr = pPtr->pNext;
	}

				/* 
				 *  Report the memory usage statistics 
				 */

	fprintf(stderr,"Maximum memory usage: %ld bytes (%ldk)\n",
		lMaxMemory, (lMaxMemory + 1023L)/1024L	);
}


