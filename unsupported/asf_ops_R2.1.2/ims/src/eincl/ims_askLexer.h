/************************************************************************
**
** FILE: ims_askLexer.h
**
** Creator: H. Sayah
**
** Date: August 4, 1991
**
** Updated:  March 1995, H. Sayah
**           ASF adoption of AMMOS-CDB code.
**
*************************************************************************/

#ifndef _IMS_ASKLEXER_H
#define _IMS_ASKLEXER_H

static char *sccsAskLexer = "@(#)ims_askLexer.h	5.1  16 Mar 1996";

#include <ctype.h>

/*
** Enumerates the possible source of input for adLexer: file or memory
*/
enum	LEX_INPUT_SOURCE	{ FILE_PTR, MEM_PTR };

typedef	char *STRING;

#define	SKIPBLANKS(i) while( isspace(*(i)) &&  (*(i)) ) (i++)

#endif	/* !_IMS_ASKLEXER_H */
