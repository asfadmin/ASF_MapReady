/*============================================================================*
 |  @(#)Parser.h	1.2 96/02/23 18:59:18
 |
 |  Object Description Language (ODL) Parser Specification.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef	_ODL_PARSER_H_
#define	_ODL_PARSER_H_

#include <string.h>
#include "yylex.h"
#include "Obj.h"

static	char sccsid_Parser_h[] =
	"@(#)Parser.h	1.2 96/02/23 18:59:18";

typedef struct {
    yylex_t	state;
    Obj_t*	tree;
    size_t	len;
    char*	buf;
    char	err[256];

} ODLparse_t;

#define YYSTYPE         Obj_t*

#endif	/*!_ODL_PARSER_H_ */
