/*-----------------------------------------------------------------------------
 * SCCS header
 * @(#)tree.h	1.2 95/02/03 04:36:38
 *
 *---------------------------------------------------------------------------*/

/*=============================================================================
 *
 *  Object Description Language (ODL) Parser Include File
 *  Alaska SAR Facility (ASF) Project.
 *
 *  Copyright (C) Jet Propulsion Laboratory.
 *  Written by Tuan N. Truong.
 *
 *============================================================================*/

#ifndef _TREE_H
#define _TREE_H

static char sccsid_tree_h[] =
	"@(#)tree.h	1.2 95/02/03 04:36:38";

#include <time.h>
#include <sys/time.h>

typedef unsigned char   U8;
typedef unsigned short  U16;
typedef unsigned short  U32;

typedef struct Tree
{   struct Tree *next;
    U16 unit;			/* Offset to pointer to unit from Tree header */
    U8  sval;			/* True => still in ASCII string format */
    U8  type;
    union {
        char  *sval;
        int    ival;
        double dval;
        struct timeval tval;
        struct {
            struct Tree *low;
            struct Tree *upp;
        } rval;
        struct {
            struct Tree *val;
            U32    cnt;
        } name;
        struct {
            struct Tree *val;
            U32	   cnt;
        } seq;
    } u;
} Tree;

#define NameOf(t)		((char*)((Tree *)(t)+1))
#define UnitOf(t)		(*(char**)((char*)(t) + ((Tree *)(t))->unit))

typedef struct Trace            /* Structure to support ODLNextVal function */
{   struct Trace *next;
    Tree  *tree;                /* Associated tree structure */
    char   format[256];         /* Name of object, attribute last accessed */
} Trace;

typedef struct ODL
{   Trace *last;                /* List of most recently accessed nodes */
    Tree  *tree;                /* Point to actual tree structure */
} *ODL;

#define NEST_LEVEL_MAX          32

typedef struct Context          /* Current context of yylex */
{   Trace *last;		/* List of most recently accessed values */
    Tree  *tree;		/* Output parsed tree -- same offset as next */
    char  *str;                 /* Remaining string to be parsed */
    size_t len;                 /* Length of remaining string */
    Tree  *val;                 /* Last value encountered */
    U32    lineno;              /* Error line number */
    U32    top;                 /* Next level */
    U32    region_top;          /* Current nesting level */
    void  *region[NEST_LEVEL_MAX];
    U32    cnt[NEST_LEVEL_MAX]; /* Count of items in sequence being parsed */
    Tree  *seq[NEST_LEVEL_MAX]; /* First item in sequence being parsed */
} Context;

#define YYCONTEXT_DECL          Context *yycontext
#define YYCONTEXT               yycontext
#define YYSTYPE                 Tree *

#endif /* !_TREE_H */
