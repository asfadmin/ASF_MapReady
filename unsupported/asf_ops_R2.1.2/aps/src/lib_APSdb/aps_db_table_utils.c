#ifdef COPYRIGHT
Copyright (c)1995, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps_db_table_utils.c

Description:	Contains functions for manipulating the data in
				the aps_db_tables

External Functions Defined:
				aps_max_fw
				aps_max_pfmt
				aps_set_fw_pfmt
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)aps_db_table_utils.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APSdb/SCCS/s.aps_db_table_utils.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <values.h>

#include "dapps_defs.h"
#include "db_sybint.h"
#include "aps_db_table.h"


/*==============================================================================
Function:       aps_max_fw

Description:    Determines the maximum field width for the table[column]
				object.  This is the number of characters needed to
				represent in a string the longest value of the object.

Parameters:     table	- table number from aps_db_tablenums.h
				column	- column number from the db_xxx.h file, where xxx is
						  the table name

Returns:        the maximum field width

Creator:        Teresa McKillop

Creation Date:  12/11/96

Notes:		
==============================================================================*/
int
aps_max_fw( int table, int column )
{
	char	*pfmt;
	char	*fmt_type;		/* eg, "s", "d", "ld", "hd" */
	int		fieldWidth;
	int		tmpint;
	char	*tmpstr;

	pfmt		= APS_PFMT( table, column );
	fmt_type	= pfmt + 1;	/* the string AFTER the percent sign */

	switch (*(tmpstr = fmt_type + strlen( fmt_type ) - 1))
	{
	case 's':	/* string */
		/* APS_SIZE includes the string terminator */
		fieldWidth	= APS_SIZE( table, column ) - 1;
		break;
	case 'c':	/* character */
		fieldWidth	= APS_SIZE( table, column );
		break;
	case 'd':
	case 'i':	
	case 'u':	/* decimal integer */
		/* see if there is any modifier */
		switch (*(tmpstr-1))
		{
		case 'h':
			tmpint = MAXSHORT;
			break;
		case 'l':
			tmpint = MAXLONG;
			break;
		default:
			tmpint = MAXINT;
			break;
		}
		if (*tmpstr == 'u')	/* unsigned */
		{
			tmpint = (tmpint + 1) * 2 - 1;
		}
		/* now calculate the field width to hold this value */
		for (fieldWidth = 0 ; tmpint > 0 ; fieldWidth++)
			tmpint /= 10;
		break;
	default:
		fieldWidth = 1;
		break;
	}

	return (fieldWidth) ;
}


/*==============================================================================
Function:       aps_max_pfmt

Description:    for the table[column] object, gets a print format item
				(i.e., "%__") with a minimum field width equal to the max
				length of the table[column] object.  And if left-adjust
				flag is TRUE, also includes the left-adjustment flag
				in the print format item.

				This allows nice columnar printing, so columns are
				always the same size, row by row.

Parameters:     table	- table number from aps_db_tablenums.h
				column	- column number from the db_xxx.h file, where xxx is
						  the table name
				leftAdj	- TRUE if want the left-adjust flag in the pfmt
						  (i.e., "%-n__");
						  FALSE if don't want the adjust flag (i.e., "%n__")

Returns:        the pfmt to use in the format string for a [sf]printf stmt
				in a malloc'ed storage area.

Creator:        Teresa McKillop

Creation Date:  01/04/96

Notes:			the max length of the table[column] object is NOT calculated
				for the following printf conversion types (instead the min
				field width is just set to one and printf does the rest):

					f, e, E, g, G, o, x, X, c, p, n

Warnings:		the caller should call free() on the returned string.
==============================================================================*/
char *
aps_max_pfmt( int table, int column, int leftAdjust )
{
	char	*maxPfmt;
	int		maxPfmt_size;
	char	*pfmt;
	char	*fmt_type;		/* eg, "s", "d", "ld", "hd" */
	int		fieldWidth;

	pfmt		= APS_PFMT( table, column );
	fmt_type	= pfmt + 1;	/* the string AFTER the percent sign */

	fieldWidth = aps_max_fw( table, column ) ;

	maxPfmt_size = strlen( pfmt ) + fieldWidth + ((leftAdjust == TRUE) ? 1 : 0);

	maxPfmt = (char *) malloc( maxPfmt_size + 1 );
	(void) sprintf( maxPfmt, "%%%s%d%s",
		(leftAdjust == TRUE) ? "-" : "",
		fieldWidth,
		fmt_type );

	return (maxPfmt);
}



/*==============================================================================
Function:       aps_set_fw_pfmt

Description:    for the table[column] object, sets the fieldWidth into the
				print format item (i.e., "%<fw>__").  And if left-adjust
				flag is TRUE, also includes the left-adjustment flag in the
				print format item.

				This allows nice columnar printing, so columns are
				always the same size, row by row.

Parameters:     table		- table number from aps_db_tablenums.h
				column		- column number from the db_xxx.h file, where xxx is
							  the table name
				fieldWidth	- the desired min. field width in the pfmt
				leftAdj		- TRUE if want the left-adjust flag in the pfmt
							  (i.e., "%-n__");
							  FALSE if don't want the adjust flag (i.e., "%n__")

Returns:        the pfmt to use in the format string for a [sf]printf stmt.

Creator:        Teresa McKillop

Creation Date:  01/04/96

Notes:			

Warnings:		the caller should call free() on the returned string.
==============================================================================*/
char *
aps_set_fw_pfmt( int table, int column, int fieldWidth, int leftAdjust )
{
	char	*Pfmt;
	int		Pfmt_size;
	char	*fmt_type;		/* eg, "s", "d", "ld", "hd" */

	/* point to the string AFTER the percent sign */
	fmt_type	= APS_PFMT( table, column ) + 1;

	Pfmt_size = strlen( fmt_type ) + 1 + fieldWidth
		+ ((leftAdjust == TRUE) ? 1 : 0);

	Pfmt = (char *) malloc( Pfmt_size + 1 );
	(void) sprintf( Pfmt, "%%%s%d%s",
		(leftAdjust == TRUE) ? "-" : "",
		fieldWidth,
		fmt_type );

	return (Pfmt);
}
