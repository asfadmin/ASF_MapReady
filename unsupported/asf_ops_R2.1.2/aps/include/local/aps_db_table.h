#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	aps_db_table.h

Description:	
	Declarations and Defines for APS specific Sybase Tables

Creator:	Ron Green
Notes:		
			10/19/95	Teresa McKillop		created APS_SIZE
			01/04/96	Teresa McKillop		added aps_max_pfmt() and
											aps_set_fw_pfmt() prototypes
			12/11/96	Teresa McKillop		added aps_max_fw() prototype
==============================================================================*/
#pragma ident	"@(#)aps_db_table.h	5.2 98/02/06 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.aps_db_table.h"


#ifndef _APS_DB_TABLE_H_
#define _APS_DB_TABLE_H_

#include <db_sybint.h>     /* for DBPROCESS           */
/*
--	Function Prototypes
*/

int db_open_APS_readonly( 
	DBPROCESS **APS_dbproc ) ;

int		aps_max_fw( int table, int column ) ;

char	*aps_max_pfmt(
		int table, int column, int leftAdjust ) ;

char	*aps_set_fw_pfmt(
		int table, int column, int fieldWidth, int leftAdjust ) ;


/* External Declaration for APS Table Names */
extern DB_TABLES aps_table[] ;


/*
--	MACROS for manipulating APS Tables
*/

#define APS_TABLE(table) \
	aps_table[(table)].name

#define APS_CDEFS(table) \
	aps_table[(table)].col_defs
	
#define APS_COL(table, column) \
    aps_table[(table)].col_defs[(column)].name

#define APS_PFMT(table, column) \
    aps_table[(table)].col_defs[(column)].format

#define APS_SIZE(table, column) \
    aps_table[(table)].col_defs[(column)].size


#include <aps_db_tablenums.h>   /* use <> to start search at top of the 
								   include list.  Don't look in this same
								   directory first.  Might want to override 
								   from a directory upstream, instead.    */

#endif  /* _APS_DB_TABLE_H_ */
