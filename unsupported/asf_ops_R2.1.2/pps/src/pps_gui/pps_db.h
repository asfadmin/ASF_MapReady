/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _PPS_DB_H_INCLUDED
#define _PPS_DB_H_INCLUDED

/* stuff for my Open Client interface library - jtg */

#pragma ident	"@(#)pps_db.h	1.1    11/21/96"

#include <ctpublic.h>

#include "pps_util.h"

#define MAX_PPS_DB_ITEMS 20

struct pps_db_query_item_dcl {
	CS_DATAFMT datafmt;
	CS_SMALLINT indicator;
	void *val;
};

struct pps_db_exec_dcl {
	struct pps_db_query_item_dcl item[MAX_PPS_DB_ITEMS];
        int num_items;
        void (*callback)(void);
};

CS_RETCODE db_connect(CS_CONNECTION **connection);
 
CS_RETCODE db_disconnect(CS_CONNECTION *connection);
 

int db_exec (CS_CONNECTION          **connection, 
	     char	            *cmd_string,
	     struct pps_db_exec_dcl *db_exec_dcl);

void pps_db_bind_int(struct pps_db_exec_dcl *db_query, 
		     int *p_int);

void pps_db_bind_char(struct pps_db_exec_dcl *db_query, 
                      char *p_char,
      		      int maxchars);

void pps_db_bind_date(struct pps_db_exec_dcl *db_query, 
                      CS_DATETIME *p_date);

void pps_db_bind_float(struct pps_db_exec_dcl *db_query,
                       float *p_float);


#endif /*_PPS_DB_H_INCLUDED*/
