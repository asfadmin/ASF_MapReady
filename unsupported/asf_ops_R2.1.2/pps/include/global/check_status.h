/* check_status.h - part of the arithmetic example from the Sterling book */

/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef    __CHECK_STATUS_H
#define    __CHECK_STATUS_H

#pragma ident "@(#)check_status.h	1.1  11/21/96"

#include <stdio.h>
#include <dce/dce_error.h>
#include <dce/pthread.h>
#include <dce/rpcexc.h>

#define RESUME	0
#define ABORT	1

#define CHECK_STATUS(input_status, comment, action) \
{ \
	if (input_status != rpc_s_ok) { \
		dce_error_inq_text(input_status, error_string, &error_stat); \
		fprintf(stderr, "%s %s\n", comment, error_string); \
		if (action == ABORT) \
			exit(1); \
	} \
}

#define CHECK_STATUS_MSG(input_status, comment, action, msg) \
{ \
        if (input_status != rpc_s_ok) { \
                dce_error_inq_text(input_status, error_string, &error_stat); \
                sprintf(msg, "%s %s\n", comment, error_string); \
                if (action == ABORT) { \
			printf("%s", msg); \
                        exit(1); \
		} \
        } \
}
 
 

static int error_stat;
static unsigned char error_string[dce_c_error_string_len];

void exit();

#endif /*__CHECK_STATUS_H*/
