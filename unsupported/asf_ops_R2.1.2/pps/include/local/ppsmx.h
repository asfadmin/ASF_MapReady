/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	ppsmx.h
Description:
	Include file for PPS mutex library contains defines and 
stuff for Mutex routines for PPS

Creator:	John Grieggs
==============================================================================*/

#ifndef _PPSMX_
#define _PPSMX_

#pragma ident "@(#)ppsmx.h	1.1  11/21/96"

#include <pthread.h>

struct mx_var {
	pthread_mutex_t m;
	int v;
};
typedef struct mx_var MX_VAR;

void inc_mx_var(MX_VAR *mv);
void dec_mx_var(MX_VAR *mv);
int test_mx_var(MX_VAR *mv);
void set_mx_var(MX_VAR *mv, int v);
int init_mx_var(MX_VAR *mv, int v);
void drop_mx_var(MX_VAR *mv);

#endif /* _PPSMX_ */

/* End of File */
