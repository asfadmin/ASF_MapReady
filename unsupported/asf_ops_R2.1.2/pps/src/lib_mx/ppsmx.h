/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _PPSMX_H
#define _PPSMX_H

#pragma ident "@(#)ppsmx.h	1.1  11/21/96"

/* defines and stuff for Mutex routines for PPS - jtg */

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

#endif /* _PPSMX_H */
