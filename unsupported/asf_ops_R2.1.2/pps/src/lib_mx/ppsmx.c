/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/* Mutex routines for PPS - jtg */

#include <stdio.h>
#include <pthread.h>
#include "ppsmx.h"

static char SccsFileId[] = "@(#)ppsmx.c	1.1  11/21/96";

extern MX_VAR dying, numrpcs;

void inc_mx_var(MX_VAR *mv)
{
	pthread_mutex_lock(&mv->m);
	mv->v++;
	pthread_mutex_unlock(&mv->m);
}

void dec_mx_var(MX_VAR *mv)
{
	pthread_mutex_lock(&mv->m);
	mv->v--;
	pthread_mutex_unlock(&mv->m);
}

int test_mx_var(MX_VAR *mv)
{
	int tmp;

	pthread_mutex_lock(&mv->m);
	tmp = mv->v;
	pthread_mutex_unlock(&mv->m);

	return tmp;
}

void set_mx_var(MX_VAR *mv, int v)
{
	int tmp;

	pthread_mutex_lock(&mv->m);
	mv->v = v;
	pthread_mutex_unlock(&mv->m);
}

int init_mx_var(MX_VAR *mv, int v)
{
	int err;

	err = pthread_mutex_init(&(mv->m), pthread_mutexattr_default);
	if (err)
		return err;

	mv->v = v;
	return 0;
}

void drop_mx_var(MX_VAR *mv)
{
	pthread_mutex_destroy(&(mv->m));
}

