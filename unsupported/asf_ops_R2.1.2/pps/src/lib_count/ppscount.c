/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/* PPS event counting routines */

#include <stdio.h>
#include "ppscount.h"
#include "ppsmx.h"

static FILE *pps_count_fp;
MX_VAR pps_count_mx;

/* call once at top of server */
int init_pps_count(void)
{
	int err;

	/* init a pps mx var */
	err = init_mx_var(&pps_count_mx, 0);
	if (err)
		return 1;

	/* open the log file */
	pps_count_fp = fopen(PPSLOGNAME, "wb");
	if (pps_count_fp == NULL)
	{
		fprintf(stderr, "Can not open %s\n", PPSLOGNAME);
		return 1;
	}

	/* looking good */
	return 0;
}

/* call once in server shutdown */
void fini_pps_count(void)
{
	/* close the log file */
	if (pps_count_fp)
		fclose(pps_count_fp);
	pps_count_fp = NULL;

	/* release the pps mx var */
	drop_mx_var(&pps_count_mx);
}

/* count the occurrence */
void pps_count(int counter)
{
	size_t cnt;

	/* MT protection */
	inc_mx_var(&pps_count_mx);
	while (test_mx_var(&pps_count_mx) > 1)
		sleep(1);

	/* record the event */
	cnt = fwrite(&counter, 1, sizeof(counter), pps_count_fp);
	if (cnt != sizeof(counter))
		fprintf(stderr, "bad length %d written in pps_count\n", cnt);

	/* free the mutex */
	dec_mx_var(&pps_count_mx);
}

