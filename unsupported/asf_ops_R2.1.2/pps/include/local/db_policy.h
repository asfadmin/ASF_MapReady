/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#ifndef _DB_POLICY_H
#define _DB_POLICY_H

#pragma ident "@(#)db_policy.h	1.1  11/21/96"


#define POLICY_JOB_TYPE			0
#define POLICY_JOB_PRIORITY		1
#define POLICY_AUTO_FLAG		2

#define NUM_POLICY_COLS     	3

#define CAST_POLICY_JOB_TYPE		(char *)
#define CAST_POLICY_JOB_PRIORITY	(char *)
#define CAST_POLICY_AUTO_FLAG		(char *)

#endif /* _DB_POLICY_H */
