#ifndef SEND_STATUS_H
#define SEND_STATUS_H

#pragma ident	"@(#)send_status.h	1.1  11/26/96"

#include "defs.h"

typedef struct ppsJobStatus
{
	int		jobId;
	int		orderId;
	int		itemId;
	char	jobType[MAXLINE];
	char	status[MAXLINE];

	char	platform[MAXLINE];
	char	sensor[MAXLINE];
	char	dataset[MAXLINE];
	char	productId[MAXLINE];
	char	comment[MAXLINE];

} PPSJobStatus;

#endif /* SEND_STATUS_H */
