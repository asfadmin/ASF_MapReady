/*=============================================================================
 |  @(#)Timer.h	1.4 98/02/10 10:29:50
 |
 |  Timing Module
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#ifndef _RDS_TIMER_H_
#define _RDS_TIMER_H_

#include <sys/time.h>

static const char sccsid_Timer_h[] =
        "@(#)Timer.h	1.4 98/02/10 10:29:50";

extern struct timeval
    sca_timer, sca_time,
    ppr_timer, ppr_time,
    sum_timer, sum_time,
    wrt_timer, wrt_time;

#ifdef	DEBUG
#define start_timer(t0,td)				\
{							\
    if (td != 0) 					\
	(td)->tv_sec = (td)->tv_usec = 0; 		\
    gettimeofday(t0);					\
}
#else
#define start_timer(t0,td)	gettimeofday(t0)
#endif

#define stop_timer(t0,td)                               \
{                                                       \
    struct timeval t1;                                  \
    gettimeofday(&t1);                                  \
    (td)->tv_sec += t1.tv_sec  - (t0)->tv_sec;          \
    if (t1.tv_usec >= (t0)->tv_usec)                    \
        (td)->tv_usec += t1.tv_usec - (t0)->tv_usec;    \
    else {                                              \
        (td)->tv_usec += t1.tv_usec - (t0)->tv_usec + 1000000;  \
        (td)->tv_sec  -= 1;                             \
    }                                                   \
    if ((td)->tv_usec >= 1000000) {			\
	(td)->tv_usec -= 1000000;			\
	(td)->tv_sec  += 1;				\
    }							\
}
#endif /*!_RDS_TIMER_H_*/
