/*
 * @(#)asf_syslog.h	1.1  12/19/96
 *
 * ASF-specific definitions for syslog facility definitions
 *
 * Corresponds to syslog LOG_LOCAL0 through LOG_LOCAL7 facilities
 *
 * Example:
 *     openlog("ASF_SYSTEM", LOG_PID|LOG_CONS|LOG_NDELAY, LOG_ASFSYSTEM);
 *     syslog(LOG_ALERT, "%m");
 */

#ifndef _ASF_SYSLOG_H
#define _ASF_SYSLOG_H
static char sccsid_asf_syslog_h[] =
	"@(#)asf_syslog.h	1.4 95/06/28 17:55:43";

#include <syslog.h>

#define LOG_FAIF	LOG_LOCAL0	/* reserved for FAIF */
#define LOG_APS		LOG_LOCAL1	/* reserved for APS */
#define LOG_PPS		LOG_LOCAL2	/* reserved for PPS */
#define LOG_CP		LOG_LOCAL3	/* reserved for CP */
#define LOG_SPS		LOG_LOCAL4	/* reserved for other SPS subsystems */
#define LOG_IMSDADS	LOG_LOCAL5	/* reserved for IMSDADS */
#define LOG_ASFSYSTEM	LOG_LOCAL6	/* reserved for ASFSYSTEM */
#define LOG_OTHER	LOG_LOCAL7	/* reserved for other ASF uses */

#endif /* !_ASF_SYSLOG_H */
