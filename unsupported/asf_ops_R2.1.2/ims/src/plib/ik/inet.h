/*************************************************************************/
/* inet.h                                                                */
/* symbol VAX should already be set to 1 in VMS environment              */
/* VAX  port 1500, 128.183.36.23                                         */
/* SUN  port 1252, 128.183.10.164                                        */
/* GCMD port 5501, 128.183.112.141                                       */
/* set symbols servhostaddr, servtcpport                                 */
/*************************************************************************/
#ifdef VAX
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include.sys]ioctl.h"
/* for use of SYS$QIO to read */
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.vms]inetiodef.h"
#define TIMER 50
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>
#define socket_read    read
#define socket_write   write
#define socket_close   close
#define socket_ioctl   ioctl
#ifdef EOSDIS
#   include "IK_Syslog.h"
#   include <errno.h>
#   define socket_perror(str) IK_vSyslog (LOG_ERR, "GCMD S/W: %s,errno = %d - %s", \
	str, errno, strerror (errno));
	
#else
#   define socket_perror  perror
#endif
#endif

#define BUF_SIZE        4096

/* definitions used by the socket function routines */
/*
#define CLIENT  0
#define SERVER  1
*/
#define C_TIMEWAIT  5   /* affecting codes -- client site */
#define S_TIMEWAIT  20  /* affecting codes -- server site */

/* definitions for types of servers */
#define DBSERVER   1
#define LOGSERVER  2
#define LINKSERVER 3
