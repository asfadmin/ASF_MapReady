/* ppscount.h - generated, do not edit */

#ifndef PPSCOUNT_H
#define PPSCOUNT_H

#define PPS_MSG_OFFSET 1000		/* offset to first message */
#define PPS_START 1000
#define PPS_SHUTDOWN 1001
#define PPS_L1_ORDER 1002
#define PPS_SCAN_ORDER 1003
#define PPS_DUB_ORDER 1004
#define PPS_L1_COMPLETE 1005
#define PPS_SCAN_COMPLETE 1006
#define PPS_DUB_COMPLETE 1007
#define PPS_CANCEL_ORDER 1008
#define PPS_SVEC_AVAIL 1009
#define PPS_MSG_LIMIT 1010		/* max message value + 1 */

#define PPSLOGNAME "ppscount.log"

#ifdef MAIN
struct {
	char *txt;
} ppscount[] = {
	"Server Startups",
	"Server Shutdowns",
	"L1 Orders Recieved",
	"Scan Orders Recieved",
	"Dub Orders Recieved",
	"L1 Orders Completed",
	"Scan Orders Completed",
	"Dub Orders Completed",
	"Cancel Orders Recieved",
	"State Vector Available Msg",
	NULL
};
#endif /* MAIN */

#endif /* PPSCOUNT_H */
