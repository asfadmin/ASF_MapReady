#ifndef APS2HC_H
#define APS2HC_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	aps2hc.h
Description:	APS client program includes and definitions
Creator:	Tieh Ku
Notes:		
==============================================================================*/
#pragma ident	"@(#)aps2hc.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.aps2hc.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <dce/dce_error.h>
#include "downlink_ingest.h" 
#include "hst_status.h" 
#include "aps_log_msg.h"
#include "aps_defs.h"

#define RESUME 1
#define ABORT  0

#define HOSTNAME_COUNT  2
#define FILE_COUNT      1 
#define APS_TIME_LENGTH 21 

#define APS_NAME_LENGTH 256 
#define HOST_NAME_LEN   20

typedef struct inputs
{
   handle_t		handle;
   process_id_t		source;
   long			count;
   process_addr_t	data_file_path[APS_NAME_LENGTH];
} CLIENT_INPUTS;

#endif	/* APS2HC_H */
