#ifndef SUBPROCESS_H
#define SUBPROCESS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	subprocess.h
Description:	This file contains declarations for structs which define
			the PROCESS structs used for spawning subprocesses.
Creator:	unknown
Notes:		
==============================================================================*/
#pragma ident	"@(#)subprocess.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.subprocess.h"

#define READ_END	0 
#define WRITE_END	1 
#define READ_ERR	2 

#define SUB_PROC_BUFFER_SIZE 1024

/*
--	special subprocess statuses, interpreted by gui only
--	(doesn't conflict with APS exit statuses (e.g., APS_EXIT_OK) because
--	exit statuses must be non-negative)
--
--	[NOTE: for non-core-dumping signals, the subprocess status is set to
--	the negative signal number]
*/
#define APSGUI_EXIT_COREDUMP	-99999	/* subprocess core dumped */

/******************************************************************************
**  Instance Struct
******************************************************************************/

typedef struct _process_info {
    char*	     creationString;		/* string cmd line.	*/
    char*	     cmd;		/* cmd to be spawned.	*/
    char*	     cmdLine;	/* cmdLine is creationString w/out path in the cmd*/
    int		     childPid;	/* PID of spawned child proc.	*/
    int		     io[3];		/* I/O pipe to spawned proc: stdin, stdout, stderr*/
    XtInputId	 id;		/* stdout data from SubProcess   */
    XtInputId	 wrId;		/* Pause on write to subProcess	*/
    XtInputId	 erId;		/* SubProcess Error message (stderr)	*/
	Widget		 display_window ;
	char		*write_data ;	/* data to write via "wrId" to the subprocess */
	void		(*read_func)() ;	/* function to process "id" data */
	void		*read_parameter ;
	void		(*done_func)() ;	/* func. to call after subprocess is done */
	void		*done_parameter ;
	int 		exit_status ;	/* if caught a signal, is -signal_# */
	FILE		*logfile ;		/* fp of log file for recording "id" data */
	int 		errBufIdx;
	char		*errBuffer;
	char		*target_filename;
	char		*original_filename;
} PROCESS_INFO ;


PROCESS_INFO *create_process( char *newCreationString,
	int *status, int do_log_file,
	char *write_data,
	void (*read_func)(), void *read_param,
	void (*done_func)(), void *done_param ) ;

void destroy_process( PROCESS_INFO *this ) ;

int start_process( PROCESS_INFO *this ) ;

#endif	/* SUBPROCESS_H */
