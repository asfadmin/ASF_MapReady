/*==============================================================================
Filename:       ADEOS_msg.h
 
Description:    Contains the data structures used in ADEOS_msg.c
 
Creator:        Phil Yurchuk
 
Notes:
==============================================================================*/
 
#ifndef _ADEOS_MSG_
#define _ADEOS_MSG_

#define ADEOS_MTYPE_SIZE 4 
#define ADEOS_FNAME_SIZE 81
#define ADEOS_FSIZE_SIZE 10
#define ADEOS_COMPID_SIZE 10
#define ADEOS_SPARE_SIZE 2
 
struct msg_node {                     /* contains the fields that are */
   char msg_type[ADEOS_MTYPE_SIZE] ;   /* in the ADEOS message         */
   char filename[ADEOS_FNAME_SIZE] ;
   char file_size[ADEOS_FSIZE_SIZE] ;
   char computerid[ADEOS_COMPID_SIZE] ;
   char spare[ADEOS_SPARE_SIZE] ;
   struct msg_node *next;
};


typedef struct msg_node MESSAGE;
typedef MESSAGE * MESSAGEPTR;

#define ADEOS_SRCHOST_EV   "ADEOS_SRCHOST"
#define ADEOS_SRCDIR_EV    "ADEOS_SRCDIR"
#define ADEOS_RECEPTDIR_EV "ADEOS_RECEPTDIR"
#define ADEOS_TRANSPROTO_EV "ADEOS_TRANSPROTO"
#define ADEOS_MAILSCRIPT_EV "ADEOS_MAIL_SCRIPT"
 
#define ADEOS_OP_USER_EV   "ADEOS_OP_USER"
#define ADEOS_OUTDIR_EV    "ADEOS_OUTDIR"

/* Default values for variables above
*/
#define ADEOS_SRCHOST_DEF   ""
#define ADEOS_SRCDIR_DEF    ""
#define ADEOS_RECEPTDIR_DEF "/local/faif/ADEOS"
#define ADEOS_TRANSPROTO_DEF "ADEOS_ftpget.csh"
#define ADEOS_MAILSCRIPT_DEF "mails.csh"
 
#define ADEOS_OP_USER_DEF   "faif"
#define ADEOS_OUTDIR_DEF    "/adeos/mmofe/nasags/asf/sen"

#define ADEOS_TMPMAIL_DIR   "FTP/mailtmp"
#define ADEOS_MSG_TMPFILE   "mailtmp.msg"
#define ADEOS_RCPT_TMPFILE  "mailtmp.rcpt"

#endif /* _ADEOS_MSG_ */

/* End of File */
