static char *sccs = "@(#)ims_DLtoDTKAux.c	1.3  07/24/97";
/***************************************************************
**
** File:        ims_DLtoDTKAux.c
**
** Function:    Perform all auxillary processing using a pipe with the
**              FTS network server for processing downlink to datatake
**              messages.  The odl file is parsed, and put into the two
**              tables downlink_entry and datatake_entry.
**
** Author:      David Pass
**
** Date:        3/12/97
**
** Copyright (C) 1996, California Institute of Technology.  U.S.
** Government Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <syslog.h>

#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>

#include <ims_job_control.h>
#include <ims_DLtoDTKAux.h>


#define  MAX_LINE       129
#define  MAX_LEN_KEYS   31      /* max length for any keywords with
                                values  */
#define  MAX_NUM_VALS   50      /* max no. of values for any keyword */


typedef struct USERSPEC
{
    char *username;
    char *password;
    char *server;
    char *program;
    char *database;
} USERSPEC;


/*
** External Functions
*/
extern char *ims_extractFileName (char *);
extern int ims_forwardMsgQueue (IMS_MSG_STRUCT *);

/*
**  these structures define the information for char keywords
**      for checking purposes: for length and valid values
*/
typedef  struct  valids__t *pnt_valids_t;
typedef  struct  valids__t {
    char  valids[MAX_NUM_VALS][MAX_LEN_KEYS+1];/* valid valiues
        if any for this keyword  */
}   valids_t;

typedef  struct  key_data__t *pnt_key_data_t;
typedef  struct  key_data__t {
    char   name[31];/* name of keyword  */
    short  length; /* max. length of char string  */
    short  num_valids; /* no. of valid strings  */
    pnt_valids_t pnt_valids;
}   key_data_t;


/*
** Local Functions
*/
static int checkRetStatus ( IMS_MSG_STRUCT * );
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int setTransState (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *);
static int getAuxLock (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static IMS_QI_DESC_OBJ *openConnection ( IMS_MSG_STRUCT *, char * );
static  int  read_odl( IMS_MSG_STRUCT *, IMS_KEYWORD_LIST *,
    pnt_downlink_t, char *, char *  );
static  int save_dtks(  IMS_MSG_STRUCT *, pnt_downlink_t );
static  int check_dl(   IMS_MSG_STRUCT *, pnt_downlink_t,
    short *, char *, char *, char * );
static  int delete_dtks( IMS_MSG_STRUCT *, pnt_downlink_t, short * );
static  int add_dtk(  IMS_MSG_STRUCT *, pnt_downlink_t,
    pnt_datatake_t  );
static  int update_dl( IMS_MSG_STRUCT *, pnt_downlink_t );
static  int add_dl(  IMS_MSG_STRUCT *, pnt_downlink_t );
static  int get_key_data( IMS_MSG_STRUCT *,char *, short *, short *,
    char[2*MAX_NUM_VALS][2*MAX_LEN_KEYS+1] );
static  int get_key_valids(  IMS_MSG_STRUCT  *, short, short *,
    char[2*MAX_NUM_VALS][2*MAX_LEN_KEYS+1] );
static  int set_valids(  IMS_MSG_STRUCT *, char *, pnt_key_data_t * );
static  int check_valids(  IMS_MSG_STRUCT *, char *,
    pnt_key_data_t );


/*
** Global Variables
*/

static char *glb_programName;
IMS_MSG_STRUCT *msgDesc;

static  IMS_QI_DESC_OBJ *qDesc;
USERSPEC userSpec;
static  char cmdBuf[IMS_COL512_LEN];



/* **************************************************************
**
** main ()
**
**************************************************************** */

main (int argc, char *argv[])
{
    char *cmds[20], *replies[20];
    int cmdLen, i;
    int status, id;
    IMS_MSG_STRUCT *msgDesc;
    char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
    char hostName[IMS_HOST_LEN+1];
    struct utsname uname_info;    /* Structure for uname() */
    IMS_KEYWORD_LIST * pnt_keyword_1st;
    pnt_downlink_t  pnt_dl_1st;
    /* ***  note that valid_start and end times are not being used
        currently ******** */
    char  valid_start_time[22]; /* start time of valid time region
            for all these downlinks  */
    char  valid_end_time[22];   /* end time of valid time region
            for all these downlinks  */
    char  str[256];


    /* *****   for checkout:  (void) sleep( 60 );   */

    /*
    ** Initialize the message processor.
    */
    if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
    {
        (void) fprintf (stderr,
            "Memory allocation for IMS_MSG_STRUCT structure failed.");
            exit (IMS_FATAL);
    }

    glb_programName = ims_extractFileName (argv[0]);
    (void) uname (&uname_info);
    (void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
    hostName[IMS_HOST_LEN] = '\0';

    (void) ims_msgSubSystem (msgDesc, "IMS");
    (void) ims_msgProgramName (msgDesc, glb_programName);
    (void) sprintf (banner, "%s::%s", hostName, glb_programName);
    (void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
    (void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
    (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
    (void) ims_msgQueueFlag(msgDesc, IMS_ON);
    (void) ims_msgStderrFlag(msgDesc, IMS_OFF);

    /*
    **
    */
    for (i = 0; i < 20; i++){
        cmds[i] = malloc(IMS_COL255_LEN);
        replies[i] = malloc(IMS_COL255_LEN);
    }

    /*
    ** Initialize commands
    */
    (void) strcpy (cmds[0], "CMD:0:dbUserName: ");
    (void) strcpy (cmds[1], "CMD:0:dbPassword: ");
    (void) strcpy (cmds[2], "CMD:0:server: ");
    (void) strcpy (cmds[3], "CMD:0:dbName: ");
    (void) strcpy (cmds[4], "CMD:0:fileName: ");
    (void) strcpy (cmds[5], "CMD:0:repositoryDir: ");
    /* ******   do not need these
    (void) strcpy (cmds[5], "CMD:0:granuleName: ");
    (void) strcpy (cmds[6], "CMD:0:granuleIdx: ");
    */

    /*
    ** request for required information
    */
    cmdLen = 6;
    for ( i=0; i < cmdLen; i++ ){
#ifdef AUX_DEBUG
        fprintf(stderr, "Sending CMD[%d] to FTS SERVER:%s\n",i,
            cmds[i]);
#endif
        (void) write (1, cmds[i], strlen (cmds[i]) + 1);
        if (read( 0, replies[i], IMS_COL255_LEN+1) == 0)
        {
            (void) ims_msg (msgDesc, IMS_FATAL,
                "Premature ftr termination, abort auxProcess");
            exit (0);
        }
#ifdef AUX_DEBUG
        fprintf(stderr, "Receiving reply from FTS SERVER:%s\n",
            replies[i]);
#endif

        /*
        ** Get rid of new line characters
        */
        replies[i][strlen(replies[i])-1] = '\0';
    }

    userSpec.username = replies[0];
    userSpec.password = replies[1];
    userSpec.server   = replies[2];
    userSpec.database = replies[3];
    userSpec.program = "ims_DLtoDTKAux";

    /*
    **  check if filename has a .D suffix: if so, change it
    **      to .M
    */
    i = ims_strIndex( replies[4], ".D" );
    if(  i  !=  -1  ){
        replies[4][i+1] = 'M';
    }

    /*
    **  first need to read the .odl file in fileName.
    **  parse the ODL file using odl parsing system
    */
    pnt_keyword_1st = (IMS_KEYWORD_LIST * ) malloc( sizeof(
        IMS_KEYWORD_LIST ) );
    strcpy( str, replies[5] );
    i = strlen( str );
    if(  str[i-1]  !=  '/'  )  strcat( str, "/" );
    strcat( str, replies[4] );
    status = ims_parseODLFile( msgDesc, str, NULL,
        &pnt_keyword_1st );
    if(  status <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "Error in parsing ODL file %s", str );
        goto ERROR;
    }

    /*
    **  now fill out the downlink and datatake structures
    */
    pnt_dl_1st = (pnt_downlink_t) malloc( sizeof(
        ims_downlink_t ) );

    status = read_odl( msgDesc, pnt_keyword_1st, pnt_dl_1st,
        valid_start_time, valid_end_time );
    if(  status <  IMS_OK ){
        (void) ims_msg (msgDesc, status,
            "Error in reading ODL file %s", replies[4] );
        goto ERROR;
    }

    /*
    **  now enter the data into the database
    */
    status = save_dtks( msgDesc, pnt_dl_1st );


    ERROR:;
    /*
    ** Close connection with server
    */
    (void) ims_forwardMsgQueue( msgDesc );


    /* free */
    ims_msgStructFree(msgDesc);

    for (i = 0; i < 20; i++){
        free(cmds[i]);
        free(replies[i]);
    }
    exit(0);
}


/***************************************************************
**
**  subr read_odl
**
** This reads the odl linked list and puts the information into
**      the pnt_dl structrues.
**
**************************************************************** */
static  int read_odl(  IMS_MSG_STRUCT *msgDesc,
    IMS_KEYWORD_LIST * pnt_keyword_1st, pnt_downlink_t pnt_dl_1st,
    char * valid_start_time, char * valid_end_time )
{
    IMS_KEYWORD_LIST * pnt_keyword;
    IMS_KEYWORD_LIST * pnt_keyword_save;
    short  n_sensor;
    short  n_platform;
    short  n_rev;
    short  n_seq;
    short  n_time_on;
    short  n_time_off;
    unsigned char  downlink_done;
    pnt_datatake_t  pnt_dtk; /* current datatake  */
    pnt_datatake_t  pnt_dtk_last; /* last datatake if any  */
    pnt_datatake_t  pnt_dtk_temp; /* temp datatake   */
    pnt_downlink_t  pnt_dl_last; /* last downtake if any */
    pnt_downlink_t  pnt_dl_temp;
    pnt_downlink_t  pnt_dl; /* current dl being used  */
    long  cnt_keys;
    short  n_datatakes;
    short  n_dls;
    int  days;
    int  msecs;
    int  compare;
    int  compare2;
    short  size;
    short  num_valids;
    long  n_errors;

    /*
    **  counters for downlinks
    */
    short  n_activity_id;
    short  n_station_id;
    short  n_antenna_id;
    short  n_transmitter_id;
    short  n_fa_schedule_link;
    short  n_time_aos;
    short  n_time_los;
    short  n_number_of_dtk_entry;
    short  n_downlink_status;
    /*
    **  counters for datatakes
    */
    short  n_quicklook_flag;
    short  n_process_auth_flag;
    short  n_mode;
    short  n_frame_mode;
    short  n_site_name;

    /*
    ** the following are for the char keywords: the length of
    **      the keywords, as well as the special values, if
    **      any.  if none, the numerber of values is zero.
    */

    pnt_key_data_t  pnt_keys_platform;
    pnt_key_data_t  pnt_keys_sensor;
    pnt_key_data_t  pnt_keys_activity_id;
    pnt_key_data_t  pnt_keys_station_id;
    pnt_key_data_t  pnt_keys_antenna_id;
    pnt_key_data_t  pnt_keys_transmitter_id;
    pnt_key_data_t  pnt_keys_fa_schedule_link;
    pnt_key_data_t  pnt_keys_time_on;
    pnt_key_data_t  pnt_keys_time_off;
    pnt_key_data_t  pnt_keys_time_aos;
    pnt_key_data_t  pnt_keys_time_los;
    pnt_key_data_t  pnt_keys_quicklook_flag;
    pnt_key_data_t  pnt_keys_process_auth_flag;
    pnt_key_data_t  pnt_keys_mode;
    pnt_key_data_t  pnt_keys_frame_mode;
    pnt_key_data_t  pnt_keys_site_name;
    pnt_key_data_t  pnt_keys_downlink_status;

    pnt_valids_t  pnt_valids;

    int status;
    int  i,j;
    char  str[MAX_LINE], str2[MAX_LINE],str3[MAX_LINE];
    unsigned char  flag;


    /*
    **  initialize the input dl structure
    */
    pnt_dl = pnt_dl_1st;

    pnt_dl->platform[0]         = '\0';
    pnt_dl->sensor[0]           = '\0';
    pnt_dl->revolution = -1;
    pnt_dl->sequence   = -1;
    pnt_dl->time_on[0]          = '\0';
    pnt_dl->time_off[0]         = '\0';
    pnt_dl->activity_id[0]      = '\0';
    pnt_dl->station_id[0]       = '\0';
    pnt_dl->antenna_id[0]       = '\0';
    pnt_dl->transmitter_id[0]   = '\0';
    pnt_dl->fa_schedule_link[0] = '\0';
    pnt_dl->time_aos[0]         = '\0';
    pnt_dl->time_los[0]         = '\0';
    pnt_dl->downlink_status[0]  = '\0';
    pnt_dl->number_of_dtk_entry = -1;
    pnt_dl->pnt_dl_next = NULL;

    /*
    **  for some reason, there is no way to detect the start of
    **      an object or group start.  however, we can detect
    **      the end of an object or group (END_ is inserted
    **      before the object name, and given as a keyword).
    **      therefore, the first time I must store two of
    **      everything.
    */
    downlink_done = IMS_FALSE;
    n_sensor = 0;
    n_platform = 0;
    n_rev = 0;
    n_seq = 0;
    n_time_on = 0;
    n_time_off = 0;
    n_datatakes = 0;

    n_activity_id = 0;
    n_station_id = 0;
    n_antenna_id = 0;
    n_transmitter_id = 0;
    n_fa_schedule_link = 0;
    n_time_aos = 0;
    n_time_los = 0;
    n_number_of_dtk_entry = 0;
    n_downlink_status = 0;

    n_quicklook_flag = 0;
    n_process_auth_flag = 0;
    n_mode = 0;
    n_frame_mode = 0;
    n_site_name = 0;

    n_dls = 1;
    cnt_keys = 0;
    n_errors = 0;
    pnt_dtk = (pnt_datatake_t)  malloc( sizeof( ims_datatake_t ) );
    pnt_dtk->pnt_next = NULL;
    pnt_dtk->platform[0]             = '\0';
    pnt_dtk->sensor[0]               = '\0';
    pnt_dtk->revolution = -1;
    pnt_dtk->sequence = -1;
    pnt_dtk->dt_sensor[0]            = '\0';
    pnt_dtk->dt_revolution = -1;
    pnt_dtk->dt_sequence = -1;
    pnt_dtk->dt_platform[0]          = '\0';
    pnt_dtk->time_on[0]              = '\0';
    pnt_dtk->time_off[0]             = '\0';
    pnt_dtk->site_name[0]            = '\0';
    pnt_dtk->mode[0]                 = '\0';
    pnt_dtk->quicklook_flag[0]       = '\0';
    pnt_dtk->process_auth_flag[0]    = '\0';
    pnt_dtk->frame_mode[0]           = '\0';
    pnt_dl->pnt_dtk_1st = pnt_dtk;

    n_errors = 0;

    /*
    **  open the connection
    */
    qDesc = (IMS_QI_DESC_OBJ *) NULL;
    qDesc = openConnection( msgDesc, userSpec.program );
    if(  qDesc  ==  (IMS_QI_DESC_OBJ *) NULL  ){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "Could not open connection." );
        return( status );
    }

    i = set_valids( msgDesc, "PLATFORM", &pnt_keys_platform );
    n_errors += i;
    i = set_valids( msgDesc, "SENSOR", &pnt_keys_sensor );
    n_errors += i;
    i = set_valids( msgDesc, "ACTIVITY_ID", &pnt_keys_activity_id );
    n_errors += i;
    i = set_valids( msgDesc, "STATION_ID", &pnt_keys_station_id );
    n_errors += i;
    i = set_valids( msgDesc, "ANTENNA_ID", &pnt_keys_antenna_id );
    n_errors += i;
    i = set_valids( msgDesc, "TRANSMITTER_ID",
        &pnt_keys_transmitter_id);
    n_errors += i;
    i = set_valids( msgDesc, "FA_SCHEDULE_LINK",
        &pnt_keys_fa_schedule_link );
    n_errors += i;
    i = set_valids( msgDesc, "TIME_ON", &pnt_keys_time_on );
    n_errors += i;
    i = set_valids( msgDesc, "TIME_OFF", &pnt_keys_time_off );
    n_errors += i;
    i = set_valids( msgDesc, "TIME_AOS", &pnt_keys_time_aos );
    n_errors += i;
    i = set_valids( msgDesc, "TIME_LOS", &pnt_keys_time_los );
    n_errors += i;
    i = set_valids( msgDesc, "QUICKLOOK_FLAG",
        &pnt_keys_quicklook_flag );
    n_errors += i;
    i = set_valids( msgDesc, "PROCESS_AUTH_FLAG",
        &pnt_keys_process_auth_flag );
    n_errors += i;
    i = set_valids( msgDesc, "MODE", &pnt_keys_mode );
    n_errors += i;
    i = set_valids( msgDesc, "FRAME_MODE", &pnt_keys_frame_mode );
    n_errors += i;
    i = set_valids( msgDesc, "SITE_NAME", &pnt_keys_site_name );
    n_errors += i;
    i = set_valids( msgDesc, "DOWNLINK_STATUS",
        &pnt_keys_downlink_status );
    n_errors += i;
    pnt_dtk_last = NULL;
    pnt_dl_temp = NULL;

    if(  n_errors  >  0  )  return( IMS_ERROR );

    pnt_keyword = pnt_keyword_1st;

    while(  pnt_keyword  !=  NULL ){
        cnt_keys++;
        (void) strcpy( str, pnt_keyword->keyword );
        i = strlen( str );
        (void) strcpy( str2, pnt_keyword->value_string );
        j = strlen( str2 );
        if(  strcmp( str, "END_COMMON_HEADER" )  ==  0  ||
            strcmp( str, "END_CATALOG_METADATA" )  ==  0  ){
            /*
            **  this is in case the headers has one of
            **      these variables in it
            */
            n_sensor = 0;
            n_platform = 0;
            n_rev = 0;
            n_seq = 0;
            n_time_on = 0;
            n_time_off = 0;

            n_activity_id = 0;
            n_station_id = 0;
            n_antenna_id = 0;
            n_transmitter_id = 0;
            n_fa_schedule_link = 0;
            n_time_aos = 0;
            n_time_los = 0;
            n_downlink_status = 0;
            n_number_of_dtk_entry = 0;

            n_quicklook_flag = 0;
            n_process_auth_flag = 0;
            n_mode = 0;
            n_frame_mode = 0;
            n_site_name = 0;
        }
        else  if(  strcmp( str, "END_DTK_ENTRY" )  ==  0  ){
            /* finished collecting entries for a datatake:
                check if 1st  */
            if(  !downlink_done ){
                downlink_done = IMS_TRUE;
                if(  pnt_dl->platform[0]  ==  '\0' ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "No data for downlink PLATFORM." );
                    return( IMS_ERROR );
                }
                status = ims_numericDateDiff2( msgDesc, pnt_dl->time_on,
                    pnt_dl->time_off, &days, &msecs, &compare );
                if(  compare  <=  0  ){
                    (void) ims_msg( msgDesc, IMS_WARNING,
                        "Downlink TIME_ON,OFF not increasing: TIME_ON ="
                        "'%s', TIME_OFF = '%s' for dl %d.",
                        pnt_dtk->time_on, pnt_dtk->time_off, n_dls );
                }

                /*
                ** make sure all the entries are there
                */
                if(  n_platform  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink PLATFORM not input for downlink %d.",
                        n_dls );
                    n_errors++;
                }
                if(  n_sensor  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink SENSOR not input for downlink %d.",
                        n_dls );
                    n_errors++;
                }
                if(  n_rev  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                      "Downlink REVOLUTION not input for downlink %d.",
                        n_dls );
                    n_errors++;
                }
                if(  n_seq  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink SEQUENCE not input for downlink %d.",
                        n_dls );
                    n_errors++;
                }
                if(  n_station_id  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink STATION_ID not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_antenna_id  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink ANTENNA_ID not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_transmitter_id  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink TRANSMITTER_ID not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_fa_schedule_link  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink FA_SCHEDULE_LINK not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_time_on  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink TIME_OD not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_time_off  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink TIME_OFF not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_time_aos  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink TIME_AOS not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_time_los  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink ACTIVITY_ID not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_downlink_status  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink DOWNLINK_STATUS not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                if(  n_number_of_dtk_entry  ==  0  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink NUMBER_OF_DTK_ENTRY not input for "
                        "downlink %d.", n_dls );
                    n_errors++;
                }
                n_activity_id = 0;
                n_station_id = 0;
                n_antenna_id = 0;
                n_transmitter_id = 0;
                n_fa_schedule_link = 0;
                n_time_aos = 0;
                n_time_los = 0;
                n_downlink_status = 0;
                n_number_of_dtk_entry = 0;
            }
            else  pnt_dtk_last->pnt_next = pnt_dtk;

            /*
            ** make sure all the entries are there
            */
            n_datatakes++;
            if(  n_sensor  <=  1  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake SENSOR not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_rev  <=  1  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake REVOLUTION not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_seq  <=  1  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake SEQUENCE not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_time_on  <=  1  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake TIME_ON not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_time_off  <=  1  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake TIME_OFF not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_quicklook_flag  ==  0  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake QUICKLOOK_FLAG not input for downlink"
                    " %d, datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_process_auth_flag  ==  0  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake PROCESS_AUTH_FLAG not input for "
                    "downlink %d, datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_mode  ==  0  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake MODE not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_frame_mode  ==  0  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake FRAME_MODE not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }
            if(  n_site_name  ==  0  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Datatake SITE_NAME not input for downlink %d, "
                    "datatake %d.", n_dls, n_datatakes );
                n_errors++;
            }

            pnt_dtk_last = pnt_dtk;
            strcpy( pnt_dtk->platform, pnt_dl->platform );
            strcpy( pnt_dtk->sensor,   pnt_dl->sensor );
            pnt_dtk->revolution = pnt_dl->revolution;
            pnt_dtk->sequence = pnt_dl->sequence;
            if(  pnt_dtk->dt_platform[0]  ==  '\0' )
                strcpy( pnt_dtk->dt_platform, pnt_dl->platform );

            /*
            **  allocate the next datatake
            */
            pnt_dtk = (pnt_datatake_t)  malloc( sizeof(
                ims_datatake_t ) );
            pnt_dtk_last->pnt_next = pnt_dtk;
            pnt_dtk->pnt_next = NULL;
            pnt_dtk->platform[0]             = '\0';
            pnt_dtk->sensor[0]               = '\0';
            pnt_dtk->revolution = -1;
            pnt_dtk->sequence = -1;
            pnt_dtk->dt_sensor[0]            = '\0';
            pnt_dtk->dt_revolution = -1;
            pnt_dtk->dt_sequence = -1;
            pnt_dtk->dt_platform[0]          = '\0';
            pnt_dtk->time_on[0]              = '\0';
            pnt_dtk->time_off[0]             = '\0';
            pnt_dtk->site_name[0]            = '\0';
            pnt_dtk->mode[0]                 = '\0';
            pnt_dtk->quicklook_flag[0]       = '\0';
            pnt_dtk->process_auth_flag[0]    = '\0';
            pnt_dtk->frame_mode[0]           = '\0';

            /*
            **  check the time_on,off values: the datatake values
            **      should be inside the dl values
            */
            status = ims_numericDateDiff2(  msgDesc,
                pnt_dtk_last->time_on, pnt_dtk_last->time_off,
                &days, &msecs, &compare );
            if(  compare  <=  0  ){
                (void) ims_msg( msgDesc, IMS_WARNING,
                    "Datatake time on,off not increasing: TIME_ON ="
                    "'%s', TIME_OFF = '%s' for dl %d, dtk %d",
                    pnt_dtk->time_on, pnt_dtk->time_off, n_dls,
                    n_datatakes );
            }

            if(  strcmp( pnt_dl->activity_id, "RLT" )  ==  0  &&
                strcmp( pnt_dl->platform, "A1" )  !=  0  ){
                /*
                **  if real time and not Adeos, then the times
                **      should be inclusive
                */
                status = ims_numericDateDiff2(  msgDesc,
                    pnt_dl->time_on, pnt_dtk_last->time_off, &days,
                    &msecs, &compare );
                status = ims_numericDateDiff2(  msgDesc,
                    pnt_dl->time_off, pnt_dtk_last->time_on, &days,
                    &msecs, &compare2 );
                if(  compare  <  0  ||  compare2  >  0  ){
                    (void) ims_msg( msgDesc, IMS_WARNING,
                        "Datatake time on,off not included in dl time "
                        "values for downlink %d, datatake %d",
                        n_dls, n_datatakes );
                }
            }
            else  if( strcmp( pnt_dl->activity_id, "DMP" )  ==  0  &&
                strcmp( pnt_dl->platform, "A1" )  !=  0  ){
                /*
                **  if a dump and not Adeos, then the dtk times
                **      should be lt dl times
                */
                status = ims_numericDateDiff2(  msgDesc,
                    pnt_dl->time_off, pnt_dtk_last->time_on, &days,
                    &msecs, &compare );
                if(  compare  >=  0   ){
                    (void) ims_msg( msgDesc, IMS_WARNING,
                        "Datatake time on less than dl time off for "
                        "a dump for downlink %d, datatake %d",
                        n_dls, n_datatakes );
                }
            }

            n_quicklook_flag = 0;
            n_process_auth_flag = 0;
            n_mode = 0;
            n_frame_mode = 0;
            n_site_name = 0;
        }
        else  if(  strcmp( str, "END_DL_TO_DTKS_ENTRY" )  ==  0  ){
            if(  n_datatakes  !=  pnt_dl->number_of_dtk_entry ){
                (void) ims_msg( msgDesc, IMS_WARNING,
                    " No. of datatakes = %ld does not match input "
                    "number value = %d", n_datatakes,
                    pnt_dl->number_of_dtk_entry );
            }
            if(  !downlink_done  ){
                /*
                **  no datatakes were input: set it up.  not used
                **      later.
                */
                pnt_dl->pnt_dtk_1st = NULL;
            }
            else   pnt_dtk_last->pnt_next = NULL;
            n_dls++;
            n_datatakes = 0;
            /*
            **  end of dl definition - get ready for next
            */
            pnt_dl_temp = pnt_dl;
            n_sensor = 0;
            n_platform = 0;
            n_rev = 0;
            n_seq = 0;
            n_time_on = 0;
            n_time_off = 0;

            pnt_dl = (pnt_downlink_t) malloc( sizeof(
                ims_downlink_t ));
            pnt_dl_temp->pnt_dl_next = pnt_dl;

            pnt_dl->platform[0]         = '\0';
            pnt_dl->sensor[0]           = '\0';
            pnt_dl->revolution = -1;
            pnt_dl->sequence   = -1;
            pnt_dl->time_on[0]          = '\0';
            pnt_dl->time_off[0]         = '\0';
            pnt_dl->activity_id[0]      = '\0';
            pnt_dl->station_id[0]       = '\0';
            pnt_dl->antenna_id[0]       = '\0';
            pnt_dl->transmitter_id[0]   = '\0';
            pnt_dl->fa_schedule_link[0] = '\0';
            pnt_dl->time_aos[0]         = '\0';
            pnt_dl->time_los[0]         = '\0';
            pnt_dl->downlink_status[0]  = '\0';
            pnt_dl->number_of_dtk_entry = -1;
            pnt_dl->pnt_dtk_1st = pnt_dtk;
            pnt_dl->pnt_dl_next = NULL;
            downlink_done = IMS_FALSE;
        }
        /*
        **  note that the following names are in datatake
        **      and downlink names
        */
        else  if(  strcmp( str, "PLATFORM" )  ==   0 ){
            n_platform++;
            if(  j  >  pnt_keys_platform->length  ){
                if(  !downlink_done  &&  n_platform  ==  1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink PLATFORM value %s is too "
                        "long for downlink %d.",
                        str2, n_dls );
                }
                else{
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Datatake PLATFORM value %s is too "
                        "long for downlink %d, datatake %d.",
                        str2, n_dls, n_datatakes+1 );
                }
                str2[pnt_keys_platform->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_platform );
            n_errors += i;
            if(  !downlink_done  &&  n_platform  ==  1  ){
                (void) strcpy( pnt_dl->platform, str2 );
            }
            else{
                (void) strcpy( pnt_dtk->dt_platform, str2 );
            }
        }
        else  if(  strcmp( str, "SENSOR" )  ==   0 ){
            n_sensor++;
            if(  j  >  pnt_keys_sensor->length  ){
                if(  !downlink_done  &&  n_sensor  ==  1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink SENSOR value %s is too "
                        "long for downlink %d.",
                        str2, n_dls );
                }
                else{
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Datatake SENSOR value %s is too "
                        "long for downlink %d, datatake %d.",
                        str2, n_dls, n_datatakes+1 );
                }
                str2[1] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_sensor );
            n_errors += i;
            if( !downlink_done  &&  n_sensor  ==  1  ){
                (void) strcpy( pnt_dl->sensor, str2 );
            }
            else{
                (void) strcpy( pnt_dtk->dt_sensor, str2 );
            }
        }
        else  if(  strcmp( str, "REVOLUTION" )  ==   0 ){
            n_rev++;
            if( pnt_keyword->value_integer  <  0  ){
                if(  !downlink_done  &&  n_rev  ==  1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink REVOLUTION value %ld is "
                        "negative for downlink %d.",
                        pnt_keyword->value_integer, n_dls );
                }
                else{
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Datatake REVOLUTION value %s is "
                        "negative for downlink %d, datatake %d.",
                        pnt_keyword->value_integer, n_dls,
                        n_datatakes+1 );
                }
                n_errors++;
            }
            if(  !downlink_done  &&  n_rev  ==  1  ){
                pnt_dl->revolution = pnt_keyword->value_integer;
            }
            else{
                pnt_dtk->dt_revolution = pnt_keyword->value_integer;
            }
        }
        else  if(  strcmp( str, "SEQUENCE" )  ==   0 ){
            n_seq++;
            if( pnt_keyword->value_integer  <  0  ){
                if(  !downlink_done  &&  n_seq  ==  1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink SEQUENCE value %ld is "
                        "negative for downlink %d.",
                        pnt_keyword->value_integer, n_dls );
                }
                else{
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Datatake SEQUENCE value %s is "
                        "negative for downlink %d, datatake %d.",
                        pnt_keyword->value_integer, n_dls,
                        n_datatakes+1 );
                }
                n_errors++;
            }
            if(  !downlink_done  &&  n_seq  ==  1  ){
                pnt_dl->sequence = pnt_keyword->value_integer;
            }
            else{
                pnt_dtk->dt_sequence = pnt_keyword->value_integer;
            }
        }
        else  if(  strcmp( str, "TIME_ON" )  ==   0 ){
            n_time_on++;
            if(  j  >  pnt_keys_time_on->length ){
                if(  !downlink_done  &&  n_time_on  ==  1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink TIME_ON value %s is too "
                        "long for downlink %d.",
                        str2, n_dls );
                }
                else{
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Datatake TIME_ON value %s is too "
                        "long for downlink %d, datatake %d.",
                        str2, n_dls, n_datatakes+1 );
                }
                str2[pnt_keys_time_on->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_time_on );
            n_errors += i;
            if(  !downlink_done  &&  n_time_on  ==  1  ){
                (void) strcpy( pnt_dl->time_on, str2 );
            }
            else{
                (void) strcpy( pnt_dtk->time_on, str2 );
            }
        }
        else  if(  strcmp( str, "TIME_OFF" )  ==   0 ){
            n_time_off++;
            if(  j  >  pnt_keys_time_off->length ){
                if(  !downlink_done  &&  n_time_off  ==  1  ){
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Downlink TIME_OFF value %s is too "
                        "long for downlink %d.",
                        str2, n_dls );
                }
                else{
                    (void) ims_msg( msgDesc, IMS_ERROR,
                        "Datatake TIME_OFF value %s is too "
                        "long for downlink %d, datatake %d.",
                        str2, n_dls, n_datatakes+1 );
                }
                str2[pnt_keys_time_off->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_time_off );
            n_errors += i;
            if(  !downlink_done  &&  n_time_off  ==  1  ){
                (void) strcpy( pnt_dl->time_off, str2 );
            }
            else{
                (void) strcpy( pnt_dtk->time_off, str2 );
            }
        }
        /*
        **  note that the following names are in only one of
        **      the downlink or datatake names
        */
        else  if(  strcmp( str, "ACTIVITY_ID" )  ==   0 ){
            n_activity_id++;
            if(  j  >  pnt_keys_activity_id->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink ACTIVITY_ID value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_activity_id->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_activity_id );
            n_errors += i;
            (void) strcpy( pnt_dl->activity_id, str2 );
        }
        else  if(  strcmp( str, "STATION_ID" )  ==   0 ){
            n_station_id++;
            if(  j  >  pnt_keys_station_id->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink STATION_ID value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_station_id->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_station_id );
            n_errors += i;
            (void) strcpy( pnt_dl->station_id, str2 );
        }
        else  if(  strcmp( str, "ANTENNA_ID" )  ==   0 ){
            n_antenna_id++;
            if(  j  >  pnt_keys_antenna_id->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink ANTENNA_ID value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_antenna_id->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_antenna_id );
            n_errors += i;
            (void) strcpy( pnt_dl->antenna_id, str2 );
        }
        else  if(  strcmp( str, "TRANSMITTER_ID" )  ==   0 ){
            n_transmitter_id++;
            if(  j  >  pnt_keys_transmitter_id->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink TRANSMITTER_ID value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_transmitter_id->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_transmitter_id );
            n_errors += i;
            (void) strcpy( pnt_dl->transmitter_id, str2 );
        }
        else  if(  strcmp( str, "FA_SCHEDULE_LINK" )  ==   0 ){
            n_fa_schedule_link++;
            if(  j  >  pnt_keys_fa_schedule_link->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink FA_SCHEDULE_LINK value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_fa_schedule_link->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_fa_schedule_link);
            n_errors += i;
            (void) strcpy( pnt_dl->fa_schedule_link, str2 );
        }
        else  if(  strcmp( str, "TIME_AOS" )  ==   0 ){
            n_time_aos++;
            if(  j  >  pnt_keys_time_aos->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink TIME_AOS value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_time_aos->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_time_aos );
            n_errors += i;
            (void) strcpy( pnt_dl->time_aos, str2 );
        }
        else  if(  strcmp( str, "TIME_LOS" )  ==   0 ){
            n_time_los++;
            if(  j  >  pnt_keys_time_los->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink TIME_LOS value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_time_los->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_time_los );
            n_errors += i;
            (void) strcpy( pnt_dl->time_los, str2 );
        }
        else  if(  strcmp( str, "NUMBER_OF_DTK_ENTRY" )  ==   0 ){
            n_number_of_dtk_entry++;
            if(  pnt_keyword->value_integer  <=  0  ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink NUMBER_OF_DTK_ENTRY value %s is "
                    "negative or zero for downlink %d.",
                    str2, n_dls );
                n_errors++;
            }
            pnt_dl->number_of_dtk_entry = pnt_keyword->value_integer;
        }
        else  if(  strcmp( str, "DOWNLINK_STATUS" )  ==   0 ){
            n_downlink_status++;
            if(  j  >  pnt_keys_downlink_status->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink DOWNLINK_STATUS value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_downlink_status->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_downlink_status );
            n_errors += i;
            (void) strcpy( pnt_dl->downlink_status, str2 );
        }
        else  if(  strcmp( str, "SITE_NAME" )  ==   0 ){
            n_site_name++;
            if(  j  >  pnt_keys_site_name->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink SITE_NAME value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_site_name->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_site_name );
            n_errors += i;
            (void) strcpy( pnt_dtk->site_name, str2 );
        }
        else  if(  strcmp( str, "MODE" )  ==   0 ){
            n_mode++;
            if(  j  >  pnt_keys_mode->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink MODE value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_mode->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_mode );
            n_errors += i;
            (void) strcpy( pnt_dtk->mode, str2 );
        }
        else  if(  strcmp( str, "QUICKLOOK_FLAG" )  ==   0 ){
            n_quicklook_flag++;
            if(  j  >  pnt_keys_quicklook_flag->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink QUICKLOOK_FLAG value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_quicklook_flag->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_quicklook_flag );
            n_errors += i;
            (void) strcpy( pnt_dtk->quicklook_flag, str2 );
        }
        else  if(  strcmp( str, "PROCESS_AUTH_FLAG" )  ==   0 ){
            n_process_auth_flag++;
            if(  j  >  pnt_keys_process_auth_flag->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink PROCESS_AUTH_FLAG value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_process_auth_flag->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2,
                pnt_keys_process_auth_flag );
            n_errors += i;
            (void) strcpy( pnt_dtk->process_auth_flag, str2 );
        }
        else  if(  strcmp( str, "FRAME_MODE" )  ==   0 ){
            n_frame_mode++;
            if(  j  >  pnt_keys_frame_mode->length ){
                (void) ims_msg( msgDesc, IMS_ERROR,
                    "Downlink FRAME_MODE value %s is too "
                    "long for downlink %d.",
                    str2, n_dls );
                str2[pnt_keys_frame_mode->length] = '\0';
                n_errors++;
            }
            i = check_valids( msgDesc, str2, pnt_keys_frame_mode );
            n_errors += i;
            (void) strcpy( pnt_dtk->frame_mode, str2 );
        }
        else  if(  strcmp( str, "VALID_START_TIME" )  ==   0 ){
            if(  j  >  21  )  str2[21] = '\0';
            (void) strcpy( valid_start_time, str2 );
        }
        else  if(  strcmp( str, "VALID_END_TIME" )  ==   0 ){
            if(  j  >  21  )  str2[21] = '\0';
            (void) strcpy( valid_end_time, str2 );
        }
        pnt_keyword = pnt_keyword->next;
    }
    /*
    **  last areas are not used: set last used to null
    */
    if(  pnt_dtk_last  !=  NULL  ){
        pnt_dtk_last->pnt_next = NULL;
    }
    if(  pnt_dl_temp  !=  NULL  )  pnt_dl_temp->pnt_dl_next = NULL;
    free( pnt_dtk );
    free( pnt_dl );

    if(  n_errors  ==  0  )  return( IMS_OK );
    return( IMS_ERROR );
}   /*  read_odl  */


/***************************************************************
**
**  subr set_valids
**
** This sets up the key_data structure for all the character
**      keywords.  the arrays are allocated and setup.
**
**************************************************************** */
static  int set_valids(  IMS_MSG_STRUCT *msgDesc, char * name,
    pnt_key_data_t * pnt_key )
{
    int n_errors;
    pnt_key_data_t  pnt_keys;
    short  size;
    short  num_valids;
    char  temp_values[2*MAX_NUM_VALS][2*MAX_LEN_KEYS+1];  /* this is
            the temp storage for the valid keyword values  */
    pnt_valids_t  pnt_valids;
    int  status;
    int  i;


    n_errors = 0;
    pnt_keys = (pnt_key_data_t) malloc( sizeof( key_data_t ));
    status = get_key_data( msgDesc, name, &size, &num_valids,
        temp_values );
    if(  status  <  IMS_OK ){
        n_errors++;
        (void) ims_msg( msgDesc, IMS_ERROR,
            "get_key_data failed for %s.", name );
    }
    if(  size  >  MAX_LEN_KEYS ){
        n_errors++;
        (void) ims_msg( msgDesc, IMS_ERROR,
            "MAX_LEN_KEYS must be at least %d for %s.", size, name );
    }
    if(  num_valids  >  MAX_NUM_VALS ){
        n_errors++;
        (void) ims_msg( msgDesc, IMS_ERROR,
            "MAX_NUM_VALS must be at least %d for %s.", size, name );
    }
    strcpy( pnt_keys->name, name );
    pnt_keys->length = size;
    pnt_keys->num_valids = num_valids;
    if(  num_valids  >  0  ){
        pnt_valids = (pnt_valids_t) malloc( sizeof( valids_t ) );
        pnt_keys->pnt_valids = pnt_valids;
        for( i=0 ; i  <  num_valids ; i++ ){
            strcpy( pnt_valids->valids[i], temp_values[i] );
        }
        for( i=num_valids ; i  <  MAX_NUM_VALS ; i++ ){
            pnt_valids->valids[i][0] = '\0';
        }
    }
    else  pnt_keys->pnt_valids = NULL;
    *pnt_key = pnt_keys;
    return( n_errors );
}   /*  set_valids  */


/***************************************************************
**
**  subr check_valids
**
** This checks the valids against the input string.
**
**************************************************************** */
static  int check_valids(  IMS_MSG_STRUCT *msgDesc, char * input,
    pnt_key_data_t pnt_key )
{
    int n_errors;
    int  i;
    short j;
    pnt_valids_t  pnt_valids;


    if(  pnt_key->num_valids  ==  0  )  return( 0 );
    j = 0;
    pnt_valids = pnt_key->pnt_valids;
    for( i=0 ; i  <  pnt_key->num_valids ; i++ ){
        if(  strcmp( input, pnt_valids->valids[i] )  ==  0  )  j = 1;
    }
    if(  j  ==  0  ){
        /*
        **  input string not found
        */
        (void)  ims_msg( msgDesc, IMS_ERROR,
            "Valid string not found for %s: %s.", pnt_key->name,
            input );
        return( 1 );
    }
    return( 0 );
}   /*  check_valids  */


/***************************************************************
**
**  subr save_dtks
**
** This does the database stuff.  It first checks to see if the
**      downlink is in the database.  if so, the old datatakes
**      are deleted and the downlink is updated.  otherwise, the
**      downlink is entered and the datatakes entered.
**
**************************************************************** */
static  int save_dtks(  IMS_MSG_STRUCT *msgDesc,
    pnt_downlink_t pnt_dl_1st )
{
    unsigned char  downlink_there;
    pnt_datatake_t  pnt_dtk; /* current datatake  */
    pnt_datatake_t  pnt_dtk_last; /* last datatake if any  */
    pnt_datatake_t  pnt_dtk_temp; /* temp datatake   */
    short  num_rows; /*  number of rows for dt  */
    char  TIME_ON[22];
    char  TIME_OFF[22];
    char  DOWNLINK_STATUS[11];
    pnt_downlink_t  pnt_dl; /* current downlink  */
    pnt_downlink_t  pnt_dl_temp; /* temp downlink structure */
    short  num_dls; /*  no. of downlinks for this odl file */
    short  num_dtks_deleted; /* no. datatakes deleted, global */
    short  num_dtks_added; /*  no. datatakes added, global  */
    short  num_old_dtks; /*  no. of dtks in db  */

    int status;
    int  i,j;
    char  str[MAX_LINE], str2[MAX_LINE],str3[MAX_LINE];
    unsigned char  flag;


    if(  pnt_dl_1st->platform[0]  ==  '\0'  ){
        /*
        **  no downlinks entered
        */
        (void) ims_msg( msgDesc, IMS_INFO,
            "No downlinks in this ODL file." );
        return( IMS_OK );
    }

    /*
    ** Begin the update transaction.
    */
    if ((status = setTransState (msgDesc, qDesc, "begin")) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not begin the update transaction.");
        return( IMS_ERROR );
    }

    /*
    ** Lock the table until update is complete.
    */
    if ((status = getAuxLock (msgDesc, qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status, "Could not get Aux lock.");
        (void) setTransState (msgDesc, qDesc, "rollback");
        (void) ims_qiResetDesc (qDesc);
        return (status);
    }

    /*
    **  go through all the dl s.  need to check if there is a
    **      downlink in the system.  if so, need to use update.
    **      in all cases, need to delete the associated dlks.
    */
    num_dls = 0;
    num_dtks_deleted = 0;
    num_dtks_added = 0;
    pnt_dl = pnt_dl_1st;
    while( pnt_dl  !=  (pnt_downlink_t)NULL ){
        num_dls++;
        if(  pnt_dl->pnt_dtk_1st  ==  NULL  ){
            /*
            **  datatakes = 0 not allowed: do not save
            */
            (void)  ims_msg( msgDesc, IMS_WARNING,
                "Downlink %d has no datatakes: not saved"
                " or replaced.", num_dls );
        }
        else{
            /*
            **  check if downlink in db
            */
            TIME_ON[0] = '\0';
            TIME_OFF[0] = '\0';
            status = check_dl( msgDesc, pnt_dl, &num_rows, TIME_ON,
                TIME_OFF, DOWNLINK_STATUS );
            if(  status  <  IMS_OK  ){
                (void) setTransState (msgDesc, qDesc, "rollback");
                (void) ims_qiResetDesc (qDesc);
                return( status );
            }

            flag = IMS_TRUE;
            if(  num_rows  ==  1  ){
                /*
                **  if downlink_status is acquired, then do not update
                **      or delete the dl and dtks.
                */
                if(  strcmp( DOWNLINK_STATUS, "ACQUIRED" )  ==  0  ){
                    flag = IMS_FALSE;
                    (void)  ims_msg( msgDesc, IMS_WARNING,
                        "Downlink %d has status ACQUIRED in DB and not "
                        "updated: %s  %ld   %d.", num_dls,
                        pnt_dl->platform, pnt_dl->revolution,
                        pnt_dl->sequence );
                }
                else{
                    /*
                    **  need to delete the old datatakes and update
                    **      the dl.
                    */
                    status = delete_dtks( msgDesc, pnt_dl,
                        &num_old_dtks );
                    if(  status  <  IMS_OK  ){
                        (void)  ims_msg( msgDesc, IMS_ERROR,
                            "Error in deleting dtks from dl %d where "
                            "platform = %s, sensor = %s, revolution "
                            "= %ld, sequence = %d", num_dls,
                            pnt_dl->platform, pnt_dl->sensor,
                            pnt_dl->revolution, pnt_dl->sequence );
                        (void) setTransState (msgDesc, qDesc,
                            "rollback");
                        (void) ims_qiResetDesc (qDesc);
                        return( status );
                    }
                    num_dtks_deleted += num_old_dtks;

                    /*
                    ** now update the dl
                    */
                    status = update_dl( msgDesc, pnt_dl );
                    if(  status  <  IMS_OK  ){
                        (void)  ims_msg( msgDesc, IMS_ERROR,
                            "Error in updateing downlink %d where "
                            "platform = %s, sensor = %s, revolution "
                            "= %ld, sequence = %d", num_dls,
                            pnt_dl->platform, pnt_dl->sensor,
                            pnt_dl->revolution, pnt_dl->sequence );
                        (void) setTransState (msgDesc, qDesc,
                            "rollback");
                        (void) ims_qiResetDesc (qDesc);
                        return( status );
                    }
                }
            }
            else{
                /*
                **  need to add the downlink
                */
                status = add_dl( msgDesc, pnt_dl );
                if(  status  <  IMS_OK  ){
                    (void)  ims_msg( msgDesc, IMS_ERROR,
                        "Error in adding downlink %d where platform"
                        " = %s, sensor = %s, revolution = %ld, sequence"
                        " = %d", num_dls, pnt_dl->platform,
                        pnt_dl->sensor, pnt_dl->revolution,
                        pnt_dl->sequence );
                    (void) setTransState (msgDesc, qDesc, "rollback");
                    (void) ims_qiResetDesc (qDesc);
                    return( status );
                }
            }

            /*
            **  now put in the datatakes
            **      note that I am assuming that if the dl is not
            **      there, there are no associated dtks.
            */
            if(  flag ){
                pnt_dtk = pnt_dl->pnt_dtk_1st;
                while( pnt_dtk  !=  (pnt_datatake_t) NULL ){
                    status = add_dtk( msgDesc, pnt_dl, pnt_dtk );
                    if(  status  <  IMS_OK  ){
                        (void)  ims_msg( msgDesc, IMS_ERROR,
                            "Error in adding datatakes for downlink %d "
                            " where  platform = %s, sensor = %s, "
                            "revolution = %ld, sequence = %d", num_dls,
                            pnt_dl->platform, pnt_dl->sensor,
                            pnt_dl->revolution, pnt_dl->sequence );
                        (void) setTransState (msgDesc, qDesc,
                            "rollback");
                        (void) ims_qiResetDesc (qDesc);
                        return( status );
                    }
                    num_dtks_added++;
                    pnt_dtk = pnt_dtk->pnt_next;
                }
            }
        }
        pnt_dl = pnt_dl->pnt_dl_next;
    }

    /*
    ** Commit the update transaction.
    */
    if ((status = setTransState (msgDesc, qDesc, "commit")) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not commit the update transaction.");
        return( status );
    }
    (void) ims_qiResetDesc (qDesc);

    if(  num_dtks_deleted  >  0  ){
        (void) ims_msg( msgDesc, IMS_INFO,
            "Processed %d downlinks, deleting %d datatakes, adding"
            " %d datatakes.", num_dls, num_dtks_deleted,
            num_dtks_added );
    }
    else{
        (void) ims_msg( msgDesc, IMS_INFO,
            "Processed %d downlinks, adding %d datatakes.",
            num_dls, num_dtks_added );
    }
    return( IMS_OK );
}   /*  save_dtks  */


/***************************************************************
**
**  subr get_key_data ()
**
**  This routine gets the key information for the given keyword.
**
**************************************************************** */
static  int get_key_data (
    IMS_MSG_STRUCT  *msgDesc,
    char *      keyword,
    short *     size,
    short *     num_valids,
    char        valids[2*MAX_NUM_VALS][2*MAX_LEN_KEYS+1] )
{
    int status;
    int rowCount;
    short keyword_idx;

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "get_key_data: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select keyword_idx, max_len  from  keyword_policy "
        "  where  keyword = '%s'", keyword );
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "get_key_data: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "get_key_data: Keyword %s not found.", keyword );
        return (IMS_ERROR);
    }
    if (IMS_AFFECTED (qDesc) > 1){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "get_key_data: More than one keyword found for %s.",
            keyword );
        (void) ims_qiResetDesc (qDesc);
        return (IMS_ERROR);
    }

    (void) memcpy ( &keyword_idx, qDesc->valAddr[0],
        qDesc->valLength[0]);

    (void) memcpy ( size, qDesc->valAddr[1], qDesc->valLength[1]);

    /*
    **  now check if there are any valid values
    */
    status = get_key_valids( msgDesc, keyword_idx, num_valids, valids );
    return( status );
}   /*  get_key_data  */


/***************************************************************
**
**  subr get_key_valids ()
**
**  This routine gets the valid names for this keyword if any.
**
**************************************************************** */
static  int get_key_valids(
    IMS_MSG_STRUCT  *msgDesc,
    short  keyword_idx,
    short *     num_valids,
    char        valids[2*MAX_NUM_VALS][2*MAX_LEN_KEYS+1] )
{
    int status;
    int rowCount;
    short  index;


    /*
    ** set up the select
    */
    (void) sprintf (cmdBuf, "select  "
        " value  from keyword_value  where  keyword_idx = %d",
        keyword_idx );

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);
    rowCount = 0;

    while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
    {
        if (status < IMS_OK)
        {
            (void) ims_msg( msgDesc, IMS_ERROR,
                "get_key_valids: Error from db." );
            return( IMS_ERROR );
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */

        if (status == IMS_ENDOFQUERY)
        {
            continue;
        }
        rowCount++;

        /*
        ** Grab data
        */

        index = 0;
        (void) memcpy( valids[rowCount-1],
            qDesc->valAddr[index], qDesc->valLength[index]);
        valids[rowCount-1][qDesc->valLength[index]] = '\0';
        ims_truncStr(  valids[rowCount-1] );
    }

    /*
    ** Check the return status
    */

    *num_valids = rowCount;
    if (checkRetStatus (msgDesc ) < IMS_OK)
    {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "get_key_valids: Error return status from db." );
        return( IMS_ERROR );
    }
    return (IMS_OK);
}   /*  get_key_valids  */


/***************************************************************
**
**  subr check_dl  checks the downlink_entry table to determine if
**      an entry exists.
**
**************************************************************** */
static  int check_dl(
    IMS_MSG_STRUCT *msgDesc,
    pnt_downlink_t  pnt_dl,
    short * num_rows,
    char *  TIME_ON,
    char *  TIME_OFF,
    char *  DOWNLINK_STATUS )
{
    int status;
    short  i_s;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "check_dl: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "select TIME_ON, TIME_OFF, DOWNLINK_STATUS from downlink_entry"
        "  where   PLATFORM  = '%s'  and  SENSOR = '%s'  and  "
        "REVOLUTION = %ld  and  SEQUENCE = %d",
        pnt_dl->platform, pnt_dl->sensor, pnt_dl->revolution,
        pnt_dl->sequence );
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "check_dl: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        *num_rows = 0;
        return (IMS_OK);
    }
    if (IMS_AFFECTED (qDesc) > 1){
        (void) ims_msg (msgDesc, IMS_ERROR,
            "check_dl: More than one downlink returned." );
        (void) ims_qiResetDesc (qDesc);
        *num_rows = IMS_AFFECTED (qDesc);
        return (IMS_ERROR);
    }
    *num_rows = 1;

    (void) memcpy ( TIME_ON, qDesc->valAddr[0], qDesc->valLength[0]);
    TIME_ON[qDesc->valLength[0]] = '\0';
    (void) ims_truncStr ( TIME_ON);

    (void) memcpy ( TIME_OFF, qDesc->valAddr[1], qDesc->valLength[1]);
    TIME_OFF[qDesc->valLength[1]] = '\0';
    (void) ims_truncStr ( TIME_OFF );

    (void) memcpy ( DOWNLINK_STATUS, qDesc->valAddr[2],
        qDesc->valLength[2]);
    DOWNLINK_STATUS[qDesc->valLength[2]] = '\0';
    (void) ims_truncStr ( DOWNLINK_STATUS );

    return( IMS_OK );
}   /*  check_dl  */


/***************************************************************
**
**  subr delete_dtks deletes the datatakes from the downlink.
**      the number of dtks is returned.
**
**************************************************************** */
static  int delete_dtks(
    IMS_MSG_STRUCT *msgDesc,
    pnt_downlink_t  pnt_dl,
    short * num_old_dtks )
{
    int status;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "delete_dtks: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "delete from  datatake_entry  where  "
        " PLATFORM  = '%s'  and  SENSOR = '%s'  and  "
        "REVOLUTION = %ld  and  SEQUENCE = %d",
        pnt_dl->platform, pnt_dl->sensor, pnt_dl->revolution,
        pnt_dl->sequence );
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "check_dl: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    *num_old_dtks = IMS_AFFECTED (qDesc);
    return( IMS_OK );
}   /*  delete_dtks  */


/***************************************************************
**
**  subr add_dtk  addes the datatakes given the pnt_dl and pnt_dtk
**
**************************************************************** */
static  int add_dtk(
    IMS_MSG_STRUCT *msgDesc,
    pnt_downlink_t  pnt_dl,
    pnt_datatake_t  pnt_dtk )
{
    int status;
    short  i_s;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "add_dtks: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "insert into datatake_entry( PLATFORM, SENSOR, REVOLUTION,"
        " SEQUENCE, DT_SENSOR, DT_REVOLUTION, DT_SEQUENCE, "
        "DT_PLATFORM, TIME_ON, TIME_OFF, SITE_NAME, MODE, "
        "QUICKLOOK_FLAG, PROCESS_AUTH_FLAG, FRAME_MODE  )    "
        "values( '%s', '%s', %ld, %d, '%s', %ld, %d, '%s', "
        "'%s', '%s', '%s', '%s', '%s', '%s', '%s' )",
        pnt_dtk->platform, pnt_dtk->sensor, pnt_dtk->revolution,
        pnt_dtk->sequence,
        pnt_dtk->dt_sensor, pnt_dtk->dt_revolution,
        pnt_dtk->dt_sequence, pnt_dtk->dt_platform,
        pnt_dtk->time_on, pnt_dtk->time_off, pnt_dtk->site_name,
        pnt_dtk->mode, pnt_dtk->quicklook_flag,
        pnt_dtk->process_auth_flag, pnt_dtk->frame_mode );
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "add_dtk: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        return (IMS_ERROR);
    }
    return( IMS_OK );
}   /*  add_dtk  */


/***************************************************************
**
**  subr add_dl  addes the downlinks given the pnt_dl
**
**************************************************************** */
static  int add_dl(
    IMS_MSG_STRUCT *msgDesc,
    pnt_downlink_t  pnt_dl )
{
    int status;
    short  i_s;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "add_dls: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "insert into downlink_entry( PLATFORM, SENSOR, REVOLUTION,"
        " SEQUENCE, TIME_ON, TIME_OFF, ACTIVITY_ID, STATION_ID, "
        "ANTENNA_ID, TRANSMITTER_ID, FA_SCHEDULE_LINK, TIME_AOS, "
        "TIME_LOS, NUMBER_OF_DTK_ENTRY, DOWNLINK_STATUS, "
        "received_time )   values( '%s', '%s', %ld, %d, '%s', '%s',"
        " '%s', '%s', '%s', '%s', '%s', '%s', '%s', %ld, '%s', "
        "getdate() )",
        pnt_dl->platform, pnt_dl->sensor, pnt_dl->revolution,
        pnt_dl->sequence,
        pnt_dl->time_on, pnt_dl->time_off, pnt_dl->activity_id,
        pnt_dl->station_id, pnt_dl->antenna_id, pnt_dl->transmitter_id,
        pnt_dl->fa_schedule_link, pnt_dl->time_aos, pnt_dl->time_los,
        pnt_dl->number_of_dtk_entry, pnt_dl->downlink_status );
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "add_dl: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        return (IMS_ERROR);
    }
    return( IMS_OK );
}   /*  add_dl  */


/***************************************************************
**
**  subr update_dl  updates the down link
**
**************************************************************** */
static  int update_dl(
    IMS_MSG_STRUCT *msgDesc,
    pnt_downlink_t  pnt_dl  )
{
    int status;
    short  i_s;


    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "update_dl: Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Assign the command buffer to the query descriptor.
    ** Set the number of rows to be returned.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf,
        "update  downlink_entry  set  TIME_ON = '%s', "
        "TIME_OFF = '%s', ACTIVITY_ID = '%s', STATION_ID = '%s',"
        " ANTENNA_ID = '%s', TRANSMITTER_ID = '%s', "
        "FA_SCHEDULE_LINK = '%s', TIME_AOS = '%s', TIME_LOS = '%s', "
        "NUMBER_OF_DTK_ENTRY = %d, DOWNLINK_STATUS = '%s', "
        "received_time = getdate()   where  PLATFORM = '%s'  and  "
        "SENSOR = '%s'  and  REVOLUTION = %ld  and  SEQUENCE = %d",
        pnt_dl->time_on, pnt_dl->time_off, pnt_dl->activity_id,
        pnt_dl->station_id, pnt_dl->antenna_id, pnt_dl->transmitter_id,
        pnt_dl->fa_schedule_link, pnt_dl->time_aos, pnt_dl->time_los,
        pnt_dl->number_of_dtk_entry, pnt_dl->downlink_status,
        pnt_dl->platform, pnt_dl->sensor, pnt_dl->revolution,
        pnt_dl->sequence );
    /*
    ** Execute the command.
    */
    if ((status = execCmd (msgDesc, qDesc )) < IMS_OK){
        (void) ims_qiResetDesc (qDesc);
        (void) ims_msg (msgDesc, status,
            "update_dl: execCmd had error.");
        return (IMS_ERROR);
    }

    /*
    ** See if we got one row returned.
    */
    if (IMS_AFFECTED (qDesc) < 1){
        return (IMS_ERROR);
    }
    return( IMS_OK );
}   /*  update_dl  */


/***************************************************************
**
** getOrderLock ()
**
** Execute stored procedure get_order_lock
**
**************************************************************** */
static int getAuxLock (
    IMS_MSG_STRUCT      *msgDesc,
    IMS_QI_DESC_OBJ     *qDesc)
{
    /*
    ** Execute stored procedure get_aux_lock
    */
    (void) sprintf (qDesc->cmd, "get_auxiliary_lock");

    if (execCmd (msgDesc, qDesc) < IMS_OK){
        (void) ims_msg (msgDesc, IMS_FATAL,
            "execution of stored procedure get_auxiliary_lock failed.");
        return (IMS_FATAL);
    }
    return (IMS_OK);
}   /*  getAuxLock  */


/***************************************************************
**
**  subr execCmd ()
**
** Execute a query. This function should only be used for commands
** that return one or no rows.
**
**************************************************************** */
static int execCmd (
    IMS_MSG_STRUCT *msgDesc, IMS_QI_DESC_OBJ  *qDesc )
{
    int status;


    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION){
        if(  status  <  IMS_OK  )  return (status);
    }

    /*
    ** Check the stored procedure status returned value.
    */
    if (checkRetStatus (msgDesc) < IMS_OK){
        return (IMS_ERROR);
    }
    return (IMS_OK);
}  /* execCmd */


/****************************************************************
**
**  subr checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
***************************************************************** */
static int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc )
{
    int procReturn;
    int severity;


    /*
    ** Check to see if the Sybase procedure returned a status. If it did
    ** and it is not 0 (the OK value for a return), deal with the error.
    ** Return status of less than -100 correspond to message facility
    ** severity levels modulo 100.
    */
    if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
    {
        if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
        {
            if (procReturn == -103)
            {
                severity = IMS_FATAL;
            }
            else if (procReturn == -102)
            {
                severity = IMS_ERROR;
            }
            else if (procReturn == -101)
            {
                severity = IMS_WARNING;
            }
            else
            {
                severity = IMS_ERROR;
            }
            (void) ims_msg (msgDesc, severity,
                "Sybase procedure '%s' returned a status of %d",
                qDesc->cmd, procReturn);
            return (severity);
        }
    }

    return (IMS_OK);
}  /* checkRetStatus */


/***************************************************************
**
**  subr openConnection ()
**
** Open a database server connection.
**
**************************************************************** */
static IMS_QI_DESC_OBJ *openConnection (
    IMS_MSG_STRUCT *msgDesc, char * program )
{
    int status;


    /*
    ** Since this is the first time to access the catalog, we
    ** need a query descriptor allocated.  If we can't get a
    ** descriptor, return with a bad status ... we can't go on.
    */
    if ((qDesc = ims_qiDescAlloc (msgDesc)) ==
        (IMS_QI_DESC_OBJ *) NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate a query descriptor.");
        return ((IMS_QI_DESC_OBJ *) NULL);
    }

    /*
    ** Setup the descriptor with necessary information about this
    ** process.
    */
    IMS_SETUSER (qDesc, userSpec.username);
    IMS_SETPSWD (qDesc, userSpec.password);
    IMS_SETPROG (qDesc, program);

    if( userSpec.server  !=  NULL ){
        if ((int) strlen (userSpec.server) > 0){
            IMS_SETSERVER (qDesc, userSpec.server);
        }
    }

    if( userSpec.database  !=  NULL ){
        if ((int) strlen ( userSpec.database) > 0){
            IMS_SETDBNAME (qDesc, userSpec.database);
        }
    }

    IMS_SET_VERBOSE (qDesc, 10);

    /*
    ** Login to the catalog database.
    */
    if ((status = ims_qiLogin (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not login to the database server.");
        (void) ims_qiFreeDesc (qDesc);
        return ((IMS_QI_DESC_OBJ *) NULL);
    }

    /*
    ** Associate the message descriptor with the dbproc so
    ** the Sybase error and message handling can be performed.
    */
    IMS_SET_USERDATA (qDesc);

    return (qDesc);
}  /* openConnection */


/***********************************************************
**
** setTransState ()
**
** Set the state of the transaction.
**
************************************************************ */
static int setTransState (
    IMS_MSG_STRUCT *msgDesc,
    IMS_QI_DESC_OBJ *qDesc,
    char *transType)
{
    static char cmdBuf[IMS_COL512_LEN];
    int status;

    /*
    ** Populate the command buffer with the SQL statement.
    */
    (void) sprintf (cmdBuf, "%s transaction", transType);

    /*
    ** Assign the command buffer to the query descriptor.
    */
    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Reset the query descriptor.
    */
    if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not reset the query descriptor.");
        return (status);
    }

    /*
    ** Execute the cammand.
    */
    if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
    {
        return (IMS_ERROR);
    }

    return (IMS_OK);
}   /*  setTransState  */
