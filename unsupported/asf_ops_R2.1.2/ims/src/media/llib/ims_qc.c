static char *sccs = "@(#)ims_qc.c	5.3  04/02/97";
/* *******************************************************************
*
*  Name: ims_qc.c
*
*  Module Type: Procedure     Language: C
*
*  Purpose:
*  Main routine to check the quality of distribution media.
*
*  Input Parameters:
*
*   Name           Type        Description
*   msgDesc         IMS_MSG_STRUCT  message structure
*   device_id       short       device no. in db table device_policy.
*                               either a tape or disk.
*   report_type     int         report type:  either 1-Full 2-Brief.
*   path            char        path for file or tape.  if a file,
*                               this must be set.  if a tape, should
*                               start with /dev/, but can be blank.  if
*                               blank, need to look at device_id to
*                               determine path.
*
*  Output Paramters:
*
*  Name             Type       Description
*   status           short      flag for sucessful run
*
*  Modification History:
*
*  Date:   30 Oct 1989 02:36:00    Revision:   1.0    Author:   CAROL
*  Date:   18 Jul 1990 10:25:26    Revision:   2.0    Author:   DBMAN
*  Date:   06 Aug 1990 10:35:10    Revision:   2.1    Author:   CAROL
*  Date:   03 Aug 1995             Revision:   r1b    Author:   DSPASS
*       modify for rib, convert to unix and sybase.
*
******************************************************************* */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_timeConv.h>
#include <ims_media.h>
#include <unistd.h>


void    RptTime( char * );
int  BeginMedia(  IMS_MSG_STRUCT *, char *, char *,
    char [][], FILE **, FILE  **, char * );
static int  openConnection( IMS_MSG_STRUCT *, MEDIA_USER_SPEC * );
int  CheckFile( IMS_MSG_STRUCT *, char *, int *, int *, char *,
    char *, int *, char [][], char [][], FILE *, FILE *, FILE *,
    char *, int );
int  EndMedia( IMS_MSG_STRUCT *, char *, char *, char [][],
    FILE *, FILE *, FILE *, char * );
static  int  checkRetStatus( IMS_MSG_STRUCT *);


static  IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;
static  char cmdBuf[255];

/*******************************************************************
**
** ims_qc ()
**
******************************************************************** */

int ims_qc (
    IMS_MSG_STRUCT *msgDesc,
    char *mediaUserSpec,
    int report_type,
    char *target_path,
    char *media_id,
    int archive_check )
{

static  char    title_buf[3][76]=
{"                             Alaska SAR Facility                Pa\
ge       ",
 "                           Distributed Product Report             \
         ",
 "                                                                  \
         "};
static  char    media_buf[9][76]=
{"Order Id:                                                         \
         ",
 "User Name:                                                        \
         ",
 "Account Id:                                                       \
         ",
 "Media Id:                                                         \
         ",
 "Media Type:                                                       \
         ",
 "Media Format:                                                     \
         ",
 "Media Sequence:                                                   \
         ",
 "Media Creation Date:                                              \
         ",
 "Media Deformat Routine:                                           \
         "};
char    now[21];
char    str[256],str2[256];
char    *device={"      "};
char    media_typ[5];
char    report_type_str[6];
int numfile, numrec, tot_media, tot_files;
int    status, istat, mstat;
int eov;
FILE    *rptfile, *tmp1file, *tmp2file;
char  path[256];
MEDIA_USER_SPEC *userSpec;
char  line_id[4];
char  first_line_id[4];
char  dir_name[128];
long i,j,k;
char  temp_qc_name[65]; /* temp name for qc file */
char * temp_ary;
char  *time_stamp;


/*
** lint: pointer cast may result in improper alignment
** ???? Review this.
*/
userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

/*
** Open the database server connection.
*/
if( (status = openConnection (msgDesc, userSpec)) < IMS_OK){
    return (status);
}
mstat = IMS_OK;
istat = IMS_OK;
(void) strcpy( path, target_path );
if(  ims_strIndex( path, "/dev/" )  ==  -1 )
    (void) strcpy( media_typ, "Disk" );
else  (void) strcpy( media_typ, "Tape" );


/*
** Determine report type.
*/
if (report_type == FULL_REPORT){
    (void) strcpy (report_type_str, "Full");
}
else if (report_type == BRIEF_REPORT){
    (void) strcpy (report_type_str, "Brief");
}
else{
    (void) ims_msg (msgDesc, IMS_ERROR,
        "The report type of '%d' does not map to a known type.",
        report_type);
    return (IMS_ERROR);
}

/* open report file */
temp_ary = ims_timeStamp();
(void) strcpy( temp_qc_name, "/tmp/qc_" );
(void) strcat( temp_qc_name, temp_ary );
(void) free( temp_ary );
strcat( temp_qc_name, ".txt" );
rptfile = fopen ( temp_qc_name, "w");
if( rptfile == NULL)
{
    (void) ims_msg (msgDesc, IMS_ERROR,
        "Error opening report file %s for write.\n",
        temp_qc_name );
    return(  IMS_ERROR ) ;
}
(void) chmod (temp_qc_name, 0664);

/* complete title section of report */
RptTime (now);
(void) strncpy (title_buf[0],now,11);
(void) strncpy (title_buf[1],now+12,8);
(void) strncpy (title_buf[2]+31, report_type_str,
    strlen(report_type_str));
(void) strcpy (title_buf[2]+(31+strlen(report_type_str)),
    " Format");
/*
**  media_id put in header if it is here.  may not be input.
**      in that case, the database value for media_id is
**      put here.  if the query fails, no media_id is used.
*/
media_buf[3][11] = ' ';
media_buf[3][12] = ' ';
if(  media_id  !=  NULL  ){
    j = strlen( media_id );
    if(  j  >  1  ){
        (void) strncpy (media_buf[3]+11, media_id, j );
    }
}

/* build media section of report */
tot_media = 1;
/*  This is no. of media in this report: taken out of header.
    (void) sprintf (media_buf[5]+65,"%d",tot_media);
 */
(void) strncpy (media_buf[4]+13,media_typ,strlen(media_typ));

eov = FALSE;

(void) ims_msg( msgDesc, IMS_INFO,
    "Quality check of device %s at %s started: report is %s.",
    media_typ, path, report_type_str );

time_stamp = ims_timeStamp();
istat = BeginMedia( msgDesc, path, report_type_str,
    media_buf, &tmp1file, &tmp2file, time_stamp);
if( istat <  IMS_OK  ){
    return(  istat ) ;
}

/* loop through files on media */
numfile = 0;
tot_files = 9999;
while  (eov == FALSE && istat >=  IMS_OK &&
    numfile < tot_files){
    numrec  = 0;
    numfile++;
    istat = CheckFile( msgDesc,path,&numfile,&numrec,report_type_str,
        media_typ,&tot_files, title_buf, media_buf,
        rptfile,tmp1file,tmp2file, line_id, archive_check );

    if(  istat  <  IMS_OK ){
        goto error;
    }
    if( istat <  IMS_OK  &&  numrec == 0 ) eov = TRUE;

    /* set the first line_id: for name of file  */
    if(  numfile  ==  2  ){
        (void) strcpy( first_line_id, line_id );
    }
}

mstat = EndMedia( msgDesc, report_type_str,media_typ,title_buf,
                            rptfile,tmp1file,tmp2file, time_stamp);
free( time_stamp );
if(  istat  <  IMS_OK ||  mstat  <  IMS_OK  ){
    (void) ims_msg( msgDesc, IMS_INFO,
        "Quality check of path %s aborted.", path);
}
else{
    (void) ims_msg( msgDesc, IMS_INFO,
        "Quality check of path %s completed.",path );
}

exit:;
if(  mstat  <  istat  )  mstat = istat;
/*  close files if necessary  */
(void) fclose (rptfile);
rptfile = NULL;

/* now rename the report file to order,first line id + .QC  */
/*  first get the staging area  */
i = QC_RPT;
str[0] = '\0';
status = get_stage_areas2( msgDesc, i, str2, str );
if(  status  <  IMS_OK )  return( status );

(void) strncpy( str2, &media_buf[0][10], 16 );
str2[16] = '\0';
(void) ims_truncStr( str2 );
(void) ims_truncStr( str );
j = strlen( str );
if(  j  >  0  &&  str[j-1]  !=  '/' ){
    (void) strcat( str, "/" );
    j++;
}
strcpy( dir_name, str );
dir_name[j-1] = '\0'; /* take out last / of directory name  */
strcat( str, str2 );
first_line_id[2] = '\0';
(void) ims_truncStr( first_line_id );
i = strlen( first_line_id );
/* force line_id to 3 characters  */
if(  i  ==  1 ){
    (void) strcpy( str2, "00" );
    (void) strcat( str2, first_line_id );
    (void) strcpy( first_line_id, str2 );
}
else  if(  i  ==  2  ){
    (void) strcpy( str2, "0" );
    (void) strcat( str2, first_line_id );
    (void) strcpy( first_line_id, str2 );
}

(void) strcat( str, first_line_id );
(void) strcat( str, ".QC" );
/*
**  having problems renameing the QC file: check the directory,
**      and if a file already exists.  if it exists, delete
**      it if possible.
*/
status = access( dir_name, W_OK );
if(  status  !=  0 ){ /* do not have write access to dirctory */
    (void) ims_msg( msgDesc, IMS_INFO,
        "User cannot write to QC repository %s.\n",
        dir_name );
    strcpy( str, str+j );
    status = rename( temp_qc_name, str );
}
else{
    /*
    **  now check if file already there
    */
    i = 0;
    try_again:;
    status = access( str, F_OK );
    if(  status  ==  0 ){ /* file already there: change name */
        /*
        **  change the 6th char: xnn.QC  (x) to letter
        */
        k = strlen( str );
        i++;
        str[k-6] = 'A'+i-1; /* starts at A, goes through alphabet */
        if(  i  <  26  )  goto try_again;
        str[k-6] = '0'; /* oh well  */
        status = remove( str );
        if(  status  <  0  ){ /* cannot delete file */
            (void) ims_msg( msgDesc, IMS_INFO,
                "Cannot delete QC file already in repository %s.\n",
                str );
            strcpy( str, str+j );
            status = rename( temp_qc_name, str );
        }
        else{
            strcpy( str2, "/bin/mv " );
            strcat( str2, temp_qc_name );
            strcat( str2, "  " );
            strcat( str2, str );
            status = system( str2 );
            if(  status  <  0  ){ /* rename failed: put locally */
                strcpy( str, str+j );
                status = rename( temp_qc_name, str );
            }
        }
    }
    else{
        strcpy( str2, "/bin/mv " );
        strcat( str2, temp_qc_name );
        strcat( str2, "  " );
        strcat( str2, str );
        status = system( str2 );
        if(  status  <  0  ){ /* rename failed: put locally */
            strcpy( str, str+j );
            strcpy( str2, "/bin/mv " );
            strcat( str2, temp_qc_name );
            strcat( str2, "  " );
            strcat( str2, str );
            status = system( str2 );
            if(  status  <  0  ){ /* this failed also: put in tmp */
                strcpy( str2, "/tmp/" );
                strcat( str2, str );
                strcpy( str, str2 );
                strcpy( str2, "/bin/mv " );
                strcat( str2, temp_qc_name );
                strcat( str2, "  " );
                strcat( str2, str );
                status = system( str2 );
                if(  status  <  0  ){ /* this failed also: give up */
                    strcpy( str, temp_qc_name );
                }
            }
        }
    }
}
(void) ims_msg( msgDesc, IMS_INFO,
    "%s report from %s finished and saved in %s.",
    report_type_str, path, str );

return(istat);
error:;
/*
**  remove temporary files
*/
status = remove( temp_qc_name );
strcpy( str, "/tmp/qt1_" ); /* note: qt => qualtmp   */
strcat( str, time_stamp );
strcat( str, ".txt" );
status = remove( str );
strcpy( str, "/tmp/qt2_" );
strcat( str, time_stamp );
strcat( str, ".txt" );
status = remove( str );
strcpy( str, "/tmp/qt3_" );
strcat( str, time_stamp );
strcat( str, ".txt" );
status = remove( str );
return(istat);
}   /*  ims_qc   */


/* ***************************************************************
**
**  subr openConnection () sets up the data base for reads.
**
***************************************************************** */

static int openConnection (
    IMS_MSG_STRUCT *msgDesc,
    MEDIA_USER_SPEC *userSpec)
{
    int status;

    /*
    ** Allocate a query descriptor.
    */
    if( (qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL){
        (void) ims_msg (msgDesc, IMS_FATAL,
            "Could not allocate a query descriptor.");
        return (IMS_FATAL);
    }

    /*
    ** Setup the descriptor with necessary information about this
    ** process.
    */
    IMS_SETUSER (qDesc, userSpec->username);
    IMS_SETPSWD (qDesc, userSpec->password);
    IMS_SETPROG (qDesc, userSpec->program);

    if( userSpec->server != (char *) NULL){
        IMS_SETSERVER (qDesc, userSpec->server);
    }

    if( userSpec->database != (char *) NULL){
        IMS_SETDBNAME (qDesc, userSpec->database);
    }

    IMS_SET_VERBOSE (qDesc, 10);

    IMS_SETCMD (qDesc, cmdBuf);

    /*
    ** Login to the catalog database.
    */
    if( (status = ims_qiLogin (qDesc)) < IMS_OK)
    {
        (void) ims_msg (msgDesc, status,
            "Could not login to the database server.");
        (void) ims_qiFreeDesc (qDesc);
        return (status);
    }

    /*
    ** Associate the message descriptor with the dbproc so
    ** the Sybase error and message handling can be performed.
    */
    IMS_SET_USERDATA (qDesc);

    return (IMS_OK);
}   /*  openConnection   */


/* **************************************************************
**
**  subr getQualInfo () - get user name and account id given
**      order_id and item_id.
**  used by the qual_chk program.  uses stored procedure:
**
**    select o.account_id, u.first_name, u.initial_name,
**        u.last_name, d.path, t.description, t2.description,
**        i.media_id
**    from order_queue o, user_profile u, dataset_path_policy d,
**        order_item i, items t, items t2
**    where o.order_id = @order_id
**    and o.user_id = u.user_id
**    and i.order_id = @order_id
**    and i.item_id = @item_id
**    and i.p_dataset_idx = d.dataset_idx
**    and ((i.p_granule_idx >= d.start_granule_idx) or
**        (d.start_granule_idx = null))
**    and ((i.p_granule_idx <= d.end_granule_idx) or
**        (d.end_granule_idx = null))
**    and i.media_type  = t.instance
**    and t.type = "media_type"
**    and i.media_fmt_type  = t2.instance
**    and t2.type = "media_fmt_type"
**************************************************************** */
int getQualInfo(  IMS_MSG_STRUCT *msgDesc, long order_id, long item_id,
    char * user_name, char * account_id, char * path,
    char * media_type_n, char * media_fmt_type_n, char * media_id )
{
    int status;
    long  i,j;
    int  rowCount;
    char  str[256];

    /*
    ** Set up the command buffer with the stored procedure call.
    */
    (void) sprintf( cmdBuf, "med_get_qual_data %ld,%ld", order_id,
        item_id);

    /*
    ** Process the result rows for this query - should be only one
        row, as each order/item combination is called.
    */
    status = ims_qiResetDesc( qDesc );
    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION){
        if( status < IMS_OK){
            return (ims_msgGetSeverity (msgDesc));
        }

        /*
        ** If ENDOFQUERY, we want to finish out command and return.
        */
        if( status == IMS_ENDOFQUERY){
            continue;
        }
        if(  rowCount  >  0  ){ /* should only be one row count */
            (void) ims_msg (msgDesc, IMS_ERROR,
                "There is more than one row for order id %d.",
                order_id );
            return (IMS_WARNING );
        }

        /*
        ** Check the returned status value.
        */
        if( (status = checkRetStatus (msgDesc)) < IMS_OK){
            return( status );
        }
        /*
        ** Copy in the returned data.
        */

        j = 0;                              /*  account_id  */
        (void) memcpy( account_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        account_id[qDesc->valLength[j]] = '\0';
        ims_truncStr( account_id );

        j++;                              /*  first_name  */
        (void) memcpy( str,
            qDesc->valAddr[j], qDesc->valLength[j]);
        str[qDesc->valLength[j]] = '\0';
        ims_truncStr( str );
        (void) strcpy( user_name, str );

        j++;                              /*  initial_name  */
        (void) memcpy( str,
            qDesc->valAddr[j], qDesc->valLength[j]);
        str[qDesc->valLength[j]] = '\0';
        ims_truncStr( str );
        (void) strcat( user_name, " " );
        i = strlen( str );
        if(  i  >  0  ){ /* if no middle letter,
            leave only one blank  */
            (void) strcat( user_name, str );
            if(  i  ==  1  )  (void) strcat( user_name, ". " );
        }

        j++;                              /*  last_name  */
        (void) memcpy( str,
            qDesc->valAddr[j], qDesc->valLength[j]);
        str[qDesc->valLength[j]] = '\0';
        ims_truncStr( str );
        (void) strcat( user_name, str );

        j++;                              /*  path  */
        (void) memcpy( path,
            qDesc->valAddr[j], qDesc->valLength[j]);
        path[qDesc->valLength[j]] = '\0';
        ims_truncStr( path );

        j++;                          /* description */
        (void) memcpy( media_type_n,
            qDesc->valAddr[j], qDesc->valLength[j]);
        media_type_n[qDesc->valLength[j]] = '\0';
        ims_truncStr( media_type_n );

        j++;                          /* description */
        (void) memcpy( media_fmt_type_n,
            qDesc->valAddr[j], qDesc->valLength[j]);
        media_fmt_type_n[qDesc->valLength[j]] = '\0';
        ims_truncStr( media_fmt_type_n );

        j++;                          /* media_id */
        (void) memcpy( media_id,
            qDesc->valAddr[j], qDesc->valLength[j]);
        media_id[qDesc->valLength[j]] = '\0';
        ims_truncStr( media_id );

        rowCount++;
    }

    /*
    ** Check to see if any devices were returned.
    */
    if( IMS_AFFECTED (qDesc) <= 0){
        /*
        **  this message is deleted because in certain cases
        **      no data is okay.
        **  (void) ims_msg (msgDesc, IMS_ERROR,
        **      "There is no data for order id %d, item id %d.",
        **      order_id, item_id );
        */
        return (IMS_WARNING );
    }
    return (IMS_OK);
}   /*  getQualInfo  */


/* *****************************************************************
**
**  subr get_stage_areas2 gets storage directory.
**      input:  stage_type.
***************************************************************** */

int get_stage_areas2(
    IMS_MSG_STRUCT * msgDesc, int stage_type, char *host,
    char * path )
{
    int status;
    static  int row_count;
    long  i,j;


    (void) sprintf(cmdBuf, "med_get_stage_area  %d",
        stage_type );


    /*  need to reset descriptor  */
    status = ims_qiResetDesc( qDesc );

    status = ims_qiNextRow (qDesc);
    if(  status  == IMS_ENDOFTRANSACTION ){ /* last row finished  */
        status = IMS_OK;
        return( status );
    }
    if (status < IMS_OK){
        return (ims_msgGetSeverity (msgDesc));
    }

    /*
    ** If ENDOFQUERY, we want to finish out command and return.
    */
    if (status == IMS_ENDOFQUERY){
        status = IMS_OK;
        return( status );
    }

    /*
    ** A row has been returned.
    */
    row_count++;

    /*
    ** Copy the returned data into the structure.
    */

    j = 0;                              /*  host */
    (void) memcpy ((char *) host,
        qDesc->valAddr[j], qDesc->valLength[j]);
    host[qDesc->valLength[j]] = '\0';
    ims_truncStr( host );

    j++;                                /*  path */
    (void) memcpy ((char *) path,
        qDesc->valAddr[j], qDesc->valLength[j]);
    path[qDesc->valLength[j]] = '\0';
    ims_truncStr( path );


    /*
    ** Check the returned status value.
    */
    if( checkRetStatus (msgDesc) < IMS_OK){
        (void) ims_msg( msgDesc, IMS_ERROR,
            "get_stage_areas2:  get_stage_areas2 had error." );
        return( IMS_ERROR );
    }

    return (IMS_OK);
}   /*  get_stage_areas2   */


/***************************************************************
**
**  subr checkRetStatus ()
**
**  Check the procedure returned status value.
**  When status returned is not an error, then return IMS_OK.
**
**************************************************************** */

static  int checkRetStatus (
    IMS_MSG_STRUCT *msgDesc)
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
                "Sybase procedure '%s' returned a status of %ld",
                qDesc->cmd, procReturn);
            return (severity);
        }
    }

    return (IMS_OK);
}   /*  checkRetStatus   */
