#ifdef COPYRIGHT
Copyright (c)1997, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       print_ims_event.c
Description:    print IMS structure from APS info
==============================================================================*/
#pragma ident   "@(#)print_ims_event.c	1.2 98/03/23 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/stats/SCCS/s.print_ims_event.c"

#include "aps_Statistics.h"  /* for IMS_CLNT_EVENT, etc.                 */


/*==============================================================================
Function:       print_ims_event()
Description:    print IMS structure from APS info
Creator:        Lawrence Stevens
Creation Date:  Tue Feb 24 14:16:10 PST 1998
==============================================================================*/
void
print_ims_event(
    FILE            *fp,
    IMS_CLNT_EVENT  *ims_event )
{
    (void)fprintf( fp, "Values of IMS_CLNT_EVENT structure ims_event:\n" ) ;
 
    /*   IMS_CLNT_REQUEST_TYPE requestType;     */
    if( ims_event->requestType == IMS_REPLACE )
        (void)fprintf( fp, "    requestType = IMS_REPLACE\n" ) ;
    else
        (void)fprintf( fp, "    requestType = ???????????\n" ) ;
 
    /*   char *username;            */
    (void)fprintf( fp, "    username = %s\n", ims_event->username ) ;
 
    /*   char *password;            */
    (void)fprintf( fp, "    password = %s\n", ims_event->password ) ;
 
    /*   char *accountId;          */
    (void)fprintf( fp, "    accountId = %s\n", ims_event->accountId ) ;
 
    /*   char *platform;    */
    (void)fprintf( fp, "    platform = %s\n", ims_event->platform ) ;

    /*   char *sensor;    */
    if( ims_event->sensor == NULL)
        (void)fprintf( fp, "    sensor = NULL\n" ) ;
    else
        (void)fprintf( fp, "    sensor = ????\n" ) ;
 
    /*   char *dataset;    */
    (void)fprintf( fp, "    dataset = %s\n", ims_event->dataset ) ;
 
    /*   char *name;    Name of PMF, without path or extension.  */
    (void)fprintf( fp, "    name = %s\n", ims_event->name ) ;
 
    /*   char *format;    */
    (void)fprintf( fp, "    format = %s\n", ims_event->format ) ;
 
    /*   short version;    */
    (void)fprintf( fp, "    version = %d\n", ims_event->version ) ;
 
    /*   short fileCount;    */
    (void)fprintf( fp, "    fileCount = %d\n", ims_event->fileCount ) ;
 
    /*   char **extensions;    */
    (void)fprintf( fp, "    extensions[0] = %s\n", ims_event->extensions[0] ) ;
 
    /*   char *sourceDir;    */
    (void)fprintf( fp, "    sourceDir = %s\n", ims_event->sourceDir ) ;
 
    /*   char localArchiveFlag;    */
    (void)fprintf( fp, "    localArchiveFlag = %c\n",
        ims_event->localArchiveFlag ) ;
 
    /*   char *programName;          */
    (void)fprintf( fp, "    programName = %s\n", ims_event->programName ) ;
 
    /*   char *catSrvName;           */
    (void)fprintf( fp, "    catSrvName = %s\n", ims_event->catSrvName ) ;
 
    /*   char *catDbName;           */
    (void)fprintf( fp, "    catDbName = %s\n", ims_event->catDbName ) ;
 
    /*   char *ftsSrvName;         */
    (void)fprintf( fp, "    ftsSrvName = NULL\n" ) ;
 
    /*   IMS_MSG_STRUCT *msgDesc;    */
 
    return ;

}
