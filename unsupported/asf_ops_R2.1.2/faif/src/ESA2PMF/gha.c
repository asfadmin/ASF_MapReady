/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	gha.c

Description:    Contains the funtions necessary to get GHA (Greenwich Hour
                Angle) from the database and to interpolate using it.

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/
static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>
#include <stdlib.h>
#include "faifdefs.h"
 
#include <unistd.h>
#include <syslog.h>
#include <sys/wait.h>
#include <sys/types.h>
 
#include <ims_query.h>
#include <ims_cmnQuery.h>
#include "ims_archive_faif.h"
 
/* Static Variables. */
static IMS_MSG_STRUCT *msgDesc;
static IMS_MSG_QUEUE *msgQueue;

static char *server = NULL;
static char *database = NULL;
static char *username = NULL;
static char *password = NULL;
static char *program = "get_gha";

FILE *file_ptr;




/*===========================================================================
Function:	get_gha
Description:	Given a date, use ims_ghaPointQuery to find nearest date 
                and GHA in the database
Parameters:
   start_date      char*    Date of desired GHA
   file_date       char*    Nearest date found in the database
   file_angle      double*  GHA in database for file_date
   cfname          char*    Config file name
Returns:	OK or ERROR
Creator:	Rodney Hoffman
Creation Date:	09/22/1995
Modification History:
   Dec. '95   RH   Changed ims_ghaQuery to (new) ims_ghaPointQuery
Notes:		
===========================================================================*/

int get_gha (char *start_date, char *file_date, double *file_angle,
             char *cfname)
{

  IMS_CMN_QUERY ghaQuery;
  char logmsg[MAX_SYSLOG_MSGLEN + 1];
  int status, qstat;
  IMS_QI_DESC_OBJ *qDesc = NULL;
  IMS_GHA_STRUCT gha;


  /*  Allocate and initialize the message facility.  */
  if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
  {
     sprintf (logmsg,
	      "Memory allocation for IMS_MSG_STRUCT structure failed.");
     syslog(LOG_ERR, logmsg);
     return (ERROR);
  }

  /* Inititialize the message facility options. */
  (void) ims_msgSubSystem (msgDesc, "IMS");
  (void) ims_msgBanner (msgDesc, program, IMS_MSG_ALLBANNER);
  (void) ims_msgStderrFlag (msgDesc, IMS_OFF);
  (void) ims_msgQueueFlag (msgDesc, IMS_ON);
  (void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
  (void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

  /* Config file section follows.
     1-Try to open the config file. If successful, use the
       values from the file.
     2-If file is not defined, or if open fails, look to environment
       variables for each value.
     3-If still not defined, error. */

  if (cfname == (char *) NULL)
  {
     sprintf (logmsg, "No config file name; looking to env vbls.");
     syslog(LOG_ERR, logmsg);
  }

  if (cfname != NULL)
  {
     file_ptr = fopen(cfname, "r");
     if (file_ptr == (FILE *)NULL)
     {
        cfname = NULL;
        sprintf (logmsg,
	       "Config_file open failure. Looking to environment variables.");
        syslog(LOG_ERR, logmsg);
     }
     else
     {
        fclose(file_ptr);
     }
  }

  if (cfname != NULL)
  {
     server    = (char *)get_config_val(cfname, IMS_DEV_EV);
     database  = (char *)get_config_val(cfname, IMS_DB_EV);
     username  = (char *)get_config_val(cfname, IMS_USERNAME_EV);
     password  = (char *)get_config_val(cfname, IMS_PASSWD_EV);
  }
  else
  {
     server   = (char *)getenv("IMS_DEV");
     database = (char *)getenv("IMS_DB");
     username = (char *)getenv("IMS_USERNAME");
     password = (char *)getenv("IMS_PASSWD");
  }

  if ((server   == (char *) NULL) || (database == (char *) NULL) ||
      (username == (char *) NULL) || (password == (char *) NULL)) 
  {
     syslog (LOG_ERR, "Unable to define necessary database parameters.\n");
     return (ERROR);
  }

  /* Populate the query structure. */
  strcpy (ghaQuery.username, username);
  strcpy (ghaQuery.password, password);
  strcpy (ghaQuery.program, program);
  strcpy (ghaQuery.server, server);
  strcpy (ghaQuery.database, database);
  ghaQuery.msgDesc   = msgDesc;
  ghaQuery.qDesc     = qDesc;
  ghaQuery.retPtr    = (void *) &gha;

  /* Perform the query */
  status = ims_ghaPointQuery(&ghaQuery, start_date);

  if ((status < IMS_OK) || (ghaQuery.retStatus != IMS_OK))   
  {
    syslog (LOG_ERR, "Problem in get_gha.\n");
    switch (ghaQuery.retStatus)
    {
      case IMS_OK: break;
      case IMS_NOCONNECT:
        syslog (LOG_ERR, "GHA Query failed: No connect.\n"); break;
      case IMS_DEADLOCK:  
        syslog (LOG_ERR, "GHA Query failed: Deadlock.\n"); break;
      case IMS_NOROWS:    
        syslog (LOG_ERR, "GHA Query failed: No rows.\n"); break;
      case IMS_QUERYFAIL: 
        syslog (LOG_ERR, "GHA Query failed.\n"); break;
      default:            
        syslog (LOG_ERR, "GHA Query failed: Unknown query status.\n");
    }
    return (ERROR);
  }

  /* Extract messages from the message queue. */
  while ((msgQueue = ims_msgQueueExtract (msgDesc)) != (IMS_MSG_QUEUE *) NULL)
  {
    (void) fprintf (stdout, "%s\n", msgQueue->msg);
    (void) ims_msgQueueFree (msgQueue);
  }

  /* Free the message descriptor. */
  (void) ims_msgStructFree (msgDesc);

  strcpy (file_date, gha.date);
  *file_angle = atof(gha.angle);
  return (OK);
}




/*===========================================================================
Function:	gha_interp
Description:	Given the closest GHA from the database, interpolate to
                the desired time.
Parameters:
  asft		*char	TIME in the format:  yyyy-dddThh:mm:ss.sss
  asftr		*char   Closest time found in GHA fixes file
  ghar		double	Corresponding GHA in degrees from 0-360.
  gha		*double	Greenwich Hour Angle for asft in degrees from 0-360.
Creator:	Rodney Hoffman	
Creation Date:	09/22/1995
Notes:		From ACS:    [BLD.ACS.LIB.STVEC]ASF2GHA.FOR
===========================================================================*/

void gha_interp (char *asft, char *asftr, double ghar, double *gha)
{ 
   int iy1, id1, ih1, im1, iy, id, ih, im, sec, msec;
   double xs1, xs2, xs, xd, xt1, xt2, xt, xang;
   double a,b,c,d;
   long jd, jd1;
   char junk;
   /* Rotation (cycles/day inertial)
       obtained from the vector library June '89 via Subroutine PSCONS.  */
   double eroti = 1.00273790930;  

   /* Compute the time delta between selected fix and desired time in days. */
   sscanf (asftr,"%4d%c%3d%c%2d%c%2d%c%2d%c%3d", 
           &iy1, &junk, &id1, &junk, &ih1, &junk, &im1, &junk, 
           &sec, &junk, &msec);
   xs1 = sec + ((double) msec)/1000.0;
   sscanf (asft, "%4d%c%3d%c%2d%c%2d%c%2d%c%3d", 
           &iy, &junk, &id, &junk, &ih, &junk, &im, &junk, &sec, &junk, &msec);
   xs = sec + ((double) msec)/1000.0;

   /* Start with just the days:  */
   xd = (id-id1) + ((double)(ih-ih1))/24.0L + 
        ((double)(im-im1))/24.0L/60.0L + 
	(xs-xs1)/24.0L/3600.0L ;
   /* Julian day for 1st of January of year.  
   Reference:  Spacecraft Attitude Determination and Control /J. Wertz., p. 20
   jd1 <=> iy1;  jd <=> iy    */
   jd1 = 1 - 32075 + 1461*(iy1+4800+(1-14)/12)/4
         +367*(1-2-(1-14)/12*12)/12
         -3*((iy1+4900+(1-14)/12)/100)/4;
   jd = 1 - 32075 + 1461*(iy+4800+(1-14)/12)/4
         +367*(1-2-(1-14)/12*12)/12
         -3*((iy+4900+(1-14)/12)/100)/4;
   xd = xd + (jd - jd1);
   /* Compute gha. eroti = cycles/day inertial.  */
   xang = xd*eroti*360.0 + ghar;
   /* xang mod 360 */
   *gha = xang - 360.0*((int) (xang/360.0));
   if (*gha < 0.0) *gha = *gha + 360.0;
   return; 
}

/* End of File */
