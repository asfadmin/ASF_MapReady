#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		dn_time_conf_rej.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)dn_time_conf_rej.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/j1_msgn/SCCS/s.dn_time_conf_rej.c"

/********************************************************************
*  Name : dn_time_conf_rej
*  Module Type: int function    Language: C
*  Purpose:     Given an activity, this routine searches the DB for conflicts 
*       and reports according to the print flag pflag.  Return value 
*       is the number of conflicts.  Returns a negative number if an 
*       error was encountered. 
*       also returns a list of satellite activities that would be 
*       rejected if the input activity held a higher priority.  
*
*       EXAMPLE:
*           if the input activity is the ASF dish is down, 
*           then a list of all ASF data-takes (up to the first 100)
*           taking place between the input start and stop times
*           will be retured.  This is because they cannot take place 
*           if this input activity stands.  
*           The calling routine generally takes this list and changes 
*           the status to REJ.  
*
*       THE ACTIVITY MUST NOT BE AN ASF DISH DATA-TAKE ACTIVITY.  
*       for that, use dtk_conflict.qc instead.  
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  p        int pointer to a structure with the input data.  
*           The structure is defined as:  
*
*       char obj[3] E1, J1, RS, AS (AS=ASF)
*                   The object is the thing that is doing 
*                   the activity.  It could be the ASF 
*                   dish or a satellite.  
*       char acty[4]    activity.  
*                   The activity is whatever is taking 
*                   place between the 2 input times below.  
*                   It could also be the fact that some 
*                   equipment is down.
*       char transid[3] 00 = E1, RS, or other activity not 
*                        involving a transmitter.  
*                   01 = J1 8150 frequency 1
*                   02 = J1 8350 frequency 2
*           NOTE:  the above 3 fields are validated by existing in 
*           a single record in the activities relation in the 
*           database.  
*
*       char asft1[22]  ASF format begin time for activity
*       char asft2[22]  ASF format end time for activity
*       int dtkid   If the input activity is a data-take, 
*                   then put the data-take id here.  This 
*                   to prevent this routine from saying 
*                   that a data-take conflicts with 
*                   itself.  
*       char fadtkid[21] If the input activity is a data-take, 
*                   then put the flight agency data-take, 
*                   if there is one, here.  This to 
*                   prevent the routine from saying that 
*                   a data-take conflicts with itself.  
*
*           Note:  EITHER a dtkid OR a fadtkid is sufficient to 
*           identify an existing dtk.  
*           If the dtk activity has the same sat/activity/frequency:
*
*               and if the dtkid or fadtkid matches, the 
*               existing dtk is considered to CONCUR with the 
*               input data-take.  
*
*               but if neither the dtkid nor the fadtkid 
*               match, the existing dtk activity is the 
*               considered the SAME as the input data-take 
*               activity.
*
*       THE FOLLOWING INTEGER COUNTERS ARE ALSO IN THE STRUCTURE 
*       AND ARE SET BY THIS ROUTINE.  
*
*       int n_confs number of CONFLICTS
*           the number of activities encountered which conflict 
*           with the input activity
*
*       int n_combines  number of COMBINES
*           the number of activities encountered which ought to be 
*           combined with the input activity into one activity.
*           example:  RTO freq 1 and RTV freq 1
*           also:     RTS freq1 and RTS freq2
*
*           in this case, an activity equivalent to the input 
*           activity can be placed into the db by simply expanding 
*           the times of the existing activity if necessary, and 
*           accepting the resulting change of frequency channel 
*           or sensor.  in this way, a data request can be 
*           fullfilled with no or very little extra work by the 
*           satellite.  
*
*       int n_sims  number of SIMILAR activities
*           the number of activities encountered which are the same 
*           activity as the input activity but DO NOT have the 
*           same id's. in other words, the input activity can be 
*           placed into the db by simply expanding the times of 
*           the existing activity if necessary.  But the id of the 
*           input activity, if any, will not exist since it will 
*           no longer be a separate activity
*
*       n_concurs   number of CONCURS
*           the number of activities encountered which are the same 
*           activity as the input activity but DO have the 
*           same id's. in other words, the activity is already in 
*           the database.  Note that the times might be different
*           if a flight agency has changed them from the 
*           original times.  
*       n_pars      number of PARALLEL activities encountered
*           the number of activities encountered which are 
*           different from the input activity, cannot be combined, 
*           and do not conflict with it.  They can still go on in 
*           parallel.  
*
* INPUT ARGUMENTS (CONTINUED)
*  Name         Type    Definition
*  pflag    int flag indicating desired output:
*
*       *   The following values ALSO indicate that only data-takes 
*       *   with a status of SCH and PLN are examined when looking 
*       *   for conflicts:  
*
*           0 => no output.
*           1 => use the LOG file (std output)
*
*       *   The following values ALSO indicate that data-takes with 
*       *   a status of QUE, SCH, and PLN are examined when 
*       *   looking for conflicts:  
*
*           10 => no output.
*           11 => use the LOG file (std output)
*
*  Output Parameters:
*  r        int pointer to a structure to contain the reject list. 
*
*           char    sat[100][3] ;       sat id's of rejects.  
*           char    sensor[100][4] ;    sensor id's of rejects.  
*           int     rev[100];           revs of rejects.  
*           int     dtkid[100] ;        dtk id's of rejects.  
*           int     rejcnt;             number of rejects.  
*
*  Returned value int   = total number of activities encountered that overlap 
*                the input activity in time.  
*           = -1 input error in object, activity, or transid.  
*                the input 3 values did not match any activity
*                in the activities relation.  
*           = -2 not enough data in the activity_conf relation
*           = -3 no data in the asf_perf relation - for aim time.
*           = -4 input error in asft1
*           = -5 input error in asft2
*           = -6 input error in pflag
*           = -7 input activity matched more than 1 db activity
*                in the activities relation.  
*           = -8 no data in the activities relation.
*           = -9 a dtk relation sat/activity/transid did not match 
*                any activity in the activities relation.  
*           = -10 error in INGRES retrieve
*           = -11 the activities relation has no ASF down activity.
*           = -12 a j1_dn_times relation activity did not match 
*                any activity in the activities relation.  
*           = -13 input error in dtkid
*           = -14 input error in activity; no ASF dish data-take 
*                activity is allowed.  
*           = -100 error in sat/activity codes in db record.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date          Rev  Author
*  14 Nov 1991 19:44:32  1.0  LARRY  
*  $Date$ $Revision$ $Author$
**********************************************************************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
 
/* this is the user - aps/mps include file located at $MPS_LIB directory */
#include "mps.h"
 #include "timeconv.h"
  
/* these are sybase include files; see the makefile -I value for directory: */
#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>


struct args 
{
    char station_id[4] ;
    char obj[3];
    char acty[4];
    char transid[3];
    char asft1[22];
    char asft2[22];
    int dtkid;
    char fadtkid[21];
    int n_confs;
    int n_combines;
    int n_sims;
    int n_concurs;
    int n_pars;
};

struct rejects          /* used to store dtk info for later rejecting   */
{
    char    sat[100][3] ;
    char    sensor[100][4] ;
    int     rev[100];
    int     dtkid[100] ;
    int     rejcnt;
} ;

/*-------------------------------------------------------------------------*/
/*  INTEGER FUNCTION dn_time_conf_rej   */                
int dn_time_conf_rej(
    DBPROCESS       *dbproc,
    struct args     *p,
    int             pflag,
    struct rejects  *r )
{
RETCODE     return_code;
char        buf[400];       /* used mainly for queries...   */

/*  no_more_processing is a flag to indicate that there is to 
    be no more processing in a retrieve loop.  
    we can't just jump out of the loop; sybase
    makes us look at each row in the results.  if not, then results
    are pending and we can't do a later retrieve.  this is used as part of
    the porting of the ingres "endretrieve" statement, which causes control
    to jump out of the retrieve loop, skipping the rest of the records. */
/*  with the flag no_more_processing, we simulate this by setting the 
    flag to keep going the rest of the way in the loop, getting the 
    Sybase result rows to satify Sybase, but there is no more processing 
    of data, just as in the original Ingres endretrieve statement.      */
int     no_more_processing;


char ans[81];
char msg1[101];

int n_acts ;    /* total number of activities in the activities relation */
int db_n_acty ;
char db_obj[3];
char db_acty[4];
char db_transid[3];
int n, m, j ;
int ier ;
char acts[101][8] ;
/*  these are the numbered activities   */
/*  the number in existence is n_acts   */
/*  obj, acty, and transid concatenated.    */
/*  this array is loaded from the activities 
/*  relation.               */
/*   1   "E1RTS00"  */
/*   2   "J1RTS01"  */
/*   3   "J1RTS02"  */
/*   4   "J1RTO01"  */
/*   .
/*   .
/*   */

/*  this array tells whether the input db activity conflicts with the db 
/*  activity or should be combined with it when they take place at the 
/*  same time.  first find the integer k, where:  acts[k] = db activity
/*
/*  then the value of confs[k] tells what kind of conflict there is between 
/*  the input activity and activity k:
/*
/*  confs[k] =  -1  error in code
/*           0  no conflict
/*           1  the two activities should be combined to one 
/*              activity
/*           2  conflict
/*           3  activities are the same; they should be combined
/*
/*  the values will be filled from the db relation activ_confs at run time 
/*  when the input activity is identified.  
/*  */
int confs[101] = 
{
       -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
} ;

int istat ;
/*  activity id and characters  */
char actin[8], actdb[8];
int n_actin, n_actdb ;
/*  asftimes for query statements.  (retrieves) */
char asftq1[22], asftq2[22] ;
/*  real julian day ephemeris times for computing the asf query times */
double tq1, tq2 ;
/*  aim time for the ASF dish.      */
double t_aim, t1, t2 ;

/*  database values retrieved   */
char dsat[3],dsen[4],dactid[7],dtransid[3],dfid[21];
int drev,did;
char dstrt[22],dstop[22];
char dtype[2];
char dreason[2];
char dremarks[61];
char dsar[2] ;
char dops[2] ;
char dmdr[2] ;
char dmdt[2] ;

int errno,rcount;
int n_total ;
                  
/* where clause string for the dtk relation retrieve.  */
char w_string[200] ;
char w_stat1[75] = 
   "(dtkstat=\"QUE\" or dtkstat=\"PLN\" or dtkstat=\"SCH\") ";
char w_stat2[55] = " (dtkstat=\"PLN\" or dtkstat=\"SCH\") " ;
char w_time1[25] = " and (strttime < \""  ;
char w_time2[25] = "\" and stoptime > \"" ;

/*  
## message " Starting conflict analysis..."
## sleep 1
**/


/* initialize the program counters */
p->n_confs  = 0;
p->n_combines   = 0;
p->n_sims   = 0;
p->n_concurs    = 0;
p->n_pars   = 0;
n_total     = 0;

/*  check the inputs.  pflag, dtkid.    */
if(pflag != 0 && pflag != 1 && pflag != 10 && pflag != 11 )
    return -6 ;
if (p->dtkid < 0 || p->dtkid > 256) 
    return -13 ;

/* set the WHERE clause for the dtk search */
if(pflag >= 10)
{
    /* looking at QUE status as well as SCH and PLN  */
    strcpy(w_string,w_stat1);
}
if(pflag < 10)
{
    /* looking at only SCH and PLN status  */
    strcpy(w_string,w_stat2);
}

/* now reset pflag for printing.  */
/* this is klugey.  I'm sorry.  But this way I don't have to rewrite 
   any old code.  I can still re-run the old tests and they will work.  */
/* remember that, in C, the value of pflag in the calling program is not 
   changed.  I'll never do this again.          */
if(pflag >= 10) pflag = pflag - 10;

/*  check the inputs    */
/*  search the activities relation for the activity number and to validate
/*  the input obj, acty, and transid.           */

if(pflag == 1 )
{
    printf("\n ===> START of conflict analysis:\n");
    printf(" ===> Activity:  obj=%s \n",p->obj);
    printf("            activity=%s \n",p->acty);
    printf("     transmission id=%s (frequency channel)\n",
                                p->transid);
    printf("               dtkid=%3.3d \n",p->dtkid);
    printf("             fadtkid=%s \n",p->fadtkid);
    printf("    start/stop times=%s  %s\n",p->asft1,p->asft2) ;
    printf(
" ===> Checking against time-overlapping activities in the database:\n");
}

j = 0;
/* old embedded QUEL code:  
## retrieve (   db_n_acty  = activities.#n_acty,
##      db_obj     = activities.#obj,
##      db_acty    = activities.#acty,
##      db_transid = activities.#transid)
## {
** end of old embedded QUEL code    */
/*  Sybase version:     */
strcpy(buf, "select n_acty, obj, acty, transid from activities ");
/*  printf("dn_time_conf_rej.c 1:  buf = %s\n", buf);  */
return_code = dbcmd(dbproc, buf);
return_code = dbsqlexec(dbproc);
while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
{
    if(return_code == SUCCEED)
    {
        dbbind(dbproc, 1, INTBIND,      (DBINT)0, (BYTE *)&db_n_acty);
        dbbind(dbproc, 2, NTBSTRINGBIND,(DBINT)0, (BYTE *)db_obj);
        dbbind(dbproc, 3, NTBSTRINGBIND,(DBINT)0, (BYTE *)db_acty);
        dbbind(dbproc, 4, NTBSTRINGBIND,(DBINT)0, (BYTE *)db_transid);
        rcount = 0;
        while (dbnextrow(dbproc) != NO_MORE_ROWS)
        {
            rcount++;
            /* retrieve loop        */
            /*
            printf("dn_time_conf_rej.c 2:  db_obj     = %s\n", db_obj);
            printf("              db_acty    = %s\n", db_acty);
            printf("              db_transid = %s\n", db_transid);
            printf("              db_n_acty  = %d\n", db_n_acty);
            **/
 
            /* load the activities array acts to later find
               db activity numbers  */

            strncpy(&acts[db_n_acty][0],db_obj,2) ;
            strncpy(&acts[db_n_acty][2],db_acty,3) ;
            strcpy (&acts[db_n_acty][5],db_transid) ;
            /* set the input activity number n_actin  */
            if( strncmp(db_obj,p->obj,2) == 0 &&
                strncmp(db_acty,p->acty,3) == 0 &&
                strncmp(db_transid,p->transid,2) == 0 ) 
            {   /* a match.  increment counter and set input activity code. */
                j++;
                n_actin = db_n_acty ;
            }
        } /* end of:   while (dbnextrow(dbproc) != NO_MORE_ROWS) */
    } /*  end of:   if(return_code == SUCCEED)  */
} /* end of:  while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS) */

/*  printf(" @ n_actin=%3d \n",n_actin);   */

if (rcount <= 0) /*  lack of data in activities relation */
     return -8 ;

if (j <= 0)     /*  did not match any activity.  */
{
    if(pflag == 1 )
    {   /* print in the log file.  */
        printf(
        " The activity was not found in the activities relation in the DB\n");
        printf(" No processing was done.  \n");
    }
    return -1 ;
}
if (j > 1)  /*  matched more than one activity.  */
     return -7 ;
n_acts = rcount ;   /* the total number of activities in the activities */
                    /* relation                         */
/*printf(" @ pflag=%3.3d \n",pflag) ;*/

istat = tc_asf2et(p->asft1,&tq1) ;
if(!istat)
{
    if(pflag == 1 )
    {   /* print in the log file.  */
        printf( " ERROR in start time.  No processing was done.  \n");
    }
    return -4 ;
}
istat = tc_asf2et(p->asft2,&tq2) ;
if(!istat)
{
    if(pflag == 1 )
    {   /* print in the log file.  */
        printf( " ERROR in stop time.  No processing was done.  \n");
    }
    return -5 ;
}

if (strcmp(p->acty,"RTS") == 0 ||
    strcmp(p->acty,"RTO") == 0 ||
    strcmp(p->acty,"RTV") == 0 ||
    strcmp(p->acty,"DMP") == 0    )
{
    if(pflag == 1 )
    {   /* print in the log file.  */
        printf( " The activity is an ASF dish data-take.  \n");
        printf(" No processing was done; the wrong routine was called.  \n");
        printf( "dn_time_conf_rej.c:  this routine processes only \n");
        printf("activities that do not involve the ground station.\n");
    }
    return -14;
}

/*  we know the input activity number.  
/*  now fill in the confs array with the conflict status of activities if 
/*  they take place at the same time as the input activity.  
/*  first put a 3 for the current activity.  */
confs[n_actin] = 3 ;
/* old embedded QUEL code:  
## retrieve (   n = activ_conf.#n_acty,
##      m = activ_conf.#m_acty,
##      j = activ_conf.#conf_status)
## {
** end of old embedded QUEL code    */
/* sybase version:      */
/* retrieve loop on the activ_conf relation     */
rcount = 0;
sprintf(buf, "select n_acty, m_acty, conf_status from activ_conf ");
/*  printf("dn_time_conf_rej.c 3:  buf = %s\n", buf);  */
return_code = dbcmd(dbproc,buf);
return_code = dbsqlexec(dbproc);
while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
{
    if(return_code == SUCCEED)
    {
        /* fields are:  n_acty, m_acty, conf_status     */
        dbbind(dbproc, 1, INTBIND, (DBINT)0, (BYTE *)&n);
        dbbind(dbproc, 2, INTBIND, (DBINT)0, (BYTE *)&m);
        dbbind(dbproc, 3, INTBIND, (DBINT)0, (BYTE *)&j);
        while (dbnextrow(dbproc) != NO_MORE_ROWS)
        {
            rcount++;
            /* printf("dn_time_conf_rej.c 4:  n = %2d, m = %2d, j = %2d\n", 
                n, m, j);
            **/
            /* load the conflict status array confs from the 
               activ_conf relation */
            /* it holds only conflicts or combines  */
            if( n == n_actin )
                confs[m] = j ;
            if( m == n_actin )
                confs[n] = j ;
            /* so if the db activity is k, then confs[k] will
            tell if there is a conflict with the input
            activity.        */
        }
    }
}/* end of retrieve loop for activ_conf.                */

/*
printf(" @confs =%2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d\n",
    confs[1],confs[2],confs[3],confs[4],confs[5],confs[6],confs[7],confs[8],
    confs[9],confs[10]);
printf(" @confs2=%2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d %2.2d\n",
    confs[11],confs[12],confs[13],confs[14],confs[15],confs[16],confs[17],
    confs[18],confs[19],confs[20]);
*/

if (rcount <= 50)   /*  lack of data in activ_conf relation */
     return -2 ;

/*  ready to search the dtk relation for conflicts.     */
/*      against the input activity.             */
/*  now set the 2 times for the query of the database.      */
strcpy(asftq1,p->asft1) ;
strcpy(asftq2,p->asft2) ;
/* printf(" @ asftq1=%s  asftq2=%s\n",asftq1,asftq2) ; */

/*  now complete the where string.  */
/* add the time bracket  */
strcat(w_string,w_time1);
strcat(w_string,asftq2);
strcat(w_string,w_time2);
strcat(w_string,asftq1);
strcat(w_string,"\" ) ");

ier = 0 ;
/*  retrieve loop:  */
/*
  Find dtks that:
  -overlap the input times 
  -are planned or scheduled
*/
/* old embedded QUEL code:  
## retrieve (   dsat=  dtk.#sat,
##      dsen=  dtk.#sensor,
##      drev=  dtk.#rev,
##      did=   dtk.#dtkid,
##      dfid=  TRIM(dtk.#fadtkid),
##      dtransid= dtk.#transid,
##      dactid=dtk.#actid,
##      dstrt= dtk.#strttime,
##      dstop= dtk.#stoptime)
##   where w_string
## {
**  end of old embedded QUEL code   */
/* Sybase version:      */
sprintf(buf, 
"select sat, sensor, rev, dtkid, fadtkid, transid, actid, strttime, stoptime \
from dtk where %s ", w_string );
/*  printf("dn_time_conf_rej.c 5:  buf = \n%s\n", buf);   */
return_code = dbcmd(dbproc, buf);
return_code = dbsqlexec(dbproc);
while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
{
    if(return_code == SUCCEED)
    {
        dbbind(dbproc, 1, NTBSTRINGBIND,(DBINT)0, (BYTE *)dsat);
        dbbind(dbproc, 2, NTBSTRINGBIND,(DBINT)0, (BYTE *)dsen);
        dbbind(dbproc, 3, INTBIND,      (DBINT)0, (BYTE *)&drev);
        dbbind(dbproc, 4, INTBIND,      (DBINT)0, (BYTE *)&did);
        dbbind(dbproc, 5, NTBSTRINGBIND,(DBINT)0, (BYTE *)dfid);
        dbbind(dbproc, 6, NTBSTRINGBIND,(DBINT)0, (BYTE *)dtransid);
        dbbind(dbproc, 7, NTBSTRINGBIND,(DBINT)0, (BYTE *)dactid);
        dbbind(dbproc, 8, NTBSTRINGBIND,(DBINT)0, (BYTE *)dstrt);
        dbbind(dbproc, 9, NTBSTRINGBIND,(DBINT)0, (BYTE *)dstop);

        no_more_processing = 0; /* this flag used to stop processing but 
                                   still complete the sybase result loop 
                                   and to prevent any still pending results */

        while (dbnextrow(dbproc) != NO_MORE_ROWS)
        {
            /**
            printf("dn_time_conf_rej.c 6:  dsat   = %s\n", dsat);
            printf("                       dsen      = %s\n", dsen);
            printf("                       drev      = %d\n", drev);
            printf("                       did       = %d\n", did);
            printf("                       dfid      = %s\n", dfid);
            printf("                       dtransid  = %s\n", dtransid);
            printf("                       dactid    = %s\n", dactid);
            printf("                       dstrt     = %s\n", dstrt);
            printf("                       dstop     = %s\n", dstop);
            **/
 
            if(no_more_processing)
                continue;

            n_total++ ;
            /*  now develop the db activity identifier actdb and value 
                n_actdb.   */
            strncpy(&actdb[0],dsat,2) ;
            strncpy(&actdb[2],dactid,3) ;
            strcpy(&actdb[5],dtransid) ;
            /*  printf(" @ retrieved activity=%s\n",actdb ); */
            n_actdb = 0 ;
            for (j=1; j <= n_acts && n_actdb == 0; j++)
            {
                /*  printf(" @ comparing with acts[j]=%s and j=%3.3d\n",
                    &acts[j][0],j ); 
                */

                if ( strcmp(actdb, &acts[j][0]) == 0 ) 
                    n_actdb = j;
            }
            if ( n_actdb == 0 )
            {
                printf(" ERROR in dtk relation.  \n");
                printf(
                " dtk activity was not found in the activities relation.\n");
                printf("     sat=%s  sensor=%s  rev=%d  dtkid=%d\n",
                    dsat,dsen,drev,did);
                printf("     fadtkid=%s   transid=%s  activity id=%s\n",
                    dfid,dtransid,dactid);
                printf("     start/stop times=%s   %s\n",
                dstrt,dstop) ;

                printf( 
                "\tCheck the values for sat, activity id, and transid.\n");
                printf( 
                "\tEither the dtk record is a bad record, or there \n");
                printf( 
                "\tshould be a record in the activities relation with \n");
                printf( "\tthe following field values: \n");
                printf( "\tobj = %s, acty = %3.3s, transid = %s\n",
                dsat,dactid,dtransid);

                ier = -9 ;
                /*  ##      endretrieve */
                no_more_processing = 1;
                continue;
            }
            /*  the input activity and the db activity overlap; compare 
                activities to identify conflict.        */
            /*  check for a conflict to report.  */

            /*  
            printf("dn_time_conf_rej.c 6.1: n_actdb=%d, confs[n_actdb] = %d\n",
                n_actdb, confs[n_actdb]);
            */

            if (confs[n_actdb] == 1)
            {  /* No conflict, but activities should be COMBINED. */
                p->n_combines ++ ;
                if(pflag == 1 )
                { /* print in the log file.  */
                    printf(
" The activity should be COMBINED with the following activity in the DB:\n");
                    printf( " COMBINE:   Data take: %.2s/%.1s/%05d.%02d  %s\n",
                         dsat,dsen,drev,did,dfid );
                    printf("%29s %s  %s  %s\n", dactid,dtransid,dstrt,dstop);
                }
            }
            else if (confs[n_actdb] == 0)
            {   /* NO CONFLICT. */
                p->n_pars ++;
                if(pflag == 1 )
                { /* print in the log file.  */
                    /* log dtk rec to log file */
                    #ifdef DO_NOT_PRINT
                    printf(" PARALLEL   Data take: %.2s/%.1s/%05d.%02d  %s\n",
                        dsat,dsen,drev,did,dfid );
                    printf("%29s %s  %s  %s\n", dactid,dtransid,dstrt,dstop);
                    #endif
                }
            }
            else if (confs[n_actdb] == 2)
            {   /* CONFLICT.    */
                p->n_confs ++;
                /* ?? */
                if ( r->rejcnt < 100 ) 
                {
                    /* conflict identified.  now store, for the benifit of  */
                    /* the calling program, the info in a list.             */
                    strcpy ( r->sat[r->rejcnt], dsat ) ;
                    strcpy ( r->sensor[r->rejcnt], dsen ) ;
                    r->rev[r->rejcnt] = drev ;
                    r->dtkid[r->rejcnt] = did ;
                    r->rejcnt ++ ;
                }
                if(pflag == 1 )
                { /* print in the log file.  */
                    /* log dtk rec to log file */
                    printf( " CONFLICT:  Data take: %.2s/%.1s/%05d.%02d  %s\n",
                        dsat,dsen,drev,did,dfid );
                    printf("%29s %s  %s  %s\n", dactid,dtransid,dstrt,dstop);
                }
            }
            else if (confs[n_actdb] == 3)
            {   /* Same kind of activity; check for identical.  */
                if(p->dtkid != did && strcmp(p->fadtkid,dfid) != 0)
                {   /* id's different; SAME activity.  must report. */
                    p->n_sims ++;
                    if(pflag == 1 )
                    { /* print in the log file.  */
                        printf( " A SIMILAR activity is already in the DB:\n");
                        printf(
                            " SIMILAR    Data take: %.2s/%.1s/%05d.%02d  %s\n",
                            dsat,dsen,drev,did,dfid );
                        printf("%29s %s  %s  %s\n",dactid,dtransid,dstrt,dstop);
                    }
                }
                else
                {   /* identical - same id.  db concurs with activity.  */
                    /* must report this...                              */
                    p->n_concurs ++ ;
                    if(pflag == 1 )
                    { /* print in the log file.  */
                        printf(" The activity is ALREADY in the DB:\n");
                        printf(
                            " CONCURRING Data take: %.2s/%.1s/%05d.%02d  %s\n",
                            dsat,dsen,drev,did,dfid );
                        printf("%29s %s  %s  %s\n",dactid,dtransid,dstrt,dstop);
                    }
                }
            } /* end of:  else if (confs[n_actdb] == 3)     */
        } /* end of:  while (dbnextrow(dbproc) != NO_MORE_ROWS)     */
    } /* end of:  if(return_code == SUCCEED)        */
} /* end of:  while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)  */
/* this the end of the retrieve loop.   */
if (ier != 0)
     return ier ;

/*  Search the asf_dn_times relation for conflicts.         */
/*  ONLY if the input activity is:  RTS, RTO, RTV, DMP DDN      */
/*  NOT if the input activity is:  RES, REO, REV, SDN, ODN, RDN, TDN*/
/*    */
if(strcmp(p->acty,"RTS") == 0 || 
   strcmp(p->acty,"RTO") == 0 || 
   strcmp(p->acty,"RTV") == 0 || 
   strcmp(p->acty,"DMP") == 0 ||
   strcmp(p->acty,"DDN") == 0   )
{   /* ===============  BEGIN IF:    SEARCH dtk             */
    /* must expand the 2 times for the query.           */
    /* must retrieve the time it takes to move the ASF dish between */
    /* data-takes.                          */
    /* ===============  START OF SEARCH asf_dn_timnes  =====    */
    /* now set the 2 times for the query of the database.       */
    /*                              */
    /* Since the input activity is a data-take, we must expand  */
    /* the 2 times for the query on both ends.  It should be expanded */
    /* by the amount of time it takes to aim the dish.          */
    /*      For activities:  RTS, RTO, RTV, DMP     */
    /*                              */
    /* use the 2 times from the previous query.  asftq1, asftq2.    */

    /*  now develop the db activity identifier actdb and value  */
    /*  n_actdb.   the activity will be ASF dish down for the   */
    /*  next search.                        */
    strcpy(actdb,"ASDDN00") ;
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) 
            n_actdb = j;
    } ;
    if ( n_actdb == 0 )
    {
        return -11 ;
    } ;
    /*
    Find ASF down times that:
    -overlap the input times 
    */
    ier = 0 ;
    /* old embedded QUEL code:  
        ## message "        Checking asf_dn_times relation..." 
        ##  retrieve (dtype = asf_dn_times.#utype,
        ##      dreason = asf_dn_times.#ureason,
        ##      dremarks= asf_dn_times.#remarks,
        ##      dstrt   = asf_dn_times.#strttime,
        ##      dstop   = asf_dn_times.#stoptime )
        ##
        ##      where ( asf_dn_times.strttime < asftq2 and 
        ##          asf_dn_times.stoptime > asftq1    ) 
    ** end of old embedded QUEL code        */
    /* Sybase version:          */
    no_more_processing = 0;
    sprintf(buf,
"select utype, ureason, remarks, strttime, stoptime from asf_dn_times \
where strttime < \"%s\" and stoptime > \"%s\" ",
        asftq2, asftq1 );
    /*  printf("dn_time_conf_rej.c 7:  buf = \n%s\n", buf);   */
    return_code = dbcmd(dbproc, buf);
    return_code = dbsqlexec(dbproc);
    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if(return_code == SUCCEED)
        {
            dbbind(dbproc, 1, NTBSTRINGBIND,(DBINT)0, (BYTE *)dtype);
            dbbind(dbproc, 2, NTBSTRINGBIND,(DBINT)0, (BYTE *)dreason);
            dbbind(dbproc, 3, NTBSTRINGBIND,(DBINT)0, (BYTE *)dremarks);
            dbbind(dbproc, 4, NTBSTRINGBIND,(DBINT)0, (BYTE *)dstrt);
            dbbind(dbproc, 5, NTBSTRINGBIND,(DBINT)0, (BYTE *)dstop);
         
            while (dbnextrow(dbproc) != NO_MORE_ROWS)
            {
                printf("dn_time_conf_rej.c 8:  dstrt  = %s\n", dstrt);
                printf("              dstop     = %s\n", dstop);
                printf("              dtype     = %s\n", dtype);
                printf("              dreason   = %s\n", dreason);
                printf("              dremarks  = %s\n", dremarks);
 
                if(no_more_processing)
                    continue;
 
                /* ================= RETRIEVE LOOP  =====================    */
                n_total++ ;
                /*  the input activity and the db activity overlap;     */
                /*  compare activities to identify conflict.            */
                /*  check for a conflict to report.                     */
                if (confs[n_actdb] == 1)
                {   /* error in code/data  */
                    ier = -2000 ;
                    /*  ##      endretrieve */
                    no_more_processing = 1;
                    continue;
                }
                else if (confs[n_actdb] == 2)
                {   /* CONFLICT.    */
                    p->n_confs ++;
                    if(pflag == 1 )
                    { /* print in the log file.  */
                        /* log dtk rec to log file */
                        printf(
" CONFLICT:  ASF dish down:  type=%.1s, reason=%.1s\n",dtype,dreason);
                        printf( " %33s%s  %s\n"," ",dstrt,dstop);
                        printf("                  %-s\n",dremarks);
                    }
                }
                else if (confs[n_actdb] == 3)
                {  /* Same kind of activity; db CONCURS.  no conflict.  */
                    p->n_concurs ++;
                    if(pflag == 1 )
                    {   /* print in the log file.  */
                        /* log dtk rec to log file */
                        printf(
" DB has a concurring ASF down time:  type=%.1s, reason=%.1s\n",dtype,dreason);
                        printf(" %33s%s  %s\n"," ", dstrt,dstop);
                        printf("                  %-60s\n",dremarks);
                    }
                } /* end of:  else if (confs[n_actdb] == 3) */
            }  /* end of:  while (dbnextrow(dbproc) != NO_MORE_ROWS) */
        } /* end of:  if(return_code == SUCCEED) */
    } /* end of:  while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)*/

    /* ==============  END OF RETRIEVE LOOP  ==================== */
    if (ier != 0)
        return ier ;

}   /* ===============  ENDIF:  SEARCH asf_dn_times  ===== */
        
/*  Search the j1_dn_times relation for conflicts.
/*  ONLY if the input object is J1 or ASF.  
/*  Use the original times from the activity.  
/*    */
if(strcmp(p->obj,"J1") == 0 || 
   strcmp(p->obj,"AS") == 0     )
{   /* ===============  BEGIN IF:    SEARCH jn_dn_times     */
    /*  now set the 2 times for the query of the database.  
    /*  */
    strcpy(asftq1,p->asft1) ;
    strcpy(asftq2,p->asft2) ;

    /*  retrieve loop:  */
    /*
      Find J1 down times that:
      -overlap the input times 
    */
    ier = 0 ;
    /* old embedded QUEL code:  
        ##  message "       Checking j1_dn_times relation..." 
        ##  retrieve (dsar  = j1_dn_times.#sar_status,
        ##      dops    = j1_dn_times.#ops_status,
        ##      dmdr    = j1_dn_times.#mdr_status,
        ##      dmdt    = j1_dn_times.#mdt_status,
        ##      dstrt   = j1_dn_times.#strttime,
        ##      dstop   = j1_dn_times.#stoptime )
        ##
        ##  where ( j1_dn_times.strttime < asftq2 and 
        ##          j1_dn_times.stoptime > asftq1   ) 
    ** end of old embedded QUEL code    */
    /* Sybase code:     */
    no_more_processing = 0;
    sprintf(buf, "select sar_status, ops_status, mdr_status, mdt_status, \
strttime, stoptime from j1_dn_times where strttime < '%s' and stoptime > '%s'",
        asftq2, asftq1 );
    /*  printf("dn_time_conf_rej.c 9:  buf = \n%s\n", buf);  */
    return_code = dbcmd(dbproc, buf);
    return_code = dbsqlexec(dbproc);
    while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)
    {
        if(return_code == SUCCEED)
        {
            dbbind(dbproc, 1, NTBSTRINGBIND,(DBINT)0, (BYTE *)dsar);
            dbbind(dbproc, 2, NTBSTRINGBIND,(DBINT)0, (BYTE *)dops);
            dbbind(dbproc, 3, NTBSTRINGBIND,(DBINT)0, (BYTE *)dmdr);
            dbbind(dbproc, 4, NTBSTRINGBIND,(DBINT)0, (BYTE *)dmdt);
            dbbind(dbproc, 5, NTBSTRINGBIND,(DBINT)0, (BYTE *)dstrt);
            dbbind(dbproc, 6, NTBSTRINGBIND,(DBINT)0, (BYTE *)dstop);
             
            while (dbnextrow(dbproc) != NO_MORE_ROWS)
{
/* ==============  START OF RETRIEVE j1_dn_times LOOP  ==================== */
/* ==============  START OF RETRIEVE j1_dn_times LOOP  ==================== */

/*  
printf("dn_time_conf_rej.c 10:  dstrt       = %s\n", dstrt);
printf("              dstop  = %s\n", dstop);
printf("              dsar   = %s\n", dsar);
printf("              dops   = %s\n", dops);
printf("              dmdr   = %s\n", dmdr);
printf("              dmdt   = %s\n", dmdt);
 */

/* the input activity and the db activity overlap;      */
/* must check to see if any or all of these activities are  */
/* indicated by this one db record:                 */
/*  J1 SAR is down.             */
/*  J1 OPS is down.             */
/*  J1 OVN is down.             */
/*  J1 MDR is down.             */
/*  J1 MDT freq 1 is down.      */
/*  J1 MDT freq 2 is down.      */

if(strncmp(dsar,"Y",1) != 0)
{
    /*  J1 SAR is down.         */
    strcpy(actdb,"J1SDN00") ;
    /*  now develop the db activity identifier actdb    */
    /*  and value n_actdb.                  */
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) n_actdb = j;
    };
    if ( n_actdb == 0 )
    {
        ier = -12 ;
        /*  ##      endretrieve  */
        no_more_processing = 1;
        continue;
    };
    /* the input activity and the db activity overlap; compare  */
    /* activities to identify conflict.                 */
    /* check for a conflict to report.  */

    if (confs[n_actdb] == 1)
    {  /* error in code/data  */
        ier = -2000 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    }
    else if (confs[n_actdb] == 2)
    {   /* CONFLICT.    */
        n_total++ ;
        p->n_confs ++;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " CONFLICT:  J1 SAR down:          %s  %s\n",
            dstrt,dstop);
        }
    }
    else if (confs[n_actdb] == 3)
    {   /* SAME KIND of activity; no conflict.  */
        p->n_concurs ++ ;
        n_total++ ;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " DB has a concurring J1 SAR down time:\n");
            printf("                       %s  %s\n", dstrt,dstop);
        }
    }
}
/* ==============  MIDDLE OF RETRIEVE j1_dn_times LOOP  ==================== */
if(strncmp(dops,"Y",1) != 0)
{
    /*  J1 OPS is down.         */
    strcpy(actdb,"J1ODN00") ;
    /*  now develop the db activity identifier actdb    */
    /*  and value n_actdb.                  */
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) n_actdb = j;
    };
    if ( n_actdb == 0 )
    {
        ier = -12 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    };
    /* the input activity and the db activity overlap; compare  */
    /* activities to identify conflict.                 */
    /* check for a conflict to report.  */


    if (confs[n_actdb] == 1)
    {  /* error in code/data  */
        ier = -2000 ;
        /* ##       endretrieve     */
        no_more_processing = 1;
        continue;
    }
    else if (confs[n_actdb] == 2)
    {   /* CONFLICT.    */
        n_total++ ;
        p->n_confs ++;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " CONFLICT:  J1 OPS down:          %s  %s\n", dstrt,dstop);
        }
    }
    else if (confs[n_actdb] == 3)
    {   /* SAME KIND of activity; no conflict.  */
        p->n_concurs ++ ;
        n_total++ ;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " DB has a concurring J1 OPS down time:\n");
            printf("                       %s  %s\n", dstrt,dstop);
        }
    }
}
/* ==============  MIDDLE OF RETRIEVE j1_dn_times LOOP  ==================== */
if(strncmp(dops,"Y",1) != 0 && strncmp(dops,"V",1) != 0 )
{
    /*  J1 OVN is down.         */
    strcpy(actdb,"J1VDN00") ;
    /*  now develop the db activity identifier actdb    */
    /*  and value n_actdb.                  */
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) n_actdb = j;
    };
    if ( n_actdb == 0 )
    {
        ier = -12 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    };
    /* the input activity and the db activity overlap; compare  */
    /* activities to identify conflict.                 */
    /* check for a conflict to report.  */


    if (confs[n_actdb] == 1)
    {  /* error in code/data  */
        ier = -2000 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    }
    else if (confs[n_actdb] == 2)
    {   /* CONFLICT.    */
        n_total++ ;
        p->n_confs ++;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf(" CONFLICT:  J1 OVN down:          %s  %s\n", dstrt,dstop);
        }
    }
    else if (confs[n_actdb] == 3)
    {   /* SAME KIND of activity; no conflict.  */
        p->n_concurs ++ ;
        n_total++ ;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " DB has a concurring J1 OVN down time:\n");
            printf("                       %s  %s\n", dstrt,dstop);
        }
    }
}
/* ==============  MIDDLE OF RETRIEVE j1_dn_times LOOP  ==================== */
if(strncmp(dmdr,"Y",1) != 0)
{
    /*  J1 MDR is down.         */
    strcpy(actdb,"J1RDN00") ;
    /*  now develop the db activity identifier actdb    */
    /*  and value n_actdb.                  */
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) n_actdb = j;
    };
    if ( n_actdb == 0 )
    {
        ier = -12;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    };
    /* the input activity and the db activity overlap; compare  */
    /* activities to identify conflict.                 */
    /* check for a conflict to report.  */


    if (confs[n_actdb] == 1)
    {  /* error in code/data  */
        ier = -2000 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    }
    else if (confs[n_actdb] == 2)
    {   /* CONFLICT.    */
        n_total++ ;
        p->n_confs ++;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " CONFLICT:  J1 MDR down:          %s  %s\n", dstrt,dstop);
        }
    }
    else if (confs[n_actdb] == 3)
    {   /* SAME KIND of activity; no conflict.  */
        p->n_concurs ++ ;
        n_total++ ;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " DB has a concurring J1 MDR down time:\n");
            printf("                       %s  %s\n", dstrt,dstop);
        }
    }
}
/* ==============  MIDDLE OF RETRIEVE j1_dn_times LOOP  ==================== */
if(strncmp(dmdt,"Y",1) != 0 && strncmp(dmdt,"1",1) != 0 )
{
    /*  J1 MDT freq 1 is down.      */
    strcpy(actdb,"J1TDNF1") ;
    /*  now develop the db activity identifier actdb    */
    /*  and value n_actdb.                  */
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) n_actdb = j;
    };
    if ( n_actdb == 0 )
    {
        ier = -12 ;
        /*  ##      endretrieve     */
        no_more_processing = 1;
        continue;
    };
    /* the input activity and the db activity overlap; compare  */
    /* activities to identify conflict.                 */
    /* check for a conflict to report.  */

    if (confs[n_actdb] == 1)
    {  /* error in code/data  */
        ier = -2000 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    }
    else if (confs[n_actdb] == 2)
    {   /* CONFLICT.    */
        n_total++ ;
        p->n_confs ++;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " CONFLICT:  J1 MDT freq 1 down:   %s  %s\n", dstrt,dstop);
        }
    }
    else if (confs[n_actdb] == 3)
    {   /* SAME KIND of activity; no conflict.  */
        p->n_concurs ++ ;
        n_total++ ;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " DB has a concurring J1 MDT freq 1 down time:\n");
            printf("                       %s  %s\n", dstrt,dstop);
        }
    }
}
/* ==============  MIDDLE OF RETRIEVE j1_dn_times LOOP  ==================== */
if(strncmp(dmdt,"Y",1) != 0 && strncmp(dmdt,"2",1) != 0 )
{
    /*  J1 MDT freq 2 is down.      */
    strcpy(actdb,"J1TDNF2") ;
    /*  now develop the db activity identifier actdb    */
    /*  and value n_actdb.                  */
    n_actdb = 0 ;
    for (j=1; j <= n_acts && n_actdb == 0; j++)
    {
        if ( strcmp(actdb, acts[j]) == 0 ) n_actdb = j ;
    };
    if ( n_actdb == 0 )
    {
        ier = -12 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    };
    /* the input activity and the db activity overlap; compare  */
    /* activities to identify conflict.                 */
    /* check for a conflict to report.  */

    if (confs[n_actdb] == 1)
    {  /* error in code/data  */
        ier = -2000 ;
        /*  ##      endretrieve */
        no_more_processing = 1;
        continue;
    }
    else if (confs[n_actdb] == 2)
    {   /* CONFLICT.    */
        n_total++ ;
        p->n_confs ++;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " CONFLICT:  J1 MDT freq 2 down:   %s  %s\n", dstrt,dstop);
        }
    }
    else if (confs[n_actdb] == 3)
    {   /* SAME KIND of activity; no conflict.  */
        p->n_concurs ++ ;
        n_total++ ;
        if(pflag == 1 )
        {   /* print in the log file.  */
            printf( " DB has a concurring J1 MDT freq 2 down time:\n");
            printf("                       %s  %s\n", dstrt,dstop);
        }
    }
}


/* ==============  END OF RETRIEVE j1_dn_times LOOP  ==================== */
/* ==============  END OF RETRIEVE j1_dn_times LOOP  ==================== */
} /*  while (dbnextrow(dbproc) != NO_MORE_ROWS)     */
        } /*  if(return_code == SUCCEED)        */
    } /*  while ((return_code = dbresults(dbproc)) != NO_MORE_RESULTS)      */
}   /* ===============  ENDIF SEARCH j1_dn_times  ===== */

if(pflag == 1 )
{   /* print in the log file.  */
    printf(
"\n Number of time-overlapping activities encountered in the database:  \n");
    printf(" concurs:    %3d\n",p->n_concurs );
    printf(" combines:   %3d\n",p->n_combines );
    printf(" similars:   %3d\n",p->n_sims );
    printf(" conflicts:  %3d\n",p->n_confs );
    printf(" parallels:  %3d\n",p->n_pars );
    printf(" ===> TOTAL:%4d \n",n_total);
    printf(" ===> END of conflict analysis for this activity.\n\n");
}

return n_total;
}
