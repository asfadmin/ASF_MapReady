#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

#pragma ident	"@(#)activity_conf.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.activity_conf.c"
/********************************************************************
*  Name : ACTIVITY_CONF.C
*  Module Type: int function	Language: C
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]ACTIVITY_CONF.C_V  $
*  Purpose: 	Given an activity, this routine calls non_dish_conf or 
*		dtk_conflict, according to whether or not the AS dish is 
*		required in this activity.  
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  p		int	pointer to a structure with the input data.  
*			The structure is defined as:  
*
*		char obj[3]	E1, J1, RS, AS (AS=ASF)
*					The object is the thing that is doing 
*					the activity.  It could be the ASF 
*					dish or a satellite.  
*		char acty[4]	activity.  
*					The activity is whatever is taking 
*					place between the 2 input times below.  
*					It could also be the fact that some 
*					equipment is down.
*		char transid[3]	00 = E1, RS, or other activity not 
*					     involving a transmitter.  
*					01 = J1 8150 frequency 1
*					02 = J1 8350 frequency 2
*			NOTE:  the above 3 fields are validated by existing in 
*			a single record in the activities relation in the 
*			database.  
*
*		char asft1[22]	ASF format begin time for activity
*		char asft2[22]	ASF format end time for activity
*		int dtkid	If the input activity is a data-take, 
*					then put the data-take id here.  This 
*					to prevent this routine from saying 
*					that a data-take conflicts with 
*					itself.  
*		char fadtkid[21] If the input activity is a data-take, 
*					then put the flight agency data-take, 
*					if there is one, here.  This to 
*					prevent the routine from saying that 
*					a data-take conflicts with itself.  
*
*			Note:  EITHER a dtkid OR a fadtkid is sufficient to 
*			identify an existing dtk.  
*			If the dtk activity has the same sat/activity/frequency:
*
*				and if the dtkid or fadtkid matches, the 
*				existing dtk is considered to CONCUR with the 
*				input data-take.  
*
*				but if neither the dtkid nor the fadtkid 
*				match, the existing dtk activity is the 
*				considered the SAME as the input data-take 
*				activity.
*
*		THE FOLLOWING INTEGER COUNTERS ARE ALSO IN THE STRUCTURE 
*		AND ARE SET BY THIS ROUTINE.  
*
*  		int n_confs	number of CONFLICTS
*			the number of activities encountered which conflict 
*			with the input activity
*
*  		int n_combines	number of COMBINES
*			the number of activities encountered which ought to be 
*			combined with the input activity into one activity.
*			example:  RTO freq 1 and RTV freq 1
*			also:     RTS freq1 and RTS freq2
*
*			in this case, an activity equivalent to the input 
*			activity can be placed into the db by simply expanding 
*			the times of the existing activity if necessary, and 
*			accepting the resulting change of frequency channel 
*			or sensor.  in this way, a data request can be 
*			fullfilled with no or very little extra work by the 
*			satellite.  
*
*  		int n_sims	number of SIMILAR activities
*			the number of activities encountered which are the same 
*			activity as the input activity but DO NOT have the 
*			same id's. in other words, the input activity can be 
*			placed into the db by simply expanding the times of 
*			the existing activity if necessary.  But the id of the 
*			input activity, if any, will not exist since it will 
*			no longer be a separate activity
*
*  		n_concurs	number of CONCURS
*			the number of activities encountered which are the same 
*			activity as the input activity but DO have the 
*			same id's. in other words, the activity is already in 
*			the database.  Note that the times might be different
*			if a flight agency has changed them from the 
*			original times.  
*  		n_pars		number of PARALLEL activities encountered
*			the number of activities encountered which are 
*			different from the input activity, cannot be combined, 
*			and do not conflict with it.  They can still go on in 
*			parallel.  
*
* INPUT ARGUMENTS (CONTINUED)
*  Name         Type    Definition
*  dbproc       *DBPROCESS      pointer to db session info.  it is declared 
*			here as void to avoid having to have any Sybase-related
*			code and include files.  
*  pflag	int	flag indicating desired output:
*
*		*	The following values ALSO indicate that only data-takes 
*		*	with a status of SCH and PLN are examined when looking 
*		*	for conflicts:  
*
*			0 => no output.
*			1 => use the LOG file only
*			2 => use prompts only
*			3 => use the LOG file and prompts both
*
*		*	The following values ALSO indicate that data-takes with 
*		*	a status of QUE, SCH, and PLN are examined when 
*		*	looking for conflicts:  
*
*			10 => no output.
*			11 => use the LOG file only
*
*  Output Parameters:
*  returned value int	= total number of activities encountered that overlap 
*			     the input activity in time.  
*			= -1 input error in object, activity, or transid.  
*			     the input 3 values did not match any activity
*			     in the activities relation.  
*			= -2 not enough data in the activity_conf relation
*			= -3 no data in the asf_perf relation - for aim time.
*			= -4 input error in asft1
*			= -5 input error in asft2
*			= -6 input error in pflag
*			= -7 input activity matched more than 1 db activity
*			     in the activities relation.  
*			= -8 no data in the activities relation.
*			= -9 a dtk relation sat/activity/transid did not match 
*			     any activity in the activities relation.  
*			= -10 error in INGRES retrieve
*			= -11 the activities relation has no ASF down activity.
*			= -12 a j1_dn_times relation activity did not match 
*			     any activity in the activities relation.  
*			= -13 input error in dtkid
*			= -14 input error in activity; no ASF dish data-take 
*			     activity is allowed.  
*			= -100 error in sat/activity codes in db record.
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
**********************************************************************/
#include "mps.h"

struct args 
{
	char stationid[4] ;
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

/*	INTEGER FUNCTION ACTIVITY_CONF 	*/                
int activity_conf(
	void       	*dbproc,
	struct args 	*p,
	int 		pflag	)
{
int stat ;

if ( strcmp (p->acty, "DMP") == 0 || strncmp(p->acty, "RT", 2) == 0 )
{
	/* this is the ASF dish taking data.  either tape dump or real time.  */
	stat = dtk_conflict (dbproc, p,pflag ) ;
}
else
{
	/* other activities.  data observations or downtime of equipment    */
	stat = non_dish_conf (dbproc, p,pflag ) ;
}

return stat ;

}
