static char *sccs = "@(#)ims_j1rt2rsp.c	5.3 11/18/96";


#include <string.h>
#include <stdio.h>

#include <ims_cpfort.h>
#include <ims_timeConv.h>

typedef struct
{
	char pstartasf[22];
	double phase_lon;
	double phase_rsp_0_lon;
	int phase_days;
	int phase_revs;
	int last_rev;
	int cycle_days;
	int cycle_revs;
} PHASE_INFO;


int phase_first_rsp_rev(PHASE_INFO *, int *, int *);
int convertET(IMS_MSG_STRUCT *, char *, double *);

/*
 * Name: ims_j1rt2rsp
 * Module Type: Subroutine
 * Language: C
 *
 * Purpose: Given a J-ERS-1 rev and start/stop times, compute
 *          the j1 rsp path and start/stop angles and times
 *
 * Input Parameters:
 * Name      Type                  Definition
 * msgDesc  IMS_MSG_STRUCT*        IMS message handling
 * userSpec  IMS_JOB_USER_SPEC*    details on the DB user
 * irev      long                  input rev number
 * satname  char[]                name of the sat ;-)
 * asft1    char[]                start time in asf format
 * Output parameters:
 * ier      int                    primative FORTRAN-like error checking.
 *                                more or less unused because of IMS error
 *                                 handling
 * irsp      long*                  rsp path number corresponding to irev
 * xangl1    double *              rsp angle corresponding to asft1
 *
 * History:
 *
 * Originally coded in FORTRAN 10/25/96
 *   Larry Stevens larry@bobby.jpl.nasa.gov
 * Translated to C and IMS DB calls added 1/29/96
 *  Corey Porter cporter@impala.jpl.nasa.gov
 */

int
ims_j1rt2rsp(  
          IMS_MSG_STRUCT *msgDesc,
          IMS_JOB_USER_SPEC *userSpec,
          long irev, 
          char satname[],
          char asft1[], 
          long *irsp, 
          double *rsp_angle)
{
  long rcode, system, nerr, nrecs;
  int ipfirstrev, ipfirstrsp;
  char pstartasf[22],asft[22];
  double tnode, etr1, etr2;
  char *phase_name;
  int nrevs;
  int rowCount;
  double delta_time_days;
  double et_start_rev, et_end_rev;
  double time_for_one_rev_days;
  double fraction;
  double et_rev_start_to_input_time;
  int int_rsp_angle_100;
  PHASE_INFO phase;
  IMS_QI_DESC_OBJ *qDesc;
  int status;
  char qbuf[IMS_COL512_LEN + 1];
  double input_time, rev_start_time;


  *irsp = 0;
  nerr = 0;


  if (convertET(msgDesc, asft1, &input_time) < IMS_OK)
  {
	(void) ims_msg(msgDesc, IMS_ERROR,
		"Input Time is not valid .");
	return(IMS_ERROR);
  }



  if(irev <= 0)
  {
    return (IMS_ERROR); /* ier = -2; */
  }

  nrecs = 0;

  /* allocate a query descriptor */

  if((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ*)NULL)
  {
    (void)ims_msg(msgDesc, IMS_FATAL, "Could not alloc a query decriptor.");
    return (IMS_ERROR);
  }

  IMS_SETUSER(qDesc, userSpec->username);
  IMS_SETPSWD(qDesc, userSpec->password);
  IMS_SETPROG(qDesc, userSpec->program);
  IMS_SETSERVER(qDesc, userSpec->server);
  IMS_SETDBNAME(qDesc, userSpec->database);

  status = ims_qiLogin(qDesc);

  if(status < IMS_OK)
  {
    (void)ims_msg(msgDesc,status,"Could not login to database.");
    ims_qiFreeDesc(qDesc);
	return(IMS_ERROR);
  }

  IMS_SET_USERDATA(qDesc);
  qDesc->cmd = qbuf;

  sprintf(qbuf,
           "select phase_start, phase_lon, \
           phase_days, phase_orbits, last_rev, \
           cycle_days, cycle_revs, rsp_0_lon from phase \
           where '%s' >= phase_start  and sat = '%s' order by \
           phase_start desc", asft1,satname);

  rowCount = 0;
  while((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
  {  
    if(status < IMS_OK)
    {
      (void)ims_msg(msgDesc,IMS_ERROR,
		"Could not preform query of phase table");
      ims_qiFreeDesc(qDesc);
      return(IMS_ERROR);
    }

    if(status == IMS_ENDOFQUERY)
    {  
      continue;
    }

	if (rowCount)
		continue;

	rowCount ++;


    /* put all info in here!!! */

    (void)memcpy((char *) phase.pstartasf,
          qDesc->valAddr[0],qDesc->valLength[0]);
	    phase.pstartasf[qDesc->valLength[0]] = '\0';
    (void)memcpy((char *) & (phase.phase_lon),
          qDesc->valAddr[1],qDesc->valLength[1]);
    (void)memcpy((char *) & (phase.phase_days),
          qDesc->valAddr[2],qDesc->valLength[2]);
    (void)memcpy((char *) & (phase.phase_revs),
          qDesc->valAddr[3],qDesc->valLength[3]);
    (void)memcpy((char *) & (phase.last_rev),
          qDesc->valAddr[4],qDesc->valLength[4]);
    (void)memcpy((char *) & (phase.cycle_days),
          qDesc->valAddr[5],qDesc->valLength[5]);
    (void)memcpy((char *) & (phase.cycle_revs),
          qDesc->valAddr[6],qDesc->valLength[6]);
    (void)memcpy((char *) & (phase.phase_rsp_0_lon),
          qDesc->valAddr[7], qDesc->valLength[7]);
  }

  ims_qiFreeDesc(qDesc);

  if (rowCount == 0)
  {
	(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine phase information for platform %s",
		satname);
	return(IMS_ERROR);
  }


  if (phase_first_rsp_rev(&phase, &ipfirstrev, &ipfirstrsp) < IMS_OK)
  {
     (void) ims_msg(msgDesc, IMS_ERROR,
		"Could not calculate first RSP revolution.");
     return(IMS_ERROR);
  }


  if(irev > phase.last_rev || irev < ipfirstrev)
  {
    goto L8002;
  }

  nrevs = irev - ipfirstrev;  /* Revs since start of phase */

  *irsp = ipfirstrsp +
	 ((nrevs - 1)  * phase.cycle_days);


  /*
  ** Now compute within the bounds of 1..Cycles 
  */

  *irsp %= phase.cycle_revs;

  if (*irsp == 0)
	 *irsp = phase.cycle_revs;


  /*
  ** Now, compute the angle.
  */

  if (convertET(msgDesc, phase.pstartasf, &rev_start_time) < IMS_OK)
  {
	(void) ims_msg(msgDesc, IMS_ERROR,
		"Phase Input Time is not valid .");
	return(IMS_ERROR);
  }

  time_for_one_rev_days =
	 (double) phase.cycle_days / (double) phase.cycle_revs;

#if 0

  delta_time_days = time_for_one_rev_days * nrevs; 

  /*
  ** Compute start/end rev times.
  */


  et_start_rev = rev_start_time + delta_time_days;
  et_end_rev =  et_start_rev + time_for_one_rev_days;


  /*
  ** Time difference between position in rev and start of rev.
  */

  et_rev_start_to_input_time = input_time - et_start_rev;
#else
  et_rev_start_to_input_time = input_time - rev_start_time;
#endif

  fraction = et_rev_start_to_input_time / time_for_one_rev_days;

  *rsp_angle = fraction * 360.0;

  /*
  ** Need to make in range 0..360.0 
  ** Modulo operator only supports integer values.
  */

  while (*rsp_angle >= 360.0)
	*rsp_angle = *rsp_angle - 360.0;
	

  /*
  ** Round off to neart .01 degress.
  */

  int_rsp_angle_100 = 100.0 * (*rsp_angle) + 0.49999999;

  *rsp_angle = (double) int_rsp_angle_100 / 100.0;


  return (IMS_OK);

L6001:
L8001:
L8002: 
L9999: return (IMS_ERROR);

}

/*==============================================================================
Function:       phase_first_rsp_rev

Description:    compute the RSP path of the first rev in a phase.  
				used in other routines, mainly, this is a private 
				kind of routine.  
Parameters:     
int NASDAc_phase_first_rsp_rev(
	DB_RECORD	**phase_rec,     phase record which has the data to use. 
	int			*rev1,   		 output rev number of first rev in phase.
	int			*rsp1 )  		 output rsp of first rev in phase.       

Returns:        
	>= 0:  No error:
			NASDA_PHASE_RSP1_OK 
	<  0:  Error:
			NASDA_INPUT_PHASE_REC_IS_NULL 
			NASDA_NOT_A_NASDA_PHASE_RECORD
			other errors as returned from called routines.  

Creator:        Lawrence Stevens

Creation Date:  Tue Jun 13 13:06:42 PDT 1995


Adopted by IMS.

Notes:		
==============================================================================*/

int phase_first_rsp_rev(
	PHASE_INFO *phase_info,
	int			*rev1,   		/* output rev number of first rev in phase. */
	int			*rsp1 )  		/* output rsp of first rev in phase.        */
{

	int			first_rev_in_phase ;
	int			rsp_first_rev ;

	double		delta_longitude_rsp_0 ;
	double		delta_longitude_per_rsp_path ; ;

	/* initialize the output value to unusable.  */
	*rsp1 = -1 ;
	*rev1 = -1 ;


	/********************************************************************/
	/*                                                                  */
	/*  get the first rev in the phase                                  */
	/*                                                                  */
	/********************************************************************/
	first_rev_in_phase = phase_info->last_rev - phase_info->phase_revs;

		
	/********************************************************************/
	/*                                                                  */
	/*  Compute RSP Step 1:                                             */
	/*  compute delta longitude between rsp 0 and first rev in phase.   */
	/*                                                                  */
	/********************************************************************/
	delta_longitude_rsp_0 =  phase_info->phase_lon - 
			phase_info->phase_rsp_0_lon;


	/********************************************************************/
	/*                                                                  */
	/*  Compute RSP Step 2:                                             */
	/*  compute delta longitude per path.                               */
	/*  it is negative; this is how NASDA works it.                     */
	/*                                                                  */
	/********************************************************************/
	delta_longitude_per_rsp_path = -1.0 * (double) 360.0 
		/ phase_info->cycle_revs;

	/********************************************************************/
	/*                                                                  */
	/*  Compute RSP Step 3:                                             */
	/*  compute number of paths in delta longitude from Step 2;         */
	/*  that number is the RSP path number for the first rev in phase.  */
	/*                                                                  */
	/********************************************************************/

	/* 
	-- using addition of 1/2 and implicit default trucation to 
	-- get a net result of roundoff to the nearest integer.  
	-- normally, the division result will be extremely close to 
	-- an integer, anyway.  
	*/
	rsp_first_rev = 
		0.499999 + ( delta_longitude_rsp_0 / delta_longitude_per_rsp_path) ;

	/* rectify this rsp to [1, PHASE_CYCLE_REVS]   */
	while ( rsp_first_rev < 1 )
		rsp_first_rev += phase_info->cycle_revs;

	while ( rsp_first_rev > phase_info->cycle_revs)
		rsp_first_rev -= phase_info->cycle_revs;

	/* assign output values:   */
	*rev1 = first_rev_in_phase;
	*rsp1 = rsp_first_rev;

	return (IMS_OK);

}

int convertET(
	IMS_MSG_STRUCT *msgDesc, 
	char *time,
	double *output_time)
{
	IMS_NUMERIC_DATE dateDef;
	int days, msecs;

	if (ims_timeToNumericDate(msgDesc, time, &dateDef) < IMS_OK)
	{
		return(IMS_ERROR);
	}

	ims_numericDateToESAI(&dateDef, &days, &msecs);

	*output_time = days + msecs / (3600000.0 * 24); 
	return(IMS_OK);
}
