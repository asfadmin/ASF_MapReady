/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   Internal-only routine.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independence.
****************************************************************/
#include "asf.h"
#include "meta_init.h"
#include "meta_init_ceos.h"
#include "ceos.h"
#include "dateUtil.h"


/*Convert the given platform position data record & time since ppdr
start into a GHA, in degrees.  Note that this is only called if the
scene's state vectors are in Inertial coordinates.*/
double ppdr2gha(struct pos_data_rec *ppdr,ceos_description *ceos,double desiredTime);
double ppdr2gha(struct pos_data_rec *ppdr,ceos_description *ceos,double desiredTime)
{
	double degPerSec=360.0/(24.0*60.0*60.0);/*Earth rotation rate, degrees/second.*/
	double start_gha=ppdr->hr_angle;/*GHA at image start.*/
	
	return start_gha+desiredTime*degPerSec;
	
/*Old way: using UTC:
	int year,julDay;
	hms_time time;
	year=ppdr->year;
	julDay=ppdr->gmt_day;  
	date_sec2hms(ppdr->gmt_sec,&time);
	return utc2gha(year,julDay,time.hour,time.min,time.sec);*/
}

/*Get_timeDelta: Return the time difference, in seconds, between 
the start of the state vectors (in the PPDR) and the start of 
the image (as described in the DSSR).  Write this time to the
given meta_parameters->stVec structure.
*/
double get_timeDelta(ceos_description *ceos,struct pos_data_rec *ppdr,meta_parameters *meta)
{
	ymd_date imgDate;
	julian_date imgJD,stJD;
	hms_time imgTime,stTime;
	double imgSec,stSec;/*Seconds since 1900 for start of image and start of state vectors*/

/*Read time of *start* of state vectors*/
	stJD.year = (int) ppdr->year;
	stJD.jd = (int) ppdr->gmt_day;
	date_sec2hms(ppdr->gmt_sec,&stTime);
	stSec=date2sec(&stJD,&stTime);

/*Read time of *center* of image from DSSR*/
	date_dssr2date(ceos->dssr.inp_sctim,&imgDate,&imgTime);
	date_ymd2jd(&imgDate,&imgJD);
	imgSec=date2sec(&imgJD,&imgTime);

/*Convert scene center time to scene *start* time, by
subtracting off the center line # * the time/line */
	if (!(ceos->facility==VEXCEL && ceos->product==CCSD))
		imgSec-=ceos->dssr.sc_lin*fabs(meta->geo->azPixTime);
	
/*Convert scene center # of seconds back to date/time*/
	sec2date(imgSec,&imgJD,&imgTime);

/*Write image time to meta->stVec structure*/
	meta->stVec->year = (int) imgJD.year;
	meta->stVec->julDay = (int) imgJD.jd;
	meta->stVec->second = date_hms2sec(&imgTime);

/*Return the time between state vector start and image start*/
	return stSec-imgSec;
}


/*Ceos_init_stVec:
	Reads state vectors from given CEOS file, writing
them in the appropriate format to SAR parameters structure.*/
void ceos_init_stVec(char *fName,ceos_description *ceos,meta_parameters *meta)
{
	int i;
	double timeStart=0.0;/*Time of first state vector, relative to image start.*/
	int areInertial=1;/*Flag: are state vectors in non-rotating frame?*/
	int areInertialVelocity=0;/*Flag: have state vectors not been corrected for non-rotating frame?*/
	int areFixedVelocity=0;/*Flag: were state vectors in fixed-earth velocity; but inertial coordinates?*/
	meta_state_vectors *s;
	struct pos_data_rec ppdr;
	
/*Fetch platform position data record.*/
	get_ppdr(fName,&ppdr);
	
/*Allocate output record.*/
	s=meta->stVec=raw_init_state(ppdr.ndata);
	
/*Determine State Vector Format.*/
	if (0==strncmp(ppdr.ref_coord,"INERTIAL",9))
		areInertial=1;/*Listed as Inertial-- believe it.*/
	if (0==strncmp(ppdr.ref_coord,"EARTH CENTERED ROT",18))
		areInertial=0;/*Listed as rotating-- believe it.*/
	if (ppdr.hr_angle<=-99.0)
		areInertial=0;/*Bogus GHA-- must be fixed-earth*/
	
	if (ceos->facility==ASF && ceos->processor==SP2 && ceos->version <=2.50)
	/*The SCANSAR processor made very odd state vectors before 2.51*/
		areInertial=0,areInertialVelocity=1;

/*Fill output record with inital time.*/
	if (ceos->facility==VEXCEL && ceos->product==CCSD)
	{/*VEXCEL CCSD's state vectors don't necessarily start
	at the same time as the image.*/
		timeStart=get_timeDelta(ceos,&ppdr,meta);
	}
	else
	{/*Most Images' state vectors start at the 
	same time as the images themselves.*/
		timeStart=0.0;
		s->year = (int) ppdr.year;
		s->julDay = (int) ppdr.gmt_day;
		s->second = ppdr.gmt_sec;
	} 

/*Fill ouput record with state vectors.*/
	for (i=0;i<s->num;i++)
	{
		/*Read a state vector from the record, fixing the units.*/
		stateVector st;
		st.pos.x=ppdr.pos_vec[i][0];
		st.pos.y=ppdr.pos_vec[i][1];
		st.pos.z=ppdr.pos_vec[i][2];
		vecScale(&st.pos,get_units(vecMagnitude(st.pos),EXPECTED_POS));
		st.vel.x=ppdr.pos_vec[i][3];
		st.vel.y=ppdr.pos_vec[i][4];
		st.vel.z=ppdr.pos_vec[i][5];
		vecScale(&st.vel,get_units(vecMagnitude(st.vel),EXPECTED_VEL));
		
		/*Correct for non-rotating frame.*/
		if (areInertial)
			gei2fixed(&st,ppdr2gha(&ppdr,ceos,i*ppdr.data_int));
		
		/*Perform velocity fixes.*/
		if (areInertialVelocity)
			gei2fixed(&st,0.0);
		else if (areFixedVelocity)
			fixed2gei(&st,0.0);
		
		/*Write out resulting state vectors.*/
		s->vecs[i].vec=st;
		s->vecs[i].time=timeStart+i*ppdr.data_int;
	}
}


