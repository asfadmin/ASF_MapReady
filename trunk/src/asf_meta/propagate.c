#include <assert.h>
#include <unistd.h>

#include "asf.h"
#include "asf_meta.h"
#include "dateUtil.h"
#include "orbital_state_vector.h"
#include "propagate.h"

/* allocation routine for meta_state_vectors */
meta_state_vectors *meta_state_vectors_init(int num_of_vectors);



void printStVec(const char *desc,stateVector s,FILE *f)
{
	fprintf(f,"%s:\n"
		"pos.x=%.2f m\n"
		"pos.y=%.2f m\n"
		"pos.z=%.2f m\n"
		"vel.x=%.2f m/s\n"
		"vel.y=%.2f m/s\n"
		"vel.z=%.2f m/s\n\n",
		desc,
		s.pos.x,s.pos.y,s.pos.z,
		s.vel.x,s.vel.y,s.vel.z);
}

/*******************************************************************************
 * Get the GHA corresponding to the given number of seconds since 1900.*/
double sec2gha(double sec)
{
	julian_date jd;
	hms_time hms;
	sec2date(sec,&jd,&hms);
	return utc2gha(jd.year,jd.jd,hms.hour,hms.min,hms.sec);
}

/*******************************************************************************
 * Write the given seconds-since-1900 value as
 * Year
 * month
 * day
 * seconds
 * to the given file.*/
void printYMDS_date(double sec,FILE *dest)
{
	julian_date jd;
	ymd_date ymd;
	hms_time hms;
	double secOfDay;
	sec2date(sec,&jd,&hms);
	date_jd2ymd(&jd,&ymd);
	secOfDay=date_hms2sec(&hms);
	fprintf(dest,"%d\n",ymd.year);
	fprintf(dest,"%d\n",ymd.month);
	fprintf(dest,"%d\n",ymd.day);
	fprintf(dest,"%f\n",secOfDay);
}

/*****************************************************************************
 * Propagate the given (fixed-earth) state vector from the given time to the
 * next given time.*/
stateVector propagate(stateVector source,double sourceSec,double destSec)
{
	stateVector ret;
	OrbitalStateVector *osv;
/*Convert input state vector to inertial coordinates*/
	fixed2gei(&source,sec2gha(sourceSec));
	osv = orbital_state_vector_new (source.pos.x, source.pos.y, 
					source.pos.z, source.vel.x, 
					source.vel.y, source.vel.z);
	orbital_state_vector_propagate (osv, destSec - sourceSec);
	ret.pos.x = osv->position->x;
	ret.pos.y = osv->position->y;
	ret.pos.z = osv->position->z;
	ret.vel.x = osv->velocity->x;
	ret.vel.y = osv->velocity->y;
	ret.vel.z = osv->velocity->z;
	vector_free (osv->position);
	vector_free (osv->velocity);
	free (osv);

	/*Convert out state vector to fixed-earth coordinates*/
	gei2fixed(&ret,sec2gha(destSec));
	return ret;
}

/*******************************************************************************
 * Propagate the state vectors in the given meta_parameters structure so they
 * start at the image start. Make nStVec of them, data_int seconds apart.*/
void propagate_state(meta_parameters *meta,int nStVec, double data_int)
{
	julian_date img_jd;
	hms_time img_time;
	double imgSec;
	
	double secToClosest=100000000000.0;
	stateVector closestSt;
	double closestSec=-1;
	
	int startNo;/*Number of starting state vector*/
	int outNo;/*Number of output state vector*/
	meta_state_vectors *new_st;/*Freshly-created state vector structure.*/

/*Search the list of state vectors for the one nearest to the image start.*/
	for (startNo=0;startNo<meta->state_vectors->vector_count;startNo++)
	{
		state_loc *loc=&(meta->state_vectors->vecs[startNo]);
		/*Compute the distance between this state vector and image start.*/
		double absSec=fabs(loc->time);
		if (secToClosest>absSec)
		{/*This state vector is closer than any state vector so far.*/
			secToClosest=absSec;
			closestSt=loc->vec;
			closestSec=loc->time;
		}
	}
/*
	if (!quietflag) printf("   Nearest state vector is %.2f seconds from image start.\n",secToClosest);
*/
	if (closestSec==-1) {
	  sprintf(errbuf,"   ERROR: Couldn't find any nearby state vectors in propagate_state!\n");
	  printErr(errbuf);
	}
	
/*Compute Seconds since 1900 of Start of Image*/
	img_jd.year = meta->state_vectors->year;
	img_jd.jd   = meta->state_vectors->julDay;
	date_sec2hms(meta->state_vectors->second,&img_time);
	
	imgSec=date2sec(&img_jd,&img_time);

/*Propagate nearest state vector to create each output state vector.*/
	new_st = meta_state_vectors_init(nStVec);
	new_st->year   = img_jd.year;
	new_st->julDay = img_jd.jd;
	new_st->second = date_hms2sec(&img_time);

	for (outNo=0;outNo<nStVec;outNo++)
	{
		double outTime=outNo*data_int;
		new_st->vecs[outNo].time=outTime;
		new_st->vecs[outNo].vec=propagate(closestSt,imgSec+closestSec,imgSec+outTime);
	}

/*Blow away the old state vector structure.*/
	FREE(meta->state_vectors);
	meta->state_vectors = new_st;
}
