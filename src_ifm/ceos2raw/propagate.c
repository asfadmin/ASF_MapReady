/*************************************************
ASF-STEP Level zero utilites:

lzers2raw:
	Converts a Level-0 Signal product into
a raw input file, suitable for use with AISP.
Switches to work with ERS, JERS, or RADARSAT.

0.8  Orion Lawlor, 8/3/98
0.9  O. Lawlor, 3/99
*/
#include "asf.h"
#include "decoder.h"
#include "dateUtil.h"
#include <unistd.h>

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

/*Prototype for asf_ceos.a routine:*/
meta_state_vectors *raw_init_state(int nStVec);

/*Get the GHA corresponding to the given number
of seconds since 1900.*/
double sec2gha(double sec)
{
	julian_date jd;
	hms_time hms;
	sec2date(sec,&jd,&hms);
	return utc2gha(jd.year,jd.jd,hms.hour,hms.min,hms.sec);
}
/*Write the given seconds-since-1900 value as
Year
month
day
seconds
to the given file.*/
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

/*Propagate the given (fixed-earth) state vector from 
the given time to the next given time.*/
stateVector propagate(stateVector source,double sourceSec,double destSec)
{
	stateVector ret;
	char inBuf[256];
	double ignored;
	int i;
	FILE *asapIn,*asapOut;
/*Convert input state vector to inertial coordinates*/
	fixed2gei(&source,sec2gha(sourceSec));
#define asapInN "asap.in"
#define asapOutN "asap.out"
/*Create ASAP Input File*/
	asapIn=FOPEN(asapInN,"w");
	fprintf(asapIn,"%f\n%f\n%f\n%f\n%f\n%f\n",
		source.pos.x/1000.0,source.pos.y/1000.0,source.pos.z/1000.0,
		source.vel.x,source.vel.y,source.vel.z);
	printYMDS_date(sourceSec,asapIn);
	printYMDS_date(destSec,asapIn);
	fprintf(asapIn,"1\n");/*One step.*/
	FCLOSE(asapIn);
/*Call Propagate*/
	system("propagate "asapInN" "asapOutN);

/*Read ASAP output file*/
	asapOut=FOPEN(asapOutN,"r");
	fgets(inBuf,256,asapOut);/*Skip echo of input vector*/
	fgets(inBuf,256,asapOut);/*Read propagated output vector*/
	for (i=strlen(inBuf)-1;i>=0;i--)
		if (inBuf[i]=='D')
			inBuf[i]='E';/*Replace 12.3D3 with 12.3E3*/
	sscanf(inBuf,"%lg%lg%lg%lg%lg%lg%lg",&ignored,/*Skip time field*/
		&ret.pos.x,&ret.pos.y,&ret.pos.z,
		&ret.vel.x,&ret.vel.y,&ret.vel.z);
	vecScale(&ret.pos,1000.0);/*Convert from km to m*/
	vecScale(&ret.vel,1000.0);/*Convert from km/s to m/s*/
	FCLOSE(asapOut);
/*Remove temporaries.*/
	unlink(asapInN);
	unlink(asapOutN);
/*Convert out state vector to fixed-earth coordinates*/
	gei2fixed(&ret,sec2gha(destSec));
	return ret;
}


/*Propagate the state vectors in the given meta_parameters
structure so they start at the image start.
Make nStVec of them, 30 seconds apart.*/
void propagate_state(meta_parameters *meta,int nStVec)
{
	double data_int=30.0;/*Write out state vectors 30 seconds apart.*/
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
	for (startNo=0;startNo<meta->stVec->num;startNo++)
	{
		state_loc *loc=&(meta->stVec->vecs[startNo]);
		/*Compute the distance between this state vector and image start.*/
		double absSec=fabs(loc->time);
		if (secToClosest>absSec)
		{/*This state vector is closer than any state vector so far.*/
			secToClosest=absSec;
			closestSt=loc->vec;
			closestSec=loc->time;
		}
	}
	
	printf("Nearest state vector is %.2f seconds from image start.\n",secToClosest);
	
	if (closestSec==-1)
		{fprintf(stderr,"Couldn't find any nearby state vectors in propagate_state!\n");exit(1);}
	
/*Compute Seconds since 1900 of Start of Image*/
	img_jd.year=meta->stVec->year;
	img_jd.jd=meta->stVec->julDay;
	date_sec2hms(meta->stVec->second,&img_time);
	
	imgSec=date2sec(&img_jd,&img_time);

/*Propagate nearest state vector to create each output state vector.*/
	new_st=raw_init_state(nStVec);
	new_st->year=img_jd.year;
	new_st->julDay=img_jd.jd;
	new_st->second=date_hms2sec(&img_time);
	for (outNo=0;outNo<nStVec;outNo++)
	{
		double outTime=outNo*data_int;
		new_st->vecs[outNo].time=outTime;
		new_st->vecs[outNo].vec=propagate(closestSt,imgSec+closestSec,imgSec+outTime);
	}

/*Blow away the old state vector structure.*/
	FREE(meta->stVec);
	meta->stVec=new_st;
}
