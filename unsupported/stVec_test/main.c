/*************************************************
ASF-STEP  utilites:

stVec_ers.c:
	Converts state vectors from ASF internal format
into ESA internal format.

1.0  Orion Lawlor, 8/21/98
*/
#include "asf.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "odlUtil.h"

#define VERSION 0.8

FILE *f_in,*f_out;
stateVector stVec_start;
ymd_date date_start;
hms_time time_start;

double diffMag(vector a,vector b)
{
	vector diff;
	vecSub(a,b,&diff);
	return vecMagnitude(diff);
}

/*Compare state vectors a and b*/
void cmpSt(stateVector a,stateVector b)
{
	fprintf(f_out,"%.3fm %.3fm/s \n",
		diffMag(a.pos,b.pos),diffMag(a.vel,b.vel));
}

/** get_gha: returns the Greenwhich Hour Angle (degrees)
at timeDelta seconds after date, time*/
double get_gha(double timeDelta)
{
	double gha;
	julian_date jdate;
	date_ymd2jd(&date_start,&jdate);
	gha=utc2gha(jdate.year,jdate.jd,
		time_start.hour,time_start.min,time_start.sec+timeDelta);
	return gha;
}

/** Asap_read: Reads a single state vector from given
string in ASAP output file format.**/
stateVector asap_read(char *buf,double *time)
{
	stateVector ret;
	int i;
/*Replace "D" with "e"*/
	for (i=strlen(buf)-1;i>=0;i--)
		if (buf[i]=='D')
			buf[i]='e';
	
	if (7!=sscanf(buf,"%lg%lg%lg%lg%lg%lg%lg",time,
		&ret.pos.x,&ret.pos.y,&ret.pos.z,
		&ret.vel.x,&ret.vel.y,&ret.vel.z))
		{printf("Couldn't read 7 floats from asap output file\n");
		exit(1);}
	vecScale(&ret.pos,1000.0);/*Convert back to m,from km*/
	vecScale(&ret.vel,1000.0);/*Convert back to m/s,from km/s*/
	return ret;	
}

/** Asap_propagate: calls asap to propagate stVec_start
to timeDelta seconds later.  Returns nArr equally-spaced 
state vectors in outArr. outArr[0] will be at time zero,
outArr[nArr-1] will be at time timeDelta.**/
void asap_propagate(double timeDelta,stateVector *outArr,int nArr)
{
	double timeofday=date_hms2msec(&time_start)/1000.0;
	double ignored;
	char buf[1000];
	int i;
	
	/*Create asap input file*/
	FILE *asap=FOPEN("asap_in","w");
	fprintf(asap,
		"%f\n%f\n%f\n%f\n%f\n%f\n" /*GEI State vector, km and m/s*/
		"%i\n%i\n%i\n%f\n" /*Starting time*/
		"%i\n%i\n%i\n%f\n" /*Ending time*/
		"%i\n" /*# of steps*/
		"%f\n"/*GHA*/,
		stVec_start.pos.x/1000.0,stVec_start.pos.y/1000.0,stVec_start.pos.z/1000.0,
		stVec_start.vel.x,stVec_start.vel.y,stVec_start.vel.z,
		date_start.year,date_start.month,date_start.day,timeofday,
		date_start.year,date_start.month,date_start.day,timeofday+timeDelta,
		(int)(nArr-1),
		get_gha(timeDelta));
	FCLOSE(asap);
	
	/*Run asap*/
	system("propagate asap_in asap_out");
	
	/*Open output file, extract each state vector*/
	asap=FOPEN("asap_out","r");
	i=0;
	while (NULL!=fgets(buf,1000,asap))
	{
		outArr[i++]=asap_read(buf,&ignored);
		if (i>=nArr) break;
	}
	FCLOSE(asap);
	unlink("asap_out");
	unlink("asap_in");
	return;
}

/*Compare: */

void compare(stateVector *prop,int end)
{
	int i;
	double posMax=0,velMax=0;
	int maxDex=0;
	fprintf(f_out,"\nInterpolating over %d seconds...\n",end);
	for (i=0;i<end;i++)
	{
		stateVector interp;
		
		interp_stVec(&prop[0],0.0,&prop[end],end,
			&interp,i);
		if (posMax<diffMag(interp.pos,prop[i].pos))
		{
			posMax=diffMag(interp.pos,prop[i].pos);
			maxDex=i;
		}
		if (velMax<diffMag(interp.vel,prop[i].vel))
			velMax=diffMag(interp.vel,prop[i].vel);
	}
	fprintf(f_out,"\tMaximum Position Error=%.6fm, at %i seconds\n",posMax,maxDex);
	fprintf(f_out,"\tMaximum Velocity Error=%.6fm/s\n",velMax);
}

void test(char *inName,char *outName)
{
	char satName[100];
	ymd_date date;
	hms_time time;
	int orbitNo;
	int isPred,i;
#define nStVec (8*60+1)
	stateVector next,prop[nStVec];
	
	f_in=rsv_open(inName,&date_start,&time_start,satName,&isPred);
	f_out=FOPEN(outName,"w");
	
	rsv_next(f_in,&orbitNo,&date_start,&time_start,&stVec_start);
	
	asap_propagate(8.0*60,prop,nStVec);
	rsv_next(f_in,&orbitNo,&date,&time,&next);
/*Compare the last propagated state vector to the next
listed in the file*/
	cmpSt(prop[nStVec-1],next);
	
/*Put the state vectors in Earth-Fixed coordinates*/
	for (i=0;i<nStVec;i++)
		gei2fixed(&prop[i],get_gha((double)i));
	
/*Compare the propagated and interpolated state vectors:*/
	for (i=2;i<nStVec;i=(int)(i*1.5))
		compare(prop,i);
	
	rsv_close(f_in);
	FCLOSE(f_out);
}
	/* Intertial -> fixed
	double gha;
	date_ymd2jd(&date_orig,&jdate);
	julian_date jdate;
	gha=utc2gha(jdate.year,jdate.jd,
		time_orig.hour,time_orig.min,time_orig.sec);
	gei2fixed(&stVec,gha);*/
/*****************************
main:
	Open input file.
	read input header
	write output header
	Read each input state vector
	write each output state vector
*/
int main(int argc,char *argv[])
{
	if (argc!=3)
		{printf("Usage: stVec_test <inName> <outName>\n");exit(1);}
	
	test(argv[1],argv[2]);
	return 0;
}
