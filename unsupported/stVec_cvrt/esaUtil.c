/*
esaUtil:
	Provides utilites for decoding, encoding, and displaying
ESA state vector files.
*/
#include "asf.h"
#include "asf_endian.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "esaUtil.h"

void esa_init_header(esa_header *h,char *satellite,ymd_date *date,hms_time *time,int isPredicted)
{
	char *fileId;
	if (isPredicted==1)
		fileId="ORPD_";
	else
		fileId="ORRE_";
	strncpy(h->fileID,fileId,5);
	date_printY2Kdate(date,0,h->genDate);
	strncpy(h->orig,"CF",2);
	strncpy(h->dest,"AF",2);
	strncpy(h->counter,"9999",4);
	h->period='.';
	strncpy(h->sat,satellite,2);
	date_printTime(time,0,':',h->genTime);
}
void esa_print_header(esa_header *h)
{
	printf("ESA Header:\n"
	"\tGeneration Date:'%.6s'\n"
	"\tValid? '%s'\n"
	"\tSatellite '%.2s'\n"
	"\tGeneration Time:'%.8s'\n\n",
	h->genDate,h->period=='.'?"Yes":"No!",
	h->sat,h->genTime);
}

void esa_init_stVec(esa_stVec *v,stateVector *s,int orbit,ymd_date *date,hms_time *time)
{
	char tmp[100];
	julian_date jdate;
	sprintf(tmp,"%5d",orbit);strncpy(v->orbit,tmp,5);
	strncpy(v->tai_utc," 00",3);
	date_ymd2jd(date,&jdate);
	lilInt32_out(date_getMJD(&jdate),v->time_mjd);
	lilInt32_out(date_hms2msec(time),v->time_msec);
	lilInt32_out(s->pos.x*ESA_STVEC_POS,v->stVec_pos[0]);
	lilInt32_out(s->pos.y*ESA_STVEC_POS,v->stVec_pos[1]);
	lilInt32_out(s->pos.z*ESA_STVEC_POS,v->stVec_pos[2]);
	lilInt32_out(s->vel.x*ESA_STVEC_VEL,v->stVec_vel[0]);
	lilInt32_out(s->vel.y*ESA_STVEC_VEL,v->stVec_vel[1]);
	lilInt32_out(s->vel.z*ESA_STVEC_VEL,v->stVec_vel[2]);
}
void esa_print_stVec(esa_stVec *v)
{
	printf("ESA State Vector:\n"
	"\tOrbit: '%.5s'\n"
	"\tTAI-UTC Difference: '%.3s'\n"
	"\tMJD: %d / Msec of Day: %.3f\n"
	"\tStVec: Position:\n"
	"\t\tx=%f\n"
	"\t\ty=%f\n"
	"\t\tz=%f\n"
	"\tStVec: Velocity:\n"
	"\t\tx=%f\n"
	"\t\ty=%f\n"
	"\t\tz=%f\n"
	"\n\n",v->orbit,v->tai_utc,
		lilInt32(v->time_mjd),
		lilInt32(v->time_msec)/1000.0,
		lilInt32(v->stVec_pos[0])/ESA_STVEC_POS,
		lilInt32(v->stVec_pos[1])/ESA_STVEC_POS,
		lilInt32(v->stVec_pos[2])/ESA_STVEC_POS,
		lilInt32(v->stVec_vel[0])/ESA_STVEC_VEL,
		lilInt32(v->stVec_vel[1])/ESA_STVEC_VEL,
		lilInt32(v->stVec_vel[2])/ESA_STVEC_VEL);
}

