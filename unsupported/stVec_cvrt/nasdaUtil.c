/*
NasdaUtil:
	A set of utilites for converting
state vectors into the NASDA Flight-agency
format.
*/
#include "asf.h"
#include "geolocate.h"
#include "dateUtil.h"
#include "nasdaUtil.h"

void nasda_createDate(ymd_date *date,hms_time *time,nasda_date *out)
{
	char tmp[100];
	double mjd=date_getJD(date)-2400000.5;
	double dayFrac=date_hms2msec(time)/(DAY2SEC*SEC2MSEC);
	mjd+=dayFrac;
	sprintf(tmp,"%14.8f",mjd);
	strncpy(out->date,tmp,15);
}

void nasda_printHeader(nasda_header *h,int *nTCE,int *nStVec)
{
	char tmp[100];
	printf("NASDA Header:\n"
	"\tdate=%.8s\n"
	"\ttime=%.8s\n"
	"\torbitStart='%.14s'\n"
	"\torbitEnd='%.14s'\n"
	"\tDm2='%.12s'\n"
	"\tDm1='%.12s'\n"
	"\tD  ='%.12s'\n"
	"\tD1 ='%.12s'\n"
	"\tD2 ='%.12s'\n"
	"\tD3 ='%.12s'\n",
	h->date,h->time,
	h->orbitStart.date,h->orbitEnd.date,
	h->D[0].offset,h->D[1].offset,h->D[2].offset,h->D[3].offset,h->D[4].offset,h->D[5].offset);
	if (nTCE!=NULL)
	{
		strncpy(tmp,h->noTCE,3);tmp[3]=0;
		sscanf(tmp,"%d",nTCE);
	}
	if (nStVec!=NULL)
	{
		int i;
		*nStVec=0;
		for (i=0;i<6;i++)
		{
			int num;
			strncpy(tmp,h->D[i].num,4);tmp[4]=0;
			sscanf(tmp,"%d",&num);
			*nStVec+=num;
		}
	}
}

void nasda_initHeader(nasda_header *h,ymd_date *date,hms_time *time,
	nasda_date *orbitStart,nasda_date *orbitEnd,int nStVec,int nTCE)
{
	nasda_loc endLoc;
	char tmp[100];
	strncpy(h->fname,"ELMF    ",8);
	strncpy(h->satName,"ERS1",4);
	strncpy(h->from,"HMMO",4);
	strncpy(h->to,"FAIS",4);
	date_printDate(date,0,h->date);
	date_printTime(time,0,':',h->time);
	h->hasFdesc='Y';
	strncpy(h->recLen,"0085",4);
	sprintf(tmp,"%5d",nStVec+nTCE);strncpy(h->noRec,tmp,5);
	strncpy(h->spare,
		"                                                                 "
		"                                                                 "
		"                                                                 "
		,82);
	sprintf(tmp,"%3d",nTCE);strncpy(h->noTCE,tmp,3);
	h->orbitStart=*orbitStart;
	h->orbitEnd=*orbitEnd;
	
	date_printDate(date,0,h->updated);
	
	sprintf(tmp,"%8d",NASDA_HEADER_LEN+nTCE*NASDA_TCE_LEN);strncpy(h->D[0].offset,tmp,8);
	sprintf(tmp,"%4d",nStVec);strncpy(h->D[0].num,tmp,4);
	
	sprintf(tmp,"%8d",NASDA_HEADER_LEN+nTCE*NASDA_TCE_LEN+NASDA_STVEC_LEN*nStVec);strncpy(endLoc.offset,tmp,8);
	sprintf(tmp,"%4d",0);strncpy(endLoc.num,tmp,4);
	h->D[1]=h->D[2]=h->D[3]=h->D[4]=h->D[5]=endLoc;
	
	strncpy(h->spare2,"                      ",17);
}

void nasda_initTCE(nasda_tce *t,ymd_date *date,hms_time *time,int msOff,
	ymd_date *preset_date,hms_time *preset_time,int presetOff)
{
	char tmp[100];
	date_printDate(date,0,t->regDate);
	date_printY2Kdate(preset_date,'-',t->presetDate);
	t->space=' ';
	date_printTime(preset_time,3,':',t->presetTime);
	sprintf(tmp,"%10d",presetOff);strncpy(t->satTime,tmp,10);
	sprintf(tmp,"%5d",msOff);strncpy(t->timeErr,tmp,5);
	date_printY2Kdate(date,'-',t->passDate);
	t->space2=' ';
	date_printTime(time,3,':',t->passTime);
	
	strncpy(t->pathNo," 99",3);/*BOGUS, HARDCODED path number.*/
	
	strncpy(t->spare,
		"                                                                 "
		,17);
}

void nasda_printTCE(nasda_tce *t)
{
	printf("NASDA TCE:\n"
	"\t%.85s\n",(char *)t);
}

void nasda_printStVec(nasda_stVec *v)
{
	printf("NASDA State Vector:\n"
	"\tDate: '%.14s'\n"
	"\tPos x: '%.13s' Km\n"
	"\tPos y: '%.13s' Km\n"
	"\tPos z: '%.13s' Km\n"
	"\tVel x: '%.10s' Km/s\n"
	"\tVel y: '%.10s' Km/s\n"
	"\tVel z: '%.10s' Km/s\n"
	"\tSpare:'%.2s'\n",
		v->time.date,
		v->pos[0],v->pos[1],v->pos[2],
		v->vel[0],v->vel[1],v->vel[2],
		v->spare);
}

void nasda_initStVec(nasda_stVec *v,stateVector *s,nasda_date *date)
{
	char tmp[100];
	v->time=*date;
	sprintf(tmp,"%13.6f",s->pos.x/1000.0);strncpy(v->pos[0],tmp,13);
	sprintf(tmp,"%13.6f",s->pos.y/1000.0);strncpy(v->pos[1],tmp,13);
	sprintf(tmp,"%13.6f",s->pos.z/1000.0);strncpy(v->pos[2],tmp,13);
	sprintf(tmp,"%10.6f",s->vel.x/1000.0);strncpy(v->vel[0],tmp,10);
	sprintf(tmp,"%10.6f",s->vel.y/1000.0);strncpy(v->vel[1],tmp,10);
	sprintf(tmp,"%10.6f",s->vel.z/1000.0);strncpy(v->vel[2],tmp,10);
	strncpy(v->spare,"  ",2);
}
