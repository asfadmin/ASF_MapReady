/*************************************************
ASF-STEP  utilites:

stVec_ers.c:
	Converts state vectors from ASF internal format
into ESA internal format.

1.0  Orion Lawlor, 8/21/98
*/
#include "asf.h"
#include "geolocate.h"
#include "asf_endian.h"
#include "dateUtil.h"
#include "odlUtil.h"
#include "odlTceUtil.h"
#include "esaUtil.h"
#include "nasdaUtil.h"
#include <math.h>

#define VERSION 0.8

/*ERS: Copy a RSV ODL file to ESA State Vector format.*/
void esa_convert(char *inName,char *outName)
{
	int i;
	FILE *f_in,*f_out;
	stateVector stVec;
	int orbitNo;
	esa_header head;
	ymd_date date;
	hms_time time;
	char satName[100];
	int isPred;
	
	f_in=rsv_open(inName,&date,&time,satName,&isPred);
	f_out=FOPEN(outName,"wb");
	
	esa_init_header(&head,satName,&date,&time,isPred);
	FWRITE(&head,1,ESA_HEADER_LEN,f_out);
	esa_print_header(&head);
	
	while (rsv_next(f_in,&orbitNo,&date,&time,&stVec))
	{
		esa_stVec esa_st;
		double gha;
		
		julian_date jdate;
		date_ymd2jd(&date,&jdate);
		gha=utc2gha(jdate.year,jdate.jd,time.hour,time.min,time.sec);
		gei2fixed(&stVec,gha);
		
		esa_init_stVec(&esa_st,&stVec,orbitNo,&date,&time);
		FWRITE(&esa_st,1,ESA_STVEC_LEN,f_out);
		esa_print_stVec(&esa_st);
	}
	
	rsv_close(f_in);
	FCLOSE(f_out);
}

/*JERS: Copy a RSV ODL file to NASDA State Vector format.*/
void nasda_convert(char *inNameSt,char *inNameTce,char *outName)
{
	int i;

#define MAX_STVEC 100000
	int nStVec;
	stateVector stVec[MAX_STVEC];
	ymd_date stDate[MAX_STVEC];
	hms_time stTime[MAX_STVEC];
	ymd_date init_date;
	hms_time init_time;
	
#define MAX_TCE 1000
	int nTCE;
	int tceSec[MAX_TCE],tceMsec[MAX_TCE];
	ymd_date tceDate[MAX_TCE];
	hms_time tceTime[MAX_TCE];
	
	FILE *f_in,*f_out;
	int orbitNo;
	nasda_header head;
	char satName[100];
	nasda_date firstDate,lastDate;
	
/*Read in all time correlation elements.*/
	f_in=tce_open(inNameTce);
	
	nTCE=0;
	while (tce_next(f_in,&orbitNo,&(tceDate[nTCE]),&(tceTime[nTCE]),
			&(tceSec[nTCE]),&(tceMsec[nTCE])))
		nTCE++;
	
	tce_close(f_in);
	
/*Read in all state vectors.*/
	f_in=rsv_open(inNameSt,&init_date,&init_time,satName,NULL);
	
	nStVec=0;
	while (rsv_next(f_in,&orbitNo,&(stDate[nStVec]),&(stTime[nStVec]),&(stVec[nStVec])))
		nStVec++;
	
	rsv_close(f_in);
	
/*Open output file and write each out.*/
	f_out=FOPEN(outName,"wb");
	nasda_createDate(&stDate[0],&stTime[0],&firstDate);
	nasda_createDate(&stDate[nStVec-1],&stTime[nStVec-1],&lastDate);
	nasda_initHeader(&head,&init_date,&init_time,&firstDate,&lastDate,nStVec,nTCE);
	FWRITE(&head,1,NASDA_HEADER_LEN,f_out);
	nasda_printHeader(&head,NULL,NULL);
	
	for (i=0;i<nTCE;i++)
	{
		nasda_tce t;
		nasda_initTCE(&t,&(tceDate[i]),&(tceTime[i]),tceMsec[i],
			&(tceDate[0]),&(tceTime[0]),tceSec[0]);
		FWRITE(&t,1,NASDA_TCE_LEN,f_out);
		nasda_printTCE(&t);
	}
	
	for (i=0;i<nStVec;i++)
	{
		nasda_stVec st;
		nasda_date nasDate;
		nasda_createDate(&(stDate[i]),&(stTime[i]),&nasDate);
		nasda_initStVec(&st,&(stVec[i]),&nasDate);
		FWRITE(&st,1,NASDA_STVEC_LEN,f_out);
		nasda_printStVec(&st);
	}
	
	FCLOSE(f_out);
	
}
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
#ifndef ERS
#define ERS 0
#endif
#ifndef JERS
#define JERS 0
#endif
#ifndef DUMP
#define DUMP 0
#endif

#if ERS
 #if DUMP
/*Testing: Dump a given ESA State Vector file and exit.*/
	FILE *f;
	unsigned char buf[1000];
	if (argc!=2)
		{printf("Usage: stVec_ers <parseThis>\n");exit(1);}
	f=FOPEN(argv[1],"rb");
	FREAD(buf,1,ESA_HEADER_LEN,f);
	esa_print_header((esa_header *)buf);
	while (1)
	{
		FREAD(buf,1,ESA_STVEC_LEN,f);
		esa_print_stVec((esa_stVec *)buf);
	}
 #else
/*Normal ERS: Copy a RSV ODL file to ESA State Vector format.*/
	
	if (argc!=3)
		{printf("Usage: stVec_ers <inName> <outName>\n");exit(1);}
	
	esa_convert(argv[1],argv[2]);
 #endif
#elif JERS
 #if DUMP
/*Testing: Dump a given JERS State Vector file and exit.*/
	FILE *f;
	int nTCE,nStVec,i;
	unsigned char buf[1000];
	if (argc!=2)
		{printf("Usage: stVec_jers <parseThis>\n");exit(1);}
	f=FOPEN(argv[1],"rb");
	FREAD(buf,1,NASDA_HEADER_LEN,f);
	nasda_printHeader((nasda_header *)buf,&nTCE,&nStVec);
	for (i=0;i<nTCE;i++)
	{
		FREAD(buf,1,NASDA_TCE_LEN,f);
		nasda_printTCE((nasda_tce *)buf);
	}
	for (i=0;i<nStVec;i++)
	{
		FREAD(buf,1,NASDA_STVEC_LEN,f);
		nasda_printStVec((nasda_stVec *)buf);
	}
 #else
/*Normal JERS: Copy a RSV ODL file to NASDA State Vector format.*/
	if (argc!=4)
		{printf("Usage: stVec_jers <inNameSt> <inNameTCE> <outName>\n");exit(1);}
	
	nasda_convert(argv[1],argv[2],argv[3]);
 #endif	
#else
/*Date testing.*/
	int year=1992;
	int jd;
	int month,day;
	for (jd=1;jd<=355;jd++)
	{
		double julian;
		date_jd2monthDay(year,jd,&month,&day);
		julian=date_getJD(year,month,day);
		printf("JD=%d; Month=%d; Day=%d; True Julian=%.2f\n",jd,month,day,julian);
	}
#endif
	return 0;
}
