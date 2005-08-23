/*RADARSAT Auxiliary data decoding utilites.*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"


/*******************************************
DecodeAux:
	Converts a raw satellite downlink (AUX)
with nasty bit-wise fields and odd meanings
to an interpreted structure (decodedAux)
with clean floating-point fields and clear meanings.
*/

void RSAT_decodeAux(RSAT_raw_aux *in,RSAT_aux *out)
{
	char *beamIdent[21]=CONF_RSAT_beamNameList;/*Beam name, indexed by beam number.*/
	int beamSeq;
	
	out->payloadStatus=GET_SAT_USHORT(in->PayloadStatus);
	out->isImaging=(0x04==(0x07&(in->PayloadStatus.Msb>>5)));
	out->replicaAGC=in->ReplicaAGC;
	out->rxAGC=in->RxAGCSetting;
	out->hasReplica=in->ReplicaPresent;
	out->nBeams=1+in->NoOfBeams;
	beamSeq=GET_SAT_USHORT(in->BeamSequence);
	if (out->nBeams==1)
	{
		/*Stripmap*/
		out->beams[0]=beamSeq&0x1f;
		out->beams[1]=out->beams[2]=out->beams[3]=0;
	} else {
		/*ScanSAR*/
		out->beams[0]=beamSeq&0x0f;
		out->beams[1]=(beamSeq>>4)&0x0f;
		out->beams[2]=(beamSeq>>8)&0x0f;
		out->beams[3]=(beamSeq>>12)&0x0f;
	}
	for (beamSeq=0;beamSeq<4;beamSeq++)
		out->beamNames[beamSeq]=beamIdent[out->beams[beamSeq]];
	out->beam=in->BeamSelect;
	
	switch(in->ADCSamplingRate)
	{
		case 0:
			out->timeBase=4.0/CONF_RSAT_SMO;break;
		case 1:
			out->timeBase=7.0/CONF_RSAT_SMO;break;
		case 2:
			out->timeBase=10.0/CONF_RSAT_SMO;break;
		case 3:
			printf("   Error! Unrecognized ADC conversion rate %d!\n",in->ADCSamplingRate);
	}
	
	out->prf_code=(((int)in->PRFBeam1)<<5)|(in->PRFBeam2);
	out->prf=1/((2+out->prf_code)*6*out->timeBase);
	out->dwp_code=(((int)in->RxWindowStartTime1)<<4)|(in->RxWindowStartTime2);
	out->dwp=(5+out->dwp_code)*6*out->timeBase;
	out->rxLen=(1+((((int)in->RxWindowDuration1)<<4)|(in->RxWindowDuration2)))*6;
	out->windowDuration=out->rxLen*out->timeBase;
	out->replicaLen=44.559E-06/out->timeBase;
	
	{/*Sort out time of aquisition, with annoying bit-twiddling*/
		struct {
			unsigned char daysHi:8;
			unsigned char daysLo:3;
			unsigned char secHi:5;
			unsigned char secMid:8;
			unsigned char secLo:4;
			unsigned char millisHi:4;
			unsigned char millisLo:6;
			unsigned char microsHi:2;
			unsigned char microsLo:8;
		} rsat_time;/*48-bit (6-byte) field */
		int day,sec,millis,micros;
		memcpy((void *)&rsat_time,in->Time,6);
		day=(((int)rsat_time.daysHi)<<3)+(((int)rsat_time.daysLo));
		sec=(((int)rsat_time.secHi)<<12)+(((int)rsat_time.secMid)<<4)+
			(((int)rsat_time.secLo));
		millis=(((int)rsat_time.millisHi)<<6)+(((int)rsat_time.millisLo));
		micros=(((int)rsat_time.microsHi)<<8)+(((int)rsat_time.microsLo));
		
		out->days=day;
		out->seconds=sec+(millis+micros/1024.0)/1000.0;
	}
}

/****************************************************
auxPrint:
	Writes the "important" values from an auxiliary data
record to the given file.
*/

void RSAT_auxPrint(RSAT_aux *aux,FILE *f)
{
	
	fprintf(f,"\n   Replica_AGC: %f\tRx_AGC: %f\tprf %f\n\trxStart %e\trxDur %e\n",
		aux->replicaAGC,aux->rxAGC,aux->prf,aux->dwp,aux->windowDuration);
	
	fprintf(f,"   fs=%e;timeBase=%e;rxLen=%d\n",1.0/aux->timeBase,aux->timeBase,aux->rxLen);
	
	fprintf(f,"   HasReplica %d; replicaLen %d; imaging=%d; nBeams %d; beam %d\n",
		aux->hasReplica,aux->replicaLen,RSAT_auxIsImaging(aux),
			aux->nBeams,aux->beam);
	fprintf(f,"   Beams=%s/%s/%s/%s\n",aux->beamNames[0],aux->beamNames[1],aux->beamNames[2],aux->beamNames[3]);
	fprintf(f,"   Day %d; Second %f of day\n",aux->days,aux->seconds);
}

/****************************************************
auxUpdate:
	Updates values in satellite record
based on values in given raw auxiliary data record.
*/

void RSAT_auxUpdate(RSAT_aux *aux,bin_state *s)
{
	int beamNo=0;
	int nPulseInAirBeam[21]=CONF_RSAT_pulsesInAirList;/*Number of pulses in air, indexed by beam number.*/
	
	s->nBeams=aux->nBeams;
	s->nPulseInAir=nPulseInAirBeam[aux->beams[beamNo]];
	s->nSamp=aux->rxLen;
	s->nValid=aux->rxLen-aux->replicaLen;
	s->fs=1.0/aux->timeBase;
	
	if (s->fs>30e6)
	{/*Very fast sampling freqency-- fine beam at 32.317 MHz, 30.30 MHz used*/
		s->azres=9.0;/*Very small azimuth resolution*/
		s->nLooks=1;/*Fine beam data takes no looks.*/
		s->slope=CONF_RSAT_slopeFast;/*Fine beam uses almost all of the available bandwidth*/
	}
	else if (s->fs>15e6)
	{/*Slowish sampling freqency-- 18.4669 MHz, 17.48 MHz used*/
		s->azres=12.0;/*Larger azimuth resolution*/
		s->nLooks=3;
		s->slope=CONF_RSAT_slopeMed;
	}else
	{/*Slowest sampling freqency-- 12.9268 MHz, 11.78 MHz used*/
		s->azres=12.0;/*Larger azimuth resolution*/
		s->nLooks=3;
		s->slope=CONF_RSAT_slopeSlow;
	}
	
/*Compute slant range to first pixel:*/
	s->prf=aux->prf;
	s->prf_code=aux->prf_code;
/*DWP=SWST+#in air/PRF-characteristic transmission delay.
I checked the satellite transmission delay (fudge factor)
using the ST6 scene over Delta Junction from orbit 8848.*/
	s->dwp_code=aux->dwp_code;
	s->dwp=aux->dwp;
	s->range_gate=s->dwp+s->nPulseInAir/aux->prf-CONF_RSAT_rangePulseDelay;
	
	s->time_code=aux->seconds;
	
	printf("   %d pulses in air; beam %d\n",s->nPulseInAir,aux->beams[0]);
	
	strcpy(s->beamMode,aux->beamNames[aux->beam]);
	
	RSAT_auxPrint(aux,stdout);
	
}

/****************************************************
auxGet routines:
	These extract a single parameter from the RADARSAT auxiliary data.
*/

int RSAT_auxHasReplica(RSAT_aux *aux)
{
	return aux->hasReplica;
}
int RSAT_auxGetBeam(RSAT_aux *aux)
{
	return aux->beam;
}
int RSAT_auxIsImaging(RSAT_aux *aux)
{
	if (aux->isImaging==0)
		printf("Warning!! Not imaging!!\n");
	return aux->isImaging;
}

/**************************************
auxAGC_window:
        call updateAGC_window with satellite
AGC and window position for this line.
*/
void RSAT_auxAGC_window(bin_state *s,RSAT_aux *aux)
{
        updateAGC_window(s,db2amp(aux->rxAGC),aux->dwp*s->fs);
}
