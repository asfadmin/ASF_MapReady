/*ERS Auxiliary data decoding utilites.*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"

/*******************************************
DecodeAux:
	Converts a raw satellite downlink (raw_aux)
with nasty bit-wise fields and odd meanings
to an interpreted structure (aux)
with clean floating-point fields and clear meanings.
*/

void ERS_decodeAux(ERS_raw_aux *in,ERS_aux *out)
{
	out->isImaging=(5==(0x07&(in->activity>>5)));
	out->onBoardTime=GET_SAT_ULONG(in->onBoardTime);
	out->formatCount=GET_SAT_ULONG(in->formatCount);
	out->rfAtten=in->rfAtten;
	out->dwp_code=GET_SAT_USHORT(in->swst);
	out->dwp=out->dwp_code*CONF_ERS_timeBase;
	out->prf_code=GET_SAT_USHORT(in->prf);
	out->prf=1.0/((out->prf_code+2)*CONF_ERS_timeBase);
}

/****************************************************
auxPrint:
	Writes the "important" values from an auxiliary data
record to the given file.
*/

void ERS_auxPrint(ERS_aux *aux,FILE *f)
{
	fprintf(f,"   IsImaging=%d  OnBoardTime=%d\n"
		"   formatCount=%d, rfAtten=%d\n"
		"   dwp=%f   prf=%f\n",
		aux->isImaging,aux->onBoardTime,aux->formatCount,aux->rfAtten,
		aux->dwp,aux->prf);
}

/****************************************************
auxUpdate:
	Updates values in satellite record
based on values in given raw auxiliary data record.
*/

void ERS_auxUpdate(ERS_aux *aux,bin_state *s)
{
	double outputDelay=0;/*Output pulse delay (overridden below)*/
	
/*Determine if it's ERS-1 or ERS-2.  We do this using the
manual reciever gain value, which seems like a dumb idea; but works.
The two satellites have different circuitry delay values for transmission.
The values were checked using the Delta Junction corner reflectors.
E1 was sub-pixel accurate; but E2 was off by about 20 meters.*/
	
	if (aux->rfAtten==30)
	{/*Receiver attenuation at 30-- is ERS-1*/
		strcpy(s->satName,"ERS1");
		outputDelay=CONF_ERS1_rangePulseDelay;
	} else if (aux->rfAtten==38) {/*Reciever attenuation at 38-- is ERS-2*/
		strcpy(s->satName,"ERS2");
		outputDelay=CONF_ERS2_rangePulseDelay;
	} else {
		printf("   WARNING!!! Can't tell if this is ERS-1 or ERS-2!!  RF Atten=%d\n",aux->rfAtten);
		if (logflag) {
		  sprintf(logbuf,"   WARNING!!! Can't tell if this is ERS-1 or ERS-2!!  RF Atten=%d\n",aux->rfAtten);
		  printLog(logbuf);
		}
	}
	
/*Compute slant range to first pixel:*/
	s->prf=aux->prf;
	s->prf_code=aux->prf_code;
	s->dwp=aux->dwp;
	s->dwp_code=aux->dwp_code;

/*9=# of pulses in air*/
	s->range_gate=s->dwp+9.0/s->prf-outputDelay;
	
	s->time_code=aux->onBoardTime;
	
	if (!quietflag) ERS_auxPrint(aux,stdout);
}
/**************************************
auxAGC_window:
	call updateAGC_window with satellite
AGC (1.0) and window position for this line.
*/
void ERS_auxAGC_window(bin_state *s,ERS_aux *aux)
{
	updateAGC_window(s,1.0,aux->dwp*s->fs);
}
