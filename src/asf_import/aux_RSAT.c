/*RADARSAT Auxiliary data decoding utilites.*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"
#include "asf_reporting.h"

/******************************************************************************
 * Information about RSAT_raw_aux for function: RSAT_decodeAux()
 * Description of the RSAT aux data header in raw form - it is a 50 byte block
 * View only using a fixed width font

 |------------AuxSynMarker[4]----------- |------ImageReferenceIdentifier[4]----
 0........ 1........ 2........ 3........ 4........ 5........ 6........ 7.......

                    CALNAttenuatorSetting  -PulseWaveformNo:4
                                   |       =Spare2:4
                  -ReplicaAGC:6    |         |
                      =Spare1:2    V         V
 |---PayloadStatus-- |------== |-------- |----==== |-Spare3- |---Temperature->>
 8........ 9........10........11........12........13........14........15.......

                                                                    PulseCount1
                                                           -NoOfBeams:2    |
                                                     =ADCSamplingRate:2    |
                                                              ~Spare4:4    V
>>Temperature(cont)- |---BeamSequence--- |-----Ephemeris---- |--==~~~~ |-------
16........17........18........19........20........21........22........23.......

                              -RxWindowStartTime2:4        -RxWindowDuration2:4
                                          =Spare6:4        =Spare7:4
                         RxWindowStartTime1  | RxWindowDuration1 |
                    -PRFBeam2:5    |         |         |         |
                  =BeamSelect:2    |         |         |         |
PulseCount2           ~Spare5:1    V         V         V         V Attitude[12]
 |-------- |PRFBeam1 |-----==~ |-------- |----==== |-------- |----==== |----->>
24........25........26........27........28........29........30........31.......



>>-----------------------------Attitude[12](cont)---------------------------->>
32........33........34........35........36........37........38........39.......



>>-----Attitude[12](cont)----- |-------------------Time[6]------------------->>
40........41........42........43........44........45........46........47.......
                               |-daysHi- |---===== |-secMid- |----==== |-------
                                          -daysLo:3           -secLo:4 microsHi
                                           =secHi:5        =millisHi:4
            -SCTO2Defaults:1
            =ReplicaPresent:1
            ~RxAGCSetting:6
Time[6](cont)
>>-------- |-=~~~~~~
48........49........
 |--------
  microsLo

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// Here's the old structure (which used bitfields and broke on Linux) //
// Uchar is unsigned char
// SAT_ULONG is unsigned long (big endian format)
// SAT_USHORT is unsigned short (big endian format)

// RADARSAT auxiliary data is 50 bytes long, and starts 10 bytes into frame.
// REV F S. 17
typedef struct {
	Uchar AuxSyncMarker[4]; //Code:	0x352EF853
	Uchar ImageReferenceIdentifier[4];
	struct SAT_USHORT PayloadStatus;
	Uchar ReplicaAGC:6;
	Uchar Spare1:2;
	Uchar CALNAttenuatorSetting;
	Uchar PulseWaveformNo:4;
	Uchar Spare2:4;
	Uchar Spare3:8;
	struct SAT_ULONG	Temperature;
	struct SAT_USHORT BeamSequence;
	struct SAT_USHORT Ephemeris;
	Uchar NoOfBeams:2;
	Uchar ADCSamplingRate:2;
	Uchar Spare4:4;
	Uchar PulseCount1;
	Uchar PulseCount2;
	Uchar PRFBeam1;
	Uchar PRFBeam2:5;
	Uchar BeamSelect:2;
	Uchar Spare5:1;
	Uchar RxWindowStartTime1;
	Uchar RxWindowStartTime2:4;
	Uchar Spare6:4;
	Uchar RxWindowDuration1;
	Uchar RxWindowDuration2:4;
	Uchar Spare7:4;
	Uchar Attitude[12];
	Uchar Time[6];
	Uchar SCTO2Defaults:1;
	Uchar ReplicaPresent:1;
	Uchar RxAGCSetting:6;
} RSAT_raw_aux;

******************************************************************************/

/********************************************************************
 * DecodeAux:
 * Converts a raw satellite downlink (AUX) with nasty bit-wise fields and odd
 * meanings to an interpreted structure (decodedAux) to clean floating-point
 * fields and clear meanings.  */
void RSAT_decodeAux(unsigned char *in, RSAT_aux *out)
{
   unsigned int PayloadStatus;
   unsigned int ReplicaAGC;
   unsigned int BeamSequence;
   unsigned int NoOfBeams;
   unsigned int ADCSamplingRate;
   unsigned int PRFBeam1;
   unsigned int PRFBeam2;
   unsigned int BeamSelect;
   unsigned int RxWindowStartTime1;
   unsigned int RxWindowStartTime2;
   unsigned int RxWindowDuration1;
   unsigned int RxWindowDuration2;
   unsigned int ReplicaPresent;
   unsigned int RxAGCSetting;
   unsigned int daysHi, daysLo;
   unsigned int secondsHi, secondsMid, secondsLo;
   unsigned int millisecondsHi, millisecondsLo;
   unsigned int microsecondsHi, microsecondsLo;
   unsigned int raw_aux[RSAT_datPerAux];
   int ii;
   int seconds,milliseconds,microseconds;
   /*Beam name, indexed by beam number.*/
   char *beamIdent[21] = CONF_RSAT_beamNameList;

   /* Cast the bytes to unsigned integers, then extract things that we want */
   for (ii=0; ii<RSAT_datPerAux; ii++) {
      raw_aux[ii] = (unsigned int)in[ii];
   }
   PayloadStatus   = ((raw_aux[8]<<8) | raw_aux[9]);
   ReplicaAGC      = (raw_aux[10] & 0xfc) >> 2;
   BeamSequence    = ((raw_aux[18]<<8) | raw_aux[19]);
   NoOfBeams       = (raw_aux[22] & 0xc0) >> 6;
   ADCSamplingRate = (raw_aux[22] & 0x30) >> 4;
   PRFBeam1        = raw_aux[25];
   PRFBeam2        = (raw_aux[26] & 0xf8) >> 3;
   BeamSelect      = (raw_aux[26] & 0x06) >> 1;
   RxWindowStartTime1 = raw_aux[27];
   RxWindowStartTime2 = (raw_aux[28] & 0xf0) >> 4;
   RxWindowDuration1  = raw_aux[29];
   RxWindowDuration2  = (raw_aux[30]&0xf0) >> 4;
   ReplicaPresent     = (raw_aux[49] & 0x40) >> 6;
   RxAGCSetting       = raw_aux[49] & 0x3f;
   daysHi         = raw_aux[43];
   daysLo         = (raw_aux[44] & 0xe0) >> 5;
   secondsHi      = raw_aux[44] & 0x1f;
   secondsMid     = raw_aux[45];
   secondsLo      = (raw_aux[46] & 0xf0) >> 4;
   millisecondsHi = raw_aux[46] & 0x0f;
   millisecondsLo = (raw_aux[47] & 0xfc) >> 2;
   microsecondsHi = raw_aux[47] & 0x03;
   microsecondsLo = raw_aux[48];

   /* Begin filling the useful RSAT_aux structure */
   out->payloadStatus = PayloadStatus;
   out->isImaging     = (0x04==(0x07&(raw_aux[8]>>5)));
   out->replicaAGC    = ReplicaAGC;
   out->rxAGC         = RxAGCSetting;
   out->hasReplica    = ReplicaPresent;
   out->nBeams        = 1 + NoOfBeams;

   /*Stripmap*/
   if (out->nBeams==1) {
      out->beams[0] = BeamSequence & 0x1f;
      out->beams[1] = 0;
      out->beams[2] = 0;
      out->beams[3] = 0;
   }
   /*ScanSAR*/
   else {
      out->beams[0] =  BeamSequence       & 0x0f;
      out->beams[1] = (BeamSequence >> 4) & 0x0f;
      out->beams[2] = (BeamSequence >> 8) & 0x0f;
      out->beams[3] = (BeamSequence >>12) & 0x0f;
   }

   for (ii=0; ii<4; ii++)
      out->beamNames[ii] = beamIdent[out->beams[ii]];

   out->beam = BeamSelect;

   switch(ADCSamplingRate) {
      case 0: out->timeBase =  4.0/CONF_RSAT_SMO; break;
      case 1: out->timeBase =  7.0/CONF_RSAT_SMO; break;
      case 2: out->timeBase = 10.0/CONF_RSAT_SMO; break;
      default:
         asfPrintStatus("Error! Unrecognized ADC conversion rate %d!\n",
                        ADCSamplingRate);
   }

   out->prf_code       = (PRFBeam1<<5) | PRFBeam2;
   out->prf            = 1/((2+out->prf_code)*6*out->timeBase);
   out->dwp_code       = (RxWindowStartTime1<<4) | RxWindowStartTime2;
   out->dwp            = (5+out->dwp_code) * 6 * out->timeBase;
   out->rxLen          = (1 + ((RxWindowDuration1<<4) | RxWindowDuration2)) * 6;
   out->windowDuration = out->rxLen * out->timeBase;
   out->replicaLen     = 44.559E-06 / out->timeBase;

   /* Time */
   out->days = (daysHi<<3) + daysLo;
   seconds      = (secondsHi<<12) + (secondsMid<<4) + secondsLo;
   milliseconds = (millisecondsHi<<6) + millisecondsLo;
   microseconds = (microsecondsHi<<8) + microsecondsLo;
   out->seconds = seconds + (milliseconds + microseconds/1024.0)/1000.0;
}


/********************************************************************
 * auxPrint:
 * Writes the "important" values from an auxiliary data record to the given
 * file.  */
void RSAT_auxPrint(RSAT_aux *aux,FILE *f)
{

	fprintf(f,"\nReplica_AGC: %f\tRx_AGC: %f\tprf %f\n\trxStart %e\trxDur %e\n",
		aux->replicaAGC,aux->rxAGC,aux->prf,aux->dwp,aux->windowDuration);

	fprintf(f,"fs=%e;timeBase=%e;rxLen=%d\n",1.0/aux->timeBase,aux->timeBase,aux->rxLen);

	fprintf(f,"HasReplica %d; replicaLen %d; imaging=%d; nBeams %d; beam %d\n",
		aux->hasReplica,aux->replicaLen,RSAT_auxIsImaging(aux),
			aux->nBeams,aux->beam);
	fprintf(f,"Beams=%s/%s/%s/%s\n",aux->beamNames[0],aux->beamNames[1],aux->beamNames[2],aux->beamNames[3]);
	fprintf(f,"Day %d; Second %f of day\n",aux->days,aux->seconds);
}

/********************************************************************
 * auxUpdate:
 * Updates values in satellite record based on values in given raw auxiliary
 * data record.  */
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
/* DWP=SWST+#in air/PRF-characteristic transmission delay. I checked the
 * satellite transmission delay (fudge factor) using the ST6 scene over Delta
 * Junction from orbit 8848.*/
	s->dwp_code=aux->dwp_code;
	s->dwp=aux->dwp;
	s->range_gate=s->dwp+s->nPulseInAir/aux->prf-CONF_RSAT_rangePulseDelay;

	s->time_code=aux->seconds;

	asfPrintStatus("%d pulses in air; beam %d\n",s->nPulseInAir,aux->beams[0]);

	strcpy(s->beamMode,aux->beamNames[aux->beam]);

	RSAT_auxPrint(aux,stdout);

}

/********************************************************************
 * auxGet routines:
 * These extract a single parameter from the RADARSAT auxiliary data.  */
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
		asfPrintWarning("Not imaging!!\n");
	return aux->isImaging;
}

/**************************************
 * auxAGC_window:
 * Call updateAGC_window with satellite AGC and window position for this
 * line.  */
void RSAT_auxAGC_window(bin_state *s,RSAT_aux *aux)
{
        updateAGC_window(s,db2amp(aux->rxAGC),aux->dwp*s->fs);
}
