/*decode routine:
	This file ingests VEXCEL Level-0 products from the JERS
satellite.
*/

#include "asf.h"
#include "decoder.h"
#include "aux.h"

/*Satellite-specific Parameters:*/
#define samplesPerFrame 6144
#define framesPerLine 1

/********************************
decodePulse:
	Extracts valid signal data from packed
pulse structure, stripping headers.
*/
void decodePulse(signalType *pulse,iqType *iqBuf)
{
	signalType alignedBuf[2*1536/8];
	int i;
	iqType *iqCurr=iqBuf;
	
	for (i=0;i<12;i++)
	{/*We have to skip the sync, h/k data, frame count, and first PCM block.
	  We then get 1536 bits from each channel, skip 3 bits, etc..*/
		extractBits(pulse,2*(30+69+24+3+1539*i),2*1536/8,alignedBuf);
		iqCurr=JERS_unpackBytes(alignedBuf,2*1536/8,iqCurr);
	}
}

/*******************************
JERS_stcCompensate:
	Compensates the given data for JRS' 
Sensitivity Time Control, a range-dependant attenuation.
*/
void JRS_stcCompensate(bin_state *s,int stcOff,int len,iqType *iqBuf)
{
/*These variables compensate for the "Sensitivity Time Control":
a range-timing dependant attenuation JERS applies to the signal.*/
	double stcTime[12]={0,50,60,80,110,150,260,300,330,350,370,500};/*Period beginning (microseconds).*/
	double stcGain[12]={0, 1, 2, 3,  4,  5,  4,  3,  2,  1,  0,  0};/*Compensation during period (dB).*/
	int stcNo;
	
#define DO_STC_COMP 1
#if DO_STC_COMP
	for (stcNo=0;stcNo<11;stcNo++)
	{
		int stcStart;/*Starting sample for STC compensation.*/
		int stcEnd;/*Ending sample for STC compensation.*/
		stcStart=(stcTime[stcNo]-stcOff) *1.0e-6 * s->fs;
		stcEnd=(stcTime[stcNo+1]-stcOff) *1.0e-6 * s->fs;
		if (stcEnd<0) continue;
		if (stcStart>len) continue;
		if (stcStart<0) stcStart=0;
		if (stcEnd>len) stcEnd=samplesPerFrame;
		scaleSamples(s,iqBuf,stcStart,stcEnd-stcStart,10.0*db2amp(stcGain[stcNo]));
	}
#endif
}
/********************************
JRS_readNextPulse:
	Fetches the next echo from the
signal data, and unpacks it into iqBuf.
Skips over any blank lines.
*/
void JRS_readNextPulse(bin_state *s,iqType *iqBuf)
{
	JRS_frame f;
	
	static int nFrames=0;
	nFrames++;
	
	JRS_readNextFrame(s,&f);
	
	JRS_auxAGC_window(s,&f.aux);
	
	decodePulse(f.data,iqBuf);
	
	if (nFrames%100==0)
	{/*Compute and print out I and Q mean values*/
		/*double iAve,qAve;
		int i;
		double iAve=0.0,qAve=0.0;
		for (i=0;i<samplesPerFrame;i++)
			{iAve+=iqBuf[2*i];qAve+=iqBuf[2*i+1];}
		printf("iAve=%f; qAve=%f\n",iAve/samplesPerFrame,qAve/samplesPerFrame);*/
		
		/* JRS_auxPrint(&f.aux,stdout); */
	}
	
	JRS_stcCompensate(s,JRS_auxStc(&f.aux),samplesPerFrame,iqBuf);
}

/*********************************
Satellite hardcoded parameters routine.
*/
void JRS_init(bin_state *s)
{
	strcpy(s->satName,"JERS1");
	CONF_JRS_fields(s);
	s->bytesPerFrame=JRS_bytesPerFrame;
	
	/*I include reasonable defaults in case no state vector is available*/
	s->re=6363490.0; /*approximate earth radius at scene center.*/
	s->vel=7576.869; /*satellite velocity, m/s->*/
	s->ht=581089.875; /*satellite height above earth, m.*/
}

/*********************************
Decoder initialization routine.
*/
bin_state *JRS_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
	bin_state *s=new_bin_state();
	JRS_frame f;
	printf("   Initializing JERS decoder...\n");
	*reader=JRS_readNextPulse;
	
	JRS_init(s);
	
	openBinary(s,inN);
	
	JRS_readNextFrame(s,&f);
	JRS_auxUpdate(&f.aux,s);
	
	seekFrame(s,0);
	
	return s;
}

#ifdef DECODE_CEOS
#define datPerAux 400
/*********************************
CEOS JRS Pulse reader
*/
void JRS_readNextCeosPulse(bin_state *s,iqType *iqBuf, char *inN, char *outN)
{
	int i;
	signalType *sig=NULL;
	JRS_raw_aux raux;
	JRS_aux aux;
	sig=getNextCeosLine(s->binary, s, inN, outN);
	for (i=0;i<2*samplesPerFrame;i++)
		iqBuf[i]=125+sig[datPerAux+i];

	JRS_auxCeosUnpack(sig,&raux);
	JRS_auxDecode(&raux,&aux);
	JRS_auxAGC_window(s,&aux);
	JRS_stcCompensate(s,JRS_auxStc(&aux),samplesPerFrame,iqBuf);
}

/*******************************
*/

bin_state *JRS_ceos_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
	bin_state *s=new_bin_state();
	signalType *sig=NULL;
	JRS_raw_aux raux;
	JRS_aux aux;
	printf("   Initializing JRS CEOS decoder...\n");
	*reader=JRS_readNextCeosPulse;
	
	JRS_init(s);
	
	s->binary=openCeos(inN, outN, s);
	sig=getNextCeosLine(s->binary, s, inN, outN);
	JRS_auxCeosUnpack(sig,&raux);
	JRS_auxDecode(&raux,&aux);
	JRS_auxUpdate(&aux,s);
	FSEEK64(s->binary,0,0);
	
	return s;
}
#endif




