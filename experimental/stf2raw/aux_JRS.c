/*JERS Auxiliary data decoding utilites.*/
#include "asf.h"
#include "decoder.h"
#include "aux.h"


/***********************************************
De-interlace:
	Extract every even bit of 2 signal bytes into
one byte of output data.
*/
void deInterlace(signalType *in,unsigned char *out)
{
	int in0=in[0],in1=in[1];
	*out=((0x01&(in0>>6))<<7)|
	     ((0x01&(in0>>4))<<6)|
	     ((0x01&(in0>>2))<<5)|
	     ((0x01&(in0>>0))<<4)|
	     ((0x01&(in1>>6))<<3)|
	     ((0x01&(in1>>4))<<2)|
	     ((0x01&(in1>>2))<<1)|
	     ((0x01&(in1>>0))<<0);
}
/*ThreeOfEight: extract the low three bits from each 
of 8 input bytes, making 3 full (8-bit) output bytes.
*/
void threeOfEight(signalType *in,unsigned char *out)
{
	out[0]=(in[0]<<5)|
	       (in[1]<<2)|
	       (in[2]>>1);
	out[1]=(in[2]<<7)|
	       (in[3]<<4)|
	       (in[4]<<1)|
	       (in[5]>>2);
	out[2]=(in[5]<<6)|
	       (in[6]<<3)|
	       (in[7]<<0);
}

/***********************************************
auxDecode:
	Converts raw signal data into decoded
auxiliary data record, starting from LZP binaries.

The auxiliary data starts at bit 60 of the input data,
and continues for 2*69 bit-doubled bits.  We first
align the data to an even byte boundary, then strip out
the I bit stream.
*/
void JRS_auxUnpack(signalType *in,JRS_raw_aux *out)
{
	int i;
	unsigned char byteAligned[32];
	unsigned char unpackHere[16];
/*H/K Data: Align to bytes,skipping sync in both channels*/
	extractBits(in,2*30,32,byteAligned);
/*Extract one channel of merged I and Q stream.*/
	for (i=0;i<16;i++)
		deInterlace(&byteAligned[i*2],&unpackHere[i]);
/*Copy over 9 bytes of aux. data*/
	for (i=0;i<9;i++)
		((unsigned char *)out)[i]=unpackHere[i];

/*Frame Counter: Align to bytes, skipping sync and H/K in both channels*/
	extractBits(in,2*(30+69),6,byteAligned);
	deInterlace(&byteAligned[0],&out->lineCountHi);
	deInterlace(&byteAligned[2],&out->lineCountMid);
	deInterlace(&byteAligned[4],&out->lineCountLo);
}

/******************************************
Extract a single JRS auxiliary data record from the given
signal data, starting from CEOS data.
*/
void JRS_auxCeosUnpack(signalType *in,JRS_raw_aux *out)
{
	unsigned char *dest=(unsigned char *)out;
	int i;
	/*Unpack the 24 3-bit/sample parts of the H/K record (!!)*/
	for (i=0;i<3;i++)
		threeOfEight(&in[108+8*i],&dest[i*3]);
	
	/*Unpack the 8 3-bit/sample parts of the line counter*/
	threeOfEight(&in[108+23],(signalType *)&(out->lineCountHi));
	
}
/******************************************
Decode the given raw auxiliary structure into
the given decoded auxiliary structure
*/
void JRS_auxDecode(JRS_raw_aux *in,JRS_aux *out)
{
	double PRF_list[8]=CONF_JRS_prfCodeList;
	out->dwp_code=in->stcStart;
	out->dwp=(out->dwp_code+1)*0.000010-0.000013;/* this fudge factor matches VEXCEL */
	out->prf_code=in->prfCode;
	out->prf=PRF_list[out->prf_code];
	
	out->stcOffset=in->stcOffset;
	
	if (in->agcOn==1)
	/*Using automatic gain control*/
		out->agc_dB=in->agcAtten;
	else /*Using manual gain control*/
		out->agc_dB=(in->gcStatus1<<4)+in->gcStatus2;
	
	out->lineCount=((int)in->lineCountHi<<16)|
		((int)in->lineCountMid<<8)|
		((int)in->lineCountLo);
}
/***********************************************
auxStc:
	Returns the start time, in microseconds,
of the JERS Sensitivity Time Control (STC) pattern.
This is used to correctly compensate for the STC
later on.
*/
int JRS_auxStc(JRS_aux *aux)
{
	return aux->stcOffset*10;
}

/***********************************************
auxPrint:
	Prints a several-line description of the
given auxiliary data record to the given file.
*/
void JRS_auxPrint(JRS_aux *aux,FILE *f)
{
	fprintf(f,"\n   dwp_code=%d,prf_code=%d,dwp=%f,prf=%f\n"
	"   stcOffset=%d, agc=%d, frame counter=%d\n",
		aux->dwp_code,aux->prf_code,aux->dwp,aux->prf,
		aux->stcOffset,aux->agc_dB,aux->lineCount);
}

/************************************************
auxUpdate:
*/
void JRS_auxUpdate(JRS_aux *aux,bin_state *s)
{
	/*
	printf("Updating metadata for...\n");
	JRS_auxPrint(aux,stdout);
	*/
	
	s->prf_code=aux->prf_code;
	s->prf=aux->prf;
	s->dwp_code=aux->dwp_code;
	s->dwp=aux->dwp;
/*7 pulses in the air.
	The transmission delay was computed from orbit 2528, a 1992
	scene over Delta Junction, Alaska.  DJ5 & DJ9 were located
	so sub-pixel accuracy with this delay. */
	s->range_gate=s->dwp+7.0/s->prf-CONF_JRS_rangePulseDelay;
}

/**************************************
auxAGC_window:
	call updateAGC_window with satellite
AGC and window position for this line.

*/
void JRS_auxAGC_window(bin_state *s,JRS_aux *aux)
{
	updateAGC_window(s,db2amp(aux->agc_dB),aux->dwp*s->fs);
}


