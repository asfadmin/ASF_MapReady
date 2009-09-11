/*Unpack routines:
These routines unpack raw (bit-packed) signal
data into (byte-packed) I/Q samples.
*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"

/**************************************
Bit fiddling:
	ERS-1 uses a 5-bit I \ 5-bit Q
sampling strategy; so we have to copy over
various horrible bitwise combinations of the
data.
*/
#define EnSig 5 /*Number of bytes of signal data converted.*/
#define EnIQ 4 /*Number of I/Q samples converted.*/
/*
ERS_convertSignalBytes:
Trade EnSig signal bytes for EnIQ iq pairs (2*nIQ bytes).
Called only by ERS_unpackBytes.
*/
void ERS_convertSignalBytes(signalType *in,iqType *out)
{
	long b=(in[0]<<24)|(in[1]<<16)|(in[2]<<8)|(in[3]);
	out[0]=0x001f&(b >> 27);
	out[1]=0x001f&(b >> 22);
	out[2]=0x001f&(b >> 17);
	out[3]=0x001f&(b >> 12);
	out[4]=0x001f&(b >> 7);
	out[5]=0x001f&(b >> 2);
	out[6]=0x001f&((b<<3)+(in[4]>>5));
	out[7]=0x001f&(in[4]);
}

/*Unpack nIn input bytes to nIn/nSig*nIQ*2 of output bytes.
Return pointer to just past end of valid output.*/
iqType *ERS_unpackBytes(signalType *in,int nIn,iqType *out)
{
	int i,len=nIn/EnSig;
	if (len*EnSig!=nIn)
		asfPrintError("Asked to convert %d bytes, which is not divisble by %d!\n",
		              nIn,EnSig);
	for (i=0;i<len;i++)
		ERS_convertSignalBytes(&in[i*EnSig],&out[i*EnIQ*2]);
	return &out[len*EnIQ*2];
}


/**************************************
Bit fiddling:
	JRS-1 uses a bizarre bit-interleaved 3-bit I \ 3-bit Q
sampling strategy; so we have to copy over
various horrible bitwise combinations of the data.
*/
#define JnSig 3 /*Number of bytes of signal data converted.*/
#define JnIQ 4 /*Number of I/Q samples converted.*/
/*
JERS_convertSignalBytes:
Trade JnSig signal bytes for JnIQ iq pairs (2*JnIQ bytes).
Called only by JERS_unpackBytes.
*/
void JERS_convertSignalBytes(signalType *in,iqType *out)
{
/*Extract I or Q channel, de-interleaving bits.*/
#define ext_i(s) ((0x4&(s>>3))|(0x2&(s>>2))|(0x1&(s>>1)))
#define ext_q(s) ((0x4&(s>>2))|(0x2&(s>>1))|(0x1&(s>>0)))

	int b=(in[0]<<16)|(in[1]<<8)|(in[2]);/*3 bytes as an int.*/
	int s;/*6 bits, forming one sample.*/

	s=0x03F&(b >> 18);/*1st sample pair*/
	out[0]=125+ext_i(s);
	out[1]=125+ext_q(s);

	s=0x03F&(b >> 12);/*2nd sample pair*/
	out[2]=125+ext_i(s);
	out[3]=125+ext_q(s);

	s=0x03F&(b >> 6);/*3rd sample pair*/
	out[4]=125+ext_i(s);
	out[5]=125+ext_q(s);

	s=0x03F&(b);/*4th sample pair*/
	out[6]=125+ext_i(s);
	out[7]=125+ext_q(s);
}

/*Unpack nIn input bytes to nIn/nSig*nIQ*2 of output bytes.
Return pointer to just past end of valid output.*/
iqType *JERS_unpackBytes(signalType *in,int nIn,iqType *out)
{
	int i,len=nIn/JnSig;
	if (len*JnSig!=nIn)
		asfPrintError("Asked to convert %d bytes, which is not divisble by %d!\n",
		              nIn,JnSig);
	for (i=0;i<len;i++)
		JERS_convertSignalBytes(&in[i*JnSig],&out[i*JnIQ*2]);
	return &out[len*JnIQ*2];
}


/**************************************
Bit fiddling:
	Radarsat uses a fairly sensible setup:
4-bits I/4-bits Q to represent its data.
*/
#define RnSig 1 /*Number of bytes of signal data converted.*/
#define RnIQ 1 /*Number of I/Q samples converted.*/
/*
convertSignalBytes:
Trade RnSig signal bytes for RnIQ iq pairs (2*nIQ bytes).
Called only by RSAT_unpackBytes.
*/
static int  RSAT_cvrt[16]={ 8, 9,10,11,12,13,14,15,
			    0, 1, 2, 3, 4, 5, 6, 7};

void RSAT_convertSignalBytes(signalType in,iqType *out)
{
	out[0]=RSAT_cvrt[ 0x00f&(in >> 4) ];
	out[1]=RSAT_cvrt[ 0x00f&(in) ];
}

/*Unpack nIn input bytes to nIn output bytes.
For CEOS frames, these have been expanded to bytes,
but still use the RSAT (base-2 signed!) convention.*/
void RSAT_unpackCeosBytes(signalType *in,int nIn,iqType *out)
{
	int i;
	for (i=0;i<nIn;i++)
		out[i]=RSAT_cvrt[in[i]];
}

/*Unpack nIn input bytes to nIn/RnSig*nIQ*2 of output bytes.
Return pointer to just past end of valid output.*/
iqType *RSAT_unpackBytes(signalType *in,int nIn,iqType *out)
{
	int i,len=nIn/RnSig;
	if (len*RnSig!=nIn)
		asfPrintError("Asked to convert %d bytes, which is not divisble by %d!\n",
		              nIn,RnSig);
	for (i=0;i<len;i++)
		RSAT_convertSignalBytes(in[i*RnSig],&out[i*RnIQ*2]);
	return &out[len*RnIQ*2];
}




