/**************
decode routine:

This file ingests VEXCEL Level-0 products from the RADARSAT satellite.

**************/

#include "asf.h"
#include "lzFetch.h"
#include "decoder.h"
#include "auxiliary.h"


/*Satellite-specific Parameters:*/
/*Private:*/
#define replicaDur 44.559E-06 /*Radarsat chirp replica length, in sec.*/


/********************************
 * RSAT_readNextPulse:
 * Fetches the next echo from the signal data, and unpacks it into iqBuf. Skips
 * over any blank lines. Updates nFrames with number of frames read. Currently
 * the 'inName' and 'outName' function parameters only exist so as to match this
 * function up with the readPulseFunc function pointer   */
void RSAT_readNextPulse(bin_state *s,iqType *iqBuf, char *inName,
                        char *outName)
{
	RSAT_frame aux_frame,f;
	int bytesToRead=RSAT_datPerAux;/*Just skip auxiliary data file.*/
	int bytesRead,dataStart;
	int repLen=s->fs*replicaDur;/*Number of samples in pulse replica.*/
	
	static unsigned int nLines=0;
	nLines++;
	
/*Skip to next auxiliary frame (start of line)*/
	while (RSAT_readNextFrame(s,&aux_frame)->is_aux!=1) {}
	
/*Check for AGC switch*/
	RSAT_auxAGC_window(s,&aux_frame.aux);
	
/*Copy auxiliary record into next frame.*/
	f=aux_frame;

/*Check for the presence of a pulse replica.*/
	if (aux_frame.hasReplica)
		bytesToRead+=repLen;
	
	bytesRead=0;
	dataStart=0;
/*Skip over the auxiliary data record, and pulse replica, if present.*/
	while (bytesRead<bytesToRead)
	{
		int skipThis=RSAT_datPerFrame;
		if (bytesRead+skipThis>bytesToRead)
			skipThis=bytesToRead-bytesRead;
		bytesRead+=skipThis;
		dataStart=skipThis;
		if (bytesRead<bytesToRead)
			RSAT_readNextFrame(s,&f);
	}

/*Unpack the echo data in each remaining frame.*/
	bytesToRead=s->nSamp;
	iqBuf=RSAT_unpackBytes(&f.data[dataStart],RSAT_datPerFrame-dataStart,iqBuf);
	bytesRead+=RSAT_datPerFrame-dataStart;
	
	while (bytesRead<bytesToRead)
	{
		int unpackThis=RSAT_datPerFrame;
		RSAT_readNextFrame(s,&f);
		if (bytesRead+unpackThis>bytesToRead)
			unpackThis=bytesToRead-bytesRead;
		iqBuf=RSAT_unpackBytes(f.data,unpackThis,iqBuf);
		bytesRead+=unpackThis;
	}
	
/* Every once in a while, write out a description of which line we're on.*/
/*	if (nLines%50==0) {
 *		printf("   Line # %d\n",nLines);
 *		RSAT_auxPrint(&aux_frame.aux,stdout);
 *	}
 */
}

/*********************************
 * outputReplica:
 * writes the given replica to the given file.  */
void outputReplica(const char *replN,iqType *replica,int repLen,
                   float repScale,double iBias,double qBias)
{
	int i,sampleStart;
	FILE *outFile;

/*Skip over zeros at start of pulse replica*/
	sampleStart=0;
	while(
		(fabs(replica[2*sampleStart]-iBias)<1.0)&&
		(fabs(replica[2*sampleStart+1]-qBias)<1.0))
		sampleStart++;
	
/*Write out the pulse replica.*/
	outFile=FOPEN(appendExt(replN,".replica"),"w");
	fprintf(outFile,"%d   ! Number of samples in reference function; I and Q data follow.\n",repLen-sampleStart);
	for (i=sampleStart;i<repLen;i++)
		fprintf(outFile,"%f\t%f\n",
			repScale*(replica[2*i]-iBias),repScale*(replica[2*i+1]-qBias));
	FCLOSE(outFile);
	
}


/*********************************
 * WriteReplica:
 * Write a pulse replica to the given file.  */
void RSAT_writeReplica(bin_state *s,char *replN,float repScale)
{
	int repLen=(int)(s->fs*replicaDur);
	int repRead=0;
	iqType *replica=(iqType *)MALLOC(sizeof(iqType)*2*repLen);
	iqType *repCurr=replica;
	RSAT_frame f={{0}};
	
/*Seek to next pulse replica start.*/
	while (f.hasReplica==0)
		/*Seek to next auxiliary data record.*/
		while (RSAT_readNextFrame(s,&f)->is_aux!=1) {}
	
/*Read in each chunk of the pulse replica.*/
	repCurr=RSAT_unpackBytes(&f.data[RSAT_datPerAux],RSAT_datPerFrame-RSAT_datPerAux,repCurr);	
	repRead+=RSAT_datPerFrame-RSAT_datPerAux;
	
	while (repRead<repLen)
	{
		int nRead=RSAT_datPerFrame;
		if (repRead+nRead>repLen)
			nRead=repLen-repRead;
		RSAT_readNextFrame(s,&f);
		
		repCurr=RSAT_unpackBytes(f.data,nRead,repCurr);	
		repRead+=nRead;
	}
	
/*Write out the pulse replica.*/
	outputReplica(replN,replica,repLen,repScale,s->I_BIAS,s->Q_BIAS);
}


/*********************************
 * RSAT_init:
 * Satellite information routine.  */
void RSAT_init(bin_state *s)
{
	strcpy(s->satName,"RSAT1");
	CONF_RSAT_fields(s);
	s->bytesPerFrame=RSAT_bytesPerFrame;
	
	/*These fields are filled out in aux_RSAT.c*/
	s->nPulseInAir=0; /*Number of pulses in the air at one time.*/
	s->nSamp=0; /*Number of samples in a line of data.*/
	s->fs=0; /*Range sampling frequency, Hz*/
	s->nValid=0; /*# of range samples for AISP to use.*/
	s->nLooks=0; /*# of looks for AISP to use.*/
	s->azres=0; /*Azimuth resolution for AISP (m).*/
	s->prf=0;/*Pulse repetition frequency, in Hz.*/
	s->slope=0.0; /*chirp slope, Hz/sec.*/
	
	/*I include reasonable defaults in case no state vector is available*/
	s->re=6370000.0; /*approximate earth radius at scene center.*/
	s->vel=7500; /*satellite velocity, m/s->*/
	s->ht=795000; /*satellite height above earth, m.*/
	s->estDop=0.0;/*Estimated doppler (PRF).*/
}

/*********************************
 * RSAT_decoder_init:
 * Decoder initialization routine.  */
bin_state *RSAT_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
	bin_state *s=new_bin_state();
	RSAT_frame aux_frame;
	
	printf("   Initializing RSAT decoder...\n");
	*reader=RSAT_readNextPulse;
	
	RSAT_init(s);
	
	openBinary(s,inN);
	/*Seek to first valid auxiliary data record.*/
	while (RSAT_readNextFrame(s,&aux_frame)->is_aux!=1 
		|| 0==RSAT_auxIsImaging(&aux_frame.aux)) {}
	
	/*Update satellite parameters based on auxiliary data record.*/
	RSAT_auxUpdate(&aux_frame.aux,s);
	
	/*Write pulse replica.*/
	RSAT_writeReplica(s,outN,1.0);
	
	seekFrame(s,0);
	
	return s;
}


#ifdef DECODE_CEOS

/**********************************
 * RSAT_readNextCEOSPulse:
 * blah */
void RSAT_readNextCeosPulse(bin_state *s,iqType *iqBuf, char *inN, char *outN)
{
	int repLen=(int)(s->fs*replicaDur);
	signalType *sig=NULL;
	RSAT_aux aux;
	static unsigned int nLines=0;
	
/*Seek to next pulse start.*/
	sig=getNextCeosLine(s->binary, s, inN, outN);
	RSAT_decodeAux((RSAT_raw_aux *)sig,&aux);
	RSAT_auxAGC_window(s,&aux);
	
	if (RSAT_auxHasReplica(&aux))
		RSAT_unpackCeosBytes(&sig[2*repLen+RSAT_datPerAux],2*s->nSamp,iqBuf);
	else
		RSAT_unpackCeosBytes(&sig[RSAT_datPerAux],2*s->nSamp,iqBuf);
	
/*	if ((nLines++)%1000==0) {
 *		printf("   Line # %d\n",nLines);
 * 		RSAT_auxPrint(&aux,stdout);
 *	}
 */
}

/*********************************
* RSAT_writeCeosReplica:
* Writes a pulse replica to the given file.  */
void RSAT_writeCeosReplica(bin_state *s,char *replN,float repScale, char *inN, char *outN)
{
	int repLen=(int)(s->fs*replicaDur);
	iqType *replica=(iqType *)MALLOC(sizeof(iqType)*2*repLen);
	signalType *sig=NULL;
	RSAT_aux aux;
	
/*Seek to next pulse replica start.*/
	sig=getNextCeosLine(s->binary, s, inN, outN);
	RSAT_decodeAux((RSAT_raw_aux *)sig,&aux);
	while (RSAT_auxHasReplica(&aux)==0)
	   {
		sig=getNextCeosLine(s->binary, s, inN, outN);
	 	RSAT_decodeAux((RSAT_raw_aux *)sig,&aux);
	   }
	
/*Read in each chunk of the pulse replica.*/
	RSAT_unpackCeosBytes(&sig[RSAT_datPerAux],2*repLen,replica);
	
/*Write out the pulse replica.*/
	outputReplica(replN,replica,repLen,repScale,s->I_BIAS,s->Q_BIAS);
}

/*************************************
 * RSAT_ceos_decoder_init:
 * CEOS Decoder initialization routine.  */
bin_state *RSAT_ceos_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
	bin_state *s=new_bin_state();
	signalType *sig=NULL;
	RSAT_aux aux;
	
	printf("   Initializing RSAT decoder...\n");
	*reader=RSAT_readNextCeosPulse;
	
	RSAT_init(s);
	
	s->binary=openCeos(inN, outN, s);
	sig=getNextCeosLine(s->binary, s, inN, outN);
	RSAT_decodeAux((RSAT_raw_aux *)sig,&aux);
	RSAT_auxUpdate(&aux,s);
	RSAT_writeCeosReplica(s,outN,1.0,inN,outN);
	FSEEK64(s->binary,0,0);
	
	return s;
}

#endif




