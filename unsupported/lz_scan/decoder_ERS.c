/*decode routine:
	This file ingests VEXCEL Level-0 products from the ERS
satellite.
*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"


#define FLYWHEEL_LENGTH 25

/********************************
ERS_readNextPulse:
	Fetches the next echo from the
signal data, and unpacks it into iqBuf.
Skips over any blank lines.
Updates nFrames with number of frames read.
*/
void ERS_readNextPulse(bin_state *s,iqType *iqBuf)
{
        static unsigned char ERS_sync[3] = {0xFA,0xF3,0x20};    /* Here are the ERS sync codes */
        static int curFrame, prevFrame;                         /* Values to keep track of the frame counter */
	static int curFormat, prevFormat = -99;
	static int nLines=0;
	static int lastGoodFormat;
	static int thisGoodFormat;
	static int goodCount=FLYWHEEL_LENGTH;
	static int badCount=0;

	int frameFormat;
        int i,skip_count;
        iqType *iqCurr=iqBuf;
        ERS_frame f;

        nLines++;
	
	/* Seek to next auxiliary data record.
	 ------------------------------------*/
	skip_count=0;
	while (ERS_readNextFrame(s,&f)->is_aux!=1) { skip_count++; }
	
	ERS_auxAGC_window(s,&f.aux);

	/* Check to see if the sync codes in the data match the actual values 
	 -------------------------------------------------------------------*/
        check_sync(f.sync,3,ERS_sync); 
	
	/* Unpack the 20 leftover bytes (16 samples) in auxilary frame.
	 -------------------------------------------------------------*/
	/* iqCurr=ERS_unpackBytes(&f.data[ERS_datPerAux],ERS_datPerFrame-ERS_datPerAux,iqCurr); */

	/* Make sure that the formats count linearly 
	 ------------------------------------------*/
        /* Use the format counter in the AUX header */
        curFormat = f.aux.formatCount & 0x0FFFFFF;
	if (prevFormat == -99) prevFormat = curFormat-1;

	/*
	if (skip_count != 0)
	   fprintf(s->fperr,"***** Warning:  Had to skip %i frames to find line %i aux *****\n",skip_count,curFormat);
	*/

	if (curFormat != ((prevFormat+1)&0x0FFFFFF))
	 {
	   if (goodCount == FLYWHEEL_LENGTH)	/* Status is now BAD */
	   	lastGoodFormat = prevFormat;
	   goodCount = 0;
	   badCount++;
	   /*
	     fprintf(s->fperr,"***** MISSING %i LINE(S) AFTER FORMAT %i BEFORE FORMAT %i *****\n",
		curFormat-prevFormat-1,prevFormat,curFormat);
	   */
	 }
	else 
	 {
	   if (goodCount == 0) thisGoodFormat = prevFormat;
	   if (goodCount < FLYWHEEL_LENGTH)	/* Status is already BAD */
	     {
		goodCount++;
		badCount++;
		if (goodCount==FLYWHEEL_LENGTH)
		  {
		    badCount -= FLYWHEEL_LENGTH+1;
		    fprintf(s->fperr,"##### DETECTED DATA GAP AFTER FORMAT %i BEFORE FORMAT %i #####\n",
			lastGoodFormat, thisGoodFormat);
		    fprintf(s->fperr,"#####    Gap Length = %i; Missing Lines = %i;  Bit Error Lines = %i\n",
			thisGoodFormat-lastGoodFormat-1,thisGoodFormat-lastGoodFormat-1-badCount, badCount);
		    badCount = 0;
		  }
	     } 
	 }

        /* This is the format counter present in every frame of a format */
        frameFormat = ((f.pulseNoHi << 8) + f.pulseNoLo) & 0x0FFFF;

        /* Lets print out some information about these values */
/*      printf("LZ2RAW Byte Offset = %lld frameFormat %d | curFormat = %d, prevFormat = %d -> ",
                ftell64(s->binary)-256, frameFormat, s->curFormat, s->prevFormat); */

        curFrame = prevFrame = f.type & 0x3F;
	fprintf(s->fpFrame,"%8i %3d",curFormat,curFrame);

/*Unpack each 250 bytes (200 samples) in framesPerLine remaining echo frames.*/
	for (i=1;i<ERS_framesPerLine;i++)
	{
		ERS_readNextFrame(s,&f);
                curFrame = f.type & 0x3F;

		if (f.is_aux == 1)	/* Premature end of line encountered */
		  {
			fseek64(s->binary,-256,SEEK_CUR);
			if (goodCount==FLYWHEEL_LENGTH)
				fprintf(s->fperr,"***** Premature end of line encountered for line %i *****\n",curFormat);
			break;
		  }
		else if (f.is_echo==0)	/* This frame is not echo data */
		  {
		        printf("***** Expected echo frame; got '%d' frame for line %i! (Assuming bit error)\n",f.type,curFormat);
                        fprintf(s->fpFrame,"%3d",curFrame); 
		        /* exit(1); */
		  }
                else
                        fprintf(s->fpFrame,"%3d",curFrame); 

		/* iqCurr=ERS_unpackBytes(f.data,ERS_datPerFrame,iqCurr); */
		prevFrame = curFrame;
	}

	fprintf(s->fpFrame,"\n");

	/*Occasionally print out the auxiliary data record*/
/*	if (nLines%1000==0)
	{
		ERS_auxPrint(&f.aux,stdout);
	}
*/
	prevFormat = curFormat;
}


/*********************************
Raw satellite initialization routine.
*/
void ERS_init(bin_state *s)
{
	strcpy(s->satName,"ERS-?");/*Filled out in ERS_auxUpdate*/
	CONF_ERS_fields(s);
	s->bytesPerFrame=ERS_bytesPerFrame;
}

/*********************************
Decoder initialization routine.
*/
bin_state *ERS_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
	bin_state *s=new_bin_state();
	ERS_frame f;
	printf("Initializing ERS decoder...\n");
	*reader=ERS_readNextPulse;
	
	ERS_init(s);
	
	openBinary(s,inN);
	while (ERS_readNextFrame(s,&f)->is_aux!=1) {}
	ERS_auxUpdate(&f.aux,s);
	seekFrame(s,0);
	
	return s;
}
#ifdef DECODE_CEOS
/************************
CEOS Echo decoder
*/
void ERS_readNextCeosPulse(bin_state *s,iqType *iqBuf)
{
	int i;
	signalType *sig=NULL;
	ERS_aux aux;
	sig=getNextCeosLine(s->binary);
	for (i=0;i<2*s->nSamp;i++)
		iqBuf[i]=sig[220+i];
	ERS_decodeAux((ERS_raw_aux *)sig,&aux);
	ERS_auxAGC_window(s,&aux);
}

/***********************
CEOS Decoder inititalization routine
*/
bin_state *ERS_ceos_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
	bin_state *s=new_bin_state();
	signalType *sig=NULL;
	ERS_aux aux;
	printf("Initializing ERS decoder...\n");
	*reader=ERS_readNextCeosPulse;
	
	ERS_init(s);
	
	s->binary=openCeos(inN);
	sig=getNextCeosLine(s->binary);
	ERS_decodeAux((ERS_raw_aux *)sig,&aux);
        ERS_auxUpdate(&aux,s);

	ERS_auxAGC_window(s,&aux);
	FSEEK64(s->binary,0,0);
	
	return s;
}

#endif
