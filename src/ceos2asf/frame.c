/*************************
Frame.c: satellite downlink "frame"-reading utilities.
The satellite sends down data in fixed-sized "frames",
which make up part of an entire echo line.  The frames
typically have some sort of synchronization code at the
beginning, some metadata, and then a payload.

Because this data has already been processed by the Vexcel
Level-Zero processor, the PRN encoding (for ERS and RSAT) has
been stripped off, the auxiliary data bits have been fixed,
and the JERS I/Q bit inversion is gone.


ERS frames are 256 bytes, 3 sync bytes, and 250 bytes of payload.
	There are 29 ERS frames per echo line.
	The first frame in a line contains 230 bytes of auxiliary data.

JRS frames are 4660 bytes, 7.5 sync bytes, and 6144*2*3 bits of payload.
	There is just one frame per echo line (frame==line).
	The frames are bit-interleaved for the I and Q "streams"
	(one bit I, one bit Q).  Each stream contains 30 bits of sync,
	69 bits of housekeeping data (which is identical in both
	streams) a 24-bit frame counter, and 12 sets of 
	1539 bits of data/3 bits of PCM code.  Needless to say, it's ugly.

RSAT frames are 323 bytes, 4 sync bytes, and 311 bytes of payload.
	There are a variable number of frames per echo line.
	The first frame in a line contains 50 bytes of auxiliary data.
*/

#include "asf.h"
#include "decoder.h"
#include "aux.h"

/*Open the given binary file, and make it s' current file*/
void openBinary(bin_state *s,const char *fName)
{
	s->binary=FOPEN(fName,"rb");
	FSEEK64(s->binary,0,SEEK_END);/*Seek to EOF*/
	s->bytesInFile=FTELL64(s->binary);/*Save file size*/
	
	/*Seek back to start of file, setting curFrame*/
	seekFrame(s,0);
}

/*Seek to the given (0-based) frame number in the given file*/
void seekFrame(bin_state *s,int frameNo)
{
	long long seekLoc=s->bytesPerFrame*frameNo;
	FSEEK64(s->binary,seekLoc,0);
	s->curFrame=frameNo;
}

/***************************************************
ERS ReadNextFrame:
Fetches the next ERS_bytesPerFrame-byte frame from given file.
Fills out the is_* fields.
Modifies and returns the pointed-to frame.
*/
ERS_frame * ERS_readNextFrame(bin_state *s,ERS_frame *f)
{
/*Read next frame in file.*/
	FREAD(f->sync,ERS_bytesPerFrame,1,s->binary);
	s->curFrame++;
	
/*Determine frame type:*/
	f->is_aux=f->is_zero=f->is_echo=0;
	if (f->type==128)/*Check auxiliary data bit.*/
		f->is_aux=1;
	else if (f->type&64)/*Check zero bit.*/
		f->is_zero=1;
	else if ((f->type>128)&&(f->type<=156))
		f->is_echo=1;
	else
	  {
	     f->is_echo =1;
	     printf("   ***** ERROR at frame %i - Unknown frame type (%i); assumed to be bit error \n",
			s->curFrame, f->type);
	  }

/*Extract & decode auxiliary data*/
	if (f->is_aux)
	{/*Extract & decode auxiliary data*/
		f->raw=(ERS_raw_aux *)f->data;
		ERS_decodeAux(f->raw,&f->aux);
	}

	return f;
}

/***************************************************
JRS ReadNextFrame:
Fetches the next JRS_bytesPerFrame-byte frame from given file.
Decodes the auxiliary data record.
Modifies and returns the pointed-to frame.
*/
JRS_frame * JRS_readNextFrame(bin_state *s,JRS_frame *f)
{
/*Read next frame in file.*/
	FREAD(f->data,1,JRS_bytesPerFrame,s->binary);
	s->curFrame++;
	
/*Extract & decode auxiliary data*/
	JRS_auxUnpack(f->data,&f->raw);
	JRS_auxDecode(&f->raw,&f->aux);
	
	return f;
}

/*************************************************************
RSAT ReadNextFrame:
Fetches the next RSAT_bytesPerFrame-byte frame from given file.
Fills out the is_* fields.
Modifies and returns the pointed-to frame.
*/
RSAT_frame * RSAT_readNextFrame(bin_state *s,RSAT_frame *f)
{
/*Read next frame in file.*/
	FREAD(f,RSAT_bytesPerFrame,1,s->binary);
	s->curFrame++;
	
/*Determine frame type:*/
	f->is_aux=f->is_zero=f->is_echo=0;
	f->beam=-1;
	f->hasReplica=0;
	
	if ((f->status[1]&1)==0)/*Check zero bit.*/
		f->is_zero=1;
	else if ((f->id[1]&6)==0)/*Check auxiliary data bit.*/
		f->is_aux=1;
	else if ((f->id[1]&6)==2)/*Check echo data bit.*/
		f->is_echo=1;
/*	else
		{printf("Error!  Unknown RSAT frame type '%d'\n",(int)f->id[1]);exit(1);}
*/	
	
	if (f->is_aux)
	{
		f->raw=(RSAT_raw_aux *)f->data;
		RSAT_decodeAux(f->raw,&f->aux);
		f->beam=RSAT_auxGetBeam(&f->aux);
		f->hasReplica=RSAT_auxHasReplica(&f->aux);
	}
	
	return f;
}




