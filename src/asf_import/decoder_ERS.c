/**************
decode routine:

This file ingests VEXCEL Level-0 products from the ERS satellite.

**************/

#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"


/********************************
 * ERS_readNextPulse:
 * Fetches the next echo from the signal data, and unpacks it into iqBuf. Skips
 * over any blank lines. Updates nFrames with number of frames read. Currently
 * the 'inName' and 'outName' function parameters only exist so as to match
 * this function up with the readPulseFunc function pointer   */
int nLines=0;
void ERS_readNextPulse(bin_state *s,iqType *iqBuf, char *inName, char *outName)
{
  int ii;
  iqType *iqCurr=iqBuf;
  ERS_frame f;

  nLines++;

  /*Seek to next auxiliary data record.*/
  while (ERS_readNextFrame(s,&f)->is_aux!=1) {}

  ERS_auxAGC_window(s,&f.aux);

  /*Unpack the 20 leftover bytes (16 samples) in auxilary frame.*/
  iqCurr = ERS_unpackBytes(&f.data[ERS_datPerAux],
                           ERS_datPerFrame-ERS_datPerAux,
                           iqCurr);

  /*Unpack each 250 bytes (200 samples) in framesPerLine remaining echo frames.*/
  for (ii=1; ii<ERS_framesPerLine; ii++) {
    ERS_readNextFrame(s,&f);
    if (f.is_echo==0) {
      if (!quietflag)
        printf("   Error! Expected echo frame; got '%d' frame! Assuming bit error\n",
               f.type);
    }
    iqCurr=ERS_unpackBytes(f.data,ERS_datPerFrame,iqCurr);
  }

  /* Occasionally print out the auxiliary data record */
/*  if (nLines%1000==0) {
 *    ERS_auxPrint(&f.aux,stdout);
 *  }
 */
}


/*********************************
 * ERS_init:
 * Raw satellite initialization routine.  */
void ERS_init(bin_state *s)
{
  strcpy(s->satName,"ERS-?");/*Filled out in ERS_auxUpdate*/
  CONF_ERS_fields(s);
  s->bytesPerFrame=ERS_bytesPerFrame;
}

/*********************************
 * ERS_decoder_init:
 * Decoder initialization routine.  */
bin_state *ERS_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
  bin_state *s=new_bin_state();
  ERS_frame f;
  printf("   Initializing ERS decoder...\n");
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
 * ERS_readNextCeosPulse:
 * CEOS Echo decoder  */
void ERS_readNextCeosPulse(bin_state *s,iqType *iqBuf, char *inName,
                           char *outName)
{
  int i;
  signalType *sig=NULL;
  ERS_aux aux;
  sig=getNextCeosLine(s->binary, s, inName, outName);
  for (i=0;i<2*s->nSamp;i++)
    iqBuf[i]=sig[220+i];
  ERS_decodeAux(sig,&aux);
  ERS_auxAGC_window(s,&aux);
}

/***********************
 * ERS_ceos_decoder_init:
 * CEOS Decoder inititalization routine  */
bin_state *ERS_ceos_decoder_init(char *inName, char *outName,
                                 readPulseFunc *reader)
{
  bin_state *s=new_bin_state();
  signalType *sig=NULL;
  ERS_aux aux;
  printf("   Initializing ERS decoder...\n");
  *reader=ERS_readNextCeosPulse;

  ERS_init(s);

  s->binary=openCeos(inName, outName, s);
  sig=getNextCeosLine(s->binary, s, inName, outName);
  ERS_decodeAux(sig,&aux);
  ERS_auxUpdate(&aux,s);

  ERS_auxAGC_window(s,&aux);
  FSEEK64(s->binary,0,0);

  return s;
}

#endif


