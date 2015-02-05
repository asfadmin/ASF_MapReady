/**************
decode routine:

This file ingests VEXCEL Level-0 products from the JERS satellite.

**************/

#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"
#include "get_stf_names.h"
#include "lzFetch.h"


/*Satellite-specific Parameters:*/
#define samplesPerFrame 6144
#define framesPerLine 1


/********************************
 * decodePulse:
 * Extracts valid signal data from packed pulse structure, stripping headers.*/
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
 * JERS_stcCompensate:
 * Compensates the given data for JRS' Sensitivity Time Control, a
 * range-dependant attenuation.  */
void JRS_stcCompensate(bin_state *s,int stcOff,int len,iqType *iqBuf)
{
    /* These variables compensate for the "Sensitivity Time Control":
     * a range-timing dependant attenuation JERS applies to the signal.*/
       /*Period beginning (microseconds).*/
    double stcTime[12]={0,50,60,80,110,150,260,300,330,350,370,500};
       /*Compensation during period (dB).*/
    double stcGain[12]={0, 1, 2, 3,  4,  5,  4,  3,  2,  1,  0,  0};
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
 * JRS_readNextPulse:
 * Fetches the next echo from the signal data, and unpacks it into iqBuf. Skips
 * over any blank lines. Currently the 'inName' and 'outName' function
 * parameters only exist so as to match this function up with the readPulseFunc
 * function pointer   */
void JRS_readNextPulse(bin_state *s,iqType *iqBuf, char *inName, char *outName)
{
    JRS_frame f;
    static int nFrames=0;
    nFrames++;
    JRS_readNextFrame(s,&f);
    JRS_auxAGC_window(s,&f.aux);
    decodePulse(f.data,iqBuf);
    JRS_stcCompensate(s,JRS_auxStc(&f.aux),samplesPerFrame,iqBuf);
}

/*********************************
 * JRS_init:
 * Satellite hardcoded parameters routine.  */
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
 * JRS_decoder_init:
 * Decoder initialization routine.  */
bin_state *JRS_decoder_init(char *inN,char *outN,readPulseFunc *reader)
{
    bin_state *s=new_bin_state();
    JRS_frame f;
    asfPrintStatus("   Initializing JERS decoder...\n");
    *reader=JRS_readNextPulse;

    JRS_init(s);

    openBinary(s,inN);

    JRS_readNextFrame(s,&f);
    JRS_auxUpdate(&f.aux,s);

    // Now pull info from the .par file as necessary
    char *parN = NULL;
    get_stf_metadata_name(inN, &parN);
    if (parN) {
        s->prf = lzDouble(parN, "prep_block.sensor.beam.PRF:", NULL);
        s->range_gate = lzDouble(parN, "prep_block.location[0].range_gate:", NULL);
        s->nFrames = s->bytesInFile / s->bytesPerFrame;
        seekFrame(s,0);
        FREE(parN);
    }
    else {
        // Should never get here (should error out in get_stf_metadata_name())
        asfPrintError("Cannot find %s.par file", inN);
    }

    return s;
}


#define datPerAux 400

/*********************************
 * JRS_readNextCeosPulse
 * CEOS JRS Pulse reader  */
void JRS_readNextCeosPulse(bin_state *s, iqType *iqBuf, char *inName,
                           char *outName)
{
    int i;
    signalType *sig=NULL;
    JRS_raw_aux raux;
    JRS_aux aux;
    sig=getNextCeosLine(s->binary, s, inName, outName);
    for (i=0;i<2*samplesPerFrame;i++)
        iqBuf[i]=125+sig[datPerAux+i];

    JRS_auxCeosUnpack(sig,&raux);
    JRS_auxDecode(&raux,&aux);
    JRS_auxAGC_window(s,&aux);
    JRS_stcCompensate(s,JRS_auxStc(&aux),samplesPerFrame,iqBuf);
}

/*******************************
 * JRS_ceos_decoder_init:
 * blah  */
bin_state *JRS_ceos_decoder_init(char *inName, char *outName,
                                 readPulseFunc *reader)
{
    bin_state *s=new_bin_state();
    signalType *sig=NULL;
    JRS_raw_aux raux;
    JRS_aux aux;
    asfPrintStatus("   Initializing JRS CEOS decoder...\n");
    *reader=JRS_readNextCeosPulse;

    JRS_init(s);

    s->binary=openCeos(inName, outName, s);
    sig=getNextCeosLine(s->binary, s, inName, outName);
    JRS_auxCeosUnpack(sig,&raux);
    JRS_auxDecode(&raux,&aux);
    JRS_auxUpdate(&aux,s);
    FSEEK64(s->binary,0,0);

    if (s->prf <= 0 || s->prf_code < 0 || s->prf_code > CONF_JRS_prfCodeMax) {
        // Ceos line had bad or missing header information
        struct dataset_sum_rec dssr;
        get_dssr(inName, &dssr);
        s->prf = dssr.prf;
        s->range_gate = dssr.rng_gate / 1000000.0;
    }

    return s;
}


/*********************************
 * ALOS_readNextCeosPulse
 * CEOS ALOS Pulse reader - no unpacking required, so heavily simplified  */
void ALOS_readNextCeosPulse(bin_state *s, iqType *iqBuf, char *inName,
                char *outName)
{
  int i;
  signalType *sig=NULL;
  JRS_raw_aux raux;
  JRS_aux aux;
  sig=getNextCeosLine(s->binary, s, inName, outName);
  //ASF_FREAD(iqBuf, sizeof(iqType), s->nSamp*2, s->binary);
  for (i=0;i<2*samplesPerFrame;i++)
    iqBuf[i]=125+sig[datPerAux+i];

  JRS_auxCeosUnpack(sig,&raux);
  JRS_auxDecode(&raux,&aux);
  JRS_auxAGC_window(s,&aux);
  JRS_stcCompensate(s,JRS_auxStc(&aux),samplesPerFrame,iqBuf);
}

/*********************************
 * ALOS_init:
 * Satellite hardcoded parameters routine.  */
void ALOS_init(bin_state *s)
{
  strcpy(s->satName,"ALOS");
  CONF_ALOS_fields(s);
  s->bytesPerFrame=JRS_bytesPerFrame;

  /*I include reasonable defaults in case no state vector is available*/
  s->re=6363490.0; /*approximate earth radius at scene center.*/
  s->vel=7716.989; /*satellite velocity, m/s->*/
  s->ht=714433.0; /*satellite height above earth, m.*/
}

bin_state *ALOS_ceos_decoder_init(char *inName, char *outName,
                  readPulseFunc *reader)
{
  bin_state *s=new_bin_state();
  signalType *sig=NULL;
  JRS_raw_aux raux;
  JRS_aux aux;
  asfPrintStatus("   Initializing ALOS CEOS decoder...\n");
  *reader=ALOS_readNextCeosPulse;

  ALOS_init(s);

  s->binary=openCeos(inName, outName, s);
  sig=getNextCeosLine(s->binary, s, inName, outName);
  JRS_auxCeosUnpack(sig,&raux);
  JRS_auxDecode(&raux,&aux);
  JRS_auxUpdate(&aux,s);
  FSEEK64(s->binary,0,0);

  return s;
}
