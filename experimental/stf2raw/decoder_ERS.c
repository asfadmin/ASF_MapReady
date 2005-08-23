/*decode routine:
	This file ingests VEXCEL Level-0 products from the ERS
satellite.
*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"

#define FLYWHEEL_LENGTH 25

extern long imgStart, imgEnd;
extern int outLine;

static iqType saveLines[FLYWHEEL_LENGTH][20000];

void writeSaveData(bin_state *s, int num)
{
	int i;

	printf("   Writing %i lines of saved data\n",num);
	if (logflag) {
	  sprintf(logbuf,"   Writing %i lines of saved data\n",num);
	  printLog(logbuf);
	}
	for (i=0; i<num; i++)
	  FWRITE(saveLines[i],s->nSamp,2,s->fpOut);   
	s->nLines += num;
}

void writeZeroData(bin_state *s, int num)
{
	int i;
	iqType *zeroBuf;

        zeroBuf=(iqType *)MALLOC(sizeof(iqType)*2*s->nSamp);                        
	for (i=0; i<s->nSamp*2; i++) zeroBuf[i] = (iqType) 0;

	printf("   Writing %i lines of zero data\n",num);
	if (logflag) {
   	  sprintf(logbuf,"   Writing %i lines of zero data\n",num);
	  printLog(logbuf);
	}
	for (i=0; i<num; i++)
	  FWRITE(zeroBuf,s->nSamp,2,s->fpOut);
	s->nLines += num;
}

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
        static int curFrame;                                    /* Values to keep track of the frame counter */
	static int curFormat, prevFormat = -99;
	static int lastGoodFormat;
	static int thisGoodFormat;
	static int goodCount=FLYWHEEL_LENGTH;
	static int badCount=0;

	/*int frameFormat;*/
        int i,skip_count;
        iqType *iqCurr=iqBuf;
        ERS_frame f;

	/* Seek to next auxiliary data record.
	 ------------------------------------*/
	skip_count=0;
	while (ERS_readNextFrame(s,&f)->is_aux!=1)
	 { 
	   if (s->curFrame >= s->nFrames-1) 
		{
		  s->readStatus = 0;
		  printf("   Hit end of file looking for aux frame (curFrame = %i)\n",s->curFrame);
		  if (logflag) {
		    sprintf(logbuf,"   Hit end of file looking for aux frame (curFrame = %i)\n",s->curFrame);
		    printLog(logbuf);
		  }
		  return;
		}
	   skip_count++; 
	 }
	
	ERS_auxAGC_window(s,&f.aux);

	/* Check to see if the sync codes in the data match the actual values 
	 -------------------------------------------------------------------*/
        check_sync(f.sync,3,ERS_sync); 
	
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
	    {
		printf("   Look-On Lost at Format %i\n",prevFormat);
		if (logflag) {
		  sprintf(logbuf,"   Look-On Lost at Format %i\n",prevFormat);
		  printLog(logbuf);
		}
	  	lastGoodFormat = prevFormat;
	        s->readStatus = 0;
	    }
	   goodCount = 0;
	   badCount++;
	 }
	else if (goodCount < FLYWHEEL_LENGTH)	/* Status is already BAD */
	 {
	   if (goodCount == 0) thisGoodFormat = prevFormat;
	   goodCount++;
	   badCount++;
	   if (goodCount==FLYWHEEL_LENGTH)	/* Status has just become GOOD again */
	     {
	        badCount -= FLYWHEEL_LENGTH+1;
	        fprintf(s->fperr,"##### DETECTED DATA GAP AFTER FORMAT %i BEFORE FORMAT %i #####\n",
		    lastGoodFormat, thisGoodFormat);
	        fprintf(s->fperr,"#####    Gap Length = %i; Missing Lines = %i;  Bit Error Lines = %i\n",
		    thisGoodFormat-lastGoodFormat-1,thisGoodFormat-lastGoodFormat-1-badCount, badCount);

		printf("   Lock-On Re-established at Format %i (1st good format is %i)\n",curFormat,thisGoodFormat);
		if (logflag) {	
		  sprintf(logbuf,"   Lock-On Re-established at Format %i (1st good format is %i)\n",curFormat,thisGoodFormat);	
		  printLog(logbuf);
		}
		if ((outLine >= imgStart) && (outLine <= imgEnd)) {
		  writeZeroData(s,thisGoodFormat-lastGoodFormat-1);
		  writeSaveData(s,FLYWHEEL_LENGTH);
		}

	        badCount = 0;
		s->readStatus = 1;
	     }
	 }

        /* This is the format counter present in every frame of a format 
        frameFormat = ((f.pulseNoHi << 8) + f.pulseNoLo) & 0x0FFFF;

        printf("   LZ2RAW Byte Offset = %lld frameFormat %d | curFormat = %d, prevFormat = %d -> ",
                FTELL64(s->binary)-256, frameFormat, s->curFormat, s->prevFormat); 
	*/


        /* Unpack the 20 leftover bytes (16 samples) in auxilary frame.
         -------------------------------------------------------------*/
        iqCurr=ERS_unpackBytes(&f.data[ERS_datPerAux],ERS_datPerFrame-ERS_datPerAux,iqCurr); 

        curFrame = f.type & 0x3F;

        /* Unpack each 250 bytes (200 samples) in framesPerLine remaining echo frames.
	 ----------------------------------------------------------------------------*/
	for (i=1;i<ERS_framesPerLine;i++)
	  {
		if (s->curFrame >= s->nFrames) 
		 {
		    s->readStatus = 0;
		    printf("   Hit end of file looking for data frame (curFrame = %i)\n",s->curFrame);
		    if (logflag) {
		      sprintf(logbuf,"   Hit end of file looking for data frame (curFrame = %i)\n",s->curFrame);
		      printLog(logbuf);
		    }
		    return;
	 	 }
		ERS_readNextFrame(s,&f);
                curFrame = f.type & 0x3F;

		if (f.is_aux == 1)	/* Premature end of line encountered */
		  {
			FSEEK64(s->binary,-256,SEEK_CUR);
			if (goodCount==FLYWHEEL_LENGTH)
			  {
				int lostFrames;

			        /* Determine the number of frames that were missing 
				 -------------------------------------------------*/
	                        lostFrames = (ERS_framesPerLine-1) - (i-1);

				fprintf(s->fperr,"***** Premature end of line encountered for line %i *****\n",curFormat);

       		                /* Let the user know that we are filling in zeros 
				 -----------------------------------------------*/
             		        printf("\n   Encountered an aux frame before the end of the data frames... " 
						" writing %d zero frames\n",lostFrames);
				if (logflag) {
             		          sprintf(logbuf,"\n   Encountered an aux frame before the end of the data frames... "
						" writing %d zero frames\n",lostFrames);
				  printLog(logbuf);
				}

       		                iqCurr=ERS_fillZeroBytes(ERS_datPerFrame*lostFrames,iqCurr);

                        	/* End this decoding run, we filled up to the end of the line */
			        break;
			  }
		  }
		else if (f.is_echo==0)	/* This frame is not echo data */
		  {
		        if (!quietflag) {
				printf("   ***** Expected echo frame; got '%d' frame for line %i! "
				" (Assuming bit error)\n",f.type,curFormat);
			}
			if (logflag && !quietflag) {
		          sprintf(logbuf,"   ***** Expected echo frame; got '%d' frame for line %i! "
				" (Assuming bit error)\n",f.type,curFormat);
			  printLog(logbuf);
			}
		  }

		iqCurr=ERS_unpackBytes(f.data,ERS_datPerFrame,iqCurr);
	  }

	/*Occasionally print out the auxiliary data record
	if ((!quietflag) && (s->nLines%1000==0)) { ERS_auxPrint(&f.aux,stdout); } 
	*/

	prevFormat = curFormat;

	/* Need to save this line of data to write out later 
	 --------------------------------------------------*/
	if (goodCount<FLYWHEEL_LENGTH && goodCount >= 0)
	  {
	    int cnt;
	
	    for (cnt = 0; cnt < s->nSamp*2; cnt++)
		saveLines[goodCount][cnt] = iqBuf[cnt];
	  }
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
	printf("   Initialization ERS decoder\n");
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
	printf("   Initialization ERS decoder\n");
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
