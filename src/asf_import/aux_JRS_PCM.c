/*JERS Auxiliary data decoding utilites--
"Pulse Code Modulated" data.

This is a 2048-bps data stream encoded into the
JRS downlink.  We are given 13 samples of the PCM's
data and clock serial lines per SAR echo line, or
about 10 samples per PCM bit.

The PCM data is phase-shift encoded on the data line,
relative to the clock line.  One bit is transferred
for each cycle of the clock, switching bits at the
"trough" (low point) of the clock cycle.
If you think of the clock line as -cos(x), the
data line reads as sin(x) for a logic 1, and
-sin(x) for a logic 0.  One-by-one, the bits
can be extracted from the data line.

XXXXXXXXXXXXXXXXX JERS PCM Data Encoding XXXXXXXXXXXXXXXXX
Data line:
1 |   ------                   -----            ------   -----
  |  /      \        |        /     \ |        /      \|/     \         |
  | /- -- - -\- - - -- - - - / - - - \ - - - -/- -- - - - - - -\- - - - ...
  |/          \     /\      /         \      /                  \      /
0 |            ----- |------          |------          |         ------ |

  |<--- bit time --->|<---- .49ms --->|<- 10 samples ->|                |
  |                  |                |                |                |
  |        1         |       0        |       0        |       1        |
  |                  |                |                |                |

Clock line:          |                |                |                |
1 |       ------           -----            -----            -----
  |      /      \    |    /     \     |    /     \     |    /     \     |
  |- - -/- -- - -\- - - -/- - - -\- - - - / - - - \ - -  - / - - - \ - - ...
  |    /          \  |  /         \   |  /         \   |  /         \   |
0 |----            -----           ------           ------           ----
XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Oddly enough, because you have 10 samples of data and clock for
each PCM bit, it's easy to correct almost all bit errors using a
"majority", or "bit polarity" approach.

Once you extract the 2048-bit per second stream from the data
and clock line samples, you look for the pattern 0xFAF320
(the JERS PCM data frame synchronization code).
This pattern marks the beginning of a 128-byte PCM minor frame.
We finally extract the satellite time code from bytes 16 and 17
of 4 consecutive minor frames.

Orion Sky Lawlor, June 17, 1999.
ASF-STEP Level-Zero Data Quality Tool
*/
#include "asf.h"
#include "decoder.h"
#include "auxiliary.h"
#include "asf_reporting.h"

JRS_PCM *new_JRS_PCM(int maxFrames)
{
	int nBytes=maxFrames*2/8;/*Number of bytes in frame buffer==(# frames)*(max PCM bits/frame)/(num bits per byte)*/
	int nFrames=(nBytes+127)/128;/*Maximum number of frames*/
	int i;
	JRS_PCM *p=(JRS_PCM *)MALLOC(sizeof(JRS_PCM));
	p->end=p->len=0;
	p->prevTrough=0;
	p->nBits=0;
	p->pcmBits=(unsigned char *)MALLOC(nBytes);
	for (i=0;i<nBytes;i++)
		p->pcmBits[i]=0;/*Initialize frame buffer to all zeros*/
	p->frames=(unsigned char *)MALLOC(nFrames*128);

	return p;
}
void delete_JRS_PCM(JRS_PCM *p)
{
	FREE(p->pcmBits);
	FREE(p->frames);
	FREE(p);
}
/*****************************
Bit-decoding utilites:*/
/*Add a newly clocked-in bit to the PCM bit stream*/
static void setBit(JRS_PCM *p,int bitVal)
{
	int nByte=p->nBits/8;
	p->pcmBits[nByte]|=(bitVal<<(7-(p->nBits-nByte*8)));
	p->nBits++;
}

/*Return the circular index of the clock
bits' "trough"-- where the clock pulse reads lowest.
Searches from end-len to end-1 in buffer,
looking for the clock trough, and returns the index of the
middle of the trough.
*/
static int clock_trough(JRS_PCM *p)
{
	int troughMaskLen=9;
	static const int troughMask[9]={5,4,-2,-3,-5,-4,-2,4,5};
	int offset,maxOffset=p->len-troughMaskLen-1;
	int start=p->end-p->len;
	int bestVal=-10000;
	int bestOffset=-1;
	for (offset=0;offset<maxOffset;offset++)
	{
		int i;
		int thisVal=0;
		for (i=0;i<troughMaskLen;i++)
			thisVal+=troughMask[i]*p->clk_bits[PCM_bit_mask&(start+offset+i)];
		if (bestVal<thisVal)
		{
			bestVal=thisVal;
			bestOffset=offset;
		}

	}
	return PCM_bit_mask&(bestOffset+start+troughMaskLen/2);
}

/*Determine the polarity of the data bit located immediately
before the given clock trough.  Needs at least 9 bits in the
data_bits buffer.
*/
int extract_bit(JRS_PCM *p,int trough)
{
	int i;
	int polarity=0;
	/*Add the bits on the late edge of the data cycle*/
	for (i=1;i<5;i++)
		polarity+=p->data_bits[PCM_bit_mask&(trough-i)];
	/*Skip bit 4 pixels away from trough*/
	/*Now subtract the bits on the early edge of the data cycle*/
	for (i=6;i<10;i++)
		polarity-=p->data_bits[PCM_bit_mask&(trough-i)];

/*asfPrintStatus("Polarity=%d, clockLen=%d\n",polarity,PCM_bit_mask&(trough-p->prevTrough));	*/
	return (polarity<0);
}


/*Add a single (at least 8x over-) sampling of the data and clock
lines to the circular buffer.

Note: dataVal and clockVal represent a single bit, but need not
actually be 0 or 1-- they can be 0 and 15, or 0 and 3, or whatever.
Analog signals (0,1,2, or 3) are even OK.
*/
void JRS_PCM_add_sample(JRS_PCM *p,int clockVal,int dataVal)
{
	p->data_bits[p->end]=dataVal;
	p->clk_bits[p->end]=clockVal;
	p->end++;p->len++;
	p->end&=PCM_bit_mask;

	if (p->len>16)
	{/*We have a long string of data and clock bits-- cut them back a bit*/
		int trough=clock_trough(p);
		setBit(p,extract_bit(p,trough));
		p->len=PCM_bit_mask&(p->end-trough);/*Set the new end-of-buffer to the clock trough*/
		p->prevTrough=trough;
	}
}

/*Find the bit-sync codes in the pcmBits array, and extract
the frames.*/
void JRS_PCM_sync(JRS_PCM *p)
{
/*Find the bit offset to the frame start (search over two frame's worth)*/
	int bitOffset;
	for (bitOffset=0;bitOffset<8*256;bitOffset++)
	{
		static const unsigned char frameSync[3]={0xFA,0xF3,0x20};
		unsigned char curSync[3];
		extractBits(p->pcmBits,bitOffset,3,curSync);
		if ((frameSync[0]==curSync[0])&&(frameSync[1]==curSync[1])&&(frameSync[2]==curSync[2]))
			break;/*Jump out of loop*/
	}
	p->startBit=bitOffset;

/*Now that we've found the bit synchronization code,
  extract each frame, bit-aligned.*/
	for (p->nFrames=0;(bitOffset+(p->nFrames+1)*8*128)<p->nBits;p->nFrames++)
		extractBits(p->pcmBits,bitOffset+p->nFrames*8*128,128,&p->frames[p->nFrames*128]);
}

/*Write the binary PCM frames to the given file*/
void JRS_PCM_write(JRS_PCM *p,const char *fName)
{
	FILE *f=FOPEN(fName,"wb");
	int i;
	for (i=0;i<p->nFrames;i++)
		FWRITE(&p->frames[i*128],128,1,f);
	FCLOSE(f);
	asfPrintStatus("Wrote %d 128-byte frames of JRS PCM data to '%s'.\n",p->nFrames,fName);
}


/*Extract the time of the first data line from the cached PCM data*/
double JRS_PCM_time(JRS_PCM *p)
{
	int time=0,timePieces=0;
	int frameNo=0;
	while (frameNo<p->nFrames)
	{
		unsigned char *curFrame=&p->frames[frameNo*128];
		int fc=curFrame[5]&0x3;/*Frame counter, modulo 4*/
		if (fc==0)
		{/*We found the first piece of the time code:
		Re-set the "pieces" list and extract the time.*/
			timePieces=0x001;
			time=curFrame[17]|((curFrame[16]&0x7)<<24);
		}
		else if (fc==1)
		{/*We found the second piece of the time code:*/
			timePieces|=0x010;
			time|=(curFrame[16]<<16);
		}
		else if (fc==2)
		{/*Finally, we have the last piece of the time code--
		If we have the other pieces, extract and return them all*/
			timePieces|=0x100;
			time|=(curFrame[16]<<8);
			if (timePieces==0x111)
			{/*We collected all three pieces! Return them*/
				int waitBits;/*Number of PCM bit-times we wasted looking for this clock frame*/
				double ret;
				asfPrintStatus("Found JRS PCM time code %d at frame %d!!\n",time,frameNo);
			/*Subtract off the amount of time we wasted *looking* for the clock--
			clock time refers to time of the first bit of the frame with fc=0*/
				waitBits=p->startBit+128*8*(frameNo-2);
				ret=time-waitBits/2048.0;/*PCM data is 2048.0 bps, so n bits==n/2048.0 seconds.*/
				asfPrintStatus("Correcting JRS PCM time to %f for first line\n",ret);
				return ret;
			}
		}
		/*Keep looking...*/
		frameNo++;
	}
	/*If we haven't returned by now, we couldn't find a time code in all the frames*/
	asfForcePrintStatus("ERROR!!  Couldn't find JRS PCM time in %d frames!!\n",
	                    p->nFrames);
	return 0;
}

