/* fra.c             RADARAT Modification
* frame level access of the raw date
* searches for synchronization and unscrambles the data framewise
*
* first setup 07-12-1994 Hans-Joerg Wagner
* first release 07-25-1994 Hans-Joerg Wagner
*
* RADARSAT Modification
* by Christian Fischer
* 09-25-95
*
*/
#include <stdio.h>
#include <memory.h>
#include <varargs.h>
#include <malloc.h>
#include "randi.h"

/*values for the estimation of the time RANDI needs to process a certain
 *raw data file. I processed (with default settings and -o) a file
 *of Size_Reference and it needed Time_Reference seconds*/
#define Size_Reference 87120000.0
#define Time_Reference 234.0

/* modul global variables */
unsigned long int g_FileLength;
int g_nBitOffset = 0; 		/* current bit offset of synchronization */
int g_nBufferOffset = 0;	/* offset of last read frame within buffer */
int g_nValidBytesInBuf = 0;
int g_bSynchron = 0;
int g_bSave;			/* flag if save reading or unsave reading */
				/* if true a frame is only valid if the sync */
				/* pattern for the next frame is found */
ulong g_luFileOffset;		/* current offset of buffer within the file */
ulong g_luLastByte;		/* last byte to read in File */
char g_pcBuffer[BUFFER_LEN];	/* buffer for file read */
FILE *g_fileHandle = NULL;	/* file handle	*/
struct STAT g_stat;	/* synchronisation status global!!! */
int g_FirstFrame;

char g_cPattern[] = { 0x1a,0xcf,0xfc,0x1d } ;

/* unscrambling pattern :Length of the Application Data Field 
 * RADARSAT 323 bytes*/
char g_cScrambler[] = {
         0x21,0x4f,0xaa,0xe0,0xc5,0x66,0x5f,0xbc,
0xdd,0xca,0x94,0x4b,0x46,0x73,0xc6,0xc2,
0x2e,0xbd,0xbe,0x1a,0x6b,0x6a,0x09,0xd9,
0x26,0x07,0x48,0xe2,0x02,0xc7,0xa1,0xfe,
0x42,0x9f,0x55,0xc1,0x8a,0xcc,0xbf,0x79,
0xbb,0x95,0x28,0x96,0x8c,0xe7,0x8d,0x84,
0x5d,0x7b,0x7c,0x34,0xd6,0xd4,0x13,0xb2,
0x4c,0x0e,0x91,0xc4,0x05,0x8f,0x43,0xfc,
0x85,0x3e,0xab,0x83,0x15,0x99,0x7e,0xf3,
0x77,0x2a,0x51,0x2d,0x19,0xcf,0x1b,0x08,
0xba,0xf6,0xf8,0x69,0xad,0xa8,0x27,0x64,
0x98,0x1d,0x23,0x88,0x0b,0x1e,0x87,0xf9,
0x0a,0x7d,0x57,0x06,0x2b,0x32,0xfd,0xe6,
0xee,0x54,0xa2,0x5a,0x33,0x9e,0x36,0x11,
0x75,0xed,0xf0,0xd3,0x5b,0x50,0x4e,0xc9,
0x30,0x3a,0x47,0x10,0x16,0x3d,0x0f,0xf2,
0x14,0xfa,0xae,0x0c,0x56,0x65,0xfb,0xcd,
0xdc,0xa9,0x44,0xb4,0x67,0x3c,0x6c,0x22,
0xeb,0xdb,0xe1,0xa6,0xb6,0xa0,0x9d,0x92,
0x60,0x74,0x8e,0x20,0x2c,0x7a,0x1f,0xe4,
0x29,0xf5,0x5c,0x18,0xac,0xcb,0xf7,0x9b,
0xb9,0x52,0x89,0x68,0xce,0x78,0xd8,0x45,
0xd7,0xb7,0xc3,0x4d,0x6d,0x41,0x3b,0x24,
0xc0,0xe9,0x1c,0x40,0x58,0xf4,0x3f,0xc8,
0x53,0xea,0xb8,0x31,0x59,0x97,0xef,0x37,
0x72,0xa5,0x12,0xd1,0x9c,0xf1,0xb0,0x8b,
0xaf,0x6f,0x86,0x9a,0xda,0x82,0x76,0x49,
0x81,0xd2,0x38,0x80,0xb1,0xe8,0x7f,0x90,
0xa7,0xd5,0x70,0x62,0xb3,0x2f,0xde,0x6e,
0xe5,0x4a,0x25,0xa3,0x39,0xe3,0x61,0x17,
0x5e,0xdf,0x0d,0x35,0xb5,0x04,0xec,0x93,
0x03,0xa4,0x71,0x01,0x63,0xd0,0xff,0x21,
0x4f,0xaa,0xe0,0xc5,0x66,0x5f,0xbc,0xdd,
0xca,0x94,0x4b,0x46,0x73,0xc6,0xc2,0x2e,
0xbd,0xbe,0x1a,0x6b,0x6a,0x09,0xd9,0x26,
0x07,0x48,0xe2,0x02,0xc7,0xa1,0xfe,0x42,
0x9f,0x55,0xc1,0x8a,0xcc,0xbf,0x79,0xbb,
0x95,0x28,0x96,0x8c,0xe7,0x8d,0x84,0x5d,
0x7b,0x7c,0x34,0xd6,0xd4,0x13,0xb2			 
};


/**********************************************************************
* fraInit openes the file to read from and initializes the positions
* returns 0 if function failes
*/
int fraInit(pszFileName, begin, end,bSave )
char *pszFileName;
ulong begin,end;
int bSave;
{
uint Seconds;
g_bSave = bSave;

/* first try to open file */
g_fileHandle = fopen(pszFileName,"r");
if (NULL == g_fileHandle) {
	Report(lvSummary,"Cannot open input file \"%s\"",pszFileName);
	return 0;
	}
	
/*Now an estimation of the time to process the raw data is performed*/
fseek(g_fileHandle,0,2); /*file end*/
g_FileLength=ftell(g_fileHandle);
/*87MB of Data needed 4 min 37 seconds */
Seconds=(uint) ((g_FileLength/Size_Reference)*Time_Reference);
Report(lvSummary,"Input file contains %lu Bytes",g_FileLength);
Report(lvSummary,"Randi will have to WORK HARD!  for about %u min %u sec",
                 (uint)(Seconds/60),Seconds%60);	

/* setting the position */
g_luFileOffset = begin;
fseek(g_fileHandle,g_luFileOffset,0);

if (end) {
	g_luLastByte = end;
	Report(lvSummary,"Opened input file \"%s\" for scanning from byte %lu to %lu",
			pszFileName,begin,end);
	}
else {
	g_luLastByte = 0xFFFFFFFF;
	Report(lvSummary,"Opened input file \"%s\" for scanning from byte %lu to EOF",
			pszFileName,begin);
	}

/* reinitialize other global variables */
g_nBitOffset = 0;
g_nBufferOffset = 0;
g_nValidBytesInBuf = 0;

g_stat.bSynchron = 0;
g_stat.loss.luCount = 0L;
g_stat.loss.luFileOffset = 0L;
g_stat.found.luFileOffset = 0L;
g_stat.found.nBitOffset = 0L;
g_stat.frame.luMissCount = 0L;
g_stat.frame.luUnsave = 0L;


/* filling the buffer */
fraFillBuffer(0);

return 1;
}


/*********************************************************************
* uint fraGetRelativeFilePosition()
* returns the actual File Position in % compared to the input
* file size
*/
uint fraGetRelativeFilePosition()
{
return ((uint) ( 100.0 * (float)(g_luFileOffset+(ulong)g_nBufferOffset) / 
                  (float)g_FileLength) );
}
  

/**********************************************************************
* fraExit
* closes the open raw date file
*/
void fraExit()
{
if (NULL != g_fileHandle) {
	Report(lvSummary,"Input file closed");
	fclose(g_fileHandle);
	}
}

/**********************************************************************
* fraGetNextFrame
* reads the next frame into the frame structure and unscrambles the data.
* Sets g_nBufferOffset to the begining of the next frame
* returns 0 if no more frames are found else it returns the file offset
* where the (frame after the actual frame?) frame is located if 
* the file offset is 0 it returns 1
*
*/
#define CRC_LENGTH 2
ulong fraGetNextFrame(pFrame)
struct frame *pFrame;
{
char *pTmp;
uchar *upTmp;

/* find next frame */
pTmp = fraFindNextFrame();
/* end of scan region ? */
if (NULL ==  pTmp)
	return 0L;

/* decode frame */
memcpy(pFrame->sync,pTmp,PATTERN_SIZE);
pFrame->MCCount = pTmp[PATTERN_SIZE+2]; /*Master   Channel Frame Counter, Rev  F S34 botto*/ 
pFrame->VCCount = pTmp[PATTERN_SIZE+3];  /*no Formatcounter, virtuall Channel 
                                                 Counter instead */ 
g_FirstFrame = ( ~ pTmp[PATTERN_SIZE+1])&2; /* is used as a flag to indicate first Frame*/

pFrame->CRC.Msb=pTmp[FRAME_SIZE-2];
pFrame->CRC.Lsb=pTmp[FRAME_SIZE-1];

/*update the current format counter*/
/*printf("Current Frame Counter is now V,M===> %u, %u\n", pFrame->VCCount, pFrame->MCCount); *//*Jason mod*/
/* Jason mod -- The VCC is a free running counter ALLWAYS, EXCEPT when one
   		finds the first frame of a format.  Then it changes to indicate
		the format number.  For this reason, the FormatCounter can only
		be read when one finds the first frame of a format.  If this is
		not the 1st frame, I will leave the format count unmolested, i.e.
		undefined. */
if (g_FirstFrame)
	foaCalculateFoCount(pFrame->VCCount);

/*  check of  CRC (before Unscrambling!) */
/* CRC Check  doesn't work at the moment...
if (!fraCrcOk(pFrame->CRC.Msb,pFrame->CRC.Lsb,
         pTmp+(FRAME_SIZE - DATAFIELD_LENGTH - CRC_LENGTH)))
  Report(lvFrame,"CRC of Frame (%u) Format %u is incorrect",
         (uint) pFrame->MCCount,
         foaGetFormatCount() );
... so I just left it out. */         
  
fraDescramble(pFrame->data,pTmp  + (FRAME_SIZE - DATAFIELD_LENGTH - CRC_LENGTH) );
             
g_nBufferOffset += FRAME_SIZE;

/* Is the data valid (last  11Bits of Header must be 1) */
upTmp=(uchar *) pTmp;
if (! ( ((upTmp[8] &7)==0x7) && (upTmp[9]==0xff)) )
  Report(lvFrame,"Data of Frame (%u) Format %u is not valid!",
          (int) pFrame->MCCount,
          foaGetFormatCount() );

return (g_luFileOffset + (ulong)g_nBufferOffset - (ulong)FRAME_SIZE) ? 
	g_luFileOffset + (ulong)g_nBufferOffset - (ulong)FRAME_SIZE: 1L;
}

/******************************************************************
* int  fraIfFirstFrame
* returns 1 if it is the first Frame of a Format
* otherwise 0
*/
int fraIfFirstFrame()
{
  return g_FirstFrame;
}

/**********************************************************************
* fraDescramble
* descrambles the scrambled data, using the scrambling pattern in
* g_cScrambler
*/
void fraDescramble(dest,src)
char *dest, *src;
{
int b;
register int count;
register char *d=dest,*s=src,*c=g_cScrambler;

for (count = 0; count < DATAFIELD_LENGTH; count++)

  (*d++) = (*s++) ^ (*c++);
/*	printf("Data as read by randi");
        for (b = 0; b < DATAFIELD_LENGTH; b++)
        printf("%.2x",(unsigned char)dest[b]);
        putchar('\n');*/

}

/****************************************************************
* fraCrcOk
* generates CRC and
* returns 1 if CRC is equal to the received CRC, else 0
* see REV F page 41
*/
int fraCrcOk(msb,lsb,data)
uchar msb,lsb;
uchar *data;
{
  register uint reg;
  register int bytecount;
  register uchar x0,x5,x8,x12,in;
  register char bitcount;

  reg=0xffff; /*start condition*/
  for (bytecount=0;bytecount<DATAFIELD_LENGTH;bytecount++)
    for (bitcount=7;bitcount>=0;bitcount--)
      {
      in=(data[bytecount] >>bitcount)&1;
      x0=((reg & 0x8000)>>15)^in;
      x8=(reg & 128)>>7;
      x5=((reg & 16)>>4)^x0;
      x12=((reg & 2048)>>11)^x0;
      /*shift and clear register-cells x1,x6,x9,x13*/
      reg=(reg<<1)&0xeede; 
      reg|=(x0+x5*32+x8*256+x12*4096);
      }
  if ( ((uint)(msb<<8) | (uint)lsb)==reg)
    return 1;
  return 0;    
}



/**********************************************************************
* fraFindNextFrame
* looks for the next frame in the file. If the file is synchron,
* the sync bytes of the current frame are tested. If they are OK
* the frame is assumed to be OK and returns a poiter to the start
* of the frame within the buffer.
* If the buffer needs to be refilled, it will be refilled.
* If the file is not synchron or a synchronization loss is detected,
* the next synchronization pattern is searched. Once found, the next
* synchronization pattern is tested. If it is not OK the search will be
* continued, otherwise the synchronization is assumed to be found
*
* The function returns a pointer to the start of the frame within the
* buffer. It returns NULL, if no frame is found because either the
* file end was, or the end of the scanning area was reached.
*/
char *fraFindNextFrame()
{
int nBufferBytes;
int bTmp;

if (g_bSynchron) {
	/* reread buffer if necessary */
	if (g_nBufferOffset > BUFFER_LEN - FRAME_SIZE - PATTERN_SIZE - 1) {
		fraFillBuffer(BUFFER_LEN - g_nBufferOffset);
		g_nBufferOffset = 0;
		}
	/* are there enough bytes left ? */
	if (FRAME_SIZE > g_nValidBytesInBuf - g_nBufferOffset)
		return NULL;
	
	/* test if still synchron, i.e. are the synchron bytes OK? */
	/* fraComparePattern takes the bitoffset rather than the byte
	* offset as third parameter. Therefore stands the "* 8"
	*/
	g_bSynchron = fraComparePattern(g_cPattern,PATTERN_SIZE,
                                (g_nBufferOffset) * 8);
	/* test if next frame is synchron too!
	*   if not -> unsave message
	*/
	/* fraComparePattern takes the bitoffset rather than the byte
	* offset as third parameter. Therefore stands the "* 8"
	*/
	bTmp = fraComparePattern(g_cPattern,PATTERN_SIZE,
				(g_nBufferOffset + FRAME_SIZE) * 8);
	if (!bTmp) {
	    Report(lvSync,"Missing sync of following frame - UNSAVE frame!!!");
	    g_stat.frame.luUnsave++;
	    /* if the save option is on, mark sync loss */
	    if (g_bSave)
		g_bSynchron = 0;
	    }
	if (!g_bSynchron) {
	    g_stat.loss.luCount++;
	    g_stat.loss.luFileOffset = g_luFileOffset + g_nBufferOffset;
	    Report(lvSync,"Lost synchronization at file offset %lu",
				g_stat.loss.luFileOffset);
	    }
	}

/* if never been synchron or if synchronization lost resynchronize */
if (!g_bSynchron) {
	do {
		if( !fraFindSync(g_cPattern, PATTERN_SIZE) )
			return NULL;
		g_bSynchron = fraComparePattern(g_cPattern, PATTERN_SIZE,
                                                FRAME_SIZE * 8 );
		/* if not sync the next 3 bytes cannot synchronize!!! */
		if (!g_bSynchron) 
			g_nBufferOffset = 3;
		} while (!g_bSynchron);
	g_stat.bSynchron = 1;
	g_stat.found.luFileOffset = g_luFileOffset + g_nBufferOffset;
	g_stat.found.nBitOffset = g_nBitOffset;
	Report(lvSync,"Found synchronization at file offset %lu, bit %d",
			g_stat.found.luFileOffset, g_stat.found.nBitOffset);
	}

g_stat.bSynchron = g_bSynchron;

return g_pcBuffer + g_nBufferOffset;
}

/**********************************************************************
* fraFillBuffer
* fills the buffer, copying nCopy bytes from the end to the beginning
* of the buffer (so nCopy old Bytes remain in the Buffer),
* filling the rest of the buffer from the file, shifting
* the last copied byte and the new read bytes g_nBitOffset left, but does
* not shift the last byte. Updates g_nBytesValidInBuf and g_luFileOffset.
*/
int fraFillBuffer(nCopy)
int nCopy;
{
int nShiftOffset;
int nBytesRead;

/* if there are not enough bytes in the buffer, try to reread the whole */
if (g_nValidBytesInBuf < BUFFER_LEN - nCopy) {
	nShiftOffset = 0;
	nBytesRead = fread(g_pcBuffer,sizeof(char),
					BUFFER_LEN,g_fileHandle);
	}
else {
	nShiftOffset = nCopy - 1;
	/* copying */
	memcpy(g_pcBuffer,g_pcBuffer + BUFFER_LEN - nCopy , nCopy);
	/* reading */
	nBytesRead = fread(g_pcBuffer + nCopy, sizeof(char),
				BUFFER_LEN - nCopy, g_fileHandle);
	}
g_nValidBytesInBuf = nCopy + nBytesRead;
g_luFileOffset = ftell(g_fileHandle) - g_nValidBytesInBuf;
if (ftell(g_fileHandle) > g_luLastByte)
        g_nValidBytesInBuf =  g_luLastByte - g_luFileOffset;

/* if end of file reached return 0 */
if (g_nValidBytesInBuf < FRAME_SIZE)
	return 0;

/* shift now */
g_pcBuffer[BUFFER_LEN - 1] = fraShiftBuffer(g_pcBuffer + nShiftOffset, 
			BUFFER_LEN - nShiftOffset, 
			g_nBitOffset);
if (ftell(g_fileHandle) > g_luLastByte)
	g_nValidBytesInBuf =  g_luLastByte - g_luFileOffset;
return 1;
}



/**********************************************************************
* fraFindSyncInBuffer
* searches bitwise for the sync pattern beginning with the start of 
* the buffer. When found, it returns the number of bit shifts needed
* to find the pattern. If not found, it returns a negative number
*/
int fraFindSyncInBuffer(pSyncPattern,nPatternLength)
int nPatternLength;
char *pSyncPattern;
{
char cComp;
int count,count1;
int bFound = 0;
int nMaxShift = (g_nValidBytesInBuf - nPatternLength - 1) * 8;

for( count = 0; count <= nMaxShift; count++) {
	bFound = fraComparePattern(pSyncPattern,nPatternLength,count);
	if (bFound)
		break;
	}
return bFound ? count : -1;
}

/**********************************************************************
* fraFindSync
* searches for the  sync pattern. Assumes that the buffer is already
* filled. If the pattern is found, 
* it sets the variables g_nBitOffset, g_luFileOffset
* reads g_pcBuffer and shifts it, except for the last byte...
* If not found it rereads the buffer, updates g_luFileOffset and 
* continues search until g_luLastByte, file end or found.
* returns 0 if not found
*/
int fraFindSync(pSyncPattern,nPatLen)
int nPatLen;
char *pSyncPattern;
{
int nBitOffset;

/* reread buffer to be byte aligned */
fseek(g_fileHandle,g_luFileOffset + g_nBufferOffset,0);
g_luFileOffset = ftell(g_fileHandle);
g_nValidBytesInBuf = fread(g_pcBuffer,sizeof(char),BUFFER_LEN,g_fileHandle);
g_nBufferOffset = 0;
g_nBitOffset = 0;
if (ftell(g_fileHandle) > g_luLastByte) 
	g_nValidBytesInBuf = g_luLastByte - g_luFileOffset;

/* read until pattern found or file end */
for ( nBitOffset = fraFindSyncInBuffer(pSyncPattern,nPatLen);
	nBitOffset < 0; nBitOffset = fraFindSyncInBuffer(pSyncPattern,nPatLen)) 
	{
	printf("Searching Sync at file offset %lu\r",g_luFileOffset);
	if ( !fraFillBuffer(nPatLen+1) )
		break;
	}
printf("                                           \r");
/* if not found return 0 */
if (nBitOffset < 0)
	return 0;

/* shift buffer as far as needed. Not the last Byte! */
g_pcBuffer[BUFFER_LEN-1] = fraShiftBuffer(g_pcBuffer + nBitOffset / 8, 
		BUFFER_LEN - nBitOffset / 8,
		nBitOffset % 8);

/* set variables */
g_nBitOffset = nBitOffset % 8;

/* reread buffer */
fraFillBuffer(BUFFER_LEN - nBitOffset / 8);

return (g_nValidBytesInBuf >= FRAME_SIZE);
}

/**********************************************************************
* fraGetShiftedChar
* returns the character (8 Bit) at the bit offset position within g_pcBuffer.
*/
char fraGetShiftedChar(nBitOffset)
int nBitOffset;
{
char ret;
ret = (g_pcBuffer[nBitOffset / 8]) << (nBitOffset % 8);
ret |= ((unsigned char)g_pcBuffer[nBitOffset / 8 + 1]) >> 
		(8 - (nBitOffset % 8));
return ret;
}

/**********************************************************************
* fraShiftBuffer 
* shifts nSize bytes in pcBuffer nShift bit left. the
* first nShift bits will be lost and the last Byte of the buffer will
* be filled with 0
* Returns the unchanged last byte of the buffer.
*/
char fraShiftBuffer(pcBuffer, nSize, nShift)
char *pcBuffer;
int nSize,nShift;
{
register int count;
register int nRegShift = nShift;
char ret;


if (nShift > 7) {
	printf("fraShiftBuffer - nShift invalid number\n");
	exit(1);
	}
if (nSize <= 0) {
	printf("fraShiftBuffer - invalid buffer size\n");
	exit(1);
	}

for(count = 0; count < nSize-1; count++) {
	pcBuffer[count] = (pcBuffer[count] << nRegShift)
			| ((unsigned char)pcBuffer[count+1] >> (8 - nRegShift));
	}
ret = pcBuffer[nSize-1];
pcBuffer[nSize-1] = ret << nRegShift;

return ret;
}

/**********************************************************************
* fraComparePattern
* compares the pattern with the bits at nBitOffset in g_pcBuffer
* returns 0 if no match
*/
int fraComparePattern(pcPattern, nPatLen, nBitOffs)
char *pcPattern;
int nPatLen, nBitOffs;
{
int count,bFound;

for (count = 0; count < nPatLen; count++) {
	if ( fraGetShiftedChar(nBitOffs + count*8) != pcPattern[count] )
		return 0;
	}
return 1;
}
 

/**********************************************************************
* fraPrintBufHex
* this is a function for debugging purpose only. It dumps the contents
* of the buffer as hex to the screen. It dumps nLines lines whith 
* 32 bytes
*/
void fraPrintBufHex(nLines)
int nLines;
{
int count1,count2;
char cTmp[9];

if (nLines > BUFFER_LEN / 32) {
	printf("fraPrintBufHex - too many lines\n");
	nLines = BUFFER_LEN / 32;
	}

for( count1 = 0; count1 < nLines; count1++ ) {
	for (count2 = 0; count2 < 32; count2 ++) {
		sprintf(cTmp,"%2X",g_pcBuffer[count1*32 + count2]);
		if (cTmp[strlen(cTmp)-2] == ' ')
			cTmp[strlen(cTmp)-2] = '0';
		printf(cTmp + strlen(cTmp) - 2);
		printf( ((count2+1) % 8) ? " " : "  ");
		}
	printf("\n");
	}
}

/**********************************************************************
* fraPrintBufBin
* this is a function for debugging purpose only. It dumps nLines a 8
* bytes binary onto the standard output.
*/
void fraPrintBufBin(nLines)
int nLines;
{
int count1,count2,count3,tmp;

if (nLines > BUFFER_LEN / 8) {
	printf("fraPrintBufBin - too many lines\n");
	nLines = BUFFER_LEN / 8;
	}

for( count1 = 0; count1 < nLines; count1++ ) {
	for (count2 = 0; count2 < 8; count2 ++) {
		tmp = g_pcBuffer[count1*8 + count2];
		for (count3 = 7; count3  >= 0; count3--) 
			printf("%d", (tmp & (1 << count3) ) >> count3 );
		printf(" ");
		}
	printf("\n");
	}
}

