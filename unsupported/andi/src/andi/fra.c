/* fra.c
* frame level access of the raw date
* searches for synchronization and unscrambles the data framewise
*
* first setup 07-12-1994 Hans-Joerg Wagner
* first release 07-25-1994 Hans-Joerg Wagner
*/
#include <stdio.h>
#include <memory.h>
#include <varargs.h>
#include <malloc.h>
#include "andi.h"

/* modul global variables */
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
struct STAT g_stat;	/* synchronazation status global!!! */

unsigned long int g_FileLength; /*added 8-23-95*/

char g_cPattern[] = { 0xfa,0xf3,0x20 } ;
/*char g_cPattern[] = { 0xfd,0xf9,0x20 } ;  Jason mod -- changed sync pattern!!! */

/* unscrambling pattern */
char g_cScrambler[] = { 0xb4,0xb4,0x99,0x2F,0xF6,0x4B,0xD0,0x24,
			0x2D,0xB4,0x90,0x99,0x2D,0xBF,0x6F,0x66,
			0x6F,0x4B,0x4B,0xDB,0xDB,0xD2,0xF6,0x64,
			0xBD,0x90,0x0B,0xF6,0xDB,0xD0,0x2D,0x09,
			0x26,0x49,0xB4,0xB6,0x42,0x42,0x6D,0x92,
			0x6F,0x64,0xB4,0x26,0xD0,0x02,0x64,0x92,
			0x6F,0xF6,0xFD,0x0B,0x42,0xDB,0xDB,0x40,
			0xBF,0x49,0x2F,0x49,0xB4,0x24,0x0B,0x6F,
			0xFF,0x4B,0xD0,0xB6,0x64,0x99,0x02,0x40,
			0x92,0x6D,0xBF,0xD9,0xBD,0x09,0xBD,0x24,
			0x0B,0xFD,0xB6,0x66,0x42,0x6F,0xDB,0x4B,
			0xD2,0xFF,0x40,0x2F,0x49,0x26,0x6D,0x26,
			0xFD,0x26,0xF4,0x02,0x66,0xDB,0x4B,0x40,
			0xB6,0x6D,0xBD,0x90,0x99,0xBF,0xF6,0x42,
			0xF4,0xB6,0xF4,0x99,0x0B,0x64,0x00,0xB4,
			0x92,0x42,0x40,0x24,0xB6,0xD9,0x2F,0xD2,
			0xD9,0x9B,0x40,0x9B,0xDB,0x64,0x2D,0x02,
			0x4B,0xFF,0xDB,0xD0,0xBF,0x40,0x0B,0xDB,
			0x6D,0x09,0x90,0x92,0xD2,0x40,0x2D,0x92,
			0x4B,0xF6,0xFF,0x42,0x66,0x6D,0x90,0x26,
			0x40,0x02,0x6D,0xB6,0xFD,0x2F,0xD0,0x90,
			0xBF,0xF6,0xD0,0xBD,0x9B,0x66,0x40,0xB4,
			0xB6,0xD0,0x0B,0x40,0x00,0xB6,0xDB,0x66,
			0xF6,0x6F,0xD0,0x26,0x64,0x90,0x26,0xD2,
			0x4B,0x40,0x24,0x24,0x90,0x02,0x40,0x00,
			0x24,0x92,0x4B,0x64,0xB6,0x6F,0xF4,0xB4,
			0x2F,0xF4,0x90,0xBD,0xBF,0xF4,0x0B,0xD0,
			0x00,0xBF,0xFF,0xF4,0x2F,0x42,0x4B,0xDB,
			0x49,0x9B,0xDB,0xF6,0x64,0x2F,0xD9,0x26,
			0x64,0x02,0x6F,0xFF,0xD9,0x99,0x9B,0xF6,
			0x40,0xBD,0x92,0x42,0xD2,0x6D,0x9B,0x4B,
			0xF6,0x6D };


/**********************************************************************
* fraInit openes the file to read from and initializes the positions
* returns 0 if function failes
*/
int fraInit(pszFileName, begin, end,bSave )
char *pszFileName;
ulong begin,end;
int bSave;
{
g_bSave = bSave;

/* first try to open file */
g_fileHandle = fopen(pszFileName,"r");
if (NULL == g_fileHandle) {
	Report(lvSummary,"Cannot open input file \"%s\"",pszFileName);
	return 0;
	}

/*get the length of the input file, added 8-23-95*/
fseek(g_fileHandle,0,2); /*file end*/
g_FileLength=ftell(g_fileHandle);



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
* where the frame is located if the file offset is 0 it returns 1
*
*/
ulong fraGetNextFrame(pFrame)
struct frame *pFrame;
{
char *pTmp;

/* find next frame */
pTmp = fraFindNextFrame();
/* end of scan region ? */
if (NULL ==  pTmp)
	return 0L;

/* decode frame */
memcpy(pFrame->sync,pTmp,PATTERN_SIZE);
pFrame->fc = ((unsigned char)pTmp[PATTERN_SIZE]) >> 6;
pFrame->frameNo = pTmp[PATTERN_SIZE] & 0x3F;
pFrame->formCycler.Msb = pTmp[PATTERN_SIZE+1];
pFrame->formCycler.Lsb = pTmp[PATTERN_SIZE+2];
fraDescramble(pFrame->data,pTmp + PATTERN_SIZE + 3);
g_nBufferOffset += FRAME_SIZE;

return (g_luFileOffset + (ulong)g_nBufferOffset) ? 
				g_luFileOffset + (ulong)g_nBufferOffset : 1L;
}

/**********************************************************************
* fraDescramble
* descrambles the scrambled data, using the scrambling pattern in
* g_cScrambler
*/
void fraDescramble(dest,src)
char *dest, *src;
{
register int count;
register char *d=dest,*s=src,*c=g_cScrambler;

for (count = 0; count < 250; count++) 
	(*d++) = (*s++) ^ (*c++);
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
* of the buffer, filling the rest of the buffer from the file, shifting
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
* returns the character at the bit offset position within g_pcBuffer.
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


/*********************************************************************
* added 8-23-95 by Christian Fischer
* uint fraGetRelativeFilePosition()
* returns the actual File Position in % compared to the input
* file size
*/
uint fraGetRelativeFilePosition()
{
return ((uint) ( 100.0 * (float)(g_luFileOffset+(ulong)g_nBufferOffset) /
                  (float)g_FileLength) );
                  }
                  
                  