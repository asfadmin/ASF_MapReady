/* foa.c
* format access of the raw data
* assembles and reads full formats
*
* first setup 07-12-1994 Hans-Joerg Wagner
* frist release 07-25-1994 Hans-Joerg Wagner
*/
#include <stdio.h>
#include <memory.h>
#include "andi.h"

/**********************************************************************
* foaInit allocates memory and initializes the frame wise access layer
* returns 0 if it failes 
*/
int foaInit(pszFileName,luStart,luEnd,bSave)
char *pszFileName;
unsigned long luStart,luEnd;
int bSave;
{
g_stat.format.luMissCount = 0;
return fraInit(pszFileName,luStart,luEnd);
}

/**********************************************************************
* foaExit frees memory and calls the exit routin of the frame wise
* access layer
*/
void foaExit()
{
fraExit();
}

/**********************************************************************
* foaGetNextFormat( union format *pForm )
* fills the next format into the union
* returns 0 if end of the reading area is reached. Otherwise it returns
* the file offset where the format begins
*/
ulong foaGetNextFormat(pForm)
union format *pForm;
{
static ushort uCyclicLastFormat;
static int bCyclicLastFormatInitalized = 0;
struct frame myFrame;
ulong luFileOffset;
int bFullyRead = 0;
int bFirstForm = 0;
int bFirstTry = !g_stat.bSynchron;   /* the first try for sync?? */
int bAlreadyReported = 0;
ushort uFormCount;
uchar cFrameCount = 0;		/* first frame expected is frame 1 */

/* repeat until file end or full format read */
do {
	luFileOffset = fraGetNextFrame(&myFrame);

	/* end of file or scan region */
	if ( 0L == luFileOffset )
		return 0L;

	/* if first try of sync report success */
	if (bFirstTry) {
	    Report(lvSummary,"First Sync found at file offset %lu,bitoffset %d",
		g_stat.found.luFileOffset, g_stat.found.nBitOffset);
	    bFirstTry = 0;
	    cFrameCount = myFrame.frameNo;
	    uFormCount = GET_SAT_USHORT(myFrame.formCycler);
	    pForm->s.bZero = (myFrame.fc == 1) ? 1 : 0;
	    }

/* Jason mod -- diag print for funny raw data */
Report(lvFrame,"cFrameCount = %d, myFrame.frameNo = %d", 
	 (int) cFrameCount, (int) myFrame.frameNo);

	/* is it the frame that was expected: 
	*   frame number cFrameCount;
	*/
	if /*Jason mod*/ (0 && ( cFrameCount != myFrame.frameNo ) ){
	    bFirstForm = 0;	/* everything read until here is invalid*/
	    Report(lvFrame,"Frame %d missed (f=%u); found frame %d(f=%u) Msg 1",
			(int)cFrameCount,uFormCount,
			myFrame.frameNo,GET_SAT_USHORT(myFrame.formCycler));
	    g_stat.frame.luMissCount++;
	    if (g_stat.level != lvSync) {
	        Report(lvFrame,"SyncStat:lost at offs %lu;found at offs %lu bit %d",
		     g_stat.loss.luFileOffset, 
		     g_stat.found.luFileOffset, g_stat.found.nBitOffset);
		}
	    cFrameCount = myFrame.frameNo + 1;
	    uFormCount = GET_SAT_USHORT(myFrame.formCycler);
	    if (cFrameCount == 29) {cFrameCount = 0; uFormCount++;}
            pForm->s.bZero = (myFrame.fc == 1) ? 1 : 0;
	    }
	else if (0 == myFrame.frameNo) {
	    /* first frame of a format */
	    memcpy(pForm->ucFrameData[cFrameCount],myFrame.data,250);
	    uFormCount = GET_SAT_USHORT(myFrame.formCycler);
	    pForm->s.bZero = (myFrame.fc == 1) ? 1 : 0;
	    cFrameCount = 1;
	    bFirstForm = 1;
	    }
	else if /*Jason mod*/ (0 && ( uFormCount != GET_SAT_USHORT(myFrame.formCycler) ) ){
	    /* frame missed */
	    Report(lvFrame,"Frame %d missed (f=%u); found frame %d(f=%u) Msg 2",
			(int)cFrameCount,uFormCount,
			myFrame.frameNo,GET_SAT_USHORT(myFrame.formCycler));
	    if (g_stat.level != lvSync) {
                Report(lvFrame,"SyncStat:lost at offs %lu;found at offs %lu bit %d",
		    g_stat.loss.luFileOffset,
		    g_stat.found.luFileOffset, g_stat.found.nBitOffset);
		}
	    g_stat.frame.luMissCount++;
            cFrameCount = myFrame.frameNo + 1;
	    uFormCount = GET_SAT_USHORT(myFrame.formCycler);
	    if (cFrameCount == 29) {cFrameCount = 0; uFormCount++;}
	    pForm->s.bZero = (myFrame.fc == 1) ? 1 : 0;
	    }
	else if ( pForm->s.bZero != (myFrame.fc == 1) ? 1 : 0 ) {
	    /* change between zero and echo data within format */
	    Report(lvFrame,"Change from zero format to data format within format");
	    pForm->s.bZero = (myFrame.fc == 1) ? 1 : 0;
	    bFirstForm = 0;
	    }
	else if (28 == myFrame.frameNo) {
	    /* last frame of format */
	    memcpy(pForm->ucFrameData[28],myFrame.data,250);
	    if (bFirstForm) 	/* if there was found a first form and no miss*/
		break;		/* fully read, therfore end of do loop */
	    cFrameCount = 0;
	    uFormCount++;
	    }
	else if (28 < myFrame.frameNo) {
	    /* invalid frame number */
	    Report(lvFrame,"Invalid frame No %d",myFrame.frameNo);
	    cFrameCount = 0;
	    bFirstForm = 0;
	    }
	else {
	    /* any other frame - just copy */
	    memcpy(pForm->ucFrameData[cFrameCount],myFrame.data,250);
	    cFrameCount++;
	    }
	} while( 1 ); /* exit loop is managed with breaks */
	    
/* is there a format missed? */
if (bCyclicLastFormatInitalized) {
	if ( uCyclicLastFormat + 1 != uFormCount ) {
	    g_stat.format.luMissCount+= uFormCount - uCyclicLastFormat - 1;
	    Report(lvFormat,"Missing %ld formats during format scan at file offset %lu",
			uFormCount - uCyclicLastFormat - 1, luFileOffset);
		}
	}
else {
	bCyclicLastFormatInitalized = 1;
	}
uCyclicLastFormat = uFormCount;

return luFileOffset;
}

