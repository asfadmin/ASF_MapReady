
/* foa.c          RADARSAT Modification
* format access of the raw data
* assembles and reads full formats
*
* first setup 07-12-1994 Hans-Joerg Wagner
* frist release 07-25-1994 Hans-Joerg Wagner
*
* RADARSAT Modification
* by Christian Fischer
* 09-25-95
*
*/
#include <stdio.h>
#include <memory.h>
#include "randi.h"

/*modul global  variables*/
uchar  g_NumberofFrames;
uint   g_CurrentFormatCount;

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
return fraInit(pszFileName,luStart,luEnd,bSave);
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
* the file offset where the format (after the next one?)begins
*/
ulong foaGetNextFormat(pForm)
union format *pForm;
{
static struct frame myFrame;
static ulong luFileOffset;

int alreadyFramesRead=0;
int bFirstTry = !g_stat.bSynchron;   /* the first try for sync?? */
int bAlreadyReported = 0;
uchar cFrameCount;
uchar cFrameOffset;    /*cFrameCount is a free running counter, this offset
                        *is the value of cFrameCount at the first Frame of
                        *a Format */
uint ExpectedFormatCount;
int firstFrame;
int NotYetReported;
static  int BeginOfNewFormat=0;


alreadyFramesRead=0;
NotYetReported=1;
/* repeat until file end or full format read */
do {
        /* If it is the first Frame of a new Format, it was already read at
         * the end of the last Format (the end was detected by this 
         * first Frame) - so don't try to read it again */
        if (!BeginOfNewFormat)
        	luFileOffset = fraGetNextFrame(&myFrame);
        BeginOfNewFormat=0;
        
	/* end of file or scan region */
	if ( 0L == luFileOffset )
		return 0L;

        firstFrame=fraIfFirstFrame();    /*1*/
        
	/* if first try of sync report success */
	if (bFirstTry) {
	    Report(lvSummary,"First Sync found at file offset %lu,bitoffset %d",
		g_stat.found.luFileOffset, g_stat.found.nBitOffset);
	    bFirstTry = 0;
	    }
	if (alreadyFramesRead)           /*2*/
	  {
	  if (firstFrame)                /*3*/
	    {
	      if (cFrameCount!=myFrame.MCCount)      /*6*/
	        {
	          alreadyFramesRead=0;
		  /*don't skip the actual frame by reading the next one*/
		  BeginOfNewFormat=1;  
		  /* Jason Feature: After a loss in synchro  the time shall be 
		   * written again */		   
		  g_RewriteTime=1;
		  foaReportMiss(cFrameCount,cFrameOffset,myFrame.MCCount,
		                ExpectedFormatCount,luFileOffset);
		  g_stat.frame.luMissCount++;
                }
              else {                       
	        BeginOfNewFormat=1;
	        break;  /*whole Format successfully read-finish*/
	        }
	    }  
	  else if ((cFrameCount==myFrame.MCCount   )&&        /*4*/
 		   (ExpectedFormatCount == g_CurrentFormatCount))
	         { /*read Frame */
		 memcpy(pForm->ucFrameData[(uchar) (cFrameCount-cFrameOffset)],
                        myFrame.data,DATAFIELD_LENGTH);
	         cFrameCount++;
	         alreadyFramesRead++;
		 if (alreadyFramesRead>MAX_FRAME_NO)  
		    {
                     Report(lvFormat,"Format %u contains too many Frames",
                            foaGetFormatCount());
                     break;
                   }
                 }
	       else 
                 {

		   alreadyFramesRead=0;
		   /*don't skip the actual frame by reading the next one*/
		   BeginOfNewFormat=1;  
		   /* Jason Feature: After a loss in synchro  the time shall be 
		    * written again */		   
		   g_RewriteTime=1;
		   foaReportMiss(cFrameCount,cFrameOffset,myFrame.MCCount,
		                 ExpectedFormatCount,luFileOffset);
 		   g_stat.frame.luMissCount++;
		 }
     	  }
	else                           
	  {/* Searching for beginning of new Format,
	    *  set Counters new */
	  cFrameCount = myFrame.MCCount;
	  ExpectedFormatCount = g_CurrentFormatCount;
	  cFrameOffset= cFrameCount;
	  if (firstFrame)                         /*5*/
	  /* Beginning of new Format */
	    {
             /*only report finding of a  new  format after a loss of 
              *formats before
              */
               if (!NotYetReported)
               {
                 Report(lvFormat,
                   "First Frame (%u) of Format %u found at Fileposition %lu",
                   cFrameCount,foaGetFormatCount(),luFileOffset);
                 NotYetReported=1;
               }    	        
             /*read Frame */
             memcpy(pForm->ucFrameData[(uchar) (cFrameCount-cFrameOffset)],
                      myFrame.data,DATAFIELD_LENGTH);
             cFrameCount++;
             alreadyFramesRead++;
            }
 	    else 
	      /* missing startframe*/
	      if (NotYetReported)
	          {
	            foaReportMiss(cFrameCount,cFrameOffset,myFrame.MCCount,
		                  ExpectedFormatCount,luFileOffset);
		    NotYetReported=0;
		  }       
	  }
} while (1);  /*exit loop is managed with breaks*/

g_NumberofFrames=(uchar) (cFrameCount-cFrameOffset);
return luFileOffset;
}

/************************************************************************
* does the same as foaGetNextFormat, but without any checks of counters.
* Therefore, strange and corrupted raw data will be read, but the results
* might be unreliable!!!
*/
ulong foaGetNextFormat_CareFree(pForm)
union format *pForm;
{
static struct frame myFrame;
static ulong luFileOffset;

int alreadyFramesRead=0;
int bFirstTry = !g_stat.bSynchron;   /* the first try for sync?? */
int bAlreadyReported = 0;
uchar cFrameCount;
uchar cFrameOffset;    /*cFrameCount is a free running counter, this offset
                        *is the value of cFrameCount at the first Frame of
                        *a Format */
uint ExpectedFormatCount;
int firstFrame;
int NotYetReported;
static  int BeginOfNewFormat=0;


alreadyFramesRead=0;
NotYetReported=1;
/* repeat until file end or full format read */
do {
        /* If it is the first Frame of a new Format, it was already read at
         * the end of the last Format (the end was detected by this 
         * first Frame) - so don't try to read it again */
        if (!BeginOfNewFormat)
        	luFileOffset = fraGetNextFrame(&myFrame);
        BeginOfNewFormat=0;
        
	/* end of file or scan region */
	if ( 0L == luFileOffset )
		return 0L;

        firstFrame=fraIfFirstFrame();    /*1*/
        
	/* if first try of sync report success */
	if (bFirstTry) {
	    Report(lvSummary,"First Sync found at file offset %lu,bitoffset %d",
		g_stat.found.luFileOffset, g_stat.found.nBitOffset);
	    bFirstTry = 0;
	    }
	if (alreadyFramesRead)           /*2*/
	  {
	  if (firstFrame)                /*3*/
	    {
	      if (0)      /*6*/
	        {
	          alreadyFramesRead=0;
		  /*don't skip the actual frame by reading the next one*/
		  BeginOfNewFormat=1;  
		  /* Jason Feature: After a loss in synchro  the time shall be 
		   * written again */		   
		  g_RewriteTime=1;
		  foaReportMiss(cFrameCount,cFrameOffset,myFrame.MCCount,
		                ExpectedFormatCount,luFileOffset);
		  g_stat.frame.luMissCount++;
                }
              else {                       
	        BeginOfNewFormat=1;
	        break;  /*whole Format successfully read-finish*/
	        }
	    }  
	  else if (1)
 		   
	         { /*read Frame */
		 memcpy(pForm->ucFrameData[(uchar) (cFrameCount-cFrameOffset)],
                        myFrame.data,DATAFIELD_LENGTH);
	         cFrameCount++;
	         alreadyFramesRead++;
		 if (alreadyFramesRead>MAX_FRAME_NO)  
		    {
                     Report(lvFormat,"Format %u contains too many Frames",
                            foaGetFormatCount());
                     break;
                   }
                 }
	       else 
                 {

		   alreadyFramesRead=0;
		   /*don't skip the actual frame by reading the next one*/
		   BeginOfNewFormat=1;  
		   /* Jason Feature: After a loss in synchro  the time shall be 
		    * written again */		   
		   g_RewriteTime=1;
		   foaReportMiss(cFrameCount,cFrameOffset,myFrame.MCCount,
		                 ExpectedFormatCount,luFileOffset);
 		   g_stat.frame.luMissCount++;
		 }
     	  }
	else                           
	  {/* Searching for beginning of new Format,
	    *  set Counters new */
	  cFrameCount = myFrame.MCCount;
	  ExpectedFormatCount = g_CurrentFormatCount;
	  cFrameOffset= cFrameCount;
	  if (firstFrame)                         /*5*/
	  /* Beginning of new Format */
	    {
             /*only report finding of a  new  format after a loss of 
              *formats before
              */
               if (!NotYetReported)
               {
                 Report(lvFormat,
                   "First Frame (%u) of Format %u found at Fileposition %lu",
                   cFrameCount,foaGetFormatCount(),luFileOffset);
                 NotYetReported=1;
               }    	        
             /*read Frame */
             memcpy(pForm->ucFrameData[(uchar) (cFrameCount-cFrameOffset)],
                      myFrame.data,DATAFIELD_LENGTH);
             cFrameCount++;
             alreadyFramesRead++;
            }
 	    else 
	      /* missing startframe*/
	      if (NotYetReported)
	          {
	            foaReportMiss(cFrameCount,cFrameOffset,myFrame.MCCount,
		                  ExpectedFormatCount,luFileOffset);
		    NotYetReported=0;
		  }       
	  }
} while (1);  /*exit loop is managed with breaks*/

g_NumberofFrames=(uchar) (cFrameCount-cFrameOffset);
return luFileOffset;
}
/* end of foaGetNextFormat_CareFree */


/**************************************
* Returns Format Counter
*/
uint foaGetFormatCount()
{
return g_CurrentFormatCount;
}

/***************************************
* calculates the format counter
*
*create out of the 1 Byte free running ImageFormatCounter (last
* Byte of Imageformatcounter) in myFrame->VCCount a 4 Byte format 
* Counter  that is always 
* counting up. Do this  by a SerialCounter (first 3 Bytes of Imageformat-
* counter)  that  counts up, when
* VCCount runs over zero
*/
void foaCalculateFoCount(VCCount)
uchar VCCount;
{
static uchar oldVCCount=0;
static uint SerialCounter;

/*printf("VCCount=%u, oldVCCount=%u, SerialCounter=%u\n", VCCount, oldVCCount,
	SerialCounter); *//*Jason mod -- debug*/
if (VCCount<oldVCCount)
  {
  /* even in case of many missing formats (like 200 missing formats after
  * number 100) the Serialcounter should be counted up correctly
  */
  SerialCounter++;
  }
oldVCCount=VCCount;
g_CurrentFormatCount=(SerialCounter<<8 | (uint) VCCount);  
}

/**********************
*returns Number of Frames of the actual Format
*/
uchar foaGetNoOfFrames()
{
  return g_NumberofFrames;
}

/*********************
/*used in Errormessages
*/
char *foaTag(number)
unsigned char number;
{
char *sTmp;
unsigned char uTmp;

uTmp=number%10;
switch (uTmp)
  {
  case 1:if (number==11) sTmp="th";
           else sTmp="st";
           break;
  case 2:if (number==12) sTmp="th";
             else sTmp="nd";
             break;
  case 3:if (number==13) sTmp="th";
               else sTmp="rd";
               break;
  default:sTmp="th";
  }
return sTmp;
}

/***********************
*foaReportMiss
*used to report missing frames/formats
*
*/
void foaReportMiss(ExpectedFrame,FrameOffset,CurrentFrame,
                   ExpectedFormat,FileOffset)
uchar ExpectedFrame,FrameOffset,CurrentFrame;
uint  ExpectedFormat;
ulong FileOffset;
{
/*don't report the frame number when it is a first frame
 *of a format, that might be confusing: 
 *e.g.
 *   Missed 1st Frame (18) of Format 138, found
 *   Frame (18) Format 138, Fileoffset 228
 *
 *otherwise report it*/
if ((ExpectedFrame-FrameOffset) == 0)
  Report(lvFormat,"Missed %u%s Frame of Format %u, found ",
          (uchar)(ExpectedFrame-FrameOffset+1),
          foaTag(ExpectedFrame-FrameOffset+1),
          ExpectedFormat);
else                          
   Report(lvFormat,"Missed %u%s Frame (%u) of Format %u, found ",
          (uchar)(ExpectedFrame-FrameOffset+1),
          foaTag(ExpectedFrame-FrameOffset+1),
          ExpectedFrame,ExpectedFormat);

Report(lvFormat,"       Frame (%u) Format %u, Fileoffset %lu",
   	 (uint)CurrentFrame,
   	 g_CurrentFormatCount,
   	 FileOffset);       
}
   	 
