/* al.c
* interface for the raw data scan utility.
* decodes the different parts of the formats and grants access to them
*
* first setup 07-18-1994 Hans-Joerg Wagner
* first release 07-25-1994 Hans-Joerg Wagner
*/
#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <malloc.h>
#include "drdapi.h"    /* for type SCcomplex */
#include "andi.h"

/* Variables to hold the scanned data until it is retrieved */
union format 	g_myForm;    			/* buffer to read Format */
int 		g_bIsImFoCountContinous;
ulong 		g_luICUBoardTime = 0;
int 		g_bIsNewICUBoardTime = 0;
ulong 		g_luImageFormatCounter;
ushort 		g_uSamplWinStart;
int 		g_bIsNewSamplWinStart = 0;
ushort 		g_uPulseRepInterval;
int 		g_bIsNewPulseRepInterval = 0;
int 		g_nCalSubsysAtten;
int 		g_bIsNewCalSubsysAtten = 0;
int 		g_nReceiverSubsysAtten;
int 		g_bIsNewReceiverSubsysAtten = 0;
int 		g_bLoopClosed;			/* No access function yet */
uchar 		*g_pucZero;
SCcomplex	g_pcCalRep[SIZE_OF_REPCAL];
int		g_bIsNewReplica = 0;
int		g_bIsNewCal = 0;
SCcomplex	g_pcNoiseEcho[SIZE_OF_ECHO];
int		g_bIsNewNoise = 0;
int		g_bIsNewEcho = 0;
int 		g_nCurrentOrbit;
int		g_bIsNewCurrentOrbit = 0;
union IDHTGH	g_myIdhtGh;			/* buffer for IDHT */
int		g_nIdhtRepCount = 0;
int 		g_nIdhtCounter;
struct EPHEMERIS g_seOrbits[16];
int 		g_bOrbitValid[16] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };



/**********************************************************************
* alInit 
* initializes the levels below and allocates the memory for
* the different data blocks
*/
int alInit(pszFileName,luStart,luEnd,bSave)
char *pszFileName;
ulong luStart,luEnd;
int bSave;
{
/* reinitialize global variables */
g_luICUBoardTime = 0;
g_bIsNewICUBoardTime = 0;
g_bIsNewSamplWinStart = 0;
g_bIsNewPulseRepInterval = 0;
g_bIsNewCalSubsysAtten = 0;
g_bIsNewReceiverSubsysAtten = 0;
g_bIsNewReplica = 0;
g_bIsNewCal = 0;
g_bIsNewNoise = 0;
g_bIsNewEcho = 0;
g_nIdhtRepCount = 0;
g_nIdhtCounter = -1;

g_stat.format.luImMissCount = 0L;

return foaInit(pszFileName,luStart,luEnd,bSave);
}


/**************************************************
writeOutESAHeaders:
	Print out information about the ESA Headers to fESAHeaders.
*/
void writeOutESAHeaders(void)
{
	static char lastOutLine[1000]={0};
	char outLine[1000];
	static long headerNo=0;
	sprintf(outLine,"FormatCode %x  OGRC %i OrbitNo %i  ICUtime %lu "
			"FormatCount %i  SampleStart %u  PulseRep %u "
			"Activity %x FirstCal %i\n",
	
		g_myForm.s.formatCode,g_myForm.s.ogrc,g_myForm.s.orbit,GET_SAT_ULONG(g_myForm.s.luBoardTime)
		,GET_SAT_ULONG(g_myForm.s.luImageFormatCount),
		         GET_SAT_USHORT(g_myForm.s.uSamplWinStart),GET_SAT_USHORT(g_myForm.s.uPulseRepInterval)
		,g_myForm.s.activity,g_myForm.s.bFirstCalRep);
	if (0!=strcmp(lastOutLine,outLine))
	{
		strcpy(lastOutLine,outLine);
		fprintf(fESAHeaders,"No %i %s",headerNo,lastOutLine);
	}
	headerNo++;
}

/**********************************************************************
* alExit
* frees the allocated memory space and calls the exit routine of the
* level below
*/
void alExit()
{
foaExit();
}

/**********************************************************************
* alNextFormat
* reads the next format data and converts it into the proper fields
* Then it sets the different boolean values for new data.
* returns 0 if no new Format could be found.
*/
int alNextBlock()
{
static int bIsPrevImFoCountValid = 0;
static ulong luPrevImFoCount;
static int nCalRepSubcomCount;
static int bIsCalRepOk = 0;	/* no error while filling the buffer*/
static int bIsCal;		/* set to 1 if cal, set to 0 if rep */
static int nIdhtStart;
static int nIdhtCurrentSubcom;
static int nIdhtPacket = -1;

int g_bIsImFoCountContinous;
ulong luFilePos,luTmp;
ushort uTmp;

/* reset all IsNew variables */
g_bIsNewICUBoardTime = 0;
g_bIsNewSamplWinStart = 0;
g_bIsNewPulseRepInterval = 0;
g_bIsNewCalSubsysAtten = 0;
g_bIsNewReceiverSubsysAtten = 0;
g_bIsNewReplica = 0;
g_bIsNewCal = 0;
g_bIsNewNoise = 0;
g_bIsNewEcho = 0;


/* return if no more format */
luFilePos = foaGetNextFormat(&g_myForm);
if (!luFilePos)
	return 0;
/* IDHT general header */
if (nIdhtPacket != g_myForm.s.idhtGhSubcom.nPacket) {
        g_nIdhtRepCount = -1;
        nIdhtStart = g_myForm.s.idhtGhSubcom.nSubcom;
        nIdhtPacket = g_myForm.s.idhtGhSubcom.nPacket;
	nIdhtCurrentSubcom = g_myForm.s.idhtGhSubcom.nSubcom;
        }
if (g_myForm.s.idhtGhSubcom.nSubcom>48 || g_myForm.s.idhtGhSubcom.nSubcom<1) {
        Report(lvFormat,"error in IDHT GH subcommutation at file offset %lu",
                        luFilePos);
        g_nIdhtRepCount = 0;
        nIdhtPacket = -1;
        }
/* is a subcom missed? not (old==new)||(old==48 && new==1)||(old+1==new)*/
else if ( (nIdhtCurrentSubcom != g_myForm.s.idhtGhSubcom.nSubcom)
          &&(48!=nIdhtCurrentSubcom
                || 1!=g_myForm.s.idhtGhSubcom.nSubcom)
          && (nIdhtCurrentSubcom+1 != g_myForm.s.idhtGhSubcom.nSubcom) )
        {               /* error in subcommutation */
        Report(lvFormat,"missing subcommutation packet for IDHT GH at file offset %lu",
                        luFilePos);
        nIdhtPacket = -1;
        g_nIdhtRepCount = 0;
        }
else {
        memcpy(g_myIdhtGh.aucSubData[g_myForm.s.idhtGhSubcom.nSubcom-1],
                g_myForm.s.idhtGhSubcom.cSubData, 8);
        if (g_myForm.s.idhtGhSubcom.nSubcom == nIdhtStart) g_nIdhtRepCount++;
        }
if (g_nIdhtRepCount == 1) {
	alAnalyzeIdht(luFilePos);
	g_nIdhtRepCount++;
	}
nIdhtCurrentSubcom = g_myForm.s.idhtGhSubcom.nSubcom;


/* Zero format - everything is zero, just zero or zero */
g_pucZero = NULL;
if ( g_myForm.s.bZero ) {
	g_pucZero = g_myForm.s1.zeroData;
	return 1; /* nothing else to do */
	}

/* is the image format counter is continous? */
luTmp = GET_SAT_ULONG(g_myForm.s.luImageFormatCount);
if ( luTmp != luPrevImFoCount+1  && bIsPrevImFoCountValid ) {
	g_stat.format.luImMissCount+= luTmp - luPrevImFoCount;
	Report(lvFormat,"Missing %lu Image Formats beween %lu and %lu at file pos %lu",
		luTmp - luPrevImFoCount, luPrevImFoCount, luTmp, luFilePos );
	g_bIsImFoCountContinous = 0;
	}
else
	g_bIsImFoCountContinous = 1;

luPrevImFoCount = luTmp;
bIsPrevImFoCountValid = 1;

/* current orbit */
if (g_myForm.s.orbit != g_nCurrentOrbit) {
	g_bIsNewCurrentOrbit = 1;
	g_nCurrentOrbit = g_myForm.s.orbit;
	}

/* testing format for correctness */
if (0xAA != g_myForm.s.formatCode)
	Report(lvFormat,"Read format %lu with invalid format Code 0x%X at file pos %lu",
			GET_SAT_ULONG(g_myForm.s.luImageFormatCount),
			(0xFF & ((int)g_myForm.s.formatCode)),
			luFilePos );
if (0 != g_myForm.s.ogrc) 
	Report(lvFormat,"Format %lu is not OGRC data at file pos %lu",
			GET_SAT_ULONG(g_myForm.s.luImageFormatCount),
			luFilePos );

writeOutESAHeaders();

/* ICU board time */
luTmp = GET_SAT_ULONG(g_myForm.s.luBoardTime);
if ( luTmp != g_luICUBoardTime ) {
	g_luICUBoardTime = luTmp;
	g_bIsNewICUBoardTime = 1;
	}


/* Image Format Counter */
g_luImageFormatCounter = GET_SAT_ULONG(g_myForm.s.luImageFormatCount);

/* Sampling Window Start Time */
uTmp = GET_SAT_USHORT(g_myForm.s.uSamplWinStart);
if ( uTmp != g_uSamplWinStart ) {
	g_uSamplWinStart = uTmp;
	g_bIsNewSamplWinStart = 1;
	}

/* Pulse repetition intervall */
uTmp = GET_SAT_USHORT(g_myForm.s.uPulseRepInterval);
if ( uTmp != g_uPulseRepInterval ) {
	g_uPulseRepInterval = uTmp;
	g_bIsNewPulseRepInterval = 1;
	}

/* calibration subsystem attenuation */
if ( (int)g_myForm.s.uCalSubsysAtten != g_nCalSubsysAtten ) {
	g_nCalSubsysAtten = (int)g_myForm.s.uCalSubsysAtten ;
	g_bIsNewCalSubsysAtten = 1;
	}

/* receiver attenuation subsystem attenuation */
if ( (int)g_myForm.s.uReceiverAtten != g_nReceiverSubsysAtten ) {
	g_nReceiverSubsysAtten = (int)g_myForm.s.uReceiverAtten;
	g_bIsNewReceiverSubsysAtten = 1;
	}

/* cal? Loop closed flag */
g_bLoopClosed = g_myForm.s.bLoopStatus;

/* calibration impulse or pulse replica data and echo/noise data decode*/
switch(g_myForm.s.activity) {
   /* Echo, cal */
   case 0xA9: 
	/* calibration pulse and echo */
	if (g_myForm.s.bFirstCalRep) {
		bIsCal = 1;
		bIsCalRepOk = 1;
		nCalRepSubcomCount = 0;
		}
	if (bIsCalRepOk)
		bIsCalRepOk = bIsCal; /* only OK if prev. was cal too */
	/* if still ok */
	if (bIsCalRepOk) {
		bIsCalRepOk = g_bIsImFoCountContinous;
		/*copy*/ 
		memcpy(((char *)g_pcCalRep)+nCalRepSubcomCount*72,
					g_myForm.s.calRep,72);
		nCalRepSubcomCount++;
		}
	g_bIsNewCal = bIsCal && bIsCalRepOk && (21 == nCalRepSubcomCount);
	/* now echo */
	g_bIsNewEcho = 1;
	break;
   case 0xAA:
	/* pulse replica and echo */
	if (g_myForm.s.bFirstCalRep) {
		bIsCal = 0;
		bIsCalRepOk = 1;
		nCalRepSubcomCount = 0;
		}
	if (bIsCalRepOk)
		bIsCalRepOk = !bIsCal; /* only OK if prev. was rep too */
	/* if still ok */
	if (bIsCalRepOk) {
		bIsCalRepOk = g_bIsImFoCountContinous;
		/*copy*/
		memcpy(((char *)g_pcCalRep)+nCalRepSubcomCount*72,
					g_myForm.s.calRep,72);
		nCalRepSubcomCount++;
		}
	g_bIsNewReplica = !bIsCal && bIsCalRepOk && (21 == nCalRepSubcomCount);
	/* now echo */
	g_bIsNewEcho = 1;
	break;
   case 0x88:
	/* new noise */
	g_bIsNewNoise = 1;
	break;
   default:
        Report(lvFormat,"Wrong activity flag in Image Format %lu,file offset %lu",
                  GET_SAT_ULONG(g_myForm.s.luImageFormatCount), luFilePos);
        bIsCalRepOk = 0;
   } /* end switch */

return 1;
}

/**********************************************************************
* alAnalyzeIdht
*       analyzes the IDHT GH and processes its subcommutation
*  this subroutine could not be tested because no IDHT data was available
*/
void alAnalyzeIdht(luFP)
ulong luFP;
{
struct EPHEMERIS *pEph;
static struct EPHEMERIS seOrbits[16];
 
/* first test IDHT GH header if OK */
if (0x04 != g_myIdhtGh.s.ucPacketID1 || 0 != g_myIdhtGh.s.ucPacketID2
		|| 377 != GET_SAT_USHORT(g_myIdhtGh.s.iLengthOfPacket) )
	{
	Report(lvFormat,"Error within IDHT GH header bytes at file offset %lu",luFP);
	}
/* is continous ? no error if first IDHT scan: g_nIdhtCounter < 0*/
else if ( (((g_nIdhtCounter + 1) & 0x3FFF) 
	!= ( 0x3FFF & GET_SAT_USHORT(g_myIdhtGh.s.iPacketSequenceControl) ) )
    && g_nIdhtCounter >= 0 )
    {
    Report(lvFormat,"Missing IDHTGH packages between %u and %u at file offset %lu",
		g_nIdhtCounter,
		0x3FFF & GET_SAT_USHORT(g_myIdhtGh.s.iPacketSequenceControl),
		luFP );
    g_nIdhtCounter=0x3FFF & GET_SAT_USHORT(g_myIdhtGh.s.iPacketSequenceControl);
    }
/* extract */
else {
    /* Ephemeris data */
    pEph = &g_myIdhtGh.s.sEphemeris;
    if (0 < pEph->counter && 16 >= pEph->counter) {
	printf("found gh data\n");
	if (memcmp(&seOrbits[pEph->counter],pEph, sizeof(struct EPHEMERIS))
		&& g_bOrbitValid[pEph->counter] )
	    {
	    Report(lvSummary,"Error - Orbit info changed-data error?-at file offset %lu",
			luFP);
	    }
	else if (! g_bOrbitValid[pEph->counter])
	    memcpy(&seOrbits[pEph->counter],pEph, sizeof(struct EPHEMERIS) );
	g_bOrbitValid[pEph->counter]++;
	}
    else if (0 == pEph->counter) {
	Report(lvFormat,"Received Orbit data block with non valid data");
	}
    else {
	Report(lvFormat,"Error in IDHT data subcommutated Ephemeris data");
	}
    /* no other data to extract yet */
    g_nIdhtCounter=0x3FFF & GET_SAT_USHORT(g_myIdhtGh.s.iPacketSequenceControl);
    }
}

/**********************************************************************
* alDecodeNoiseEcho
* reads the noise echo data from g_myForm and converts it into the field
* g_pcNoiseEcho as twos complement 8 bit integers
*/
void alDecodeNoiseEcho()
{
register int count;
register int bitOffset = 0;

for(count = 0; count < SIZE_OF_ECHO; count++) {
	/* first the I byte */
	g_pcNoiseEcho[count].I = alGetNoiseEchoByteAt(bitOffset);
	bitOffset += 5;

	/* now the Q byte */
	g_pcNoiseEcho[count].Q = alGetNoiseEchoByteAt(bitOffset);
	bitOffset += 5;
	}
return;
}

/**********************************************************************
* alGetNoiseEchoByteAt()
* gets the signed character of noise echo data of the format at
* bitoffset bitOffset
*/

char alGetNoiseEchoByteAt(bitOffset)
uint bitOffset;
{
register uchar ucTmp,uTmp1;
register int bo = bitOffset & 0x7;  /* bitOffset % 8; */
register int byo = bitOffset >> 3 ; /* bitOffset / 8 */

ucTmp = g_myForm.s.echoNoise[byo];
if ( bo > 3) {  /* part of byte is in next bit */
	uTmp1 = g_myForm.s.echoNoise[byo+1] >> (8-(bo - 3));
	ucTmp = (ucTmp << (bo - 3));
	ucTmp |= uTmp1;
	}
else if ( bo < 3 ) 
	ucTmp = ucTmp >> ( 3 - bo );

/* translate it to twos complement  the smallest positive  */
/* value is mapped to 0, the smallest negative value to -1 */
return (char)((ucTmp & 0x0F) | ((ucTmp & 0x10) ? 0 : 0xF0));
}

/**********************************************************************
* alDecodeRepCal
* reads the rep cal data from g_myForm and converts it into the field
* g_pcRepCal as twos complement 8 bit integers
*/
void alDecodeRepCal()
{
register int count;
register uchar ucQ,ucI;

for (count = 0; count < SIZE_OF_REPCAL; count++ ) {
	/* this exchange of I and Q is intended! Because of the funny
	* data format and the memcopy while dissolving the subcommutation
	*/
	ucQ = g_pcCalRep[count].I;
	ucI = g_pcCalRep[count].Q;
	ucQ = (ucQ << 2) | (ucI >> 6);
	g_pcCalRep[count].Q = (char)((ucQ & 0x1F) | ((ucQ & 0x20) ? 0 : 0xE0));
	g_pcCalRep[count].I = (char)((ucI & 0x1F) | ((ucI & 0x20) ? 0 : 0xE0));
	}
return;
}

/**********************************************************************
* ACCESS functions to get access to the included data
*/

/**********************************************************************
* alGetOrbitInfo
* returns a pointer to a EPHEMERIS structure of a new orbit.
* if no new Orbit info is available it returns 0;
*/
struct EPHEMERIS *alGetOrbitInfo()
{
int count;
struct EPHEMERIS *pRet = NULL;
for(count = 0; count < 16; count++) {
	if (g_bOrbitValid[count] == 1) {
		pRet = &g_seOrbits[count];
		g_bOrbitValid[count]++;
		break;
		}
	}
return pRet;
}

/**********************************************************************
* alGetCurrentOrbit
* returns an integer beween 0 and 15 refering to orbit 1 to 16
* returns < 0 if no new orbit
*/
int alGetCurrentOrbit()
{
if (g_bIsNewCurrentOrbit) {
	g_bIsNewCurrentOrbit = 0;
	return g_nCurrentOrbit;
	}
else
	return -1;
}

/**********************************************************************
* alGetBoardTime
* returns a double with the board time. If not available the returned
* value is negative. The returned value is in seconds
*/
double alGetBoardTime()
{
if (g_bIsNewICUBoardTime) {
	g_bIsNewICUBoardTime = 0;
	return (double)g_luICUBoardTime /* / 256.0 */ ;
	}
else
	return -1.0;
}

/**********************************************************************
* alGetImageFormatCounter
* returns the current image format counter
*/
ulong alGetImageFormatCounter()
{
return g_luImageFormatCounter;
}

/**********************************************************************
* alGetSampleWinStartTime
* returns the sample window start time in seconds
* If not available the function returns -1
*/
double alGetSampleWinStartTime()
{
if (g_bIsNewSamplWinStart) {
	g_bIsNewSamplWinStart = 0;
	return 210.94e-9 * (double)g_uSamplWinStart;
	}
else
	return -1.0;
}

/**********************************************************************
* alGetPulseRepInterval
* returns the pulse repetition interval in seconds
* if not available the function returns -1
*/
double alGetPulseRepInterval()
{
if (g_bIsNewPulseRepInterval) {
	g_bIsNewPulseRepInterval = 0;
	return 210.94e-9 * ((double)g_uPulseRepInterval + 2);
	}
else
	return -1;
}

/**********************************************************************
* alGetCalSubsysAtten
* returns the attenuation of the calibration subsystem in dB
* returns a negative number if not available
*/
int alGetCalSubsysAtten()
{
if (g_bIsNewCalSubsysAtten) {
	g_bIsNewCalSubsysAtten = 0;
	return g_nCalSubsysAtten;
	}
else
	return -1;
}

/**********************************************************************
* alGetReceiverSubsysAtten
* returns the attenuation of the receiver subsystem in dB
* returns a negative number if not available
*/
int alGetReceiverSubsysAtten()
{
if (g_bIsNewReceiverSubsysAtten) {
	g_bIsNewReceiverSubsysAtten = 0;
	return g_nReceiverSubsysAtten;
	}
else
	return -1;
}

/**********************************************************************
* alGetZeroData
* returns a pointer to the buffer with zero data, returns NULL if
* no zero data available
*/
uchar *alGetZeroData()
{
return g_pucZero;
}

/**********************************************************************
* alIsCalData returns true if cal data is available
*/
int alIsCalData()
{
return g_bIsNewCal;
}

/**********************************************************************
* alGetCalData
* converts cal data to the buffer and returns a pointer to the buffer
* returns NULL if no data available
*/
char *alGetCalData()
{
if ( g_bIsNewCal ) {
	g_bIsNewCal = 0;
	alDecodeRepCal();
	return (char *)g_pcCalRep;
	}
else
	return NULL;
}
 
/**********************************************************************
* alIsRepData returns true if cal data is available
*/
int alIsRepData()
{
return g_bIsNewReplica;
}

/**********************************************************************
* alGetRepData
* converts rep data to the buffer and returns a pointer to the buffer
* returns NULL if no data available
*/
char *alGetRepData()
{
if ( g_bIsNewReplica ) {
	g_bIsNewReplica = 0;
	alDecodeRepCal();
	return (char *)g_pcCalRep;
	}
else
	return NULL;
}

/**********************************************************************
* alIsNoiseData returns true if cal data is available
*/
int alIsNoiseData()
{
return g_bIsNewNoise;
}

/**********************************************************************
* alGetNoiseData
* converts noise data to the buffer and returns a poitner to the buffer
* returns NULL if no data available
*/
char *alGetNoiseData()
{
if ( g_bIsNewNoise ) {
	g_bIsNewNoise = 0;
	alDecodeNoiseEcho();
	return (char *)g_pcNoiseEcho;
	}
else
	return NULL;
}

/**********************************************************************
* alIsEchoData returns true if cal data is available
*/
int alIsEchoData()
{
return g_bIsNewEcho;
}

/**********************************************************************
* alGetEchoData
* returns a pointer to the bufferwith echo data
* returns NULL if no data available
*/
char *alGetEchoData()
{
if ( g_bIsNewEcho) {
	g_bIsNewEcho = 0;
	alDecodeNoiseEcho();
	return (char *)g_pcNoiseEcho;
	}
else
	return NULL;
}

/**********************************************************************
* alGetIdhtHeader
*	returns a pointer to a full IDHTGH structure 
*	if the Header is not complete, or already read it returns NULL
*/
union IDHTGH *alGetIdhtHeader()
{
printf("alGetIdhtHeader\n");
if( 2 == g_nIdhtRepCount) {
	printf(" returned IDHGH\n");
	g_nIdhtRepCount++;	/* mark as read */
	return &g_myIdhtGh;
	}
else
	return NULL;
}

