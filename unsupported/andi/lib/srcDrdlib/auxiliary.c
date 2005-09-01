/* aux.c
* this file includes the DRD library access functions for the
* auxiliary data file
*
* release 1.0 07-11-1994 Hans-Joerg Wagner
*/
 
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <ctype.h>
#include "drd.h"
 
#define FILETYPE 	AUX_FILETYPE
#define FILEEXT 	AUX_FILEEXT
 
/** default comment */
char *apszAuxDefaultComment[] = {
        "This file includes auxiliary data from a SAR satelite.",
        NULL
        };

/* Global variable for read Value in aux File. Only for internal
* use! is overwritten every auxRead call!
*/
char auxPszValue[AUX_VAL_STRING_LENGTH];

/* definition to make it easier to implement the debuging code
*/
#define BEGIN_TEST_READ(x,z) \
	DPRINT1("BEGIN %s\n",x); \
	RETURN_IF_DRDFILE_NOT_VALID(x,z); \
	RETURN_IF_WRONG_FILE_TYPE(x,FILETYPE,z); \
	RETURN_IF_WRITE(x,z); \

#define BEGIN_TEST_WRITE(x,z) \
	DPRINT1("BEGIN %s\n",x); \
        RETURN_IF_DRDFILE_NOT_VALID(x,z); \
        RETURN_IF_WRONG_FILE_TYPE(x,FILETYPE,z); \
        RETURN_IF_READ(x,z); \
 
/**********************************************************************
* auxOpen
* openes an existing auxiliary data file, creates a DRDFILE structure
* for it an initializes the structure reading the header.
* Returns a pointer to the DRDFILE structure. If an Error occures,
* it returns NULL
*/
DRDFILE *auxOpen(pszFileRoot)
char *pszFileRoot;
{
DRDFILE *p;
DPRINT("BEGIN auxOpen\n");
RETURN_IF_NOT_INIT("auxOpen");
p = drdOpenInternal(pszFileRoot,FILEEXT,FILETYPE);
DPRINT("END auxOpen\n");
return p;
}

/**********************************************************************
* auxClose
* this function closes the auxiliary data file and destroys the DRDFILE 
* structure
* return 0 if an error occures
*/
int auxClose(pFH)
DRDFILE *pFH;
{
int ret;
DPRINT("BEGIN auxClose\n");
RETURN_IF_DRDFILE_NOT_VALID("auxClose",pFH);
RETURN_IF_WRONG_FILE_TYPE("auxClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END auxClose\n");
return ret;
}
 
/**********************************************************************
* auxCreate
* this function creates a new empty auxiliary data file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated 
*/
DRDFILE *auxCreate(pszFileRoot,pszID,pszSatellite,apszComments)
char *pszFileRoot;
char *pszID;
char *pszSatellite;
char **apszComments;
{
DRDFILE *pFH;

DPRINT("BEGIN auxCreate\n");
RETURN_IF_NOT_INIT("auxCreate");
/* parameter testing */
if (NULL == pszID) {
	DPRINT("END auxCreate - parameter pszID is NULL\n");
        return NULL;
	}
if (NULL == pszFileRoot)  {
	DPRINT("END auxCreate - parameter pszFileRoot is NULL\n");
        return NULL;
	}
if (NULL == pszSatellite) {
	DPRINT("END auxCreate - parameter pszSatellite is NULL\n");
        return NULL; 
	}

/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
	DPRINT("END auxCreate - error allocating DRDFILE structure\n");
        return NULL;
	}
/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
        drdFreeDRDFILE(pFH);
	DPRINT("END auxCreate - error allocating memory for file name\n");
        return NULL;
        }
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("Filename is %s\n",pFH->pszFileName); 

/* file type */
strcpy(pFH->szType,FILETYPE);
 
/* Format version */
pFH->iTypeMajorVer = AUX_MAJOR_VERSION;
pFH->iTypeMinorVer = AUX_MINOR_VERSION;
DPRINT3("Type is %s V%d.%d\n",pFH->szType,pFH->iTypeMajorVer,
		pFH->iTypeMinorVer);
 
/* now create */
if (!drdCreateInternal(pFH,pszID, pszSatellite, apszAuxDefaultComment, apszComments)) {
        drdFreeDRDFILE(pFH);
	DPRINT("END auxCreate - file creation failed\n");
        return NULL;
        }
DPRINT("END auxCreate\n");
return pFH;
}
 
/**********************************************************************
* auxWrite
* For library internal use only. Appends an entry to the auxiliary
* data file. It takes care that the scan no is ascendent.
* Returns 0 if an error occures
*/
int auxWrite(pFH,luImageScanNo,pszEntry,pszValue)
DRDFILE *pFH;
ulong luImageScanNo;
char *pszEntry,*pszValue;
{
BEGIN_TEST_WRITE("auxWrite",pFH);
fprintf(pFH->file,"%lu %s = %s\n",luImageScanNo,pszEntry,pszValue);
DPRINT3("Writing:%lu %s = %s\n",luImageScanNo,pszEntry,pszValue);
DPRINT("END auxWrite\n");
return 1;
}

/**********************************************************************
* auxRead
* For library internal use only. Searches an specific entry in the
* auxiliary data file and reads it. It searches for the entry with
* the same or the most recent image scan number. If there is no entry
* with an image scan number less or equal the given, the value with 
* the closest image scan number is read. The value is
* copied as an string to auxPszValue.
* It returns the found image scan number if successfull.
*/
ulong auxRead(pFH,luImageScanNo,pszEntry)
DRDFILE *pFH;
ulong luImageScanNo;
char *pszEntry;
{
char *pszBuf,*pszTok;
ulong luReadImScNo,luValueImScNo;
int count;

BEGIN_TEST_READ("auxRead",pFH);
auxPszValue[0] = '\0';
luValueImScNo = 0;

/* go to start of file */
fseek(pFH->file, pFH->lnHeaderLength, 0);

/* allocate read buffer */
pszBuf = malloc(AUX_VAL_STRING_LENGTH + strlen(pszEntry) + 30);
if (NULL == pszBuf) {
	DPRINT("END auxRead - error allocating read buffer\n");
	return 0;
	}

/* scanning through the file */
while(NULL!=fgets(pszBuf,AUX_VAL_STRING_LENGTH+strlen(pszEntry)+29,pFH->file)){
	/* first read image scan No */
	pszTok = strtok(pszBuf," ");
	if (NULL == pszTok) {
		DPRINT("Invalid entry\n");
		continue;
		}
	sscanf(pszTok," %lu",&luReadImScNo);
	/* if already passed luImageScanNo and found entry - stop loop*/
	if ((luReadImScNo > luImageScanNo) && (auxPszValue[0] != '\0'))
		break;
	/* divide into entry and Value */
	pszTok = strtok(NULL,"=");
	if (NULL == pszTok) {
                DPRINT("Invalid entry\n");
                continue;
                }
	/* removing peceiding and trailing space characters */
	while( isspace(*pszTok) ) pszTok++;
	for(count=strlen(pszTok)-1;isspace(pszTok[count]) && count>=0;count--)
		pszTok[count] = '\0';
	/* is this the fitting entry? */
	if (!strcmp(pszTok,pszEntry)) {
		/* get the value */
		pszTok = strtok(NULL,"");
		if (NULL == pszTok) {
                	DPRINT("Invalid entry\n");
                	continue;
                	}
		/* remove preceiding and trailing space characters */
		while (isspace(*pszTok) ) pszTok++;
		for(count=strlen(pszTok)-1; 
			isspace(pszTok[count]) && count >= 0; count--)
		   {
			pszTok[count] = '\0';
		   }
		/* copy into value buffer to return */
		strcpy(auxPszValue,pszTok);
		/* storing the image scan no */
		luValueImScNo = luReadImScNo;
		DPRINT3("Found: %ld %s = %s\n",
				luValueImScNo,pszEntry,auxPszValue);
		}
	} /* end scanning loop */

/* returning */
free(pszBuf);
DPRINT("END auxRead\n");
return  luValueImScNo;
}

/**********************************************************************
* auxWriteOrbit
* Writes the position, time and speed of the ascending node of orbit n
* Returns 0 if an error occures
*/
int auxWriteOrbit(pFH,n, x, y, dx, dy, dz, time)
DRDFILE *pFH;
int n;
long x,y,dx,dy,dz;
ulong time;
{
char szEntry[11];
char szValue[80];
int ret;

BEGIN_TEST_WRITE("auxWriteOrbit",pFH);

sprintf(szEntry,"ORBIT%d",n);
sprintf(szValue,"%ld,%ld,%ld,%ld,%ld,%lu",x,y,dx,dy,dz,time);
ret = auxWrite(pFH,1L,szEntry,szValue);
DPRINT("END auxWriteOrbit\n");
return ret;
}

/**********************************************************************
* auxGetOrbit
* Reads the specified Orbit data
* Returns 0 if an error occures
*/
int auxGetOrbit(pFH,n,px,py,pdx,pdy,pdz,pTime)
DRDFILE *pFH;
int n;
long *px,*py,*pdx,*pdy,*pdz;
ulong *pTime;
{
char szEntry[11];
int ret;

BEGIN_TEST_READ("auxGetOrbit",pFH);
sprintf(szEntry,"ORBIT%d",n);
if ( !auxRead(pFH,0xFFFFFFFF,szEntry) ) {
	DPRINT("END auxGetOrbit - error reading entry\n");
	return 0;
	}
ret = (5 != sscanf(auxPszValue,"%ld ,%ld ,%ld ,%ld ,%ld ,%lu",
			px,py,pdx,pdy,pdz,pTime));
DPRINT("END auxGetOrbit\n");
return ret;
}

/* auxGetCurrentOrbitNo
* looks for the current orbit number of the satellite
* entry CURRENT ORBIT
* returns the number of the current orbit or -1 if an error occures or
* if no current orbit number is found
*/
int auxGetCurrentOrbitNo(pFH,luImScNo)
DRDFILE *pFH;
ulong luImScNo;
{
int ret,bOk;

BEGIN_TEST_READ("auxGetCurrentOrbitNo",pFH);
if ( !auxRead(pFH,luImScNo,"CURRENT ORBIT") ) {
	DPRINT("END auxGetCurrentOrbitNo - error reading entry\n");
	return -1;
	}
if (1 != sscanf(auxPszValue,"%d",&ret) ) {
	DPRINT("END auxGetCurrentOrbitNo - error decoding Orbit No\n");
	ret = -1;
	}
else
	DPRINT("END auxGetCurrentOrbitNo\n");
return ret;
}

/* auxWriteCurrentOrbitNo
* writes the currentorbit number of the satellite
* entry CURRENT ORBIT
* return 0 if an error occures
*/
int auxWriteCurrentOrbitNo(pFH, ulImScNo, n)
DRDFILE *pFH;
ulong ulImScNo;
int n;
{
char szValue[10];
int ret;

BEGIN_TEST_WRITE("auxWriteCurrentOrbitNo",pFH);

sprintf(szValue,"%d",n);
ret = auxWrite(pFH,ulImScNo,"CURRENT ORBIT",szValue);
DPRINT("END auxWriteCurrentOrbitNo\n");
return ret;
}
 
/* auxGetCurrentOrbitData
* looks for the current orbit and than reads the data for this
* Orbit
* returns the number of the current orbit or -1 if an error occures
*/
int auxGetCurrentOrbitData(pFH, luImScNo, pliX,pliY,pliDx,pliDy,pliDz,pluTime)
DRDFILE *pFH;
ulong luImScNo, *pluTime;
long *pliX,*pliY, *pliDx, *pliDy, *pliDz;
{
int ret;

BEGIN_TEST_READ("auxGetCurrentOrbitData",pFH);
ret = auxGetCurrentOrbitNo(pFH,luImScNo);
if (-1 == ret) {
	DPRINT("END auxGetCurrentOrbitData - cannot find Orbit\n");
	return -1;
	}
if (0 == auxGetOrbit(pFH,ret,pliX,pliY,pliDx,pliDy,pliDz,pluTime) ) {
	DPRINT("END auxGetCurrentOrbitData - error reading Orbit data\n");
	ret = -1;
	}
else {
	DPRINT("END auxGetCurrentOrbitData\n");
	}
return ret;
}

/* auxGetSampleWindowStartTime
* reads the time beween pulse and start of the sample windows...
* entry SAMPLE WINDOW START TIME
* returns a negative value if an error occured, otherwise the time in 
* micro seconds
*/
double auxGetSampleWindowStartTime(pFH,luImScNo)
DRDFILE *pFH;
ulong luImScNo;
{
double ret;

BEGIN_TEST_READ("auxGetSampleWindowStartTime",pFH);

if (! auxRead(pFH,luImScNo,"SAMPLE WINDOW START TIME") ) {
	DPRINT("END auxGetSampleWindowStartTime - error reading entry\n");
	return -1.0;
	}
if ( 1 != sscanf(auxPszValue,"%lf",&ret) ) {
	DPRINT("END auxGetSampleWindowStartTime - error decoding Number\n");
	return -1.0;
	}
else {
	DPRINT("END auxGetSampleWindowStartTime\n");
	return ret;
	}
}

/* auxWriteSampleWindowStartTime
* writes the entry SAMPLE WINDOW START TIME
* returns 0 if an error occures
*/
int auxWriteSampleWindowStartTime(pFH,luImScNo,lfStartTime)
DRDFILE *pFH;
ulong luImScNo;
double lfStartTime;
{
char szValue[31];
int ret;

BEGIN_TEST_WRITE("auxWriteSampleWindowStartTime",pFH);

sprintf(szValue,"%3.3lf in e-6 sec",lfStartTime);
ret = auxWrite(pFH,luImScNo,"SAMPLE WINDOW START TIME",szValue);
DPRINT("END auxWriteSampleWindowStartTime\n");
return ret;
}

/* auxGetPulseRepetitionInterval
* reads the entry PULSE REPETITION INTERVAL (in micro seconds)
* returns the read value or a negative number if an error occures
*/
double auxGetPulseRepetitionInterval(pFH,luImScNo)
DRDFILE *pFH;
ulong luImScNo;
{
double ret;

BEGIN_TEST_READ("auxGetPulseRepetitionInterval",pFH);

if ( !auxRead(pFH,luImScNo,"PULSE REPETITION INTERVAL") ) {
	DPRINT("END auxGetPulseRepetitionInterval - error reading entry\n");
	return -1.0;
	}

if (1 != sscanf(auxPszValue,"%lf",&ret) ) {
	DPRINT("END auxGetPulseRepetitionInterval - error decoding number\n");
	ret = -1.0;
	}
else
	DPRINT("END auxGetPulseRepetitionInterval\n");
return ret;
}

/* auxWritePulseRepetitionInterval
* writes the entry PULSE REPETITION INTERVAL
* returns 0 if an error occures
*/
int auxWritePulseRepetitionInterval(pFH, luImScNo, lfInterval)
DRDFILE *pFH;
ulong luImScNo;
double lfInterval;
{
char szBuffer[50];
int ret;

BEGIN_TEST_WRITE("auxWritePulseRepetitionInterval",pFH);

sprintf(szBuffer,"%3.3lf in e-6 sec",lfInterval);
ret = auxWrite(pFH,luImScNo,"PULSE REPETITION INTERVAL",szBuffer);
DPRINT("END auxWritePulseRepetitionInterval\n");
return ret;
}

/* auxGetCalSubsysAttenuation
* reads the entry CALIBRATION SUBSYSTEM ATTENUATION
* returns the value in dB, a negative Number if an error occured
*/
int auxGetCalSubsysAttenuation(pFH,luImScNo)
DRDFILE *pFH;
ulong luImScNo;
{
int ret;

BEGIN_TEST_READ("auxGetCalSubsysAttenuation",pFH);

if ( !auxRead(pFH,luImScNo,"CALIBRATION SUBSYSTEM ATTENUATION") ) {
	DPRINT("END auxGetCalSubsysAttenuation - error reading entry\n");
	return -1;
	}
if (1 != sscanf(auxPszValue,"%d",&ret) ) {
	DPRINT("END auxGetCalSubsysAttenuation - error decoding number \n");
	ret = -1;
	}
else
	DPRINT ("END auxGetCalSubsysAttenuation\n");
return ret;
}

/* auxWriteCalSubsysAttenuation
* writes the entry CALIBRATION SUBSYSTEM ATTENUATION
* returns 0 if an error occures
*/

int auxWriteCalSubsysAttenuation(pFH,luImScNo,iAtten)
DRDFILE *pFH;
ulong luImScNo;
int iAtten;
{
char szBuffer[15];
int ret;

BEGIN_TEST_WRITE("auxWriteCalSubsysAttenuation",pFH);
sprintf(szBuffer,"%d dB",iAtten);
ret = auxWrite(pFH,luImScNo, "CALIBRATION SUBSYSTEM ATTENUATION",szBuffer);
DPRINT("END auxWriteCalSubsysAttenuation\n");
return ret;
}

/* auxGetRepSubsysAttenuation
* reads the entry RECEIVER CHAIN ATTENUATION
* returns the value in dB, a negative Number if an error occured
*/
int auxGetReceiverChainAttenuation(pFH,luImScNo)
DRDFILE *pFH;
ulong luImScNo;
{
int ret;
 
BEGIN_TEST_READ("auxGetReceiverChainAttenuation",pFH);
 
if ( !auxRead(pFH,luImScNo,"RECEIVER CHAIN ATTENUATION") ) {
        DPRINT("END auxGetReceiverChainAttenuation - error reading entry\n");
        return -1;
        }
if (1 != sscanf(auxPszValue,"%d",&ret) ) {
        DPRINT("END auxGetReceiverChainAttenuation - error decoding number \n");
        ret = -1;
        }
else
        DPRINT ("END auxGetReceiverChainAttenuation\n");
return ret;
}

/* auxWriteReceiverChainAttenuation
* writes the entry RECEIVER CHAIN ATTENUATION
* returns 0 if an error occures
*/
 
int auxWriteReceiverChainAttenuation(pFH,luImScNo,iAtten)
DRDFILE *pFH;
ulong luImScNo;
int iAtten;
{
char szBuffer[15];
int ret;
 
BEGIN_TEST_WRITE("auxWriteReceiverChainAttenuation",pFH);
sprintf(szBuffer,"%d dB",iAtten);
ret = auxWrite(pFH,luImScNo, "RECEIVER CHAIN ATTENUATION",szBuffer);
DPRINT("END auxWriteReceiverChainAttenuation\n");
return ret;
}

/**********************************************************************
* auxGetSamplingRate
* gets the sampling rate for a specific sample.
* the argument type has to be the same as the FILETYPE described
* in the "File Format for Decoded Raw Data" document.
* The function looks for the entry 
* type SAMPLING RATE
* if this entry is not found or if the filetype parameter is NULL
* it looks for the entry
* SAMPLING RATE
*
* the specific entry allways overwrites the SAMPLING RATE entry
*
* It returns the sampling rate in kHz/second
* 0 if not found or an error occures
*/
ulong auxGetSamplingRate(pFH,type,luImScNo)
DRDFILE *pFH;
char *type;
ulong luImScNo;
{
static char szDefEntry[] = "SAMPLING RATE";
char *pszEntry, szValue[26];
ulong luGotImScNo,luSamplingRate,luTmp;

BEGIN_TEST_READ("auxGetSamplingRate",pFH);

/* get default value first */
luGotImScNo = auxRead(pFH,luImScNo,szDefEntry);
if (1 != sscanf(auxPszValue,"%lu",&luSamplingRate) ) {
	luSamplingRate = 0L;
	}

/* look for specific number if type not NULL */
if (NULL != type) {
	/* allocating memory for entry */
	pszEntry = malloc( strlen(type) + strlen(szDefEntry) + 2 );
	/* test if allocatin was successful */
	if (NULL == pszEntry) {
	    DPRINT("END auxGetSamplingRate - memory allocation error\n");
	    return 0;
	    }
	/* generating entry string */
	strcpy(pszEntry,type);
	strcat(pszEntry," ");
	strcat(pszEntry,szDefEntry);

	/* now read again */
	luGotImScNo = auxRead(pFH,luImScNo,pszEntry);
	if (1 == sscanf(auxPszValue,"%lu",&luTmp) ) {
		luSamplingRate = luTmp;
		}
	free(pszEntry);
	}

DPRINT("END auxGetSamplingRate\n");
return luSamplingRate;
}

/**********************************************************************
* auxWriteSamplingRate
* writes the specific sampling rate or the common sampling rate.
* the entry is produced as described in auxGetSamplingRate
* Returns 0 if an error occures
*/
int auxWriteSamplingRate(pFH,type,luImScNo,luSamplingRate)
DRDFILE *pFH;
char *type;
ulong luImScNo,luSamplingRate;
{
static char szDefEntry[] = "SAMPLING RATE";
char *pszEntry, szValue[26];
int ret;

BEGIN_TEST_WRITE("auxWriteSamplingRate",pFH);
 
/* allocate memory for entry string	*/
if (NULL == type) 
	pszEntry = malloc( strlen(szDefEntry) + 1);
else
	pszEntry = malloc( strlen(szDefEntry) + strlen(type) + 2);

if (NULL == pszEntry) {
	DPRINT("END auxWriteSamplingRate - memory allocation error\n");
	return 0;
	}

/* now generate entry string */
if (NULL == type)
	strcpy(pszEntry,szDefEntry);
else {	
	strcpy(pszEntry,type);
	strcat(pszEntry," ");
	strcat(pszEntry,szDefEntry);
	}

/* now generate value string */
sprintf(szValue,"%lu x1000 Samples/s",luSamplingRate);

/* and write the ;apw9orughbap;wrogubs */
ret = auxWrite(pFH,luImScNo,pszEntry,szValue);

DPRINT("END auxWriteSamplingRate\n");
return ret;
}

/**********************************************************************
* auxGetRadarFrequency
* reads the Radar frequency.
* returns 0 if an error occures otherwise the frequency in kHz
*/
ulong auxGetRadarFrequency(pFH,luImScNo)
DRDFILE *pFH;
ulong luImScNo;
{
ulong ret;
 
BEGIN_TEST_READ("auxGetRadarFrequency",pFH);
 
if ( !auxRead(pFH,luImScNo,"RADAR FREQUENCY")) {
        DPRINT("END auxGetRadarFrequency - error reading entry\n");
        return -1;
        }
if (1 != sscanf(auxPszValue,"%lu",&ret) ) {
        DPRINT("END auxGetGetRadarFrequency - error decoding number \n");
        ret = -1L;
        }
else
        DPRINT ("END auxGetRadarFrequency\n");
return ret;
}

/**********************************************************************
* auxWriteRadarFrequency
* writes the Radar frequency.
*	takes the frequency in kHz
* returns 0 if an error occures
*/
int auxWriteRadarFrequency(pFH,luImScNo,luFrequency)
DRDFILE *pFH;
ulong luImScNo;
ulong luFrequency;
{
char szBuffer[30];
int ret;
 
BEGIN_TEST_WRITE("auxWriteRadarFrequency",pFH);
sprintf(szBuffer,"%lu kHz",luFrequency);
ret = auxWrite(pFH,luImScNo, "RADAR FREQUENCY",szBuffer);
DPRINT("END auxWriteRadarFrequency\n");
return ret;
}

/**********************************************************************
* auxWriteMixingFrequencyShift
* writes the frequency shift resulting of mixing the received radar
* signal to reduce frequency and to decode I and Q
* The FreqShift is in kHz
* the entry is produced as described in auxGetMixingFrequencyShift
* Returns 0 if an error occures
*/
int auxWriteMixingFrequencyShift(pFH,type,luImScNo,luFreqShift)
DRDFILE *pFH;
char *type;
ulong luImScNo,luFreqShift;
{
static char szDefEntry[] = "MIXING FREQUENCY SHIFT";
char *pszEntry, szValue[26];
int ret;
 
BEGIN_TEST_WRITE("auxWriteMixingFrequencyShift",pFH);
 
/* allocate memory for entry string     */
if (NULL == type)
        pszEntry = malloc( strlen(szDefEntry) + 1);
else
        pszEntry = malloc( strlen(szDefEntry) + strlen(type) + 2);
 
if (NULL == pszEntry) {
        DPRINT("END auxWriteMixingFrequencyShift - memory allocation error\n");
        return 0;
        }
 
/* now generate entry string */
if (NULL == type)
        strcpy(pszEntry,szDefEntry);
else {
        strcpy(pszEntry,type);
        strcat(pszEntry," ");
        strcat(pszEntry,szDefEntry);
        }
 
/* now generate value string */
sprintf(szValue,"%lu kHz",luFreqShift);
 
/* and write the ;apw9orughbap;wrogubs */
ret = auxWrite(pFH,luImScNo,pszEntry,szValue);
 
DPRINT("END auxWriteMixingFrequencyShift\n");
return ret;
}

/**********************************************************************
* auxGetMixingFrequencyShift
* gets the frequency shift resultin of mixing the received data for
* a specific sample.
* the argument type has to be the same as the FILETYPE described
* in the "File Format for Decoded Raw Data" document.
* The function looks for the entry
* type MIXING FREQUENCY SHIFT
* if this entry is not found or if the filetype parameter is NULL
* it looks for the entry
* MIXING FREQUENCY SHIFT
*
* the specific entry allways overwrites the MIXING FREQUENCY SHIFT entry
*
* It returns the sampling rate in kHz
* 0 if not found or an error occures
*/
ulong auxGetMixingFrequencyShift(pFH,type,luImScNo)
DRDFILE *pFH;
char *type;
ulong luImScNo;
{
static char szDefEntry[] = "MIXING FREQUENCY SHIFT";
char *pszEntry, szValue[26];
ulong luGotImScNo,luFreqShift,luTmp;
BEGIN_TEST_READ("auxGetMixingFrequencyShift",pFH);
 
/* get default value first */
luGotImScNo = auxRead(pFH,luImScNo,szDefEntry);
if (1 != sscanf(auxPszValue,"%lu",&luFreqShift) ) {
        luFreqShift = 0L;
        }
 
/* look for specific number if type not NULL */
if (NULL != type) {
        /* allocating memory for entry */
        pszEntry = malloc( strlen(type) + strlen(szDefEntry) + 2 );
        /* test if allocatin was successful */
        if (NULL == pszEntry) {
            DPRINT("END auxGetMixingFrequencyShift - memory allocation error\n");
            return 0;
            }
        /* generating entry string */
        strcpy(pszEntry,type);
        strcat(pszEntry," ");
        strcat(pszEntry,szDefEntry);
 
        /* now read again */
        luGotImScNo = auxRead(pFH,luImScNo,pszEntry);
        if (1 == sscanf(auxPszValue,"%lu",&luTmp) ) {
                luFreqShift = luTmp;
                }
        free(pszEntry);
        }
 
DPRINT("END auxGetMixingFrequencyShift\n");
return luFreqShift;
}

