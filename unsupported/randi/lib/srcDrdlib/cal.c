/* cal.c
* this file includes the DRD library access functions for the
* calibration pulse data file
*
* release 1.0 07-07-1994 Hans-Joerg Wagner
*/
#include <stdio.h>
#include <malloc.h>
#include "drd.h"
 
/* this file contains echo and echo and echo and echo */
#define FILETYPE CAL_FILETYPE
#define FILEEXT  CAL_FILEEXT
 
/** default comment */
char *apszCalDefaultComment[] = {
        "This file includes the samples of SAR radar calibration pulses",
        NULL
        };

/*********************************************************************
*
* calOpen
* openes an existing cal file, creates a DRDFILE structure for it
* and initializes the structure reading the header.
* Returns a pointer to the DRDFILE structure. If an Error occures,
* it returns NULL
*/
DRDFILE *calOpen(pszFileRoot)
char *pszFileRoot;
{
DRDFILE *p;
DPRINT("BEGIN calOpen\n");
RETURN_IF_NOT_INIT("calOpen");
p = drdOpenInternal(pszFileRoot,FILEEXT,FILETYPE);
DPRINT("END calOpen\n");
return p;
}
 
/*********************************************************************
*
* calClose
* this function closes the cal file and destroys the DRDFILE structur
* return 0 if an error occures
*/
int calClose(pFH)
DRDFILE *pFH;
{
int ret;

DPRINT("BEGIN calClose\n");
RETURN_IF_DRDFILE_NOT_VALID("calClose",pFH);
RETURN_IF_WRONG_FILE_TYPE("calClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END calClose\n");
return ret;
}
 
/**********************************************************************
* calCreate
* this function creates a new empty cal file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated 
*/
DRDFILE *calCreate(pszFileRoot,pszID,pszSatellite,apszComments)
char *pszFileRoot;
char *pszID;
char *pszSatellite;
char **apszComments;
{
DRDFILE *pFH;

DPRINT("BEGIN calCreate\n");
RETURN_IF_NOT_INIT("calCreate");
/* parameter testing */
if (NULL == pszID) {
	DPRINT("END calCreate - paramter pszID is NULL\n");
        return NULL;
	}
if (NULL == pszFileRoot) {
	DPRINT("END calCreate - parameter pszFileRoot is NULL\n");
	return NULL;
	}
if (NULL == pszSatellite) {
	DPRINT("END calCreate - parameter pszSatellite is NULL\n");
	return NULL; 
	}

/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
	DPRINT("END calCreate - error allocating DRDFILE struct.\n");
	return NULL;
	}

/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
	drdFreeDRDFILE(pFH);
	DPRINT("END calCreate - error allocating memory for file name\n");
	return NULL;
	}
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("File name is %s\n",pFH->pszFileName);

/* file type */
strcpy(pFH->szType,FILETYPE);

/* Format version */
pFH->iTypeMajorVer = CAL_MAJOR_VERSION;
pFH->iTypeMinorVer = CAL_MINOR_VERSION;
DPRINT3("File type is %s V%d.%d\n",pFH->szType,
		pFH->iTypeMajorVer, pFH->iTypeMinorVer);

/* now create */
if (!drdCreateInternal(pFH,pszID, pszSatellite, apszCalDefaultComment, apszComments)) {
	drdFreeDRDFILE(pFH);
	DPRINT("END calCreate - error creating file\n");
	return NULL;
	}
DPRINT("END calCreate\n");
return pFH;
}

/**********************************************************************
* echoCalFirstScan
* sets the current file position to the first cal data block, useing
* the lnHeaderLength member of pFH
* returns 0 if not successful, e.g. the file is not opened for reading
*/
int calGotoFirstScan(pFH)
DRDFILE *pFH;
{
char buf;

DPRINT("BEGIN calGotoFirstScan\n");
RETURN_IF_DRDFILE_NOT_VALID("calGotoFirstScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGotoFirstScan",FILETYPE,pFH);
RETURN_IF_WRITE("calGotoFirstScan",pFH);

fseek(pFH->file,pFH->lnHeaderLength,0);
/* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&buf,sizeof(char),1,pFH->file);
fseek(pFH->file,pFH->lnHeaderLength,0);
DPRINT("END calGotoFirstScan\n");
return pFH->bFilePosValid;
}

/**********************************************************************
* calGotoLastScan
* sets the current file position to the beginning of the last cal data 
* block. Therefore it scans to the whole file to the end of the file!
* return 0 if not successful
*/
int calGotoLastScan(pFH)
DRDFILE *pFH;
{
long lnCurPos;
char buf;

DPRINT("BEGIN calGotoLastScan\n");
RETURN_IF_DRDFILE_NOT_VALID("calGotoLastScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGotoLastScan",FILETYPE,pFH);
RETURN_IF_WRITE("calGotoLastScan",pFH);

/* goto the first data block */
if (!calGotoFirstScan(pFH)) {
	DPRINT("END calGotoLastScan - error searching first data block\n");
	return 0;
	}

/* scanning through the file */
do {
	lnCurPos = ftell(pFH->file);
	} while (calGotoNextScan(pFH));
fseek(pFH->file,lnCurPos,0);

pFH->bFilePosValid = 1;
DPRINT("END calGotoLastScan\n");
return -1;
}

/**********************************************************************
* calGotoNextScan
* sets the current file position to the next data block, therefore
* it reads the header of the current data block to determin the
* start of the next data block
* returns 0 if not successful, e.g. if the file end is reched
*/
int calGotoNextScan(pFH)
DRDFILE *pFH;
{
struct CAL_DB_HEAD head;
int nRead;
ulong luFilePos;

DPRINT("BEGIN calGotoNextScan\n");
RETURN_IF_DRDFILE_NOT_VALID("calGotoNextScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGotoNextScan",FILETYPE,pFH);
RETURN_IF_WRITE("calGotoNextScan",pFH);
RETURN_IF_FILE_POS_INVALID("calGotoNextScan",pFH);

nRead = fread(&head,sizeof(struct CAL_DB_HEAD),1,pFH->file);
/* if no full header read */
if ( nRead <= 0 ) {
	DPRINT("END calGotoNextScan - error reading file\n");
	return 0;
	}

/* skip data block */
nRead = fseek(pFH->file,head.uLength,1);

/* try to read another byte to test fileend*/
luFilePos = ftell(pFH->file); /* store current position */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&head,1,1,pFH->file);
fseek(pFH->file,luFilePos,0);  /* restore pos */
 /* read something !! bFilePosValid is just 1 bit long!!!*/

DPRINT("END calGotoNextScan\n");
return pFH->bFilePosValid;
/* if this was not the end, at least one byte was read */
}

/**********************************************************************
* calGotoPrevScan
* sets the current file position to the previous data block.
* this version achieves the task by restarting at the very begining
* and scanning forewards through the file;
* return 0 if not successful, e.g. the current position is already the
* very first
*/
int calGotoPrevScan(pFH)
DRDFILE *pFH;
{
long lPos,lOldPos;

DPRINT("BEGIN calGotoPrevScan\n");
RETURN_IF_DRDFILE_NOT_VALID("calGotoPrevScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGotoPrevScan",FILETYPE,pFH);
RETURN_IF_WRITE("calGotoPrevScan",pFH);
RETURN_IF_FILE_POS_INVALID("calGotoPrevScan",pFH);

/* save current position */
lOldPos = ftell(pFH->file);

/* is current possiton already the first position ? */
if (lOldPos <= pFH->lnHeaderLength) {
	DPRINT("END calGotoPrevScan - already at first position\n");
	return 0;
	}

/* scaning through the file */
if (!calGotoFirstScan(pFH)) {
	DPRINT("END calGotoPrevScan - cannot find first possition\n");
	return 0;
	}

do {
	lPos = ftell(pFH->file);
        calGotoNextScan(pFH);
	} while (lOldPos > ftell(pFH->file));

fseek(pFH->file,lPos,0);
DPRINT("END calGotoPrevScan\n");
return -1;
}

/**********************************************************************
* calGetCurImageScanNo
* this function looks for the image scan no at the current file
* position. It returns the Calibration pulse for the current image
* scan number, because there could be more than one Calibration pulse
* associated with the same image scan number.
* It returns 0 if an error occures
*/
uint calGetCurImageScanNo(pFH,pluImageScanNo)
DRDFILE *pFH;
ulong *pluImageScanNo;
{
struct CAL_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN calGetCurImageScanNo\n");
RETURN_IF_DRDFILE_NOT_VALID("calGetCurImageScanNo",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGetCurImageScanNo",FILETYPE,pFH);
RETURN_IF_WRITE("calGetCurImageScanNo",pFH);
RETURN_IF_FILE_POS_INVALID("calGetCurImageScanNo",pFH);

/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct CAL_DB_HEAD),1,pFH->file);
/* restore file position
fseek(pFH->file,lCurPos,0);
/* return No  and Image scan No */
(*pluImageScanNo) = head.luImageScanNo;
DPRINT("END calGetCurImageScanNo\n");
return head.uPulseNo;
}

/**********************************************************************
* calGetCurrentScanLength
* returns the min. required buffersize to receive the echo data block
* by reading the header information of the current block
*/
ushort calGetCurrentScanLength(pFH)
DRDFILE *pFH;
{
struct CAL_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN calGetCurrentScanLength\n");
RETURN_IF_DRDFILE_NOT_VALID("calGetCurrentScanLength",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGetCurrentScanLength",FILETYPE,pFH);
RETURN_IF_WRITE("calGetCurrentScanLength",pFH);
RETURN_IF_FILE_POS_INVALID("calGetCurrentScanLength",pFH);

/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct CAL_DB_HEAD),1,pFH->file);
/* resore file position */
fseek(pFH->file,lCurPos,0);
/* return length */
DPRINT("END calGetCurrentScanLength\n");
return head.uLength;
}

/**********************************************************************
* calGetCurrentScan
* fills pcBuf with the current cal data. The buffer must have at least
* the size returned by the calGetCurrentScanLength function.
* returns the No of the pulse. If an error occures it returns 0
*/
int calGetCurrentScan(pFH, pluImScNo, pcBuf)
DRDFILE *pFH;
ulong *pluImScNo;
char *pcBuf;
{
struct CAL_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN calGetCurrentScan\n");
RETURN_IF_DRDFILE_NOT_VALID("calGetCurrentScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("calGetCurrentScan",FILETYPE,pFH);
RETURN_IF_WRITE("calGetCurrentScan",pFH);
RETURN_IF_FILE_POS_INVALID("calGetCurrentScan",pFH);

/* store current position */
lCurPos = ftell(pFH->file);

/* read header */
fread(&head,sizeof(struct CAL_DB_HEAD),1,pFH->file);
*pluImScNo = head.luImageScanNo;

/* read buffer */
if (head.uLength != fread(pcBuf,sizeof(char),head.uLength,pFH->file) ) {
	DPRINT("END calGetCurrentScan - error reading file\n");
	return 0;
	}

/* restore position */
fseek(pFH->file,lCurPos,0);

DPRINT("END calGetCurrentScan\n");
return head.uPulseNo;
}

/**********************************************************************
* calWriteScan
* writes the scan data from the buffer into the file. Returns 0 if
* an error occures.
* assures, that the image scans No are ascendent, i.e. returns error
* if not
*/
int calWriteScan(pFH, luImScNo, pcBuf, uLength)
DRDFILE *pFH;
ulong luImScNo;
char *pcBuf;
ushort uLength;
{
struct CAL_DB_HEAD head;

DPRINT("BEGIN calWriteScan\n");
RETURN_IF_DRDFILE_NOT_VALID("calWriteScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("calWriteScan",FILETYPE,pFH);
RETURN_IF_READ("calWriteScan",pFH);

/* is image scan no ascendent ? */
if (luImScNo < pFH->luImageScanNo) {
	DPRINT("END calWriteScan - image scan No not ascendent\n");
	return 0;
	}

/* if same image scan No than increase uPulseNo, else set it to 1 */
if (luImScNo == pFH->luImageScanNo)
	pFH->uNo++;
else
	pFH->uNo = 1;

/* write header */
head.luImageScanNo = luImScNo;
head.uLength = uLength;
head.uPulseNo = pFH->uNo;
fwrite(&head,sizeof(struct CAL_DB_HEAD),1,pFH->file);

/* write data */
fwrite(pcBuf,sizeof(char),uLength,pFH->file);

pFH->luImageScanNo = luImScNo;

DPRINT("END calWriteScan\n");
return -1;
}


