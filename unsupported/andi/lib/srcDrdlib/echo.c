/* echo.c
* this file includes the DRD library access functions for the
* echo data file
*
* release 1.0 06-29-1994 Hans-Joerg Wagner
*/

#include <stdio.h>
#include <malloc.h>
#include "drd.h"

/* this file contains echo and echo and echo and echo */
#define FILETYPE 	ECHO_FILETYPE
#define FILEEXT 	ECHO_FILEEXT
 
/** default comment */
char *apszEchoDefaultComment[] = {
	"This file includes the samples of the echo of a SAR radar.",
	NULL
	};

/**********************************************************************
* echoOpen
* openes an existing echo file, creates a DRDFILE structure for it
* and initializes the structure reading the header.
* Returns a pointer to the DRDFILE structure. If an Error occures,
* it returns NULL
*/ 
DRDFILE *echoOpen(pszFileRoot)
char *pszFileRoot;
{
DRDFILE *p;
DPRINT("BEGIN echoOpen\n");
RETURN_IF_NOT_INIT("echoOpen");
p = drdOpenInternal(pszFileRoot,FILEEXT,FILETYPE);
DPRINT("END echoOpen\n");
return p;
}

/**********************************************************************
* echoClose
* this function closes the echo file and destroys the DRDFILE structure
* return 0 if an error occures
*/
int echoClose(pFH)
DRDFILE *pFH;
{
int ret;

DPRINT("BEGIN echoClose\n");
RETURN_IF_NOT_INIT("echoClose");
RETURN_IF_WRONG_FILE_TYPE("echoClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END echoClose\n");
return ret;
}

/**********************************************************************
* echoCreate
* this function creates a new empty echo file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated 
*/
DRDFILE *echoCreate(pszFileRoot,pszID,pszSatellite,apszComments)
char *pszFileRoot;
char *pszID;
char *pszSatellite;
char **apszComments;
{
DRDFILE *pFH;

DPRINT("BEGIN echoCreate\n");
RETURN_IF_NOT_INIT("echoCreate\n");
/* parameter testing */
if (NULL == pszID) {
	DPRINT("END echoCreate - parameter pszID is NULL\n");
        return NULL;
	}
if (NULL == pszFileRoot) {
	DPRINT("END echoCreate - parameter pszFileRoot is NULL\n");
	return NULL;
	}
if (NULL == pszSatellite) {
	DPRINT("END echoCreate - parameter pszSatellite is NULL\n");
	return NULL; 
	}

/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
	DPRINT("END echoCreate - error Allocating a new DRDFILE struct\n");
	return NULL;
	}

/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
	DPRINT("END echoCreate - failure allocating memory for filename\n");
	drdFreeDRDFILE(pFH);
	return NULL;
	}
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("File to open is %s\n",pFH->pszFileName);

/* file type */
strcpy(pFH->szType,FILETYPE);

/* Format version */
pFH->iTypeMajorVer = ECHO_MAJOR_VERSION;
pFH->iTypeMinorVer = ECHO_MINOR_VERSION;

DPRINT3("File type is %s V%d.%d\n",pFH->szType,pFH->iTypeMajorVer,
					pFH->iTypeMinorVer);
/* now create */
if (!drdCreateInternal(pFH,pszID, pszSatellite, apszEchoDefaultComment,apszComments)) {
	drdFreeDRDFILE(pFH);
	DPRINT("END echoCreate - error opening file\n");
	return NULL;
	}

DPRINT("END echoCreate\n");
return pFH;
}

/**********************************************************************
* echoGotoFirstScan
* sets the current file position to the first scan data block, useing
* the lnHeaderLength member of pFH
* returns 0 if not successful, e.g. the file is not opened for reading
*/
int echoGotoFirstScan(pFH)
DRDFILE *pFH;
{
char buf;

DPRINT("BEGIN echoGotoFirstScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoGotoFirstScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoGotoFirstScan",FILETYPE,pFH);
RETURN_IF_WRITE("echoGotoFirstScan",pFH);

fseek(pFH->file,pFH->lnHeaderLength,0);

/* filepos is invalid if end of file */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&buf,sizeof(char),1,pFH->file);
fseek(pFH->file,pFH->lnHeaderLength,0);

DPRINT("END echoGotoFirstScna\n");
return pFH->bFilePosValid;
}

/**********************************************************************
* echoGotoLastScan
* sets the current file position to the beginning of the last scan data 
* block. Therefore it scans to the whole file to the end of the file!
* return 0 if not successful
*/
int echoGotoLastScan(pFH)
DRDFILE *pFH;
{
long lnCurPos;
char buf;

DPRINT("BEGIN echoGotoLastScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoGotoLastScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoGotoLastScan",FILETYPE,pFH);
RETURN_IF_WRITE("echoGotoLastScan",pFH);

/* goto the first data block */
if (!echoGotoFirstScan(pFH)) {
	DPRINT("END echoGotoLastScan - could not find first scan\n");
	return 0;
	}

/* scanning through the file */
do {
	lnCurPos = ftell(pFH->file);
	} while (echoGotoNextScan(pFH));
fseek(pFH->file,lnCurPos,0);

pFH->bFilePosValid = 1;
DPRINT("END echoGotoLastScan\n");
return -1;
}

/**********************************************************************
* echoGotoNextScan
* sets the current file position to the next data block, therefore
* it reads the header of the current data block to determin the
* start of the next data block
* returns 0 if not successful, e.g. if the file end is reched
*/
int echoGotoNextScan(pFH)
DRDFILE *pFH;
{
struct ECHO_DB_HEAD head;
int nRead;
ulong luFilePos;

DPRINT("BEGIN echoGotoNextScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoGotoNextScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoGotoNextScan",FILETYPE,pFH);
RETURN_IF_WRITE("echoGotoNextScan",pFH);
RETURN_IF_FILE_POS_INVALID("echoGotoNextScan",pFH);

nRead = fread(&head,sizeof(struct ECHO_DB_HEAD),1,pFH->file);
/* if no full header read */
if ( nRead <= 0 ) {
	DPRINT1("END echoGotoNextScan - error reading in file %s\n",
		pFH->pszFileName);
	return 0;
	}

/* skip data block */
nRead = fseek(pFH->file,head.uLength,1);

/* try to read another byte to test fileend*/
luFilePos = ftell(pFH->file); /* store current position */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&head,1,1,pFH->file);
fseek(pFH->file,luFilePos,0);  /* restore pos */

DPRINT("END echoGotoNextScan\n");
return pFH->bFilePosValid;
/* if this was not the end, at least one byte was read */
}

/**********************************************************************
* echoGotoPrevScan
* sets the current file position to the previous data block.
* this version achieves the task by restarting at the very begining
* and scanning forewards through the file;
* return 0 if not successful, e.g. the current position is already the
* very first
*/
int echoGotoPrevScan(pFH)
DRDFILE *pFH;
{
long lPos,lOldPos;

DPRINT("BEGIN echoGotoPrevScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoGotoPrevScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoGotoPrevScan",FILETYPE,pFH);
RETURN_IF_WRITE("echoGotoPrevScan",pFH);
RETURN_IF_FILE_POS_INVALID("echoGotoPrevScan",pFH);

/* save current position */
lOldPos = ftell(pFH->file);

/* is current possiton already the first position ? */
if (lOldPos <= pFH->lnHeaderLength) {
	DPRINT("END echoGotoPrevScan - already at first position\n"); 
	return 0;
	}

/* scaning through the file */
if (!echoGotoFirstScan(pFH)) {
	DPRINT("END echoGotoPrevScan - error searching first scan\n");
	return 0;
	}

do {
	lPos = ftell(pFH->file);
        echoGotoNextScan(pFH);
	} while (lOldPos > ftell(pFH->file));

fseek(pFH->file,lPos,0);
DPRINT("END echoGotoPrevScan\n");
return -1;
}

/**********************************************************************
* echoFindScan 
* sets the current file position to the echo data block, that
* contains the image scan with the number luImageScanNo. If the
* image scan no cannot be found or if an error occurs, the
* current file position is undefined and 0 is returned. Otherwise
* the function returns -1
*/
int echoFindScan(pFH, luImageScanNo)
DRDFILE *pFH;
ulong luImageScanNo;
{
struct ECHO_DB_HEAD edbh;
int bFound = 0;
long liPos;

DPRINT("BEGIN echoFindScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoFindScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoFindScan",FILETYPE,pFH);
RETURN_IF_WRITE("echoFindScan",pFH);
RETURN_IF_FILE_POS_INVALID("echoFindScan",pFH);

/* goto the first data block */
if (!echoGotoFirstScan(pFH)) {
	DPRINT("END echoFindScan - error searching first scan\n");
	return 0;
	}
/* scan through file to find data block */
do {
	liPos = ftell(pFH->file);
	fread(&edbh,sizeof(struct ECHO_DB_HEAD),1,pFH->file);
	fseek(pFH->file,liPos,0);
	if ( edbh.luImageScanNo == luImageScanNo )
		bFound = -1;
	} while ( echoGotoNextScan(pFH) && !bFound );

/* if found set the file position */
if (bFound) {
	fseek(pFH->file,liPos,0);
	pFH->bFilePosValid = 1;
	}
else
	pFH->bFilePosValid = 0;
DPRINT1("END echoFindScan - Scan %s\n",bFound ? "found" : "not found");
return bFound;
}

/**********************************************************************
* echoGetCurrentScanLength
* returns the min. required buffersize to receive the echo data block
* by reading the header information of the current block
*/
ushort echoGetCurrentScanLength(pFH)
DRDFILE *pFH;
{
struct ECHO_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN echoGetCurrentScanLength\n");
RETURN_IF_DRDFILE_NOT_VALID("echoGetCurrentScanLength",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoGetCurrentScanLength",FILETYPE,pFH);
RETURN_IF_WRITE("echoGetCurrentScanLength",pFH);
RETURN_IF_FILE_POS_INVALID("echoGetCurrentScanLength",pFH);

/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct ECHO_DB_HEAD),1,pFH->file);
/* resore file position */
fseek(pFH->file,lCurPos,0);
/* return length */
DPRINT("END echoGetCurrentScanLength\n");
return head.uLength;
}

/**********************************************************************
* echoGetCurrentScan
* fills pcBuf with the current scan data. The buffer must have at least
* the size returned by the echoGetCurrentScanLength function.
* returns 0 if an error occures
*/
int echoGetCurrentScan(pFH, pluImScNo, pcBuf)
DRDFILE *pFH;
ulong *pluImScNo;
char *pcBuf;
{
struct ECHO_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN echoGetCurrentScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoGetCurrentScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoGetCurrentScan",FILETYPE,pFH);
RETURN_IF_WRITE("echoGetCurrentScan",pFH);
RETURN_IF_FILE_POS_INVALID("echoGetCurrentScan",pFH);

/* store current position */
lCurPos = ftell(pFH->file);

/* read header */
fread(&head,sizeof(struct ECHO_DB_HEAD),1,pFH->file);
*pluImScNo = head.luImageScanNo;

/* read buffer */
if (head.uLength != fread(pcBuf,sizeof(char),head.uLength,pFH->file) ) {
	DPRINT("END echoGetCurrentScan - error reading file\n");
	return 0;
	}

/* restore position */
fseek(pFH->file,lCurPos,0);

DPRINT("END echoGetCurrentScan\n");
return -1;
}

/**********************************************************************
* echoWriteScan
* writes the scan data from the buffer into the file. Returns 0 if
* an error occures.
* assures, that the image scans No are ascendent, i.e. returns error
* if not
*/
int echoWriteScan(pFH, luImScNo, pcBuf, uLength)
DRDFILE *pFH;
ulong luImScNo;
char *pcBuf;
ushort uLength;
{
struct ECHO_DB_HEAD head;

DPRINT("BEGIN echoWriteScan\n");
RETURN_IF_DRDFILE_NOT_VALID("echoWriteScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("echoWriteScan",FILETYPE,pFH);
RETURN_IF_READ("echoWriteScan",pFH);
/* is image scan no ascendent ? */
if ( luImScNo <= pFH->luImageScanNo ) {
	DPRINT("END echoWriteScan - Image No. not ascendent\n");
	return 0;
	}

/* write header */
head.luImageScanNo = luImScNo;
head.uLength = uLength;
fwrite(&head,sizeof(struct ECHO_DB_HEAD),1,pFH->file);

/* write data */
fwrite(pcBuf,sizeof(char),uLength,pFH->file);

pFH->luImageScanNo = luImScNo;
DPRINT("END echoWriteScan\n");
return -1;
}

