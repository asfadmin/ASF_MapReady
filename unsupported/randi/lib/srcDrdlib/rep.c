/* rep.c
* this file includes the DRD library access functions for the
* pulse replica data file
* 
* release 1.0 07-11-1994 Hans-Joerg Wagner
*/
#include <stdio.h>
#include <malloc.h>
#include "drd.h"
 
#define FILETYPE REP_FILETYPE
#define FILEEXT REP_FILEEXT
 
/** default comment */
char *apszRepDefaultComment[] = {
        "This file includes the samples of SAR radar pulse replicas",
        NULL
        };

/*********************************************************************
*
* repOpen
* openes an existing rep file, creates a DRDFILE structure for it
* and initializes the structure reading the header.
* Returns a pointer to the DRDFILE structure. If an Error occures,
* it returns NULL
*/
DRDFILE *repOpen(pszFileRoot)
char *pszFileRoot;
{
DRDFILE *p;
DPRINT("BEGIN repOpen\n");
RETURN_IF_NOT_INIT("repOpen");
p = drdOpenInternal(pszFileRoot,FILEEXT,FILETYPE);
DPRINT("END repOpen\n");
return p;
}
 
/*********************************************************************
*
* repClose
* this function closes the rep file and destroys the DRDFILE structur
* return 0 if an error occures
*/
int repClose(pFH)
DRDFILE *pFH;
{
int ret;
DPRINT("BEGIN repClose\n");
RETURN_IF_DRDFILE_NOT_VALID("repClose",pFH);
RETURN_IF_WRONG_FILE_TYPE("repClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END repClose\n");
return ret;
}
 
/**********************************************************************
* repCreate
* this function creates a new empty rep file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated 
*/
DRDFILE *repCreate(pszFileRoot,pszID,pszSatellite,apszComments)
char *pszFileRoot;
char *pszID;
char *pszSatellite;
char **apszComments;
{
DRDFILE *pFH;

DPRINT("BEGIN repCreate\n");
RETURN_IF_NOT_INIT("repCreate");
/* parameter testing */
if (NULL == pszID) {
	DPRINT("END repCreate - parameter pszID is NULL\n");
        return NULL;
	}
if (NULL == pszFileRoot) {
	DPRINT("END repCreate - parameter pszFileRoot is NULL\n");
	return NULL;
	}
if (NULL == pszSatellite) {
	DPRINT("END repCreate - parameter pszSatellite is NULL\n");
	return NULL; 
	}

/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
	DPRINT("END repCreate - error allocating DRDFILE struct\n");
	return NULL;
	}

/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
	drdFreeDRDFILE(pFH);
	DPRINT("END repCreate - error allocating memory for file name\n");
	return NULL;
	}
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("Filename is %s\n",pFH->pszFileName);

/* file type */
strcpy(pFH->szType,FILETYPE);

/* Format version */
pFH->iTypeMajorVer = REP_MAJOR_VERSION;
pFH->iTypeMinorVer = REP_MINOR_VERSION;
DPRINT3("File type is %s V%d.%d\n",pFH->szType,pFH->iTypeMajorVer,
                                        pFH->iTypeMinorVer);

/* now create */
if (!drdCreateInternal(pFH,pszID, pszSatellite, apszRepDefaultComment, apszComments)) {
	drdFreeDRDFILE(pFH);
	DPRINT("END repCreate - error creating file\n");
	return NULL;
	}
DPRINT("END repCreate\n");
return pFH;
}

/**********************************************************************
* echoGotoFirstScan
* sets the current file position to the first rep data block, useing
* the lnHeaderLength member of pFH
* returns 0 if not successful, e.g. the file is not opened for reading
*/
int repGotoFirstScan(pFH)
DRDFILE *pFH;
{
char buf;

DPRINT("BEGIN repGotoFirstScan\n");
RETURN_IF_DRDFILE_NOT_VALID("repGotoFirstScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGotoFirstScan",FILETYPE,pFH);
RETURN_IF_WRITE("repGotoFirstScan",pFH);

fseek(pFH->file,pFH->lnHeaderLength,0);
/* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&buf,sizeof(char),1,pFH->file);
fseek(pFH->file,pFH->lnHeaderLength,0);

DPRINT("END repGotoFirstScan\n");
return pFH->bFilePosValid;
}

/**********************************************************************
* repGotoLastScan
* sets the current file position to the beginning of the last rep data 
* block. Therefore it scans to the whole file to the end of the file!
* return 0 if not successful
*/
int repGotoLastScan(pFH)
DRDFILE *pFH;
{
long lnCurPos;
char buf;

DPRINT("BEGIN repGotoLastScan\n");
RETURN_IF_DRDFILE_NOT_VALID("repGotoLastScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGotoLastScan",FILETYPE,pFH);
RETURN_IF_WRITE("repGotoLastScan",pFH);

/* goto the first data block */
if (!repGotoFirstScan(pFH)) {
	DPRINT("END repGotoLastScan - error searching first data block\n");
	return 0;
	}

/* scanning through the file */
do {
	lnCurPos = ftell(pFH->file);
	} while (repGotoNextScan(pFH));
fseek(pFH->file,lnCurPos,0);

pFH->bFilePosValid = 1;
DPRINT("END repGotoLastScan\n");
return -1;
}

/**********************************************************************
* repGotoNextScan
* sets the current file position to the next data block, therefore
* it reads the header of the current data block to determin the
* start of the next data block
* returns 0 if not successful, e.g. if the file end is reched
*/
int repGotoNextScan(pFH)
DRDFILE *pFH;
{
struct REP_DB_HEAD head;
int nRead;
ulong luFilePos;

DPRINT("BEGIN repGotoNextScan\n");
RETURN_IF_DRDFILE_NOT_VALID("repGotoNextScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGotoNextScan",FILETYPE,pFH);
RETURN_IF_WRITE("repGotoNextScan",pFH);
RETURN_IF_FILE_POS_INVALID("rep	GotoNextScan",pFH);

nRead = fread(&head,sizeof(struct REP_DB_HEAD),1,pFH->file);
/* if no full header read */
if ( nRead <= 0 ) {
	DPRINT("END repGotoNextScan - error reading block header\n");
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

DPRINT("END repGotoNextScan\n");
return pFH->bFilePosValid;
/* if this was not the end, at least one byte was read */
}

/**********************************************************************
* repGotoPrevScan
* sets the current file position to the previous data block.
* this version achieves the task by restarting at the very begining
* and scanning forewards through the file;
* return 0 if not successful, e.g. the current position is already the
* very first
*/
int repGotoPrevScan(pFH)
DRDFILE *pFH;
{
long lPos,lOldPos;

DPRINT("BEGIN repGotoPrevScan\n");
RETURN_IF_DRDFILE_NOT_VALID("repGotoPrevScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGotoPrevScan",FILETYPE,pFH);
RETURN_IF_WRITE("repGotoPrevScan",pFH);
RETURN_IF_FILE_POS_INVALID("repGotoPrevScan",pFH);

/* save current position */
lOldPos = ftell(pFH->file);

/* is current possiton already the first position ? */
if (lOldPos <= pFH->lnHeaderLength) {
	DPRINT("END repGotoPrevScan - already at first data block\n");
	return 0;
	}

/* scaning through the file */
if (!repGotoFirstScan(pFH)) {
	DPRINT("END repGotoPrevScan - cannot find first data block\n");
	return 0;
	}

do {
	lPos = ftell(pFH->file);
        repGotoNextScan(pFH);
	} while (lOldPos > ftell(pFH->file));

fseek(pFH->file,lPos,0);
DPRINT("END repGotoPrevScan\n");
return -1;
}

/**********************************************************************
* repGetCurImageScanNo
* this function looks for the image scan no at the current file
* position. It returns the Calibration pulse for the current image
* scan number, because there could be more than one Calibration pulse
* associated with the same image scan number.
* It returns 0 if an error occures
*/
uint repGetCurImageScanNo(pFH,pluImageScanNo)
DRDFILE *pFH;
ulong *pluImageScanNo;
{
struct REP_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN repGetCurImageScanNo\n");
RETURN_IF_DRDFILE_NOT_VALID("repGetCurImageScanNo",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGetCurImageScanNo",FILETYPE,pFH);
RETURN_IF_WRITE("repGetCurImageScanNo",pFH);
RETURN_IF_FILE_POS_INVALID("repGetCurImageScanNo",pFH);

/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct REP_DB_HEAD),1,pFH->file);
/* restore file position
fseek(pFH->file,lCurPos,0);
/* return No  and Image scan No */
(*pluImageScanNo) = head.luImageScanNo;
DPRINT("END repGetCurImageScanNo");
return -1;
}

/**********************************************************************
* repGetCurrentScanLength
* returns the min. required buffersize to receive the echo data block
* by reading the header information of the current block
*/
ushort repGetCurrentScanLength(pFH)
DRDFILE *pFH;
{
struct REP_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN repGetCurrentScanLength\n");
RETURN_IF_DRDFILE_NOT_VALID("repGetCurrentScanLength",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGetCurrentScanLength",FILETYPE,pFH);
RETURN_IF_WRITE("repGetCurrentScanLength",pFH);
RETURN_IF_FILE_POS_INVALID("repGetCurrentScanLength",pFH);

/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct REP_DB_HEAD),1,pFH->file);
/* resore file position */
fseek(pFH->file,lCurPos,0);
/* return length */
DPRINT("END repGetCurrentScanLength\n");
return head.uLength;
}

/**********************************************************************
* repGetCurrentScan
* fills pcBuf with the current rep data. The buffer must have at least
* the size returned by the repGetCurrentScanLength function.
* returns the No of the pulse. If an error occures it returns 0
*/
int repGetCurrentScan(pFH, pluImScNo, pcBuf)
DRDFILE *pFH;
ulong *pluImScNo;
char *pcBuf;
{
struct REP_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN repGetCurrentScan\n");
RETURN_IF_DRDFILE_NOT_VALID("repGetCurrentScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("repGetCurrentScan",FILETYPE,pFH);
RETURN_IF_WRITE("repGetCurrentScan",pFH);
RETURN_IF_FILE_POS_INVALID("repGetCurrentScan",pFH);

/* store current position */
lCurPos = ftell(pFH->file);

/* read header */
fread(&head,sizeof(struct REP_DB_HEAD),1,pFH->file);
*pluImScNo = head.luImageScanNo;

/* read buffer */
if (head.uLength != fread(pcBuf,sizeof(char),head.uLength,pFH->file) ) {
	DPRINT("END repGetCurrentScan - error reading file\n");
	return 0;
	}

/* restore position */
fseek(pFH->file,lCurPos,0);

DPRINT("END repGetCurrentScan\n");
return -1;
}

/**********************************************************************
* repWriteScan
* writes the scan data from the buffer into the file. Returns 0 if
* an error occures.
* assures, that the image scans No are ascendent, i.e. returns error
* if not
*/
int repWriteScan(pFH, luImScNo, pcBuf, uLength)
DRDFILE *pFH;
ulong luImScNo;
char *pcBuf;
ushort uLength;
{
struct REP_DB_HEAD head;

DPRINT("BEGIN repWriteScan\n");
RETURN_IF_DRDFILE_NOT_VALID("repWriteScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("repWriteScan",FILETYPE,pFH);
RETURN_IF_READ("repWriteScan",pFH);


/* write header */
head.luImageScanNo = luImScNo;
head.uLength = uLength;
fwrite(&head,sizeof(struct REP_DB_HEAD),1,pFH->file);

/* write data */
fwrite(pcBuf,sizeof(char),uLength,pFH->file);

pFH->luImageScanNo = luImScNo;

DPRINT("END repWriteScan\n");
return -1;
}


