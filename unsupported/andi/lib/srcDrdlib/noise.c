/* noise.c
* this file includes the DRD library access functions for the
* noise data file
*
* release 1.0 07-06-1994 Hans-Joerg Wagner
*/
#include <stdio.h>
#include <malloc.h>
#include "drd.h"

#define FILETYPE NOISE_FILETYPE
#define FILEEXT NOISE_FILEEXT

/* default comment lines  for voice data file */
char *apszNoiseDefaultComment[] = {
	"This file includes the noise samples of a SAR radar.",
	NULL
	};

/**********************************************************************
* noiseOpen
* openes an existing noise file, creates a DRDFILE structure for it
* and initializes the structure reading the header.
* Returns a pointer to the DRDFILE structure. If an Error occures,
* it returns NULL
*/
DRDFILE *noiseOpen(pszFileRoot)
char *pszFileRoot;
{
DRDFILE *p;

DPRINT("BEGIN noiseOpen\n");
RETURN_IF_NOT_INIT("noiseOpen");
p = drdOpenInternal(pszFileRoot,FILEEXT,FILETYPE);
DPRINT("END noiseOpen\n");
return p;
}

/**********************************************************************
* noiseClose
* this function closes the noise file and destroys the DRDFILE structure
*/
int noiseClose(pFH)
DRDFILE *pFH;
{
int ret;

DPRINT("BEGIN noiseClose\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseClose",pFH);
RETURN_IF_WRONG_FILE_TYPE("noiseClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END noiseClose\n");
return ret;
}

/**********************************************************************
* noiseCreate
* this function creates a new empty noise file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated
*/
DRDFILE *noiseCreate(pszFileRoot,pszID,pszSatellite,apszComments)
char *pszFileRoot;
char *pszID;
char *pszSatellite;
char **apszComments;
{
DRDFILE *pFH;
DPRINT("BEGIN noiseCreate\n");
RETURN_IF_NOT_INIT("noiseCreate");
/* parameter testing */
if (NULL == pszID) {
	DPRINT("END noiseCreate - pszID is NULL\n");
        return NULL;
	}
if (NULL == pszFileRoot){
	DPRINT("END noiseCreate - pszFileRoot is NULL\n");
        return NULL;
	}
if (NULL == pszSatellite) {
	DPRINT("END noiseCreate - pszSatellite is NULL\n");
        return NULL;
	}
 
/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
	DPRINT("END noiseCreate - error creating DRDFILE struct\n");
        return NULL;
	}
 
/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
        drdFreeDRDFILE(pFH);
	DPRINT("END noiseCreate - error allocating memory for file name\n");
        return NULL;
        }
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("Filename is %s\n",pFH->pszFileName);
 
/* file type */
strcpy(pFH->szType,FILETYPE);
 
/* Format version */
pFH->iTypeMajorVer = NOISE_MAJOR_VERSION;
pFH->iTypeMinorVer = NOISE_MINOR_VERSION;
DPRINT3("File type is %s V%d.%d\n",pFH->szType,
		pFH->iTypeMajorVer,pFH->iTypeMinorVer);
 
/* now create */
if (!drdCreateInternal(pFH,pszID,pszSatellite, apszNoiseDefaultComment,apszComments)) {
        drdFreeDRDFILE(pFH);
	DPRINT("END noiseCreate - error creating noise file\n");
        return NULL;
        }
DPRINT("END noiseCreate\n");
return pFH;
}

/**********************************************************************
* noiseGotoFirstScan
* sets the current file position to the first noise data block, useing
* the lnHeaderLength member of pFH
* returns 0 if not successful, e.g. the file is not opened for reading
*/
int noiseGotoFirstScan(pFH)
DRDFILE *pFH;
{
char buf;

DPRINT("BEGIN noiseGotoFirstScan\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseGotoFirstScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("noiseGotoFirstScan",FILETYPE,pFH);
RETURN_IF_WRITE("noiseGotoFirstScan",pFH); 

fseek(pFH->file,pFH->lnHeaderLength,0);
 
/* filepos is invalid if end of file */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&buf,sizeof(char),1,pFH->file);
fseek(pFH->file,pFH->lnHeaderLength,0);
DPRINT("END noiseGotoFirstScan\n");
return pFH->bFilePosValid;
}
 
/**********************************************************************
* noiseGotoLastScan
* sets the current file position to the beginning of the last noise data
* block. Therefore it scans to the whole file to the end of the file!
* return 0 if not successful
*/
int noiseGotoLastScan(pFH)
DRDFILE *pFH;
{
long lnCurPos;
char buf;

DPRINT("BEGIN noiseGotoLastScan\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseGotoLastScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("noiseGotoLastScan",FILETYPE,pFH);
RETURN_IF_WRITE("noiseGotolastScan",pFH);
 
/* goto the first data block */
if (!noiseGotoFirstScan(pFH)) {
	DPRINT("END noiseGotoLastScan - error going to first noise scan\n");
        return 0;
	}
 
/* scanning through the file */
do {
        lnCurPos = ftell(pFH->file);
        } while (noiseGotoNextScan(pFH));
fseek(pFH->file,lnCurPos,0);
 
pFH->bFilePosValid = 1;
DPRINT("END noiseGotoLastScan\n");
return -1;
}
 
/**********************************************************************
* noiseGotoNextScan
* sets the current file position to the next data block, therefore
* it reads the header of the current data block to determin the
* start of the next data block
* returns 0 if not successful, e.g. if the file end is reched
*/
int noiseGotoNextScan(pFH)
DRDFILE *pFH;
{
struct NOISE_DB_HEAD head;
int nRead;
ulong luFilePos;

DPRINT("BEGIN noiseGotoNextScan\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseGotoNextScan",pFH); 
RETURN_IF_WRONG_FILE_TYPE("noiseGotoNextScan",FILETYPE,pFH);
RETURN_IF_WRITE("noiseGotoNextScan",pFH);
RETURN_IF_FILE_POS_INVALID("noiseGotoNextScan",pFH); 
 
nRead = fread(&head,sizeof(struct NOISE_DB_HEAD),1,pFH->file);
/* if no full header read */
if ( nRead <= 0 ) {
	DPRINT("END noiseGotoNextScan - error reading block header\n");
        return 0;
	}
 
/* skip data block */
nRead = fseek(pFH->file,head.uLength,1);

/* try to read another byte to test fileend*/
luFilePos = ftell(pFH->file); /* store current position */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&head,1,1,pFH->file);
fseek(pFH->file,luFilePos,0);  /* restore pos */
 
DPRINT("END noiseGotoNextScan\n");
return pFH->bFilePosValid;
/* if this was not the end, at least one byte was read */
}
 
/**********************************************************************
* noiseGotoPrevScan
* sets the current file position to the previous data block.
* this version achieved the task by restarting at the very begining
* and scanning forewards through the file;
* return 0 if not successful, e.g. the current position is already the
* very first
*/
int noiseGotoPrevScan(pFH)
DRDFILE *pFH;
{
long lPos,lOldPos;

DPRINT("BEGIN noiseGotoPrevScan\n"); 
RETURN_IF_DRDFILE_NOT_VALID("noiseGotoPrevScan",pFH);
RETURN_IF_WRONG_FILE_TYPE("noiseGotoPrevScan",FILETYPE,pFH);
RETURN_IF_WRITE("noiseGotoPrevScan",pFH);
RETURN_IF_FILE_POS_INVALID("noiseGotoPrevScan",pFH);

/* save current position */
lOldPos = ftell(pFH->file);
 
/* is current possiton already the first position ? */
if (lOldPos <= pFH->lnHeaderLength) {
	DPRINT("END noiseGotoPrevScan - already at first position\n");
        return 0;
	}
 
/* scaning through the file */
if (!noiseGotoFirstScan(pFH)) {
	DPRINT("END noiseGotoPrevScan - cannot find first Scan\n");
        return 0;
	}
 
do {
        lPos = ftell(pFH->file);
        noiseGotoNextScan(pFH);
        } while (lOldPos > ftell(pFH->file));
 
fseek(pFH->file,lPos,0);
DPRINT("END noiseGotoPrevScan\n");
return -1;
}
 
/**********************************************************************
* noiseGetCurrentScanLength
* returns the min. required buffersize to receive the noise data block
* by reading the header information of the current block
*/
ushort noiseGetCurrentScanLength(pFH)
DRDFILE *pFH;
{
struct NOISE_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN noiseGetCurrentScanLength\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseGetCurrentScanLength",pFH); 
RETURN_IF_WRONG_FILE_TYPE("noiseGetCurrentScanLength",FILETYPE,pFH);
RETURN_IF_WRITE("noiseGetCurrentScanLength",pFH);
RETURN_IF_FILE_POS_INVALID("noiseGetCurrentScanLength",pFH);
 
/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct NOISE_DB_HEAD),1,pFH->file);
/* resore file position */
fseek(pFH->file,lCurPos,0);
/* return length */
DPRINT("END noiseGetCurrentScanLength\n");
return head.uLength;
}
 
/**********************************************************************
* noiseGetCurrentScan
* fills pcBuf with the current noise data block. The buffer must have at least
* the size returned by the noiseGetCurrentScanLength function.
* returns 0 if an error occures
*/
int noiseGetCurrentScan(pFH, pluImScNo, pcBuf)
DRDFILE *pFH;
ulong *pluImScNo;
char *pcBuf;
{
struct NOISE_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN noiseGetCurrentScan\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseGetCurrentScan",pFH); 
RETURN_IF_WRONG_FILE_TYPE("noiseGetCurrentScan",FILETYPE,pFH);
RETURN_IF_WRITE("noiseGetCurrentScan",pFH);
RETURN_IF_FILE_POS_INVALID("noiseGetCurrentScan",pFH);
 
/* store current position */
lCurPos = ftell(pFH->file);
 
/* read header */
fread(&head,sizeof(struct NOISE_DB_HEAD),1,pFH->file);
*pluImScNo = head.luImageScanNo;
 
/* read buffer */
if (head.uLength != fread(pcBuf,sizeof(char),head.uLength,pFH->file) ) {
	DPRINT("END noiseGetCurrentScan - error reading file\n");
        return 0;
	}
 
/* restore position */
fseek(pFH->file,lCurPos,0);

DPRINT("END noiseGetCurrentScan\n"); 
return -1;
}
 
/**********************************************************************
* noiseWriteScan
* writes the noise data from the buffer into the file. Returns 0 if
* an error occures.
* assures, that the image scans No are ascendent, i.e. returns error
* if not
*/
int noiseWriteScan(pFH, luImScNo, pcBuf, uLength)
DRDFILE *pFH;
ulong luImScNo;
char *pcBuf;
ushort uLength;
{
struct NOISE_DB_HEAD head;

DPRINT("BEGIN noiseWriteScan\n");
RETURN_IF_DRDFILE_NOT_VALID("noiseWriteScan",pFH); 
RETURN_IF_WRONG_FILE_TYPE("noiseWriteScan",FILETYPE,pFH);
RETURN_IF_READ("noiseWriteScan",pFH);

/* is image scan no ascendent ? */
if ( luImScNo <= pFH->luImageScanNo ) {
	DPRINT("END noiseWriteScan - image No. not ascendent\n");
        return 0;
	}
 
/* write header */
head.luImageScanNo = luImScNo;
head.uLength = uLength;
fwrite(&head,sizeof(struct NOISE_DB_HEAD),1,pFH->file);
 
/* write data */
fwrite(pcBuf,sizeof(char),uLength,pFH->file);

pFH->luImageScanNo = luImScNo; 
DPRINT("END noiseWriteScan\n");
return -1;
}

