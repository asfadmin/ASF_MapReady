/* zero.c
* this file includes the DRD library access functions for the
* zero data file
*
* release 1.0 07-06-1994 Hans-Joerg Wagner
*/

#include <stdio.h>
#include <malloc.h>
#include "drd.h"

/* this file contains zeros */
#define FILETYPE ZERO_FILETYPE
#define FILEEXT 	ZERO_FILEEXT
 
/** default comment */
char *apszZeroDefaultComment[] = {
	"This file includes the Zero data i.e. constant data received",
	"from the satellite to fill non data transmission time.",
	NULL
	};

/**********************************************************************
* zeroOpen
* openes an existing zero file, creates a DRDFILE structure for it
* and initializes the structure reading the header.
* Returns a pointer to the DRDFILE structure. If an Error occures,
* it returns NULL
*/ 
DRDFILE *zeroOpen(pszFileRoot)
char *pszFileRoot;
{
DRDFILE *p;
DPRINT("BEGIN zeroOpen\n");
RETURN_IF_NOT_INIT("zeroOpen");
p = drdOpenInternal(pszFileRoot,FILEEXT,FILETYPE);
DPRINT("END zeroOpen\n");
return p;
}

/**********************************************************************
* zeroClose
* this function closes the zero file and destroys the DRDFILE structure
* return 0 if an error occures
*/
int zeroClose(pFH)
DRDFILE *pFH;
{
int ret;

DPRINT("BEGIN zeroClose\n");
RETURN_IF_NOT_INIT("zeroClose");
RETURN_IF_DRDFILE_NOT_VALID("zeroClose",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END zeroClose\n");
return ret;
}

/**********************************************************************
* zeroCreate
* this function creates a new empty zero file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated 
*/
DRDFILE *zeroCreate(pszFileRoot,pszID,pszSatellite,apszComments)
char *pszFileRoot;
char *pszID;
char *pszSatellite;
char **apszComments;
{
DRDFILE *pFH;

DPRINT("BEGIN zeroCreate\n");
RETURN_IF_NOT_INIT("zeroCreate");
/* parameter testing */
if (NULL == pszID) {
	DPRINT("END zeroCreate - parameter pszID is NULL\n");
        return NULL;
	}
if (NULL == pszFileRoot)  {
	DPRINT("END zeroCreate - parameter pszFileRoot is NULL\n");
	return NULL;
	}
if (NULL == pszSatellite) {
	DPRINT("END zeroCreate - parameter pszSatellite is NULL \n");
	return NULL; 
	}

/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
	DPRINT("END zeroCreate - error allocating DRDFILE struct\n");
	return NULL;
	}

/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
	drdFreeDRDFILE(pFH);
	DPRINT("END zeroCreate - error allocating memory for file name \n");
	return NULL;
	}
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("Filename is %s\n",pFH->pszFileName);

/* file type */
strcpy(pFH->szType,FILETYPE);

/* Format version */
pFH->iTypeMajorVer = ZERO_MAJOR_VERSION;
pFH->iTypeMinorVer = ZERO_MINOR_VERSION;
DPRINT3("File type is %s V%d.%d\n",pFH->szType,
	pFH->iTypeMajorVer, pFH->iTypeMinorVer);
/* now create */
if (!drdCreateInternal(pFH,pszID, pszSatellite, apszZeroDefaultComment,apszComments)) {
	drdFreeDRDFILE(pFH);
	DPRINT("END zeroCreate - error creating file\n");
	return NULL;
	}
DPRINT("END zeroCreate\n");
return pFH;
}

/**********************************************************************
* zeroGotoFirstBlock
* sets the current file position to the first zero, useing
* the lnHeaderLength member of pFH
* returns 0 if not successful, e.g. the file is not opened for reading
*/
int zeroGotoFirstBlock(pFH)
DRDFILE *pFH;
{
char buf;

DPRINT("BEGIN zeroGotoFirstBlock\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroGotoFirstBlock",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroGotoFirstBlock",FILETYPE,pFH);
RETURN_IF_WRITE("zeroGotoFirstBlock",pFH);

fseek(pFH->file,pFH->lnHeaderLength,0);

/* filepos is invalid if end of file */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&buf,sizeof(char),1,pFH->file);
fseek(pFH->file,pFH->lnHeaderLength,0);
DPRINT("END zeroGotoFirstBlock\n");
return pFH->bFilePosValid;
}

/**********************************************************************
* zeroGotoLastBlock
* sets the current file position to the beginning of the last zero block.
* Therefore it scans through the whole file to the end of the file!
* return 0 if not successful
*/
int zeroGotoLastBlock(pFH)
DRDFILE *pFH;
{
long lnCurPos;
char buf;

DPRINT("BEGIN zeroGotoLastBlock\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroGotoLastBlock",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroGotoLastBlock",FILETYPE,pFH);
RETURN_IF_WRITE("zeroGotoLastBlock",pFH);

/* goto the first data block */
if (!zeroGotoFirstBlock(pFH)) {
	DPRINT("END zeroGotoLastBlock - could not find first block\n");
	return 0;
	}

/* scanning through the file */
do {
	lnCurPos = ftell(pFH->file);
	} while (zeroGotoNextBlock(pFH));
fseek(pFH->file,lnCurPos,0);

pFH->bFilePosValid = 1;
DPRINT("END zeroGotoLastBlock\n");
return -1;
}

/**********************************************************************
* zeroGotoNextBlock
* sets the current file position to the next zero block, therefore
* it reads the header of the current zero block to determin the
* start of the next zero block
* returns 0 if not successful, e.g. if the file end is reched
*/
int zeroGotoNextBlock(pFH)
DRDFILE *pFH;
{
struct ZERO_DB_HEAD head;
int nRead;
ulong luFilePos;

DPRINT("BEGIN zeroGotoNextBlock\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroGotoNextBlock",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroGotoNextBlock",FILETYPE,pFH);
RETURN_IF_WRITE("zeroGotoNextBlock",pFH);
RETURN_IF_FILE_POS_INVALID("zeroGotoNextBlock",pFH);

nRead = fread(&head,sizeof(struct ZERO_DB_HEAD),1,pFH->file);
/* if no full header read */
if ( nRead <= 0 ) {
	DPRINT1("END zeroGotoNextBlock - error reading in file %s\n",
		pFH->pszFileName);
	return 0;
	}

/* skip data block */
nRead = fseek(pFH->file,head.luLength,1);

/* try to read another byte to test fileend*/
luFilePos = ftell(pFH->file); /* store current position */
 /* read something !! bFilePosValid is just 1 bit long!!!*/
pFH->bFilePosValid = fread(&head,1,1,pFH->file);
fseek(pFH->file,luFilePos,0);  /* restore pos */

DPRINT("END zeroGotoNextBlock\n");
return pFH->bFilePosValid;
/* if this was not the end, at least one byte was read */
}

/**********************************************************************
* zeroGotoPrevBlock
* sets the current file position to the previous zero block.
* this version achieves the task by restarting at the very begining
* and scanning forewards through the file;
* return 0 if not successful, e.g. the current position is already the
* very first
*/
int zeroGotoPrevBlock(pFH)
DRDFILE *pFH;
{
long lPos,lOldPos;

DPRINT("BEGIN zeroGotoPrevBlock\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroGotoPrevBlock",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroGotoPrevBlock",FILETYPE,pFH);
RETURN_IF_WRITE("zeroGotoPrevBlock",pFH);
RETURN_IF_FILE_POS_INVALID("zeroGotoPrevBlock",pFH);

/* save current position */
lOldPos = ftell(pFH->file);

/* is current possiton already the first position ? */
if (lOldPos <= pFH->lnHeaderLength) {
	DPRINT("END zeroGotoPrevBlock - already at first position\n");
	return 0;
	}

/* scaning through the file */
if (!zeroGotoFirstBlock(pFH)) {
	DPRINT("END zeroGotoPrevBlock - cannot find first data block\n");
	return 0;
	}

do {
	lPos = ftell(pFH->file);
        zeroGotoNextBlock(pFH);
	} while (lOldPos > ftell(pFH->file));

fseek(pFH->file,lPos,0);
DPRINT("END zeroGotoPrevBlock\n");
return -1;
}

/**********************************************************************
* zeroGetCurrentBlockLength
* returns the min. required buffersize to receive the zero block
* by reading the header information of the current block
*/
ulong zeroGetCurrentBlockLength(pFH)
DRDFILE *pFH;
{
struct ZERO_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN zeroGetCurrentBlockLength\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroGetCurrentBlockLength",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroGetCurrentBlockLength",FILETYPE,pFH);
RETURN_IF_WRITE("zeroGetCurrentBlockLength",pFH);
RETURN_IF_FILE_POS_INVALID("zeroGetCurrentBlockLength",pFH);

/* save current possition */
lCurPos = ftell(pFH->file);
/* read header */
fread(&head,sizeof(struct ZERO_DB_HEAD),1,pFH->file);
/* resore file position */
fseek(pFH->file,lCurPos,0);
/* return length */
DPRINT("END zeroGetCurrentBlockLength\n");
return head.luLength;
}

/**********************************************************************
* zeroGetCurrentBlock
* fills pcBuf with the current block. The buffer must have at least
* the size returned by the zeroGetCurrentBlockLength function.
* returns 0 if an error occures
*/
int zeroGetCurrentBlock(pFH, pluImScNo, pcBuf)
DRDFILE *pFH;
ulong *pluImScNo;
char *pcBuf;
{
struct ZERO_DB_HEAD head;
long lCurPos;

DPRINT("BEGIN zeroGetCurrentBlock\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroGetCurrentBlock",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroGetCurrentBlock",FILETYPE,pFH);
RETURN_IF_WRITE("zeroGetCurrentBlock",pFH);
RETURN_IF_FILE_POS_INVALID("zeroGetCurrentBlock",pFH);

/* store current position */
lCurPos = ftell(pFH->file);

/* read header */
fread(&head,sizeof(struct ZERO_DB_HEAD),1,pFH->file);
*pluImScNo = head.luImageScanNo;

/* read buffer */
if (head.luLength != fread(pcBuf,sizeof(char),head.luLength,pFH->file) ) {
	DPRINT("END zeroGetCurrentBlock - error reading file\n");
	return 0;
	}

/* restore position */
fseek(pFH->file,lCurPos,0);

DPRINT("END zeroGetCurrentBlock\n");
return -1;
}

/**********************************************************************
* zeroWriteBlock
* writes the block from the buffer into the file. Returns 0 if
* an error occures.
* assures, that the image scans No are ascendent, i.e. returns error
* if not. If the image scan number is the same as in the write
* cycle before, the data is appended to the block before. To find the 
* position of the header before, the luBlockLength is filled with the
* length of the last written data block
*/
int zeroWriteBlock(pFH, luImScNo, pcBuf, luLength)
DRDFILE *pFH;
ulong luImScNo;
char *pcBuf;
ulong luLength;
{
struct ZERO_DB_HEAD head;

DPRINT("BEGIN zeroWriteBlock\n");
RETURN_IF_DRDFILE_NOT_VALID("zeroWriteBlock",pFH);
RETURN_IF_WRONG_FILE_TYPE("zeroWriteBlock",FILETYPE,pFH);
RETURN_IF_READ("zeroWriteBlock",pFH);
/* is image scan no ascendent ? */
if ( luImScNo < pFH->luImageScanNo ) {
	DPRINT("END zeroWriteBlock - image scan No not ascendent\n");
	return 0;
	}

head.luImageScanNo = luImScNo;
head.luLength = luLength;
/* if the same image #: find the header and append the data */
if (luImScNo == pFH->luImageScanNo ) {
	/* errorfix: 07-26-1994. if luImScNo is 0 and the first block, do */
	/* not overwrite header!! HJWagner */
	if (luImScNo != 0 || ftell(pFH->file) != pFH->lnHeaderLength ) {
	   /* go to the beginning of the last written block */
	   fseek(pFH->file,- pFH->luBlockLength -sizeof(struct ZERO_DB_HEAD),1);
	   }
	/* rewrite the header */
	head.luLength += pFH->luBlockLength;
	fwrite(&head, sizeof(struct ZERO_DB_HEAD),1,pFH->file);
	/* go to end of block to write the additional data */
	fseek(pFH->file, pFH->luBlockLength, 1);
	}
else {
	/* write header */
	fwrite(&head,sizeof(struct ZERO_DB_HEAD),1,pFH->file);
	}

/* write data */
fwrite(pcBuf,sizeof(char),luLength,pFH->file);

pFH->luImageScanNo = luImScNo;
pFH->luBlockLength = head.luLength;

DPRINT("END zeroWriteBlock\n");
return -1;
}

