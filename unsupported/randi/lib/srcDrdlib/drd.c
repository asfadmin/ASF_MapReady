/* drd.c
* this file includes the code for the common functions and structures
* of the drd access library
* first setup 6-21-1994 by Hans-Joerg Wagner
* release 1.0 07-11-1994 HJW
*/

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <malloc.h>
#include <ctype.h>
#include <unistd.h>
#include "drd.h"
/**********************************************************************
* global variables
*/
char *drdpszProgram = NULL;
int drdiProgMajorVer;
int drdiProgMinorVer;
char *drdpszFacility = NULL;
int bDrdDebug = 0;

/**********************************************************************
* debugPrintDRDFILE
* prints the contents of a DRDFILE structure to the standard screen
*/
#define PRINTSTRING(a,b) if (NULL != b) printf("\t%s = %s\n",a,b); \
			 else printf("\t%s = NULL\n",a);
#define PRINTFLAG(a,b) printf("\t%s = %s\n",a,b ? "TRUE" : "FALSE");

void debugPrintDRDFILE(pFH)
DRDFILE *pFH;
{
int count;

printf("DRDFILE structure:\n");
printf("\tfile = %s\n",(NULL == pFH->file) ? "NULL" : "not NULL");
PRINTSTRING("pszFileName",pFH->pszFileName);
PRINTFLAG("bRead",pFH->bRead);
PRINTFLAG("bStdOut",pFH->bStdOut);
PRINTFLAG("bFilePosValid",pFH->bFilePosValid);
printf("\tlnHeaderLength = %ld\n",pFH->lnHeaderLength);
PRINTSTRING("szType",pFH->szType);
printf("\tTypeVersion %d.%d\n",pFH->iTypeMajorVer,pFH->iTypeMinorVer);
PRINTSTRING("szDate",pFH->szDate);
PRINTSTRING("pszProgram",pFH->pszProgram);
printf("\tProgVersion %d.%d\n",pFH->iProgMajorVer,pFH->iProgMinorVer);
PRINTSTRING("pszSatellite",pFH->pszSatellite);
PRINTSTRING("pszFacility",pFH->pszFacility);
PRINTSTRING("pszID",pFH->pszID);
if (NULL == pFH->apszComments)
	printf("\tapszComments = none\n");
else {
	for (count = 0; NULL != pFH->apszComments[count]; count++)
	   printf("\tapszComments[%d] = %s\n",count,pFH->apszComments[count]);
	}
printf("DRDFILE end structure\n");
}
#undef PRINTFLAG
#undef PRINTSTRING

/**********************************************************************
* drdInit 
* Initializes the drd access library. This function has to be
* called before any other function of the library is executed.
* returns 0 if an error occures.
*/
int drdInit(pszProgram,iProgMajorVer,iProgMinorVer,pszFacility) 
char *pszProgram,*pszFacility;
int iProgMajorVer,iProgMinorVer;
{
DPRINT("BEGIN drdInit:\n");
if (NULL != drdpszProgram || NULL != drdpszFacility ) {
	DPRINT("END drdInit - Called drdInit twice\n");
	return 0;
	}
if (NULL == pszProgram) {
	DPRINT("END drdInit - no program name given\n");
	return 0;
	}
if (NULL == pszFacility) {
	DPRINT("END drdInit - no facility name given\n");
	return 0;
	}

drdpszProgram = malloc((int)strlen(pszProgram) + 2);
if (NULL == drdpszProgram) {
	DPRINT("END drdInit - Failed allocating memory for Program name\n");
	return 0;
	}
strcpy(drdpszProgram,pszProgram);
DPRINT1("Program name set to: %s\n",drdpszProgram);

drdiProgMajorVer = iProgMajorVer;
drdiProgMinorVer = iProgMinorVer;
DPRINT2("Program version set to: %d.%d\n",drdiProgMajorVer,drdiProgMinorVer);

drdpszFacility = malloc(strlen(pszFacility) + 2);
if (NULL == drdpszFacility) {
	DPRINT("END drdInit - Failed allocating memory for Facility name\n");
	free(drdpszProgram);
	return 0;
	}
strcpy(drdpszFacility,pszFacility);
DPRINT1("Facility name set to: %s\n",drdpszFacility);

DPRINT("END drdInit\n");
return -1;
}

/**********************************************************************
* drdExit 
* Exits the drd access library. This function should be executed after
* the last call to any other library routine and before exiting the
* program
*/
void drdExit()
{
DPRINT("BEGIN drdExit\n");
if (NULL != drdpszProgram)
	free(drdpszProgram);
else
	DPRINT("Warning: No Program name was set\n");
drdpszProgram = NULL;
if (NULL != drdpszFacility)
	free(drdpszFacility);
else
	DPRINT("Warning: No Facility name was set\n");
drdpszFacility = NULL;
DPRINT("END drdExit\n");
}

/**********************************************************************
* drdOpen
* opens a file from filename and reads the header
* returns a valid pointer to a DRDFILE structure, describing the file
* returns a NULL pointer if an error occures
*/
DRDFILE *drdOpen(pszName)
char *pszName;
{
DRDFILE *pFile;
 
DPRINT("BEGIN drdOpen\n");
/* generate DRDFILE structure: */
pFile = drdAllocDRDFILE();
if (NULL == pFile) {
        DPRINT("END drdOpen -Failed allocating DRDFILE structure\n");
        return NULL;
        }
 
/* generating name for file */
pFile->pszFileName = malloc( strlen(pszName) + 1);
if (NULL == pFile->pszFileName) {
        DPRINT("END drdOpen - Failed allocating memory for filename\n");
        drdFreeDRDFILE(pFile);
        return NULL;
        }
strcpy(pFile->pszFileName,pszName);
DPRINT1("File to open is %s\n",pFile->pszFileName);
 
pFile->file = fopen(pFile->pszFileName,"r");
 
/* if something was wrong until here error! */
if (NULL == pFile->file) {
        DPRINT("END drdOpen - Failed opening file");
        drdFreeDRDFILE(pFile);
        return NULL;
        } 
DPRINT("File successfuly opened\n");
 
/* mark as read only */
pFile->bRead = 1;
 
/* now read the header */
if (!drdReadHeaderInternal(pFile)) {
        DPRINT("Failed reading file Header\n");
        fclose (pFile->file);
        drdFreeDRDFILE(pFile);
        DPRINT("END drdOpen - Failed reading file Header\n");
        return NULL;
        }
pFile->bFilePosValid = 0; 
DPRINT("END drdOpen\n");
return pFile;
}

/***********************************************************************
* drdOpenOtherFileType
* replaces the extention of name with the new extention and openes
* the file. Used to Open e.g. the auxiliary file fitting to the echo file
* Sample:
* drdOpenOtherFileType("hahaha.echo",AUX_FILEEXT);
*
* Returns NULL if not successful
*/
DRDFILE *drdOpenOtherFileType(pszName,pszNewExt)
char *pszName,*pszNewExt;
{
char *pTmp;
char *pszNewName;
DRDFILE *pFH;

DPRINT("BEGIN drdOpenOtherFileType\n");

/* find extention */
pTmp = strrchr(pszName,'.');
if (NULL == pTmp)  {
	DPRINT1("Cannot find extention in file name '%s'\n",pszName);
	return NULL;
	}

/* allocate space for new name, the buffer is a little too long, who cares? */
pszNewName = malloc(strlen(pszName) + strlen(AUX_FILEEXT) + 1);
strncpy(pszNewName,pszName, strlen(pszName) - strlen(pTmp) );

strcat(pszNewName,pszNewExt);

pFH = drdOpen(pszNewName);

free(pszNewName);
return pFH;
}

/**********************************************************************
* drdClose closes a file independend of the type
* this function is just an "object oriented" overhead and selects
* the proper close function from the szType entry within the DRDFILE 
* structure
* returns 0 if an error occures
*/
int drdClose(pFH)
DRDFILE *pFH;
{
int ret;

DPRINT("BEGIN drdClose\n");

if (NULL == pFH) {
	DPRINT("Invalid DRDFILE structure");
	return 0;
	}

if (!strcmp(pFH->szType,ECHO_FILETYPE) )
	ret = echoClose(pFH);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
	ret = noiseClose(pFH);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
	ret = calClose(pFH);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
	ret = repClose(pFH);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
	ret = zeroClose(pFH);
else if (!strcmp(pFH->szType,AUX_FILETYPE) )
        ret = auxClose(pFH);
else if (!strcmp(pFH->szType,REPORT_FILETYPE) )
	ret = reportClose(pFH);
else {
    if (!bDrdDebug)
	fprintf(stderr,"Warning: cannot execute drdClose for file type '%s'\n",
		pFH->szType);
    DPRINT1("Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret) {
    DPRINT("END drdClose\n");
    }
else
    DPRINT("END drdClose - an error occured\n");
return ret;
}

/**********************************************************************
* drdGotoFirstBlock
* sets the current file position to the first data block, using
* the lnHeaderLength member of pFH
* returns 0 if not successful, e.g. the file is not opened for reading
*
* this function is just an "object oriented" overhead and selects
* the proper GotoFirstBlock or GotoFirstScan function from the szType 
* entry within the DRDFILE structure
*/
int drdGotoFirstBlock(pFH)
DRDFILE *pFH;
{
int ret;
 
DPRINT("BEGIN drdGotoFirstBlock\n");
 
if (!strcmp(pFH->szType,ECHO_FILETYPE) )
        ret = echoGotoFirstScan(pFH);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
        ret = noiseGotoFirstScan(pFH);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
        ret = calGotoFirstScan(pFH);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
        ret = repGotoFirstScan(pFH);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
        ret = zeroGotoFirstBlock(pFH);
else {
    if (!bDrdDebug) 
        fprintf(stderr,"Warning: cannot execute drdGotoFirstBlock for file type '%s'\n", pFH->szType);
    DPRINT1("drdGotoFirstBlock - Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret) 
        { DPRINT("END drdGotoFirstBlock\n"); }
else	
        { DPRINT("END drdGotoFirstBlock - an error occured\n"); }
return ret;
}

/**********************************************************************
* drdGotoLastBlock
* sets the current file position to the beginning of the last scan data
* block. Therefore it scans to the whole file to the end of the file!
* return 0 if not successful
*
* this function is just an "object oriented" overhead and selects
* the proper GotoLastBlock or GotoLastScan function from the szType 
* entry within the DRDFILE structure
*/
int drdGotoLastBlock(pFH)
DRDFILE *pFH;
{
int ret;
 
DPRINT("BEGIN drdGotoLastBlock\n");
 
if (!strcmp(pFH->szType,ECHO_FILETYPE) )
        ret = echoGotoLastScan(pFH);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
        ret = noiseGotoLastScan(pFH);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
        ret = calGotoLastScan(pFH);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
        ret = repGotoLastScan(pFH);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
        ret = zeroGotoLastBlock(pFH);
else {
    if (!bDrdDebug) 
        fprintf(stderr,"Warning: cannot execute drdGotoLastBlock for file type '%s'\n", pFH->szType);
    DPRINT1("drdGotoLastBlock - Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret)
        DPRINT("END drdGotoLastBlock\n")
else
        DPRINT("END drdGotoLastBlock - an error occured\n");
return ret;
}

/**********************************************************************
* drdGotoNextBlock
* sets the current file position to the next data block, therefore
* it reads the header of the current data block to determin the
* start of the next data block
* returns 0 if not successful, e.g. if the file end is reched
*
* this function is just an "object oriented" overhead and selects
* the proper GotoNextBlock or GotoNextScan function from the szType
* entry within the DRDFILE structure
*/
int drdGotoNextBlock(pFH)
DRDFILE *pFH;
{
int ret;
 
DPRINT("BEGIN drdGotoNextBlock\n");
 
if (!strcmp(pFH->szType,ECHO_FILETYPE) )
        ret = echoGotoNextScan(pFH);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
        ret = noiseGotoNextScan(pFH);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
        ret = calGotoNextScan(pFH);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
        ret = repGotoNextScan(pFH);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
        ret = zeroGotoNextBlock(pFH);
else {
    if (!bDrdDebug)
        fprintf(stderr,"Warning: cannot execute drdGotoNextBlock for file type '%s'\n", pFH->szType);
    DPRINT1("drdGotoNextBlock - Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret)
        DPRINT("END drdGotoNextBlock\n")
else
        DPRINT("END drdGotoNextBlock - an error occured\n")
return ret;
}

/**********************************************************************
* drdGotoPrevBlock
* sets the current file position to the previous data block.
* this version achieves the task by restarting at the very begining
* and scanning forewards through the file;
* return 0 if not successful, e.g. the current position is already the
* very first
*
* this function is just an "object oriented" overhead and selects
* the proper GotoPrevBlock or GotoNextScan function from the szType
* entry within the DRDFILE structure
*/
int drdGotoPrevBlock(pFH)
DRDFILE *pFH;
{
int ret;
 
DPRINT("BEGIN drdGotoPrevBlock\n");
 
if (!strcmp(pFH->szType,ECHO_FILETYPE) )
        ret = echoGotoPrevScan(pFH);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
        ret = noiseGotoPrevScan(pFH);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
        ret = calGotoPrevScan(pFH);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
        ret = repGotoPrevScan(pFH);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
        ret = zeroGotoPrevBlock(pFH);
else {
    if (!bDrdDebug)
        fprintf(stderr,"Warning: cannot execute drdGotoPrevBlock for file type '%s'\n", pFH->szType);
    DPRINT1("drdGotoPrevBlock - Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret)
        DPRINT("END drdGotoPrevBlock\n")
else
        DPRINT("END drdGotoPrevBlock - an error occured\n")
return ret;
}

/**********************************************************************
* drdGetCurrentBlockLength
* returns the min. required buffersize to receive the echo data block
* by reading the header information of the current block
*
* this function is just an "object oriented" overhead and selects
* the proper GetCurrentBlockLength or GetCurrentScaneLength function from
* the szType entry within the DRDFILE structure
*/
ushort drdGetCurrentBlockLength(pFH)
DRDFILE *pFH;
{
int ret;
 
DPRINT("BEGIN drdGetCurrentBlockLength\n");
 
if (!strcmp(pFH->szType,ECHO_FILETYPE) )
        ret = echoGetCurrentScanLength(pFH);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
        ret = noiseGetCurrentScanLength(pFH);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
        ret = calGetCurrentScanLength(pFH);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
        ret = repGetCurrentScanLength(pFH);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
        ret = zeroGetCurrentBlockLength(pFH);
else {
    if (!bDrdDebug)
        fprintf(stderr,"Warning: cannot execute drdGetCurrentBlockLength for file type '%s'\n", pFH->szType);
    DPRINT1("drdGetCurrentBlockLength - Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret)
        DPRINT("END drdGetCurrentBlockLength\n")
else
        DPRINT("END drdGetCurrentBlockLength - an error occured\n")
return ret;
}

/**********************************************************************
* drdGetCurrentBlock
*
* this function is just an "object oriented" overhead and selects
* the proper GetCurrentBlockLength or GetCurrentScaneLength function from
* the szType entry within the DRDFILE structure
*/
int drdGetCurrentBlock(pFH,pluImScNo,pBuf)
DRDFILE *pFH;
ulong *pluImScNo;
char *pBuf;
{
int ret;
 
DPRINT("BEGIN drdGetCurrentBlock\n");
 
if (!strcmp(pFH->szType,ECHO_FILETYPE) )
        ret = echoGetCurrentScan(pFH,pluImScNo,pBuf);
else if (!strcmp(pFH->szType,NOISE_FILETYPE) )
        ret = noiseGetCurrentScan(pFH,pluImScNo,pBuf);
else if (!strcmp(pFH->szType,CAL_FILETYPE) )
        ret = calGetCurrentScan(pFH,pluImScNo,pBuf);
else if (!strcmp(pFH->szType,REP_FILETYPE) )
        ret = repGetCurrentScan(pFH,pluImScNo,pBuf);
else if (!strcmp(pFH->szType,ZERO_FILETYPE) )
        ret = zeroGetCurrentBlock(pFH,pluImScNo,pBuf);
else {
    if(!bDrdDebug)
	fprintf(stderr,"Warning: cannot execute drdGetCurrentBlock for file type'%s'\n",pFH->szType);
    DPRINT1("drdGetCurrentBlock - Invalid file type '%s'\n",pFH->szType);
    ret = 0;
    }
if (ret)
        DPRINT("END drdGetCurrentBlock\n")
else
        DPRINT("END drdGetCurrentBlock - an error occured\n")
return ret;
}

/**********************************************************************
* drdOpenInternal
* openes the files, reads the headers and allocates a DRDFILE structure.
* if it failes, it returns NULL. This function shall be only used by
* the ...Open functions. It compares the file type with pszType and fails
* if they are not the same!
*/
DRDFILE *drdOpenInternal(pszFileRoot, pszExt, pszType)
char *pszFileRoot,*pszExt,*pszType;
{
DRDFILE *pFile;
 
DPRINT("BEGIN drdOpenInternal\n");
/* generate DRDFILE structure: */
pFile = drdAllocDRDFILE();
if (NULL == pFile) {
        DPRINT("END drdOpenInternal - Failed allocating DRDFILE structure\n");
        return NULL;
        }
 
/* generating name for file */
pFile->pszFileName = malloc( strlen(pszFileRoot) + strlen(pszExt) + 1);
if (NULL == pFile->pszFileName) {
        DPRINT("END drdOpenInternal - Failed allocating memory for filename\n");
        drdFreeDRDFILE(pFile);
        return NULL;
        }
strcpy(pFile->pszFileName,pszFileRoot);
strcat(pFile->pszFileName,pszExt);
DPRINT1("File to open is %s\n",pFile->pszFileName);
 
pFile->file = fopen(pFile->pszFileName,"r");
 
/* if something was wrong until here error! */
if (NULL == pFile->file) {
        DPRINT("END drdOpenInternal - Failed opening file");
        drdFreeDRDFILE(pFile);
        return NULL;
        }
DPRINT("File successfuly opened\n");
 
/* mark as read only */
pFile->bRead = 1;
/* now read the header */
if (!drdReadHeaderInternal(pFile)) {
        DPRINT("Failed reading file Header\n");
        fclose (pFile->file);
        drdFreeDRDFILE(pFile);
        DPRINT("END drdOpenInternal - Failed reading file Header\n");
        return NULL;
        }
/* is it realy the right file type */
if (strcmp(pFile->szType,pszType) ) {
        fclose (pFile->file);
        drdFreeDRDFILE(pFile);
        DPRINT("END drdOpenInternal - Wrong Filetype\n");
        return NULL;
        }
pFile->bFilePosValid = 0;
DPRINT("END drdOpenInternal\n");
return pFile;
}


/**********************************************************************
* drdCreateInternal
* creates a new file of the specified type and writes its header.
* As Argument it expects an allocated DRDFILE structure with already
* initialized members pszFileName, bStdOut, szType, iTypeMajorVer,
*	iTypeMinorVer  
* The members file,bRead,lnHeaderLength, szDate, pszProgram,
*	iProgMajorVer, iProgMinorVer, pszFacility, apszComments 
*	pszSatellite,pszID
* will be filled by this function.
* the Program name and version and the facility are new set from the
* values set in the Init file if they are not yet initialized
* the comments given in apszComments will be appended to the standard comment
* the function returns 0 if an error occures
*/
int drdCreateInternal(pFH,pszID,pszSat,apszStdComments,apszComments)
DRDFILE *pFH;
char *pszID,*pszSat;
char **apszStdComments;
char **apszComments;
{
int count,count1;
time_t CurTime;

DPRINT4("BEGIN drdCreateInternal for file %s with type %s V%d.%d\n",
	pFH->pszFileName,pFH->szType,pFH->iTypeMajorVer,pFH->iTypeMinorVer);
RETURN_IF_NOT_INIT("drdCreateInternal");

if (NULL == pFH) {
	DPRINT("Parameter pFH is NULL and therefore not valid\n");
	return 0;
	}

/* first open the new file */
pFH->file = fopen(pFH->pszFileName,"w");
if (NULL == pFH->file) {
	DPRINT("END drdCreateInternal - failure opening file for writing\n");	
	return 0;
	}
DPRINT("Opened file for writing\n");

/* comments */
if (NULL == apszStdComments) {
	DPRINT("END drdCreateInternal - missing standard comments\n");
	return 0;
	}
/* counting comment lines */
for (count = 0; NULL != apszStdComments[count]; count++);
if (NULL != apszComments)
	for (count1 = 0;NULL!=apszComments[count1]; count1++,count++);
/* allocating memory for comments */
pFH->apszComments = (char **)calloc(count + 1, sizeof(char *) );
if (NULL == pFH->apszComments) {
	DPRINT("END drdCreateInternal - Failed allocating memory for comments\n");
	return 0;
	}
/* copying standard comments */
for (count = 0; NULL != apszStdComments[count]; count++) {
	pFH->apszComments[count] = malloc(strlen(apszStdComments[count])+1);
	if (NULL == pFH->apszComments[count]) {
		DPRINT("END drdCreateInternal - Failed allocating mem. for comments\n");
		return 0;
		}
	strcpy(pFH->apszComments[count],apszStdComments[count]);
	}
if (NULL != apszComments) {
    for (count1 = 0; NULL != apszComments[count1]; count1++, count++) {
	pFH->apszComments[count] = malloc(strlen(apszComments[count1])+1);
	if (NULL == pFH->apszComments[count]) {
		DPRINT("END drdCreateInternal - Failed allocating mem. for comments\n");
		return 0;
		}
	strcpy(pFH->apszComments[count],apszComments[count1]);
	}
    }
pFH->apszComments[count] = NULL;
DPRINT("Comments processed\n");

/* program and facility names */
if (NULL == pFH->pszProgram ) { /* Initialize if necessary */
    pFH->pszProgram = malloc(strlen(drdpszProgram) + 1);
    if (NULL == pFH->pszProgram) {
        DPRINT("END drdCreateInternal - failure allocating memory for Program name\n");
	return 0;
	}
    strcpy(pFH->pszProgram,drdpszProgram);

    /* setting Program version */
    pFH->iProgMajorVer = drdiProgMajorVer;
    pFH->iProgMinorVer = drdiProgMinorVer;
    }

DPRINT3("Program name and version are %s V%d.%d\n",
	drdpszProgram,drdiProgMajorVer,drdiProgMinorVer);

if (NULL == pFH->pszFacility ) { /* Initialize if necessary */
    pFH->pszFacility = malloc(strlen(drdpszFacility) + 1);
    if (NULL == pFH->pszFacility) {
        DPRINT("END drdCreateInternal - failure allocating memory for Facility name\n");
	return 0;
	}
    strcpy(pFH->pszFacility,drdpszFacility);
    }
DPRINT1("Facility name is %s\n",drdpszFacility);

/* date */
if ( !strlen(pFH->szDate)) {
    CurTime = time(NULL);
    strftime(pFH->szDate,19,"%D",localtime(&CurTime));
    }
DPRINT1("Creation date is %s\n",pFH->szDate);

/* satellite */
pFH->pszSatellite = malloc(strlen(pszSat) + 1);
if (NULL == pFH->pszSatellite) {
	DPRINT("END drdCreateInternal -failure allocating memory for Satellite name\n");
        return 0;
	}
strcpy(pFH->pszSatellite,pszSat);
DPRINT1("Satellite name set to %s\n",pszSat);
 
/* pszID */
pFH->pszID = malloc(strlen(pszID) + 1);
if (NULL == pFH->pszID) {
        DPRINT("END drdCreateInternal -failure allocating memory for ID name\n");
        return 0;
	}
strcpy(pFH->pszID,pszID);
DPRINT1("ID set to %s\n",pszID);

/* Read mode */
pFH->bRead = 0;

/* now write the header */
drdWriteHeaderInternal(pFH);

DPRINT("END drdCreateInternal\n")
return -1;
}

/***********************************************************************
* drdCopyFile
* creates a new file and copies the old file data into the new file
* using the given new header information. All other header information
* is copied from the DRDFILE structure of the old file.
*
* If the name of the old file is the same as the name of the new file,
* an error occures.
*
* returns 2 if file is already opened or accessed by another program
*         1 if another error occures
*         0 if everything is OK
*/
#define COPYBUFFERSIZE 200000
int 
drdCopyFile(pSrc, pszDestName, pszDate, pszProgram, iLoVer, iHiVer,
		pszFacility, pszID, apszComments)
DRDFILE *pSrc;
char *pszDestName, *pszDate, *pszProgram, *pszFacility, *pszID;
int iLoVer,iHiVer;
char **apszComments;
{
DRDFILE *pOut;
char *pBuffer;
int nRead;

DPRINT("BEGIN drdCopyFile\n");
RETURN_IF_NOT_INIT("drdCopyFile");
RETURN_IF_DRDFILE_NOT_VALID("drdCopyFile",pSrc);
RETURN_IF_WRITE("drdCopyFile",pSrc);

if ( !strcmp(pszDestName,pSrc->pszFileName) ) {
	DPRINT("END drdCopyFile - source and destination file names equal\n");
	return 1;
	}

/* a new DRDFILE structure */
pOut = drdAllocDRDFILE();
if (NULL == pOut) {
        DPRINT("END drdCopyFile - error allocating DRDFILE structure\n");
        return 1;
        }

/* filling the structure */
/* file name */
pOut->pszFileName = malloc(strlen(pszDestName) + 1);
if (NULL == pOut->pszFileName) {
	drdFreeDRDFILE(pOut);
	DPRINT("END drdCopyFile - error allocating memory for filename\n");
	return 1;
	}
strcpy(pOut->pszFileName, pszDestName);

/* Type */
strcpy(pOut->szType, pSrc->szType);
pOut->iTypeMajorVer = pSrc->iTypeMajorVer;
pOut->iTypeMinorVer = pSrc->iTypeMinorVer;

/* Date */
if (NULL == pszDate) pszDate = pSrc->szDate;
if (strlen(pszDate) > sizeof(pOut->szDate) ) {
	drdFreeDRDFILE(pOut);
	DPRINT("END drdCopyFile - date string too long\n");
	return 1;
	}
strcpy(pOut->szDate,pszDate);

/* Program data */
if (NULL == pszProgram) {
	pszProgram = pSrc->pszProgram;
	pOut->iProgMinorVer = pSrc->iProgMinorVer;
	pOut->iProgMajorVer = pSrc->iProgMajorVer;
	}
else {
	pOut->iProgMinorVer = iLoVer;
	pOut->iProgMajorVer = iHiVer;
	}
pOut->pszProgram = malloc(strlen(pszProgram) + 1);
if (NULL == pOut->pszProgram) {
	drdFreeDRDFILE(pOut);
	DPRINT("END drdCopyFile - error allocating memory for program name\n");
	return 1;
	}
strcpy(pOut->pszProgram,pszProgram);

/* facility */
if (NULL == pszFacility) pszFacility = pSrc->pszFacility;
pOut->pszFacility = malloc(strlen(pszFacility) + 1);
if (NULL == pOut->pszFacility) {
	drdFreeDRDFILE(pOut);
	DPRINT("END drdCopyFile - error allocating memory for facility name\n");
	return 1;
	}
strcpy(pOut->pszFacility,pszFacility);

/* ID */
if (NULL == pszID) 
	pszID = pSrc->pszID;

/* comments */
if (NULL == apszComments) 
	apszComments = pOut->apszComments;

/* allocate copy buffer */
pBuffer = malloc(COPYBUFFERSIZE);
if (NULL == pBuffer) {
	drdFreeDRDFILE(pOut);
	DPRINT("END drdCopyFile - cannot allocate copy buffer\n");
	return 1;
	}

/* now create the new file */
if ( !drdCreateInternal(pOut,pszID,pSrc->pszSatellite,apszComments,NULL) ) {
	drdFreeDRDFILE(pOut);
	free(pBuffer);
	DPRINT("END drdCopyFile - error creating file\n");
	return 1;
	}

/* try to lock input files for all others access to prevent data jam */
/*
if (lockf(fileno(pSrc->file),F_LOCK,0)) {
	free(pBuffer);
	drdCloseInternal(pOut);
	DPRINT("END drdCopyFile - cannot lock output file\n");
	return 2;
	}
*/

/* copy */
fseek(pSrc->file,pSrc->lnHeaderLength,0);

do {
   nRead = fread(pBuffer,sizeof(char),COPYBUFFERSIZE,pSrc->file);
   fwrite(pBuffer,nRead,sizeof(char),pOut->file);
   } while (nRead == COPYBUFFERSIZE);

/* close output and unlock input */
pSrc->bFilePosValid = 0;
/* lockf(fileno(pSrc->file),F_ULOCK,0); */
drdCloseInternal(pOut);

free(pBuffer);

DPRINT("END drdCopyFile\n");
return 0;
}

/**********************************************************************
* drdWriteHeaderInternal
* writes a header to a new created file. This function is only for
* internal use of the ...Create functions.
* after writing the header it sets the entry for the length of the header
*/
int drdWriteHeaderInternal(pFH)
DRDFILE *pFH;
{
int count;

DPRINT("BEGIN drdWriteHeaderInternal\n");
RETURN_IF_NOT_INIT("drdWriteHeaderInternal");
RETURN_IF_DRDFILE_NOT_VALID("drdWriteHeaderInternal",pFH);
RETURN_IF_READ("drdWriteHeaderInternal",pFH);
if (NULL == pFH->file) { /* the file has to be opened */
	DPRINT("END drdWriteHeaderInternal - file has to be opened\n");
	return 0;
	}
if (0 != ftell(pFH->file)) { /* the file should be empty */
	DPRINT("END drdWriteHeaderInternal - file is not empty\n");
	return 0;
	}

/* line 1 - filetype */
fprintf(pFH->file,"Type %s V%d.%d\n",pFH->szType,pFH->iTypeMajorVer,
						pFH->iTypeMinorVer);
DPRINT3("Filetype written: %s V%d.%d\n",pFH->szType,pFH->iTypeMajorVer,
                                                pFH->iTypeMinorVer);
 
/* line 2 - date of creation  */
fprintf(pFH->file,"Created %s\n",pFH->szDate);
DPRINT1("Creation date written: %s\n",pFH->szDate);

/* line 3 - program name */
if (NULL == pFH->pszProgram)
	return 0;
fprintf(pFH->file,"Program %s V%d.%d\n",pFH->pszProgram,pFH->iProgMajorVer,
					pFH->iProgMinorVer );
DPRINT3("Program name and version written: %s V%d.%d\n",
	pFH->pszProgram,pFH->iProgMajorVer,pFH->iProgMinorVer );

/* line 4 - satellite */
if (NULL == pFH->pszSatellite)
	return 0;
fprintf(pFH->file,"Satellite %s\n",pFH->pszSatellite);
DPRINT1("Satellite name written: %s\n",pFH->pszSatellite);

/* line 5 - facility */
if (NULL == pFH->pszFacility)
	return 0;
fprintf(pFH->file,"%s\n",pFH->pszFacility);
DPRINT1("Facility name written: %s\n",pFH->pszFacility);

/* line 6 - id */
if (NULL == pFH->pszID)
	return 0;
fprintf(pFH->file,"ID %s\n",pFH->pszID);
DPRINT1("ID written: %s\n",pFH->pszID);

/* line 7... comments */
for (count = 0; NULL != pFH->apszComments[count]; count++) {
	fprintf(pFH->file,"%s\n",pFH->apszComments[count]);
	}
DPRINT("Comments written\n");

/* last line: <HJW> */
fprintf(pFH->file,"<HJW>\n");


/* length of header */
pFH->lnHeaderLength = ftell(pFH->file);
DPRINT("END drdWriteHeaderInternal\n");
return -1;
}

/**********************************************************************
* drdAllocDRDFILE
* allocates and initializes the memory for a new DRDFILE structure.
*/
DRDFILE *drdAllocDRDFILE()
{
DRDFILE *tmp;
DPRINT("BEGIN drdAllocDRDFILE\n");
tmp = (DRDFILE *)malloc(sizeof(DRDFILE));
if (NULL != tmp) {
	tmp->file = NULL;
	tmp->pszProgram = NULL;
	tmp->pszSatellite = NULL;
	tmp->pszFacility = NULL;
	tmp->pszID = NULL;
	tmp->apszComments = NULL;
	tmp->bFilePosValid = 0;
	tmp->luImageScanNo = 0L;
	tmp->luBlockLength = 0L;
	tmp->szDate[0] = '\0';
	tmp->szType[0] = '\0';
	}
DPRINT("END drdAllocDRDFILE\n");
return tmp;
}

/**********************************************************************
* drdFreeDRDFILE
* frees all the memory occupied by the DRDFILE structure. It frees also
* the memory, pointed by the pointers within the structure.
* do not access the DRDFILE structure after calling this function!
*/
void drdFreeDRDFILE(p)
DRDFILE *p;
{
int count;

DPRINT("BEGIN drdFreeDRDFILE\n"); 
if (NULL == p)
	return;

if (NULL != p->pszProgram)
	free(p->pszProgram);
if (NULL != p->pszSatellite)
	free(p->pszSatellite);
if (NULL != p->pszFacility)
	free(p->pszFacility);
if (NULL != p->pszID)
	free(p->pszID);
if (NULL != p->apszComments) {
	for (count=0; NULL != p->apszComments[count]; count++) {
		free(p->apszComments[count]);
		}
	free(p->apszComments);
	}
free (p);
DPRINT("END drdFreeDRDFILE\n");
}

/**********************************************************************
* drdSplitOffKeyWord
* tests the keyword at the begining of the line until to the first
* space. returns a pointer to the character, that followes the space.
* Returns NULL if the keyword does not fit or if an error occures
*/
char *drdSplitOffKeyWord(szSource,szKeyword)
char *szSource,*szKeyword;
{
char *pszReturn;

pszReturn = strtok(szSource," ");
if (strcmp(pszReturn,szKeyword) )
	pszReturn = NULL;
else
	pszReturn = strtok(NULL,"");
return pszReturn;
}

/*********************************************************************
* drdSplitOffVersion splits of the version from a string with the 
* following format:
* somestring Vmajor.minor
* It replaces the V with a null character and reads the major and minor
* It removes all blank characters in front of the "V"
* It returns 0 if an Error occures
*/
int drdSplitOffVersion(pszString, piMajor, piMinor)
char *pszString;
int *piMajor,*piMinor;
{
char *pV;
pV = strrchr(pszString, 'V');
if (NULL == pV) 
	return 0;
/* removeing trailing blank characters of reststring */
for(pV--;isspace(*pV); pV--);
pV++;
(*(pV++)) = '\0';

/* reading version number; */
sscanf(pV," V%d.%d",piMajor,piMinor);
return -1;
}


/**********************************************************************
* drdCloseInternal 
* closes the file, deleted the DRDFILE structure.
* returns 0 if an error occures
*/
int drdCloseInternal(pFH)
DRDFILE *pFH;
{
DPRINT("BEGIN drdCloseInternal\n");
if (NULL == pFH) {
	DPRINT("END drdCloseInternal - NULL pointer as argument\n")
	return 0;
	}
if (pFH->file != NULL) {
	fclose(pFH->file); 
	}
DPRINT("File closed\n");
drdFreeDRDFILE(pFH);
DPRINT("END drdCloseInternal\n");
return 1;
}

/***********************************************************************
* drdReadHeaderInternal
* reads the header of a drd file and fills the DRDFILE structure. 
* This function is only for internal use by the ...Open functions
* before calling this function the file should be already opened by
* a fopen call.
* Allocated memory will be freed within the drdFreeDRDFILE routine, even
* though an error occured!
*/
int drdReadHeaderInternal(pFH)
DRDFILE *pFH;
{
char buffer[HEADER_LINE_LENGTH + 1];
char *pszEntry;
int count;
long lCommentOffset;

DPRINT("BEGIN drdReadHeaderInternal\n");
/* the file has to be already open */
if (NULL == pFH || NULL == pFH->file) {
	DPRINT("END drdReadHeaderInternal - File not opened\n");
	return 0;
	}

rewind(pFH->file);	/* Reset file to the very begining */
pFH->bFilePosValid = 0;

/* processing line 1 - filetype */
if (NULL == fgets(buffer,HEADER_LINE_LENGTH + 1,pFH->file) ) {
	DPRINT("END drdReadHeaderInternal - error reading file\n");
	return 0;
	}
if (strlen(buffer) > 1) /* NewLine entfernen */
        buffer[strlen(buffer)-1] = '\0';
if (!drdSplitOffVersion(buffer, &(pFH->iTypeMajorVer), &(pFH->iTypeMinorVer))){
	DPRINT("END drdReadHeaderInternal - error decoding Version\n");
	return 0;
	}
pszEntry = drdSplitOffKeyWord(buffer,"Type"); 
if (NULL == pszEntry ) {
	DPRINT("END drdReadHeaderInternal - error decoding key word Type\n");
	return 0;
	}
if ( strlen(pszEntry) >= sizeof(pFH->szType) - 1) {
	DPRINT("END drdReadHeaderInternal - wrong file type - too long\n");
	return 0;
	}
strcpy( pFH->szType, pszEntry );
DPRINT1("Filtype read as %s\n",pFH->szType);

/* processing line 2 - Creation date */
if (NULL == fgets(buffer,HEADER_LINE_LENGTH + 1,pFH->file) ){
        DPRINT("END drdReadHeaderInternal - error reading file\n");
	return 0;
	}
if (strlen(buffer) > 1) /* NewLine entfernen */
        buffer[strlen(buffer)-1] = '\0';
pszEntry = drdSplitOffKeyWord(buffer,"Created");
if (NULL == pszEntry) {
        DPRINT("END drdReadHeaderInternal - error decoding key word `Created`\n");
	return 0;
	}
if (strlen (pszEntry) >= sizeof(pFH->szDate) ) {
        DPRINT("END drdReadHeaderInternal - invalid date entry - too long\n");
	return 0;
	}
strcpy( pFH->szDate, pszEntry);
DPRINT1("Creation date read: %s\n",pFH->szDate);

/* processinge line 3 - program name */
if (NULL == fgets(buffer,HEADER_LINE_LENGTH +1,pFH->file) ) {
        DPRINT("END drdReadHeaderInternal - error reading file\n");
	return 0;
	}
if (strlen(buffer) > 1) /* NewLine entfernen */
        buffer[strlen(buffer)-1] = '\0';
if (!drdSplitOffVersion(buffer, &(pFH->iProgMajorVer), &(pFH->iProgMinorVer))){
	DPRINT("END drdReadHeaderInternal - error decoding Program version\n");
	return 0;
	}
pszEntry = drdSplitOffKeyWord(buffer,"Program");
if (NULL == pszEntry) {
        DPRINT("END drdReadHeaderInternal - error decoding key word `Program`\n");
	return 0;
	}
pFH->pszProgram = malloc( strlen(pszEntry) + 1 );
if (NULL == pFH->pszProgram) {
	DPRINT("END drdReadHeaderInternal -failure allocating memory for prog name\n");
	return 0;
	}
strcpy(pFH->pszProgram,pszEntry);
DPRINT3("Program name and version read: %s V%d.%d\n",pFH->pszProgram,
	pFH->iProgMajorVer, pFH->iProgMinorVer);

/* procesing line 4 - satellite name */
if (NULL == fgets(buffer,HEADER_LINE_LENGTH +1, pFH->file) ) {
        DPRINT("END drdReadHeaderInternal - error reading file\n");
	return 0;
	}
if (strlen(buffer) > 1) /* NewLine entfernen */
        buffer[strlen(buffer)-1] = '\0';
pszEntry = drdSplitOffKeyWord(buffer,"Satellite");
if (NULL == pszEntry) {
        DPRINT("END drdReadHeaderInternal - error decoding key word `Satellite`\n");
	return 0;
	}
pFH->pszSatellite = malloc( strlen(pszEntry) + 1 );
if (NULL == pFH->pszSatellite) {
        DPRINT("END drdReadHeaderInternal -failure allocating memory for Satellite\n");
	return 0;
	}
strcpy(pFH->pszSatellite,pszEntry);
DPRINT1("Satellite name read: %s\n",pFH->pszSatellite);

/* processing line 5 - Facility */
if (NULL == fgets(buffer,HEADER_LINE_LENGTH + 1, pFH->file) ) {
        DPRINT("END drdReadHeaderInternal - error reading file\n");
	return 0;
	}
if (strlen(buffer) > 1) /* remove NewLine */
        buffer[strlen(buffer)-1] = '\0';
pFH->pszFacility = malloc( strlen(buffer) + 1 );
if (NULL == pFH->pszFacility) {
        DPRINT("END drdReadHeaderInternal -failure allocating memory for Facility\n");
	return 0;
	}
strcpy(pFH->pszFacility,buffer);
DPRINT1("Facility name read: %s\n",pFH->pszFacility);

/* processing line 6 - ID */
if (NULL == fgets(buffer,HEADER_LINE_LENGTH + 1, pFH->file) ) {
        DPRINT("END drdReadHeaderInternal - error reading file\n");
	return 0;
	}
if (strlen(buffer) > 1) /* NewLine entfernen */
	buffer[strlen(buffer)-1] = '\0';

pszEntry = drdSplitOffKeyWord(buffer,"ID");
if (NULL == pszEntry) {
        DPRINT("END drdReadHeaderInternal - error decoding key word `ID`\n");
	return 0;
	}
pFH->pszID = malloc( strlen(pszEntry) + 1);
if (NULL == pFH->pszID) {
        DPRINT("END drdReadHeaderInternal -failure allocating memory for ID\n");
	return 0;
	}
strcpy(pFH->pszID,pszEntry);
DPRINT1("ID read: %s\n",pFH->pszID);

/* processing comments */
lCommentOffset = ftell(pFH->file);
/* first counting comments */
for( count = 0; ; count++ ) {
	if (NULL == fgets (buffer, HEADER_LINE_LENGTH + 1, pFH->file) )  {
        	DPRINT("END drdReadHeaderInternal - error reading file\n");
		return 0;
		}
	if ( !strcmp(buffer,"<HJW>\n") )
		break;
	}
/* headerlength is available! */
pFH->lnHeaderLength = ftell(pFH->file);
/* place for the comments array */
pFH->apszComments = (char **)calloc (count + 1, sizeof(char *) );

/* and now read the comments */
fseek(pFH->file,lCommentOffset,0);
for(count = 0; ; count++) {
	if (NULL == fgets(buffer,HEADER_LINE_LENGTH +1, pFH->file) )  {
                DPRINT("END drdReadHeaderInternal - error reading file\n");
		return 0;
		}
	if (strlen(buffer) > 1) /* NewLine entfernen */
		buffer[strlen(buffer)-1] = '\0';
	if ( !strcmp("<HJW>",buffer) ) 
		break;
	pFH->apszComments[count] = malloc(strlen(buffer) + 1);
	if (NULL == pFH->apszComments[count]) {
	    DPRINT1("END drdReadHeaderInternal - failure allocating memory comm. %d\n",
			count);
	    return 0;
	    }
	strcpy(pFH->apszComments[count], buffer);
	}
pFH->apszComments[count] = NULL;
DPRINT1("Read %d comment lines\n",count);
/* setting the file offset to the beginning of the data section */
fseek(pFH->file,pFH->lnHeaderLength,0);
DPRINT("END drdReadHader\n");
return -1;
}

