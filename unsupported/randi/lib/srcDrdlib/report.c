/* report.c
* this file includes the DRD library functions for the report file
* release 1.0 07-11-1994
*/
 
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <ctype.h>
#include <varargs.h>
#include "drd.h"
 
#define FILETYPE 	REPORT_FILETYPE
#define FILEEXT 	REPORT_FILEEXT

/** default comment */
char *apszReportDefaultComment[] = {
        "This file includes a report from the decoding process of the raw data",
        NULL
        };



/* reportCreate
* creates a new report file and writes its header
* if the file already exists it is overwritten!
* If no comment is given, i.e. apszComments is NULL, a standard comment
* is generated 
* returns af pointer to a DRDFILE structure if successful, otherwise NULL
*/
DRDFILE *reportCreate(pszFileRoot,pszID,pszSatellite,apszComments,bStdOut)
char *pszFileRoot,*pszID,*pszSatellite;
int bStdOut;
char **apszComments;
{
DRDFILE *pFH;

DPRINT("BEGIN reportCreate\n");
RETURN_IF_NOT_INIT("reportCreate");
/* parameter testing */
if (NULL == pszID) {
        DPRINT("END reportCreate - parameter pszID is NULL\n");
        return NULL;
        }
if (NULL == pszFileRoot)  {
        DPRINT("END reportCreate - parameter pszFileRoot is NULL\n");
        return NULL;
        }
if (NULL == pszSatellite) {
        DPRINT("END reportCreate - parameter pszSatellite is NULL\n");
        return NULL; 
        }
 
/* first a new DRDFILE structure */
pFH = drdAllocDRDFILE();
if (NULL == pFH) {
        DPRINT("END reportCreate - error allocating DRDFILE structure\n");
        return NULL;
        }
/* creating file name */
pFH->pszFileName = malloc(strlen(pszFileRoot) + strlen(FILEEXT) + 1);
if (NULL == pFH->pszFileName) {
        drdFreeDRDFILE(pFH);
        DPRINT("END reportCreate - error allocating memory for file name\n");
        return NULL;
        }
strcpy(pFH->pszFileName,pszFileRoot);
strcat(pFH->pszFileName,FILEEXT);
DPRINT1("Filename is %s\n",pFH->pszFileName); 
 
/* file type */
strcpy(pFH->szType,FILETYPE);
 
/* Format version */
pFH->iTypeMajorVer = REPORT_MAJOR_VERSION;
pFH->iTypeMinorVer = REPORT_MINOR_VERSION;
DPRINT3("Type is %s V%d.%d\n",pFH->szType,pFH->iTypeMajorVer,
                pFH->iTypeMinorVer);

/* auch nach StdOut? */
pFH->bStdOut = bStdOut;

/* now create */
DPRINT("Trying to create file\n");
if (!drdCreateInternal(pFH,pszID,pszSatellite,apszReportDefaultComment,apszComments)) {
        drdFreeDRDFILE(pFH);
        DPRINT("END reportCreate - file creation failed\n");
        return NULL;
        }
DPRINT("END reportCreate\n");
return pFH;
}

/**********************************************************************
* reportClose
* this function closes the report file and destroys the DRDFILE 
* structure
* return 0 if an error occures
*/
int reportClose(pFH)
DRDFILE *pFH;
{
int ret;
DPRINT("BEGIN reportClose\n");
RETURN_IF_DRDFILE_NOT_VALID("reportClose",pFH);
RETURN_IF_WRONG_FILE_TYPE("reportClose",FILETYPE,pFH);
ret = drdCloseInternal(pFH);
DPRINT("END reportClose\n");
return ret;
}

/**********************************************************************
* reportWrite
* writes a line into the report file, adding a newline. The parameters
* are similar to the fsprintf function. The first parameter is a pointer
* to a DRDFILE structure, the second the format string, followed by
* additional arguments
* if the bStdOut flag in the DRD structure is set (during opening the file)
* the string is written to stdout too
* returns 0 if an error occures
*/
int reportWrite(va_alist)
va_dcl
{
va_list args;
DRDFILE *pFH;
char *format;

DPRINT("BEGIN reportWriteLine\n");

va_start(args);
pFH = va_arg(args,DRDFILE *);
format = va_arg(args,char *);

RETURN_IF_DRDFILE_NOT_VALID("reportWriteLine",pFH);
RETURN_IF_WRONG_FILE_TYPE("reportWriteLine",FILETYPE,pFH);
RETURN_IF_READ("reportWriteLine",pFH);

reportVWrite(pFH,format,args);

va_end(args);

DPRINT("END reportWriteLine\n");
return 1;
}

/* the same as above, with variable arguments
* does the main work!!
* reportvWrite(pFH,format,args)
*/

int reportVWrite(pFH,format,args)
DRDFILE *pFH;
char *format;
va_list args;
{
DPRINT("BEGIN reportVWriteLine\n");
RETURN_IF_DRDFILE_NOT_VALID("reportWriteLine",pFH);
RETURN_IF_WRONG_FILE_TYPE("reportWriteLine",FILETYPE,pFH);
RETURN_IF_READ("reportWriteLine",pFH);

vfprintf(pFH->file,format,args);
fputc('\n',pFH->file);
 
if (pFH->bStdOut) {
        printf("REPORT: ");
        vprintf(format,args);
        putchar('\n');
        }

DPRINT("END reportVWriteLine\n");
return 1;
}



