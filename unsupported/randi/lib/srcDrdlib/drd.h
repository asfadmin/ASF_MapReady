/* drd.h
 * header for internal used functions for decoded raw data file access library
 * contains the declarations of the internal used functions and variables.
 *
 * first setup 06-22-1994 Hans-Joerg Wagner
 * release 1.0 07-12-1994 HJW
 */

#include <stdio.h>
#include <string.h>
#include "drdapi.h"

/**********************************************************************
* prototypes of functins used for every file type
* implemented ind drd.c
* NOTE: These functions are created for internal library use only!!!
*/
DRDFILE *drdOpenInternal(/*char *pszRootFile,char *pszFileExt,char *pszType*/);
int drdCloseInternal(/*DRDFILE *pFH*/);
int drdCreateInternal(/*DRDFILE *pFH,char *, char *,char **,char **apszComment*/);
int drdReadHeaderInternal(/*DRDFILE *pFH*/);        /* reads header */
int drdWriteHeaderInternal(/*DRDFILE *pFH*/);       /* writes header */
DRDFILE *drdAllocDRDFILE();             /* allocates DRDFILE structure */
void drdFreeDRDFILE(/*DRDFILE *p*/);        /* frees DRDFILE structure */
char *drdSplitOffKeyWord(/*char *szSource, char *szKeyword*/);
int drdSplitOffVersion(/*char *pszString, int *, int * */);

/**********************************************************************
* global variables for use within the library
*/
extern char *drdpszProgram;
extern int drdiProgMajorVer;
extern int drdiProgMinorVer;
extern char *drdpszFacility;

/**********************************************************************
* internal data types
*/
struct ECHO_DB_HEAD {
	ulong luImageScanNo;
	ushort uLength;
	};
struct NOISE_DB_HEAD {
	ulong luImageScanNo;
	ushort uLength;
	};
struct CAL_DB_HEAD {
	ulong luImageScanNo;
	ushort uPulseNo;
	ushort uLength;
	};
struct REP_DB_HEAD {
	ulong luImageScanNo;
	ushort uLength;
	};
struct ZERO_DB_HEAD {
	ulong luImageScanNo;
	ulong luLength;
	};

#define AUX_VAL_STRING_LENGTH 100
/**********************************************************************
* definitions for all files
*/

/* DPRINT
* prints the given string to stdio if bDrdDebug flag is set
*/
#define DPRINT(x)		{ if (bDrdDebug) printf(x); }
#define DPRINT1(x,x1)   	{ if (bDrdDebug) printf(x,x1); }
#define DPRINT2(x,x1,x2)        { if (bDrdDebug) printf(x,x1,x2); }
#define DPRINT3(x,x1,x2,x3)	{ if (bDrdDebug) printf(x,x1,x2,x3); }
#define DPRINT4(x,x1,x2,x3,x4)	{ if (bDrdDebug) printf(x,x1,x2,x3,x4); }


/* PRINT_WRONG_FILE_TYPE prints an error message to stderr, if a function
* is used with the wrong filtype
*/
#define PRINT_WRONG_FILE_TYPE(func,typex) \
    DPRINT2("END %s - function cannot be used for file type '%s' !!\n",\
				func,typex);
/* RETURN_IF_WRONG_FILE_TYPE compares the type given in type with
* the type of the file given with drdFileStruct. If the filetype strings
* are not equal, it creates an error message to stderr and a return statement
* that returns 0
*/
#define RETURN_IF_WRONG_FILE_TYPE(func,type,drdFileStruct) \
	if ( strcmp(type,drdFileStruct->szType) ) { \
		PRINT_WRONG_FILE_TYPE(func,drdFileStruct->szType); \
		return 0; \
		}
/* RETURN_IF_DRDFILE_NOT_VALID
* creates an error message and a return statement if the file parameter
* is NULL
*/
#define RETURN_IF_DRDFILE_NOT_VALID(func,file) \
	if (NULL == file) { \
		DPRINT1("END %s - error: parameter file is NULL\n", func); \
		return 0; \
		}
/* RETURN_IF_READ
* creates an error message and a return statement if the file is in
* read mode
*/
#define RETURN_IF_READ(func,file) \
	if ( file->bRead ) { \
	   DPRINT2("END %s -function cannot be used in Read mode.File %s\n",\
		func, file->pszFileName);\
	   return 0; \
	   }

/* RETURN_IF_WRITE
* creates an error message and a return statment if the file is in write mode
*/
#define RETURN_IF_WRITE(func,file) \
	if ( !file->bRead ) { \
	   DPRINT2("END %s -function cannot be used in Write mode.File %s\n",\
		func, file->pszFileName);\
	   return 0; \
	   }

/* RETURN_IF_FILE_POS_INVALID
* creates an error message and a return statment if the file position is
* invalid.
*/
#define RETURN_IF_FILE_POS_INVALID(func,pFH) \
	if(!pFH->bFilePosValid) { \
	  DPRINT2("END 2 - current position in file %s not valid\n", \
		func, pFH->pszFileName); \
	  return 0; \
	  }
/* RETURN_IF_NOT_INIT
* creates an error message and return statment if the library is not 
* properly initialized, i.e. the drdInit function was not executed
* without eror
*/
#define RETURN_IF_NOT_INIT(func) \
	if (NULL == drdpszProgram || NULL == drdpszFacility) { \
	   DPRINT1("END %s - Library is not properly initialized\n",func);\
	   return 0; \
	   }
