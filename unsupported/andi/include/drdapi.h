/* drdapi.h
 * header for decoded raw data file access library
 * include this header to access the library functions of the drd file
 * access library. It defines the DRDFILE datatype and includes the 
 * function prototypes
 *
 * first setup 06-21-1994 Hans-Joerg Wagner
 * release 1.0 07-12-1994 HJW
 */

/* do not process header twice */
#ifndef __DRDAPI.H__
#define __DRDAPI.H__

#include <stdio.h>

/**********************************************************************
* shortcuts for integer datatypes
*/
#ifndef UNSIGNED_TYPES
#define UNSIGNED_TYPES

/* to come along with sarmacros.h */
#ifndef SARMACROS

#ifndef ulong
typedef unsigned long	ulong;
#endif

#ifndef __sys_types_h
#ifndef ushort
typedef unsigned short	ushort;
#endif

#ifndef uint
typedef unsigned int	uint;
#endif uint

#endif /* !__sys_types_h */
#ifndef uchar
typedef unsigned char	uchar;
#endif
#endif /*UNSIGNED_TYPES */
#endif /*SARMACROS */

/* datatype char complex */
typedef struct SCcomplex_tag {
        char I,Q;
        } SCcomplex;


/**********************************************************************
* definition of datalength etc.
*/
#define HEADER_LINE_LENGTH	80

/**********************************************************************
* DRDFILE datatype
* This datatype is provided for administration of the different files.
* One instance is new allocated by every ...Open and ...Create funciton
* of the library. The memory is freed with the ...Close funciton.
* One instance of this datatype holds the whole header information
* and the status of one file. Do not modify the contents of an established
* instance!
* For changes within the structure istself refer to the functions:
* drdReadHeader,drdWriteHeader,drdRmDRDFILE and ...Open of every file format
*/
typedef struct tagDRDFILE {
	FILE *file;		/* file handle returned by an fopen call */
	char *pszFileName;	/* file name */
	uint bRead:1;   	/* flag for read or write mode */
	uint bStdOut:1; 	/* output to stdout too - only report */
	uint bFilePosValid:1;	/* is current file pos valid - only read mode*/
	ulong luImageScanNo;	/* to assure ascend writing - only write mode*/
	ushort uNo;		/* additional Numbering-only for write in cal*/
	ulong luBlockLength;	/* block length, used for zero writing */
	long lnHeaderLength;	/* length of the header in byte. Only
				   used in read mode for faster access */
	char szType[15];	/* type of file */
	int iTypeMajorVer;	/* Major file format version of this file type*/
	int iTypeMinorVer;	/* Minor file format version of file type */
	char szDate[20];	/* Date of creation of file in verbose form */
	char *pszProgram;	/* name of program that generated the file */
	int iProgMajorVer;	/* Major version of the program */
	int iProgMinorVer;	/* Minor version of the program */
	char *pszSatellite;	/* Name of the satellite that produced data */
	char *pszFacility;	/* Facility that processed the data */
	char *pszID;		/* File ID - same for files belonging together*/
	char **apszComments;	/* Comments */
	} DRDFILE;

/*********************************************************************
* Initialization and exit functions. Thee drdInit function has to be called
* before the first call to any other library function. The function
* drdExit has to be called as last function before quitting the program.
*/
int drdInit( /* char *pszProgram , int iProgMajorVer, 
		int iProgMinorVer, char *pszFacility*/ );
void drdExit();
void debugPrintDRDFILE();	/* for debuging */
DRDFILE *drdOpen();
DRDFILE *drdOpenOtherFileType();
int drdClose();
int drdChangeHeader();
int drdGotoFirstBlock();
int drdGotoLastBlock();
int drdGotoPrevBlock();
int drdGotoNextBlock();
ushort drdGetCurrentBlockLength();
int drdGetCurrentBlock();

/**********************************************************************
* bDrdDebug is the debug flag. If set, the library sends debug information
* output to stdio. Default reset. Implemented in drd.c
*/
extern int bDrdDebug;

/*********************************************************************
* Prototypes of functions used for the echo data file
* implemented in echo.c
*/
#define ECHO_MAJOR_VERSION 	1
#define ECHO_MINOR_VERSION 	0
#define ECHO_FILETYPE		"ECHO"
#define ECHO_FILEEXT		".echo"

DRDFILE *echoOpen(/*char *pszFileRoot*/);
int echoClose(/*DRDFILE *pFH*/);
DRDFILE *echoCreate(/*char *pszFileRoot,char *pszID, char *pszSatellite,
			 char **apszComment*/);
int echoGotoFirstScan(/*DRDFILE *pFH*/);
int echoGotoLastScan(/*DRDFILE *pFH*/);
int echoGotoNextScan(/*DRDFILE *pFH*/);
int echoGotoPrevScan(/*DRDFILE *pFH*/);
int echoFindScan(/*DRDFILE *pFH, ulong luImageScanNo*/);
unsigned short echoGetCurrentScanLength(/*DRDFILE *pFH*/);
int echoGetCurrentScan(/*DRDFILE *pFH, ulong *pluImScNo, char *pcBuf*/);
int echoWriteScan();

/*********************************************************************
* Prototypes of functions used for the noise data file
* implemented in noise.c
*/
#define NOISE_MAJOR_VERSION 	1
#define NOISE_MINOR_VERSION 	0
#define NOISE_FILETYPE		"NOISE"
#define NOISE_FILEEXT		".noise"

DRDFILE *noiseOpen();
int noiseClose();
DRDFILE *noiseCreate();
int noiseGotoFirstScan();
int noiseGotoLastScan();
int noiseGotoNextScan();
int noiseGotoPrevScan();
int noiseFindScan();
ushort noiseGetCurrentScanLength();
int noiseGetCurrentScan();
int noiseWriteScan();

/*********************************************************************
* Prototypes of functions for the calibration pulse data files
* implemented in cal.c
*/
#define CAL_MAJOR_VERSION 	1
#define CAL_MINOR_VERSION 	0
#define CAL_FILETYPE		"CALPULSE"
#define CAL_FILEEXT		".cal"
DRDFILE *calOpen();
int calClose();
DRDFILE *calCreate();
int calGotoFirstScan();
int calGotoLastScan();
int calGotoNextScan();
int calGotoPrevScan();
int calFindScan();
uint calGetCurImageScanNo();
ushort calGetCurrentScanLenght();
int calGetCurrentScan();
int calWriteScan();
int calGetCountScansForCurImNo();

/*********************************************************************
* Prototypes of functions for the pulse replica data files
* implemented in rep.c
*/
#define REP_MAJOR_VERSION 	1
#define REP_MINOR_VERSION 	0
#define REP_FILETYPE		"REPPULSE"
#define REP_FILEEXT		".rep"
DRDFILE *repOpen();
int repClose();
DRDFILE *repCreate();
int repGotoFirstScan();
int repGotoLastScan();
int repGotoNextScan();
int repGotoPrevScan();
int repFindScan();
ulong repGetCurImageScnaNo();
ushort repGetCurrentScanLength();
int repGetCurrentScan();
int repWriteScan();

/**********************************************************************
* Prototypes of functions for the zero data files
* implemented in zero.c
*/
#define ZERO_MAJOR_VERSION 1
#define ZERO_MINOR_VERSION 0
#define ZERO_FILETYPE		"ZERO"
#define ZERO_FILEEXT		".zero"

DRDFILE *zeroOpen();
int zeroClose();
DRDFILE *zeroCreate();
int zeroGotoFirstBlock();
int zeroGotoLastBlock();
int zeroGotoNextBlock();
int zeroGotoPrevBlock();
ulong zeroGetCurBlockImageScanNo();
ulong zeroGetCurrentBlockLength();
int zeroGetCurrentBlock();
int zeroWriteBlock();

/***********************************************************************
* Prototypes of functions for the auxiliary data file
* implemented in aux.c
*/
#define AUX_MAJOR_VERSION 1
#define AUX_MINOR_VERSION 0
#define AUX_FILETYPE		"AUXILIARY"
#define AUX_FILEEXT		".aux"

DRDFILE *auxOpen();
int auxClose();
DRDFILE *auxCreate();
ulong auxRead();
int auxWrite();
int auxWriteOrbit();
int auxReadOrbit();
int auxGetCurrentOrbitNo();
int auxGetCurrentOrbitData();
int auxWriteCurrentOrbitNo();
double auxGetSampleWindowStartTime();
int auxWriteSampleWindowStartTime();
double auxGetPulseRepetitionInterval();
int auxWritePulsePepetitionInterval();
int auxGetCalSubsysAttenuation();
int auxWriteCalSubsysAttenuation();
int auxGetRepSubsysAttenuation();
int auxWriteReceiverChainAttenuation();
int auxWriteMixingFrequencyShift();
ulong auxGetMixingFrequencyShift();

/**********************************************************************
* Prototypes for the report data file functions
* implemented in report.c
*/
#define REPORT_MAJOR_VERSION 1
#define REPORT_MINOR_VERSION 0
#define REPORT_FILETYPE		"REPORT"
#define REPORT_FILEEXT		".report"

DRDFILE *reportCreate();
int reportClose();
int reportWrite();
int reportVWrite();

#endif /* __DRDAPI.H__ */
