/* ANDI   main program
*  analyzes and decodes a raw data bitstream file.
*  produces the output files described in document 
*  "File format for Decoded Raw Data"
*  to provide the data in the raw data file for further processing.
*  
*  This main program only acts as connection between the DRD library and the 
*  decodeing of the raw data (coded in files al.c, foa.c, fra.c)
*
*  files necessary: andi.h, drdapi.h, al.c, foa.c, fra.c, drdlib.a
*
*  First Setup:	07-20-1994 by Hans-Joerg Wagner
*  First Release: 07-25-1994 Hans-Joerg Wagner 
*                     without tested IDHD GH becaus missing data to test
*/
 
#include <stdio.h>
#include <varargs.h>
#include <sys/types.h>
#include <sys/time.h>
#include <malloc.h>
#include <string.h>
#include "hjwlib.h"
#include "drdapi.h"
#include "andi.h"

/* command line parameter */
struct {
	char	*pszInFile;
	char	*pszOutFileRoot;
	char 	*pszID;
	int	bIsVerbose;
	int	bCommentLines;
	int 	nLevel;
	ulong	luStartByte;
	ulong	luEndByte;
	int	bSave;
	/* which files to produce */
	int	bEcho:1;
	int	bNoise:1;
	int	bCal:1;
	int 	bRep:1;
	int	bZero:1;
	int	bAux:1;
	} g_cmdLine; 

/* output files */
DRDFILE *g_echo,*g_noise,*g_zero,*g_rep,*g_cal,*g_aux,*g_report;

/* other global variables */
ulong	g_luFormatsRead = 0;
ulong	g_luImageFormatsRead = 0;
ulong	g_luPulseReplicaRead = 0;
ulong	g_luCalibrationPulsesRead = 0;
ulong	g_luZeroBlocksRead = 0;
ulong	g_luNoiseScansRead = 0;
ulong	g_luEchoScansRead = 0;

/**********************************************************************
* main program
*/
main(argc,argv)
int argc;
char **argv;
{
time_t startTime;

startTime = Hello();
if ( !CommandLine(argc, argv) ) {
	Usage();
	exit(1);
	}
WriteComInfo();

/* initialize library */
/* bDrdDebug = 1; */
if ( !InitLib() ) {
	printf("error initializing DRD library \n");
	exit(1);
	}

/* Open input file */
if ( !alInit(g_cmdLine.pszInFile,g_cmdLine.luStartByte,g_cmdLine.luEndByte,g_cmdLine.bSave) ) {
        printf("error opening input file %s\n",g_cmdLine.pszInFile);
	ExitLib();
        exit(1);
        }

WriteHardInfo();
Scan();			/* Scan the input file */
alExit();		/* close input file */

/* End of scanning */
ReportStatistics();
ExitLib();	/* close library */
GoodBye(startTime);
}

/**********************************************************************
* Hello says hello
* returns the start time
*/
time_t Hello()
{
printf("%s\n",PROGRAM);
return time(NULL);
}

/**********************************************************************
* Goodbye says bye
*/
void GoodBye(startTime)
time_t startTime;
{
printf("ANDI read %lu formats in %lu seconds\n",
		g_luFormatsRead,(ulong)(time(NULL) - startTime));
printf("ANDI says goodbye\n\n");
}

/**********************************************************************
* Usage
* explains the useage of the program
*/
void Usage()
{
printf("usage:\n");
printf("    andi infile [-o outfile] [+|-]f typelist [-s startpos] [-e endpos]\n");
printf("	[-i ID] [-v[on|off] [-u] [-c] [-l level]\n\n");
printf("    infile	input file\n");
printf("    outfile	output file root name, if not given only\n");
printf("		a report.report file is created\n");
printf("    -f 		produce all files except the ones listed in typelist\n");
printf("    +f          produce only files listed in typelist\n");
printf("    typelist    list of file types, seperated by \",\". NO SPACES!!!\n");
printf("                valid types are: aux,cal,echo,noise,rep,zero.\n");
printf("    startpos	start position of scanning in infile\n");
printf("    endpos	end position of scanning in infile\n");
printf("    ID		file ID defaults to infile\n");
printf("    -v		Verbose flag. Default is -von\n");
printf("    -u          Enforce unsafe reading of frames\n");
printf("    -c          Asks for comment lines for evey file\n");
printf("    level       Message level:\n");
printf("                      0 = report only summary\n");
printf("                      1 = report format level messages = default\n");
printf("                      2 = report frame level messages\n");
printf("                      3 = report scan level messages\n");
}

/**********************************************************************
* CommandLine
* interprets the command line and fills the g_cmdLine structure
* returns 0 if an error occures during interpretion
*/
int CommandLine(argc,argv)
int argc;
char **argv;
{
int count;
char *pszTypeList = NULL;
char *pszTmp;
int bAddList;

/* argv[0] = "andi"			*/
/* argv[1] = input file name		*/

if (argc < 2)
        return 0;
else
	g_cmdLine.pszInFile = argv[1];


/* defaults */
g_cmdLine.pszOutFileRoot = NULL;
g_cmdLine.luStartByte = 0L;
g_cmdLine.luEndByte = 0L;
g_cmdLine.bIsVerbose = 1;
g_cmdLine.bCommentLines = 0;
g_cmdLine.pszID = g_cmdLine.pszInFile;
g_cmdLine.nLevel = 1;
g_cmdLine.bSave = 1;
/* the default for output files is write not until outputname will be set */

for( count = 2; count < argc; count++) {
    if ('-' == argv[count][0]) {
	switch (argv[count][1]) {
	    case 'c':		/* comment lines */
		g_cmdLine.bCommentLines = 1;
		break;
            case 'e':      /* end byte offset of scan */
                if (2 != strlen(argv[count]) ) return 0;
                count++;
                if (count >= argc) return 0;
                sscanf(argv[count],"%lu",&g_cmdLine.luEndByte);
                break;
	    case 'f':	/* type list - farther processing at end of function*/
		if (2 != strlen(argv[count]) ) return 0;
		count++;
		if (count >= argc) return 0;
		if (NULL != pszTypeList) return 0;
		pszTypeList = argv[count];
		bAddList = 0;
		break;
	    case 'i':		/* ID defaults to name of infile*/
		if (2 != strlen(argv[count]) ) return 0;
		count++;
		if (count >= argc) return 0;
		g_cmdLine.pszID = argv[count];
		break;
	    case 'l':
		if (2 != strlen(argv[count]) ) return 0;
		count++;
		if (count >= argc) return 0;
		g_cmdLine.nLevel = atoi(argv[count]);
		if (g_cmdLine.nLevel > 3 || g_cmdLine.nLevel < 0)
			return 0;
		break;
	    case 'o':		/* output file name */
		if (2 != strlen(argv[count]) ) return 0;
		count++;
		if (count >= argc) return 0;
		g_cmdLine.pszOutFileRoot = argv[count];
		break;
	    case 's':		/* start byte offset */
		if (2 != strlen(argv[count]) ) return 0;
		count++;
		if (count >= argc) return 0;
		sscanf(argv[count],"%lu",&g_cmdLine.luStartByte);
		break;
	    case 'u':
		g_cmdLine.bSave = 0;
		break;
	    case 'v':		/* verbose mode ? */
		if ( !strcmp(argv[count],"-von") )
			g_cmdLine.bIsVerbose = 1;
		else if ( !strcmp(argv[count],"-voff") )
			g_cmdLine.bIsVerbose = 0;
		else
			return 0;
		break;
	    default:
		return 0;
	    } /* end switch cmdline option */
	} /* end if - */
    /* file list addition */
    else if (!strcmp(argv[count],"+f")) {
	count++;
	if (count >= argc) return 0;
	if (NULL != pszTypeList) return 0;
	pszTypeList = argv[count];
	bAddList = 1;
	}
    } /*end for */

/* set level for report */
g_stat.level = g_cmdLine.nLevel;
	
/* test for reasonable start and stop positions */
if ( g_cmdLine.luEndByte && g_cmdLine.luStartByte > g_cmdLine.luEndByte ) {
	printf("startpos %lu is greater or equal than endpos %lu\n",
			g_cmdLine.luStartByte, g_cmdLine.luEndByte);
	return 0;
	}

/* which files to produce */
if (NULL == g_cmdLine.pszOutFileRoot) {
    /* no output!!! regardless of file flags */
    g_cmdLine.bAux = 0; g_cmdLine.bCal = 0; g_cmdLine.bEcho = 0;
    g_cmdLine.bNoise = 0; g_cmdLine.bRep = 0; g_cmdLine.bZero = 0;
    }
else if (NULL == pszTypeList) {
    /* output everything */
    g_cmdLine.bAux = 1; g_cmdLine.bCal = 1; g_cmdLine.bEcho = 1;
    g_cmdLine.bNoise = 1; g_cmdLine.bRep = 1; g_cmdLine.bZero = 1;
    } 
else {
    /* default is 0 if adding files, 1 if removeing file types */
    g_cmdLine.bAux = !bAddList; g_cmdLine.bCal = !bAddList; 
    g_cmdLine.bEcho = !bAddList; g_cmdLine.bNoise = !bAddList;
    g_cmdLine.bRep = !bAddList; g_cmdLine.bZero = !bAddList;
    pszTmp = strtok(pszTypeList,", ");
    while(NULL != pszTmp) {
	if (!strcmp(pszTmp,"aux") ) g_cmdLine.bAux = bAddList;
	if (!strcmp(pszTmp,"cal") ) g_cmdLine.bCal = bAddList;
	if (!strcmp(pszTmp,"echo") ) g_cmdLine.bEcho = bAddList;
	if (!strcmp(pszTmp,"noise") ) g_cmdLine.bNoise = bAddList;
	if (!strcmp(pszTmp,"rep") ) g_cmdLine.bRep = bAddList;
	if (!strcmp(pszTmp,"zero") ) g_cmdLine.bZero = bAddList;
	pszTmp = strtok(NULL,", ");
	}
    }
fESAHeaders=fopen("esa_headers","w");
return 1;
}
FILE *fESAHeaders;
/**********************************************************************
* InitLib
* initializes the DRD library and opens the output files
* returns 0 if an error occures
*/
int InitLib()
{
char *pszId;
int count, max;
char *pszFile;
FILE file;
char *pszTmp;
char **comments = NULL;

if (!drdInit(PROGRAM,MAJOR_VERSION,MINOR_VERSION,FACILITY) ) {
	printf("error during initialization of DRD library\n");
	return 0;
	}

/* read comment lines from console if necessary */
if (g_cmdLine.bCommentLines && NULL != g_cmdLine.pszOutFileRoot) {
	printf("call edit_lines\n");
	comments = edit_lines(comments);
        }

pszId = g_cmdLine.pszID;

/* if no outputfile given, open just a report.report file for report */
if (NULL == g_cmdLine.pszOutFileRoot)
	g_report = reportCreate("report","Scan",SATELLITE,NULL,
					g_cmdLine.bIsVerbose);
else
	g_report = reportCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,
                comments,g_cmdLine.bIsVerbose);
if (NULL == g_report) {
	printf("error opening report file\n");
	return 0;
	}

if (g_cmdLine.bEcho) {
    g_echo = echoCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,comments);
    if (NULL == g_echo) {
	printf("error opening echo output file\n");
	return 0;
	}
    }

if (g_cmdLine.bNoise) {
    g_noise = noiseCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,comments);
    if (NULL == g_noise) {
        printf("error opening noise output file\n");
        return 0;
        }
    }
if (g_cmdLine.bZero) {
    g_zero = zeroCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,comments);
    if (NULL == g_zero) {
        printf("error opening zero output file\n");
        return 0;
        }
    }
if (g_cmdLine.bRep) {
    g_rep = repCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,comments);
    if (NULL == g_rep) {
        printf("error opening pulse replica output file\n");
        return 0;
        }
    }
if (g_cmdLine.bCal) {
    g_cal = calCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,comments);
    if (NULL == g_cal) {
        printf("error opening calibration pulse  output file\n");
        return 0;
        }
    }
if (g_cmdLine.bAux) {
    g_aux = auxCreate(g_cmdLine.pszOutFileRoot,pszId,SATELLITE,comments);
    if (NULL == g_aux) {
        printf("error opening auxiliary output file\n");
        return 0;
        }
    }

return 1;
}

/**********************************************************************
* ExitLib
* closes the open DRD library files and exits the library
*/
void ExitLib()
{
if (NULL != g_echo) echoClose(g_echo);
if (NULL != g_noise) noiseClose(g_noise);
if (NULL != g_zero) zeroClose(g_zero);
if (NULL != g_rep) repClose(g_rep);
if (NULL != g_cal) calClose(g_cal);
if (NULL != g_aux) auxClose(g_aux);
if (NULL != g_report) reportClose(g_report);

drdExit();
}

/**********************************************************************
* WriteHardInfo
* writes all information into aux file, that is not included in the
* data stream, but is known as hard encoded.
* Momentary these are for ERS-1
* 	SAMPLING RATE
* 	POLARISATION
*	INCIDENCE ANGLE
*	QUANTISATION
*	RADAR FREQUENCY
*/
void WriteHardInfo()
{
if (g_cmdLine.bAux) {
    auxWriteSamplingRate(g_aux,NULL,1L,18960L);
    auxWrite(g_aux,1L,"POLARISATION","Linear Vertical");
    auxWrite(g_aux,1L,"INCIDENCE ANGLE","23 degree nominal");
    auxWrite(g_aux,1L,"QUANTISATION","5I 5Q");
    auxWrite(g_aux,1L,"REPPULSE QUANTISATION","6I 6Q");
    auxWrite(g_aux,1L,"CALPULSE QUANTISATION","6I 6Q");
    auxWriteRadarFrequency(g_aux,1L,5300000L);
    auxWriteMixingFrequencyShift(g_aux,NULL,1L,5223200L);
    }
}

/**********************************************************************
* WriteComInfo
* Write command line info to report file
*/
void WriteComInfo()
{
char *szTmp = malloc( strlen(AUX_FILEEXT) + strlen(CAL_FILEEXT)
	+ strlen(REP_FILEEXT) + strlen(NOISE_FILEEXT) + strlen(ECHO_FILEEXT)
	+ strlen(ZERO_FILEEXT) + 6 );
if (NULL == szTmp) {
	fprintf(stderr,"Error allocating memory in WriteComInfo\n");
	exit(100);
	}

Report(lvSummary,"Decode with the following options:");
if (!g_cmdLine.bAux && !g_cmdLine.bCal && !g_cmdLine.bEcho
    && !g_cmdLine.bNoise && !g_cmdLine.bRep && !g_cmdLine.bZero) 
    {
    Report(lvSummary,"No output files will be produced\n");
    }
else {
    Report(lvSummary,"Procuced files root name '%s':",g_cmdLine.pszOutFileRoot);
    szTmp[0] = '\0';
    if (g_cmdLine.bAux) {strcat(szTmp,AUX_FILEEXT); strcat(szTmp," "); }
    if (g_cmdLine.bCal) {strcat(szTmp,CAL_FILEEXT); strcat(szTmp," "); }
    if (g_cmdLine.bRep)	{strcat(szTmp,REP_FILEEXT); strcat(szTmp," "); }
    if (g_cmdLine.bNoise) {strcat(szTmp,NOISE_FILEEXT); strcat(szTmp," "); }
    if (g_cmdLine.bEcho) {strcat(szTmp,ECHO_FILEEXT); strcat(szTmp," "); }
    if (g_cmdLine.bZero) strcat(szTmp,ZERO_FILEEXT);
    Report(lvSummary,szTmp);
    }
Report(lvSummary,"This report includes the following report messages:");
Report(lvSummary,"     Summary");
if( g_cmdLine.nLevel >= 1 ) Report(lvSummary,"    Format level messages");
if( g_cmdLine.nLevel >= 2) Report(lvSummary,"    Frame level messages");
if( g_cmdLine.nLevel >= 3) Report(lvSummary,"    Lowes level messages");

if (!g_cmdLine.bSave)
	Report(lvSummary,"Unsave frame reading is allowed");

free(szTmp);
}

/**********************************************************************
*  Scan
* reads formats from input file and writes the output files
*/
void Scan()
{
double lfTmp,lfBoardTime = -1.0,lfFirstBoardTime = -1.0;
ulong lu;
ulong luLastImageFormat = 0;
int bFirstFormat = 1; 
int  iTmp;
char *pcTmp;
struct EPHEMERIS *pEph;

/* repeat until end of scanning area */
while (alNextBlock()) {
	/* First get the current image format counter */
	lu = alGetImageFormatCounter();
	printf("ImageFormat %8lu --> %u%% done \r",lu,
	         fraGetRelativeFilePosition()); 
	 /*modified 8-23-95, percentage counter by Christian Fischer*/        
	         
	         /* say Yes, I am still working */
	/* maybe it returns several times with the same image format
	* this line is propably not necessary because alNextBlock ever
	* reads a new Image Format. However it was planed different and
	* one never knows. So this line stays here... Never touch a running
	* system!!
	* Is this to skip the zero formats?
	*/
	if (lu != luLastImageFormat) g_luImageFormatsRead++;
	if (bFirstFormat && 0L != luLastImageFormat) {
	    Report(lvSummary,"First image format # %lu",luLastImageFormat);
	    bFirstFormat = 0;
	    }
	luLastImageFormat = lu;
	g_luFormatsRead++;

	/* Board time */
	lfTmp = alGetBoardTime();
	/*printf("%6lu: newBoardTime is %lf\n", lu, lfTmp);*/
	if (lfTmp > 0) {
	    lfBoardTime = lfTmp;
	    if (lfFirstBoardTime < 0.0) {
		/* making report if first time board time is encountered */
		Report(lvSummary,"First detected ICU board time %f sec",
								lfBoardTime);
		lfFirstBoardTime = lfBoardTime;
		}
	    printf("%6lu: BoardTime is %lf\n",lu,lfTmp);
	    }

	/* Sample window start time */
	lfTmp = alGetSampleWinStartTime();
	if (lfTmp >= 0) {
		if (g_cmdLine.bAux)
		    auxWriteSampleWindowStartTime(g_aux,lu,lfTmp * 1e6);
		}

	/* Pulse repetition intervall */
	lfTmp = alGetPulseRepInterval();
	if (lfTmp >= 0) {
		if (g_cmdLine.bAux)
		    auxWritePulseRepetitionInterval(g_aux,lu,lfTmp * 1e6);
		}

	/* Calibration subsystem attenuation */
	iTmp = alGetCalSubsysAtten();
	if (iTmp >= 0) {
		if (g_cmdLine.bAux)
		    auxWriteCalSubsysAttenuation(g_aux,lu,iTmp);
		}

	/* Receiver subsystem attenuation */
	iTmp = alGetReceiverSubsysAtten();
	if (iTmp >= 0) {
		if (g_cmdLine.bAux)
		    auxWriteReceiverChainAttenuation(g_aux,lu,iTmp);
		}

	/* Orbit */
	pEph = alGetOrbitInfo();
	if (NULL != pEph) {
		printf("got orbit info\n");
		if (g_cmdLine.bAux) {
		    auxWriteOrbit(g_aux, pEph->counter - 1,
			GET_EPH_SIGNED_LONG(pEph->rx),
			GET_EPH_SIGNED_LONG(pEph->ry),
			GET_EPH_SIGNED_LONG(pEph->drx),
			GET_EPH_SIGNED_LONG(pEph->dry),
			GET_EPH_SIGNED_LONG(pEph->drz),
			GET_SAT_ULONG(pEph->timeSat) );
		    }
		}
	iTmp = alGetCurrentOrbit();
	if (iTmp >= 0) {
		if (g_cmdLine.bAux)
		    auxWriteCurrentOrbitNo(g_aux,lu,iTmp);
		}

	/* Zero data */
	pcTmp = (char *)alGetZeroData();
	if (NULL != pcTmp) {
		g_luZeroBlocksRead++;
		if (g_cmdLine.bZero) {
		    zeroWriteBlock(g_zero,lu,pcTmp,29 * 250 - 8 );
		    /* printf("%6lu: Received Zero data\n",lu); */
		    }
		}

	/* Calibration data */
	if (alIsCalData()) {
	    g_luCalibrationPulsesRead++;
	    if (g_cmdLine.bCal) {
		calWriteScan(g_cal,lu,alGetCalData(),SIZE_OF_REPCAL * 2);
		/* printf("%6lu: Received calibration data\n",lu); */
		}
	    }

	/* Replica data */
        if (alIsRepData()) {
	    g_luPulseReplicaRead++;
	    if (g_cmdLine.bRep) {
		repWriteScan(g_rep,lu,alGetRepData(),SIZE_OF_REPCAL * 2);
                /* printf("%6lu: Received Replica data\n",lu); */
                }
	    }
 
	/* Noise data */
	if (alIsNoiseData()) {
	    g_luNoiseScansRead++;
	    if (g_cmdLine.bNoise) {
		noiseWriteScan(g_noise,lu,alGetNoiseData(),SIZE_OF_ECHO * 2);
                /* printf("%6lu: Received Noise data\n",lu); */
                }
	    }
 
	/* Echo data */
	if (alIsEchoData()) {
	    g_luEchoScansRead++;
	    if (g_cmdLine.bEcho) {
		echoWriteScan(g_echo,lu,alGetEchoData(),SIZE_OF_ECHO * 2);
                /* printf("%6lu: Received Echo data\n",lu); */
                }
	    }
	} /* end while loop */
Report(lvSummary,"Last image format # %lu",luLastImageFormat);
Report(lvSummary,"Last detected ICU board time %f sec",lfBoardTime);
Report(lvSummary,"----> Time range is %f sec",lfBoardTime - lfFirstBoardTime);
}

/**********************************************************************
* ReportStatistiks writes report about the scanning process
*/
void ReportStatistics()
{
Report(lvSummary,"------------- Total of %lu formats read ---------------",
							g_luFormatsRead);
Report(lvSummary,"      %8lu image formats",g_luImageFormatsRead);
Report(lvSummary,"      %8lu zero blocks",g_luZeroBlocksRead);
Report(lvSummary,"      %8lu pulse replicas",g_luPulseReplicaRead);
Report(lvSummary,"      %8lu calibration pulses",g_luCalibrationPulsesRead);
Report(lvSummary,"      %8lu noise scans",g_luNoiseScansRead);
Report(lvSummary,"      %8lu echo scans",g_luEchoScansRead);
Report(lvSummary,"------------- Error sumarization -------------------");
Report(lvSummary,"      %8lu losses of synchronization",
							g_stat.loss.luCount);
Report(lvSummary,"      %8lu frames missed",g_stat.frame.luMissCount);
if (!g_cmdLine.bSave)
    Report(lvSummary,"      %8lu unsafe frames read",g_stat.frame.luUnsave);
Report(lvSummary,"      %8lu formats missed",g_stat.format.luMissCount);
Report(lvSummary,"      %8lu image formats missed",g_stat.format.luImMissCount);
}

/**********************************************************************
* Report writes report dependend on level
* call Report(level,format,arg1,arg2.....)
* Writes the report line with format like the printf function if the level
* is lower or equal the given g_stat.level
*/
void Report(va_alist)
va_dcl
{
char *format;
int nLevel;
va_list args;

va_start(args);
nLevel = va_arg(args,int);
format = va_arg(args,char *);
if (nLevel <= g_stat.level)
	reportVWrite(g_report,format,args);
va_end(args);
}

