/* RANDI   main program 
*  analyzes and decodes a raw data bitstream file.
*  produces the output files described in document 
*  "File format for Decoded Raw Data"
*  to provide the data in the raw data file for further processing.
*  
*  This main program only acts as connection between the DRD library and the 
*  decodeing of the raw data (coded in files al.c, foa.c, fra.c)
*
*  files necessary: randi.h, drdapi.h, al.c, foa.c, fra.c, drdlib.a
*
*  First Setup:	07-20-1994 by Hans-Joerg Wagner
*  First Release: 07-25-1994 Hans-Joerg Wagner 
*                     without tested IDHD GH becaus missing data to test
*
* RADARSAT Modification
* by Christian Fischer
* 09-25-95
*
*/
 
#include <stdio.h>
#include <varargs.h>
#include <sys/types.h>
#include <sys/time.h>
#include <malloc.h>
#include <string.h>
#include <math.h>
#include "hjwlib.h"
#include "drdapi.h"
#include "randi.h"

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
DRDFILE *g_echo,*g_zero,*g_rep,*g_aux,*g_report;

/* other global variables */
ulong	g_luFormatsRead = 0;
ulong	g_luPulseReplicaRead = 0;
ulong	g_luZeroBlocksRead = 0;
ulong	g_luEchoScansRead = 0;

int g_RewriteTime; /*global*/

/* CareFree mode off is default, on means, that 
 * RANDI reads corrupted raw data without checking
 * please read manual */
int g_CareFree=0;


char BeamTable[21][26]={ "Beam Not Used",
                         "Standard Beam S1",
                         "Standard Beam S2",
                         "Standard Beam S3",
                         "Standard Beam S4",
                         "Standard Beam S5",
                         "Standard Beam S6",
                         "Standard Beam S7",
                         "Wide Beam W1",
                         "Wide Beam W2",
                         "Wide Beam W3",
                         "Wide Beam W2 recorded",
                         "Experimental Beam E1",
                         "Experimental Beam E2",
                         "Experimental Beam E3",
                         "Experimental Beam E4",
                         "Fine Resolution Beam F1",
                         "Fine Resolution Beam F2",
                         "Fine Resolution Beam F3",
                         "Fine Resolution Beam F4",
                         "Fine Resolution Beam F5"  };


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

/* initialize library */
/*   show everything you do: bDrdDebug = 1; */
if ( !InitLib() ) {
	printf("error initializing DRD library \n");
	exit(1);
	}

WriteComInfo();

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
printf("RANDI read %lu formats in %lu seconds              \n",
		g_luFormatsRead,(ulong)(time(NULL) - startTime));
printf("RANDI says goodbye\n\n");
}

/**********************************************************************
* Usage
* explains the useage of the program
*/
void Usage()
{
printf("usage:\n");
printf("    randi infile [-o outfile] [+|-]f typelist [-s startpos] [-e endpos]\n");
printf("	[-i ID] [-v[on|off] [-u] [-c] [-l level]\n\n");
printf("    infile	input file\n");
printf("    outfile	output file root name, if not given only\n");
printf("		a report.report file is created\n");
printf("    -f 		produce all files except the ones listed in typelist\n");
printf("    +f          produce only files listed in typelist\n");
printf("    typelist    list of file types, seperated by \",\". NO SPACES!!!\n");
printf("                valid types are: aux,echo,rep,zero.\n");
printf("    startpos	start position of scanning in infile\n");
printf("    endpos	end position of scanning in infile\n");
printf("    ID		file ID defaults to infile\n");
printf("    -v		Verbose flag. Default is -von\n");
printf("    -u          Enforce unsafe reading of frames\n");
printf("    -c          Asks for comment lines for every file\n");
printf("    -a          Care Free mode, please read description\n");
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


/* argv[0] = "randi"			*/
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
	    case 'a':  /* Care Free mode, read very corrupted data*/
	         g_CareFree=1;
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
/*no .noise or .cal file for RADARSAT*/
g_cmdLine.bNoise = 0;
g_cmdLine.bCal = 0;
if (NULL == g_cmdLine.pszOutFileRoot) {
    /* no output!!! regardless of file flags */
    g_cmdLine.bAux = 0;  g_cmdLine.bEcho = 0;
    g_cmdLine.bRep = 0; g_cmdLine.bZero = 0;
    }
else if (NULL == pszTypeList) {
    /* output everything */
    g_cmdLine.bAux = 1; g_cmdLine.bEcho = 1;
    g_cmdLine.bRep = 1; g_cmdLine.bZero = 1;
    } 
else {
    /* default is 0 if adding files, 1 if removeing file types */
    g_cmdLine.bAux = !bAddList;
    g_cmdLine.bEcho = !bAddList;
    g_cmdLine.bRep = !bAddList; 
    g_cmdLine.bZero = !bAddList;
    pszTmp = strtok(pszTypeList,", ");
    while(NULL != pszTmp) {
	if (!strcmp(pszTmp,"aux") ) g_cmdLine.bAux = bAddList;
	if (!strcmp(pszTmp,"echo") ) g_cmdLine.bEcho = bAddList;
	if (!strcmp(pszTmp,"rep") ) g_cmdLine.bRep = bAddList;
	if (!strcmp(pszTmp,"zero") ) g_cmdLine.bZero = bAddList;
	pszTmp = strtok(NULL,", ");
	}
    }

return 1;
}

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
if (NULL != g_zero) zeroClose(g_zero);
if (NULL != g_rep) repClose(g_rep);
if (NULL != g_aux) auxClose(g_aux);
if (NULL != g_report) reportClose(g_report);

drdExit();
}

/**********************************************************************
* WriteHardInfo
* writes all information into aux file, that is not included in the
* data stream, but is known as hard encoded.
* Momentary these are for RADARSAT
*	QUANTIZATION
*       RADAR FREQUENCY
*       
*/
void WriteHardInfo()
{
    randi_auxWrite(g_aux,1L,"QUANTIZATION","4I 4Q");
    randi_auxWrite(g_aux,1L,"RADAR FREQUENCY","5300000 kHz");
    /* Mixing Frequency Shift is yet unkwown, but this value will give
     * good results using anapulse
     * PLEASE CORRECT THIS VALUE !!!*/
     randi_auxWrite(g_aux,1L,"MIXING FREQUENCY SHIFT","5300000 kHz");
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
    
if (g_CareFree)
  Report(lvSummary,"ATTENTION!!! RANDI works in CARE FREE mode!!!");
      
Report(lvSummary,"This report includes the following report messages:");
Report(lvSummary,"     Summary");
if( g_cmdLine.nLevel >= 1 ) Report(lvSummary,"    Format level messages");
if( g_cmdLine.nLevel >= 2) Report(lvSummary,"    Frame level messages");
if( g_cmdLine.nLevel >= 3) Report(lvSummary,"    Lowest level messages");

if (!g_cmdLine.bSave)
	Report(lvSummary,"Unsave frame reading is allowed");

free(szTmp);
}

/*********************************************************************
* calculates the temperature out of the given aux Data Code
*/
long float Temperature(code)
uchar code;
{
  long float rth;
  rth=2940.0*code/(294.0-code);
  return (1/(.0014733+.0002372*log(rth)+1.074e-7*pow(log(rth),3)) -273.15 );
}
  

/**********************************************************************
*  Scan
* reads formats from input file and writes the output files
*/
void Scan()
{
uint FormatCounter;
int  iTmp;
long int liTmp;
float fTmp;
float *floatTmp;
double lfTmp;
double *doubleTmp;
char *sTmp;
char *pcTmp;
struct AUX AuxData; /*no static, Scan is running only one Time */
struct AUXDATAVALID NewAuxData; 
struct SAT_USHORT EphemerisData[NoOfEphWords+1];
uint SizeOfApplDataField;
uint SizeOfReplica;
uint SizeOfEcho;
uint SizeOfFill;
uint SamplesPerScan=0;
char *BufferPointer;  /* points to the actual data type (replica, zero,
                       * or fill bytes) in g_ApplicationDataFieldBuffer
                       * defined in al.c
                       */  
double SampleRate;
double TimeUnit;
int Scansar;
int FirstFormat=1;

sTmp=(char *)  malloc(80);
/*write Time in the beginning*/
g_RewriteTime=1;

/* repeat until end of scanning area */
while (alNextBlock()) {
        FormatCounter=foaGetFormatCount();
        
        g_luFormatsRead++;
        /* say Yes, I am still working */
	printf("-->> Still working (%u%%)... processed %lu Formats\r",
	       fraGetRelativeFilePosition(),g_luFormatsRead); 
	
	if (FirstFormat) {
	    Report(lvSummary,"FIRST FORMAT * Image-Scan-Number: %u      "
	           ,FormatCounter);
	    FirstFormat = 0;
	    }

	/* Auxiliary Data */ 
        if ( alAuxSyncPatternIsCorrect() )
           { 
           /*get the Auxiliary Data, use for the Ephemeridisdata an extra array */
           
           alGetAuxData(&AuxData,&NewAuxData,EphemerisData);
           
           /*write the time in the beginning or each time when there occurs
            *new auxiliary data or  after a loss in synchronisation
            */
           if (g_RewriteTime)
               { /* see REV F, p. 33 */
               sprintf(sTmp,"Day %d Second %d.%d%03.0lf ",
   /*days*/      (uint)(AuxData.Time[0]<<3)|(uint)(AuxData.Time[1]>>5),
   /*seconds*/   (uint)((AuxData.Time[1]&31)<<12)|(uint)(AuxData.Time[2]<<4)
                        |(uint)(AuxData.Time[3]>>4),
   /*millis*/    (uint)((AuxData.Time[3]&15)<<6)|(uint)(AuxData.Time[4]>>2),
   /*micros*/    ((uint)((AuxData.Time[4]&3)<<8)|(uint)(AuxData.Time[5]))/1.024);
               randi_auxWrite(g_aux,FormatCounter,"TIME",sTmp);
	       g_RewriteTime=0;	
               }
	   if (NewAuxData.PSval)
  	      { /*see REV F, p. 19 */
       	      sprintf(sTmp,"");
       	      randi_auxWrite(g_aux,FormatCounter,"PAYLOAD STATUS",sTmp);
       	      switch (AuxData.PayloadStatus.Msb>>5 &7)
       	      {
       	      case 0:sTmp="Off";break;
       	      case 1:sTmp="Idle";break;
       	      case 2:sTmp="Ready Calibration";break;
       	      case 3:sTmp="Ready to Image";break;
       	      case 4:sTmp="Image";break;
       	      default:sTmp="spare";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - PAYLOAD MODE",sTmp);
       	      switch ( (AuxData.PayloadStatus.Msb&0x18)>>3 )
       	      {
       	      case 0:sTmp="Off";break;
       	      case 1:sTmp="Executing Image";break;
       	      case 2:sTmp="Scan for Image";break;
       	      case 3:sTmp="Image failed";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - IMAGE STATUS",sTmp);
       	      switch ( ((AuxData.PayloadStatus.Msb&7)<<1)|AuxData.PayloadStatus.Lsb>>7 )
       	      {
       	      case 0:sTmp="Off";break;
       	      case 1:sTmp="Executing CAL 1";break;
       	      case 2:sTmp="Executing CAL 2";break;
       	      case 8:sTmp="Scanning for CAL";break;
       	      case 9:sTmp="CAL failed";break;
       	      default:sTmp="spare";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - CALIBRATION STATUS",sTmp);
       	      switch (  (AuxData.PayloadStatus.Lsb&0x40)>>6 )
       	      {
       	      case 0:sTmp="No Fault";break;
       	      case 1:sTmp="Fault";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - FAULT",sTmp);
       	      switch ( (AuxData.PayloadStatus.Lsb&0x20)>>5 )
       	      {
       	      case 0:sTmp="No Warning";break;
       	      case 1:sTmp="Warning";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - WARNING",sTmp);
       	      switch ( (AuxData.PayloadStatus.Lsb&0x18)>>3 )
       	      {
       	      case 0:sTmp="Real-Time";break;
       	      case 1:sTmp="Store";break;
       	      default:sTmp="spare";break;
       	      }
       	      /*Jason Feature: this shall be  also reported*/
       	      Report(lvSummary,"Type of transmitted Image: %s",sTmp);
       	      randi_auxWrite(g_aux,FormatCounter," - TYPE OF IMAGE",sTmp); 
       	      Scansar=0;
       	      switch ( (AuxData.PayloadStatus.Lsb&4)>>2 )
       	      {
       	      case 0:sTmp="Scansar";Scansar=1;break;
       	      case 1:sTmp="StripMap";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - SCANSAR/STRIPMAP",sTmp);
       	      switch ( (AuxData.PayloadStatus.Lsb&2)>>1 )
       	      {
       	      case 0:sTmp="HPMC ready less than 120 min.";break;
       	      case 1:sTmp="HPMC ready more than 120 min.";break;
       	      }
       	      randi_auxWrite(g_aux,FormatCounter," - HPMC READY",sTmp);
              switch (AuxData.PayloadStatus.Lsb&1)
              {
              case 0:sTmp="Manual";break;
              case 1:sTmp="Automatic";break;
              }
              randi_auxWrite(g_aux,FormatCounter," - AUTO/MANUAL",sTmp);
	      }
           if (NewAuxData.Rval)
              {  /*this Counter has its Lsb left */
	      iTmp=((AuxData.ReplicaAGC&32)+(AuxData.ReplicaAGC&16)* 2
                     +(AuxData.ReplicaAGC&8)* 4+(AuxData.ReplicaAGC&4)* 8
	             +(AuxData.ReplicaAGC&2)* 16+(AuxData.ReplicaAGC&1)* 32) ;
              sprintf(sTmp,"%ddB ",iTmp);
              randi_auxWrite(g_aux,FormatCounter,"REPLICA AGC",sTmp);
              }
            if (NewAuxData.Attenval)
              {
              if (AuxData.CALNAttenuatorSetting<32)
	        { /* Nom. Atten. */
                iTmp=((AuxData.CALNAttenuatorSetting&8)+
		      (AuxData.CALNAttenuatorSetting&4)*2+
		      (AuxData.CALNAttenuatorSetting&2)*4+
		      (AuxData.CALNAttenuatorSetting&1)*8);
                fTmp=iTmp*6.1;
                }
	      else 
		fTmp=25+0.5*((AuxData.CALNAttenuatorSetting>>4)-2);
              sprintf(sTmp,"%2.1fdB ",fTmp);
              randi_auxWrite(g_aux,FormatCounter,"CALN ATTENUATOR",sTmp);
              }
            if (NewAuxData.PWval)
              {
              switch (AuxData.PulseWaveformNo)
              {
                case 0:sTmp="Chirp Pulse #1 BW 30.00 MHZ";break;
                case 1:sTmp="Chirp Pulse #2 BW 17.28 MHZ";break;                
                case 2:sTmp="Chirp Pulse #3 BW 11.58 MHZ";break;                
                case 3:sTmp="Continuos Wave #1";break;
                case 4:sTmp="Continuos Wave #2";break;                
                case 5:sTmp="Continuos Wave #3";break;
                case 6:sTmp="Continuos Wave #4";break;
                case 7:sTmp="Continuos Wave #5";break;
                default:sTmp="Programmable Waveform";break;
              }  
              randi_auxWrite(g_aux,FormatCounter,"PULSE WAVEFORM",sTmp);
              }
            if (NewAuxData.Tempval)
              {
              sprintf(sTmp,"%4.3lf deg. Celsius",Temperature(AuxData.Temperature.Lsw.Lsb) );
              randi_auxWrite(g_aux,FormatCounter,"RECEIVER LNA        TEMPERATURE",sTmp);
	      sprintf(sTmp,"%4.3lf deg. Celsius",Temperature(AuxData.Temperature.Lsw.Msb) );
              randi_auxWrite(g_aux,FormatCounter,"RECEIVER SUB-SYST.  TEMPERATURE",sTmp);
              sprintf(sTmp,"%4.3lf deg. Celsius",Temperature(AuxData.Temperature.Msw.Lsb) );
              randi_auxWrite(g_aux,FormatCounter,"RECEIVER PROT. UNIT TEMPERATURE",sTmp);
              sprintf(sTmp,"%4.3lf deg. Celsius",Temperature(AuxData.Temperature.Msw.Msb) );
              randi_auxWrite(g_aux,FormatCounter,"CALIB. SUB-SYST.    TEMPERATURE",sTmp);
              }
            if (NewAuxData.BSqval)
              {
                if (Scansar)
                {
                   /*Jason Feature: this shall also be reported*/
                   Report(lvSummary,"Satellite works in Scansar-Mode");
                   sprintf(sTmp,"");
                   randi_auxWrite(g_aux,FormatCounter,"BEAM SEQUENCE",sTmp);
                   sTmp=BeamTable[AuxData.BeamSequence.Msb>>4];
                   Report(lvSummary,"First  Beam: %s",sTmp);
                   randi_auxWrite(g_aux,FormatCounter," - FIRST BEAM ",sTmp);
                   sTmp=BeamTable[AuxData.BeamSequence.Msb&15];
                   Report(lvSummary,"Second Beam: %s",sTmp);
                   randi_auxWrite(g_aux,FormatCounter," - SECOND BEAM ",sTmp);                 
                   sTmp=BeamTable[AuxData.BeamSequence.Lsb>>4];
                   Report(lvSummary,"Third  Beam: %s",sTmp);
                   randi_auxWrite(g_aux,FormatCounter," - THIRD BEAM",sTmp);                 
                   sTmp=BeamTable[AuxData.BeamSequence.Lsb&15];
                   Report(lvSummary,"Fourth Beam: %s",sTmp);
                   randi_auxWrite(g_aux,FormatCounter," - FOURTH BEAM",sTmp);                 
                }
                else 
                {
                   sTmp=BeamTable[AuxData.BeamSequence.Lsb & 0x1f];
		   /*Jason Feature: this shall also be reported*/
                   Report(lvSummary,"Satellite works in Stripmap-Mode: %s",sTmp);                   
                   randi_auxWrite(g_aux,FormatCounter,"BEAM SEQUENCE",sTmp);
                                   
                }   
              }
            if (NewAuxData.Eval)
              {  /*Data Words are counted like in the Manual REV F,p. 26 */
                sprintf(sTmp,"");
                randi_auxWrite(g_aux,FormatCounter,"EPHEMERIS",sTmp);
                iTmp=(int)(EphemerisData[3].Msb<<8)|(int)(EphemerisData[3].Lsb)|
                     (int)(EphemerisData[4].Msb<<24)|(int)(EphemerisData[4].Lsb<<16);
             /*   iTmp=~iTmp;
                iTmp++;  */   
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Revolution Number",sTmp);
                
                iTmp=(int)(EphemerisData[5].Msb<<8)|(int)(EphemerisData[5].Lsb);
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Year",sTmp);
                
                iTmp=(int)(EphemerisData[6].Msb<<8)|(int)(EphemerisData[6].Lsb);
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Day of Year",sTmp);
                
		iTmp=(int)(EphemerisData[7].Msb<<8)|(int)(EphemerisData[7].Lsb);
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Hours",sTmp);
                
                iTmp=(int)(EphemerisData[8].Msb<<8)|(int)(EphemerisData[8].Lsb);
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Minutes",sTmp);
                
                iTmp=(int)(EphemerisData[9].Msb<<8)|(int)(EphemerisData[9].Lsb);
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Seconds",sTmp);
                
                iTmp=(int)(EphemerisData[10].Msb<<8)|(int)(EphemerisData[10].Lsb);
                sprintf(sTmp,"%d",iTmp);
                randi_auxWrite(g_aux,FormatCounter," - Milliseconds",sTmp);
                
                doubleTmp=(double *) malloc (sizeof(double));
                liTmp=(long int)(EphemerisData[14].Msb<<56)|
                      (long int)(EphemerisData[14].Lsb<<48)|
                      (long int)(EphemerisData[13].Msb<<40)|
                      (long int)(EphemerisData[13].Lsb<<32)|
                      (long int)(EphemerisData[12].Msb<<24)|
                      (long int)(EphemerisData[12].Lsb<<16)|
                      (long int)(EphemerisData[11].Msb<<8)|
                      (long int)(EphemerisData[11].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));
                sprintf(sTmp,"%g radians",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Greenwich Angle",sTmp);
                
                liTmp=(long int)(EphemerisData[18].Msb<<56)|
                      (long int)(EphemerisData[18].Lsb<<48)|
                      (long int)(EphemerisData[17].Msb<<40)|
                      (long int)(EphemerisData[17].Lsb<<32)|
                      (long int)(EphemerisData[16].Msb<<24)|
                      (long int)(EphemerisData[16].Lsb<<16)|
                      (long int)(EphemerisData[15].Msb<<8)|
                      (long int)(EphemerisData[15].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));      
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Equinoctial Element A",sTmp);
                
                liTmp=(long int)(EphemerisData[22].Msb<<56)|
                      (long int)(EphemerisData[22].Lsb<<48)|
                      (long int)(EphemerisData[21].Msb<<40)|
                      (long int)(EphemerisData[21].Lsb<<32)|
                      (long int)(EphemerisData[20].Msb<<24)|
                      (long int)(EphemerisData[20].Lsb<<16)|
                      (long int)(EphemerisData[19].Msb<<8)|
                      (long int)(EphemerisData[19].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Equinoctial Element H",sTmp);
                      
                liTmp=(long int)(EphemerisData[26].Msb<<56)|
                      (long int)(EphemerisData[26].Lsb<<48)|
                      (long int)(EphemerisData[25].Msb<<40)|
                      (long int)(EphemerisData[25].Lsb<<32)|
                      (long int)(EphemerisData[24].Msb<<24)|
                      (long int)(EphemerisData[24].Lsb<<16)|
                      (long int)(EphemerisData[23].Msb<<8)|
                      (long int)(EphemerisData[23].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Equinoctial Element K",sTmp); 

                liTmp=(long int)(EphemerisData[30].Msb<<56)|
                      (long int)(EphemerisData[30].Lsb<<48)|
                      (long int)(EphemerisData[29].Msb<<40)|
                      (long int)(EphemerisData[29].Lsb<<32)|
                      (long int)(EphemerisData[28].Msb<<24)|
                      (long int)(EphemerisData[28].Lsb<<16)|
                      (long int)(EphemerisData[27].Msb<<8)|
                      (long int)(EphemerisData[27].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Equinoctial Element P",sTmp);
                
                liTmp=(long int)(EphemerisData[34].Msb<<56)|
                      (long int)(EphemerisData[34].Lsb<<48)|
                      (long int)(EphemerisData[33].Msb<<40)|
                      (long int)(EphemerisData[33].Lsb<<32)|
                      (long int)(EphemerisData[32].Msb<<24)|
                      (long int)(EphemerisData[32].Lsb<<16)|
                      (long int)(EphemerisData[31].Msb<<8)|
                      (long int)(EphemerisData[31].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));      
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Equinoctial Element Q",sTmp);
                
                liTmp=(long int)(EphemerisData[38].Msb<<56)|
                      (long int)(EphemerisData[38].Lsb<<48)|
                      (long int)(EphemerisData[37].Msb<<40)|
                      (long int)(EphemerisData[37].Lsb<<32)|
                      (long int)(EphemerisData[36].Msb<<24)|
                      (long int)(EphemerisData[36].Lsb<<16)|
                      (long int)(EphemerisData[35].Msb<<8)|
                      (long int)(EphemerisData[35].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Equinoctial Element L",sTmp);
                      
                liTmp=(long int)(EphemerisData[42].Msb<<56)|
                      (long int)(EphemerisData[42].Lsb<<48)|
                      (long int)(EphemerisData[41].Msb<<40)|
                      (long int)(EphemerisData[41].Lsb<<32)|
                      (long int)(EphemerisData[40].Msb<<24)|
                      (long int)(EphemerisData[40].Lsb<<16)|
                      (long int)(EphemerisData[39].Msb<<8)|
                      (long int)(EphemerisData[39].Lsb);
                memcpy(doubleTmp,&liTmp,sizeof(double));
                sprintf(sTmp,"%g",*doubleTmp);
                randi_auxWrite(g_aux,FormatCounter," - Drag",sTmp);                 
		
		floatTmp=(float *)malloc(sizeof(float));		
                iTmp=(int)(EphemerisData[44].Msb<<24)|
                     (int)(EphemerisData[44].Lsb<<16)|
                     (int)(EphemerisData[43].Msb<<8)|
                     (int)(EphemerisData[43].Lsb);
                memcpy(floatTmp,&iTmp,sizeof(float));
                sprintf(sTmp,"%g degrees",*floatTmp);
                randi_auxWrite(g_aux,FormatCounter," - Roll Bias",sTmp);  
                
                iTmp=(int)(EphemerisData[46].Msb<<24)|
                     (int)(EphemerisData[46].Lsb<<16)|
                     (int)(EphemerisData[45].Msb<<8)|
                     (int)(EphemerisData[45].Lsb);
                memcpy(floatTmp,&iTmp,sizeof(float));
                sprintf(sTmp,"%g  degrees",*floatTmp);
                randi_auxWrite(g_aux,FormatCounter," - Pitch Bias",sTmp);                  

                iTmp=(int)(EphemerisData[48].Msb<<24)|
                     (int)(EphemerisData[48].Lsb<<16)|
                     (int)(EphemerisData[47].Msb<<8)|
                     (int)(EphemerisData[47].Lsb);
                memcpy(floatTmp,&iTmp,sizeof(float));
                sprintf(sTmp,"%g degrees",*floatTmp);
                randi_auxWrite(g_aux,FormatCounter," - Yaw Bias",sTmp);  
              }
            if (NewAuxData.NBval)
              {
              sprintf(sTmp,"%1x",AuxData.NoOfBeams +1);
              randi_auxWrite(g_aux,FormatCounter,"NUMBER OF BEAMS",sTmp);
              }
            if (NewAuxData.ADCval)
              {
              switch (AuxData.ADCSamplingRate)
              {
              case 0:SampleRate=30.94;break;
	      case 1:SampleRate=54.15;break;
	      case 2:SampleRate=77.36;break;
              default:SampleRate=0;break;
              }
              sprintf(sTmp,"%lu x1000 Samples/s",(ulong)(1000000.0/SampleRate) );
              randi_auxWrite(g_aux,FormatCounter,"SAMPLING RATE",sTmp);
              }
            if (NewAuxData.PC1val)
              {
              sprintf(sTmp,"%x",AuxData.PulseCount1);
              randi_auxWrite(g_aux,FormatCounter,"PULSE COUNT  1",sTmp);
              }
	    if (NewAuxData.PC2val)
              {
              sprintf(sTmp,"%x",AuxData.PulseCount2);
              randi_auxWrite(g_aux,FormatCounter,"PULSE COUNT  2",sTmp);
              }
	    if (NewAuxData.PRFval)
              {
              iTmp=(int)(AuxData.PRFBeam1<<5) | (int)(AuxData.PRFBeam2);
              TimeUnit=6*SampleRate;
              fTmp=1/((2+ iTmp)* (TimeUnit*1e-9) );
              sprintf(sTmp,"%5.4f HZ",fTmp);
              randi_auxWrite(g_aux,FormatCounter,"PRF BEAM",sTmp);
              }
            if (NewAuxData.BSlval)
              {
              switch (AuxData.BeamSelect)
               {
                 case 0:sTmp="REGISTER A";break;
                 case 1:sTmp="REGISTER B";break;
                 case 2:sTmp="REGISTER C";break;
                 case 3:sTmp="REGISTER D";break;
               }  
              randi_auxWrite(g_aux,FormatCounter,"BEAM SELECT"   ,sTmp);
              }
            if (NewAuxData.RXSval)
              {
              iTmp=((int)(AuxData.RxWindowStartTime1<<4) |(int)( AuxData.RxWindowStartTime2));
              fTmp=(iTmp+5)* (TimeUnit*1e-9);
              sprintf(sTmp,"%g s",fTmp);
              randi_auxWrite(g_aux,FormatCounter,"RX WINDOW START    TIME",sTmp);
              }
            if (NewAuxData.RXDval)
              {
              iTmp=((int)(AuxData.RxWindowDuration1<<4) |(int)( AuxData.RxWindowDuration2));
              
              /*calculate the number of echo samples per scan:
               * RxWindowDurationTime/ADCIntervall = 
               * (code+1)*6*ADCIntervall/ADCIntervall =
               * (code+1)*6
               * read Rev. F,  p. 28, 31 and 36 (3.4.5.3.3)
               */
              SamplesPerScan=(iTmp+1) * 6;
               
              fTmp=(iTmp+1)* (TimeUnit*1e-9);
              sprintf(sTmp,"%g s",fTmp);
              randi_auxWrite(g_aux,FormatCounter,"RX WINDOW DURATION TIME",sTmp);
              }
            if (NewAuxData.Attval)
              {
              sprintf(sTmp,"");
              randi_auxWrite(g_aux,FormatCounter,"ATTITUDE",sTmp);
              if (!AuxData.Attitude[1]&1)
                sTmp="Roll Error is outside range";
              else {
                lfTmp=.35/32767*((uint)(AuxData.Attitude[0]<<7)|
                                (uint)(AuxData.Attitude[1]>>1))-0.175;
                sprintf(sTmp,"%1.8lf Rad",lfTmp);
                }
              randi_auxWrite(g_aux,FormatCounter," - ROLL ERROR ",sTmp);  
              if (!AuxData.Attitude[3]&1)
                sTmp="Pitch Error is outside range";
              else {
                lfTmp=.35/32767*((uint)(AuxData.Attitude[2]<<7)|
                                (uint)(AuxData.Attitude[3]>>1))-0.175;
                sprintf(sTmp,"%1.8lf Rad",lfTmp);
                }
              randi_auxWrite(g_aux,FormatCounter," - PITCH ERROR",sTmp);  
              if (!AuxData.Attitude[5]&1)
                sTmp="Yaw Error is outside range";
              else {
                lfTmp=.35/32767*((uint)(AuxData.Attitude[4]<<7)|
                                (uint)(AuxData.Attitude[5]>>1))-0.175;
                sprintf(sTmp,"%1.8lf Rad",lfTmp);
                }
              randi_auxWrite(g_aux,FormatCounter," - YAW ERROR  ",sTmp);  
              if (!AuxData.Attitude[7]&1)
                sTmp="Roll Rate is outside range";
              else {
                lfTmp=.035/32767*((uint)(AuxData.Attitude[6]<<7)|
                                (uint)(AuxData.Attitude[7]>>1))-0.0175;
                sprintf(sTmp,"%1.8lf Rad/s",lfTmp);
                }
              randi_auxWrite(g_aux,FormatCounter," - ROLL RATE  ",sTmp);  
              if (!AuxData.Attitude[9]&1)
                sTmp="Pitch Rate is outside range";
              else {
                lfTmp=.035/32767*((uint)(AuxData.Attitude[8]<<7)|
                                (uint)(AuxData.Attitude[9]>>1))-0.0175;
                sprintf(sTmp,"%1.8lf Rad/s",lfTmp);
                }
              randi_auxWrite(g_aux,FormatCounter," - PITCH RATE ",sTmp);  
              if (!AuxData.Attitude[11]&1)
                sTmp="Yaw Rate is outside range";
              else {
                lfTmp=.035/32767*((uint)(AuxData.Attitude[10]<<7)|
                                (uint)(AuxData.Attitude[11]>>1))-0.0175;
                sprintf(sTmp,"%1.8lf Rad/s",lfTmp);
                }
              randi_auxWrite(g_aux,FormatCounter," - YAW RATE   ",sTmp);  
              }
             if (NewAuxData.RAGCval)
               {
               if (AuxData.RxAGCSetting<32)
                 iTmp=AuxData.RxAGCSetting;
               else
                 iTmp=AuxData.RxAGCSetting-24;
               sprintf(sTmp,"%i dB",iTmp);
               randi_auxWrite(g_aux,FormatCounter,"RxAGCSetting",sTmp);
               }
	  } /* end of Inputs in Aux Data File */
  
	      
        /*Now the Data has to be decoded (extract the 4Bit I, 4Bit Q ...)
         * set the DataPointer to the beginning of the Application Data
         * field  
         */
	SizeOfApplDataField=alGetSizeOfApplDataField();
	BufferPointer=alDecodeApplicationData(SizeOfApplDataField);
        /*printf("p1:%p ",BufferPointer);*/
         
        /*each byte  (4 bit I, 4 bit Q) has been extracted to 2 bytes, because
         *of that all sizes *  2 
         */
        /* Replica data (each 8th Pulse) */
        SizeOfReplica=0;
        if (alIsRepData()) {
            g_luPulseReplicaRead++;
            SizeOfReplica=alGetSizeOfReplica();
            if (g_cmdLine.bRep) {
                repWriteScan(g_rep,FormatCounter,BufferPointer,SizeOfReplica * 2);
                }
            BufferPointer+= SizeOfReplica *2;
           /* printf("p2:%p ",BufferPointer);*/
            }

	/* Echo data - follows the Header or the ReplicaData*/
        g_luEchoScansRead++;
        /*if no aux data could be gained, SamplesPerScan is unknown,
         *store then the whole format*/
        if (SamplesPerScan)
          SizeOfEcho=SamplesPerScan;
        else SizeOfEcho=SizeOfApplDataField-SizeOfReplica;  
        if (SizeOfEcho > (SizeOfApplDataField-SizeOfReplica) )
            {
            Report(lvFormat,"Not enough Scans in Format %u, Format stored nevertheless\n",
                            FormatCounter);
            SizeOfEcho=SizeOfApplDataField-SizeOfReplica;
            }
        if (g_cmdLine.bEcho) 
        {
          echoWriteScan(g_echo,FormatCounter,BufferPointer,
                            SizeOfEcho * 2);
        }
        BufferPointer+= SizeOfEcho * 2;
        /*printf("p3:%p ",BufferPointer);*/
        
        /*Fill Data at the end of the format*/
        SizeOfFill=SizeOfApplDataField-SizeOfReplica-SizeOfEcho;
        if (SizeOfFill>0)
          {
          g_luZeroBlocksRead ++;
          if (g_cmdLine.bZero)
            {
            zeroWriteBlock(g_zero,FormatCounter,BufferPointer,
                             SizeOfFill * 2);
            }
          }
            
        /*Debug:
        printf(" Ech:%u appl:%u rep%u fill%u\n",SizeOfEcho,
            SizeOfApplDataField,SizeOfReplica,SizeOfFill);*/

	} /* end while loop */

Report(lvSummary,"LAST  FORMAT * Image-Scan-Number: %u               ",
        FormatCounter);
}

/**********************************************************************
* ReportStatistiks writes report about the scanning process
*/
void ReportStatistics()
{
Report(lvSummary,"------------- Total of %lu formats read ---------------",
							g_luFormatsRead);
Report(lvSummary,"      %8lu zero blocks",g_luZeroBlocksRead);
Report(lvSummary,"      %8lu pulse replicas",g_luPulseReplicaRead);
Report(lvSummary,"      %8lu echo scans",g_luEchoScansRead);
Report(lvSummary,"------------- Error sumarization -------------------");
Report(lvSummary,"      %8lu losses of synchronization",
							g_stat.loss.luCount);
Report(lvSummary,"      %8lu frames missed",g_stat.frame.luMissCount);
if (!g_cmdLine.bSave)
    Report(lvSummary,"      %8lu unsafe frames read",g_stat.frame.luUnsave);
Report(lvSummary,"      %8lu formats missed",g_stat.format.luImMissCount);
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



/*********************************************************
 * does the same as auxWrite, but checks before, if the output file
 * shall be produced
 */
int randi_auxWrite(File,ScanNumber,Entry,Value)
DRDFILE *File;
ulong ScanNumber;
char *Entry;
char *Value;
{
int back;

if  (g_cmdLine.bAux)
  {
  back=auxWrite(File,ScanNumber,Entry,Value);
  }
else 
  back=1;
return back;
}

