/* andi.h
* header for the Analysis and Decode Tool 
*       Hans-Joerg Wagner
*/
 
/* do not process header twice */
#ifndef __ANDI.H__
#define __ANDI.H__

/* about this program */
#define PROGRAM		"ANDI Analysis and Decode tool for ERS1 raw data"
#define FACILITY	"Alaska SAR Facility"
#define MAJOR_VERSION	1
#define MINOR_VERSION	0
#define SATELLITE	"ERS-1"

#ifndef UNSIGNED_TYPES
#define UNSIGNED_TYPES
typedef unsigned long   ulong;
typedef unsigned short  ushort;
typedef unsigned int    uint;
typedef unsigned char   uchar;
#endif /*UNSIGNED_TYPES */

/* structures and makros to access integers system independent */
struct SAT_USHORT {
        uchar Msb;
        uchar Lsb;
        };
#define GET_SAT_USHORT(x) ( ((ushort)((x).Msb) << 8) | ((ushort)((x).Lsb)) )
 
struct SAT_ULONG {
        struct SAT_USHORT Msw;
        struct SAT_USHORT Lsw;
        };
#define GET_SAT_ULONG(x) ( ((ulong)GET_SAT_USHORT((x).Msw) << 16) \
                                | ((ulong)GET_SAT_USHORT((x).Lsw)) )

struct SIGNED_LONG {
	uchar sign:1;
	uchar MswMsb:7;
	uchar MswLsb;
	uchar LswMsb;
	uchar LswLsb;
	};

#define GET_EPH_SIGNED_LONG(x)  \
	( ((x).sign) ? \
	  (- ((x).LswLsb+((x).LswMsb<<8)+((x).MswLsb<<16)+((x).MswMsb<<24)) ) \
	  : ((x).LswLsb+((x).LswMsb<<8)+((x).MswLsb<<16)+((x).MswMsb<<24)) )

/**********************************************************************
* data exchange structures
*/
/* structure for sync status, included after removing the
* Synchronization found - lost messages out of fra.c and moving them
* to foa.c to reduce the frequency. Now they are just displayed if a
* frame is missed
*/
struct STAT {
	int bSynchron;
	enum { lvSummary = 0, lvFormat, lvFrame, lvSync } level;
	struct {
		ulong luCount;
		ulong luFileOffset;
		} loss;
	struct {
		ulong	luFileOffset;
		int	nBitOffset;
		} found;
	struct {
		ulong luMissCount;
		ulong luUnsave;
		} frame;
	struct {
		ulong luMissCount;
		ulong luImMissCount;
		} format;
	};

/**********************************************************************
* structures of data format of the raw data
*/
 
/* structure of frame */
struct frame {
        char sync[3];
        uchar fc:2;
        uchar frameNo:6;
        struct SAT_USHORT formCycler;
        char data[250];
        };

/* structure of subcommutated IDHT GH */
struct IDHTGHSUB {
        uchar nPacket;
        uchar nSubcom;
        char cSubData[8];
        };

/* structure of ephemeris data */
struct EPHEMERIS {
	uchar 			dummy:2;
	uchar 			counter:6;
	struct SAT_ULONG	timeSat;
	struct SIGNED_LONG	rx;
	struct SIGNED_LONG	ry;
	struct SIGNED_LONG	drx;
	struct SIGNED_LONG	dry;
	struct SIGNED_LONG	drz;
	};
	
/* structure of IDHT General Header */
union IDHTGH {
	uchar aucSubData[48][8];
	struct {
		uchar ucPacketID1;
		uchar ucPacketID2;
		struct SAT_USHORT iPacketSequenceControl;
		struct SAT_USHORT iLengthOfPacket;
		uchar aucPlatformData[256];
		struct EPHEMERIS sEphemeris;
		uchar dummy[97];
		} s;
	};

/* structure of format */
union format {
        unsigned char ucFrameData[29][250];
	struct {
		struct IDHTGHSUB dummy;
		uchar zeroData[29 * 250 - 8];
		} s1;
        struct {
                struct IDHTGHSUB idhtGhSubcom;
                uchar formatCode;
                uchar ogrc:1;
                uchar orbit:4;
                uchar dummy1:3;
                struct SAT_ULONG luBoardTime;
                uchar activity;
                uchar bEchoValid:1;
                uchar bCalRepValid:1;
                uchar bFirstNoise:1;
                uchar bFirstCalRep:1;
                uchar bFirstEcho:1;
                uchar dummy2:3;
                struct SAT_ULONG luImageFormatCount;
                struct SAT_USHORT uSamplWinStart;
                struct SAT_USHORT uPulseRepInterval;
                uchar dummy3:1;
                uchar uCalSubsysAtten:5;
                uchar dummy4:1;
                uchar bLoopStatus:1;
                uchar dummy5:2;
                uchar uReceiverAtten:5;
                uchar dummy6:1;
                char dummyData[130];
                uchar calRep[72];
                uchar echoNoise[7020];
/*flag if zero data!derived by program from frame info! */
/*not part of origin format*/
                int bZero;
                } s;
        };

/**********************************************************************
* definitions
*/
/* definition of buffer sizes */
#define PATTERN_SIZE 3
#define BUFFER_LEN ( 8*256 + PATTERN_SIZE + 1)
#define FRAME_SIZE sizeof(struct frame)
#define SIZE_OF_REPCAL	768
#define SIZE_OF_ECHO	5616

/**********************************************************************
* global variables
*/
/* syncrhonization status of bitwise scaning */
extern struct STAT g_stat;

extern FILE *fESAHeaders;

/**********************************************************************
* prototypes
*/
 
/* prototypes frame wise access level*/
int fraInit();
void fraExit();
char fraShiftBuffer();
char fraGetShiftedChar();
void fraPrintBufHex();
void fraPrintBufBin();
int fraFindSyncInBuffer();
int fraFindSync();
int fraFillBuffer();
char *fraFindNextFrame();
ulong fraGetNextFrame();
void fraDescramble();
uint fraGetRelativeFilePosition();

/* prototypes format wise access level */
int foaInit();
void foaExit();
ulong foaGetNextFormat();

/* access level interface*/
int alInit();
void alExit();
int alNextBlock();
void alDecodeRepCal();
void alDecodeNoiseEcho();
char alGetNoiseEchoByteAt();
void alAnalyzeIdht();

struct EPHEMERIS *alGetOrbitInfo();
int alGetCurrentOrbit();
double alGetBoardTime();
ulong alGetImageFormatCounter();
double alGetSampleWinStartTime();
double alGetPulseRepInterval();
int alGetCalSubsysAtten();
int alGetReceiverSubsysAtten();
uchar *alGetZeroData();
int alIsCalData();
char *alGetCalData();
int alIsRepData();
char *alGetRepData();
int alIsNoiseData();
char *alGetNoiseData();
int alIsEchoData();
char *alGetEchoData();

/* main program prototypes */
long Hello(); /* compiler does not understand time_t!! */
void GoodBye();
void Usage();
int InitLib();
void ExitLib();
void WriteHardInfo();
void WriteComInfo();
void Scan();
void Report();
void ReportStatistics();
 
#endif  /* __ANDI.H__ */

