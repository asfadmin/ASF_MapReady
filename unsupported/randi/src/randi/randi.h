/* randi.h                        
* header for the Analysis and Decode Tool 
*       Hans-Joerg Wagner
* 
* RADARSAT Modification
* by Christian Fischer
* 09-25-95
*
*/
 
/* do not process header twice */
#ifndef __ANDI.H__
#define __ANDI.H__

/* about this program */
#define PROGRAM		"RANDI Analysis and Decode tool for RADARSAT raw data"
#define FACILITY	"Alaska SAR Facility"
#define MAJOR_VERSION	1
#define MINOR_VERSION	0
#define SATELLITE	"RADARSAT"

#ifndef UNSIGNED_TYPES
#define UNSIGNED_TYPES
typedef unsigned long   ulong;
typedef unsigned short  ushort;
typedef unsigned int    uint;
typedef unsigned char   uchar;
#endif /*UNSIGNED_TYPES */

/**********************************************************************
* definitions
*/
/* definition of buffer sizes */
#define PATTERN_SIZE 4  /*Length of Frame Synchronisation Marker*/
#define AUX_DATAFIELD_LENGTH 50  /* Bytes*/
#define DATAFIELD_LENGTH  311 /* Length of Application Data Field*/
#define FRAME_SIZE sizeof(struct frame)
#define BUFFER_LEN ( 8*FRAME_SIZE + PATTERN_SIZE + 1)
#define MAX_FRAME_NO 64  /* max processible No of Frames in a Format, should
                            be large enough */
#define NoOfEphWords 55




/**********************************************************************
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
        char sync[PATTERN_SIZE];
        struct SAT_USHORT frameID;
        uchar MCCount;
        uchar VCCount;
        struct SAT_USHORT frameStatus;
        char data[DATAFIELD_LENGTH];
        struct SAT_USHORT CRC;
        };

struct AUX  {             /*REV F S. 17 */
               uchar AuxSyncMarker[4];
               uchar ImageReferenceIdentifier[4];
               struct SAT_USHORT PayloadStatus;
               uchar ReplicaAGC:6;
               uchar Spare1:2;
               uchar CALNAttenuatorSetting;
               uchar PulseWaveformNo:4;
               uchar Spare2:4;
               uchar Spare3:8;
               struct SAT_ULONG Temperature;
               struct SAT_USHORT BeamSequence;
               struct SAT_USHORT Ephemeris;
               uchar NoOfBeams:2;
               uchar ADCSamplingRate:2;
               uchar Spare4:4;
               uchar PulseCount1;
               uchar PulseCount2;
               uchar PRFBeam1;
               uchar PRFBeam2:5;
               uchar BeamSelect:2;
               uchar Spare5:1;
               uchar RxWindowStartTime1;
               uchar RxWindowStartTime2:4;
               uchar Spare6:4;
               uchar RxWindowDuration1;
               uchar RxWindowDuration2:4;
               uchar Spare7:4;
               uchar Attitude[12];
               uchar Time[6];
               uchar SCTO2Defaults:1;
               uchar ReplicaPresent:1;
               uchar RxAGCSetting:6;
               } ;

	

/* structure of format */
union format {
        unsigned char ucFrameData[MAX_FRAME_NO][DATAFIELD_LENGTH];
        struct AUX s;             /*REV F S. 17 */
      };

struct AUXDATAVALID{
	int PSval;
	int Rval;
	int Attenval;
	int PWval;
	int Tempval;
	int BSqval;
	int Eval;
	int NBval;
	int ADCval;
	int PC1val;
	int PC2val;
	int PRFval;
	int BSlval;
	int RXSval;
	int RXDval;
	int Attval;
	int RAGCval;
      };

/**********************************************************************
* global variables
*/
/* syncrhonization status of bitwise scaning */
extern struct STAT g_stat;
extern int g_RewriteTime;
extern int g_CareFree;
/**********************************************************************
* prototypes
*/
 
/* prototypes frame wise access level*/
int fraInit();
uint fraGetRelativeFilePosition();
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
int fraIfFirstFrame();
void fraDescramble();

/* prototypes format wise access level */
int foaInit();
void foaExit();
ulong foaGetNextFormat();
ulong foaGetNextFormat_CareFree();
uint foaGetFormatCount();
void foaCalculateFormatCount();
uchar foaGetNoOfFrames();
char *foaTag();
void foaReportMiss();

/* access level interface*/
int alInit();
void alExit();
int alNextBlock();
void alGetAuxData();
char * alDecodeApplicationData();
int alIsReData();
uint alGetSizeOfReplica();
uint alGetSizeOfApplDataField();



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
int randi_auxWrite();
 
#endif  /* __ANDI.H__ */

