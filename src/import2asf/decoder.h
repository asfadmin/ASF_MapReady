#include "asf.h"
#include "geolocate.h"
#include "asf_meta.h"

#include "config.h"

typedef enum {
	sat_unknown=10,
	sat_ers=11,
	sat_jrs=12,
	sat_rsat=13
} sat_type;
sat_type determine_satellite(const char *binName);


typedef unsigned char iqType;
typedef unsigned char signalType;

/*This is the "binary decoding state" structure*/
typedef struct {
/*Metadata*/
	char satName[100];			/* Satellite name*/
	char beamMode[4];		/* Mode of the sensor */
	int zeroDopSteered;		/* Is the satellite steered to zero doppler?*/
	int nPulseInAir; 		/* Number of pulses in the air at one time.*/
	int nSamp; 			/* Number of samples in a line of data.*/
	int nBeams;			/* Number of beams used (1 except for ScanSAR)*/
	double slope;			/* Range chirp slope (Hz/sec)*/
	double frequency;		/* Satellite carrier freqency (Hz) */
	double fs;			/* Range sampling freqency (Hz) */
	double prf;			/* Pulse Repetition Freqency (Hz) */
	int prf_code;			/* Satellite binary code for PRF*/
	double dwp;			/* Delay between PRF trigger and recieve window start (s)*/
	int dwp_code;			/* Satellite binary code for DWP*/
	double range_gate;		/* Delay between chirp transmission and reception (s) */
	double time_code;		/* Satellite time code at first line: 
					   ERS-- ticks (32-bit counter, 256 ticks/sec); 
					   JRS-- seconds (27-bit counter, 0==mission start?); 
					   RSAT-- seconds past GMT midnight*/

/*Fields used during processing*/
	FILE *binary;			/* Binary file pointer (read)*/
	int curFrame;			/* Zero-based number of current frame*/
	int nFrames;			/* Number of frames in the file */
	int bytesPerFrame;		/* Number of bytes in a single satellite frame*/
	long long bytesInFile;		/* Number of bytes in entire file*/
	void *missing;			/* "Missing" data pointer (see missing.c)*/
	int readStatus;			/* Status of current data: 0 - bad, 1 - good */
	
/*Fields filled out/used by lz/ceos2raw during processing*/
	int nValid;			/* Number of valid output samples (nSamp-replica length)*/
	double estDop;			/* Estimated doppler, in PRF (based on nominal pointing).*/
	double I_BIAS,Q_BIAS; 		/* DC bias for I and Q channels.*/
	
	double re;			/* Local earth radius (m) */
	double vel;			/* Satellite orbital velocity (m/s) */
	double ht;			/* Satellite height above earth's surface (m)*/
	double pulsedur;		/* Satellite range pulse duration (s)*/
	char lookDir;			/* SAR Look Direction (L or R)*/
	double azres;			/* SAR Azimuth resolution (m) */
	int nLooks;			/* Number of looks in azimuth to make square pixels */

	FILE *dotFMT;			/* ASCII .fmt file pointer (write)*/	
	FILE *fperr;			/* Output error log file pointer (write) */
	FILE *fpOut;			/* Output raw SAR data file (write) */
	int nLines;			/* Number of lines written so far */

/*Fields used by lops_qc during processing*/
#define MAX_BEAMS 4 /*Largest number of beams on any satellite*/
	void *firstFrame[MAX_BEAMS];/*The first auxiliary frame in the file, decoded*/
} bin_state;

bin_state *new_bin_state(void);
void delete_bin_state(bin_state *s);
/*
The global satellite structure above is initialized
by new_satellite_ptr; called by the ####_decoder_init() 
routines below.  It is read-only to all other routines.
*/

/*Internal routine: write the default values for this satellite
to the given binary_parameters structure*/
void RSAT_init(bin_state *s);
void JRS_init(bin_state *s);
void ERS_init(bin_state *s);

/*External interface: read the next echo from the given binary file*/
typedef void (*readPulseFunc)(bin_state *s,iqType *iqBuf, char *inN, char *outN);

bin_state *RSAT_decoder_init(char *inN,char *outN,readPulseFunc *reader);
bin_state *JRS_decoder_init(char *inN,char *outN,readPulseFunc *reader);
bin_state *ERS_decoder_init(char *inN,char *outN,readPulseFunc *reader);

bin_state *RSAT_ceos_decoder_init(char *inN,char *outN,readPulseFunc *reader);
bin_state *JRS_ceos_decoder_init(char *inN,char *outN,readPulseFunc *reader);
bin_state *ERS_ceos_decoder_init(char *inN,char *outN,readPulseFunc *reader);


/******************************** Metadata-type utilities
updateAGC_window:
	Writes the given AGC value (floating-point amplitude
amplification that should be applied to this image) and 
window position (starting offset of this row in pixels) to
the .fmt file.  Uses s->dotFMT and s->nLines.
*/
void updateAGC_window(bin_state *s,float amplify,float startOff);

/*This is a routine in ceos2raw used to read a line of CEOS data.*/
FILE *openCeos(char *fName, char *outN, bin_state *s);
signalType *getNextCeosLine(FILE *f, bin_state *s, char *inN, char *outN);

/*Write the bin_state * fields into the given meta_parameters*/
void updateMeta(bin_state *s,meta_parameters *meta);
void addStateVector(bin_state *s,stateVector *stVec);


/*AISP Utilities.*/
void writeAISPparams(bin_state *s,char *outN, double fd, double fdd, double fddd);
void writeAISPformat(bin_state *s,char *outN);


/********************************* Raw bit-twiddling,decoding utilities.
*/
void check_sync(const unsigned char *sync,int nBytes,const unsigned char *trueVal);
float db2amp(float dB);
double realRand(void);/*Return a "random" number between 0 and 1*/
void scaleSamples(bin_state *s,iqType *buf,int start,int num,float factor);
int num_bit_errors(unsigned char a,unsigned char b);
unsigned char *createBitErrorTable(void);
void extractBits(signalType *in,int bitStart,int nBytes,signalType *out);

/*Data unpacking utilities*/
void RSAT_unpackCeosBytes(signalType *in,int nIn,iqType *out);
iqType *RSAT_unpackBytes(signalType *in,int nIn,iqType *out);
iqType *JERS_unpackBytes(signalType *in,int nIn,iqType *out);
iqType *ERS_unpackBytes(signalType *in,int nIn,iqType *out);







