/**************************
JERS: Auxiliary data utilities.  JRS has a 
"Sensitivity Time Control" (stc) system, which is a range
antenna pattern compensation applied at the satellite.
*/

/*The JERS Auxiliary data is 69+24 bits long, called H/K data
in the documentation, and is stored in both the I and Q bit streams.
It starts in each stream after the 30 bit sync code.*/
typedef struct {
/*69 bits (8 5/8 bytes) Housekeeping (H/K) data*/
	unsigned char prfOn:1;
	unsigned char prfCode:3;
	unsigned char calOn:1;
	unsigned char obsOn:1;
	unsigned char stcChangePattern1:2;
	unsigned char stcChangePattern2:3;
	unsigned char stcInitialStart:5;
	unsigned char stcStart:5;
	unsigned char stcOffset:3;
	unsigned char agcOn:1;
	unsigned char agcTime:1;
	unsigned char agcAtten:5;
	unsigned char gcStatus1:1;
	unsigned char gcStatus2:4;
	unsigned char stpAtt:3;
	unsigned char autoOn:1;
	unsigned char observationStart:1;
	unsigned char combiner:3;
	unsigned char receiverSw:1;
	unsigned char stcCal:1;
	unsigned char calSw:1;
	unsigned char standbyCode1:1;
	unsigned char standbyCode2:1;
	unsigned char agcChange:1;
	unsigned char freqOn:1;
	unsigned char controlOn:1;
	unsigned char transmitterOn:1;
	unsigned char receiverOn:1;
	unsigned char signalProcessorOn:1;
	unsigned char hpaAOn:1;
	unsigned char hpaBOn:1;
	unsigned char hpaROn:1;
	unsigned char stcInitial:1;
	unsigned char strOn:1;
	unsigned char strCode1:3;
	unsigned char strCode2:5;
/*24-bit (3-byte) Line (==frame) counter*/
	unsigned char lineCountHi;
	unsigned char lineCountMid;
	unsigned char lineCountLo;
	unsigned char pad[10];
} JRS_raw_aux;

typedef struct {
	int dwp_code,prf_code;
	double dwp,prf;/*Data window position (sec), pulse repetition freqency (Hz)*/
	int stcOffset;/*Start of STC, in 10's of microseconds*/
	int agc_dB;/*Automatic Gain control attenuation, in dB*/
	int lineCount;/*3-byte line counter*/
} JRS_aux;

void JRS_auxDecode(JRS_raw_aux *in,JRS_aux *out);

/*Extract a JRS auxiliary data record from the given signal data*/
void JRS_auxUnpack(signalType *in,JRS_raw_aux *aux);
void JRS_auxCeosUnpack(signalType *in,JRS_raw_aux *aux);


/*JRS has no "frames" per se-- it's just a seqence of (bit-packed) echoes
for a single line, preceeded by the auxilary data.
The format for "data" is:
2 streams, I and Q.  Streams are bit-interleaved (I, Q, I, Q, ...)

Each stream contains:
{
	-30 bits of sync code
	-96 bits (12 bytes) of H/K data (above)
	-24 bit (3 byte) line counter (above)
	
	12 times:
	{
		-3 bits of PCM code
		-1536 bits (192 bytes) containing 512 3-bit echo samples
	}
}
*/

#define JRS_bytesPerFrame 4660

typedef struct {
/*These fields are read straight from the file*/
	unsigned char data[JRS_bytesPerFrame];/*bit-packed fields for one echo line*/
	
/*These fields are computed in readNextFrame*/
	JRS_raw_aux raw;/*Raw auxiliary data for this line (always present)*/
	JRS_aux aux;/*Decoded auxiliary data for this line (always present)*/
} JRS_frame;
JRS_frame * JRS_readNextFrame(bin_state *s,JRS_frame *f);


int JRS_auxStc(JRS_aux *aux);
void JRS_auxUpdate(JRS_aux *aux,bin_state *s);
void JRS_auxPrint(JRS_aux *aux,FILE *f);
void JRS_auxAGC_window(bin_state *s,JRS_aux *aux);


/****************************************
JRS PCM data utilities
*/

typedef struct {
/*Bitwise-oversampled values for data and clock lines:*/
#define PCM_bit_wrap 256
#define PCM_bit_mask 0xff
	int end,len;/*Circular buffer end pointer and length*/
	unsigned char data_bits[PCM_bit_wrap];/*Circular bit data buffer*/
	unsigned char clk_bits[PCM_bit_wrap];/*Circular bit clock buffer*/
	int prevTrough;/*Previous clock trough index*/
	
	int nBits;/*Number of bits stored in frames, below*/
	unsigned char *pcmBits;/*Raw PCM bit samples (2048 baud bit stream).*/
	int startBit;/*Bit offset in pcmBits array to PCM frames*/
	int nFrames;/*Number of 128-byte bit-aligned frames below*/
	unsigned char *frames;/*Data extracted from PCM bit samples (frame sync'd).*/
} JRS_PCM;


JRS_PCM *new_JRS_PCM(int maxFrames);
void delete_JRS_PCM(JRS_PCM *p);

/*Add a single (at least 8x over-) sampling of the data and clock
lines to the circular buffer.

Note: dataVal and clockVal represent a single bit, but need not 
actually be 0 or 1-- they can be 0 and 15, or 0 and 3, or whatever.
Analog signals (0,1,2, or 3) are even OK.
*/
void JRS_PCM_add_sample(JRS_PCM *p,int clockVal,int dataVal);

/*Find the bit-sync codes in the pcmBits array, and extract
the frames.*/
void JRS_PCM_sync(JRS_PCM *p);

/*Write the binary PCM frames to the given file*/
void JRS_PCM_write(JRS_PCM *p,const char *fName);

/*Extract the time of the first telemetry line 
from the cached PCM frames*/
double JRS_PCM_time(JRS_PCM *p);

