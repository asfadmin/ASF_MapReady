/**************************
ERS: Auxiliary Data utilities.  Nothing special here.

*/

/*Auxiliary data starts 6 bytes from frame start, and has this format:*/
typedef struct 
{
	Uchar idhtPacket:8;
	Uchar idhtSubCount:8;
	Uchar idht[8];
	Uchar formatCode:8;
	Uchar orbitNo:8;
	struct SAT_ULONG onBoardTime;
	Uchar activity:8;
	Uchar flags:8;
	struct SAT_ULONG formatCount;
	struct SAT_USHORT swst;
	struct SAT_USHORT prf;
	Uchar calAtten:8;
	Uchar rfAtten:8;
} ERS_raw_aux;

/*This is the decoded auxiliary data structure*/
typedef struct
{
	int isImaging;   /*Is satellite sending out pulses?*/
	unsigned int onBoardTime; /*On board clock time*/
	unsigned int formatCount; /*Line counter*/
	double dwp,prf;  /*Data Window Position and Pulse Repetition Freqency (s & Hz)*/
	int dwp_code,prf_code; /*Binary codes for above*/
	int rfAtten; /*Reciever attenuation code*/
} ERS_aux;


#define ERS_bytesPerFrame 256
#define ERS_framesPerLine 29
#define ERS_datPerAux 230 /*Bytes of auxiliary data per (aux) frame*/
#define ERS_datPerFrame 250 /*Bytes of echo data per (non-aux) frame*/

typedef struct {
/*These fields are read straight from the file*/
	Uchar sync[3];/*3 bytes of synchronization code*/
	Uchar type;/*ESA "frame type" byte*/
	Uchar pulseNoHi;/*High byte of pulse count*/
	Uchar pulseNoLo;/*Low byte of pulse count*/
	Uchar data[250];/*Data in frame*/
/*These fields are computed in readNextFrame*/
	int is_aux;/*Frame has auxiliary data*/
	int is_zero;/*Frame is all zeros (for padding?)*/
	int is_echo;/*Frame describes part of an echo*/
/*If this is an auxiliary frame, the following structures contain values*/
	ERS_raw_aux *raw;/*points into data array above*/
	ERS_aux aux;/*Decoded auxiliary data*/	
} ERS_frame;
ERS_frame * ERS_readNextFrame(bin_state *s,ERS_frame *f);

void ERS_decodeAux(ERS_raw_aux *in,ERS_aux *out);
void ERS_auxUpdate(ERS_aux *aux,bin_state *s); 
void ERS_auxPrint(ERS_aux *aux,FILE *f);
void ERS_auxAGC_window(bin_state *s,ERS_aux *aux);




