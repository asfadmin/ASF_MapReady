/**************************
RADARSAT: Auxiliary data utilites.  RADARSAT sends down useful
pulse replicas and has a SCANSAR mode, so this is the most complex
auxiliary data.
*/


/*This decoded auxiliary data structure is created by decodeAux,
called by readNextFrame.
*/
typedef struct {
	int payloadStatus;/*Payload status word*/
	int isImaging; /*1 if satellite is sending out pulses*/
	double replicaAGC,rxAGC;/*Replica and reciever attenuation, in dB.*/
	int replicaLen,rxLen;
	int hasReplica;
	int nBeams;
	int beams[4];/*Beam ID codes.*/
	char *beamNames[4];
	int beam;
	double timeBase;/*1.0/ADC Sampling rate, in seconds.*/
	int prf_code;
	double prf;
	double windowDuration;/*Recieve window start and duration, in sec.*/
	int dwp_code;
	double dwp;
	int days;/*Days (since launch?)*/
	double seconds;/*Seconds since midnight GMT*/
} RSAT_aux;

void RSAT_decodeAux(unsigned char *in,RSAT_aux *out);


#define RSAT_bytesPerFrame 323
#define RSAT_datPerAux 50 /*Bytes of auxiliary data (aux) frame.*/
#define RSAT_datPerFrame 311 /*Bytes of echo data in each (non-aux) frame.*/

typedef struct {
/*These fields are read directly from the file*/
	unsigned char sync[4];/*Synchronization word: 0x1acffc1d*/
	unsigned char id[2];/*Frame identification*/
	unsigned char count[2];/*Master/Virtual channel frame count*/
	unsigned char status[2];/*Frame data field status*/
	unsigned char data[RSAT_datPerFrame];/*Data in frame*/
	unsigned char crc[2];/*Cyclic Redundancy Check word*/
	
/*These fields are computed in readNextFrame*/
	int is_aux;
	int is_zero;
	int is_echo;
	int beam;/*The beam for this frame.*/
	int hasReplica;/*Line contains valid pulse replica.*/

/*These fields are only valid if this is an auxiliary frame,
  and are set by readNextFrame*/
	unsigned char raw[RSAT_datPerAux]; /*RSAT auxiliary raw data block*/
	RSAT_aux aux;/*Decoded auxiliary data*/
} RSAT_frame;
RSAT_frame * RSAT_readNextFrame(bin_state *s,RSAT_frame *f);


int RSAT_auxHasReplica(RSAT_aux *aux);
int RSAT_auxGetBeam(RSAT_aux *aux);
int RSAT_auxIsImaging(RSAT_aux *aux);

void RSAT_auxUpdate(RSAT_aux *aux,bin_state *s); 
void RSAT_auxPrint(RSAT_aux *aux,FILE *f);
void RSAT_auxAGC_window(bin_state *s,RSAT_aux *aux);




