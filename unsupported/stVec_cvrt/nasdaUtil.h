typedef struct {
	char date[14];/*MJD day (JD-2400000.5) / UTC fraction of day (decimal).
		Use: %14.8f*/
} nasda_date;

typedef struct {
	char offset[8];/*Byte offset from beginning of file.*/
	char num[4];/*Number of records.*/
} nasda_loc;

#define NASDA_HEADER_LEN 256
typedef struct {
	char fname[8];/*'ELMF    '*/
	char satName[4];/*'ERS1'*/
	char from[4];/*'HMMO'*/
	char to[4];/*'FAIS'*/
	char date[8];/*YYYYMMDD*/
	char time[8];/*HH:MM:SS*/
	char hasFdesc;/*'Y'*/
	char recLen[4];/*Record length (0085)*/
	char noRec[5];/*Number of records to follow.*/
	char spare[82];/*Spaces.*/
/*File Descriptor (for hasFdesc):*/
	char noTCE[3];/*Number of time correction elements.*/
	nasda_date orbitStart;/*Beginning of orbit data.*/
	nasda_date orbitEnd;/*End of orbit data.*/
	char updated[8];/*YYYYMMDD*/
	nasda_loc D[6];/*Data element descriptors.*/
	char spare2[17];
} nasda_header;

#define NASDA_TCE_LEN 85
typedef struct {
	char regDate[8];/*YYYYMMDD*/
	char presetDate[8];/*YY-MM-DD*/
	char space;
	char presetTime[12];/*hh:mm:ss.ttt*/
	char satTime[10];/*Satellite time counter.*/
	char timeErr[5];/*Satellite time error (ms).*/
	char passDate[8];/*YY-MM-DD*/
	char space2;
	char passTime[12];/*hh:mm:ss.ttt*/
	char pathNo[3];
	char spare[17];
} nasda_tce;

#define NASDA_STVEC_LEN 85
typedef struct {
	nasda_date time;
	char pos[3][13];/*Inertial Position, Km (%13.6f)*/
	char vel[3][10];/*Inertial Velocity, Km/s (%10.6f)*/
	char spare[2];
} nasda_stVec;

void nasda_createDate(ymd_date *date,hms_time *time,nasda_date *out);

void nasda_printHeader(nasda_header *h,int *nTCE,int *nStVec);
void nasda_initHeader(nasda_header *h,ymd_date *date,hms_time *time,nasda_date *orbitStart,nasda_date *orbitEnd,int nStVec,int nTCE);

void nasda_initTCE(nasda_tce *t,ymd_date *date,hms_time *time,int msOff,
	ymd_date *preset_date,hms_time *preset_time,int presetOff);
void nasda_printTCE(nasda_tce *t);

void nasda_printStVec(nasda_stVec *v);
void nasda_initStVec(nasda_stVec *v,stateVector *s,nasda_date *date);
