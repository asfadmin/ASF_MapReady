/*The ESA Header occupies 30 bytes on disk.*/
#define ESA_HEADER_LEN 30
typedef struct 
{
	char fileID[5];/*'ORRE_'*/
	char genDate[6];/*Generation date: YYMMDD*/
	char orig[2];/*Originating facility ('CF')*/
	char dest[2];/*Receiving facility ('AF')*/
	char counter[4];/*Cyclic counter.*/
	char period;/*Always '.'*/
	char sat[2];/*'E1'-- ERS-1; 'E2'-- ERS-2.*/
	char genTime[8];/*Generation time: HH:MM:SS*/
} esa_header;


/*Each ESA state vector occupies 40 bytes on disk.*/
#define ESA_STVEC_LEN 40
typedef struct
{
	char orbit[5];/*Orbit number ('  601')*/
	char tai_utc[3];/*TAI-UTC Difference, in seconds (' 00').*/
	unsigned char time_mjd[4];/*Little-endian integer number of days since 1950.*/
	unsigned char time_msec[4];/*Little-endian integer number of milliseconds since midnight.*/
#define ESA_STVEC_POS 100.0 /*Position conversion factor.*/
	unsigned char stVec_pos[3][4];/*State vector array-- position X, Y, Z.*/
#define ESA_STVEC_VEL 100000.0 /*Velocity conversion factor.*/
	unsigned char stVec_vel[3][4];/*State vector array-- velocity X, Y, Z.*/
} esa_stVec;


void esa_init_header(esa_header *h,char *satellite,ymd_date *date,hms_time *time,int isPredicted);
void esa_print_header(esa_header *h);

void esa_init_stVec(esa_stVec *v,stateVector *s,int orbit,ymd_date *date,hms_time *time);
void esa_print_stVec(esa_stVec *v);
