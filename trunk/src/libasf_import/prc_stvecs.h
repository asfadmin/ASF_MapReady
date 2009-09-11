/*
	D-PAF Precise Orbit Foramt Records
*/

/******** Dataset Identification Record *************************/
typedef struct {
	char 	reckey[7];	/* Record Key = 'DSIDP' */
	char 	prodid[16];	/* Product Id 		*/
	char 	dattyp[7];	/* Data Type  		*/
	char 	spare[104];	/* Spare space 		*/
} DSIDP;

/******** Data Header Record ************************************/
typedef struct {
	char 	reckey[7];	/* Record Key = 'STATE' 		*/
	float 	startDate;	/* Start date, days since 1/1/2000 12h 	*/
	float 	endDate;	/* End date, days since 1/1/2000 12h   	*/
	char 	obstyp[7];	/* Observation type 			*/
	char 	obslev[7];	/* Observation level 			*/
	short  	modid;		/* Model Identifier 			*/
	short  	relid;		/* Release Identifier 			*/ 
	int    	rmsfit;		/* Rms-Fit of Orbit (mm) 		*/
	int    	sigpos;		/* Sigma of satellite position (mm) 	*/
	int    	sigvel;		/* Sigma of satellite velocity (E-6 m/s) */
	char  	qualit;		/* Quality flag 			*/
	float  	tdtutc;		/* Time difference TDT-UTC 		*/
	char   	cmmnt[79];	/* Comments 				*/
} STATE;

/********* Trajectory Record ************************************/
typedef struct {
	char 	reckey[7];		/* Record key = 'STINER' 		*/
	int	satid;			/* Satellite Id 			*/
	char	orbtyp;			/* Orbit type 				*/
	float	ttagd;			/* Julian days since 1/1/2000 12h in TDT */
	long long ttagms;		/* Microseconds since 0:00 TDT 		*/
	long long xsat, ysat, zsat;	/* X,Y,Z Satellite Coordinates (mm) 	*/
	long long xdsat, ydsat, zdsat;	/* X,Y,Z Satellite Velocity (E-6 m/s) 	*/
	float	roll, pitch, yaw;	/* Pointing info (degree) 		*/
	short	ascarc;			/* ascending state vector flag 		*/
	short	check;			/* Checksum 				*/
	char	quali;			/* Quality flag 			*/
	short	radcor;			/* Radial Orbit Correction 		*/
	char	spare[3];		/* Spares 				*/
} PRC_REC;


DSIDP *fetch_prc_id(char *file);
STATE *fetch_prc_header(char *file); 
PRC_REC *fetch_prc_rec(char *file, int recnum);
void display_prc_rec(PRC_REC *tmp);
void display_prc_header(STATE *tmp);
void display_prc_id(DSIDP *tmp);
void display_prc_rec_mod(PRC_REC *tmp, float tdtutc_offset);
