
FILE *tce_open(char *fName);
int tce_next(FILE *inF,int *orbitNo,ymd_date *date,hms_time *time,int *satClock,int *satMs);
void tce_close(FILE *inF);
