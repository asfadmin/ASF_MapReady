FILE *rsv_open(char *fName,ymd_date *date,hms_time *time,char *satName,int *isPredicted);
int rsv_next(FILE *inF,int *orbitNo,ymd_date *date,hms_time *time,stateVector *stVec);
void rsv_close(FILE *inF);
