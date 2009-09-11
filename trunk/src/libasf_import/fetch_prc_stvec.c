/******************************************************************************
NAME:   fetch_prc - Reader routines for DLR precision orbit files

SYNOPSIS:

DESCRIPTION:

EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:   DATE:  AUTHOR:      PURPOSE:
    ---------------------------------------------------------------
    1.0     5/01   T. Logan     Read DLR precision orbit state vectors

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/

#include "asf.h"
#include "prc_stvecs.h"
#include "dateUtil.h"
#include "asf_meta.h"
#include <sys/stat.h>
#include <dirent.h>

void prc_date_time(PRC_REC *,float,ymd_date *,hms_time *);
void find_closest_prc_rec(char *file, ymd_date *seekDate, hms_time *seekTime,
                                stateVector *vec, float *offset_time);
void find_prc_file(char *prc_path,int requested_date, int orbit, char *prc_file);

int fetch_prc_stvec(char *prc_path, ymd_date *seekDate, hms_time *seekTime,
        stateVector *retVec, ymd_date *retDate, hms_time *retTime, int orbit)
{
  char         prc_file[256];
  stateVector  vec;
  float        offset_time;
  int          requested_date;

  asfPrintStatus("   Seek Date/Time: %.4i%.2i%.2iT%.2i%.2i%5.3f\n",seekDate->year,
                 seekDate->month, seekDate->day, seekTime->hour,
                 seekTime->min, seekTime->sec);

  requested_date = seekDate->year - (seekDate->year/100)*100;
  requested_date = requested_date*100+seekDate->month;
  requested_date = requested_date*100+seekDate->day;

  find_prc_file(prc_path,requested_date,orbit,prc_file);

  find_closest_prc_rec(prc_file, seekDate, seekTime, &vec, &offset_time);

  retVec->pos.x = vec.pos.x;
  retVec->pos.y = vec.pos.y;
  retVec->pos.z = vec.pos.z;
  retVec->vel.x = vec.vel.x;
  retVec->vel.y = vec.vel.y;
  retVec->vel.z = vec.vel.z;

  *retDate = *seekDate;
  *retTime = *seekTime;

  add_time(offset_time,retDate,retTime);

  return(1);
}

void find_prc_file(char *prc_path,int requested_date, int orbit, char *prc_file)
{
  struct stat buf;
  DIR    *dirp;
  struct dirent *dent;
  int    tmpRequest;
  int    minDiff = 1000000;
  char   saveName[256];

  /* Make sure given path is valid
   ------------------------------*/
  if (stat(prc_path, &buf)==-1) {
    perror("stat");
    exit(EXIT_FAILURE);
  }

  if (!(S_ISDIR(buf.st_mode)))
    asfPrintError("   ERROR: '%s' is not a directory\n",prc_path);

  /* Open the directory structure
   -----------------------------*/
  if ((dirp = opendir(prc_path))==NULL) {
    perror("opendir");
    exit(1);
  }

  tmpRequest = requested_date - ((requested_date/1000000)*1000000);

  while ((dent=readdir(dirp)) != 0) {
    char tmpName[256];
    int tmpDate;
    int tmpOrbit;

    strcpy(tmpName,dent->d_name);
    if (strncmp(tmpName,"PRC_",4)==0) {
      tmpDate = atoi(&(tmpName[4]));
      tmpOrbit = atoi(&(tmpName[11]));

      /* Calculation by orbit number */
      if (  (tmpOrbit < orbit) && ((orbit - tmpOrbit) < minDiff)) {
        minDiff = (orbit-tmpOrbit);
        strcpy(saveName,tmpName);
      }
    }
  }
  (void)closedir(dirp);
  asfPrintStatus("   Closest file found is %s\n",saveName);

  strcpy(prc_file,prc_path);
  strcat(prc_file,"/");
  strcat(prc_file,saveName);
}

void find_closest_prc_rec(char *prc_file, ymd_date *seekDate, hms_time *seekTime,
                                stateVector *vec, float *offset_time)
{
  int        currRec;
  STATE     *header;
  PRC_REC   *rec1 = NULL;
  PRC_REC   *rec2 = NULL;
  ymd_date   date, date1, date2;
  hms_time   time, time1, time2;
  float      diff1, diff2;
  int        startRec;         /* Marks start of earth-fixed state vectors */

  header = fetch_prc_header(prc_file);

  currRec = 0;

  /* Skip to the Earth-Fixed Data Records */
  asfPrintStatus("   Skipping to earth-fixed state vector records...\n");
  do {
    if (rec1!=(PRC_REC *) NULL) free(rec1);
    rec1 = fetch_prc_rec(prc_file,currRec);
    currRec++;
  }
  while (strncmp(rec1->reckey,"STTERR",6)!=0);
  startRec = currRec -= 1;

  /* Skip to the first state vector after seek time counting by 100 */
  asfPrintStatus("   Seeking to the first state vector after seek time...\n");
  do {
    free(rec1);
    rec1 = fetch_prc_rec(prc_file,currRec);
    prc_date_time(rec1, header->tdtutc, &date, &time);
    currRec+=100;
  }
  while (compare_time(&date,&time,seekDate,seekTime)==-1);
  currRec -=200;
  if (currRec < startRec) currRec = startRec;

  /* Skip to the first state vector after seek time counting by 1 */
  do {
    free(rec1);
    rec1 = fetch_prc_rec(prc_file,currRec);
    prc_date_time(rec1, header->tdtutc, &date, &time);
    currRec++;
  }
  while (compare_time(&date,&time,seekDate,seekTime)==-1);
  if (currRec < startRec) currRec = startRec+2;

  rec2 = rec1;
  rec1 = fetch_prc_rec(prc_file,currRec-2);

  prc_date_time(rec1, header->tdtutc, &date1, &time1);
  prc_date_time(rec2, header->tdtutc, &date2, &time2);

  diff1 = date_difference(&date1, &time1, seekDate, seekTime);
  diff2 = date_difference(&date2, &time2, seekDate, seekTime);

  if (diff1 < diff2) {
    *offset_time = -1.0*diff1;
    asfPrintStatus("   Closest state vector to requested time (offset = %lf):\n",
                   *offset_time);
    display_prc_rec_mod(rec1,header->tdtutc);
    vec->pos.x = rec1->xsat / 1000.0;
    vec->pos.y = rec1->ysat / 1000.0;
    vec->pos.z = rec1->zsat / 1000.0;
    vec->vel.x = rec1->xdsat / 1000000.0;
    vec->vel.y = rec1->ydsat / 1000000.0;
    vec->vel.z = rec1->zdsat / 1000000.0;
  }
  else {
    *offset_time = diff2;
    asfPrintStatus("   Closest state vector to requested time (offset = %lf):\n",
                   *offset_time);
    display_prc_rec_mod(rec2,header->tdtutc);
    vec->pos.x = rec2->xsat / 1000.0;
    vec->pos.y = rec2->ysat / 1000.0;
    vec->pos.z = rec2->zsat / 1000.0;
    vec->vel.x = rec2->xdsat / 1000000.0;
    vec->vel.y = rec2->ydsat / 1000000.0;
    vec->vel.z = rec2->zdsat / 1000000.0;
  }
}

PRC_REC *fetch_prc_rec(char *file, int recnum)
{
  FILE *fp;
  char buf[131];
  PRC_REC *tmp;
  int offset, i;
  char tmpbuf[20][20];

  /* Add null strings to end of temporary buffer */
  for (i=0; i< 20; i++) tmpbuf[i][19]='\0';

  /* Create structure & Add null characters to string fields */
  tmp = (PRC_REC *) MALLOC (sizeof(PRC_REC));
  tmp->reckey[6]='\0';
  tmp->spare[2]='\0';

  /* Open file, skip id and header, seek to recnum record */
  fp = FOPEN(file,"r");
  offset = 130 + 130 + 130 * recnum;
  if (fseek(fp,offset,0)!=0)
    asfPrintError("No state vector found - read past end of file \n");

  /* Read the current record */
  FREAD(buf,130,1,fp);

  FCLOSE(fp);

  /* Convert the buffer */
  sscanf(buf,"%6c%7c%c%6c%11c%12c%12c%12c%11c%11c%11c%6c%6c%6c%2c%3c%c%4c%2c",
         tmp->reckey, tmpbuf[0], &tmp->orbtyp, tmpbuf[1],tmpbuf[2],
         tmpbuf[3], tmpbuf[4], tmpbuf[5], tmpbuf[6], tmpbuf[7], tmpbuf[8],
         tmpbuf[9], tmpbuf[10], tmpbuf[11], tmpbuf[12], tmpbuf[13],
         &tmp->quali, tmpbuf[14], tmp->spare);

  /* Convert tmpbuf strings into numeric values */
  tmp->satid  = atoi(tmpbuf[0]);
  tmp->ttagd  = atof(tmpbuf[1]) / 10.0;   /* Units are 0.1 days */
  tmp->ttagms = strtoll(tmpbuf[2], (char **)NULL, 10);
  tmp->xsat   = strtoll(tmpbuf[3], (char **)NULL, 10);
  tmp->ysat   = strtoll(tmpbuf[4], (char **)NULL, 10);
  tmp->zsat   = strtoll(tmpbuf[5], (char **)NULL, 10);
  tmp->xdsat  = strtoll(tmpbuf[6], (char **)NULL, 10);
  tmp->ydsat  = strtoll(tmpbuf[7], (char **)NULL, 10);
  tmp->zdsat  = strtoll(tmpbuf[8], (char **)NULL, 10);
  tmp->roll   = atof(tmpbuf[9])/1000.0;   /* Units are 0.001 degrees */
  tmp->pitch  = atof(tmpbuf[10])/1000.0;  /* Units are 0.001 degrees */
  tmp->yaw    = atof(tmpbuf[11])/1000.0;  /* Units are 0.001 degrees */
  tmp->ascarc = atoi(tmpbuf[12]);
  tmp->check  = atoi(tmpbuf[13]);
  tmp->radcor = atoi(tmpbuf[14]);

  return(tmp);
}


void display_prc_rec(PRC_REC *tmp)
{
  asfPrintStatus("Record Key            : %s\n",tmp->reckey);
  asfPrintStatus("Satellite Id          : %i\n",tmp->satid);
  asfPrintStatus("Orbit Type            : %c\n",tmp->orbtyp);
  asfPrintStatus("Julian Days since 1/1/2000 12h in TDT : %f\n",tmp->ttagd );
  asfPrintStatus("Microseconds since 0:00 TDT           : %lli\n",tmp->ttagms );
  asfPrintStatus("X-Coordinate          : %lli\n",tmp->xsat);
  asfPrintStatus("Y-Coordinate          : %lli\n",tmp->ysat);
  asfPrintStatus("Z-Coordinate          : %lli\n",tmp->zsat);
  asfPrintStatus("X-Velocity            : %lli\n",tmp->xdsat);
  asfPrintStatus("Y-Velocity            : %lli\n",tmp->ydsat);
  asfPrintStatus("Z-Velocity            : %lli\n",tmp->zdsat);
  asfPrintStatus("Roll                  : %f\n",tmp->roll);
  asfPrintStatus("Pitch                 : %f\n",tmp->pitch);
  asfPrintStatus("Yaw                   : %f\n",tmp->yaw);
  asfPrintStatus("Ascending Flag        : %i\n",tmp->ascarc);
  asfPrintStatus("Checksum              : %i\n",tmp->check);
  asfPrintStatus("Quality Flag          : %c\n",tmp->quali );
  asfPrintStatus("Radial Orbit Correction              : %i\n",tmp->radcor);
  asfPrintStatus("Spare                 : %s\n",tmp->spare);
}

void display_prc_rec_mod(PRC_REC *tmp, float tdtutc_offset)
{
  double          dtmp;
  hms_time        tmp_time;
  ymd_date        tmp_date;
  double          offset;

  /* tmp_time to hold 1/1/2000 12h
   ------------------------------*/
  tmp_time.hour = 12;
  tmp_time.min = 0;
  tmp_time.sec = 0.0;

  tmp_date.year = 2000;
  tmp_date.month = 1;
  tmp_date.day = 1;

  /* offset by given julian days
   ----------------------------*/
  offset = tmp->ttagd * 86400.0;
  if (offset < 0.0)       sub_time(-1.0*offset,&tmp_date,&tmp_time);
  else if (offset > 0.0)  add_time(offset,&tmp_date,&tmp_time);

  /* offset by given second of day
   ------------------------------*/
  offset = tmp->ttagms/1000000.0;
  if (offset < 0.0)       sub_time(-1.0*offset,&tmp_date,&tmp_time);
  else if (offset > 0.0)  add_time(offset,&tmp_date,&tmp_time);

  sub_time(tdtutc_offset,&tmp_date,&tmp_time);

  asfPrintStatus("   Date/Time of state vector: %.4i%.2i%.2iT%.2i%.2i%5.3f\n",
                 tmp_date.year, tmp_date.month, tmp_date.day,
                 tmp_time.hour, tmp_time.min,tmp_time.sec);


  dtmp = tmp->xsat / 1000.0;
  asfPrintStatus("   X-Coordinate          : %lf\n",dtmp);
  dtmp = tmp->ysat / 1000.0;
  asfPrintStatus("   Y-Coordinate          : %lf\n",dtmp);
  dtmp = tmp->zsat / 1000.0;
  asfPrintStatus("   Z-Coordinate          : %lf\n",dtmp);
  dtmp = tmp->xdsat / 1000000.0;
  asfPrintStatus("   X-Velocity            : %lf\n",dtmp);
  dtmp = tmp->ydsat / 1000000.0;
  asfPrintStatus("   Y-Velocity            : %lf\n",dtmp);
  dtmp = tmp->zdsat / 1000000.0;
  asfPrintStatus("   Z-Velocity            : %lf\n",dtmp);
}

void prc_date_time(PRC_REC *tmp,float tdtutc_offset, ymd_date *date, hms_time *time)
{
  hms_time  tmp_time;
  ymd_date  tmp_date;
  double    offset;

  /* tmp_time to hold 1/1/2000 12h
   ------------------------------*/
  tmp_time.hour = 12;
  tmp_time.min = 0;
  tmp_time.sec = 0.0;

  tmp_date.year = 2000;
  tmp_date.month = 1;
  tmp_date.day = 1;

  /* offset by given julian days
   ----------------------------*/
  offset = tmp->ttagd * 86400.0;

  if (offset < 0.0)       sub_time(-1.0*offset,&tmp_date,&tmp_time);
  else if (offset > 0.0)  add_time(offset,&tmp_date,&tmp_time);

  /* offset by given second of day
   ------------------------------*/
  offset = tmp->ttagms/1000000.0;

  if (offset < 0.0)       sub_time(-1.0*offset,&tmp_date,&tmp_time);
  else if (offset > 0.0)  add_time(offset,&tmp_date,&tmp_time);

  sub_time(tdtutc_offset,&tmp_date,&tmp_time);

  date->year = tmp_date.year;
  date->month = tmp_date.month;
  date->day = tmp_date.day;
  time->hour = tmp_time.hour;
  time->min = tmp_time.min;
  time->sec = tmp_time.sec;
}

STATE *fetch_prc_header(char *file)
{
        FILE *fp;
  char buf[131];
  STATE *tmp;
  int i;
  char tmpbuf[10][7];

  /* Add null strings to end of temporary buffer */
  for (i=0; i< 10; i++) tmpbuf[i][6]='\0';

  /* Create structure & Add null characters to string fields */
  tmp = (STATE *) MALLOC (sizeof(STATE));
  tmp->reckey[6]='\0';
  tmp->obstyp[6]='\0';
  tmp->obslev[6]='\0';
  tmp->cmmnt[78]='\0';

  /* Open file & skip over id record */
  fp = FOPEN(file,"r");
  FREAD(buf,130,1,fp);

  /* Read the header record */
  FREAD(buf,130,1,fp);
  FCLOSE(fp);

  /* Convert the input buffer */
  sscanf(buf,"%6c%6c%6c%6c%6c%2c%2c%4c%4c%4c%c%5c%78c",
      tmp->reckey, tmpbuf[0], tmpbuf[1], tmp->obstyp,
      tmp->obslev, tmpbuf[2], tmpbuf[3], tmpbuf[4],
      tmpbuf[5], tmpbuf[6], &tmp->qualit, tmpbuf[7], tmp->cmmnt);

  /* Convert tmpbuf strings into numeric values */
  tmp->startDate = atof(tmpbuf[0])/10.0;  /* Units are 0.1 days */
  tmp->endDate = atof(tmpbuf[1])/10.0;    /* Units are 0.1 days */
  tmp->modid = atoi(tmpbuf[2]);
  tmp->relid = atoi(tmpbuf[3]);
  tmp->rmsfit = atoi(tmpbuf[4]);
  tmp->sigpos = atoi(tmpbuf[5]);
  tmp->sigvel = atoi(tmpbuf[6]);
  tmp->tdtutc = atof(tmpbuf[7])/1000.0;   /* Units are 0.001 secs */

  return(tmp);
}

void display_prc_header(STATE *tmp)
{
  asfPrintStatus("Record Key            : %s\n",tmp->reckey);
  asfPrintStatus("Start Date            : %f\n",tmp->startDate);
  asfPrintStatus("End Date              : %f\n",tmp->endDate);
  asfPrintStatus("Observation Type      : %s\n",tmp->obstyp);
  asfPrintStatus("Observation Level     : %s\n",tmp->obslev);
  asfPrintStatus("Model Identifier      : %i\n",tmp->modid);
  asfPrintStatus("Release Identifier    : %i\n",tmp->relid);
  asfPrintStatus("RMS Fit               : %i\n",tmp->rmsfit);
  asfPrintStatus("Sigma of sat. Position: %i\n",tmp->sigpos);
  asfPrintStatus("Sigma of sat. Velocity: %i\n",tmp->sigvel);
  asfPrintStatus("Quality Flag          : %c\n",tmp->qualit);
  asfPrintStatus("Time diff. (TDT-UTC)  : %f\n",tmp->tdtutc);
  asfPrintStatus("Comments              : %s\n",tmp->cmmnt);
}

DSIDP *fetch_prc_id(char *file)
{
  FILE *fp;
  char buf[131];
  DSIDP *tmp;

  tmp = (DSIDP *) MALLOC (sizeof(DSIDP));

  /* Add null characters to string fields */
  tmp->reckey[6]='\0';
  tmp->prodid[15]='\0';
  tmp->dattyp[6]='\0';
  tmp->spare[103]='\0';

  fp = FOPEN(file,"r");
  FREAD(buf,130,1,fp);
  FCLOSE(fp);

  sscanf(buf,"%6c%15c%6c%103c",tmp->reckey, tmp->prodid, tmp->dattyp, tmp->spare);

  return(tmp);
}

void display_prc_id(DSIDP *tmp)
{
  asfPrintStatus("Record Key            : %s\n",tmp->reckey);
  asfPrintStatus("Product Id            : %s\n",tmp->prodid);
  asfPrintStatus("Data Type             : %s\n",tmp->dattyp);
  asfPrintStatus("Spare                 : %s\n",tmp->spare);
}
