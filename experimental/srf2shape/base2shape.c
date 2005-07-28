#include <asf.h>
#include <asf_meta.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <dateUtil.h>

#define VERSION 1.0

void check_return(int ret, char *msg)
{
        if (ret==-1) {
          sprintf(errbuf, "\n   ERROR: %s\n\n", msg);
          printErr(errbuf);
        }
}

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s  <base>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   base    List of baseline parsing files.");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s creates shape file out of baseline parsing files.\n",
         name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fpList=NULL, *fpBase=NULL, *fpShape=NULL, *fpXmlIn=NULL, *fpXmlOut=NULL;
  char infile[255], shapeFile[255], baseFile[255], xmlFile[255], line[1024];
  char base[1024], shape[1024], *date, title[30];
  char bla[30], mode[10], sensor[10], name[100], cmd[1024], direction;
  char month[5], day[5], master_time[30], slave_time[30];
  long n=0;
  int frame, master_orbit, master_sequence, slave_orbit, slave_sequence;
  int ii, track, test_frame[1000], b_par, b_perp, b_temp;
  float near_start_lat[100000], near_start_lon[100000], far_start_lat[100000], far_start_lon[100000];
  float near_end_lat[100000], near_end_lon[100000], far_end_lat[100000], far_end_lon[100000];
  float center_lat, center_lon, westbc, eastbc, northbc, southbc;
  time_t t;
  ymd_date ymd;
  hms_time hms;

  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  sprintf(infile, "%s", argv[1]);

  /* Open baseline parse file list */
  fpList = FOPEN(infile, "r");
  while (fgets(line, 1024, fpList)) {

    n = 0;
    strncpy(name, line, 19);
    name[9] = '\0';

    /* Create shape and database files */
    sprintf(cmd, "shpcreate %s polygon", name);
    system(cmd);
    sprintf(cmd, "cp template.dbf %s.dbf", name);
    system(cmd);

    /* Take care of file names */
    sprintf(shapeFile, "%s_shape.in", name);
    sprintf(baseFile, "%s.out", name);
    sprintf(xmlFile, "%s.shp.xml", name);

    /* Read the corresponding coordinates from shape parse file */
    fpShape = FOPEN(shapeFile, "r");
    while (fgets(shape, 1024, fpShape)) {
      sscanf(shape, "%s %s %i %i %s %f %f %f %f %f %f %f %f",
             sensor, mode, &master_orbit, &test_frame[n], bla, &near_start_lat[n], &near_start_lon[n],
             &far_start_lat[n], &far_start_lon[n], &near_end_lat[n], &near_end_lon[n], &far_end_lat[n], &far_end_lon[n]);
      n++;
    }
    FCLOSE(fpShape);
    track = master_orbit-(master_orbit/343)*343;

    /* Extract information out of each baseline parse file on the list */
    fpBase = FOPEN(baseFile, "r");
    while (fgets(base, 1024, fpBase)) {

      /* Read baseline information */
      sscanf(base, "%s %i %i %i %s %i %i %s %i %i %i %f %f", 
             mode, &frame, &master_orbit, &master_sequence, master_time, &slave_orbit, &slave_sequence, 
             slave_time, &b_par, &b_perp, &b_temp, &center_lat, &center_lon);
      if (frame>=225 && frame<=675) direction = 'D';
      else direction = 'A';
      parse_odlTime(master_time, &ymd, &hms);
      if (ymd.month>9)
        sprintf(month, "%i", ymd.month);
      else
        sprintf(month, "0%i", ymd.month);
      if (ymd.day>9)
        sprintf(day, "%i", ymd.day);
      else
        sprintf(day, "0%i", ymd.day);
      sprintf(master_time, "%i%s%s", ymd.year, month, day);
      parse_odlTime(slave_time, &ymd, &hms);
      if (ymd.month>9)
        sprintf(month, "%i", ymd.month);
      else
        sprintf(month, "0%i", ymd.month);
      if (ymd.day>9)
        sprintf(day, "%i", ymd.day);
      else
        sprintf(day, "0%i", ymd.day);
      sprintf(slave_time, "%i%s%s", ymd.year, month, day);

      /* Keep adding shapes to file as long as they are frames */
      for (ii=0; ii<n; ii++) 
        if (frame == test_frame[ii]) {
          sprintf(cmd, "shpadd %s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f", name,
  	          near_start_lon[ii], near_start_lat[ii], far_start_lon[ii], far_start_lat[ii], far_end_lon[ii], far_end_lat[ii], 
	          near_end_lon[ii], near_end_lat[ii], near_start_lon[ii], near_start_lat[ii]);
          system(cmd);
          sprintf(cmd, "dbfadd %s.dbf %s %s %i %i %c %i %s %i %i %s %i %i %i %i %9.4f %9.4f",
	          name, sensor, mode, frame, track, direction, master_orbit, master_time, master_sequence, 
		  slave_orbit, slave_time, slave_sequence, b_par, b_perp, b_temp, center_lat, center_lon);
          system(cmd);
          break;
        }
    } 

    FCLOSE(fpBase); 

    /* Update XML metadata file */
    date = (char*) MALLOC(25*sizeof(char));
    t = time(NULL);
    strftime(date, 9, "%Y%m%d", localtime(&t));
    if (master_orbit >= 10000)
      sprintf(title, "%s_%i baselines", mode, master_orbit);
    else
      sprintf(title, "%s_0%i baselines", mode, master_orbit);
    westbc = 180.0;
    eastbc = -180.0;
    northbc = -90.0;
    southbc = 90.0;
    for (ii=0; ii<n; ii++) {
      if (near_start_lat[ii] < southbc) southbc = near_start_lat[ii];
      if (near_end_lat[ii] < southbc) southbc = near_end_lat[ii];
      if (far_start_lat[ii] < southbc) southbc = far_start_lat[ii];
      if (far_end_lat[ii] < southbc) southbc = far_end_lat[ii];
      if (near_start_lat[ii] > northbc) northbc = near_start_lat[ii];
      if (near_end_lat[ii] > northbc) northbc = near_end_lat[ii];
      if (far_start_lat[ii] > northbc) northbc = far_start_lat[ii];
      if (far_end_lat[ii] > northbc) northbc = far_end_lat[ii];
      if (near_start_lon[ii] > eastbc) eastbc = near_start_lon[ii];
      if (near_end_lon[ii] > eastbc) eastbc = near_end_lon[ii];
      if (far_start_lon[ii] > eastbc) eastbc = far_start_lon[ii];
      if (far_end_lon[ii] > eastbc) eastbc = far_end_lon[ii];
      if (near_start_lon[ii] < westbc) westbc = near_start_lon[ii];
      if (near_end_lon[ii] < westbc) westbc = near_end_lon[ii];
      if (far_start_lon[ii] < westbc) westbc = far_start_lon[ii];
      if (far_end_lon[ii] < westbc) westbc = far_end_lon[ii];
    }
    fpXmlIn = FOPEN("template.xml", "r");
    fpXmlOut = FOPEN(xmlFile, "w");
    
    while (fgets(line, 1024, fpXmlIn)) {
      if (strncmp(line, "<pubdate>", 9) == 0) {
	sprintf(cmd, "<pubdate>%s</pubdate>\n", date); 
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<title>", 7) == 0) {
	sprintf(cmd, "<title>%s</title>\n", title);
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<westbc>", 8) == 0) {
	sprintf(cmd, "<westbc>%.4f</westbc>\n", westbc);
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<eastbc>", 8) == 0) {
	sprintf(cmd, "<eastbc>%.4f</eastbc>\n", eastbc);
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<northbc>", 8) == 0) {
	sprintf(cmd, "<northbc>%.4f</northbc>\n", northbc);
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<southbc>", 8) == 0) {
	sprintf(cmd, "<southbc>%.4f</southbc>\n", southbc);
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<procdate>", 10) == 0) {
	sprintf(cmd, "<procdate>%s</procdate>\n", date); 
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<ptvctcnt>", 10) == 0) {
	sprintf(cmd, "<ptvctcnt>%s</ptvctcnt>\n", date); 
	fprintf(fpXmlOut, "%s", cmd);
      }
      else if (strncmp(line, "<metd>", 6) == 0) {
	sprintf(cmd, "<metd>%s</metd>\n", date); 
	fprintf(fpXmlOut, "%s", cmd);
      }
      else
	fprintf(fpXmlOut, "%s", line);
    }
    
    FCLOSE(fpXmlOut);
    FCLOSE(fpXmlIn);

    sprintf(cmd, "tar cf %s.tar %s.shp %s.dbf %s.shx %s.shp.xml", name, name, name, name, name);
    system(cmd);
    sprintf(cmd, "gzip -f %s.tar", name);
    system(cmd);
  }

  FCLOSE(fpList);
  return (0);

} /* main */

