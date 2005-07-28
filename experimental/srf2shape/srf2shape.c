#include <asf.h>
#include <stdio.h>
#include <string.h>

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
         "   %s  <srf>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
         "   srf    List of Scan Results Files.");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s extracts the latitude and longitude information"
         " for all corner coordinates of a frame from a list of SRFs.\n",
         name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fpList=NULL, *fpIn=NULL;
  int nSegments=0, nFrames=0;
  char infile[255], file[100], line[1024], time[30];
  char mode[10], sensor[10], granule_id[25], name[25], cmd[1024];;
  int i, j, orbit, frame;
  float near_start_lat, near_start_lon, far_start_lat, far_start_lon;
  float near_end_lat, near_end_lon, far_end_lat, far_end_lon;

  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  sprintf(infile, "%s", argv[1]);

  /* Open scan results file list */
  fpList = FOPEN(infile, "r");
  while (fgets(line, 1024, fpList)) {

    strncpy(file, line, 19);
    /* Extract information out of each scan results file on the list */
    fpIn = FOPEN(file, "r");
    while (fgets(line, 1024, fpIn)) {

      /* Look for platform */
      if (strstr(line,  "PLATFORM")) {
        sscanf(line, "  PLATFORM = \"%s\"", sensor);
        sensor[2] = '\0';
      }

      /* Look for segments */
      if (strstr(line, "SEGMENT_COUNT")) 
        sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
      for (i=0; i<nSegments; i++) {
        if (strstr(line, "OBJECT = SEGMENT") &&
            strncmp(line, "  OBJECT = SEGMENT", 18)==0) {

          /* Check beam mode */
          fgets(line, 1024, fpIn);
          fgets(line, 1024, fpIn);
          if (strstr(line, "MODE")) {
            sscanf(line, "   MODE = \"%s\"", mode);
            mode[3] = '\0';
          }

          /* Look for frames */
          while (fgets(line, 1024, fpIn)) {
            if (strstr(line, "FRAME_COUNT"))
              sscanf(line, "   FRAME_COUNT = %i", &nFrames);
            for (j=0; j<nFrames; j++) {
              while (fgets(line, 1024, fpIn)) {
                if (strstr(line, "OBJECT = FRAME") &&
                  strncmp(line, "   OBJECT = FRAME", 17)==0) {
                  while (fgets(line, 1024, fpIn)) {
                    if (strstr(line, "END_OBJECT = FRAME") &&
                        strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
                    else if (strstr(line, "FRAME_ID"))
                      sscanf(line, "    FRAME_ID = %i", &frame);
                    else if (strstr(line, "CENTER_TIME"))
                      sscanf(line, "    CENTER_TIME = %s", time);
                    else if (strstr(line, "NEAR_START_LAT"))
                      sscanf(line, "    NEAR_START_LAT = %f", &near_start_lat);
                    else if (strstr(line, "NEAR_START_LON"))
                      sscanf(line, "    NEAR_START_LON = %f", &near_start_lon);
                    else if (strstr(line, "FAR_START_LAT"))
                      sscanf(line, "    FAR_START_LAT = %f", &far_start_lat);
                    else if (strstr(line, "FAR_START_LON"))
                      sscanf(line, "    FAR_START_LON = %f", &far_start_lon);
                    else if (strstr(line, "NEAR_END_LAT"))
                      sscanf(line, "    NEAR_END_LAT = %f", &near_end_lat);
                    else if (strstr(line, "NEAR_END_LON"))
                      sscanf(line, "    NEAR_END_LON = %f", &near_end_lon);
                    else if (strstr(line, "FAR_END_LAT"))
                      sscanf(line, "    FAR_END_LAT = %f", &far_end_lat);
                    else if (strstr(line, "FAR_END_LON"))
                      sscanf(line, "    FAR_END_LON = %f", &far_end_lon);
                    else if (strstr(line, "REVOLUTION")) {
                      sscanf(line, "      REVOLUTION = %i", &orbit);
                      if (orbit >= 10000) {
                        sprintf(granule_id, "%s_%d_%s", sensor, orbit, mode);
                        sprintf(name, "%s_%d", mode, orbit);
                      }
                      else {
                        sprintf(granule_id, "%s_0%d_%s", sensor, orbit, mode);
                        sprintf(name, "%s_0%d", mode, orbit);
                      }
                    }

                  } /* while inside OBJECT = FRAME */

                    if (j == 0) {
                      sprintf(cmd, "shpcreate %s polygon", name);
                      system(cmd);
                      sprintf(cmd, "dbfcreate %s.dbf -s Sensor 5 -s Beam_Mode 5 -n Orbit 6 0 -n Frame 4 0 -s Granule_ID 15 -s Date 30 " \
			      "-n Lat_1 9 4 -n Lon_1 9 4 -n Lat_2 9 4 -n Lon_2 9 4 -n Lat_3 9 4 -n Lon_3 9 4 -n Lat_4 9 4 -n Lon_4 9 4", name);
                      system(cmd);
                    }
		    sprintf(cmd, "shpadd %s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f", name,
			    near_start_lat, near_start_lon, far_start_lat, far_start_lon, near_end_lat, near_end_lon, 
			    far_end_lat, far_end_lon, near_start_lat, near_start_lon);
                    system(cmd);
                    sprintf(cmd, "dbfadd %s.dbf %s %s %i %i %s %s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f",
			    name, sensor, mode, orbit, frame, granule_id, time, near_start_lat, near_start_lon, 
			    far_start_lat, far_start_lon, near_end_lat, near_end_lon, far_end_lat, far_end_lon);
                    system(cmd);

                  if (strstr(line, "END_OBJECT = FRAME") &&
                      strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
                } /* if within a FRAME object */
                if (strstr(line, "END_OBJECT = FRAME") &&
                    strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		
              } /* while looking for FRAME objects */

            } /* looping through all the frames (for) */
            
          } /* while inside OBJECT = SEGMENT */
          if (strstr(line, "END_OBJECT = SEGMENT") &&
              strncmp(line, "  END_OBJECT = SEGMENT", 22)==0) break;
        } /* if within a SEGMENT object */
      } /* looping through a SEGMENT object (for) */
    } /* while reading an individual scan results file */

    FCLOSE(fpIn);
  } /* while reading through list of scan results files */

  FCLOSE(fpList);
  return (0);

} /* main */

