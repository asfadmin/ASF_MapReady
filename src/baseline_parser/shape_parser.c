#include <asf.h>
#include <stdio.h>
#include <string.h>

#define VERSION 1.0

void usage(char *name)
{
  printf("\n"
         "USAGE:\n"
         "   %s <srf>\n",name);
  printf("\n"
         "REQUIRED ARGUMENTS:\n"
	 "   srf    List of Scan Results Files.");
  printf("\n\n"
         "DESCRIPTION:\n"
         "   %s extracts the required information for the shape file creation"
	 " from a list of SRFs.\n",
         name);
  printf("\n"
         "Version %.2f, ASF SAR Tools\n"
         "\n",VERSION);
  exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
  FILE *fpList=NULL, *fpIn=NULL, *fpOut=NULL;
  int nSegments=0, nFrames=0;
  char infile[255], outfile[255], file[100], line[1024], mode[10];
  char sensor[5], granule_id[15], time[1000][30], cmd[255];
  int i, j, k, n=0, orbit[1000], frame[1000];
  float ns_lat[1000], fs_lat[1000], ne_lat[1000], fe_lat[1000];
  float ns_lon[1000], fs_lon[1000], ne_lon[1000], fe_lon[1000];

  /* Parse command line args */
  if ((argc-currArg) < 1) {
    printf("Insufficient arguments.\n"); 
    usage(argv[0]);
  }

  sprintf(infile, "%s", argv[1]);

  /* Open scan results file list */
  fpList = FOPEN(infile, "r");
  while (fgets(line, 1024, fpList)) {

    n=0;
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
		      sscanf(line, "    FRAME_ID = %i", &frame[n]);
                    else if (strstr(line, "CENTER_TIME"))
                      sscanf(line, "    CENTER_TIME = %s", time[n]);
		    else if (strstr(line, "NEAR_START_LAT"))
		      sscanf(line, "    NEAR_START_LAT = %f", &ns_lat[n]);
		    else if (strstr(line, "NEAR_START_LON"))
		      sscanf(line, "    NEAR_START_LON = %f", &ns_lon[n]);
		    else if (strstr(line, "FAR_START_LAT"))
		      sscanf(line, "    FAR_START_LAT = %f", &fs_lat[n]);
		    else if (strstr(line, "FAR_START_LON"))
		      sscanf(line, "    FAR_START_LON = %f", &fs_lon[n]);
		    else if (strstr(line, "NEAR_END_LAT"))
		      sscanf(line, "    NEAR_END_LAT = %f", &ne_lat[n]);
		    else if (strstr(line, "NEAR_END_LON"))
		      sscanf(line, "    NEAR_END_LON = %f", &ne_lon[n]);
		    else if (strstr(line, "FAR_END_LAT"))
		      sscanf(line, "    FAR_END_LAT = %f", &fe_lat[n]);
		    else if (strstr(line, "FAR_END_LON"))
		      sscanf(line, "    FAR_END_LON = %f", &fe_lon[n]);
                    else if (strstr(line, "REVOLUTION")) 
                      sscanf(line, "      REVOLUTION = %i", &orbit[n]);
		  } /* while inside OBJECT = FRAME */
                  n++;
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

        if (orbit[0]>=10000) {
          sprintf(outfile, "%s_%d_shape.in", mode, orbit[0]);
          sprintf(granule_id, "%s_%d_%s", sensor, orbit[0], mode);
        }
        else {
          sprintf(outfile, "%s_0%d_shape.in", mode, orbit[0]);
          sprintf(granule_id, "%s_0%d_%s", sensor, orbit[0], mode);
        }
        fpOut = FOPEN(outfile, "a");

        for (k=0; k<n; k++)
          fprintf(fpOut, "%s %s %5d %3d %s %12.6f %10.4f %10.4f %10.4f"
	                 " %10.4f %10.4f %10.4f %10.4f\n", sensor, mode, 
                         orbit[k], frame[k], time[k],
	                 ns_lat[k], ns_lon[k], fs_lat[k], fs_lon[k], 
                         ne_lat[k], ne_lon[k], fe_lat[k], fe_lon[k]);      

        FCLOSE(fpOut);

	} /* if within a SEGMENT object */
      } /* looping through a SEGMENT object (for) */
    } /* while reading an individual scan results file */
    FCLOSE(fpIn);

  } /* while reading through list of scan results files */

  FCLOSE(fpList);
  return (0);

} /* main */

