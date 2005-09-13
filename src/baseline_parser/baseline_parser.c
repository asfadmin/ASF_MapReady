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
         "   %s extracts the required information for interferometric baseline"
	 " calculation from a list of SRFs.\n",
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
  int i, j, k, m, n=0, rev;
  char time[85000][30];
  int track[85000], orbit[85000], frame[85000], sequence[85000];
  float x[85000], y[85000], z[85000], vx[85000], vy[85000], vz[85000];
  float lat[85000], lon[85000], slant_range[85000], fd[85000];

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

      /* Look for sequence number */
      if (strstr(line, "SEQUENCE"))
        sscanf(line, "  SEQUENCE = %i", &sequence[n]);

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
		    else if (strstr(line, "CENTER_LAT"))
		      sscanf(line, "    CENTER_LAT = %f", &lat[n]);
		    else if (strstr(line, "CENTER_LON"))
		      sscanf(line, "    CENTER_LON = %f", &lon[n]);
		    else if (strstr(line, "SL_RNG_MID_PIX"))
		      sscanf(line, "    SL_RNG_MID_PIX = %f", &slant_range[n]);
		    else if (strstr(line, "REVOLUTION"))
		      sscanf(line, "      REVOLUTION = %i", &orbit[n]);
		    else if (strstr(line, "X_POSITION"))
		      sscanf(line, "      X_POSITION = %f", &x[n]);
		    else if (strstr(line, "Y_POSITION"))
		      sscanf(line, "      Y_POSITION = %f", &y[n]);
		    else if (strstr(line, "Z_POSITION"))
		      sscanf(line, "      Z_POSITION = %f", &z[n]);
		    else if (strstr(line, "X_VELOCITY"))
		      sscanf(line, "      X_VELOCITY = %f", &vx[n]);
		    else if (strstr(line, "Y_VELOCITY"))
		      sscanf(line, "      Y_VELOCITY = %f", &vy[n]);
		    else if (strstr(line, "Z_VELOCITY"))
		      sscanf(line, "      Z_VELOCITY = %f", &vz[n]);
		    else if (strstr(line, "DOPPLER_FREQ"))
		      sscanf(line, "    DOPPLER_FREQ = (%f", &fd[n]);
		  } /* while inside OBJECT = FRAME */
		  if (j>0) sequence[n] = sequence[n-1];
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
	} /* if within a SEGMENT object */
      } /* looping through a SEGMENT object (for) */
    } /* while reading an individual scan results file */
    FCLOSE(fpIn);
  } /* while reading through list of scan results files */

  /* Assigning an internal track numbers to orbits for potential repeat pass */
  for (i=0; i<n; i++) 
    track[i] = orbit[i]-(orbit[i]/343)*343;

  /* Look for image pairs */
  for (i=0; i<n; i++) {

    rev = orbit[i]; /* reference orbit */
    if (rev>=10000)
      sprintf(outfile, "%s_%d.in", mode, rev);
    else
      sprintf(outfile, "%s_0%d.in", mode, rev);
    fpOut = FOPEN(outfile, "w");

    /* How many frames are in the reference orbit */
    for (m=0; m<1000; m++) 
      if (orbit[i+m] != rev) break;
 
    /* Match up frame pairs */
    for (j=0; j<i; j++) 
      for (k=0; k<m; k++)
	if (track[j]==track[i+k] && orbit[j]!=orbit[i+k] && frame[j]==frame[i+k])
	  fprintf(fpOut, "%s %3d %5d %2d %s %12.6f %12.6f %12.6f %12.6f %12.6f "
		  "%12.6f %12.6f %12.6f %5d %2d %s %12.6f %12.6f %12.6f %12.6f "
		  "%12.6f %12.6f %12.6f %12.6f\n", 
		  mode, frame[i+k], orbit[i+k], sequence[i+k], time[i+k], 
		  x[i+k], y[i+k], z[i+k], vx[i+k], vy[i+k], vz[i+k], 
		  slant_range[i+k]*1000, fd[i+k], orbit[j], sequence[j], time[j], 
		  x[j], y[j], z[j], vx[j], vy[j], vz[j], lat[i+k], lon[i+k]);
      
    FCLOSE(fpOut);
    i+=m-1;
  }

  FCLOSE(fpList);
  return (0);

} /* main */

