#include "asf_baseline.h"

#define SIZE 50000

void find_pairs(int track, char *list, char *pairs, int *nPairs) 
{
  FILE *fpList=NULL, *fpIn=NULL, *fpOut=NULL;
  int nSegments=0, nFrames=0, *sequence;
  char file[100], line[1024], *s;
  int i, k, l, m, n=0, rev=0, matches=0, test_orbit, *orbit, *frame;

  // Allocate some serious memory for orbits and frames
  orbit = (int *) MALLOC (sizeof(int)*SIZE);
  frame = (int *) MALLOC (sizeof(int)*SIZE);
  sequence = (int *) MALLOC (sizeof(int)*SIZE);
  s = (char *) MALLOC (sizeof(char)*25);

  // Open scan results file list
  fpList = FOPEN(list, "r");
  while (fgets(line, 1024, fpList)) {

    // Check whether orbit in on track
    strncpy(file, line, 19);
    sprintf(s, "%s", &file[2]);
    s[5] = '\0';
    test_orbit = atoi(s);
    if (track == test_orbit-(test_orbit/343)*343) {

      fpIn = FOPEN(file, "r");
      while (fgets(line, 1024, fpIn)) {

	// Look for sequence number
	if (strstr(line, "SEQUENCE"))
	  sscanf(line, "  SEQUENCE = %i", &sequence[n]);
      
	// Look for segments
	if (strstr(line, "SEGMENT_COUNT")) 
	  sscanf(line, "  SEGMENT_COUNT = %i", &nSegments);
	for (i=0; i<nSegments; i++) {
	  if (strstr(line, "OBJECT = SEGMENT") &&
	      strncmp(line, "  OBJECT = SEGMENT", 18)==0) {
	    
	    // Look for frames
	    nFrames = 0;
	    while (fgets(line, 1024, fpIn)) {
	      if (strstr(line, "FRAME_COUNT")) 
		sscanf(line, "   FRAME_COUNT = %i", &nFrames);
	      for (k=0; k<nFrames; k++) {
		while (fgets(line, 1024, fpIn)) {
		if (strstr(line, "OBJECT = FRAME") &&
		    strncmp(line, "   OBJECT = FRAME", 17)==0) {
		  while (fgets(line, 1024, fpIn)) {
		    if (strstr(line, "END_OBJECT = FRAME") &&
			strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		    else if (strstr(line, "FRAME_ID")) {
		      sscanf(line, "    FRAME_ID = %i", &frame[n]);
		      orbit[n] = test_orbit;
		    }
		  } // while inside OBJECT = FRAME
		  if (k>0) sequence[n] = sequence[n-1];
		  n++;

                  if (strstr(line, "END_OBJECT = FRAME") &&
		      strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		} // if within a FRAME object
		
		if (strstr(line, "END_OBJECT = FRAME") &&
		    strncmp(line, "   END_OBJECT = FRAME", 21)==0) break;
		} // while looking for FRAME objects

	      } // looping through all the frames (for)
	    
	    } // while inside OBJECT = SEGMENT

	    if (strstr(line, "END_OBJECT = SEGMENT") &&
		strncmp(line, "  END_OBJECT = SEGMENT", 22)==0) break;
	  } // if within a SEGMENT object
	  
	} // looping through a SEGMENT object (for)
	
      } // while reading an individual scan results file
      
      FCLOSE(fpIn);
    } // if orbit is on track

  } // while reading through list of scan results files

  fpOut = FOPEN(pairs, "w");
  
  // Look for image pairs
  for (i=0; i<n; i++)
    if (orbit[i]>rev) {
      rev = orbit[i];
      for (k=0; k<n; k++) 
	if (orbit[i]!=orbit[k]) {
	  for (m=i; m<n; m++) 
	    for (l=k; l<n; l++) {
	      if (orbit[m]==orbit[i] && orbit[l]==orbit[k] 
		  && frame[m]==frame[l]) {
		fprintf(fpOut, "%d,%d,%d,%d,%d\n", orbit[m], sequence[m], 
			orbit[l], sequence[l], frame[l]);
		matches++;
	      }
	      else
		break;
	    }
	}
    }
  
  FCLOSE(fpOut);
  FCLOSE(fpList);
  *nPairs = matches;
  FREE(orbit);
  FREE(frame);
  FREE(s);

}
