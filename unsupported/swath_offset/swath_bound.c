#ifndef SWATH_BOUND_C
#define SWATH_BOUND_C

#include <stdio.h>
#include "asf.h"
#include "swath_bound.h"

/*
struct swathbounds
{
	int start_offset1, end_offset1;
	int start_offset2, end_offset2;
	int masked_start_offset1, masked_end_offset1;
	int masked_start_offset2, masked_end_offset2;
	int total_loc1, total_loc2;
	int aisp_patches, masked_patches;
	int par_start_loc1, par_end_loc1;
	int par_start_loc2, par_end_loc2;
	char *parFile1, *parFile2;
	char *metaFile1, *metaFile2;
};*/

  /*
   * Write the swathbounds structure out to a file.
   */

void make_bound_file(char *fname, struct swathbounds boundOut)
{
	FILE *oFile;

	oFile = FOPEN(fname, "w");
	fprintf(oFile, "aisp_patches: \t\t%i\n", boundOut.aisp_patches);
	fprintf(oFile, "masked_patches: \t\t%i\n", boundOut.masked_patches);
	fprintf(oFile, "Swath 1 \n{\n");
	fprintf(oFile, "\tpar_file: \t\t\t%s\n", boundOut.parFile1);
	fprintf(oFile, "\tmeta_file: \t\t\t%s\n", boundOut.metaFile1);
	fprintf(oFile, "\tstart_line: \t\t\t%i\n", boundOut.start_offset1);
	fprintf(oFile, "\tend_line: \t\t\t%i\n", boundOut.end_offset1);
	fprintf(oFile, "\tmasked_start: \t\t\t%i\n", boundOut.masked_start_offset1);
	fprintf(oFile, "\tmasked_end: \t\t\t%i\n", boundOut.masked_end_offset1);
	fprintf(oFile, "\ttotal_par_locations: \t\t%i\n", boundOut.total_loc1);
	fprintf(oFile, "\tstart_par_location: \t\t%i\n", boundOut.par_start_loc1);
	fprintf(oFile, "\tend_par_location: \t\t%i\n", boundOut.par_end_loc1);
	fprintf(oFile, "}\n");
	fprintf(oFile, "Swath 2 \n{\n");
	fprintf(oFile, "\tpar_file: \t\t\t%s\n", boundOut.parFile2);
	fprintf(oFile, "\tmeta_file: \t\t\t%s\n", boundOut.metaFile2);
	fprintf(oFile, "\tstart_line: \t\t\t%i\n", boundOut.start_offset2);
	fprintf(oFile, "\tend_line: \t\t\t%i\n", boundOut.end_offset2);
	fprintf(oFile, "\tmasked_start: \t\t\t%i\n", boundOut.masked_start_offset2);
	fprintf(oFile, "\tmasked_end: \t\t\t%i\n", boundOut.masked_end_offset2);
	fprintf(oFile, "\ttotal_par_locations: \t\t%i\n", boundOut.total_loc2);
	fprintf(oFile, "\tstart_par_location: \t\t%i\n", boundOut.par_start_loc2);
	fprintf(oFile, "\tend_par_location: \t\t%i\n", boundOut.par_end_loc2);
	fprintf(oFile, "}\n");
	FCLOSE(oFile);
	

}

  /* 
   * Read the swath bounds file into a structure.
   */
void read_bound_file(char *fname, struct swathbounds *boundOut)
{
	FILE *iFile;
	char param[255];
	char tempFile[255];

	iFile = FOPEN(fname, "r");
	fscanf(iFile, "%s%i", param, &boundOut->aisp_patches);
	fscanf(iFile, "%s%i", param, &boundOut->masked_patches);
	fscanf(iFile, "%s%s", param, tempFile);
	fscanf(iFile, "%s", param);
	fscanf(iFile, "%s%s", param, tempFile);
	boundOut->parFile1 = (char *) MALLOC(sizeof(char)*strlen(tempFile)); 
	strcpy(boundOut->parFile1, tempFile);
	fscanf(iFile, "%s%s", param, tempFile);
	boundOut->metaFile1 = (char *) MALLOC(sizeof(char)*strlen(tempFile));
	strcpy(boundOut->metaFile1, tempFile);
	fscanf(iFile, "%s%i", param, &boundOut->start_offset1);
	fscanf(iFile, "%s%i", param, &boundOut->end_offset1);
	fscanf(iFile, "%s%i", param, &boundOut->masked_start_offset1);
	fscanf(iFile, "%s%i", param, &boundOut->masked_end_offset1);
	fscanf(iFile, "%s%i", param, &boundOut->total_loc1);
	fscanf(iFile, "%s%i", param, &boundOut->par_start_loc1);
	fscanf(iFile, "%s%i", param, &boundOut->par_end_loc1);
	fscanf(iFile, "%s", param);
	fscanf(iFile, "%s%s", param, tempFile);
	fscanf(iFile, "%s", param);
	fscanf(iFile, "%s%s", param, tempFile);
	boundOut->parFile2 = (char *) MALLOC(sizeof(char)*strlen(tempFile)); 
	strcpy(boundOut->parFile2, tempFile);
	fscanf(iFile, "%s%s", param, tempFile);
	boundOut->metaFile2 = (char *) MALLOC(sizeof(char)*strlen(tempFile));
	strcpy(boundOut->metaFile2, tempFile);
	fscanf(iFile, "%s%i", param, &boundOut->start_offset2);
	fscanf(iFile, "%s%i", param, &boundOut->end_offset2);
	fscanf(iFile, "%s%i", param, &boundOut->masked_start_offset2);
	fscanf(iFile, "%s%i", param, &boundOut->masked_end_offset2);
	fscanf(iFile, "%s%i", param, &boundOut->total_loc2);
	fscanf(iFile, "%s%i", param, &boundOut->par_start_loc2);
	fscanf(iFile, "%s%i", param, &boundOut->par_end_loc2);

	FCLOSE(iFile);

}



#endif

