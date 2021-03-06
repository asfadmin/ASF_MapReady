/* Input/output routines for .stat files */

#include "asf.h"
#include "stats.h"

//double stat_get_double(FILE *stat_file)
//{
//	char line[256];
//	fgets(line,255,stat_file);
//	while (line[0]=='#') fgets(line,255,stat_file); /* skip lines starting with # */
//	strtok(line,"#");/*Hack comments off the end of the line.*/
//	return strtod(line,NULL);
//}
//
//int stat_get_int(FILE *stat_file)
//{
//	char line[256];
//	fgets(line,255,stat_file);
//	while (line[0]=='#') fgets(line,255,stat_file); /* skip lines starting with # */
//	strtok(line,"#");/*Hack comments off the end of the line.*/
//	return atoi(line);
//}
//
///*******************************************************************************
// * Get the data from the .stat file and fill the stats structure              */
//void stat_read(stat_parameters *stats, const char *file_name)
//{
//	FILE *stat_file;
//	char stat_name[256];
//	char line[256];
//	int ii=0;
//
//	create_name(stat_name, file_name, ".stat");
//	stat_file = FOPEN(stat_name, "r");
//
///* get data line by line */
//	stats->min = stat_get_double(stat_file);
//	stats->max = stat_get_double(stat_file);
//	stats->mean = stat_get_double(stat_file);
//	stats->rmse = stat_get_double(stat_file);
//	stats->std_deviation = stat_get_double(stat_file);
//	stats->mask = stat_get_double(stat_file);
//	stats->slope = stat_get_double(stat_file);
//	stats->offset = stat_get_double(stat_file);
//	stats->upper_left_line = stat_get_int(stat_file);
//	stats->upper_left_samp = stat_get_int(stat_file);
//	stats->lower_right_line = stat_get_int(stat_file);
//	stats->lower_right_samp = stat_get_int(stat_file);
//
///* get histogram */
//	ii=0;
//	while (fgets(line,255,stat_file) && ii<256) {
//		char *temp;
//		if (line[0]=='#') continue;/*Skip comments at beginning of line.*/
//		if (line[0]=='\n') continue;/*Skip blank lines.*/
//		temp = &line[8];
//		sscanf(temp," %d %d %d %d %d %d %d %d\n",&stats->histogram[ii],
//			&stats->histogram[ii+1],&stats->histogram[ii+2],
//			&stats->histogram[ii+3],&stats->histogram[ii+4],
//			&stats->histogram[ii+5],&stats->histogram[ii+6],
//			&stats->histogram[ii+7]);
//		ii+=8;
//	}
//}

/*******************************************************************************
 * Write the data from the stats structure to the specified file name         */
void stat_write(stat_parameters *stats, const char *file_name, const int num_bands)
{
	FILE *stat_file;
	int  band, ii;

	stat_file = FOPEN(file_name, "w");

  /* Write stats structure to file */
	fprintf(stat_file,"# Statistics for image file of the same base name\n");
  fprintf(stat_file,"Number of bands: %d\n", num_bands);
  for (band=0; band<num_bands; band++) {
    fprintf(stat_file,"Band number: %d\n", band);
    fprintf(stat_file,"%-16.11g\t# minimum\n",stats[band].min);
    fprintf(stat_file,"%-16.11g\t# maximum\n",stats[band].max);
    fprintf(stat_file,"%-16.11g\t# mean\n",stats[band].mean);
    fprintf(stat_file,"%-16.11g\t# rms error\n",stats[band].rmse);
    fprintf(stat_file,"%-16.11g\t# standard deviation\n",stats[band].std_deviation);
    fprintf(stat_file,"%-16.11g\t# masked value (none if NaN)\n",stats[band].mask);
    fprintf(stat_file,"\n");

    /* Write out histogram */	
    fprintf(stat_file,"# Histogram\n");
    for (ii=0; ii<256; ii++) {
      if (ii%8 == 0)
        fprintf(stat_file, "%s%3i-%3i:", (ii==0) ? "" : "\n", ii, ii+7);
      fprintf(stat_file, " %8i", stats[band].histogram[ii]);
    }
    fprintf(stat_file, "\n");
  }

	FCLOSE(stat_file);
}

