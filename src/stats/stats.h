#ifndef __STATS_H
#define __STATS_H

typedef struct {
  double min, max;       /* Minimum and maximum sample values                 */
  double mean;           /* Mean average of sample values                     */
  double rmse;           /* Root mean squared error                           */
  double std_deviation;  /* Standard deviation                                */
  double percent_valid;  // Percent of valid values
  double mask;           /* Value ignored while taking statistics             */
  int histogram[256];    /* Histogram of data fit to [0..255]                 */
} stat_parameters;  

//void stat_read(stat_parameters *stats, const char *file_name);
void stat_write(stat_parameters *stats, const char *file_name, const int num_bands);

#endif
