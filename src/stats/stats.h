
#ifndef __STATS_H
#define __STATS_H

typedef struct {
  double min, max;       /* Minimum and maximum sample values                 */
  double mean;           /* Mean average of sample values                     */
  double rmse;           /* Root mean squared error                           */
  double std_deviation;  /* Standard deviation                                */
  double mask;           /* Value ignored while taking statistics             */
  double slope, offset;  /* Slope and offset of line fitting data to [0..255] */
  int upper_left_line;   /* Upper left y coordinate of the area in question   */
  int upper_left_samp;   /* Upper left x coordinate of the area in question   */
  int lower_right_line;  /* Lower right y coordinate of the area in question  */
  int lower_right_samp;  /* Lower right x coordinate of the area in question  */
  int histogram[256];    /* Histogram of data fit to [0..255]                 */
} stat_parameters;  

void stat_read(stat_parameters *stats, const char *file_name);
void stat_write(stat_parameters *stats, const char *file_name);

#endif
