
#include "asf.h"
#include "hdr.h"


/* Read .hdr and fill the struct */
void get_hdr(char *hdr_name, jpl_header *hdr)
{
  char line[255];
  FILE *fp = FOPEN(hdr_name, "r");
  fgets(line, 255, fp);
  sscanf(line, "%d", &hdr->magnitude_bytes);
  fgets(line, 255, fp);
  sscanf(line, "%d", &hdr->elevation_bytes);
  fgets(line, 255, fp);
  sscanf(line, "%s", hdr->data_type);
  fgets(line, 255, fp);
  sscanf(line, "%lf %lf", &hdr->elevation_scale, &hdr->elevation_shift);
  fgets(line, 255, fp);
  sscanf(line, "%lf %lf", &hdr->line_increment, &hdr->sample_increment);
  fgets(line, 255, fp);
  sscanf(line, "%lf %lf", &hdr->start_lat, &hdr->start_lon);
  fgets(line, 255, fp);
  sscanf(line, "%d %d", &hdr->line_count, &hdr->sample_count);
  FCLOSE(fp);
}
