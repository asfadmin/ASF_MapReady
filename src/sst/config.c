#include "asf.h"
#include "asf_simulation.h"
#include <ctype.h>

static int strindex(char s[], char t[])
{
  int i, j, k;

  for (i=0; s[i]!='\0'; i++) {
    for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
      ;
    if (k>0 && t[k]=='\0')
      return i;
  }
  return -1;
}

static char *read_param(char *line)
{
  int i, k;
  char *value=(char *)MALLOC(sizeof(char)*255);

  strcpy(value, "");
  i=strindex(line, "]");
  k=strindex(line, "=");
  if (i>0) strncpy(value, line, i+1);
  if (k>0) strncpy(value, line, k);
  return value;
}

static char *read_str(char *line, char *param)
{
  static char value[255];
  char *p = strchr(line, '=');

  // skip past the '=' sign, and eat up any whitespace
  ++p;
  while (isspace(*p))
      ++p;

  strcpy(value, p);

  // eat up trailing whitespace, too
  p = value + strlen(value) - 1;
  while (isspace(*p))
      *p-- = '\0';

  return value;
}

static int read_int(char *line, char *param)
{
  char *tmp;
  int value;

  tmp = read_str(line, param);
  sscanf(tmp, "%i", &value);

  return value;
}

static double read_double(char *line, char *param)
{
  char *tmp;
  double value;

  tmp = read_str(line, param);
  sscanf(tmp, "%lf", &value);

  return value;
}

satellite_t *read_satellite_config(char *sensor, char *mode)
{
  FILE *fp;
  char line[255], params[50];
  char *test;
  satellite_t *sat = (satellite_t *) MALLOC(sizeof(satellite_t));
  char *header = (char *) MALLOC(sizeof(char)*64);

  strcpy(params, "");
  if (sat == NULL) 
    check_return(1, "Creating configuration structure.\n");
  fp = fopen_share_file("satellite.config", "r");
  if (!fp) 
    return NULL;
  while (fgets(line, 255, fp)) {

    sprintf(header, "[%s]", sensor);
    if (strncmp_case(line, header, strlen(header))==0) 
      strcpy(params, sensor);
    if (strncmp_case(params, sensor, strlen(sensor))==0) {
      test = read_param(line);
      if (strncmp(test, "sensor", 6) == 0)
        strcpy(sat->sensor, read_str(line, "sensor"));
      else if (strncmp(test, "beam mode", 9) == 0)
	strcpy(sat->beam_mode, read_str(line, "beam mode"));
      else if (strncmp(test, "wavelength", 10) == 0)
	sat->wavelength = read_double(line, "wavelength");
      else if (strncmp(test, "look angle", 10) == 0)
	sat->look_angle = read_double(line, "look angle");
      else if (strncmp(test, "pixel size", 10) == 0)
	sat->pixel_size = read_double(line, "pixel size");
      else if (strncmp(test, "pixel count", 11) == 0) {
	sat->pixel_count = read_int(line, "pixel count");
	if (sat->pixel_count > 10000) {
	  asfPrintWarning("Selected size (%d) to large for simulation. "
			  "Setting size to 7500 pixels\n");
	  sat->pixel_count = 7500;
	}
	if (strcmp_case(sat->beam_mode, mode) == 0)
	  break;
      } 
    }
  }

  // Clean up
  FCLOSE(fp);
  FREE(header);

  return sat;
}

