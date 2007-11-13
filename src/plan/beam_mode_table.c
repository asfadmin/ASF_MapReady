#include "beam_mode_table.h"
#include "asf.h"

char **get_all_beam_modes(int *num)
{
  FILE *fp = fopen_share_file("beam_modes.txt", "r");
  if (!fp)
    asfPrintError("Couldn't open beam_modes.txt: %s\n", strerror(errno));

  int n=0;
  while (1) {
    char line[1024];
    char *p=fgets(line, 1024, fp);
    if (!p) break; else ++n;
  }
  fclose(fp);

  char **ret = MALLOC(sizeof(char*)*(n+1));
  fp = fopen_share_file("beam_modes.txt", "r");

  int i=0;
  while (1) {
    char line[1024];
    char *p=fgets(line, 1024, fp);
    if (!p)
      break; //eof
    else {
      char sat[1024], bm[1024];
      sscanf(line, "%s %s", sat, bm);
      char *combo = MALLOC(sizeof(char)*(strlen(sat)+strlen(bm)+10));
      sprintf(combo, "%s / %s", sat, bm);
      ret[i++] = combo;
    }
  }
  fclose(fp);

  *num=n;
  return ret;
}

BeamModeInfo *get_beam_mode_info(const char *satellite, const char *beam_mode)
{
  FILE *fp = fopen_share_file("beam_modes.txt", "r");
  if (!fp)
    asfPrintError("Couldn't open beam_modes.txt: %s\n", strerror(errno));

  char *p;
  int found=FALSE;
  float look=0, width=0, length=0, image_time=0;

  while (1) {
    char line[1024];
    p=fgets(line, 1024, fp);
    if (!p) {
      // eof, not found!
      asfPrintError(
        "Satellite: %s, Beam Mode: %s not found in beam_modes.txt!\n",
        satellite, beam_mode);
      break;
    }

    char sat[1024], bm[1024];
    sscanf(line, "%s %s %f %f %f %f", 
        sat, bm, &look, &width, &length, &image_time);

    if (strcmp_case(bm, beam_mode)==0 && strcmp_case(satellite, sat)==0) {
      found = TRUE;
      break;
    }
  }
  fclose(fp);

  if (!found) {
    return NULL;
  }
  else {
    BeamModeInfo *ret = MALLOC(sizeof(BeamModeInfo));
    ret->look_angle = look;
    ret->width_m = width*1000.;
    ret->length_m = length*1000.;
    ret->image_time = image_time;
    return ret;
  }
}
