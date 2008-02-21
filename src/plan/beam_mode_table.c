#include "beam_mode_table.h"
#include "asf.h"
#include <assert.h>

void get_all_beam_modes(const char *satellite, int *num_out,
                        char ***names_out, double **min_looks_out,
                        double **max_looks_out, double **look_incrs_out)
{
  FILE *fp = fopen_share_file("beam_modes.txt", "r");
  if (!fp)
    asfPrintError("Couldn't open beam_modes.txt: %s\n", strerror(errno));

  int len = strlen(satellite);

  // First -- count the number of lines, so we can allocate
  int n=0;
  while (1) {
    char line[1024];
    char *p=fgets(line, 1024, fp);
    if (!p)
      break;
    else if (line[0] == '#')
      continue;
    else if (strncmp_case(line, satellite, len)==0)
      ++n;
  }
  fclose(fp);

  char **names = MALLOC(sizeof(char*)*(n+1));
  double *min_looks = MALLOC(sizeof(double)*(n+1));
  double *max_looks = MALLOC(sizeof(double)*(n+1));
  double *look_incrs = MALLOC(sizeof(double)*(n+1));

  fp = fopen_share_file("beam_modes.txt", "r");
  assert(fp);

  int i=0;
  while (1) {
    char line[1024];
    char *p=fgets(line, 1024, fp);
    if (!p)  //eof
      break;
    else if (line[0] == '#') //comment line
      continue; 
    else if (strncmp_case(line, satellite, len)==0) {
      char sat[1024], bm[1024];
      double min_look, max_look, look_incr;
      sscanf(line, "%s %s %lf %lf %lf",
             sat, bm, &min_look, &max_look, &look_incr);
      assert(strcmp_case(sat,satellite)==0);
      names[i] = STRDUP(bm);
      min_looks[i] = min_look;
      max_looks[i] = max_look;
      look_incrs[i] = look_incr;
      ++i;
    }
  }
  fclose(fp);

  *num_out=n;
  *names_out=names;
  *min_looks_out=min_looks;
  *max_looks_out=max_looks;
  *look_incrs_out=look_incrs;
}

BeamModeInfo *get_beam_mode_info(const char *satellite, const char *beam_mode)
{
  FILE *fp = fopen_share_file("beam_modes.txt", "r");
  if (!fp)
    asfPrintError("Couldn't open beam_modes.txt: %s\n", strerror(errno));

  char *p;
  int found=FALSE;
  double min_look=0, max_look=0, look_incr=0, width=0, length=0, image_time=0;
  int num_buffer_frames=0;

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

    // ignore comment lines (starting with '#')
    if (line[0] == '#')
      continue;

    char sat[1024], bm[1024];
    sscanf(line, "%s %s %lf %lf %lf %lf %lf %lf %d", 
        sat, bm, &min_look, &max_look, &look_incr, &width, &length,
           &image_time, &num_buffer_frames);

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
    ret->min_look_angle = min_look;
    ret->max_look_angle = max_look;
    ret->look_angle_increment = look_incr;
    ret->width_m = width*1000.;
    ret->length_m = length*1000.;
    ret->image_time = image_time;
    ret->num_buffer_frames = num_buffer_frames;
    return ret;
  }
}
