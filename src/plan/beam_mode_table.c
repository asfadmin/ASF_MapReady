#include "beam_mode_table.h"
#include "asf.h"
#include <assert.h>

void get_all_beam_modes(const char *satellite, int *num_out,
                        char ***names_out, double **min_looks_out,
                        double **max_looks_out, double **look_incrs_out,
                        char ***allowed_look_angles_out)
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

  // now allocate ...
  char **names = MALLOC(sizeof(char*)*(n+1));
  double *min_looks = MALLOC(sizeof(double)*(n+1));
  double *max_looks = MALLOC(sizeof(double)*(n+1));
  double *look_incrs = MALLOC(sizeof(double)*(n+1));
  char **allowed_look_angles = MALLOC(sizeof(char*)*(n+1));

  // ... and reopen the file
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
      double min_look, max_look, look_incr, ignore;
      int format;
      allowed_look_angles[i] = MALLOC(sizeof(char)*64);
      sscanf(line, "%s %d", sat, &format);
      if (format==1) {
        // Format 1: Satellite, 1, Beam Mode,
        // Swath Width (km), Swath Length (km), Swath Length (sec),
        // # Buffer Frames, Min Look Angle, Max Look Angle, Increment
        sscanf(line, "%s %d %s %lf %lf %lf %lf %lf %lf %lf",
               sat, &format, bm, &ignore, &ignore, &ignore, &ignore,
               &min_look, &max_look, &look_incr);
        assert(format==1);
        allowed_look_angles[i] = NULL;
        min_looks[i] = min_look;
        max_looks[i] = max_look;
        look_incrs[i] = look_incr;
      }
      else if (format==2) {
        // Format 2: Satellite, 2, Beam Mode, Swath Width (km),
        // Swath Length (km), Swath Length (sec), # Buffer Frames,
        // Allowable Beam Modes (1 or more, separated by commas)
        allowed_look_angles[i] = MALLOC(sizeof(char)*64);
        sscanf(line, "%s %d %s %lf %lf %lf %lf %s",
               sat, &format, bm, &ignore, &ignore, &ignore, &ignore,
               allowed_look_angles[i]);
        assert(format==2);
        min_looks[i] = max_looks[i] = look_incrs[i] = -1;
      }

      assert(strcmp_case(sat,satellite)==0);
      names[i] = STRDUP(bm);
      ++i;
    }
  }
  fclose(fp);

  *num_out=n;
  *names_out=names;
  *min_looks_out=min_looks;
  *max_looks_out=max_looks;
  *look_incrs_out=look_incrs;
  *allowed_look_angles_out=allowed_look_angles;
}

BeamModeInfo *get_beam_mode_info(const char *satellite, const char *beam_mode)
{
  FILE *fp = fopen_share_file("beam_modes.txt", "r");
  if (!fp)
    asfPrintError("Couldn't open beam_modes.txt: %s\n", strerror(errno));

  char *p;
  int found=FALSE;
  double min_look=0, max_look=0, look_incr=0, width=0, length=0, image_time=0;
  char allowed_look_angles[1024];
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
    int format;

    sscanf(line, "%s %d", sat, &format);
    if (format==1) {
      // Format 1: Satellite, 1, Beam Mode,
      // Swath Width (km), Swath Length (km), Swath Length (sec),
      // # Buffer Frames, Min Look Angle, Max Look Angle, Increment
      sscanf(line, "%s %d %s %lf %lf %lf %d %lf %lf %lf", 
             sat, &format, bm, &width, &length, &image_time,
             &num_buffer_frames, &min_look, &max_look, &look_incr);
      strcpy(allowed_look_angles, "");
    }
    else if (format==2) {
      // Format 2: Satellite, 2, Beam Mode, Swath Width (km),
      // Swath Length (km), Swath Length (sec), # Buffer Frames,
      // Allowable Beam Modes (1 or more, separated by commas)
      sscanf(line, "%s %d %s %lf %lf %lf %d %s",
             sat, &format, bm, &width, &length, &image_time,
             &num_buffer_frames, allowed_look_angles);
      if (strlen(allowed_look_angles) > 64) // truncate if necessary
        allowed_look_angles[63] = '\0';
      min_look = max_look = look_incr = -1;
    }

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
    strcpy(ret->allowed_look_angles, allowed_look_angles);
    return ret;
  }
}
