#include "asf_baseline.h"

void filter_srf(char *output, char *sensor, char *mode, int *nFiles)
{
  struct dirent *dp;
  struct stat statbuf;
  DIR *dir;
  FILE *fp;
  int orbit, old_orbit=0, sequence, old_sequence=0, file_size, n=0;
  char file_name[255], old_file[255], *s; 

  s = (char *) MALLOC(sizeof(char)*25);
  fp = FOPEN(output, "w");

  // Check out the files in current directory
  dir = opendir(".");
  while ((dp = readdir(dir)) != NULL) {

    if (stat(dp->d_name, &statbuf) == -1)
      continue;
    sprintf(file_name, "%s", dp->d_name);
    file_size = (int)statbuf.st_size;
    if (file_size > 2000 && strlen(file_name) == 19 && strstr(file_name, mode)) {
      sprintf(sensor, "%s", file_name);
      sensor[2] = '\0';
      sprintf(s, "%s", &file_name[2]);
      s[5] = '\0';
      orbit = atoi(s);
      sprintf(s, "%s", &file_name[7]);
      s[2] = '\0';
      sequence = atoi(s);
      sprintf(mode, "%s", &file_name[9]);
      mode[3] = '\0';
      if ((orbit != old_orbit || sequence != old_sequence) && old_orbit != 0) {
	fprintf(fp, "%s\n", old_file);
	n++;
      }
      old_orbit = orbit;
      old_sequence = sequence;
      sprintf(old_file, "%s", file_name);
    }
  }
  fprintf(fp, "%s\n", old_file);
  *nFiles = n+1;
  closedir(dir);
  FCLOSE(fp);
  FREE(s);
}
