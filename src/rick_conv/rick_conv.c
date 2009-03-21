#include "asf.h"

#include <stdio.h>
#include <stdlib.h>

#define TABBED 1
#define START_AT_40 2

int main(int argc, char **argv)
{
  if (argc != 3) {
    printf("Usage: rick_conv <input> <output>\n");
    exit(1);
  }

  FILE *in = FOPEN(argv[1], "r");
  FILE *out = FOPEN(argv[2], "w");
  int i,line_num = 0;

  char line[101];
  while (fgets(line, 100, in) != NULL) {

    ++line_num;
    double f1, f2, f3, f4, f5, f6;
    int n;

    // Rick's description of the file format was ambiguous....
    // There appear to be two input formats we must support (1) where the
    // data starts at character 40, and (2) where the data starts after a tab.
    // We'll do it this way: if the line contains a tab, then assume it
    // is the latter, otherwise assume it must be the former
    char *tab = strchr(line, '\t');
    int which_format = 0;
    if (tab) {
      n = sscanf(tab+1, "%lf %lf %lf %lf %lf %lf",
                 &f1, &f2, &f3, &f4, &f5, &f6);
      which_format = TABBED;
    }
    else {
      n = sscanf(line+40, "%lf %lf %lf %lf %lf %lf",
                 &f1, &f2, &f3, &f4, &f5, &f6);
      which_format = START_AT_40;
    }

    int ok=TRUE;
    double lat_sec, lon_sec;
    int lat_deg, lat_min, lon_deg, lon_min;

    if (n==2) {
      // two numbers: first is lat, second is lon
      lat_deg = (int)floor(f1);
      double m = (f1-lat_deg)*60.;
      lat_min = (int)floor(m);
      lat_sec = (m-lat_min)*60.;

      lon_deg = (int)floor(f2);
      m = (f2-lon_deg)*60.;
      lon_min = (int)floor(m);
      lon_sec = (m-lon_min)*60.;
    }
    else if (n==4) {
      // four numbers: lat deg, lat min, lon deg, lon min
      lat_deg = (int)f1;
      lat_sec = 60.0*f2;
      lat_min = lat_sec/60;
      lat_sec -= lat_min*60;

      lon_deg = (int)f3;
      lon_sec = 60.0*f4;
      lon_min = lon_sec/60;
      lon_sec -= lon_min*60;
    }
    else if (n==6) {
      // six numbers: lat deg, lat min, lat sec, lon deg, lon min, lon sec
      lat_deg = (int)f1;
      lat_min = (int)f2;
      lat_sec = f3;

      lon_deg = (int)f4;
      lon_min = (int)f5;
      lon_sec = f6;
    }
    else {
      asfPrintWarning("Invalid line %d.\n", line_num);
      ok=FALSE;
    }

    if (ok) {
      char name[42];
      for (i=0; i<42; ++i) name[i]='\0';

      if (which_format==TABBED) {
        *tab = '\0';
        strncpy(name, line, 40);
        *tab = '\t';
      }
      else {
        strncpy(name, line, 40);
      }

      char *name_tr = trim_spaces(name);

      // I tried to mimic Rick's example output exactly -- even down to
      // the fact that lat sec's have 4 decimal places, and the lon secs
      // have 5 places, and that everything has leading zeros...
      fprintf(out, "%-40s %02d %02d %07.4f %03d %02d %08.5f\n", name_tr,
              lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec);
      //printf("%40s %d %d %.4f %d %d %.4f\n", name_tr,
      //       lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec);

      free(name_tr);
    }
  }
  printf("Converted %d lines.\n", line_num);

  FCLOSE(in);
  FCLOSE(out);
}
