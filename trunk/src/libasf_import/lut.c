#include "asf_import.h"
#include <dirent.h>
#include <dateUtil.h>

// Look for a LUT in <sharedir>/look_up_tables/import/* that might
// need to automatically applied
char *check_luts(meta_parameters *meta)
{
  char luts_dir[1024],name[1024];
  sprintf(luts_dir,"%s/look_up_tables/import",get_asf_share_dir());
  char *ret=NULL;
  struct dirent *dp;
  DIR *dfd;

  // This is the character string that must be in the first line
  // of the LUT (in a comment).  After that, we expect "YYYY/MM/DD <sensor>"
  const char *tag = "ASF Import";

  if ((dfd = opendir(luts_dir)) == NULL)
    return NULL; // directory not found, probably we have no luts

  while ((dp = readdir(dfd)) != NULL) {

    if (strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0) {
      continue;
    }

    if (strlen(luts_dir)+strlen(dp->d_name)+2 > sizeof(name)) {
      asfPrintWarning("dirwalk: name %s/%s exceeds buffersize.\n",
                      luts_dir, dp->d_name);
      return NULL; // error
    }
    else {
      sprintf(name, "%s%c%s", luts_dir, DIR_SEPARATOR, dp->d_name);
      //printf("name: %s\n", name);

      FILE *fp = fopen(name, "r");
      if (fp) {
        char buf[256];
        if (fgets(buf, 255, fp)) {
          // Look for the "ASF Import" tag in the first line
          char *p = strstr(buf, tag);
          if (strlen(buf)>0 && buf[0]=='#' && p!=NULL)
          {
            int y, m, d;
            char sensor[256];
            p += strlen(tag);
            int n = sscanf(p, "%d/%d/%d %s", &y, &m, &d, sensor);
            if (n==4 && strlen(sensor)>0) {
              //printf("LUT Found for sensor %s, to be applied for data after "
              //       "%d/%d/%d\n", sensor, y,m,d);

              if (strcmp_case(meta->general->sensor,sensor)==0) {
                // Sensor matched, see if the date is after the lut date
                ymd_date ymd;
                hms_time hms;
                parse_DMYdate(meta->general->acquisition_date, &ymd, &hms);

                if ((ymd.year > y) ||
                    (ymd.year == y && ymd.month > m) ||
                    (ymd.year == y && ymd.month == m && ymd.day >= d))
                {
                  // Yes!  Look up table should be applied
                  char *base = get_basename(name);
                  asfPrintStatus("Applying look up table: %s\n", base);
                  ret = STRDUP(name);
                  FREE(base);
                }
              }
            }
          }
        }
      }
      FCLOSE(fp);
    }
  }
  closedir(dfd);
  return ret;
}

// Read user defined look up table
void read_cal_lut(meta_parameters *meta, char *lutName, 
		  double **incid_table, double **scale_table, 
		  int *min, int *max)
{
  FILE *fpLut;
  double *incid, *scale, old, new;
  double UL_incid, UR_incid, LL_incid, LR_incid;
  double min_incid=100.0, max_incid=0.0;
  char line[255];
  int ii, nLut=0, n;
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  const int MAX_tableRes = 512;

  // Allocate memory for tables
  incid = (double *) MALLOC(sizeof(double)*MAX_tableRes);
  scale = (double *) MALLOC(sizeof(double)*MAX_tableRes);

  // Read look up table
  fpLut = FOPEN(lutName, "r");
  for (ii=0; ii<MAX_tableRes; ii++) {
    incid[ii] = 0.0;
    scale[ii] = 0.0;
  }
  while(fgets(line, 255, fpLut)) {
    if (line[0]=='#') continue; // skip comment lines
    sscanf(line, "%lf\t%lf", &incid[nLut], &scale[nLut]);
    nLut++;
  }
  FCLOSE(fpLut);

  // Calculate minimum and maximum incidence angle
  UL_incid = meta_incid(meta, 0, 0);
  UR_incid = meta_incid(meta, nl, 0);
  LL_incid = meta_incid(meta, 0, ns);
  LR_incid = meta_incid(meta, nl, ns);
  if (UL_incid < min_incid) min_incid = UL_incid;
  if (UL_incid > max_incid) max_incid = UL_incid;
  if (UR_incid < min_incid) min_incid = UR_incid;
  if (UR_incid > max_incid) max_incid = UR_incid;
  if (LL_incid < min_incid) min_incid = LL_incid;
  if (LL_incid > max_incid) max_incid = LL_incid;
  if (LR_incid < min_incid) min_incid = LR_incid;
  if (LR_incid > max_incid) max_incid = LR_incid;
  min_incid *= R2D;
  max_incid *= R2D;

  // Look up the index for the minimum in the LUT
  n = 0;
  old = 100000000;
  for (ii=0; ii<nLut; ii++) {
    new = min_incid - incid[ii];
    if (fabs(new) < fabs(old)) {
      old = new;
      n++;
    }
    else break;
  }
  *min = n;

  // Look up the index for the maximum in the LUT
  n = 0;
  old = 100000000;
  for (ii=0; ii<nLut; ii++) {
    new = max_incid - incid[ii];
    if (fabs(new) < fabs(old)) {
      old = new;
      n++;
    }
    else break;
  }
  *max = n;
  
  *incid_table = incid;
  *scale_table = scale;
}

