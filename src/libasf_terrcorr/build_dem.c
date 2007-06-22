#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "asf.h"
#include "asf_meta.h"
#include "asf_sar.h"
#include "asf_terrcorr.h"
#include "libasf_proj.h"

static int iabs(int i)
{
    return i>0 ? i : -i;
}

// this is from the comp.graphics.algorithms FAQ
// see http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
static int pnpoly(int npol, double *xp, double *yp, double x, double y)
{
  int i, j, c = 0;
  for (i = 0, j = npol-1; i < npol; j = i++) {
    if ((((yp[i]<=y) && (y<yp[j])) ||
      ((yp[j]<=y) && (y<yp[i]))) &&
      (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))

      c = !c;
  }
  return c;
}

// return TRUE if there is any overlap between the two scenes
static int test_overlap(meta_parameters *meta1, meta_parameters *meta2)
{
    int zone1 = utm_zone(meta1->general->center_longitude);
    int zone2 = utm_zone(meta2->general->center_longitude);

    // if zone1 & zone2 differ by more than 1, we can stop now
    if (iabs(zone1-zone2) > 1)
      return FALSE;

    // The Plan:
    // we'll test if any of the 4 corner points of meta2 are in the
    // polygon for meta1.  We will convert all corner points to UTM
    // in the zone for meta1.

    // first, get the corners of meta1.
    double xp[4], yp[4];
    if (meta1->location) {
        // use the location block if available
        latLon2UTM_zone(meta1->location->lat_start_near_range,
            meta1->location->lon_start_near_range, 0, zone1, &xp[0], &yp[0]);
        latLon2UTM_zone(meta1->location->lat_start_far_range,
            meta1->location->lon_start_far_range, 0, zone1, &xp[0], &yp[0]);
        latLon2UTM_zone(meta1->location->lat_end_far_range,
            meta1->location->lon_end_far_range, 0, zone1, &xp[0], &yp[0]);
        latLon2UTM_zone(meta1->location->lat_end_near_range,
            meta1->location->lon_end_near_range, 0, zone1, &xp[0], &yp[0]);
    } else {
        double lat, lon;
        int nl1 = meta1->general->line_count;
        int ns1 = meta1->general->sample_count;

        // must call meta_get_latLon for each corner
        meta_get_latLon(meta1, 0, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[0], &yp[0]);

        meta_get_latLon(meta1, nl1-1, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[1], &yp[1]);

        meta_get_latLon(meta1, nl1-1, ns1-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[2], &yp[2]);

        meta_get_latLon(meta1, 0, ns1-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[3], &yp[3]);
    }

    // now test each corner point for meta2.  If any point is inside the
    // xp[],yp[] polygon, then the images overlap.  (And we can stop checking)
    double x, y;
    if (meta2->location) {
        latLon2UTM_zone(meta2->location->lat_start_near_range,
            meta2->location->lon_start_near_range, 0, zone1, &x, &y);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;

        latLon2UTM_zone(meta2->location->lat_start_far_range,
            meta2->location->lon_start_far_range, 0, zone1, &xp[0], &yp[0]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;

        latLon2UTM_zone(meta2->location->lat_end_far_range,
            meta2->location->lon_end_far_range, 0, zone1, &xp[0], &yp[0]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;

        latLon2UTM_zone(meta2->location->lat_end_near_range,
            meta2->location->lon_end_near_range, 0, zone1, &xp[0], &yp[0]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;
    }
    else {
        double lat, lon;
        int nl2 = meta1->general->line_count;
        int ns2 = meta1->general->sample_count;

        meta_get_latLon(meta2, 0, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[0], &yp[0]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;

        meta_get_latLon(meta2, nl2-1, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[1], &yp[1]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;

        meta_get_latLon(meta2, nl2-1, ns2-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[2], &yp[2]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;

        meta_get_latLon(meta2, 0, ns2-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp[3], &yp[3]);
        if (pnpoly(4, xp, yp, x, y))
            return TRUE;
    }

    // no corner points inside --> no overlap
    return FALSE;
}

// wrapper for test_overlap that will load the metadata for file #2
// because generally we have 1 file (with metadata we have already
// loaded), and we want to run through a list of other files.
static int test_overlap_file(meta_parameters *meta, const char *filename)
{
    char *meta_filename = appendExt(filename, ".meta");
    meta_parameters *m2 = meta_read(meta_filename);
    int ret = test_overlap(meta, m2);
    free(meta_filename);
    return ret;
}

static char *spaces(int n)
{
    int i;
    static char buf[256];
    for (i=0; i<256; ++i)
        buf[i] = i<n*3 ? ' ' : '\0';
    return buf;
}

static void process(const char *what, int level, int recursive,
                    char *overlapping_dems[], int *next_dem_number,
                    meta_parameters *meta, int *n_dems_total);

static void process_dir(const char *dir, int top, int recursive,
                        char *overlapping_dems[], int *next_dem_number,
                        meta_parameters *meta, int *n_dems_total)
{
  char name[1024];
  struct dirent *dp;
  DIR *dfd;

  if ((dfd = opendir(dir)) == NULL) {
    asfPrintStatus("  Cannot open %s\n",dir);
    return; // error
  }
  while ((dp = readdir(dfd)) != NULL) {
    // skip current, parent dir entries
    if (strcmp(dp->d_name, ".")==0 || strcmp(dp->d_name, "..")==0) {
      continue;
    }
    if (strlen(dir)+strlen(dp->d_name)+2 > sizeof(name)) {
      asfPrintWarning("dirwalk: name %s/%s exceeds buffersize.\n",
                      dir, dp->d_name);
      return; // error
    }
    else {
      // process this entry
      sprintf(name, "%s%c%s", dir, DIR_SEPARATOR, dp->d_name);
      process(name, top, recursive, overlapping_dems, next_dem_number,
          meta, n_dems_total);
    }
  }
  closedir(dfd);
}

static void process_file(const char *file, int level,
                         char *overlapping_dems[], int *next_dem_number,
                         meta_parameters *meta, int *n_dems_total)
{
    char *base = get_filename(file);
    char *ext = findExt(base);
    if (ext && strcmp_case(ext, ".img") == 0)
    {
        char *does;
        ++(*n_dems_total);
        if (test_overlap_file(meta, file)) {
            // overlaps!
            overlapping_dems[*next_dem_number] = STRDUP(file);
            ++(*next_dem_number);
            does = "Overlaps";
        } else {
            does = "No";
        }
        asfPrintStatus("  %s%s - %s\n", spaces(level), base, does);
    }
    else if (ext && strcmp_case(ext, ".meta") == 0) {
        // silently ignore the .meta files -- they will be picked up
        // when processing the corresponding .img file
        ;
    }
    else {
        // loudly ignore other stuff in the directory
        asfPrintStatus("  %s%s (ignored)\n", spaces(level), base);
    }
    FREE(base);
}

static void process(const char *what, int level, int recursive,
                    char *overlapping_dems[], int *next_dem_number,
                    meta_parameters *meta, int *n_dems_total)
{
  struct stat stbuf;

  if (stat(what, &stbuf) == -1) {
    asfPrintStatus("  Cannot access: %s\n", what);
    return;
  }

  char *base = get_filename(what);

  if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
      if (level==0 || recursive) {
          asfPrintStatus("  %s%s/\n", spaces(level), base);
          process_dir(what, level+1, recursive, overlapping_dems,
              next_dem_number, meta, n_dems_total);
      }
      else {
          asfPrintStatus("  %s%s (skipped)\n", spaces(level), base);
      }
  }
  else {
      process_file(what, level, overlapping_dems, next_dem_number,
          meta, n_dems_total);
  }

  FREE(base);
}

static char **find_overlapping_dems_dir(meta_parameters *meta,
                                        const char *dem_dir,
                                        int *n_dems_total)
{
    int i,n=0;
    const int recursive = TRUE;

    // hard-coded limit of 100 dems
    const int max_dems = 100;
    char **overlapping_dems = MALLOC(sizeof(char*)*max_dems);
    for (i=0; i<max_dems; ++i)
        overlapping_dems[i] = NULL;

    process(dem_dir, 0, recursive, overlapping_dems, &n, meta,
        n_dems_total);

    if (n > 0) {
        asfPrintStatus("  Found %d overlapping dems.\n", n);
        for (i=0; i<max_dems; ++i)
            if (overlapping_dems[i])
                asfPrintStatus("    %s\n", overlapping_dems[i]);

        return overlapping_dems;
    } else {
        asfPrintStatus("  No DEMs found in: %s\n", dem_dir);
        free(overlapping_dems);
        return NULL;
    }
}

char **find_overlapping_dems(meta_parameters *meta,
                             const char *file_with_dem_dirs,
                             int *n_dems_found)
{
    // hard-coded limit of 100 dems
    int i;
    const int max_dems = 100;
    char **overlapping_dems = MALLOC(sizeof(char*)*max_dems);
    for (i=0; i<max_dems; ++i)
        overlapping_dems[i] = NULL;

    *n_dems_found = 0;
    int n_dirs_checked = 0;
    int n_dems_total = 0;
    char line[512];

    FILE *fp = FOPEN(file_with_dem_dirs, "r");
    while (NULL != fgets(line, 512, fp)) {
        while (isspace(line[strlen(line)-1])) line[strlen(line)-1] = '\0';
        asfPrintStatus("Looking for DEMs in directory: %s\n", line);
        char **dems = find_overlapping_dems_dir(meta, line, &n_dems_total);
        if (dems) {
            char **p = dems;
            while (*p) {
                if (*n_dems_found < 100) {
                    overlapping_dems[*n_dems_found] = STRDUP(*p);
                    ++(*n_dems_found);
                } else {
                    asfPrintWarning("Too many DEMS!");
                }
                free(*p);
                ++(*p);
            }
            free(dems);
        }
        ++n_dirs_checked;
    }
    fclose(fp);

    asfPrintStatus("In %d directories, found %d DEMS.  %d overlapped.\n",
        n_dirs_checked, n_dems_total, *n_dems_found);

    if (*n_dems_found > 0) {
        asfPrintStatus("Found %d overlapping dems.\n", *n_dems_found);
        for (i=0; i<max_dems; ++i)
            if (overlapping_dems[i])
                asfPrintStatus("    %s\n", overlapping_dems[i]);

        return overlapping_dems;
    } else {
        asfPrintWarning("No DEMs found!\n");
        free(overlapping_dems);
        return NULL;
    }
}
