#include <asf_meta.h>

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#ifdef linux
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>

#include "asf.h"
#include "asf_meta.h"
#include "asf_nan.h"
#include "asf_sar.h"
#include "asf_terrcorr.h"
#include "asf_geocode.h"
#include "asf_raster.h"
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

//  public domain function by Darel Rex Finley, 2006
//  modified for ASF by kh.  Code was found at: http://alienryderflex.com/intersect/

//  Determines the intersection point of the line segment defined by points A and B
//  with the line segment defined by points C and D.
//
//  Returns YES if the intersection point was found.
//  Returns NO if there is no determinable intersection point.

//  Known bug: returns FALSE if the segments are colinear, even if they overlap

int lineSegmentsIntersect(
    double Ax, double Ay,
    double Bx, double By,
    double Cx, double Cy,
    double Dx, double Dy)
{
  double  distAB, theCos, theSin, newX, ABpos ;

  //  Fail if either line segment is zero-length.
  if ((Ax==Bx && Ay==By) || (Cx==Dx && Cy==Dy)) return FALSE;

  //  (1) Translate the system so that point A is on the origin.
  Bx-=Ax; By-=Ay;
  Cx-=Ax; Cy-=Ay;
  Dx-=Ax; Dy-=Ay;

  //  Discover the length of segment A-B.
  distAB=sqrt(Bx*Bx+By*By);

  //  (2) Rotate the system so that point B is on the positive X axis.
  theCos=Bx/distAB;
  theSin=By/distAB;
  newX=Cx*theCos+Cy*theSin;
  Cy  =Cy*theCos-Cx*theSin; Cx=newX;
  newX=Dx*theCos+Dy*theSin;
  Dy  =Dy*theCos-Dx*theSin; Dx=newX;

  //  Fail if segment C-D doesn't cross line A-B.
  if ((Cy<0. && Dy<0.) || (Cy>=0. && Dy>=0.)) return FALSE;

  //  (3) Discover the position of the intersection point along line A-B.
  ABpos=Dx+(Cx-Dx)*Dy/(Dy-Cy);

  //  Fail if segment C-D crosses line A-B outside of segment A-B.
  if (ABpos<0. || ABpos>distAB) return FALSE;

  //  Success.
  return TRUE;
}

// return TRUE if there is any overlap between the two scenes
static int test_overlap(meta_parameters *meta1, meta_parameters *meta2)
{
    if (!meta_is_valid_double(meta1->general->center_longitude)) {
        int nl = meta1->general->line_count;
        int ns = meta1->general->sample_count;

        meta_get_latLon(meta1, nl/2, ns/2, 0,
            &meta1->general->center_latitude,
            &meta1->general->center_longitude);
    }

    if (!meta_is_valid_double(meta2->general->center_longitude)) {
        int nl = meta2->general->line_count;
        int ns = meta2->general->sample_count;

        meta_get_latLon(meta2, nl/2, ns/2, 0,
            &meta2->general->center_latitude,
            &meta2->general->center_longitude);
    }

    int zone1 = utm_zone(meta1->general->center_longitude);
    int zone2 = utm_zone(meta2->general->center_longitude);

    // if zone1 & zone2 differ by more than 1, we can stop now
    if (iabs(zone1-zone2) > 1) {
        return FALSE;
    }

    // The Plan:
    // Generate polygons for each metadata, then test of any pair of
    // line segments between the polygons intersect.

    // Other possibility: meta1 is completely contained within meta2,
    // or the reverse.

    // corners of meta1.
    double xp_1[5], yp_1[5];

    if (meta1->location) {
        // use the location block if available
        latLon2UTM_zone(meta1->location->lat_start_near_range,
            meta1->location->lon_start_near_range, 0, zone1, &xp_1[0], &yp_1[0]);
        latLon2UTM_zone(meta1->location->lat_start_far_range,
            meta1->location->lon_start_far_range, 0, zone1, &xp_1[1], &yp_1[1]);
        latLon2UTM_zone(meta1->location->lat_end_far_range,
            meta1->location->lon_end_far_range, 0, zone1, &xp_1[2], &yp_1[2]);
        latLon2UTM_zone(meta1->location->lat_end_near_range,
            meta1->location->lon_end_near_range, 0, zone1, &xp_1[3], &yp_1[3]);
    } else {
        double lat, lon;
        int nl1 = meta1->general->line_count;
        int ns1 = meta1->general->sample_count;

        // must call meta_get_latLon for each corner
        meta_get_latLon(meta1, 0, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_1[0], &yp_1[0]);

        meta_get_latLon(meta1, nl1-1, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_1[1], &yp_1[1]);

        meta_get_latLon(meta1, nl1-1, ns1-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_1[2], &yp_1[2]);

        meta_get_latLon(meta1, 0, ns1-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_1[3], &yp_1[3]);
    }

    // close the polygon
    xp_1[4] = xp_1[0];
    yp_1[4] = yp_1[0];

    // corners of meta2.
    double xp_2[5], yp_2[5];

    if (meta2->location) {
        // use the location block if available
        latLon2UTM_zone(meta2->location->lat_start_near_range,
            meta2->location->lon_start_near_range, 0, zone1, &xp_2[0], &yp_2[0]);
        latLon2UTM_zone(meta2->location->lat_start_far_range,
            meta2->location->lon_start_far_range, 0, zone1, &xp_2[1], &yp_2[1]);
        latLon2UTM_zone(meta2->location->lat_end_far_range,
            meta2->location->lon_end_far_range, 0, zone1, &xp_2[2], &yp_2[2]);
        latLon2UTM_zone(meta2->location->lat_end_near_range,
            meta2->location->lon_end_near_range, 0, zone1, &xp_2[3], &yp_2[3]);
    } else {
        double lat, lon;
        int nl2 = meta2->general->line_count;
        int ns2 = meta2->general->sample_count;

        // must call meta_get_latLon for each corner
        meta_get_latLon(meta2, 0, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_2[0], &yp_2[0]);

        meta_get_latLon(meta2, nl2-1, 0, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_2[1], &yp_2[1]);

        meta_get_latLon(meta2, nl2-1, ns2-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_2[2], &yp_2[2]);

        meta_get_latLon(meta2, 0, ns2-1, 0, &lat, &lon);
        latLon2UTM_zone(lat, lon, 0, zone1, &xp_2[3], &yp_2[3]);
    }

    // close the polygon
    xp_2[4] = xp_2[0];
    yp_2[4] = yp_2[0];

    // loop over each pair of line segments, testing for intersection
    int i, j;
    for (i = 0; i < 4; ++i) {
        for (j = 0; j < 4; ++j) {
            if (lineSegmentsIntersect(
                xp_1[i], yp_1[i], xp_1[i+1], yp_1[i+1],
                xp_2[j], yp_2[j], xp_2[j+1], yp_2[j+1]))
            {
                return TRUE;
            }
        }
    }

    // test for containment: meta2 in meta1
    int all_in=TRUE;
    for (i=0; i<4; ++i) {
        if (!pnpoly(5, xp_1, yp_1, xp_2[i], yp_2[i])) {
            all_in=FALSE;
            break;
        }
    }

    if (all_in)
        return TRUE;

    // test for containment: meta1 in meta2
    all_in = TRUE;
    for (i=0; i<4; ++i) {
        if (!pnpoly(5, xp_2, yp_2, xp_1[i], yp_1[i])) {
            all_in=FALSE;
            break;
        }
    }

    if (all_in)
        return TRUE;

    // no overlap
    return FALSE;
}

// this is just to make the recursive searching of directories look nice
static char *spaces(int n)
{
    int i;
    static char buf[256];
    for (i=0; i<256; ++i)
        buf[i] = i<n*3 ? ' ' : '\0';
    return buf;
}

// forward declaration of the wrapper that calls process_dir or
// process_file as appropriate
static void process(const char *what, int level, int recursive,
                    char *overlapping_dems[], int *next_dem_number,
                    meta_parameters *meta, int *n_dems_total);

// process all things in a directory.  Calls "process" to decide
// if the "thing" is a dir (process_dir) or a file (process_file)
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

// if a file is a .img file, test it for overlap, otherwise do nothing
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

        char *meta_filename = appendExt(file, ".meta");
        meta_parameters *meta_dem = meta_read(meta_filename);

        if (meta_dem->general->image_data_type != DEM) {
            does = "Not a DEM";
        } else if (test_overlap(meta, meta_dem)) {
            // overlaps!
            overlapping_dems[*next_dem_number] = STRDUP(file);
            ++(*next_dem_number);
            does = "Overlaps";
        } else {
            does = "No";
        }

        free(meta_filename);
        meta_free(meta_dem);

        asfPrintStatus("  %s%s - %s\n", spaces(level), base, does);
    }
    else if (ext && (strcmp_case(ext, ".meta") == 0 ||
                     strcmp_case(ext, ".ddr") == 0)) {
        // silently ignore the .meta, etc files -- they will be picked up
        // when processing the corresponding .img file
        ;
    }
    else {
        // loudly ignore other stuff in the directory
        asfPrintStatus("  %s%s (ignored)\n", spaces(level), base);
    }
    FREE(base);
}

// calls either process_dir or process_file, above
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

// in a given directory, find all overlapping dems.  Calls
// "process" to do the real work
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
        asfPrintStatus("Found %d overlapping dem%s:\n", n, n==1?"":"s");
        for (i=0; i<max_dems; ++i)
            if (overlapping_dems[i])
                asfPrintStatus("  %s\n", overlapping_dems[i]);

        return overlapping_dems;
    } else {
        if (*n_dems_total == 0)
            asfPrintStatus("No DEMs found in: %s\n", dem_dir);
        else
            asfPrintStatus("No overlapping DEMs found in: %s\n", dem_dir);
        free(overlapping_dems);
        return NULL;
    }
}

// given a metadata file, and a file that contains a list of
// directories containing DEMs, return the DEMs that overlap
// with the given metadata.
static char **find_overlapping_dems(meta_parameters *meta,
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

    FILE *fp = fopen(file_with_dem_dirs, "r");
    if (!fp) {
        asfPrintError("Failed to open: %s\n", file_with_dem_dirs);
    }

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
        asfPrintStatus("Found %d overlapping dem%s:\n", *n_dems_found,
            *n_dems_found == 1 ? "" : "s");
        for (i=0; i<max_dems; ++i)
            if (overlapping_dems[i])
                asfPrintStatus("    %s\n", overlapping_dems[i]);

        return overlapping_dems;
    } else {
        asfPrintWarning("No DEMs found!\n");
        free(overlapping_dems);
        return NULL;
    }

    // not reached
}

static int try_ext(const char *filename, const char *ext)
{
    char *buf = MALLOC(sizeof(char)*(strlen(filename)+strlen(ext)+5));
    if (ext[0]=='.' && filename[strlen(filename)-1]=='.')
        sprintf(buf, "%s%s", filename, ext+1);
    else if (ext[0]=='.' || filename[strlen(filename)-1]=='.')
        sprintf(buf, "%s%s", filename, ext);
    else
        sprintf(buf, "%s.%s", filename, ext);

    int ret = fileExists(buf);
    free(buf);

    return ret;
}

// can get rid of this, I think -- use the one in asf.a
static int is_dir_s(const char *what)
{
    struct stat stbuf;
    if (stat(what, &stbuf) == -1)
        return FALSE;
    return (stbuf.st_mode & S_IFMT) == S_IFDIR;
}

static void update_extents(double x, double y,
                           double *x_lo, double *x_hi,
                           double *y_lo, double *y_hi)
{
    if (x < *x_lo) *x_lo = x;
    if (y < *y_lo) *y_lo = y;

    if (x > *x_hi) *x_hi = x;
    if (y > *y_hi) *y_hi = y;
}

static void get_bounding_box_latlon(meta_parameters *meta,
                                    double *lat_lo, double *lat_hi,
                                    double *lon_lo, double *lon_hi)
{
    *lat_lo = *lon_lo = 999;
    *lat_hi = *lon_hi = -999;
    double size_lat;
    double size_lon;
    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    if (meta->location) {
        meta_location *ml = meta->location;
        update_extents(ml->lat_start_near_range, ml->lon_start_near_range,
            lat_lo, lat_hi, lon_lo, lon_hi);
        update_extents(ml->lat_start_far_range, ml->lon_start_far_range,
            lat_lo, lat_hi, lon_lo, lon_hi);
        update_extents(ml->lat_end_near_range, ml->lon_end_near_range,
            lat_lo, lat_hi, lon_lo, lon_hi);
        update_extents(ml->lat_end_far_range, ml->lon_end_far_range,
            lat_lo, lat_hi, lon_lo, lon_hi);

        size_lat = fabs(ml->lat_start_far_range - ml->lat_start_near_range);
        size_lon = fabs(ml->lon_start_far_range - ml->lon_start_near_range);
    }
    else {
        double lat, lon;

        // must call meta_get_latLon for each corner
        meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
        update_extents(lat, lon, lat_lo, lat_hi, lon_lo, lon_hi);
        size_lat = lat;
        size_lon = lon;

        meta_get_latLon(meta, 0, ns-1, 0, &lat, &lon);
        update_extents(lat, lon, lat_lo, lat_hi, lon_lo, lon_hi);
        size_lat -= lat;
        size_lon -= lon;

        meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
        update_extents(lat, lon, lat_lo, lat_hi, lon_lo, lon_hi);

        meta_get_latLon(meta, nl-1, ns-1, 0, &lat, &lon);
        update_extents(lat, lon, lat_lo, lat_hi, lon_lo, lon_hi);

        size_lat = fabs(size_lat);
        size_lon = fabs(size_lon);
    }

    // Add a little bit of fudge to each -- we want there to be some room
    // for adjustment via the co-registration.

    // Try to add about 100 pixels worth to each top/left/bottom/right.
    // To get that much, we estimate how many pixels per lat & long degree
    // from the "size_lat/lon" variables we calculated above.

    // We aren't really concerned if this isn't too accurate.

    double lat_fudge = size_lat / (double)ns * 100;
    double lon_fudge = size_lon / (double)ns * 100;

    *lat_lo -= lat_fudge;
    *lat_hi += lat_fudge;

    *lon_lo -= lon_fudge;
    *lon_hi += lon_fudge;
}

static void get_bounding_box_linesamp(meta_parameters *metaSAR,
                                      meta_parameters *metaDEM,
                                      double *line_lo, double *line_hi,
                                      double *samp_lo, double *samp_hi,
                                      int h_padding, int v_padding)
{
    *line_lo = metaDEM->general->line_count;
    *samp_lo = metaDEM->general->sample_count;
    *line_hi = *samp_hi = 0;

    // must call meta_get_latLon for each corner
    int nl = metaSAR->general->line_count;
    int ns = metaSAR->general->sample_count;
    double lat, lon, line, samp;

    meta_get_latLon(metaSAR, 0, 0, 0, &lat, &lon);
    meta_get_lineSamp(metaDEM, lat, lon, 0, &line, &samp);
    if (meta_is_valid_double(line) && meta_is_valid_double(samp))
        update_extents(line, samp, line_lo, line_hi, samp_lo, samp_hi);

    meta_get_latLon(metaSAR, 0, ns-1, 0, &lat, &lon);
    meta_get_lineSamp(metaDEM, lat, lon, 0, &line, &samp);
    if (meta_is_valid_double(line) && meta_is_valid_double(samp))
        update_extents(line, samp, line_lo, line_hi, samp_lo, samp_hi);

    meta_get_latLon(metaSAR, nl-1, 0, 0, &lat, &lon);
    meta_get_lineSamp(metaDEM, lat, lon, 0, &line, &samp);
    if (meta_is_valid_double(line) && meta_is_valid_double(samp))
        update_extents(line, samp, line_lo, line_hi, samp_lo, samp_hi);

    meta_get_latLon(metaSAR, nl-1, ns-1, 0, &lat, &lon);
    meta_get_lineSamp(metaDEM, lat, lon, 0, &line, &samp);
    if (meta_is_valid_double(line) && meta_is_valid_double(samp))
        update_extents(line, samp, line_lo, line_hi, samp_lo, samp_hi);

    // Add a little bit of fudge to each -- we want there to be some room
    // for adjustment via the co-registration.

    // Add about some pixels worth to each top/left/bottom/right.
    nl = metaDEM->general->line_count;
    ns = metaDEM->general->sample_count;

    *line_lo -= v_padding;
    if (*line_lo < 0) *line_lo = 0;
    if (*line_lo > nl-1) *line_lo = nl-1;

    *line_hi += v_padding;
    if (*line_hi < 0) *line_hi = 0;
    if (*line_hi > nl-1) *line_hi = nl-1;

    *samp_lo -= h_padding;
    if (*samp_lo < 0) *samp_lo = 0;
    if (*samp_lo > ns-1) *samp_lo = ns-1;

    *samp_hi += h_padding;
    if (*samp_hi < 0) *samp_hi = 0;
    if (*samp_hi > ns-1) *samp_hi = ns-1;
}

static int asf_mosaic_utm(char **files, char *outfile, int zone,
                          double lat_lo, double lat_hi,
                          double lon_lo, double lon_hi,
                          double background_val)
{
    project_parameters_t pp;
    projection_type_t projection_type = UNIVERSAL_TRANSVERSE_MERCATOR;

    pp.utm.zone = zone;

    // force calculation of these values
    pp.utm.lon0 = MAGIC_UNSET_DOUBLE;
    pp.utm.lat0 = MAGIC_UNSET_DOUBLE;

    int force = TRUE;
    resample_method_t resample = RESAMPLE_BILINEAR;
    double height = 0;
    datum_type_t datum = WGS84_DATUM;
    spheroid_type_t spheroid = WGS84_SPHEROID;
    double ps = -1;
    char overlap[25]="OVERLAY";

    return asf_mosaic(&pp, projection_type, force, resample, height,
	datum, spheroid, ps, TRUE, 0, files, outfile, background_val,
        lat_lo, lat_hi, lon_lo, lon_hi, overlap, FALSE);
}

// External entry point
//  --> meta: SAR metadata
//  --> dem_cla_arg: either (1) a DEM, (2) a directory with DEMs,
//                   (3) a file containing directories of DEMs.
// In case (1), nothing is done, return value is dem_cla_arg.
// In case (2), the directory is scanned and a DEM is built from
//              the DEMs found in that directory.
// In case (3), All of the directories are scanned, and a DEM is
//              built from the DEMs in all of the directories.
char *build_dem(meta_parameters *meta, const char *dem_cla_arg,
                const char *dir_for_tmp_dem)
{
    char *ext = findExt(dem_cla_arg);
    if (ext) {
        // check against known DEM extensions
        if (strcmp_case(ext, ".img") == 0 ||
            strcmp_case(ext, ".tif") == 0 ||
            strcmp_case(ext, ".tiff") == 0)
        {
            asfPrintStatus("%s: DEM.\n", dem_cla_arg);
            // presumably, this is a DEM -- case (1) above.
            return STRDUP(dem_cla_arg);
        }
    }
    else {
        // no extension -- user may have just provided the basename
        if (try_ext(dem_cla_arg, ".img")) {
            asfPrintStatus("%s: DEM.\n", dem_cla_arg);
            return STRDUP(dem_cla_arg);
        } else if (try_ext(dem_cla_arg, ".tiff")) {
            asfPrintStatus("%s: DEM.\n", dem_cla_arg);
            return STRDUP(dem_cla_arg);
        } else if (try_ext(dem_cla_arg, ".tif")) {
            asfPrintStatus("%s: DEM.\n", dem_cla_arg);
            return STRDUP(dem_cla_arg);
        }
    }

    char **list_of_dems = NULL;
    // Eliminated case (1) -- try case (2)
    if (is_dir_s(dem_cla_arg)) {
        asfPrintStatus("%s: directory containing DEMs.\n", dem_cla_arg);
        int n;
        list_of_dems =
            find_overlapping_dems_dir(meta, dem_cla_arg, &n);
    }
    else {
        // this is case (3)
        asfPrintStatus("%s: file containing directories of DEMs.\n", dem_cla_arg);
        if (fileExists(dem_cla_arg)) {
            int n;
            list_of_dems =
                find_overlapping_dems(meta, dem_cla_arg, &n);
        }
    }

    if (list_of_dems) {
        // form a bounding box
        double lat_lo, lat_hi, lon_lo, lon_hi;
        get_bounding_box_latlon(meta, &lat_lo, &lat_hi, &lon_lo, &lon_hi);

        // hard-coded name of the built dem
        char *built_dem;
        if (strlen(dir_for_tmp_dem) > 0) {
            built_dem = MALLOC(sizeof(char)*(strlen(dir_for_tmp_dem)+10));
            sprintf(built_dem, "%s/built_dem", dir_for_tmp_dem);
        } else
            built_dem = STRDUP("built_dem");

        // always geocode to utm -- we may wish change this to use the
        // user's preferred projection...
        asf_mosaic_utm(list_of_dems, built_dem,
            utm_zone(meta->general->center_longitude), lat_lo, lat_hi,
            lon_lo, lon_hi, meta->general->no_data);

        asfPrintStatus("Constructed DEM: %s\n", built_dem);
        return built_dem;
    }
    else {
        asfPrintError("DEM not found: %s\n", dem_cla_arg);
        return NULL; // not reached
    }
}

// External entry point
int get_dem_chunk(char *dem_in, char *dem_out, meta_parameters *metaDEM,
                  meta_parameters *metaSAR)
{
    // this is the amount of padding that is added to the edge of
    // the cut dem.  We add quite a bit, especially in the vertical
    // direction, since it is not unusual to see JERS imagery, for
    // example, shifting by 800 pixels in azimuth.
    const int h_padding = 200;
    const int v_padding = 800;

    // find bounding line/sample box
    double line_lo, line_hi, samp_lo, samp_hi;
    get_bounding_box_linesamp(metaSAR, metaDEM,
        &line_lo, &line_hi, &samp_lo, &samp_hi, h_padding, v_padding);

    int nl = metaDEM->general->line_count;
    int ns = metaDEM->general->sample_count;
    int lines = (int)(line_hi - line_lo);
    int samps = (int)(samp_hi - samp_lo);
    double pct = 100. * (float)lines/(float)nl * (float)samps / (float)ns;

    asfPrintStatus("DEM size: %dx%d LxS\n", metaDEM->general->line_count,
        metaDEM->general->sample_count);
    asfPrintStatus("SAR image occupies lines %d-%d, samples %d-%d in the DEM. (%.2f%%)\n",
        (int)line_lo, (int)line_hi, (int)samp_lo, (int)samp_hi, pct);

    // chop out a section if we cover less than 20%
    if (pct < 20)
    {
        asfPrintStatus("Cutting out a section of the DEM...\n");

        // cut out this piece
        trim(dem_in, dem_out, (int)samp_lo, (int)line_lo, samps, lines);
        return TRUE;
    }
    else {
        // no action needed
        return FALSE;
    }
}
