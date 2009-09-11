#include "asf.h"
#include "asf_meta.h"
#include "dateUtil.h"

#include <assert.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>


static FILE *fopen_workreport_ext(const char *fileName, report_level_t level)
{
  char *path = getPath(fileName);
  char *basename = get_basename(fileName);
  char *txtFile = MALLOC(sizeof(char) * (strlen(path)+strlen(fileName)+20));
  
  char *p;
  if (strncmp_case(basename, "LED-", 4)==0)
    p = basename + 4;
  else
    p = basename;

  if (strlen(path) > 0)
    sprintf(txtFile, "%s/%s", path, p);
  else
    strcpy(txtFile, p);

  char *workreport_filename = appendExt(txtFile, ".txt");

  // first attempt: basename.txt
  FILE *fp;
  if (!fileExists(workreport_filename)) {
    // second attempt: path/'workreport'
    FREE(workreport_filename);
    workreport_filename = MALLOC(sizeof(char) * (strlen(path) + 20));

    if (strlen(path) > 0)
      sprintf(workreport_filename, "%s/workreport", path);
    else
      strcpy(workreport_filename, "workreport");

    if (!fileExists(workreport_filename)) {

      // third attempt: path/'summary.txt'
      if (strlen(path) > 0)
        sprintf(workreport_filename, "%s/summary.txt", path);
      else
        strcpy(workreport_filename, "summary.txt");

      if (!fileExists(workreport_filename)) {
        // failed!
        fp = NULL;
      }
      else {
        // success with 'summary.txt'
        asfReport(level, "workreport file found as: summary.txt\n");
        fp = FOPEN(workreport_filename, "r");
      }
    }
    else {
      // success with 'workreport'
      asfReport(level, "workreport file found as: workreport\n");
      fp = FOPEN(workreport_filename, "r");
    }
  }
  else {
    // success with 'basename.txt'
    char *basename = get_basename(workreport_filename);
    asfReport(level, "workreport file found as: %s\n", basename);
    fp = FOPEN(workreport_filename, "r");
    FREE(basename);
  }

  FREE(workreport_filename);
  FREE(txtFile);
  FREE(path);
  FREE(basename);

  return fp;
}

FILE *fopen_workreport(const char *fileName)
{
  return fopen_workreport_ext(fileName, REPORT_LEVEL_STATUS);
}

// Get the delta image time for ALOS data out of the summary file
int get_alos_delta_time (const char *fileName, double *delta)
{
  struct dataset_sum_rec dssr;
  hms_time dssr_time, summary_time, start_time, end_time;
  ymd_date dssr_date, summary_date, start_date, end_date;
  char line[512], dateStr[30], *str;

  get_dssr(fileName, &dssr);
  date_dssr2date(dssr.inp_sctim, &dssr_date, &dssr_time);

  FILE *fp = fopen_workreport(fileName);
  if (!fp) {
    // no workreport file...
    *delta = 0;
    return FALSE;
  }

  while (fgets(line, 512, fp)) {
    if (strstr(line, "Img_SceneCenterDateTime")) {
      str = strchr(line, '"');
      sprintf(dateStr, "%s", str+1);
      dateStr[strlen(dateStr)-2] = '\0';
      date_alos2date(dateStr, &summary_date, &summary_time);
      // bumped up the tolerance to 2 seconds... can't see how this would
      // introduce any false positives, and definitely reduces false negatives
      if (date_difference(&dssr_date, &dssr_time,
          &summary_date, &summary_time) > 2.0)
      {
        asfPrintWarning("Summary file does not correspond to leader file.\n"
                        "DSSR: %s\nSummary: %s\n",
                        dssr.inp_sctim, dateStr);
        *delta = 0;
        FCLOSE(fp);
        return FALSE;
      }
    }
    else if (strstr(line, "Img_SceneStartDateTime")) {
      str = strchr(line, '"');
      sprintf(dateStr, "%s", str+1);
      dateStr[strlen(dateStr)-2] = '\0';
      date_alos2date(dateStr, &start_date, &start_time);
    }
    else if (strstr(line, "Img_SceneEndDateTime")) {
      str = strchr(line, '"');
      sprintf(dateStr, "%s", str+1);
      dateStr[strlen(dateStr)-2] = '\0';
      date_alos2date(dateStr, &end_date, &end_time);
    }
  }

  *delta = date_difference(&start_date, &start_time, &end_date, &end_time);
  FCLOSE(fp);
  return TRUE;
}

// Get the SAR processor version from the workreport
int get_alos_processor_version (const char *fileName, double *version)
{
  char line[512], versionStr[10], *str;

  FILE *fp = fopen_workreport_ext(fileName, REPORT_LEVEL_NONE);
  if (!fp) {
    // no workreport file...
    *version = 0.0;
    return FALSE;
  }

  while (fgets(line, 512, fp)) {
    if (strstr(line, "Ver_PSR_CorPrcSigmaSAR")) {
      str = strchr(line, '"');
      sprintf(versionStr, "%s", str+1);
      versionStr[strlen(versionStr)-2] = '\0';
      *version = atof(versionStr);
    }
  }

  FCLOSE(fp);
  return TRUE;
}


// ------------------------------------------------------------------------
// helper code for refine_slc_geolocation_from_workreport()

struct refine_shift_params {
    meta_parameters *meta;
    double center_lat, center_lon;
    double ul_lat, ul_lon;
    double ur_lat, ur_lon;
    double ll_lat, ll_lon;
    double lr_lat, lr_lon;
};

static double lldist(double lat1, double lat2, double lon1, double lon2)
{
    double dlat = lat1 - lat2;
    double dlon;

    // some kludgery to handle crossing the meridian
    // get lat1,lon1 to be on the same side as lat2,lon2
    if (fabs(lon1-lon2) > 300) {
        if (lon2 < 0 && lon1 > 0) lon1 -= 360;
        if (lon2 > 0 && lon1 < 0) lon1 += 360;
    }
    
    dlon = lon1 - lon2;
    
    // Scale longitude difference to take into accound the fact
    // that longitude lines are a lot closer at the pole.
    dlon *= cos (lat2 * PI / 180.0);

    return dlat * dlat + dlon * dlon;
}

static double err_at_pixel(struct refine_shift_params *p, double off_t,
                           double off_x, double line, double samp,
                           double real_lat, double real_lon)
{
    const double huge_err = 999999.9;

    meta_parameters *meta = p->meta;
    double lat, lon;
    int bad=0;

    bad = meta_get_latLon(meta, line, samp, 0.0, &lat, &lon);
    if (bad) return huge_err;

    return lldist(lat, real_lat, lon, real_lon);
}

static int
getObjective(const gsl_vector *x, void *params, gsl_vector *f)
{
    double dt = gsl_vector_get(x,0);
    double ds = gsl_vector_get(x,1);

    if (!meta_is_valid_double(ds) || !meta_is_valid_double(dt)) {
        // This does happen sometimes, when we've already found the root
	return GSL_FAILURE;
    }

    struct refine_shift_params *p = (struct refine_shift_params *)params;
    meta_parameters *meta = p->meta;

    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    double saved_timeOffset = meta->sar->time_shift;
    double saved_slant = meta->sar->slant_shift;
    meta->sar->time_shift += dt;
    meta->sar->slant_shift += ds;

    double err = 0.0;

    // add up the errors for the 5 known points
    err += err_at_pixel(p, dt, ds, nl/2, ns/2, p->center_lat, p->center_lon); 

    if (meta->general->orbit_direction=='A') {
      err += err_at_pixel(p, dt, ds, 0,    0,    p->ll_lat, p->ll_lon);
      err += err_at_pixel(p, dt, ds, nl-1, 0,    p->ul_lat, p->ul_lon);
      err += err_at_pixel(p, dt, ds, 0,    ns-1, p->lr_lat, p->lr_lon);
      err += err_at_pixel(p, dt, ds, nl-1, ns-1, p->ur_lat, p->ur_lon);
    }
    else {
      err += err_at_pixel(p, dt, ds, 0,    0,    p->ul_lat, p->ur_lon);
      err += err_at_pixel(p, dt, ds, nl-1, 0,    p->ll_lat, p->lr_lon);
      err += err_at_pixel(p, dt, ds, 0,    ns-1, p->ur_lat, p->ul_lon);
      err += err_at_pixel(p, dt, ds, nl-1, ns-1, p->lr_lat, p->ll_lon);
    }
    
    meta->sar->time_shift = saved_timeOffset;
    meta->sar->slant_shift = saved_slant;

    gsl_vector_set(f,0,err);
    gsl_vector_set(f,1,err);
    return GSL_SUCCESS;
}

static void coarse_search(double t_extent_min, double t_extent_max,
                          double s_extent_min, double s_extent_max,
                          double *t_min, double *s_min,
                          struct refine_shift_params *params)
{
    double the_min = 9999999;
    double min_t=99, min_s=99;
    int i,j,k=6;
    double t_extent = t_extent_max - t_extent_min;
    double s_extent = s_extent_max - s_extent_min;
    gsl_vector *v = gsl_vector_alloc(2);
    gsl_vector *u = gsl_vector_alloc(2);
    //printf("           ");
    //for (j = 0; j <= k; ++j) {
    //    double s = s_extent_min + ((double)j)/k*s_extent;
    //    printf("%9.3f ", s);
    //}
    //printf("\n           ");
    //for (j = 0; j <= k; ++j)
    //    printf("--------- ");
    //printf("\n");
    for (i = 0; i <= k; ++i) {
        double t = t_extent_min + ((double)i)/k*t_extent;
        //printf("%9.3f | ", t);

        for (j = 0; j <= k; ++j) {
            double s = s_extent_min + ((double)j)/k*s_extent;

            gsl_vector_set(v, 0, t);
            gsl_vector_set(v, 1, s);
            getObjective(v,(void*)params, u);
            double n = gsl_vector_get(u,0);
            //printf("%9.3f ", n);
            if (n<the_min) { 
                the_min=n;
                min_t=gsl_vector_get(v,0);
                min_s=gsl_vector_get(v,1);
            }
        }
        //printf("\n");
    }

    *t_min = min_t;
    *s_min = min_s;

    gsl_vector_free(v);
    gsl_vector_free(u);
}

/* this one can be used to do a time-only search (no slant adjustment)
static void coarse_search_t(double t_extent_min, double t_extent_max,
                            double *t_min, struct refine_shift_params *params)
{
    double the_min = 9999999;
    double min_t=99;
    int i,k=10;
    double t_extent = t_extent_max - t_extent_min;
    gsl_vector *v = gsl_vector_alloc(2);
    gsl_vector *u = gsl_vector_alloc(2);
    for (i = 0; i <= k; ++i) {
        double t = t_extent_min + ((double)i)/k*t_extent;

        gsl_vector_set(v, 0, t);
        gsl_vector_set(v, 1, 0);
        getObjective(v,(void*)params, u);
        double n = gsl_vector_get(u,0);
        if (n<the_min) { 
            the_min=n;
            min_t=gsl_vector_get(v,0);
        }
    }

    *t_min = min_t;

    gsl_vector_free(v);
    gsl_vector_free(u);
}
*/

static void generate_start(struct refine_shift_params *params,
                           double *start_t, double *start_s)
{
    int i;

    double extent_t_min = -20;
    double extent_t_max = 20;

    double extent_s_min = -5000;
    double extent_s_max = 5000;

    double t_range = extent_t_max - extent_t_min;
    double s_range = extent_s_max - extent_s_min;

    for (i = 0; i < 12; ++i)
    {
        coarse_search(extent_t_min, extent_t_max, extent_s_min, extent_s_max,
                      start_t, start_s, params);
        //coarse_search_t(extent_t_min, extent_t_max, start_t, params);

        t_range /= 3;
        s_range /= 3;

        extent_t_min = *start_t - t_range/2;
        extent_t_max = *start_t + t_range/2;

        extent_s_min = *start_s - s_range/2;
        extent_s_max = *start_s + s_range/2;

        //printf("refining search to region: (%9.3f,%9.3f)\n"
        //       "                           (%9.3f,%9.3f)\n",
        //       extent_t_min, extent_t_max,
        //       extent_s_min, extent_s_max);
    }
}

/* small debug func
static void print_state(int iter, gsl_multiroot_fsolver *s)
{
    printf("iter = %3d   x = (%.3f %.3f)   f(x) = %.8f\n", iter,
           gsl_vector_get(s->x, 0), gsl_vector_get(s->x, 1),
           gsl_vector_get(s->f, 0));
}
*/

// This won't change the passed-in metadata -- afterwards, if you want
// to apply the found corrections, do this:
//   meta->sar->slant_shift += slant_shift_adjustment;
//   meta->sar->time_shift += time_shift_adjustment;
// usually you will only want to do this if the function returns TRUE
// (on success), though if it returns FALSE (failed) these two parameters
// will be 0
int refine_slc_geolocation_from_workreport(const char *metaName,
                                           const char *basename,
                                           meta_parameters *meta,
                                           double *time_shift_adjustment,
                                           double *slant_shift_adjustment)
{
  char *fileName = MALLOC(sizeof(char)*(10+strlen(basename)+strlen(metaName)));
  char *dirname = get_dirname(metaName);
  if (strlen(dirname)>0)
    sprintf(fileName, "%s%s", dirname, basename);
  else
    strcpy(fileName, basename);
  FILE *fp = fopen_workreport(fileName);
  FREE(dirname);
  if (!fp) {
    // failed to open workreport file
    asfPrintWarning(
"Attempted to refine the geolocation with the workreport file.  However,\n"
"this file could not be found.  To get the most accurate geolocations with\n"
"Palsar SLC data, include the workreport file in the same directory as the\n"
"data file.  First <basename>.txt is checked, then 'workreport'.\n\n"
"Proceeding... however, extremely poor geolocations may result.\n");
    *slant_shift_adjustment = 0.0;
    *time_shift_adjustment = 0.0;
    FREE(fileName);
    return FALSE;
  }

  // Read the workreport file, find the 4 corners and scene center
  // lat and long info.  We also check to make sure this is the right
  // workreport file...

  struct dataset_sum_rec dssr;
  hms_time dssr_time, summary_time;
  ymd_date dssr_date, summary_date;
  char line[512], tmp[512], *p;
  get_dssr(metaName, &dssr);
  date_dssr2date(dssr.inp_sctim, &dssr_date, &dssr_time);

  double center_lat=-999, center_lon=-999;
  double ul_lat=-999, ul_lon=-999;
  double ur_lat=-999, ur_lon=-999;
  double ll_lat=-999, ll_lon=-999;
  double lr_lat=-999, lr_lon=-999;

  while (fgets(line, 512, fp)) {
    if (strstr(line, "Img_SceneCenterDateTime")) {
      p = strchr(line, '"');
      sprintf(tmp, "%s", p+1);
      tmp[strlen(tmp)-2] = '\0';
      date_alos2date(tmp, &summary_date, &summary_time);
      if (date_difference(&dssr_date, &dssr_time,
          &summary_date, &summary_time) > 10.0)
      {
        asfPrintWarning(
"Summary file does not correspond to leader file.\n"
"  DSSR: %s\n  Summary: %s\n"
"This is most likely because you have a mismatched workreport file.  Since\n"
"by default these files are all named 'workreport' this file may have been\n"
"overwritten with another scene's workreport file.  You may wish to rename\n"
"your workreport files using the <basename>.txt convention, for example:\n"
"  %s.txt\n"
"Proceeding... however, extremely poor geolocations may result.\n",
                        dssr.inp_sctim, tmp, fileName);
        *slant_shift_adjustment = 0;
        *time_shift_adjustment = 0;
        FCLOSE(fp);
        FREE(fileName);
        return 0;
      }
    }
    else if (strstr(line, "ImageSceneCenterLatitude")) {
      p = strchr(line, '"');
      if (p) center_lat = atof(p+1);
    }
    else if (strstr(line, "ImageSceneCenterLongitude")) {
      p = strchr(line, '"');
      if (p) center_lon = atof(p+1);
    }
    else if (strstr(line, "ImageSceneLeftTopLatitude")) {
      p = strchr(line, '"');
      if (p) ul_lat = atof(p+1);
    }
    else if (strstr(line, "ImageSceneLeftTopLongitude")) {
      p = strchr(line, '"');
      if (p) ul_lon = atof(p+1);
    }
    else if (strstr(line, "ImageSceneRightTopLatitude")) {
      p = strchr(line, '"');
      if (p) ur_lat = atof(p+1);
    }
    else if (strstr(line, "ImageSceneRightTopLongitude")) {
      p = strchr(line, '"');
      if (p) ur_lon = atof(p+1);
    }
    else if (strstr(line, "ImageSceneLeftBottomLatitude")) {
      p = strchr(line, '"');
      if (p) ll_lat = atof(p+1);
    }
    else if (strstr(line, "ImageSceneLeftBottomLongitude")) {
      p = strchr(line, '"');
      if (p) ll_lon = atof(p+1);
    }
    else if (strstr(line, "ImageSceneRightBottomLatitude")) {
      p = strchr(line, '"');
      if (p) lr_lat = atof(p+1);
    }
    else if (strstr(line, "ImageSceneRightBottomLongitude")) {
      p = strchr(line, '"');
      if (p) lr_lon = atof(p+1);
    }
  }

  FREE(fileName);
  fclose(fp);

  // Did we get all that we needed?
  int ok = TRUE;
  if (center_lat == -999) {
    asfPrintStatus("Missing: ImageSceneCenterLatitude\n");
    ok = FALSE;
  }
  if (center_lon == -999) {
    asfPrintStatus("Missing: ImageSceneCenterLongitude\n");
    ok = FALSE;
  }
  if (ul_lat == -999) {
    asfPrintStatus("Missing: ImageSceneTopLeftLatitude\n");
    ok = FALSE;
  }
  if (ul_lon == -999) {
    asfPrintStatus("Missing: ImageSceneTopLeftLongitude\n");
    ok = FALSE;
  }
  if (ur_lat == -999) {
    asfPrintStatus("Missing: ImageSceneTopRightLatitude\n");
    ok = FALSE;
  }
  if (ur_lon == -999) {
    asfPrintStatus("Missing: ImageSceneTopRightLongitude\n");
    ok = FALSE;
  }
  if (ll_lat == -999) {
    asfPrintStatus("Missing: ImageSceneBottomLeftLatitude\n");
    ok = FALSE;
  }
  if (ll_lon == -999) {
    asfPrintStatus("Missing: ImageSceneBottomLeftLongitude\n");
    ok = FALSE;
  }
  if (lr_lat == -999) {
    asfPrintStatus("Missing: ImageSceneBottomRightLatitude\n");
    ok = FALSE;
  }
  if (lr_lon == -999) {
    asfPrintStatus("Missing: ImageSceneBottomRightLongitude\n");
    ok = FALSE;
  }
  if (!ok) {
    asfPrintWarning("Not all necessary information is available in the "
                    "workreport file.\nProceeding... however, extremely "
                    "poor geolocations may result.\n");    
    *slant_shift_adjustment = 0.0;
    *time_shift_adjustment = 0.0;
    return FALSE;
  }

  // all required info is present-- search for min slant and time shifts

  struct refine_shift_params params;
  params.meta = meta;
  params.center_lat = center_lat;
  params.center_lon = center_lon;
  params.ul_lat = ul_lat;
  params.ul_lon = ul_lon;
  params.ur_lat = ur_lat;
  params.ur_lon = ur_lon;
  params.ll_lat = ll_lat;
  params.ll_lon = ll_lon;
  params.lr_lat = lr_lat;
  params.lr_lon = lr_lon;

  //int status;
  //int iter = 0, max_iter = 1000;
  //const gsl_multiroot_fsolver_type *T;
  //gsl_multiroot_fsolver *s;
  //gsl_error_handler_t *prev;
  //const size_t n = 2;

  //gsl_multiroot_function F = {&getObjective, n, &params};
  //gsl_vector *x = gsl_vector_alloc(n);

  double start_t, start_s;
  generate_start(&params, &start_t, &start_s);
  //printf("Starting point at (%f, %f)\n", start_t, start_s);

/*
  gsl_vector_set (x, 0, start_t);
  gsl_vector_set (x, 1, start_s);

  T = gsl_multiroot_fsolver_hybrid;
  s = gsl_multiroot_fsolver_alloc(T, n);
  gsl_multiroot_fsolver_set(s, &F, x);

  prev = gsl_set_error_handler_off();

  do {
    ++iter;
    status = gsl_multiroot_fsolver_iterate(s);

    //print_state(iter, s);

    // abort if stuck
    if (status) break;

    status = gsl_multiroot_test_residual (s->f, 1e-8);
  } while (status == GSL_CONTINUE && iter < max_iter);

  *time_shift_adjustment = gsl_vector_get(s->x, 0);
  *slant_shift_adjustment = gsl_vector_get(s->x, 1);

  gsl_vector *retrofit = gsl_vector_alloc(n);
  gsl_vector_set(retrofit, 0, *time_shift_adjustment);
  gsl_vector_set(retrofit, 1, *slant_shift_adjustment);
  gsl_vector *output = gsl_vector_alloc(n);
  getObjective(retrofit, (void*)&params, output);
  double val= gsl_vector_get(output,0);
  printf("GSL Result: %f at (%f,%f)\n",
         val, *time_shift_adjustment, *slant_shift_adjustment);
  gsl_vector_free(retrofit);
  gsl_vector_free(output);
  
  gsl_multiroot_fsolver_free(s);
  gsl_vector_free(x);
  gsl_set_error_handler(prev);
*/
  *time_shift_adjustment = start_t;
  *slant_shift_adjustment = start_s;

  // the rest of this is a bunch of debug code
/*
  int nl = meta->general->line_count;
  int ns = meta->general->sample_count;
  double lat,lon;

  if (meta->general->orbit_direction=='A') {

    printf("BEFORE-->\n");
    meta_get_latLon(meta, nl/2, ns/2, 0, &lat, &lon);
    printf("  CN: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,center_lat,center_lon,
           lldist(lat,center_lat,lon,center_lon));
    meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
    printf("  UL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ul_lat,ul_lon,lldist(lat,ul_lat,lon,ul_lon));
    meta_get_latLon(meta, nl-1, ns-1, 0, &lat, &lon);
    printf("  UR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ur_lat,ur_lon,lldist(lat,ur_lat,lon,ur_lon));
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    printf("  LL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ll_lat,ll_lon,lldist(lat,ll_lat,lon,ll_lon));
    meta_get_latLon(meta, 0, ns-1, 0, &lat, &lon);
    printf("  LR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,lr_lat,lr_lon,lldist(lat,lr_lat,lon,lr_lon));
    
    double saved_timeOffset = meta->sar->time_shift;
    double saved_slant = meta->sar->slant_shift;
    meta->sar->time_shift += *time_shift_adjustment;
    meta->sar->slant_shift += *slant_shift_adjustment;
    
    printf("AFTER-->\n");
    meta_get_latLon(meta, nl/2, ns/2, 0, &lat, &lon);
    printf("  CN: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,center_lat,center_lon,
           lldist(lat,center_lat,lon,center_lon));
    meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
    printf("  UL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ul_lat,ul_lon,lldist(lat,ul_lat,lon,ul_lon));
    meta_get_latLon(meta, nl-1, ns-1, 0, &lat, &lon);
    printf("  UR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ur_lat,ur_lon,lldist(lat,ur_lat,lon,ur_lon));
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    printf("  LL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ll_lat,ll_lon,lldist(lat,ll_lat,lon,ll_lon));
    meta_get_latLon(meta, 0, ns-1, 0, &lat, &lon);
    printf("  LR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,lr_lat,lr_lon,lldist(lat,lr_lat,lon,lr_lon));

    meta->sar->time_shift = saved_timeOffset;
    meta->sar->slant_shift = saved_slant;
  }
  else {
    printf("BEFORE-->\n");
    meta_get_latLon(meta, nl/2, ns/2, 0, &lat, &lon);
    printf("  CN: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,center_lat,center_lon,
           lldist(lat,center_lat,lon,center_lon));
    meta_get_latLon(meta, 0, ns-1, 0, &lat, &lon);
    printf("  UL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ul_lat,ul_lon,lldist(lat,ul_lat,lon,ul_lon));
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    printf("  UR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ur_lat,ur_lon,lldist(lat,ur_lat,lon,ur_lon));
    meta_get_latLon(meta, nl-1, ns-1, 0, &lat, &lon);
    printf("  LL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ll_lat,ll_lon,lldist(lat,ll_lat,lon,ll_lon));
    meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
    printf("  LR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,lr_lat,lr_lon,lldist(lat,lr_lat,lon,lr_lon));
    
    double saved_timeOffset = meta->sar->time_shift;
    double saved_slant = meta->sar->slant_shift;
    meta->sar->time_shift += *time_shift_adjustment;
    meta->sar->slant_shift += *slant_shift_adjustment;
    
    printf("AFTER-->\n");
    meta_get_latLon(meta, nl/2, ns/2, 0, &lat, &lon);
    printf("  CN: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,center_lat,center_lon,
           lldist(lat,center_lat,lon,center_lon));
    meta_get_latLon(meta, 0, ns-1, 0, &lat, &lon);
    printf("  UL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ul_lat,ul_lon,lldist(lat,ul_lat,lon,ul_lon));
    meta_get_latLon(meta, 0, 0, 0, &lat, &lon);
    printf("  UR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ur_lat,ur_lon,lldist(lat,ur_lat,lon,ur_lon));
    meta_get_latLon(meta, nl-1, ns-1, 0, &lat, &lon);
    printf("  LL: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,ll_lat,ll_lon,lldist(lat,ll_lat,lon,ll_lon));
    meta_get_latLon(meta, nl-1, 0, 0, &lat, &lon);
    printf("  LR: mgll: %f,%f - wr:%f,%f - err:%f\n",
           lat,lon,lr_lat,lr_lon,lldist(lat,lr_lat,lon,lr_lon));

    meta->sar->time_shift = saved_timeOffset;
    meta->sar->slant_shift = saved_slant;
  }
*/
  return TRUE;
}
