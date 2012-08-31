#include <asf_terrcorr.h>
#include <stdio.h>
#include <assert.h>
#include <gsl/gsl_spline.h>
#include <asf_raster.h>
#include "float_image.h"
#include "vector.h"

static float height_shift(meta_parameters *meta, double h, double incid)
{
        // shift RIGHT in ascending images, LEFT in descending
        //double shift;
        if (meta->general->orbit_direction=='A')
          //shift = h*tan(PI/2-incid)/meta->general->x_pixel_size;
          return h*sin(PI/2-incid)/meta->general->x_pixel_size;
        else
          //shift = -h*tan(PI/2-incid)/meta->general->x_pixel_size;
          return -h*sin(PI/2-incid)/meta->general->x_pixel_size;
        // convert shift to slant range
        //return shift*cos(PI/2-incid);
}

static double max_arr(double *a, int n)
{
  double max = a[0];
  int i;
  for (i=1; i<n; ++i) {
    if (a[i] > max) max = a[i]; 
  }
  return max;
}

static double min_arr(double *a, int n)
{
  double min = a[0];
  int i;
  for (i=1; i<n; ++i) {
    if (a[i] < min) min = a[i]; 
  }
  return min;
}

void proj_to_linesamp(meta_parameters *meta, double x, double y, double *line, double *samp)
{
  meta_projection *mp = meta->projection;
  *line = (y - mp->startY)/mp->perY;
  *samp = (x - mp->startX)/mp->perX;
}

void sar_to_proj(meta_parameters *meta, meta_projection *mp,
                 double line, double samp, double *y, double *x)
{
  double lat, lon, z;
  int ret = meta_get_latLon(meta, line, samp, 0, &lat, &lon);
  if (ret != 0) {
    asfPrintError("meta_get_latLon error: line = %f, samp = %f\n", line, samp);
  }
  latlon_to_proj(mp, 'R', lat*D2R, lon*D2R, 0, x, y, &z);
  //printf("sar_to_proj: %f %f -> %f %f -> %f %f\n", line, samp, lat, lon, *x, *y);
} 

typedef struct {
    float *prev;
    float *curr;
    float *next;

    Vector *vec_prev;
    Vector *vec_curr;
    Vector *vec_next;
} dem_lines_t;
/*
static void geodetic_to_ecef(double lat, double lon, double h, Vector *v)
{
  const double a = 6378144.0;    // GEM-06 Ellipsoid.
  const double e = 8.1827385e-2; // GEM-06 Eccentricity
  const double e2 = e*e;

  lat *= D2R;
  lon *= D2R;

  double sin_lat = sin(lat);
  double cos_lat = cos(lat);

  double f = sqrt(1 - e2*sin_lat*sin_lat);
  double af = a/f;

  v->x = (af + h)*cos_lat*cos(lon);
  v->y = (af + h)*cos_lat*sin(lon);
  v->z = (a*(1-e2)/f + h)*sin_lat;
}
*/
static void height2vec(meta_parameters *meta, int line, int samp, float height, Vector *v)
{
    v->x = samp;
    v->y = line;
    v->z = height;

    //vector_multiply(v, 1./vector_magnitude(v));
    //double lat, lon;
    //meta_get_latLon(meta, line, samp, 0, &lat, &lon);
    //geodetic_to_ecef(lat, lon, height, v);
}
/*
static void fake_it(float *a, int ns)
{
    int ii;
    for (ii=0; ii<ns; ++ii) a[ii] = 100;
}
*/
static void get_next_dem_lines(dem_lines_t *dem_lines, int line, FILE *fp, meta_parameters *meta)
{
    int jj, ns=meta->general->sample_count;
    if (line == 0) {
        // initialize the structure
        dem_lines->prev = MALLOC(sizeof(float)*ns);
        dem_lines->curr = MALLOC(sizeof(float)*ns);
        dem_lines->next = MALLOC(sizeof(float)*ns);
        dem_lines->vec_prev = MALLOC(sizeof(Vector)*ns);
        dem_lines->vec_curr = MALLOC(sizeof(Vector)*ns);
        dem_lines->vec_next = MALLOC(sizeof(Vector)*ns);

        // read in the heights
        // nothing read in yet, have to read in 2 lines (curr and next) and fake the prev
        get_float_line(fp, meta, 0, dem_lines->curr);
        get_float_line(fp, meta, 1, dem_lines->next);
        //fake_it(dem_lines->curr, ns);
        //fake_it(dem_lines->next, ns);

        // calculate the vectors to each point in ecef
        for (jj=0; jj<ns; ++jj) {
            height2vec(meta, 0, jj, dem_lines->curr[jj], &dem_lines->vec_curr[jj]);
            height2vec(meta, 1, jj, dem_lines->next[jj], &dem_lines->vec_next[jj]);
        }
        // use line 0 as line -1
        memcpy(dem_lines->prev, dem_lines->curr, sizeof(float)*ns);
        memcpy(dem_lines->vec_prev, dem_lines->vec_curr, sizeof(Vector)*ns);
    }
    else {
        memcpy(dem_lines->prev, dem_lines->curr, sizeof(float)*ns);
        memcpy(dem_lines->vec_prev, dem_lines->vec_curr, sizeof(Vector)*ns);
        memcpy(dem_lines->curr, dem_lines->next, sizeof(float)*ns);
        memcpy(dem_lines->vec_curr, dem_lines->vec_next, sizeof(Vector)*ns);
        if (line == meta->general->line_count - 1) {
            // use line n-1 as line n
            memcpy(dem_lines->next, dem_lines->curr, sizeof(float)*ns);
            memcpy(dem_lines->vec_next, dem_lines->vec_curr, sizeof(Vector)*ns);
        }
        else {
            get_float_line(fp, meta, line+1, dem_lines->next);
            //fake_it(dem_lines->next, ns);
            for (jj=0; jj<ns; ++jj) {
                height2vec(meta, line+1, jj, dem_lines->next[jj], &dem_lines->vec_next[jj]);
            }
        }
    }
}

static void free_dem_lines(dem_lines_t *dem_lines)
{
    FREE(dem_lines->prev);
    FREE(dem_lines->curr);
    FREE(dem_lines->next);
    FREE(dem_lines->vec_prev);
    FREE(dem_lines->vec_curr);
    FREE(dem_lines->vec_next);
}

static double calculate_correction(dem_lines_t *dem_lines, double *incid,
                                  int ii, int jj, meta_parameters *meta)
{
    // use same correction for first & last cols as for second & second-to-last cols
    if (jj==0) jj=1;
    if (jj==meta->general->sample_count-1) jj=meta->general->sample_count-2;

    Vector *v1, *v2, *v_next, *v_prev, *n;
    double R, ang;

    v1 = vector_copy(&dem_lines->vec_prev[jj]);
    vector_subtract(v1, &dem_lines->vec_next[jj]);

    v_prev = vector_copy(&dem_lines->vec_curr[jj-1]);
    R = (meta->general->sample_count - (jj-1)) * meta->general->x_pixel_size;
    ang = PI/2 - incid[jj-1];
    v_prev->z -= R * tan(ang);

    v_next = vector_copy(&dem_lines->vec_curr[jj+1]);
    R = (meta->general->sample_count - (jj+1)) * meta->general->x_pixel_size;
    ang = PI/2 - incid[jj+1];
    v_next->z -= R * tan(ang);

    v2 = vector_copy(v_prev);
    vector_subtract(v2, v_next);

    n = vector_cross(v2, v1);
    vector_multiply(n, 1./vector_magnitude(n));

    Vector *Rx = vector_new(0,0,1);
    double cosphi = vector_dot(Rx,n);
    //if (cosphi < 0) cosphi = -cosphi;

    vector_free(Rx);
    vector_free(n);
    vector_free(v1);
    vector_free(v2);
    vector_free(v_next);
    vector_free(v_prev);

    return acos(cosphi); // / sin(incid);
}

void sr_tc(const char *demBase, const char *sarBase, const char *outputBase)
{
  // all this stuff should probably be configurable
  double ps = 12.5;
  project_parameters_t pp;
  projection_type_t proj_type = UNIVERSAL_TRANSVERSE_MERCATOR;
  pp.utm.zone = MAGIC_UNSET_INT;
  pp.utm.lon0 = MAGIC_UNSET_DOUBLE;
  pp.utm.lat0 = MAGIC_UNSET_DOUBLE;
  datum_type_t datum = WGS84_DATUM;
  double background_value = -40;
  int do_radiometric = TRUE;
  // up to here

  meta_parameters *meta_sar = meta_read(sarBase);
  meta_parameters *meta_dem = meta_read(demBase);

  int nb = meta_sar->general->band_count;
  int nl = meta_sar->general->line_count;
  int ns = meta_sar->general->sample_count;
  char **bands = extract_band_names(meta_sar->general->bands, nb);

  double center_lat, center_lon;
  meta_get_latLon(meta_sar, nl/2, ns/2, 0, &center_lat, &center_lon);

  if (proj_type == UNIVERSAL_TRANSVERSE_MERCATOR && pp.utm.zone == MAGIC_UNSET_INT) {
    pp.utm.zone = utm_zone(center_lon);
  }

  meta_projection *mp = meta_projection_init();
  mp->type = proj_type;
  mp->datum = datum;
  mp->param = pp;
  strcpy(mp->units, proj_type == LAT_LONG_PSEUDO_PROJECTION ? "degrees" : "meters");
  mp->hem = center_lat > 0 ? 'N' : 'S';

  double x[4], y[4];
  sar_to_proj(meta_sar, mp, 0, 0, &y[0], &x[0]);
  sar_to_proj(meta_sar, mp, nl-1, 0, &y[1], &x[1]);
  sar_to_proj(meta_sar, mp, 0, ns-1, &y[2], &x[2]);
  sar_to_proj(meta_sar, mp, nl-1, ns-1, &y[3], &x[3]);
  double min_x = min_arr(x,4);
  double max_y = max_arr(y,4);
  double min_y = min_arr(y,4);
  double max_x = max_arr(x,4);

  asfPrintStatus("Extents: x - (%f %f), y - (%f %f)\n",
                min_x, max_x, min_y, max_y);

  // padding to allow for height shifts
  int x_pad = ns/10;
  max_x += x_pad*ps;
  min_x -= x_pad*ps;
  int y_pad = nl/50;
  max_y += y_pad*ps;
  min_y -= y_pad*ps;

  asfPrintStatus("Padded extents: x - (%f %f), y - (%f %f)\n",
                min_x, max_x, min_y, max_y);

  int onl = (max_y - min_y) / ps;
  int ons = (max_x - min_x) / ps;

  char *demImg = appendExt(demBase, ".img");
  char *sarImg = appendExt(sarBase, ".img");
  char *outImg = appendExt(outputBase, ".img");
  char *outMeta = appendExt(outputBase, ".meta");

  FILE *demFp = FOPEN(demImg, "rb");
  FILE *sarFp = FOPEN(sarImg, "rb");

  dem_lines_t dem_lines;
  float *sarBuf = MALLOC(sizeof(float)*ns);
  double *incidArr = MALLOC(sizeof(double)*ns);

  meta_parameters *meta_out = meta_read(sarBase);
  meta_out->projection = mp;

  meta_general *mg = meta_out->general;
  mg->line_count = onl;
  mg->sample_count = ons;
  mg->x_pixel_size = ps;
  mg->y_pixel_size = ps; 
  mg->start_line = 0;
  mg->start_sample = 0;
  mg->line_scaling = 1;
  mg->sample_scaling = 1;
  mg->no_data = background_value;

  mp->startX = min_x;
  mp->startY = max_y;
  mp->perX = ps;
  mp->perY = -ps;
  mp->height = 0;
  mp->datum = datum;
 
  FloatImage *fiOut;
  if (background_value != 0)
    fiOut = float_image_new_with_value(ons, onl, background_value);
  else
    fiOut = float_image_new(ons, onl);

  FloatImage *fiCorr = float_image_new_with_value(ons, onl, 1.0);
 
  int ii, jj;
  for (jj=0; jj<ns; ++jj)
    incidArr[jj] = meta_incid(meta_sar, nl/2, jj);
  for (ii=0; ii<nl; ++ii) {
  //for (ii=0; ii<2500; ++ii) {
    get_next_dem_lines(&dem_lines, ii, demFp, meta_dem); 
    get_float_line(sarFp, meta_sar, ii, sarBuf);
    int last_s=0, last_l=0;
    for (jj=0; jj<ns; ++jj) {
      float val = sarBuf[jj];
      double h = (double)dem_lines.curr[jj];
      if (h<-900) continue;
      double incid = incidArr[jj];
      double shift = height_shift(meta_sar, h, incid);
      double samp = jj + shift;
      double proj_x, proj_y, proj_line, proj_samp;
      sar_to_proj(meta_sar, mp, ii, samp, &proj_y, &proj_x);
      proj_to_linesamp(meta_out, proj_x, proj_y, &proj_line, &proj_samp);
      int l = (int)(proj_line + .5);
      int s = (int)(proj_samp + .5);
      float c=1;
      if (l >=0 && s >= 0 && s < ons && l < onl) {
        // already something in the output image?  Skip if so
        if (float_image_get_pixel(fiOut, s, l) == background_value) {
          if (do_radiometric) {
            c = calculate_correction(&dem_lines, incidArr, ii, jj, meta_sar);
            //val = get_rad_cal_dn(meta_sar, l, s, bands[0], val, c);
            val *= cos((double)c) / sin(incid);
            float_image_set_pixel(fiCorr, s, l, c*R2D);
            //val = 10.0*log10((double)val);
          }
          float_image_set_pixel(fiOut, s, l, val);
        }
        //if (jj>0 && s > last_s+1 && last_l == l) {
        //  int kk;
        //  for (kk=last_s+1; kk<s; ++kk) {
        //    if (do_radiometric && float_image_get_pixel(fiCorr, kk, l) == 1)
        //      float_image_set_pixel(fiCorr, kk, l, c*R2D);
        //    if (float_image_get_pixel(fiOut, kk, l) == background_value)
        //      float_image_set_pixel(fiOut, kk, l, val);
        //  }
        //}
      }
      //else
        //printf("Out of image %d %d -> %d %d (%f %f)\n", ii, jj, l, s, proj_x, proj_y);
      //printf("%d %d --> %f %f --> %d %d\n", ii, jj, proj_x, proj_y, l, s);
      //if (jj>1100) exit(1);
      last_s = s;
      last_l = l;
    }
    asfLineMeter(ii,nl);
  }
  free_dem_lines(&dem_lines);

  // this is to address the off-by-one error introduced by flipping the image
  //mp->startY += ps;

  // trying to clean up the output a little
  for (ii=1; ii<fiOut->size_y-1; ++ii) {
    for (jj=1; jj<fiOut->size_x-1; ++jj) {
      float val = float_image_get_pixel(fiOut, jj, ii);
      if (val == background_value) {
        float valp = float_image_get_pixel(fiOut, jj-1, ii);
        float valn = float_image_get_pixel(fiOut, jj+1, ii);
        if (valp != background_value && valn != background_value) {
          float_image_set_pixel(fiOut, jj, ii, .5*(valp+valn)); 
          if (do_radiometric)
            float_image_set_pixel(fiCorr, jj, ii, float_image_get_pixel(fiCorr, jj-1, ii)); 
        }
        else {
          float valp = float_image_get_pixel(fiOut, jj, ii-1);
          float valn = float_image_get_pixel(fiOut, jj, ii+1);
          if (valp != background_value && valn != background_value) {
            float_image_set_pixel(fiOut, jj, ii, .5*(valp+valn));
            if (do_radiometric)
              float_image_set_pixel(fiCorr, jj, ii, float_image_get_pixel(fiCorr, jj, ii-1));
          }
        }
      }
    }
  }
          
  meta_write(meta_out, outMeta);

  const float_image_byte_order_t fibo_be = FLOAT_IMAGE_BYTE_ORDER_BIG_ENDIAN;
  int ret = float_image_store(fiOut, outImg, fibo_be);
  if (ret!=0)
    asfPrintError("Error storing output image!\n");

  float_image_free(fiOut);

  meta_write(meta_out, "corr.meta");
  float_image_store(fiCorr, "corr.img", fibo_be);

  FCLOSE(demFp);
  FCLOSE(sarFp);

  for (ii=0; ii<nb; ++ii)
    FREE(bands[ii]);
  FREE(bands);

  FREE(demImg);
  FREE(sarImg);
  FREE(outImg);
  FREE(outMeta);
  FREE(sarBuf);
  FREE(incidArr);

  meta_free(meta_out);
  meta_free(meta_sar);
  meta_free(meta_dem);
}

