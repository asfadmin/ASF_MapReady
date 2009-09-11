#include "asf.h"
#include "asf_meta.h"
#include "libasf_proj.h"
#include "proj_api.h"

#include <stdio.h>
#include <math.h>
#include <sys/time.h>
#include <assert.h>
#include <stdlib.h>

int n_bad=0, n_ok=0;

/**************************************************************************
  NAD27 -> NAD83 testing functions.  See if we can find the grid shift
  files at runtime.  If this test fails, you should probably set the
  PROJ_DEBUG flag to see if it is finding the shift files, and if not where
  it is looking for them.
**************************************************************************/
static int double_equals(double a, double b, int n_digits)
{
    double tol = pow(10,-n_digits);
    return fabs((a-b)/a)<tol;
}

static void
test_proj_direct(const char *input_projection_info, const char *proj_name,
                 double projX, double projY, double height,
                 double expected_lat, double expected_lon,
                 const char *datum)
{
    double x[1];
    double y[1];
    double hts[1];

    char output_projection_info[255];
    sprintf(output_projection_info, "+proj=latlong +datum=WGS84");

    projPJ input_projection, output_projection;

    input_projection = pj_init_plus(input_projection_info);
    output_projection = pj_init_plus(output_projection_info);
    
    y[0] = projX;
    x[0] = projY;
    hts[0] = height;

    pj_transform (input_projection, output_projection, 1, 1, x, y, hts);

    if (pj_errno != 0) {
        asfPrintWarning("libproj error: %s\n", pj_strerrno(pj_errno));
    }

    x[0] *= R2D;
    y[0] *= R2D;

    if (double_equals(x[0], expected_lat, 6) && 
        double_equals(y[0], expected_lon, 6))
    {
        asfPrintStatus("%s %s (fwd): %f, %f ... OK\n",
                       proj_name, datum, projX, projY);
        ++n_ok;
    }
    else
    {
        asfPrintStatus("%s (fwd):  %s %f, %f ... ERROR\n",
                       proj_name, datum, projX, projY);
        asfPrintStatus("Result:   %.10f %.10f (%.10f)\n",
                       x[0], y[0], hts[0]);
        asfPrintStatus("Expected: %.10f %.10f\n",
                       expected_lat, expected_lon);
        ++n_bad;
    }

    // now project the other way
    y[0] = expected_lon*D2R;
    x[0] = expected_lat*D2R;
    hts[0] = height;

    pj_transform(output_projection, input_projection, 1, 1, x, y, hts);

    if (pj_errno != 0) {
        asfPrintWarning("libproj error: %s\n", pj_strerrno(pj_errno));
    }

    pj_free(input_projection);
    pj_free(output_projection);

    if (double_equals(y[0], projX, 5) && 
        double_equals(x[0], projY, 5))
    {
        asfPrintStatus("%s %s (rev): %f, %f ... OK\n", 
                       proj_name, datum, expected_lon, expected_lat);
        ++n_ok;
    }
    else
    {
        asfPrintStatus("%s (rev):  %s %f, %f ... ERROR\n",
                       proj_name, datum, expected_lon, expected_lat);
        asfPrintStatus("Result:    %.10f %.10f (%.10f)\n",
                       y[0], x[0], hts[0]);
        asfPrintStatus("Expected:  %.10f %.10f\n",
                       projX, projY);
        ++n_bad;
    }
}

static void 
test_utm_direct(double projX, double projY, double height, int zone,
                double expected_lat, double expected_lon,
                const char *datum)
{
    char input_projection_info[255];
    sprintf(input_projection_info, "+proj=utm +zone=%d +datum=%s",
            zone, datum);

    test_proj_direct(input_projection_info, "UTM", projX, projY, height,
                     expected_lat, expected_lon, datum);
}

static int test_nad27_ll(double lat, double lon,
                         double expected_lat, double expected_lon)
{
    double lats[1];
    double lons[1];
    double hts[1];

    char input_projection_info[255];
    char output_projection_info[255];

    projPJ input_projection, output_projection;

    sprintf(input_projection_info, "+proj=latlong +datum=NAD27");
    sprintf(output_projection_info, "+proj=latlong +datum=NAD83");

    input_projection = pj_init_plus(input_projection_info);
    output_projection = pj_init_plus(output_projection_info);
    
    lats[0] = lat * D2R;
    lons[0] = lon * D2R;
    hts[0] = 0;

    pj_transform (input_projection, output_projection, 1, 1, 
                  lats, lons, hts);

    if (pj_errno != 0) {
        asfPrintWarning("libproj error: %s\n", pj_strerrno(pj_errno));
    }

    pj_free(input_projection);
    pj_free(output_projection);

    lats[0] *= R2D;
    lons[0] *= R2D;

    if (double_equals(lats[0], expected_lat, 6) &&
        double_equals(lons[0], expected_lon, 6))
    {
        asfPrintStatus("Proj (fwd): %f, %f ... OK\n", lat, lon);
        return TRUE;
    }
    else
    {
        asfPrintStatus("Proj (fwd): %f, %f ... ERROR\n", lat, lon);
        asfPrintStatus("Result:                 %.10f %.10f (%.10f)\n",
                       lats[0], lons[0], hts[0]);
        asfPrintStatus("Expected:               %.10f %.10f\n",
                       expected_lat, expected_lon);
        return FALSE;
    }
}

typedef int project_fn_t(project_parameters_t *pps,
                       double lat, double lon, double height,
                       double *x, double *y, double *z,
                       datum_type_t datum);

typedef int project_fn_arr_t(project_parameters_t *pps,
                             double *lat, double *lon, double *height,
                             double **x, double **y, double **z, long length,
                             datum_type_t datum);

static void
test_wrapper(const char * proj_name, project_parameters_t *pps,
             project_fn_t *project_fwd,
             project_fn_t *project_rev,
             project_fn_arr_t *project_fwd_arr,
             project_fn_arr_t *project_rev_arr,
             double projY, double projX, double height,
             double lon, double lat, datum_type_t datum)
{
    {
        double x, y, z;
        project_fwd(pps, lat*D2R, lon*D2R, height, &x, &y, &z, datum);
        
        if (double_equals(x, projX, 5) && double_equals(y, projY, 5)) {
            asfPrintStatus("%s Wrapper (fwd): %f, %f ... OK\n",
                           proj_name, lat, lon);
            ++n_ok;
        } else {
            asfPrintStatus("%s Wrapper (fwd): %f, %f ... ERROR\n",
                           proj_name, lat, lon);
            asfPrintStatus("Result:   %.10f %.10f (%.10f)\n", x, y, z);
            asfPrintStatus("Expected: %.10f %.10f\n", projX, projY);
            ++n_bad;
        }
        
        // test the backwards projection, too
        double testLat, testLon;
        project_rev(pps, projX, projY, height, &testLat, &testLon, &z, datum);
        testLat *= R2D;
        testLon *= R2D;
        
        if (double_equals(lat, testLat, 5) && double_equals(lon, testLon, 5)) {
            asfPrintStatus("%s Wrapper (rev): %f, %f ... OK\n",
                           proj_name, projX, projY);
            ++n_ok;
        } else {
            asfPrintStatus("%s Wrapper (rev): %f, %f ... ERROR\n",
                           proj_name, projX, projY);
            asfPrintStatus("Result:   %.10f %.10f (%.10f)\n",
                           testLat, testLon, z);
            asfPrintStatus("Expected: %.10f %.10f\n", lat, lon);
            ++n_bad;
        }
    }

    // also test the array versions
    {
        double *x, *y, *z;
        double lats[1], lons[1], hts[1];
        
        lats[0] = lat*D2R;
        lons[0] = lon*D2R;
        hts[0] = height;
        x = y = z = NULL;

        project_fwd_arr(pps, lats, lons, hts, &x, &y, &z, 1, datum);
        
        if (double_equals(x[0], projX, 5) && double_equals(y[0], projY, 5)) {
            asfPrintStatus("%s Array (fwd): %f, %f ... OK\n",
                           proj_name, lat, lon);
            ++n_ok;
        } else {
            asfPrintStatus("%s Array (fwd): %f, %f ... ERROR\n",
                           proj_name, lat, lon);
            asfPrintStatus("Result:   %.10f %.10f (%.10f)\n",
                           x[0], y[0], z[0]);
            asfPrintStatus("Expected: %.10f %.10f\n", projX, projY);
            ++n_bad;
        }

        free(x);
        free(y);
        free(z);
    }

    {
        double x[1], y[1], z[1];
        double *lats, *lons, *hts;

        x[0] = projX;
        y[0] = projY;
        z[0] = height;
        lats = lons = hts = NULL;

        project_rev_arr(pps, x, y, z, &lats, &lons, &hts, 1, datum);
        lats[0] *= R2D;
        lons[0] *= R2D;

        if (double_equals(lats[0], lat, 5) &&
            double_equals(lons[0], lon, 5))
        {
            asfPrintStatus("%s Array (rev): %f, %f ... OK\n",
                           proj_name, projX, projY);
            ++n_ok;
        } else {
            asfPrintStatus("%s Array (rev): %f, %f ... ERROR\n",
                           proj_name, projX, projY);
            asfPrintStatus("Result:   %.10f %.10f (%.10f)\n",
                           lats[0], lons[0], hts[0]);
            asfPrintStatus("Expected: %.10f %.10f\n", lat, lon);
            ++n_bad;
        }

        free(lats);
        free(lons);
        free(hts);
    }

    // another array version test, this time caller allocates the memory
    // just do the forward transform on this one
    {
        double x[1], y[1], z[1];
        double lats[1], lons[1], hts[1];
        
        lats[0] = lat*D2R;
        lons[0] = lon*D2R;
        hts[0] = height;

        double *px = x;  double **xx = &px;
        double *py = y;  double **yy = &py;
        double *pz = z;  double **zz = &pz;

        project_fwd_arr(pps, lats, lons, hts, xx, yy, zz, 1, datum);

        if (double_equals(x[0], projX, 5) && double_equals(y[0], projY, 5)) {
            asfPrintStatus("%s Array 2 (fwd): %f, %f ... OK\n",
                           proj_name, lat, lon);
            ++n_ok;
        } else {
            asfPrintStatus("%s Array 2 (fwd): %f, %f ... ERROR\n",
                           proj_name, lat, lon);
            asfPrintStatus("Result:   %.10f %.10f (%.10f)\n",
                           x[0], y[0], z[0]);
            asfPrintStatus("Expected: %.10f %.10f\n", projX, projY);
            ++n_bad;
        }
    }
}

static void 
test_utm(double projY, double projX, double height, int zone,
         double lon, double lat, datum_type_t datum)
{
    project_parameters_t pps;
    pps.utm.zone = zone;
    pps.utm.lat0 = lat*D2R;

    test_wrapper("UTM", &pps,
                 project_utm, project_utm_inv,
                 project_utm_arr, project_utm_arr_inv,
                 projY, projX, height, lon, lat, datum);
}

static void
test_p2ll(meta_projection *mp, const char *proj_name,
          double projY, double projX, double height, int zone,
          double lon, double lat, datum_type_t datum)
{
    double testLat, testLon, z;
    proj_to_latlon(mp, projX, projY, height, &testLat, &testLon, &z);
    
    testLat *= R2D;
    testLon *= R2D;

    if (double_equals(lat, testLat, 5) && double_equals(lon, testLon, 5)) {
        asfPrintStatus("%s proj_to_latlon Testing: %f, %f ... OK\n",
                       proj_name, projX, projY);
        ++n_ok;
    } else {
        asfPrintStatus("%s proj_to_latlon Testing: %f, %f ... ERROR\n",
                       proj_name, projX, projY);
        asfPrintStatus("Result:   %.10f %.10f (%.10f)\n", testLat, testLon, z);
        asfPrintStatus("Expected: %.10f %.10f\n", lat, lon);
        ++n_bad;
    }

    double x, y;
    latlon_to_proj(mp, '?', lat*D2R, lon*D2R, height, &x, &y, &z);

    if (double_equals(x, projX, 5) && double_equals(y, projY, 5)) {
        asfPrintStatus("%s latlon_to_proj Testing: %f, %f ... OK\n",
                       proj_name, lat, lon);
        ++n_ok;
    } else {
        asfPrintStatus("%s latlon_to_proj Testing: %f, %f ... ERROR\n",
                       proj_name, lat, lon);
        asfPrintStatus("Result:   %.10f %.10f (%.10f)\n", x, y, z);
        asfPrintStatus("Expected: %.10f %.10f\n", projX, projY);
        ++n_bad;
    }
}

static void
test_p2ll_utm(double projY, double projX, double height, int zone,
              double lon, double lat, datum_type_t datum)
{
    meta_projection *mp = meta_projection_init();
    mp->type = UNIVERSAL_TRANSVERSE_MERCATOR;
    mp->datum = datum;
    mp->param.utm.zone = zone;

    test_p2ll(mp, "UTM", projY, projX, height, zone, lon, lat, datum);

    free(mp);
}

static void
test_p2utm(double projY, double projX, double height, int zone,
           double lon, double lat)
{
    double testLat, testLon;
    UTM2latLon(projX, projY, height, zone, &testLat, &testLon);

    if (double_equals(lat, testLat, 5) && double_equals(lon, testLon, 5)) {
        asfPrintStatus("UTM2latLon Testing: %f, %f ... OK\n", projX, projY);
        ++n_ok;
    } else {
        asfPrintStatus("UTM2latLon Testing: %f, %f ... ERROR\n",
                       projX, projY);
        asfPrintStatus("Result:   %.10f %.10f\n", testLat, testLon);
        asfPrintStatus("Expected: %.10f %.10f\n", lat, lon);
        ++n_bad;
    }

    double x, y;
    latLon2UTM(lat, lon, height, &x, &y);

    if (double_equals(x, projX, 5) && double_equals(y, projY, 5)) {
        asfPrintStatus("latLon2UTM Testing: %f, %f ... OK\n", lat, lon);
        ++n_ok;
    } else {
        asfPrintStatus("latLon2UTM Testing: %f, %f ... ERROR\n", lat, lon);
        asfPrintStatus("Result:   %.10f %.10f\n", x, y);
        asfPrintStatus("Expected: %.10f %.10f\n", projX, projY);
        ++n_bad;
    }

    latLon2UTM_zone(lat, lon, height, zone, &x, &y);

    if (double_equals(x, projX, 5) && double_equals(y, projY, 5)) {
        asfPrintStatus("latLon2UTM_zone Testing: %f, %f ... OK\n", lat, lon);
        ++n_ok;
    } else {
        asfPrintStatus("latLon2UTM_zone Testing: %f, %f ... ERROR\n", lat, lon);
        asfPrintStatus("Result:   %.10f %.10f\n", x, y);
        asfPrintStatus("Expected: %.10f %.10f\n", projX, projY);
        ++n_bad;
    }
}

static int test_from_file_utm(const char *filename)
{
    asfPrintStatus("Testing from file: %s\n", filename);
    if (fileExists(filename))
    {
        FILE *fp = FOPEN(filename, "r");

        double wgs84_long_deg;
        double wgs84_lat_deg;
        double wgs84_elev_m;

        double nad83_northing_m;
        double nad83_easting_m;
        double nad83_elev_m;
        int nad83_zone;
        char nad83_zone_char;

        double nad27_northing_m;
        double nad27_easting_m;
        double nad27_elev_m;
        int nad27_zone;
        char nad27_zone_char;

        while (!feof(fp)) {
            int n = fscanf(fp,
                   "%lf %lf %lf %lf %lf %lf %d %c %lf %lf %lf %d %c\n",
                   &wgs84_long_deg,
                   &wgs84_lat_deg,
                   &wgs84_elev_m,
                   &nad83_northing_m,
                   &nad83_easting_m,
                   &nad83_elev_m,
                   &nad83_zone,
                   &nad83_zone_char,
                   &nad27_northing_m,
                   &nad27_easting_m,
                   &nad27_elev_m,
                   &nad27_zone,
                   &nad27_zone_char);
            
            if (n==13) {
                asfPrintStatus("\n\nTesting next line: %f %f\n",
                               wgs84_long_deg, wgs84_lat_deg);

                // low-level tests -- call proj directly
                test_utm_direct(nad83_northing_m, nad83_easting_m,
                                nad83_elev_m, nad83_zone, wgs84_long_deg,
                                wgs84_lat_deg, "NAD83");
                test_utm_direct(nad27_northing_m, nad27_easting_m,
                                nad27_elev_m, nad27_zone, wgs84_long_deg,
                                wgs84_lat_deg, "NAD27");

                // next-level up wrapper tests: project_utm & project_utm_inv
                test_utm(nad83_northing_m, nad83_easting_m,
                         nad83_elev_m, nad83_zone, wgs84_long_deg,
                         wgs84_lat_deg, WGS84_DATUM);
                test_utm(nad27_northing_m, nad27_easting_m,
                         nad27_elev_m, nad27_zone, wgs84_long_deg,
                         wgs84_lat_deg, NAD27_DATUM);

                // test the proj_to_latlon & latlon_to_proj wrappers
                test_p2ll_utm(nad83_northing_m, nad83_easting_m,
                              nad83_elev_m, nad83_zone, wgs84_long_deg,
                              wgs84_lat_deg, WGS84_DATUM);
                test_p2ll_utm(nad27_northing_m, nad27_easting_m,
                              nad27_elev_m, nad27_zone, wgs84_long_deg,
                              wgs84_lat_deg, NAD27_DATUM);

                // test latLon2UTM and UTM2latLon
                // these functions assume wgs84, no nad27 test
                test_p2utm(nad83_northing_m, nad83_easting_m,
                           nad83_elev_m, nad83_zone, wgs84_long_deg,
                           wgs84_lat_deg);
            }
            else
                break;
        }

        fclose(fp);
    }
    else
    {
        asfPrintError("Failed to find test point file: %s", filename);
    }

    return 0;
}

int main()
{
    asfPrintStatus("NAD27 Test...\n");

    //char tmp[255];
    //sprintf(tmp, "PROJ_LIB=%s/proj/", get_asf_share_dir());
    //putenv(tmp);

    asfPrintStatus("LatLon projection...\n");
#define TEST(x,y,xr,yr) if (test_nad27_ll(x,y,xr,yr)) ++n_ok; else ++n_bad;
    TEST(-117, 30, -117.0008058128, 30.0001131919);
#undef TEST

    test_from_file_utm("nad27_nad83_utm_ak.txt");
    asfPrintStatus("Tested %d points: %d ok.  (%.2f%%)\n", n_bad+n_ok, n_ok,
                   (float)n_ok/(n_bad+n_ok)*100);

    return n_bad > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
