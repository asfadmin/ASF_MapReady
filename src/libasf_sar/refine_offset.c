#include "asf.h"
#include "asf_meta.h"
#include <unistd.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

struct refine_offset_params {
        meta_parameters *meta;
        double x_off, y_off;
};

static double err_at_pixel(struct refine_offset_params *p, double off_t,
                           double off_x, double line, double samp)
{
    const double huge_err = 999999.9;

    meta_parameters *meta = p->meta;
    double line_out, samp_out;
    double lat, lon;
    int bad=0;

    double saved_timeOffset = meta->sar->time_shift;
    double saved_slant = meta->sar->slant_shift;

    // converting the pixel's location back to lat/lon is done before
    // applying the prospective slant and range shifts.  
    bad = meta_get_latLon(meta, line, samp, 0.0, &lat, &lon);
    if (bad) { printf("bad: meta_get_latLon\n"); return huge_err; }

    meta->sar->time_shift += off_t;
    meta->sar->slant_shift += off_x;

    bad = meta_get_lineSamp(meta, lat, lon, 0.0, &line_out, &samp_out);

    meta->sar->time_shift = saved_timeOffset;
    meta->sar->slant_shift = saved_slant;

    // meta_get_lineSamp returns a non-zero value if, for the specified
    // lat&lon, we weren't able to determine a line & sample.  In this case,
    // we just return a giant error value.
    if (bad) { printf("bad: meta_get_lineSamp\n"); return huge_err; }

    //printf("trying %f,%f\n",off_t,off_x);
    //printf("at %7.3f,%7.3f,%7.3f,%7.3f,%7.3f,%7.3f,%7.3f,%7.3f -> %7.3f\n",
    //       off_t, off_x, line, samp, line_out, samp_out, p->y_off, p->x_off,
    //       fabs(line-line_out-p->y_off)+fabs(samp-samp_out-p->x_off));
    double xdiff = line_out - line - p->y_off;
    double ydiff = samp_out - samp - p->x_off;
    return sqrt(xdiff*xdiff + ydiff*ydiff);
}

static int
getObjective(const gsl_vector *x, void *params, gsl_vector *f)
{
    double off_t = gsl_vector_get(x,0);
    double off_x = gsl_vector_get(x,1);

    struct refine_offset_params *p = (struct refine_offset_params *)params;
    meta_parameters *meta = p->meta;

    int nl = meta->general->line_count;
    int ns = meta->general->sample_count;

    double err = 0.0;

    err += err_at_pixel(p, off_t, off_x, nl/8, ns/8); 
    err += err_at_pixel(p, off_t, off_x, 7*nl/8, ns/8);
    err += err_at_pixel(p, off_t, off_x, nl/8, 7*ns/8); 
    err += err_at_pixel(p, off_t, off_x, 7*nl/8, 7*ns/8); 

    err += err_at_pixel(p, off_t, off_x, nl/8, ns/2); 
    err += err_at_pixel(p, off_t, off_x, 7*nl/8, ns/2);
    err += err_at_pixel(p, off_t, off_x, nl/2, 7*ns/8); 
    err += err_at_pixel(p, off_t, off_x, nl/2, ns/8); 

    err += err_at_pixel(p, off_t, off_x, nl/2, ns/2); 
    
    gsl_vector_set(f,0,err);
    gsl_vector_set(f,1,err);
    return GSL_SUCCESS;
}

/*
static void print_state(int iter, gsl_multiroot_fsolver *s)
{
    printf("iter = %3d   x = (%.3f %.3f)   f(x) = %.8f\n", iter,
           gsl_vector_get(s->x, 0), gsl_vector_get(s->x, 1),
           gsl_vector_get(s->f, 0));
}
*/

void coarse_search(double t_extent_min, double t_extent_max,
                   double x_extent_min, double x_extent_max,
                   double *t_min, double *x_min,
                   struct refine_offset_params *params)
{
    double the_min = 9999999;
    double min_t=99, min_x=99;
    int i,j,k=6;
    double t_extent = t_extent_max - t_extent_min;
    double x_extent = x_extent_max - x_extent_min;
    gsl_vector *v = gsl_vector_alloc(2);
    gsl_vector *u = gsl_vector_alloc(2);
    //printf("           ");
    //for (j = 0; j <= k; ++j) {
    //    double x = x_extent_min + ((double)j)/k*x_extent;
    //    printf("%9.3f ", x);
    //}
    //printf("\n           ");
    //for (j = 0; j <= k; ++j)
    //    printf("--------- ");
    //printf("\n");
    for (i = 0; i <= k; ++i) {
        double t = t_extent_min + ((double)i)/k*t_extent;
        //printf("%9.3f | ", t);

        for (j = 0; j <= k; ++j) {
            double x = x_extent_min + ((double)j)/k*x_extent;

            gsl_vector_set(v, 0, t);
            gsl_vector_set(v, 1, x);
            getObjective(v,(void*)params, u);
            double n = gsl_vector_get(u,0);
            //printf("%9.3f ", n);
            if (n<the_min) { 
                the_min=n;
                min_t=gsl_vector_get(v,0);
                min_x=gsl_vector_get(v,1);
            }
        }
        //printf("\n");
    }

    *t_min = min_t;
    *x_min = min_x;

    gsl_vector_free(v);
    gsl_vector_free(u);
}

void generate_start(struct refine_offset_params *params,
                    double *start_t, double *start_x)
{
    int i;

    double extent_t_min = -10;
    double extent_t_max = 10;

    double extent_x_min = -5000;
    double extent_x_max = 5000;

    double t_range = extent_t_max - extent_t_min;
    double x_range = extent_x_max - extent_x_min;

    for (i = 0; i < 12; ++i)
    {
        coarse_search(extent_t_min, extent_t_max, extent_x_min, extent_x_max,
                      start_t, start_x, params);

        t_range /= 3;
        x_range /= 3;

        extent_t_min = *start_t - t_range/2;
        extent_t_max = *start_t + t_range/2;

        extent_x_min = *start_x - x_range/2;
        extent_x_max = *start_x + x_range/2;

        //printf("refining search to region: (%9.3f,%9.3f)\n"
        //       "                           (%9.3f,%9.3f)\n",
        //       extent_t_min, extent_t_max,
        //       extent_x_min, extent_x_max);
    }

}

void refine_offset(double x_off, double y_off, meta_parameters *meta,
                   double *out_t, double *out_x)
{
    int status;
    int iter = 0, max_iter = 1000;
    const gsl_multiroot_fsolver_type *T;
    gsl_multiroot_fsolver *s;
    gsl_error_handler_t *prev;
    struct refine_offset_params params;
    const size_t n = 2;

    params.meta = meta;
    params.x_off = x_off;
    params.y_off = y_off;

    gsl_multiroot_function F = {&getObjective, n, &params};
    gsl_vector *x = gsl_vector_alloc(n);

    double start_t, start_x;
    generate_start(&params, &start_t, &start_x);
    //printf("Starting point at (%f,%f)\n", start_t, start_x);

    gsl_vector_set (x, 0, start_t);
    gsl_vector_set (x, 1, start_x);

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

    *out_t = gsl_vector_get(s->x, 0);
    *out_x = gsl_vector_get(s->x, 1);

    gsl_vector *retrofit = gsl_vector_alloc(n);
    gsl_vector_set(retrofit, 0, *out_t);
    gsl_vector_set(retrofit, 1, *out_x);
    gsl_vector *output = gsl_vector_alloc(n);
    getObjective(retrofit, (void*)&params, output);
    //double val= gsl_vector_get(output,0);
    //printf("GSL Result: %f at (%f,%f)\n", val, *out_t, *out_x);
    gsl_vector_free(retrofit);
    gsl_vector_free(output);

    gsl_multiroot_fsolver_free(s);
    gsl_vector_free(x);
    gsl_set_error_handler(prev);
}
