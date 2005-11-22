#ifndef _ASF_IMPORT_DEM_H_
#define _ASF_IMPORT_DEM_H_

#include <asf_meta.h>
#include "parse_options.h"
#include "seamless_meta.h"
#include "data_to_fit.h"

#define DEG_TO_RAD  PI/180.0
#define RAD_TO_DEG  180.0/PI

typedef int (*PROJECT_T) (project_parameters_t *pps, double lat, double lon,
                          double *x, double *y);
typedef int (*PROJECT_ARR_T) (project_parameters_t *pps,
                              double *lat, double *lon,
                              double **projected_x, double **projected_y,
                              long length);
typedef int (*UNPROJECT_T) (project_parameters_t *pps, double x, double y,
                            double *lat, double *lon);

double
reverse_map_x (data_to_fit_t *dtf, double x, double y);
double
reverse_map_y (data_to_fit_t *dtf, double x, double y);
#define X_PIXEL(x, y) reverse_map_x (dtf, x, y)
#define Y_PIXEL(x, y) reverse_map_y (dtf, x, y)

seamless_meta_t *
gridFloat_metadata_to_seamless_meta( char *baseName);

void
fill_proj_meta(projection_type_t pt, project_parameters_t *outProjPrms,
               seamless_meta_t *smeta, PROJECT_T project,
               double *average_height, double *pixel_size);

void
find_extents ( seamless_meta_t *smeta, project_parameters_t *outProjPrms,
               PROJECT_ARR_T project_arr,
               double *min_x, double *max_x, double *min_y, double *max_y);

void
create_grid ( seamless_meta_t *smeta,  project_parameters_t *outProjPrms,
              UNPROJECT_T unproject,
              const int grid_size, const int sparse_grid_stride,
              double min_x, double max_x, double min_y, double max_y,
              data_to_fit_t *dtf);

void
check_splines (data_to_fit_t *dtf, const int grid_size);

void
write_asf_dem ( seamless_meta_t *smeta,
                data_to_fit_t *dtf,
                double pixel_size,
                project_parameters_t *outProjPrms,
                projection_type_t projection_type,
                resample_method_t resample_method,
                char *inImageName,
                char *outImageName,
                char *outMetaName,
                double min_x, double max_x, double min_y, double max_y);

#endif // _ASF_IMPORT_DEM_H_
