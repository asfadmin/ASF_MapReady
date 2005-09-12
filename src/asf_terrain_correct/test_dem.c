#include "dem.h"

#include <string.h>
#include <assert.h>

static void 
print_surrounding_grid(Dem * dem, double lat, double lon, int grid_size)
{
    double cs = dem->cellsize;

    printf("-------------------------------\n");
    printf("Lat,Lon - (%f,%f) Spacing: %f\n", lat, lon, cs);

    if (dem_contains_coordinate(dem, lat, lon))
    {
	int i, j;
	int n = (grid_size - 1) / 2;
	
	for (i = -n; i <= n; ++i) {
	    for (j = n; j >= -n; --j)
		if (j==0 && i==0)
		    printf("*%8.3lf ", dem_get_height(dem, lat+i*cs, lon+j*cs));
		else
		    printf("%9.3lf ", dem_get_height(dem, lat+i*cs, lon+j*cs));
	    printf("\n");
	}
    }
    else
	printf("Not In Image: (%lf,%lf) - (%lf,%lf)\n",
	       dem->yllcorner, dem->yllcorner + dem->nrows * dem->cellsize,
	       dem->xllcorner, dem->xllcorner + dem->ncols * dem->cellsize);

}

static void
print_height(Dem * dem, const char * id, double lat, double lon, double h )
{
    printf("%-7s %15.6lf %15.6lf %10.3lf %10.3lf\n",
	   id, lat, lon, h, dem_get_height(dem, lat, lon));
}

int main(int argc, char *argv[])
{
    assert (argc > 1);

    char * base = argv[1];

    char file[256];
    strcpy(file, base);
    strcat(file, ".flt");

    Dem * dem = dem_new_from_file(file);

    char jpeg_file[256];
    strcpy(jpeg_file, base);
    strcat(jpeg_file, ".jpg");

//    float_image_export_as_jpeg(dem->float_image, jpeg_file, 800);

    int gs = 15;

    double lat, lon;

//    lat = 63.805141;
//    lon = -145.005981;
    lat = 63.7831;
    lon = -145.7925;
    
    print_surrounding_grid(dem, lat, lon, gs);

    print_height(dem, "DJR1", 63.805141,       -145.005981,     448.4);
    print_height(dem, "DJR4", 63.800026,       -145.803543,     705.1);
    print_height(dem, "DJR9", 63.965054,       -145.125732,     332.2);
    print_height(dem, "DJR15", 63.927238,       -145.323578,     365.5);
    print_height(dem, "DJR19", 64.013672,       -145.11322,      330.0);
    print_height(dem, "DJR20", 63.887505,       -145.318069,     382.3);
    print_height(dem, "DJR21", 63.887478,       -145.317871,     382.5);
    print_height(dem, "DJR22", 63.887447,       -145.317932,     377.9);
    print_height(dem, "DJR23", 63.887486,       -145.317993,     378.0);
    print_height(dem, "DJR24", 64.145569,       -145.784393,     303.8);

    return 0;
}
