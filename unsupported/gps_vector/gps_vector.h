/* 
   NAME: gps_vector.h

   DESCRIPTION: Contains struct declaration, type definitions, const
		definitions, and function declarations. This file is to be
		used with gps_vector and not gps_deform.

   AUTHOR:  Mike Shindle, August 21, 1995.
*/

#include "asf.h"


/* Struct definitions */
struct	gps_meta {
	char	
		padding[12],	/* padding for ceos record data format */
		pid[11],	/* product ID */
		apid[11],	/* image A ID */
		bpid[11],	/* image B ID */
		ayear[5],	/* year of image A */
		atime[17],	/* time of image A */
		byear[5], 	/* year of image B */
		btime[17],	/* time of image B */
		something[28],	/* some data not relevant to this program */
                a_ul_lat[10],
                a_ul_lon[10],
                a_ur_lat[10],
                a_ur_lon[10],
                a_ll_lat[10],
                a_ll_lon[10],
                a_lr_lat[10],
                a_lr_lon[10],
		min_x[10],	/* min grid coord x */
		max_x[10],	/* max grid coord x */
		min_y[10],	/* min grid coord y */
		max_y[10],	/* max grid coord y */
                b_ul_lat[10],
                b_ul_lon[10],
                b_ur_lat[10],
                b_ur_lon[10],
                b_ll_lat[10],
                b_ll_lon[10],
                b_lr_lat[10],
                b_lr_lon[10],
                bmin_x[10],
                bmax_x[10],
                bmin_y[10],
                bmax_y[10],
		between[46],	/* another set of irrelevant items */
		avgdispx[10],
		avgdispy[10],
		deltatime[8],
		pixspace[8],
		gridspace[6],	/* grid spacing */
		tailer[131];	/* irrelevant data and spare space */
};

struct gps_rec {
	char	
		padding[12],	/* padding for ceos record data format */
		a_lat[10],	/* lat of point in ref image */
		a_lon[10],	/* lon of point in ref image */
		b_lat[10],	/* lat of point in image B */
		b_lon[10],	/* lon of point in image B */
		x_disp[10],	/* displacement in x direction */
		y_disp[10],	/* displacement in y direction */
		rotangle[7],	/* rotation angle */
		match[2];	/* goodness of match metric */
};

struct motion_ele {
	double	a_lat,		/* lat of point in ref image */
		a_lon,		/* lon of point in ref image */
		b_lat,		/* lat of point in image B */
		b_lon,		/* lon of point in image B */
		start_area,	/* start area of quadrangle */
		end_area,	/* end area of quadrangle */
		div,		/* divergence */
		div_pct;	/* divergence area percentage */
	int	row,		/* row position in ref image */
		col,		/* col position in ref image */
		point_flag,	/* flagging if the point has data */
		area_flag,	/* flagging if the grid exists */
		area_accu_index;	/* flagging if the grid has accurate area */
};

struct vector_grid {
	int row;
	int col;
	int status;
};

struct point {
	int row;
	int col;
	int gps_row;
	int gps_col;
};

struct ll_pos {
	double lat;
	double lon;
};

/* new types */
typedef unsigned char ** BYTEIMAGE;
typedef struct ll_pos LATLON;

/* definitions for arrow-drawing routine */
#define ARROW_LENGTH	9.0	/* pixels */
#define ARROW_ANGLE	30	/* in degrees */
#define LEADING_COL     12      /* number of leading bytes on each line */
#define LEADING_ROW     1       /* number of header rows */
#define MAXLINE         1024
#define CORRECTION      0.01    /* Sparc seems to have a precision problem */
#define RAD		( PI/180.0 )

/* Define Semi-major & semi-minor axis of Hughes Ellipsoid */
#define HUGHES_MAJ	6378273.0
#define HUGHES_MIN	6356889.4

#define VERSION		2.1
#define TRUE		1
#define FALSE		0

/* definitions for command line options */
#define DRAWGRID		1
#define SUBTRACT_META_AVG	2
#define SUBTRACT_USER_AVG	4

/* function declaration */

 void create_gps_ddr (char *fname, struct gps_meta *meta, 
 	int nrow, int ncol, int pixsize, int nbands); 
 void draw_vector_arrow (BYTEIMAGE image, int ax, int ay, 
 	int bx, int by, int fillvalue, int mag, int nrow, int ncol); 
 void check_bound (int *x, int *y, int nr, int nc); 
 double find_angle (double diff_y, double diff_x); 
 void drawline (BYTEIMAGE image_array, int x1, int y1, int x2, int y2, int fillvalue); 
 void find_closest_vector (LATLON *input, LATLON *head, LATLON *tail, FILE *file); 
 void print_usage (void);
 int main (int argc, char **argv); 
 void fwriteConst (void *data, size_t size, size_t nitems, FILE *fp); 
 void print_usage (void); 
 void grid (unsigned char **oimage, FILE *fpdat, struct gps_meta *meta, 
 	double gridspace, int nr, int nc, int mag, int pixel_size, 
 	double vector_res, int options, LATLON user); 
 int CheckForNextRow (struct vector_grid **vg, int row, int col, int max_row); 
 int CheckForNextCol (struct vector_grid **vg, int row, int col, int max_col); 
 void ll_to_ssmi (double alat, double along, double *x, double *y); 
 void print_data_info (struct gps_meta *meta); 
 double s_to_dbl (char *s, int n); 
 int ssmi_gps (double x, double x0, double dx); 
 void vector (unsigned char **oimage, FILE *fpdat, 
 	struct gps_meta *meta, double gridspace, int nr, int nc, 
 	int mag, int pixel_size, double vector_res, int options, LATLON user); 

