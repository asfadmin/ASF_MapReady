#include "asf.h"
#include "ddr.h"
#include "asf_meta.h"
#include "proj.h"

double ex_pix_size(char *metaname); /*(proj_pix.c)*/

extern double aLat,aLon;/*First lat/lon point in grid (for zone calculations)*/

/******************* Projection Parameters Routines (proj.c) **************/
typedef struct {
	int proj,zone,units,datum;/*Projection codes (straight from LAS)*/
	double parms[15];
} proj_prm;/*LAS projection parameters*/

/*Fetch LAS projection parameters from projection file 
(from the projprm command)*/
int get_proj_prm(char *proj_file, char *proj_key, proj_prm *proj);/*(proj_prm.c)*/

/*Initialize the transform, and get a function pointer*/
forward_transform init_proj(proj_prm *proj);

/********************* Window Routines (proj_win.c) **************/

typedef struct {
	double minX,maxX;
	double minY,maxY;
} window;/*Projection window*/

window *getUserWindow(int pointOfInterest,char *win);/*Read N S E W window*/

/*Map-project the given output window, 
taking min/max X->lon; min/max Y->lat*/
void projectWindow(window *w,forward_transform f);

/*Map-project the given output window, as a point of interest:
maxY->lat; minY->lon; maxX->nl; minX->ns*/
void projectPoint(window *w,double pixSize,forward_transform f);

void window_expand(double pixSize,window *w);

/******************* Metadata Routines (proj.c) **************/

/*Write the given projection parameters into the given DDR*/
void write_proj_ddr(double pixSize,const proj_prm *proj,const window *w,
	struct DDR *ddr);

/*Write_proj_meta: updates the given meta_parameters structure
to the new projection.  Returns 0 on error.*/
int write_proj_meta(double pixSize,proj_prm *proj,const window *w,
	meta_parameters *meta);


/************************ Grid Points Routines (proj_grid.c) *****************/
#define MAX_GRIDPTS 1000
typedef struct {
	double inX,inY;/*Location in input space (pixels)*/
	double lat,lon;/*Latitude & longitude of point (degrees)*/
	double projX,projY;/*Location in projection space (m)*/
	double outX,outY;/*Location in output space (pixels)*/
} gridPoint;/*One point in the map-projection grid*/

void read_grid(const char *in_tie,gridPoint g[],int *nGrid);/*Read lat,lon,inX,inY*/
void project_grid(gridPoint g[],int nGrid,proj_prm *proj);/*Convert lat,lon to projX,projY*/
window *window_grid(gridPoint g[],int nGrid);/*Get max/min projection coordinates*/
void convert_grid(double pixSize,const window *w,gridPoint g[],int nGrid);
	/*convert projX,projY to outX,outY*/
void write_grid(const char *out_tie,gridPoint g[],int nGrid);/*Write outX,outY,inX,inY*/
