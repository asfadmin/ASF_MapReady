#include "ddr.h"

/*Spans/Extents: see extents.c for an overview.*/
typedef struct {
	double min,max;
} span;
span nullSpan(void);
span newSpan(double a,double b);
span sumSpan(span a,span b);

typedef struct {
	span h,v;
} extents;
extents nullExtents(void);
extents sumExtents(extents a,extents b);


/*Image/DEM utilites: open_dem.c*/
typedef struct {
	struct DDR ddr;
	double pStartX,pStartY,pDistX,pDistY;
	extents ext;
	char name[255];
	FILE *f;
	float *readBuf;
} image;
typedef struct {
	image *height;/*The DEM height image (e.g.: meters of elevation.)*/
	image *err;/*The DEM error estimate image (e.g.: meters of one-sigma error.)*/
} dem;

image *open_image(char *name);
dem * open_dem(char *name);


/*Utilities.*/

void extract_proj(struct DDR *inDDR,
	double *pStartX,double *pStartY,
	double *pDistX,double *pDistY);

void create_outDDR(extents ext,
	struct DDR *inDDR,
	double *pStartX,double *pStartY,
	double *pDistX,double *pDistY,struct DDR *outDDR);
	
void getLineByProj(double px,double py,int ns,
	dem *in,float *destHt,float *destErr);
