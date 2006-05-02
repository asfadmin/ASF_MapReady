#include "poly.h"

typedef int pixelFetcher;

typedef struct {
	double x,y;
} fPoint;

typedef void (*mappingFunc)(void *map, fPoint in, fPoint *out);

typedef struct {
	mappingFunc doMap;
	poly_2d *inToOutX,*inToOutY;
	poly_2d *outToInX,*outToInY;
} polyMapRec;

typedef struct {
    double upleft[2];
    double loleft[2];
    double upright[2];
    double loright[2];    
} cornerCoords;

/* From fetcher.c */
float fetchPixelValue(pixelFetcher *inGetRec,int x, int y);
pixelFetcher *createFetchRec(FILE *in,meta_parameters *meta);
void killFetchRec(pixelFetcher *inGetRec);

/* From remap.c */
int remap_poly(poly_2d *fwX, poly_2d *fwY, poly_2d *bwX, poly_2d *bwY,
	       int outWidth, int outHeight, char *infile, char *outfile);

