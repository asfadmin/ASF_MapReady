
/*Fetch.c Interfaces/Structures:*/
typedef int pixelFetcher;

pixelFetcher *createFetchRec(FILE *in, struct DDR *inDDR,int outDtype,int bandNo);
float fetchPixelValue(pixelFetcher *getRec,int x, int y);
void killFetchRec(pixelFetcher *getRec);

/*writer.c Interface:*/
void writePixelLine(FILE *out, struct DDR *outDDR,int y, int bandNo, float *line, char *outBuf);

/*Map/Sampling Interface:*/

typedef enum {
	invalidMap=100,
	matrixMap,/*Linear 2x3 matrix map*/
	quadraticMap, /*Quadratic 2D function map*/
	polyMap, /*Polynomial 2D function map*/
	warpMap /*Image-file-based warping function map*/
} mappingType;
typedef void (*mappingFunc)(void *map, fPoint in, fPoint *out);

typedef struct {
	mappingType type;
	mappingFunc doMap;
} mappingStruct, *mappingFunction;

mappingFunction createMap(mappingType m,void *param);
void addMatrix(mappingFunction map,Matrix2D *m);
void updateDDR(mappingFunction map,struct DDR *inDDR, struct DDR *outDDR);
void forwardMap(mappingFunction map, fPoint in, fPoint *out);
void killMap(mappingFunction map);

typedef enum {
	invalidSamp=1000,
	nearestSamp,
	bilinearSamp,
	kernelSamp,
	sincSamp
} sampleType;
typedef float (*sampleFunc)(void *samp,pixelFetcher *getRec,fPoint inPt);

typedef struct {
	sampleType type;
	char description[200];
	sampleFunc doSamp;
} sampleStruct, *sampleFunction;

sampleFunction createSamp(sampleType s);
sampleFunction createKernelSamp(int x,int y);
sampleFunction readKernelFile(char *fname);
void killSamp(sampleFunction samp);

/*Mapping.c Interface:*/
void perform_mapping(FILE *in,struct DDR *inDDR, FILE *out, struct DDR *outDDR,
	mappingFunction map, sampleFunction samp,int bandNo);

/*CLA.c Interface:*/
int process_CLAs(int argc, char *argv[], mappingFunction *map,
	sampleFunction *samp,int *outWidth,int *outHeight,struct DDR **asDDR);
extern float VERSION;

/*For Pixel Value Scaling/Background:*/
extern float minFlt, maxFlt, backgroundFill;




