
/*******************Main image processing routines  **/

/*Throw away cached image info.*/
void image_delete(void);

/*Load routines-- these read the metadata of the
given image and set link_imagewidth and height. They
return 0 on failure.*/
int image_loadLas(char *fileName);
int image_loadCeos(char *fileName);
int image_loadNewMeta(char *fileName); /* Load from new-style meta file.  */

int image_saveJpeg(const char * imageName);/* TCL asks C to 
	save the current image as a JPEG  with the given filename.  Returns 0 on failure; 1 on sucess.*/

/*Render the current image to the given Tcl/Tk photo name,
starting at (startX,startY) in the current image,
zooming it out by a factor of zoom, writing a total 
image which is width by height.*/
void image_drawToPhoto(char *imgName,
		int startX,int startY,double zoom, int width,int height);

/*Compute the pixel scaling slope and offset*/
void image_slopeOffset(void);

/*Extract the entire given line from the current image*/
void image_readLine(float *dest,int lineY,int bandNo);

/*Write a description of the current image to the given string*/
void image_describeImage(char *dest);

/*Write a description of the current pixel to the given string*/
void image_describePixel(char *dest,double x,double y);

/*return 1 if the image projection is valid; 0 if it is not*/
int image_projOK(void);

/*Return the projection coordinates of the given point*/
void image_point2proj(double x,double y,double *projX,double *projY);

/*Scale to bytes and draw floating-point values into destination buffer
as greyscale.*/
void drawFloatsToGrey(float *zoomed,int *dest,int width);
/*Scale to bytes and draw floating-point values into destination buffer
as a single color.*/
void drawFloatsToColor(float *zoomed,int *dest,int width,int color);

/******************** Main image variables  **/
typedef enum {
	image_none=0,/*No image*/
	image_ddr=100,/*LAS image, with DDR*/
	image_ceos=200/*CEOS image, with leader*/
	image_new_meta = 300;	/* Image with new-style meta file.  */
} image_type;

extern image_type type;
/*Valid for LAS images:*/
extern struct DDR *ddr;
extern FILE *ddr_file;
/*Valid for CEOS images:*/
extern CEOS_FILE *ceos;

/*Possibly valid for either*/
extern meta_parameters *meta;

extern histogram *data_hist;
