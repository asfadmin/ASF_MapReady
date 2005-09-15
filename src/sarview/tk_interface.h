#ifndef tk_interface
#define tk_interface

/*Tck/Tk-to-C Interface for SARview application.
Lists shared variables, commands, etc.*/
/*Shared variables: set from C*/
extern char *link_path;/*Absolute path to the "support" directory*/

/*Information about the loaded image (set by cproc_loadimage)*/
typedef struct {
	char fName[1024];/*Name of input file*/
	int width,height;/*number of pixels in image in horiz & vert. directions*/
	double min,max;/*Minimum and maximum values seen in image so far (inexact)*/
	double r_slope,r_offset;/*unadjusted bytes=r_slope*in_float+r_offset*/
	double slope,offset;/*screen bytes=slope*in_float+offset (as above, but with B/C)*/
} image_info;
extern image_info image;
#define scale_toByte(floatVal) (int)(image.slope*(floatVal)+image.offset)
#define scale_fromByte(byteVal) (((byteVal)-image.offset)/image.slope)

/*Shared variables: set from TCL*/
extern int link_screenx,link_screeny;/*Screen width and height, pixels*/
extern double link_zoom;/*Main window zoom-out factor*/

/*Tcl interface to shared commands (tkCommand.c)*/
int Cmd_loadimage(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_saveimage(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_drawtophoto(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_imageinfo(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_pointinfo(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_ne_arrows(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_polyinfo(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_pointloc(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_log(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_renderhist(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_initCanvas(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_fromCanvas(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_toCanvas(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);
int Cmd_renderhist(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[]);


/*Shared commands*/
int cproc_loadimage(char * imageName);/*  TCL asks C to 
											load up the given
   image.  Returns 0 on failure; 1 on success, stashing the image
   name and any relevant data in C global storage.
   Sets the link_imagewidth and link_imageheight variables.*/

void cproc_imageinfo(char *dest);/*: TCL asks C to 
	return a string describing the entire image.*/

void cproc_pointinfo(char *dest,double x,double y);/*: TCL asks C to 
	return a string describing the pixel in the main image at (x,y).*/

void cproc_ne_arrows(char *canvas,double x,double y);/*: TCL asks C to 
	draw arrows pointing north and east from the image point (x,y) in the image
	into the given canvas.*/

void cproc_polyinfo(char *dest);/*: TCL asks C to 
	return a string describing the currently selected region.*/

int cproc_pointloc(double x,double y, double *lat,double *lon);/*TCL asks C
	to compute the latitude and longitude of (x,y)*/

void cproc_log(const char *fName,const char *writeThis);/*Log writeThis to fName*/

void cproc_renderhist(int width,int height,int *minHt,int *maxHt,double *minVal,
		double *maxVal,double *mean, double *rms);/* TCL asks C to render the
	image histogram into img_hist.*/

/*Canvas coordinate system routines*/
void cproc_initCanvas(int canvNo,double startx,double starty,double zoom);/*Save given
	coordinate system under canvasNo*/
int cproc_fromCanvas(int canvNo,double *x,double *y);/*Map given point from canvas
	coordinates to image coordinates*/
int cproc_toCanvas(int canvNo,double *x,double *y);/*Map given point from image
	coordinates to canvas coordinates*/

#endif
