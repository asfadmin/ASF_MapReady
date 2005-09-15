/*tkCommand.c:
	Routes Tcl/Tk-callable routines to standard
C routines in cproc.c & image.c
*/
#include "main.h"
#include "image.h"

#define errorString(str) {\
	Tcl_SetObjResult(interp,Tcl_NewStringObj(str,-1));\
	return TCL_ERROR;}

/***************** loadimage command **************/
int Cmd_loadimage(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char *fName;
	int ret;
	if (objc!=2) errorString("usage: loadimage <inputname>");
	fName=Tcl_GetStringFromObj(objv[1],NULL);

	ret=cproc_loadimage(fName);
	
	Tcl_SetObjResult(interp,Tcl_NewIntObj(ret));
	return TCL_OK;
}

/***************** saveimage command **************/
int Cmd_saveimage(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char *fName;
	int ret;
	if (objc!=2) errorString("usage: saveimage <outputname>");
	fName=Tcl_GetStringFromObj(objv[1],NULL);

	ret=image_saveJpeg(fName);
	
	Tcl_SetObjResult(interp,Tcl_NewIntObj(ret));
	return TCL_OK;
}

/***************** drawtophoto command **************/
int Cmd_drawtophoto(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char *photo;
	int x,y,sizeX,sizeY;
	double zoom;
	if (objc!=7) errorString("usage: drawtophoto <photo> <x> <y> <zoom> <sizeX> <sizeY>");
	
	photo=Tcl_GetStringFromObj(objv[1],NULL);
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[2],&x)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[3],&y)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[4],&zoom)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[5],&sizeX)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[6],&sizeY)) return TCL_ERROR;

	image_drawToPhoto(photo,x,y,zoom,sizeX,sizeY);
	return TCL_OK;
}

/***************** imageinfo command **************/
int Cmd_imageinfo(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char retStr[10024];
	if (objc!=1) errorString("usage: imageinfo");
	
	image_describeImage(retStr);

	Tcl_SetObjResult(interp,Tcl_NewStringObj(retStr,-1));
	return TCL_OK;
}
/**************** pointinfo command *************/
int Cmd_pointinfo(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char retStr[10024];
	double x,y;
	if (objc!=3) errorString("usage: pointinfo <x> <y>");
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[1],&x)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[2],&y)) return TCL_ERROR;

	image_describePixel(retStr,x,y);

	Tcl_SetObjResult(interp,Tcl_NewStringObj(retStr,-1));
	return TCL_OK;
}
/**************** ne_arrows command *************/
int Cmd_ne_arrows(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char *canvas;
	double x,y;
	if (objc!=4) errorString("usage: ne_arrows <canvas> <x> <y>");
	canvas=Tcl_GetStringFromObj(objv[1],NULL);
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[2],&x)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[3],&y)) return TCL_ERROR;

	cproc_ne_arrows(canvas,x,y);

	return TCL_OK;
}
/**************** polyinfo command *************/
int Cmd_polyinfo(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char retStr[10024];
	if (objc!=1) errorString("usage: polyinfo");

	cproc_polyinfo(retStr);

	Tcl_SetObjResult(interp,Tcl_NewStringObj(retStr,-1));
	return TCL_OK;
}

/**************** pointloc command *************/
int Cmd_pointloc(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	int ret;
	double x,y,lat,lon;
	if (objc!=5) errorString("usage: pointloc <x> <y> >lat >lon");
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[1],&x)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[2],&y)) return TCL_ERROR;

	ret=cproc_pointloc(x,y,&lat,&lon);

	Tcl_ObjSetVar2(interp,objv[3],NULL,Tcl_NewDoubleObj(lat),0);
	Tcl_ObjSetVar2(interp,objv[4],NULL,Tcl_NewDoubleObj(lon),0);
	Tcl_SetObjResult(interp,Tcl_NewIntObj(ret));
	return TCL_OK;
}

/********************** log command ********************/
int Cmd_log(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	char *fName,*writeThis;
	if (objc!=3) errorString("usage: log <file> <writeThis>");	
	fName=Tcl_GetStringFromObj(objv[1],NULL);
	writeThis=Tcl_GetStringFromObj(objv[2],NULL);

	if (0==strlen(fName)) return TCL_OK;/*Bail on empty log name*/

	cproc_log(fName,writeThis);

	return TCL_OK;
}

/********************** renderhist command ********************/
int Cmd_renderhist(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	int width,height,minHt,maxHt;
	double minVal,maxVal,mean,rms;
	if (objc!=9) errorString("usage: renderhist <width> <height> "
				 "<minHt> <maxHt> <minVal> <maxVal> <mean> <rms>");	
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[1],&width)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[2],&height)) return TCL_ERROR;
	
	cproc_renderhist(width,height,&minHt,&maxHt,&minVal,&maxVal,&mean,&rms);

	Tcl_ObjSetVar2(interp,objv[3],NULL,Tcl_NewIntObj(minHt),0);
	Tcl_ObjSetVar2(interp,objv[4],NULL,Tcl_NewIntObj(maxHt),0);
	Tcl_ObjSetVar2(interp,objv[5],NULL,Tcl_NewDoubleObj(minVal),0);
	Tcl_ObjSetVar2(interp,objv[6],NULL,Tcl_NewDoubleObj(maxVal),0);
	Tcl_ObjSetVar2(interp,objv[7],NULL,Tcl_NewDoubleObj(mean),0);
	Tcl_ObjSetVar2(interp,objv[8],NULL,Tcl_NewDoubleObj(rms),0);
	return TCL_OK;
}

/*************** Canvas-related commands**********************/
int Cmd_initCanvas(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	int canvNo;
	double startX,startY,zoom;
	if (objc!=5) errorString("usage: initCanvas <no> <startX> <startY> <zoom>");	
	
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[1],&canvNo)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[2],&startX)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[3],&startY)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,objv[4],&zoom)) return TCL_ERROR;

	cproc_initCanvas(canvNo,startX,startY,zoom);
	return TCL_OK;
}
int Cmd_fromCanvas(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	int canvNo;
	double x,y;
	if (objc!=4) errorString("usage: fromCanvas <no> <x> <y>");	
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[1],&canvNo)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,
		Tcl_ObjGetVar2(interp,objv[2],NULL,0),&x)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,
		Tcl_ObjGetVar2(interp,objv[3],NULL,0),&y)) return TCL_ERROR;
	
	if (0==cproc_fromCanvas(canvNo,&x,&y)) return TCL_ERROR;
	
	Tcl_ObjSetVar2(interp,objv[2],NULL,Tcl_NewDoubleObj(x),0);
	Tcl_ObjSetVar2(interp,objv[3],NULL,Tcl_NewDoubleObj(y),0);
	
	return TCL_OK;
}
int Cmd_toCanvas(ClientData cd,
	Tcl_Interp *interp, int objc, struct Tcl_Obj * CONST objv[])
{
	int canvNo;
	double x,y;
	if (objc!=4) errorString("usage: toCanvas <no> <x> <y>");	
	if (TCL_OK!=Tcl_GetIntFromObj(interp,objv[1],&canvNo)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,
		Tcl_ObjGetVar2(interp,objv[2],NULL,0),&x)) return TCL_ERROR;
	if (TCL_OK!=Tcl_GetDoubleFromObj(interp,
		Tcl_ObjGetVar2(interp,objv[3],NULL,0),&y)) return TCL_ERROR;
	
	if (0==cproc_toCanvas(canvNo,&x,&y)) return TCL_ERROR;
	
	Tcl_ObjSetVar2(interp,objv[2],NULL,Tcl_NewDoubleObj(x),0);
	Tcl_ObjSetVar2(interp,objv[3],NULL,Tcl_NewDoubleObj(y),0);
	
	return TCL_OK;
}
