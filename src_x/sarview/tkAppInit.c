/*
tkAppInit.c --

  Main file for SARview application: initializes Tcl/Tk.

  Orion Lawlor, 5/6/99

This file registers the shared variables between Tcl and C,
registers the Tcl-callable C routines, and calls the Tcl
scripts	support/init.tcl
 */
#include "main.h"
#include <unistd.h> /*for getcwd() for link_cwd */

char *link_path;/*Absolute path to the "support" directory*/
char *link_cwd;/*Absolute path to current working directory*/
int link_screenx,link_screeny;/*Screen width and height, pixels*/
double link_zoom;/*Main window zoom-out factor*/


/****************************************************************
 * SetSupportPath:
 * sets the "link_path" shared variable from an environment
 * variable, or based on the location of the application binary
 * if the environment variable isn't found.
 */
void setSupportPath(const char *appLoc)
{
  char sarview_support_dir[2048]; /* From environment variable.  */
  int i;                          /* String traversal index. */

  link_path=Tcl_Alloc(1024);      /* Space for the link path name.  */

  if ( getenv("SARVIEW_SUPPORT_DIR") != NULL ) {
    strncpy(sarview_support_dir, getenv("SARVIEW_SUPPORT_DIR"), 2048);
    if ( strlen(sarview_support_dir) >= 1024 ) {
      fprintf(stderr, "ridiculously long SARVIEW_SUPPORT_DIR environment variable detected, giving up\n");
      exit(EXIT_FAILURE);
    }
    strncpy(link_path, sarview_support_dir, 1024);
    return;
  }

  /* Didn't find an environment variable, try looking at the
     invocation name.  */

  /*Find the directory separator*/
  i = strlen(appLoc)-1;
  while ((i>0)&&(appLoc[i]!='\\')&&
                (appLoc[i]!=':' )&&
                (appLoc[i]!='/' ))
    i--;/*Move backwards until we hit a directory separator*/
  /*Clip off the executable's name from the path*/
  if (i<=0) {
    link_path[0]=0;
  }
  else {
    strncpy(link_path,appLoc,++i);
    link_path[i++]=0;
  }
  /*Copy the "support" name onto the end of the path*/
  strcat(link_path,"support");
}

/******************************************************************
 * Application main routine: just passes control off to Tcl/Tk,
 * which will return the favor immediately by calling Tcl_AppInit.
 */
char *firstArg=NULL;
int main(int argc, char *argv[])
{
	/* Set the "support" directory path, based on the application
	 * location or environment variable.*/
	setSupportPath(argv[0]);

	if (argc>1)
		firstArg=argv[1];
	argc=1;    /* Hide our arguments from Tcl                   */

	Tk_Main(argc, argv, Tcl_AppInit);
	return 0;  /* Needed only to prevent compiler warning.      */
}

/*****************************************************************
 * Application Initialization routine.  Loads up linked variables,
 * registers Tcl-callable C routines, and runs the initialization
 * and main TCL scripts.
 */
Tcl_Interp *interp=NULL;
int Tcl_AppInit(Tcl_Interp *loc_interp)		/* Interpreter for application. */
{
	char buf[1024];
	char *cwd;
/*Initialize the Tcl libraries we'll use*/
	interp=loc_interp;
	if (Tcl_Init(interp) == TCL_ERROR)  return TCL_ERROR;
	if (Tk_Init(interp) == TCL_ERROR)  return TCL_ERROR;
	Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);

	link_cwd=Tcl_Alloc(1024);
	if (NULL == (cwd = getcwd (link_cwd, 1024)))
		Tcl_Free (cwd);

	sprintf(link_cwd,"%s%c",link_cwd,DIR_SEPARATOR);

/*Link the Tcl-to-C shared variables */
	Tcl_LinkVar(interp,"link_path",(char *)&link_path,TCL_LINK_STRING|TCL_LINK_READ_ONLY);
	Tcl_LinkVar(interp,"link_cwd",(char *)&link_cwd,TCL_LINK_STRING|TCL_LINK_READ_ONLY);
	Tcl_LinkVar(interp,"link_imagewidth",(char *)&image.width,TCL_LINK_INT);
	Tcl_LinkVar(interp,"link_imageheight",(char *)&image.height,TCL_LINK_INT);
	Tcl_LinkVar(interp,"link_r_slope",(char *)&image.r_slope,TCL_LINK_DOUBLE);
	Tcl_LinkVar(interp,"link_r_offset",(char *)&image.r_offset,TCL_LINK_DOUBLE);
	Tcl_LinkVar(interp,"link_slope",(char *)&image.slope,TCL_LINK_DOUBLE);
	Tcl_LinkVar(interp,"link_offset",(char *)&image.offset,TCL_LINK_DOUBLE);
	Tcl_LinkVar(interp,"link_screenx",(char *)&link_screenx,TCL_LINK_INT);
	Tcl_LinkVar(interp,"link_screeny",(char *)&link_screeny,TCL_LINK_INT);
	Tcl_LinkVar(interp,"link_zoom",(char *)&link_zoom,TCL_LINK_DOUBLE);

/*Create Tcl-callable C commands*/
	Tcl_CreateObjCommand(interp,"cproc_loadimage",Cmd_loadimage,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_saveimage",Cmd_saveimage,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_drawtophoto",Cmd_drawtophoto,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_imageinfo",Cmd_imageinfo,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_pointinfo",Cmd_pointinfo,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_ne_arrows",Cmd_ne_arrows,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_polyinfo",Cmd_polyinfo,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_pointloc",Cmd_pointloc,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_log",Cmd_log,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_renderhist",Cmd_renderhist,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_initCanvas",Cmd_initCanvas,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_fromCanvas",Cmd_fromCanvas,0,NULL);
	Tcl_CreateObjCommand(interp,"cproc_toCanvas",Cmd_toCanvas,0,NULL);

/*Run the init.tcl script in the support directory to begin the program*/
	sprintf(buf,"%s%cinit.tcl",link_path,DIR_SEPARATOR);
	Tcl_EvalFile(interp,buf);

/*Load up command-line image if possible*/
	if (firstArg!=NULL)
	{
	/*	cproc_log("opening.txt",firstArg);*/
		if (firstArg[0] == '/')
			sprintf(buf,"doOpenFile {%s}\n",firstArg);
		else
			sprintf(buf,"doOpenFile {%s%s}\n",link_cwd,firstArg);
		Tcl_Eval(interp,buf);
	}

	return TCL_OK;
}
