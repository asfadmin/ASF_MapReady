/* drawdia is an Xview program that reads diagram data from a file and
* displays it in an window.
* It calculates the scaleing dependent on the size of the window.
* created by Hans-Joerg Wagner		08-03-1994
*
* format of the file:
* the whole file is in ascii format. I.e. values are given in text form. It
* has an bunch of optional header lines, that begin with special Patterns.
* the order of the header lines is not essential. The Patterns have to be
* in capital letters
* Any command line option overwrites the information in the header
* These header lines are:
*	X name				name of x-axis
*	Y name				nAme of y-axis
*	TITLE name			Title of the diagram
*	WINDOW name			Name of the window
*	C comment			Comment without effect.
*
* Every following line contains the x-y pair that represent a point. The
* x value is the first and the y values the second. The values are seperated
* by a white space (tabulator, spaces).
*
* enhanced by Hans-Joerg Wagner 	08-25-1994
*	now up to 4 graphs can be shown in the same diagram
*/

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
/* #include <olgx/olgx.h> */
#include <xview/canvas.h>
#include <xview/cms.h>
/* #include <xview/cursor.h> */
#include <xview/xv_xrect.h> 
/* #include <xview/scrollbar.h> */
#include <xview/font.h>
#include <stdio.h>
#include <malloc.h>
#include <math.h>
#include "hjwlib.h"

#define MAX_GRAPHS	4

/* function prototypes */
void Help();
void Hello();
void Bye();
int ProcessCmdLine();
int ReadFile();
void XV_init_X();
void XV_hans_repaint_canvas();
void XV_hans_repaint_panel();
void XV_hans_resize_canvas();
Notify_value XV_hans_destroy();
int XV_die_button_pressed();
int CalculateTicking();
void FormatValue();

/* generic variables - 
 * frame, canvas, panel
 * display, xid
 */
struct {
	XID	xid;
	Frame	frame;
	Panel	panel;
	Display	*display;
	Canvas	canvas;
	Xv_Font	font_canvas;
	GC	gc,panel_gc;
	} g_xwin;

/* another few global variables that have noting to do with XView */
struct {
	char *pszInFile;
	int  bDeleteInputFile;
	int  bGrid;
        int  bScatter;
	} g_cmdLine;

struct {
	char *pszWindow;		/* title of window */
	char *pszDiagram;		/* title of diagram */
	char *pszXlabel;		/* label for x-axis */
	char *pszYlabel;		/* label for y-axis */
	double fxTickMin,fxTickDelta;	/* min and delta for x ticking */
	double fyTickMin,fyTickDelta;	/* min and delta for y ticking */
	int  xAcc,yAcc;
	int top,bottom,left,right;	/* axii */
	} g_disp = { NULL,NULL, NULL,NULL };

/* Values for display */
double 		*g_aX = NULL;		/* values for x-axis */
double 		*g_aY[4] = NULL;	/* values for y-axis */
double		g_xmin,g_xmax;		/* min and max for x */
double		g_ymin,g_ymax;		/* min and max for y */
int		g_nValues;		/* number of x-y value pairs */
int		g_nGraphs;		/* number of graphs (max 4) */

/* enumeration for color indizes */
enum {
	CI_BACKGROUND = 0,
	CI_XAXIS,
	CI_XLABEL,
	CI_XTICK,
	CI_XTICKLABEL,
	CI_YAXIS,
	CI_YLABEL,
	CI_YTICK,
	CI_YTICKLABEL,
	CI_GRAPH1,
	CI_GRAPH2,
	CI_GRAPH3,
	CI_GRAPH4,
	/* MAX_GRAPH = 4 !!! */
	CI_LASTCOLOR
	};

/* a almost simple main program */
main(argc, argv)
int     argc;
char    *argv[];
{
    Hello();
    XV_init_X (&argc, argv);
    if ( !ProcessCmdLine(argc,argv) ) 
	Help(); /* if error in command line - teach these fools */
    else if ( !ReadFile() )
	printf("DIAGRAM Error reading input file\n"); /* file error ! */
    else {
	xv_set(g_xwin.frame, FRAME_LABEL, g_disp.pszWindow, NULL);
	/* size canvas */
	XV_hans_resize_canvas(g_xwin.canvas,xv_get(g_xwin.canvas,XV_WIDTH),
                                xv_get(g_xwin.canvas,XV_HEIGHT) ) ;
        xv_main_loop (g_xwin.frame);  /* main loop if everything is OK */
	}
    /* close file if requested */
    if (g_cmdLine.bDeleteInputFile) 
	unlink(g_cmdLine.pszInFile);
    Bye();
    exit(0);
}

/**********************************************************************
* Hello,Bye
* Hello says hello
* Bye says bye
*/
void Hello()
{
printf("DIAGRAM plots a diagram from values within a file\n");
}

void Bye()
{
printf("DIAGRAM died. Mourning for\n\tTitle: %s\n\tWindow: %s\n",
	g_disp.pszDiagram,g_disp.pszWindow);
}

/**********************************************************************
* Help prints help onto the screen
*/
void Help()
{
printf("usage:\n");
printf("\tdiagram infile [-t diagramtitle] [-w windowtitle] [-d] [-g] [-s]\n\n");
printf("\tinfile   filename of file that contains the data to display\n");
printf("\tdiagramtitle	title of diagram\n");
printf("\twindowtitle	title of window\n");
printf("\t-d		delete input file after reading - for temp files!\n");
printf("\t-g            draw grid lines\n");
printf("\t-s            scatter plot (don't connect points)\n");
}

/**********************************************************************
* ProcessCmdLine
* processes the command line arguments
* returns 0 if an error occures
*/
int ProcessCmdLine(argc,argv)
int argc;
char **argv;
{
int count;

/* argv[0] = "DIAGRAM" */
/* argv[1] = infile    */
if ( argc < 2)
	return 0;
g_cmdLine.pszInFile = argv[1];
g_cmdLine.bDeleteInputFile = 0;
g_cmdLine.bGrid = 0;
g_cmdLine.bScatter = 0;

for (count = 2; count < argc; count++) {
    if ('-' == argv[count][0]) {
	if (strlen(argv[count]) != 2)
		return 0;
	switch(argv[count][1]) {
	    case 'd':
		/* delete input file after displaying */
		g_cmdLine.bDeleteInputFile = 1;
		break;
	    case 't':
		/* diagram title */
		count++;
		if (count >= argc) return 0;
		g_disp.pszDiagram = argv[count];
		break;
	    case 'w':
		/* window name */
		count++;
		if (count >= argc) return 0;
		g_disp.pszWindow = argv[count];
		break;
            case 's':
                /* scatter plot */
                g_cmdLine.bScatter = 1;
                break;
	    case 'g':
		/* turn gridlines on */
		g_cmdLine.bGrid = 1;
		break;
	    default:
		return 0;
	    } /* end switch */
	} /* end if */
    } /* end for */

return 1;
}

/**********************************************************************
* ReadFile
* reads the input file and allocates the necessary memory
* returns 0 if an error occures
*/
int ReadFile()
{
FILE *file;
unsigned uAllocated = 0;
char *buffer = NULL;
char *pTmp;
int nTmp;
register int i;

file = fopen(g_cmdLine.pszInFile,"r");
if (NULL == file) {
	printf("cannot open file %s\n",g_cmdLine.pszInFile);
	return 0;
	}

/* reading header lines */

do {
    buffer = fgets_unknown_length(file,buffer,&uAllocated);
    if ( ! strncmp(buffer,"X",1) ) {
	strtok(buffer," "); pTmp = strtok(NULL,"");
	g_disp.pszXlabel = malloc( strlen(pTmp) + 1 );
	strcpy(g_disp.pszXlabel,pTmp);
	}
    else if ( ! strncmp(buffer,"Y",1) ) {
        strtok(buffer," "); pTmp = strtok(NULL,"");
	g_disp.pszYlabel = malloc( strlen(pTmp) + 1);
	strcpy(g_disp.pszYlabel,pTmp);
	}
    else if ( ! strncmp(buffer,"TITLE",5) ) {
	strtok(buffer," "); pTmp = strtok(NULL,"");
 	if (NULL == g_disp.pszDiagram) {
	    g_disp.pszDiagram = malloc( strlen(pTmp) + 1);
	    strcpy(g_disp.pszDiagram,pTmp);
	    /* remove new line */
	    g_disp.pszDiagram[strlen(g_disp.pszDiagram)-1] = '\0';
	    }
	}
    else if ( ! strncmp(buffer,"WINDOW",6) ) {
	strtok(buffer," "); pTmp = strtok(NULL,"");
	if (NULL == g_disp.pszWindow) {
	    g_disp.pszWindow = malloc( strlen(pTmp) + 1);
	    strcpy(g_disp.pszWindow,pTmp);
	    /* remove new line */
	    g_disp.pszWindow[strlen(g_disp.pszWindow)-1] = '\0';
	    }
	}
    else if ( ! strncmp(buffer,"C",1) ) {
	}
    else
	break;
    } while ( !feof(file) );

/* set default if neither in command line  nor in file header
*  the lables are set
 */
if (NULL == g_disp.pszXlabel) g_disp.pszXlabel = "X-axis";
if (NULL == g_disp.pszYlabel) g_disp.pszYlabel = "Y-axis";
if (NULL == g_disp.pszWindow) g_disp.pszWindow = "Diagram";
if (NULL == g_disp.pszDiagram) g_disp.pszDiagram = "";

g_nGraphs = 0;

/* now reading the values to display. */
/* the size of the fields g_aX and g_aY are increased by */
/* FIELDINC if necessary */
#define FIELDINC  100 
g_aX = (double *)malloc(FIELDINC * sizeof(double));
for (i=0; i < MAX_GRAPHS; i++) {
    g_aY[i] = (double *)malloc(FIELDINC * sizeof(double));
    if (NULL == g_aX || NULL == g_aY[i]) {
	printf("error initializeing g_aY and g_aX\n");
	return 0;
	}
    }

for (g_nValues = 0; 1;) {

    /* if necessary increase x and y field size */
    if ( !(g_nValues % FIELDINC)  && g_nValues) {
	/* x */
	pTmp = realloc(g_aX,(g_nValues + FIELDINC) * sizeof(double));
	if (NULL == pTmp) {
		printf("error allocating memory for g_aX\n");
		free(g_aX); g_aX = NULL;
		free(buffer);
		return 0;
		}
	else
		g_aX = (double *)pTmp;
	/* y */
	for (i=0; i < MAX_GRAPHS; i++) {
	    pTmp = realloc(g_aY[i],(g_nValues + FIELDINC) *sizeof(double));
            if (NULL == pTmp) {
                printf("error allocating memory for g_aY[%d]\n",i);
		for (i=0; i < MAX_GRAPHS; i++)
                    { free(g_aY[i]); g_aY[i] = (double *)NULL; }
		free(buffer);
                return 0;
                }
            else
                g_aY[i] = (double *)pTmp;
	    } /* end for every graph */
	} /* end if, increasing x,y field */

    if (NULL == buffer)
	return 0;
    /* skip empty lines */
    if (strlen(buffer) >= 3)  {
	/* scanning values , assuming MAX_GRAPHS = 4*/
        nTmp = sscanf(buffer," %lf %lf %lf %lf %lf ",g_aX + g_nValues, 
		&g_aY[0][g_nValues], &g_aY[1][g_nValues],
		&g_aY[2][g_nValues], &g_aY[3][g_nValues]);
	if (0 == g_nGraphs)
		g_nGraphs = nTmp - 1;
	/* looking for min and max */
	if (0 == g_nValues) { 
		g_xmin = g_xmax = g_aX[0];
		g_ymin = g_ymax = g_aY[0][0];
		}
	if (g_aX[g_nValues] > g_xmax) g_xmax = g_aX[g_nValues];
        else if (g_aX[g_nValues] < g_xmin) g_xmin = g_aX[g_nValues];
	for (i = 0; i < g_nGraphs; i++) {
            if (g_aY[i][g_nValues] > g_ymax) g_ymax = g_aY[i][g_nValues];
            else if (g_aY[i][g_nValues] < g_ymin) g_ymin = g_aY[i][g_nValues];
	    }
	/* prepare next value */
	g_nValues++;
	}

    if (feof(file) )
	break;
    /* read next line */
    buffer = fgets_unknown_length(file,buffer,&uAllocated);
    } /* end for */
printf("DIAGRAM read %u values\n",g_nValues);
    
free(buffer);
return 1;
}

/**********************************************************************
* XV_init_X
* initializes and openes a window to display. It removes the 
* Xview specific parameters from the command line
*/
void 
XV_init_X(pargc, argv)
int *pargc;
char *argv[];
{
    Xv_cmsdata cms_data;
    Xv_Font font;
    unsigned char red[CI_LASTCOLOR],green[CI_LASTCOLOR],blue[CI_LASTCOLOR];
    XGCValues gcVals;
    GC gc_panel;
    Display *dispPanel;

    /* the colors: */
    red[CI_BACKGROUND]=  0; green[CI_BACKGROUND] =  0; blue[CI_BACKGROUND]=  0;
    red[CI_XAXIS]     =255; green[CI_XAXIS]      =255; blue[CI_XAXIS]     =255;
    red[CI_XLABEL]    =200; green[CI_XLABEL]     =200; blue[CI_XLABEL]    =255;
    red[CI_XTICK]     =180; green[CI_XTICK]      =180; blue[CI_XTICK]     =180;
    red[CI_XTICKLABEL]=255; green[CI_XTICKLABEL] =255; blue[CI_XTICKLABEL]=255;
    red[CI_YAXIS]     =255; green[CI_YAXIS]      =255; blue[CI_YAXIS]     =255;
    red[CI_YLABEL]    =200; green[CI_YLABEL]     =200; blue[CI_YLABEL]    =255;
    red[CI_YTICK]     =180; green[CI_YTICK]      =180; blue[CI_YTICK]     =180;
    red[CI_YTICKLABEL]=255; green[CI_YTICKLABEL] =255; blue[CI_YTICKLABEL]=255;
    red[CI_GRAPH1]    =255; green[CI_GRAPH1]     =120; blue[CI_GRAPH1]    = 80;
    red[CI_GRAPH2]    = 60; green[CI_GRAPH2]     =255; blue[CI_GRAPH2]    = 60;
    red[CI_GRAPH3]    = 80; green[CI_GRAPH3]     = 80; blue[CI_GRAPH3]    =255;
    red[CI_GRAPH4]    =200; green[CI_GRAPH4]     =255; blue[CI_GRAPH4]    =  0;

    cms_data.type = XV_STATIC_CMS;
    cms_data.size = CI_LASTCOLOR;
    cms_data.rgb_count = CI_LASTCOLOR;
    cms_data.index = 0;
    cms_data.red = red;
    cms_data.green = green;
    cms_data.blue = blue;

    /* some Xview initialization call; strips args or something... */
    xv_init(XV_INIT_ARGC_PTR_ARGV, pargc, argv, NULL);

    /* Create the display frame and a die button */
 
    g_xwin.frame = (Frame) xv_create(NULL, FRAME,
        FRAME_LABEL,          "Diagram",
        XV_WIDTH,             800,
        XV_HEIGHT,            420,
        XV_X,                 100,
        XV_Y,                 300,
        NULL);
    /* how to die */
    notify_interpose_destroy_func(g_xwin.frame,XV_hans_destroy);
 
    g_xwin.panel = (Panel) xv_create(g_xwin.frame, PANEL,
        /* XV_WIDTH,             	800, */
        XV_HEIGHT,            	25,
	PANEL_REPAINT_PROC,	XV_hans_repaint_panel, 
        NULL);

    /* setting new background and foreground color and font for panel */
    dispPanel = (Display *)xv_get(g_xwin.panel, XV_DISPLAY);
    font = (Xv_Font)xv_find(g_xwin.panel, FONT,
			FONT_FAMILY,	FONT_FAMILY_DEFAULT,
			FONT_STYLE,	FONT_STYLE_NORMAL,
			FONT_SIZE,	18,
			NULL);
    if (NULL == font) {
	font = (Xv_Font)xv_get(g_xwin.panel,XV_FONT);
	fprintf(stderr,"Error loading font for panel\n");
	}
    gcVals.font = (Font)xv_get(font,XV_XID);
    gcVals.foreground = WhitePixel(dispPanel, DefaultScreen(dispPanel));
    gcVals.background = BlackPixel(dispPanel, DefaultScreen(dispPanel));
    gcVals.graphics_exposures = False;
    g_xwin.panel_gc=XCreateGC(dispPanel,xv_get(g_xwin.panel,XV_XID),
		GCFont | GCGraphicsExposures |GCForeground | GCBackground, 
			&gcVals);
 
    /* adding die button */
    xv_create (g_xwin.panel, PANEL_BUTTON,
        PANEL_NOTIFY_PROC, XV_die_button_pressed,
        PANEL_LABEL_STRING, "Die!",
	/*
        XV_X, 380,
        XV_Y, 2,
	*/
        NULL);

    g_xwin.canvas = xv_create (g_xwin.frame, CANVAS,
        CANVAS_REPAINT_PROC,	XV_hans_repaint_canvas,
	CANVAS_RESIZE_PROC,	XV_hans_resize_canvas,
        CANVAS_X_PAINT_WINDOW,	TRUE,
	CANVAS_RETAINED,	FALSE,
	/*CANVAS_AUTO_SHRINK,	TRUE, */
	/*CANVAS_AUTO_EXPAND,	TRUE, */
	WIN_DYNAMIC_VISUAL,	FALSE,
	WIN_CMS_NAME,		"HJW",
	WIN_CMS_DATA,		&cms_data,
        XV_X,                  0,
        XV_Y,                  25,
        NULL);
    /* initialize the generic variables */
    g_xwin.display = (Display *)xv_get(g_xwin.frame, XV_DISPLAY);
    g_xwin.xid = (XID)xv_get(canvas_paint_window(g_xwin.canvas), XV_XID);

    /* gc with font for canvas */
    g_xwin.font_canvas = (Xv_Font)xv_get(g_xwin.panel,XV_FONT);
    gcVals.font = (Font)xv_get(g_xwin.font_canvas,XV_XID);
    g_xwin.gc = XCreateGC(g_xwin.display, g_xwin.xid, GCFont, &gcVals );

    xv_set (g_xwin.frame, XV_SHOW, TRUE, NULL);

    /* scram from XV_init_X() */
    return;
}

/**********************************************************************
* XV_hans_destroy
* is called when destroying the window i.e. exiting the program
*/
Notify_value XV_hans_destroy(client,status)
Notify_client client;
Destroy_status status;
{
if (DESTROY_CHECKING == status) {
	/* insert code to ask: Do you realy want to kill me? */
	}
else if (DESTROY_CLEANUP == status) {
	/* free all used memory here */
	/* notifying the others */
	return notify_next_destroy_func(client,status);
	}
else if (DESTROY_SAVE_YOURSELF) {
	/* perhaps you have to save. Do not rely that this is executed */
	}
else {
	}
return NOTIFY_DONE;
}
	

/**********************************************************************
* XV_quit 
* closes and destroys the window
*/
int
XV_die_button_pressed()
{ 
return xv_destroy_safe (g_xwin.frame);
}

/*********************************************************************
* XV_hans_resize_canvas
* called if canvas size is changed. Recalculate the tickmarks and
* the borders for painting
*/
#define RIGHT_BORDER	20
#define LEFT_BORDER	20
#define TOP_BORDER	20
#define BOTTOM_BORDER	20
#define TICK_SIZE	15
#define LABEL_DISTANCE   5

void 
XV_hans_resize_canvas (canvas, width, height)
Canvas canvas;
int width,height;
{
Font_string_dims dims;
double lfTmp;
int nTmp,size;
static int number = 1;	

/*printf("DIAGRAM resize #%d width = %d, height = %d\n",number++,width,height);
*/

/* avoid division by zero */
if (g_xmax == g_xmin) { g_xmax += 0.5; g_xmin -= 0.5; }
if (g_ymax == g_ymin) { g_ymax += 0.5; g_ymin -= 0.5; }
 
/* calculate top and bottom */
xv_get(g_xwin.font_canvas, FONT_STRING_DIMS,"0.0",&dims);
g_disp.top = TOP_BORDER;
g_disp.bottom = height - BOTTOM_BORDER - 2 * dims.height;
 
/* Ticking and size for y labeling */
nTmp = CalculateTicking(g_disp.bottom - g_disp.top, 
			g_ymin, g_ymax, dims.height+LABEL_DISTANCE,
                        &g_disp.fyTickDelta, &g_disp.fyTickMin);
g_disp.yAcc = (nTmp < 0) ? -nTmp : 0;
/* size of largest string */
nTmp = SizeOfValueString(g_xwin.font_canvas,g_ymin,g_ymax,g_disp.yAcc,&dims);
 
/* calculate left and right axis position */
g_disp.right = width - RIGHT_BORDER;
xv_get(g_xwin.font_canvas, FONT_STRING_DIMS, g_disp.pszYlabel, &dims);
g_disp.left = LEFT_BORDER + TICK_SIZE/2 
		+ ((dims.width/2 > nTmp) ? dims.width/2 : nTmp);

/* Ticking for x labeling, */
nTmp = -1;

/* increase accuracy until it is too big */
for (g_disp.xAcc = 0; g_disp.xAcc < -nTmp ;g_disp.xAcc++) {
    size=SizeOfValueString(g_xwin.font_canvas,g_xmin,g_xmax,g_disp.xAcc,&dims);
    nTmp = CalculateTicking(g_disp.right - g_disp.left, 
			g_xmin, g_xmax, size + LABEL_DISTANCE,
                        &g_disp.fxTickDelta, &g_disp.fxTickMin);
    }

size = SizeOfValueString(g_xwin.font_canvas,g_xmin,g_xmax,g_disp.xAcc,&dims);
nTmp = CalculateTicking(g_disp.right - g_disp.left, 
			g_xmin, g_xmax, size + LABEL_DISTANCE,
                        &g_disp.fxTickDelta, &g_disp.fxTickMin);
g_disp.xAcc = (nTmp < 0) ? -nTmp : 0;

/* wmgr_refreshwindow( g_xwin.frame); */
}

/**********************************************************************
* SizeOfValueString
* calculates the maximum size for a label string
*/
int SizeOfValueString(font,min,max,acc,pDims)
Xv_Font font;
double min,max;
int acc;
Font_string_dims *pDims;
{
char szTmp[126];
int size;

FormatValue(szTmp,min,min,max,acc);
xv_get(font, FONT_STRING_DIMS, szTmp, pDims);
size = pDims->width;
FormatValue(szTmp,max,min,max,acc);
xv_get(font, FONT_STRING_DIMS, szTmp, pDims);
if (pDims->width > size) size = pDims->width;
return size;
}

/**********************************************************************
* FormatValue
* output of double value to string in format
*/
void FormatValue(psz,value,min,max,acc)
char *psz;
double value,min,max;
int acc;
{
sprintf(psz,"%.*lf",acc,value);
}

/*********************************************************************
* XV_hans_repaint_canvas
* repaints the window with the diagrams
*/
void
XV_hans_repaint_canvas (canvas, paint_window, display, xid, xrects)
Canvas      canvas;
Xv_Window   paint_window;
Display     *display;
Window      xid;
Xv_xrectlist *xrects;
{
double xScale,yScale;
unsigned long *colors;
unsigned count;
Xv_Font font = g_xwin.font_canvas;
XGCValues gcVals;
Font_string_dims dims;
char szTmp[126];
int nTmp,size,i;
int nTickFrom,nTickTo;
double lfTmp;
static nRepaints = 1;

/* printf("DIAGRAM repainting #%d\n",nRepaints); nRepaints++; */
/* if no value paint nothing */
if (!g_nValues)
	return;

/* calculate scaling for map to display */
xScale = (double)(g_disp.right - g_disp.left) / (g_xmax - g_xmin);
yScale = (double)(g_disp.bottom - g_disp.top) / (g_ymin - g_ymax);

/* get color map */
colors = (unsigned long *)xv_get(g_xwin.canvas,WIN_X_COLOR_INDICES);

/* erase background */
XSetForeground(display,g_xwin.gc, colors[CI_BACKGROUND]);
XFillRectangle(display,xid ,g_xwin.gc, 0,0, 
			(int)xv_get(paint_window,XV_WIDTH),
                        (int)xv_get(paint_window,XV_HEIGHT) );

/* draw graph */
XSetLineAttributes(display, g_xwin.gc, 0, LineSolid, CapButt, JoinMiter);
for (i=0; i < g_nGraphs; i++) {
    XSetForeground(display, g_xwin.gc, colors[CI_GRAPH1 + i]);
    if (g_cmdLine.bScatter==0) {  /* draw lines */
        XSetLineAttributes(display, g_xwin.gc, 0, LineSolid, CapButt, JoinMiter);
        for (count=0; count < g_nValues - 1; count++) {
	    XDrawLine(display, xid, g_xwin.gc,
		    (int)((g_aX[count] - g_xmin) * xScale) + g_disp.left,
		    (int)((g_aY[i][count] - g_ymax) * yScale) + g_disp.top,
		    (int)((g_aX[count+1] - g_xmin) * xScale) + g_disp.left,
                    (int)((g_aY[i][count+1] - g_ymax) * yScale) + g_disp.top );
	    } /* end for */
        } /* end if */
      else { /* just points */
        XSetLineAttributes(display, g_xwin.gc, 2, LineSolid, CapButt, JoinMiter);
        for (count=0; count < g_nValues ; count++) { /* removed ues -1 */
            XDrawPoint(display, xid, g_xwin.gc,
                    (int)((g_aX[count] - g_xmin) * xScale) + g_disp.left,
                    (int)((g_aY[i][count] - g_ymax) * yScale) + g_disp.top);
            } /* end for */
        } /* end if else */
    } /* end big for */

/* draw axii */
XSetLineAttributes(display, g_xwin.gc, 2, LineSolid, CapButt, JoinMiter);
XSetForeground(display,g_xwin.gc, colors[CI_XAXIS]);
XDrawLine(display, xid, g_xwin.gc, g_disp.left, g_disp.bottom, 
				g_disp.right, g_disp.bottom);
XSetForeground(display,g_xwin.gc, colors[CI_YAXIS]);
XDrawLine(display, xid, g_xwin.gc, g_disp.left, g_disp.top, 
				g_disp.left, g_disp.bottom);

/* X draw ticks or grid*/
XSetForeground(display,g_xwin.gc, colors[CI_XTICK]);
/* ticks are solid, grids are dashed */
XSetLineAttributes(display, g_xwin.gc, 0, 
	g_cmdLine.bGrid ? LineOnOffDash : LineSolid ,CapButt, JoinMiter);
/* the y possitions are not changeing */
nTickFrom = g_disp.bottom - (g_cmdLine.bGrid ? TICK_SIZE / 2 : 0) ;
nTickTo   = (g_cmdLine.bGrid ? g_disp.top : g_disp.bottom + TICK_SIZE / 2);
for (lfTmp = g_disp.fxTickMin; lfTmp <= g_xmax; lfTmp += g_disp.fxTickDelta) {
	XDrawLine(display,xid, g_xwin.gc, 
		(int)((lfTmp - g_xmin) * xScale) + g_disp.left,
		nTickFrom,
		(int)((lfTmp - g_xmin) * xScale) + g_disp.left,
		nTickTo );
	}

/* X print numbers */
XSetForeground(display,g_xwin.gc, colors[CI_XTICKLABEL]);
XSetLineAttributes(display, g_xwin.gc, 1, LineSolid, CapButt, JoinMiter);
for (lfTmp = g_disp.fxTickMin; lfTmp < g_xmax; lfTmp += g_disp.fxTickDelta) {
	FormatValue(szTmp,lfTmp,g_xmin,g_xmax,g_disp.xAcc);
	xv_get(font, FONT_STRING_DIMS, szTmp, &dims);
	XDrawString(display, xid, g_xwin.gc,
		(int)((lfTmp - g_xmin) *xScale) + g_disp.left - dims.width/2,
		g_disp.bottom + dims.height + TICK_SIZE / 2,
		szTmp,strlen(szTmp));
	}

/* X label */
XSetForeground(display,g_xwin.gc, colors[CI_XLABEL]);
xv_get(font, FONT_STRING_DIMS, g_disp.pszXlabel, &dims);
XDrawString(display, xid, g_xwin.gc,
		g_disp.right - dims.width, 
		g_disp.bottom + 2 * dims.height + TICK_SIZE / 2,
		g_disp.pszXlabel, strlen(g_disp.pszXlabel) - 1 );


/* Y draw ticks and grids*/
XSetForeground(display,g_xwin.gc, colors[CI_YTICK]);
/* ticks are solid, grids are dashed */
XSetLineAttributes(display, g_xwin.gc, 0,
        g_cmdLine.bGrid ? LineOnOffDash : LineSolid ,CapButt, JoinMiter);
/* the x possitions are not changeing */
nTickFrom = g_disp.left - (g_cmdLine.bGrid ? TICK_SIZE / 2 : 0) ;
nTickTo   = (g_cmdLine.bGrid ? g_disp.right : g_disp.left + TICK_SIZE / 2);
for (lfTmp = g_disp.fyTickMin; lfTmp <= g_ymax; lfTmp += g_disp.fyTickDelta) {
	XDrawLine(display, xid, g_xwin.gc,
		nTickFrom,
		(int)((lfTmp - g_ymax) * yScale) + g_disp.top,
		nTickTo,
		(int)((lfTmp - g_ymax) * yScale) + g_disp.top);
	}
/* Y print numbers */
XSetForeground(display,g_xwin.gc, colors[CI_YTICKLABEL]);
XSetLineAttributes(display, g_xwin.gc, 1, LineSolid, CapButt, JoinMiter);
for (lfTmp = g_disp.fyTickMin; lfTmp < g_ymax; lfTmp += g_disp.fyTickDelta) {
	FormatValue(szTmp,lfTmp,g_ymin,g_ymax,g_disp.yAcc);
	xv_get(font, FONT_STRING_DIMS, szTmp, &dims);
	XDrawString(display, xid, g_xwin.gc,
		g_disp.left - dims.width - TICK_SIZE / 2,
		(int)((lfTmp - g_ymax) * yScale) + g_disp.top + dims.height / 2,
		szTmp,strlen(szTmp));
	}

/* Y label */
XSetForeground(display, g_xwin.gc, colors[CI_YLABEL]);
xv_get(font, FONT_STRING_DIMS, g_disp.pszYlabel, &dims);
XDrawString(display, xid, g_xwin.gc,
		g_disp.left - dims.width/2, g_disp.top-5,
		g_disp.pszYlabel, strlen(g_disp.pszYlabel)-1);
}

/**********************************************************************
* CalculateTicking
* calculates the spacing of the tickmarks and the position of the first
* tickmark on an axis
*       nDispSize       length of the whole axis
*       lfMin           minimum value displayed on axis
*       lfMax           maximum value displayed on axis
*       nTickLabSize    size of the largest tickmark label
*       plfDelta        pointer to an double, receives the calculated
*                       distance for the tickmarks
*       plfMin          pointer to an double, receives the value for the
*                       first tickmark
* returns the dimension of of plfDelta
*/
int CalculateTicking(nDispSize,lfMin,lfMax,nTickLabSize,plfDelta,plfMin)
int nDispSize;          /* available space on display in pixels */
double lfMin,lfMax;   /* minimum and maximum value to display */
int nTickLabSize;       /* size of largest tick label to display in pixels */
double *plfDelta;       /* Increment of x for tick labels (return) */
double *plfMin;         /* Minimal value for x. Start of tick labels (return)*/
{
int nTmp,dim;
 
nTmp = nDispSize / nTickLabSize; /* how max ticks maximal */
/* distance between these ticks */
(*plfDelta) = (lfMax - lfMin) / (double)nTmp;
 
/* calculate dimension */
dim = (int)floor(log10(fabs((*plfDelta))));
 
/* we will have an increment of 1 2 or 5 */
if ((*plfDelta)*exp10((double)-dim) <= 1.0)
        (*plfDelta) = exp10((double)dim);
else if ((*plfDelta) <= 2.0*exp10((double)dim))
        (*plfDelta) = 2.0*exp10((double)dim);
else if ((*plfDelta) <= 5.0*exp10((double)dim))
        (*plfDelta) = 5.0*exp10((double)dim);
else
        (*plfDelta) = 10.0*exp10((double)dim);
 
/* now we will start at an rounded number */
(*plfMin) = (*plfDelta) * ceil(lfMin / (*plfDelta));
/*
printf("Ticking min = %lf, delta = %lf, dim = %d\n", *plfMin, *plfDelta, dim);
*/

return dim;
}

/**********************************************************************
* repaint_panel	 paints the panel background black
*/
void XV_hans_repaint_panel(panel,pw)
Panel panel;
Xv_Window pw;
{
XID xid = (XID)xv_get(pw,XV_XID);
Display  *display = (Display *)xv_get(panel,XV_DISPLAY);
XGCValues gcVals;
Font_string_dims dims;

XGetGCValues(display,g_xwin.panel_gc, GCForeground | GCBackground, &gcVals);

/* draw background */
XSetForeground(display,g_xwin.panel_gc, gcVals.background);
XFillRectangle(display	, xid , g_xwin.panel_gc,
	0, 0, xv_get(pw,XV_WIDTH), xv_get(pw,XV_HEIGHT) );

/* draw title */
xv_get((Xv_Font)xv_get(panel,XV_FONT),FONT_STRING_DIMS,
			g_disp.pszDiagram,	&dims);
xv_get(pw,XV_WIDTH);

XSetForeground(display,g_xwin.panel_gc, gcVals.foreground);
XDrawString(display, xid, g_xwin.panel_gc, 
		(xv_get(pw,XV_WIDTH)-dims.width)/2, 22,
		g_disp.pszDiagram, strlen(g_disp.pszDiagram));
}


