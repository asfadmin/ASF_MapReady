#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <malloc.h>
#include <errno.h>

#include <X11/Xlib.h>

/* Use these if no motif */
#include <X11/Intrinsic.h>   
#include <X11/StringDefs.h>    


/* Use these if motif */
/*
#include <Xm/Xm.h>  
#include <Xm/DrawingA.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ScrolledW.h>
#include <Xm/Label.h>
*/

#include <X11/cursorfont.h>
#include <X11/keysym.h>    /* for popup window only */
  
#define MAX_COLORS 256

#define SGImode

#define icon_width 29
#define icon_height 29
static char icon_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3e, 0x8e, 0x11, 0x02,
   0x08, 0x41, 0x32, 0x02, 0x88, 0x21, 0x54, 0x02, 0x88, 0x20, 0x94, 0x02,
   0x88, 0x20, 0x94, 0x02, 0x88, 0x21, 0x94, 0x02, 0x08, 0x41, 0x12, 0x03,
   0x3e, 0x8e, 0x11, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

/* Global variables */
Display 	*dgx;
GC			gc;
Window		root;
Window      top[40];
Window 		wgx[40];
Window 		fgx[40];
Window 		lgx[40];
Widget		scrl[40];
Widget		labl[40];
Widget		draw[40];
Widget    	form[40];
Widget      formy;
XEvent		event;
Pixmap		icon, disparity;
Colormap	cmap;
int     i_type[40];
int     i_gx = 0;
int     i_init = 0;
int     i_app = 0;
int 	i_ctype = 0;
int     i_clrs = 0;
int		screen;
int		i_ctble;
int     allocated;
int     i_push = 0;
unsigned long	indices[MAX_COLORS]; 
unsigned char	red[MAX_COLORS], green[MAX_COLORS], blue[MAX_COLORS];

/* Local functions */
static void  initializeComm();
static void  Button_1();
static void  Button_2();
static void  Button_3();
static void  Button_4();
static void  Button_5();
static void  Button_6();
static void  Button_7();
static void  Button_8();
static void  Button_9();
static void  Button_10();
static void  Button_11();
static void  Button_12();
static void  Button_13();
static void  Button_14();
static void  Button_15();
static void  Button_16();
static void  Button_17();
static void  Button_18();
static void  Button_19();
static void  Button_20();
static void  Button_21();
static void  Button_22();
static void  Button_23();
static void  Button_24();
static void  Button_25();
static void  Button_26();
static void  Button_27();
static void  Button_28();
static void  Button_29();
static void  Button_30();
static void  Button_31();
static void  Button_32();
static void  Button_33();
static void  Button_34();
static void  Button_35();
static void  Button_36();
static void  Button_37();
static void  Button_38();
static void  Button_39();
static void  Button_40();
static void  Button_quit();
static void  grey_8bita();
static void  grey_8bitb();
static void  grey_4bita();
static void  grey_4bitb();
static void  colr_8bita();
static void  colr_4bita();
static void  colr_4bitb();
static void  mixr_8bita();
static void  alo_colors();
static void  put_colors();
static void  fre_colors();
void	     read_events();


/*******************************************/
#ifdef SGI
void plot_data_(i_w,i_num,r_x,r_y)
#endif
#ifdef HP
void plot_data(i_w,i_num,r_x,r_y)
#endif
    int i_w[];
    int i_num[];
    float r_x[10000], r_y[10000];
{
    int           i;
    int           i_x1,i_y1;
    int           i_x2,i_y2;
    int           xr, yr;
    unsigned int  width, height, bwr, dr;
    
    
    /* Scale the values according to the size of the window */
    XGetGeometry(dgx, wgx[i_w[0]], &root, &xr, &yr, &width, &height, &bwr, &dr);

    for (i = 0; i < i_num[0] - 1; i++) {
     
      i_x1 = (int) ((r_x[i]) * width );
      i_y1 = height - (int) ((r_y[i]) * height );
      i_x2 = (int) ((r_x[i+1]) * width );
      i_y2 = height - (int) ((r_y[i+1]) * height );
    
      if (i_x1 < 0)   i_x1 = 0;
      if (i_x1 > width) i_x1 = width;
      if (i_x2 < 0)   i_x2 = 0;
      if (i_x2 > width) i_x2 = width;

      if (i_y1 < 0)   i_y1 = 0;
      if (i_y1 > height) i_y1 = height;
      if (i_y2 < 0)   i_y2 = 0;
      if (i_y2 > height) i_y2 = height;

      /* printf("hi from plot_data3\n"); */
      XDrawLine(dgx, wgx[i_w[0]], gc, i_x1, i_y1, i_x2, i_y2);
     }

}

#ifdef SGI
void display_idx_(i_w, i_x, i_y, i_width, i_height, i_bpl, i_data)
#endif
#ifdef HP
void display_idx(i_w, i_x, i_y, i_width, i_height, i_bpl, i_data)
#endif
    int i_w, i_x, i_y, i_width, i_height, i_bpl;
    float i_data[];
	
{   unsigned char i_nbits[4000001];
	int i, j;
    int i_val;

    XImage      xim;
    

	for (i = 0 ; i < i_width ; i++) for(j = 0; j < i_height ; j++) {
             if (i*i_width+j > 4000000) {
               printf("error - %d %d %d %d %d\n",i,j,i_width,i_height,i*i_width+j);
               exit(0);
             } /* endif */
             i_val = i_data[i*i_bpl+j];
             i_nbits[i*i_width+j] = indices[i_val];  }
	
    xim.depth          = DefaultDepth(dgx, screen); 
    xim.data           = i_nbits;
    xim.bitmap_pad     = 8;
    xim.width          = i_width;
    xim.height         = i_height;
    xim.format         = ZPixmap;
    xim.bits_per_pixel = 8;
    xim.byte_order     = MSBFirst;
    xim.bytes_per_line = i_width;
	
    XPutImage(dgx, wgx[i_w], gc, &xim, 0, 0, i_x, i_y, i_width, i_height);
 
}


#ifdef SGI
void display_img_(i_w, i_x, i_y, i_width, i_height, i_bpl, r_dat)
#endif
#ifdef HP
void display_img(i_w, i_x, i_y, i_width, i_height, i_bpl, r_dat)
#endif

    int i_w[];
    int i_x[];
    int i_y[];
    int i_width[];
    int i_height[];
    int i_bpl[];
    float r_dat[];
	
{   unsigned char i_nbits[4000001];
	int i, j, k;

    XImage      xim;
    

	for (i = 0 ; i < i_height[0] ; i++) for(j = 0; j < i_width[0] ; j++) {
             if (i*i_width[0]+j > 4000000) {
               printf("error - %d %d %d %d %d\n",i,j,i_width[0],i_height[0],i*i_width[0]+j);
               exit(0);
             } /* endif */
             k = (int)((float)(i_ctble-1)*r_dat[i*i_bpl[0]+j]);
             if (k < 0) { 
               printf("rdat < 0 %d \n",k);
               k = 0;
             } /* endif */
             if (k > i_ctble-1) { 
               printf("rdat > i_ctble-1   %d %d \n",k,i_ctble-1);
               k = i_ctble-1;
             } /* endif */
             i_nbits[i*i_width[0]+j] = indices[k];  }
	
    xim.depth          = DefaultDepth(dgx, screen); 
    xim.data           = i_nbits;
    xim.bitmap_pad     = 8;
    xim.width          = i_width[0];
    xim.height         = i_height[0];
    xim.format         = ZPixmap;
    xim.bits_per_pixel = 8;
    xim.byte_order     = MSBFirst;
    xim.bytes_per_line = i_width[0];
	
    XPutImage(dgx, wgx[i_w[0]], gc, &xim, 0, 0, i_x[0], i_y[0], i_width[0], i_height[0]);
 
}


#ifdef SGI
void display_rmg_(i_w, i_x, i_y, i_width, i_height, i_bpl, r_dat1,r_dat2)
#endif
#ifdef HP
void display_rmg(i_w, i_x, i_y, i_width, i_height, i_bpl, r_dat1,r_dat2)
#endif

    int i_w[];
    int i_x[];
    int i_y[];
    int i_width[];
    int i_height[];
    int i_bpl[];
    float r_dat1[];
    float r_dat2[];
	
{   unsigned char i_nbits[4000001];
	int i, j;
    int i_magn;
    int i_colr;

    int           i_val, i_ppp;
    unsigned int  fwidth, fheight, width, height, bwr, dr;

    XImage      xim;
    
    i_magn = (int)sqrt((float)i_ctble);
    i_colr = i_magn;

    if (i_width[0]*i_height[0] > 4000000) {
      printf("error - viewport too big (4000000 max) -  %d %d %d\n",i_width[0],i_height[0],
            i_width[0]*i_height[0]);
      exit(0);
    } /* endif */

	for (i = 0 ; i < i_height[0] ; i++) for(j = 0; j < i_width[0] ; j++) {

             i_ppp = i*i_bpl[0]+j;

             i_val = i_colr*(int)(i_magn*r_dat1[i_ppp]) + (int)(i_colr*r_dat2[i_ppp]);
             if (i_val > i_ctble-1) {
               printf("i_val error   %d  %d\n",i_val,i_ctble);
               i_val = i_ctble-1;
             } /* endif */
             if (i_val < 0) {
               printf("i_val error   %d  %d\n",i_val,0);
               i_val = 0;
             } /* endif */

             i_nbits[i*i_width[0]+j] = indices[i_val];  

    } /* enddo */
	
    xim.depth          = DefaultDepth(dgx, screen); 
    xim.data           = i_nbits;
    xim.bitmap_pad     = 8;
    xim.width          = i_width[0];
    xim.height         = i_height[0];
    xim.format         = ZPixmap;
    xim.bits_per_pixel = 8;
    xim.byte_order     = MSBFirst;
    xim.bytes_per_line = i_width[0];
	
    XPutImage(dgx, wgx[i_w[0]], gc, &xim, 0, 0, i_x[0], i_y[0], i_width[0], i_height[0]);
 
}

#ifdef SGI
void display_label_(i_w,a_string)
#endif
#ifdef HP
void display_label(i_w,a_string)
#endif

int i_w[];
char a_string[80];
{
    int i;
    int i_cnt;
    char a_lbl[80];

    /* displays a string at the top of a window */
    XmString   motif_string;
    Arg        args[2];
        

    i_cnt = 0;
    for (i=0; i < 78; i++) {
          a_lbl[i] = a_string[i]; 
          if (a_lbl[i] != 0 && a_lbl[i] != 32 ) i_cnt = i+1;
        } /* enddo */
        a_lbl[i_cnt] = 0;

    motif_string = XmStringCreate((char *)a_lbl, XmSTRING_DEFAULT_CHARSET);
    
    XtSetArg(args[0], XmNlabelString, motif_string);
    XtSetValues(labl[i_w[0]], args, 1);
    XFlush(dgx);    
    
}

#ifdef SGI
void get_widget_(i_widget)
#endif
#ifdef HP
void get_widget(i_widget)
#endif

    int i_widget[];
	
{
    i_widget[0] = i_push;

}


#ifdef SGI
void topwin_(i_w)
#endif
#ifdef HP
void topwin(i_w)
#endif

    int i_w[];
	
{   int i, j;

    if (top[i_w[0]] != 0) XRaiseWindow(dgx, top[i_w[0]]);
   
}


#ifdef SGI
void get_wininfo_(i_w, i_vx, i_vy, i_vw, i_vh, i_cw, i_ch,i_widget)
#endif
#ifdef HP
void get_wininfo(i_w, i_vx, i_vy, i_vw, i_vh, i_cw, i_ch,i_widget)
#endif

    int i_w[];
    int i_vx[], i_vy[];
    int i_vw[], i_vh[];
    int i_cw[], i_ch[];
    int i_widget[];
	
{   int i, j;

    int           fxr, fyr, xr, yr;
    unsigned int  fwidth, fheight, width, height, bwr, dr;

    XImage      xim;

    XGetGeometry(dgx, wgx[i_w[0]], &root, &xr, &yr, &width, &height, &bwr, &dr);
   
    i_vx[0] = -xr;
    i_vy[0] = -yr;
    i_cw[0] = width;
    i_ch[0] = height;

    XGetGeometry(dgx, fgx[i_w[0]], &root, &xr, &yr, &width, &height, &bwr, &dr);

    i_vw[0] = width;
    i_vh[0] = height;
 
    i_widget[0] = i_push;

}

#ifdef SGI
void move_scroll_(i_w,i_x,i_y)
#endif
#ifdef HP
void move_scroll(i_w,i_x,i_y)
#endif


    int  i_w[];
    int  i_x[];
    int  i_y[];

{ 
    int   n;
    Arg   args[10];
    Widget vsb;
    Widget hsb;

    XWindowChanges    xwc;
    XEvent            report;

    int increment=0;
    int maximum=0;
    int minimum=0;
    int page_incr=0;
    int slider_size=0;
    int value=0;

    XtVaGetValues(scrl[i_w[0]],XmNverticalScrollBar,  &vsb,NULL);
    XtVaGetValues(scrl[i_w[0]],XmNhorizontalScrollBar,&hsb,NULL);

    XtVaGetValues(vsb,XmNincrement,     &increment,
                      XmNmaximum,       &maximum,
                      XmNminimum,       &minimum,
                      XmNpageIncrement, &page_incr,
                      XmNsliderSize,    &slider_size,
                      XmNvalue,         &value,
                      NULL);
    
/*  
    printf("inc=%d, max=%d, min=%d, page=%d, slider=%d, value=%d\n",
         increment,maximum,minimum,page_incr,slider_size,value); 
*/

    value=i_y[0];
    if (value < minimum) value = minimum;
    if (value > maximum-slider_size) value = maximum-slider_size;
    XmScrollBarSetValues(vsb,value,slider_size,increment,page_incr,True);

    XtVaGetValues(hsb,XmNincrement,     &increment,
                      XmNmaximum,       &maximum,
                      XmNminimum,       &minimum,
                      XmNpageIncrement, &page_incr,
                      XmNsliderSize,    &slider_size,
                      XmNvalue,         &value,
                      NULL);

    value=i_x[0];
    if (value < minimum) value = minimum;
    if (value > maximum-slider_size) value = maximum-slider_size;
    XmScrollBarSetValues(hsb,value,slider_size,increment,page_incr,True);
    
/*     The following code would change the slider positions, but not move the data properly
    n = 0;
    XtSetArg(args[n], XmNvalue, i_x[0]); n++;
    XtSetValues(hsb, args, n); 

    n = 0;
    XtSetArg(args[n], XmNvalue, i_y[0]); n++;
    XtSetValues(vsb, args, n); 

    xwc.x=-i_x[0];
    xwc.y=-i_y[0];

    XConfigureWindow(dgx, wgx[i_w[0]], CWX | CWY, &xwc); 
*/
                        
}


#ifdef SGI
void move_win_(i_w,i_x,i_y)
#endif
#ifdef HP
void move_win(i_w,i_x,i_y)
#endif


    int  i_w[];
    int  i_x[];
    int  i_y[];

{ 
    XWindowChanges    xwc;

    xwc.x=-i_x[0];
    xwc.y=-i_y[0];

    XConfigureWindow(dgx, wgx[i_w[0]], CWX | CWY, &xwc);
            
}


#ifdef SGI
void getevent_(i_flg,i_event)
#endif
#ifdef HP
void getevent(i_flg,i_event)
#endif

    int i_flg[];
    int i_event[10];
{
    XEvent        report;

    int           i;
    int          id;
    int      i_loop;

	char buffer[40];
	int bufsize = 40;
	KeySym keysym;
	XComposeStatus compose;
	int count;

      i_event[0] = 0;
      i_event[1] = 0;
      i_event[2] = 0;
      i_event[3] = 0;
      i_event[4] = 0;
      i_event[5] = 0;
      i_event[6] = 0;
      if (i_flg[0] == 0 | XPending(dgx) ) {
        i_loop = 0;
        while(i_loop == 0) {
          XNextEvent(dgx,&report);
          /* printf("report.type = %d \n",report.type); */
          /* switch (report.type) {
            case Expose:
              printf("report=Expose %d\n",report.xexpose.window);
              break;
            case ConfigureNotify:
              printf("report=ConfigureNotify %d\n",report.xconfigure.window);
              break;
            case ButtonPress:
              printf("report=ButtonPress %d\n",report.xbutton.window);
              break;
            case ButtonRelease:
              printf("report=ButtonRelease %d\n",report.xbutton.window);
              break;
            case KeyPress:
              printf("report=KeyPress %d\n",report.xkey.window);
              break;
            case KeyRelease:
              printf("report=KeyRelease %d\n",report.xkey.window);
              break;
            case DestroyNotify:
              printf("report=DestroyNotify %d\n",report.xdestroywindow.window);
              break;
            default:
              break;  */ /* do nothing */
          /* }  */ /* end case */
          switch (report.type) {
            case Expose:
              for(i=1; i<i_gx+1; i++) 
                if (report.xexpose.window == wgx[i]) i_event[0] = i; 
              if (i_event[0] == 0) break;
              i_event[1] = 1;
              i_event[2] = report.xexpose.x;
              i_event[3] = report.xexpose.y;
              i_event[4] = report.xexpose.width;
              i_event[5] = report.xexpose.height;
              i_event[6] = report.xexpose.count;
              i_loop = 1;
              break;
            case ConfigureNotify:
              for (i=1; i<i_gx+1; i++) 
                if (report.xconfigure.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xconfigure.window == fgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              if (i_event[0] > 0) {
                i_event[1] = 2;
                i_event[2] = -report.xconfigure.x;
                i_event[3] = -report.xconfigure.y;
                i_event[4] = report.xconfigure.width;
                i_event[5] = report.xconfigure.height;
                i_event[6] = 0;
                i_loop = 1; }
              else {
                i_event[0] = -i_event[0];
                i_event[1] = 3;
                i_event[2] = report.xconfigure.x;
                i_event[3] = report.xconfigure.y;
                i_event[4] = report.xconfigure.width;
                i_event[5] = report.xconfigure.height;
                i_event[6] = 0;
                i_loop = 1;
              } /* endif */
              break;
            case ButtonPress:
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              i_event[1] = 4;
              i_event[2] = report.xbutton.button;
              i_event[3] = report.xbutton.x;
              i_event[4] = report.xbutton.y;
              i_event[5] = 0;
              i_event[6] = 0;
              i_loop = 1;
              break;
            case ButtonRelease:
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              i_event[1] = 5;
              i_event[2] = report.xbutton.button;
              i_event[3] = report.xbutton.x;
              i_event[4] = report.xbutton.y;
              i_event[5] = 0;
              i_event[6] = 0;
              i_loop = 1;
              break;
            case KeyPress:
              for (i=1; i<i_gx+1; i++) 
                if (report.xkey.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xkey.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              count = XLookupString(&report.xkey, buffer,bufsize,&keysym,&compose); 
              i_event[1] = 6;
              i_event[2] = report.xkey.keycode;
              i_event[3] = report.xkey.x;
              i_event[4] = report.xkey.y;
              i_event[5] = keysym;
              i_event[6] = 0;
              i_loop = 1;
              break;
            case KeyRelease:
              for (i=1; i<i_gx+1; i++) 
                if (report.xkey.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xkey.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              i_event[1] = 7;
              i_event[2] = report.xkey.keycode;
              i_event[3] = report.xkey.x;
              i_event[4] = report.xkey.y;
              i_event[5] = 0;
              i_event[6] = 0;
              i_loop = 1;
              break;
            case DestroyNotify:
              for (i=1; i<i_app+1; i++) {
                if (report.xdestroywindow.window == top[i]) {
                      top[i] = 0;
                      /* printf("Setting top[%d] to zero\n",i); */
                }} 
              for (i=1; i<i_gx+1; i++) 
                if (report.xdestroywindow.window == wgx[i]) i_event[0] = i; 
              if (i_event[0] == 0) break;
              i_event[1] = 8;
              i_event[2] = 0;
              i_loop = 1; 
              break;
            case MotionNotify:
              for (i=1; i<i_gx+1; i++) 
                if (report.xmotion.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xmotion.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              i_event[1] = 9;
              i_event[2] = report.xmotion.state;
              i_event[3] = report.xmotion.x;
              i_event[4] = report.xmotion.y;
              i_event[5] = 0;
              i_event[6] = 0;
              i_loop = 1;
              break;
            default:
              break; /* do nothing */
          } /* end case */
          if (i_event[1] == 4 | i_event[1] == 5) {
            if (i_event[2] == 2 ) {
              if (i_event[0] > 0 ) {
                if (i_type[ i_event[0]] == 1 ) report.xbutton.button = 1; }
              else {
                if (i_type[-i_event[0]] == 4 ) report.xbutton.button = 1;
                if (i_type[-i_event[0]] == 3 ) report.xbutton.button = 1;
              } /* endif */
            } /* endif */
          } /* endif */
          XtDispatchEvent(&report);
          if (i_flg[0] == 1 && !XPending(dgx)) i_loop = 1;
        } /* end while */        
      } /* end if */ 
}

#ifdef SGI
void getbutton_(i_w,i_pos,i_button)
#endif
#ifdef HP
void getbutton(i_w,i_pos,i_button)
#endif

    int i_w[1];
    int i_pos[2];
    int i_button[1];
{
    XEvent        report;

    int           i;
    int          id;

      i_w[0] =  0;
      i_pos[0] = -1;
      i_pos[1] = -1;
      i_button[0] = 0;

      while(i_button[0] == 0 ) {
          XNextEvent(dgx,&report);
          if (report.type == ButtonPress) {
            i_pos[0] = report.xbutton.x;
            i_pos[1] = report.xbutton.y;
            id = report.xbutton.window;
            i_button[0] = report.xbutton.button;
            for (i = 1; i < i_gx+1; i++) {
              if (id == wgx[i]) i_w[0] = i;
            } /* enddo */ 
	        XtDispatchEvent(&report);  }
          else {
            if (report.type == ButtonRelease) {
              i_pos[0] = report.xbutton.x;
              i_pos[1] = report.xbutton.y;
              id = report.xbutton.window;
              i_button[0] = -report.xbutton.button;
              for (i = 1; i < i_gx+1; i++) {
                if (id == wgx[i]) i_w[0] = i;
              } /* enddo */ 
              XtDispatchEvent(&report); }
            else {
   	          XtDispatchEvent(&report);
            } /* endif */
          } /* endif */
      } /* enddo */ 
}


#ifdef SGI
void getcursor_(i_w,i_pos)
#endif
#ifdef HP
void getcursor(i_w,i_pos)
#endif

    int i_w[1];
    int i_pos[3];
{
    int          i_rx, i_ry, i_cx, i_cy;
    Window       rootc, childc;
    unsigned int i_keys;

      i_pos[0] = -9999;
      i_pos[1] = -9999;
      i_pos[2] =  0;
      XQueryPointer(dgx, wgx[i_w[0]], &rootc, &childc, &i_rx, &i_ry, &i_cx, &i_cy, &i_keys);
      i_pos[0] = i_cx;
      i_pos[1] = i_cy;
      i_pos[2] = i_keys;
      read_events();
}


#ifdef SGI
void setcolor_(i_c)
#endif
#ifdef HP
void setcolor(i_c)
#endif

    int i_c[];
{

      if (i_c[0] > 0) { i_ctype = i_c[0]; }  else { i_ctype = -i_c[0];  }

      if (allocated == 0) { alo_colors(); allocated = 1; }

      if (i_ctype == 1) { grey_8bita(); put_colors(); }
      if (i_ctype == 2) { grey_4bita(); put_colors(); }
      if (i_ctype == 3) { grey_4bitb(); put_colors(); }
      if (i_ctype == 4) { colr_4bita(); put_colors(); }
      if (i_ctype == 5) { colr_4bitb(); put_colors(); }
      if (i_ctype == 6) { mixr_8bita(); put_colors(); }
      if (i_ctype == 7) { grey_8bita(); put_colors(); }
      if (i_ctype == 8) { grey_8bitb(); put_colors(); }
      if (i_ctype == 9) { colr_8bita(); put_colors(); }

      if (i_c[0] > 0) { fre_colors(); allocated = 0;  }
}


#ifdef SGI
void clear_win_(i_w)
#endif
#ifdef HP
void clear_win(i_w)
#endif

    int i_w[];
{
    XClearWindow(dgx, wgx[i_w[0]]);
}

#ifdef SGI
void get_dialog_test_(a_msg,a_rsp)
#endif
#ifdef HP
void get_dialog_test(a_msg,a_rsp)
#endif

    char a_msg[];
    char a_rsp[];
{
    XEvent report;

    int           i;
    int           j;
    int          id;
    int      i_loop;

    Window pop_top;
    Window pop_win;
    Widget pop_app;
    Widget pop_ddd;
	char buffer[40];
	int bufsize;
	int start_x,start_y;
	KeySym keysym;
	XComposeStatus compose;
	int count;
	unsigned int pop_width, pop_height;
	char a_lbl[40];
	char a_top[40];
    int x,y;
	int length;
    int i_cnt;
    int i_event[10];
    Arg             args[15];
    int             n = 1;

      i_event[0] = 0;
      i_event[1] = 0;
      i_event[2] = 0;
      i_event[3] = 0;
      i_event[4] = 0;
      i_event[5] = 0;
      i_event[6] = 0;

        bufsize = 40;
        count = 0;
        x = 100;
        y = 100;


        i_cnt=0;
        for (j=0; j < 39; j++) {
          a_rsp[j] = 0;
          a_lbl[j] = a_msg[j]; 
          if (a_lbl[j] != 0 && a_lbl[j] != 32 ) i_cnt = j+1;
        } /* enddo */
        if (i_cnt == 40) i_cnt = 39;
        a_lbl[i_cnt] = 0;



        strcpy (a_top,"Input Window");
        n = 0;
        XtSetArg(args[n], XmNtitle,           a_top); n++;
        XtSetArg(args[n], XmNx,                   0); n++;
        XtSetArg(args[n], XmNy,                   0); n++;
        XtSetArg(args[n], XmNwidth,             300); n++;
        XtSetArg(args[n], XmNheight,             75); n++;
        XtSetArg(args[n], XmNbackground, BlackPixel(dgx,screen)); n++; 
        XtSetArg(args[n], XmNforeground, WhitePixel(dgx,screen)); n++;
        pop_app = XtAppCreateShell(a_lbl,"appClass",
         	topLevelShellWidgetClass,dgx,
	     	args, n);

        XtRealizeWidget(pop_app);

        n = 0;
        XtSetArg(args[n], XmNx,                   0); n++;
        XtSetArg(args[n], XmNy,                   0); n++;
        XtSetArg(args[n], XmNwidth,             300); n++;
        XtSetArg(args[n], XmNheight,             75); n++;
/*
        XtSetArg(args[n], XmNbackground, BlackPixel(dgx,screen)); n++; 
        XtSetArg(args[n], XmNforeground, WhitePixel(dgx,screen)); n++;
*/
        pop_ddd = XtCreateWidget("Window", 
			       xmDrawingAreaWidgetClass, pop_app, 
			       args, n);
        XtManageChild(pop_ddd);  
        pop_top = XtWindow(pop_app);
        pop_win = XtWindow(pop_ddd);

/*        printf("pop_win = %d\n",pop_win); */
          
        /* Calculate starting position of string in window */

        start_x = 5;
        start_y = 20;

        XSelectInput(dgx,pop_win,ExposureMask | KeyPressMask | StructureNotifyMask | ButtonPressMask ); 

        i_loop = 0;
/*
        printf("Expose = %d\n",Expose);
        printf("ButtonPress type = %d\n",ButtonPress);
        printf("KeyPress = %d\n",KeyPress);
        printf("DestroyNotify = %d\n",DestroyNotify);
        printf("UnmapNotify = %d\n",UnmapNotify);
*/
        while(i_loop == 0) {
          XNextEvent(dgx,&report);
/*          printf("report type = %d\n",report.type); */
          switch (report.type) {
            case Expose:
              if (report.xexpose.window == pop_win) { 
                XDrawString(dgx,pop_win,gc,start_x,start_y   ,a_lbl,strlen(a_lbl));
                XDrawString(dgx,pop_win,gc,start_x,start_y+15,a_rsp,strlen(a_rsp));
              }
              break;
            case ButtonPress:
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              i_event[1] = 4;
              i_event[2] = report.xbutton.button;
              i_event[3] = report.xbutton.x;
              i_event[4] = report.xbutton.y;
              i_event[5] = 0;
              i_event[6] = 0;
              break;
            case KeyPress:
              if (report.xkey.window == pop_win) { 
                count = XLookupString(&report.xkey, buffer,bufsize,&keysym,&compose); 
                if (count == 40) count=39;
                buffer[count]=0;
                if ((keysym == XK_Return) || (keysym == XK_KP_Enter) ||
                    (keysym== XK_Linefeed)) {
                  XUnmapWindow(dgx,pop_top);
                  XDestroyWindow(dgx,pop_top);
                  i_loop = 1;
                  break; }
                else if (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9)) ||
                          ((keysym >= XK_space) && (keysym <= XK_asciitilde))) {
                  if ((strlen(a_rsp) + strlen(buffer)) >= 40 ) XBell(dgx,100);
                  else strcat(a_rsp,buffer); }
                else if ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R));
                  /* Do Nothing because it's a modifier key */
                else if ((keysym >= XK_F1) && (keysym <= XK_F35)) {
                  if (buffer == NULL) printf("Unmapped function key\n");
                  else if ((strlen(a_rsp) + strlen(buffer)) >= 40) { XBell(dgx,100); }
                  else { strcat(a_rsp,buffer); } }
                else if ((keysym == XK_BackSpace) || (keysym == XK_Delete)) {
                  if ((length = strlen(a_rsp)) > 0) {
                    a_rsp[length - 1] = NULL;
                    XClearWindow(dgx,pop_win);  }
                  else {
                    XBell(dgx,100); } }
                else {
                  printf("keysym %s is not handled\n",XKeysymToString(keysym));
                  XBell(dgx,100); }

                XDrawString(dgx,pop_win,gc,start_x,start_y   ,a_lbl,strlen(a_lbl));
                XDrawString(dgx,pop_win,gc,start_x,start_y+15,a_rsp,strlen(a_rsp));
                break;
                
              }
              break;
            case DestroyNotify:
/*              printf("destroy_win = %d\n",report.xdestroywindow.window); */
              if (report.xdestroywindow.window == pop_top) {
                i_loop = 1; 
                for (j=0; j < 38; j++) {
                  a_rsp[j]=32;
                } /* enddo */
              } /* endif */
              break;
            default:
              break; /* do nothing */
          } /* end case */
          if (i_event[1] == 4 | i_event[1] == 5) {
            if (i_event[2] == 2 ) {
              if (i_event[0] > 0 ) {
                if (i_type[ i_event[0]] == 1 ) report.xbutton.button = 1; }
              else {
                if (i_type[-i_event[0]] == 4 ) report.xbutton.button = 1;
                if (i_type[-i_event[0]] == 3 ) report.xbutton.button = 1;
              } /* endif */
            } /* endif */
          } /* endif */
          XtDispatchEvent(&report);
        } /* end while */    
    
        for (j=0; j < 38; j++) {
          if (a_rsp[j] == 0 ) a_rsp[j]=32;
        } /* enddo */
}

#ifdef SGI
void get_dialog_(a_msg,a_rsp)
#endif
#ifdef HP
void get_dialog(a_msg,a_rsp)
#endif
    char a_msg[];
    char a_rsp[];
{
    XEvent report;

    int           i;
    int           j;
    int          id;
    int      i_loop;

    static Window pop_win;
	char buffer[40];
	int bufsize;
	int start_x,start_y;
	KeySym keysym;
	XComposeStatus compose;
	int count;
	unsigned int pop_width, pop_height;
	char a_lbl[40];
    int x,y;
	int length;
    int i_cnt;
    int i_event[10];

      i_event[0] = 0;
      i_event[1] = 0;
      i_event[2] = 0;
      i_event[3] = 0;
      i_event[4] = 0;
      i_event[5] = 0;
      i_event[6] = 0;

        bufsize = 40;
        count = 0;
        x = 100;
        y = 100;


        i_cnt=0;
        for (j=0; j < 39; j++) {
          a_rsp[j] = 0;
          a_lbl[j] = a_msg[j]; 
          if (a_lbl[j] != 0 && a_lbl[j] != 32 ) i_cnt = j+1;
        } /* enddo */
        if (i_cnt == 40) i_cnt = 39;
        a_lbl[i_cnt] = 0;


        pop_width = 300;
        pop_height = 75;
        pop_win = XCreateSimpleWindow(dgx, root,x,y,pop_width,pop_height, 
                     3,BlackPixel(dgx,screen),WhitePixel(dgx,screen)); 
          
        /* Calculate starting position of string in window */

        start_x = 5;
        start_y = 20;
        XSelectInput(dgx,pop_win,ExposureMask | KeyPressMask ); 

        XMapWindow(dgx,pop_win);

        i_loop = 0;
        while(i_loop == 0) {
          XNextEvent(dgx,&report);
          switch (report.type) {
            case Expose:
              if (report.xexpose.window == pop_win) { 
                XDrawString(dgx,pop_win,gc,start_x,start_y   ,a_lbl,strlen(a_lbl));
                XDrawString(dgx,pop_win,gc,start_x,start_y+15,a_rsp,strlen(a_rsp));
              }
              break;
            case ButtonPress:
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == wgx[i]) i_event[0] = i; 
              for (i=1; i<i_gx+1; i++) 
                if (report.xbutton.window == lgx[i]) i_event[0] = -i; 
              if (i_event[0] == 0) break;
              i_event[1] = 4;
              i_event[2] = report.xbutton.button;
              i_event[3] = report.xbutton.x;
              i_event[4] = report.xbutton.y;
              i_event[5] = 0;
              i_event[6] = 0;
              break;
            case KeyPress:
              if (report.xkey.window == pop_win) { 
                count = XLookupString(&report.xkey, buffer,bufsize,&keysym,&compose); 
                if (count == 40) count=39;
                buffer[count]=0;
                if ((keysym == XK_Return) || (keysym == XK_KP_Enter) ||
                    (keysym== XK_Linefeed)) {
                  XUnmapWindow(dgx,pop_win);
                  XDestroyWindow(dgx,pop_win);
                  i_loop = 1;
                  break; }
                else if (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9)) ||
                          ((keysym >= XK_space) && (keysym <= XK_asciitilde))) {
                  if ((strlen(a_rsp) + strlen(buffer)) >= 40 ) XBell(dgx,100);
                  else strcat(a_rsp,buffer); }
                else if ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R));
                  /* Do Nothing because it's a modifier key */
                else if ((keysym >= XK_F1) && (keysym <= XK_F35)) {
                  if (buffer == NULL) printf("Unmapped function key\n");
                  else if ((strlen(a_rsp) + strlen(buffer)) >= 40) { XBell(dgx,100); }
                  else { strcat(a_rsp,buffer); } }
                else if ((keysym == XK_BackSpace) || (keysym == XK_Delete)) {
                  if ((length = strlen(a_rsp)) > 0) {
                    a_rsp[length - 1] = NULL;
                    XClearWindow(dgx,pop_win);  }
                  else {
                    XBell(dgx,100); } }
                else {
                  printf("keysym %s is not handled\n",XKeysymToString(keysym));
                  XBell(dgx,100); }

                XDrawString(dgx,pop_win,gc,start_x,start_y   ,a_lbl,strlen(a_lbl));
                XDrawString(dgx,pop_win,gc,start_x,start_y+15,a_rsp,strlen(a_rsp));
                break;
                
              }
              break;
            default:
              break; /* do nothing */
          } /* end case */
          if (i_event[1] == 4 | i_event[1] == 5) {
            if (i_event[2] == 2 ) {
              if (i_event[0] > 0 ) {
                if (i_type[ i_event[0]] == 1 ) report.xbutton.button = 1; }
              else {
                if (i_type[-i_event[0]] == 4 ) report.xbutton.button = 1;
                if (i_type[-i_event[0]] == 3 ) report.xbutton.button = 1;
              } /* endif */
            } /* endif */
          } /* endif */
          XtDispatchEvent(&report);
        } /* end while */    
    
        for (j=0; j < 38; j++) {
          if (a_rsp[j] == 0 ) a_rsp[j]=32;
        } /* enddo */
}


/*******************************************/
#ifdef SGI
int init_gx_(i_gxi,i_typ,a_labl,i_wxs,i_wys,i_frx,i_fry,i_cin)
#endif
#ifdef HP
int init_gx(i_gxi,i_typ,a_labl,i_wxs,i_wys,i_frx,i_fry,i_cin)
#endif


    int  i_gxi[];
    int  i_typ[];
    int  i_wxs[];
    int  i_wys[];
    int  i_frx[];
    int  i_fry[];
    int  i_cin[];

    char a_labl[];
{ 
	Widget          toplevel;
    Widget          pane;
    XWMHints        wm_hints;
    Arg             args[15];
    int             n = 1;
    char			*ww1[2];
    char			ww2[80];
    char            a_lbl[80];
    int             i,j;
    int             ix,iy;
    int             i_tttt;
    int             i_cnt[20];
    XWindowAttributes    xwa;
    XSetWindowAttributes xswa;
            
      ww1[0] = &ww2[0];
      strcpy (ww2,"test");
    
      printf("Start.\n");

      /* Initialize the communication between the programs */
      /* initializeComm(); */

      /* Initialize the intrinsics     */

      if (i_init == 0) {
        i_init = 1;
        i_clrs = i_cin[0];
        if (i_clrs <   0) i_clrs = 0;
        if (i_clrs > 256) i_clrs = 256;
        printf("Initializing X toolkit\n");
        i_cnt[0]=0;
        for (j=0; j < 78; j++) {
          a_lbl[j] = a_labl[(j)]; 
          if (a_lbl[j] != 0 && a_lbl[j] != 32 ) i_cnt[0] = j+1;
        } /* enddo */
        a_lbl[i_cnt[0]] = 0;
        if (i_cnt[0] == 0) strcpy(a_lbl,"GraphX");
        toplevel  = XtInitialize("Test", a_lbl, NULL, 0, &n, ww1);
        dgx       = XtDisplay(toplevel); 
        root      = XDefaultRootWindow(dgx);
      } /* endif */

      /* printf("dgx = %d \n",dgx);  */
      /* printf("root = %d\n",root);  */
        
      if (i_gxi[0] > 40 - i_gx) i_gxi[0] = 40 - i_gx;
      if (i_wxs[0] < 1 ) i_wxs[0] = 500;
      if (i_wys[0] < 1 ) i_wys[0] = 400;
      for (i=1; i < i_gxi[0]+1; i++) {
        if (i_wxs[i] < 1) i_wxs[i] = i_wxs[0];
        if (i_wys[i] < 1) i_wys[i] = i_wys[0];
      }
    
      i_cnt[0]=0;
      for (j=0; j < 78; j++) {
        a_lbl[j] = a_labl[(i*80+j)]; 
        if (a_lbl[j] != 0 && a_lbl[j] != 32 ) i_cnt[0] = j+1;
      } /* enddo */
      a_lbl[i_cnt[0]] = 0;
      if (i_cnt[0] == 0) strcpy(a_lbl,"GraphX");

      printf("creating shell\n");

      n = 0;
      XtSetArg(args[n], XmNtitle,         "SHELL"); n++;
      XtSetArg(args[n], XmNx,                   0); n++;
      XtSetArg(args[n], XmNy,                   0); n++;
      XtSetArg(args[n], XmNwidth,        i_wxs[0]); n++;
      XtSetArg(args[n], XmNheight,       i_wys[0]); n++;
      /*  XtSetArg(args[n], XmNbackground,     palette[bg]); n++;  */
      /*  XtSetArg(args[n], XmNforeground,     palette[fg]); n++; */

      i_app = i_app+1;
      toplevel = XtAppCreateShell(a_lbl,"appClass",
         	topLevelShellWidgetClass,dgx,
	     	args, n);

      n = 0;
      XtSetArg(args[n], XmNsashWidth,  6); n++;
      XtSetArg(args[n], XmNsashHeight, 6); n++;
      pane  = XtCreateManagedWidget("pane",
	 		xmPanedWindowWidgetClass, toplevel,
			args, n);
        
      ix = 0;
      /* printf("i = %d \n",i); */
      /* printf("i_gxi = %d \n",i_gxi[0]); */
      for (i=1; i < i_gxi[0]+1; i++) {
        /* printf("loop. %d %d %d \n",i,ix,i_frx[i]); */

        if (ix == 0) {
          n = 0;
          XtSetArg(args[n], XmNborderWidth,         0); n++;
          XtSetArg(args[n], XmNfractionBase, i_frx[0]); n++;
          XtSetArg(args[n], XmNhorizontalSpacing,   0); n++;
          XtSetArg(args[n], XmNverticalSpacing,     0); n++;
          if (i_fry[i] < 0) {
            XtSetArg(args[n], XmNpaneMinimum, -i_fry[i]); n++;
            XtSetArg(args[n], XmNpaneMaximum, -i_fry[i]); n++;}
          else {
            if (i_fry[i] != 0) XtSetArg(args[n], XmNheight,       i_fry[i]); n++;
            XtSetArg(args[n], XmNpaneMinimum,        30); n++;
          } /* endif */
          formy = XtCreateManagedWidget("form", 
	   		    xmFormWidgetClass, pane, 
	   		    args, n);
        } /* endif */

        if (ix+i_frx[i] > i_frx[0]) i_frx[i] = i_frx[0]-ix;

        n = 0;
        XtSetArg(args[n], XmNborderWidth,                      0); n++;
        XtSetArg(args[n], XmNfractionBase,                     1); n++;
        XtSetArg(args[n], XmNbottomAttachment,     XmATTACH_FORM); n++;
        XtSetArg(args[n], XmNtopAttachment,        XmATTACH_FORM); n++;
        XtSetArg(args[n], XmNleftAttachment,   XmATTACH_POSITION); n++;
        XtSetArg(args[n], XmNleftPosition,                    ix); n++;
        XtSetArg(args[n], XmNrightAttachment,  XmATTACH_POSITION); n++;
        XtSetArg(args[n], XmNrightPosition,          ix+i_frx[i]); n++;
        XtSetArg(args[n], XmNhorizontalSpacing,                0); n++;
        XtSetArg(args[n], XmNverticalSpacing,                  0); n++;
        form[i_gx+i] = XtCreateWidget("subform", 
			 xmFormWidgetClass, formy, 
			 args, n); 

        ix = ix+i_frx[i];
        if (ix == i_frx[0]) ix = 0;

        i_cnt[i] = 0;
        for (j=0; j < 78; j++) {
          a_lbl[j] = a_labl[(i*80+j)]; 
          if (a_lbl[j] != 0 && a_lbl[j] != 32 ) i_cnt[i] = j+1;
        } /* enddo */
        a_lbl[i_cnt[i]] = 0;

        /* printf("i_cnt = %d %d \n",i_cnt[i],i); */

        /* printf("i_typ = %d \n",i_typ[i]); */
        if (i_typ[i] == 5) i_typ[i] = -4;  /* to be backward compatible with graphx14 */
        i_tttt = i_typ[i];
        if (i_tttt < 0) i_tttt = -i_tttt;
        i_type[i_gx+i]=i_tttt;
        switch (i_tttt) {
          case 0:
            draw[i_gx+1] = form[i_gx+1];

          case 1:
            if (i_cnt[i] > 0) {
              n = 0;
              XtSetArg(args[n], XmNtopAttachment,        XmATTACH_FORM); n++;
              XtSetArg(args[n], XmNbottomAttachment,     XmATTACH_FORM); n++;
              XtSetArg(args[n], XmNleftAttachment,       XmATTACH_FORM); n++;
              XtSetArg(args[n], XmNrightAttachment,      XmATTACH_FORM); n++;
              XtSetArg(args[n], XmNwidth,                     i_wxs[i]); n++;
              XtSetArg(args[n], XmNheight,                    i_wys[i]); n++;

              draw[i_gx+i] = XtCreateManagedWidget(a_lbl,
	  			 xmPushButtonWidgetClass, form[i_gx+i],
		  		 args, n);

              /* printf("doing addcallbacks\n"); */
              if (i_gx+i == 1) XtAddCallback(draw[1], XmNactivateCallback, (XtCallbackProc) Button_1,&disparity);
              if (i_gx+i == 2) XtAddCallback(draw[2], XmNactivateCallback, (XtCallbackProc) Button_2,&disparity);
              if (i_gx+i == 3) XtAddCallback(draw[3], XmNactivateCallback, (XtCallbackProc) Button_3,&disparity);
              if (i_gx+i == 4) XtAddCallback(draw[4], XmNactivateCallback, (XtCallbackProc) Button_4,&disparity);
              if (i_gx+i == 5) XtAddCallback(draw[5], XmNactivateCallback, (XtCallbackProc) Button_5,&disparity);
              if (i_gx+i == 6) XtAddCallback(draw[6], XmNactivateCallback, (XtCallbackProc) Button_6,&disparity);
              if (i_gx+i == 7) XtAddCallback(draw[7], XmNactivateCallback, (XtCallbackProc) Button_7,&disparity);
              if (i_gx+i == 8) XtAddCallback(draw[8], XmNactivateCallback, (XtCallbackProc) Button_8,&disparity);
              if (i_gx+i == 9) XtAddCallback(draw[9], XmNactivateCallback, (XtCallbackProc) Button_9,&disparity); 
              if (i_gx+i == 10) XtAddCallback(draw[10], XmNactivateCallback, (XtCallbackProc) Button_10,&disparity);
              if (i_gx+i == 11) XtAddCallback(draw[11], XmNactivateCallback, (XtCallbackProc) Button_11,&disparity);
              if (i_gx+i == 12) XtAddCallback(draw[12], XmNactivateCallback, (XtCallbackProc) Button_12,&disparity);
              if (i_gx+i == 13) XtAddCallback(draw[13], XmNactivateCallback, (XtCallbackProc) Button_13,&disparity);
              if (i_gx+i == 14) XtAddCallback(draw[14], XmNactivateCallback, (XtCallbackProc) Button_14,&disparity);
              if (i_gx+i == 15) XtAddCallback(draw[15], XmNactivateCallback, (XtCallbackProc) Button_15,&disparity);
              if (i_gx+i == 16) XtAddCallback(draw[16], XmNactivateCallback, (XtCallbackProc) Button_16,&disparity);
              if (i_gx+i == 17) XtAddCallback(draw[17], XmNactivateCallback, (XtCallbackProc) Button_17,&disparity);
              if (i_gx+i == 18) XtAddCallback(draw[18], XmNactivateCallback, (XtCallbackProc) Button_18,&disparity);
              if (i_gx+i == 19) XtAddCallback(draw[19], XmNactivateCallback, (XtCallbackProc) Button_19,&disparity);
              if (i_gx+i == 20) XtAddCallback(draw[20], XmNactivateCallback, (XtCallbackProc) Button_20,&disparity); 
              if (i_gx+i == 21) XtAddCallback(draw[21], XmNactivateCallback, (XtCallbackProc) Button_21,&disparity);
              if (i_gx+i == 22) XtAddCallback(draw[22], XmNactivateCallback, (XtCallbackProc) Button_22,&disparity);
              if (i_gx+i == 23) XtAddCallback(draw[23], XmNactivateCallback, (XtCallbackProc) Button_23,&disparity);
              if (i_gx+i == 24) XtAddCallback(draw[24], XmNactivateCallback, (XtCallbackProc) Button_24,&disparity);
              if (i_gx+i == 25) XtAddCallback(draw[25], XmNactivateCallback, (XtCallbackProc) Button_25,&disparity);
              if (i_gx+i == 26) XtAddCallback(draw[26], XmNactivateCallback, (XtCallbackProc) Button_26,&disparity);
              if (i_gx+i == 27) XtAddCallback(draw[27], XmNactivateCallback, (XtCallbackProc) Button_27,&disparity);
              if (i_gx+i == 28) XtAddCallback(draw[28], XmNactivateCallback, (XtCallbackProc) Button_28,&disparity);
              if (i_gx+i == 29) XtAddCallback(draw[29], XmNactivateCallback, (XtCallbackProc) Button_29,&disparity);
              if (i_gx+i == 30) XtAddCallback(draw[30], XmNactivateCallback, (XtCallbackProc) Button_30,&disparity); 
              if (i_gx+i == 31) XtAddCallback(draw[31], XmNactivateCallback, (XtCallbackProc) Button_31,&disparity);
              if (i_gx+i == 32) XtAddCallback(draw[32], XmNactivateCallback, (XtCallbackProc) Button_32,&disparity);
              if (i_gx+i == 33) XtAddCallback(draw[33], XmNactivateCallback, (XtCallbackProc) Button_33,&disparity);
              if (i_gx+i == 34) XtAddCallback(draw[34], XmNactivateCallback, (XtCallbackProc) Button_34,&disparity);
              if (i_gx+i == 35) XtAddCallback(draw[35], XmNactivateCallback, (XtCallbackProc) Button_35,&disparity);
              if (i_gx+i == 36) XtAddCallback(draw[36], XmNactivateCallback, (XtCallbackProc) Button_36,&disparity);
              if (i_gx+i == 37) XtAddCallback(draw[37], XmNactivateCallback, (XtCallbackProc) Button_37,&disparity);
              if (i_gx+i == 38) XtAddCallback(draw[38], XmNactivateCallback, (XtCallbackProc) Button_38,&disparity);
              if (i_gx+i == 39) XtAddCallback(draw[39], XmNactivateCallback, (XtCallbackProc) Button_39,&disparity);
              if (i_gx+i == 40) XtAddCallback(draw[30], XmNactivateCallback, (XtCallbackProc) Button_40,&disparity); }
            else {
              draw[i_gx+i] = form[i_gx+i];
            } /* endif */

            XtManageChild(form[i_gx+i]);  
            XtManageChild(draw[i_gx+i]);

            break;

          case 2:
            n = 0;
            XtSetArg(args[n], XmNborderWidth,                     0); n++;
            XtSetArg(args[n], XmNrightAttachment,     XmATTACH_FORM); n++;
            XtSetArg(args[n], XmNleftAttachment,      XmATTACH_FORM); n++;  
            XtSetArg(args[n], XmNheight,                   i_wys[i]); n++;
            draw[i_gx+i] = XtCreateWidget("                                                  ",
	    		  xmLabelWidgetClass, form[i_gx+i],
	    		  args, n);

            XtManageChild(form[i_gx+i]);  
            XtManageChild(draw[i_gx+i]);

            break;

          case 3:
            n = 0;
            XtSetArg(args[n], XmNborderWidth,                     0); n++;
            XtSetArg(args[n], XmNrightAttachment,     XmATTACH_FORM); n++;
            XtSetArg(args[n], XmNleftAttachment,      XmATTACH_FORM); n++;  
            labl[i_gx+i] = XtCreateWidget("                                                  ",
	    		  xmLabelWidgetClass, form[i_gx+i],
	    		  args, n);
    
            n = 0;
            XtSetArg(args[n], XmNtopAttachment,     XmATTACH_WIDGET); n++;
            XtSetArg(args[n], XmNtopWidget,            labl[i_gx+i]); n++;
            XtSetArg(args[n], XmNbottomAttachment,    XmATTACH_FORM); n++; 
            XtSetArg(args[n], XmNleftAttachment,      XmATTACH_FORM); n++; 
            XtSetArg(args[n], XmNrightAttachment,     XmATTACH_FORM); n++;
            XtSetArg(args[n], XmNborderWidth,                     1); n++;
            XtSetArg(args[n], XmNwidth,                    i_wxs[i]); n++;
            XtSetArg(args[n], XmNheight,                   i_wys[i]); n++;
            draw[i_gx+i] = XtCreateWidget("draw", 
			       xmDrawingAreaWidgetClass, form[i_gx+i], 
			       args, n);

            XtManageChild(form[i_gx+i]);  
            XtManageChild(draw[i_gx+i]);
            XtManageChild(labl[i_gx+i]);

            break;

          case 4:
            n = 0;
            XtSetArg(args[n], XmNborderWidth,                     0); n++;
            XtSetArg(args[n], XmNrightAttachment,     XmATTACH_FORM); n++;
            XtSetArg(args[n], XmNleftAttachment,      XmATTACH_FORM); n++;  
            labl[i_gx+i] = XtCreateWidget("                                                  ",
	    		  xmLabelWidgetClass, form[i_gx+i],
	    		  args, n);
    
            n = 0;
            XtSetArg(args[n], XmNscrollingPolicy,              XmAUTOMATIC); n++;
            XtSetArg(args[n], XmNscrollBarDisplayPolicy,       XmAS_NEEDED); n++;
            XtSetArg(args[n], XmNbottomAttachment,           XmATTACH_FORM); n++;
            XtSetArg(args[n], XmNtopAttachment,            XmATTACH_WIDGET); n++;
            XtSetArg(args[n], XmNtopWidget,                   labl[i_gx+i]); n++;
            XtSetArg(args[n], XmNrightAttachment,        XmATTACH_POSITION); n++;
            XtSetArg(args[n], XmNrightPosition,                          1); n++;
            XtSetArg(args[n], XmNleftAttachment,             XmATTACH_FORM); n++;
            scrl[i_gx+i] = XtCreateWidget("scroll", 
	    		   xmScrolledWindowWidgetClass, form[i_gx+i],
	    		   args, n);
    
            n = 0;
            XtSetArg(args[n], XmNwidth,       i_wxs[i]); n++;
            XtSetArg(args[n], XmNheight,      i_wys[i]); n++;
            XtSetArg(args[n], XmNborderWidth,        1); n++;
            draw[i_gx+i] = XtCreateWidget("draw", 
			       xmDrawingAreaWidgetClass, scrl[i_gx+i], 
			       args, n);
      
            XtManageChild(form[i_gx+i]);  
            XtManageChild(draw[i_gx+i]);
            XtManageChild(labl[i_gx+i]);
            XtManageChild(scrl[i_gx+i]);

            break;
        } /* end case */

      } /* Enddo */


    XtRealizeWidget(toplevel);
    top[i_app] = XtWindow(toplevel);
    /* printf("top= %d %d\n",top[i_app],i_app); */

    for (i=1; i < i_gxi[0]+1; i++) {
      fgx[i_gx+i] = XtWindow(form[i_gx+i]);
      wgx[i_gx+i] = XtWindow(draw[i_gx+i]);	/* get the window id's for drawing */
      if (i_type[i_gx+i] == 3) lgx[i_gx+i] = XtWindow(labl[i_gx+i]);	/* get the labels id's for drawing */
      if (i_type[i_gx+i] == 4) lgx[i_gx+i] = XtWindow(labl[i_gx+i]);	/* get the labels id's for drawing */
      /* printf("fgx= %d %d\n",fgx[i_gx+i],i_gx+i);  */
      /* printf("wgx= %d %d\n",wgx[i_gx+i],i_gx+i);  */
      /* if (i_type[i_gx+i] == 4) printf("lgx= %d %d\n",lgx[i_gx+i],i_gx+i);  */
      XSelectInput(dgx,wgx[i_gx+i],ExposureMask | ButtonPressMask | ButtonReleaseMask | PointerMotionMask |
          KeyPressMask | KeyReleaseMask | StructureNotifyMask); 
      XSelectInput(dgx,fgx[i_gx+i],ExposureMask | ButtonPressMask | ButtonReleaseMask | PointerMotionMask |
          KeyPressMask | KeyReleaseMask | StructureNotifyMask); 
      if (i_typ[i] < 0) {
        /* XGetWindowAttributes(dgx,wgx[i_gx+i],xwa); */
        xswa.backing_store=Always;
        XChangeWindowAttributes(dgx,wgx[i_gx+i],CWBackingStore,&xswa);
      } /* Endif */
    } /* Enddo */


    /* Create our own icon */
    icon = XCreateBitmapFromData(dgx, top[i_app], icon_bits,
			       icon_width, icon_height);
    wm_hints.icon_pixmap = icon;
    wm_hints.flags = IconPixmapHint;
    
    i_cnt[0]=0;
    for (j=0; j < 78; j++) {
      a_lbl[j] = a_labl[(j)]; 
      if (a_lbl[j] != 0 && a_lbl[j] != 32 ) i_cnt[0] = j+1;
    } /* enddo */
    a_lbl[i_cnt[0]] = 0;
    if (i_cnt[0] == 0) strcpy(a_lbl,"GraphX");
    XSetStandardProperties(dgx, top[i_app], a_lbl, a_lbl, 
			 icon, ww1, 1, NULL);

    screen  	= DefaultScreen(dgx);    
    gc	        = DefaultGC(dgx, screen);

/*    if (i_gx == 0) { */
    if (0 == 0) {
       /* printf("calling alo_colors\n"); */
      alo_colors();
      allocated=1; 
      if (i_ctble < 16) {
        printf("Not enough i_ctble colors  -  Program stopping  %d\n",i_ctble);
        exit(0);
      } /* endif */ 

      printf("Allocating %d colors\n",i_ctble);

      /* printf("calling grey_8bita\n"); */
      if (i_ctype == 0) { grey_8bita(); }
      if (i_ctype == 1) { grey_8bita(); }
      if (i_ctype == 2) { grey_4bita(); }
      if (i_ctype == 3) { grey_4bitb(); }
      if (i_ctype == 4) { colr_4bita(); }
      if (i_ctype == 5) { colr_4bitb(); }
      if (i_ctype == 6) { mixr_8bita(); }
      if (i_ctype == 7) { grey_8bita(); }
      if (i_ctype == 8) { grey_8bitb(); }
      if (i_ctype == 9) { colr_8bita(); }

      /* printf("calling put_colors\n"); */
      put_colors();
      /* printf("calling fre_colors\n"); */
      fre_colors();
      allocated=0;    
    } /* endif */

    i_gx = i_gx + i_gxi[0];
    i_gxi[0] = i_gx;
    printf("Graphx initialization complete\n");
    return(i_gx);

}

static void initializeComm()
{
    /* We've got to have a display if we are to send a property to the
    * root window. Initialize the toolkit, but do not use widgets in
    * any other way.
    */
    
    if ((dgx = XOpenDisplay(NULL)) == NULL) {
	fprintf(stderr, "GraphX: Couldn't open display %s\n",
	      XDisplayName(NULL));
	exit(2);
    }	      
}

void free_graphics()
{
    read_events();


  XFlush(dgx);
  XCloseDisplay(dgx);
}

static void Button_1(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=1;
}

static void Button_2(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=2;
}

static void Button_3(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=3;
}

static void Button_4(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=4;
}

static void Button_5(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=5;
}

static void Button_6(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=6;
}

static void Button_7(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=7;
}

static void Button_8(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=8;
}

static void Button_9(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=9;
}

static void Button_10(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=10;
}

static void Button_11(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=11;
}

static void Button_12(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=12;
}

static void Button_13(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=13;
}

static void Button_14(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=14;
}

static void Button_15(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=15;
}

static void Button_16(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=16;
}

static void Button_17(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=17;
}

static void Button_18(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=18;
}

static void Button_19(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=19;
}

static void Button_20(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=20;
}

static void Button_21(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=21;
}

static void Button_22(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=22;
}

static void Button_23(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=23;
}

static void Button_24(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=24;
}

static void Button_25(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=25;
}

static void Button_26(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=26;
}

static void Button_27(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=27;
}

static void Button_28(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=28;
}

static void Button_29(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=29;
}

static void Button_30(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=30;
}

static void Button_31(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=31;
}

static void Button_32(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=32;
}

static void Button_33(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=33;
}

static void Button_34(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=34;
}

static void Button_35(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=35;
}

static void Button_36(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=36;
}

static void Button_37(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=37;
}

static void Button_38(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=38;
}

static void Button_39(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=39;
}

static void Button_40(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    i_push=40;
}

static void Button_quit(w, free, data)
    Widget w;
    Pixmap free;
    XmAnyCallbackStruct *data;
{
    /* Quit Graphsub */
    XFreePixmap(XtDisplay(w), free); 
    exit(0);
}

static void grey_8bita()
{
    int i;
    
    for (i = 0 ; i < i_ctble ; i++) 
	   red [i] = green [i] = blue [i] = (255*i)/(i_ctble-1);  
    red[i_ctble-1]   =   0;
    green[i_ctble-1] =   0;
    blue[i_ctble-1]  = 255;
}

static void grey_8bitb()
{
    int i;
    
    for (i = 0 ; i < i_ctble ; i++) 
	   red [i] = green [i] = blue [i] = (255*(i_ctble-1-i))/(i_ctble-1);  
    red[i_ctble-1]   =   0;
    green[i_ctble-1] =   0;
    blue[i_ctble-1]  = 255;
}

static void grey_4bita()
{
    int i,j;
    int i_magn;
    int i_colr;
    
    i_magn = (int)sqrt((float)i_ctble);
    i_colr = i_magn;

    for (i = 0 ; i < i_magn ; i++) {
       for (j = 0; j < i_colr ; j++) 
         red [i*i_colr+j] = green [i*i_colr+j] = blue [i*i_colr+j] = (255*i)/(i_magn-1);
    } /* enddo */
    for (i = i_magn*i_colr ; i < i_ctble ; i++) {
         red [i] = green [i] = blue [i] = 0;
    } /* enddo */
}

static void grey_4bitb()
{
    int i,j;
    int i_magn;
    int i_colr;
    
    i_magn = (int)sqrt((float)i_ctble);
    i_colr = i_magn;

    for (i = 0 ; i < i_magn ; i++) {
       for (j = 0; j < i_colr ; j++) 
         red [i*i_colr+j] = green [i*i_colr+j] = blue [i*i_colr+j] = (255*j)/(i_colr-1);
    } /* enddo */
    for (i = i_magn*i_colr ; i < i_ctble ; i++) {
         red [i] = green [i] = blue [i] = 0;
    } /* enddo */
}

static void colr_8bita()
{
    int i,j;
    int i_colr;
    int i_step;
    int i_delt;
    
    i_colr = i_ctble;

    i_step = i_colr/3+1;
    i_delt = 255/i_step;
    
       for (j = 0; j < i_step ; j++) {
         red [j]   = i_delt*j;
         green [j] = 255-i_delt*j;
         blue [j]  = 255;
       } /* enddo */
       for (j = i_step; j < 2*i_step; j++) {
         red [j]   = 255;
         green [j] = i_delt*(j-i_step);
         blue [j]  = 255 - i_delt*(j-i_step);
       } /* enddo */
       for (j = 2*i_step; j < i_colr ; j++) {
         red [j]   = 255 - i_delt*(j-2*i_step);
         green [j] = 255;
         blue [j]  = i_delt*(j-2*i_step);
       } /* enddo */
}

static void colr_4bita()
{
    int i,j;
    int i_magn;
    int i_colr;
    int i_step;
    int i_delt;
    
    i_magn = (int)sqrt((float)i_ctble);
    i_colr = i_magn;

    i_step = i_colr/3+1;
    i_delt = 255/i_step;
    
    for (i = 0 ; i < i_magn ; i++) {
       for (j = 0; j < i_step ; j++) {
         red [i*i_colr+j]   = i_delt*j;
         green [i*i_colr+j] = 255-i_delt*j;
         blue [i*i_colr+j]  = 255;
       } /* enddo */
       for (j = i_step; j < 2*i_step; j++) {
         red [i*i_colr+j]   = 255;
         green [i*i_colr+j] = i_delt*(j-i_step);
         blue [i*i_colr+j]  = 255 - i_delt*(j-i_step);
       } /* enddo */
       for (j = 2*i_step; j < i_colr ; j++) {
         red [i*i_colr+j]   = 255 - i_delt*(j-2*i_step);
         green [i*i_colr+j] = 255;
         blue [i*i_colr+j]  = i_delt*(j-2*i_step);
       } /* enddo */
    } /* enddo */
    for (i = i_magn*i_colr ; i < i_ctble ; i++) {
         red [i] =   red[0];
         green [i] = green[0];
         blue [i] =  blue[0];
    } /* enddo */
}

static void colr_4bitb()
{
    int i,j;
    int i_magn;
    int i_colr;
    int i_step;
    int i_delt;
    
    i_magn = (int)sqrt((float)i_ctble);
    i_colr = i_magn;

    i_step = i_magn/3+1;
    i_delt = 255/i_step;
    
    for (j = 0 ; j < i_colr ; j++) {
       for (i = 0; i < i_step ; i++) {
         red [i*i_colr+j]   = i_delt*i;
         green [i*i_colr+j] = 255-i_delt*i;
         blue [i*i_colr+j]  = 255;
       } /* enddo */
       for (i = i_step; i < 2*i_step; i++) {
         red [i*i_colr+j]   = 255;
         green [i*i_colr+j] = i_delt*(i-i_step);
         blue [i*i_colr+j]  = 255 - i_delt*(i-i_step);
       } /* enddo */
       for (i = 2*i_step; i < i_magn ; i++) {
         red [i*i_colr+j]   = 255 - i_delt*(i-2*i_step);
         green [i*i_colr+j] = 255;
         blue [i*i_colr+j]  = i_delt*(i-2*i_step);
       } /* enddo */
    } /* enddo */
    for (i = i_magn*i_colr ; i < i_ctble ; i++) {
         red [i] =   red[0];
         green [i] = green[0];
         blue [i] =  blue[0];
    } /* enddo */
}

static void mixr_8bita()
{
    int i,j;
    int i_magn;
    int i_colr;
    int i_step;
    int i_delt;
    float r_fact;
    
    i_magn = (int)sqrt((float)i_ctble);
    i_colr = i_magn;

    i_step = i_colr/3+1;
    i_delt = 255/i_step;
    
    for (i = 0 ; i < i_magn ; i++) {
       r_fact = (float)(i)/(float)(i_magn-1);
       for (j = 0; j < i_step ; j++) {
         red [i*i_colr+j]   = (int)(r_fact*(i_delt*j));
         green [i*i_colr+j] = (int)(r_fact*(255-i_delt*j));
         blue [i*i_colr+j]  = (int)(r_fact*(255));
       } /* enddo */
       for (j = i_step; j < 2*i_step; j++) {
         red [i*i_colr+j]   = (int)(r_fact*(255));
         green [i*i_colr+j] = (int)(r_fact*(i_delt*(j-i_step)));
         blue [i*i_colr+j]  = (int)(r_fact*(255 - i_delt*(j-i_step)));
       } /* enddo */
       for (j = 2*i_step; j < i_colr ; j++) {
         red [i*i_colr+j]   = (int)(r_fact*(255 - i_delt*(j-2*i_step)));
         green [i*i_colr+j] = (int)(r_fact*(255));
         blue [i*i_colr+j]  = (int)(r_fact*(i_delt*(j-2*i_step)));
       } /* enddo */
    } /* enddo */
}

static void alo_colors()
{
    int           signed_nentries,j;
    unsigned long masks[8];
    unsigned int  nplanes = 0, nentries;
    Colormap  default_cmap;
    XColor    default_colors[256];

    if (i_clrs == 0) {
      cmap  = XDefaultColormap(dgx, screen);
      signed_nentries = nentries = MAX_COLORS;


      while((XAllocColorCells(dgx, cmap, True, masks, nplanes, indices, 
			  nentries) == 0) && (signed_nentries >= 2)) {
  	  signed_nentries = --nentries;
      }  
    
      if (signed_nentries < 0) {
        fprintf(stderr, "Automatch: Couldn't allocate any colors\n");
        nentries = 0;
      }
    
      i_ctble = nentries; }
    else {

      i_ctble = DisplayCells(dgx,DefaultScreen(dgx));
      if (i_ctble > i_clrs) i_ctble = i_clrs;
      default_cmap  = XDefaultColormap(dgx, screen);
      for (j = 0; j < i_ctble; j++){
      	 default_colors[j].pixel = j;
	     default_colors[j].flags = DoRed|DoGreen|DoBlue;
         indices[j] = (unsigned long) j+(256-i_ctble);
      }
      XQueryColors(dgx,default_cmap,default_colors,i_ctble);
      cmap = XCreateColormap(dgx,DefaultRootWindow(dgx),
			 DefaultVisual(dgx,DefaultScreen(dgx)),AllocAll);
      XStoreColors(dgx,cmap,default_colors,i_ctble); 
    } /* endif */

}

static void fre_colors()
{
    unsigned int  nplanes = 0;

    if(i_clrs == 0) XFreeColors(dgx, cmap, indices, i_ctble, nplanes); 
    XFlush(dgx);    
}

static void put_colors()
{
    int i;
    XColor *xcolors;
    
    if ((xcolors = (XColor *) malloc ((long) 
			      (i_ctble * sizeof (XColor)))) == NULL) {
	fprintf(stderr, "Automatch: Couldn't allocate space for colors\n");
	return;
    }
    
    for (i = 0; i < i_ctble; i++) {
	xcolors[i].red   = red [i] * 256;
	xcolors[i].green = green [i] * 256;
	xcolors[i].blue  = blue [i] * 256;
	xcolors[i].flags = DoRed | DoGreen | DoBlue;
	xcolors[i].pixel = indices [i];
    }

    XStoreColors(dgx, cmap, xcolors, i_ctble);
    for (i = 1; i < i_app+1; i++) {
	  if (top[i] != 0) XSetWindowColormap(dgx,top[i],cmap);
    }
    /* free(xcolors); */
}

void read_events()
{
    XFlush(dgx);
    while(XPending(dgx)) {
	XtNextEvent(&event);
	XtDispatchEvent(&event);
    }
}



/*        io routines         */

/* SccsId[ ]= @(#)io.c	1.1 2/5/92 */
#include <stdio.h> 
#include <fcntl.h>

#define PERMS 0666
/* IO library:
 * done by quyen dinh nguyen
 * 11/12/91:
 */

/* To open a file and assign a channel to it. This must be
   done before any attempt is made to access the file. The 
   return value (initdk) is the file descriptor. The file can
   be closed with the closedk subroutine.
   
   Remember, always open files before you need to access to them
   and close them after you don't need them any more. In UNIX,
   there is a limit (20) of number files can be opened at once.

   Note that, if the file is not existing, the routine will create
   the new file  with PERM=0666.

   Calling sequence(from FORTRAN):
         fd = initdk(lun,filename)
   where:
         fd is the long int for file descriptor.

         lun is the dummy variable to be compatible with VMS calls.

         filename is the name of the file. Include directory paths 
         if necessary.
 */
#ifdef SGI
int initdk_(lun, filename)
#endif
#ifdef HP
int initdk(lun, filename)
#endif

int *lun; char *filename;
{  int i;
   int fd;
   for(i=0; i < strlen(filename); i++)
     if( *(filename+i) == ' ') *(filename+i) = '\0' ;
   if((fd=open(filename,O_RDWR)) < 0){
       if( (fd = open(filename,O_RDONLY)) > 0)
           printf(" Open filename %s as READ ONLY\n",filename);
   }
   if( fd < 0 ) fd = open(filename,O_CREAT|O_RDWR,0666);
   if(fd == -1)printf(" Cannot open the filename: %s\n",filename);
   return(fd);
}

/* To write data into a previous opened file. This routine
   will wait until the write operations are completed.
  
   Calling sequence (from FORTRAN):
         nbytes = iowrit( chan, buff, bytes)
	 call iowrit(chan,buff,bytes)
   where:
         nbytes is the number bytes that transfered.
   
         chan is the file descriptor.

         buff is the buffer or array containing the data you
         wish to write.

         bytes is the number of bytes you wish to write.
*/ 
#ifdef SGI
int iowrit_(chan, buff, bytes)
#endif
#ifdef HP
int iowrit(chan, buff, bytes)
#endif

int *chan, *bytes;
char *buff;
{  
   int nbytes;
   nbytes = write(*chan, buff, *bytes);
   if(nbytes != *bytes) fprintf(stderr,
       " ** ERROR **: only %d bytes transfered out of %d bytes\n",
       nbytes, *bytes);
   return(nbytes);
}

/* To read data from a previously opened file. This routine will
   wait until after its operations are completed.

   Calling sequence (from FORTRAN):
       nbytes = ioread( chan, buff, bytes)
       call ioread( chan, buff, bytes)
   where:
       nbytes is the number bytes that transfered.
  
       chan is the file descriptor.
 
       buff is the buffer or array containning the data you wish
       to read.

       bytes is the number of bytes you wish to read.

 */
#ifdef SGI
int ioread_(chan, buff, bytes)
#endif
#ifdef HP
int ioread(chan, buff, bytes)
#endif

int *chan, *bytes ;
char *buff;
{  
   int nbytes;
   nbytes = read(*chan, buff, *bytes);
/*   if(nbytes != *bytes) fprintf(stderr,
     " ** ERROR **: only %d bytes are read out of %d requested\n",
     nbytes, *bytes); */
   return(nbytes);
}


/* To position the file pointer. This routine will call the lseek 
   to update the file pointer.

   Calling sequence (from FORTRAN):
      file_loc = ioseek(chan,loc_byte)
      call ioseek(chan,loc_byte)
   where:
        file_loc is the returned file location.

        chan is the file descriptor.

        loc_byte is byte location that requested to be set. This value
        must be greater or equal to zero for positioning the file at
        that location. If loc_byte is negative, the file pointer will
        move abs(loc_byte) from the current location.
        
*/

#ifdef SGI
int ioseek_(chan, loc_byte)
#endif
#ifdef HP
int ioseek(chan, loc_byte)
#endif

int *chan, *loc_byte;
{  
   int ibytes,nloc;
   ibytes = *loc_byte ;
   if(ibytes >= 0) nloc = lseek(*chan, ibytes, 0);
   else {
      ibytes = - ibytes;
      nloc = lseek(*chan, ibytes, 1);
   }
   return(nloc);
}



/* To close the file previously opened by initdk.

   Calling sequence (from FORTRAN):
      istatus = closedk( lun, chan)
      call closedk( lun, chan)
   where:
      istatus is the return value (0 is success, -1 is error)
 
      lun is the dummy variable to be compatible the VAX VMS call.

      chan is the file descriptor that you want to close.
 */

#ifdef SGI
int closedk_(lun,chan)
#endif
#ifdef HP
int closedk(lun,chan)
#endif

int *lun, *chan;
{
   return(close(*chan));
}


#ifdef SGI

/*        io64 routines         */

/* SccsId[ ]= @(#)io.c	1.1 2/5/92 */
#include <stdio.h> 
#include <fcntl.h>

#define PERMS 0666
/* IO library:
 * done by quyen dinh nguyen
 * 11/12/91:
 */

/* To open a file and assign a channel to it. This must be
   done before any attempt is made to access the file. The 
   return value (initdk) is the file descriptor. The file can
   be closed with the closedk subroutine.
   
   Remember, always open files before you need to access to them
   and close them after you don't need them any more. In UNIX,
   there is a limit (20) of number files can be opened at once.

   Note that, if the file is not existing, the routine will create
   the new file  with PERM=0666.

   Calling sequence(from FORTRAN):
         fd = initdk(lun,filename)
   where:
         fd is the long int for file descriptor.

         lun is the dummy variable to be compatible with VMS calls.

         filename is the name of the file. Include directory paths 
         if necessary.

   Modified ioseek for very large offsets using fseek64 instead of fseek,
   which takes and returns 8-byte integers instead of 4-byte integers,
   requiring mods to the calling programs  (jmm, 5/22/96)
 */
int initdk64_(lun, filename)
int *lun; char *filename;
{  int i;
   int fd;
   for(i=0; i < strlen(filename); i++)
     if( *(filename+i) == ' ') *(filename+i) = '\0' ;
   if((fd=open(filename,O_RDWR)) < 0){
       if( (fd = open(filename,O_RDONLY)) > 0)
           fprintf(stderr," Open filename %s as READ ONLY\n",filename);
   }
   if( fd < 0 ) fd = open(filename,O_CREAT|O_RDWR,0666);
   if(fd == -1)fprintf(stderr," Cannot open the filename: %s\n",filename);
   return(fd);
}

/* To write data into a previous opened file. This routine
   will wait until the write operations are completed.
  
   Calling sequence (from FORTRAN):
         nbytes = iowrit( chan, buff, bytes)
	 call iowrit(chan,buff,bytes)
   where:
         nbytes is the number bytes that transfered.
   
         chan is the file descriptor.

         buff is the buffer or array containing the data you
         wish to write.

         bytes is the number of bytes you wish to write.
*/ 
int iowrit64_(chan, buff, bytes)
int *chan, *bytes;
char *buff;
{  
   int nbytes;
   nbytes = write(*chan, buff, *bytes);
   if(nbytes != *bytes) fprintf(stderr,
       " ** ERROR **: only %d bytes transfered out of %d bytes\n",
       nbytes, *bytes);
   return(nbytes);
}

/* To read data from a previously opened file. This routine will
   wait until after its operations are completed.

   Calling sequence (from FORTRAN):
       nbytes = ioread( chan, buff, bytes)
       call ioread( chan, buff, bytes)
   where:
       nbytes is the number bytes that transfered.
  
       chan is the file descriptor.
 
       buff is the buffer or array containning the data you wish
       to read.

       bytes is the number of bytes you wish to read.

 */
int ioread64_(chan, buff, bytes)
int *chan, *bytes ;
char *buff;
{  
   int nbytes;
   nbytes = read(*chan, buff, *bytes);
/*    if(nbytes != *bytes) fprintf(stderr,
     " ** ERROR **: only %d bytes are read out of %d requested\n",
     nbytes, *bytes); */
   return(nbytes);
}


/* To position the file pointer. This routine will call the lseek 
   to update the file pointer.

   Calling sequence (from FORTRAN):
      file_loc = ioseek(chan,loc_byte)
      call ioseek(chan,loc_byte)
   where:
        file_loc is the returned file location.

        chan is the file descriptor.

        loc_byte is byte location that requested to be set. This value
        must be greater or equal to zero for positioning the file at
        that location. If loc_byte is negative, the file pointer will
        move abs(loc_byte) from the current location.
        
*/

off64_t ioseek64_(chan, loc_byte)
int *chan;
off64_t *loc_byte;
{  
   off64_t ibytes,nloc;
   ibytes = *loc_byte ;
   if(ibytes >= 0) nloc = lseek64(*chan, ibytes, 0);
   else {
      ibytes = - ibytes;
      nloc = lseek(*chan, ibytes, 1);
   }
   return(nloc);
}



/* To close the file previously opened by initdk.

   Calling sequence (from FORTRAN):
      istatus = closedk( lun, chan)
      call closedk( lun, chan)
   where:
      istatus is the return value (0 is success, -1 is error)
 
      lun is the dummy variable to be compatible the VAX VMS call.

      chan is the file descriptor that you want to close.
 */

int closedk64_(lun,chan)
int *lun, *chan;
{
   return(close(*chan));
}


#endif
