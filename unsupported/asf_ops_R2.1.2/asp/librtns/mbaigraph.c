/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaigraph.c -- graphics routines for ibob

	This set of routines comprises the graphics support for
	the IBOB routine.  All calls to the graphics display
	device are located here, to facilitate transportability.
*/


#include <stdio.h>
#include <ctype.h>
#include <aspdecl.h>
#include <ibobdefs.h>


extern max_cname,max_bname;		/* max size of cage, board names */

extern char system_name[50];		/* name of hardware system */

extern CAGEPTR cage_list[20];
extern int ncages;			/* number of cages defined */

extern WATCHPTR watch_list;		/* watch object list */
extern int watch_sw;			/* 1 = selecting watch object */

int cur_page = 1;			/* currently displayed page */
BOARDPTR selected_bp = NULL;		/* currently selected board */

typedef enum {regs, fields} smode;
extern smode screen_mode;		/* current screen mode */

extern int lowcase_sw;			/* 1 = lower case input is OK */

extern char regs_file[50];		/* register load file name */
extern int regs_modified;		/* 1 = modified since loaded */
typedef enum {previous, init, loadfile} rsource;
extern rsource regs_source;		/* register contents source */
static char *rs_name[3] = { "previous data",
			    "initialization values",
			    regs_file };

/*   graphics data areas -------------------------------------------------*/

extern  int minpix,minlin,maxpix,maxlin; /* screen boundary */
extern	int screen_npix,screen_nlin;	/* screen size in pixels */
extern	int plane_mask;			/* bit mask for graphics planes */

	int dxmin,dxmax,dymin,dymax;	/* display area boundary */
	int dxmid,dymid;		/* center of display area */
	int txmin,txmax,tymin,tymax;	/* menu display area boundary */
	int tx_wordsize;		/* size of one menu word */
	int cagex,presx,boardx,testx,regx; /* start character locations */
	int gcolm,grow;			/* cursor location in chars */
	int regs_per_line;		/* # of displayable regs per line */
	int bregs_per_line;		/* * of byte regs per line */
	int column_size = 28;		/* size of one field column */

	int black,red,green,yellow;	/* color names */
	int blue,magenta,cyan,gray,white,orange;
	int real_black = 0;		/* color table entry 0 */

	int lcol,ccol,bcol,rcol,fcol;	/* color definitions */
	int hcol,mcol,mbgcol,curscol,bypcol;
	int tcol,pcol,bgcol,memcol,hicol;
	int wcol,wbgcol;
	int cur_col;			/* currently set color */

	int cx,cy;			/* cursor location */
	int button_holder = 0;		/* mouse button flags */

	int bigfont = 3;		/* font # of large size font */
	int line_spacing;		/* spacing between lines */
	int char_width,char_height;	/* character size */
	int columns,rows;		/* screen size in characters */
	int row_extra;			/* remaining odd screen lines */

	int gb_pt = 0;			/* group box switch 1=working */
	int gb_x,gb_y;			/* first group box point */

/**********************************************************************/

/*  init_ibob_graphics -------------------------------------------------
	This routine initializes the graphics system, sets up the screen
	area sizes.
*/
init_ibob_graphics()
{
	static int c_shape[8] = {
		0x8000c000,0xe000f000,
		0xf800fc00,0xfe00f800,
		0xf8009800,0x0c000c00,
		0x06000600,0x03000300
	};
	static int first = 1;
	int button_handler();
	int x1,y1,x2,y2;

	if (init_graphics() == 0) return (0);
	clear_plane(0xfff);	/* clear the screen (including text) */
	set_text_vis(0);	/* make text invisible */
    /* set up text and fonts */
/*
	mgipln(plane_mask);
	if (first) mgifetchgf(bigfont,"9x11_b.gpf");
*/
	set_font(bigfont);
    /* calculate screen area sizes */
	columns = screen_npix / char_width;
	rows = screen_nlin / line_spacing;
	row_extra = screen_nlin % line_spacing;
	dxmin = minpix; 	/* data display area */
	dymin = minlin + line_spacing + row_extra;
	dxmax = maxpix;
	dymax = maxlin;
	dxmid = (dxmin + dxmax) / 2;
	dymid = (dymin + dymax) / 2;
	txmin = dxmin;		/* menu display area */
	tymin = minlin;
	txmax = dxmax;
	tymax = dymin;
	tx_wordsize = 11 * char_width;
	cagex = 1;
	presx = cagex + max_cname + 1;
	boardx = presx + 2;
	testx = boardx + max_bname + 2;
	regx = testx + 4;
	regs_per_line = (columns - regx) / 5;
	bregs_per_line = (columns - regx) / 3;
	calc_cage_locs();
    /* set up the colors to be used */
	setup_color_table();
	bgcol = real_black;	/* screen background */
	hcol = gray;		/* heading words */
	ccol = gray;		/* cage names */
	bcol = yellow;		/* board names */
	rcol = white;		/* register data */
	fcol = blue;		/* field data */
	memcol = orange;	/* memory data */
	mcol = white;		/* menu text */
	mbgcol = magenta;	/* menu background */
	hicol = magenta;	/* text highlight background */
	wcol = white;		/* watch text */
	wbgcol = red;		/* watch background */
	lcol = cyan;		/* lines */
	pcol = red;		/* board presence "lights" */
	tcol = green;		/* test bus */
	bypcol = red;		/* bypass indicators */
	curscol = white;	/* cursor */
    /* draw the initial screen display */
	draw_rack(cur_page);
	draw_menu();
    /* initialize the cursor */
/*
	mgicursmode(0);
	mgiloadcurs(0,0,curscol,c_shape);
	mgicursmode(5);
	set_curs_xy(screen_npix/2,screen_nlin/2);
	mgibuttonint(button_handler);
*/
	first = 0;
	return (1);
}

/* setup_color_table() ------------------------------------------
	This routine ensures that the color table will contain colors
	which will not blow up due to a strange Masscomp bug...
*/
setup_color_table()
{
	static int cmap[16];
	int i;
	int base = 256;

	for (i = 0; i < 16; i++)
	    cmap[i] = 0;
	black   = base + 0;
	red     = base + 1;
	green   = base + 2;
	yellow  = base + 3;
	blue    = base + 4;
	magenta = base + 5;
	cyan    = base + 6;
	gray    = base + 7;
	white   = base + 8;
	orange  = base + 9;

	cmap[black   - base] = 0x000000;
	cmap[red     - base] = 0xff0000;
	cmap[green   - base] = 0x00ff00;
	cmap[yellow  - base] = 0xffff00;
	cmap[blue    - base] = 0xaaaaff;
	cmap[magenta - base] = 0xcc00cc;
	cmap[cyan    - base] = 0x44ffff;
	cmap[gray    - base] = 0xdddddd;
	cmap[white   - base] = 0xffffff;
	cmap[orange  - base] = 0xffcc00;

    /*
	printf("\nColors:");
	for (i = 0; i < 16; i++)
	    printf(" %x",cmap[i]);
	printf("\n");
	mgicms(base,16,cmap);
    */
}

/* end_ibob_graphics() -------------------------------------------------
	This routine returns the graphics system and display to its
	normal state.
*/
end_ibob_graphics()
{
/*	mgicursmode(0);		   turn off the cursor */
	clear_plane(plane_mask);  /* clear the screen */
	set_text_vis(1);	  /* make text visible again */
}

/* set_text_vis(opt) --------------------------------------------------
	This routine sets text visible (opt=1) or invisible (opt=0).
*/
set_text_vis(opt)
int opt;
{
printf("SET_TEXT_VIS is not yet supported for R1B\n");
sleep( 2 );
/*
	mgiocm(0,0);
	mgiocm(1,opt);
	mgiocm(2,opt);
	mgiocm(3,opt);
*/
}

/* calc_cage_locs() ----------------------------------------------------
	This routine calculates the screen location at which each cage,
	board and register will be displayed.
*/
calc_cage_locs()
{
	CAGEPTR cp;
	BOARDPTR bp;
        int i,j,r;
        int l = 1;
        int p = 1;

        for (i = 0; i < ncages; i++) {
            cp = cage_list[i];
            cp->nlines = 0;
            for (j = 0; j < cp->nboards; j++) {
                bp = cp->board_list[j];
		r = bp->byteregs ? bregs_per_line : regs_per_line;
                bp->nlines = ((bp->nregs - 1) / r) + 1;
                cp->nlines += bp->nlines;
            }
            if (l + cp->nlines > rows - 2) {
                l = 1;
                p++;
            }
            cp->page = p;
            cp->line = l;
            for (j = 0; j < cp->nboards; j++) {
                bp = cp->board_list[j];
                bp->line = l;
                l += bp->nlines;
            }
	    l++;
        }
}

/* clear_text_area() -------------------------------------------------
	This routine clears the register text display area to blank.
*/
clear_text_area()
{
	set_color(real_black);
	c_box(regx,1,columns,rows-2,1);
}

/* draw_screen() -------------------------------------------------------
	This routine redraws the screen in the current mode and
	settings.
*/
draw_screen()
{
	if (screen_mode == regs) draw_rack(cur_page);
	if (screen_mode == fields) draw_fields(selected_bp);
	draw_menu();
}

/* draw_rack(pg) -------------------------------------------------------
	This routine draws one page of the rack display.
*/
draw_rack(pg)
int pg;
{
        CAGEPTR cp;
        BOARDPTR bp;
        REGPTR rp;
	WATCHTBLPTR wdp;
        int i,j,l,r,x1,y1,x2,y2,rval,rinc;
	static char chead[] = "cages";
	static char phead[] = "p";
	static char bhead[] = "boards";
	static char thead[] = "t b";
	char rhead[100];
	char t[10];

	make_watch_invis();
	set_color(bgcol);
	box(dxmin,dymin,dxmax,dymax,1);
	sprintf(rhead,"---------- registers (source: %s) %s ----"
		,rs_name[regs_source]
		,(regs_modified) ? "(mod) " : "");
        set_color(hcol);
        draw_text(cagex,rows-1,0,chead);
        draw_text(presx,rows-1,0,phead);
        draw_text(boardx,rows-1,0,bhead);
        draw_text(testx,rows-1,0,thead);
        draw_text(regx,rows-1,0,rhead);
	set_color(orange);
	draw_text(columns-strlen(system_name),rows-1,0,system_name);
	cur_page = pg;
        for (i = 0; i < ncages; i++) {
            cp = cage_list[i];
            if (cp->page == pg) {
                set_color(ccol);
                draw_text(cagex,cp->line,0,cp->name);
                for (j = 0; j < cp->nboards; j++) {
                    bp = cp->board_list[j];
                    set_color(lcol);
                    x1 = screen_x(boardx) - (char_width / 2);
                    y1 = screen_y(bp->line) - 1;
                    x2 = screen_x(testx-1) - (char_width / 2);
                    y2 = y1 + bp->nlines * line_spacing;
                    box(x1,y1,x2,y2,0);
                    set_color(bcol);
                    draw_text(boardx,bp->line,0,bp->name);
		    r = regx;
		    rinc = bp->byteregs ? 3 : 5;
		    l = bp->line;
		    for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
			rval = get_reg(bp,rp);
			if (bp->byteregs)
			    sprintf(t,"%.2X",rval);
			else
			    sprintf(t,"%.4X",rval);
			set_color(rcol);
			if (rp->watchptr != NULL) {
			    set_color(wbgcol);
			    c_box(r,l,r + rinc - 2,l,1);
			    set_color(wcol);
			    wdp = rp->watchptr->d;
			    wdp->visible = 1;
			    wdp->col = r;
			    wdp->row = l;
			}
			draw_text(r,l,0,t);
			if ((r += rinc) > columns - rinc + 1) {
			    r = regx;
			    l++;
			}
		    }
		}  /* for j */
	    }  /* if cp->page */
	}  /* for i */
	draw_presence();
}

/* draw_fields(bp) ----------------------------------------------------
	This routine uses the register area of the rack display to
	display the individual bit fields of each register of the
	given board.
*/
draw_fields(bp)
BOARDPTR bp;
{
	REGPTR rp;
	FIELDPTR fp;
	MEMPTR mp;
	WATCHTBLPTR wdp;
	int row,col,rval,f,c,rlen;
	char t[30];

	make_watch_invis();
	if (selected_bp != NULL) draw_board_box(selected_bp,real_black);
	selected_bp = bp;
	draw_board_box(bp,hicol);
	col = regx;
	row = rows - 3;
	set_color(real_black);
	clear_text_area();
	for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
	    if (rp->nfields + 2 > row) {
		row = rows - 3;
		col += column_size;
	    }
	    rval = get_reg(bp,rp);
	    if (bp->byteregs)
		sprintf(t,"REGISTER %d = %.2X",rp->num,rval);
	    else
		sprintf(t,"REGISTER %d = %.4X",rp->num,rval);
	    set_color(rcol);
	    rp->line = row;
	    rp->col = (col - regx) / column_size;
	    draw_text(col,row,0,t);
	    if (rp->watchptr != NULL) {
		set_color(wbgcol);
		rlen = (bp->byteregs) ? 1 : 3;
		c = col + 13 + (rp->num > 9);
		c_box(c,row,c + rlen,row,1);
		set_color(wcol);
		draw_text(c,row,0,&t[c-col]);
		wdp = rp->watchptr->d;
		wdp->visible = 1;
		wdp->col = c;
		wdp->row = row;
	    }
	    row--;
	    set_color(fcol);
	    for (fp = rp->field_list; fp != NULL; fp = fp->nxt_field) {
		fp->line = row;
		f = (rval & fp->mask) >> fp->shift;
		sprintf(t,"%-17s %4X",fp->name,f);
		draw_text(col+2,row--,0,t);
	    }  /* for fp */
	    row--;
	}  /* for rp */

    /* display memories */
	for (mp = bp->mem_list; mp != NULL; mp = mp->nxt_mem) {
	    if (mp->disp_len + 3 > row) {
		row = rows - 3;
		col += column_size;
	    }
	    mp->line = row;
	    mp->col = (col - regx) / column_size;
	    draw_memory(bp,mp);
	    row -= mp->disp_len + 3;
	}  /* for mp */
	draw_presence();
}

/* draw_memory(bp,mp) --------------------------------------------------
	This routine displays the current display segment of the
	given memory.
*/
draw_memory(bp,mp)
BOARDPTR bp;
MEMPTR mp;
{
	WATCHPTR wp;
	WATCHTBLPTR wdp;
	int i1,i2,row,col,off;
	int mem_end,pid_save;
	int val,w;
	char t[50];

	row = mp->line;
	col = mp->col * column_size + regx;
	pid_save = mb.w[0];
	mb.w[0] = mp->page;
	i1 = (mb.w[0]<<20) | mp->addr;
	i2 = mp->addr>>1;
	asp_read( i1, &mb.w[i2], mp->len );	/* read in whole memory */
	set_color(real_black);
	c_box(col,row-mp->disp_len-1,col+column_size-1,row,1);
	set_color(rcol);
	sprintf(t,"MEMORY %s",mp->name);
	draw_text(col,row--,0,t);
	if (strlen(mp->load_file)) sprintf(t,"  SOURCE: %s",mp->load_file);
	else strcpy(t,"  SOURCE: prev. data");
	if (mp->modified) strcat(t," (mod)");
	draw_text(col,row--,0,t);
	mem_end = mp->disp_addr + 8 * mp->disp_len;
	if (mem_end > mp->addr + mp->len) mem_end -= mp->len;
	for (i1 = mp->disp_addr; i1 != mem_end; i1 += 8) {
	    if (i1 >= mp->addr + mp->len) i1 -= mp->len;
	    set_color(memcol);
	    sprintf(t,"%.5X: ",i1);
	    draw_text(col,row,0,t);
	    off = 7;
	    for (i2 = 0; i2 < 4; i2++) {
	    /* select normal color or "watch" colors */
		set_color(memcol);
		if ((wp = mp->watchptr) != NULL) {
		    for (w = 0, wdp = wp->d; w < wp->nwatches;
				w++, wdp++) {
			if (wdp->addr == i1 + 2*i2) {
			    set_color(wbgcol);
			    c_box(col+off,row,col+off+3,row,1);
			    set_color(wcol);
			    wdp->visible = 1;
			    wdp->col = col + off;
			    wdp->row = row;
			    break;
			}  /* if wdp->addr */
		    }  /* for w */
		}  /* if wp */
	    /* pick up 16-bit value and display it */
		val = mb.w[(i1 >> 1) + i2 ] & 0xffff;
		sprintf(t,"%.4X",val);
		draw_text(col+off,row,0,t);
		off += 5;
	    }  /* for i2 */
	    row--;
	}
	mb.w[0] = pid_save;
}

/* draw_board_box(bp,color) --------------------------------------------
	This routine draws the given board name with a surrounding box.
	"color" specifies the background color to be used.
*/
draw_board_box(bp,color)
int color;
BOARDPTR bp;
{
	int x1,y1,x2,y2;

	x1 = screen_x(boardx) - (char_width / 2);
	y1 = screen_y(bp->line) - 1;
	x2 = screen_x(testx-1) - (char_width / 2);
	y2 = y1 + bp->nlines * line_spacing;
	set_color(color);
	box(x1,y1,x2,y2,1);
	set_color(lcol);
	box(x1,y1,x2,y2,0);
	set_color(bcol);
	draw_text(boardx,bp->line,0,bp->name);
}

/* draw_test_bus() -----------------------------------------------------
	This routine draws a highlighted line to indicate the current
	extent of the test bus, and a different highlight bar beside 
	each board which currently has any active block(s).
*/
draw_test_bus()
{
	CAGEPTR cp;
	BOARDPTR bp;
	REGPTR rp;
	FIELDPTR fp;
	int i,j,rval,v;
	int fval,allbyp,val,nbyps;

	set_color(real_black);
	c_box(testx,1,testx+2,rows-2);
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    if (cp->page == cur_page) {
		for (j = 0; j < cp->nboards; j++) {
		    bp = cp->board_list[j];
		    allbyp = 1;
		    nbyps = 0;
		    fval = 0;
		    if (bp->status) {
			for (rp = bp->reg_list; rp != NULL;
					rp = rp->nxt_reg) {
			    rval = get_reg(bp,rp);
			    for (fp = rp->field_list; fp != NULL;
					    fp = fp->nxt_field) {
				val = (rval & fp->mask)
					    >> fp->shift;
				if (fp->inverted)
				    val = 1 - val;
				if (fp->tap)
				    fval |= val;
				if (fp->bypass) {
				    allbyp &= val;
				    nbyps++;
				}
			    }  /* for fp */
			}  /* for rp */
			set_color(tcol);
			v = (i > 0) && (j == 0);
			c_box(testx,bp->line-v,testx,bp->line,1);
			if (allbyp == 0 || nbyps == 0) {
			    set_color(bypcol);
			    c_box(testx+2,bp->line-v,testx+2,bp->line,1);
			}
			if (fval)
			    return;
		    }  /* if bp->status */
		}  /* for j */
	    }  /* if cp->page */
	}  /* for i */
}

/* draw_presence() -----------------------------------------------------
	This routine draws the red dots beside the board names to
	show which boards are present in the system.
*/
draw_presence()
{
	CAGEPTR cp;
	BOARDPTR bp;
	int i,j;
	char c,pres_char = 221;

	set_color(real_black);
	c_box(presx,1,presx,rows-2,1);
	set_color(pcol);
	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    if (cp->page == cur_page) {
		for (j = 0; j < cp->nboards; j++) {
		    bp = cp->board_list[j];
		    c = bp->status ? pres_char : ' ';
		    draw_text(presx,bp->line,1,&c);
		}   /* for j */
	    }  /* if cp->page */
	}  /* for i */
	draw_test_bus();
}

/* draw_menu() ---------------------------------------------------------
	This routine draws the function selection menu on the screen.
*/
draw_menu()
{
	int m,color;
	int x,y;
	static char menu_text[] = " Presence   Initialize  Save regs  Load regs  Watch      Quit";

	display_text(menu_text,0);
}

/* draw_text_display ---------------------------------------------------
	This routine draws the current text_display area.
*/
draw_text_display()
{
	set_color(mbgcol);
	box(txmin,tymin,txmax,tymin + line_spacing,1);
}

/* draw_group_box(x,y) -------------------------------------------------
	This routine draws the rubberband box within which group 
	selections are made.
*/
draw_group_box(x,y)
int x,y;
{
	static int x2,y2;

	if (gb_pt) {
	    set_color(0);
	    box(x2,y2,gb_x,gb_y,0);
	    x2 = x;
	    y2 = y;
	    set_color(ccol);
	    box(x2,y2,gb_x,gb_y,0);
	}
}

/* clear_plane(mask) ------------------------------------------------
	This routine clears the plane(s) represented by mask to
	zeroes.
*/
clear_plane(mask)
int mask;
{
printf("CLEAR_PLANE is not yet supported for R1B\n");
/*
	mgiclearpln(2,mask,0);
*/
}

/* print_error (msg) --------------------------------------------------
	This routine issues a beep tone and displays the given message
	in the text display area.   It then waits for any keystroke to
	be entered, redisplays the menu, and returns.
*/
print_error (msg)
char *msg;
{
	char c = '\0';
	int color_save;

	printf("\07\n");
	color_save = mbgcol;
	mbgcol = red;
	display_text(msg,0);
	mbgcol = color_save;
	while (c == '\0')
	    get_byte(&c);
	draw_menu();
}

/* display_text(s,ctr) -------------------------------------------------
	This routine displays the text string s in the text display 
	area, in the currently selected color.  If ctr=1 the string 
	is centered, otherwise it is left justified.
*/
display_text(s,ctr)
char *s;
int ctr;
{
	int xw,xloc;

	draw_text_display();
	xw = strlen(s);
	xloc = (ctr) ? (columns - xw) / 2 : 1;
	set_color(mcol);
	draw_text(xloc,0,0,s);
}

/* get_string(s,p) -----------------------------------------------------
	This routine displays prompt p in the text display area, and 
	then gets a string response from the user and returns it in s.
*/
get_string(s,p)
char *p,s[];
{
	int save_color;
	int i = 0;
	char c = '\0';

	save_color = mbgcol;
	mbgcol = blue;
	display_text(p,0);
	while (c == '\0')
	    get_byte(&c);
	while (c != '\n' && c != 27) {
	    if (c == 127 || c == 8) {
		if (i) 
		    i--;
	    }
	    else
		s[i++] = c;
	    s[i] = '\0';
	    display_text(s,0);
	    c = '\0';
	    while (c == '\0')
		get_byte(&c);
	}
	if (c == 27) {
	    i = 0;
	    s[0] = '\0';
	}
	mbgcol = save_color;
	return(i);
}

/* display_question(t) -------------------------------------------------
	This routine displays the question t in the data display box, 
	then reads a Y or N response.  The routine returns 1 if Y 
	or 0 if N.
*/
display_question(t)
char *t;
{
	char c;
	int ans;

	display_text(t,1);
	for (c = '\0'; c != 'Y' && c != 'N';get_byte(&c))
	    if (c != '\0') printf("\07\n");
	ans = (c == 'Y');
	draw_text_display();
	return (ans);
}

/* draw_text(c,r,nc,text) ----------------------------------------------
	This routine draws a text string at the given x,y location.
*/
draw_text(c,r,nc,t)
int c,r;			/* location of text */
int nc;				/* number of characters */
char *t;			/* text string to draw */
{
	int x,y;

	x = screen_x(c);
	y = screen_y(r);
/*
	mgigfs(x,y,nc,t);
*/
}

/* screen_x(col) -------------------------------------------------------
	This routine converts a column number into a screen x value.
*/
screen_x(col)
int col;
{
	return (col * char_width);
}

/* screen_y(row) -------------------------------------------------------
	This routine converts a row number into a screen y value.
*/
screen_y(row)
int row;
{
	int y;

	y = row * line_spacing;
	if (row) y += row_extra;
	return (y);
}

/* line(x1,y1,x2,y2) ---------------------------------------------------
	This routine draws a line from x1,y1 to x2,y2 on the board
	display area.  The line is clipped to the boundary of the 
	display area.
*/
line(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
	int c,c1,c2,x,y;

	c1 = clip_code(x1,y1);
	c2 = clip_code(x2,y2);
	while ((c1 || c2) && !(c1 & c2)) {
	    c = c1 ? c1 : c2;
	    if (c & 3) {
		x = (c & 1) ? minpix : maxpix;
		y = y2 + (y1 - y2) * (x - x2) / (x1 - x2);
	    } else {
		y = (c & 4) ? minlin : maxlin;
		x = x2 + (x1 - x2) * (y - y2) / (y1 - y2);
	    }
	    if (c == c1) {
		x1 = x;
		y1 = y;
		c1 = clip_code(x,y);
	    } else {
		x2 = x;
		y2 = y;
		c2 = clip_code(x,y);
	    }
	}
	if (c1 || c2) return;
/*
	mgil(x1,y1,x2,y2);
*/
}

/* clip_code(x,y) ------------------------------------------------------
	This routine returns the Cohen-Sutherland clipping code for the
	point x,y clipped against the board display area boundary.
*/
clip_code(x,y)
int x,y;
{
	int c;

	c = 0;
	if (x < minpix) c = 1;
	if (x > maxpix) c = 2;
	if (y < minlin) c += 4;
	if (y > maxlin) c += 8;
	return (c);
}

/* c_box(c1,r1,c2,r2,fill) --------------------------------------------
	This routine draws a box in the character positions given.
*/
c_box(c1,r1,c2,r2,fill)
int c1,r1,c2,r2,fill;
{
	int x1,y1,x2,y2;

	x1 = screen_x(c1);
	y1 = screen_y(r1) - 1;
	x2 = screen_x(c2) + char_width - 1;
	y2 = screen_y(r2) + line_spacing - 1;
	box(x1,y1,x2,y2,fill);
}

/* box(xlo,ylo,xhi,yhi,fill) -------------------------------------------
	This routine draws a filled (fill=1) or unfilled (fill=0) box
	at the given location.  If the box is filled, the fill is in 
	background color, while the outline is in the preset color.
*/
box(xlo,ylo,xhi,yhi,fill)
int xlo,ylo,xhi,yhi;		/* boundary location of box */
int fill;			/* fill option */
{
printf("BOX is not yet supported for R1B\n");
/*
	if (fill) mgibox(xlo,yhi,xhi,ylo);
	else {
	    mgil(xlo,ylo,xlo,yhi);
	    mgil(xlo,yhi,xhi,yhi);
	    mgil(xhi,yhi,xhi,ylo);
	    mgil(xhi,ylo,xlo,ylo);
	}
*/
}

/* get_input(c) --------------------------------------------------------
	This routine reads input from the keyboard/mouse.  The new 
	position of the cursor is returned in the global variables
	cx,cy.  If a character is typed in, the character value is 
	returned in c.  The routine return value is the state of the 
	buttons, as follows:
		0 = no buttons pressed
		1 = either outside button pressed
		2 = middle button pressed
*/
get_input(c)
char c[];		/* return for character typed */
{
	static int first=1;	/* first time switch */
	static int fp;		/* file pointer for keyboard */
	static int prev_buttons; /* previous button status */
	char ccc[1];		/* character flush hold area */
	int buttons;		/* button status hold area */
	int x,y;

    /* get a keyboard character, if any */
	get_byte(c);

    /* get cursor position, magnifying mouse motion by 2 */
	get_curs(&cx,&cy,&buttons);
	cx *= 2;
	x = cx;
	if (x < minpix) x = minpix;
	if (x > maxpix) x = maxpix;
	cy *= 2;
	y = cy;
	if (y < minlin) y = minlin;
	if (y > maxlin) y = maxlin;
	set_curs_xy(x,y);

    /* return button status */
	if (buttons == prev_buttons) return (0);
	prev_buttons = buttons;
	if (buttons & 5) return (1);
	return (buttons & 2);
}

/* set_curs_xy(x,y) ----------------------------------------------------
	This routine sets the current cursor location to x,y.
*/
set_curs_xy(x,y)
int x,y;
{
printf("SET_CURS_XY is not yet supported for R1B\n");
/*
	if (y != cy || x != cx) {
	    mgicursmode(1);
	    mgicursxy(2,4,x/2,y/2);
	    mgicursmode(5);
	    cx = x;
	    cy = y;
	}
	mgicursxy(2,2,x,y);
*/
}

/* get_curs(x,y,buttons) -----------------------------------------------
	This routine returns the current cursor x,y location and button
	state.
*/
get_curs(x,y,buttons)
int *x,*y,*buttons;
{
	static int xx,yy;

	if (button_holder) {
	    *x = xx;
	    *y = yy;
	    *buttons = button_holder;
	    button_holder = 0;
	} else {
printf("GET_CURS is not yet supported for R1B\n");
/*
	    mgigetcursxy(2,4,x,y);
*/
	    *buttons = 0;
	    xx = *x;
	    yy = *y;
	}
}

/* button_handler(x,y,buttons) -----------------------------------------
	This routine is the interrupt handler for mouse button presses.
*/
button_handler(x,y,buttons)
int x,y,buttons;
{
	button_holder = buttons;
}

/* set_color(color) ----------------------------------------------------
	This routine sets the current drawing color as specified.
*/
set_color(color)
int color;
{
printf("SET_COLOR is not yet supported for R1B\n");
/*
	    mgipln(0x3ff);
	    mgihue(cur_col = color);
*/
}

/* set_font(font) ------------------------------------------------------
	This routine sets the currently active font to the one 
	specified by font.
*/
set_font(font)
int font;		/* font number desired */
{
printf("SET_FONT is not yet supported for R1B\n");
/*
	mgigf(font);
	line_spacing = 20;
	char_width = 11;
	char_height = 12;
*/
}

/* pick(button,id,val) -------------------------------------------------
	This routine decides which screen quadrant or menu word was 
	picked by a mouse button press.
*/
pick (button,id,val)
int button;			/* state of mouse buttons */
int *id,*val;			/* returned pick id and value */
{
	BOARDPTR bp,find_board();
	char c;				/* holder for input character */
	int r;
	static char menu_tbl[] = " pislwq";
	char *strchr();

	c = *id;
	if (isupper(c)) c = tolower(c);
	*val = 0;
	*id = -1;
    /* was a mouse button pushed? */
	if (button) {
	/* was pick on the menu? */
	    if (cy < tymax) {
		*id = 0;
		*val = 1 + (cx - txmin) / tx_wordsize;
	    /*  printf ("pick id = %d, val = %d\n",*id,*val);  */
		return;
	    }
	/* determine which character position was picked */
	    *id = (cy - row_extra) / line_spacing;
	    *val = cx / char_width;
	    gcolm = *val;
	    grow = *id;
	    if (*val < presx) *val = 1;
	    else if (*val < boardx) *val = 2;
	    else if (*val < testx) *val = 3;
	    else if (*val < regx) *val = 4;
	    else if (screen_mode == regs) {
		if ((bp = find_board(*id)) != NULL) {
		    r = bp->byteregs ? 3 : 5;
		    *val = 5 + (*val - regx) / r;
		}
	    }
	    else if (screen_mode == fields)
		*val = 5 + (*val - regx) / column_size;
	/*  printf ("pick id = %d, val = %d\n",*id,*val);  */
	    return;
	}  /* if button */
    /* was a character typed? */
	if (c) {
	    if (strchr(menu_tbl,c) != NULL) {
		*val = strchr(menu_tbl,c) - menu_tbl;
		*id = 0;
	    /*  printf ("pick id = %d, val = %d\n",*id,*val);  */
		return;
	    }
	}
}

/* change_reg(bp,regno,button) ----------------------------------------
	This routine reads a hex value from the user and sets
	the value of the given reg accordingly.
*/
change_reg(bp,regno,button)
BOARDPTR bp;
int regno,button;
{
	REGPTR rp;
	int rval,col,row,rsize;
	char t[20];

	regno -= 5;
	rsize = (bp->byteregs) ? 3 : 5;
	rsize *= regno;
    /* find the correct register block */
	for (rp = bp->reg_list; rp != NULL && regno; rp = rp->nxt_reg)
	    regno--;
	if (rp != NULL) {
	/* if setting a watch, do it */
	    if (watch_sw) {
		watch_sw = 0;
		if (screen_mode == regs) {
		    col = regx + rsize;
		    row = bp->line;
		} else {
		    col = regx + (rp->col * column_size) + 13
			       + (rp->num > 9);
		    row = rp->line;
		}
		set_reg_watch(bp,rp,col,row,button);
	    } else {  /* not setting a watch */
		get_string(t,"Enter new register value:");
		sscanf(t,"%x",&rval);
		if (strlen(t) && isxdigit(t[0]))
		    set_reg(bp,rp,rval);
		draw_screen();
	    }  /* else */
	    draw_menu();
	}  /* if rp */
}

/* change_field(bp,col,fieldno,button) ---------------------------------
	This routine changes the value of a single field in a register.
	If the value is a single bit, the bit is toggled.  If the
	field is larger, a numeric value is read from the user.
*/
change_field(bp,col,fieldno,button)
BOARDPTR bp;
int col,fieldno,button;
{
	REGPTR rp;
	FIELDPTR fp;
	int rval,fval,r;
	char t[30];

     /* printf("entering change_field\n"); */
	if (screen_mode != fields) return;
	col -= 5;
	r = 4;
     /* printf("searching for col %d, row %d\n",col,fieldno);  */
	for (rp = bp->reg_list; rp != NULL; rp = rp->nxt_reg) {
	    r++;
	    if (rp->col == col) {
		if (rp->line == fieldno) change_reg(bp,r,button);
		else {
		    for (fp = rp->field_list; fp != NULL;
			       fp = fp->nxt_field) {
			if (fp->line == fieldno) {
			/* pick up old field value */
			    fval = get_field(bp,rp,fp);
			/* get the new field value */
			    if (fp->nbits == 1) {
				fval = 1 - fval;
				set_field(bp,rp,fp,fval);
			    } else {
				get_string(t,"Enter new field value:");
				if (strlen(t) && isxdigit(t[0])) {
				    sscanf(t,"%x",&fval);
				    set_field(bp,rp,fp,fval);
				}
			    }
			/* display the new value */
			    draw_screen();
			    break;
			}  /* if fp->line */
		    }  /* for fp */
		}  /* else */
	    }  /* if rp->col */
	}  /* for rp */
}

/* change_memory(bp,col,fieldno,button) --------------------------------
	This routine changes the display or contents of multibus
	memory, or saves the contents to disk.
*/
change_memory(bp,col,fieldno,button)
BOARDPTR bp;
int col,fieldno,button;
{
	MEMPTR mp;
	int m,mval, i, j;
	int col_start,pid_save;
	char t[30];

     /* printf("entering change_memory\n"); */
	if (screen_mode != fields) return;
	col -= 5;
	col_start = regx + column_size * col;
     /* printf("searching for col %d, row %d\n",col,fieldno);  */
	for (mp = bp->mem_list; mp != NULL; mp = mp->nxt_mem) {
	    if (mp->col == col) {
		if ((mp->line == fieldno) || ((mp->line - 1) == fieldno)) {
		    if (watch_sw)
			return;
		    if (button == 1)
			load_memory(mp,"");
		    if (button == 2)
			save_memory(mp);
		    break;
		} else {
		    if ((fieldno < mp->line - 1) &&
			    (fieldno >= mp->line - mp->disp_len - 1)) {
		    /* if the click was out of range, quit */
			if (gcolm > col_start + 25)
			    return;
		    /* check for click on address */
			if (gcolm < col_start + 6) {
			    if (watch_sw)
				return;
			    if (button == 1) mp->disp_addr += 8 * mp->disp_len;
			    if (button == 2) mp->disp_addr -= 8 * mp->disp_len;
			    if (mp->disp_addr >= mp->addr + mp->len)
				mp->disp_addr -= mp->len;
			    if (mp->disp_addr < mp->addr)
				mp->disp_addr += mp->len;
			}
		    /* it was a click on memory contents */
			else {  /* if gcolm */
			    pid_save = mb.w[0];
			    mb.w[0] = mp->page;	/* set the PID reg */
			    i = (mb.w[0]<<20) | mp->addr;
			    j = mp->addr>>1;
			    asp_read( i, &mb.w[j], mp->len );
			/* calculate memory location */
			    m = mp->disp_addr +
				(8 * (mp->line - 2 - fieldno)) +
				(2 * ((gcolm - col_start - 6) / 5));
			    if (m >= mp->disp_addr + mp->len) m -= mp->len;
			/* if setting watch, do it */
			    if (watch_sw) {
				col = gcolm + 1 - 
					((gcolm - col_start - 6) % 5);
				set_mem_watch(bp,mp,m,col,fieldno
					,button);
				mb.w[0] = pid_save;
				return;
			    }
			/* pick up old memory value */
			    mval = mb.w[m >> 1] & mp->mask;
			/* get the new memory value */
			    get_string(t,"Enter new value:");
			    if (strlen(t) && isxdigit(t[0]))
				sscanf(t,"%x",&mval);
			/* insert new value in memory location */
			    if (mval != mb.w[m >> 1]) mp->modified = 1;
			    mb.w[m >> 1] = mval;
			    i = (mb.w[0]<<20) | m;
			    j = m>>1;
			    asp_write( i, &mb.w[j], 2 );
			    mb.w[0] = pid_save;	/* reset the PID */
			}  /* else (gcolm) */
		    /* display the new value */
			draw_memory(bp,mp);
			draw_menu();
			break;
		    }  /* if fieldno */
		}  /* else (mp->line != fieldno) */
	    }  /* if mp->col */
	}  /* for mp */
}

/* find_board(line) --------------------------------------------------
	This routine determines which board is pointed to by a
	pick on the given screen line.
*/
BOARDPTR find_board(line)
int line;
{
	BOARDPTR bp;
	CAGEPTR cp;
	int i,j;

	for (i = 0; i < ncages; i++) {
	    cp = cage_list[i];
	    if ((line >= cp->line) && (line < cp->line + cp->nlines)) {
		for (j = 0; j < cp->nboards; j++) {
		    bp = cp->board_list[j];
		    if (line <= bp->line + bp->nlines - 1) return (bp);
		}  /* for j */
	    }  /* if line */
	}  /* for i */
	return (NULL);
}

/* load_memory(mp,fname) -----------------------------------------------
	This routine loads the given memory from a disk file.
*/
load_memory(mp,fname)
MEMPTR mp;
char *fname;
{
	char t[50],filename[50];
	int m,n,val,pid_save, i, j;

	if (strlen(fname)) strcpy(filename,fname);
	else get_filename("Enter load file name:",filename,".mem");
	if (strlen(filename)) {
	    if (open_file(filename,"") == 0) {
		print_error("Cannot open file");
		return;
	    }
	    display_text("Loading...",0);
	    pid_save = mb.w[0];
	    mb.w[0] = mp->page;
	    for (m = mp->addr; m < mp->addr + mp->len; m += 2) {
		if ((n = next_token(t)) == 0) break;
		sscanf(t,"%X",&val);
		mb.w[m >> 1] = val;
	    }  /* for m */
	    i = (mb.w[0]<<20) | mp->addr;
	    j = mp->addr>>1;
	    asp_write( i, &mb.w[j], mp->len );
	    mb.w[0] = pid_save;
	    mp->modified = 0;
	    strcpy(mp->load_file,filename);
	    if (n == 0)
		print_error("Load file shorter than memory (loaded anyway)");
	    else if (next_token(t) != 0)
		print_error("Load file longer than memory- truncated");
	    close_file();
	}  /* if strlen */
	draw_memory(selected_bp,mp);
	draw_menu();
}

/* save_memory(mp) ----------------------------------------------------
	This routine saves the given memory to a disk file.
*/
save_memory(mp)
MEMPTR mp;
{
	FILE *fp;
	char t[50],filename[50];
	int l,m,val,pid_save, i, j;

	get_filename("Enter save file name:",filename,".mem");
	if (strlen(filename)) {
	    if ((fp = fopen(filename,"w")) == NULL) {
		print_error("Cannot open file");
		return;
	    }
	    display_text("Saving...",0);
	    l = 0;
	    pid_save = mb.w[0];
	    mb.w[0] = mp->page;
	    i = (mb.w[0]<<20) | mp->addr;
	    j = mp->addr>>1;
	    asp_read( i, &mb.w[j], mp->len );
	    for (m = mp->addr; m < mp->addr + mp->len; m += 2) {
		val = mb.w[m >> 1] & mp->mask;
		fprintf(fp,"%.4X",val);
		fprintf(fp,((++l % 8) == 0) ? "\n" : ",");
	    }  /* for m */
	    fclose(fp);
	    mb.w[0] = pid_save;
	    if (mp->modified) {
		mp->modified = 0;
		strcpy(mp->load_file,filename);
		draw_memory(selected_bp,mp);
	    }
	}  /* if strlen */
	draw_menu();
}

/* watch() -------------------------------------------------------------
	This routine draws any visible watch addresses on the screen.
*/
watch()
{
	WATCHPTR wp;
	WATCHTBLPTR wdp;
	BOARDPTR bp;
	REGPTR rp;
	MEMPTR mp;
	int w,len,val,pid_save, i, j;
	char t[10];

	pid_save = mb.w[0];
	for (wp = watch_list; wp != NULL; wp = wp->nxt_watch) {
	    bp = wp->bp;
	    for (w = 0, wdp = wp->d; w < wp->nwatches; w++, wdp++ ) {
		if (wdp->visible) {
		/* pick up current value */
		    if ((rp = wp->rp) != NULL) {
			val = get_reg(bp,rp);
			len = bp->byteregs ? 1 : 3;
		    }
		    if ((mp = wp->mp) != NULL) {
			mb.w[0] = wdp->page;
			i = (mb.w[0]<<20) | wdp->addr;
			j = wdp->addr>>1;
			asp_read( i, &mb.w[j], 2 );
			val = mb.w[wdp->addr >> 1] & mp->mask;
			len = 3;
		    }
		/* if value has changed, display it */
		    if (val != wdp->oldval) {
			set_color(wbgcol);
			c_box (wdp->col, wdp->row,
				wdp->col+len, wdp->row, 1);
			if (len == 1)
			    sprintf(t,"%.2X",val);
			else
			    sprintf(t,"%.4X",val);
			set_color(wcol);
			draw_text(wdp->col,wdp->row,0,t);
			wdp->oldval = val;
		    }  /* if val */
		}  /* if wdp->visible */
	    }  /* for w */
	}  /* for wp */
	mb.w[0] = pid_save;
}

/* set_reg_watch(bp,rp,col,row,button) ---------------------------------
	This routines sets or clears a watch block for a register.
*/
set_reg_watch (bp,rp,col,row,button)
BOARDPTR bp;
REGPTR rp;
int col,row,button;
{
	WATCHPTR wp,create_watch();
	WATCHTBLPTR wdp;
	int w,val,rlen;
	char t[30];

    /* if entry already exists, clear it */
	if ((wp = rp->watchptr) != NULL) {
	    wdp = wp->d;
	    if (wdp->visible) {
		rlen = (bp->byteregs) ? 1 : 3;
		set_color(real_black);
		c_box(wdp->col,wdp->row,wdp->col+rlen,wdp->row,1);
		set_color(rcol);
		if (bp->byteregs)
		    sprintf(t,"%.2X",wdp->oldval);
		else
		    sprintf(t,"%.4X",wdp->oldval);
		draw_text(wdp->col,wdp->row,0,t);
	    }
	    unlink_watch(wp);
	    free(wp);
	    rp->watchptr = NULL;
	    return;
	}
    /* no entry exists, so make one */
	wp = create_watch(NULL,1);
	if (wp != NULL) {
	    rp->watchptr = wp;
	    wp->bp = bp;
	    wp->rp = rp;
	    wp->mp = NULL;
	    wdp = wp->d;
	    wdp->page = 0;
	    wdp->addr = rp->addr;
	    wdp->row = row;
	    wdp->col = col;
	    wdp->visible = 1;
	    wdp->oldval = ~(get_reg(bp,rp));
	}
}

/* set_mem_watch(bp,mp,addr,col,row,button) ----------------------------
	This routine adds a memory watch block to the watch list, or
	deletes it if one already exists.
*/
set_mem_watch(bp,mp,addr,col,row,button)
BOARDPTR bp;
MEMPTR mp;
int addr,col,row,button;
{
	WATCHPTR wp, wp2, create_watch();
	WATCHTBLPTR wdp;
	int n,w,len,pid_save;

	printf("SMW: %X, %d, %d\n",addr,col,row);
    /* determine if the block entry already exists */
	if ((wp = mp->watchptr) != NULL) {
	    n = wp->nwatches;
	    printf("watch exists, n=%d\n",n);
	    for (w = 0, wdp = wp->d; w < n; w++, wdp++) {
		printf("w=%d ",w);
		if (wdp->addr == addr) {
		/* the block exists, so remove it */
		    if (--n) {
			len = (wp->nwatches - (w+1)) * sizeof(WATCHTBL);
			memcopy(wdp,wdp+1,len);
			wp2 = create_watch(wp,n);
			if (wp2 != NULL)
			    mp->watchptr = wp2;
		    } else {
			unlink_watch(wp);
			free(wp);
			mp->watchptr = NULL;
		    }
		    draw_memory(bp,mp);
		    return;
		}  /* if wdp->addr */
	    }  /* for w */
	}  /* if wp */
    /* the block doesn't exist, so create one */
	if (wp == NULL)
	    n = 0;
	wp = create_watch(wp,n+1);
	if (wp != NULL) {
	    mp->watchptr = wp;
	    wp->bp = bp;
	    wp->mp = mp;
	    wp->rp = NULL;
	    wdp = &wp->d[n];
	    wdp->page = mp->page;
	    wdp->addr = addr;
	    wdp->row = row;
	    wdp->col = col;
	    wdp->visible = 1;
	    pid_save = mb.w[0];
	    mb.w[0] = wdp->page;
	    wdp->oldval = ~(mb.w[addr >> 1] & mp->mask);
	    mb.w[0] = pid_save;
	    print_watch(wp);
	}  /* if wp != NULL */
}

/* create_watch(wp,n)---------------------------------------------------
	This routine allocates storage for a new watch block, or
	changes an existing watch block to the given size.  The size,
	n, is given in number of entries.  Once the watch block is
	available, it is linked into the head of the watch list.
*/
WATCHPTR create_watch(wp,n)
WATCHPTR wp;
int n;
{
	WATCHPTR wp1,wp2;
	int size1,size2;
	void *malloc();

	printf("CREATE WATCH: wp=%X, n=%d\n",wp,n);
    /* allocate a new memory block for the watch data */
	if ((wp1 = (WATCHPTR) malloc(size1 = (sizeof(WATCH)
		    + (n-1) * sizeof(WATCHTBL)))) == NULL) {
	    print_error("No more room for watches");
	    return (NULL);
	}
    /* if there is an existing block, take care of it */
	if (wp != NULL) {
	/* copy the old block to the new one */
	    size2 = sizeof(WATCH) + (wp->nwatches-1) * sizeof(WATCHTBL);
	    if (size1 > size2)
		size1 = size2;
	    memcopy(wp1,wp,size1);
	/* remove the old block */
	    unlink_watch(wp);
	    free(wp);
	}  /* if wp */
    /* link the new block into the list */
	wp1->nwatches = n;
	wp2 = watch_list;
	watch_list = wp1;
	wp1->nxt_watch = wp2;
	wp1->prev_watch = NULL;
	if (wp2 != NULL) wp2->prev_watch = wp1;
	return (wp1);
}

/* unlink_watch(wp) ----------------------------------------------------
	This routine removes the given watch block from the watch list.
*/
unlink_watch(wp)
WATCHPTR wp;
{
	WATCHPTR wp1,wp2;

	wp1 = wp->prev_watch;
	wp2 = wp->nxt_watch;
	if (wp1 == NULL) watch_list = wp2;
	else wp1->nxt_watch = wp2;
	if (wp2 != NULL) wp2->prev_watch = wp1;
}

/* make_watch_invis() --------------------------------------------------
	This routine sets all the watch data blocks to "invisible".
*/
make_watch_invis()
{
	WATCHPTR wp;
	int w;

	for (wp = watch_list; wp != NULL; wp = wp->nxt_watch)
	    for (w = 0; w < wp->nwatches; w++)
		wp->d[w].visible = 0;
}

/* print_watch(wp) -----------------------------------------------------
	This routine prints the watch block wp on standard out.
*/
print_watch(wp)
WATCHPTR wp;
{
	WATCHTBLPTR wdp;
	int w;
	int b = 0;
	static char *btype[3] = { "nothing",
				  "register",
				  "memory"};

	if (wp->rp != NULL) b = 1;
	if (wp->mp != NULL) b = 2;
	printf("Watch block for %s",btype[b]);
	printf(": nwatches = %d\n",wp->nwatches);
	for (w = 0, wdp = wp->d; w < wp->nwatches; w++, wdp++) {
	    printf("  pg=%d, addr=%.5X, row=%.2d",wdp->page,wdp->addr,wdp->row);
	    printf(", col=%.2d, vis=%d, oldval=%.4X",wdp->col,wdp->visible,
			wdp->oldval);
	    printf("\n");
	}
}
