/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaimsubs.c -- image routines for bobtalk

		These routines provide image display functions
		for the bobtalk environment.  The routines are:

		cg (text)

			Clear the graphics screen

		ig (lo, hi, log)

			Set up a greyscale color map

		il (pixel, line)

			Set the location for image display

		show_im (start, npixels, nlines, compx, compy, comptype)

			Display an image segment
		
		See the head of each routine for a full description.
*/

#include <aspimg.h>
#include <math.h>

	int screen_npix,screen_nlin;	   /* screen size */
	int minpix,minlin,maxpix,maxlin;   /* screen boundaries */
	int plane_mask;			   /* graphic bit mask */

	static int image_mask;
	static int colortab[256];

/* init_graphics() ---------------------------------------------------
	This routine initializes the graphics screen and software, if
	it hasn't already been initialized.
*/
init_graphics()
{
	static int first=1;	/* first time switch */
	int mdata[36];		/* graphics system parameters */
	int l;			/* * of lines on screen */
	int c;			/* # of columns on screen */

	termsize(&l,&c);
	if (l <= 25) return (0);
	if (first) {
/*
	    mgiasngp(0,0);
	    mgigethc(36,mdata);
*/
	    screen_npix = mdata[0];
	    screen_nlin = mdata[1];
	    minpix = minlin = 0;
	    maxpix = screen_npix - 1;
	    maxlin = screen_nlin - 1;
	    plane_mask = mdata[24];
	    image_mask = iclamp(1,plane_mask,255);
	    imagex = minpix;
	    imagey = minlin;
	    rainbow();
	}
	first = 0;
printf("INIT_GRAPHICS is not fully supported for R1B\n");
sleep( 5 );
	return (0);
}

/* end_graphics() ----------------------------------------------------
	This routine terminates access to the graphics screen and
	associated software.
*/
end_graphics()
{
printf("END_GRAPHICS is not supported for R1B\n");
sleep( 5 );
/*
	mgideagp();
*/
}

/* rainbow() --------------------------------------------------------
	This routine loads the color table with a rainbow of colors,
	which are used for pseudo-color display of data.
*/
rainbow()
{
	int i,j,inc;
	int k = 0;
	int r = 0;	/* red component */
	int g = 0;	/* green */
	int b = 0;	/* blue */
	static int lim[16] = {
	    16, 32, 48, 64, 80, 96,112,128,144,160,176,192,208,224,240,256};
	static int ri[16] = {
	     9,  7, -3, -3, -4, -6,  0,  0,  0,  0, 11,  5,  0,  0,  0,  0};
	static int gi[16] = {
	     0,  0,  0,  0,  0,  5,  4,  3,  2,  2,  0,  0, -3, -3, -4, -6};
	static int bi[16] = {
	     9,  7,  0,  0,  0, -2, -2, -3, -4, -5,  0,  0,  0,  0,  0,  0};

	inc = 256 / (image_mask + 1);
	for (i = 0, j = 0; i < 256; i += inc) {
	    j += (i >= lim[j]);
	    colortab[k++] = (r << 16) | (g << 8) | b;
	    r = iclamp(0,r + (ri[j] * inc),255);
	    g = iclamp(0,g + (gi[j] * inc),255);
	    b = iclamp(0,b + (bi[j] * inc),255);
	}
	colortab[0] = 0;
printf("RAINBOW is not fully supported for R1B\n");
sleep( 5 );
/*
	mgicms(0,image_mask + 1,colortab);
*/
	iglo = ighi = 0;
}

/* cg (text) ----------------------------------------------------------
	This routine clears the graphics screen.  If text=1, the text
	overlay plane(s) is also cleared.  If text=2, only the text
	planes are cleared.
*/
cg (text)
int text;
{
	int planes = 0;

	if (init_graphics()) {
	    switch (text) {
		case 0:
		    planes = image_mask;
		    break;
		case 1:
		    planes = 0xfff;
		    break;
		case 2:
		    planes = 0xc00;
		    break;
	    }
printf("CG is not fully supported for R1B\n");
sleep( 5 );
/*
	    mgipln(planes);
	    mgihue(0);
	    mgibox(minpix,maxlin,maxpix,minlin);
*/
	}  /* init_gr */
	else system("clear");
	if (text != 2) {
	    imagex = minpix;
	    imagey = minlin;
	}
}

/* ig (lo, hi, logsw) --------------------------------------------------
	This routine loads a greyscale table into the color lookup 
	table, so greyscale images can be shown on the screen.  The 
	dynamic range of the greyscale is linearly (logsw = 0) or 
	logarithmically (logsw = 1) stretched between the entries 
	'lo' and 'hi', while all entries below 'lo' are black and 
	all entries above 'hi' are white.  lo and hi must be between 
	0 and the maximum color number displayable (which is 255 on
	Aurora displays and 63 on IGP displays).
	NOTE: if both hi and lo are 0, the rainbow color table is
	loaded.
*/
ig (lo, hi, logsw)
int lo,hi,logsw;
{
	int i, j, ilim;
	double d,dif;

	if (init_graphics()) {
	    if ((lo == 0) && (hi == 0)) {
		rainbow();
		return;
	    }
	    if (lo > hi) {
		j = lo;
		lo = hi;
		hi = j;
	    }
	    if (lo > image_mask) lo = image_mask;
	    iglo = lo;
	    ighi = hi;
	    ilim = (image_mask < hi) ? image_mask + 1 : hi;
	    i = 0;
	    while (i < lo) colortab[i++] = 0;
	    if (logsw) {
		dif = hi - lo;
		dif = log(dif);
		while (i < hi) {
		    d = i - lo;
		    if (i == lo) j = 0;
		    else j = (log(d) * 255) / (dif);
		    colortab[i++] = j | (j << 8) | (j << 16);
		}
	    } else {
		while (i < ilim) {
		    j = ((i - lo) * 255) / (hi - lo);
		    colortab[i++] = j | (j << 8) | (j << 16);
		}
	    }
	    while (i <= image_mask) colortab[i++] = 0xffffff;
printf("IG is not fully supported for R1B\n");
sleep( 5 );
/*
	    mgicms(0,image_mask + 1,colortab);
*/
	}
	else printf("not a graphics terminal\n");
}

/* iclamp (lo, val, hi) ------------------------------------------------
	This routine returns the value 'val', clamped between 'lo'
	and 'hi' inclusive.
*/
iclamp (lo, val, hi)
int lo,val,hi;
{
	if (val < lo) return (lo);
	if (val > hi) return (hi);
	return (val);
}

/* il (xloc, yloc) -----------------------------------------------------
	This routine sets the screen location at which the next
	image segment will be displayed.
*/
il (xloc, yloc)
int xloc,yloc;
{
	if (init_graphics()) {
	    imagex = iclamp(minpix,xloc,maxpix);
	    imagey = iclamp(minlin,yloc,maxlin);
	}
}

/* show_im (image, npix, nlin, compx, compy, comptype) -----------------
	This routine displays an image or image segment at the
	current location (global variables imagex,imagey) on the 
	screen.  npix,nlin give the size of the image.  On exit,
	the current location is updated to be just below the segment 
	just displayed.  'compx' gives the compression (positive) or
	expansion (negative) factor in the x dimension as an integer 
	value.  'compy' gives the y compression factor, but the sign
	of 'compy' is forced to match to that of 'compx'.  'comptype'
	gives the technique to use if compressing (0 = average, 1 =
	minimum, 2 = maximum).
*/

show_im (image, npix, nlin, compx, compy, comptype)
	int npix,nlin,compx,compy,comptype;
	char image[];
{
	int xl,yt,xr,yb;	/* image boundary */
	int ci[1200];		/* holder for pixel averages */
	static char cb[1200];	/* holder for pixels */
	int i,j,k,l,p;		/* indexing variables */
	int cc,val;
	int plim;		/* maximum pixel number */

	if (init_graphics() == 0) {
	    printf("not a graphics terminal\n");
	    return;
	}
	xl = imagex;
	yt = imagey;
	if (compy == -1 || compy == 0) compy = 1;
	if (compx >= -1 && compx <= 1) compx = (compy < 0) ? -1 : 1;

    /* 1-to-1 display */
	if (compx == 1 && compy == 1) {
	    xr = iclamp(xl,xl + npix - 1, maxpix);
	    yb = iclamp(yt,yt + nlin - 1, maxlin);
printf("SHOW_IM is not fully supported for R1B\n");
sleep( 5 );
/*
	    if ((xr - xl + 1) == npix)
		mgiimage(image,xl,y_conv(yt),xr,y_conv(yb));
	    else {
		for (k = 0, l = yt; l <= yb; l++) {
		    mgiimage(&image[k],xl,y_conv(l),xr,y_conv(l));
		    k += npix;
		}
	    }
	    imagey = (yb >= maxlin) ? minlin : yb + 1;
	    mgisyncrb(1);
*/
	    return;
	}

	compy = iclamp(1,abs(compy),256);
    /* compressed display */
	if (compx > 0) {
	    xr = iclamp(xl,xl + npix/compx - 1, maxpix);
	    yb = iclamp(yt,yt + nlin/compy - 1, maxlin);
	    plim = xr - xl + 1;
	    cc = compx * compy;
	    k = 0;
printf("SHOW_IM is not fully supported for R1B\n");
sleep( 5 );
/*
	    for (l = yt; l <= yb; l++) {
		for (p = 0; p < plim; p++)
		    ci[p] = (comptype == 1) ? 99999999 : 0;
		for (i = 0; i < compy; i++) {
		    for (p = 0; p < plim; p++) {
			for (j = 0; j < compx; j++) {
			    val = image[k++];
			    if (val < 0) val = 256 + val;
			    switch (comptype) {
				case 0:
				    ci[p] += val;
				    break;
				case 1:
				    if (val < ci[p])
					ci[p] = val;
				    break;
				case 2:
				    if (val > ci[p])
					ci[p] = val;
				    break;
			    }
			}
		    }
		    k += (npix - (compx * plim));
		}
		for (p = 0; p < plim; p++) {
		    if (comptype) cb[p] = ci[p];
		    else cb[p] = ci[p] / cc;
		}
		mgiimage(cb,xl,y_conv(l),xr,y_conv(l));
	    }
	    imagey = (yb >= maxlin) ? minlin : yb + 1;
	    mgisyncrb(1);
*/
	    return;
	}

    /* expanded display */
	compx = -compx;
	xr = iclamp(xl,xl + npix*compx - 1,maxpix);
	yb = iclamp(yt,yt + nlin*compy - 1,maxlin);
	k = 0;
	cc = compx - 1;
	plim = xr - xl + 1;
printf("SHOW_IM is not fully supported for R1B\n");
sleep( 5 );
/*
	for (l = yt; l <= yb; l++) {
	    if (((l - yt) % compy) == 0) {
		mgisyncrb(1);
		j = k;
		for (p = 0; p < plim; p++) {
		    cb[p] = image[k];
		    k += ((p % compx) == cc);
		}
		k = j + npix;
	    }
	    mgiimage(cb,xl,y_conv(l),xr,y_conv(l));
	}
	imagey = (yb >= maxlin) ? minlin : yb + 1;
	mgisyncrb(1);
*/
	return;
}

/* y_conv (y) ---------------------------------------------------------
	This routine converts a y value from origin at upper left
	corner of the screen to one with origin at the lower left
	corner.
*/

y_conv (y)
int y;
{
	return (maxlin - y);
}
