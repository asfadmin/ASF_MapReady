/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbahist.c -- plot routine for plotting histograms */

#include <aspdecl.h>
#include <math.h>

#define XSIZE 1024

extern minpix,minlin,maxpix,maxlin;	/* screen boundary */


/* p1 (pp) -------------------------------------------------------------
	This routine extracts and formats data for a histogram plot,
	and then passes the data to the plot_hist routine for plotting.
	pp is a structure of type PLOTPARMS.
*/

p1 (pp)
	PLOTPARAMS pp;
{
	float  *a, *xp;
	int     p, i, x, ppgo;
	float   ieee_out[2];

	if (init_graphics() == 0) {
	    printf("not a graphics terminal\n");
	    return;
	}
	p = XSIZE;
	if (pp.cmprss > 0)
	    p *= pp.cmprss;
	else if (pp.cmprss < 0)
	    p /= (-pp.cmprss);
	if (p > MAXFFTLEN)
	    p = MAXFFTLEN;
	
	if (pp.cen > MAXFFTLEN - p / 2)
	    pp.cen = MAXFFTLEN - p / 2;
	if (pp.cen < p / 2)
	    pp.cen = p / 2;
	ppgo = pp.cen - p / 2;

	a = (float *) malloc (p * sizeof(float *));
	xp = (float *) malloc (p * sizeof(float *));

	for (i = 0; i < p; i++)
	{
	    x = ppgo + i;
	    data_to_ieee (&pp.base[x], ieee_out);
	    xp[i] = x;
	    if (pp.intensity)
		a[i] = ieee_out[0] * ieee_out[0]
		    + ieee_out[1] * ieee_out[1];
	    else 
	    {
		if (pp.real)
		    a[i] = ieee_out[0];
		else
		    a[i] = ieee_out[1];
	    }
	    if (pp.log == 1) 
		if (a[i] <= 0.)
		    a[i] = 1e-12;
	}
	if (pp.log)
	    for (i = 0; i < p; i++)
		a[i] = log (a[i]);

	plot_hist (a, p, xp);
}


/* plot_hist (data, points, xelements) ---------------------------------
	This routine draws a histogram-style plot of 'data', 'points'
	points long.  The values of each bin are given by 'xelements'.
*/

plot_hist (data, points, xelements)
float  *data, *xelements;
int     points;

#define XOFFSET	50
#define YOFFSET 40
#define YHEIGHT 800

{
	int     i, j, colors[5], p_y0;
	int     compress, expand, width, y0, y1, y2, x1, x2, zero_y;
	float   high, low, max, avg, min, vert_scale, tic_space;
	char    tic_label[25];
	int xoffset, yoffset, yheight;
	int font = 4;
	int black = 0;
	int red = 1;
	int yellow = 2;
	int blue = 3;
	int white = 4;
	static int first = 1;

	xoffset = minpix + XOFFSET;
	yoffset = minlin + YOFFSET;
	yheight = YHEIGHT;
/*
	colors[black ] = mgfcns ("black");
	colors[red   ] = mgfcns ("red");
	colors[yellow] = mgfcns ("yellow");
	colors[blue  ] = mgfcns ("blue");
	colors[white ] = mgfcns ("white");
	mgicms (0, 5, colors);
	if (first) {
	    first = 0;
	    mgifetchgf (font, "5x7");
	}
*/

	if (points <= XSIZE) {
		compress = 1;
		expand = XSIZE / points;
		width = expand * points;
	} else {
		compress = (points + XSIZE-1) / XSIZE;
		expand = 1;
		width = (points + compress/2) / compress;
	}
	tic_space = width / 10.0;

	cg (1);
/*
	mgihue (white);
	mgigf (font);
	mgil (xoffset, yoffset, xoffset, yoffset + yheight);
	for (i = 0; i <= 10; i++)
		mgil (xoffset, yheight / 10 * i + yoffset,
				xoffset - 5, yheight / 10 * i + yoffset);
	mgil (xoffset, yoffset, xoffset + width, yoffset);
	for (i = 0; i <= 10; i++) {
		x1 = xoffset + 0.5 + tic_space * i;
		mgil (x1, yoffset - 5, x1, yoffset);
		if (!(i & 1)) {
			if (i == 10) sprintf (tic_label, "%-.4g",
						xelements[points - 1]);
			else sprintf (tic_label, "%-.4g",
						xelements[i * points / 10]);
			mgigfs (x1, yoffset - 20, 0, tic_label);
		}
	}
*/
	low = data[0];
	high = data[0];
	for (i = 0; i < points; i++) {
		if (data[i] < low) low = data[i];
		if (data[i] > high) high = data[i];
	}
	if (high == low) {
		if (low < 0) high = 0 ;
		else if (low > 0) low = 0;
		else {
			high = 1.0;
			low = -1.0;
		}
	}
		
	vert_scale = (high - low) / yheight;
	zero_y = (-low) / vert_scale;
/*
	for (i = 0; i <= 10; i++) {
		sprintf (tic_label, "%-.3g", low + i * (high - low) / 10.);
		mgigfs (minpix + XOFFSET - 45, yoffset + i * yheight / 10,
				0, tic_label);
	}
*/

	for (i = 0; i < points; i += compress) {
		j = 0;
		avg = 0;
		max = data[i];
		min = data[i];
		while (j < compress) {
			if (data[i + j] > max) max = data[i + j];
			if (data[i + j] < min) min = data[i + j];
			avg += data[i + j];
			j++;
		}
		avg = avg / compress;
		y0 = (min - low) / vert_scale;
		y1 = (avg - low) / vert_scale;
		y2 = (max - low) / vert_scale;
		x1 = (i / compress) * expand;
		x2 = ((i / compress) + 1) * expand;
		if (i == 0) p_y0 = y0;
/*
		mgihue (yellow);
		if (expand > 1) {
			mgil (x1 + xoffset + 1, p_y0 + yoffset + 1,
				x1 + xoffset + 1, yoffset + 1 + y0);
			mgil (x1 + xoffset + 1, yoffset + 1 + y0,
				x2 + xoffset, yoffset + 1 + y0);
		} else {
			mgibox (x1 + xoffset + 1, p_y0 + yoffset + 1,
				x2 + xoffset, yoffset + 1 + y0);
			mgihue (red);
			mgibox (x1 + xoffset + 1, yoffset + 1 + y0,
				x2 + xoffset, yoffset + 1 + y1);
			mgihue (blue);
			mgibox (x1 + xoffset + 1, yoffset + 1 + y1,
				x2 + xoffset, yoffset + 1 + y2);
		}
*/
		p_y0 = y0;
	}
/*
	mgihue (white);
	if ((low < 0) && (high >= 0))
		mgil (xoffset, yoffset + zero_y,
			xoffset + width, yoffset + zero_y);
*/
	return;
}
