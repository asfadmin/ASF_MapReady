/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  mbaim.c -- extract and display an 8-bit image 
	    from data in the mb array
*/

#include <stdio.h>
#include <fcntl.h>
#include <aspdecl.h>
#include <aspimg.h>
#include <math.h>

#define MEND_EXEC 0x100000
#define RMODE_MEM_READ 0
#define RMODE_SYNC_READ 0
#define FMAX 0x7fffffff
#define FMIN 0x80000000
#define IMGSIZE 262144
#define ISHIFT 22
#define MBADDRBUF 0x70000		/* image file data buffer offset */

/* data retrieval routine parameters */
static int off[MAXLINELEN * 2];		/* line offset table */
static int page_sw = 0;			/* 1=this line covers 2 pages */
				    	/* (set to 0 for imaging files */
static int pgaddr;			/* current page address */
static int page_end;			/* end index of current page */
static int mbaddr;			/* starting line offset */
static int mbaddr2;			/* 2nd page line offset */

static int ivtab[MAXLINELEN];		/* table to hold 1 data line */

static unsigned char sqroot[65536];     /* table for 64K values of sqroot */
static int ifp;				/* input image file descriptor */

/* im (start,x,y,npix,nlin,compx,compy,comptype) -----------------------
    This routine extracts one of several types of data from the
    mb array and formats it as an array of 8-bit values in "image".
    The output data is then displayed as an image on the graphics
    screen.

    Global variables from aspimg.h are used to define the input data
    format and type of data to extract.  The data in the mb array
    is treated as a 2-dimensional array.  The width of the array is
    defined by idline.  The length of a sub-line is given by idlen
    (for purposes of unscrambling from hi-lo or bit-reversed order).
    From each array data line npix pixels are extracted, starting
    with the data value at x,y.  nlin lines are extracted.
*/
im (start, x, y, npix, nlin, compx, compy, comptype)
int start;	/* initial byte offset into the memory (file) */
int x, y;	/* x,y coordinates of first sample to be displayed */
int npix;	/* number of samples to display (x dimension) */
int nlin;	/* number of lines of length npix to display ( y dimension) */
int compx,compy;    	/* compression factors */
int comptype;		/* compression type */
{
    char image[IMGSIZE];
    int i, j, c, l, end, inc, inc1, inc2, ival, linelen, linelen2;
    int mode, csize, npout, nlout, lend, comp_flag, xend, mask;
    int nl, np, nlp, nlpow, cx, cy, a, aend; 
    int pid;	/* location to save current PID */
    int disp;	/* display mode = display type for current data type */
    char *p, *pstart, *pend, *imagend;
    int ipix[MAXLINELEN], *ip, *ipend, *iv, *ivend;
    int ifinc;
    float ffinc;
    double dval, dmax, dmin, im_get_log ();
    int lcount = 0;
    int buftab[MAXLINELEN];		/* table to save contents of user memory */
    long lseek ();

/* open graphics mode */
    if (init_graphics() == 0) {
	printf("not a graphics terminal\n");
	return;
    }
/* set correct response board mode */
    if (isswitch) {
	pid = mb.w[RLOC_REP];
	if (idrel) {
	    mb.w[RLOC_REP] = PID_EXEC;
	    mode = (idtype == 0xD) ? RMODE_SYNC_READ : RMODE_MEM_READ;
	    ex_set_resp_mode (mode);
	}
    }
/* open input image file */
    else {
	if ((ifp = open (isource,O_RDONLY,NULL)) == -1) {
	    printf ("mbaim.c: Cannot open file %s\n",isource);
	    return;
	}
	else {
	    getdata (buftab,MBADDRBUF,MAXLINELEN);	/* save user data from buffer space */
	    page_sw = 0;				/* line is always within the buffer */
	}
    }
/* initialize the square root table */
    sqrootgen();
/* calculate processing parameters */
    if (compx == -1 || compx == 0)	/* actual x compress factor */
	compx = 1;
    if (compy == -1 || compy == 0)	/* actual y compress factor */
	compy = 1;
    np = (compx < 1) ? 1 : compx;   /* # of x samples per pixel */
    nl = (compy < 1) ? 1 : compy;   /* # of y samples per pixel */
    nlp = (comptype == 0) ? nl * np : 1; /* # samples per pixel */
    nlpow = sig_bits(nlp);		/* log2 of nlp */
    npout = npix / np;		/* # of pixels output */
    nlout = nlin / nl;		/* # of lines output */
    if ((linelen = idline) == -1)   /* data samples per line in */
	linelen = x + npix;
    if (linelen > MAXLINELEN) {
	linelen = MAXLINELEN;
	printf("Reducing line length to %d\n",linelen);
    }
    inc = (idtype == 8) ? 1 : 2;    /* sample spacing: 1 = one byte, 2 = compl. (4 bytes) */
    linelen *= inc;			/* data samples in short int */
    x *= inc;			/* in short int */
    xend = x + (npix * inc);	/* in short int */
    im_calc_offsets (linelen, inc);
    linelen2 = linelen * inc;		/* data samples in bytes */
    start += y * linelen2;			/* in bytes */
    end = start + (linelen2 * nlin);	/* in bytes */
    if (isswitch)
	if (idrel == 0 && end > MEND_EXEC) {
	    printf ("im command: image too big, not displayed\n");
	    return;
	}
    /*
    else {
	printf ("image source file %s too big, not displayed\n",isource);
	return;
    }
    */

/* determine which display mode to use */
    disp = (ifdisps[idtype] == 3) ? 3 : ifdisp; /* previous or new */
    ifdisps[idtype] = (disp == 1) ? 3 : disp;   /* save mode for next invokation */
						/* 3 not to calc min/max over entire */
						/* image again                       */

/* (disp == 0) means use entire dynamic range */
    if (disp == 0) {
	ifmax = 255;
	ifmin = 0; 
	if (idtype == 0xA || idtype == 0xB) {
	    ifmax = 0x7fff;
	    ifmin = -0x8000;
	}
	if (idtype == 0xF) ifmax = 0xffff;
	if (idtype == 0xC) ifmax = 0x1fffffff / nlp;
    }

/* (disp == 1) means compute min and max from all data in swath */
/* (disp == 2) means compute min and max from displayed data */
    if (disp == 1 || disp == 2) {
	if (end - start > 250000) printf("computing min & max ...\n");
	ifmin = FMAX;
	ifmax = FMIN;
	a = (disp == 1) ? 0 : x;
	aend = (disp == 1) ? linelen : xend;
	ivend = ivtab + (aend - a) / inc;
	for (j = start; j < end; j += linelen2) {
	    im_set_resp_line (j, linelen2, inc);
	    im_get_resp_line (a, inc, aend, ivtab, nlpow);
	    for (iv = ivtab; iv < ivend; iv += inc) {
		if (*iv < ifmin) ifmin = *iv;
		if (*iv > ifmax) ifmax = *iv;
	    }
	}
    }

/* (disp == 3) means use the existing min & max */
    if (disp == 3) {
	ifmin = ifmins[idtype];
	ifmax = ifmaxes[idtype];
    } else {
	ifmins[idtype] = ifmin;
	ifmaxes[idtype] = ifmax;
    }

/* (disp == 4) means data will be masked, not stretched */

/* calculate intensity stretch value */
    if (disp != 4) {
	if (ifscale) {
	    dmax = im_get_log (dval = ifmax);
	    dmin = im_get_log (dval = ifmin);
	    if ((dval = dmax - dmin) == 0.0) dval = 1.0;
	    ffinc = 255.0 / dval;
	} else {
	    ifmax *= nlp;
	    ifmin *= nlp;
	    if ((ival = ifmax - ifmin) == 0) ival = 1;
	    ifinc = (255 << ISHIFT) / ival;
	}
    }  /* if disp != 4 */

    printf("ifmin=%d, ifmax=%d\n",ifmin,ifmax);

/* calculate the chunk size */
    csize = IMGSIZE / npout;
    comp_flag = (compx > 1) || (compy > 1);
    cx = (compx > 1) ? 1 : compx;
    cy = (compy > 1) ? 1 : compy;

/* build and display image */
    l = start;
    xend = x + (npix * inc);
    ivend = &ivtab[npix];
    for (c = 0; c < nlout; c += csize) {
	if ((nlout - c) < csize) csize = nlout - c;
	imagend = image + (npout * csize);
    /* build one chunk of image */
	for (pstart = image; pstart < imagend; pstart = pend) {
	    pend = pstart + npout;
	    ipend = ipix + npout;
	/* compute one line of pixel values */
	    if (comp_flag) {		/* compressing */
	    /* clear the value array */
		ival = 0;
		if (comptype == 1) ival = FMAX;
		if (comptype == 2) ival = FMIN;
		for (ip = ipix; ip < ipend;) *ip++ = ival;
		lend = l + nl * linelen2;
	    /* retrieve the values from the data buffer */
		while (l < lend) {
		    register int *ip, *iv;
		    im_set_resp_line (l,linelen2,inc);
		    l += linelen2;
		    im_get_resp_line (x, inc, xend, ivtab, nlpow);
		    iv = ivtab;
		    switch (comptype) {
			case 0:			/* average */
			    for (ip = ipix; ip < ipend; ip++)
				for (j = 0; j < np; j++) *ip += *iv++;
			    break;
			case 1:			/* minimum */
			    for (ip = ipix; ip < ipend; ip++)
				for (j = 0; j < np; j++) {
				    if (*ip > *iv) *ip = *iv;
				    iv++;
				}
			    break;
			case 2:			/* maximum */
			    for (ip = ipix; ip < ipend; ip++)
				for (j = 0; j < np; j++) {
				    if (*ip < *iv) *ip = *iv;
				    iv++;
				}
			    break;
		    }  /* switch */
		}  /* while l */
	    }  /* if comp_flag */
	    else {			/* not compressing */
		im_set_resp_line (l,linelen2,inc);
		l += linelen2;
		im_get_resp_line (x, inc, xend, ipix, nlpow);
	    }  /* else */
	/* take average, if needed */
	/* don't take average - min and max are increased
	    if (nlp > 1) for (ip = ipix; ip < ipend;) *ip++ /= nlp;
	*/
	/* convert values on line to pixels */
	    if (ifdisp == 4) {
	    /* mask values against bit mask */
		register char *p;
		register int *ip;
		register int mask;
		mask = ifmask;
		for (p = pstart, ip = ipix; p < pend;) *p++ = (*ip++) & mask;
	    } else {
	    /* scale value between 0 and 255 */
		register char *p;
		register int *ip;
		if (ifscale)
		    for (p = pstart, ip = ipix; p < pend;)
			*p++ = (im_get_log (dval=(*ip++)) - dmin) * ffinc;
		else
		    for (p = pstart, ip = ipix; p < pend;) {
			if (*ip >= ifmax) *p++ = 255;
			else if (*ip <= ifmin) *p++ = 0;
			    else *p++ = ((*ip - ifmin) * ifinc) >> ISHIFT;
			    ip++;
			}
		}  /* else */
	    }  /* for pstart */
	/* display the chunk */
	    printf("npout=%d, csize=%d, cx=%d, cy=%d\n",npix,csize,cx,cy);
	    show_im (image, npout, csize, cx, cy, comptype);
	}  /* for c */

    /* restore PID or close input image file */
	if (isswitch) mb.w[RLOC_REP] = pid;
	else {
	    close (ifp);
	    putdata (buftab,MBADDRBUF,MAXLINELEN);	/* restore user data in buffer space */
        }
}

/* im_get_log (d) ------------------------------------------------------
	This routine returns the natural log of d.  If d is zero,
	the routine returns zero.  If d is negative, the routine
	returns the negative of the log of the absolute value of d.
*/
double im_get_log (d)
double d;
{
	if (d > 0.0) return (log (d));
	if (d == 0.0) return (0.0);
	return (- log (- d));
}

/* im_calc_offsets (linelen, inc) --------------------------------------
	This routine builds a table of line offsets for use by the
	image building routine.  The table gives the offset needed to
	retrieve the first, second, third, etc. sequential data point
	on the line, compensating for whatever actual order the data
	are arranged in.
*/
im_calc_offsets (linelen, inc)
int linelen, inc;
{
	int i, j, k, inc1, inc2, sublen, bump;

	sublen = (idlen == -1) ? linelen : idlen * inc;
	if (sublen > linelen) sublen = linelen;
	for (i = MAXLINELEN * 2; i > 0; i >>= 1) {
	    if (sublen & i) break;
	}
	if (i != sublen) idform &= 0x0c;
    /* build the basic order */
	switch (idform) {
	    case 0:			/* sequential */
	    case 4:			/* sequential mux */
		for (i = 0; i < linelen; i += inc) off[i] = i;
		break;
	    case 1:			/* bit reversed */
	    case 5:			/* bit reversed mux */
		for (j = 0; j < linelen; j += sublen) {
		    off[j] = j;
		    bump = inc;
		    for (inc1 = sublen; inc1 > inc; inc1 = inc2) {
			inc2 = inc1 >> 1;
			for (i = inc2; i < sublen; i += inc1)
			    off[j+i] = off[j+i-inc2] + bump;
			bump <<= 1;
		    }
		}
		break;
	    case 2:			/* hi-lo */
	    case 6:			/* hi-lo mux */
		inc2 = linelen / 2;
		for (k = 0; k < linelen; k += sublen) {
		    for (i=0, j=0; i < inc2; i += inc, j += 2*inc) {
			off[k+j] = i;
			off[k+j+inc] = i + inc2;
		    }
		}
		break;
	}  /* switch */

    /* if complex data, fill in the imaginary side */
	if (inc == 2)
	    for (i = 0; i < linelen; i += 2) off[i+1] = off[i] + 1;

    /* if mux format, modify the order */
	if (idform > 3) {
	    for (i = 0; i < linelen; i += 4) {
		j = off[i+1];
		off[i+1] = off[i+2];
		off[i+2] = j;
	    }
	}
}

/* im_set_resp_line (line, linelen, inc) -------------------------------
	This routine sets the current line parameters for data
	retrieval by the im_get_resp_val routine.
	In case of file access, it fetches required data.
*/
im_set_resp_line (linest, linelen, inc)
int linest, linelen, inc;
{
	if (isswitch)	/* EXEC */
	    if (idrel) {
		ex_get_resp_addr (linest, &pgaddr, &mbaddr);
		if (page_sw = (mbaddr + linelen >= MEND_EXEC)) {
		    mbaddr2 = (MLOC_EXEC - (MEND_EXEC - mbaddr)) / inc;
		    page_end = (MEND_EXEC - mbaddr) / inc;
		}
		ex_set_resp_page (pgaddr);
	    }
	    else mbaddr = linest;
	else {		/* file */
	    lseek (ifp, (long) linest,0);
	    read (ifp, &mb.b[MBADDRBUF], linelen);
	    mbaddr = MBADDRBUF;
	}
	mbaddr /= inc;
}

/* im_get_resp_val (i) -------------------------------------------------
	This routine retrieves data value i from the current data line.
	The type of data returned is defined by the global variable
	idtype.
*/
im_get_resp_val (i)
int i;
{
	int j, rval, ival, loc;
	static int first = 1;

	if (page_sw) {
	    ex_set_resp_page (pgaddr + (off[i] >= page_end));
	    j = (off[i] >= page_end) ? mbaddr2 : mbaddr;
	}
	else j = mbaddr;
	switch (idtype) {
	    case 0:
	    case 1:
	    case 2:
	    case 3:
		j += off[i + (idtype >> 1)];
		asp_read( j<<1, &mb.b[j<<1], 2 );
		ival = mb.b[(j << 1) + (idtype & 1)] & 0xff;
		break;
	    case 8:			/* consecutive bytes */
		j += off[i + (idtype >> 1)];
		loc = ( j+off[i] ) & 0xfffffffe;
		asp_read( loc, &mb.b[loc], 2 );
		ival = mb.b[j + off[i]] & 0xff;
		break;
	    case 0xA:			/* real value */
		loc = j+off[i];
		asp_read( loc<<1, &mb.w[loc], 2 );
		ival = mb.w[j + off[i]];
		break;
	    case 0xB:			/* imaginary value */
		loc = j+off[i+1];
		asp_read( loc<<1, &mb.w[loc], 2 );
		ival = mb.w[j +off[i+1]];
		break;
	    case 0xC:			/* intensity */
		loc = j+off[i];
		asp_read( loc<<1, &mb.w[loc], 4 );
		rval = mb.w[j + off[i]];
		ival = mb.w[j + off[i+1]];
		ival = rval * rval  +  ival * ival;
		break;
	    case 0xD:			/* sync code */
		asp_read( j<<1, &mb.w[j], 4 );
		ival = mb.w[j] + off[i] & 0x03;
		break;
	    case 0xE:			/* square root of real value */
		loc = j+off[i];
		asp_read( loc<<1, &mb.w[loc], 2 );
		ival = mb.w[j + off[i]] & 0xffff;
		ival = sqroot[ival];
		break;
	    case 0xF:			/* real value in unsigned notation */
		loc = j+off[i];
		asp_read( loc<<1, &mb.w[loc], 2 );
		ival = mb.w[j + off[i]] & 0xffff;
		break;
	    default:
		ival = 0;
		if (first) printf("invalid idtype: %d\n",idtype);
		break;
	}  /* switch */
	return (ival);
}

/* im_get_resp_line (xgo, inc, xend, ipix, nlpow) ----------------------
	This routine retrieves the relevant data to be displayed from
	the current data line.
*/
im_get_resp_line (xgo, inc, xend, ipix, nlpow)
int xgo, inc, xend, *ipix, nlpow;
{
	int page, prev_page, rval, ival;
	register int j, x;
	int bytoff = 0;

	if (page_sw) {
	    prev_page = page = pgaddr + (off[xgo] >= page_end);
	    ex_set_resp_page(page);
	    j = (page == pgaddr) ? mbaddr : mbaddr2;
	}
	else j = mbaddr;
	switch (idtype) {
	    case 2:			/* one byte out of 4 */
	    case 3:
		bytoff = off[xgo+1] - off[xgo];
	    case 0:
	    case 1:
		bytoff += (idtype & 1);
		j = j << 1 + bytoff;
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			    j = j << 1 + bytoff;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    *ipix++ = mb.w[j + off[x]] & 0xff;
		}
		break;
	    case 8:			/* consecutive bytes */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    *ipix++ = mb.b[j + off[x]] & 0xff;
		}
		break;
	    case 0xA:			/* real value */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    *ipix++ = mb.w[j + off[x]];
		}
		break;
	    case 0xB:			/* imaginary value */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x+1])<<1, &mb.w[j+off[x+1]], 2 );
		    *ipix++ = mb.w[j + off[x+1]];
		}
		break;
	    case 0xC:			/* intensity */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    rval = mb.w[j + off[x  ]];
		    asp_read( (j+off[x+1])<<1, &mb.w[j+off[x+1]], 2 );
		    ival = mb.w[j + off[x+1]];
		    *ipix++ = (rval * rval  +  ival * ival) >> nlpow;
		}
		break;
	    case 0xD:			/* sync code */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    *ipix++ = mb.w[j + off[x]] & 0x03;
		}
		break;
	    case 0xE:			/* square root of real value */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    *ipix++ = sqroot[mb.w[j + off[x]] & 0xffff];
		}
		break;
	    case 0xF:			/* real value (unsigned) */
		for (x = xgo; x < xend; x += inc) {
		    if (page_sw) {
			page = pgaddr + (off[x] >= page_end);
			if (page != prev_page) {
			    ex_set_resp_page (prev_page = page);
			    j = (page == pgaddr) ? mbaddr : mbaddr2;
			}
		    }
		    asp_read( (j+off[x])<<1, &mb.w[j+off[x]], 2 );
		    *ipix++ = mb.w[j + off[x]] & 0xffff;
		}
		break;
	    default:
		break;
	}  /* switch */
}

/* sig_bits(ival) ------------------------------------------------------
	This routine returns the number of significant bits in ival.
*/
sig_bits (ival)
long int ival;
{
	register long int i;
	register int ans = 1;

	i = abs(ival);
	while (i >>= 1) ans++;
	return (ans);
}

/* dump_img_data() ---------------------------------------
	This routine prints a debugging dump of the 
	data in the aspimg.h area.
*/
dump_img_data()
{
	static char *dttab[] = { "0: byte 1 of 4",
				 "1: byte 2 of 4",
				 "2: byte 3 of 4",
				 "3: byte 4 of 4",
				 "4: undefined",
				 "5: undefined",
				 "6: undefined",
				 "7: undefined",
				 "8: consecutive bytes",
				 "9: undefined",
				 "a: real two's compl",
				 "b: imaginary two's compl",
				 "c: intensity",
				 "d: sync codes",
				 "e: real square root",
				 "f: real unsigned",
				 };
	static char *dftab[] = { "0: sequential",
				 "1: bit-reversed",
				 "2: hi-lo",
				 "3: undefined",
				 "4: sequential mux",
				 "5: bit-reversed mux",
				 "6: hi-lo mux",
				 };
	static char *sctab[] = { "0: linear",
				 "1: logarithmic",
				 };
	static char *dstab[] = { "0: max dynamic range",
				 "1: min/max from all data",
				 "2: min/max from displayed data",
				 "3: previous or user-supplied min/max",
				 "4: bit mask",
				 };

	printf ("   image source = %s\n",isource);
	printf ("      data type = %s\n",dttab[idtype]);
	printf ("    data format = %s\n",dftab[idform]);
	printf ("    line length = %d\n",idline);
	printf ("sub-line length = %d\n",idlen);
	printf ("        scaling = %s\n",sctab[ifscale]);
	printf ("  display range = %s\n",dstab[ifdisp]);
	if (ifdisp == 4) printf ("   display mask = %x\n",ifmask);
	else {
	    if (ifdisp == 3)
		printf ("      min / max = %d / %d\n",ifmins[idtype]
					,ifmaxes[idtype]);
	    else printf ("      min / max = %d / %d\n",ifmin,ifmax);
	}
	printf ("screen location = %x %x\n",imagex,imagey);
	if (iglo == 0 && ighi == 0) printf ("    color table = rainbow\n");
	else printf ("greyscale lo/hi = %x / %x\n",iglo,ighi);
}

/* sqrootgen() ---------------------------------------------------------
	This routine generates a 16-bit square root lookup table
	(64K entries).
*/
sqrootgen ()
{
    short int a, d;
    int b, c;
    static first = 1;

    /* generate square root table in expanded format */

    if (first) {
	sqroot[0] = 0;
	for (a = 1, d = 2; a < 256; a++, d++) {    /* d = a + 1 */
	    c = a*d;
	    for (b = a*a; b <= c; b++) sqroot[b] = a;
	    for (b = c + 1; b < d*d; b++) {
		if (d < 255) sqroot[b] = d;
		else sqroot[b] = 255;
	    }
	}
    }
    first = 0;
}
