/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaplot.c -- line plot routine */

#include <aspdecl.h>
/*#include <libmp.h>*/
#define SIZEOFGCA 1000

/* plot (pp) ----------------------------------------------------
	This routine extracts and formats data for a line plot, and then
	passes that data to the Masscomp plot routines for plotting.
*/
plot(pp)
PLOTPARAMS pp;
{
	int i, j, p, ppgo, x[512];
	int gls[SIZEOFGCA];
	char *command;
	static char logplot[] = "log -b10.00 \0";
	float ieee_out[2], in[512], re[512], im[512], lowx, highx;
	float intensity;

    /* validate incoming parameters */
	if (pp.cmprss <= 0) pp.cmprss = 1;
	if (pp.cmprss > (MAXFFTLEN / 512)) {
	    pp.cmprss = MAXFFTLEN / 512;
	    printf ("reducing compression to %d\n",pp.cmprss);
	}
	p = 512 * pp.cmprss;
	
	if (pp.cen < p / 2) pp.cen = p / 2;
	if (pp.cen > MAXFFTLEN - p / 2) pp.cen = MAXFFTLEN - p / 2;
	ppgo = pp.cen - p / 2;

    /* extract data */
	for (i = 0; i < 512; i++) {
	    x[i] = p = ppgo + (pp.cmprss * i);
	    in[i] = re[i] = im[i] = (pp.avg == 1) ? 0. : -4.3e9;
	    for (j = 0; j < pp.cmprss; j++) {
		data_to_ieee (&pp.base[p + j], ieee_out);
		if (pp.intensity) {
		    intensity = ieee_out[0]*ieee_out[0]
			      + ieee_out[1]*ieee_out[1];
		    if (pp.avg == 1) in[i] += intensity;
		    else if (in[i] < intensity) in[i] = intensity;
		}
		if (pp.imag) {
		    if (pp.avg == 1) im[i] += ieee_out[1];
		    else if (im[i] < ieee_out[1]) im[i] = ieee_out[1];;
		}
		if (pp.real) {
		    if (pp.avg == 1) re[i] += ieee_out[0];
		    else if (re[i] < ieee_out[0]) re[i] = ieee_out[0];
		}
	    }
	    if (pp.log == 1) {
		if (pp.intensity & (in[i] <= 0.)) in[i] = 1e-12;
		if (pp.real & (re[i] <= 0.)) re[i] = 1e-6;
		if (pp.imag & (im[i] <= 0.)) im[i] = 1e-6;
	    }
	}
	lowx = x[0];
	highx = x[511];

    /* plot the data as a line plot */

printf("MBA_PLOT:  MP routines have not been ported to R1B yet\n");
sleep( 5 );
/*
	mpinit(gls);
	mpdevice(gls, "mcd", 2, 0);
	mpfile(gls, "mbplot.tmp", 1, 2);
	command = (pp.log == 1) ? logplot : NULL;
	if (pp.intensity != 0) {
	    mplotsrcy(gls, 1, 512, 0, in, "F", 1, 1, command, NULL);
	    mplotsrcx(gls, 1, 512, 0, x, "I", 1, 1, NULL, NULL);
	}
	if (pp.real != 0) {
	    mplotsrcy(gls, 2, 512, 0, re, "F", 1, 1, command, NULL);
	    mplotsrcx(gls, 2, 512, 0, x, "I", 1, 1, NULL, NULL);
	}
	if (pp.imag != 0) {
	    mplotsrcy(gls, 3, 512, 0, im, "F", 1, 1, command, NULL);
	    mplotsrcx(gls, 3, 512, 0, x, "I", 1, 1, NULL, NULL);
	}
	mplot(gls, 0, 1, 0);
	mpend(gls);
*/
	return;
}
