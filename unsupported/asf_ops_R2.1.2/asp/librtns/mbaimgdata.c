/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbaimgdata.c -- ASP image routines global variables */

char isource[80] = "EXEC";	/* image source filename, or EXEC   */
int isswitch = 1;		/* 1 if EXEC is source, 0 if file   */

int idtype = 0xC;		/* data type: 0-3 = byte in rsp.buf */
				/*	      8 = consecutive bytes */
				/*	      A = real value (2's)  */
				/*	      B = imaginary value   */
				/*	      C = intensity         */
				/*	      D = sync codes        */
				/*	      E = sqroot of real    */
				/*	      F = unsigned real     */

int idform = 0;			/* data format: 0 = sequential     */
				/*		1 = bit-reversed   */
				/*		2 = hi-lo          */
				/*		4 = seq. mux       */
				/*		5 = bit-rev. mux   */
				/*		6 = hi-lo mux	   */

int idline = -1;		/* data line length */

int idlen = -1;			/* data sub-line length */

int idrel = 0;			/* addressing mode: 0 = absolute */
				/*		    1 = relative */

int ifscale = 0;		/* display scale: 0 = linear */
				/* 		  1 = log    */

int ifdisp = 0;			/* display type:		      */
				/*    0 = entire range                */
				/*    1 = min/max from all data       */
				/*    2 = min/max from displayed data */
				/*    3 = min/max from user or previous calc */
				/*    4 = bit mask                    */

int ifmask = 0xff;		/* display data bit-mask (8-bit) */

int ifmin,ifmax;		/* display min & max value range */

int ifmaxes[16], ifmins[16];	/* min & max for each data type */

int ifdisps[16] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
				/* display type for each data type */

int imagex = 0, imagey = 0;	/* screen loc of image upper left corner */

int iglo = 0, ighi = 0;		/* greyscale stretch limits (0,0 = rainbow) */
