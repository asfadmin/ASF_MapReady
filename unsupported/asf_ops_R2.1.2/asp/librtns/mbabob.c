/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbabob.c -- bob debugging tool command interpreter */

#define VERSION  "2.00"		/* set to current version number */
#define VDATE   "04-12-95"	/* set to current version date */

/* abob () -------------------------------------------------------------
	This routine comprises the bob language interpreter.  The bob
	language is a batch or interactive debugging language which
	accesses and manipulates a 1-megabyte array (the "mb" array).
	Within the mb array certain locations are mapped to registers
	and/or memory buffers in the hardware to be tested.  The
	remaining locations are local memory.  Bob has commands to
	display, set, copy and graph values contained in the mb array,
	and thus control, test and debug the hardware being tested.

	The routine can be called either in interactive mode (CALL = 0)
	or batch mode (CALL > 0).  When in interactive mode the routine
	prompts the user for each command.  When in batch mode, the
	routine reads each successive command from the file named in
	the external variable "name".

	While in bob, a bob command file can be executed by using the
	"b" command (batch).  This command can be used in batch files
	as well.  Batch files can thus call other batch files, up to
	10 levels deep.

	The routine continues to read commands until end-of-file is
	reached (batch mode), or until the user enters either the "br"
	command (break) or the "q" command (quit).  The routine then
	returns PASS unless the q command was entered, in which case 
	FAIL is returned.
*/

#include <aspdecl.h>
#include <aspimg.h>
#include <math.h>
#include <stdio.h>
#include <fcntl.h>
#include <time.h>

extern int no_break;		/* 0 = ctrl-C entered */

extern int  CALL;		/* flag for passing a file name to 
				   execute in name[]   */
char    s[80], name[80];
int     n[20], nn, cmd;

/* history table */
#define HLINES 101
char hist[HLINES][100];		/* last 100 lines entered */

/******************************************************************/
abob ()
{
    int     i, j, k, cn, hl, end, err, val, ilim;
    short int   js, n4;
    float   ieee_out[2], source[MAXFFTLEN], dest[MAXFFTLEN],
            diff, maxdiff, ieee, rdata, idata;
    DATA Dat[MAXFFTLEN];
    PLOTPARAMS pl;
    int     STOPONERR, VISUAL;
    FILE *hp;
    int imfp;		/* input image file descriptor */
/* batch file stack structure */
    struct {
	FILE   *fp;
	char    name[80];
	int     line;
	int     tm;		/*  number of triggers per T command */
	int	reps;		/*  times to repeat the command */
	int	rep;		/*  current repetition number */
    }       infile[10];
    int     curlev;
    struct tm *date;
    time_t loc_time;
    char current_time[80];

    loc_time = time( NULL );
    date = localtime(&loc_time);
    for( i=0; i<80; i++ ) current_time[i] = '\0';
    strftime( current_time, 80, "%a %c %Y", date );
/* say hello */
#ifdef DEBUG
    printf ("BOB DEBUGGING VERSION %s modified by Eugene Chu\n",VERSION);
#else
    printf ("\nEugene Chu's BOB Version %s -- %s\n\n",VERSION,VDATE);
#endif

/* initialize routine variables */
    end = 0;
    cn = 1;
    curlev = 0;
    infile[0].fp = stdin;
    infile[0].name[0] = NULL;
    infile[0].line = 0;
    infile[0].tm = 1;
    err = PASS;
    VISUAL = 0;
    STOPONERR = 0;

/* if in batch mode, open the batch file */
    if (CALL) {
	curlev++;
	infile[curlev].line = 0;
	infile[curlev].tm = 1;
	strcpy (infile[curlev].name, name);
	if( !(infile[curlev].fp=fopen( infile[curlev].name, "r") ) ){
	    printf ("File %s does not exist\n",
		    infile[curlev].name);
	    err = FAIL;
	    curlev--;
	}
    }

printf("Current time=%s, current level=%d\n", current_time, curlev );
/* ----Top of command loop---- */
    while (cn) {
    /* if error encountered, close all batch files */
	if (err != PASS) {
	    for (i = curlev; i > 0; i--) {
		printf ("\nFile %s    ", infile[i].name);
		printf ("rep. %d    ", infile[i].rep);
		printf ("line %d\n", infile[i].line);
	    }
	    if (STOPONERR) {
		while (curlev > 0) {
		    fclose (infile[curlev].fp);
		    curlev--;
		}
	    }
	    err = PASS;
	}
	infile[curlev].line++;
	if ((curlev == 0) || (VISUAL))	    /* display prompt */
	    printf ("%d>", infile[curlev].line);
	else				    /* display dots instead */
	    if ((infile[curlev].line & 0xf) == 0) {
		printf (".");		      /* 1 dot ea. 16 cmds */
		if ((infile[curlev].line & 0x3ff) == 0)
		    printf ("\n");	      /* new line ea. 64 dots */
		fflush (stdout);
	    }

    /* read next input line */
	while ((fgets (s, 80, infile[curlev].fp) == NULL) && curlev > 0) {
	/* if any batch file(s) at EOF, close it (them) */
	    if (--infile[curlev].reps > 0) {
		rewind(infile[curlev].fp);
		infile[curlev].line = 0;
		infile[curlev].rep++;
	    } else {
		fclose (infile[curlev].fp);
		curlev--;
	    }

	    if (curlev == 0 && CALL){	/* if top file closing, exit */
		CALL = 0;
		return (end);
	    }

	    printf ("\n");
	    infile[curlev].line++;
	    if ((curlev == 0) || (VISUAL))
		printf ("%d>", infile[curlev].line);
	}
	if ((VISUAL) && (curlev != 0))   /* print batch command */
	    printf ("%s", s);

    /* do history stuff */
	if (curlev == 0) {
	/* if history command, get the actual command requested */
	    if (s[0] == '!') {
		if (s[1] == '!') i = infile[0].line - 1;
		else i = atoi(&s[1]);
		if (i < 0) i = infile[0].line + i;
		ilim = infile[0].line - HLINES;
		if (ilim < 1) ilim = 1;
		if (i == 0) {
		    j = strlen(s + 1) - 1;
		    for (i = infile[0].line - 1; i >= ilim; i--) {
			if (strncmp(hist[i % HLINES], &s[1], j) == 0)
			    break;
		    }
		}
		if (i < ilim) {
		    printf("history: line %d not available\n",i);
		    s[0] = '\0';
		} else {
		    strcpy(s,hist[i % HLINES]);
		    printf(s);
		}
	    }
	    strcpy(hist[infile[0].line % HLINES],s);
	}

	ascan (s);			/* parse the command */

    /* interpret the command */
    /*
#ifdef DEBUG
	    printf ("cmd = %d\n",cmd);
#endif
    */
	switch (cmd) {
	    /* set word (no byte swap) */
	    case 0: 				/* s */
		i = n[0];
		n[0] &= 0xfffff;
		mb.w[n[0] >> 1] = n[1];
		if( i<4096 || i>=0x80000 ) asp_write( i, &mb.w[ n[0]>>1 ], 2 );
		break;

	    /* set word in stimulus buffer */
	    case 1:				/* ss */
		fs (n[0], n[0] + 2, n[1]);
		break;

	    /* display word */
	    case 2:				/* d */ 
/*
		if ((n[0] > 0xfffff) || (n[1] > 0xfffff)) {
		    printf ("Address too big\n");
		    break ;
		}
*/
		if (n[1] < 1) n[1] = 1;
		set_break ();
		i = n[0];
		n[0] &= 0xfffff;
		mb.w[0] = (i>>20) & 0xf;
		if( i<4096 || i>=0x80000 ) asp_read( i, &mb.w[n[0]>>1], n[1]*16 );
		for (i = n[0], j = 0; j < n[1] && no_break; j++) {
		    printf("%.6X:  ",(mb.w[0]<<20)|i);
		    for (k = i + 16; i < k; i += 4) 
			printf("%.4X %.4X  ",mb.w[i >> 1] & 0xffff,
				   mb.w[(i >> 1) + 1] & 0xffff);
		    printf("\n");
		}
		break;
		
	    /* display response */
	    case 3:				/* dr */
		if (n[1] == 0) n[1] = 1;
		dr (n[0], n[1]);
		break;

	    /* display floating point */
	    case 4: 				/* df */
		if ((n[0] > 0xfffff) || (n[1] > 0xfffff)) {
		    printf ("Address too big\n");
		    break ;
		}
		if (n[1] == 0) n[1] = 1;
		set_break ();
		for (i = 0, j = n[0] >> 1; i < n[1] && no_break; i++) {
		    printf("%.6X:  ",j << 1);
		    rdata = mb.w[j++] / 32768.0;
		    idata = mb.w[j++] / 32768.0;
		    printf("% f % f    ",rdata,idata);
		    rdata = mb.w[j++] / 32768.0;
		    idata = mb.w[j++] / 32768.0;
		    printf("% f % f\n",rdata,idata);
		}
		break;

	    /* display floating point response */
	    case 5:				/* dfr */
		if (n[1] == 0) n[1] = 1;
		dfr (n[0], n[1]);
		break;

	    /* move words */
	    case 6: 				/* m */
		if ((n[0] & 1) && (n[1] & 1) && (n[2] & 1)) {
		    n[0]--;
		    n[1]--;
		    n[2]--;
		    if (n[3] & 1) n[3]--;
		}
		if (n[3] == 0) n[3] = n[2] + n[1] - n[0];
		if ((n[0] > 0xfffff)||(n[1] > 0xfffff)||
			(n[2] > 0xfffff)||(n[3] > 0xfffff)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		m (n[0], n[1], n[2], n[3]);
		break;

	    /* move response */
	    case 7:				/* mr */
		if ((n[0] > 0x1ffffff) || (n[1] > 0x2000000) ||
		    (n[2] > 0xfffff) || (n[3] > 0xfffff)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		if (n[3] == 0) n[3] = n[2] + n[1] - n[0];
		mr (n[0], n[1], n[2], n[3]);
		break;

	    /* compare */
	    case 8: 				/* c */
		if (n[3] == 0) n[3] = n[2] + n[1] - n[0];
		if ((n[0] > 0xfffff)||(n[1] > 0xfffff)||
			(n[2] > 0xfffff)||(n[3] > 0xfffff)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		err = c (n[0],n[1],n[2],n[3]);
		break;

	    /* compare response */
	    case 9:				/* cr */
		if ((n[0] > 0x1ffffff) || (n[1] > 0x2000000) ||
		    (n[2] > 0xfffff) || (n[3] > 0xfffff)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		err = cr (n[0], n[1], n[2], n[3]);
		break;

	    /* fuzzy compare */
	    case 10:				/* cf */
	    /* fuzzy compare response */
	    case 11:				/* cfr */
		printf ("fuzzy compare not currently implemented\n");
		break;

	    /* fill words */
	    case 12:				/* f */
		if ((n[0] > 0xfffff) || (n[1] > 0xfffff)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		if (n[0] > n[1]) {
		    printf ("Start greater than end\n");
		    break;
		}
		for (i = n[0] >> 1; i < n[1] >> 1; i++) mb.w[i] = n[2];
		break;

	    /* fill words in stimulus buffer */
	    case 13:				/* fs */
		if (n[0] > n[1]) {
		    printf ("Start greater than end\n");
		    break;
		}
		fs (n[0], n[1], n[2]);
		break;

	    /* fill words with counter */
	    case 14: 				/* fc */
		if (n[5] == 0) n[5] = 2;
		if ((n[0] > 0xfffff)||(n[1] > 0xfffff + n[5]*2)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		fc (n[0], n[1], n[2], n[3], n[4], n[5]);
		break;

	    /* fill words with counter, in stimulus buffer */
	    case 15:				/* fcs */
		fcs (n[0], n[1], n[2], n[3], n[4], n[5]);
		break;

	    /* line plot */
	    case 16: 				/* p */
		if (n[0] > 0xfffff) {
		    printf ("Invalid Address\n");
		    break ;
		}
	    /* line plot response */
	    case 17:				/* pr */
		if (init_graphics() == 0) {
		    printf("not a graphics terminal\n");
		    break;
		}
		if (nn < 6) {
		    printf ("Enter plot parameters:\n");
		    printf ("center,   intensity flag,  real flag,  ");
		    printf ("imaginary flag,\n\tavg_maxprime,");
		    printf ("  log flag,  compression :\n");
		    gets (s);
		    if (strlen(s) == 0) break;
		    strcat (s,"\n");
		    if (curlev == 0) {
			i = infile[0].line % HLINES;
			hist[i][strlen(hist[i])-1] = ' ';
			strcat (hist[i],s);
		    }
		    sscanf (s,"%x %x %x %x %x %x %x",
			    &pl.cen, &pl.intensity, &pl.real,
			    &pl.imag, &pl.avg,
			    &pl.log, &pl.cmprss);
		} else {
		    pl.cen = n[1];
		    pl.intensity = n[2];
		    pl.real = n[3];
		    pl.imag = n[4];
		    pl.avg = n[5];
		    pl.log = n[6];
		    pl.cmprss = n[7];
		}
		if (cmd == 16) getdata (Dat, n[0], MAXFFTLEN);
		else getrdata (Dat, n[0], MAXFFTLEN);
		pl.base = Dat;
		plot (pl);
		break;

	    /* bar plot */
	    case 18: 				/* p1 */
		if (n[0] > 0xfffff) {
		    printf ("Invalid Address\n");
		    break ;
		}
	    /* bar plot response */
	    case 19: 				/* p1r */
		if (init_graphics() == 0) {
		    printf("not a graphics terminal\n");
		    break;
		}
		if (nn < 6) {
		    printf ("Enter plot parameters:\n");
		    printf ("center,   intensity flag,  real flag,  ");
		    printf ("imaginary flag,\n\tavg_maxprime,");
		    printf ("  log flag,  compression :\n");
		    gets (s);
		    if (strlen(s) == 0) break;
		    strcat (s,"\n");
		    if (curlev == 0) {
			i = infile[0].line % HLINES;
			hist[i][strlen(hist[i])-1] = ' ';
			strcat (hist[i],s);
		    }
		    sscanf (s,"%x %x %x %x %x %x %x",
			    &pl.cen, &pl.intensity, &pl.real, &pl.imag, 
			    &pl.avg, &pl.log, &pl.cmprss);
		} else {
		    pl.cen = n[1];
		    pl.intensity = n[2];
		    pl.real = n[3];
		    pl.imag = n[4];
		    pl.avg = n[5];
		    pl.log = n[6];
		    pl.cmprss = n[7];
		}
		if (cmd == 18) getdata (Dat, n[0], MAXFFTLEN);
		else getrdata (Dat, n[0], MAXFFTLEN);
		pl.base = Dat;
		p1 (pl);
		break;

	    /* load greyscale color look-up table */
	    case 20:				/* ig */
		ig (n[0], n[1], n[2]);
		break;

	    /* set image starting location */
	    case 21:				/* il */
		il (n[0], n[1]);
		break;

	    /* define image data type and format */
	    case 22:				/* id */
		if (n[0] < 0 || n[0] > 0xF ||
			(n[0] > 3 && n[0] < 8) || n[0] == 9) {
		    printf ("invalid data type code\n");
		    break;
		}
		if (n[1] < 0 || n[1] > 6) {
		    printf("invalid data format code\n");
		    break;
		}
		idtype = n[0];
		if (nn < 2) break;
		idform = n[1];
		if (nn < 3) break;
		idline = n[2] ? n[2] : -1;
		idlen = n[3] ? n[3] : idline;
#ifdef DEBUG
		dump_img_data();
#endif
		break;

	    /* define image display format */
	    case 23:				/* if */
		if (nn < 1) break;
		if (n[0] < 0 || n[0] > 1) {
		    printf ("scale not 0 or 1\n");
		    break;
		}
		ifscale = n[0];
		if (nn < 2) break;
		if (n[1] < 0 || n[1] > 4) {
		    printf ("range not 0..4\n");
		    break;
		}
		ifdisp = n[1];
		if (n[1] == 3) {
		    if (nn > 3) {
			if (n[2] >= n[3]) {
			    printf ("user specified range: min >= max\n");
			    break;
			}
			ifmin = n[2];
			ifmax = n[3];
		    }
		    ifmins[idtype] = ifmin;
		    ifmaxes[idtype] = ifmax;
		}
		if ((ifdisp == 4) && (nn > 2)) ifmask = n[2];
		ifdisps[idtype] = ifdisp;
#ifdef DEBUG
		dump_img_data();
#endif
		break;

	    /* display an image segment */
	    case 24:				/* im */
	    /* display image response */
	    case 25:				/* imr */
		idrel = (cmd == 25);
#ifdef DEBUG
		dump_img_data();
#endif
		im (n[0], n[1], n[2], n[3], n[4], n[5], n[6], n[7]);
		break;

	    /* decode address */
	    case 26: 				/* ad */
		ex_get_resp_addr (n[0], &i, &j);
		printf ("page=%x, offset=%x\n",i,j);
		break;

	    /* set number of triggers per T call */
	    case 27: 				/* tm */
		infile[curlev].tm = n[0];
		break;

	    /* trigger */
	    case 28: 				/* t */
		j = (n[0]) ? n[0] : infile[curlev].tm;
		if (j < 1) j = 1;
		while (j--) t ();
		break;

	    /* setup registers in pup configuration */
	    case 29: 				/* pup */
	    case 30:				/* i */
		pup();
		break;

	    /* turn on stop-on-error */
	    case 31: 				/* a */
		STOPONERR = 1;
		break;

	    /* turn on visual mode (prints each command) */
	    case 32: 				/* v */
		VISUAL = 1;
		break;

	    /* normal mode (turn off stop-on-error and visual) */
	    case 33: 				/* n */
		STOPONERR = 0;
		VISUAL = 0;
		break;

	    /* process file in batch mode */
	    case 34: 				/* b */
		if (n[1] == 0) n[1] = 1;
		if (curlev == 9) err = FAIL;
		else {
		    curlev++;
		    infile[curlev].line = 0;
		    infile[curlev].tm = 1;
		    infile[curlev].reps = n[1];
		    infile[curlev].rep = 1;
		    strcpy (infile[curlev].name, name);
		    if (!(infile[curlev].fp =
				fopen (infile[curlev].name, "r"))) {
			printf ("File %s does not exist\n",
				infile[curlev].name);
			err = FAIL;
			curlev--;
		    }
		}
		break;

	    /* help */
	    case 35: 				/* h */
	    case 44:				/* help */
	    case 45:				/* ? */
		if (nn == 0) n[0] = -1;
		bob_help (n[0], HLINES - 1);
		break;

	    /* break and continue program */
	    case 36: 				/* br */
		cn = 0;
		end = PASS;
		break;

	    /* quit */
	    case 37: 				/* q */
		cn = 0;
		end = FAIL;
		break;

	    /* clear graphics screen */
	    case 38:				/* cg */
		cg(n[0]);
		break;

	    /* move data to stimulus buffer */
	    case 39:				/* ms */
		if ((n[0] > 0x7ffff) || (n[1] > 0x7ffff)) {
		    printf("source address too big\n");
		    break;
		}
		if (n[3] == 0) n[3] = n[2] + n[1] - n[0];
		ms (n[0], n[1], n[2], n[3]);
		break;

	    /* fill with random data */
	    case 40:				/* fr */
	    /* blank command line */
		if ((n[0] > 0xfffff) || (n[1] > 0xfffff)) {
		    printf ("Address too big\n");
		    break ;
		}
		if (n[2] == 0) n[2] = 2;
		fr (n[0], n[1], n[2], n[3]);
		break;

	    /* fill stimulus with random data */
	    case 41:				/* frs */
		if (n[2] == 0) n[2] = 2;
		frs (n[0], n[1], n[2], n[3]);
		break;

	    /* list history on screen */
	    case 42:				/* hi */
		if (curlev > 0) break;
		if (n[0] <= 0 || n[0] >= HLINES) n[0] = HLINES - 1;
		if ((i = infile[0].line - n[0]) < 1) i = 1;
		while (i < infile[0].line) {
		    printf("%4d  %s",i,hist[i % HLINES]);
		    i++;
		}
		break;

	    /* write history to disk file */
	    case 43:				/* w */
		if (curlev > 0) break;
		i = infile[0].line - HLINES + 1;
		if (i < 1) i = 1;
		if ((n[0] < i) || n[0] > n[1] || n[1] > infile[0].line) {
		    printf ("invalid line number range\n");
		    break;
		}
		if ((hp = fopen(name,"w")) == NULL) {
		    printf("Cannot open file %s\n",name);
		    break;
		}
		for (i = n[0]; i <= n[1]; i++) fputs(hist[i % HLINES],hp);
		fclose(hp);
		break;

	    /* loop on read or write */
	    case 46:				/* lo */
		loopon (name,n[1],n[2]);
		break;

	    /* compare syncs */
	    case 47:				/* csr */
		/* derive number of syncs */
		asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 4 );
		if (n[3] == 0) n[3] = mb.w[RLOC_STIM + 1] + 1;
		else n[3] = (n[3] - n[2]) / 2;
		/* derive sync spacing */
		if (n[4] == 0) n[4] = (mb.w[RLOC_STIM] & 0x00f0) >> 4;
		n[4] = 1 << (n[4] + 2);
		err = csr (n[0],n[1],n[2],n[3],n[4]);
		break;

	    /* display floating point intensities */
	    case 48: 				/* dfi */
		if ((n[0] > 0xfffff) || (n[1] > 0xfffff)) {
		    printf ("Address too big\n");
		    break ;
		}
		if (n[1] == 0) n[1] = 1;
		set_break ();
		for (i = 0, j = n[0] >> 1; i < n[1] && no_break; i++) {
		    printf("%.7X:  ",j << 1);
		    k = j + 8;
		    while (j < k) {
			rdata = mb.w[j++] / 32768.0;
			idata = mb.w[j++] / 32768.0;
			printf("% f ",rdata * rdata  +  idata * idata);
		    }
		    printf ("\n");
		}
		break;

	    /* display floating point intensity response */
	    case 49:				/* dfir */
		if (n[1] == 0) n[1] = 1;
		dfir (n[0], n[1]);
		break;

	    /* run the ibob graphical interface */
	    case 50:				/* ibob */
		ibob();
		printf("\n");
		break;

	    /* drop into c shell */
	    case 51:				/* sh */
		run_shell();
		printf("\n");
		break;

	    /* change directory */
	    case 52:				/* cd */
		if (chdir(name)) {
		    sprintf (s,"cannot change to %s",name);
		    perror(s);
		}
		break;

	    /* display image parameters */
	    case 53:				/* dip */
		dump_img_data();
		break;
	    
	    /* edit a file */
	    case 54:				/* vi */
		run_vi(name);
		printf("\n");
		break;

	    /* set image source */
	    case 55:				/* is */
		if (strcmp (name,"exec") == 0) {
		    isswitch = 1;
		    strcpy (name,"EXEC");
		} else {
		    if ((imfp = open (name,O_RDONLY,NULL)) == -1)
			printf ("Cannot open file %s\n",name);
		    else {
			isswitch = 0;
			close (imfp);
		    }
		}
		strcpy(isource,name);
		break;

	    /* display register */
	    case 56:				/* dreg */
		if( nn<2 ) n[1] = -1;
		dreg(name,n[1]);
		break;

	    /* set register */
	    case 57:				/* sreg */
		if (nn < 4) {
		    n[3] = n[2];
		    n[2] = -1;
		}
		sreg(name,n[1],n[2],n[3]);
		break;

	    /* display cage */
	    case 58:				/* dc */
		if (nn < 1) name[0] = '\0';
		dc(name);
		break;

	    /* compare response real */
	    case 59:				/* crr */
		if ((n[0] > 0x1ffffff) || (n[1] > 0x2000000) ||
			(n[2] > 0xfffff) || (n[3] > 0xfffff)) {
		    printf ("Invalid Address\n");
		    break ;
		}
		err = crr (n[0], n[1], n[2], n[3]);
		break;

	    /* write data to a disk file */
	    case 60:				/* wd */
		wd (n[0], n[1], name);
		break;

	    /* write response data to a disk file */
	    case 61:				/* wdr */
		wdr (n[0], n[1], name);
		break;

	    /* set VME interface parameters CAUTION!!! */
	    case 62:
		if( n[0]>2 && n[0]<15 ) set_vme_param( n[0], n[1] );
		else printf("Unknown VME parameter:  %d\n", n[0] );
		break;

	    /* blank line */
	    case 254:
		break;

	    /* any undefined command code */
	    default: 
		printf ("unknown command\n");
		break;
	}
    }
    return (end);
}
