static char *sccs = "@(#)ims_frpio.c	1.2  04/08/97";

/*  ims_frpio.c  set of routines to support transfers of full res data files
    from ASP control computer to Post Processor through TCP/IP stream sockets.
*/

/*
** Undefine Posix Source flag because of imcompatibility
** with socket stuff.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <math.h>
extern int errno;

#define SOCK_BUFLEN 32768

#define DESC_LEN 720
#define DESC_FIXED_LEN 180
#define PREFIX_LEN 12		/*  length of 12 byte prefix  */
#define O_REC_SIZ 8		/*  byte offset to record length in prefix  */

#define MAX_FR_LEN 8192
#define MAX_FR_REC 8192
#define MAX_LR_LEN 2048
#define MAX_LR_REC 2048
#define MAX_DISP_LEN 1024
#define MAX_DISP_REC 1024

extern int verbose;
extern int buflen;
extern char *buf;

#if defined(sun)
#if defined(__SVR4) || defined(__svr4)
#if !defined(SOLARIS)
#define SOLARIS
#endif
#endif
#endif

#ifdef SOLARIS
#define SYSV
#endif

#ifdef hpux
#define SYSV
#endif

#ifndef SYSV
#define HAVE_bcopy
#define HAVE_bzero
#endif

#ifndef HAVE_bcopy
#define bcopy(s,d,n) memcpy((d),(s),(n))
#endif

#ifndef HAVE_bzero
#define bzero(p,n) memset((p),0,(n))
#endif

#if defined(SYSV)
#include <sys/times.h>
#include <sys/param.h>
#endif

char filename[80], desc[MAX_LR_LEN];

/*  open_tcp( host, port )---------------------------------------------

  Routine to open a TCP stream socket.

  Inputs:

    (char *) HOST:  String containing host to attach to.  If given, this
	program is initiating as a client to attach to the host.
	If not given, this program is the server waiting for a
	connection from a client.

    (int) PORT:  Port number to make link connection through.
*/
int open_tcp( host, port )
char *host;
int port;
{
    struct sockaddr_in sinme, sinhim, frominet;
    int fromlen, fd, sd;
    int options = 0;		/* socket options */
    int one = 1;		/* for 4.3 BSD style setsockopt() */
    int bufsize = SOCK_BUFLEN;
    struct linger lingopt;
    struct hostent * addr;
    unsigned long addr_tmp;

    bzero((char *)&sinme, sizeof(sinme));
    bzero((char *)&sinhim, sizeof(sinhim));
    bzero((char *)&frominet, sizeof(frominet));
    if( host != 0 ){		/* host given, we are client */

printf("OPEN_TCP got server host=%s, port=%d\n", host, port );

	if( atoi(host) > 0 ){	/* Numeric host address */

	    sinhim.sin_family = AF_INET;
	    sinhim.sin_addr.s_addr = inet_addr(host);

	} else {		/* ASCII host name */

	    if( (addr=gethostbyname(host)) == NULL)
		err("OPEN_TCP bad hostname");
	    sinhim.sin_family = addr->h_addrtype;
	    bcopy(addr->h_addr,(char*)&addr_tmp, addr->h_length);
	    sinhim.sin_addr.s_addr = addr_tmp;
	}
	sinhim.sin_port = htons(port);
	sinme.sin_port = 0;

    } else {			/*  we are server  */

	sinme.sin_port = htons( port );
	sinme.sin_addr.s_addr = INADDR_ANY;

    }
    sinme.sin_family = AF_INET;

/*  create a socket to the INET, UDP protocol=0  */

    if( (sd = socket(AF_INET, SOCK_STREAM, 0)) < 0){
	err("OPEN_TCP error Create Socket");	
	return( -1 );
    }
    options = SO_LINGER;
    lingopt.l_onoff = 0;
    lingopt.l_linger = 0;
    if( setsockopt(sd, SOL_SOCKET, options, &lingopt, sizeof(lingopt)) < 0)
	err("OPEN_TCP error Setting LINGER Option");

    options = SO_REUSEADDR;
    if( setsockopt(sd, SOL_SOCKET, options, &one, sizeof(one)) < 0)
	err("OPEN_TCP error Setting REUSEADDR Option");

/*  bind a name in sinme to the socket using UNIX domain name family  */

    if(bind(sd, &sinme, sizeof(sinme)) < 0){
	err("OPEN_TCP error Binding Socket");
	return( -1 );
    }

    if( host!=0 ){

	options = SO_SNDBUF;
	if( setsockopt(sd, SOL_SOCKET, options, &bufsize, sizeof(bufsize)) < 0)
	    err("OPEN_TCP error Setting SNDBUF Option");

/*  Connect to the server  */

	if(connect(sd, &sinhim, sizeof(sinhim) ) > 0) return( sd );
	err("OPEN_TCP error Connecting as Client");

    } else {    		/* we are server */

printf("OPEN_TCP got port=%d\n", port );

	options = SO_RCVBUF;
	if( setsockopt(sd, SOL_SOCKET, options, &bufsize, sizeof(bufsize)) < 0)
	    err("OPEN_TCP error Setting Server Socket options");

/*  Wait for connection from client  */

	listen(sd,1);   /* allow a queue of 1 */
	fromlen = sizeof(frominet);

/*  set ACCEPT on socket for receiver  */

	if( (fd=accept(sd, &frominet, &fromlen) ) > 0){
	    shutdown( sd, 2 );
	    return( fd );
	}
	err("OPEN_TCP error Accepting Connection");

    }		/*  done setting socket for client or server  */
    return( -1 );	/*  errors occurred  */
}

/*  send_ceos_ldr( outfd, path, code )--------------------------
  Routine to read records of CEOS leader/trailer file and send to network
*/
int send_ceos_ldr( outfd, path, code )
int outfd, code;
char *path;
{
    int bytes, i, j, k, l, m, n, infd, ldr_cnt, skipped;
    unsigned int *lp;
    char *cptr, cbuf[64], *cp;

printf("SEND_CEOS_LDR, code=%d path=%s\n", code, path );

    strcpy( filename, path );
    strcat( filename, ".L" );
    infd = open( filename, O_RDONLY );
    if( infd<=0 ){
	perror("SEND_CEOS_LDR file open error");
	j = -1;
	j = htonl( j );
	write( outfd, &j, 4 );	/*  send error code  */
	return( -1 );
    }

/*  read the complete LDR file into buffer  */

    cp = buf;
    ldr_cnt = 0;
    bytes = 0;
    while( ( j=read( infd, cp, SOCK_BUFLEN ) ) == SOCK_BUFLEN ){
	cp += SOCK_BUFLEN;
	ldr_cnt += j;
if(verbose)
printf("SEND_CEOS_LDR read %d bytes\n", j );
    }
    ldr_cnt += j;
if(verbose)
printf("SEND_CEOS_LDR read %d total bytes\n", ldr_cnt );
    close( infd );
    if( j<0 ){
	perror("SEND_CEOS_LDR file read error");
	j = -1;
	j = htonl( j );
	write( outfd, &j, 4 );	/*  send error code  */
	return( -1 );
    }

/*  handshake with client  */

printf("SEND_CEOS_LDR sending final handshake=%d\n", code );
    j = htonl( code );
    write( outfd, &j, 4 );	/*  send code as handshake  */

    read( outfd, &j, 4 );	/*  get final client handshake  */
    j = ntohl( j );
    if( j!=code ){
	printf("SEND_CEOS_LDR aborted by client\n");
	return( -1 );	/*  error at client  */
    }

/* send descriptor and other LDR records with counts and legnths of each
   record from descriptor variable section, but send only one of each */

    cp = buf;				/* point to start of LDR */
    i = k = 1;				/* initialize record count */
    cptr = buf + DESC_FIXED_LEN;	/* point to start of record counts */
    skipped = 0;
    m = 720;				/* descriptor length */

    while( cptr < buf+DESC_LEN ){	/* scan through desc */

	memcpy( &n, cp+8, 4 );		/* get length from record */
	n = ntohl( n );
if( verbose )
printf("Record %d has %d bytes, CP=%12d\n", k, n, cp );
	if( cp+n>buf+ldr_cnt ){
	    printf("Correcting record %d from length %d\n", k, n );
	    printf("Backing up previous record length: %d\n", j );
	    cp -= j;	/* backup previous incorrect record length */
	    printf("Forwarding to next record, length: %d\n", l );
	    cp += l;	/* forward correct record length */
	    memcpy( &j, cp+8, 4 );
	    j = ntohl( j );
	    if( j!=m ) j = m;	/* over-ride with length in DESC */
	    printf("Extracted new length: %d\n", j );
	} else {
	    j = n;
	}
	while( i>1 ){			/* skip multiples of any record type */
	    cp += j;			/* point to next record */
	    skipped += j;
if(verbose)
printf("SEND_CEOS_LDR skipped %d bytes, rec=%d\n", j, k );
	    memcpy( &j, cp+8, 4 );
	    j = ntohl( j );
	    i--;		/* decrement count */
	}
	n = htonl( k );
	memcpy( cp, &n, 4 );		/* set record number */
	write( outfd, cp+8, 4 );	/* send byte count */
	write_b( outfd, cp, j );	/* send record */
if(verbose){
printf("SEND_CEOS_LDR sent %d bytes, rec=%d\n", j, k );
/*
for( i=0; i<16; i++ ) printf("%3x", 0xff&cp[i] );
printf("\n");
*/
}

/*  skip 0 record counts in DESC  */

	l = m;		/* save length from current DESC */
	for( i=0; cptr<buf+DESC_LEN && i==0; cptr+=12 ){
	    memcpy( cbuf, cptr, 6 );
	    cbuf[6] = 0;
	    i = atoi( cbuf );
	    if( i>1 ) memcpy( cptr, "     1", 6 );
	    memcpy( cbuf, cptr+6, 6 );
	    cbuf[6] = 0;
	    m = atoi( cbuf );
	}
if( verbose)
printf("New count=%d, length=%d, saved length=%d\n", i, m, l );
	cp += j;			/* next record */
	bytes += j;
	k++;		/* increment number of records processed */
    }
    close( infd );
printf("SEND_CEOS_LDR sent %d total bytes\n", bytes );

    j = 0;
    write_b( outfd, &j, 4 );
    if( bytes!=ldr_cnt-skipped ){
	printf("SEND_CEOS_LDR consistency error, sent:%d, expected%d\n",
		bytes, ldr_cnt-skipped );
    }

    return( 1 );
}

/*  get_ceos_data_par( cpix, clin, mp, ml, ilen, p1, l1, np, nl )
  Interpret image parameters from data file descriptor record and
  determine data sizes to set file input parameters.
*/
int get_ceos_data_par( cpix, clin, mp, ml, ilen, p1, l1, np, nl )
int cpix, clin, mp, ml, *ilen, *p1, *l1, *np, *nl;
{
    int pix, lines, i;
    char *cptr;

if(verbose)
printf("GET_CEOS_DATA_PAR cpix=%d clin=%d mp=%d ml=%d\n", cpix, clin, mp, ml );

/*  interpret data values from descriptor  */

    cptr = desc + 186;			/* record length */
    sscanf( cptr, "%6d", ilen );

    cptr = desc + 236;			/* number of image lines */
    sscanf( cptr, "%8d", &lines );

    cptr = desc + 248;			/* total number of pixels per record */
    sscanf( cptr, "%8d", &pix );

if(verbose)
printf("GET_CEOS_DATA_PAR ilen=%d pix=%d lines=%d\n", *ilen, pix, lines );

    if( cpix < mp/2 ) cpix = mp/2;
    if( clin < ml/2 ) clin = ml/2;
    if( cpix > pix-mp/2 ) cpix = pix - mp/2;
    if( clin > lines-ml/2 ) clin = lines - ml/2;

    if( cpix-mp/2 > 0 ) *p1 = cpix - mp/2;	/* first pixel */
    else *p1 = 0;
    if( clin-ml/2 > 0 ) *l1 = clin - ml/2;	/* first line */
    else *l1 = 0;

    if( pix-(*p1)<mp ) *np = pix - (*p1);	/* number of pixels to get */
    else *np = mp;
    if( lines-(*l1) < ml ) *nl = lines-(*l1);	/* number of lines to get */
    else *nl = ml;

    cptr = desc + 276;			/* record prefix length */
    sscanf( cptr, "%4d", &i );
    *p1 += i;				/* skip prefix */

    (*l1)++;				/* skip descriptor record */

    cptr = desc + 244;			/* left border pixels */
    sscanf( cptr, "%8d", &i );
    pix -= i;				/* ignore left border pixels */
    *p1 += i;				/* also skip left border pixels */

    cptr = desc + 256;			/* right border pixels */
    sscanf( cptr, "%8d", &i );
    pix -= i;				/* ignore right border pixels */

    return( 1 );
}

/*  set_ceos_data_par( len, p1, p2, p3, np, nl )
  Set CEOS data file descriptor record parameters
    len = record length in bytes
    p1 = prefix length
    p2 = left border pixels
    p3 = right border pixels
    np = total pixels per line
    nl = total number of SAR data lines
*/
int set_ceos_data_par( len, p1, p2, p3, np, nl )
int len, p1, p2, p3, np, nl;
{
    int *lptr;
    char *cptr, cbuf[32];

/*  interpret data values from descriptor  */

    lptr = (int *) desc;
    lptr[2] = ntohl( len );		/* length of each record */

    cptr = desc + 180;			/* number of data records */
    sprintf( cbuf, "%6d", nl );
    strncpy( cptr, cbuf, 6 );

    cptr = desc + 186;			/* length of each data record */
    sprintf( cbuf, "%6d", len );
    strncpy( cptr, cbuf, 6 );

    cptr = desc + 236;			/* lines per data set */
    sprintf( cbuf, "%8d", nl );
    strncpy( cptr, cbuf, 8 );

    cptr = desc + 244;			/* left border pixels */
    sprintf( cbuf, "%4d", p2 );
    strncpy( cptr, cbuf, 4 );

    cptr = desc + 248;			/* total pixels per line */
    sprintf( cbuf, "%8d", np );
    strncpy( cptr, cbuf, 8 );

    cptr = desc + 256;			/* right border pixels */
    sprintf( cbuf, "%4d", p3 );
    strncpy( cptr, cbuf, 4 );

    cptr = desc + 276;			/* data prefix length */
    sprintf( cbuf, "%4d", p1 );
    strncpy( cptr, cbuf, 4 );

    cptr = desc + 280;			/* SAR data bytes */
    sprintf( cbuf, "%8d", np );
    strncpy( cptr, cbuf, 8 );

/* zero values */

    sprintf( cbuf, "%4d", 0 );
    cptr = desc + 260;			/* top border lines */
    strncpy( cptr, cbuf, 4 );
    cptr = desc + 264;			/* bottom border lines */
    strncpy( cptr, cbuf, 4 );

    return( 1 );
}

/*  send_buf( outfd, obuf, np, lper, afd, fac, scale, fbuf )
    Routine to send a buffer of data and average it if necessary
*/
int send_buf( outfd, obuf, np, lper, afd, fac, scale, fbuf )
int outfd, np, lper, afd, fac;
unsigned char *obuf;
float scale, *fbuf;
{
    int i, j, k,l, m, n;

    n = 0;
    if( fac>1 ) for( i=0; i<np/fac; i++ ) fbuf[i] = 0.0;
    for( j=0; j<lper; j++ ){
	write_b( outfd, obuf, np );
	if( fac>1 ){		/* accumulate for average */
	    for( k=0; k<np; k++ ) fbuf[k/fac] += (float) obuf[k];
	    n++;
	    if( n==fac ){	/* write out averaged line */
		for( k=0; k<np/fac; k++ ){
		    obuf[k] = (unsigned char ) (fbuf[k]*scale + 0.5);
		    fbuf[k] = 0.0;
		}
		write( afd, obuf, np/fac );
		n = 0;
	    }
	}
	obuf += np;
    }
}

/*  send_ceos_dat( outfd, path, name, cpix, clin, code )---------------------
  Routine to send CEOS data files to client and create its average if needed
*/
int send_ceos_dat( outfd, path, name, cpix, clin, code )
int outfd, cpix, clin, code;
char *path, *name;
{
    int bytes, i, j, k, fac, infd, status, afd, ofd, bufsiz;
    int mp, ml, np, nl, lper, ipt, ipinc, iter, isiz, opi;
    int ilen, p1, l1, b0, binc, blinc, opinc, ibinc, opt;
    float scale, fbuf[MAX_FR_LEN];
    char loc_buf[MAX_FR_LEN], *ibuf, *obuf, afile[80];

printf("SEND_CEOS_DAT input path=%s name=%s\n", path, name );
if(verbose)
printf("cpix=%d clin=%d code=%d\n", cpix, clin, code );

    strcpy( filename, path );
    strcat( filename, name );
    strcat( filename, ".D" );
    if( (infd = open( filename, O_RDONLY )) < 0 ){
	perror("SEND_CEOS_DAT error openning data file");
	j = -1;
	j = htonl( j );
	write( outfd, &j, 4 );
	return( -1 );
    }

/* set maximum usable record lengths and counts for the different formats */

    if( code==1 || code==3 ){	/*  full res  */
	mp = MAX_FR_LEN;
	ml = MAX_FR_REC;
    } else {
	mp = MAX_LR_LEN;
	ml = MAX_LR_REC;
    }

/* read data descriptor record */

    if( (status=read(infd,desc,MAX_LR_LEN)) != MAX_LR_LEN ){
	printf("SEND_CEOS_DAT error reading descriptor record");
	j = -1;
	j = htonl( j );
	write( outfd, &j, 4 );
	close( infd );
	return( -1 );
    }

/* get length of each record, first byte to read, first record to read,
   number of bytes to read, and number of records to read given requested
   center pixel and line, and maximum pixel and line */

    get_ceos_data_par( cpix, clin, mp, ml, &ilen, &p1, &l1, &np, &nl );

if(verbose)
printf("SEND_CEOS_DAT ilen=%d p1=%d l1=%d np=%d nl=%d\n",
ilen, p1, l1, np, nl );

    if( nl>MAX_DISP_REC || np>MAX_DISP_LEN ){	/* generate averaged file */
if(verbose)
printf("Openning averaged display file\n");
	strcpy( afile, path );
	strcat( afile, "TEMP.AVG" );
	if( (afd = open( afile, O_RDWR|O_CREAT, 0644 ))<0 ){
/***	if( (afd = open( "TEMP.AVG", O_RDWR|O_CREAT, 0644 ))<0 ){ ***/
	    perror("SEND_CEOS_DAT error openning average file");
	    j = -1;
	    j = htonl( j );
	    write( outfd, &j, 4 );
	    close( infd );
	    return( -1 );
	}
	i = ( np+MAX_DISP_LEN-1 )/MAX_DISP_LEN;
	j = ( nl+MAX_DISP_REC-1 )/MAX_DISP_REC;
	if( i>j ) fac = i;
	else fac = j;
	if( fac<=0 ) fac = 1;
	scale = 1.0 / ( ( (float) fac ) * ( (float) fac ) );
    } else {
	afd = -1;
	fac = 1;
	scale = 1.0;
    }

/* calculate transfer parameters */

    lper = buflen/np;		/* number of lines per iteration */
    i = lper/fac;
    lper = i*fac;
    bufsiz = lper * np;
    if( lper>nl ) lper = nl;
    ipt = p1 + l1*ilen;		/* first byte to read */
    if( np==ilen ){
	iter = 1;
	ipinc = 0;
	ibinc = 0;
	isiz = lper*np;
    } else {
	iter = lper;
	ipinc = ilen;
	ibinc = np;
	isiz = np;
    }

if(verbose)
printf("SEND_CEOS_DAT lper=%d ipt=%d iter=%d ipinc=%d ibinc=%d isiz=%d\n",
lper, ipt, iter, ipinc, ibinc, isiz );

    if( (code==2 || code==3) && np*nl>bufsiz/2 ){
printf("Disk based rotation, ");
	strcpy( afile, path );
	strcat( afile, "TEMP.DAT" );
	ofd = open( afile, O_RDWR|O_CREAT, 0644 );
/***	ofd = open( "TEMP.DAT", O_RDWR|O_CREAT, 0644 );	***/
	if( code==2 ){		/* 90 degrees CW */
printf("90 degrees CW\n");
	    opi = nl - lper;
	    opinc = -lper;
	    b0 = (lper-1) * np;
	    binc = -np;
	    blinc = 1;
	} else {		/* 90 degrees CCW */
printf("90 degrees CCW\n");
	    opi = 0;
	    opinc = lper;
	    b0 = np-1;
	    binc = np;
	    blinc = -1;
	}
	for( i=nl; i>0; i-=lper ){
	    if( i<lper ){
		lper = i;
		if( np==ilen ){
		    isiz = np*lper;
		} else {
		    iter = lper;
		}
	    }
	    ibuf = buf;
if(verbose)
printf("Reading for disk based rotation iter=%d isiz=%d\n", iter, isiz );
	    for( j=0; j<iter; j++ ){
		lseek( infd, ipt, 0 );
		if( (status=read( infd, ibuf, isiz )) != isiz ){
		    perror("SEND_CEOS_DAT error reading file for rotation");
		    printf("Requested %d bytes, received %d\n", isiz, status);
		    close( infd );
		    if( fac>1 ) close( afd );
		    j = -1;
		    j = htonl( j );
		    write( outfd, &j, 4 );
		    return( -1 );
		}
		ibuf += ibinc;
		ipt += ipinc;
	    }
	    opt = opi;
if(verbose)
printf("Writing disk based rotation opt=%d lper=%d\n", opt, lper );
	    for( k=0; k<np; k++ ){
		lseek( ofd, opt, 0 );
		for( j=0; j<lper; j++ ) loc_buf[j] = buf[b0+j*binc+k*blinc];
		write( ofd, loc_buf, lper );
		opt += nl;
	    }
	    opi += opinc;
	}

/* reset parameters for sending rotated file */

	ilen = nl;
	l1 = 0;
	p1 = 0;
	nl = np;
	np = ilen;
	lper = bufsiz/np;
	if( lper>nl ) lper = nl;
	isiz = lper*np;
	iter = 1;
	ipinc = 0;
	ibinc = 0;
	close( infd );
	infd = ofd;
	ipt = 0;
	lseek( infd, ipt, 0 );
    }

if(verbose)
printf("SEND_CEOS_DAT IN=%d OUT=%d NP=%d NL=%d ILEN=%d FAC=%d scale=%f code=%d\n",
	infd, outfd, np, nl, ilen, fac, scale, code );

    j = htonl( code );
    write( outfd, &j, 4 );	/*  send handshake  */

/* build new descriptor and send it */

    if( code==2 ){
	i = nl;
	j = np;
    } else {
	i = np;
	j = nl;
    }
    set_ceos_data_par( i, 0, 0, 0, i, j );
    bzero( buf, i );
    bcopy( desc, buf, MAX_LR_LEN );
    write( outfd, buf, 1024 );

/* read code as final handshake */

    read( outfd, &j, 4 );
    j = ntohl( j );
    if( j!=nl*np ){
	close( infd );
	printf("Handshake abort\n");
	if( fac>1 ) close( afd );
	return( -1 );
    }

/* now send the data */

    i = nl;
    while( i>0 ){
	if( i<lper ){
	    lper = i;
	    if( np==ilen ){
		isiz = lper*np;
	    } else {
		iter = lper;
	    }
	}

/* read cluster */

if(verbose)
printf("Reading iter=%d isiz=%d\n", iter, isiz );
	ibuf = buf;
	for( j=0; j<iter; j++ ){
	    if( ipt>0 ) lseek( infd, ipt, 0 );
	    if( (status=read( infd, ibuf, isiz )) != isiz ){
		perror("SEND_CEOS_DAT fatal error reading input file");
		printf("Requested %d bytes, received %d\n", isiz, status);
		close( infd );
		if( fac>0 ) close( afd );
		return( -1 );
	    }
	    ibuf += ibinc;
	    ipt += ipinc;
	}

/*  output for different types of file */

	if( code==1 || (code==2&&np*nl>bufsiz/2) ||
		(code==3&&np*nl>bufsiz/2) || code==4 ){

if(verbose)
printf("Writing output buffer np=%d lper=%d\n", np, lper );
	    send_buf( outfd, buf, np, lper, afd, fac, scale, fbuf );

	} else {		/* LR, rotate 90 degrees CW in memory */

printf("In memory rotation np=%d nl=%d\n", np, nl );

	    obuf = buf + np*nl;
	    for( j=(nl-1)*np; j<np*nl; j++ )
		for( k=j; k>=0; k-=np ) *obuf++ = buf[k];
	    obuf = buf + np*nl;
if(verbose)
printf("Writing output buffer np=%d nl=%d\n", nl, np );
	    send_buf( outfd, obuf, nl, np, afd, fac, scale, fbuf );

	}
	i -= lper;
    }
    close( infd );

    if( fac>1 ){	/* send averaged file */
printf("Sending averaged file, %d %d\n", np/fac, nl/fac );
	lseek( afd, 0, 0 );
	i = np/fac;
	j = nl/fac;
	k = i*j;
	read( outfd, &j, 4 );
	j = ntohl( j );
	if( j!=k ){
	    close( afd );
	    return( -1 );
	}
	read( afd, buf, k );
	send_buf( outfd, buf, np/fac, nl/fac, 0, 0, scale, fbuf );
	close( afd );
    }
    return( 1 );
}

/* read_b( fd, buf, bytes )----------------------
	read data from fd into buffer 32768 bytes at a time
*/
int read_b( fd, buf, bytes )
int fd, bytes;
unsigned char *buf;
{
    int j, i, k;
/*
if(verbose)
printf("READ_B reading %d bytes from FD: %d\n", bytes, fd );
*/
    j = SOCK_BUFLEN;
    for( k=0; k<bytes; ){
	if( j>bytes ) j = bytes;
	if( (i=read( fd, buf, j )) <= 0 ){
	    perror("Error reading data");
	    printf("Bytes attempted %d, status %d\n", j, i );
	    return( -1 );
	}
	k += i;
	buf += i;
    }
/*
printf("READ_B completed successfully\n");
*/
    return( k );
}

/* write_b( fd, buf, bytes )----------------------
	write data from fd into buffer 32768 bytes at a time
*/
int write_b( fd, buf, bytes )
int fd, bytes;
unsigned char *buf;
{
    int j, i, k;
/*
if(verbose)
printf("WRITE_B writing %d bytes to FD: %d\n", bytes, fd );
*/
    j = SOCK_BUFLEN;
    for( k=0; k<bytes; ){
	if( j>bytes ) j = bytes;
	if( (i=write( fd, buf, j )) <= 0 ){
	    perror("Error writing data");
	    printf("Bytes attempted %d, status %d\n", j, i );
	    return( -1 );
	}
	k += i;
	buf += i;
    }
/*
printf("WRITE_B completed successfully\n");
*/
    return( k );
}

/****  ERR  Print error message  ****/

err(s)
char *s;
{
    perror(s);
    fprintf(stderr,"errno=%d\n",errno);
    exit(1);
}

/****  ZTERM  zero terminate a character array  ****/
int zterm( buf, len )
char *buf;
int len;
{
    len--;
    while( *buf!=0 && *buf!=0x20 && len>0 ){
	buf++;
	len--;
    }
    *buf = 0;
    return;
}
