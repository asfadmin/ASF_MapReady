static char *sccs = "@(#)ims_frp_server.c	1.1  11/21/96";

/*   ims_frp_server.c
  Link server program for supporting IMS FRP data transfer operations.
*/

#include <time.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
extern int errno;
int verbose;
int buflen = 16384*1024;		/* length of buffer=16meg */
char *buf;

char Usage[] = "Usage: ims_frp_server [-l# -p# ]\n\
    -l  length of bufs written to network (default 32768)\n\
    -p  IP port number (default 5614) ";

main(argc,argv)
int argc;
char **argv;
{
    struct tm *date;
    time_t loc_time;
    short port = 5614;		/*  default port  */
    int open_tcp(), copy_file(), send_cpx_ceos();
    int send_lr_ceos(), send_gl_ceos(), send_fr_ceos(), send_gf_ceos();
    int command, nfd, status, i, cpix, clin;
    char path[256], name[80], *cptr;
    FILE *fp, *popen();

    argv++; argc--;
    while( argc>0 && argv[0][0] == '-' )  {
	switch (argv[0][1]) {

	case 'l':	/*  size of buffer  */
	    buflen = atoi(&argv[0][2]);
	    break;
	case 'p':	/*  port number  */
	    port = atoi( &argv[0][2] );
	    break;
	case 'v':	/*  port number  */
	    verbose = 1;
	    break;
	default:
	    goto usage;
	}	/*  end case on options  */
	argv++; argc--;
    }		/*  end argv parsing  */

    setlinebuf( stdout );
    setlinebuf( stderr );

if(verbose)
printf("IMS_FRP_SERVER parameters: port=%d, buffer length=%d\n", port, buflen );
    if( (buf = (char *)malloc(buflen)) == (char *)NULL){
	err("IMS_FRP_SERVER error in malloc");
	exit( -1 );
    }
if(verbose)
printf("IMS_FRP_SERVER got memory buffer, PTR=%x\n", buf );

    printf("\n*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*\n");
    printf("IMS_FRP_SERVER starting up at: ");
    loc_time = time(NULL);
    date = localtime(&loc_time);
    printf("%d-%d-%d %02d:%02d:%02d\n",
	date->tm_mon+1, date->tm_mday, date->tm_year,
	date->tm_hour, date->tm_min, date->tm_sec );
    if( (nfd = open_tcp( 0, port )) == 0 ){
	printf("IMS_FRP_SERVER error openning TCP link\n");
	exit( -1 );
    }
    printf("\n--------------------\n");
    printf("IMS_FRP_SERVER received connection, FD=%d.\nCurrent time: ", nfd);
    loc_time = time(NULL);
    date = localtime(&loc_time);
    printf("(day of week:%d) %d-%d-%d %02d:%02d:%02d\n",
	date->tm_wday, date->tm_mon+1, date->tm_mday, date->tm_year,
	date->tm_hour, date->tm_min,date->tm_sec );

/*  Server loop  */

    while( 1 ){	/*  read command code  */
	printf("IMS_FRP_SERVER Getting next command\n");
	command = -1;
	read( nfd, &command, 4 );		/*  read command code  */
	command = ntohl( command );		/*  change byte order */
if(verbose)
printf("IMS_FRP_SERVER got command code=%d\n", command );

	if( command<1 ||command>9 ){		/*  exit  */
	    printf("\nIMS_FRP_SERVER received terminate command:  %d\n", command );
	    shutdown( nfd, 2 );
	    exit(0);

	} else if( command==1 ||	/* Send Standard Full-Res file */
		   command==2 ||	/* Send Standard Low-Res file */
		   command==3 ||	/* Send Geocoded Full-Res file */
		   command==4 ){	/* Send Geocoded Low-Res file */

	    status = read( nfd, path, 80 );	/* get path name */
/*	    zterm( path, 80 );			null terminate it */
	    read( nfd, &cpix, 4 );	/* get center pixel */
	    cpix = ntohl( cpix );
	    read( nfd, &clin, 4 );	/* get center line */
	    clin = ntohl( clin );
if(verbose){
printf("IMS_FRP_SERVER sending CEOS file type: %d\n   PATH=%s\n", command, path );
printf("IMS_FRP_SERVER received center:  (%d,%d)\n", cpix, clin );
}
	    cptr = rindex( path, '/' ) + 1;
	    i = cptr - path;
	    strcpy( name, cptr );		/* extract file root */
	    path[i] = 0;			/* separate path */
	    if( (status = send_ceos_dat
		( nfd, path, name, cpix, clin, command )) != 1 ){

		printf("IMS_FRP_SERVER Data file send error: %d\n", status );

	    } else {	/*  send the leader file  */

		strcat( path, name );
		if( (status = send_ceos_ldr
			( nfd, path, command )) !=1 ){

		    printf("IMS_FRP_SERVER Leader file send error %s\n", path);
		}
	    }

	} else if( command==7 ){	/* receive shell command */

	    status = read( nfd, path, 256 );
if(verbose)
printf("Received shell command:\n%s\n", path );
	    fp = popen( path, "r" );	/* execute the command */
	    i = pclose( fp );		/* close pipe and get status */
	    if( i==0 ) i = 1;
	    else i = 0;
	    i = htonl( i );
	    status = write( nfd, &i, 4 );	/* return status */

	} else{
	    printf("Unsupported function code: %d\n", command );
	}
    printf("IMS_FRP_SERVER Current time: ");
    loc_time = time(NULL);
    date = localtime(&loc_time);
    printf("(day of week=%d) %d-%d-%d %02d:%02d:%02d\n",
	date->tm_wday, date->tm_mon+1, date->tm_mday, date->tm_year,
	date->tm_hour, date->tm_min, date->tm_sec );
    }

usage:
    printf(Usage);
    exit(1);
}
