/**************************************************************
**
** File:        main.c
**
** Function:    This is the driver for eosmenu program
**  
** Author:		Tieh Ku
**
** Date:        9/27/96
**
** Modified: 
**
*********************************************************************/
#pragma ident   "@(#)eosmain.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/aps_mkorbcov/SCCS/s.eosmain.c"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>



void usage(char*);

/**************************************************************
**
** main ()
**
**
*****************************************************************/

void main (int argc, char *argv[])
{

    extern int  optind ;
    extern char *optarg ;

	int slen,return_code ;
    int  i,j,opt, opt_flag ;
    double l1,l2,l3,l4,t1,t2,yaw;
    double lat1,lon1,lat2,lon2,smin,ssec,yawang;
    char *optlist  = "hp:P:y:a:o:A:O:i:d:m:s:";
    char *progName;
    char *ptr;
	char instrument[80] ;
	char duration[80] ;
	char platform[80] ;
    char filename[80];
	char projection;
	/*char msg[MSG_LEN];*/


	extern eosmenu_(char*,int*,char*,char*,double*,double*,double*,double*,char*,char*,double*,double*,double*);



    progName = (char *) strdup(argv[0]) ;
	/*
    aps_open_syslog();
	sprintf(msg, "Program started with arguments: " ) ;
	for( j = 1; j < argc; j++ )
	{
	    strcat(msg, " " ) ;
	    strcat(msg, argv[j] ) ;
	}
	aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
 */	 
    opt_flag = 0;
	
    while((opt = getopt(argc, argv, optlist)) != EOF)
    {
		switch(opt)
		{
 	    	case 'h' :
				usage(progName) ;
				break ;

	    	case 'p' :
        		opt_flag++;
				strcpy(platform,optarg);
				break ;

	    	case 'P' :
        		opt_flag++;
				projection = *optarg;
				break ;

	    	case 'y' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&yaw);
				if( return_code != 1)
				fprintf(stderr, "Incorrect yaw input\n" ) ;
				yawang = yaw;
				break ;

	    	case 'a' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&l1);
				if( return_code != 1)
				fprintf(stderr, "Incorrect lat input\n" ) ;
				lat1= l1;
				break ;

	    	case 'o' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&l2);
				if( return_code != 1)
				fprintf(stderr, "Incorrect lon input\n" ) ;
				lon1= l2;
				break ;

	    	case 'A' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&l3);
				if( return_code != 1)
				fprintf(stderr, "Incorrect lat input\n" ) ;
				lat2= l3;
				break ;

	    	case 'O' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&l4);
				if( return_code != 1)
				fprintf(stderr, "Incorrect lon input\n" ) ;
				lon2= l4;
				break ;

	    	case 'i' :
        		opt_flag++;
				strcpy(instrument,optarg);
				slen = strlen(instrument);
				break ;

	    	case 'd' :
        		opt_flag++;
				strcpy(duration,optarg);
				break ;

	    	case 'm' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&t1);
				if( return_code != 1)
				fprintf(stderr, "Error in decoding\n" ) ;
				smin= t1;
				break ;

	    	case 's' :
        		opt_flag++;
				return_code = sscanf(optarg, "%lf",&t2);
				if( return_code != 1)
				fprintf(stderr, "Error in decoding\n" ) ;
				ssec= t2;
				break ;

	    	default :
				usage(progName) ;
				break ;
	 	}
    }


	if (opt_flag < 10)
 	{
		usage(progName) ;
	 	exit(1);
	}		
	/*
	if ((strcmp(platform,"R1") !=0) &&
		(strcmp(platform,"ers1_A") !=0) &&
		(strcmp(platform,"ers1_B") !=0) &&
		(strcmp(platform,"ers1_C") !=0) &&
		(strcmp(platform,"ers1_D") !=0) &&
		(strcmp(platform,"ers1_E") !=0) &&
		(strcmp(platform,"ers1_F") !=0) &&
		(strcmp(platform,"ers1_G") !=0) &&
		(strcmp(platform,"ers2_A") !=0) &&
		(strcmp(platform,"J1") !=0)) 
	{
		printf("Error: Incorrect Platform type\n");
		usage(progName) ;
	}
	*/

	if ((projection != 'C') &&
	    (projection != 'M') &&
	    (projection != 'O') &&
	    (projection != 'N') &&
	    (projection != 'G') &&
	    (projection != 'S')) 
	{
		printf("Error: Incorrect Projection type\n");
		usage(progName) ;
	}

	if (projection == 'G') 
	{
		if (lon2 <= 0)
			lon2 =1;
	}

	if ((projection == 'N') ||
	    (projection == 'S')) 
	{
		if (lat1 >= 90)
		{
			printf("Error: Incorrect lat number\n");
			usage(progName) ;
		}
	}


	sprintf(filename,"%s.%s.%c",platform,instrument,projection);

	eosmenu_(filename,&slen,platform,&projection,&lat1,&lon1,&lat2,&lon2,instrument,duration,&smin,&ssec,&yawang);

} /* main */

/***************************************************************
**
** usage ()
**
**************************************************************** */
void usage(char *progName)
{
	printf("Usage:\n%s -h -p<platform> -P<projection> [-y<yaw>] -a<lat1> -o<lon1>\
	-A<lat2> -O<lon2> -i<instrument> -d<duration> -m<smin> -s<ssec>\n", progName) ;
	printf("where....\n") ;
	printf("\n") ;
	printf("\t-h <help>\n");
	printf("\t-p <platform>   -- ers1_A .. ers1_G, ers2_A, R1, jers1_A\n");
	printf("\t-P <projection> -- (C) Cylindrical,(M) Mercator,(G) Globe\n");
	printf("\t	   	   (N) North Pole ,(S) South Pole\n");
	printf("\t[-y <yaw>]   	-- Yaw angle for N or S projections\n");
	printf("\t-a <lat1>   	-- Latitude of upper left corner\n");
	printf("\t-o <lon1>   	-- Longitude of upper left corner\n");
	printf("\t-A <lat1>   	-- Latitude of lower right corner\n");
	printf("\t-O <lon1>   	-- Longitude of lower right corner\n");
	printf("\n") ;
	printf("\t   For projection C or M, lat1,lon1,lat2,lon2 are the two corners\n");
	printf("\t   For projection N or S, only lat1 is needed but should be < 90\n");
	printf("\t   For projection G, lat1 and lon1 is the center point, lat2 is\n");
	printf("\t   used as axis tilt(default=0),lon2 is zoom factor (default=1)\n");
	printf("\n") ;
	printf("\t-i <instrument> -- ST1..ST7,WD1..WD3,FN1..FN5,EL1,EH1..EH6\n");
	printf("\t	 	   SWA,SWB,SNA,SNB for R1, JERS-1/A for J1\n");
	printf("\t 	 	   ERS-1/A..ERS-1/G for E1, ERS-2/A for E2\n");
	printf("\t-d <duration>   -- r1,R2.. number of rev's\n");
	printf("\t-m <smin>       -- Step time, minutes\n");
	printf("\t-s <ssec>       -- Step time, seconds\n");
	exit(1) ;
} /* usage */

