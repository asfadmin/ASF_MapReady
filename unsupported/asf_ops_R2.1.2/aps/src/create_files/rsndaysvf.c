#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*============================================================================
Filename:
 
Description:
 
External Functions Defined:
   
File Scope Functions:
   
External Variables Defined:
   
File Scope Variables:
 
Notes:
 
==============================================================================
*/
#pragma ident   "@(#)rsndaysvf.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/create_files/SCCS/s.rsndaysvf.c"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>

#include "dapps_defs.h"
#include "apspath.h"

#include "apsfiledef.h"
#include "timeconv.h"

/*  #define  MAIN  */

#define  PTHSIZ 128
#define  FILSIZ  64

/*  #define  DEBUG  */
#undef DEBUG 
#define  LOGMSG
#define  STDMSG

char msg[512];

int  rsndaysvf( char *file, char *firstdoy, char *lastdoy );
void close_files(FILE *ifp, FILE *ofp );
int  getrev(FILE *mf, FILE *ifp, char *ibuf, char *doyfname, int *mrev, void (*minmaxf)());
void maxrev( int *lastrev , int *revs );
void minrev( int *firstrev, int *revs );
void prtmsg(FILE *mf, char *msg, int MSG_TYP );


#if defined (MAIN)
/*=============================================================================
Function:    main

Description: This is the main function for the Radarsat n-day state vector
             file creation program. Creation of the n-day state vector
             file is done in the function rsndaysvf.c (described below). 
             The main function performs the following setup tasks for the
             n-day file creation function:
             1. prints a greeting message containing the program name and
                number of arguments on the command line.
             2. tests for valid number of command line arguments ( always 4
                including the program name as the 1st argument ).
             3. prints the command line arguments (excluding the program name).
                The 1st argument after the program name is the full path/file
                name of the output n-day state vector file. 
                The 2nd argument is a 21 character epoch in the form:
                yyyy:ddd:hh:mm:ss.fff 
                This argument identifies the first day of year file to be
                output to the n-day file. This input day of year file is 
                constructed by the program in the form r1_yyyyddd, where yyyy is
                the year and ddd is the day of year. This file is the output of 
                the rsdoysvf program. It resides in an APS directory whose name
                is controlled by an environment variable named APS_RADARSAT_SVF.
                The 3rd argument is a 21 character epoch in the form:
                yyyy:ddd:hh:mm:ss.fff
                This argument identifies the last day of year file to be
                output to the n-day file. This input day of year file is
                constructed by the program in the form r1_yyyyddd, where yyyy is
                the year and ddd is the day of year. This file is the output of
                the rsdoysvf program. It resides in an APS directory whose name
                is controlled by an environment variable named APS_RADARSAT_PSV.  
                The n chronological day of year file names between the first and
                last day of year files are calculated by the program automatically
                as needed, and together with the first and last files, form the
                n-day output state vector file.
             4. invokes the function rsndaysvf to create the n-day state vector
                file ordered by rev number.
             5. prints a program termination message indicating normal/abnormal
                completion.

Input command: rsndaysvf.e outputpath/filename yyyy:ddd:hh:mm:ss.fff yyyy:ddd:hh:mm:ss.fff

Returns:
        int:
        = 0 normal completion; no errors detected.
        = 1 abnormal ending; processing errors detected.

Creator:    Ted Pavlovitch

Creation date: 03/27/95

===========================================================================*/
main(int argc, char *argv[])
{

	FILE *mf;  /*  message file  */

	int  i , status;
	char file[PTHSIZ + FILSIZ + 1] = "\0" ;     /* 1st input argument          */
	char 
		firstdoy[22] = "\0", 
		lastdoy[22] = "\0" ; /* 2nd and 3rd input arguments */

	mf = stdout;

#if defined (LOGMSG)
	openlog("rsndaysvf.e",LOG_PID|LOG_CONS|LOG_NDELAY,LOG_LOCAL1);
	setlogmask(LOG_UPTO(LOG_DEBUG));
#endif

	sprintf(msg,"%s program started - argc:%d \n", argv[0], argc);
	prtmsg(mf,msg,LOG_INFO);

	if ( argc != 4 )
	{
		sprintf(msg,"incorrect number of arguments on command line-argc:%d\n",
		    argc);
		prtmsg(mf,msg,LOG_ERR);
		exit(1); /* error return */
	}

	sprintf(msg,"command line arguments follow:\n");
	prtmsg(mf,msg,LOG_INFO);

	for ( i=1 ; i<argc ; i++ )
	{
		sprintf(msg,"%s%s", argv[i], ( i < argc -1 ? " " : ""));
		prtmsg(mf,msg,LOG_INFO);
	}  /*   end for   */

	sprintf(msg,"\n");
	prtmsg(mf,msg,LOG_INFO);

	/*  test for excessive output file name length in the 1st argument  */

	if ( (int)strlen(argv[1]) > PTHSIZ + FILSIZ  )
	{
		sprintf(msg,"output path/file name is too long:%s\n", argv[1]);
		prtmsg(mf,msg,LOG_ERR);
		return(TRUE);
	}

	strcpy(file,argv[1]);   /*  nominal n-day output file name:R01.SVD  */

	if ( (int)strlen(argv[2]) != 21 || (int)strlen(argv[3]) != 21 )
	{
		sprintf(msg,"bad first/last day of year argument length:%s   %s\n", argv[2], argv[3] );
		prtmsg(mf,msg,LOG_ERR);
		return(TRUE);
	}

	strcpy(firstdoy,argv[2]);
	strcpy( lastdoy,argv[3]);

	if ( ( status=rsndaysvf( file, firstdoy, lastdoy ) ) == APS_REPORT_OK )
	{
		sprintf(msg,"radarsat n-day state vector file processed successfully\n");
		prtmsg(mf,msg,LOG_INFO);
	}
	else
	{
		sprintf(msg,"error processing n-day state vector file:%s -  exit code:%d\n",
                             file,status);
		prtmsg(mf,msg,LOG_ERR);
	}

exit(status); 

}  /* end main function */

#endif   /*  MAIN  */


/*=============================================================================
Function:    rsndaysvf.c
Description: This function creates the n-day state vector file ordered by
             rev number. The function is called with 3 arguments, which are
             the output file name, an epoch identifying the 1st day of year
             filename, and an epoch identifying the last day of year filename.
             Given these two epochs, the function automatically calculates all
             input state vector day of year filenames and copies their contents
             to the output n-day file. The input file names are in the form
             r1_yyyyddd, where yyyy is the year, and ddd is the day of year.
             From this input, it follows that the n-day state vector file will
             contain last - first + 1 day of year state vector files.
             These files were created by the rsdoysvf program and contain all
             received state vectors for their respective day of year. Each
             state vector record contains a rev number. This rev number 
             identifies the state vector to be used for this rev. All input 
             files are located in an APS directory controlled by an environment
             variable named APS_RADARSAT_SVF. APS_RADARSAT_SVF and r1_yyyyddd  
             are input to the aps function named aps_fullpath, which constructs
             the actual full path/filename and returns a pointer to it. 
             The output n-day state vector full path/file is input in the first 
             argument and may reside in any valid directory. 
             This function performs the following processing steps:
             1. Performs tests to ensure valid 21 character input epochs in the
                year and day of year fields of the arguments. The year must be 
                >= 1995; the day of year 1-366 for a leap year, or 1-365 
                otherwise.
             2. opens the output n-day state vector file to write.
             3. for each of the n-days:
                3.1 open the input file r1_yyyyddd.
                3.2 input the header and meta data records.
                3.3 test for valid header and meta data records after 
                    receiving the first pair of these records, usually from
                    the start day. Identify any differences.
                3.4 write (append) to the n-day file all state vector records
                    in the current day of year file.
                3.5 construct the next day of year filename. Note that the last
                    day of year file name is really the first day past the end  
                    of the  n-day file. This file name is used later to test 
                    for missing state vectors at the end of the n-day file. 
             4. test that the n-day file has been created successfully.
             5. write dummy header and meta data records that contain negative
                rev numbers to the end of the n-day file. These records must
                be written with the exact record lengths of these records,
                since they will be overwritten later with the true header and 
                meta data records after the n-day file is sorted.
             6. construct the unix shell sort command and use the system
                function named system to execute the sort utility. close the
                n-day file before issuing the sort command, so that no file
                conflict will exist.
             7. upon a successfull sort execution, reopen the n-day file
                this time to read and write.
             8. input the header and meta data records and test to ensure that
                the negative rev numbers sorted to the top of the file: the
                header first, then the meta data record.
             9. Set the file position ( fseek ) to the start of the file, then
                overwrite the true header and meta data records to the n-day
                output file.
             10.This step is most important. The n-day file must now be closed
                again, then reopened for read only, in order to flush (force
                write) the header and meta data records to the n-day file.
                Failure to execute the close/reopen causes the system to lose
                file position, and instead of being positioned to the first
                state vector record, file position remains at the start of the
                file. You will be reading the header again, when you are
                expecting a state vector record.
             11.Space over the header and meta data records to position the
                n-day file to the first state vector record in preparing to
                test for missing state vectors.
             12.construct the file name for day of year preceding first day of
                year in the n-day file. This is the day before file used
                for continuity testing of missing state vectors at the start
                of the n-day file. This file will be searched by the getrev
                and maxrev functions to find and return the highest rev number
                in the day before file. Open the day before file.
             13.If the day before file fails to open, then start missing state
                vector testing with the 1st state vector in the n-day file.
             14.test for all missing state vectors in the n-day file until
                the end of file occurs. Notify the operator of any missing 
                state vectors.
             15.When the end of file occurs on the n-day file, open the
                first day of year file past the last day of the n-day file.
             16.When the file opens successfully, it is searched by the getrev
                and minrev functions to find and return the lowest numbered
                rev from this file. The last rev in the n-day file and the
                the lowest rev number returned by the getrev and minrev can
                then be compared to detect missing rev(s) at the end of the
                n-day file. When the file fails to open successfully, the 
                last rev in the n-day file cannot be tested for missing
                state vector(s). The operator is notified of this condition. 
             17.print the number of state vectors in the n-day file, then
                return to the main function.

Input arguments:
        char *file
        = character string containing the output full path/file name
        char *firstdoy
        = character string containing start epoch in the form:
          yyyy:ddd:hh:mm:ss.fff [year,day of year,hours,minutes,seconds]
        char *lastdoy
        = character string containing stop epoch in the form:
          yyyy:ddd:hh:mm:ss.fff [year,day of year,hours,minutes,seconds]
Returns:
        int:
        = 0 normal completion; no errors detected.
        = 1 abnormal ending; processing errors detected.

Creator:    Ted Pavlovitch

Creation date: 03/27/95

Notes: This function is designed to continue processing as long as possible
       in an attempt to always return an n-day file even if one or more
       day of the year files fail to open or are missing. It will terminate
       processing only in the event of an absolute error, such as the failure
       to write a state vector record successfully, or a read error occurs
       that does not allow a state vector to be input.

===========================================================================*/
int  rsndaysvf( char *file, char *firstdoy, char *lastdoy )
{

#define SORTCMDSIZ PTHSIZ+FILSIZ+64
#define HDRECL  52    /* head record length + 2 in bytes          */
#define MDRECL  36    /* meta data record length + 2 in bytes     */
#define SVRECL 111    /* state vector record length + 11 in bytes */
#define BIGNUM 0X7FFFFFFF

	FILE *mf;        /* message file */
	FILE *ofp;       /* output n-day state vector file */
	FILE *ifp;       /* input day of year state vector file */

	int  rev, lastrev;
	int  i, sys_status;
	int  nsvif, nsvof;
	int  hdr, saved , okay;
	int  idoy, iyear, month, day, jds, jde, ndays, iyyyyddd;
        int  decade, hh, mm, ss, ms ;
	char cdoy[] = "ddd", cyear[] = "yyyy", cyyyyddd[] = "yyyyddd", r1_yyyyddd[] = "r1_yyyyddd" ;
	char firstdoyfile[] = "r1_yyyyddd" ;
	char crev[] = "nnnnn";
	char  ibuf[BUFSIZ],  jbuf[BUFSIZ];
	char hdrec[HDRECL], mdrec[MDRECL];
	char *db4fname;
	char *doyfname;
	char sortcmd[SORTCMDSIZ] = "sort -o";
	char sorttyp[] = " -n +0.0 -0.0 ";

	typedef struct
	        {
		int  rev;
		char epoch[22];
		union
		           {
			double vector[5];
			struct
			              {
				double  x;
				double  y;
				double  z;
				double dx;
				double dy;
				double dz;
			}coords;
		}stvect;
	}VECTORS;

	VECTORS vectors ;

	mf = stdout;

	/*  construct first day of year file name  */

	sprintf(r1_yyyyddd,"r1_%.4s%.3s",firstdoy,firstdoy+5);

	if (!tc_parse_asftime(
			firstdoy, &iyear, &decade, &idoy, &hh, &mm, &ss, &ms ) )
	{
		sprintf(msg,"start date/time argument error:%s\n",firstdoy);
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_ERROR);
	} 


	/* 
	-- calculate julian day of first day of year argument; 
	-- but first need to convert day of year to
	-- month and  day for julian day function.
	*/

	if ( !tc_doy2cal( iyear, idoy, &month, &day ) )
	{
		sprintf(msg, 
			"error converting start date argument year:%d  doy:%d to calendar month/day\n", 
			iyear, idoy );
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_ERROR);
	}

	jds = tc_cal2julian( iyear, month, day );

	if (!(tc_parse_asftime( 
		lastdoy, &iyear, &decade, &idoy, &hh, &mm, &ss, &ms)))
	{
		sprintf(msg,"end date/time argument error:%s\n",lastdoy);
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_ERROR);
	} 

	if ( !tc_doy2cal( iyear, idoy, &month, &day ) )
	{
		sprintf(msg,"error converting end date argument year:%d  doy:%d to calendar month/day\n", iyear, idoy );
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_ERROR);
	}

	jde = tc_cal2julian( iyear, month, day );

	if ( jds > jde )
	{
		sprintf(msg,"start date argument:%s  >  end date argument:%s\n", firstdoy, lastdoy);
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_ERROR);
	}

	/* calculate the number of days in the n-day output file */

	ndays = jde - jds + 1;

	/* open the n-day output file to write */

	if ( (ofp = fopen(file,"w")) == NULL )
	{
		sprintf(msg,"%s %s\n", file, "output file open failure");
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_FILE_ERROR);
	}

	sprintf(msg,"n-day output file name:%s\n",file);
	prtmsg(mf,msg,LOG_INFO);

	/* construct the full path/file name of the first day of year input file */
        doyfname = aps_fullpath(APS_RADARSAT_SVF,r1_yyyyddd);

	strcpy(firstdoyfile,r1_yyyyddd);   /*  save the first day of year input file name.  */

	/* begin constructing the n-day output file ; move the first of the day of year files
	 * to the output n-day file, then append the next n-1 consecutive days' files
	 * to the output n-day file.
	 */

	nsvof = 0;
	saved = FALSE;

	for ( i=1 ; i<=ndays ; i++ )
	{
        okay = TRUE;
	sprintf(msg,"\nprocessing daily vector file:%s\n",doyfname);
	prtmsg(mf,msg,LOG_INFO);

		/* open the current day of the year input file to read */
		if ( (ifp = fopen(doyfname,"r")) == NULL)
		{
                        okay = FALSE;
			sprintf(msg,"file open failure\n");
			prtmsg(mf,msg,LOG_WARNING);
		}

		else

		{

			fgets(ibuf,HDRECL,ifp);
			if ( !feof(ifp) )
			{
				if ( ferror(ifp) )
				{
					hdr = FALSE;
                                        okay = FALSE;
					sprintf(msg,"read error trying to read header from file\n");
					prtmsg(mf,msg,LOG_WARNING);

				}
				else
				{
					hdr = TRUE;
				}
				fgets(jbuf,MDRECL,ifp);
				if ( !feof(ifp) )
				{
					if ( ferror(ifp) )
					{
                                                okay = FALSE;
						sprintf(msg,"read error trying to read meta data record from file\n");
						prtmsg(mf,msg,LOG_WARNING);
					}
					else if ( saved )
					{
						if ( strncmp(&ibuf[22],&hdrec[22],10) != 0 )
						{
                                                        okay = FALSE;
                                                        sprintf(msg,"header:%s does not match n-day file header:%s\n",&ibuf[22],&hdrec[22]);
							prtmsg(mf,msg,LOG_WARNING);
						}
						if ( strcmp(jbuf,mdrec) != 0 )
						{
                                                        okay = FALSE;
                                                        sprintf(msg,"meta data record:%s does not match n-day file meta data record:%s\n",jbuf,mdrec);
							prtmsg(mf,msg,LOG_WARNING);
						}
					}
					else if ( hdr )
					{
						saved = TRUE;
						strcpy(hdrec,ibuf);
						strcpy(mdrec,jbuf);
					}

					nsvif = 0;

					while ( (fgets(ibuf,SVRECL,ifp)) != NULL )
					{
						nsvif+=1;
						/* copy record to the n-day output file */
						if ( (fputs(ibuf,ofp)) == EOF )
						{
							sprintf(msg,"error writing record:%s to n-day output file\n",ibuf);
							prtmsg(mf,msg,LOG_ERR);
							close_files( ifp, ofp );
							return(APS_REPORT_FILE_ERROR);
						}
						else
						{
							nsvof+=1;
						}
					}  /* end while */

					if ( feof(ifp) )
					{
						/* expected normal end of file return; check if state vector
						                * count in file > 0, i.e., were any state vectors input?
						                    */
						if ( nsvif == 0 )
						{
                                                        okay = FALSE;
							sprintf(msg,"warning - file is empty-no state vectors in the file\n");
							prtmsg(mf,msg,LOG_WARNING);
						}
					} /* end if feof */
					else if ( ferror (ifp) )
					{
						/* read error reading current day of year file */
						sprintf(msg,"read error copying records from file:%s to file:\n%s\n",doyfname
						   , file);
						prtmsg(mf,msg,LOG_ERR);
						sprintf(msg,"number of records copied:%d\n",nsvof);
						prtmsg(mf,msg,LOG_ERR);
						close_files( ifp, ofp );
						return(APS_REPORT_FILE_ERROR);
					}
				}
				else
				{
                                        okay = FALSE;
					sprintf(msg,"end of file trying to read meta data record\n");
					prtmsg(mf,msg,LOG_WARNING);
				}
			}
			else
			{
                                okay = FALSE;
				sprintf(msg,"end of file trying to read header record\n");
				prtmsg(mf,msg,LOG_WARNING);
			}
			fclose(ifp);
		} /* end if ifp=fopen */

                if( okay )
		{
                sprintf(msg,"OK\n");
                prtmsg(mf,msg,LOG_INFO);
	        }

		/* construct next day of year input file name. */

		strncpy(cdoy,&r1_yyyyddd[7],3);
		idoy = atoi(cdoy);

		if ( idoy >= 365 )
		{
			strncpy(cyear,&r1_yyyyddd[3],4);
			iyear = atoi(cyear);
			if ( tc_leapyr(iyear) )
			{
				if ( idoy == 365 )
				{
					idoy = 366;  /* day of year incremented by 1; same year */
				}
				else
				{
					++iyear;     /* year incremented by 1        */
					idoy = 1;    /* day of year incremented by 1 */
				}
				iyyyyddd = 1000*iyear + idoy;
                                sprintf(cyyyyddd,"%7ld",iyyyyddd);
			}
			else
			{
				++iyear;   /* not leap year, so increment year by 1 */
				idoy = 1;  /* day of year incemented by 1           */
				iyyyyddd = 1000*iyear + idoy;
                                sprintf(cyyyyddd,"%7ld",iyyyyddd);
			}
			strncpy(&r1_yyyyddd[3],cyyyyddd,7);
		}
		else
		{
			/* not end of year; so increment day of year by 1 */
			++idoy;
                        sprintf(cdoy,"%3.3ld",idoy);
			strncpy(&r1_yyyyddd[7],cdoy,3);
		}

		doyfname = aps_fullpath(APS_RADARSAT_SVF,r1_yyyyddd);

	} /* end for i=1; i<=ndays */

	/* test that we have a header and meta data record for the n-day file */

	if ( !saved )
	{
		sprintf(msg,"did not get a header and a meta data record for the n-day file-run aborts\n");
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_ERROR);
	}

	/* test state vector count in n-day file; must be > 0 */

	if ( nsvof == 0 )
	{
		sprintf(msg,"no state vectors copied to n-day output file; run aborts\n");
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_ERROR);
	}
	else
	{
		sprintf(msg,"%d state vectors copied to n-day output file\n",nsvof);
		prtmsg(mf,msg,LOG_INFO);
	}

	/* write dummy header and meta data records to the output n-day file. These
	 * records contain negative rev numbers in the first five characters, while
	 * maintaining the true record lengths. We need these to fool the sort and 
	 * force it to sort these two records to the top of the file. After the sort
	 * we'll overwrite these two records with the first header and meta data 
	 * records input successfully, not necessarily the records in the 1st day of
	 * the n-day file.
	 */

	strcpy(ibuf,"-8888 ");     /* rev number for header  */
	for (i = 6 ; i <= (int)(strlen(hdrec)) - 2 ; i++)
		ibuf[i] = '.';             /* pad to the header size */
	ibuf[strlen(hdrec) - 1] = '\0';   /* end of string */

	if ( ( fprintf(ofp,"%s\n",ibuf)) < 0 )
	{
		sprintf(msg,"write error trying to write dummy header:%s for sort\n",ibuf);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}

	strcpy(ibuf,"-9999 "); /* rev number for meta data record  */
	for (i = 6 ; i <= (int)(strlen(mdrec)) - 2 ; i++)
		ibuf[i] = '.';         /* pad to meta data record size     */

	ibuf[strlen(mdrec) - 1] = '\0';       /* end of string */

	if ( ( fprintf(ofp,"%s\n",ibuf)) < 0 )
	{
		sprintf(msg,"write error trying to write dummy meta data record:%s for sort\n",
		    ibuf);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}

	/* everything okay to here; sort the output n-day state vector file.
	 * Need to concatenate n-day file name to sort command first to
	 * complete full sort command. use the system function to spawn a
	 * sub process to exit to the unix shell for the purpose of executing
	 * the unix sort utility. Because this program must wait for the sort
	 * to complete, it must close the n-day file here, then re open it
	 * after a successfull sort.
	 */

	fclose(ofp);
	strcat(sortcmd,file);
	strcat(sortcmd,sorttyp);
	strcat(sortcmd,file);

	/* call the system sort to sort the n-day state vector file. */

	sys_status = system(sortcmd);

	/* check system status to determine if sort was successful */

	if ( sys_status == -1 )
	{
		sprintf(msg,"shell sort command failure returned by system status:%d\n",
		    sys_status);
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_ERROR);
	}

	/* re open the n-day state vector file to read and write; need to write the
	 * header and meta data record to the n-day file; thereafter we only read 
	 * the file.
	 */

	if ( (ofp = fopen(file,"r+")) == NULL )
	{
		sprintf(msg,"%s %s\n", file, "file open failure after sorting");
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_FILE_ERROR);
	}

	/* first read the dummy headers back in to check that negative rev numbers 
	 * sorted to the top of the file. rev -8888 is the header; rev -9999 is the
	 * meta data record.
	 */

	if ( (fgets(ibuf,HDRECL,ofp)) != NULL )
	{
		if ( strncmp(ibuf,"-8888",5) != 0 )
		{
			sprintf(msg,"post sort dummy header not first record in the output file \n\
post sort header:%s\n",
			    ibuf);
			prtmsg(mf,msg,LOG_ERR);
			fclose(ofp);
			return(APS_REPORT_ERROR);
		}
	}
	else if ( feof(ofp) )
	{
		sprintf(msg,"end of file trying to read header after sort in file:%s\n",
		    file);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}
	else if ( ferror(ofp) )
	{
		sprintf(msg,"read error trying to read header after sort in file:%s\n",
		    file);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}

	if ( (fgets(ibuf,MDRECL,ofp)) != NULL )
	{
		if ( strncmp(ibuf,"-9999",5) != 0 )
		{
			sprintf(msg,"post sort dummy meta data record not second record in the output file;post sort meta data record:%s\n",
			    ibuf);
			prtmsg(mf,msg,LOG_ERR);
			fclose(ofp);
			return(APS_REPORT_ERROR);
		}
	}
	else if (feof(ofp) )
	{
		sprintf(msg,"end of file trying to read meta data record after sort in file:%s\n",file);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}
	else if ( ferror(ofp) )
	{
		sprintf(msg,"read error trying to read meta data record after sort in file:%s\n",file);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}


	/* rewind the file and overwrite the true header and meta data records.
	 * If the rewind fails, try closing the file and reopening it.
	 */

	if ( (fseek(ofp,0l,SEEK_SET)) != 0 )
	{
		/*  printf("fseek failed\n");  */
		fclose(ofp);
		if ( (ofp = fopen(file,"r+")) == NULL )
		{
			sprintf(msg,"%s %s\n",file,"file open failure after rewind fails");
			prtmsg(mf,msg,LOG_ERR);
			return(APS_REPORT_FILE_ERROR);
		}
	}

	/* write the header record      */

	if ( (i=fprintf(ofp,"%s",hdrec)) < 0 )
	{
		sprintf(msg,"write failure trying to overwrite dummy header in file:%s\n",
		    file);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}

	/* write the meta data record   */

	if ( (i=fprintf(ofp,"%s",mdrec)) < 0 )
	{
		sprintf(msg,"write failure trying to overwrite dummy meta data record in file:%s\n",file);
		prtmsg(mf,msg,LOG_ERR);
		fclose(ofp);
		return(APS_REPORT_FILE_ERROR);
	}

	/* Because we just overwrote the header and meta data records, we need to close the n-day output
	 * file to flush the true headers to the file; otherwise, subsequent reads will not read the
	 * file correctly. So, first close the file. Then, re open it to read only.
	 */

	fclose(ofp);

	/* re open the n-day state vector file to read only; need to read the
	 * header and meta data records to position the n-day file to 1st state  
	 * vector record; thereafter we only read state vectors from the file.
	 */

	if ( (ofp = fopen(file,"r")) == NULL )
	{
		sprintf(msg,"%s %s\n", file, "file open failure after overwriting headers");
		prtmsg(mf,msg,LOG_ERR);
		return(APS_REPORT_FILE_ERROR);
	}

	/* space over the header and meta data records; the file is positioned to first state vector record */

	fgets(ibuf,HDRECL,ofp);
	fgets(ibuf,MDRECL,ofp);

	/* file sorted successfully; dummy headers successfully replaced with the 
	 * correct records; construct the file name for day of year immediately 
	 * preceding the n-day file's first day of year. Need this to get last
	 * rev number just prior to the first day of the n-day file for detecting
	 * missing rev numbers. Also, n-day file is now positioned to the first
	 * state vector in the file.
	 */

	strncpy(cdoy,&firstdoyfile[7],3);
	idoy = atoi(cdoy);

	if ( idoy == 1 )
	{
		strncpy(cyear,&firstdoyfile[3],4);
		iyear = atoi(cyear) - 1; /* previous year */
                idoy = 365 + tc_leapyr(iyear);
		iyyyyddd = 1000*iyear + idoy;
                sprintf(cyyyyddd,"%7ld",iyyyyddd);

	}
	else
	{
		idoy--;
                sprintf(cdoy,"%3.3ld",idoy);
		strncpy(cyyyyddd,&firstdoyfile[3],4);
		strncpy(&cyyyyddd[4],cdoy,3);
	}


	strncpy(&r1_yyyyddd[3],cyyyyddd,7);

	db4fname = aps_fullpath(APS_RADARSAT_SVF,r1_yyyyddd);

	sprintf(msg,"day before 1st day of year in n-day file name:%s\n",db4fname);
	prtmsg(mf,msg,LOG_INFO);


	/* Open the day of year file previous to the first day of the n-day file
	 * and begin checking for missing state vectors by comparing the rev
	 * numbers on consecutive state vector records. This begins looking for missing
	 * state vectors at the start of the file. Then check all state vectors
	 * in the n-day file between consecutive records; they should differ
	 * by one. Also look for a missing state vector at the end of the n-day
	 * file. report any missing state vectors to the operator.
	 */

	if ( (ifp = fopen(db4fname,"r")) == NULL )
	{
		/* file could not be opened or does not exist */
		sprintf(msg,"%s file open failure\n",db4fname);
		prtmsg(mf,msg,LOG_WARNING);

		sprintf(msg,"missing state vector testing will begin with 1st state vector in the n-day file\n");
		prtmsg(mf,msg,LOG_WARNING);

		/* input first state vector record in the n-day file */

		if ( (fgets(ibuf,SVRECL,ofp)) == NULL )
		{

			sprintf(msg,"can't read 1st state vector record in the n-day file:%s trying to test for missing state vector[s]\n",
			    file);
			prtmsg(mf,msg,LOG_ERR);
			fclose(ofp);
			return(APS_REPORT_FILE_ERROR);
		}
		strncpy(crev,ibuf,5);
		lastrev = atoi(crev);
		nsvof = 1;
#if defined (DEBUG)
		fprintf(mf,"1st sv rec:%s\n",ibuf);
		fprintf(mf,"crev=%s ; lastrev=%d   press return to continue\n",crev,lastrev);
		getchar();
#endif  /* DEBUG */
	}
	else
	{
		/* get last rev number in the file preceding the first day in the n-day file */
		lastrev = -BIGNUM;  /* will look for maximum rev number */
		if ( !getrev( mf,ifp,ibuf,db4fname,&lastrev,maxrev ) )
		{
			sprintf(msg,"1st state vector rev at start of n-day file could not be tested for missing state vector[s]\n");
			prtmsg(mf,msg,LOG_WARNING);

			sprintf(msg,"missing state vector testing will begin with 1st state vector in the n-day file\n");
			prtmsg(mf,msg,LOG_WARNING);

			/* input first state vector record in the n-day file */
			if ( (fgets(ibuf,SVRECL,ofp)) == NULL )
			{
				sprintf(msg,"can't read 1st state vector record in the n-day file:%s trying to test for missing state vector[s]\n",
				    file);
				prtmsg(mf,msg,LOG_WARNING);
				fclose(ofp);
				return(APS_REPORT_FILE_ERROR);
			}
			strncpy(crev,ibuf,5);
			lastrev = atoi(crev);
			nsvof = 1;
		}
		else
		{
			nsvof = 0;
		}
	}

	/* start testing for missing state vectors. */

	for ( ; ; )
	{
		if ( (fgets(ibuf,SVRECL,ofp)) != NULL )
		{
			nsvof++;
			strncpy(crev,ibuf,5);
			rev = atoi(crev);
			if ( rev != lastrev + 1 )
			{
				sprintf(msg,"state vector[s] missing after rev:%d and before rev:%d\n",
				    lastrev, rev);
				prtmsg(mf,msg,LOG_WARNING);
			}
			lastrev = rev;
		}
		else if ( feof(ofp) )
		{
			/* normal end of file return; test last rev in the n-day file versus
			    * the 1st rev in 1st day of year file past the end of the n-day file.
			    * Open this file; the file name was calculated in the last pass of the 
			    * loop that wrote the n-day state vector file. 
			    */
			if ((ifp = fopen(doyfname,"r")) == NULL )
			{
				sprintf(msg,"%s file open failure\n",doyfname);
				prtmsg(mf,msg,LOG_WARNING);
				sprintf(msg,"can't test for missing state vector[s] at end of n-day file\n");
				prtmsg(mf,msg,LOG_WARNING);
			}
			else
			{
				rev = BIGNUM;  /* will look for minimum rev number */
				if ( !getrev( mf,ifp,ibuf,doyfname,&rev,minrev ) )
				{
					sprintf(msg,"last rev in n-day file could not be tested for missing state vector\n");
					prtmsg(mf,msg,LOG_WARNING);
				}
				else
				{
					if ( rev != lastrev + 1 )
					{
						sprintf(msg,"state vector[s] missing after rev:%d and before rev:%d\n",
						    lastrev, rev);
						prtmsg(mf,msg,LOG_WARNING);
					}
				}
			}
			break;
		}
		else if ( ferror(ofp) )
		{
			sprintf(msg,"error reading n-day file:%s while testing for missing state vectors\n", file);
			prtmsg(mf,msg,LOG_WARNING);
			nsvof++;
			sprintf(msg,"error occured at state vector record number:%d\n", nsvof);
			prtmsg(mf,msg,LOG_WARNING);
			lastrev++;  /* assumes error record had next expected rev number */
		}
	}  /* end for */

	sprintf(msg,"n-day file:%s contains %d state vectors \n",file, nsvof);
	prtmsg(mf,msg,LOG_INFO);

	return(APS_REPORT_OK);

} /* end function rsndaysvf */



/*=============================================================================
Function:    close_files.c
Description: This function closes a pair of files, given a pair of file
             pointers. It uses the fclose function to close the files and
             requires the include <stdio.h> statement. It is used to close
             both input and output files in a single statement.

Input arguments:  
        FILE *ifp   
        = a pointer to a file structure (input file).
        FILE *ofp
        = a pointer to a file structure (output file)

Returns:
        void

Creator:   Ted Pavlovitch

Creation date: 03/27/95

===========================================================================*/
void close_files(FILE *ifp, FILE *ofp )
{
	fclose(ifp);  /*  input day of year file        */
	fclose(ofp);  /*  n-day state vector file       */
	return;
} /* end function close_files */



/*===========================================================================
Function:    getrev.c

Description: This function searches a day of the year file and returns either
             the minimum or maximum rev number in a day of the year file. The
             function is called with the maxrev function as an argument in the
             calling sequence when the maximum rev number in the file is to be
             found. Alternatively, the function is called with the minrev
             function as an argument in the calling sequence when the minimum
             rev number in the file is to be found. This function is called by
             the rsndaysvf function to find the maximum rev number in the day 
             of year file prior to the first day of year in the n-day file,
             for the purpose of testing for missing state vector(s) at the 
             start of the n-day file. Similarly, this function is called by
             the rsndaysvf function to find the minimum rev number in the  
             first day of year file past the last day of year in the n-day
             file for the purpose of testing for missing state vector(s) at the
             end of the n-day file. The minrev and maxrev functions must be
             initialized to positive and negative big number values 
             respectively prior to calling the getrev function.

Input arguments:
      FILE *mf  
      = a pointer to a file structure where non syslog messages are directed.
        It is currently set to stdout; however, it can be changed to any other
        file. This is the first argument to the function fprintf in <stdio.h>.
      FILE *ifp 
      = a pointer to the input day of the year file.
      char *ibuf 
      = a pointer to an input buffer where the records are input.
      char *doyfname
      = a day of the year file name containing state vector records to be 
        searched for min/max rev numbers. This filename must be either the
        day of year before the first day of year in the n-day file, or the
        first day of year past the last day of year in the n-day file. The 
        pointer must be the pointer returned from a call to aps_fullpath, which
        controls the APS directory in which the files reside via an environment
        variable.
      minmaxf()
      = an integer function, either a minimum or maximum function, to be used
        by getrev in searching the file for the desired min or max rev number.
        The function must be initialized to + or - a big number prior to 
        calling getrev.

Returns:
      int *mrev 
      = a pointer to an integer in the calling function where the minimum or
        maximum rev number found in the file is returned by getrev. The object
        of the search.  

Creator: Ted Pavlovitch

Creation date: 03/27/95

Note: the input day of year file is not necessarily in order by rev number; 
      for this reason the entire file is searched.

===========================================================================*/
int getrev(FILE *mf, FILE *ifp, char *ibuf, char *doyfname, int *mrev, void (*minmaxf)())
{

	int  initval;
        int  revs;
        
	initval = *mrev;

	/* space over the header and meta data records */

	fgets(ibuf,HDRECL,ifp);
	fgets(ibuf,MDRECL,ifp);

	for ( ; ; )
	{
		if ( (fgets(ibuf,SVRECL,ifp)) != NULL )
		{
			if ( (sscanf(ibuf,"%5d",&revs)) != 1 )
			{
				sprintf(msg,"can't convert rev number to integer in state vector:\n%s\n in file:%s\n", ibuf
				   , doyfname);
				prtmsg(mf,msg,LOG_WARNING);
			}
			else
			{
				minmaxf(mrev,&revs);
#if defined (DEBUG)
				sprintf(msg,"minmax:%d doyfname:%s\n",*mrev,doyfname);
				prtmsg(mf,msg,LOG_DEBUG);
#endif  /* DEBUG */
			}
		}
		else if ( feof(ifp) )
		{
			/* end of file return; check that we found min or max rev number */
#if defined (DEBUG)
			sprintf(msg,"min/max rev:%d doyfname:%s\n", *mrev, doyfname);
			prtmsg(mf,msg,LOG_DEBUG);
#endif  /* DEBUG */

			if ( *mrev == initval )
				return(FALSE);
			break;
		}
		else if ( ferror(ifp) )
		{
			sprintf(msg,"error reading day of year state vector file:%s\n%s\n",doyfname,
			    "while searching for 1st/last rev at start/end of n-day file");
			prtmsg(mf,msg,LOG_WARNING);
		}
	} /* end for */

	return(TRUE);

} /* end function getrev */



/*=============================================================================
Function:    maxrev

Description: This function returns a maximum value after comparing two rev
             numbers. The function must be initialized to minus a big number 
             value prior to its first call.

Input arguments:
        int lastrev
               = The currently existing maximum rev number during the process
                 of finding the maximum rev number. Each rev number is compared
                 to this running maximum value. This is also the integer return
                 value of the function.
        int revs
               = An integer rev number containing the current rev number to be
                 compared to the maximum rev number currently existing in the
                 process of finding the maximum rev number.

Returns:
        int lastrev
               = the maximum rev number in the day of the year file.

Creator:   Ted Pavlovitch

Creation date: 03/24/95

===========================================================================*/
void maxrev( int *lastrev, int *revs )
{
	if ( *revs > *lastrev )
		*lastrev = *revs;
	return;
} /* end function maxrev */



/*=============================================================================
Function:    minrev

Description: This function returns a minimum value after comparing two rev
             numbers. The function must be initialized to a positive big  
             number value prior to its first call.

Input arguments:
        int firstrev
               = The currently existing minimum rev number during the process
                 of finding the minimum rev number. Each rev number is compared
                 to this running minimum value. This is also the integer return
                 value of the function.
        int revs
               = An integer rev number containing the current rev number to be
                 compared to the minimum rev number currently existing in the
                 process of finding the minimum rev number.

Returns:
        int firstrev
        = the minimum rev number in the day of the year file.

Creator:   Ted Pavlovitch

Creation date: 03/24/95

===========================================================================*/
void minrev( int *firstrev, int *revs )
{

	if ( *revs < *firstrev )
		*firstrev = *revs;
	return;
} /* end function minrev */



/*=============================================================================
Function:    prtmsg.c

Description: This function conditionally prints messages to a standard output
             file and/or to syslog. The standard output file is currently
             stdout (the terminal screen) and is written by a call to fprintf.
             Output to syslog is via a call to the syslog function. Either or
             both calls can be compiled into the program by the C language
             pre processor. The defined names for compiling the fprintf and
             and syslog function calls are STDMSG and LOGMSG respectively.

Input arguments:
        FILE *mf  
        = a pointer to a file structure where non syslog messages are directed.
        It is currently set to stdout; however, it can be changed to any other
        file. This is the first argument to the function fprintf in <stdio.h>.
        char *msg 
        = a character string containing the message to be output by the fprintf
          and syslog functions. This message is created by a call to sprintf
          externally and the formatted string is input to prtmsg for output.
        MSG_TYP 
        int:
        = a value defined by a define statement found in <syslog.h> used to
          identify the message type. For example, LOG_INFO, LOG_WARNING,
          LOG_ERR, LOG_DEBUG, LOG_NOTICE, etc.

Returns:
        void

Creator:  Ted Pavlovitch

Creation date: 03/27/95

===========================================================================*/
void prtmsg(FILE *mf, char *msg, int MSG_TYP )
{

#if defined (STDMSG)
	fprintf(mf,"%s",msg);
#endif

#if defined (LOGMSG)
	syslog(MSG_TYP,msg);
#endif

}
