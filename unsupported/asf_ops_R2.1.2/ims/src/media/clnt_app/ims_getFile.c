static char *sccs = "@(#)ims_getFile.c	5.5  09/15/97";
/* *****************************************************************
**
** File:        ims_getFile.c
**
** Function:    this program (subr) read a ceos tape and pulls
**                  off the nth file, as input by the user, and
**                  names it as input by the user.
**              Note:  if the input file does not have /dev/
**                  in it, the program assumes that the file
**                  is a disk file, not tape, and the rewind,
**                  and forward, and other things are not done.
**                  this was done for checkout, but it can be
**                  used to copy a CEOS file.
**
** Author:      David Pass
**
** Date:        6/28/95
**
** Modified:    1/23/97 - D. Pass - R2.1
**                    Added option to discontinue tape size check.
**                      The reportType is >= 100 in this case.
**              7/29/97 - D. Pass - R2.1.1
**                     Added -diff, -neg options. Incresed buffer
**                      size.
**
** Notes:
**
******************************************************************** */

#include    <stdio.h>
#include    <stdlib.h>
#include    <string.h>
#include    <errno.h>
#include    <fcntl.h>
#include    <sys/types.h>
#include    <unistd.h>
#include    <sys/utsname.h>
#include    <time.h>

#include    <errno.h>
#include    <fcntl.h>

#include    <sys/ioctl.h>

/*
** The following non-POSIX definitions are required by the header
** file that follows.
*/
typedef unsigned char u_char;
typedef unsigned short u_short;

#include    <sys/mtio.h>


typedef  unsigned short  boolean;

#define  get_file  main

#define   MAX_LINE  1024     /*  max. name with no hierarchy  */
#define   MAX_NAME  128      /*  max. name with no hierarchy  */
#define   MAX_BUF  40000     /*  max. size of buffer for records */
#define   MATCH_FOUND  0     /*  for loop control.  */

#define  TRUE   1
#define  FALSE  0

long   str_index();  /*  str1, str2  - find where str2 starts in str1 -
                        if not, then -1.   */


get_file( argc, argv )
long   argc;
char *argv[];
{

long  i,j,k,l  ;
char  str[MAX_LINE+1], str2[MAX_LINE+1], str3[MAX_LINE+1];

char    tape_name[MAX_NAME]; /* input tape path */
char    file_name[MAX_NAME]; /*  name for file pulled off tape  */
long    n_file; /*  file number to pull off tape  */
int     tdesc; /*  tape file descriptor  */
int     fdesc; /*  new file descriptor  */
unsigned  char  buf[MAX_BUF];
long  n_recs; /*  no. records read  */
long  n_bytes_sum;/*  sum of bytes read  */
long  nbytes; /*  no. bytes in current record  */
int   ret;
long  rsize; /*  current size of record  */
long  rec_min, rec_max; /* min and max of record size */
short  reading_tape; /* if true, reading tape. if not, file
            (mainly for checkout)  */
short  debug; /*  if true, print size of each record  */
boolean  donot_write; /* if true, file is not written  */
boolean  diff_write; /* when writting, print only if difference
            between the last record size  */
int  status;
boolean  print_neg; /* if true, print all records that are neg */
long  num_neg; /* cntr for no. of records whose length is 0
    (word 8)    */
long  num_zero; /* cntr for no. of records whose length is < 0
    (word 8)  */
long  last_rec_size; /* size of last record  */

struct  mtop  mt_command; /*  ioctl command structure  */
struct  mtget  mt_status; /*  ioctl results structure  */

long sec_clock;          /* Number of seconds returned by time */
struct tm *tm_ptr;       /* tm is defined in system library include
                            file time.h */
char *  ltoa( long );


tape_name[0] ='\0';
file_name[0] = '\0';
n_file = 1;
debug = FALSE;
donot_write = FALSE;
print_neg = FALSE;
diff_write = FALSE;

i = 0;
for( j=1 ; j  <  argc ; j++ ){
    if( argv[j][0]  ==  '-'  ){
        i = 9;
        k = strlen( argv[j] );
        for( l=0 ;  l  <  k  ; l++ )
            if(  isupper( argv[j][l] )   )  argv[j][l] =
                tolower( argv[j][l] );
        if(        strcmp( argv[j],  "-tape" )  ==  0 )  i = 1;
        else  if(  strcmp( argv[j],  "-output" )  ==  0   ||
            strcmp( argv[j],  "-name" )  ==  0 )  i = 2;
        else  if(  strcmp( argv[j],  "-num" )  ==  0  ||
            strcmp( argv[j],  "-file"  )  ==  0 )  i = 3;
        else  if(  strcmp( argv[j],  "-debug" )  ==  0 ) debug = TRUE;
        else  if(  strcmp( argv[j],  "-diff" )  ==  0 ) diff_write = TRUE;
        else  if(  strcmp( argv[j],  "-no_wrt" )  ==  0 )
            donot_write = TRUE;
        else  if(  strcmp( argv[j],  "-neg" )  ==  0 )
            print_neg = TRUE;
        else  if(  strcmp( argv[j],  "-help" )  ==  0 ){
            print_help:;
(void) printf( " ims_getFile reads a file from a CEOS tape.  The user\n" );
(void) printf( "       inputs the file number, and its name.\n" );
(void) printf( "       The tape path is input after get_file.\n" );
(void) printf( "   -tape pname  The path for the tape is\n");
(void) printf( "       input here.  Note that it can also be\n" );
(void) printf( "       input after get_file.\n" );
(void) printf( "   -name fname  The name of the file to be pulled off\n" );
(void) printf( "       tape is input here.  Default is fileN.VDF, where\n" );
(void) printf( "       N is the file number on the tape.  Optionally,\n" );
(void) printf( "       the keyword -output can be used.\n" );
(void) printf( "   -file n  This is the file number to pull off the\n" );
(void) printf( "       tape.  Default is 1.  Optionally the keyword -num\n" );
(void) printf( "       can be used.\n" );
(void) printf( "   -debug  This prints out the size of each record.\n" );
(void) printf( "   -no_wrt  This stops the file from being written, so\n" );
(void) printf( "       only the statistics are collected.\n" );
(void) printf( "   -neg  This prints when a record is 0 or negative.\n" );
(void) printf( "   -diff This is used with debug and only prints a record\n");
(void) printf( "       size if it is different from the previous record.\n" );
(void) printf( "   -help      Print help (this is it).\n");
(void) printf( "   Note:  if the path is a disk file, then the disk file\n" );
(void) printf( "        is copied into fname.\n" );
            (void) printf( "\n" );
            exit( 0 );
        }
        else {
            (void) printf( " *** Command line option not recognized - %s\n",
                argv[j] );
        }
    }
    else {
        if(  i  ==  0  ){       /*  tape path input  */
            (void) strcpy( tape_name, argv[j] );
        }
        else  if(  i  ==  1  ){ /*  tape path input  */
            (void) strcpy( tape_name, argv[j] );
        }
        else  if(  i  ==  2  ){ /*  new output file  */
            (void) strcpy( file_name, argv[j] );
        }
        else  if(  i  ==  3  ){ /*  file number  */
            n_file = atol( argv[j] );
        }
        else {
            (void) printf( " *** Command line argument not recognized - %s\n",
                argv[j] );
        }
        i = 9;
    }
}

if(  tape_name[0]  ==  '\0' )  goto  print_help;
if(  file_name[0]  ==  '\0' ){ /*  make name for file  */
    (void) strcpy( file_name, "tape" );
    (void) strcpy( str,  ltoa( n_file ) );
    (void) strcat( file_name, str );
    (void) strcat( file_name, ".VDF" );
}


/* Get the system time and pointer to tm record structure. If
   the address of sec_clock and tm_ptr are NULL, then return error
   status code. */
(void) time(&sec_clock);
if( &sec_clock == NULL)
    exit( -1 );
if( (tm_ptr = localtime(&sec_clock)) == NULL)
    exit( -2 );

(void) strcpy( str, "* JPL ***********************************************" );
(void) strcat( str, "********* Pasadena, CA *"   );
(void) puts( str );
(void) strcpy( str, "*               I M S _ G E T F I L E            REV." );
(void) strcat( str, " 1.0                   *"   );
(void) puts( str );
(void) strcpy( str, "*                                                     ");
(void) strcat( str, "                      *"   );
i = strlen( file_name );
if(  i  >  0  )  {
    k = (80-i)/2-2;
    for( j=0; j  <  i  ;  j++  )  str[k+j] = file_name[j];
    (void) puts( str );
}
(void) strcpy( str, "*   /  /   *******************************************");
(void) strcat( str, "************   :  :   *" );
(void) sprintf(  str2, "%2d:%2d:%2d",  tm_ptr->tm_hour, tm_ptr->tm_min,
    tm_ptr->tm_sec   );
if(  tm_ptr->tm_hour  <  10  )  str2[0] = '0';
if(  tm_ptr->tm_min   <  10  )  str2[3] = '0';
if(  tm_ptr->tm_sec   <  10  )  str2[6] = '0';
tm_ptr->tm_mon++;
(void) sprintf(  str3, "%2d/%2d/%2d",  tm_ptr->tm_mon,  tm_ptr->tm_mday,
    tm_ptr->tm_year  );
if(  tm_ptr->tm_mon    <  10  )  str3[0] = '0';
if(  tm_ptr->tm_mday   <  10  )  str3[3] = '0';

k = 2;
l = strlen( str2 );
for( j=0; j  <  l ;  j++  )  str[k+j] = str2[j];
k = 67;
l = strlen( str3 );
for( j=0; j  <  l ;  j++  )  str[k+j] = str3[j];
(void) puts( str );

if(  str_index( tape_name, "/dev/"  )  ==  -1  ){
    reading_tape =  FALSE;
    (void) printf( "\n     File assumed to be from disk, not tape.\n" );
}
else  reading_tape = TRUE;

(void) printf(  " *****  Read file %ld into %s from %s  *******\n", n_file,
    file_name, tape_name );

tdesc = open( tape_name, O_RDONLY );
if(  tdesc  ==  -1  ){
    (void) printf( "\n Error opening tape:  %s\n",
        strerror( errno ) );
    exit( -5 );
}

if(  !donot_write ){
    fdesc = open( file_name, O_WRONLY |  O_CREAT | O_TRUNC );
    if(  fdesc  ==  -1  ){
        (void) printf( "\n  Error opening file:  %s\n",
            strerror( errno ) );
        exit( -6 );
    }
    i = chmod( file_name, 0755 );
}

/*  make sure the tape is rewound  */
mt_command.mt_op = MTREW;
mt_command.mt_count = 1;
if(  reading_tape ){
    (void) ioctl( tdesc, MTIOCTOP, &mt_command );
    (void) ioctl( tdesc, MTIOCGET, (char *)&mt_status );
    if(  mt_status.mt_erreg  <  0  ){
        (void) printf(
            "\n ********  Error rewinding tape: %s  ******\n",
            strerror( errno ) );
        return -10;
    }
}

/*  now position the tape at the nth file  */
mt_command.mt_op = MTFSF;
mt_command.mt_count = n_file-1;
if(  n_file  >  1  ){
    if(  reading_tape ){
        (void) ioctl( tdesc, MTIOCTOP, &mt_command );
        (void) ioctl( tdesc, MTIOCGET, (char *)&mt_status );
        if(  mt_status.mt_erreg  <  0  ){
            (void) printf(
                "\n ********  Error forwarding tape: %s  ******\n",
                strerror( errno ) );
            return -10;
        }
    }
}

/*  now start reading and writting the tape  */
ret = MATCH_FOUND;
n_recs = 0;
n_bytes_sum = 0;
rec_min = MAX_BUF;
rec_max = 0;
num_neg = 0;
num_zero = 0;
last_rec_size = -101;
while (ret==MATCH_FOUND) {
    /* read first 12 bytes of the record */
    if(  reading_tape  )  nbytes = read( tdesc, buf, MAX_BUF );
    else    nbytes = read( tdesc, buf, 12 );

    switch ( nbytes ) {

        case ( 0 ):
            (void) printf( " *****  Read EOF on tape  *****\n" );
            ret = MATCH_FOUND+1;
            break;
        case ( -1 ):
            ret = MATCH_FOUND+1;
            (void) printf( " *****  Read error on tape  *****\n" );
            break;

        default:
            if(  nbytes  >  0  ){
                rsize = cvt2int(&buf[8]);
                if(  rsize  >  MAX_BUF ){
                    (void) printf(
                        "\n**** Record no. %ld has too large a record:"
                        " %ld.  Continuing.\n", n_recs+1, rsize );
                }
                if(  rsize  <  0  ){
                    num_neg++;
                    if(  print_neg ){
                        (void) printf(
                        "\n**** Record no. %ld has neg. value for "
                        "word 8.\n", n_recs+1 );
                    }
                }
                if(  rsize  ==  0  ){
                    num_zero++;
                    if(  print_neg ){
                        (void) printf(
                        "\n**** Record no. %ld has zero value for "
                        "word 8.\n", n_recs+1 );
                    }
                }
                if(  !reading_tape &&  rsize > 12 ){ /* read rest of
                    record */
                    nbytes = read( tdesc, buf+12, rsize-12 );
                    nbytes += 12;
                }
                if(  debug ){
                    if(  diff_write ){
                        if(  nbytes  !=  last_rec_size  ){
                            (void) printf( "  Rec %3ld   size = %ld\n",
                                n_recs+1, nbytes );
                        }
                    }
                    else{
                        (void) printf( "  Rec %3ld   size = %ld\n",
                            n_recs+1, nbytes );
                    }
                }
                if(  nbytes  >=  MAX_BUF  ){
                    (void) printf(
                        "\n******  Buffer size too small %ld\n",
                        nbytes );
                    nbytes = MAX_BUF;
                }
                /* ****  write file to disk  **** */
                if(  !donot_write )
                    status = write( fdesc, buf, nbytes );
                n_recs++;
                n_bytes_sum += nbytes;
                if(  rec_min  >  nbytes )  rec_min = nbytes;
                if(  rec_max  <  nbytes )  rec_max = nbytes;
            }
            else{
                ret = MATCH_FOUND+1;
                (void) printf( " **** ERROR: read gives %ld bytes.\n",
                    nbytes );
            }
    last_rec_size = nbytes;
    }
}
(void) printf(  "\n ****  Read %ld records and %ld bytes  ****\n",
    n_recs, n_bytes_sum );
(void) printf(  "             Min, max record sizes =  %ld    %ld\n",
    rec_min, rec_max );
if(  num_neg  >  0  |  num_zero  >  0  ){
    (void) printf(  "             Word 8 zeros = %ld   neg = %ld\n",
         num_zero, num_neg );
}

(void) close( tdesc );
if(  !donot_write )  (void) close( fdesc );

exit( 0 );
return 0;
}       /*  get_file  */


long   str_index( str1, str2 )
/*===========================================================
    subr str_index is a utility which gives the index of str2
        within str1.  if no match, returns -1.
  ===========================================================*/
char  str1[];
char  str2[];

{

long   i,j,k;
boolean  flag;

i = strlen( str1 );
j = strlen( str2 );
if(  j  ==  1  )  flag = TRUE;
else  flag = FALSE;
if(  j  >  i  )  return( -1 ); /*  no chance  */
for( k=0 ; k <=  i-j ; k++ ){
    if(  str1[k]  ==  str2[0]  ){ /*  possible match  */
        if(  flag  )  return( k );
        if(  strncmp( str1+k+1, str2+1, j-1 )  ==  0  )
            return( k );
    }
}
return( -1 );
}  /*   str_index   */


char  *ltoa( i_in )
/*===========================================================
    subr ltoa changes a long integer to a character.
  ===========================================================*/
long  i_in;
{
static  char  str_trim[32];

(void) sprintf( str_trim, "%ld", i_in );
return  str_trim;
}  /*  ltoa  */


char  *trimr( str1 )
/*===========================================================
    subr trimr takes all the blanks from the end only of
        the input array.  the returned array may be the same
        as the input array.  input array is not changed.
  ===========================================================*/
char  str1[];

{

short  i,k;


i = strlen( str1 );
if(  str1[i-1]  !=  ' '  ){
    return  str1 ; /*  not any blanks at the end, so return original
        string  */
}
for( k=i-1 ; k  >=  0  ;  k-- ){
    if(  str1[k]  !=  ' ' ){ /*  found last non-blank */
        str1[k+1] = '\0';
        return  str1;
    }
}
*str1 = '\0';
return  str1 ;
}  /*  trimr  */


/* ************************************************************* */
/* convert 4 bytes to an integer */
int cvt2int(unsigned char *cint)
{
   unsigned char *cnum;
   int num;
   int i;

   cnum = (unsigned char *) &num;
   for (i=0;i<4;i++) {
#if DEC
      cnum[3-i] = cint[i];
   }
#else
      cnum[i] = cint[i];
   }
#endif

   return(num);
}
