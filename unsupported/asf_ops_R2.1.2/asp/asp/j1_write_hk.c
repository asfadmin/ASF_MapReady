/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  j1_write_hk( hk_buf, start_time, prf, hk_count, file ) ----------------
	Write contents of buffer containing sequence of J-ERS-1 HK data sets 

    Inputs:

	HK_BUF:  Buffer containing de-interleaved, non-inverted HK data
	START_TIME:  String containing GMT of first HK set
	PRF:  Value of PRF of data take
	HK_COUNT:  Number of HK data sets
	FILE:  Name of file into which interpreted HK data will be written

Each HK data set consists of 12 bytes, the first 9 conting the HK data,
the last 3 of which is the format ID.  The point of this routine is to extract
the AGC information of the calibration sequneces and the data collection
operations and write them to a file.  This routine will attempt to collect
at least 4 sequences, 8 maximum.
*/
#include <stdio.h>

int j1_write_hk( hk_buf, start_time, prf, hk_count, file )
unsigned char *hk_buf;
char *start_time, *file;
long hk_count;
float prf;
{
int id, gc;
int seq, step, att, i, j, agc;
unsigned long *hk_ptr;
FILE *out, *fopen();

/*
printf("In J1_WRITE_HK, outfile=%s\n", file );
*/
out = fopen( file, "w" );	/*  open HK file  */
if( out == NULL ){		/*  Check file status  */
    printf( "***  J1_WRITE_HK cannot open output file %s  ***\n", file );
    return( 0 );
}

fprintf( out, "HK_TIME = %s\n", start_time );
fprintf( out, "PRF = %f\n", prf );
agc = ( int ) hk_buf[3] & 0x40;		/*  AGC time constant  */
if( agc == 0 ){
    fprintf( out, "AGC_CONSTANT = 64\n" );
    fprintf( out, "AGC_TIME = %f\n", 64.0/prf );
} else {
    fprintf( out, "AGC_CONSTANT = 128\n" );
    fprintf( out, "AGC_TIME = %f\n", 128.0/prf );
}
fprintf( out, "Number of steps=%d\n", hk_count );
/*  fprintf( out, "SEQUENCES = 8\n" );  */
agc = ( int ) hk_buf[3] & 0x80;
/*
if( agc == 0 ){
    fprintf( out, "Manual Gain Control " );
} else {
    fprintf( out, "Auto Gain Control " );
}
*/

hk_ptr = (unsigned long *)hk_buf;	/*  save input buffer pointer  */
att = ( hk_ptr[1]>>25 ) & 7;		/*  get first attenuation step  */
seq = 0;
while( seq<8 && hk_count>0 ){

    step = att;		/*  set STEP to new value  */
    fprintf( out, "\nSEQ = %5d FORMAT       STEP        AGC\n\n", seq );
    while( step<=att && hk_count>0 ){	/*  Start new sequence number  */

	id = hk_ptr[2] & 0x00ffffff;	/*  get format ID  */
	step = att;
	gc = ( hk_ptr[0]>>1 ) & 0x1f;	/*  get AGC data  */
	fprintf( out, "        %10d %10d %10d\n", id, step, gc );
	hk_count -= 1;		/*  decriment count  */
	hk_ptr += 3;
	att = ( hk_ptr[1]>>25 ) & 7;	/*  next step attenuator value  */
    }
    seq += 1;		/*  set next sequence value  */
}

fclose( out );
return( 1 );
}
