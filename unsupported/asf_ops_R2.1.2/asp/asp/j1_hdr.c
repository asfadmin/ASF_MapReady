/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  j1_hdr( ref_count, ref_time, hk_file, pcm_file, start_id ) ---------------
	decode header information for J-ERS-1

  Inputs:
	REF_COUNT:  Reference time count of spacecraft clock
	REF_TIME:  Spacecraft time of reference time.  This will be
			updated with the time of the first HK format.
	HK_FILE:  Name of file into which decoded HK data will be written
	PCM_FILE:  Name for file into which decoded PCM data will be written
	START_ID:  Pointer to variable which will receive first format ID
		   from HK buffer.

Decodes the telemetry data stored in the Multibus buffers containing the
HK data and PCM data captured by the IIF and JDFM hardware, respectively.
The HK data will be accessed by setting the MB pointers to reference
page PID_IIF_HEAD with offset MLOC_IIF_HEAD.  The PCM data will be accessed
by setting the MB pointers to reference page PID_JDFM and offset of
MLOC_JDFM_PCM.  The HK data consists of the J-ERS-1 House Keeping 
information extracted from the J-ERS-1 header data from the signal tape.
Each HK data set data will contain 23 bytes of actual house keeping data 
which consists of 68 bits of HK data plus 24 bits of format (PRF) ID, each
bit duplicated as in the I/Q channels.  The PCM data consists of spcecraft 
time codes, attitude and state vectors, plus various other ancillary sensor 
information.  The time code of the start of the first major PCM frame located
will be used with the reference time given to calculate the GMT of the first
bit of the PCM data.  This will correspond to the GMT of the first HK format
stored into the buffer.

The PCM data is composed of 128 bytes per minor frame and 64 minor frames 
per major frame.  This data can start at random bit positions within the 
PCM buffer.  At the start of start of each minor frame is a 3 byte sync code.  
The 32 bit PCM time code is interleaved across minor frames 0, 1, and 2,
word 16, and word 17 containing the 8 LS bits of the code.  Since the time
changes once per second and each frame lasts 1/2 second, the time code in
frame 2 indicates the correct start time for that frame.  Then, using the
PCM clock rate of 2048 Hz, the time difference between frame 2 of the major
frame and the start of the PCM bits can be calculated.

The PCM data starting from the major frame is decoded and recorded into the
PCM_FILE.  The HK data starting from the next STEP 0 of a calibration
sequence is decoded and recorded into the HK_FILE.

The MB pointer is restored at each return point.
*/
#include <procfil.h>
#include <aspdecl.h>
#include <stdio.h>

extern int vbose;
int j1_hdr( ref_count, ref_time, hk_file, pcm_file, start_id )
char *hk_file, *pcm_file;
int ref_count, *start_id;
GMT_PTR ref_time;
{
unsigned char *hk_buf, *pcm_buf;
unsigned char *hk_pout, hk_out[24576], junk[47104];
int pcm_1, pcm_start, time_code, status, hk_count;
unsigned int *loc_buf, *mb_ptr;
double time;
double time_rate = 1.0 - 0.097/86400;  /* adjust for drift of clock */
GMT hk_time, pcm_time;
int byte, id, prf_code, t0, t1, t2, t3, pid_save;
float prfs[8];
float pcm_freq = 2048.0;

prfs[0] = 1505.8;
prfs[1] = 1530.1;
prfs[2] = 1555.2;
prfs[3] = 1581.1;
prfs[4] = 1606.0;
prfs[5] = 1646.75537;
prfs[6] = 1.0;
prfs[7] = 1.0;
pcm_1 = 0;
pcm_start = 0;
pid_save = mb.w[RLOC_REP];		/*  save MB repeater page address  */

sprintf( junk, "%04d %03d:%02d:%02d:%6.3f", 
ref_time->yr, ref_time->day, ref_time->hr, ref_time->min, ref_time->second );

/**/
/* Get HK data out of input HK buffer, eliminate double-bit redundancy */
/**/

if (vbose)
  printf("Setting MB for HK buffer, LOC=%d, PID=%d\n",RLOC_REP,PID_IIF_HEAD);

mb.w[RLOC_REP] = PID_IIF_HEAD;		/*  set MB repeater page address  */
asp_read( MEM_IIF_HEAD, &mb.u[MLOC_IIF_HEAD>>2], 47104 );
mb_ptr = &(mb.u[MLOC_IIF_HEAD>>2]);	/*  get address of HK buffer  */
loc_buf = (unsigned int *) junk;
for( id=0; id<11776; id++ ) *loc_buf++ = *mb_ptr++;	/*  copy HK data  */
hk_buf = junk;
hk_pout = hk_out;			/*  save pointer to output buffer  */

/*  Locate start of HK sets with proper PRF(1), CAL(1), and OBS(0) codes  */

for( id=0; 
	(hk_buf[0]&0xc0)!=0x80 && (hk_buf[1]&0xc0)!=0x80 && id<1024*512/23;
		id++ ) hk_buf += 23;

if (vbose) printf("J1_HDR skipped %d invalid HK data sets\n", id );

/*  Local buffer can hold maximum of 2048 sets.  */

for( hk_count=0; 
	(hk_buf[0]&0xc0)==0x80 && (hk_buf[1]&0xc0)==0x80 && hk_count<2048;
		hk_count++ ){
    get_2bits( hk_buf, 0, hk_pout, 68 );	/*  the HK data  */
    get_2bits( hk_buf+17, 0, hk_pout+9, 24 );	/*  the prf number  */
    hk_buf += 23;	/*  next HK set in HK buffer  */
    hk_pout += 12;	/*  next HK set in output buffer  */
}
if( hk_count <= 0 ){
    printf("J1_HDR could not locate any valid HK data set!\n");
    mb.w[RLOC_REP] = pid_save;
    return( -1 );
}

if (vbose) printf("J1_HDR saved %d valid HK data sets\n", hk_count );
for( id=0; id<12; id++ ) 
   if (vbose) printf(" %x,", hk_out[id] );
printf("\n");

/**/
/*  Look for PCM frame sync, locate time code and calculate S/C times.  */
/**/

if (vbose)
printf("Setting MB for PCM buffer, LOC=%d, PID=%d\n",RLOC_REP,PID_JDFM);

mb.w[RLOC_REP] = PID_JDFM;		/*  set MB repeater page address  */
asp_read( MEM_JDFM_PCM, &mb.u[MLOC_JDFM_PCM>>2], 32768 );
mb_ptr = &(mb.u[MLOC_JDFM_PCM>>2]);	/*  get address of PCM buffer  */
loc_buf = (unsigned *) junk;
for( id=0; id<8192; id++ ) *loc_buf++ = *mb_ptr++;	/*  copy PCM data  */
/****
for( id=0; id<32768; id+=2 ){
    junk[32768] = junk[id];
    junk[id] = junk[id+1];
    junk[id+1] = junk[32768];
}
****/

if (vbose) printf("First 32 bytes of PCM buffer:\n");
for( id=0; id<32; id++ ) if (vbose) printf("%3.2x", junk[id] );
printf("\n");

pcm_buf = junk;
pcm_1 = (int) (j1_sync_pcm( pcm_buf, 16384 ));	/*  find first PCM SYNC  */
if( pcm_1 < 0 ){
    printf("J1_HDR:  ****  J_SYNC_PCM failed to locate first SYNC!  ****\n");
    mb.w[RLOC_REP] = pid_save;	/*  restore MB repeater page address  */
    return( pcm_1 );
}

/*  Get frame ID to determine where start of major frame is  */

get_bits( pcm_buf, pcm_1+5*8, &id, 8 );
id = ntohl(id);
id = (id >> 24)&0x3f; 
if( id>63 ){
    printf("J1_HDR:  ****  got invalid PCM minor FrameID:%d  ****\n", id );
    mb.w[RLOC_REP] = pid_save;	/*  restore MB repeater page address  */
    return( -1 );
}
pcm_start = pcm_1;		/*  bits to start of first minor frame  */
if( (id&0x3)>0 ) pcm_start = pcm_start + (4-(id&0x3))*128*8;

if (vbose){
printf("J1_HDR found first PCM offset=%d, frame ID=%d\n", pcm_1, id );
printf("J1_HDR:  offset to nearest factor 4 minor frame=%d\n", pcm_start );
}

/*  Get time code of nearest factor of 4 minor frame.  26 bits of time are 
interleaved across minor frames n, n+1, and n+2, word 16 and 17  */

get_bits( pcm_buf, pcm_start+16*8, &t2, 8 );	/*  bits[26:24]  */
get_bits( pcm_buf, pcm_start+(128+16)*8, &t1, 8 );	/*  bits[23:16]  */
get_bits( pcm_buf, pcm_start+(128*2+16)*8, &t0, 16 );	/*  bits[15:0]  */
if (vbose) printf("Time code bytes: t2=%08x, t1=%08x, t0=%08x\n", t2, t1, t0 );
/* swapped the byte 2/28/96 CV */
t2=ntohl(t2);
t1=ntohl(t1);
t0=ntohl(t0);
time_code = (t2&0x07000000) | ((t1>>8)&0xff0000) | ((t0>>16)&0xffff);

if (vbose) {
printf("Time code bytes: t2=%08x, t1=%08x, t0=%08x\n", t2, t1, t0 );
printf("J1_HDR time code from nearest factor 4 minor frame=%d\n", time_code );
}

/*  Calculate GMT of first bit of PCM data.  Each TIME_CODE count 
represents one second, and each minor frame of 1024 bits sampled 
at 2048 bps represents one-half second.  REF_COUNT references REF_TIME.  
The REF_TIME value will be adjusted by the difference in seconds
between the TIME_CODE and the REF_TIME, subtracted by the time
from the nearest factor 4 minor frame to the first bit of PCM,
and the 2 minor frames offset from which the time code was obtained.  */

time = (double) ( time_code - ref_count );	/*  seconds since ref clock  */
time -= (int)(2048+pcm_start)/pcm_freq;/*  adjust to start of minor frame  */
time *= time_rate;
if (vbose) printf("time = %g\n", time);
add_seconds( ref_time, time );			/*  add to reference GMT  */
sprintf( junk, "%04d %03d:%02d:%02d:%6.3f", 
ref_time->yr, ref_time->day, ref_time->hr, ref_time->min, ref_time->second );

if (vbose) printf("time = %g, J1_HDR new (PCM_1) ref time=%s\n", time, junk );


/*  Calculate GMT of next major frame using ID of first minor frame  */

pcm_time = *ref_time;		/*  get GMT of first PCM bit  */
pcm_start = pcm_1;		/*  bit offset to first minor frame  */
if( id>0 ) pcm_start = pcm_start + (64-id)*128*8;

if (vbose) printf("J1_HDR:  PCM major frame offset=%d\n", pcm_start );


time = -(double) (pcm_start / pcm_freq);	/*  delta from 1st PCM bit  */
add_seconds( &pcm_time, time );			/*  add to GMT of PCM_START  */
sprintf( junk, "%04d %03d:%02d:%02d:%6.3f", 
	pcm_time.yr, pcm_time.day, pcm_time.hr, pcm_time.min, pcm_time.second );

printf("J1_HDR:  New time of PCM major frame=%s\n", junk );


/*  Get ID of first HK format and PRF code from HK buffer.  */

*start_id = (unsigned int) ( hk_out[9]<<16 | hk_out[10]<<8 | hk_out[11] );
prf_code = (unsigned int) ( hk_out[0]>>1 & 7 );

/*
printf("J1_HDR:  first format ID:%d, PRF code=%d\n", *start_id, prf_code );
*/

/*  Locate step 0 of attenuater of next calibration sequence in HK set  */

hk_pout = hk_out;
for( id=0; hk_count>id && ((hk_pout[4]>>4) & 7 )!=0; id++ ) hk_pout += 12;
if( id==hk_count && ((hk_pout[4]>>4 & 7) & 7 ) !=0 ){
    printf("**J1_HDR could not locate start of CAL sequence in HK data**\n");
    printf("J1_HDR checked %d HK formats out of %d available formats.\n",
	id, hk_count );
    printf("J1_HDR:  CAL operation may not be possible!\n");
    hk_pout -= 12;	/*   restore previous HK set  */
}
hk_count -= id;		/*  subtract number of HK sets skipped  */

/*  get its format ID  */

id = ((unsigned int)hk_pout[9])<<16 | ((unsigned int)hk_pout[10])<<8 | 
	((unsigned int)hk_pout[11] );
/*
printf("Format of first HK CAL step 0=%d\n", id );
*/

hk_count -= t3;			/*  adjust count of formats for Cal  */

/*  calculate time of step 0 of sequence in HK data using new REF_TIME */

hk_time = *ref_time;	/*  GMT of 1st HK frame = GMT of 1st PCM bit  */
time = (double) ( (float) t3 / prfs[prf_code] );
add_seconds( &hk_time, time );

/*  Write HK info  */

sprintf( junk, "%04d %03d:%02d:%02d:%6.3f", 
	hk_time.yr, hk_time.day, hk_time.hr, hk_time.min, hk_time.second );

/*
printf("J1_HDR HK_time=%s\n", junk );
printf("J1_HDR:  total HK sets being printed=%d\n", hk_count );
*/

status = j1_write_hk( hk_out, junk, prfs[ prf_code ], hk_count, hk_file );

/*  Write PCM info  */

status = j1_write_pcm( pcm_buf, junk, pcm_start, pcm_file );

mb.w[RLOC_REP] = pid_save;	/*  restore MB repeater page address  */
printf("Leaving J1_HDR, New ref time=%04d %03d:%02d:%02d:%6.3f\n", 
ref_time->yr, ref_time->day, ref_time->hr, ref_time->min, ref_time->second );
return( status );
}
