/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*
setup_rdfm(enable,rec,repsync,invlflag,auxsync,agcrep,auxheader,
	repheader,scsfil,maxerr,maxauxerr,maxfsdo)
	enable		Each nibble enable(1) or disables (0) one
			functional block of the board:
			0x100 = Enable Frame Sync
			0x010 = Enable Pseudo-Random Decode
			0x001 = Enable Auto Gain Control
	rec		Controls the inversion of every other bit
			= 0   record mode, invert every other bit
			!= 0  real-time mode, no inversion
	repsync		Allows the use of the replica length to resync
			the format counter.
			= 0   Do not resync
			!= 0  Resync after long format
	invlflag	Enable use of frame invalid flag
			= 0   Ignore invalid flag
			!= 0  Use invalid flag
	auxsync		Enable use of Aux sync pattern for format sync
			= 0   Ignore Aus sync pattern
			!= 0  Establish format sync on Aux sync pattern
	agcrep		Controls use of AGC table
			= 0   AGC echo data
			!= 0  AGC replica data
	auxheader	Allows transmission of Aux data to header memory
			= 0   Discard Aux data
			!= 0  Store Aux data in header memory on IIF
	repheader	Allows transmission of replica data to header
			= 0   Do not send rep data to header memory
			!= 0  Record replica data in header memory
	scsfil		Enables scansar filter
			= 0   Ingore scansar beam
			!= 0  Filter for specified scansar beam
	maxerr		The maximum number of bit errors allowed in a
			frame sync pattern. 0 to 15.
	maxauxerr	The maximum number of bit errors allowed in an
			aux sync pattern (relevant if auxsync!=0). 
			0 to 15.
	maxfsdo		The number of bad frames before dropout.
			0 to 255.

setup_rdfm_scs(beam,rlen0,elen0,rlen1,elen1,rlen2,elen2,rlen3,elen3)
	beam	Scansar beam to filter for, only relevent if scsfil!=0.
		0 to 3.
	rlen0	Replica length, in bytes, of beam 0, also used if 
		scsfil = 0. 0 to 65535.
	elen0	Echo length, in bytes, of beam 0, also used if 
		scsfil = 0. 0 to 65535.
	rleni	Replica length, in bytes, of beam i, only relevant if 
		scsfil != 0. 0 to 65535.
	eleni	Echo length, in bytes, of beam i, only relevant if 
		scsfil != 0. 0 to 65535.

setup_rdfm_agcmem(realagctab,imagagctab)
	realagctab	char array of length 1024
			4 low order bits of index represent I value
			6 high order bits of index represent AGC value
	imagagctab	char array of length 1024
			4 low order bits of index represent Q value
			6 high order bits of index represent AGC value

get_rdfm_status(&dropout,&fracount,&frasynerr,&badfracnt,&docnt,&forcnt,
	&crcerrcnt,&fratoterr,&bitslip)
	dropout		Cleared to 0 by RPI reset
			Set to 1 by maxfsdo bad frames
	fracount	Number of frames since reset: 32 bits
	frasynerr	Number of frames with any sync error: 16 bits
	badfracnt	Number of bad frames since reset:16 bits
	docnt		Number of bad frames since last good grame:8 bit
	forcnt		Number of formats since reset: 16 bits
	crcerrcnt	Number of frames with CRC errors: 16 bits
	fratoterr	Number of bit errors in frame sync patterns:16 bits
	bitslip		Number of times bitslip detected: 4 bits

*/
#include <stdio.h>
#include "aspdecl.h"

static short int   cregs[19];

setup_rdfm(enable,rec,repsync,invlflag,auxsync,agcrep,auxheader,
	repheader,scsfil,maxerr,maxauxerr,maxfsdo)
int enable,rec,repsync,invlflag,auxsync,agcrep,auxheader,
	repheader,scsfil,maxerr,maxauxerr,maxfsdo;
{
	cregs[0]=0;
	if ((enable & 0xf00)!=0) cregs[0]|=0x8000;
	if ((enable & 0xf0 )!=0) cregs[0]|=0x4000;
	if ((enable & 0xf  )!=0) cregs[0]|=0x2000;
	if (rec!=0) cregs[0]|=1;
	if (repsync==0) cregs[0]|=4;/*reset rsena*/
	if (invlflag==0) cregs[0]|=8;
	if (auxsync==0) cregs[0]|= 0x10;
	if (agcrep==1) cregs[0]|= 0x20;
	if (auxheader==0) cregs[0]|=0x40;
	if (repheader==0) cregs[0]|=0x80;
	if (scsfil==0) cregs[0]|=0x100;
	cregs[1]=(maxfsdo & 0xff) | (maxerr & 15)<<8 | (maxauxerr & 15)<<12;
	mb.w[RLOC_RDFM] = cregs[0];
	mb.w[RLOC_RDFM + 1] = cregs[1];
	asp_write( RLOC_RDFM<<1, &mb.w[RLOC_RDFM], 4 );
	return;
}

setup_rdfm_scs(beam,rlen0,elen0,rlen1,elen1,rlen2,elen2,rlen3,elen3)
int beam,rlen0,elen0,rlen1,elen1,rlen2,elen2,rlen3,elen3;
{
	asp_read( RLOC_RDFM<<1, &mb.w[RLOC_RDFM], 2 );
	cregs[0]=0xf9ff & mb.w[RLOC_RDFM];
	cregs[0]|=(beam&3)<<9;
	cregs[2]=rlen0&0xffff;
	cregs[3]=rlen1&0xffff;
	cregs[4]=rlen2&0xffff;
	cregs[5]=rlen3&0xffff;
	cregs[6]=elen0&0xffff;
	cregs[7]=elen1&0xffff;
	cregs[8]=elen2&0xffff;
	cregs[9]=elen3&0xffff;
	mb.w[RLOC_RDFM] = cregs[0];
	asp_write( RLOC_RDFM<<1, &mb.w[RLOC_RDFM], 2 );
	mb.w[RLOC_RDFM + 2] = cregs[2];
	mb.w[RLOC_RDFM + 3] = cregs[3];
	mb.w[RLOC_RDFM + 4] = cregs[4];
	mb.w[RLOC_RDFM + 5] = cregs[5];
	mb.w[RLOC_RDFM + 6] = cregs[6];
	mb.w[RLOC_RDFM + 7] = cregs[7];
	mb.w[RLOC_RDFM + 8] = cregs[8];
	mb.w[RLOC_RDFM + 9] = cregs[9];
	asp_write( (RLOC_RDFM+2)<<1, &mb.w[RLOC_RDFM+2], 16 );
	return;
}

setup_rdfm_agcmem(realagctab,imagagctab)
char *realagctab,*imagagctab;
{
	int i,pidsave,scratch;
	unsigned short int *mp;

	set_vme_param( 7, 2 );
	pidsave = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = PID_RDFM;
	mp = &mb.w[MLOC_RDFM_AGC >> 1];
	for(i=0;i<1024;i++) {
	    scratch = (int) realagctab[i];   /* don't sign extend */
	    *mp++ = (( (int) imagagctab[i] ) << 8) | (scratch & 0xff);
	}
	asp_write( MEM_RDFM_AGC, &mb.w[MLOC_RDFM_AGC>>1], 2048 );
	mb.w[RLOC_REP] = pidsave;
	set_vme_param( 7, 0 );
}

get_rdfm_status(dropout,fracount,frasynerr,badfracnt,docnt,forcnt,
	crcerrcnt,fratoterr,bitslip)
int *dropout,*fracount,*frasynerr,*badfracnt,*docnt,*forcnt,
	*crcerrcnt,*fratoterr,*bitslip;
{
	asp_read( RLOC_RDFM<<1, &mb.w[RLOC_RDFM], 38 );
	cregs[10]=mb.w[RLOC_RDFM + 10];
	cregs[11]=mb.w[RLOC_RDFM + 11];
	cregs[12]=mb.w[RLOC_RDFM + 12];
	cregs[13]=mb.w[RLOC_RDFM + 13];
	cregs[14]=mb.w[RLOC_RDFM + 14];
	cregs[15]=mb.w[RLOC_RDFM + 15];
	cregs[16]=mb.w[RLOC_RDFM + 16];
	cregs[17]=mb.w[RLOC_RDFM + 17];
	cregs[18]=mb.w[RLOC_RDFM + 18];
	*fracount=( cregs[10] & 0x7fff )<<16 | cregs[11];
	*frasynerr=cregs[12];
	*badfracnt=cregs[13];
	*docnt=cregs[14] & 0xff;
	if ( (cregs[14] & 0x100)==0 ) *dropout=1;
	else *dropout=0;
	*bitslip= (cregs[14]&0x1e00)>>9;
	*forcnt=cregs[15];
	*crcerrcnt=cregs[16];
	*fratoterr=( cregs[17] &0xff ) | cregs[18];
	return;
}
