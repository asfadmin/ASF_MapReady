/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* mbarel.c -- routines for relative addressing of the
		new ASP exec stimulus & response buffers.
	
	The stimulus routines (ms, fs, fcs, and frs) address the 
	stimulus buffer relative to the start of the buffer.  Buffer
	addresses are given as a number from 0 to 0x1fffffe; the 
	corresponding physical page and offset are generated 
	automatically by the routines.  (Bits 24-19 are the page 
	number, bits 18-0 are the offset.)

	The response routines (dr, dfr, dfir, mr, cr and csr) address
	the new exec response buffer relative to the start of captured
	data.  Two reserved locations in the mb array are used to
	record information about the captured data:

		mb + 0x1000 = DATA_START  (32 bits long)
		mb + 0x1004 = RESP_LENGTH (32 bits long)

	These data are used to determine the location of the start of
	captured data in the response buffer, by the method given in
	the routine start_of_data().

	When the user calls a response routine, he supplies a response
	buffer address in which 0 represents the start of captured
	data, and the buffer is treated as 32Mbytes of contiguous
	memory.  (A negative address accesses data preceding the
	actual start of captured data.)  The routine determines the
	physical page and offset of the actual start of data, and
	adjusts the user's address accordingly.

	This address is actually composed of two parts: the page address
	(bits 24 thru 19) and the buffer offset (bits 18 thru 0).  To
	access the response buffer, the page address is loaded into the
	PGADDR field of the exec control register, the PID register in
	the Multibus Repeater is set to 0, and the buffer offset is
	increased by 0x80000 (the location of the start of the response
	buffer in the mb array).  Accessing then proceeds sequentially
	until the offset reaches 0xfffff, at which time the page address
	is incremented and the offset is reset to 0x80000.  In this way
	the entire response buffer may be accessed in a circular buffer
	fashion.

New for R1B: 

The VME control interface can accept a full 32 bit address starting from
the base VME address, so splitting of bits to set page ID will no longer
be necessary; all directly accessed mb (VME) memory will be directly
accessed using the contiguous VME address space.

*/

#include <aspdecl.h>

#define    MEND_EXEC    0xfffff		/* response buffer end */
#define    RMODE_MEM_READ   0		/* normal memory read mode */
#define    RMODE_SYNC_READ   2		/* normal sync read mode */
#define    RMODE_MEM_WRITE  0		/* normal memory write mode */

extern int no_break;			/* 0 = ctrl-C was entered */
extern union {
	unsigned long i;
	unsigned int *u;
	unsigned short int *w;
	unsigned char *b;
	} mb_sync;

/* ms (mbstart, mbend, stimstart, stimend) -----------------------------
	This routine moves data from the mb array to the stimulus
	buffer, automatically handling buffer paging and offsets.
	Major modifications done for R1B to use VME I/O routines.
	All addresses are assumed to be bytes.
*/
ms (mbstart, mbend, stimstart, stimend)
int mbstart, mbend, stimstart, stimend;
{
	int pgaddr, pgend, i, iend, j, pid, stim1;
	unsigned register short int *m, *mend, *s, *send;
	int bytes, addr;

/*
s = (unsigned short *) &mb.w[mbstart>>1];
printf("MS MB: %08x %08x STIM: %08x %08x MB_ADDR:%x\n",
mbstart, mbend, stimstart, stimend, s );
*/
	mb.u[RESP_PAGE_PTR] = -1;
	mb.u[SYNC_PAGE_PTR] = -1;
	if( stimstart>=stimend || mbstart>=mbend ||
		mbstart<MEM_MB || mbend>=PAGE_SIZE ){
	    printf("MS: invalid parameters: MBSTART=%x, MBEND=%x, ",
		mbstart, mbend );
	    printf("STIMSTART=%x, STIMEND=%x\n", stimstart, stimend );
	    return;
	}
	if (stimstart >= stimend) return;
	pid = mb.w[RLOC_REP];
	asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	stim1 = mb.w[RLOC_STIM];
	mb.w[RLOC_REP] = 0;
	ex_get_stim_addr (stimstart, &pgaddr, &i);
	ex_get_stim_addr (stimend, &pgend, &j);
	set_vme_param( 7, 2 );
/*
printf("MS pgaddr=%x i=%x pgend=%x j=%x\n", pgaddr, i, pgend, j );
*/
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	m = &mb.w[mbstart >> 1];
	mend = &mb.w[mbend >> 1];
	ex_set_stim_mode (RMODE_MEM_WRITE);
	s = &mb.w[i >> 1];
	addr = (PID_EXEC<<20) | i;
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    send = &mb.w[iend >> 1];
	    ex_set_stim_page (pgaddr++);
	    bytes = 0;
	    while (s < send) {
		*s++ = *m++;
		bytes += 2;
		if (m == mend) m = &mb.w[mbstart >> 1];
	    }
	    asp_write( addr, &mb.w[addr>>1], bytes );
	    addr = MEM_EXEC;
	    s = &mb.w[MLOC_EXEC >> 1];
	}
	mb.w[RLOC_STIM] = stim1;
	asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	mb.w[RLOC_REP] = pid;
}

/* fs (stimstart, stimend, value) --------------------------------------
	This routine fills the given range of locations in the stimulus
	buffer with the given value.
	Use the MB array to generate the value and use VME I/O to send
	to STIM buffer.
*/
fs (stimstart, stimend, value)
int stimstart, stimend, value;
{
	int pgaddr, pgend, i, iend, j, pid, stim1;
	unsigned register short int *s, *send;
	int bytes, addr;

	if (stimstart >= stimend) return;

	mb.u[RESP_PAGE_PTR] = -1;
	mb.u[SYNC_PAGE_PTR] = -1;
	pid = mb.w[RLOC_REP];
	asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	stim1 = mb.w[RLOC_STIM];
	mb.w[RLOC_REP] = 0;
	ex_get_stim_addr (stimstart, &pgaddr, &i);
	ex_get_stim_addr (stimend, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	ex_set_stim_mode (RMODE_MEM_WRITE);
	s = &mb.w[i >> 1];
	addr = i;
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    send = &mb.w[iend >> 1];
	    ex_set_stim_page (pgaddr++);
	    bytes = 0;
	    while (s < send){
		*s++ = value;
		bytes += 2;
	    }
	    asp_write( addr, &mb.w[addr>>1], bytes );
	    s = &mb.w[MLOC_EXEC >> 1];
	    addr = MLOC_EXEC;
	}
	mb.w[RLOC_STIM] = stim1;
	asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	mb.w[RLOC_REP] = pid;
}

/* fcs (stimstart, stimend, value, vinc, vend, addr_inc) ---------------
	This routine fills the given range of locations in the stimulus
	buffer with a counter value.
*/
fcs (stimstart, stimend, value, vinc, vend, addr_inc)
int stimstart, stimend, value, vinc, vend, addr_inc;
{
	int pgaddr, pgend, i, iend, j, v, pid, stim1;
	unsigned register short int *s, *send;
	int bytes, addr;

	if (stimstart >= stimend) return;
	mb.u[RESP_PAGE_PTR] = -1;
	mb.u[SYNC_PAGE_PTR] = -1;
	pid = mb.w[RLOC_REP];
	asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	stim1 = mb.w[RLOC_STIM];
	mb.w[RLOC_REP] = 0;
	ex_get_stim_addr (stimstart, &pgaddr, &i);
	ex_get_stim_addr (stimend, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	ex_set_stim_mode (RMODE_MEM_WRITE);
	s = &mb.w[i >> 1];
	addr = i;
	v = value;
	if (addr_inc <= 0) addr_inc = 1;
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    send = &mb.w[iend >> 1];
	    ex_set_stim_page (pgaddr++);
	    bytes = 0;
	    while (s < send) {
		*s = v;
		if( addr_inc>1 ){	/* this will be slow! */
		    asp_write( addr, s, 2 );
		    addr += addr_inc<<1;
		} else bytes += 2;
		s += addr_inc;
		v += vinc;
		if (vinc >= 0) {
		    if (v > vend) v = value;
		}
		else { if (v < vend) v = value;
		}
	    }
	    if( addr_inc==2 ) asp_write( addr, &mb.w[addr>>1], bytes );
	    addr = MLOC_EXEC;
	    s -= 0x80000 >> 1;
	}
	mb.w[RLOC_STIM] = stim1;
	asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	mb.w[RLOC_REP] = pid;
}

/* frs (stimstart, stimend, addr_inc, stimno) --------------------------
	This routine fills the stimulus buffer with random data.
*/
frs (stimstart, stimend, addr_inc, stimno)
int stimstart, stimend, addr_inc, stimno;
{
	static int first = 1;
	static char state[256];
	int pgaddr, pgend, i, iend, j, pid, stim1;
	unsigned register short int *s, *send;
	int bytes, addr;

	if (stimstart >= stimend) return;

	mb.u[RESP_PAGE_PTR] = -1;
	mb.u[SYNC_PAGE_PTR] = -1;
	pid = mb.w[RLOC_REP];
	asp_read( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	stim1 = mb.w[RLOC_STIM];
	if (addr_inc <= 0) addr_inc = 2;
    /* reset random number generator to start, if needed */
	if (first || (stimno > 0)) {
	    srandom(1);
	    initstate(1,state,256);
	    first = 0;
	}

    /* scan forward to correct starting place in random sequence */
	if (stimno > 1) {
	    while (--stimno) {
		i = ((stimend - stimstart) >> 1) / addr_inc;
		while (i--) random();
	    }
	}

    /* load random sequence into stimulus buffer */
	mb.w[RLOC_REP] = 0;
	ex_get_stim_addr (stimstart, &pgaddr, &i);
	ex_get_stim_addr (stimend, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	ex_set_stim_mode (RMODE_MEM_WRITE);
	s = &mb.w[i >> 1];
	addr = i;
	if (addr_inc <= 0) addr_inc = 1;
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    send = &mb.w[iend >> 1];
	    ex_set_stim_page (pgaddr++);
	    bytes = 0;
	    while (s < send) {
		*s = random();
		if( addr_inc>1 ){
		    asp_write( addr, s, 2 );
		    addr += addr_inc<<1;
		} else bytes += 2;
		s += addr_inc;
	    }
	    if( addr_inc==2 ) asp_write( addr, &mb.w[addr>>1], bytes );
	    addr = MLOC_EXEC;
	    s -= 0x80000 >> 1;
	}
	mb.w[RLOC_STIM] = stim1;
	asp_write( RLOC_STIM<<1, &mb.w[RLOC_STIM], 2 );
	mb.w[RLOC_REP] = pid;
}

/* dr (respaddr,nlines) ------------------------------------------------
	This routine displays on the screen consecutive words from the
	response buffer.
*/
dr (respaddr, nlines)
int respaddr, nlines;
{
	int pgaddr, i, j, k, cflag, resp1;
	int syncaddr[1000];
	char syncbytes[1000];
	int synclen;
	char c, fs[3], vl[3], vf[3], al[3], ss[3];

	asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	resp1 = mb.w[RLOC_RESP];
	respaddr &= 0x01fffff0;
	synclen = 1000;
	ex_get_sync_bytes (respaddr, respaddr+16*nlines, syncaddr,
				   syncbytes, &synclen);
	ex_get_resp_addr (respaddr, &pgaddr, &i);
	ex_set_resp_page (pgaddr);
	ex_set_resp_mode (RMODE_MEM_READ);

/* set page offset and read remaining words */
	asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
	mb.u[RESP_PAGE_PTR] = pgaddr;
	i >>= 1;
	k = 0;
	set_break();
	while (nlines-- && no_break) {
	    printf("%.7X:  ",respaddr & 0x1ffffff);
	    cflag = 0;
	    for (j = 0; j < 4; j++) {
		if (i > MEND_EXEC >> 1) {	/* set and read new page */
		    ex_set_resp_page (++pgaddr);
		    asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
		    mb.u[RESP_PAGE_PTR] = pgaddr;
		}
		c = ' ';
		if ((k < synclen) && (respaddr == syncaddr[k])) {
		    c = '*';
		    cflag = 1;
		}
		printf("%.4X%c%.4X  ", mb.w[i] & 0xffff,
				   c,  mb.w[i+1] & 0xffff);
		i += 2;
		respaddr += 4;
	    }
	    if (cflag) {
		c = syncbytes[k++];
		strcpy (fs, ((c & 0x01) == 0) ? "FS" : "..");
		strcpy (vl, ((c & 0x02) == 0) ? "VL" : "..");
		strcpy (vf, ((c & 0x04) == 0) ? "VF" : "..");
		strcpy (al, ((c & 0x08) == 0) ? "AL" : "..");
		strcpy (ss, ((c & 0x10) == 0) ? "SS" : "..");
		printf ("%.2X  %s %s %s %s %s",c & 0xff,
			       fs,vl,vf,al,ss);
	    }
	    printf("\n");
	}
	mb.w[RLOC_RESP] = resp1;
	asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
}

/* dfr (respaddr,nlines) -----------------------------------------------
	This routine displays on the screen consecutive words from the
	response buffer, in fixed-point decimal format.
*/
dfr (respaddr, nlines)
int respaddr, nlines;
{
	int pgaddr, i, j, pid, resp1;
	float rdata,idata;

	pid = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = 0;
	asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	resp1 = mb.w[RLOC_RESP];
	respaddr &= 0x01fffff8;
	ex_get_resp_addr (respaddr, &pgaddr, &i);
	ex_set_resp_page (pgaddr);
	ex_set_resp_mode (RMODE_MEM_READ);

/* set page offset and read remaining words */
	asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
	mb.u[RESP_PAGE_PTR] = pgaddr;
	i >>= 1;
	set_break ();
	while (nlines-- && no_break) {
	    printf("%.7X:  ",respaddr & 0x1ffffff);
	    for (j = 0; j < 2; j++) {
		if (i > MEND_EXEC >> 1) {	/* set and read new page */
		    ex_set_resp_page (++pgaddr);
		    asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
		    mb.u[RESP_PAGE_PTR] = pgaddr;
		}
		rdata = mb.w[i] / 32768.0;
		idata = mb.w[i+1] / 32768.0;
		printf("% f % f    ", rdata, idata);
		i += 2;
		respaddr += 4;
	    }
	    printf("\n");
	}
	mb.w[RLOC_RESP] = resp1;
	asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	mb.w[RLOC_REP] = pid;
}

/* dfir (respaddr,nlines) ----------------------------------------------
	This routine displays on the screen consecutive intensities
	(r*r + i*i) from the response buffer, in fixed-point decimal
	format.
*/
dfir (respaddr, nlines)
int respaddr, nlines;
{
	int pgaddr, i, j, pid, resp1;
	float rdata,idata;

	pid = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = 0;
	asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	resp1 = mb.w[RLOC_RESP];
	respaddr &= 0x01fffff8;
	ex_get_resp_addr (respaddr, &pgaddr, &i);
	ex_set_resp_page (pgaddr);
	ex_set_resp_mode (RMODE_MEM_READ);

/* set page offset and read remaining words */
	asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
	mb.u[RESP_PAGE_PTR] = pgaddr;
	i >>= 1;
	set_break ();
	while (nlines-- && no_break) {
	    printf("%.7X:  ",respaddr & 0x1ffffff);
	    for (j = 0; j < 4; j++) {
		if (i > MEND_EXEC >> 1) {	/* set and read next page */
		    ex_set_resp_page (++pgaddr);
		    asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
		    mb.u[RESP_PAGE_PTR] = pgaddr;
		}
		rdata = mb.w[i] / 32768.0;
		idata = mb.w[i+1] / 32768.0;
		printf("% f ", rdata * rdata  +  idata * idata);
		i += 2;
		respaddr += 4;
	    }
	    printf("\n");
	}
	mb.w[RLOC_RESP] = resp1;
	asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	mb.w[RLOC_REP] = pid;
}

/* mr (respstart, respend, mbstart, mbend) -----------------------------
	This routine moves data from the response buffer to some other
	location in the mb array.
*/
mr (respstart, respend, mbstart, mbend)
int respstart, respend, mbstart, mbend;
{
	int i, j;

	if (respstart>=respend || mbstart<MEM_MB || mbend>PAGE_SIZE ){
	    printf("MR: invalid arguments: RESPSTART=%x, RESPEND=%x, ",
						respstart, respend );
	    printf("MBSTART=%x, MBEND=%x\n", mbstart, mbend );
	    return;
	}
	i = respend - respstart;
	j = mbend - mbstart;
	j = ( i>j ) ? j : i;
	getrdata( &mb.w[mbstart>>1], respstart, j>>2 );
}

/* cr (respstart, respend, mbstart, mbend) -----------------------------
	This routine compares data from the response buffer to data at
	some other location in the mb array.  The routine returns PASS
	if the data match, or FAIL if not.
*/
cr (respstart, respend, mbstart, mbend)
int respstart, respend, mbstart, mbend;
{
	int pgaddr, pgend, i, iend, j, k, pid, resp1;
	register unsigned short int *m, *mend, *r, *rend, *rpgend;
	unsigned short int *r1, *r2, *m1, *m2;
	int p1, p2, d1m, d1r, d2m, d2r;
	int error = 0;
	int s0, s1, us0, us1;
	float time;
/*
printf("CR: RESP: %08x %08x MB: %08x %08x current page=%x\n",
respstart, respend, mbstart, mbend, mb.u[RESP_PAGE_PTR] );
*/
	if (respstart >= respend) return(PASS);
	if ( mbstart<MEM_MB || mbend>PAGE_SIZE ){
	    printf("CR: invalid arguments: MBSTART=%x, MBEND=%x\n",
		mbstart, mbend );
	    return(FAIL);
	}
	set_vme_param( 7, 0 );
	pid = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = 0;
	asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	resp1 = mb.w[RLOC_RESP];
	ex_get_resp_addr (respstart, &pgaddr, &i);
	ex_get_resp_addr (respend, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	if (mbend == 0) mbend = mbstart + (respend - respstart);
	if (mbend > MLOC_EXEC) mbend = MLOC_EXEC;
	m = &mb.w[mbstart >> 1];
	mend = &mb.w[mbend >> 1];
	ex_set_resp_mode (RMODE_MEM_READ);
	r = &mb.w[i >> 1];
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    rpgend = rend = &mb.w[iend >> 1];
	    if ((rend - r) > (mend - m)) rend = r + (mend - m);
	    if( pgaddr!=mb.u[RESP_PAGE_PTR] ){
/*
printf("CR reading page:%x, address:%x(%d)\n",
pgaddr, i, i );
*/
		ex_set_resp_page (pgaddr);
		asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
		mb.u[RESP_PAGE_PTR] = pgaddr;
	    }
	    while (r < rend) {
		if (*m++ != *r++) {
		    if (++error == 1) {	/* page and offset of first error */
			r1 = r-1;
			m1 = m-1;
			p1 = pgaddr;
			d1m = *(m-1);
			d1r = *(r-1);
		    }
		    r2 = r-1;	/* page and offset of last error */
		    m2 = m-1;
		    p2 = pgaddr;
		    d2m = *(m-1);
		    d2r = *(r-1);
		}  /* if *m++ */
	    }  /* while r */
	    if (m >= mend) m = &mb.w[mbstart >> 1];
	    if (r >= rpgend) {
		r = &mb.w[MLOC_EXEC >> 1];
		i = MLOC_EXEC;
		pgaddr++;
	    }
	}  /* while pgaddr */
	if (error == 1 && d1m == d1r) {
	    r = &mb.w[MLOC_EXEC >> 1];
	    i = ex_get_rel_addr(p1, (r1 - r) << 1);
	    printf("transient read error at location %x\n", i & 0x1ffffff);
	    error = 0;
	}
	if (error) {
	    r = &mb.w[MLOC_EXEC >> 1];
	    i = ex_get_rel_addr(p1, (r1 - r) << 1);
	    j = ex_get_rel_addr(p2, (r2 - r) << 1);
	    printf("CR:  %d words differ ", error);
	    printf("first=%x,rsp=%4.4x,mem=%4.4x ",
		    i & 0x1ffffff, d1r & 0xffff, d1m & 0xffff);
	    printf("last=%x,rsp=%4.4x,mem=%4.4x\n",
		    j & 0x1ffffff, d2r & 0xffff, d2m & 0xffff);
	    printf("First page=%x, M1=%x, last page=%x, M2=%x\n",
		p1, m1-m, p2, m2-m );

if( p1!=mb.u[RESP_PAGE_PTR] ){
    printf("CR Reading first page %x for display...\n", p1 );
	ex_set_resp_page (p1);
	asp_read( 0x80000, &mb.w[0x40000], PAGE_SIZE );
    mb.u[RESP_PAGE_PTR] = p1;
}

printf("First mismatch area [mb]=%x:\n", m1 );
for( k=0; k<32; k+=8 ){
    printf("MEMB[%08x]: ", ((m1-m)<<1)+mbstart );
    for( j=k; j<k+8; j++ ) printf(" %04x", *m1++ );
    printf("\n");
    printf("RESP[%08x]: ", (r1-r)<<1 );
    for( j=k; j<k+8; j++ ) printf(" %04x", *r1++ );
    printf("\n");
}

if( p2!=mb.u[RESP_PAGE_PTR] ){
    printf("CR Reading first page %x for display...\n", p2 );
	ex_set_resp_page (p2);
	asp_read( 0x80000, &mb.w[0x40000], PAGE_SIZE );
}
printf("Last mismatch area:\n");
j = m2 - m;
if( j>24 ) j = 24;
m2 -= j;
r2 -= j;
for( k=0; k<32; k+=8 ){
    printf("MEMB[%08x]: ", ((m2-m)<<1)+mbstart );
    for( j=k; j<k+8; j++ ) printf(" %04x", *m2++ );
    printf("\n");
    printf("RESP[%08x]: ", (r2-r)<<1 );
    for( j=k; j<k+8; j++ ) printf(" %04x", *r2++ );
    printf("\n");
}
printf("CR: RESP: %08x %08x MB: %08x %08x\n",
respstart, respend, mbstart, mbend );

	}	/*  end error reporting  */

	mb.w[RLOC_RESP] = resp1;
	asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	set_vme_param( 7, 2 );
	return (error ? FAIL : PASS);
}

/* crr (respstart, respend, mbstart, mbend) ----------------------------
	This routine compares real only data from the response buffer
	to real data at some other location in the mb array.  The
	routine returns PASS if the data match, or FAIL if not.
*/
crr (respstart, respend, mbstart, mbend)
int respstart, respend, mbstart, mbend;
{
	int pgaddr, pgend, i, iend, j, k, pid, resp1;
	register unsigned short int *m, *mend, *r, *rend, *rpgend;
	unsigned short int *r1, *r2;
	int p1, p2, d1m, d1r, d2m, d2r;
	int error = 0;

	if (respstart >= respend) return(PASS);
	pid = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = 0;
	asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	resp1 = mb.w[RLOC_RESP];
	ex_get_resp_addr (respstart, &pgaddr, &i);
	ex_get_resp_addr (respend, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	if (mbend == 0) mbend = mbstart + (respend - respstart);
	if (mbend > MLOC_EXEC) mbend = MLOC_EXEC;
	m = &mb.w[mbstart >> 1];
	mend = &mb.w[mbend >> 1];
	ex_set_resp_mode (RMODE_MEM_READ);
	r = &mb.w[i >> 1];
	set_vme_param( 7, 0 );
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    rpgend = rend = &mb.w[iend >> 1];
	    if ((rend - r) > (mend - m)) rend = r + (mend - m);
	    ex_set_resp_page (pgaddr);
	    if( pgaddr!=mb.u[RESP_PAGE_PTR] ){
		asp_read( MLOC_EXEC, &mb.w[MLOC_EXEC>>1], PAGE_SIZE );
		mb.u[RESP_PAGE_PTR] = pgaddr;
	    }
	    while (r < rend) {
		if (*m != *r) {
		    if (++error == 1) {
			r1 = r;
			p1 = pgaddr;
			d1m = *m;
			d1r = *r;
		    }
		    r2 = r;
		    p2 = pgaddr;
		    d2m = *m;
		    d2r = *r;
		}  /* if *m++ */
		m += 2;
		r += 2;
	    }  /* while r */
	    if (m >= mend) m = &mb.w[mbstart >> 1];
	    if (r >= rpgend) {
		r = &mb.w[MLOC_EXEC >> 1];
		i = MLOC_EXEC;
		pgaddr++;
	    }
	}  /* while pgaddr */
	if (error == 1 && d1m == d1r) {
	    r = &mb.w[MLOC_EXEC >> 1];
	    i = ex_get_rel_addr(p1, (r1 - r) << 1);
	    printf("transient read error at location %x\n", i & 0x1ffffff);
	    error = 0;
	}
	if (error) {
	    r = &mb.w[MLOC_EXEC >> 1];
	    i = ex_get_rel_addr(p1, (r1 - r) << 1);
	    j = ex_get_rel_addr(p2, (r2 - r) << 1);
	    printf("%d words differ ", error);
	    printf("first=%x,rsp=%4.4x,mem=%4.4x ",
		    i & 0x1ffffff, d1r & 0xffff, d1m & 0xffff);
	    printf("last=%x,rsp=%4.4x,mem=%4.4x\n",
		    j & 0x1ffffff, d2r & 0xffff, d2m & 0xffff);
	}
	mb.w[RLOC_RESP] = resp1;
	asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	mb.w[RLOC_REP] = pid;
	set_vme_param( 7, 2 );
	return (error ? FAIL : PASS);
}

/* csr (respstart, respend, syncloc, nsyncs, spacing) ------------------
	This routine retrieves the sync codes corresponding to the
	address range given, and compares them to the array of sync
	codes given by the caller.  Any errors are reported.  If any
	errors are found, the routine returns FAIL, else the routine
	returns PASS.  It is assumed that the first sync code should
	be found at the location given by respstart.
*/
csr (respstart, respend, syncloc, nsyncs, spacing)
	int respstart, respend, syncloc, nsyncs, spacing;
{
	char syncs[0x10000];
	int adds[0x10000];
	int i, j, k, len, resp1, expsyncs, cerr;
	int aerr, ae1b,ae1c, ae2b,ae2c;
	int serr, se1a,se1b,se1c, se2a,se2b,se2c;
	char c;
/*
printf("CSR: RESP: %08x %08x SYNCLOC=%x, NSYNCS=%d, spacing=%d\n",
respstart, respend, syncloc, nsyncs, spacing );
*/
	if (respstart >= respend) return(PASS);
	asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
	resp1 = mb.w[RLOC_RESP];
	len = 0x10000;
	ex_get_sync_bytes (respstart, respend, adds, syncs, &len);
	expsyncs = respend - respstart;
	while (expsyncs < 0) expsyncs += 0x2000000;
	expsyncs = (expsyncs + spacing - 1) / spacing;
	if ((cerr = (len != expsyncs)))
	    printf ("expected %d syncs, found %d syncs\n",expsyncs,len);
	syncloc >>= 1;
	k = syncloc;
	j = respstart;
	aerr = serr = 0;
	for (i = 0; i < len; i++) {	/* check sync addresses */
	    if (j != adds[i]) {
		if (aerr++ == 0) {
		    ae1b = j;
		    ae1c = adds[i];
		}
		ae2b = j;
		ae2c = adds[i];
	    } else {			/* check sync codes */
		c = mb.w[k];
		if (c != syncs[i]) {
		    if (serr++ == 0) {
			se1a = i;
			se1b = c & 0xff;
			se1c = syncs[i] & 0xff;
		    }
		    se2a = i;
		    se2b = c & 0xff;
		    se2c = syncs[i] & 0xff;
		}
	    }  /* else */
	    while (j <= adds[i]) {
		j += spacing;
		aerr += (j < adds[i]);
		if (++k >= syncloc + nsyncs) k = syncloc;
	    }
	}  /* for i */
	if (aerr) {
	    printf("%d sync address errors: first exp=%x, actual=%x\n",
				aerr,ae1b,ae1c);
	    printf("			last exp=%x, actual=%x\n",
				ae2b,ae2c);
	}
	if (serr) {
	    printf("%d sync code errors: first at %x, exp=%.2x, actual=%.2x\n",
				serr,adds[se1a],se1b,se1c);
	    printf("		     last at %x, exp=%.2x, actual=%.2x\n",
				adds[se2a],se2b,se2c);
	}
	mb.w[RLOC_RESP] = resp1;
	asp_write( RLOC_RESP<<1, &mb.w[RLOC_RESP], 2 );
/*
printf("CSR ending\n");
*/
	return (((aerr + serr + cerr) > 0) ? FAIL : PASS);
}

/* ex_get_rel_addr (pgaddr, offset) ----------------------------------
	This routine calculates the address relative to the start of
	data, represented by the input hardware offset and page values.
*/
int ex_get_rel_addr (pgaddr, offset)
int pgaddr, offset;
{
	int i;

	i = offset + ((pgaddr & 0x3f) << 19) - start_of_data();
	if (i < 0) i += 0x2000000;
	return (i);
}

/* start_of_data() -----------------------------------------------------
	This routine returns the offset to the start of data, as
	indicated by the values in DATA_START and RESP_LENGTH, possibly
	combined with the register values FREEZEADDR and DELAY.  The
	algorithm is:
		if DATA_START is a valid number, use it
		else if RESP_LENGTH is a valid number,
			use FREEZEADDR - RESP_LENGTH
		    else use FREEZEADDR - DELAY
	a number is valid unless it is the maximum negative value,
	0x80000000.
*/
start_of_data()
{
	int i,j,k,s;

	if ((i = mb.u[DATA_START]) == 0x80000000) {
	    asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 10 );
	    s = mb.w[RLOC_CNTRL] & 0x0080;
	    k = 4 + (16 * (s == 0));
	    asp_read( RLOC_RESP<<1, &mb.w[RLOC_RESP], 10 );
	    i = (((mb.w[RLOC_RESP + 2] << 16) 
			+ (mb.w[RLOC_RESP + 1] & 0xffff)) << 2) - k;
	    if ((j = mb.u[RESP_LENGTH]) == 0x80000000)
		j = (((mb.w[RLOC_CNTRL + 4] & 0xff) << 16)
			+ (mb.w[RLOC_CNTRL + 3] & 0xffff)) << 2;
	    if ((i -= j) < 0) i += 0x2000000;
	}
/*
printf("START_OF_DATA: S=%x K=%x J=%x I=%x\n", s, k, j, i );
*/
	return (i);
}

/* ex_get_resp_addr (reladdr, pgaddr, offset) -------------------------
	This routine returns the absolute page address and offset in the
	response buffer, represented by the relative address reladdr.
*/
ex_get_resp_addr (reladdr, pgaddr, offset)
int reladdr, *pgaddr, *offset;
{
	int i;

/*
printf("GET_RESP_ADDR:  REL=%x ", reladdr );
*/
	reladdr += start_of_data();
	while (reladdr < 0) reladdr += 0x2000000;
	*pgaddr = (reladdr >> 19) & 0x3f;
	*offset = (reladdr & 0x7ffff) + MLOC_EXEC;
/*
printf("REL=%x PG=%x OFFSET=%x\n", reladdr, *pgaddr, *offset );
*/
}

/* ex_get_stim_addr (reladdr, pgaddr, offset) -------------------------
	This routine returns the absolute page address and offset in the
	stimulus buffer, represented by the relative address reladdr.
*/
ex_get_stim_addr (reladdr, pgaddr, offset)
int reladdr, *pgaddr, *offset;
{
	int i;

	while (reladdr < 0) reladdr += 0x2000000;
	*pgaddr = (reladdr >> 19) & 0x3f;
	*offset = (reladdr & 0x7ffff) + MLOC_EXEC;
}

/* ex_get_sync_bytes (start, end, syncaddr, syncbytes, len) ------------
	This routine retrieves the sync codes associated with the
	response data between start and end, and places them in the
	user's arrays syncaddr (location of the byte) and syncbytes
	(the 8 bits of sync codes accumulated for this byte).  The
	size available in syncaddr & syncbytes is given by len.
*/
ex_get_sync_bytes (start, end, syncaddr, syncbytes, len)
int start, end, syncaddr[], *len;
char syncbytes[];
{
	int pgaddr, pgend, i, iend, j, k, m, loc, pid, sync1;
	register unsigned short int *r, *rend, *s, *send;
	int save;
	char c;
	int s0, s1, us0, us1;
	float time;
/*
printf("EX_GET_SYNC_BYTES inputs: START=%x END=%x last page=%x\n",
start, end, 0xff&mb.u[SYNC_PAGE_PTR] );
*/
	if (start >= end) {
	    *len = 0;
	    return;
	}
	set_vme_param( 7, 0 );
	pid = mb.w[RLOC_REP];
	mb.w[RLOC_REP] = 0;
	asp_read( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
	sync1 = mb.w[RLOC_CNTRL];
	ex_set_resp_mode (RMODE_SYNC_READ);
	ex_get_resp_addr (start - 20, &pgaddr, &i);
	ex_get_resp_addr (end - 20, &pgend, &j);
	if (pgend < pgaddr || (pgend == pgaddr && j <= i)) pgend += 64;
	r = &mb_sync.w[i >> 1];
	send = &mb_sync.w[(MEND_EXEC+1) >> 1];
	loc = start;
	k = 0;
	while (pgaddr <= pgend) {
	    iend = (pgaddr == pgend) ? j : MEND_EXEC + 1;
	    rend = &mb_sync.w[iend >> 1];
	    pgaddr++;
	    if( (pgaddr-1)!=mb.u[SYNC_PAGE_PTR] ){
		ex_set_sync_page (pgaddr-1);
/*
printf("EX_GET_SYNC_BYTES reading first page %x I=%x IEND=%x K=%d\n",
pgaddr-1, i, iend, k );
*/
		asp_read( MLOC_EXEC, &mb_sync.w[ MLOC_EXEC>>1 ], PAGE_SIZE );
		mb.u[SYNC_PAGE_PTR] = pgaddr-1;
	    }
	    while (r < rend) {
		if (((save = *r) & 2) == 0) {	/* if line sync present */
		    syncaddr[k] = loc;
		    c = 0;
		    for (s = r, m = 0; m < 8; m++) { /* get sync codes */
			c |= ((save = *s) & 1) << m;
			if ((s += 2) >= send) {
			    s = &mb_sync.w[MLOC_EXEC >> 1];
			    if( pgaddr!=mb.u[SYNC_PAGE_PTR] ){
				ex_set_sync_page (pgaddr);
/*
printf("EX_GET_SYNC_BYTES reading new page %x I=%x IEND=%x\n",
pgaddr, i, iend );
*/
				asp_read( MLOC_EXEC, &mb_sync.w[MLOC_EXEC>>1],
					PAGE_SIZE );
				mb.u[SYNC_PAGE_PTR] = pgaddr;
			    }
			}
		    }
		    syncbytes[k] = c;
		    if (++k >= *len) {
			mb.w[RLOC_CNTRL] = sync1;
			asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
			mb.w[0] = pid;
			return;
		    }
		    if( (pgaddr-1)!=mb.u[SYNC_PAGE_PTR] ){
			ex_set_sync_page (pgaddr - 1); 
/*
printf("EX_GET_SYNC_BYTES reading another page %x I=%x IEND=%x code=%x\n",
pgaddr-1, i, iend, c );
*/
			asp_read( MLOC_EXEC, &mb_sync.w[MLOC_EXEC>>1],
				PAGE_SIZE );
			mb.u[SYNC_PAGE_PTR] = pgaddr - 1;
		    }
		}  /* if *r */
		r += 2;
		loc += 4;
	    }  /* while r */
	    r = &mb_sync.w[MLOC_EXEC >> 1];
	}  /* while pgaddr */
	*len = k;
	mb.w[RLOC_CNTRL] = sync1;
	asp_write( RLOC_CNTRL<<1, &mb.w[RLOC_CNTRL], 2 );
	set_vme_param( 7, 2 );
	mb.w[RLOC_REP] = pid;
/*
gettime_(&s1, &us1 );
time = (float)( s1-s0 ) + (float)( us1-us0 ) / 1000000.0;
printf("EX_GET_SYNC_BYTES returning, time=%f\n", time );
*/
}

/* ex_get_spaced_syncs(start, end, syncbytes, len, spacing) ------------
	This routine retrieves sync bytes from the sync response
	buffer and places them in the user's array.  The syncs are
	expected to be evenly spaced, with spacing given by "spacing".
	On input, "len" contains the size of the syncbytes array.  On
	output, the number of syncs found is returned in len.
	NOTE: This routine will not detect syncs located at other than
	the given spacing interval.
*/
ex_get_spaced_syncs (start, end, syncbytes, len, spacing)
int start, end, spacing;
int *len;
char syncbytes[];
{
	int i,j,addr,slen;
	int error = 0;

	if (start >= end) {
	    *len = 0;
	    return;
	}
	for (i = start, j = 0; (i < end) && (j < *len); i += spacing) {
	    slen = 1;
	    ex_get_sync_bytes(i,i+4,&addr,&syncbytes[j++],&slen);
	    if (slen != 1) {
		j--;
		error = 1;
	    }
	}
	if (error && (j < 3)) {
	    printf("syncs not spaced as specified in ex_get_spaced_syncs\n");
	}
	*len = j;
}
