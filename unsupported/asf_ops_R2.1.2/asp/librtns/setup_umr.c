/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* setup_umr.c - This module sets the registers on the UMR Board
   It contains two function:

   int setup_umr - This function sets the board control registers.
   void setup_rref_memory - This function loads the rref memory.

   The calling sequences and command line arguments for these
   functions are explained below.

int setup_umr(name,enable,rotate,rrefrc,test,format,conj,refdat,
	      hlseq,lpow)
    name      - "rf" = Range forward UMR Board
		"ri" = Range inverse UMR Board
    enable    - Each nibble specifies one functional block to
		  enable (1) or bypass (0):
		0x1000 = Inverse MUX
		0x0100 = Unscrambler
		0x0010 = MPY
		0x0001 = Forward MUX
    rotate    - Amount for Unscrambler to rotate, 13 bits
    rrefrc    - 1 = Set memory for range reference
		0 = Set memory for radiometric compensation
    test      - 1 = Send output to test bus
    format    - 1 = Divide the output of MPY by two
    conj      - 1 = Complex conjugate an input to MPY
		0 = Normal input to MPY
    refdat    - 1 = Select reference to be conjugated or bypassed
		0 = Select data to be conjugated or bypassed
    hlseq     - 1 = Unscrambler output is in hi-lo order
		0 = Unscrambler output is sequential
    lpow      - Linelength in power of two notation
    The data propagation delay is returned.

void setup_rref_memory(name,In,rref,hlseq,rotate,lpow,alpow)
    name      - "rf" = Range forward UMR Board
		"ri" = Range inverse UMR Board
    In        - The input reference function in sequential format
    rref      - 1 = Generate range reference function
		0 = Generate radiometric compensation function
    hlseq     - 1 = reorder function into hi-lo order
		0 = leave function in sequential order
    rotate    - Amount to circular shift the reference function
    lpow      - Linelength in power of two notation
    alpow     - Distance between "actual" lines (must be the
		same as lpow for the rref=1 case)

void setup_rc_memory (name, In, lpow, vlen, corr_echo, nsegments)

     name = "rf" (range forward) or "ri" (range inverse)
     In = the input radiometric compensation function in sequential order
     lpow = the linelength (stage length) in power of two notation
     vlen = number of completely correlated points in a line
     corr_echo = number of points that can be correlated in the echo
			 (echo - chirp)
     nsegments = number of segments in staged FFT

------------------------------------------------------------------
*/

#include <aspdecl.h>

/* setup_umr(name,enable,rot,rrefrc,test,fmt,conj,refdat,hlseq,lpow)-------

	 This routine changes the UMR register values to correspond
	 to the loop parameters in the test.
*/
int setup_umr (name,enable,rot,rrefrc,test,form,conj,refdat,hlseq,lpow)
int enable,rot,rrefrc,test,form,conj,refdat,hlseq,lpow;
char name[2];
{
    int     i, trigger, rloc;
    int     enableimux, enablefmux, enablempy, enableuns;
    static short int cregs[2];

 /* decide which umr board is named (rf=0, ri=1)   */

    rloc = (name[1] == 'i') ? RLOC_UMR_RI : RLOC_UMR_RF;

 /* set up the register bits */

    enableimux = ((enable & 0xf000) != 0);
    enableuns  = ((enable & 0x0f00) != 0);
    enablempy  = ((enable & 0x00f0) != 0);
    enablefmux = ((enable & 0x000f) != 0);
    cregs[0] = 0x00c0;
    cregs[0] |= (rrefrc << 15) | ((!test) << 14);
    cregs[0] |= (enableimux << 13) | (enablefmux << 12) | (enablempy << 11);
    cregs[0] |= ((form) << 10) | ((!conj) << 9) | (refdat << 8);
    cregs[0] |= (hlseq << 5) | (enableuns << 4) | lpow;
    cregs[1] = rot & 0x1fff;

 /* set the trigger count */

    trigger = 7 + (enablempy * 3) + (enableuns * (1 << lpow)) + !test;

 /* move the control registers to the board */

    for (i = 0; i < 2; i++) {
	mb.w[rloc + i] = cregs[i];
    }
    asp_write( rloc<<1, &mb.w[rloc], 4 );
/*
    printf ("sync codes = %X\n", gsync);
    printf ("\ncregs = %x %x\n", (cregs[0] & 0xffff), cregs[1]);
*/

    return (trigger);
}

/* setup_rref_memory (name, In, rref, hlseq, rotate, lpow, alpow) ------

     This routine loads the Range Reference memory in the appropriate
     order to make the outgoing data in high-low order.  
	     name = "rf" (range forward) or "ri" (range inverse)
	     In = the input reference function in sequential format
		upon exiting this routine, it is high low ordered if
		necessary for simulation.
	     rref = 1 means doing range reference function
		    0 means doing radiometric compensation function
	     hlseq = 1 means reorder the function into hi-lo order
	     rotate = the amount to circular shift the reference
		   function
	     lpow = the linelength in power of two notation
	     alpow = the distance between "actual" lines -- this must
		     be the same as lpow for rref=1.

     The input data array is read into an intermediate array in bit
     reversed order (which puts the data into sequential order)
     and then read out of the array in High-Low order.

     This function is slightly different from the one outlined in
     the ASP USERS MANUAL.  Instead, In(n) and Out(n) take the
     place of f(n) and l(n), respectively.  Also there are no functions
     f sub j, r sub j, etc.  These functions should be implemented as
     matrices, and since the range of their respective indices would
     create 32 MByte arrays, it is not practical to implement them as
     two-dimensional vectors.  Instead, they are implemented as one-
     dimensional arrays with for loops taking care of the independent
     indices.

*/
setup_rref_memory (name, In, rref, hlseq, rotate, lpow, alpow)
char name[3];
DATA In[2*MAXLINELEN];
int rref, hlseq, lpow, alpow, rotate;
{
       static short int Out[2*MAXLINELEN];
       static DATA r[2*MAXLINELEN];        /* rotated           */
       static DATA h[2*MAXLINELEN];        /* rotated, hi-lo    */
       static DATA d[2*MAXLINELEN];        /* delayed, one line + one  */
       int i, j, k, n;
       int rotadd, hladd, N, linelen, linehalf, rrefadd, byterev;
       int pid;
 
       pid = mb.w[RLOC_REP];
       if (rref) alpow = lpow;
       linelen = 1 << lpow;
       linehalf = linelen / 2;
       N = 1 << alpow;

  /* Step [1]  is implicit in for loops  */

  /* Step [2]:  rotate                   */

	for (n = 0; n < linelen; n++) {
	    rotadd = (n + rotate) % linelen;
	    for (i = 0; i < 2*MAXLINELEN; i += N)
		for (j = 0; j < N; j += linelen)
		    r[i + j + rotadd] =  In[j + n];
	} 

  /* Step [3]:  high-low reorder        */

	if (hlseq) {
	   for (j = 0, k = 0; j < 2*MAXLINELEN; j += linelen)
	       for (n = j; n < j + linehalf; n++) {
		   h[k++] = r[n];
		   h[k++] = r[n + linehalf];
	       }
	} else {
	    bcopy( r, h, 8*MAXLINELEN );
	}

/* copy reorded contents to user's input array */
	bcopy( h, In, 8*MAXLINELEN );

   /* Step [4]:  delay by one cycle + linelength        */

	for (n = 0; n < N; n++) {
	    rotadd = (n + linelen + 1) % N;
	    for (i = 0; i < 2*MAXLINELEN; i += N) d[i + rotadd] = h[i + n];
	}

   /* Step [5]:  memory map             */

       if (rref) {
	    bcopy( d, Out, linelen*4 );
       } else {
	   for (n = 0; n < 2*MAXLINELEN; n++)   /* zero imaginary   */
	       In[n].idat = 0;                  /* part of response */
	   for (n = 0, k = 1; n < MAXLINELEN; n++, k += 2)
	       Out [k] = d[n].rdat;
	   if (N > MAXLINELEN)
	   for (n = MAXLINELEN, k = 0; n < 2*MAXLINELEN; n++, k += 2)
	       Out [k] = d[n].rdat;
       }
    
    /* Load the data to the range reference memory */

	if (rref) mb.w[RLOC_REP] = PID_UMR_RF;
	else mb.w[RLOC_REP] = PID_UMR_RI;
	rrefadd = (name[1] == 'i') ? MEM_UMR_RI : MEM_UMR_RF;
	bcopy( Out, &mb.w[(rrefadd&0xfffff)>>1], 4*MAXLINELEN );
	asp_write( rrefadd, &mb.w[(rrefadd&0xfffff)>>1], 4*MAXLINELEN );
	mb.w[RLOC_REP] = pid;
}

/* setup_rc_memory (name, In, lpow, vlen, corr_echo, nsegments) ------

     This routine loads the Radiometric Compensation memory in the appropriate
     order.
	     name = "rf" (range forward) or "ri" (range inverse)
	     In = the input radiometric compensation function in sequential order
	     lpow = the linelength (stage length) in power of two notation
	     vlen = number of completely correlated points in a line
	     corr_echo = number of points that can be correlated in the echo
			 (echo - chirp)
             nsegments = number of segments in staged FFT

     The loading process accomodates the requirements of the staged fft algorithm.
     Due to hardware setup, loading addresses are advanced by one line length
     plus one point.
*/
setup_rc_memory (name, In, lpow, vlen, corr_echo, nsegments)
char name[3];
short int In[];
int lpow, vlen, corr_echo, nsegments;
{
       int i, j, k, n, linelen, start, doublen;
       int rcadd, pid;
       static short int temp[MAXLINELEN << 1];
 
       pid = mb.w[RLOC_REP];
       linelen = 1 << lpow;
       doublen = MAXLINELEN << 1;
       if (corr_echo > doublen) corr_echo = doublen;

    /* Load the radiometric compensation memory */

	mb.w[RLOC_REP] = (name[1] == 'i') ? PID_UMR_RI: PID_UMR_RF;

    /* Zero temporary array */
       for (j = 0; j < doublen; j++) temp[j] = 0;

    /* Zero tail portion of the function array */
       for (j = corr_echo; j < doublen; j++) In[j] = 0;

    /* Spread function in segments */
       start = 0;
       n = 0;
       while ( n < corr_echo) {
	   for (j = start; j < (start + vlen); j++) temp[j] = In[n++];
	   start += linelen;
       }

    /* Zero function array */
       for (j = 0; j < doublen; j++) In[j] = 0;

    /* Offset by a line and a sample */

       for (k = 1, n = 0; k <= nsegments; k++) {
	   start = k * linelen + 1;    /* First n - 1 segments are offset */
	   if (k == nsegments)         /* Last segment goes to the front */
	       start = 1;
	   for (j = start; j < start + linelen; j++, n++)
	       In[j % doublen] = temp[n];
       }

    /* dbg */
       /*
       for (j = 0; j < doublen;) {
	   printf ("j = %d", j);
	   for (n = 0; n < 8; n++) {
	       printf (" %d ", In[j++]);
	   }
	   printf ("\n");
       }
       */

    /* Load memory */
	i = (name[1] == 'i') ? MEM_UMR_RI : MEM_UMR_RF;
	rcadd = ( i & 0x000fffff ) >> 1;
	for (j = 0, k = 0; j < MAXLINELEN; j++, k += 2) {
	    mb.w[rcadd + k]     = In[j];               /* Real */
	    mb.w[rcadd + k + 1] = In[j + MAXLINELEN];  /* Imaginary */
        }
	asp_write( i, &mb.w[rcadd], 4*MAXLINELEN );

	mb.w[RLOC_REP] = pid;
}
