/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* setup_fft.c - This module sets the control registers for
	a given 13-stage fft.  It contains the following
	functions:
	int setup_fft - sets control registers for all three
			fft boards and returns data propagation
			delay
        void get_fft_oflo_regs - gets the fft module overflow
			register values
        void clear_fft_oflo_regs - clears the fft module overflow
			register values

   The calling sequences and command line arguments for these
   functions are explained below.

int setup_fft(fftname,stages,fwdinv,pow,scale)
    fftname - rf = range forward fft
	      ri = range inverse
	      af = azimuth forward
	      ai = azimuth inverse
    stages  - 13 bits, each asserted bit activates the
	      corresponding stage.
    fwdinv  - 1 = All stages do forward fft
	      0 = All stages do inverse fft
    pow     - Input line length in power of 2 notation
    scale   - 13 bits, each zero bit causes the corresponding 
	      stage's output to be divided by 2.  The first 
	      stage is assigned to the most significant bit.

void get_fft_oflo_regs(name,oflo)
    name    - Same as fftname above
    oflo    - 13 value array providing the contents of overflow
	      register from each stage

void clear_fft_oflo_regs(name)
    name    - Same as fftname above

-----------------------------------------------------------------
*/

#include <aspdecl.h>

/* setup_fft(fftname, stages, fwdinv, pow, scale) ---------------
	This routine sets the fft board control registers for all
	three boards to the values corresponding to the input 
	parameters.
*/
setup_fft(fftname, stages, fwdinv, pow, scale)
char fftname[3];
int stages, fwdinv, pow, scale;
{
	int i,j,trigger,rloc,fftno;
	short int b1cregs[4], b2cregs[4], b3cregs[5];
	static int fftaddr[4] = { RLOC_FFT_RF1,RLOC_FFT_RI1,
				  RLOC_FFT_AF1,RLOC_FFT_AI1};
	static short int cregs[5];

    /* decide which fft module was requested 
		(rf=0, ri=1, af=2, ai=3)     */
	fftno =   (tolower(fftname[0]) == 'a') * 2
		+ (tolower(fftname[1]) == 'i');

    /* set bypass, fwd/inv and scale bits */
	b1cregs[1] = ((stages & 0x1e00) << 3) | (fwdinv << 8)
		| ((scale & 0x1e00) >> 5) | 0x0e0f;
	b2cregs[1] = ((stages & 0x1e0) << 7) | (fwdinv << 8)
		| ((scale & 0x1e0) >> 1) | 0x0e0f;
	b3cregs[1] = ((stages & 0x1f) << 11) | (fwdinv << 8)
		| ((scale & 0x1f) << 3) | 0x0607;

    /* set pow switches and trigger count */
	trigger = 18;
	i = 1 << 12;
	b1cregs[0] = 0;
	for (j = 12; j >= 0; i >>= 1, j -= 4)
	    if (stages & i) {
		b1cregs[0] |= (pow << j);
		pow--;
		trigger += (1 << pow) + 4;
		trigger += (pow == 0);
	    }
	b2cregs[0] = 0;
	for (j = 12; j >= 0; i >>= 1, j -= 4)
	    if (stages & i) {
		b2cregs[0] |= (pow << j);
		pow--;
		trigger += (1 << pow) + 4;
		trigger += (pow == 0);
	    }
	b3cregs[0] = 0;
	for (j = 12; j >= 0; i >>= 1, j -= 4)
	    if (stages & i) {
		b3cregs[0] |= (pow << j);
		pow--;
		if (i != 2) {
		    trigger += (1 << pow) + 4;
		    trigger += (pow == 0);
		}
	    }
	if (stages & 2)
	    trigger += 3;

    /* set the overflow counters */
	b1cregs[2] = b1cregs[3] = 0;
	b2cregs[2] = b2cregs[3] = 0;
	b3cregs[2] = b3cregs[3] = b3cregs[4] = 0;

    /* move the control registers to the board */
	rloc = fftaddr[fftno];
	for (i = 0; i < 4; i++) {
	    mb.w[rloc + i] = b1cregs[i];
	    mb.w[rloc + i] = b1cregs[i];
	}
	asp_write( rloc<<1, &mb.w[rloc], 8 );
	asp_write( rloc<<1, &mb.w[rloc], 8 );
	rloc += 8;
	for (i = 0; i < 4; i++) {
	    mb.w[rloc + i] = b2cregs[i];
	    mb.w[rloc + i] = b2cregs[i];
	}
	asp_write( rloc<<1, &mb.w[rloc], 8 );
	asp_write( rloc<<1, &mb.w[rloc], 8 );
	rloc += 8;
	for (i = 0; i < 5; i++) {
	    mb.w[rloc + i] = b3cregs[i];
	    mb.w[rloc + i] = b3cregs[i];
	}
	asp_write( rloc<<1, &mb.w[rloc], 10 );
	asp_write( rloc<<1, &mb.w[rloc], 10 );

	return (trigger);
}

/* get_fft_oflo_regs(name,oflo) ------------------------------------------
	This routine returns 13 overflow register values from the
	fft module named.  The names are:
		rf = range forward fft
		ri = range inverse
		af = azimuth forward
		ai = azimuth inverse
*/
get_fft_oflo_regs (name, oflo)
char name[20];
int oflo[13];
{
	int fftno, rloc;
	static int fftaddr[4] = { RLOC_FFT_RF1,RLOC_FFT_RI1,RLOC_FFT_AF1,RLOC_FFT_AI1 };

    /* decide which fft module was requested 
		(rf=0, ri=1, af=2, ai=3)     */
	fftno =   (tolower(name[0]) == 'a') * 2
		+ (tolower(name[1]) == 'i');
	rloc = fftaddr[fftno];
	asp_read( (rloc+2)<<1, &mb.w[rloc+2], 4 );
	oflo[0] = (mb.w[rloc+3] >> 8) & 0xff;
	oflo[1] =  mb.w[rloc+3]       & 0xff;
	oflo[2] = (mb.w[rloc+2] >> 8) & 0xff;
	oflo[3] =  mb.w[rloc+2]       & 0xff;
	rloc += 8;
	asp_read( (rloc+2)<<1, &mb.w[rloc+2], 4 );
	oflo[4] = (mb.w[rloc+3] >> 8) & 0xff;
	oflo[5] =  mb.w[rloc+3]       & 0xff;
	oflo[6] = (mb.w[rloc+2] >> 8) & 0xff;
	oflo[7] =  mb.w[rloc+2]       & 0xff;
	rloc += 8;
	asp_read( (rloc+2)<<1, &mb.w[rloc+2], 6 );
	oflo[8]  = (mb.w[rloc+4] >> 8) & 0xff;
	oflo[9]  =  mb.w[rloc+4]       & 0xff;
	oflo[10] = (mb.w[rloc+3] >> 8) & 0xff;
	oflo[11] =  mb.w[rloc+3]       & 0xff;
	oflo[12] =  mb.w[rloc+2];
}

/* clear_fft_oflo_regs(name) ------------------------------------------
	This routine clears all 13 overflow registers in the
	fft module named.  The names are:
		rf = range forward fft
		ri = range inverse
		af = azimuth forward
		ai = azimuth inverse
*/

clear_fft_oflo_regs (name)
	char name[3];
{
	int fftno, rloc;
	static int fftaddr[4] = { RLOC_FFT_RF1,RLOC_FFT_RI1,RLOC_FFT_AF1,RLOC_FFT_AI1 };

    /* decide which fft module was requested 
		(rf=0, ri=1, af=2, ai=3)     */
	fftno =   (tolower(name[0]) == 'a') * 2
		+ (tolower(name[1]) == 'i');
	rloc = fftaddr[fftno];

    /* clear overflow registers on first fft board */
	mb.w[rloc+2] = 0;
	mb.w[rloc+3] = 0;
	asp_write( (rloc+2)<<1, &mb.w[rloc+2], 4 );

    /* clear second fft board */
	rloc += 8;
	mb.w[rloc+2] = 0;
	mb.w[rloc+3] = 0;
	asp_write( (rloc+2)<<1, &mb.w[rloc+2], 4 );

    /* clear five board */
	rloc += 8;
	mb.w[rloc+2] = 0;
	mb.w[rloc+3] = 0;
	mb.w[rloc+4] = 0;
	asp_write( (rloc+2)<<1, &mb.w[rloc+2], 6 );
}

/* set_fft_scaling(fftname,scale) ------------------------------------
	This routine sets the scaling bits for the named fft module.
*/
set_fft_scaling(fftname,scale)
char fftname[3];
int scale;
{
	static int fftaddr[4] = { RLOC_FFT_RF1,RLOC_FFT_RI1,
				  RLOC_FFT_AF1,RLOC_FFT_AI1};
	short int b1cregs[4], b2cregs[4], b3cregs[5];
	int rloc,fftno;

    /* decide which fft module was requested 
		(rf=0, ri=1, af=2, ai=3)     */
	fftno =   (tolower(fftname[0]) == 'a') * 2
		+ (tolower(fftname[1]) == 'i');

    /* get the original register contents */
	rloc = fftaddr[fftno];
	asp_read( (rloc+1)<<1, &mb.w[rloc+1], 2 );
	b1cregs[1] = mb.w[rloc + 1] & 0xff0f;
	asp_read( (rloc+8+1)<<1, &mb.w[rloc+8+1], 2 );
	b2cregs[1] = mb.w[rloc + 8 + 1] & 0xff0f;
	asp_read( (rloc+16+1)<<1, &mb.w[rloc+16+1], 2 );
	b3cregs[1] = mb.w[rloc + 16 + 1] & 0xff07;
    /* set bypass, fwd/inv and scale bits */
	b1cregs[1] |= (scale & 0x1e00) >> 5;
	b2cregs[1] |= (scale & 0x1e0) >> 1;
	b3cregs[1] |= (scale & 0x1f) << 3;
    /* return values to the fft registers */
	mb.w[rloc + 1] = b1cregs[1];
	asp_write( (rloc+1)<<1, &mb.w[rloc+1], 2 );
	mb.w[rloc + 8 + 1] = b2cregs[1];
	asp_write( (rloc+8+1)<<1, &mb.w[rloc+8+1], 2 );
	mb.w[rloc + 16 + 1] = b3cregs[1];
	asp_write( (rloc+16+1)<<1, &mb.w[rloc+16+1], 2 );
}
