/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  This is the GOLD version.  The final (heh heh) 16X16 multiply
       simulator.  The simulator against which all others pale in
       significance.
       
           EMULATION ROUTINE FOR MPY016K MULTIPLIER
	(or any plug compatible 16X16 multiplier)

	Input is ( X, Y, RND, FA, TCX, TCY )
	    all variables are integers.
		X, Y = two 16-bit values to multiply together
		RND = 1 means round by 1/2 (0 = round by 1/4)
		FA = 0 means output fractional 2's complement
			shifted format
		TCX = 1 means x value is two's complement 
		TCY = 1 means y value is two's complement 
	Output is the exact 32 bits output by the multiplier as P(0:31).
		Out is not necessarily a two's complement integer, and
		its format depends on the control settings to the MPY016K.

	Modified by Ben Charny to accomodate unsigned multiplication.
		1.26.89

        Re-modified by Ted Robnett to really accomodate unsigned
	    multiplication  2.22.89
*/

/* mpy (xdat,ydat,rnd,fa,tcx,tcy) --------------------------------------
	This routine emulates the MPY016K multiplier chip.
*/
mpy (xdat, ydat, rnd, fa, tcx, tcy)
short int xdat, ydat;
short int rnd, fa, tcx, tcy;
{
	int	x, y;
	int     ans;
	int     out;

	x = xdat;	/* Type conversion for two's complement */
	y = ydat;
	if (tcx == 0)	/* Type conversion for unsigned magnitude */
		x &= 0xffff;
	if (tcy == 0)
		y &= 0xffff;
	ans = x * y;
	if (fa == 0) {
	    if (rnd == 1) ans = ans + 0x4000;
	    out = ((ans * 2) & 0xffff0000) + (ans & 0x7fff)
		+ ((ans >> 16) & 0x8000);
	} else {
		if (rnd == 1) out = ans + 0x8000;
		else out = ans;
	}
	return (out);
}
