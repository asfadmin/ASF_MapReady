/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* get_bits( in_buf, offset, out_buf, bits ) -------------------------
	extract data from input buffer with bit offset

    Inputs:
	IN_BUF:  Pointer to input buffer
	OFFSET:  first bit to extract from input buffer
	OUT_BUF:  Buffer to store extracted bits
	BITS:  Number of bits to extract
*/
int get_bits( in_buf, offset, out_buf, bits )
char *in_buf, *out_buf;
int offset, bits;
{
unsigned char mask, unmask;

/*  Move data from offset of input buffer to beginning of output buffer  */

in_buf += offset >> 3;		/*  go to first byte of offset  */
offset = offset & 7;		/*  bit offset within byte  */
unmask = ~0<<(8-offset);	/*  other bits within byte  */
mask = ~unmask;			/*  bit mask within byte  */
out_buf[0] = 0;

if( offset==0 ){
    while( bits>0 ){
	*out_buf++ = *in_buf++;
	bits -= 8;
    }
} else {
    while( bits>0 ){
/*  combine upper bits from first byte and lower bits from next byte  */

	*out_buf++ = (in_buf[0]&mask)<<offset | (in_buf[1]&unmask)>>(8-offset);
	in_buf ++;
	bits -= 8;
    }
}
if( bits<0 ){		/*  get rid of extra stuff  */
    out_buf--;
    *out_buf = *out_buf & ~(~0<<(8+bits));
}
return(1);
}
