/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  get_2bits( in_buf, offset, out_buf, bits ) --------------------------
	extract 2 times redundant bit-interleaved data given bit offset 

  Inputs:
	IN_BUF:  Pointer to input buffer
	OFFSET:  first bit to extract from input buffer
	OUT_BUF:  Buffer to store extracted bits
	BITS:  Number of bits to extract
*/
int get_2bits( in_buf, offset, out_buf, bits )
char *in_buf, *out_buf;
int offset, bits;
{
unsigned long mask, unmask;

/*  Move data from offset of input buffer to beginning of output buffer  */

in_buf += offset >> 3;		/*  set input to first byte of offset  */
offset = offset & 7;		/*  number of bits to offset within byte  */
mask = 1 << offset;
unmask = 1;
out_buf[0] = 0;

while( bits>0 ){

    if( ( *in_buf & mask ) != 0 ) *out_buf = *out_buf | unmask;
    if( mask == 0x40 ){		/*  update input mask  */
	in_buf += 1;
	mask = 1;
    } else if( mask == 0x80 ){
	in_buf += 1;
	mask = 2;
    } else {
	mask = mask << 2;
    }
    if( unmask == 0x80 ){	/*  update output mask  */
	out_buf += 1;
	unmask = 1;
	*out_buf = 0;
    } else {
	unmask = unmask << 1;
    }
    bits -= 1;
}
return( 1 );
}
