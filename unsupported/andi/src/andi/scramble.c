/* scramble generator for ERS-I
* Hans-Joerg Wagner 07-25-94
*/
#include <stdio.h>

main ()
{
int x,lauf;
int byte;
int I,Q,in;

x = 0x154;

for(lauf = 0; lauf <= 1000; lauf++) {
	in = ((x & (1 << 7)) >> 7) ^ (x & 1);
	I = in;
	Q = (x & (1 << 9));
	byte |= I ? 1 : 0; byte = byte << 1;
	byte |= Q ? 1 : 0; byte = byte << 1;
	if (!(lauf % 4)) {
		printf(" %2X",byte>>1);
		if ( !(lauf % 64) )
			printf("\n");
		byte = 0;
		}
	x = x >> 1; x |= (in) ? (1 << 9) : 0;
	}
printf("\n");
}
