/* scramble generator for RADARSAT
* Christian Fischer  07-19-95
*/
#include <stdio.h>

main ()
{
int x,lauf;
int byte;
int in;

x = 0xff; /*Start Condition*/

for(lauf = 1; lauf <= (8*311); lauf++) {
in = ( ((x&2)>>1)+((x&4)>>2)+((x&8)>>3)+((x&128)>>7) );  /*Input  next Clock
                   is 1 for result of the summ 1 and 3, 0 for 0,2 and 4*/
        in &=1;
        byte<<=1;
	byte |= in;
	if (!(lauf % 8)) {
		printf("0x%02x,",byte);
		if ( !(lauf % 64) )
			printf("\n");
		byte = 0;
		}
        x = x << 1; 
        x |=in;
        x&=255;/*Shiftregister limit to 1  Byte*/
	}
printf("\n");
}
