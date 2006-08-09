/**
Tiny program to read the EGM96 Geoid image, which is just 16-bit
big-endian signed geoid heights, in centimeters, arranged in a 1440x721
grid.

The heights are rounded to meters and stored in a PPM file.

Orion Sky Lawlor, olawlor@acm.org, 2006/07/21 (ASF)
*/

#include <stdio.h>

int main() {
	FILE *f=fopen("WW15MGH.DAC","rb");
	FILE *o=fopen("out.ppm","wb");
	FILE *r=fopen("out.raw","wb");
	int w=1440, h=721;
	fprintf(o,
	"P6\n"
	"%d %d\n"
	"%d\n",w,h,255);
	double max=-100000,min=+1000000;
	for (int y=0;y<h;y++)
	for (int x=0;x<w;x++) {
		signed char hi=fgetc(f);
		unsigned char lo=fgetc(f);
		double height=((hi<<8)+lo)*(1.0/100);
		if (max<height) max=height;
		if (min>height) min=height;
		// unsigned char v=(int)(height*255/(2*108.0)+128+.5); // weird scaling
		int v=(int)(height+128+.5); // just meters
		if (v<0) v=0;
		if (v>255) v=255;
		fputc(v,o); fputc(v,o); fputc(v,o); 
		fputc(v,r);
	}
	printf("Max %f m, min %f m\n",max,min);
	fclose(f);
	fclose(o);
	return 0;
}
