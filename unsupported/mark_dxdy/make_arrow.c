/****************************************************************
FUNCTION NAME: make_arrow - creates a LAS image arrow

SYNTAX: make_arrow(name, size)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    name	char[]		Name of the cross file to create
    dx,dy	int		length and direction of arrow

DESCRIPTION:  	Creates a LAS image that looks like an arrow.
		To be used to annotate images at certain points

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:  1.0  Initial Creation - Rudi Gens 5/01
		  Adapted from make_cross

****************************************************************/
#include "asf.h"
#include "ddr.h"

int make_arrow(name,dx,dy)
  char *name;
  int   dx,dy;
{
  int  i, j, s, x, y, size, square, height, width, minx, miny, maxx, maxy;
  float t;
  char file[256];
  FILE *fp;
  struct DDR ddr;
  unsigned char *cbuf, *crot, *cout, *cback;

  s = (int) sqrt(dx*dx+dy*dy);
  t = (PI/2-atan2(dy,dx))+PI/2;
  size = 25;
  square = 20*s+8;
  minx = miny = 999;
  maxx = maxy = -999;

  /* Minimum/maximum coordinates after rotation */
  x = (int) (cos(t)*9-sin(t)); /* for (x,y)=(9,1) */
  y = (int) (sin(t)*9+cos(t));
  if (x<minx) minx = x; if (y<miny) miny = y; if (x>maxx) maxx = x; if (y>maxy) maxy = y;
  x = (int) (cos(t)*12-sin(t)*square); /* for (x,y)=(12,20*s+8) */
  y = (int) (sin(t)*12+cos(t)*square);
  if (x<minx) minx = x; if (y<miny) miny = y; if (x>maxx) maxx = x; if (y>maxy) maxy = y;
  x = (int) (cos(t)*15-sin(t)); /* for (x,y)=(15,1) */
  y = (int) (sin(t)*15+cos(t));
  if (x<minx) minx = x; if (y<miny) miny = y; if (x>maxx) maxx = x; if (y>maxy) maxy = y;
  x = (int) (cos(t)*size-sin(t)*(square-12)); /* for (x,y)=(25,20*s-4) */
  y = (int) (sin(t)*size+cos(t)*(square-12));
  if (x<minx) minx = x; if (y<miny) miny = y; if (x>maxx) maxx = x; if (y>maxy) maxy = y;
  x = (int) (cos(t)-sin(t)*(square-12)); /* for (x,y)=(25,20*s-4) */
  y = (int) (sin(t)+cos(t)*(square-12));
  if (x<minx) minx = x; if (y<miny) miny = y; if (x>maxx) maxx = x; if (y>maxy) maxy = y;

  width = maxx-minx;
  height = maxy-miny;

  /* Allocating some space */
  cbuf = (unsigned char *) MALLOC ((8+20*s) * 25 * sizeof(unsigned char));
  crot = (unsigned char *) MALLOC ((8+20*s) * (8+20*s) * sizeof(unsigned char));
  cout = (unsigned char *) MALLOC (width * height * sizeof(unsigned char));
  cback = (unsigned char *) MALLOC (width * height * sizeof(unsigned char));
  
  /* Zero out the images */
  for (i=0; i<(8+20*s)*25; i++) cbuf[i] = 0;
  for (i=0; i<(8+20*s)*(8+20*s); i++) crot[i] = 0;
  for (i=0; i<width*height; i++) cout[i] = 0; 

  /* Make the tip */
  for (i = 1; i< 13; i++)
   for (j = 0; j < 24; j++)
     if (((i-11+j) > 1) && ((13+i-j) > 1)) cbuf[i*size+j] = 255;

  /* Make the rest */
  for (i = 13; i< 20*s+8; i++)
   for (j = 9; j < 16; j++)
     cbuf[i*size+j] = 255;

  /* Rotate the arrow */
  for (i = 0; i< 20*s+8; i++)
   for (j = 0; j < 25; j++) {
     x = (int) (cos(t)*j - sin(t)*i)-minx+1;
     y = (int) (sin(t)*j + cos(t)*i)-miny-1;
     crot[y*square+x] = cbuf[i*size+j];
   }

  /* Shrink the arrow because it has tiny little holes */
  for (i = 0; i< 20*s+8; i=i+2)
   for (j = 0; j < 20*s+8; j=j+2) {
       x = (int) i/2;
       y = (int) j/2;
       cout[y*width+x] = (crot[i*square+j]+crot[(i+1)*square+j]+crot[i*square+j+1]+crot[(i+1)*square+j+1])/4;
       if (cout[y*width+x]>64) cout[y*width+x] = 255;
       else cout[y*width+x] = 0;
     }

  /* Create inverted arrow */
  for (i=0; i<width*height; i++) { 
    if (cout[i] == 0) cback[i] = 255;
    else cback[i] = 0;
  }

  /* Create the image file and ddr */
  strcat(strcpy(file, name),".img");
  fp = fopenImage(file,"wb");
  if (fp==NULL) {fprintf(stderr,"Make_Arrow:: Can't Create %s\n",file);exit(1);}
  fwrite(cout,width*height,1,fp);
  fclose(fp);
  c_intddr(&ddr);
  ddr.nbands=1;
  ddr.dtype=DTYPE_BYTE;
  ddr.nl=height;
  ddr.ns=width;
  c_putddr(file,&ddr);

  /* Write away the inverted arrow */
  strcat(strcpy(file, name),"2.img");
  fp = fopenImage(file,"wb");
  if (fp==NULL) {fprintf(stderr,"Make_Arrow:: Can't Create %s\n",file);exit(1);}
  fwrite(cback,width*height,1,fp);
  fclose(fp);
  c_putddr(file,&ddr);

  /* Clearing up */
  free(cbuf);
  free(crot);
  free(cout);
  free(cback);

  return(1);
}

