/****************************************************************
FUNCTION NAME: make_cross - creates a LAS image cross

SYNTAX: make_cross(name, size)

PARAMETERS:
    NAME:	TYPE:		PURPOSE:
    --------------------------------------------------------
    name	char[]		Name of the cross file to create
    size	int		length and width of cross

DESCRIPTION:  	Creates a LAS image that looks like a cross.
		To be used to annotate images at certain points

RETURN VALUE:

SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:  1.0  Initial Creation - Tom Logan 4/97

****************************************************************/
#include "asf.h"
#include "ddr.h"

int make_cross(char *name,int size);
int make_cross(char *name,int size)
{
  int  block, base, lb, ub, i, j;
  char file[256];
  FILE *fp;
  unsigned char *cbuf;


  size = ((size+4) / 5) * 5;
  block = size / 5;

  cbuf = (unsigned char *) MALLOC (size * size * sizeof(unsigned char));

  /* Zero out the image */
  for (i=0; i<size*size; i++) cbuf[i] = 0;

  /* Make the large cross */
  for (i = 0; i< size; i++)
   {
    if (i < block*2 || i >= block*3)
      for (j=block*2; j<block*3; j++) cbuf[i*size+j] = 255;
    else
      for (j=0; j<size; j++) cbuf[i*size+j] = 255;
   }

  /* Embed the smaller cross */
  base = block*2;
  for (i = 0; i< block; i++)
   for (j = 0; j < block; j++)
     if (((i%(block-1))+(j%(block-1))) != 0) cbuf[(i+base)*size + j+base] = 1;

  /* Hollow out the embedded cross */
  lb = (block/2)-1 + base;
  ub = block-(block/2) + base;
  for (i=lb ; i<= ub; i++)
   {
    if (i==lb||i==ub) for (j=lb+1; j<ub ; j++) cbuf[i*size+j] = 255;
    else              for (j=lb  ; j<=ub; j++) cbuf[i*size+j] = 255;
   }

  /* Create the image file and ddr */
  strcat(strcpy(file, name),".img");
  fp = fopenImage(file,"wb");
  if (fp==NULL) {fprintf(stderr,"Make_Cross:: Can't Create %s\n",file);exit(1);}
  FWRITE(cbuf,(unsigned)(size*size),1,fp);
  fclose(fp);
  free(cbuf);
  {
	struct DDR ddr;
	c_intddr(&ddr);
	ddr.nbands=1;
	ddr.dtype=DTYPE_BYTE;
	ddr.nl=ddr.ns=size;
	c_putddr(file,&ddr);
  }
  return(1);
}

