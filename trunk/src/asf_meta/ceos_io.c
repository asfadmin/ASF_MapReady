/*CEOS Input Library implementation.

*/
#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "ceos.h"
#include "ceos_io.h"

int firstRecordLen(char *ceosName)
{
  FILE *f;
  struct HEADER h;

  f=FOPEN(ceosName,"rb");    /*Open file.*/
  FREAD(&h,1,12,f);          /*Read first CEOS header.*/
  FCLOSE(f);                 /*Close file*/
  return bigInt32(h.recsiz); /*put recsiz in proper endian format & return*/
}


CEOS_FILE *fopenCeos(char *fName)
{
  struct IOF_VFDR iof;    /*Imagery Options File, from CEOS*/
  CEOS_FILE *c = (CEOS_FILE *)MALLOC(sizeof(CEOS_FILE));

  get_ifiledr(fName, &iof);
  c->lineBytes = iof.reclen;

/* Determine data header size. Set c->name (filename) */
  if (set_era(fName,c->name,0)) /*new data header size*/
    c->headerBytes = firstRecordLen(c->name) + 192;
  else /*old data header size*/
    c->headerBytes = firstRecordLen(c->name) + 12;

/* Get meta structs from CEOS */
  c->meta = meta_create(fName);

/* Fill DDR struct for backwards compatibility */
  meta2ddr(c->meta, &c->ddr);

/* Open input file.*/
  c->f_in=FOPEN(c->name,"rb");

  return c;
}

void readCeosLine(int *dest,int y,CEOS_FILE *c)
{
  int ns=c->ddr.ns;
  int lineLen=ns*dtype2dsize(c->ddr.dtype,NULL);
  unsigned char *buf=(unsigned char *)MALLOC(lineLen);
  int i;
  FSEEK64(c->f_in,(long long)c->headerBytes+y*c->lineBytes,0);
  FREAD(buf,1,lineLen,c->f_in);
  if (c->ddr.dtype==DTYPE_BYTE)
    for (i=0;i<ns;i++)
      dest[i]=buf[i];
  else
    if (c->ddr.dtype==DTYPE_SHORT)
    for (i=0;i<ns;i++)
      dest[i]=(buf[2*i]<<8)+buf[2*i+1];
  else
  {
    fprintf(stderr,"Attempted to read unsupported CEOS data type %d\n"
      "from image file %s!\n",c->ddr.dtype,c->name);
    exit(1);
  }
  FREE(buf);
}

void closeCeos(CEOS_FILE *in)
{
  FCLOSE(in->f_in);
  in->f_in=NULL;
  meta_free(in->meta);
  FREE((void *)in);
}
