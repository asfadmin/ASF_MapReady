/********************************************************************
NAME:   get_ifiledr.c --
            reads ASF groundstation imagery files, extract the file
            descriptor record, and return pointer to IOF_VFDR 
            (imagery options file value of file descriptor record)
            structure.
 
SYNOPSIS:get_ifiledr(char *filename, struct IOF_VFDR *vfdr) 
 
PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           9/93   T. Logan (ASF)
  1.1           8/95   M. Shindle      System V declarations added
  1.2		3/96   T. Logan        Updating for metadata project
  1.3           9/96   T. Logan (ASF)  Modified to return era of record
  1.4		7/97  O. Lawlor (ASF)   Fixed crashing bug-- buffer overwrite.

*********************************************************************/
#include "asf.h"
#include "ceos.h"

int  myDecode_SARL_IFILEDR(struct IOF_FDR* ifdr,struct IOF_VFDR *i);

int get_myifiledr(filename, vfdr)
  char *filename;
  struct IOF_VFDR *vfdr;
{
  FILE    *fp;
  char    *buff;
  int     itype, length, era;
  char    name[256];
  struct  HEADER  bufhdr;
  struct  IOF_FDR *ip;

  era = set_era(filename,name,0);
  if ((fp = fopen(name, "r")) == NULL)
   {fprintf (stderr, "GET_IFILEDR: Unable to open file %s\n", name); exit (0); }
  itype = -1; 
  while (itype != 192)
   {
     if (fread (&bufhdr, 12, 1, fp) != 1)
      { fprintf(stderr," End of file detected.\n"); exit(0); }
     itype = bufhdr.rectyp[1];
     length = bigInt32(bufhdr.recsiz) -12 ;
     buff=(char *)MALLOC(length);
     if((fread(buff, length, 1, fp)) != 1) 
      { fprintf(stderr," Error reading data portion of record.\n"); exit(0); }
     if (itype == 192)   /*  This is the File Descriptor Record */
      {
        ip = (struct IOF_FDR *) buff;
        myDecode_SARL_IFILEDR(ip, vfdr);     
      }
     free(buff);
   }    
  fclose(fp);
  return(era);
}
 
/****************************************************************
NAME:   Decode_SARL_IFILEDR  -- Decodes SAR Image File Descriptor Record

SYNOPSIS: Decode_SARL_IFILEDR(unsigned char* buf,struct IOF_VFDR *i)

DESCRIPTION:  Decodes the values from a character buffer into the
              Image File Descriptor Record structure.

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
1.0             4/96   T. Logan 
*****************************************************************/
int  myDecode_SARL_IFILEDR(struct IOF_FDR* ifdr,struct IOF_VFDR *i)
{

    /* process the record */
    i->numofrec = get_I4(ifdr->numofrec,6);
    i->reclen   = get_I4(ifdr->reclen,6);
    i->bitssamp = get_I4(ifdr->bitssamp,4);
    i->sampdata = get_I4(ifdr->sampdata,4);
    i->bytgroup = get_I4(ifdr->bytgroup,4);
    strncpy(i->justific,ifdr->justific,4); i->justific[4] = '\0';
    i->sarchan  = get_I4(ifdr->sarchan,4);
    i->linedata = get_I4(ifdr->linedata,8);
    i->lbrdrpxl = get_I4(ifdr->lbrdrpxl,4);
    i->datgroup = get_I4(ifdr->datgroup,8);
    i->rbrdrpxl = get_I4(ifdr->rbrdrpxl,4);
    i->topbrdr  = get_I4(ifdr->topbrdr,4);
    i->botbrdr  = get_I4(ifdr->botbrdr,4);
    strncpy(i->interlv,ifdr->interlv,4); i->interlv[4] = '\0';
    i->recline  = get_I4(ifdr->recline,2);
    i->mrecline = get_I4(ifdr->mrecline,2);
    i->predata  = get_I4(ifdr->predata,4);
    i->sardata  = get_I4(ifdr->sardata,8);
    i->sufdata  = get_I4(ifdr->sufdata,4);
    strncpy(i->repflag,ifdr->repflag,4); i->repflag[4] = '\0';
    strncpy(i->formatid,ifdr->formatid,28); i->formatid[28] = '\0';
    strncpy(i->formcode,ifdr->formcode,4); i->formcode[4] = '\0';
    i->leftfill = get_I4(ifdr->leftfill,4);
    i->rigtfill = get_I4(ifdr->rigtfill,4);
    i->maxidata = get_I4(ifdr->maxidata,8);

    return(0);
}
