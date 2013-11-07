/********************************************************************
NAME:     print_string_ifiledr.c --  print file descriptor record

SYNOPSIS: prn_ifiledr(struct IOF_VFDR *ifdr)

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           9/93   T. Logan (ASF)
  1.1		4/96   T. Logan (ASF)  Updated for metadata project
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_ifiledr(struct IOF_VFDR *ifdr)
{
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n********** begin of Image File Descriptor record ***********\n\n");
  add(&ret, "Number of data records =                  %i\n", ifdr->numofrec );
  add(&ret, "Record Length =                           %i\n", ifdr->reclen );
  add(&ret, "Bits per sample =                         %i\n", ifdr->bitssamp );
  add(&ret, "Samples per data group =                  %i\n", ifdr->sampdata );
  add(&ret, "Bytes per group =                         %i\n", ifdr->bytgroup );
  add(&ret, "Justification & order of samples =        %s\n", ifdr->justific );
  add(&ret, "SAR channels =                            %i\n", ifdr->sarchan );
  add(&ret, "Lines per data set =                      %i\n", ifdr->linedata );
  add(&ret, "Left border pixels per line =             %i\n", ifdr->lbrdrpxl );
  add(&ret, "Total number of data groups =             %i\n", ifdr->datgroup );
  add(&ret, "Right border pixels per line =            %i\n", ifdr->rbrdrpxl );
  add(&ret, "Top border lines =                        %i\n", ifdr->topbrdr );
  add(&ret, "Bottom border lines =                     %i\n", ifdr->botbrdr );
  add(&ret, "Interleave indicator =                    %s\n", ifdr->interlv );
  add(&ret, "Physical records per line =               %i\n", ifdr->recline );
  add(&ret, "Physical records per multi-line channel = %i\n", ifdr->mrecline );
  add(&ret, "Bytes of prefix data per record =         %i\n", ifdr->predata );
  add(&ret, "Bytes of SAR data per record =            %i\n", ifdr->sardata );
  add(&ret, "Bytes of suffix data per record =         %i\n", ifdr->sufdata );
  add(&ret, "Suffix/Prefix repeat flag =               %s\n", ifdr->repflag );
  add(&ret, "SAR data format identifier =              %s\n", ifdr->formatid );
  add(&ret, "SAR data format code =                    %s\n", ifdr->formcode );
  add(&ret, "Left fill bits per pixel =                %i\n", ifdr->leftfill );
  add(&ret, "Right fill bits per pixel =               %i\n", ifdr->rigtfill );
  add(&ret, "Maximum Data Value =                      %i\n", ifdr->maxidata );
  add(&ret, "*********** end of Image File Descriptor record **************\n\n");
  return ret;
}

void prn_ifiledr(FILE *fp, struct IOF_VFDR *ifdr)
{
    char *rec = sprn_ifiledr(ifdr);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

