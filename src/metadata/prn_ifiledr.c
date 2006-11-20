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

void prn_ifiledr(FILE *fp, struct IOF_VFDR *ifdr)
{
  fprintf(fp, "\n********** begin of Image File Descriptor record ***********\n\n");
  fprintf(fp, "Number of data records =                  %i\n", ifdr->numofrec );
  fprintf(fp, "Record Length =                           %i\n", ifdr->reclen );
  fprintf(fp, "Bits per sample =                         %i\n", ifdr->bitssamp );
  fprintf(fp, "Samples per data group =                  %i\n", ifdr->sampdata );
  fprintf(fp, "Bytes per group =                         %i\n", ifdr->bytgroup );
  fprintf(fp, "Justification & order of samples =        %s\n", ifdr->justific );
  fprintf(fp, "SAR channels =                            %i\n", ifdr->sarchan );
  fprintf(fp, "Lines per data set =                      %i\n", ifdr->linedata );
  fprintf(fp, "Left border pixels per line =             %i\n", ifdr->lbrdrpxl );
  fprintf(fp, "Total number of data groups =             %i\n", ifdr->datgroup );
  fprintf(fp, "Right border pixels per line =            %i\n", ifdr->rbrdrpxl );
  fprintf(fp, "Top border lines =                        %i\n", ifdr->topbrdr );
  fprintf(fp, "Bottom border lines =                     %i\n", ifdr->botbrdr );
  fprintf(fp, "Interleave indicator =                    %s\n", ifdr->interlv );
  fprintf(fp, "Physical records per line =               %i\n", ifdr->recline );
  fprintf(fp, "Physical records per multi-line channel = %i\n", ifdr->mrecline );
  fprintf(fp, "Bytes of prefix data per record =         %i\n", ifdr->predata );
  fprintf(fp, "Bytes of SAR data per record =            %i\n", ifdr->sardata );
  fprintf(fp, "Bytes of suffix data per record =         %i\n", ifdr->sufdata );
  fprintf(fp, "Suffix/Prefix repeat flag =               %s\n", ifdr->repflag );
  fprintf(fp, "SAR data format identifier =              %s\n", ifdr->formatid );
  fprintf(fp, "SAR data format code =                    %s\n", ifdr->formcode );
  fprintf(fp, "Left fill bits per pixel =                %i\n", ifdr->leftfill );
  fprintf(fp, "Right fill bits per pixel =               %i\n", ifdr->rigtfill );
  fprintf(fp, "Maximum Data Value =                      %i\n", ifdr->maxidata );
  fprintf(fp, "*********** end of Image File Descriptor record **************\n\n");
  return;
}

