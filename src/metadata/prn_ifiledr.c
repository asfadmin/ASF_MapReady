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

void prn_ifiledr(struct IOF_VFDR *ifdr)
{
 printf("\n********** begin of Image File Descriptor record *************\n\n");
 printf("Number of data records =                  %i\n", ifdr->numofrec );
 printf("Record Length =                           %i\n", ifdr->reclen );
 printf("Bits per sample =                         %i\n", ifdr->bitssamp );
 printf("Samples per data group =                  %i\n", ifdr->sampdata );
 printf("Bytes per group =                         %i\n", ifdr->bytgroup );
 printf("Justification & order of samples =        %s\n", ifdr->justific );
 printf("SAR channels =                            %i\n", ifdr->sarchan );
 printf("Lines per data set =                      %i\n", ifdr->linedata );
 printf("Left border pixels per line =             %i\n", ifdr->lbrdrpxl );
 printf("Total number of data groups =             %i\n", ifdr->datgroup );
 printf("Right border pixels per line =            %i\n", ifdr->rbrdrpxl );
 printf("Top border lines =                        %i\n", ifdr->topbrdr );
 printf("Bottom border lines =                     %i\n", ifdr->botbrdr );
 printf("Interleave indicator =                    %s\n", ifdr->interlv );
 printf("Physical records per line =               %i\n", ifdr->recline );
 printf("Physical records per multi-line channel = %i\n", ifdr->mrecline );
 printf("Bytes of prefix data per record =         %i\n", ifdr->predata );
 printf("Bytes of SAR data per record =            %i\n", ifdr->sardata );
 printf("Bytes of suffix data per record =         %i\n", ifdr->sufdata );
 printf("Suffix/Prefix repeat flag =               %s\n", ifdr->repflag );
 printf("SAR data format identifier =              %s\n", ifdr->formatid );
 printf("SAR data format code =                    %s\n", ifdr->formcode );
 printf("Left fill bits per pixel =                %i\n", ifdr->leftfill );
 printf("Right fill bits per pixel =               %i\n", ifdr->rigtfill );
 printf("Maximum Data Value =                      %i\n", ifdr->maxidata );
 printf("*********** end of Image File Descriptor record **************\n\n");
 return;
}

