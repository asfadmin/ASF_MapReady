/* ber.c
* ber program
* estimates the bit error rate in a decoded raw data file. Therefore
* it scans to the zero file, created by the ANDI tool and counts the
* non zero bits
*
* First setup 07-25-1994 by Hans-Joerg Wagner
*/

#include <stdio.h>
#include <malloc.h>
#include "drdapi.h"

/* prototyping */
void Hello();
int DecodeArgs();
void Usage();
void CalculateBitErrorRate();
void Bye();

/* structure for command line arguments */
struct {
	char *pszInFile;
	} CmdLine;

/**********************************************************************
* main program
*/
main(argc,argv)
int argc;
char **argv;
{
Hello();
if (!DecodeArgs(argc,argv)) {
	Usage();
	exit(1);
	}
drdInit("ber",1,0,"Alaska SAR Facility");
CalculateBitErrorRate();
drdExit();
Bye();
}

/**********************************************************************
* Hello says hello
*/
void Hello()
{
printf("BER V1.0 Estimation of bit error rate after decodeing raw data file\n");
}

/**********************************************************************
* Bye says good bye
*/
void Bye()
{
printf("BER end - have an nice day\n");
}

/**********************************************************************
* Usage explains usage
*/
void Usage()
{
printf("usage:\n");
printf("\tber infileroot\n\n");
printf("\tinfileroot   root name of decoded raw data file, i.e. without\n");
printf("\t             the \".zero\" extention.\n");
}

/**********************************************************************
* DecodeArgs 
* decodes arguments (i.e. input file name) and puts them into the global
* structure
* returns 0 if an error is detected
*/
int DecodeArgs(argc,argv)
int argc;
char **argv;
{

/* argv[0] = "ber"		*/
/* argv[1] = infile root	*/
if (2 != argc)
	return 0;

CmdLine.pszInFile = argv[1];
return 1;
}

/**********************************************************************
* CalculateBitErrorRate
* openes the input file, scans it for ones and calculates the bit
* error rate. Then it prints the statistics to stdout.
*/
void CalculateBitErrorRate()
{
DRDFILE *zero;
char *buffer;
int i;
register ulong count;
ulong nBufLen;
ulong luImScNo;
ulong luBytesScaned = 0;
ulong luBitErrors = 0;

zero = zeroOpen(CmdLine.pszInFile);
if (NULL == zero) {
	printf("error opening file %s.zero\n",CmdLine.pszInFile);
	return;
	}

if (!zeroGotoFirstBlock(zero)) {
	printf("cannot find zero data in file %s.zero\n",CmdLine.pszInFile);
        return;
        }

do {
	nBufLen = zeroGetCurrentBlockLength(zero);
	buffer = malloc(nBufLen);
	if (! zeroGetCurrentBlock(zero,&luImScNo,buffer) ) {
		free(buffer);
		printf("error reading file %s.zero\n",CmdLine.pszInFile);
        	return;
        	}
	for (count = 0; count < nBufLen; count++) {
		luBytesScaned ++;
		/* if current byte not 0 count 1s in byte */
		if (buffer[count]) {
			for (i=0; i<8; i++) {
				if (buffer[count] & 1) luBitErrors ++;
				buffer[count] = buffer[count] >> 1;
				}
			}
		}
	free(buffer);
	}while ( zeroGotoNextBlock(zero) );

printf("FILE %s\n",zero->pszFileName);
printf("Scanned bytes: %lu\n",luBytesScaned);
printf("Scanned bits: %lu\n",luBytesScaned*8);
printf("False bits: %lu\n",luBitErrors);
printf("Bit error rate %le\n",(double)luBitErrors/((double)luBytesScaned*8.0));

zeroClose(zero);
}

