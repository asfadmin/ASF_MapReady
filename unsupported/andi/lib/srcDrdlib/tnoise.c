/* file to test library
*/
#include <stdio.h>
#include <malloc.h>
#include "drdapi.h"

char *apszComments[] = { "First comment line",
			"here is the second garabage line",
			"and another one",
			NULL
			};

main()
{
DRDFILE *noise1,*noise2;
uint count;
int bError;
char *pBuf;
char buf[40];
ulong luReadImNo;

drdInit("Testprogramm", 1, 0, "GI-ASF");
noise1 = noiseCreate("testfile","ThisIsTheID",
		"Sat around the world",apszComments);
noise2 = noiseCreate("test2","ThisIsAnotherID","XSAT",NULL);
for(count=1; count < 5; count++) {
	pBuf = malloc(10000*count);
	memset(pBuf,'1',10000*count);
	noiseWriteScan(noise1,(ulong)count,pBuf,count*10000);
	free(pBuf);
	
	sprintf(buf,"This is the scan No %d, %d\n",count,count*30);
	printf(buf);
	noiseWriteScan(noise2,(ulong)count,buf,strlen(buf)+1);
	}
noiseClose(noise2);
noiseClose(noise1);

printf("open to read\n");
noise2 = noiseOpen("test2");
noise1 = noiseOpen("testfile");

echoGotoLastScan(noise2); /* tset of error handling */
printf("scan foreward and read\n");
if (!noiseGotoFirstScan(noise2)) 
	exit();
do {
	pBuf = malloc(noiseGetCurrentScanLength(noise2));
	noiseGetCurrentScan(noise2,&luReadImNo,pBuf);
	printf("Scan %lu: %s",luReadImNo,pBuf);
	free(pBuf);
	} while( noiseGotoNextScan(noise2));
if(!noiseGotoFirstScan(noise1))
	exit();
do {
	pBuf = malloc(noiseGetCurrentScanLength(noise1));
	noiseGetCurrentScan(noise1,&luReadImNo,pBuf);
	bError = 0;
	for(count = 0; count < (uint)luReadImNo*10000; count++)
		bError = ('1' != pBuf[count]);
	printf("Read %u, Scan %lu, error = %d\n",
		noiseGetCurrentScanLength(noise1),luReadImNo,bError);
	free(pBuf);
	} while( noiseGotoNextScan(noise1));

printf("scan backwards and read\n");
if (!noiseGotoLastScan(noise2))
	exit();
do {
	pBuf = malloc(noiseGetCurrentScanLength(noise2));
        noiseGetCurrentScan(noise2,&luReadImNo,pBuf);
        printf("Scan %lu: %s",luReadImNo,pBuf);
        free(pBuf);
        } while( noiseGotoPrevScan(noise2));

if(!noiseGotoLastScan(noise1))
        exit();
do {
        pBuf = malloc(noiseGetCurrentScanLength(noise1));
        noiseGetCurrentScan(noise1,&luReadImNo,pBuf);
        bError = 0;
        for(count = 0; count < (uint)luReadImNo*10000; count++)
                bError = ('1' != pBuf[count]);
        printf("Read %u, Scan %lu, error = %d\n",
                noiseGetCurrentScanLength(noise1),luReadImNo,bError);
        free(pBuf);
        } while( noiseGotoPrevScan(noise1));

drdExit();
}

