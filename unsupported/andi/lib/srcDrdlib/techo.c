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
DRDFILE *echo1,*echo2;
uint count;
int bError;
char *pBuf;
char buf[40];
ulong luReadImNo;

drdInit("Testprogramm", 1, 0, "GI-ASF");
echo1 = echoCreate("testfile","ThisIsTheID",
		"Sat around the world",apszComments);
echo2 = echoCreate("test2","ThisIsAnotherID","XSAT",NULL);
for(count=1; count < 5; count++) {
	pBuf = malloc(10000*count);
	memset(pBuf,'1',10000*count);
	echoWriteScan(echo1,(ulong)count,pBuf,count*10000);
	free(pBuf);
	
	sprintf(buf,"This is the scan No %d, %d\n",count,count*30);
	printf(buf);
	echoWriteScan(echo2,(ulong)count,buf,strlen(buf)+1);
	}
echoClose(echo2);
echoClose(echo1);

printf("open to read\n");
echo2 = echoOpen("test2");
echo1 = echoOpen("testfile");

printf("scan foreward and read\n");
noiseGotoFirstScan(echo2);
if (!echoGotoFirstScan(echo2)) 
	exit();
do {
	pBuf = malloc(echoGetCurrentScanLength(echo2));
	echoGetCurrentScan(echo2,&luReadImNo,pBuf);
	printf("Scan %lu: %s",luReadImNo,pBuf);
	free(pBuf);
	} while( echoGotoNextScan(echo2));
if(!echoGotoFirstScan(echo1))
	exit();
do {
	pBuf = malloc(echoGetCurrentScanLength(echo1));
	echoGetCurrentScan(echo1,&luReadImNo,pBuf);
	bError = 0;
	for(count = 0; count < (uint)luReadImNo*10000; count++)
		bError = ('1' != pBuf[count]);
	printf("Read %u, Scan %lu, error = %d\n",
		echoGetCurrentScanLength(echo1),luReadImNo,bError);
	free(pBuf);
	} while( echoGotoNextScan(echo1));

printf("scan backwards and read\n");
if (!echoGotoLastScan(echo2))
	exit();
do {
	pBuf = malloc(echoGetCurrentScanLength(echo2));
        echoGetCurrentScan(echo2,&luReadImNo,pBuf);
        printf("Scan %lu: %s",luReadImNo,pBuf);
        free(pBuf);
        } while( echoGotoPrevScan(echo2));

if(!echoGotoLastScan(echo1))
        exit();
do {
        pBuf = malloc(echoGetCurrentScanLength(echo1));
        echoGetCurrentScan(echo1,&luReadImNo,pBuf);
        bError = 0;
        for(count = 0; count < (uint)luReadImNo*10000; count++)
                bError = ('1' != pBuf[count]);
        printf("Read %u, Scan %lu, error = %d\n",
                echoGetCurrentScanLength(echo1),luReadImNo,bError);
        free(pBuf);
        } while( echoGotoPrevScan(echo1));


printf("random 2 and 4\n");
if (!echoFindScan(echo2,4L) )
	exit();
pBuf = malloc(echoGetCurrentScanLength(echo2));
echoGetCurrentScan(echo2,&luReadImNo,pBuf);
printf("Scan 4 is %ld: %s",luReadImNo,pBuf);
free(pBuf);

if(!echoFindScan(echo1,4L) )
	exit();
pBuf = malloc(echoGetCurrentScanLength(echo1));
echoGetCurrentScan(echo1,&luReadImNo,pBuf);
bError = 0;
for(count = 0; count < (uint)luReadImNo*10000; count++)
       bError = ('1' != pBuf[count]);
printf("Scan 4 is %ld: error = %d\n",luReadImNo,bError);

if (!echoFindScan(echo2,2L) )
        exit();
pBuf = malloc(echoGetCurrentScanLength(echo2));
echoGetCurrentScan(echo2,&luReadImNo,pBuf);
printf("Scan 2 is %ld: %s",luReadImNo,pBuf);
free(pBuf);

if(!echoFindScan(echo1,2L) )
        exit();
pBuf = malloc(echoGetCurrentScanLength(echo1));
echoGetCurrentScan(echo1,&luReadImNo,pBuf);
bError = 0;
for(count = 0; count < (uint)luReadImNo*10000; count++)
       bError = ('1' != pBuf[count]);
printf("Scan 2 is %ld: error = %d\n",luReadImNo,bError);

drdExit();
}

