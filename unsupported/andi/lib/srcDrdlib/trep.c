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
DRDFILE *rep1,*rep2;
uint count;
int bError;
char *pBuf;
char buf[40];
ulong luReadImNo;

drdInit("Testprogramm", 1, 0, "GI-ASF");
rep1 = repCreate("testfile","IDIDID",
		"Sat around the world",apszComments);
rep2 = repCreate("test2","ThisIsAnotherID","XSAT",NULL);
for(count=1; count < 5; count++) {
	pBuf = malloc(10000*count);
	memset(pBuf,'1',10000*count);
	repWriteScan(rep1,(ulong)count,pBuf,count*10000);
	repWriteScan(rep1,(ulong)count,pBuf,count*10000);
	free(pBuf);
	
	sprintf(buf,"This is the scan No %d, %d\n",count,count*30);
	printf(buf);
	repWriteScan(rep2,(ulong)count,buf,strlen(buf)+1);
	}
repClose(rep2);
repClose(rep1);

printf("open to read\n");
rep2 = repOpen("test2");
rep1 = repOpen("testfile");
printf("scan foreward and read\n");
if (!repGotoFirstScan(rep2)) 
	exit();
do {
	pBuf = malloc(repGetCurrentScanLength(rep2));
	repGetCurrentScan(rep2,&luReadImNo,pBuf);
	printf("Scan %lu: %s",luReadImNo,pBuf);
	free(pBuf);
	} while( repGotoNextScan(rep2));

if(!repGotoFirstScan(rep1))
	exit();
do {
	pBuf = malloc(repGetCurrentScanLength(rep1));
	repGetCurrentScan(rep1,&luReadImNo,pBuf);
	bError = 0;
	for(count = 0; count < (uint)luReadImNo*10000; count++)
		bError = ('1' != pBuf[count]);
	printf("Read %u, Scan %lu, error = %d\n",
		repGetCurrentScanLength(rep1),luReadImNo,bError);
	free(pBuf);
	} while( repGotoNextScan(rep1));

printf("scan backwards and read\n");
if (!repGotoLastScan(rep2))
	exit();
do {
	pBuf = malloc(repGetCurrentScanLength(rep2));
        repGetCurrentScan(rep2,&luReadImNo,pBuf);
        printf("Scan %lu :%s",luReadImNo,pBuf);
        free(pBuf);
        } while( repGotoPrevScan(rep2));

if(!repGotoLastScan(rep1))
        exit();
do {
        pBuf = malloc(repGetCurrentScanLength(rep1));
        repGetCurrentScan(rep1,&luReadImNo,pBuf);
        bError = 0;
        for(count = 0; count < (uint)luReadImNo*10000; count++)
                bError = ('1' != pBuf[count]);
        printf("Read %u, Scan %lu, error = %d\n",
                repGetCurrentScanLength(rep1),luReadImNo,bError);
        free(pBuf);
        } while( repGotoPrevScan(rep1));

drdExit();
}

