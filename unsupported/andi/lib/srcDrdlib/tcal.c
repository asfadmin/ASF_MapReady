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
DRDFILE *cal1,*cal2;
uint count,ui;
int bError;
char *pBuf;
char buf[40];
ulong luReadImNo;

drdInit("Testprogramm", 1, 0, "GI-ASF");
cal1 = calCreate("testfile","IDIDID",
		"Sat around the world",apszComments);
cal2 = calCreate("test2","ThisIsAnotherID","XSAT",NULL);
for(count=1; count < 5; count++) {
	pBuf = malloc(10000*count);
	memset(pBuf,'1',10000*count);
	calWriteScan(cal1,(ulong)count,pBuf,count*10000);
	calWriteScan(cal1,(ulong)count,pBuf,count*10000);
	free(pBuf);
	
	sprintf(buf,"This is the scan No %d, %d\n",count,count*30);
	printf(buf);
	calWriteScan(cal2,(ulong)count,buf,strlen(buf)+1);
	}
calClose(cal2);
calClose(cal1);

printf("open to read\n");
cal2 = calOpen("test2");
cal1 = calOpen("testfile");
printf("scan foreward and read\n");
if (!calGotoFirstScan(cal2)) 
	exit();
do {
	pBuf = malloc(calGetCurrentScanLength(cal2));
	ui = calGetCurrentScan(cal2,&luReadImNo,pBuf);
	printf("Scan %lu No %u: %s",luReadImNo,ui,pBuf);
	free(pBuf);
	} while( calGotoNextScan(cal2));

if(!calGotoFirstScan(cal1))
	exit();
do {
	pBuf = malloc(calGetCurrentScanLength(cal1));
	ui = calGetCurrentScan(cal1,&luReadImNo,pBuf);
	bError = 0;
	for(count = 0; count < (uint)luReadImNo*10000; count++)
		bError = ('1' != pBuf[count]);
	printf("Read %u, Scan %lu No %u, error = %d\n",
		calGetCurrentScanLength(cal1),luReadImNo,ui,bError);
	free(pBuf);
	} while( calGotoNextScan(cal1));

printf("scan backwards and read\n");
if (!calGotoLastScan(cal2))
	exit();
do {
	pBuf = malloc(calGetCurrentScanLength(cal2));
        ui = calGetCurrentScan(cal2,&luReadImNo,pBuf);
        printf("Scan %lu No %u: %s",luReadImNo,ui,pBuf);
        free(pBuf);
        } while( calGotoPrevScan(cal2));

if(!calGotoLastScan(cal1))
        exit();
do {
        pBuf = malloc(calGetCurrentScanLength(cal1));
        ui = calGetCurrentScan(cal1,&luReadImNo,pBuf);
        bError = 0;
        for(count = 0; count < (uint)luReadImNo*10000; count++)
                bError = ('1' != pBuf[count]);
        printf("Read %u, Scan %lu # %u, error = %d\n",
                calGetCurrentScanLength(cal1),luReadImNo,ui,bError);
        free(pBuf);
        } while( calGotoPrevScan(cal1));

drdExit();
}

