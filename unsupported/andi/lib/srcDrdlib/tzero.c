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
DRDFILE *zero1,*zero2;
uint count;
int bError;
char *pBuf;
char buf[40];
ulong luReadImNo,lu;

drdInit("Testprogramm", 1, 0, "GI-ASF");
zero1 = zeroCreate("testfile","ThisIsTheID",
		"Sat around the world",apszComments);
zero2 = zeroCreate("test2","ThisIsAnotherID","XSAT",NULL);

for(count=1; count < 5; count++) {
	pBuf = malloc(10000*count);
	memset(pBuf,'1',10000*count);
	zeroWriteBlock(zero1,(ulong)count,pBuf,count*10000);
	/* sometimes write it double to test block extention */
	if (count%2)
		zeroWriteBlock(zero1,(ulong)count,pBuf,count*10000);
	free(pBuf);
	
	sprintf(buf,"This is the scan No %d, %d\n",count,count*30);
	printf(buf);
	zeroWriteBlock(zero2,(ulong)count,buf,strlen(buf)+1);
	}
zeroClose(zero2);
zeroClose(zero1);

printf("open to read\n");
zero2 = zeroOpen("test2");
zero1 = zeroOpen("testfile");

printf("scan foreward and read\n");
if (!zeroGotoFirstBlock(zero2)) 
	exit();
do {
	pBuf = malloc(zeroGetCurrentBlockLength(zero2));
	zeroGetCurrentBlock(zero2,&luReadImNo,pBuf);
	printf("Scan %lu: %s",luReadImNo,pBuf);
	free(pBuf);
	} while( zeroGotoNextBlock(zero2));
if(!zeroGotoFirstBlock(zero1))
	exit();
do {
	pBuf = malloc(zeroGetCurrentBlockLength(zero1));
	zeroGetCurrentBlock(zero1,&luReadImNo,pBuf);
	bError = 0;
	lu = luReadImNo * ((luReadImNo % 2) ? 2 : 1);
	for(count=0;count< (uint)lu*10000;count++)
		bError = ('1' != pBuf[count]);
	if (lu*10000 != zeroGetCurrentBlockLength(zero1)) 
		printf("Bad Length\n");
	printf("Read %u, block %lu, error = %d\n",
		zeroGetCurrentBlockLength(zero1),luReadImNo,bError);
	free(pBuf);
	} while( zeroGotoNextBlock(zero1));

printf("scan backwards and read\n");
if (!zeroGotoLastBlock(zero2))
	exit();
do {
	pBuf = malloc(zeroGetCurrentBlockLength(zero2));
        zeroGetCurrentBlock(zero2,&luReadImNo,pBuf);
        printf("Scan %lu: %s",luReadImNo,pBuf);
        free(pBuf);
        } while( zeroGotoPrevBlock(zero2));

if(!zeroGotoLastBlock(zero1))
        exit();
do {
	pBuf = malloc(zeroGetCurrentBlockLength(zero1));
        zeroGetCurrentBlock(zero1,&luReadImNo,pBuf);
        bError = 0;
        lu = luReadImNo * ((luReadImNo % 2) ? 2 : 1);
        for(count=0;count< (uint)lu*10000;count++)
                bError = ('1' != pBuf[count]);
	printf("tested %lu\n",lu*10000);
        if (lu*10000 != zeroGetCurrentBlockLength(zero1)) 
                printf("Bad Length\n");
        printf("Read %u, Scan %lu, error = %d\n",
                zeroGetCurrentBlockLength(zero1),luReadImNo,bError);
        free(pBuf);
        } while( zeroGotoPrevBlock(zero1));


drdExit();
}

