 /* testprog for aux
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
DRDFILE *file1;
long x,y,dx,dy,dz;
ulong time,luImScNo;
double lfTmp;
int iTmp;

bDrdDebug = 0;
drdInit("taux",1,2,"ASF");
printf("taux started\n");
file1 = auxCreate("testfile","TheIdIs20","TESTSAT",apszComments);
printf("created\n");
auxWriteOrbit(file1,0,10,11,20,21,22,30);
auxWriteOrbit(file1,1,100,1001,200,2001,2002,3000);
auxWriteCurrentOrbitNo(file1,100,0);
auxWriteCurrentOrbitNo(file1,200,1);
auxWriteSampleWindowStartTime(file1,500,50.3445);
auxWritePulseRepetitionInterval(file1,600,10000.3);
auxWriteCalSubsysAttenuation(file1,400,10);
auxWriteRepSubsysAttenuation(file1,400,12);
drdClose(file1);
/* Read now */
file1 = auxOpen("testfile");
auxGetOrbit(file1,0,&x,&y,&dx,&dy,&dz,&time);
printf("read orbit: x = %ld, y = %ld, dx = %ld, dy = %ld, dz = %ld, t = %lu\n",
	x,y,dx,dy,dz,time);
auxGetCurrentOrbitData(file1,50,&x,&y,&dx,&dy,&dz,&time);
printf("orbit 50: x = %ld, y = %ld, dx = %ld, dy = %ld, dz = %ld, t = %lu\n",
        x,y,dx,dy,dz,time);
auxGetCurrentOrbitData(file1,150,&x,&y,&dx,&dy,&dz,&time);
printf("orbit 150: x = %ld, y = %ld, dx = %ld, dy = %ld, dz = %ld, t = %lu\n",
        x,y,dx,dy,dz,time);
auxGetCurrentOrbitData(file1,250,&x,&y,&dx,&dy,&dz,&time);
printf("orbit 250: x = %ld, y = %ld, dx = %ld, dy = %ld, dz = %ld, t = %lu\n",
        x,y,dx,dy,dz,time);

lfTmp = auxGetSampleWindowStartTime(file1,900);
printf("SampleWindowStartTime = %f\n",lfTmp);
lfTmp = auxGetPulseRepetitionInterval(file1,900);
printf("PulseRepetitionInterval = %f\n",lfTmp);

iTmp = auxGetRepSubsysAttenuation(file1,900);
printf("RepSubsysAttentuation =  %d\n",iTmp);
iTmp = auxGetCalSubsysAttenuation(file1,900);
printf("CalSubsysAttentuation = %d\n",iTmp);

drdExit();
exit();
}
