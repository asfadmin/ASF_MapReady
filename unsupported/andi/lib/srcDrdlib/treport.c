/* testfile for DRD report
*/

#include <stdio.h>
#include <malloc.h>
#include "drdapi.h"
 
main()
{
DRDFILE *file1;

bDrdDebug = 0;
drdInit("taux",1,2,"ASF");
file1 = reportCreate("testfile","TheIdIs20","TESTSAT",NULL,1);
reportWrite(file1,"This is the first line for the report"); 
reportWrite(file1,"Another %d,%d one special for Jason",3,5,44,1,3);
reportWrite(file1,"One for me");
reportWrite(file1,"Writing just garbage");
reportWrite(file1,"Lets think for a minute about all the waisted time");
reportWrite(file1,"for writing this library");
reportClose(file1);

drdExit();
exit();
}
