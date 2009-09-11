/*****************************************
lzFetch:
	A collection of utilies for extracting various
data from VEXCEL Level-Zero Granule and Parameter files.

	This format is a simple, ASCII-based line-oriented
format.  The basic structure is:
<field>: <value1>
<struct> {
	<subField>: <subValue>
}


	Note that these routines allocate their
return strings in the heap, so their strings must
be free'd afterward, or memory will leak.
*/

double lzDouble(char *granN,char *desiredParam,int *err);
int lzInt(char *granN,char *desiredParam,int *err);
char * lzStr(char *granN,char *desiredParam,int *err);
