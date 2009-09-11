/* complex number def'n */
typedef struct {
	float r;
	float i;
} complex;

/************************
Filter sizes:
	These are expressed as powers of 2--
	dMx=3 -> filter 8-pixel chunk; write 4-pixel chunk;
	dMx=4 -> filter 16-pixel chunk; write 8-pixel chunk;
	dMx=5 -> filter 32-pixel chunk; write 16-pixel chunk;
	dMx=6 -> filter 64-pixel chunk; write 32-pixel chunk;
	dMx=7 -> filter 128-pixel chunk; write 64-pixel chunk;
*/

#define dMx 5
#define dMy 5

/*Size of filtered chunk.*/
#define dx (1<<dMx)
#define dy (1<<dMy)

/*Size of output chunk (requires 4 filtered chunks).*/
#define ox (1<<(dMx-1))
#define oy (1<<(dMy-1))


extern int nl,ns;/*Output number of lines and samples.*/

void blendData(complex **chunks,complex **last_chunks,
	float *weight,float *outBuf);
