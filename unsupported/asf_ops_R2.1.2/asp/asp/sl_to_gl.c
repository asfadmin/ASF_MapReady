/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <math.h>

#define FAIL -1
#define CEOS_PREFIX_SIZE 12
void SrlineToGrline ();

int sl_to_gl ( fname, reclen, nrecs, factor )
	char *fname;
	int reclen, nrecs;
	float factor;
{
	int ofd, status;
	unsigned char *data;
	float fac, del_fac;
	int i;

	if ( factor == 1.0 ) return(0);
	if ( (factor < 0.0) || (factor > 2.0) ) {
		printf("factor %f is not in range (0.0-2.0)\n", factor);
		return(FAIL);
	} else del_fac = ( factor - 1.0 ) / nrecs;

	printf("Converting slant-range data to ground-range data.\n");
	data = (unsigned char *) malloc(reclen);
	ofd = open( fname, O_RDWR, 0770 ); 
	lseek( ofd, reclen+CEOS_PREFIX_SIZE, SEEK_CUR );
	for ( i = 0, fac = 1.0; i < nrecs; i++, fac += del_fac ){
	   lseek( ofd, CEOS_PREFIX_SIZE, SEEK_CUR );
	   if ( (status = read( ofd, data, reclen )) != reclen ){
		close(ofd);
		free(data);
		return(FAIL);
	   }
	   SrlineToGrline( data, reclen, fac ); 	
	   lseek( ofd, -reclen, SEEK_CUR );
	   if ( (status = write( ofd, data, reclen )) != reclen ){
		close(ofd);
		free(data);
		return(FAIL);
	   }
	}
	close(ofd);
	free(data);

}
void SrlineToGrline ( data, reclen, factor )
	unsigned char *data;
	int reclen;
	float factor;
{
	unsigned char *in;
	int newreclen;
	int st_pix, end_pix;
	int i;
	float ratio;

	if ( factor != 1.0 ){
		in = (unsigned char *)malloc(reclen);
		memcpy( in, data, reclen );
		memset( data, 0, reclen );
		newreclen = factor * reclen;
		st_pix = abs(newreclen - reclen) / 2;
		end_pix = reclen - st_pix - 1;
		ratio = (float)(end_pix-st_pix+1) / reclen;
		if ( factor > 1.0 ) {
			for ( i = 0; i < reclen; i++ ){
			   data[i] = in[ (int) (st_pix + i*ratio) ];
			}
		} else {
			for ( i = 0; st_pix <= end_pix; i++ ){
			   data[st_pix++] = in[ (int) nint( i/ratio ) ];
			}
		}
		free( in );
	}
}
