/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <errno.h>
#include <i_fdr.h>
#include <asp_shm.h>
#include <procfil.h>
#include <scene_file.h>
#define FAIL -1
#define PASS 0

extern RQST_PTR Cur_Rqst;       /* current job request */
extern int ap_rmlines;		/* actual number of range pixels */

int get_outb( fname, nlines, reclen, offset, hdr )
	char *fname;
	int nlines, reclen, offset;
	I_FDR_FILE *hdr;
{
	int pid, good_pix; 
	int status, ofd;
	int blocks, over, bp, binc, bsize, bstart;
	int rdlen, wrlen, dp, dinc, dreclen, recs, reccnt;
	int bufsize, len, total_rdlen, remain;
	int i, j;
	int seq_number;
	int dptr, zpix, nz, zs;
	unsigned short *sp;
	unsigned char *data[2], *cptr;

	printf("GET_OUTB: nlines=%d reclen=%d fname=%s\n",
			nlines, reclen, fname );

	cptr = (unsigned char *) hdr;
        ofd = open( fname, O_WRONLY, 0770 );
        if ( ofd == FAIL ) {
	    perror(fname);
	    return(FAIL);
	}

        data[0] = shm_data;
	data[1] = shm_data + BUFLEN;
	dptr = 0;

	dreclen = abs(reclen) + offset;
	bsize = nlines*abs(reclen);
	rdlen = BUFLEN;
	blocks = rdlen >> 9;
	if ( reclen < 0 ) {
	    wrlen = -reclen;		/* write 1 reclen per disk write */
	    bstart = BUFLEN + reclen;	/* start at end-1 record of buffer */
	    binc = reclen;		/* go backward in buffer */
	    recs = BUFLEN/wrlen;	/* iterate by records */
	    dinc = -dreclen*recs;	/* decrement by 1 buffer each time */
	    dp = (nlines+1)*dreclen;	/* seek to end of file */
	} else {
	    wrlen = reclen;		/* write 1 reclen each time */
	    bstart = 0;			/* start at beginning of buffer */
	    recs = BUFLEN/wrlen;	/* iterate by records */
	    lseek( ofd, dreclen, SEEK_SET );	/* seek past descriptor */
	}

/* Spawn sub-process to get data from outboard */
	shm_flags->rdb = 1;
	pid = fork();
	if ( pid == 0 ){
	   do {
		if ( (shm_flags->get_outb == 1) && (bsize > 0) ){
		   if ( rdlen > bsize ) blocks = bsize >> 9; 
	           printf("**** WARNING: NO CONTROL-C please.\n");
	           printf("if hung, \"cam_reset\" & wait 5 min. for time out.\n");
	   	   status = rdb( 0, blocks, data[dptr], 0, &over );
		   dptr = dptr ? 0 : 1;
		   bsize -= BUFLEN;
		   if ( status == FAIL ){
			shm_flags->get_outb = FAIL;
			shm_flags->rdb = FAIL;
		    } else shm_flags->rdb = 0;
		} else shm_flags->rdb = 0;
	   	while ( shm_flags->rdb==0 && (shm_flags->get_outb==1) ) 
				usleep(1000);
	   } while ( shm_flags->get_outb == 1 );
printf("GET_OUTB exiting, SHM_FLAGS->RDB=%d, SHM_FLAGS->GET_OUTB=%d\n",
shm_flags->rdb, shm_flags->get_outb );
	   exit();
	}

/*  Do the transfer loop  */

	good_pix = ap_rmlines;
	if( good_pix==8192 ) good_pix = 8191;
	zpix = wrlen>good_pix &&
		strcmp( Cur_Rqst->type, "CPX" ) !=0 &&
		strcmp( Cur_Rqst->type, "CSD" ) != 0;

printf("GET_OUTB: RDLEN=%d WRLEN=%d RECS=%d BUFSIZE=%d ZPIX=%d\n",
	rdlen, wrlen, recs, BUFLEN, zpix );
	reccnt = 0; total_rdlen = 0; remain = 0;
	while( bsize > 0 ){
/*	wait for rdb  */
	    while ( shm_flags->rdb == 1 ) usleep(1000);
	    if ( shm_flags->rdb == FAIL ) {
		printf("GET_OUTB encountered error from RDB\n");
		close(ofd);
		return(FAIL);
	    }
	    shm_flags->rdb = 1;

	    if ( rdlen > bsize ) rdlen = bsize;
	    if ( reclen < 0 ) dp += dinc;

	    if ( reclen > 0 )
		printf("GET_OUTB: SIZE=%d SEQ=%d\n", bsize, reccnt+1 );
	    else
		printf("GET_OUTB: SIZE=%d SEQ=%d\n", bsize, dp/dreclen);

	    if ( reclen < 0 ) {
		lseek( ofd, dp, SEEK_SET );
		seq_number = dp / dreclen;
	    } else seq_number = reccnt + 1;

	    bp = bstart;
	    bufsize = rdlen;
	    len = remain ? remain : wrlen;
	    while ( bufsize > 0 ){
		if ( len == wrlen ) {	/* CEOS header to start new record */
		    i = htonl( seq_number );
		    bcopy( &i, &cptr[12], 4 );
		    hdr->seq_number = ntohl( ++seq_number );
		    status = write( ofd, hdr, offset );
		    if ( status != offset ){
			perror("GET_OUTB Write ceos prefix failed");
			close(ofd);
			return(FAIL);
		    }
		    zs = good_pix;	/* zero pixel start */
		    if ( bufsize >= wrlen ){	/* write whole record */
			len = wrlen;
			nz = wrlen - good_pix;
		    } else {		/* write part of record */
			len = bufsize;
			if( len-good_pix>0 ) nz = len - good_pix;
			else nz = 0;	/* less than or equal to actual */
		    }
		} else {	/* remainder of previous record in new buffer */
		    len = wrlen - len;	/* remaining bytes */
		    if( len-wrlen+good_pix>0 ){	/* starts before fill */
			zs = len-wrlen+good_pix;
			nz = wrlen - good_pix;
		    } else {		/* starts at or after fill */
			zs = 0;
			nz = len;
		    }
		}
		if( strcmp( Cur_Rqst->type, "CPX" )==0 ){
		    sp = (unsigned short *) (data[dptr]+bp);
		    for( i=0; i<len/2; i++ ) sp[i] = htons( sp[i] );
		}
/* zero out invalid data area */
		if( zpix && nz ) bzero( data[dptr]+bp+zs, nz );
		
		status = write( ofd, data[dptr]+bp, len );
		if ( status != len ){
			perror("GET_OUTB data write failed");
			close(ofd);
			return(FAIL);
		}
		if ( reclen < 0 ) bp -= len;
		else bp += len;
		bufsize -= len;
		len = wrlen;
	    }	/*  end write loop */
	    bsize -= BUFLEN;
	    total_rdlen += rdlen;
	    remain = total_rdlen % wrlen;
	    reccnt = total_rdlen/wrlen;
	    if ( remain > 0 ) reccnt++;
	    dptr = dptr ? 0 : 1;
	}
	fsync( ofd );
	printf("Closing file %s\n",fname);
	close( ofd );
	printf("%s closed\n",fname);

	if ( reccnt != nlines ) {
		printf("GET_OUTB: Error %d records were written\n",
			reccnt );
		return(FAIL);
	} else {
		return(PASS);
	}
}
