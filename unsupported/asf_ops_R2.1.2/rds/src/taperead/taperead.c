/*=============================================================================
 |  %W% %E% %U%
 |
 |  DCRSI & SONY-ID1 Tape Reader Main.
 |  Alaska SAR Facility (ASF) Project.
 |
 |  Copyright (C) Jet Propulsion Laboratory.
 |
 *============================================================================*/
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/statfs.h>
#include <libgen.h>
#include <aio.h>
#include "tape.h"
#include "version.h"

extern int byte_order;

static char sccsid_taperead_c[] =
        "%W% %E% %U%";

static	char *usage 	= "usage: %s \
{dcrsi|sony} start_address last_address filename [-c 7|8] [-d directory ...]\n";

static struct timeval
    seek_timer, seek_time = {0, 0},
    read_timer, read_time = {0, 0};

#define start_timer(t0)         gettimeofday(t0)
#define stop_timer(t0,td)                               \
{                                                       \
    struct timeval t1;                                  \
    gettimeofday(&t1);                                  \
    (td)->tv_sec += t1.tv_sec  - (t0)->tv_sec;		\
    if (t1.tv_usec >= (t0)->tv_usec)			\
	(td)->tv_usec += t1.tv_usec - (t0)->tv_usec;    \
    else {						\
	(td)->tv_usec += t1.tv_usec - (t0)->tv_usec + 1000000;	\
	(td)->tv_sec  -= 1;				\
    }							\
}

int main (int argc, char *argv[])
{
    int n, i, len, start_blk, end_blk, BLOCK_SIZE, BLK_PER_FILE, BLK_PER_READ;
    int err = 0, tape, file = -1, filenum = 0, filelen = 0, csize = 8;
    int blocks, seek_dir;
    char outbase[256], outdir[256], *tape_name, *outfile=0, **dirs= 0;
    char *id, *buf, *dd[2] = {".", 0}, wait[32];
    id = ((id = strrchr(argv[0], '/')) ? id+1 : argv[0]);

    /*----------------------------------*
     |  Extract command-line arguments
     *----------------------------------*/

    if (argc < 4) {
	printf(usage, id);
	exit(-1);
    }
    if (strcasecmp(argv[1], "dcrsi") == 0) {
	BLOCK_SIZE = DCRSI_BLOCK_SIZE;
	BLK_PER_FILE = 492544;		/* 962*512 blocks < 2GB */
	BLK_PER_READ = 512;
    }
    else if (strcasecmp(argv[1], "sony") == 0) {
	BLOCK_SIZE = SONY_BLOCK_SIZE;
	BLK_PER_FILE = 14848;		/* 464*32 blocks < 2GB */
	BLK_PER_READ = 32;
    }
    else {
	printf("%s is not a valid tape device name\n", argv[1]);
	printf(usage, id);
	exit(-1);
    }
    tape_name = argv[1];

    if (sscanf(argv[2], "%d", &start_blk) != 1 || start_blk < 0) {
	printf("%s not a valid start block\n", argv[2]);
	printf(usage, id);
	exit(-1);
    }
    if (sscanf(argv[3], "%d", &end_blk) != 1 ||
	(BLOCK_SIZE == DCRSI_BLOCK_SIZE && end_blk < start_blk)) {
	printf("%s not a valid end block\n", argv[2]);
	printf(usage, id);
	exit(-1);
    }
    if (start_blk > end_blk) {
	seek_dir = 5;
	byte_order = 5;
	blocks = ((start_blk-end_blk+BLK_PER_READ)/BLK_PER_READ)*BLK_PER_READ;
	blocks = i * BLK_PER_READ;
	end_blk = start_blk - blocks + 1;
    }
    else {
	seek_dir = 1;
	byte_order = 1;
	blocks = ((end_blk-start_blk+BLK_PER_READ)/BLK_PER_READ)*BLK_PER_READ;
	end_blk = start_blk + blocks - 1;
    }
    for (i = 4; argv[i] != NULL; ++i) {
	if (*argv[i] != '-') {
	    if (outfile != NULL) {
		err = 1;
		break;
	    }
	    outfile = argv[i];
	}
	else if (strcmp(argv[i], "-c") == 0) {
	    if (argv[++i] == NULL) {
		printf("-c argument missing\n");
		err = 1;
		break;
	    }	
	    if (sscanf(argv[i], "%d", &csize) != 1 || csize<7 || csize>8) {
		printf("'%s' not a valid value\n", argv[i]);
		err = 1;
		break;
	    }
	}
	else if (strcmp(argv[i], "-d") == 0) {
	    if (dirs != NULL)
		err = 1;
	    else
		dirs = &argv[++i];
	    break;
	}
	else {
	    printf("%s not a valid option\n", argv[i]);
	    err = 1;
	    break;
	}
    }
    if (err) {
	printf(usage, id);
	exit(-1);
    }
    if (outfile) {
	strcpy(outbase, basename(outfile));		/* base filename */
	strcpy(dd[0] = outdir, dirname(outfile));	/* base directory */
/* 	printf("OUTBASE=%s OUTDIR=%s\n", outbase, outdir ); */
    }
    if (dirs == NULL)
	dirs = dd;

    /*------------------------*
     |  Allocate read buffer
     *------------------------*/

    if ((buf = memalign(512, BLK_PER_READ * BLOCK_SIZE)) == NULL) {
	printf("No memory for a %d-block buffer\n", BLK_PER_READ);
	exit(-1);
    }
/*  printf("Locking buffer in memory... %s\n",
 |  mpin(buf, BLK_PER_READ * BLOCK_SIZE) == -1 ? "FAILED" : "OK");
 */
    printf("Initializing %s...\n", tape_name);

    /*---------------------------------------------------*
     |  Open tape drive and seek to desired start block.
     *---------------------------------------------------*/

    start_timer(&seek_timer);

    tape = tape_open(tape_name, O_RDONLY, csize);
    if (tape == -1) exit(-1);
    printf("Seeking to block #%d...\n", start_blk);

    if (tape_seek(tape, start_blk, seek_dir) != start_blk) {
	printf("%s: can't seek to block %d\n", id, start_blk);
	tape_close(tape);
	exit(-1);
    }
    stop_timer(&seek_timer, &seek_time);

    /*-------------------------------------------------------------*
     |  Begin reading tape blocks and save results to output file.
     *-------------------------------------------------------------*/

    start_timer(&read_timer);

    for (i = 0; i < blocks; i += BLK_PER_READ) {
#ifndef DEBUG
	printf("#%d%c", (seek_dir==5) ? start_blk-i : start_blk+i,
		i+BLK_PER_READ >= blocks ? '\n' :
		((i/BLK_PER_READ)%8 == 7 ? '\n' : ' '));
#endif

	if (outfile && filelen < BLK_PER_READ) {
	    char *format, fn[256], ln[256];
	    struct statfs fs;
	    if (file != -1) close(file);

	    for (;;) {
		if (*dirs == NULL) {
		    printf("%s: insufficient space\n", id);
		    tape_close(tape);
		    exit(-1);
		}
		if (statfs(*dirs, &fs, sizeof(struct statfs), 0) == -1) {
		    perror("statfs");
		    tape_close(tape);
		    exit(-1);
		}
		/*  filelen should be a multiple of BLK_PER_READ */
		filelen = (int) (((double) fs.f_bsize * fs.f_bfree) / 
			  	 (BLK_PER_READ * BLOCK_SIZE)) * BLK_PER_READ
			- BLK_PER_READ;
		if (filelen > 0) {
		    if (filelen > BLK_PER_FILE)
			filelen = BLK_PER_FILE;
		    break;
		}	
		++dirs;
	    }
	    if (realpath(*dirs, fn) == NULL ||
		realpath(outdir, ln) == NULL) {
		perror("realpath");
		tape_close(tape);
		exit(-1);
	    }
	    format = (++filenum == 1 && blocks < BLK_PER_FILE &&
		      blocks < filelen) ? "/%s" : "/%s.%d";

	    sprintf(fn+strlen(fn), format, outbase, filenum);
	    if ((file = open(fn,O_WRONLY|O_CREAT|O_TRUNC|O_DIRECT,0644)) == -1){
		printf("%s: can't create output file %s\n", id, fn);
		tape_close(tape);
		exit(-1);
	    }
	    sprintf(ln+strlen(ln), format, outbase, filenum);
	    if (strcmp(fn, ln) && (unlink(ln), symlink(fn, ln)) == -1) {
		perror("symlink");
		tape_close(tape);
		exit(-1);
	    }
	}
	if (BLOCK_SIZE == DCRSI_BLOCK_SIZE) {
            static struct timeval wait_time = {0, 100000};
            n = select(0,0,0,0, &wait_time);
        
	    n = tape_read(tape, buf, BLK_PER_READ*BLOCK_SIZE);
	    if (n != BLK_PER_READ*BLOCK_SIZE) {
		printf("%s: read error, got %d of %d blocks: %s\n",
			id, n, BLK_PER_READ*BLOCK_SIZE, strerror(errno));
		tape_close(tape); 
		exit(-1);
	    }
	    if (outfile) {
		n = write(file, &buf[0], BLK_PER_READ*BLOCK_SIZE);
		if (n != BLK_PER_READ*BLOCK_SIZE) {
		    printf("%s: write error, %d of %d bytes written, %s\n",
			    id, n, BLK_PER_READ*BLOCK_SIZE, strerror(errno));
		    close(file);
		    tape_close(tape); 
		    exit(-1);
		}
	    }
	}
	else {
	    register int i, size = (BLK_PER_READ/2)*BLOCK_SIZE;
#ifdef	LIO
	    aiocb_t *list[2], aio[2] = {0};
	    list[0] = &aio[0];
	    list[0]->aio_lio_opcode = LIO_READ;
	    list[0]->aio_fildes = tape;
	    list[0]->aio_buf = &buf[size+256];
	    list[0]->aio_nbytes = size+256;

	    list[1] = &aio[1];
	    list[1]->aio_lio_opcode = LIO_READ;
	    list[1]->aio_fildes = tape;
	    list[1]->aio_buf = &buf[size-256];
	    list[1]->aio_nbytes = size-256;

	    if (lio_listio(LIO_WAIT, list, 2, 0) == -1) {
		printf("lio_listio failed, %s\n", strerror(errno));
		tape_close(tape);
		exit(-1);
	    }
	    for (i = 0; i < 2; ++i) {
		register int err;
		if ((err = aio_error(list[i])) != 0) {
		    printf("lio_listio #%d failed, %s\n", i, strerror(err));
		    tape_close(tape);
		    exit(-1);
		}
	    }
#else
	    n = tape_read(tape, &buf[0], size+256);
	    if (n != size+256) {	
		printf("%s: read error, got %d of %d bytes, %s\n",
			id, n, size+256, strerror(errno));
		tape_close(tape); 
		exit(-1);
	    }
	    n = tape_read(tape, &buf[size+256], size-256);
	    if (n != size-256) {	
		printf("%s: read error, got %d of %d bytes, %s\n",
			id, n, size-256, strerror(errno));
		tape_close(tape); 
		exit(-1);
	    }
#endif
	    if (outfile) {
		n = write(file, &buf[0], size+256);
		if (n != size+256) {
		    printf("%s: write error, %d of %d bytes written, %s\n",
			    id, n, size+256, strerror(errno));
		    close(file);
		    tape_close(tape); 
		    exit(-1);
		}
		n = write(file, &buf[size+256], size-256);
		if (n != size-256) {
		    printf("%s: write error, %d of %d bytes written, %s\n",
			    id, n, size-256, strerror(errno));
		    close(file);
		    tape_close(tape); 
		    exit(-1);
		}
	    }
	}
	filelen -= BLK_PER_READ;
    }
    stop_timer(&read_timer, &read_time);

    /*-----------------------------*
     |  Clean up and exit program.
     *-----------------------------*/

    tape_close(tape);
    if (outfile) {
	printf("%d blocks written to %d file(s) with basename \"%s\".\n",
		blocks, filenum, outbase);
	close(file);
    }
    printf("Seek time: %d.%d secs.\n", seek_time.tv_sec, seek_time.tv_usec);
    printf("%s time: %d.%d secs.\n", outfile ? " R/W" : "Read",
	    read_time.tv_sec, read_time.tv_usec);
    printf("%s rate: %e bytes/sec.\n", outfile ? " R/W" : "Read",
	   (double) (blocks) * BLOCK_SIZE /
	   (read_time.tv_sec + read_time.tv_usec/1.0E6));
}
