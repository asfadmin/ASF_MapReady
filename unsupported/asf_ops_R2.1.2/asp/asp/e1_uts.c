/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* e1_uts.c - This module contains routines for
	sync detection and decoding of ERS-1
	format data.
*/

#include <stdio.h>
#include <procfil.h>
#include <procdec.h>
#include <sys/types.h>
#include <fcntl.h>

#define    FRAMES_PER_FMT 29
#define    E_INV_CODE 0x050cdf


extern float fmt_ratio;		/* ratio of IDHT formats to AMI ones */
extern DPP_FILE dpf;		/* df processing parameter file */
extern int vbose;		/* 1 = print debugging messages */

/*SONY*/
extern char media_type[];
extern char data_direction[];

extern RQST_PTR    Cur_Rqst;   /* Info for current request */
extern char chk_media_id[];



/* e1_match(word,bit_err) ----------------------------------------------
	This routine compares word with the ERS1 sync code, allowing 
	bit_err number of bit errors.  It returns the polarity of the 
	input data if a match is found:  1 = normal, 0 = inverted.
	The routine returns FAIL if a match is not found.
*/

int e1_match(word,bit_err)
	int *word,bit_err;
{
    /* Compare input to normal sync */
	if (bit_dif(*word,E_CODE,bit_err) <= bit_err)
	    return(1);    /* Matched normal sync */

    /* Compare input to inverted sync */
	if (bit_dif(*word,E_INV_CODE,bit_err) <= bit_err)
	    return(0);        /* Matched inverted sync */

	return(FAIL);
}



/* e1_comp(word) -------------------------------------------------------
	This routine compares word with the ERS1 sync code but does not
	allow any bit errors.  It returns the polarity of the input 
	word if a match is found:  1 = normal, 0 = inverted.
	The routine returns FAIL if a match is not found.
*/

int e1_comp(word)
	int *word;
{
    /* Compare input to non-inverted sync */

	if (*word == E_CODE)
	    return(1);

    /* Compare input to inverted sync */
	if (*word == E_INV_CODE)
	    return(0);

	return(FAIL);
}


/* bit_dif(c1,c2,max) --------------------------------------------------
	This routine compares two integer values and returns the number 
	of bits different between them.  If more than max bits are found
	different, the routine stops counting and returns a value
	greater than max.
*/

bit_dif(c1,c2,max)
	int c1,c2;
{
	register int dif, count;

	if ((dif = c1 ^ c2) == 0)
	    return (0);
    /* count first bit */
	count = dif & 1;
    /* strip off sign bit and count 2nd bit */
	count += (dif = (dif >> 1) & 0x7fffffff) & 1;
    /* count rest of bits (if any) */
	while ((dif >>= 1) && (count <= max))
	    count += dif & 1;
	return (count);
}



/* e1_loc_fmt(frame,blk,offset,fmt_blk,fmt_offset) ---------------------
	This routine calculates the position of the start
	of the next format given the start of any frame.  The
	arguments, fmt_blk and fmt_offset must be passed
	as addresses.
*/

e1_loc_fmt(frame,blk,offset,fmt_blk,fmt_offset)
	int frame, blk, offset;   /* Location of a frame */
	int *fmt_blk, *fmt_offset;   /* Location of format start */
{
	int    incr;

	if (frame == 0)
	{
	    *fmt_blk = blk;
	    *fmt_offset = offset;
	}
	else
	{
	    incr = ((FRAMES_PER_FMT - frame) * E_CODE_SEP) + offset;
	    *fmt_offset = incr % TAPE_BLOCK_LEN;
	    *fmt_blk = blk + (incr / TAPE_BLOCK_LEN);
	}
}



/* e1_srch_sync(nsyncs,bit_err,nblks,block,offset,norm_inv,iformat,
			ami_fmt, pri) ----------------------------
	This routine searches for ERS-1 syncs starting at the current
	tape location.  If syncs are found, the sync position and
	data polarity are returned in user-provided arguments, and the
	routine returns PASS.  If not, the routine returns FAIL.
	    nsyncs    - The number of consecutive syncs which consti-
			tute a find (maximum of 16).
	    bit_err   - The number of bit errors allowed in a sync
			NOTE: first sync must match exactly
	    nblks     - The number of tape blocks to search
	    block     - The tape search area starting block address

	The following are returned, indicating the position of the
	start of the first complete format in the search area (these
	must be passed as an address):
	    block     - Block address
	    offset    - The bit offset within the block
	    norm_inv  - The data polarity (1 = normal, 0 = inverted)
	    iformat   - The IDHT format # of the returned format
	    ami_fmt   - The AMI format # of the returned format
	    pri       - The PRI of the returned format

	NOTE: the input DCRS tape is assumed to be already positioned
	at the start block given by 'block'.

	Modification: 5/9/91,dtc - in order to skip over zero format
	data, the return will be fail so that the calling routine
	(e1_scan_take) will use a runtime value from df_proc_params
	file to skip a large number of blocks before trying to find
	SAR data.
*/

int e1_srch_sync (nsyncs,bit_err,nblks,block,offset,norm_inv,iformat,
			ami_fmt,pri)
	int nsyncs, bit_err, nblks;
	int *block, *offset, *norm_inv, *iformat, *ami_fmt, *pri;
{
#define BUFSIZE (2*ID1_BLOCK_LEN)
	char raw_dat[BUFSIZE+7];  /* Double blk buffer */
	int   i,k,n;
	int   shift;       /* Sync start bit within byte */
	int   match;       /* Indicates sync match */
	int   polarity;    /* Polarity of data (1=normal, 0=inverted) */
	int   start;       /* Start of search buffer */
	int   stop;        /* End of search buffer */
	int   bytes;       /* Size of search buffer */
	int   blkno;       /* Block currently being searched */
	int   zero_SAR;    /* flag for zero fmt or SAR data */
	int   frame;       /* Frame number of sync found */
	int   fmt;	   /* Format number at sync found */
	int   ans;	   /* Return value from function calls */
	int   zeroid, frmid, fmtid; /* pointer id's for best_val routine */
	int   fmt_blk, fmt_offset;  /* Pos of current format start */
	int   mbaddr = 0x10000;
	int   synccount;   /* Sync match attempts this block */
	int   sync_tmp;
/* for debugging
int ofd;
char fname[40] = "rpi";
*/
	union data_tag {
	    char b[8];
	    int  d[2];
	} data,work;

    /* Set up RPI board and DCRSi in play-back */

/*SONY*/
        if (!strcmp( media_type, "DCRSI" ) ) {
          if (start_byte_play(*block) == FAIL){
            dc_stop(IN);
            return (FAIL);
          }
        }
        else {
              if (!strcmp( data_direction, "REVERSE" ) ){
               if (sony_start_byte_play_REV(*block) == FAIL){
                 sony_stop(IN);
                 return (FAIL);
               }
              }
              else {
               if (sony_start_byte_play(*block) == FAIL){
                 sony_stop(IN);
                 return (FAIL);
               }
              }
             }
/* SONY
   check media_id
*/

/*
    if ((!strcmp(chk_media_id,"YES")) || (strcmp(media_type,"DCRSI"))) {
     if (sony_tape_mnt(IN,Cur_Rqst->tape_id) == FAIL)
         return(FAIL);
    }
*/

    if (!strcmp(chk_media_id,"YES")) {
      if (strcmp(media_type,"DCRSI")){
        if (sony_tape_mnt(IN,Cur_Rqst->tape_id) == FAIL)
           return(FAIL);
      }
/*
      else {
            if (tape_mnt(IN,Cur_Rqst->tape_id) == FAIL)
              return(FAIL);
      }
*/
    }


    /* initialize some loop variables and constants */
	if (nsyncs > 16)
	    nsyncs = 16;
	bytes = TAPE_BLOCK_LEN;
printf("TAPE_BLOCK_LEN = %d\n", bytes);
	blkno = 0;          /* Block to be searched */
	start = 0;
	if (nblks > 1000)
	    nblks = 10;

    /* Get first block of data */
/*
	if (get_rpi_blk(raw_dat) <= 0) {
	if (get_rpi_fifo(raw_dat, ID1_BLOCK_LEN) <= 0) {
*/
        if (get_rpi_fifo(raw_dat, TAPE_BLOCK_LEN >> 1) <= 0) {
	    printf("RPI unable to read 1st block\n");
/*SONY*/
            if( strcmp( media_type, "DCRSI" ) )
               sony_stop(IN);
            else
               dc_stop(IN);

	    return (FAIL);    /* Unable to read data block */
	}
/* for debugging
printf("Got rpi blk from tape at block %d \n",*block);
sprintf(fname,"rpi.%d",*block);
ofd = open( fname, O_CREAT | O_WRONLY, 0644 );
printf("open %d\n",ofd); fflush(stdout);
if ( ofd == FAIL ){
	perror(fname);
	return(FAIL);
}
printf("Writing raw data buff to rpi.dat\n"); fflush(stdout);
if ( write( ofd, raw_dat, TAPE_BLOCK_LEN ) != TAPE_BLOCK_LEN ){ 
	perror("write");
	return(FAIL);
}
close(ofd);
printf("Done\n");
*/
	putdata(raw_dat,mbaddr,TAPE_BLOCK_LEN/4);
	mbaddr += TAPE_BLOCK_LEN;

    /* Search for syncs in nblks number of tape blocks or */
    /*  until syncs found or until no data */
	data.b[3] = 0;
	data.b[4] = raw_dat[0];
	data.b[5] = raw_dat[1];
	data.b[6] = raw_dat[2];
	while (blkno < nblks)
	{
	    printf("searching block %d\n",blkno);
	/* Get next block into search buffer */
/*SONY
	    if (get_rpi_blk(&raw_dat[bytes - start]) <= 0) {
	    if (get_rpi_fifo(&raw_dat[bytes - start], ID1_BLOCK_LEN >>1) <= 0) {
*/
            if (get_rpi_fifo(&raw_dat[bytes - start], TAPE_BLOCK_LEN >> 1) <= 0) {
		printf("...failed to read block %d\n",blkno+1);
		break;
	    }
	    for (k = 0; k < 7; k++)
		raw_dat[BUFSIZE+k] = raw_dat[k];
	    putdata(&raw_dat[bytes - start],mbaddr,TAPE_BLOCK_LEN/4);
	    mbaddr += TAPE_BLOCK_LEN;
	    stop = start + bytes;
	    synccount = 0;
	/* check one block of data */
	    for (i = start; i < stop; i++)
	    {
		data.b[7] = raw_dat[i+3];

	    /* Find first sync word */
		for (shift = 0; shift < 8; shift++)
		{
		    sync_tmp = (ntohl(data.d[1]) >> 8) & 0xffffff;
		    polarity = e1_comp(&sync_tmp);
		    match = polarity;
		    n = 0;
/*
	if ( match != FAIL ) printf("Matching...%X\n",sync_tmp);
*/
		    if (match != FAIL)  /* Found possible first sync */
		    {
			synccount++;
			if (vbose && synccount == 1 && frame == 28) {
			    printf("Possible sync in blk %d at pos %d, ",
				blkno+*block,i%TAPE_BLOCK_LEN);
			    printf("shift=%d, polarity=%d\n",shift,polarity);
			}
		    /* get frame number, IDHT and AMI format number */
			work.b[0] = raw_dat[i+3];
			work.b[1] = raw_dat[i+4];
			work.b[2] = raw_dat[i+5];
			work.b[3] = raw_dat[i+6];
			work.d[0] = ntohl( ntohl(work.d[0]) << shift );
			/* M. Chen, 1/21/93 */
			frame = work.b[0] & 0x1f;
			if (polarity == 0)
			    frame = ~frame & 0x1f;
			if (frame != 0) {
			    synccount = 0;
			    match = FAIL;
			    break;
			}
			zeroid = init_best_val(nsyncs);
			frmid = init_best_val(nsyncs);
			fmtid = init_best_val(nsyncs);
			zero_SAR = (work.b[0] >> 6) & 0x3;
			frame = work.b[0] & 0x1f;
			fmt = (ntohl(work.d[0]) >> 8) & 0xffff;
			work.b[0] = raw_dat[i+25];
			work.b[1] = raw_dat[i+26];
			work.b[2] = raw_dat[i+27];
			work.b[3] = raw_dat[i+28];
			work.d[0] = ntohl( ntohl(work.d[0]) << shift );
			/* unscramble it for ami_fmt*/
			work.b[0] ^= 0xdb;
			work.b[1] ^= 0xdb;
			work.b[2] ^= 0xd2;
			*ami_fmt = (ntohl(work.d[0]) >> 8) & 0xffffff;
			work.b[0] = raw_dat[i+30];
			work.b[1] = raw_dat[i+31];
			work.b[2] = raw_dat[i+32];
			work.d[0] = ntohl( ntohl(work.d[0]) << shift );
			/* unscramble it for pri */
			work.b[0] ^= 0xbd;
			work.b[1] ^= 0x90;
			*pri = (ntohl(work.d[0]) >> 16) & 0xffff;
			if (polarity == 0) {
			    zero_SAR = ~zero_SAR & 0x3;
			    frame = ~frame & 0x1f;
			    fmt = ~fmt & 0xffff;
			    *ami_fmt = ~(*ami_fmt) & 0xffffff;
			    *pri = ~(*pri) & 0xffff;
			}
			printf("AMI = %d, PRI = %d\n", *ami_fmt, *pri);
			save_best_val(zeroid,zero_SAR);
			save_best_seq_val(frmid,frame,FRAMES_PER_FMT);
			save_best_val(fmtid,fmt);
			n = 1;      /* One sync found */
			break;
		    }  /* if match */
		    data.d[1] = ntohl( ntohl(data.d[1])<<1 );

		}  /* for shift */
	    /* If first sync found, find rest */
		k = i;
		while ((match != FAIL) && (n < nsyncs))
		{
		/* Pos of next sync */
		    k = (k + E_CODE_SEP) % BUFSIZE;
		/* Check next sync */
		    work.b[3] = 0;
		    work.b[4] = raw_dat[k];
		    work.b[5] = raw_dat[k+1];
		    work.b[6] = raw_dat[k+2];
		    work.b[7] = raw_dat[k+3];
      		    work.d[1] = ntohl( ntohl(work.d[1]) << shift ); 

		    sync_tmp = (ntohl(work.d[1]) >> 8) & 0xffffff;
		    if (polarity != (ans = e1_match(&sync_tmp,bit_err))) {
			match = FAIL;
			if (vbose && synccount == 1)
			    printf("  ...failed checking sync %d, ans=%d\n",n+1,ans);
		    }
		    else {   /* Next sync OK, get its frame number */
			work.b[0] = raw_dat[k+3];
			work.b[1] = raw_dat[k+4];
			work.b[2] = raw_dat[k+5];
			work.b[3] = raw_dat[k+6];
			work.d[0] = ntohl( ntohl(work.d[0]) << shift );
			zero_SAR = (work.b[0] >> 6) & 0x3;
			frame = work.b[0] & 0x1f;
			fmt = (ntohl(work.d[0]) >> 8) & 0xffff;
			if (polarity == 0) {
			    zero_SAR = ~zero_SAR & 0x3;
			    frame = ~frame & 0x1f;
			    fmt = ~fmt & 0xffff;
			}
			if (frame < n)
			    fmt = (fmt - 1) & 0xffff;
			save_best_val(zeroid,zero_SAR);
			save_best_seq_val(frmid,frame,FRAMES_PER_FMT);
			save_best_val(fmtid,fmt);
			n++;
		    }
		}  /* while match */

		if (match != FAIL)  /* All syncs found */
		{
		/* Get the best guess values for frame & format # */
		    zero_SAR = get_best_val(zeroid);
		    frame = get_best_val(frmid);
		    *iformat = get_best_val(fmtid);
		    if (frame != 0)
			(*iformat)++;
		    n = 0;
		    if (zero_SAR == 0x1) {
			match = FAIL;
			if (vbose && synccount == 1)
			    printf("  ...found zero format data.\n");
		    }
		    if (frame == FAIL) {
			match = FAIL;
			if (vbose && synccount == 1)
			    printf("  ...failed to find good frame #\n");
		    }
		}  /* if match (frame number check) */
		if (match != FAIL)  /* Everything checks OK */
		{
		/* Calculate position of start of format */
		    e1_loc_fmt (frame,*block+blkno,i-start,
				&fmt_blk,&fmt_offset);
		    *block = fmt_blk;
		    *offset = (fmt_offset << 3) + shift;
		    *norm_inv = polarity;
/*SONY*/
                    if (strcmp( media_type, "DCRSI" ) )
                       sony_stop(IN);
                    else
	    	       dc_stop(IN);

		    return (PASS);
		}  /* if match */
		if (n) {		/* reset best_val */
		    k = get_best_val(zeroid);
		    k = get_best_val(frmid);
		    k = get_best_val(fmtid);
		}
		data.b[3] = 0;
		data.b[4] = raw_dat[i+1];
		data.b[5] = raw_dat[i+2];
		data.b[6] = raw_dat[i+3];
	    }  /* for i */
	    start = bytes - start;
	    blkno++;
	}  /* while blkno */

	printf("Unable to find %d syncs\n",nsyncs);
/*SONY*/
        if (strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

	return (FAIL);
}



/* e1_get_fmt_loc(ifmt,afmt,sp,block,offset) ---------------------------
	This routine returns the block and offset location of the
	format given by afmt, an AMI format number.  The location is
	calculated relative to the start format, block and offset
	recorded in the segment record pointed to by sp.  If ifmt is
	positive, it is used to precisely locate the format.  Otherwise,
	the location returned is three formats in front of the best
	guess location of the desired format.
*/

e1_get_fmt_loc(ifmt,afmt,sp,block,offset)
	int ifmt,afmt,*block,*offset;
	TAPE_SEG_PTR sp;
{
	int    mv_fmt;     /* Number of formats to move */
	double new_byte;   /* Byte number to move to */
	double fmtsize = FRAMES_PER_FMT * E_CODE_SEP;  /* Bytes in 1 format */
	double blksize = TAPE_BLOCK_LEN;          /* Bytes in 1 block */
	short int f, f1;

    /* calculate rough distance from segment reference position */
	mv_fmt = (afmt - sp->fmt_id) * fmt_ratio;
    /* refine distance using idht format # (if present) */
	if (ifmt < 0)
	/* no idht fmt #: make sure we are in front of desired spot */
	    mv_fmt -= 3;
	else {
	/* use idht fmt # to find exact position */
	/*  this must be done in 16-bit math */
	    f1 = sp->fmt_start + mv_fmt;
	    f = ifmt - f1;
	    mv_fmt += f;
	}
    /* calculate # of bytes to new position */
/*SONY*/
if ((!strcmp( data_direction, "REVERSE" ) ) && (strcmp(media_type, "DCRSI")))
	new_byte = (sp->blk_start * blksize - (sp->bit_off >> 3)) +
		    (mv_fmt * fmtsize);
else
	new_byte= (sp->blk_start * blksize + (sp->bit_off >> 3)) +
		    (mv_fmt * fmtsize);

    /* turn bytes into block and offset address */
	*block = new_byte / blksize;
	*offset = new_byte - (*block * blksize);
	*offset = (*offset * 8) + (sp->bit_off & 7);
    /*
	printf(" message from get_fmt_loc, sp->fmt_id = %d, sp->fmt_start = %d\n",
					sp->fmt_id, sp->fmt_start);
	printf(" message from get_fmt_loc, sp->blk_start = %d, ifmt = %d\n",
					sp->blk_start,ifmt);
	printf(" message from get_fmt_loc, new_block = %d, mv_fmt = %d\n", *block, mv_fmt);
    */
}


/* e1_dump_aux_data(ap) ------------------------------------------
	This routine prints the contents of an aux data block
	on the screen.
*/

e1_dump_aux_data(ap)
	AUX_PTR ap;
{
	printf("Aux data dump:\n");
	printf("  ifmt = %d\n",ap->ifmt);
	printf("  obrc = %d\n",ap->obrc);
	printf("  time = %d\n",ap->time);
	printf("  task = %.2x\n",ap->task);
	printf("  sample = %.2x\n",ap->sample);
	printf("  format = %d\n",ap->format);
	printf("  window_start = %d\n",ap->window_start);
	printf("  pri = %d\n",ap->pri);
	printf("  cal_att = %d\n",ap->cal_att);
	printf("  rec_gain = %d\n",ap->rec_gain);
}
