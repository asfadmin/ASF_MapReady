/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* j1_uts.c - This module contains routines for
	sync detection and decoding of JERS-1
	format data.

   CLW 12/18/95 - Modify j1_srch_sync to work on ALPHA with VME interface
*/

#include <stdio.h>
#include <math.h>
#include <procfil.h>
#include <procdec.h>
#include <netinet/in.h>

#define	   BUFSIZE (ID1_BLOCK_LEN*2)

extern int vbose;
/*SONY*/
extern char media_type[];
extern char data_direction[];

extern RQST_PTR    Cur_Rqst;   /* Info for current request */
extern char chk_media_id[];

char raw_dat[BUFSIZE+32];


/* j1_match(word1, word2, bit_err, shift, polarity, extra_off) ------
	This routine compares word with the JERS1 sync code, subject 
	up to 3 bits of extra shift in either direction, allowing 
	bit_err number of bit errors.  It returns the polarity of the 
	input data if a match is found:  1 = normal, 0 = inverted.
	The routine returns FAIL if a match is not found.
*/

int j1_match(word1, word2, bit_err, shift, polarity, extra_off)
	int *word1, *word2, bit_err, shift, polarity, *extra_off;
{
    	int sync1, sync2, i;

   	if (shift != 0)
    *word1 = ((*word1 << shift) & (0xfffffffe << (shift-1))) |
	    	((*word2 >> (32-shift)) & (0x7fffffff >> (31-shift)));
    *word1 &= 0x00ffffff; /* use only 3 bytes */
    *word2 <<= shift;
    *word2 &= 0xfffff000; /* use only 2.5 bytes */
    /* Compare input to sync */
    for (i = 0; i < 7; i++) {
	sync1 = ((J_CODE1 << i) | (J_CODE2 >> (32-i))) & 0x00ffffff;
	sync2 = (J_CODE2 << i) & 0xfffff000;
	if (polarity == 1) {
	    if (bit_dif(*word1,sync1,bit_err) <= bit_err
		&& bit_dif(*word2,sync2,bit_err) <= bit_err) {
		*extra_off = -i;
/*		printf("off = %d\n", *extra_off); */
		return(1);	/* Matched nprmal sync */
	    }
	}
	else {
	    if (bit_dif((~*word1)&0x00ffffff,sync1,bit_err) <= bit_err
		&& bit_dif((~*word2)&0xfffff000,sync2,bit_err) <= bit_err) {
		*extra_off = -i;
		return(0);	/* Matched inversed sync */
	    }
	}
	sync1 = (J_CODE1 >> i) & 0x00ffffff;
	sync2 = ((J_CODE1 << (32-i)) | (J_CODE2 >> i)) & 0xfffff000;
	if (polarity == 1) {
	    if (bit_dif(*word1,sync1,bit_err) <= bit_err
		&& bit_dif(*word2,sync2,bit_err) <= bit_err) {
		*extra_off = i;
/*		printf("off = %d\n", *extra_off); */
		return(1);	/* Matched nprmal sync */
	    }
	}
	else {
	    if (bit_dif((~*word1)&0x00ffffff,sync1,bit_err) <= bit_err
		&& bit_dif((~*word2)&0xfffff000,sync2,bit_err) <= bit_err) {
		*extra_off = i;
		return(0);	/* Matched inversed sync */
	    }
	}
    }
    return(FAIL);
}



/* j1_comp(word1, word2) -----------------------------------
	This routine compares word with the JERS1 sync code but does not
	allow any bit errors.  It returns the polarity of the input 
	word if a match is found:  1 = normal, 0 = inverted.
	The routine returns FAIL if a match is not found.
*/

int j1_comp(word1, word2)
	int *word1, *word2;
{
	int temp2;

	temp2 = *word2 & 0xffffff00;
    /* Compare input to non-inverted sync */
	if (*word1 == J_CODE1 && temp2 == (J_CODE2 & 0xffffff00))
	    return(1);

    /* Compare input to inverted sync */
	if (*word1 == J_INV_CODE1 && temp2 ==
				(J_INV_CODE2 & 0xffffff00))
	    return(0);

	return(FAIL);
}



/* j1_get_prf(word, shift, polarity) --------------------

	This routine pulls out JERS1 prf value from interleaved 
	HK data.
*/

j1_get_prf(word, shift, polarity)
	int word, shift, polarity;

{
	int i, prf;

	prf = 0;
/*	printf("word = %x\n", word); */
	word = ((word << shift) >> 24) & 0xff;
/*	printf("word = %x\n", word); */
	if ((polarity == 1 && (word >> 6) != 2) ||
		(polarity == 0 && (word >> 6) != 1)) {
	    if (vbose) printf("PRF OFF ...\n");
	    return(0);
	}
	for (i = 0; i < 3; i++) {
	    if ((word & 0x3) != 2 && (word & 0x3) != 1) {
		printf("Suspicious interleaved prf ...\n");
		return(0);
	    }
	    if (polarity == 1)
	        prf += pow(2.,(double)i)*((word & 0x3) - 1);
	    else
		prf += pow(2.,(double)i)*(-1*(word & 0x3) + 2);
	    word >>= 2;
	}
	return(prf);
}



/* j1_get_fmt(word1, word2, shift, polarity) ------------

	This routine pulls out frame number from interleaved
	frame counter.
*/

j1_get_fmt(word1, word2, shift, polarity)
	int word1, word2, shift, polarity;
{
	int i,j,count;

	j = 16 - shift;
	count = 0;
/*	printf("DBG: j1_get_fmt: word1=%x, word2=%x\n",word1,word2); */
	if (j > 0) {
	    word2 = ((word2 >> j) & (0x7fffffff >> (j-1))) |
		     ((word1 << (32-j)) & (0xfffffffe << (31-j)));
	    word1 = ((word1 << shift) >> 16) & 0xffff;
/*	    printf("DBG: j1_get_fmt: word1=%x, word2=%x\n",word1,word2); */
	}
	for (i = 0; i < 24; i++) {
	  if (i < 16) {
	    if ((word2 & 0x3) != 2 && (word2 & 0x3) != 1) {
		printf("Suspicious interleaved number ...\n");
		return(-1);
	    }
	    if (polarity == 1)
	        count += pow(2.,(double)i)*((word2 & 0x3) - 1);
	    else
		count += pow(2.,(double)i)*(-1*(word2 & 0x3) + 2);
	    word2 >>=2;
	  }
	  else {
	    if ((word1 & 0x3) != 2 && (word1 & 0x3) != 1) {
		printf("Suspicious interleaved number ...\n");
		return(-1);
	    }
	    if (polarity == 1)
	        count += pow(2.,(double)i)*((word1 & 0x3) - 1);
	    else
		count += pow(2.,(double)i)*(-1*(word1 & 0x3) + 2);
	    word1 >>= 2;
	  }
	}
	return(count);
}



/* j1_srch_sync(nsyncs,bit_err,nblks,block,offset,norm_inv, --------
		iformat,prf,length)

	This routine searches for JERS-1 syncs starting at the current
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
	    iformat   - The format # of the returned format

	NOTE: the input DCRS tape is assumed to be already positioned
	at the start block given by 'block'.

	Modification: 5/9/91,dtc - in order to skip over zero format
	data, the return will be fail so that the calling routine
	(j1_scan_take) will use a runtime value from df_proc_params
	file to skip a large number of blocks before trying to find
	SAR data.??????
*/

int j1_srch_sync (nsyncs,bit_err,nblks,block,offset,norm_inv,
		  iformat,prf,length)
	int nsyncs, bit_err, nblks;
	int *block, *offset, *norm_inv, *iformat, *length;
	float *prf;
{
	int   i,j,k,n,i_off,byte_off,bit_off,shift1;
	int   blkno_1, no_byte, prfhk, extra_off, fmt_1;
	int   shift;       /* Sync start bit within byte */
	int   match;       /* Indicates sync match */
	int   polarity;    /* Polarity of data (1=normal, 0=inverted) */
	int   start;       /* Start of search buffer */
	int   stop;        /* End of search buffer */
	int   bytes;       /* Size of search buffer */
	int   blkno;       /* Block currently being searched */
	int   fmt;	   /* Format number at sync found */
	int   mbaddr = 0x10000;
	int   synccount;   /* Sync match attempts this block */
	static float j1_prfs[6] = {1505.8,1530.1,1555.2,1581.1,
				1606.0,1646.75};
			   /* the five prf values for JERS-1 */
	static int j1_code_sep[6] = {39844,39212,38580,37948,
				37358,37648};
			   /* average format length based on prf */

	union data_tag {
	    char b[8];
	    int  d[2];
	} data,work;
	int sync_tmp[2];

/*SONY*/
        int BLKSIZE;
  
        BLKSIZE = TAPE_BLOCK_LEN;

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
                 sony_stop(IN);
               if (sony_start_byte_play_REV(*block) == FAIL){
                 sony_stop(IN);
                 return (FAIL);
               }
              }
              else {
                 sony_stop(IN);
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
	if (nsyncs > 2)
	    nsyncs = 2;
	bytes = TAPE_BLOCK_LEN;
printf("TAPE_BLOCK_LEN = %d\n", bytes);
	blkno = 0;          /* Block to be searched */
	start = 0;
	if (nblks > 1000)
	    nblks = 10;

    /* Get first block of data */
/*SONY
	if (get_rpi_blk(raw_dat) <= 0) {
	if (get_rpi_fifo(raw_dat, ID1_BLOCK_LEN >> 1) <= 0) {
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
    /*
	putdata(raw_dat,mbaddr,BLKSIZE/4);
	mbaddr += BLKSIZE;
    */

    /* Search for syncs in nblks number of tape blocks or */
    /*  until syncs found or until no data */
	for (j = 0; j < 8; j++) 
		data.b[j] = raw_dat[j];

	while (blkno < nblks) {
	    printf("searching block %d\n",blkno);
	/* Get next block into search buffer */
/*SONY
	    if (get_rpi_blk(&raw_dat[bytes - start]) <= 0) {
	    if (get_rpi_fifo(&raw_dat[bytes - start], ID1_BLOCK_LEN) <= 0) {
*/
            if (get_rpi_fifo(&raw_dat[bytes - start], TAPE_BLOCK_LEN >> 1) <= 0){
		printf("...failed to read block %d\n",blkno+1);
		break;
	    }
	    for (j = 0; j < 32; j++)
		raw_dat[BUFSIZE+j] = raw_dat[j];
	/*
	    putdata(&raw_dat[bytes - start],mbaddr,BLKSIZE/4);
	    mbaddr += BLKSIZE;
	*/
	    stop = start + bytes;
	    synccount = 0;
	/* check one block of data */
	    for (i = start; i < stop; i++)
	    {
		data.b[7] = raw_dat[i+7];
	    /* Find first sync word */
		for (shift = 0; shift < 8; shift++)
		{
		    sync_tmp[0] = ntohl( data.d[0] );
		    sync_tmp[1] = ntohl( data.d[1] );
		    polarity = j1_comp(&sync_tmp[0], &sync_tmp[1]);
		    match = polarity;
		    n = 0;
		    if (match != FAIL)  /* Found possible first sync */
		    {
			synccount++;
			if (vbose && synccount == 1) {
			    printf("Possible sync in blk %d at pos %d, ",
				blkno+*block,i%BLKSIZE);
			    printf("shift=%d, polarity=%d\n",shift,polarity);
			}
		    /* get prf from HK data */
			work.b[0] = raw_dat[i+7];
			work.b[1] = raw_dat[i+8];
			work.b[2] = raw_dat[i+9];

			prfhk = j1_get_prf(ntohl(work.d[0]),shift+4,polarity);
			if (prfhk < 0 || prfhk > 5)
			   printf("Suspicious prf code =%d\n",prfhk);
			else
			   *prf = j1_prfs[prfhk];
			printf("prf = %g\n", *prf);

		    /* get format number */
			for (j=0; j<8; j++)
				work.b[j] = raw_dat[i+j+24];
			fmt = j1_get_fmt(ntohl(work.d[0]), ntohl(work.d[1]), 
					shift+6, polarity);
			printf("format(1) = %d\n", fmt);
			fmt_1 = fmt;
			/*
			fmtid = init_best_val(nsyncs);
			save_best_val(fmtid,fmt);
			*/
			n = 1;      /* One sync found */
			break;
		    }  /* if match */
		    data.d[0] = ntohl( data.d[0] );
		    data.d[1] = ntohl( data.d[1] );
		    data.d[0] = ((data.d[0] << 1) & 0xfffffffe) |
				((data.d[1] >> 31) & 0x1);
		    data.d[1] <<= 1;
		    data.d[0] = ntohl( data.d[0] );
		    data.d[1] = ntohl( data.d[1] );
		}  /* for shift */

	    /* If first sync found, find rest */
		k = i;
		i_off = i - start;
		blkno_1 = blkno;
		while ((match != FAIL) && (n < nsyncs))
		{
		/* Pos of next sync */
		    bit_off = shift + j1_code_sep[prfhk];
		    byte_off = (int)(bit_off/8);
		    shift1 = bit_off % 8; 
		    k = (k + byte_off) % BUFSIZE;
		/* Check next sync */
		    for (j = 0; j < 8; j++)
			data.b[j] = raw_dat[k+j];
       
		    sync_tmp[0] = ntohl( data.d[0] );
		    sync_tmp[1] = ntohl( data.d[1] );
		    if (j1_match(&sync_tmp[0], &sync_tmp[1], bit_err,
				shift1,polarity,&extra_off) == FAIL) {
			match = FAIL;
			if (vbose && synccount == 1)
			    printf("failed checking sync %d\n",n+1);
		    }
		    else {  /* Next sync OK, get its frame number */
			*length = extra_off + j1_code_sep[prfhk];
			for (j=0; j<8; j++)
				work.b[j] = raw_dat[j+k+24];
			fmt = j1_get_fmt(ntohl(work.d[0]), ntohl(work.d[1]), 
					extra_off+shift1+6, polarity);
			printf("format = %d\n", fmt);
			/*
			save_best_val(fmtid,fmt);
			*/
			if (fmt!=fmt_1+1) {
			    printf("ERROR in 2nd fmt\n");
			    match = FAIL;
			}
			else n++;
		    }
		}  /* while match */

		if (match != FAIL)  /* All syncs found */
		{
		/* Get the best guess values for format # */
		    /*
		    *iformat = get_best_val(fmtid);
		    */
		    *iformat = fmt_1;
		    printf("first sync at format %d\n", *iformat);
		/* Calculate position of start of format */
		    *block += blkno_1;
		    *offset = (i_off * 8) + shift;
		    *norm_inv = polarity;
		    printf("sync at block=%d,bit_off=%d,polarity=%d\n",
				*block, *offset, *norm_inv);
/*SONY*/
                   if( strcmp( media_type, "DCRSI" ) )
                      sony_stop(IN);
                   else
                      dc_stop(IN);

		    return (PASS);
		}  /* if match */
		if (n > 0) {
		    break;  /* start from reading next block of data */	
		}
		else {
		    for (j = 0; j < 7; j++) 
			data.b[j] = raw_dat[i+1+j];
		}
	    }  /* for i */
	    start = bytes - start;
	    blkno++;
	}  /* while blkno */

	printf("Unable to find %d syncs\n",nsyncs);
/*SONY*/
        if( strcmp( media_type, "DCRSI" ) )
           sony_stop(IN);
        else
           dc_stop(IN);

	return (FAIL);
}

