/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* r1_uts.c - This module contains routines for
	sync detection and decoding of RSAT 
	format data.
	??? TBF, if we can use inst. mode in proc_req,
		 we do not need r1_cal_pixel

   CLW 6/21/95 - mofify r1_srch_sync to work on ALPHA with VME interface
   CLW 5/26/95 - remove bit_dif function
*/

#include <stdio.h>
#include <procdec.h>
#include <procfil.h>
#include <math.h>
#include <netinet/in.h>

#define    AUX1_NUM 0xffffff90
#define    AUX2_NUM 0xffffff98
#define    AUX3_NUM 0xffffff91  /* recorded dump data */
#define    AUX4_NUM 0xffffff99  /* recorded dump data */
#define    IMAGE_PERIOD 0x08

extern float fmt_ratio;		/* ratio of IDHT formats to AMI ones */
extern DPP_FILE dpf;		/* df processing parameter file */
extern int vbose;		/* 1 = print debugging messages */
extern int bytes_format, bytes_fmt_rep;
extern int echo_byte, rep_byte;
extern SP_FILE spf;
extern int VALID_PIXELS;
extern int rec;

extern RQST_PTR    Cur_Rqst;   /* Info for current request */
extern char chk_media_id[];

/*SONY*/
extern char media_type[];
extern char data_direction[];

/* r1_match(word,bit_err) ----------------------------------------------
	This routine compares word with the RSAT sync code, allowing 
	bit_err number of bit errors.  It returns the polarity of the 
	input data if a match is found:  1 = normal, 0 = inverted.
	The routine returns FAIL if a match is not found.
*/

int r1_match(word,bit_err)
	int *word,bit_err;
{
    /* Compare input to normal sync */
	if (bit_dif(*word,R_VCODE,bit_err) <= bit_err)
	    return(1);    /* Matched normal sync */

    /* Compare input to inverted sync */
	if (bit_dif(*word,R_INV_CODE,bit_err) <= bit_err)
	    return(0);        /* Matched inverted sync */

	return(FAIL);
}



/* r1_comp(word) -------------------------------------------------------
	This routine compares word with the RSAT sync code but does not
	allow any bit errors.  It returns the polarity of the input 
	word if a match is found:  1 = normal, 0 = inverted.
	The routine returns FAIL if a match is not found.
*/

int r1_comp(word)
	int *word;
{
    /* Compare input to non-inverted sync */
	if (*word == R_VCODE)
	    return(1);

    /* Compare input to inverted sync */
	if (*word == R_INV_CODE)
	    return(0);

	return(FAIL);
}
/* r1_srch_sync(nsyncs,bit_err,nblks,block,offset,norm_inv,itime,
			 prf) ----------------------------
	This routine searches for RSAT syncs starting at the current
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
	    prf       - The PRF of the returned format

	NOTE: the input DCRS tape is assumed to be already positioned
	at the start block given by 'block'.

*/

int r1_srch_sync (nsyncs,bit_err,nblks,block,offset,norm_inv,itime,
			prf)
	int nsyncs, bit_err, nblks;
	int *block, *offset, *norm_inv, *itime;
	double *prf;
{
#define BUFSIZE (2*ID1_BLOCK_LEN)
	char raw_dat[BUFSIZE+62];  /* Double blk buffer */
	int   i,k,n,m;
	int   fprf;
	int   shift;       /* Sync start bit within byte */
	int   match;       /* Indicates sync match */
	int   polarity;    /* Polarity of data (1=normal, 0=inverted) */
	int   start;       /* Start of search buffer */
	int   stop;        /* End of search buffer */
	int   bytes;       /* Size of search buffer */
	int   blkno;       /* Block currently being searched */
	int   found_aux;   /* found aux at that frame */
	int   fmt_block, fmt_off;  /* Pos of current format start */
	int   ans, aux_code;
	int   image_ref,rep_val,b_seq, n_samp, win_dur, win_start;    
	int   sync_tmp, payload_status;
	int   num;

	union data_tag {
	    char b[8];
	    int  d[2];
	} data,work;

	rep_val = 0;
	payload_status = 0;

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

    if (!strcmp(chk_media_id,"YES")) {
      if (strcmp(media_type,"DCRSI")){
        if (sony_tape_mnt(IN,Cur_Rqst->tape_id) == FAIL)
           return(FAIL);
        else {
           if (!strcmp( data_direction, "REVERSE" ) ){
              sony_stop(IN);
              return(PASS);
           }
	}	
      }
    }

    /* initialize some loop variables and constants */
	if (nsyncs > 2)
	    nsyncs = 2;
	bytes = TAPE_BLOCK_LEN;
printf("TAPE_BLOCK_LEN = %d\n", bytes);
	blkno = 0;          /* Block to be searched */
	start = 0;
	found_aux = -1;
	if (nblks > 1000)
	    nblks = 20;

    /* Get first block of data */

	if (get_rpi_fifo(raw_dat, TAPE_BLOCK_LEN >> 1) <= 0) {
	    printf("RPI unable to read 1st block\n");
/*SONY*/
            if( strcmp( media_type, "DCRSI" ) )
               sony_stop(IN);
             else
               dc_stop(IN);

	    return (FAIL);    /* Unable to read data block */
	}

    /* Search for syncs in nblks number of tape blocks or */
    /*  until syncs found or until no data */

	for (i=0; i<7; i++)
		data.b[i] = raw_dat[i];

	while (blkno < nblks)
	{
	    printf("searching block %d\n",blkno);
	/* Get next block into search buffer */
	    if (get_rpi_fifo(&raw_dat[bytes - start], TAPE_BLOCK_LEN >> 1) <= 0) {
		printf("...failed to read block %d\n",blkno+1);
		break;
	    }
	    if (blkno > 0) {
		for (k=0; k<7; k++)
		   data.b[k]= raw_dat[start+k];
	    }
		
	    for (k = 0; k < 62; k++)
		raw_dat[BUFSIZE+k] = raw_dat[k];
	    stop = start + bytes;
	/* check one block of data */
	    for (i = start; i < stop; i++)
	    {
		data.b[7] = raw_dat[i+7];
	    /* Find first sync word */
		for (shift = 0; shift < 8; shift++)
		{
		    sync_tmp = ntohl( data.d[0] );
		    polarity = r1_comp(&sync_tmp);
		    match = polarity;
		    n = 0;
		    if (match != FAIL)  /* Found possible first sync */
		    {
			if ((data.b[5]==AUX1_NUM) || (data.b[5] == AUX2_NUM) ||
			   (data.b[5] == AUX3_NUM) || (data.b[5] == AUX4_NUM)){
			   if ((data.b[5] == AUX1_NUM) ||  
				    (data.b[5] == AUX3_NUM))
			      rec = 1;
			   if ((data.b[5] == AUX2_NUM) ||  
				    (data.b[5] == AUX4_NUM))
			      rec = 0;

			   /* get rep_valid */
			   work.b[0] = raw_dat[i+59];
			   work.b[1] = raw_dat[i+60];
			   work.b[2] = raw_dat[i+61];
			   work.b[3] = raw_dat[i+62];

			   if (shift > 0)
				work.d[0] = ntohl(ntohl(work.d[0])<<shift);
			   /* unscramble */
			   work.b[0] ^= 0x7b;
			   rep_val = (work.b[0] >> 6) & 0x01;

			   /* get payload status */
			   work.b[0] = raw_dat[i+18];
			   work.b[1] = raw_dat[i+19];
			   work.b[2] = raw_dat[i+20];
			   work.b[3] = raw_dat[i+21];
			   if (shift > 0)
				work.d[0] = ntohl(ntohl(work.d[0])<<shift);

			   /* unscramble */
			   work.b[0] ^= 0xdd;
			   work.b[1] ^= 0xca;
			   work.b[0] &= 0x18;
			   if (work.b[0] == IMAGE_PERIOD) { 
			   	payload_status = 1;
				if (vbose)printf("work.b[0]=%x\n", work.b[0]);
			   }
			   if ((rep_val == 1) && (payload_status== 1)){
				found_aux = 0;
			   	printf("(1)Found sync in blk %d at pos %d, ",
					blkno+*block,i%TAPE_BLOCK_LEN);
			   	printf("shift=%d, polarity=%d\n",shift,polarity);
				/* get aux code # */
				work.b[0] = raw_dat[i+10];
				work.b[1] = raw_dat[i+11];
				work.b[2] = raw_dat[i+12];
				work.b[3] = raw_dat[i+13];
				work.b[4] = raw_dat[i+14];
				work.b[5] = raw_dat[i+15];
				work.b[6] = raw_dat[i+16];
				work.b[7] = raw_dat[i+17];
		    		if (shift > 0) {
				   work.d[0] = ntohl( work.d[0] );
				   work.d[1] = ntohl( work.d[1] );
		    		   work.d[0] = ((work.d[0] << shift) & 
						(0xfffffffe << (shift-1))) |
						((work.d[1] >> (32-shift)) &
						(0x7fffffff >> (31-shift))); 
				   work.d[1] <<= shift;
				   work.d[0] = ntohl( work.d[0] );
				   work.d[1] = ntohl( work.d[1] );
		    		}
				/* unscramble */
				work.b[0] ^= 0x21;
				work.b[1] ^= 0x4f;
				work.b[2] ^= 0xaa;
				work.b[3] ^= 0xe0;
				aux_code = ntohl( work.d[0] );

				/* image ref. no */
				work.b[0] = raw_dat[i+14];
				work.b[1] = raw_dat[i+15];
				work.b[2] = raw_dat[i+16];
				work.b[3] = raw_dat[i+17];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xc5;
				work.b[1] ^= 0x66;
				work.b[2] ^= 0x5f;
				work.b[3] ^= 0xbc;
				image_ref = ntohl( work.d[0] );

				/* get beam sequence */
				work.b[0] = raw_dat[i+28];
				work.b[1] = raw_dat[i+29];
				work.b[2] = raw_dat[i+30];
				work.b[3] = raw_dat[i+31];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xbe;
				work.b[1] ^= 0x1a;
				b_seq = (ntohl(work.d[0]) >> 16) & 0x0000ffff;

				/* get ADC sampling rate */
				work.b[0] = raw_dat[i+32];
				work.b[1] = raw_dat[i+33];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0x09;
				n_samp = (work.b[0] >> 4) & 0x03;

				/* get PRF */
				work.b[0] = raw_dat[i+35];
				work.b[1] = raw_dat[i+36];
				work.b[2] = raw_dat[i+37];
				work.b[3] = raw_dat[i+38];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0x07;
				work.b[1] ^= 0x48;
				work.b[2] ^= 0xe2;
				fprf = (ntohl(work.d[0]) >> 19) & 0x00001fff;

				/* get window start time*/
				work.b[0] = raw_dat[i+37];
				work.b[1] = raw_dat[i+38];
				work.b[2] = raw_dat[i+39];
				work.b[3] = raw_dat[i+40];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xe2;
				work.b[1] ^= 0x02;
				win_start = (ntohl(work.d[0])>>20)&0x00000fff;
				
				/* get window duration */
				work.b[0] = raw_dat[i+39];
				work.b[1] = raw_dat[i+40];
				work.b[2] = raw_dat[i+41];
				work.b[3] = raw_dat[i+42];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xc7;
				work.b[1] ^= 0xa1;
				win_dur = (ntohl(work.d[0]) >> 20) & 0x00000fff;

                                /* get time */
                                work.b[0] = raw_dat[i+55];
                                work.b[1] = raw_dat[i+56];
                                work.b[2] = raw_dat[i+57];
                                work.b[3] = raw_dat[i+58];
                                work.b[4] = raw_dat[i+59];
                                if (shift > 0) {
					work.d[0] = ntohl( work.d[0] );
					work.d[1] = ntohl( work.d[1] );
                                        work.d[0] = ((work.d[0] << shift) &
                                                (0xfffffffe << (shift-1))) |
                                                ((work.d[1] >> (32-shift)) &
                                                (0x7fffffff >> (31-shift)));
                                        work.d[1] <<= shift;
					work.d[0] = ntohl( work.d[0] );
					work.d[1] = ntohl( work.d[1] );
                                }
                                /* unscramble */
                                work.b[0] ^= 0xe7;
                                work.b[1] ^= 0x8d;
                                work.b[2] ^= 0x84;
                                work.b[3] ^= 0x5d;
                                *itime =work.d[0];
                                if (vbose) printf("*itime=%x\n", *itime);

			    	n = 1;      /* One sync found */
			    	break;
			    
			    } else {
			       found_aux = -1;
			       payload_status = 0;
			       rep_val = 0;
			       break;
			      }
			} else {
				found_aux = -1;
				break;
			  }
		    }  /* if match */
		    data.d[0] = ntohl( data.d[0] );
		    data.d[1] = ntohl( data.d[1] );
		    data.d[0] = (((data.d[0] << 1) & 0xfffffffe) |
				((data.d[1] >> 31) & 0x1));
		    data.d[1] <<= 1;
		    data.d[0] = ntohl( data.d[0] );
		    data.d[1] = ntohl( data.d[1] );
		}  /* for shift */
	    /* If first sync found, find rest */
		k = i;
		fmt_block = blkno;
		fmt_off = i - start;
		while ((match != FAIL) && (found_aux == -1))
		{
		/* Pos of next sync */
		   k= k + R_VCODE_SEP;
		   if (k > stop) {
		       found_aux = -1;
		       match = FAIL;
		       break;
		   }
		   else {
		    k = k % stop;
		/* Check next sync */
		    for (m=0; m<8; m++)
		    	work.b[m] = raw_dat[k+m];

		    if (shift > 0) {
			work.d[0] = ntohl( work.d[0] );
			work.d[1] = ntohl( work.d[1] );
		    	work.d[0] = ((work.d[0] << shift) & 
				(0xfffffffe << (shift-1))) |
				((work.d[1] >> (32-shift)) &
				(0x7fffffff >> (31-shift))); 
		    	work.d[1] <<= shift;
			work.d[0] = ntohl( work.d[0] );
			work.d[1] = ntohl( work.d[1] );
		    }
		    sync_tmp = ntohl( work.d[0] );
		    if (polarity != (ans = r1_match(&sync_tmp,bit_err))){
			match = FAIL;
			if (vbose){
			   printf(" ...failed checking sync %d",blkno+*block); 
                           printf(" at pos %d\n", k%stop);
                        }
		    }
		    else {   /* Next sync OK, get its virtual id */
			n++;
			if ((work.b[5]==AUX1_NUM) || (work.b[5] == AUX2_NUM) ||
			   (work.b[5] == AUX3_NUM) || (work.b[5] == AUX4_NUM)){
			   if ((work.b[5] == AUX1_NUM) ||  
				    (work.b[5] == AUX3_NUM))
			      rec = 1;
			   if ((work.b[5] == AUX2_NUM) ||  
				    (work.b[5] == AUX4_NUM))
			      rec = 0;
			   /* get rep_valid */
			   work.b[0] = raw_dat[k+59];
			   work.b[1] = raw_dat[k+60];
			   work.b[2] = raw_dat[k+61];
			   work.b[3] = raw_dat[k+62];
			   if (shift > 0)
				work.d[0] = ntohl(ntohl(work.d[0])<<shift);
			   /* unscramble */
			   work.b[0] ^= 0x7b;
			   rep_val = (work.b[0] >> 6) & 0x01;

			   /* get payload status */
			   work.b[0] = raw_dat[k+18];
			   work.b[1] = raw_dat[k+19];
			   work.b[2] = raw_dat[k+20];
			   work.b[3] = raw_dat[k+21];
			   if (shift > 0)
				work.d[0] = ntohl(ntohl(work.d[0])<<shift);

			   /* unscramble */
			   work.b[0] ^= 0xdd;
			   work.b[1] ^= 0xca;
			   work.b[0] &= 0x18;
			   if (work.b[0] == IMAGE_PERIOD) {  
			   	payload_status = 1;
				if (vbose)printf("work.b[0]=%x\n", work.b[0]);
			   }
			   if ((rep_val == 1) && (payload_status== 1)){
				found_aux = 0;
			   	printf("Found sync in blk %d at pos %d, ",
					blkno+*block,k%TAPE_BLOCK_LEN);
			   	printf("shift=%d, polarity=%d\n",shift,polarity);
				
                                fmt_off = k % TAPE_BLOCK_LEN;
				/* get aux code # */
				work.b[0] = raw_dat[k+10];
				work.b[1] = raw_dat[k+11];
				work.b[2] = raw_dat[k+12];
				work.b[3] = raw_dat[k+13];
				work.b[4] = raw_dat[k+14];
				work.b[5] = raw_dat[k+15];
				work.b[6] = raw_dat[k+16];
				work.b[7] = raw_dat[k+17];
		    		if (shift > 0) {
				   work.d[0] = ntohl( work.d[0] );
				   work.d[1] = ntohl( work.d[1] );
		    		   work.d[0] = ((work.d[0] << shift) & 
						(0xfffffffe << (shift-1))) |
						((work.d[1] >> (32-shift)) &
						(0x7fffffff >> (31-shift))); 
		    		   work.d[1] <<= shift;
				   work.d[0] = ntohl( work.d[0] );
				   work.d[1] = ntohl( work.d[1] );
		    		}
				work.b[0] ^= 0x21;
				work.b[1] ^= 0x4f;
				work.b[2] ^= 0xaa;
				work.b[3] ^= 0xe0;
				aux_code = ntohl(work.d[0]);
				
				/* image ref. no */
				work.b[0] = raw_dat[k+14];
				work.b[1] = raw_dat[k+15];
				work.b[2] = raw_dat[k+16];
				work.b[3] = raw_dat[k+17];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xc5;
				work.b[1] ^= 0x66;
				work.b[2] ^= 0x5f;
				work.b[3] ^= 0xbc;
				image_ref = ntohl(work.d[0]);

				/* get beam sequence */
				work.b[0] = raw_dat[k+28];
				work.b[1] = raw_dat[k+29];
				work.b[2] = raw_dat[k+30];
				work.b[3] = raw_dat[k+31];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xbe;
				work.b[1] ^= 0x1a;
				b_seq = (ntohl(work.d[0]) >> 16) & 0x0000ffff;

				/* get ADC sampling rate */
				work.b[0] = raw_dat[k+32];
				work.b[1] = raw_dat[k+33];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0x09;
				n_samp = (work.b[0] >> 4) & 0x03;

				/* get PRF */
				work.b[0] = raw_dat[k+35];
				work.b[1] = raw_dat[k+36];
				work.b[2] = raw_dat[k+37];
				work.b[3] = raw_dat[k+38];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0x07;
				work.b[1] ^= 0x48;
				work.b[2] ^= 0xe2;
				fprf = (ntohl(work.d[0]) >> 19) & 0x00001fff;

				/* get window start time*/
				work.b[0] = raw_dat[k+37];
				work.b[1] = raw_dat[k+38];
				work.b[2] = raw_dat[k+39];
				work.b[3] = raw_dat[k+40];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xe2;
				work.b[1] ^= 0x02;
				win_start = (ntohl(work.d[0]) >> 20) & 0x00000fff;

				/* get window duration */
				work.b[0] = raw_dat[k+39];
				work.b[1] = raw_dat[k+40];
				work.b[2] = raw_dat[k+41];
				work.b[3] = raw_dat[k+42];
				if (shift > 0)
				   work.d[0] = ntohl(ntohl(work.d[0])<<shift);
				/* unscramble */
				work.b[0] ^= 0xc7;
				work.b[1] ^= 0xa1;
				win_dur = (ntohl(work.d[0]) >> 20) & 0x00000fff;

                                /* get time */
                                work.b[0] = raw_dat[k+55];
                                work.b[1] = raw_dat[k+56];
                                work.b[2] = raw_dat[k+57];
                                work.b[3] = raw_dat[k+58];
                                work.b[4] = raw_dat[k+59];
                                if (shift > 0) {
					work.d[0] = ntohl( work.d[0] );
					work.d[1] = ntohl( work.d[1] );
                                        work.d[0] = ((work.d[0] << shift) &
                                                (0xfffffffe << (shift-1))) |
                                                ((work.d[1] >> (32-shift)) &
                                                (0x7fffffff >> (31-shift)));
                                        work.d[1] <<= shift;
					work.d[0] = ntohl( work.d[0] );
					work.d[1] = ntohl( work.d[1] );
                                }
                                /* unscramble */
                                work.b[0] ^= 0xe7;
                                work.b[1] ^= 0x8d;
                                work.b[2] ^= 0x84;
                                work.b[3] ^= 0x5d;
                                *itime =work.d[0];
                                if (vbose) printf("*itime=%x\n", *itime);

                           }
			   else {
				found_aux = -1;
				rep_val = 0;
				payload_status = 0;
			   }
			}
			else found_aux = -1;
		   }
		 } /* else */
		}  /* while match */

		if ((match != FAIL) && (found_aux == 0)){
	        /* Everything checks OK */
		/* Calculate position of start of format */
		    *block += fmt_block;
		    printf("i=%d, start=%d,fmt_off=%d\n",i,start,fmt_off);
		    *offset = (fmt_off * 8) + shift;  /* bit offset */
		    *norm_inv = rec;
		    if (vbose) printf("rec=%d\n", rec);
		    if (vbose) printf("b_seq = %x\n",b_seq);
		    /*norm_inv = polarity;
		    printf("block = %d\n",*block);
		    printf("offset = %d\n",*offset);
		    printf("norm_inv = %d\n", *norm_inv);
		    printf("aux_code = %x\n",aux_code);
		    printf("image_ref = %x\n",image_ref);
		    printf("b_seq = %x\n",b_seq);
		    printf("n_samp = %d\n", n_samp);
		    printf("rep_present = %d\n",rep_val);
		    printf("win_dur = %x = %d\n",win_dur,win_dur);
		    printf("win_start = %x\n", win_start);
		    printf("rec = %x = %d\n", rec, rec);*/
		    *prf = fprf;
		/* if data is junk or scansar, assum no sync found */
		    if (r1_samp(b_seq,n_samp,prf,rep_val,win_dur,win_start)==
			FAIL) return(FAIL);
		    if (vbose) printf("after prf = %g \n", *prf);
/*SONY*/
                    if( strcmp( media_type, "DCRSI" ) ) {
/*
                       sony_stop(IN);
*/
                    }
                    else
                       dc_stop(IN);

		    return (PASS);
		}  /* if match */
		if (n>0) {		/* reset best_val */
		    break;
		} else {
		    for (n=0; n<7; n++){
			data.b[n] = raw_dat[i+1+n];
		    }
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


/* r1_samp.c -- calculate number of replica sample, number of
		bytes per format without rep or with rep data, 
		convert time_unit of Rx window start
		time, Rx window duration, and PRF period   
*/


r1_samp(b_seq,n_samp, prf,rep_val,win_dur,win_start) 
	int b_seq,n_samp,rep_val,win_dur,win_start;
	double *prf;
{

	double win_dur_time;
	int samp_code, no_samp;
	int first_rep_frame, no_rep_frame, remain_rep_bit;
	double time_unit;
	double win_start_time;
	double samp_rate;
	int no_echo_bits, no_rep_bits, total_rep_frame;
	int first_echo_frame, no_echo_frame, remain_echo_bit;
	int total, second_time;
	int fmt_no;
	double prf_period;
	int rep_valid;   	/* 1 = replica present
				   0 = no replica 	*/

  	fmt_no = 0;
	samp_code = n_samp;

	switch (b_seq) {
		case 1: spf.csr = 18.46E6;
			spf.gamma_nom = 23;	
			break;
		case 2: spf.csr = 18.46E6;
			spf.gamma_nom = 28;
			break;
		case 3: spf.csr = 12.92E6;
			spf.gamma_nom = 34;
			break;
		case 4: spf.csr = 12.92E6;
			spf.gamma_nom = 37;
			break;
		case 5: spf.csr = 12.92E6;
			spf.gamma_nom = 39;
			break;
		case 6: spf.csr = 12.92E6;
			spf.gamma_nom = 44;
			break;
		case 7: spf.csr = 12.92E6;
			spf.gamma_nom = 47;
			break;
		default:
			printf("It is not standard beam\n");
			return(FAIL);
	
	}
	   if (samp_code == 0){
		no_samp = 1440;
		time_unit = 0.00000018566;
		samp_rate = 129268300 / 4;
	   }
	   else if (samp_code == 1){
		no_samp = 822;
/*		time_unit = 0.00000032491; */
		time_unit = 0.000000324905642;
		samp_rate = 129268300 / 7;
	   }
	   else if (samp_code == 2){
		no_samp = 576;
		time_unit = 0.00000046415;
		samp_rate = 129268300 / 10;
	   }
	   else {
	      	printf ("illegal code\n");
	   }  
	   win_dur_time = (win_dur+1) * time_unit; 
	   win_start_time = (win_start+5) * time_unit;
	   prf_period = (1/((*prf+2) * time_unit));
	   printf("prf_period=%12.7f\n", prf_period);
	   echo_byte = (win_dur_time * samp_rate) - 1;
	   no_echo_bits = echo_byte * 8;
	   rep_byte = no_samp - 1;
	   VALID_PIXELS = echo_byte;
	   *prf = prf_period;

	   /*printf("time_unit = %g\n", time_unit);
	   printf("samp_rate = %g\n", samp_rate);
	   printf ("win_dur_time = %g\n",win_dur_time);
	   printf ("prf_period = %d\n",prf_period);
	   printf ("VALID_PIXEL = %d \n", VALID_PIXELS);
	   printf("no_echo_bits =%d\n",no_echo_bits);*/


      /* calculate no of frame for replica */
   	  if (rep_valid == 1) {
		no_rep_bits = no_samp * 8;
	   	first_rep_frame = no_rep_bits - 2088;
	   	no_rep_frame = first_rep_frame / 2488 + 1;
	   	remain_rep_bit = first_rep_frame % 2488;
		if (remain_rep_bit > 0) 
			total_rep_frame = no_rep_frame + 1; 
	   	/*printf("number of frames contains replica: %d\n",
				total_rep_frame);*/
	   	first_echo_frame = (remain_rep_bit + no_echo_bits) - 2488;
	   	no_echo_frame = first_echo_frame / 2488;
	   }
	   else {
	   	first_echo_frame = no_echo_bits - 2088;
	   	no_echo_frame = first_echo_frame / 2488 + 1;
	   }
      /* calculate no of frames in each format */
	   remain_echo_bit = first_echo_frame % 2488;
	   if (remain_echo_bit > 0) no_echo_frame += 1;
	   if (rep_valid == 1) {
	   	total = no_echo_frame + no_rep_frame;
	   	bytes_fmt_rep = total * 323;
	   	/*printf(" number of bytes in the format (include rep): %d\n",
			bytes_fmt_rep);*/
	   }
	   else {
		bytes_fmt_rep = 0;
	   	total = no_echo_frame;
	   	bytes_format = total * 323;
	   	/*printf(" number of bytes in the format(no_rep): %d\n", bytes_format);*/
	   }
	   /*printf(" total valid frames in this format: %d\n", total);*/
	
}


payload_status(ap,value)
	RSAT_AUX_PTR ap;
	int value;
{

	/* check image status in payload status */
	ap->payload.imgsta = value >> 11 & 0x0003;
	if (ap->payload.imgsta == 0) 
		strcpy(ap->payload.img_status,"off");
	if (ap->payload.imgsta == 1) 
		strcpy(ap->payload.img_status,"excuting image");
	if (ap->payload.imgsta == 2) 
		strcpy(ap->payload.img_status,"scan for image");
	if (ap->payload.imgsta == 3) 
		strcpy(ap->payload.img_status,"image failed");

	/* check calibration status in payload status */
	ap->payload.calsta = value >> 7 & 0x000f;
	if (ap->payload.calsta == 0) 
		strcpy(ap->payload.cal_status,"off");
	if (ap->payload.calsta == 1) 
		strcpy(ap->payload.cal_status,"executing CAL 1");
	if (ap->payload.calsta == 2) 
		strcpy(ap->payload.cal_status,"executing CAL 2");
	if (ap->payload.calsta == 8) 
		strcpy(ap->payload.cal_status,"CAL on");
	if (ap->payload.calsta == 9) 
		strcpy(ap->payload.cal_status,"CAL failed");

	/* check type of image in payload status */
	ap->payload.ty_img = value >> 3 & 0x0003;
	if (ap->payload.ty_img == 0) 
		strcpy(ap->payload.type_img,"real-time");
	if (ap->payload.ty_img == 1) 
		strcpy(ap->payload.type_img,"store");

	/* check mode in payload status */
	ap->payload.pr_mode = value >> 2 & 0x0001;
	if (ap->payload.pr_mode == 0) 
		strcpy(ap->payload.type_mode,"scansar");
	if (ap->payload.pr_mode == 1) 
		strcpy(ap->payload.type_mode,"stripmap");
}

double cal_temp(temp_value)
	int temp_value;
{

	double rec_th, log_val, val1, val2, val3;

	rec_th = (2940*temp_value) / (294-temp_value);
	log_val = log(rec_th); 
	val1 = .0014733 + (.0002372*log_val);
	val2 = .0000001074 * (log_val*log_val*log_val);
	val3 = (1/(val1 + val2)) - 273.15;
	return(val3);
}
