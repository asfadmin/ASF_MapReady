/* Alaska SAR Processor (ASP) %W% %E% %U% */
/*  j1_sync_pcm( pcm_buf, bytes ) ----------------------------------------
	Searches input buffer for PCM_SYNC code and returns bit offset to first
	matching occurrance.

  Inputs:
	PCM_BUF:  Pointer to buffer containing J-ERS-1 PCM data.
	BYTES:  The number of bytes in input buffer to scan
  Returns:
	bit off set to first SYNC found
	-1 if sync not found
*/
int j1_sync_pcm( pcm_buf, bytes )
unsigned char *pcm_buf;
long bytes;

{	/**BO**  check byte order on different machines  **BO**/
    unsigned long sync_val, pcm_sync, sync2;
    int byte, bit, l;

pcm_sync=0xfaf32000;

/*  Look for SYNC pattern in data  */

sync_val = ((((unsigned long)pcm_buf[0])<<24) & 0xff000000) |
	   ((((unsigned long)pcm_buf[1])<<16) & 0xff0000) | 
	   ((((unsigned long)pcm_buf[2])<<8) & 0xff00);  /**BO**/

for( byte=3; byte<bytes; byte++ ){
    sync_val = (sync_val&0xffffff00) | (unsigned long) pcm_buf[byte];
    for( bit=0; bit<8; bit++ ){
	if( bit_dif((sync_val&0xffffff00), pcm_sync, 1) <= 1){
	    printf("GOT FIRST PCM_SYNC!, byte=%d, bit=%d\n", byte-3, bit);
	    if( byte+128<bytes ){
		l = byte + 128;
		sync2 = ((((unsigned long)pcm_buf[l-3])<<24) & 0xff000000) |
			((((unsigned long)pcm_buf[l-2])<<16) & 0xff0000) | 
			((((unsigned long)pcm_buf[l-1])<<8) & 0xff00) |
			(((unsigned long)pcm_buf[l]) & 0xff );
		sync2 = sync2 << bit;
		if( bit_dif( sync2&0xffffff00, pcm_sync, 1 ) <= 1 )
			printf("Second minor frame SYNC verified!\n");
	    } else {
		printf("Insufficient data to verify additional frames\n");
	    }
/*
	    printf("BYTES:");
	    for( l=byte-3; l<byte+16; l++ ) printf(" %02x", pcm_buf[l] );
	    printf("||\n");
*/
	    return( ((byte-3)<<3)+bit );
	}
/*
	if( bit_dif(((~sync_val)&0xffffff00), pcm_sync, 1) <= 1) {
	    printf("GOT IT INVERSED!, byte=%d, bit=%d\n", byte-3, bit);
	    printf("BYTES:");
	    for( l=byte-3; l<byte+16; l++ ) printf(" %02x", pcm_buf[l] );
	    printf("||\n");
	    return( ((byte-3)<<3)+bit );
	}
*/
	sync_val = sync_val << 1;	/*  shift in MSB from next byte  */
    }
}	/*  end scanning through BYTES bytes  */
return( -1 );	/*  sync code not found  */
}
