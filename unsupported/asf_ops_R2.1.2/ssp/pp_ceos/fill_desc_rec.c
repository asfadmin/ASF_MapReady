/* SccsId[]= @(#)fill_desc_rec.c	2.41 3/24/98 */
static char sccsid_fill_desc_rec[]= "@(#)PPfill_desc_rec.c:2.41";

#include <stdio.h>
#include "odl_file.h"
#include "externs.h"

int fill_image_descriptor_record(char *buf)
{

int size;

typedef unsigned char B1;
typedef unsigned int B4;

B4 rec_seq = 1;	/* Record sequence number */
B1 rec_sub1 = 63;	/* First record sub-type code */
B1 rec_type = 192;	/* Record type code */
B1 rec_sub2 = 18;	/* Second record sub-type code */
B1 rec_sub3 = 18;	/* Third record sub-type code */
B4 length;	/* Length of this record */
char* ascii_flag = "A ";	/* ASCII FLAG */
char* format_doc = "CEOS-SAR-CCT";	/* Format control document */
char* format_rev = " B";	/* Format document revision */
char* design_rev = " B";	/* File design revision */
char* software_id = "subsystem2.0";	/* Software identifier */
char* file_num = "   2";	/* File number */
char product_id[17];	/* File name */
char* rec_seq_flag = "FSEQ";	/* Record sequence/location flag */
char* seq_loc = "       1";	/* Sequence number location */
char* seq_len = "   4";		/* Sequence number length */
char* rec_code = "FTYP";	/* Record code/location flag */
char* code_loc = "       5";	/* Record code location */
char* code_len = "   4";	/* Record code length */
char* rec_len = "FLGT";		/* Record length/location flag */
char* rlen_loc = "       9";	/* Record length location */
char* rlen_len = "   4";	/* Record length, bytes */
char n_dataset[7];	/* Number of SAR data records */
char l_dataset[7]; 	/* SAR data record length, bytes */
char nbit[5];	/* Number of bits per sample */
char nsamp[5];	/* Samples per data group */
char nbyte[5];	/* Bytes per data group or per pixel */
char* justify = "    ";		/* Sample justification and order */
char* nchn = "   1";	/* Number of SAR channels */
char nlin[9];	/* Lines per data set */
char* nleft = "   0";	/* Left border pixels per line */
char ngrp[9];	/* Pixels per line per channel */
char* nright = "   0";	/* Right border pixels per line */
char* ntop = "   0"; /* Top border lines */
char* nbox1 = "   0";	/* Bottom border lines */
char* intleav = "BSQ ";	/* Interleave indicator */
char* nrec_lin = " 1";	/* Records per line */
char* nrec_chn = " 1";	/* Records per channel */
char* n_prefix = " 192";	/* Prefix data per record */
char n_sar[9];		/* SAR data byte count */
char* n_suffix = "   0";	/* Suffix data per record */
char* lin_loc = "  1354PB";	/* Line number locator */
char* chn_loc = "  4952PB";	/* Channel number locator */
char* time_loc = "  4554PB";	/* Time locator */
char* left_loc = "  21 4PB";	/* Left fill locator */
char* right_loc = "  29 4PB";	/* Right fill locator */
char* pad_ind = "    ";		/* Pad pixel indicator */
char* qual_loc = "        ";	/* Quality code locator */
char* cali_loc = "        ";	/* Calibration info locator */
char* gain_loc = "        ";	/* Gain value locator */
char* bias_loc = "        ";	/* Bias value locator */
char type_id[29];		/* Data type identifier */
char type_code[5];		/* Data type code */
char* left_fill = "   0";	/* Number left fill bits */
char* right_fill = "   0";	/* Number right fill bits */
char pixel_rng[9];		/* Pixel data range */

int sar_num_rec;

/*******************************************************************************/
	if (!strcmp(odl_file->prod_type, "STANDARD")) {
		length = rec_length + 192;
		sar_num_rec = num_rec;
		strcpy(nbit, "   8");
		strcpy(nsamp, "   1");
		strcpy(nbyte, "   1");
		Fill_str(type_id, sizeof(type_id), "UNSIGNED INTEGER*1");
		strcpy(type_code, "IU1 ");
		strcpy(pixel_rng, "     255");
		sprintf(ngrp, "%8d", rec_length);
	}
	else if (!strcmp(odl_file->prod_type, "RAMP")) {
		length = rec_length + 192;
		sar_num_rec = num_rec;
		strcpy(nbit, "  16");
		strcpy(nsamp, "   1");
		strcpy(nbyte, "   2");
		Fill_str(type_id, sizeof(type_id), "UNSIGNED INTEGER*2");
		strcpy(type_code, "IU2 ");
		strcpy(pixel_rng, "   65535");
		sprintf(ngrp, "%8d", rec_length/2);
	}
	else {     /* COMPLEX */
		length = rec_length;
		/* num_rec--; */
		sar_num_rec = num_rec - 1;
		strcpy(nbit, "  16");
		strcpy(nsamp, "   2");
		strcpy(nbyte, "   4");
		Fill_str(type_id, sizeof(type_id),  "COMPLEX INTEGER*4");
		strcpy(type_code, "CI*4");
		strcpy(pixel_rng, "   65535");
		sprintf(ngrp, "%8d", (rec_length - 192)/4);
	}


	memset(buf, ' ', length);

	memcpy(&buf[0], &rec_seq, 4); /* Record sequence number */
	memcpy(&buf[4], &rec_sub1, 1); /* First record sub-type code */
	memcpy(&buf[5], &rec_type, 1); /* Record type code */
	memcpy(&buf[6], &rec_sub2, 1); /* Second record sub-type code */
	memcpy(&buf[7], &rec_sub3, 1); /* Third record sub-type code */
	memcpy(&buf[8], &length, 4); /* Length of this record */

	memcpy(&buf[12], ascii_flag, strlen(ascii_flag)); /* ASCII flag */
	memset(&buf[14], ' ', 2); /* Unused */
	memcpy(&buf[16], format_doc, strlen(format_doc)); /* Format control document */
	memcpy(&buf[28], format_rev, strlen(format_rev)); /* Format document revision */
	memcpy(&buf[30], design_rev, strlen(design_rev)); /* File design revision */
	memcpy(&buf[32], software_id, strlen(software_id)); /* Software identifier */
	memcpy(&buf[44], file_num, strlen(file_num)); /* File number */

	memcpy(&buf[48], odl_file->product_id, strlen(odl_file->product_id)); /* File name */

	memcpy(&buf[64], rec_seq_flag, strlen(rec_seq_flag)); /* Record sequence/location flag */
	memcpy(&buf[68], seq_loc, strlen(seq_loc)); /* Sequence number location */
	memcpy(&buf[76], &seq_len, strlen(seq_len)); /* Sequence number length */
	memcpy(&buf[80], rec_code, strlen(rec_code)); /* Record code/location flag */ 
	memcpy(&buf[84], code_loc, strlen(code_loc)); /* Record code location */
	memcpy(&buf[92], code_len, strlen(code_len)); /* Record code length */
	memcpy(&buf[96], rec_len, strlen(rec_len)); /* Record length/location flag */
	memcpy(&buf[100], rlen_loc, strlen(rlen_loc)); /* Record length location */
	memcpy(&buf[108], rlen_len, strlen(rlen_len)); /* Record length, bytes */
	memset(&buf[112], ' ', 68);	/* Reserved */
	
	sprintf(n_dataset, "%6d", sar_num_rec); 
	memcpy(&buf[180], n_dataset, strlen(n_dataset)); /* Number of SAR data records */

	sprintf(l_dataset, "%6d", length);
	memcpy(&buf[186], l_dataset, strlen(l_dataset)); /* SAR data record length, bytes */

	memset(&buf[192], ' ', 24);	/* Unused */

	
	memcpy(&buf[216], nbit, strlen(nbit)); /* Number of bits per sample */

	
	memcpy(&buf[220], nsamp, strlen(nsamp)); /* Samples per data group */

	memcpy(&buf[224], nbyte, strlen(nbyte)); /* Bytes per data group or per pixel */

	memcpy(&buf[228], justify, strlen(justify)); /* Sample justification and order */
	memcpy(&buf[232], nchn, strlen(nchn)); /* Number of SAR channels */

	sprintf(nlin, "%8d", sar_num_rec);
	memcpy(&buf[236], nlin, strlen(nlin)); /* Lines per data set */

	memcpy(&buf[244], nleft, strlen(nleft)); /* Left border pixels per line */

	memcpy(&buf[248], ngrp, strlen(ngrp)); /* Pixels per line per channel */

	memcpy(&buf[256], nright, strlen(nright)); /* Right border pixels per line */
	memcpy(&buf[260], ntop, strlen(ntop)); /* Top border lines */
	memcpy(&buf[264], nbox1, strlen(nbox1)); /* Bottom border lines */
	memcpy(&buf[268], intleav, strlen(intleav)); /* Interleave indicator */
	memcpy(&buf[272], nrec_lin, strlen(nrec_lin)); /* Records per line */
	memcpy(&buf[274], nrec_chn, strlen(nrec_chn)); /* Records per channel */
	memcpy(&buf[276], n_prefix, strlen(n_prefix)); /* Prefix data per record */

	sprintf(n_sar, "%8d", sar_num_rec);
	memcpy(&buf[280], n_sar, strlen(n_sar)); /* SAR data byte count */

	memcpy(&buf[288], n_suffix, strlen(n_suffix)); /* Suffix data per record */
	memset(&buf[292], ' ', 4); /* Unused */
	memcpy(&buf[296], lin_loc, strlen(lin_loc)); /* Line number locator */
	memcpy(&buf[304], chn_loc, strlen(chn_loc)); /* Channel number locator */
	memcpy(&buf[312], time_loc, strlen(time_loc)); /* Time locator */
	memcpy(&buf[320], left_loc, strlen(left_loc)); /* Left fill locator */
	memcpy(&buf[328], right_loc, strlen(right_loc)); /* Right fill locator */
	memcpy(&buf[336], pad_ind, strlen(pad_ind)); /* Pad pixel indicator */

	memset(&buf[340], ' ', 28);

	memcpy(&buf[368], qual_loc, strlen(qual_loc)); /* Quality code locator */
	memcpy(&buf[376], cali_loc, strlen(cali_loc)); /* Calibration info locator */
	memcpy(&buf[384], gain_loc, strlen(gain_loc)); /* Gain value locator */
	memcpy(&buf[392], bias_loc, strlen(bias_loc)); /* Bias value locator */

	memcpy(&buf[400], type_id, strlen(type_id)); /* Data type identifier */

	memcpy(&buf[428], type_code, strlen(type_code)); /* Data type code */

	memcpy(&buf[432], left_fill, strlen(left_fill)); /* Number left fill bits */
	memcpy(&buf[436], right_fill, strlen(right_fill)); /* Number right fill bits */

	memcpy(&buf[440], pixel_rng, strlen(pixel_rng)); /* Pixel data range */
	

	memset(&buf[448], ' ', length-448); 


	return 0;
}
