/*
VDF.h:
	This header file contains structures for the VDR-- Volume
Descriptor Record, FPR-- File Pointer Records, and TDR-- Text Description
Records, which together make up a Volume Descriptor File (VDF).

Checked into ClearCASE on 6/19/98
*/
#include	<stdio.h>

typedef enum {
	volumeDescriptorRecord=192,
	filePointer=219,
	textRecord=18
} subType1Code;

typedef struct volume_descriptor_record {
	int	record_num;	   /* I*4 Record number */
	unsigned char	subType1, /*First record sub-type code*/
		subType2, /*Second record sub-type code (192)*/
		subType3, /*Third record sub-type code (18)*/
		subType4; /*Fourth record sub-type code (18)*/
	int 	record_length;	   /* Number of bytes in this record, incl. header.*/
	char	ascii_ebcdic_flag[2], /* Tells whether data is ASCII or EBCDIC.*/
		blank[2],
		format_control_doc[12], /*Superstructure format control doc (?) */
		format_control_vers[2], /*version number of above. */
		format_control_revision[2], /*revision number of above. */
		facility_revision[12], /*Logical volume generating facitily release/revision code.*/
		tape_id[16], /* Id of physical volume containting this VDF.*/
		vol_id[16], /* Volume ID */
		set_id[16], /* Volume set ID */
		total_volumes[2], /* I*2 total number of volumes in order */
		first_volume[2],  /* I*2 first tape number */
		last_volume[2],	/* I*2 last tape number */
		this_volume[2],	  /* I*2 this volume number */
		  crap_middle[60],	  /* 101 padding */
		numFPR[4],	  /* 161 Number of file pointer records in VDF*/
		numRecords[4],	  /* 165 Number of records in VDF (as above, plus number of text records)*/
		  crap_end[192];	  /* 169 irrelevent */
} volume_descriptor_record;
/*See comment below-- Divide by Zero when volume_descriptor_record is the wrong size.*/
const int vdrsizeFlag=1/(360==sizeof(volume_descriptor_record));


typedef struct file_pointer_record {
	int	record_num;	   /* I*4 Record number */
	unsigned char	subType1, /*First record sub-type code*/
		subType2, /*Second record sub-type code (192)*/
		subType3, /*Third record sub-type code (18)*/
		subType4; /*Fourth record sub-type code (18)*/
	int 	record_length;	   /* Number of bytes in this record, incl. header.*/
	char	ascii_ebcdic_flag[2], /* Tells whether data is ASCII or EBCDIC.*/
		blank[2],
		ser_num[4],	   /* I*4 sequential number of the file */
		i_id[16],	   /* image ID string */
		f_type[28],	   /* File type string */
		f_code[4],	   /* File code: "SARL", "IMOP", or "SART" */
		d_type[28],	   /* Data type string */
		d_code[4],	   /* Data code: "MBAA", "BINO", "COMP", or "REAL" */
		n_line[8],	   /* I*8 number of records */
		line_size[8],	   /* I*8 size of first record */
		max_rec_len[8],	   /* I*8 max record length */
		record_length_type[12],	   /* record_length type string*/
		record_length_code[4],	   /* record_length code: "FIXD" or "VARE"*/
		phys_vol_start[2], /* I*2 physical volume for start of file */
		phys_vol_end[2],   /* I*2 physical volume for end of file */
		rec_phys_start[8], /* I*8 First Record Number on this Volume */
		rec_phys_end[8],   /* I*8 Last Record Number on this Volume */
		  crap_end[200];	   /* No use defined */
} file_pointer_record;

/*This next lines are to make sure that we find out at compile time
  if the size of the above records are suddently no longer 360 bytes.
  If 360!=sizeof(fpr), then we'll get a divide-by-zero error at compile
  time, and we should change the above definition so we get exactly
  360 bytes.*/
const int fprsizeFlag=1/(360==sizeof(file_pointer_record));
