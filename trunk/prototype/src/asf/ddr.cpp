/**
  LAS Image File Format "DDR" (Data Descriptor Record) I/O.
  This format comes from the Eros Data Center's Land Analysis System (LAS).
  
  Orion Sky Lawlor, olawlor@acm.org, 2006/05/10 (Copyright ASF)
*/
#include <stdio.h> /* for sprintf */
#include <string.h> /* for strlen, memset */
#include <stdlib.h> /* for exit */
#include <string>
#include "osl/io_types.h" /* for osl::Big32 class */
#include "ddr.h" /* for main DDR class */

#ifndef FOPEN
#define  FOPEN fopen
#define  FREAD fread
#define  FWRITE fwrite
#define  FCLOSE fclose
#endif

/**
  Band data descriptor record.  This is a silly, useless struct. 
*/
struct BDDR
{
    int bandno;	  /* band no. of the record 		     */
    int valid;		  /* min/max validity flag of the band	     */
                          /*   =0:  not valid                        */
                          /*   =1:  valid                            */
 		      	  /*   =2:  bounded		             */

    double minval;	  /* minimum value of the band		     */
    double maxval;	  /* maximum value of the band		     */

    char source[32];	  /* source of data (with NULL)		     */
    char instrument[32];  /* type of sensor (with NULL)		     */
    char direction[64];	  /* direction of capture process(with NULL) */
    char date[10];	  /* capture date (with NULL)		     */
			  /* NOTE: All ddr dates are stored as       */
			  /* "dd-mmm-yy".  For example, a date       */
			  /* of December 31, 1986 is stored as	     */
			  /* "31-dec-86" with the month in lower     */
			  /* case.				     */
    char time[8];	  /* capture time (with NULL)		     */
			  /* NOTE: All ddr times are stored          */
			  /* using a twenty-four hour clock.	     */
			  /* Seconds are speperated by a colon.	     */
			  /* Example: 1:05:55 pm is stored as        */
			  /* 1305:55				     */
};

typedef unsigned char byte;

/**
  This struct exactly matches the layout of a DDR file section header
  on disk.  It should be a total of exactly 32 bytes long.
  Strings are all padded to length with spaces.
*/
struct ddr_header_ondisk {
public:
	char  len[13]; /* number of bytes of data */
	char  type[3]; /* 2-byte data code, like "I4" or "R8" */
	char  key[15]; /* data key, like DDRINT or DDRDUB */
	char nul; /* nul termination */
	/** Fill out a ddr header with c bytes of character data,
	  and b bytes of binary data of type t with key k */
	void set(int c,int b,const char *t,const char *k);
	/** Replace nuls in fields with spaces, and add nul termination */
	void pad(void);
	/** Check this header to make sure it contains this exact data */
	void check(const char *rightValue);
};

/**
 This struct exactly matches the layout of an entire DDR file on disk.
 It's followed by the band records.
*/
struct ddr_file_ondisk {
public:
	ddr_header_ondisk int_header; /* "47/72        I4 DDRINT         ^@" */
	enum {char_len=47};
	byte char_data[char_len]; /* system through last_used_time (nul terminated ASCII) */
	enum {int_len=18};
	osl::io::Big32 int_data[int_len]; /* image size (big-endian) */
	
	ddr_header_ondisk double_header; /* "216          R8 DDRDUB         ^@" */
	enum {double_len=27};
	byte double_data[double_len*8]; /* projection data  (native endian) */
};

/**
 This struct exactly matches a DDR file band record on disk.  
 All the band records come just after the main ddr_file record.
*/
struct ddr_band_ondisk {
public:
	ddr_header_ondisk band_header; /* "151/16       R8 BAND%d          ^@" */
	
	byte char_data[151]; /* data source through capture time */
	enum {double_len=2};
	byte double_data[double_len*8]; /* min and max values for band (native endian) */
};


void ddr_bad(const char *why) {
	fprintf(stderr,"FATAL ERROR> %s\n",why);
	exit(1);
}

/************** DDR File I/O ************/
/** Fill out a ddr header with c bytes of character data (possibly 0),
  and b bytes of binary data of type t with key k */
void ddr_header_ondisk::set(int c,int b,const char *t,const char *k)
{
	if (c==0) sprintf(len,"%d",b); /* no character data; no slash */
	else sprintf(len,"%d/%d",c,b); /* both character and binary data */
	if (strlen(t)>=sizeof(type)) ddr_bad("DDR type string too long");
	if (strlen(k)>=sizeof(key)) ddr_bad("DDR key string too long");
	strcpy(type,t);
	strcpy(key,k);
	pad();
}
/** Replace nuls in fields with spaces, and add nul termination */
void ddr_header_ondisk::pad(void) {
	char *p;
	p=len+strlen(len);
	while (p<type) *p++=' ';
	p=type+strlen(type);
	while (p<key) *p++=' ';
	p=key+strlen(key);
	while (p<&nul) *p++=' ';
	nul=0;
}
/** Check this header to make sure it contains this exact data */
void ddr_header_ondisk::check(const char *rightValue)
{
	if (0!=strncmp(len,rightValue,strlen(rightValue))) {
		ddr_bad("Unexpected value in DDR header--corrupt DDR?");
	}
}

/** Return 1 if we're a big-endian machine; 0 if little-endian */
static int is_big_endian(void) {
	int i=1;
	char *c=(char *)&i;
	return *c==(char)0x00; /* will equal 0x01 on little-endian machine */
}

/**
  Convert these bytes into a double-precision float.
  If endianSwap, swap endianness for your machine.
*/
static double byte2double(int endianSwap,byte *src) {
	double value; /* place we're copying swapped bytes */
	byte *dest=(byte *)&value;
	if (!endianSwap) memcpy(dest,src,sizeof(double));
	else { /* swap endian-ness */
		for (unsigned int i=0;i<sizeof(double);i++)
			dest[i]=src[sizeof(double)-1-i];
	}
	return *(double *)dest;
}

static void set_system(struct DDR *ddr) {
	if (is_big_endian()) /* "standard" big-endian system */
		strcpy(ddr->system,"ieee-std");
	else /* little-endian */
		strcpy(ddr->system,"ieee-lil");
}

/** Initialize a DDR to all zeros. */
ASF_COREDLL void c_intddr(struct DDR *ddr)
{
	memset(ddr,0,sizeof(*ddr));
	set_system(ddr);
	ddr->master_line=ddr->master_sample=1;
	ddr->line_inc=ddr->sample_inc=1.0;
}

/** Read a DDR from the file with this name. Always returns 0. */
ASF_COREDLL int c_getddr(const char *hname,struct DDR *ddr)
{
	c_intddr(ddr);
	/* Read new DDR */
	FILE *f=FOPEN(hname,"rb");
	if (f==NULL) ddr_bad(("Can't open DDR file "+std::string(hname)).c_str());
	ddr_file_ondisk d;
	FREAD(&d,1,sizeof(d),f);
	fclose(f);
	
	/* Check DDR values */
	d.int_header.check("47/72        I4 DDRINT        ");
	d.double_header.check("216          R8 DDRDUB         ");
	int source_big_endian=0, dest_big_endian=is_big_endian();
	if (0==strcmp("ieee-std",(char *)d.char_data)) source_big_endian=1;
	else if (0==strcmp("ieee-lil",(char *)d.char_data)) source_big_endian=0;
	else ddr_bad("DDR is corrupt or some unsupported non-IEEE float format");
	ddr->needs_swap=source_big_endian!=dest_big_endian;
	
	/* Copy fields of disk-DDR onto real DDR */
	int i, *ddr_int=&ddr->nl; /* place to copy DDR int data (FIXME: depends on DDR struct layout) */
	for (i=0;i<ddr_file_ondisk::int_len;i++)
		ddr_int[i]=d.int_data[i];

	char *ddr_char=ddr->system; /* place to copy DDR character data */
	for (i=0;i<ddr_file_ondisk::char_len;i++)
		ddr_char[i]=d.char_data[i];
	
	double *ddr_double=ddr->proj_coef; /* place to copy DDR double data */
	for (i=0;i<ddr_file_ondisk::double_len;i++)
		ddr_double[i]=byte2double(ddr->needs_swap,&d.double_data[i*8]);
	return 0;
}

/** Store a DDR to the file with this name. Always returns 0. */
ASF_COREDLL int c_putddr(const char *hname,struct DDR *ddr)
{
	/* Update the DDR with the WRITING system's value.
	   This is annoying--we will create the new DDR with 
	   the endianness of the writing system; but the image
	   data may have some other endianness if it was 
	   (highly unlikely) somehow left unmodified.
	*/
	set_system(ddr);
	
	/* Set up DDR header values */
	ddr_file_ondisk d;
	d.int_header.set(47,72,"I4","DDRINT");
	d.double_header.set(0,216,"R8","DDRDUB");
	
	/* Copy fields of real DDR onto disk-DDR */
	int i, *ddr_int=&ddr->nl; /* place to read DDR int data (FIXME: depends on DDR struct layout) */
	for (i=0;i<ddr_file_ondisk::int_len;i++)
		d.int_data[i]=ddr_int[i];

	char *ddr_char=ddr->system; /* place to read DDR character data */
	for (i=0;i<ddr_file_ondisk::char_len;i++)
		d.char_data[i]=ddr_char[i];
	
	double *ddr_double=ddr->proj_coef; /* place to read DDR double data */
	memcpy(d.double_data,ddr_double,ddr_file_ondisk::double_len*8);
	
	/* Write the new disk-DDR */
	FILE *f=FOPEN(hname,"wb");
	FWRITE(&d,1,sizeof(d),f);
	
	/* Write fake "band" information */
	for (int b=0;b<ddr->nbands;b++) {
		ddr_band_ondisk d;
		memset(&d,0,sizeof(d));
		char bandNo[100];
		sprintf(bandNo,"BAND%d",b+1);
		d.band_header.set(151,16,"R8",bandNo);
		FWRITE(&d,1,sizeof(d),f);
	}
	
	fclose(f);
	
	return 0;
}
