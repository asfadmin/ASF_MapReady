#include "asf.h"

struct FDESC		/* file descriptor */
{

	int nl;		/* number of lines in image window */
	int ns;		/* number of samples in image window */
	int img_nl;		/* image number of lines */
	int img_ns;		/* image number of samples */
	int sl;		/* starting line */
	int ss;		/* starting sample */
	int *offset;		/* pointer to users offset array */
 	int acc;		/* access READ WRITE or UPDATE */
	int method;		/* access method RANDOM or SEQ */
	int dtype;		/* data type of data */
	int nbuf;		/* number of circular buffers */
	int linsiz;		/* actual line size in bytes */
	int conv_type;		/* data type to be converted to */
	int cnvlsz;		/* actual line size of converted data */
	char fname[CMLEN];	/* host name of the data file */
	int nbands;		/* number of bands to be processed */
	unsigned int flags;		/* misc flags */
	int bands[MAXBND];	/* band array of specific band numbers */
/*	struct IFCB ifcb; */	/* the i_ ifcb */	
	struct BUF *buf;	/* buffer structure */
	struct UPBUF *upbuf;	/* update buffer structure */
        FILE *fp;               /* file pointer for compressed or dal files */
	int dalfd;		/* dal image file descriptor */
	unsigned char *conv_buf;	/* conversion buffer address */
	int file_linsiz;	/* linesize of the file */
        int compres;           /* compression image flag    */
	int dal;		/* dal image flag */
	int tae;		/* tae image flag */
	int *foffset;		/* compression file pointer	      */
	int *f_pointer;	/* line, band offset array for compressed file*/
	int mbands;		/* number of bands in compressed file */
	int mns;		/* number of bands in compressed file */
	unsigned char *tempbuf; /* temporary buffer for a single line */
};

struct GDESC		/* group descriptor */
{
	struct FDESC *fdarray[MAXIMG];	 /* array of file descriptors group */
	int	nl;			/* number of lines to be processed */
	int	nfiles;			/* number of files in group */
	int	line_cnt;		/* line counter for current line */
	unsigned char	*thebuf;	/* address of buffer pool */
	int	thebufsz;		/* size of buffer pool */
	unsigned char	*the_nxt_space;	/* next allocatable address in buffer
						pool */
	int	gbl_nlines;		/* number of lines in buffer pool */
};

FUNCTION char *c_ealloc(struct FDESC *fdarray[], int *buffersz);
lasErr FUNCTION c_eclose(struct FDESC **fdesc);
lasErr FUNCTION c_egclse(struct GDESC **gdesc);
lasErr FUNCTION c_egroup(struct FDESC *fdarray[], struct GDESC **gdesc, unsigned char *buffer, int *buffersz);
lasErr FUNCTION c_eopenr(struct FDESC **fdesc,char *fname,int *nbands,
		int *sl,int *ss,int *nl,int *ns,
		int *acc,int *dtype,int opt[],int *maxline);
		
lasErr FUNCTION c_eopens(struct FDESC **fdesc,char *fname,int bands[],
		int *sl,int *ss,int *nl,int *ns,int offset[],
		int *acc,int *dtype,int *nbuf,int opt[]);
		
lasErr FUNCTION c_eread(struct FDESC **fdesc, int *eband,int * eline, 
		unsigned char *euserbuf,int *nlines);
int FUNCTION c_ewrite(struct FDESC **fdesc,int * eband, int *eline, 
		const unsigned char *euserbuf,int * nlines);

lasErr FUNCTION c_estep(struct GDESC **gdesc);
lasErr FUNCTION do_open(struct FDESC *fd);
