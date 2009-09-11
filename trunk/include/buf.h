#ifndef _BUF_H_
#define _BUF_H_

struct BUF
{
	unsigned char *buf_start;
	unsigned char *buf_end;
	unsigned char *nxt_offset;
	unsigned char *nxt_buf;
	int cnt;
	unsigned char *conbuf;
};

struct UPBUF
{
	unsigned char *buf_start;
	unsigned char *buf_end;
	unsigned char *nxtrd_buf;
	unsigned char *nxtwt_buf;
	unsigned char *nxt_offset;
	int rdcnt;
	int wtcnt;
	unsigned char *conbuf;
};

#endif
