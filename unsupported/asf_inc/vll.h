#define	READ	0
#define	WRITE	1
#define APPEND  2
#define	RW	3

#define	LENL	13
#define	TYPL	3
#define	KEYL	16
#define	HDRL	(LENL + TYPL + KEYL)

#ifndef min
#define min(a,b)	((a) < (b) ? (a) : (b))
#endif

struct HDR
    {
    char  len[LENL];
    char  type[TYPL];
    char  key[KEYL];
    };

#ifdef VMS

#define	unlink	delete

#define	L_SET	0
#define	L_INCR	1
#define	L_XTND	2

#define R_OK	4
#define	W_OK	2
#define	X_OK	1
#define	F_OK	0

#endif
