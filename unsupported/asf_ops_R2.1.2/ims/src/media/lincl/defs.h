#include  <stdio.h>
#include  <sys/types.h>
#include  "recvdr.h"
#include  "oceos.h"

#ifndef     TRUE
#define     TRUE   1
#endif
#ifndef     FALSE
#define     FALSE  0
#endif

#define  NONO      -1
#define  FROM_LDR  0
#define  FROM_VDF  1
#define  FROM_TAPE 2

#if 0
#define  MATCH_RECORD(tb, b0, b1, b2, b3)  ( b0==tb[0] && b1==tb[1] && b2==tb[2] && b3==tb[3] )
#endif

#define  MATCH_RECORD(tb, b0, b1, b2, b3)  ( b0==tb[0] && b1==tb[1] && b3==tb[3] )

#define  READ_ERROR -2
#define  END_OF_FILE -1
#define  DECODE_ERROR 0
#define  DECODE_OK    1

#define  MATCH_FOUND 1
#define  NO_MATCH    2

#define  BUF_FDR_SIZE  720
#define  BUF_BIG_SIZE  20000

#define  READ_ONLY    1
#define  WRITE_ONLY   2

#define  EMPTY -1

