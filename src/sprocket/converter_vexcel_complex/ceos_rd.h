#include <sys/types.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

        /* Data file */
#define LENGTH_OF_IOF           ( 16252 )
#define LENGTH_OF_PDR           ( 9860 )

        /* Leader file */
#define LENGTH_OF_LEADER        ( 720 )
#define LENGTH_OF_DSSR          ( 4096 )
#define LENGTH_OF_DQSR          ( 1620 )
#define LENGHT_OF_SDHR          ( 16920 )
#define LENGTH_OF_PDHR          ( 16920 )
#define LENGTH_OF_PPR           ( 7726 )
#define LENGTH_OF_PPDR          ( 8960)
#define LENGTH_OF_ADR           ( 8960 )
#define LENGTH_OF_RDR           ( 9860 )

#define START_OF_LEADER  (0)
#define START_OF_DSSR           ( START_OF_LEADER + LENGTH_OF_LEADER )
#define START_OF_DQSR           ( START_OF_DSSR + LENGTH_OF_DSSR )
#define START_OF_SDHR           ( START_OF_DQSR + LENGTH_OF_DQSR )
#define START_OF_PDHR           ( START_OF_SDHR + LENGHT_OF_SDHR )
#define START_OF_PPR            ( START_OF_PDHR + LENGTH_OF_PDHR )
#define START_OF_PPDR           ( START_OF_PPR + LENGTH_OF_PPR )
#define START_OF_ADR            ( START_OF_PPDR + LENGTH_OF_PPDR)
#define START_OF_RDR            ( START_OF_ADR + LENGTH_OF_ADR )
#define START_OF_RCDR           ( START_OF_RDR + LENGTH_OF_RDR )

#define START_OF_IOF            ( 0 )
#define START_OF_PDR            ( START_OF_IOF + LENGTH_OF_IOF)


extern double ceos_read_double ( int fd, int position, int length);
extern int ceos_read_int( int fd, int position, int length );
extern void ceos_read_char ( int fd, int position, int length, char * str );
extern int ceos_read_binary ( int fd, int position, int length);
