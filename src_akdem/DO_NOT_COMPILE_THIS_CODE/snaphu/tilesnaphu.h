/* file tilesnaphu.h */


/* defined constants */

#define DEF_NPTHREADS    1
#define MAXPTHREADS      64
#define SECONDSPERPIXEL  5e-7
#define DEF_MSTCOSTSCALE 200

#ifdef USE_PTHREADS
#include <pthread.h>
#endif


/* usage message */

#define TILEUSEMESSAGE\
 "usage: tilesnaphu nx ny xovrlp yovrlp [options] <snaphu command> \n"\
 "nx, ny            number of tiles across and down\n"\
 "xovrlp, yovrlp    number of pixels tiles should overlap in x and y\n"\
 "options:\n"\
 "  --path <path>          path to snaphu executable\n"\
 "  --nproc <int>          number of processors to use\n"\
 "  --quiet                supress stdout of snaphu calls\n"\
 "  --demifm <dem_argfile>  generate seed points.\n"\
 "   		demargfile contents:\n"\
 "   <igram_byte image.ext> <meta file> <base file> <dem file.ext>\n"

/* structures and function prototypes for parallel code */

#ifdef USE_PTHREADS

typedef struct callsnaphuargST{
  long *iptr;
  char **snaphucallarr;
  pthread_mutex_t *nextcallmutex;
  long ncalls;
}callsnaphuargT;

void *CallSnaphu(void *callsnaphuarg);

#endif


/* function prototypes */

void AssembleTiles(char ***outfilearr, float ***magptr, float ***phaseptr, 
		   long linelen, long nlines, long nrow, long ncol, 
		   long rowovrlp, long colovrlp, long ni, long nj, 
		   signed char fileformat);
void AdjustTileOffsets(float **unwphase, long linelen, long nlines, 
		       long nrow, long ncol, long rowovrlp, long colovrlp, 
		       long ni, long nj);
void UnsetDumpFile(char **dumpfile, char *printwarningptr);



