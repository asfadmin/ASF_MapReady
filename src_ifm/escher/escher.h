
#include "asf.h"
#include "ifm.h"
#include "ddr.h"
#include "asf_meta.h"

/* general constants */
#define MAXNAME         256
#define DO_DEBUG_CHECKS 0

/* 
 * bits are number from 0 (lsb) to 7 (msb)
 *
 * status array 'mask' uses 5 bits: 
 *
 * bit 0 TRUE => +residue
 * bit 1 TRUE => -residue
 * bit 2 TRUE => in a branch cut
 * bit 3 TRUE => grounded
 * bit 4 TRUE => in current search tree
 * bit 5 TRUE => integrated
 *
 * Note IICG means INTEGRTED | IN_CUT | GROUNDED
 *
 * integration array 'im' uses 8 bits:
 * 
 * bit 0 TRUE => tried to move up
 * bit 1 TRUE => tried to move right
 * bit 2 TRUE => tried to move down
 * bit 3 TRUE => tried to move left
 * bit 4 TRUE => arrived at from below
 * bit 5 TRUE => arrived at from left
 * bit 6 TRUE => arrived at from above
 * bit 7 TRUE => arrived at from right
 * 
 * These translate as:
 */

#define ZERO                  (0x00)        /* 0 0 0 0  0 0 0 0 */
#define NEUTRAL               (0xfc)        /* 1 1 1 1  1 1 0 0 */
#define POSITIVE_CHARGE       (0x01)        /* 0 0 0 0  0 0 0 1 */
#define NEGATIVE_CHARGE       (0x02)        /* 0 0 0 0  0 0 1 0 */
#define SOME_CHARGE           (0x03)        /* 0 0 0 0  0 0 1 1 */
#define NOT_IN_CUT            (0xfb)        /* 1 1 1 1  1 0 1 1 */
#define IN_CUT                (0x04)        /* 0 0 0 0  0 1 0 0 */
#define NOT_GROUNDED          (0xf7)        /* 1 1 1 1  0 1 1 1 */
#define GROUNDED              (0x08)        /* 0 0 0 0  1 0 0 0 */
#define NOT_INTEGRATED        (0xef)        /* 1 1 1 0  1 1 1 1 */
#define INTEGRATED            (0x10)        /* 0 0 0 1  0 0 0 0 */
#define IICG                  (0x1c)        /* 0 0 0 1  1 1 0 0 */
#define NOT_IN_TREE           (0xdf)        /* 1 1 0 1  1 1 1 1 */
#define IN_TREE               (0x20)        /* 0 0 1 0  0 0 0 0 */
#define NO_TRIES              (0x00)        /* 0 0 0 0  0 0 0 0 */
#define TRIED_U               (0x01)        /* 0 0 0 0  0 0 0 1 */
#define TRIED_R               (0x02)        /* 0 0 0 0  0 0 1 0 */
#define TRIED_UR              (0x03)        /* 0 0 0 0  0 0 1 1 */
#define TRIED_D               (0x04)        /* 0 0 0 0  0 1 0 0 */
#define TRIED_URD             (0x07)        /* 0 0 0 0  0 1 1 1 */
#define TRIED_L               (0x08)        /* 0 0 0 0  1 0 0 0 */
#define TRIED_URDL            (0x0f)        /* 0 0 0 0  1 1 1 1 */
#define SOURCE_B              (0x10)        /* 0 0 0 1  0 0 0 0 */
#define SOURCE_L              (0x20)        /* 0 0 1 0  0 0 0 0 */
#define SOURCE_A              (0x40)        /* 0 1 0 0  0 0 0 0 */
#define SOURCE_R              (0x80)        /* 1 0 0 0  0 0 0 0 */
#define SOURCE_X              (0xf0)        /* 1 1 1 1  0 0 0 0 */


/* New Data types */
typedef struct _Point { 
  int i; 
  int j; 
} Point;

#define MAX_PLIST 1048576
typedef struct _PList { 
  int n; 
  Point p[1048576]; 
  int c[1048576]; 
} PList;

/* globals */
extern PList list;
extern Uchar *mask;     /* phase-state mask  */
extern Uchar *im;       /* integration mask  */
extern float *phase;   /* input phase       */
extern float *coh;     /* coherence 'rho'   */
/*This is now "phase": float *uwp;   unwrapped phase   */
extern int wid;
extern int len;
extern int size;

/* function declarations */
void usage(char *name);
void loadWrappedPhase(char *phaseName);
void groundBorder(void);
void makeMask(void);
void verifyCuts(void);
Uchar chargeCalc(float ul, float ll, float lr, float ur);
float phaseRemap(float in);
void installCordon(char *cordonName);
void cutMask(void);
void generateCut(int x, int y);
void makeBranchCut(int x1, int y1, int x2, int y2, Uchar orBy);
void saveMask(Uchar *mask, char *maskName);
void readMask(Uchar *mask, char *maskName);
void finishUwp(void);
void checkSeed(int *new_seedX, int *new_seedY);
void integratePhase(int x, int y);
void saveUwp(char *uwpName);
void doStats(char *message);

